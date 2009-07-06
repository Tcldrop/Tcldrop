# server/main --
#	Handles:
#		* IRC server support.
#	Depends: core::conn, core.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008,2009 Tcldrop Development Team <Tcldrop-Dev@Tcldrop.US>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program (see gpl.txt); if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# Or visit http://www.GNU.Org/licenses/gpl.html#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
#	IRC server module for Tcldrop (makes client connections to other IRC servers).
#	Depends on: core::conn.

namespace eval ::tcldrop::server {
	variable version {0.8}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable name {server}
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {core::conn core}
	variable author {Tcldrop-Dev}
	variable description {IRC server support.}
	variable rcsid {$Id$}
	# The isupport ensemble:
	namespace ensemble create -command isupport -subcommands [list get set] -map [dict create get isupport_get set isupport_set]
	variable commands [list isbotnick jump putserv puthelp putquick queuesize clearqueue putqueue putnow server quit callraw isupport]
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
}

proc ::tcldrop::server::isbotnick {nick} { string equal -nocase $nick $::botnick }

# server binds are even more "raw" than RAW binds..
proc ::tcldrop::server::callserver {idx line} {
	foreach {type flags mask proc} [bindlist server] {
		if {[string equal -nocase $mask $line]} {
			if {[catch { $proc $idx $line } err]} {
				putlog "(server) Error in script: $proc: $err"
				puterrlog "$::errorInfo"
			} elseif {[string equal $err {1}]} {
				# Abort processing further binds if they return 1.
				return 1
			}
		}
	}
	set lline [split $line]
	if {[string index $line 0] != {:}} {
		set from ${::server}
		set key [lindex $lline 0]
		set arg [join [lrange $lline 1 end]]
	} else {
		set from [string range [lindex $lline 0] 1 end]
		set key [lindex $lline 1]
		set arg [join [lrange $lline 2 end]]
	}
	callraw $from $key $arg
	putloglev r * "\[@\] $line"
	if {${::check-stoned}} {
		# Receiving anything from the server should mean it's still alive..
		variable LastPONG [clock seconds]
	}
}

# Unused, callserver is the "control" proc now.
proc ::tcldrop::server::Read {idx line} { callserver $idx $line }

# Sends the NICK and USER info to the IRC server as soon as the socket is open:
proc ::tcldrop::server::Write {idx} {
	if {[valididx $idx]} {
		global nick username realname my-hostname my-host serveraddress server-idx check-stoned
		set server-idx $idx
		callevent preinit-server
		# FixMe: Send the PASS command here, if necessary.
		puthelp "NICK $nick"
		if {[info exists my-hostname] && ${my-hostname} != {}} { set hostname ${my-hostname} } elseif {[info exists my-host] && ${my-host} != {}} { set hostname ${my-host} } else { set hostname [info hostname] }
		puthelp "USER $username $hostname [lindex [split $serveraddress {:}] 0] :$realname"
		# RFC2812:    USER <user> <mode> <unused> :<realname>
		# Eggdrop:    USER SafeTcl . . :http://SafeTcl.AltURL.Com/
		# mIRC:       USER FireEgl "sebastian" "10.1.7.1" :FireEgl
		# irssi:      USER root Ariel 10.1.7.1 :root
		# CGIIRC:     USER 40e9ad6a localhost irc.tcldrop.us :[40e9ad6a] AnonProx.com User
		# tcllib irc: USER $username $hostname $servername :$userinfo
		# Other:      USER someName localhost irc.afternet.org :Firstname Lastname
		# HydraIRC:   USER FireEagle 0 * :FireEgl
		# BitchX:     USER root +iws root :root
		if {${check-stoned}} {
			# Initialize LastPONG (We don't need to PING the server right away, we just connected):
			variable LastPONG [clock seconds]
			# Start a timer that checks LastPONG and sends the PINGs:
			# Note: Eggdrop sends a PING every 5 minutes.
			# This should be higher than the frequency the server PINGs us.
			# FixMe: Perhaps find out the frequency the server is sending us PINGs and make sure we already have a timer higher than that..
			timer 14 [list {::tcldrop::server::CheckStoned}] -1
		}
	} else {
		Error SOCKET {Unknown error}
	}
}

proc ::tcldrop::server::CheckStoned {} {
	variable LastPONG
	if {$LastPONG} {
		if {[clock seconds] - $LastPONG > 999} {
			# It's been too long since we heard anything from the server, assume it's dead:
			quit "Stoned server..  \xAF\\(o_\xBA)/\xAF  "
			# Jump to another server, subtracting the time we last heard from the server from the server-cycle-wait time (Let utimer deal with negative numbers):
			variable ServerCycleTimerID [utimer [expr { ${::server-cycle-wait} - ([clock seconds] - $LastPONG) }] [list ::tcldrop::server::jump]]
		} elseif {[clock seconds] - $LastPONG > 360} {
			# Only send PINGs when we haven't heard from the server in a while.
			putqueue idle "PING [clock seconds]"
		}
	}
}

# Update LastPONG whenever we receive a PONG (in response to our PING):
proc ::tcldrop::server::PONG {from key arg} {
	variable LastPONG [clock seconds]
	return 0
}

# Closes the connect to the current IRC server.
proc ::tcldrop::server::quit {{reason {}}} {
	if {${::server-idx}} {
		variable LastPONG 0
		# We have a server idx, but it may not necessarily be fully connected (logged into) the server.
		if {${::server-online}} {
			# We're fully logged into the server.
			# Call the predisconnect-server events:
			callevent predisconnect-server
			# Set return to the server-online time:
			set return ${::server-online}
		} else {
			# Set return to 1, to indicate that we were (at least partially) connected to a server:
			set return 1
		}
		# Send a QUIT to the server, to be nice:
		putnow "QUIT :$reason"
		# Kill any background timers that may try to do a jump:
		variable ServerCycleTimerID [killtimer $ServerCycleTimerID]
		# Clear the server queue:
		clearqueue all
		# Set the globals to their offline defaults:
		set ::server [set ::serveraddress {}]
		set ::server-online 0
		# Kill the IDX to the server:
		killidx ${::server-idx}
		set ::server-idx 0
		# If we were fully connected to a server before, call the disconnect-server events:
		if {$return > 1} { callevent disconnect-server }
		return $return
	}
	return 0
}

proc ::tcldrop::server::server {args} { Server $args }
proc ::tcldrop::server::Server {opts} {
	callevent connect-server
	global server serveraddress network server-online server-idx server-cycle-wait
	array set options [list -control ::tcldrop::server::callserver -errors ::tcldrop::server::Error -writable ::tcldrop::server::Write]
	array set options $opts
	if {[info exists options(-pass)]} {
		# FixMe: Store the password somewhere so we can use it during login to the ircd.
		unset options(-pass)
	}
	if {![catch { ::tcldrop::core::conn::Connect [array get options] } idx]} {
		switch -- $network {
			{Unknown} - {I.didn't.edit.my.config.file.net} - {unknown-net} - {} { set handle {(server)} }
			{default} { set handle $network }
		}
		idxinfo $idx handle $handle remote $options(-address):$options(-port) hostname $options(-address) type {SERVER} other {serv} traffictype {irc} module {server} timestamp [clock seconds]
		set server-idx $idx
		# Eggdrop compatibility stuff:
		set serveraddress "$options(-address):$options(-port)"
		set server-online 0
	} else {
		# Wait $server-cycle-wait before trying again.
		variable ServerCycleTimerID [utimer ${server-cycle-wait} [list ::tcldrop::server::jump]]
	}
	return "$options(-address):$options(-port)"
}

# This procs job is to close the current server connection,
# and then wait until it's time to connect to another.
proc ::tcldrop::server::Error {idx {reason {Error}}} {
	quit $reason
	variable ServerCycleTimerID [utimer ${::server-cycle-wait} [list ::tcldrop::server::jump]]
}

# Eggdrop style: jump <server> <port> [password]
# Tcldrop style: jump <chain> [-pass <password>]
# options style: jump -proxychain <chain>
proc ::tcldrop::server::jump {args} {
	quit {Changing Servers...}
	array set options [list -address {} -port {0} -pass {} -retry 0 -buffering line -blocking 0 -myaddr ${::my-ip} -async 1 -connect-timeout ${::server-timeout} -inactive-timeout ${::inactive-timeout} -ssl 0 -proxychain {} -proxyinfo [list]]
	# There's 3 different ways to specify the address and port to connect to...
	# Proxychain style: address:port or http://127.0.0.1:8080/address:port
	# Option type style: -address <address> -port <port>
	# Eggdrop style: address port
	switch -glob -- [lindex $args 0] {
		{*:*} {
			# Proxychain style.
			array set proxychain [::proxy::splitchain [lindex $args 0]]
			lappend args -address $proxychain(address) -port $proxychain(port) -pass $proxychain(extra) -proxychain [lindex $args 0] -proxyinfo [array get proxychain]
			if {[llength [lrange $args 1 end]] % 2} {
				lappend args -pass [lindex $args 1]
				array set options [lrange $args 2 end]
			} else {
				array set options [lrange $args 1 end]
			}
		}
		{-*} {
			# Option type style.
			array set options $args
		}
		{default} {
			# Eggdrop style.
			array set options [list -address [lindex $args 0] -port [lindex $args 1]]
			set args [lrange $args 2 end]
			if {[llength [lrange $args 2 end]] % 2} {
				# Odd number, meaning they also specified a password as the 3rd arg.
				lappend args -pass [lindex $args 2]
				array set options [lrange $args 3 end]
			} else {
				array set options [lrange $args 2 end]
			}
		}
	}
	# Make sure the -port option is valid:
	if {![string is int -strict $options(-port)] || 1 > $options(-port) || $options(-port) > 65535 } {
		set options(-port) ${::default-port}
	}
	# If -address isn't set, it means a server wasn't specified and we should pick one from $::servers:
	if {$options(-address) == {}} {
		# FixMe: There should be a global option to decide weather to choose a random server, or to go in the same order as the $::servers list.
		after idle [list jump [set nextserver [lindex $::servers [rand [llength $::servers]]]]]
		return $nextserver
	} elseif {![info exists options(-proxychain)] || $options(-proxychain) == {}} {
		set options(-proxychain) "$options(-address):$options(-port)"
	}
	# Make sure we didn't leave the password at the end of -proxychain:
	if {$options(-pass) != {} && [string match "*$options(-pass)" $options(-proxychain)]} { set options(-proxychain) [join [lrange [split $options(-proxychain) :] 0 end-1] :] }
	# Make sure the port is at the end of -proxychain:
	if {[string match {*:} $options(-proxychain)]} { append options(-proxychain) $options(-port) }
	Server [array get options]
}

# This runs all the RAW IRC binds:
# (They should return 0 if they want tcldrop to continue processing, 1 if not.)
proc ::tcldrop::server::callraw {from key arg} {
	foreach {type flags mask proc} [bindlist raw] {
		if {[string equal -nocase $mask $key]} {
			if {[catch { $proc $from $key $arg } err]} {
				putlog "(callraw) Error in script: $proc: $err"
				puterrlog "$::errorInfo"
			} elseif {$err == 1} {
				# Abort processing further binds if they return 1.
				break
			}
		}
	}
}

# FixMe: This should run all the RAW IRC filt binds:
# (This is so scripts can modify what the rest of Tcldrop, and other scripts see)
# They should return the line, modified or not.
# If they return "" then Tcldrop will abort processing any further binds.
proc ::tcldrop::server::FILTRAW {line} { set line }

proc ::tcldrop::server::clearqueue {{queue {all}}} {
	variable QueueAliases
	if {$queue == {all}} {
		set queue {*}
	} elseif {[info exists QueueAliases($queue)]} {
		set queue $QueueAliases($queue)
	}
	variable Queue
	set count [queuesize $queue]
	array unset Queue $queue
	set count
}

proc ::tcldrop::server::queuesize {{queue {all}}} {
	variable QueueAliases
	if {$queue == {all}} {
		set queue {*}
	} elseif {[info exists QueueAliases($queue)]} {
		set queue $QueueAliases($queue)
	}
	variable Queue
	set count 0
	foreach a [array names Queue $queue] { incr count [llength $Queue($a)] }
	set count
}

# Penalties are only used when we've already sent 5 lines within the last 10 seconds.
# The penalty for the next line we should send is in milliseconds.
proc ::tcldrop::server::GetPenalty {line} {
	variable SentData
	# Add the current milliseconds for this line to the list..
	lappend SentData(lastclicks) [clock clicks -milliseconds]
	# Make sure there's only at most 5 in the list..
	if {[llength $SentData(lastclicks)] > 5} { set SentData(lastclicks) [lrange $SentData(lastclicks) end-4 end] }
	set allowance 0
	# See how much allowance we've saved up for bursts:
	foreach b $SentData(lastclicks) { incr allowance [expr {[clock clicks -milliseconds] - $b}] }
	# > 50000 means we haven't bursted our 5 lines in 10 seconds allowance..
	if {$allowance > 50000} {
		# Note, it's possible that this gives a percent over 100..
		#putloglev d * {burst: 1}
		# Let's spend our allowance!  =D
		set penalty 0
	} else {
		#putloglev d * {burst: 0}
		# We can't burst any more, so apply the penalties...
		# Note that 1000 (ms) equals 1 second....
		switch -- [string toupper [lindex [split $line] 0]] {
			{INVITE} { set penalty 3000 }
			{JOIN} { set penalty 2000 }
			{PART} { set penalty 4000 }
			{VERSION} { set penalty 2000 }
			{TIME} { set penalty 2000 }
			{TRACE} { set penalty 2000 }
			{NICK} { set penalty 3000 }
			{ISON} { set penalty 1000 }
			{WHOIS} { set penalty 2000 }
			{DNS} { set penalty 2000 }
			{PING} { set penalty 2000 }
			{PONG} { set penalty 750 }
			{default} {
				# The number of destinations (targets) for the command:
				# Note: it's possible that targets can be 0, if the command
				#       we're sending doesn't have any arguments to it.
				set targets [llength [split [lindex [split $line] 1] ,]]
				# The length:
				set length [string length $line]
				# 2 second minimum (I know it says 1 here, but it works out to 2 later):
				set minimum {1}
				# Uhm..
				set divisor {120.0}
				# Anyways.. I fiddled with the numbers until it looked right to me..
				# Somebody else can fiddle with them some more if they like.
				set penalty [expr { int(($minimum + ($divisor + $length * $targets) / $divisor) * 1000) }]
				# The smallest result will be 2004.
			}
		}
		# Return the penalty:
		set penalty
	}
}

# Takes lines into the outgoing server queue,
# sends them immediately if it's able to burst,
# and flushes lines out when it calls itself.
# $queue should be a number, the lower the number the higher priority the $line will be.
# Please note that queue 0 is reserved.  Use 1 or higher when specifying queues.
# $line is the text you want to send to the server.
# $option can be either -normal or -next (Like in Eggdrop).
proc ::tcldrop::server::Queue {{queue {99}} {line {}} {option {-normal}}} {
	variable Queue
	if {$queue != 0} {
		# Add $line to $queue...
		if {[string equal {-next} $option]} {
			# They want it next, fine!
			if {[info exists Queue($queue)]} {
				set Queue($queue) [linsert $Queue($queue) 0 $line]
			} else {
				set Queue($queue) [list $line]
			}
		} else {
			# Otherwise we just append it to the list..
			lappend Queue($queue) $line
		}
	}
	if {![array size Queue]} {
		# Leave if there's no queues to play with..
		return 0
	}
	variable SentData
	# Let's see, how long did we wait...
	if {[set waited [expr {[clock clicks -milliseconds] - [lindex $SentData(lastclicks) end] + 99}]] >= $SentData(penalty)} {
		# The queues are sorted by their priority (lower number means it gets dealt with first)..
		foreach a [lsort [array names Queue]] {
			# Process the next queue.
			# Process the next line in this queue.
			foreach line $Queue($a) {
				if {[llength $Queue($a)] <= 1} {
					# Only one line left? Well that's the one we're about to send!
					# So we delete the queue...
					unset Queue($a)
				} else {
					# Otherwise, we just remove the current line from the queue.
					set Queue($a) [lreplace $Queue($a) 0 0]
				}
				# Flush this line to the server using putnow..
				putnow $line
				putloglev v * "\[$a->\] $line"
				# The time (in milliseconds) before we try again..
				if {[set SentData(penalty) [GetPenalty $line]] > 0} {
					# Try again after the penalty expires (plus a tad longer):
					after [expr {$SentData(penalty) + 99 + $a}] [list ::tcldrop::server::Queue 0]
					# Get out of here, there's nothing more we should do since we have to wait...
					return
				}
				# We process the next queued line on the next iteration..
			}
			# We process the next queue on the next iteration..
		}
	} elseif {$queue == 0} {
		# What?!  We didn't wait long enough!?  How is that possible! =/
		# Anyways, let's wait out the rest of the time, plus a little bit more and try again..
		after [expr {$SentData(penalty) - $waited + 99 + $a}] [list ::tcldrop::server::Queue 0]
	}
}

# Sends the text to the IRC server.
# Note: This shouldn't be used by scripters, as it bypasses the queuing and penalty systems completely.
#       But if they really want to flood the server, there should be no stopping them from their stupidity.
proc ::tcldrop::server::putnow {text} { if {![putidx ${::server-idx} $text]} { clearqueue all } }

proc ::tcldrop::server::TraceNick {name1 name2 op} { puthelp "NICK $::nick" }
trace add variable ::nick write [list ::tcldrop::server::TraceNick]

# Adds $text to a queue..
# $queue must be mode, serv, help, or a integer from 1-99.
# $option can be -normal or -next (Like in Eggdrop).
proc ::tcldrop::server::putqueue {queue text {option {-normal}}} {
	if {[queuesize] < ${::max-queue-msg}} {
		variable QueueAliases
		if {[info exists QueueAliases($queue)]} { set priority $QueueAliases($queue) }
		variable Queue
		# Unlike Eggdrop, we deal with people sending multiple lines at once..
		foreach line [split $text \n] {
			if {$line != {}} {
				if {[info exists Queue($priority)] && [info exists "::double-$queue"] && ![set "::double-$queue"] && [lsearch -exact $Queue($priority) $line] != -1} {
					putloglev d * "msg already queued. skipping: $line"
				} else {
					Queue $priority $line $option
					putloglev v * "\[!$queue\] $line"
				}
			}
		}
	}
}

proc ::tcldrop::server::putquick {text {option {-normal}}} { putqueue mode $text $option }

proc ::tcldrop::server::putserv {text {option {-normal}}} { putqueue server $text $option }

proc ::tcldrop::server::puthelp {text {option {-normal}}} { putqueue help $text $option }

# Reply to server PINGs:
proc ::tcldrop::server::PING {from key arg} { putqueue quick "PONG $arg" -next }

# Handle server ERRORs:
proc ::tcldrop::server::ERROR {from key arg} { if {${::servererror-quit}} { Error {ERROR} $arg } }

# Set our basic info (botnick, etc) and call the init-server event:
proc ::tcldrop::server::001 {from key arg} {
	variable ServerCycleTimerID
	catch { killutimer $ServerCycleTimerID }
	# Save the server's real name.
	set ::server ${from}:[lindex [split $::serveraddress :] end]
	idxinfo ${::server-idx} remote $::server
	set ::botnick [lindex [split $arg] 0]
	set ::server-online [clock seconds]
	# Set isupport to user-defined defaults:
	set ::isupport($::serveraddress) [dict create NETWORK $::network MODES ${::modes-per-line} MAXBANS ${::max-bans} MAXEXEMPTS ${::max-exempts} MAXINVITES ${::max-invites} NICKLEN $::nicklen]
	# Eval $init-server (obsolete in Eggdrop):
	if {[info exists ::init-server] && ${::init-server} != {}} { catch { eval ${::init-server} } }
	# Call the init-server binds:
	callevent init-server
}

# Note, other RAW binds are in the irc module.


# Note: These are the raw 005's from several networks.  (collected on November 29, 2003)
# FixMe: Add support for these to Tcldrop.
# calvino.freenode.net: 5 FireEgl MODES=4 MAXCHANNELS=20 NICKLEN=16 USERLEN=10 HOSTLEN=63 TOPICLEN=450 KICKLEN=450 CHANNELLEN=30 KEYLEN=23 CHANTYPES=# PREFIX=@+ CASEMAPPING=ascii CAPAB IRCD=dancer are available on this server
# irc.choopa.net: 5 FireEgl STD=i-d STATUSMSG=@+ KNOCK EXCEPTS INVEX MODES=4 MAXCHANNELS=90 MAXBANS=100 MAXTARGETS=6 NICKLEN=9 TOPICLEN=120 KICKLEN=120 are supported by this server
# irc.choopa.net: 5 FireEgl CHANTYPES=#& PREFIX=(ov)@+ CHANMODES=eIb,k,l,imnpst NETWORK=EFnet CASEMAPPING=rfc1459 CHARSET=ascii CALLERID ETRACE WALLCHOPS are supported by this server
# localhost.localdomain: 5 FireEgl WHOX WALLCHOPS WALLVOICES USERIP CPRIVMSG CNOTICE SILENCE=15 MODES=6 MAXCHANNELS=15 MAXBANS=45 NICKLEN=9 TOPICLEN=160 AWAYLEN=160 KICKLEN=160 are supported by this server
# localhost.localdomain: 5 FireEgl CHANTYPES=#& PREFIX=(ov)@+ CHANMODES=b,k,l,imnpstr CASEMAPPING=rfc1459 NETWORK=UnderNet are supported by this server
# Elsene.Be.Eu.undernet.org: 5 FireEgl WHOX WALLCHOPS WALLVOICES USERIP CPRIVMSG CNOTICE SILENCE=15 MODES=6 MAXCHANNELS=20 MAXBANS=45 NICKLEN=9 TOPICLEN=160 AWAYLEN=160 KICKLEN=160 are supported by this server
# Elsene.Be.Eu.undernet.org: 5 FireEgl CHANTYPES=#& PREFIX=(ov)@+ CHANMODES=b,k,l,imnpstr CASEMAPPING=rfc1459 NETWORK=UnderNet are supported by this server
# hotspeed.sg.as.dal.net: 5 FireEgl NOQUIT WATCH=128 SAFELIST MODES=6 MAXCHANNELS=20 MAXBANS=100 NICKLEN=30 TOPICLEN=307 KICKLEN=307 CHANTYPES=# PREFIX=(ov)@+ NETWORK=DALnet SILENCE=10 CASEMAPPING=ascii CHANMODES=b,k,l,ciLmMnOprRst are available on this server
#
# Note: Here's my idea for adding support for these...
#       When the bot connects to a server (raw 001), it takes all the globals
#       such as modes-per-line, for example, and other per-server specific
#       settings and puts them into the servers idxinfo..
#       When it reaches raw 005 it'll replace those defaults in the servers
#       idxinfo with the ones the server supports, like MODES=4, for example.
#       The bot should then always look at the servers idxinfo instead of the
#       global var for deciding how many modes-per-line to use (for example).
#
# References:
# http://www.irc.org/tech_docs/005.html
# http://www.irc.org/tech_docs/draft-brocklesby-irc-isupport-03.txt
# http://www.unrealircd.com/files/docs/unreal32docs.html#userchannelmodes
proc ::tcldrop::server::005 {from key arg} { isupport set {*}[lrange [split $arg] 1 end] }

# isupport set <key>=<value> key -key ...
proc ::tcldrop::server::isupport_set {args} { global isupport serveraddress
	# Yes, this is a string map on a list..But it doesn't hurt anything in this case:
	foreach i [string map {{are supported by this server} {} {:are supported by this server} {} {are available on this server} {}} $args] {
		set key [string range [string range $i 0 [set eq [string first = $i]]] 0 end-1]
		set value [string range [string range $i $eq end] 1 end]
		switch -glob -- $i {
			{} - { } {}
			{-*} {
				# "... used to negate a previously specified parameter; that is, revert to the behaviour that would occur if the parameter had not been specified."
				switch -- $key {
					{-NETWORK} { dict set isupport($serveraddress) NETWORK $::network }
					{-MODES} { dict set isupport($serveraddress) MODES ${::modes-per-line} }
					{-MAXBANS} { dict set isupport($serveraddress) MAXBANS ${::max-bans} }
					{-MAXEXEMPTS} { dict set isupport($serveraddress) MAXEXEMPTS ${::max-exempts} }
					{-MAXINVITES} { dict set isupport($serveraddress) MAXINVITES ${::max-invites} }
					{-NICKLEN} { dict set isupport($serveraddress) NICKLEN $::nicklen }
				}
			}
			{*=*} {
				# If they don't need special treatment we deal with them here.
				dict set isupport($serveraddress) $key $value
			}
			{default} {
				# This should be set to boolean true, but not set to "1" so it doesn't get misinterpreted as an amount:
				dict set isupport($serveraddress) $i {true}
			}
		}
	}
}

# This is just to compliment the "isupport set" command..
# There's a reason isupport is a global variable..it's expected that people will at least read it directly, if not write to it directly as well.
# isupport get <key>
proc ::tcldrop::server::isupport_get {key} { global isupport serveraddress
	if {[info exists isupport($serveraddress)] && [dict exists $isupport($serveraddress) $key]} {
		dict get $isupport($serveraddress) $key
	}
}

proc ::tcldrop::server::EVNT_exit {event} { if {[info exists ::die]} { putnow "QUIT :$::die" } else { putnow "QUIT :$event" } }

proc ::tcldrop::server::DIE {reason} { quit $reason }

# This is a LOAD event, because the variables should be there in case some 3rd-party script does a jump during a (re)start:
bind load - server ::tcldrop::server::LOAD -priority 0
proc ::tcldrop::server::LOAD {module} {
	# Initialize variables:
	variable Queue
	array set Queue {}
	variable SentData
	# Set the initial penalty and seed the burst allowance:
	array set SentData [list penalty 0 lastclicks [list [expr {[clock clicks -milliseconds] - 99999}] [expr {[clock clicks -milliseconds] - 99999}] [expr {[clock clicks -milliseconds] - 99999}] [expr {[clock clicks -milliseconds] - 99999}] [expr {[clock clicks -milliseconds] - 99999}]]]
	# These are aliases for the queues, because we use integers to specify queues internally.
	variable QueueAliases
	array set QueueAliases [list quick 10 q 10 mode 15 m 15 server 30 serv 30 s 30 help 75 h 75 last 99 l 99 idle 99 i 99]
	variable ServerCycleTimerID 0
	# Default server related settings (These are here in case the user doesn't provide them in his/her config):
	setdefault servers [list]
	setdefault default-port {6667}
	setdefault nick {Tcldrop}
	setdefault username {Tcldrop}
	setdefault realname {www.Tcldrop.US}
	setdefault server-online {0} -protect 1
	setdefault server {} -protect 1
	setdefault serveraddress {} -protect 1
	setdefault server-idx {0} -protect 1
	setdefault botname {} -protect 1
	setdefault botnick {} -protect 1
	setdefault server-timeout {87}
	setdefault server-cycle-wait {93}
	setdefault servererror-quit {1}
	setdefault check-stoned {1}
	setdefault max-queue-msg {99}
	setdefault network {Unknown}
	setdefault modes-per-line {3} -protect 1
	setdefault max-bans {12}
	setdefault max-exempts {13}
	setdefault max-invites {13}
	setdefault nicklen {9}
	bind raw - PONG ::tcldrop::server::PONG -priority 100
	bind raw - PING ::tcldrop::server::PING -priority 100
	bind raw - ERROR ::tcldrop::server::ERROR -priority 100
	bind raw - 001 ::tcldrop::server::001 -priority 100
	bind raw - 005 ::tcldrop::server::005 -priority 100
	bind die - * ::tcldrop::server::DIE -priority 0
	bind evnt - exit ::tcldrop::server::EVNT_exit -priority 0
	bind evnt - loaded ::tcldrop::server::EVNT_loaded -priority 100000
	checkmodule server::dcc
	return 0
}

proc ::tcldrop::server::EVNT_loaded {event} {
	# Make sure $::servers is a list with no empty elements:
	set servers $::servers
	set ::servers [list]
	foreach s $servers { if {$s != {}} { lappend ::servers $s } }
	# Start trying to connect to IRC:
	if {!${::server-idx}} { jump }
}

proc ::tcldrop::server::UNLD {module} {
	if {![info exists ::restart]} {
		#unbind * * * ::tcldrop::server::*
		quit $module
		#unsetdefault servers
		#unsetdefault default-port
		#unsetdefault server-online
		#unsetdefault server
		#unsetdefault serveraddress
		#unsetdefault server-idx
		#unsetdefault botname
		#unsetdefault botnick
		#unsetdefault server-timeout
		#unsetdefault server-cycle-wait
		#unsetdefault servererror-quit
		#unsetdefault check-stoned
		#unsetdefault max-queue-msg
	}
	unloadmodule server::dcc
	return 1
}
bind unld - server ::tcldrop::server::UNLD -priority 10
