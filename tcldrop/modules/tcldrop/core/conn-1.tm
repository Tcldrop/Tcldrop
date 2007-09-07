# core/conn --
#	Provides:
#		* The connect and control commands, used for all outgoing connections.
#	Depends: core.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
# Or visit http://www.GNU.Org/licenses/gpl.html
#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
# Notes:
# connect address port -buffering line -control procname -proxytype https -proxyhost address -proxytype socks5 -proxyhost address

namespace eval ::tcldrop::core::conn {
	variable version {0.7}
	variable name {core::conn}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {core}
	variable author {Tcldrop-Dev}
	variable description {The connect and control commands, used for all outgoing connections.}
	variable rcsid {$Id$}
	variable commands [list valididx killsock dccused sock2idx idx2sock initidx idxlist listidx setidxinfo getidxinfo idxinfo assignidx registeridx unregisteridx putidx killidx killsock idx2sock sock2idx listen connect control config controlsock addlistentype myip traffic timeout]
	::package require proxy
	::package require proxy::https
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
	# Create ensembles:
	namespace ensemble create -subcommands $commands
	namespace ensemble create -command ::tcldrop::conn -subcommands $commands
	namespace ensemble create -command ::conn -subcommands $commands
}


# Returns the bots IP in long form:
proc ::tcldrop::core::conn::myip {} { if {${::my-ip} != {}} { ip2decimal ${::my-ip} } else { ip2decimal ${::default-ip} } }

# Keeps track of how many bytes go in/out:
proc ::tcldrop::core::conn::traffic {{type {}} {direction {}} {bytes {0}}} {
	variable Traffic
	if {$type != {}} {
		if {[info exists Traffic($type)]} {
			# Set the info array to the current counts.
			array set info $Traffic($type)
		} else {
			# Initialize the counts.
			array set info [list total-in 0 total-out 0 daily-in 0 daily-out 0 restart [clock seconds]]
		}
		# Increase the counters:
		if {$bytes} {
			incr info(total-$direction) $bytes
			incr info(daily-$direction) $bytes
		}
		# See if 24 hours have elapsed, and if it has then clear the daily counts.
		# FixMe: Put this next line in a bind that executes only once a day:
		if {[clock seconds] - $info(restart) > 86400} { array set info [list daily-in 0 daily-out 0 restart [clock seconds]] }
		# Write the new counts back to the Traffic variable:
		set Traffic($type) [array get info]
		if {$direction != {}} {
			# Return either in or out for $type:
			list $info(daily-$direction) $info(total-$direction)
		} else {
			# Return both directions for $type:
			list $info(daily-in) $info(total-in) $info(daily-out) $info(total-out)
		}
	} else {
		# Show them all the traffic stats.
		array set total [list total-out 0 total-in 0 daily-out 0 daily-in 0]
		foreach t [array names Traffic] {
			array set info $Traffic($t)
			lappend out [list $t $info(daily-in) $info(total-in) $info(daily-out) $info(total-out)]
			incr total(total-out) $info(total-out)
			incr total(total-in) $info(total-in)
			incr total(daily-out) $info(daily-out)
			incr total(daily-in) $info(daily-in)
		}
		lappend out [list total $total(daily-in) $total(total-in) $total(daily-out) $total(total-out)]
	}
}

proc ::tcldrop::core::conn::connect {args} { Connect $args }
proc ::tcldrop::core::conn::Connect {opts} {
	# Set default values:
	array set options [list -address {} -port {} -buffering line -blocking 0 -encoding identity -translation auto -myaddr ${::my-ip} -async 1 -fconfigure [list] -connect-timeout ${::connect-timeout} -inactive-timeout ${::inactive-timeout} -ssl 0 -proxychain {} -proxyinfo [list] -socket-command [list socket] traffictype unknown]
	switch -glob -- [lindex $opts 0] {
		{*:*} {
			# Proxychain style: <address:port> or <https://127.0.0.1:8080/address:port>
			array set proxychain [::proxy::splitchain [lindex $opts 0]]
			lappend opts -address $proxychain(address) -port $proxychain(port) -proxychain [lindex $opts 0] -proxyinfo [array get proxychain]
			array set options [lrange $opts 1 end]
		}
		{-*} {
			# key/value style: -address <address> -port <port>
			array set options $opts
		}
		{default} {
			# Eggdrop style: <address> <port>
			lappend opts -address [lindex $opts 0] -port [lindex $opts 1]
			array set options [lrange $opts 2 end]
		}
	}
	# Make sure the address and port were specified:
	if {$options(-address) eq {}} { return -code error "You must specify an address to connect to." } elseif {$options(-port) eq {}} { return -code error "You must specify a port to connect to." }
	# Following mIRC's lead of using +'s in front of the port to mean it's an SSL connection:
	if {[string match {+*} $options(-port)]} {
		set options(-port) [string range $options(-port) 1 end]
		set options(-ssl) 1
	}
	# Note/FixMe: I don't know how SSL connections work, so I don't know what happens when you try to use SSL and proxies together. (I haven't tested it)
	if {$options(-ssl)} { if {![catch { package require tls }]} { set options(-socket-command) [list ::tls::socket] } else { return -code error {SSL not supported, because the "tls" package is not installed.} } }
	# We have to pass $options(-proxychain) to the ::proxy::connect command, so make sure it's set to something useful:
	if {$options(-proxychain) eq {}} { set options(-proxychain) "$options(-address):$options(-port)" }
	# Prepend the $::proxy chain to the chain we already have:
	set options(-proxychain) "$::proxy$options(-proxychain)"
	switch -- [set proxycount [regexp -all {://} $options(-proxychain)]] {
		{0} { set info "$options(-address):$options(-port)" }
		{1} { set info "$options(-address):$options(-port)  (via $proxycount proxy)" }
		{default} { set info "$options(-address):$options(-port)  (via $proxycount proxies)" }
	}
	# Buried in the next line is the call to ::proxy::connect, which BTW can make direct connections as well as proxied connections:
	if {[catch { registeridx [set idx [assignidx]] [concat [array get options] [list idx $idx timestamp [clock seconds] sock [set sock [::proxy::connect $options(-proxychain) -command [list ::tcldrop::core::conn::ProxyControl $idx]]] remote "$options(-address):$options(-port)" info $info]] } error]} {
		return -code error $error
	} else {
		# catch { if {$options(-myaddr) eq {} || $options(-myaddr) eq {0.0.0.0} || ${::default-ip} eq {}} { set ::default-ip [lindex [fconfigure $sock -sockname] 0] } }
		timeout start $idx -timeout [expr { $options(-connect-timeout) * 1000 }] -callback [list ::tcldrop::core::conn::ConnectTimeout $idx]
		return $idx
	}
}

proc ::tcldrop::core::conn::ProxyControl {idx sock status id {reason {}}} {
	global idxlist
	if {[info exists ::idxlist($idx)]} {
		if {$status eq {ok}} {
			array set idxinfo $::idxlist($idx)
			eval [list fconfigure $sock -buffering $idxinfo(-buffering) -blocking $idxinfo(-blocking) -encoding $idxinfo(-encoding) -translation $idxinfo(-translation)] $idxinfo(-fconfigure)
			fileevent $sock writable [list ::tcldrop::core::conn::Write $idx]
			fileevent $sock readable [list ::tcldrop::core::conn::Read $idx]
			foreach {idxinfo(local-ip) idxinfo(local-hostname) idxinfo(local-port)} [fconfigure $sock -sockname] { break }
			foreach {idxinfo(remote-ip) idxinfo(remote-hostname) idxinfo(remote-port)} [fconfigure $sock -peername] { break }
			set idxinfo(port) $idxinfo(local-port)
			setidxinfo $idx [array get idxinfo]
		} else {
			killidx $idx
		}
	} else {
		catch { fileevent $sock writable {} }
		catch { fileevent $sock readable {} }
		catch { fconfigure $sock -buffering full -blocking 0 }
		catch { close $sock }
	}
}

# Takes a previously unknown socket connection and creates an idx for it:
proc ::tcldrop::core::conn::controlsock {sock args} { ControlSock $sock $args }
proc ::tcldrop::core::conn::ControlSock {sock opts} {
	if {![eof $sock]} {
		array set idxinfo [list idx [set idx [assignidx]] sock $sock -buffering line -blocking 0 -connect-timeout ${::connect-timeout} -inactive-timeout ${::inactive-timeout} timestamp [clock seconds] traffictype unknown]
		array set idxinfo $opts
		registeridx $idx [array get idxinfo]
		fconfigure $sock -buffering $idxinfo(-buffering) -blocking $idxinfo(-blocking)
		fileevent $sock writable [list ::tcldrop::core::conn::Write $idx $sock]
		fileevent $sock readable [list ::tcldrop::core::conn::Read $idx $sock]
		timeout start $idx -timeout [expr { $idxinfo(-connect-timeout) * 1000 }] -callback [list ::tcldrop::core::conn::ConnectTimeout $idx]
		return $idx
	} else {
		return -code error "EOF on $sock"
	}
}

# Note that you can tell connect what command to use with the -control option.
proc ::tcldrop::core::conn::control {idx command args} { Control $idx $command $args }
proc ::tcldrop::core::conn::Control {idx command arg} { setidxinfo $idx [concat [list -control $command] $arg] }

proc ::tcldrop::core::conn::Timeout {command id {arg {}}} {
	variable Timeout
	switch -- $command {
		{start} {
			# Start a timeout session for $id
			if {[info exists Timeout($id)]} {
				# $id already exists, so cancel it's afterid..
				array set options $Timeout($id)
				after cancel $options(afterid)
			}
			array set options [list -timeout 0 -callback {}]
			array set options $arg
			if {$options(-timeout) && $options(-callback) != {}} {
				set Timeout($id) [concat [array get options] [list timeout [set timeout [list [namespace current]::Timeout timeout $id]] afterid [after $options(-timeout) $timeout]]]
				return 1
			} else {
				return 0
			}
		}
		{reset} {
			# Reset the timeout to the starting amount..
			if {[info exists Timeout($id)]} {
				array set idinfo $Timeout($id)
				after cancel $idinfo(afterid)
				set idinfo(afterid) [after $idinfo(-timeout) $idinfo(timeout)]
				set Timeout($id) [array get idinfo]
				return 1
			} else {
				# It doesn't already exist, so attempt to start a new timeout session..
				Timeout start $id $arg
			}
		}
		{timeout} {
			# This when the timeout actually occurs.
			if {[info exists Timeout($id)]} {
				array set idinfo $Timeout($id)
				unset Timeout($id)
				# Do the callback as an idle event..
				after idle $idinfo(-callback)
				return 1
			} else {
				return 0
			}
		}
		{cancel} {
			# Cancels a timeout.
			if {[info exists Timeout($id)]} {
				array set idinfo $Timeout($id)
				unset Timeout($id)
				after cancel $idinfo(afterid)
				return 1
			} else {
				return 0
			}
		}
		{default} { return -code error "Unknown subcommand to timeout: $command" }
	}
}
proc ::tcldrop::core::conn::timeout {command id args} { Timeout $command $id $args }

proc ::tcldrop::core::conn::Read {idx {sock {}}} {
	global idxlist
	if {![info exists idxlist($idx)]} {
		putloglev d * "net: error!(read) idx $idx  Invalid IDX"
		catch { close $sock }
	} else {
		array set idxinfo $::idxlist($idx)
		if {[catch { fconfigure $idxinfo(sock) -error } error] || $error != {}} {
			set error "net: error!(read) idx $idx  $error"
		} elseif {[info exists idxinfo(-control)]} {
			# For speed, we process all available lines.  (This is absolutely necessary when running inside an Eggdrop, because Eggdrop's event loops are 1 second apart)
			set trafficlength 0
			while {[info exists idxlist($idx)] && [set length [gets $idxinfo(sock) line]] != -1} {
				if {[catch { $idxinfo(-control) $idx $line } retval]} {
					putlog [set error "Error in $idxinfo(-control): $retval"]
					puterrlog $::errorInfo
					break
				} elseif {[string equal {1} $retval]} {
					# The control proc requested a killidx by returning 1.
					putloglev d * "net: killidx! idx $idx (socket $sock)  (requested by $idxinfo(-control))"
					Timeout cancel $idx
					killidx $idx
					break
				}
				incr trafficlength $length
			}
			traffic $idxinfo(traffictype) in $trafficlength
			if {[eof $idxinfo(sock)]} { set error "net: eof!(read) idx $idx" }
		} else {
			set error "net: control!(read) idx $idx  (no control proc defined!)"
		}
		if {$error != {} && [info exists idxlist($idx)]} {
			putloglev d * $error
			# Tell the errors proc about the error, if they have one defined.
			if {[info exists idxinfo(-errors)]} { if {[set trycontrol [catch { $idxinfo(-errors) $idx $error } error]]} { putloglev e * "Error in $idxinfo(-errors): $error" } } else { set trycontrol 1 }
			killidx $idx
			# Since the errors proc wasn't defined (or failed), try just sending {} to the control proc..
			if {$trycontrol && [info exists idxinfo(-control)] && [catch { $idxinfo(-control) $idx {} } error]} { putloglev e * "Error in $idxinfo(-control): $error" }
			Timeout cancel $idx
		} else {
			# Reset the inactive-timeout timer:
			Timeout reset $idx
		}
	}
}

proc ::tcldrop::core::conn::Write {idx {sock {}}} {
	Timeout cancel $idx
	if {[info exists ::idxlist($idx)]} {
		array set idxinfo $::idxlist($idx)
		# Note, I don't know if it's possible to get an error or EOF from here, but just in case:
		if {[catch { fconfigure $idxinfo(sock) -error } error] || $error != {}} {
			set error "net: error!(write) idx $idx  $error"
		} elseif {[eof $idxinfo(sock)]} {
			set error  "net: error!(write) idx $idx  EOF"
		}
		if {$error != {}} {
			putloglev d * "${error}"
			# Tell the errors proc about the error, if they have one defined.
			if {[info exists idxinfo(-errors)]} { if {[set trycontrol [catch { $idxinfo(-errors) $idx $error } error]]} { putloglev de * "Error in $idxinfo(-errors): $error" } } else { set trycontrol 1 }
			killidx $idx
			# Since the errors proc wasn't defined (or failed), try just sending {} to the control proc..
			# FixMe: Find out if Eggdrop calls the control proc with "" even though the connection was never established.
			#if {$trycontrol && [info exists idxinfo(-control)] && [catch { $idxinfo(-control) $idx {} } error]} { putloglev d * "Error in $idxinfo(-control): $error" }
		} elseif {[info exists idxinfo(-writable)]} {
			catch { fileevent $idxinfo(sock) writable {} }
			$idxinfo(-writable) $idx
		}
		# Start the inactive-timeout timer:
		if {$error eq {}} { timeout start $idx -timeout [expr { $idxinfo(-inactive-timeout) * 1000 }] -callback [list ::tcldrop::core::conn::InactiveTimeout $idx] }
	} else {
		catch { close $sock }
	}
}

proc ::tcldrop::core::conn::ConnectTimeout {idx} {
	if {[info exists ::idxlist($idx)]} {
		array set idxinfo $::idxlist($idx)
		putloglev d * [set error "net: timeout!(connect) idx $idx"]
		# Tell the errors proc about the error, if they have one defined.
		if {[info exists idxinfo(-errors)]} { if {[set trycontrol [catch { $idxinfo(-errors) $idx $error } error]]} { putloglev de * "Error in $idxinfo(-errors): $error" } } else { set trycontrol 1 }
		killidx $idx
		# Since the errors proc wasn't defined (or failed), try just sending {} to the control proc..
		if {$trycontrol && [info exists idxinfo(-control)] && [catch { $idxinfo(-control) $idx {} } error]} { putloglev de * "Error in $idxinfo(-control): $error" }
	}
}

proc ::tcldrop::core::conn::InactiveTimeout {idx} {
	if {[info exists ::idxlist($idx)]} {
		putloglev d * [set error "net: timeout!(inactive) idx $idx"]
		array set idxinfo $::idxlist($idx)
		# Tell the errors proc about the error, if they have one defined.
		if {[info exists idxinfo(-errors)]} { if {[set trycontrol [catch { $idxinfo(-errors) $idx $error } error]]} { putloglev de * "Error in $idxinfo(-errors): $error" } } else { set trycontrol 1 }
		killidx $idx
		# Since the errors proc wasn't defined (or failed), try just sending {} to the control proc..
		if {$trycontrol && [info exists idxinfo(-control)] && [catch { $idxinfo(-control) $idx {} } error]} { putloglev de * "Error in $idxinfo(-control): $error" }
	}
}

#  listen <port> <type> [options] [flag]
#    Description: opens a listening port to accept incoming telnets; type
#      must be one of "bots", "all", "users", "script", or "off":

#        listen <port> bots [mask]
#          Description: accepts connections from bots only; the optional mask
#            is used to identify permitted bot names. If the mask begins with
#            '@', it is interpreted to be a mask of permitted hosts to accept
#            connections from.
#          Returns: port number

#        listen <port> users [mask]
#          Description: accepts connections from users only (no bots); the
#            optional mask is used to identify permitted nicknames. If the
#            mask begins with '@', it is interpreted to be a mask of permitted
#            hosts to accept connections from.
#          Returns: port number

#        listen <port> all [mask]
#          Description: accepts connections from anyone; the optional mask
#            is used to identify permitted nicknames/botnames. If the mask
#            begins with '@', it is interpreted to be a mask of permitted
#            hosts to accept connections from.
#          Returns: port number

#        listen <port> script <proc> [flag]
#          Description: accepts connections which are immediately routed to
#            a proc. The proc is called with one parameter: the idx of the
#            new connection. Flag may currently only be 'pub', which makes
#            the bot allow anyone to connect.
#          Returns: port number

#        listen <port> off
#          Description: stop listening on a port
#          Returns: nothing
#    Module: core

# Note/FixMe: Tcldrop does not support flag or mask in the above.
proc ::tcldrop::core::conn::listen {port type args} {
	# Following mIRC's lead of using +'s in front of the port to mean it's an SSL connection.
	if {[string match {+*} $port]} {
		set port [string range $port 1 end]
		set ssl 1
	} else {
		set ssl 0
	}
	global idxlist
	# First make sure we're not already listening on $port:
	foreach i [array names idxlist] {
		array set idxinfo $idxlist($i)
		if {[info exists idxinfo(local-port)] && [string equal $port $idxinfo(local-port)]} {
			if {[string equal -nocase {off} $type]} {
				catch { close $idxinfo(sock) }
			} else {
				putlog "already listening on port $port"
			}
			return $port
		}
		unset idxinfo
	}
	switch -- [set type [string tolower $type]] {
		{all} { return -code error {Tcldrop doesn't support "listen <port> all"; please use a separate port for bots and users.} }
		{script} { array set options [concat [list traffictype unknown type $type ident 0 dns 0 ssl $ssl connect [lindex $args 0]] [lrange $args 1 end]] }
		{off} { return 0 }
		{default} {
			# Support for types that modules may provide goes here.
			variable ListenTypes
			if {[info exists ListenTypes($type)]} {
				if {${::my-ip} != {}} { set myaddr ${::my-ip} } else { set myaddr ${::default-ip} }
				array set options [concat [list traffictype unknown type $type ident 0 dns 0 ssl $ssl connect {} myaddr $myaddr] $ListenTypes($type) $args]
			} else {
				return -code error "No such listen type: $type"
			}
		}
	}
	if {$options(ssl)} {
		if {![catch { package require tls }]} {
			set socket {::tls::socket}
		} else {
			return -code error {SSL not supported, because the "tls" package is not installed.}
		}
	} else {
		set socket {socket}
	}
	switch -- $myaddr {
		{} - {*} - {0.0.0.0} - {-} - {INADDR_ANY} - {ANY} - {ALL} {
			set fail [catch { $socket -server [list ::tcldrop::core::conn::ListenConnect [array get options]] $port } sock]
			set myaddr {0.0.0.0}
		}
		{default} { set fail [catch { $socket -server [list ::tcldrop::core::conn::ListenConnect [array get options]] -myaddr $myaddr $port } sock] }
	}
	if {!$fail} {
		fconfigure $sock -buffering line -blocking 0
		if {$myaddr eq {0.0.0.0}} { set info "Listening on *:$port" } else { set info "Listening on ${myaddr}:$port" }
		foreach {local-ip local-hostname local-port} [fconfigure $sock -sockname] { break }
		registeridx [set idx [assignidx]] [list idx $idx sock $sock module $type handle ($type) remote $myaddr hostname $myaddr local-ip ${local-ip} local-hostname ${local-hostname} local-port ${local-port} port $port myaddr $myaddr type $type other "lstn  $port" timestamp [clock seconds] info $info]
		putlog "Listening at ${myaddr}:$port  ($type)"
		return $port
	} else {
		return -code error "error while trying to listen on ${myaddr}:$port: $sock"
	}
}

# Adds new listen types (so that the [listen] command can know what to do with them):
# $args should contain the following:
# ident 1/0
# connect <procname>
proc ::tcldrop::core::conn::addlistentype {type args} {
	variable ListenTypes
	set ListenTypes([string tolower $type]) $args
}

# This is a callback to the ::ident::ident command,
# it simply updates the idx info to show the ident responce.
proc ::tcldrop::core::conn::Ident {idx options id status response} {
	if {[info exists ::idxlist($idx)]} {
		if {$status eq {ok}} {
			setidxinfo $idx [list ident $response]
		} else {
			setidxinfo $idx [list ident -telnet]
		}
	}
}

# This is the callback to the dnslookup command,
# it simply updates the idx info to show the remotes hostname.
proc ::tcldrop::core::conn::DNSLookup {ip hostname status idx options} {
	if {$status && [info exists ::idxlist($idx)]} { setidxinfo $idx [list hostname $hostname remote-hostname $hostname] }
}

proc ::tcldrop::core::conn::ListenConnect {options sock ip port} {
	fconfigure $sock -blocking 0 -buffering line
	if {[dccused] >= ${::max-dcc}} {
		putloglev d * "net: error!(connect) too many dcc's in use ([dccused]).  Try raising max-dcc in the config."
		catch {
			fileevent $sock writable {}
			fileevent $sock readable {}
			close $sock
		}
		return
	}
	foreach {idxinfo(local-ip) idxinfo(local-hostname) idxinfo(local-port)} [fconfigure $sock -sockname] { break }
	foreach {idxinfo(remote-ip) idxinfo(remote-hostname) idxinfo(remote-port)} [fconfigure $sock -peername] { break }
	array set optinfo $options
	registeridx [set idx [assignidx]] [concat [array get idxinfo] [list -inactive-timeout ${::inactive-timeout} idx $idx sock $sock handle * ident {-telnet} hostname [set hostname [lindex [fconfigure $sock -peername] 1]] ip $ip remote -telnet@$hostname port $port type $optinfo(type) ssl $optinfo(ssl) other "$optinfo(type)-in" timestamp [unixtime] traffictype misc]]
	putloglev d * "net: connect! idx $idx (socket $sock)"
	if {[testip $idxinfo(remote-hostname)]} {
		switch -- $optinfo(dns) {
			{1} - {2} - {default} {
				after idle [list dnslookup $ip ::tcldrop::core::conn::DNSLookup $idx [array get optinfo]]
			}
		}
	} else {
		setidxinfo $idx [list hostname $hostname]
	}
	switch -- $optinfo(ident) {
		{1} - {2} - {default} {
			afteridle [list ::ident::ident -sock $sock -timeout [expr { ${::ident-timeout} * 1001 }] -command [list ::tcldrop::core::conn::Ident $idx [array get optinfo]]]
		}
	}
	if {$optinfo(connect) != {}} { $optinfo(connect) $idx }
	fileevent $sock writable [list ::tcldrop::core::conn::Write $idx $sock]
	fileevent $sock readable [list ::tcldrop::core::conn::Read $idx $sock]
}

# Sends $text to $idx:
# Note: This is an Eggdrop v1.6 and less command.
proc ::tcldrop::core::conn::putidx {idx text {opts {}}} {
	if {[info exists ::idxlist($idx)]} {
		array set idxinfo $::idxlist($idx)
		# The filter can return the new $text to send, or if it returns an error code we'll assume the filter sent the text itself.
		if {[info exists idxinfo(filter)] && [set retval [catch { $idxinfo(filter) $idx $text $opts } text]]} { return $retval }
		array set options [list -nonewline 0 -flush 1]
		array set options $opts
		if {$options(-nonewline) && [info exists idxinfo(nonewline)] && $idxinfo(nonewline)} {
			# If -nonewline 1 is specified in $args, and the idx supports using nonewline (eg. DCC CHAT's do NOT support it) then we send it without a newline:
			if {[catch { puts -nonewline $idxinfo(sock) $text }]} {
				# CheckMe/FixMe: How soon should the idx be killed?  Immediately, or after idle, or after idle + after 0?
				after idle [list after 0 [list killidx $idx]]
				return 0
			}
		} else {
			if {[catch { puts $idxinfo(sock) $text }]} {
				# CheckMe/FixMe: How soon should the idx be killed?  Immediately, or after idle, or after idle + after 0?
				after idle [list after 0 [list killidx $idx]]
				return 0
			}
		}
		# -flush 1 (the default) means we flush the channel:
		if {$options(-flush) && [catch { flush $idxinfo(sock) }]} { return 0 }
		timeout reset $idx
		traffic $idxinfo(traffictype) out [string length $text]
		return 1
	}
	return 0
}

# Closes any sockets that the idx is associated with, and unsets all the info known about it:
# Note: This is a Tcldrop specific command.
proc ::tcldrop::core::conn::killidx {idx args} {
	if {[info exists ::idxlist($idx)]} {
		array set idxinfo $::idxlist($idx)
		catch { fileevent $idxinfo(sock) writable {} }
		catch { fileevent $idxinfo(sock) readable {} }
		#catch { flush $idxinfo(sock) }
		unset -nocomplain ::idxlist($idx)
		array set options [list -now 0]
		array set options $args
		if {$options(-now)} {
			catch { close $idxinfo(sock) }
		} else {
			if {[info exists idxinfo(sock)]} { after idle [list after 0 [list catch [list close $idxinfo(sock)]]] }
		}
		return 1
	}
	return 0
}

proc ::tcldrop::core::conn::sock2idx {sock} { lindex [listidx [list sock $sock]] 0 }

proc ::tcldrop::core::conn::killsock {sock} { killidx [lindex [listidx [list sock $sock]] 0] }

proc ::tcldrop::core::conn::idx2sock {idx} { idxinfo $idx sock }

# Assigns a unique idx handle for a connection:
proc ::tcldrop::core::conn::assignidx {args} { variable IDXCount
	if {$IDXCount > ${::max-dcc}} { set IDXCount 0 }
	while {[info exists ::idxlist([incr IDXCount])]} {}
	return $IDXCount
}

# Registers an idx.
# info is a key/value pair list.
# Required keys are: type sock idx hostname port timestamp handle other
# See info about the dcclist command in eggdrop/doc/tcl-commands.doc,
# because it's dcclist that requires most of these keys.
proc ::tcldrop::core::conn::registeridx {idx {info {}}} { set ::idxlist($idx) $info }

# Unregisters an idx:
proc ::tcldrop::core::conn::unregisteridx {idx} { array unset ::idxlist $idx }

# Asks for a new idx to be assigned, sets it's info to $info, and returns the assigned idx:
# info is a key/value pair list.
# Required keys are: type sock idx hostname port timestamp handle other
# See info about the dcclist command in eggdrop/doc/tcl-commands.doc,
# because it's dcclist that requires most of these keys.
# Note: This does the job of both assignidx and registeridx.
proc ::tcldrop::core::conn::initidx {{info {}}} {
	variable IDXCount
	set ::idxlist([set count [incr IDXCount]]) $info
	return $count
}

#  idxlist ?info?
#    Returns: a key/value pair list of active local connections:
#        {<idx>} {<info>}
#
#      The types are: chat, bot, files, file_receiving, file_sending,
#      file_send_pending, script, socket (these are connections that have
#      not yet been put under 'control'), telnet, and server. The timestamp
#      is in unixtime format.
#    Module: core
# Example: idxlist {type CHAT}
# Would return all the idx's and their info matching type CHAT.
proc ::tcldrop::core::conn::idxlist {{info {}}} {
	global idxlist
	switch -- $info {
		{} { array get idxlist }
		{default} {
			lappend list
			foreach idx [array names idxlist] {
				array set idxinfo $idxlist($idx)
				set add 1
				foreach {i p} $info {
					if {![info exists idxinfo($i)] || ![string match -nocase $p $idxinfo($i)]} {
						set add 0
						break
					}
				}
				if {$add} { lappend list $idx $idxlist($idx) }
				array unset idxinfo
			}
			return $list
		}
	}
}

# Works just like idxlist, but this just returns the idx's (without their info).
proc ::tcldrop::core::conn::listidx {{info {}}} {
	global idxlist
	switch -- $info {
		{} { array get idxlist }
		{default} {
			lappend list
			foreach idx [array names idxlist] {
				array set idxinfo $idxlist($idx)
				set add 1
				foreach {i p} $info {
					if {![info exists idxinfo($i)] || ![string match -nocase $p $idxinfo($i)]} {
						set add 0
						break
					}
				}
				if {$add} { lappend list $idx }
				array unset idxinfo
			}
			return $list
		}
	}
}

proc ::tcldrop::core::conn::dccused {} { array size ::idxlist }

# Returns or sets one piece of info about $idx:
# Or returns all info about the idx.
# Note: Using setidxinfo and getidxinfo are prefered over using this command.
proc ::tcldrop::core::conn::idxinfo {idx {info {}} {value {}}} {
	if {[info exists ::idxlist($idx)]} {
		switch -- $info {
			{} { return $::idxlist($idx) }
			{default} {
				array set idxinfo $::idxlist($idx)
				if {$value != {}} {
					set idxinfo($info) $value
					set ::idxlist($idx) [array get idxinfo]
					return $value
				} elseif {[info exists idxinfo($info)]} {
					return $idxinfo($info)
				} else {
					return -code error "no such type: $info"
				}
			}
		}
	} else {
		return -code error "invalid idx: $idx"
	}
}

# Adds to or replaces info about an existing idx:
proc ::tcldrop::core::conn::setidxinfo {idx {info {}}} {
	if {[info exists ::idxlist($idx)]} { array set idxinfo $::idxlist($idx) }
	array set idxinfo $info
	set ::idxlist($idx) [array get idxinfo]
}

# Companion to setidxinfo:
proc ::tcldrop::core::conn::getidxinfo {idx} { if {[info exists ::idxlist($idx)]} { set ::idxlist($idx) } }

# Deletes a piece of info about the idx (to save a tad of memory):
proc ::tcldrop::core::conn::delidxinfo {idx info} {
	if {[info exists ::idxlist($idx)]} {
		array set idxinfo $::idxlist($idx)
		if {[info exists $idxinfo($info)]} {
			unset idxinfo($info)
			set ::idxlist($idx) [array get idxinfo]
			return 1
		} else {
			return 0
		}
	} else {
		return 0
	}
}

# Returns 1 if $idx is a valid idx, or 0 if it's not:
# Note: This is an Eggdrop v1.6 and less command.
proc ::tcldrop::core::conn::valididx {idx} { info exists ::idxlist($idx) }

# This should stay as a LOAD bind:
bind load - core::conn ::tcldrop::core::conn::LOAD -priority 0
proc ::tcldrop::core::conn::LOAD {module} {
	setdefault my-hostname {}
	setdefault my-ip {}
	setdefault ident-timeout 13
	setdefault connect-timeout 237
	setdefault inactive-timeout 9999
	setdefault resolve-timeout 15
	if {[setdefault max-dcc 99] > 999} { set ::max-dcc 999 }
	setdefault proxy {} -protect 1
	variable Traffic
	array set Traffic {}
	array set ::idxlist {}
	# Protect the idxlist global from being deleted during restarts:
	if {[lsearch -exact $::protected(globals) {idxlist}] == -1} { lappend ::protected(globals) {idxlist} }
	variable IDXCount
	if {![info exists IDXCount]} { variable IDXCount 1 }
	package require ident
}

bind evnt - loaded ::tcldrop::core::conn::EVNT_loaded -priority 100000
proc ::tcldrop::core::conn::EVNT_loaded {event} { if {[setdefault max-dcc 99] > 998} { set ::max-dcc 997 } }

bind evnt - prerestart ::tcldrop::core::conn::EVNT_prerestart -priority 100000
proc ::tcldrop::core::conn::EVNT_prerestart {event} {
	global protected
	counter start exemptidx
	counter start successsock
	counter start fail
	counter start exempt
	set status {}
	foreach c [file channels] {
		if {[lsearch -exact $protected(filechannels) $c] == -1} {
			if {[sock2idx $c] != {}} {
				counter incr exemptidx
			} elseif {![catch { close $c }]} {
				counter incr successsock
			} else {
				counter incr fail
			}
		} else {
			counter incr exempt
		}
	}
	if {[set count [counter end successsock]]} { lappend status "$count Files/Sockets Closed" }
	if {[set count [counter end exemptidx]]} { lappend status "$count IDXs Exempt" }
	if {[set count [counter end exempt]]} { lappend status "$count Sockets Exempt" }
	if {[set count [counter end fail]]} { lappend status "$count Failed Close" }
	if {[llength $status]} { putlog "Files/IDXs/Sockets: [join $status {, }]." }
}

# Don't allow the core::conn module to be unloaded:
bind unld - core::conn ::tcldrop::core::conn::UNLD -priority 0
proc ::tcldrop::core::conn::UNLD {module} {
	package forget ident
	return 1
}
