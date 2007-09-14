# bots/oldbotnet --
#	Handles:
#		* Provides oldbotnet (Eggdrop v1.4) botnet support (it's compatible with Eggdrop v1.6).
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
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.
#
#	bots::oldbotnet module for Tcldrop.  (optional, but loaded by default for Eggdrop compatibility reasons)
#
# This module provides the ability to talk to old eggdrop bots (1.6 and less).
# It does NOT support Eggdrop 1.7 (1.9?) and higher bots.
# Not even if they have their oldbotnet module loaded!
#
# The reason is because the eggdrop botnet protocol changed in the 1.6 series (I think)
# and they started using shorter names for the bot commands and
# also used a non-standard base64 encoding on some things.
# Although eggdrop 1.6 is still able to talk to bots using the old (original) botnet protocol.
#
# This module only provides support for the ORIGINAL eggdrop botnet protocol.

namespace eval ::tcldrop::bots::oldbotnet {
	variable name {bots::oldbotnet}
	variable version {0.1}
	variable predepends {bots}
	variable depends {bots core::conn core::users core}
	variable author {Tcldrop-Dev}
	variable description {oldbotnet support.}
	variable rcsid {$Id$}
	namespace export putoldbotnet
	variable commands [namespace export]
	namespace unknown unknown
	namespace path [list ::tcldrop::bots ::tcldrop]
	variable script [info script]
	set ::modules($name) [list name $name version $version depends $depends author $author description $description rcsid $rcsid commands $commands script $script namespace [namespace current]]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	if {![info exists ::tcldrop]} { return }
}

# This is for INCOMING bot connections only:
proc ::tcldrop::bots::oldbotnet::Connect {idx} {
	putloglev d * "in oldbotnet::Connect $idx"
	setidxinfo $idx [list -control ::tcldrop::bots::oldbotnet::Read -writable ::tcldrop::bots::oldbotnet::Write_IN module oldbotnet state BOT_IN other {b-in  } timestamp [unixtime] traffictype botnet]
}

# This is for INCOMING bot connections only:
proc ::tcldrop::bots::oldbotnet::Write_IN {idx {line {}}} {
	putloglev d * "in oldbotnet::Write_IN $idx $line"
	putidx $idx {Nickname.}
	setidxinfo $idx [list state BOT_ID other {b-id  } traffictype {botnet} timestamp [clock seconds] direction {in} handlen 9 numversion 0]
}

proc ::tcldrop::bots::oldbotnet::Errors {idx error} {
	puterrlog "Got error on ${idx}: $error"
	# FixMe: This should handle all errors.
}

# This is for OUTGOING bot connections only:
proc ::tcldrop::bots::oldbotnet::Write_OUT {idx} {
	putlog "Got Write"
	setidxinfo $idx [list state BOT_NEW other {botnew} traffictype {botnet} timestamp [clock seconds] direction {out} handlen 9 numversion 0]
}

# Tcldrop> linking TO Eggdrop< (negotiates new password):
# < Nickname.
# > Tcltest
# < *hello!
# < version 1061603 9 eggdrop v1.6.16+hpux <EFNet>
# < handshake j03pfnf918
# > version 1021603 9 tcldrop v0.5.1 <EFNet>
# < thisbot Atlantica
# < nlinked SafeTcl Atlantica +1061603
# < nlinked Flounder Atlantica +1061403
# < join Atlantica FireEgl 0 *15 Proteus759@adsl-17-135-6.bhm.bellsouth.net
# < idle Atlantica 15 26
# < zapf Atlantica Tcltest assoc A Atlantica Botnet
# < el

# Tcldrop> linking TO Eggdrop< (password already set)
# < Nickname.
# > Tcltest
# < passreq <13f0409eb186@Atlantica>
# > j03pfnf918
# < *hello!
# < version 1061603 9 eggdrop v1.6.16+hpux <EFNet>
# > version 1021603 9 tcldrop v0.5.1 <EFNet>
# < thisbot Atlantica
# < nlinked SafeTcl Atlantica +1061603
# < nlinked Flounder Atlantica +1061403
# < join Atlantica FireEgl 0 *15 Proteus759@adsl-17-135-6.bhm.bellsouth.net
# < idle Atlantica 15 26
# < zapf Atlantica Tcltest assoc A Atlantica Botnet
# < el

# Eggdrop> linking TO Tcldrop< (password already set)
# < Nickname.
# > Atlantica
# < passreq
# > woot123
# < *hello!
# > version 1061603 9 eggdrop v1.6.16+hpux <EFNet>
# < version 1021603 9 tcldrop v0.5.1 <EFNet>
# > thisbot Atlantica
# > nlinked Flounder Atlantica +1061403
# > nlinked SafeTcl Atlantica +1061603
# > nlinked Flounder Atlantica +1061403
# > join Atlantica FireEgl 0 *15 Proteus759@adsl-17-135-6.bhm.bellsouth.net
# > idle Atlantica 15 26
# > zapf Atlantica Tcltest assoc A Atlantica Botnet
# > el

proc ::tcldrop::bots::oldbotnet::Read {idx line} {
	putloglev d * "in oldbotnet::Read $idx $line"
	array set idxinfo $::idxlist($idx)
	putloglev d * "idxinfo(state): $idxinfo(state)"
	switch -- $idxinfo(state) {
		{BOT_IN} { Write_IN $idx $line }
		{BOT_ID} {
			# This is for INCOMING bot connections only; $line should contain the remotes response to our "Nickname." prompt.
			if {[matchattr $line b]} {
				if {![passwdok $line -]} {
					# They have a password set, ask them what it is:
					putidx $idx {passreq}
					setidxinfo $idx [list handle $line state CHAT_PASS other {pass}]
				} else {
					# They don't have a password set.. Accept the line.
					setidxinfo $idx [list handle $line state BOT other {bot}]
					after idle [list ::tcldrop::bots::oldbotnet::LinkedPeer $idx]
				}
			} else {
				# Note: Eggdrop sends the following message, but I don't think we should send anything at all.
				#putidx $idx {This telnet port is for bots only.}
				# If they're not a bot, drop them immediately by returning 1:
				return 1
			}
		}
		{CHAT_PASS} {
			# This is for INCOMING bot connections only; $line should contain the correct password.
			if {[string equal [getuser $idxinfo(handle) PASS] $line] || [passwdok $idxinfo(handle) $line]} {
				# Password matches.. Accept the link.
				setidxinfo $idx [list state BOT other {bot}]
				after idle [list ::tcldrop::bots::oldbotnet::LinkedPeer $idx]
			} else {
				putidx $idx {badpass}
				return 1
			}
		}
		{BOT_NEW} {
			# This is for OUTGOING bot connections only; We wait for $line to be a standard "Nickname." prompt, and then we send our ${::botnet-nick}
			switch -- $line {
				{Nickname.} - {Handle.} - {Nickname:} - {Handle:} - {Login:} - {Login.} - {login:} {
					setidxinfo $idx [list state BOT_PASS other {bot_pass}]
					putidx $idx ${::botnet-nick}
				}
				{default} {
					# If they didn't send "Nickname." then they're showing their banner (which comes before the "Nickname." prompt).  So we just ignore it.
					# FixMe: We need a login-timeout.. So that somebody can't tie up a connection forever without being logged in.
				}
			}
		}
		{BOT_PASS} {
			# This is for OUTGOING bot connections only; This is when the remote bot is asking what our password is "passreq ",
			#                                            or if no password is set on the other end, they send "*hello!".
			if {[string equal {*hello!} $line]} {
				# The other end will send "*hello!" when they've accepted the link.
				setidxinfo $idx [list state BOT_HELLO other {bot_hello}]
			} elseif {[string match {passreq*} $line]} {
				if {[set pass [getuser $idxinfo(handle) PASS]] != {}} {
					# They're asking for our password, so send it:
					putidx $idx $pass
				} else {
					# They're asking for our password, but we don't know it! Abort!
					putlog "$idxinfo(handle) is asking for a password, but I don't know it!  Closing IDX $idx ..."
					return 1
				}
			} else {
				putlog "$idxinfo(handle) sent junk during link.  Closing IDX $idx ..."
				return 1
			}
		}
		{BOT_HELLO} {
			# This is for OUTGOING bot connections only; after we've sent our handle and password to the remote bot.
			# Note: They must send the version information immediately after they send "*hello!"..
			# Note: Sorta hackish, we call the Version bind manually here:
			if {[string equal [Version $idxinfo(handle) $idx [string trim [lindex [split $line] 0]] [string trimleft [join [lrange [split $line] 1 end]]]] {1}]} {
				# If Version returned 1, it means the other end send unacceptable version info, so we kill the connection by returning 1 here:
				putlog "$idxinfo(handle) sent unacceptable version information.  Closing IDX $idx ..."
				return 1
			} else {
				# Accept the link:
				setidxinfo $idx [list state BOT other {bot}]
				after idle [list ::tcldrop::bots::oldbotnet::LinkedPeer $idx]
			}
		}
		{BOT} {
			calloldbotnet $idxinfo(handle) $idx [string trim [lindex [split $line] 0]] [string trimleft [join [lrange [split $line] 1 end]]]
			putloglev t * $line
			setidxinfo $idx [list timestamp [clock seconds]]
		}
		{default} {
			puterrlog "Unknown state in ::tcldrop::bots::oldbotnet::Read: $idxinfo(state)"
			return 1
		}
	}
	return 0
}

proc ::tcldrop::bots::oldbotnet::LinkedPeer {idx args} {
	array set idxinfo $::idxlist($idx)
	# We send 1029899 instead of $::numversion because we need to make it use the old (original) botnet protocol with us:
	putidx $idx "*hello!\nversion 1029899 $idxinfo(handlen) tcldrop $::tcldrop(version) <${::network}>"
	if {$idxinfo(direction) == {in} && [passwdok $idxinfo(handle) -]} {
		# Tell them the password to use for future connections:
		putidx $idx "handshake [set randpass [::tcldrop::randstring 15]]"
		setuser $idxinfo(handle) PASS $randpass -type null
	}
	putidx $idx "thisbot ${::botnet-nick}"

	variable Bots
	if {[info exists Bots([set lowerbot [string tolower $idxinfo(handle)]])]} { array set botinfo $Bots($lowerbot) }
	if {[info exists botinfo(numversion)]} { set numversion $botinfo(numversion) } else { set numversion $botinfo(numversion) }
	array set botinfo [list handle $botinfo(handle) numversion $numversion icon - uplink ${::botnet-nick} peeridx $idx peer $botinfo(handle) tracepath [list ${::botnet-nick}]]
	set Bots($lowerbot) [array get botinfo]
	variable Linked
	set Linked($lowerbot) [list $botinfo(handle)]
	variable Peers
	set Peers($idx) $botinfo(handle)
	registerbot $botinfo(handle) [list handle $botinfo(handle) id $lowerhandle type oldbotnet]
	calllink $botinfo(handle) ${::botnet-nick}

	# join !botname user chan# (icon)sock user@host
	# FixMe: This is the party.tcl modules job of sending the join notice to other bots.
	#foreach {i d} [idxlist {state CHAT}] {
	#	array set idxinfo $d
	#	putidx $idx "join ${::botnet-nick} $idxinfo(handle) $idxinfo(console-chan) ?$i $idxinfo(remote)"
	#}

	putidx $idx {el}
}

proc ::tcldrop::bots::oldbotnet::calloldbotnet {handle idx cmd arg} {
	# retval will be the number of binds that were triggered..
	set retval 0
	foreach {type flags mask proc} [bindlist oldbotnet $cmd] {
		if {[string equal -nocase $cmd $mask] && [matchattr $handle $flags]} {
			if {[catch { $proc $handle $idx $cmd $arg } err]} {
				putlog "error in script: $proc: $err"
			} elseif {[string equal $err {1}]} {
				# Abort processing further binds if they return 1.
				break
			}
			incr retval
			countbind $type $mask $proc
		}
	}
	if {!$retval} { putdebuglog "calloldbotnet: $handle $idx $cmd $arg" }
	set retval
}

proc ::tcldrop::bots::oldbotnet::islinked {bot} {
	variable Bots
	info exists Bots([string tolower $bot])
}

proc ::tcldrop::bots::oldbotnet::bots {{mask {*}}} {
	variable Bots
	array names Bots [string tolower $mask]
}

proc ::tcldrop::bots::oldbotnet::botlist {{mask {*}}} {
	variable Bots
	lappend botlist
	foreach bot [array names Bots [string tolower $mask]] {
		array set botinfo $Bots($bot)
		lappend botlist [list $botinfo(handle) $botinfo(uplink) $botinfo(numversion) $botinfo(icon)]
	}
	return $botlist
}

proc ::tcldrop::bots::oldbotnet::putbot {bot message} {
	variable Bots
	if {[info exists Bots([set bot [string tolower $bot]])]} {
		array set botinfo $Bots($bot)
		putidx $botinfo(peeridx) "zapf ${::botnet-nick} $botinfo(handle) $message"
		return 1
	}
	return 0
}

proc ::tcldrop::bots::oldbotnet::putallbots {message} {
	variable Peers
	foreach i [array names Peers] { putidx $i "zapf-broad ${::botnet-nick} $message" }
	info exists i
}

proc ::tcldrop::bots::oldbotnet::putoldbotnet {mask line} {
	variable Peers
	if {[string equal [string index $mask] {!}]} {
		if {[info exists Peers([set mask [string range $mask 1 end]])]} {
			foreach i [array names Peers] { if {$i != $mask} { putidx $i $line } }
		} else {
			foreach i [array names Peers] { if {![string equal -nocase $Peers($i) $mask]} { putidx $i $line } }
		}
	} else {
		if {[info exists Peers($mask)]} {
			putidx $mask $line
		} elseif {$mask == {*}} {
			foreach i [array names Peers] { putidx $i $line }
		} else {
			return -code error "Invalid idx: $mask"
		}
	}
}

proc ::tcldrop::bots::oldbotnet::link {viabot {bot {}}} {
	if {$bot == {}} {
		set bot $viabot
		set viabot {}
		if {![catch { connect [set host [lindex [set botaddr [getuser $bot BOTADDR]] 0]] [set port [lindex $botaddr 1]] -connecttimeout ${::connect-timeout} -inactivetimeout ${::inactive-timeout} -myaddr ${::my-ip} -control ::tcldrop::bots::oldbotnet::Read -writable ::tcldrop::bots::oldbotnet::Write_OUT -errors ::tcldrop::bots::oldbotnet::Errors } idx]} {
			setidxinfo $idx [list handle $bot remote $host hostname $host port $port state FORK_BOT other {conn  bot} timestamp [set timestamp [unixtime]] traffictype botnet module oldbotnet]
			#set TimerID [utimer 99 [list ::tcldrop::bots::oldbotnet::BOTConnectTimeout $idx]]
			return 1
		} else {
			return 0
		}
	} else {
		# FixMe: Tell $viabot to link to $bot.
	}
	return 0
}

proc ::tcldrop::bots::oldbotnet::unlink {bot} {
	variable Bots
	if {[info exists Bots([set lowerbot [string tolower $bot]])]} {
		array set botinfo $Bots($lowerbot)
		variable Peers
		if {[info exists Peers($botinfo(peeridx))]} {
			# We're directly connected to $bot.
			killidx $botinfo(peeridx)
			calldisc $bot
			unset Peers($botinfo(peeridx))
		} else {
			# We're not directly connected.
			# FixMe: Make it request the remote bot to unlink $bot.
		}
		return 1
	}
	return 0
}

bind oldbotnet b handshake ::tcldrop::bots::oldbotnet::Handshake -priority 1000
proc ::tcldrop::bots::oldbotnet::Handshake {handle idx cmd arg} {
	setuser $handle PASS [lindex [split $arg] end] -type null
}

bind oldbotnet b version ::tcldrop::bots::oldbotnet::Version -priority -1000
proc ::tcldrop::bots::oldbotnet::Version {handle idx cmd arg} {
	if {([string equal $cmd {version}] || [string equal $cmd {v}]) && [string is integer -strict [set handlen [lindex [set sline [split $arg]] 1]]]} {
		foreach {numversion handlen bottype version network} $sline { break }
		setidxinfo $idx [set version [list numversion $numversion handlen $handlen bottype $bottype version $version network $network]]
		variable Bots
		if {[info exists Bots([set lowerbot [string tolower $handle]])]} { array set botinfo $Bots($lowerbot) }
		array set botinfo $version
		set Bots($lowerbot) [array get botinfo]
		return 0
	} else {
		# Return 1 if we don't like what their version info has in it..
		return 1
	}
}

bind oldbotnet b thisbot ::tcldrop::bots::oldbotnet::Thisbot -priority 1000
bind oldbotnet b tb ::tcldrop::bots::oldbotnet::Thisbot -priority 1000
proc ::tcldrop::bots::oldbotnet::Thisbot {handle idx cmd arg} {
	variable Peers
	if {![info exists Peers($idx)]} {
		# Previously unknown bot!  o_O
		killidx $idx
		return 1
	} elseif {![string equal -nocase $Peers($idx) $handle]} {
		# Imposter?
		killidx $idx
		return 1
	} elseif {![string equal $Peers($idx) $handle]} {
		# It's the right bot, just need to update some variables to get the cAsE right..
		set Peers($idx) $arg
		variable Bots
		if {[info exists Bots([set handle [string tolower $handle]])]} {
			array set botinfo $Bots($handle)
			array set botinfo [list handle $arg peer $arg]
			set Bots($handle) [array get botinfo]
		}
		# FixMe: Also update the Linked variable.
		setidxinfo $idx [list handle $arg]
		return 0
	}
}

# zapf Yum Tcldrop mycmd testing 1 2 3 4 5 6
bind oldbotnet b zapf ::tcldrop::bots::oldbotnet::Zapf -priority 1000
bind oldbotnet b z ::tcldrop::bots::oldbotnet::Zapf -priority 1000
proc ::tcldrop::bots::oldbotnet::Zapf {handle idx cmd arg} {
	if {[isbotnetnick [set bot [string tolower [lindex [set larg [split $arg]] 1]]]]} {
		callbot [lindex $larg 0] [lindex $larg 2] [join [lrange $larg 3 end]]
	} else {
		# Relay it to $bot
		variable Bots
		if {[info exists Bots($bot)]} {
			array set botinfo $Bots($bot)
			putidx $botinfo(peeridx) "$cmd $arg"
		}
	}
	return 0
}

# zapf-broad Atlantica mycommand testing 1 2 3 4 5
bind oldbotnet b zapf-broad ::tcldrop::bots::oldbotnet::Zapf-broad -priority 1000
bind oldbotnet b zb ::tcldrop::bots::oldbotnet::Zapf-broad -priority 1000
proc ::tcldrop::bots::oldbotnet::Zapf-broad {handle idx cmd arg} {
	callbot $handle [lindex [set larg [split $arg]] 1] [join [lrange $larg 2 end]]
	# Pass it along to all our peer bots (except the one that sent it to us):
	variable Peers
	foreach i [array names Peers] { if {$i != $idx} { putidx $i "$cmd $arg" } }
	return 0
}


# This is a [dccputchan] coming from KEEEEEEEL:
# c KEEEEEEEL A Testing 1 2 3
# chan KEEEEEEEL 0 Testing 1 2 3
bind oldbotnet b chan ::tcldrop::bots::oldbotnet::Chan -priority 1000
# Unsupported because of non-standard base64 component: bind oldbotnet b c ::tcldrop::bots::oldbotnet::C -priority 1000
proc ::tcldrop::bots::oldbotnet::Chan {handle idx cmd arg} {
	set sarg [split $arg]
	set remote [lindex $sarg 0]
	set chan [lindex $sarg 1]
	set text [join [lrange $sarg 2 end]]
	# if {![string match *@* $remote]} { switch -glob -- $arg { {* has joined the party line.} - {* has left the party line: *} { return 0 } } }
	callparty chat *:$remote chan $chan line $text
	return 0
}

# join Stupito FireEgl 0 *13 Proteus-D@adsl-220-213-190.bhm.bellsouth.net
# chan Stupito 0 FireEgl has joined the party line.
# part Atlantica FireEgl 26
# chan Atlantica 0 FireEgl has left the party line: file sys
# pt Atlantica FireEgl a file system
# part KEEEEEEEL FireEgl 14
# pt KEEEEEEEL FireEgl O byee...

# actchan FireEgl@KEEEEEEEL 0 this is an action.
bind oldbotnet b actchan ::tcldrop::bots::oldbotnet::Actchan -priority 1000
# Unsupported because of non-standard base64 component: bind oldbotnet b a ::tcldrop::bots::oldbotnet::A -priority 1000
proc ::tcldrop::bots::oldbotnet::Actchan {handle idx cmd arg} {
	set sarg [split $arg]
	set remote [lindex $sarg 0]
	set chan [lindex $sarg 1]
	set text [join [lrange $sarg 2 end]]
	callparty action *:$remote chan $chan line $text
	return 0
}

# This is a [dccbroadcast] coming from KEEEEEEEL:
# chat KEEEEEEEL Testing 1 2 3
# ct KEEEEEEEL Testing 1 2 3
bind oldbotnet b chat ::tcldrop::bots::oldbotnet::Chat -priority 1000
bind oldbotnet b ct ::tcldrop::bots::oldbotnet::Chat -priority 1000
proc ::tcldrop::bots::oldbotnet::Chat {handle idx cmd arg} {
	callparty broadcast [set bot [lindex [set sarg [split $arg]] 0]] bot $bot line [join [lrange $sarg 1 end]]
	return 0
}

bind oldbotnet b update ::tcldrop::bots::oldbotnet::Update -priority 1000
proc ::tcldrop::bots::oldbotnet::Update {handle idx cmd arg} {
	foreach {bot numversion} [split $arg] {break}
	variable Bots
	if {[info exists Bots([set bot [string tolower $bot]])]} {
		array set botinfo $Bots($bot)
		array set botinfo [list icon [string index $numversion 0] numversion [string range $numversion 1 end]]
		set Bots($bot) [array get botinfo]
	}
	return 0
}

# <llength> *** (YSL) Linked to NauGhTy.
# nlinked NauGhTy YSL -1061500
bind oldbotnet b nlinked ::tcldrop::bots::oldbotnet::Nlinked -priority 1000
# Unsupported because of the non-standard base64 components: bind oldbotnet b n ::tcldrop::bots::oldbotnet::N -priority 1000
proc ::tcldrop::bots::oldbotnet::Nlinked {handle idx cmd arg} {
	foreach {bot uplink numversion} [split $arg] {break}
	set icon [string index $numversion 0]
	set numversion [string range $numversion 1 end]
	lappend tracepath $uplink
	variable Linked
	foreach colonpath [array names Linked *:[set colonpath [string tolower $uplink]]] { set tracepath $Linked($colonpath) }
	set Linked(${colonpath}:[set lowerbot [string tolower $bot]]) [concat $tracepath [list $bot]]
	variable Bots
	set Bots($lowerbot) [list handle $bot tracepath $tracepath uplink $uplink numversion $numversion icon $icon peer $handle peeridx $idx]
	registerbot $lowerbot [list handle $bot id $lowerbot type oldbotnet]
	calllink $bot $uplink
	return 0
}

# unlinked WhiSPeR
# un Flounder Unlinked from: Flounder (Atlantica) (lost 1 bot and 0 users)
bind oldbotnet b unlinked ::tcldrop::bots::oldbotnet::Unlinked -priority 1000
bind oldbotnet b un ::tcldrop::bots::oldbotnet::Unlinked -priority 1000
proc ::tcldrop::bots::oldbotnet::Unlinked {handle idx cmd arg} {
	variable Linked
	array unset Linked *:[set bot [string tolower [lindex [set arg [split $arg]] 0]]]:*
	array unset Linked *:$bot
	variable Bots
	array unset Bots $bot
	unregisterbot $bot
	calldisc $bot [join [lrange $arg 1 end]]
	return 0
}

# join SaHeR ZimoZimo 0 *9 rajeh@riy-t2p134.saudi.net.sa
# j KEEEEEEEL FireEgl A *O Proteus-D@adsl-220-213-190.bhm.bellsouth.net
bind oldbotnet b join ::tcldrop::bots::oldbotnet::Join -priority 1000
# Unsupported because of non-standard base64 components: bind oldbotnet j ::tcldrop::bots::oldbotnet::J -priority 1000
proc ::tcldrop::bots::oldbotnet::Join {handle idx cmd arg} {
	foreach {bot handle chan flagidx userhost} [split $arg] {
		callparty join [set usersidx [string range $flagidx 1 end]]:${handle}@$bot bot $bot handle $handle chan $chan flag [string index $flagidx 0] idx $usersidx userhost $userhost
		break
	}
	return 0
}

# part KEEEEEEEL FireEgl 14
# pt KEEEEEEEL FireEgl O byee...
bind oldbotnet b part ::tcldrop::bots::oldbotnet::Part -priority 1000
proc ::tcldrop::bots::oldbotnet::Part {handle idx cmd arg} {
	foreach {bot usershandle usersidx} [split $arg] {
		callparty part ${usersidx}:${usershandle}@$bot bot $bot handle $usershandle idx $usersidx line {Parted.}
		break
	}
	return 0
}

# chan Atlantica 0 FireEgl is now away: la la la....
# away Atlantica 26 la la la...
# aw KEEEEEEEL O fell asleep..
bind oldbotnet b away ::tcldrop::bots::oldbotnet::Away -priority 1000
proc ::tcldrop::bots::oldbotnet::Away {handle idx cmd arg} {
	callparty away [set usersidx [lindex [set arg [split $arg]] 1]]:*@[set bot [lindex $arg 0]] bot $bot idx $usersidx line [join [lrange $arg 2 end]]
	return 0
}

# idle Egoist 9 3175

# trace 7:FireEgl@Proteus Tcldrop :1071884289:Proteus:Atlantica

# info? 21:FireEgl@Atlantica

# This is a link REQUEST (I think):
# link 14:FireEgl@llength Tcldrop _8ball_

# This is an unlink REQUEST (I think):
# ul Yum Atlantica flounder

# This is a [boot FireEgl@Tcldrop] coming from Stupito:
# reject Stupito FireEgl@Tcldrop For this reason.
# And a [boot FireEgl@Stupito] from KEEEEEEEL:
# r KEEEEEEEL FireEgl@Stupito The reason..

bind oldbotnet b ping ::tcldrop::bots::oldbotnet::Ping -priority 1000
bind oldbotnet b pi ::tcldrop::bots::oldbotnet::Ping -priority 1000
proc ::tcldrop::bots::oldbotnet::Ping {handle idx cmd arg} {
	putidx $idx [string trimright "pong $arg"]
	return 0
}

bind oldbotnet b share ::tcldrop::bots::oldbotnet::Share
proc ::tcldrop::bots::oldbotnet::Share {handle idx cmd arg} {
	# FixMe: Make sure we're receiving this from a share-bot.
	foreach {type command arguments options} [split $arg] { break }
	array set opts $options
	array set opts [list -module oldbotnet]
	share $type $command $arguments [array get opts]
}


bind chat p * ::tcldrop::bots::oldbotnet::CHAT
proc ::tcldrop::bots::oldbotnet::CHAT {handle chan text} {
	variable Peers
	variable Bots
	if {![info exists Bots([set bot [string tolower [lindex [split $handle @] end]]])] && ![string match {*@*} $handle] && ![isbotnetnick $bot]} {
		# Locally generated, send it to all our peer bots:
		set handle $handle@${::botnet-nick}
		foreach i [array names Peers] {
			putlog "Sending1: chan $handle $chan $text"
			putidx $i "chan $handle $chan $text"
		}
	} else {
		# Came from a remote bot, send it to all our peers, except the one we got it from:
		if {[info exists Bots($bot)]} {
			array set botinfo $Bots($bot)
			foreach i [array names Peers] {
				if {![string equal $i $botinfo(peeridx)]} {
					putlog "Sending2: chan $handle $chan $text"
					putidx $i "chan $handle $chan $text"
				}
			}
		}
	}
	return 0
}

proc ::tcldrop::bots::oldbotnet::LOAD {module} {
	variable Bots
	array set Bots {}
	variable Linked
	array set Linked {}
	variable Peers
	array set Peers {}
	# Add a new bot type:
	addbottype oldbotnet namespace ::tcldrop::bots::oldbotnet putbot ::tcldrop::bots::oldbotnet::putbot putallbots ::tcldrop::bots::oldbotnet::putallbots
	# Add new listen types (these are used by the [listen] command):
	addlistentype oldbotnet connect ::tcldrop::bots::oldbotnet::Connect ident 1
	addlistentype eggdrop1.6 connect ::tcldrop::bots::oldbotnet::Connect ident 1
	addlistentype eggdrop1.5 connect ::tcldrop::bots::oldbotnet::Connect ident 1
	addlistentype eggdrop1.4 connect ::tcldrop::bots::oldbotnet::Connect ident 1
	addlistentype eggdrop1.3 connect ::tcldrop::bots::oldbotnet::Connect ident 1
	addlistentype eggdrop connect ::tcldrop::bots::oldbotnet::Connect ident 1
	addlistentype bots connect ::tcldrop::bots::oldbotnet::Connect ident 1
	# FixMe: It would be slightly more preferred if all the bind's were moved into this proc.
}
bind load - bots::oldbotnet ::tcldrop::bots::oldbotnet::LOAD -priority 0

proc ::tcldrop::bots::oldbotnet::UNLD {module} {
	# FixMe: Need to unbind all of the binds used in this .tcl
	# FixMe: Need to delbottype and dellistentype here.
	return 1
}
bind unld - bots::oldbotnet ::tcldrop::bots::oldbotnet::UNLD -priority 0

