# dcc::irc --
#	Handles:
#		IRC interface for users.
#	Depends: party.
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
# Or visit http://www.GNU.Org/licenses/gpl.html
#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
# This module provides the IRC interface for users to access the bot.

# http://www.eggfaq.com/docs/raw.html

namespace eval ::tcldrop::dcc::irc {
	variable name {dcc::irc}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable depends {party irc server channels console core::users core::dcc core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Provides the IRC interface for users.}
	variable rcsid {$Id$}
	variable commands [list ircparty callircparty setircparty getircparty unsetircparty]
	package provide tcldrop::$name $version
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
	namespace path ::tcldrop
}

proc ::tcldrop::dcc::irc::Connect {idx} {
	# Next line is used only during testing:
	#~ uplevel #0 { source [file join modules party ircparty.tcl] }
	idxinfo $idx -control {::tcldrop::dcc::irc::Read} -writable {::tcldrop::dcc::irc::Write} -errors {::tcldrop::dcc::irc::Error} state {CONNECT} other {conn} handle {*} module {ircparty} timestamp [clock seconds]
	setircparty user $idx idx $idx nick {*} nickname "*@${::botnet-nick}:$idx" mode {w} username {-} realname {?}
	if {![info exists ::tcldrop]} { control $idx ::tcldrop::dcc::irc::Read }
	Write $idx
}

proc ::tcldrop::dcc::irc::Write {idx} {
	array set idxinfo [idxinfo $idx]
	if {$idxinfo(state) eq {CONNECT}} {
		idxinfo $idx state {REGISTER} other {reg} traffictype {partyline} timestamp [clock seconds]
		Putidx $idx "NOTICE AUTH :*** Looking up your hostname..."
		Putidx $idx "NOTICE AUTH :*** Checking Ident"
		Putidx $idx "NOTICE AUTH :*** Got Ident response"
		Putidx $idx "NOTICE AUTH :*** Found your hostname"
		return 0
	} else {
		return 1
	}
}

proc ::tcldrop::dcc::irc::Read {idx line} {
	if {$line != {}} {
		array set idxinfo [idxinfo $idx]
		switch -- $idxinfo(state) {
			{CHAT} {
				if {![callircparty $idx [set command [string toupper [lindex [set sline [split [string trim $line]]] 0]]] [join [lrange $sline 1 end]]]} {
					PutRAW $idx "421 [getircparty user $idx nick] $command :Unknown command"
					putdebuglog "$idx: $line"
				}
				return 0
			}
			{REGISTER} {
				switch -- [set command [string toupper [lindex [set sline [split [string trim $line]]] 0]]] {
					{NICK} - {USER} - {PASS} - {OPER} - {QUIT} - {PASSWORD} - {LOGIN} - {LOGON} {
						# ^ These are the only commands available until they send the PASS/NICK/USER commands. ^
						if {![callircparty $idx $command [join [lrange $sline 1 end]]]} {
							PutRAW $idx "421 [getircparty user $idx nick] $command :Unknown command"
							putdebuglog "$idx: $line"
							return 1
						} elseif {[ircparty registered $idx]} {
							idxinfo $idx state CHAT other {chat} timestamp [clock seconds]
							Welcome $idx
						}
						return 0
					}
					{default} {
						PutRAW $idx "451 [getircparty user $idx nick] :You have not registered"
						putdebuglog "$idx: $line"
						return 0
					}
				}
			}
			{CONNECT} { Write $idx }
			{default} { return 1 }
		}
	} elseif {![valididx $idx]} {
		Error $idx
	} else {
		return 1
	}
}

proc ::tcldrop::dcc::irc::Error {idx {error {}}} {
	#array set idxinfo [idxinfo $idx]
	#callparty quit ${idx}:$idxinfo(handle)@${::botnet-nick}
	return 1
}

proc ::tcldrop::dcc::irc::Welcome {idx} {
	array set userinfo [getircparty user $idx]
	PutRAW $idx "001 $userinfo(nickname) :Welcome to the Tcldrop Internet Relay Chat Network $userinfo(nickname)"
	variable version
	PutRAW $idx "002 $userinfo(nickname) :Your host is ${::botnet-nick}, running ircparty version $version"
	PutRAW $idx "003 $userinfo(nickname) :This server was created Wed Feb 16 2005 at 09:34:49 PST"
	PutRAW $idx "004 $userinfo(nickname) Tcldrop ircparty oiwszcerkfydnxbauglZC biklmnopstveI bkloveI"
	PutRAW $idx "005 $userinfo(nickname) CHANTYPES=&#% EXCEPTS INVEX CHANMODES=eIb,k,l,imnpst CHANLIMIT=&#:40 PREFIX=(ov)@+ MAXLIST=beI:100 NETWORK=ircparty MODES=4 STATUSMSG=@+ KNOCK CALLERID=g NICKLEN=9 :are supported by this server"
	PutRAW $idx "005 $userinfo(nickname)  SAFELIST ELIST=U CASEMAPPING=rfc1459 CHARSET=ascii CHANNELLEN=50 TOPICLEN=160 KICKLEN=120 ETRACE :are supported by this server"
	PutRAW $idx "251 $userinfo(nickname) :There are 1 users and 1 invisible on 1 servers"
	PutRAW $idx "252 $userinfo(nickname) 0 :IRC Operators online"
	PutRAW $idx "253 $userinfo(nickname) 0 :unknown connection(s)"
	PutRAW $idx "254 $userinfo(nickname) [llength [channels]] :channels formed"
	PutRAW $idx "255 $userinfo(nickname) :I have 1 clients and 1 servers"
	PutRAW $idx "265 $userinfo(nickname) 1 1 :Current local users 1, max 1"
	PutRAW $idx "266 $userinfo(nickname) 1 1 :Current global users 1, max 1"
	PutRAW $idx "250 $userinfo(nickname) :Highest connection count: 1 (1 clients) (1 connections received)"
	PutRAW $idx "375 $userinfo(nickname) :- ${::botnet-nick} Message of the Day -"
	if {![catch { open [file join ${::text-path} motd] r } fid]} {
		foreach line [textsubst [idx2hand $idx] [read -nonewline $fid] -blanklines 0 -returnlist 1] { PutRAW $idx "372 $userinfo(nickname) :- $line" }
		close $fid
	}
	#~ PutRAW $idx "372 $userinfo(nickname) :- MotD goes here.  =D"
	PutRAW $idx "376 $userinfo(nickname) :End of /MOTD command."
	# Auto-join all the channels they have access to:
	ircparty_JOIN $idx {JOIN} [join [channels] ,]

	# Auto-join the command channel for this bot:
	Putidx $idx ":$userinfo(nickname)!$userinfo(username)@$userinfo(vhost) JOIN :%${::botnet-nick}" [list -flush 1]
	# /names list for the command channel:
	PutRAW $idx "353 $userinfo(nickname) = %${::botnet-nick} :@${::botnet-nick} $userinfo(nickname)"
	PutRAW $idx "366 $userinfo(nickname) %${::botnet-nick} :End of /NAMES list."
	# topic for the command channel:
	PutRAW $idx "332 $userinfo(nickname) %${::botnet-nick} :Welcome to the Command Channel for ${::botnet-nick}!"
	PutRAW $idx "333 $userinfo(nickname) %${::botnet-nick} ${::botnet-nick}![string tolower ${::botnet-nick}]@ircparty. $::uptime"
	idxinfo $idx filter ::tcldrop::dcc::irc::PutidxFilter
	initconsole $idx
}

# Sets ircparty related info about a chan/user/chanuser ($type).
# $what is $idx for the "user" type..
# $what will be a channel for a "chan" type.  (Eggdrop only supports integers)  The assoc module will be used to convert the int to a channel name if needed.
# $what will be $channel,$idx for a "chanuser" type.
proc ::tcldrop::dcc::irc::setircparty {type args} {
	switch -- [string tolower $type] {
		{chanuser} - {chanusers} {
			variable IRCParty_chanusers
			if {[info exists IRCParty_chanusers([set chan [string tolower [lindex $args 0]]],[set idx [lindex $args 1]])]} {
				array set info $IRCParty_chanusers($chan,$idx)
			}
			array set info [lrange $args 2 end]
			set IRCParty_chanusers($chan,$idx) [array get info]
		}
		{user} - {users} {
			variable IRCParty_users
			if {[info exists IRCParty_users([set idx [lindex $args 0]])]} {
				array set info $IRCParty_users($idx)
			}
			array set info [lrange $args 1 end]
			set IRCParty_users($idx) [array get info]
		}
		{chan} - {chans} - {channel} - {channels} {
			variable IRCParty_chans
			if {[info exists IRCParty_chans([set chan [string tolower [lindex $args 0]]])]} {
				array set info $IRCParty_chans($chan)
			}
			array set info [lrange $args 1 end]
			set IRCParty_chans($chan) [array get info]
		}
	}
}

# Gets ircparty related info:
proc ::tcldrop::dcc::irc::getircparty {type args} {
	switch -- [string tolower $type] {
		{chanuser} - {chanusers} {
			variable IRCParty_chanusers
			variable IRCParty_users
			if {[info exists IRCParty_chanusers([set chan [string tolower [lindex $args 0]]],[set idx [lindex $args 1]])]} {
				array set info [idxinfo $idx]
				array set info $IRCParty_chanusers($chan,$idx)
				array set info $IRCParty_users($idx)
				if {[set getinfo [lindex $args 2]] != {}} {
					if {[info exists info($getinfo)]} { return $info($getinfo) } else { return {} }
				} else {
					return [array get info]
				}
			}
		}
		{user} - {users} {
			variable IRCParty_users
			if {[info exists IRCParty_users([set idx [lindex $args 0]])]} {
				array set info [idxinfo $idx]
				array set info $IRCParty_users($idx)
				if {[set getinfo [lindex $args 1]] != {}} {
					if {[info exists info($getinfo)]} { return $info($getinfo) } else { return {} }
				} else {
					return [array get info]
				}
			}
		}
		{chan} - {chans} - {channel} - {channels} {
			variable IRCParty_chans
			if {[info exists IRCParty_chans([set chan [string tolower [lindex $args 0]]])]} {
				array set info $IRCParty_chans($chan)
				if {[set getinfo [lindex $args 1]] != {}} {
					if {[info exists info($getinfo)]} { return $info($getinfo) } else { return {} }
				} else {
					return [array get info]
				}
			}
		}
	}
}

# [ircparty userlist <idxmask>] -> list of idxes
# [ircparty chanlist <channel> [info1] [info2] ...] -> list of idxes, plus the values of the info's.
# [ircparty idxonchan <idx> <channel>] -> 1 or 0
# [ircparty onchan <info> <channel> <infoname>] -> 1 or 0
# [ircparty handonchan <handle> <channel>] -> 1 or 0
# [ircparty channels] -> list of all active (occupied) channels
# [ircparty chaninfo <channel>] -> key/value list containing all info on channel
# [ircparty chanuserinfo <channel> <idx>] -> key/value list of channel-specific info on idx
# [ircparty userinfo <idx>] -> key/value list of idx (ircparty only) info.
# [ircparty unset channel <channel>] -> removes info on a channel, including any users who are on the channel.
# [ircparty registered <idx>] -> 1 or 0
proc ::tcldrop::dcc::irc::ircparty {command args} {
	switch -- $command {
		{userlist} {
			variable IRCParty_users
			return [array names IRCParty_users]
		}
		{chanlist} {
			variable IRCParty_chanusers
			variable IRCParty_users
			set list {}
			foreach cu [array names IRCParty_chanusers [set chan [string tolower [lindex $args 0]]],*] {
				array set userinfo $IRCParty_chanusers($cu)
				array set userinfo $IRCParty_users($userinfo(idx))
				foreach i [lrange $args 1 end] { if {[info exists userinfo($i)]} { lappend list $userinfo($i) } else { lappend list {} } }
				if {![info exists i]} { lappend list $userinfo(idx) }
			}
			return $list
		}
		{idxonchan} {
			variable IRCParty_chanusers
			return [info exists IRCParty_chanusers([string tolower [lindex $args 1]],[lindex $args 0])]
		}
		{handonchan} { ircparty onchan {*}$args handle }
		{validchan} {
			return [info exists IRCParty_chans([string tolower [lindex $args 0]])]
		}
		{onchan} {
			variable IRCParty_chanusers
			variable IRCParty_users
			set findhand [lindex $args 0]
			set info [lindex $args 2]
			foreach u [array names IRCParty_chanusers [string tolower [lindex $args 1]],*] {
				set userinfo $IRCParty_chanusers($u)
				set userinfo [idxinfo $idx]
				set userinfo $IRCParty_users($userinfo(idx))
				if {[string equal -nocase $findhand $userinfo($info)]} { return 1 }
			}
			return 0
		}
		{registered} {
			array set idxinfo [idxinfo [set idx [lindex $args 0]]]
			array set userinfo [getircparty user $idx]
			if {$idxinfo(handle) != {*} && $userinfo(username) != {-} && $userinfo(nick) != {*}} {
				return 1
			}
			return 0
		}
	}
}

#putircparty -chan $chan ":$userinfo(nickname)!$userinfo(username)@$userinfo(vhost) JOIN :$chan"
# putircparty -chan $chan -flags n ":$nick!$host PRIVMSG $chan :$text"
# putircparty -flags n
proc ::tcldrop::dcc::irc::putircparty {args} {
	array set putinfo [list -excludeidx {} -chan {} -text [lindex $args end] -flags {}]
	array set putinfo [lrange $args 0 end-1]
	#putdebuglog "::tcldrop::dcc::irc::putircparty putinfo [array get putinfo]"
	if {$putinfo(-chan) != {}} {
		foreach i [ircparty chanlist $putinfo(-chan)] {
			if {$i != $putinfo(-excludeidx) && [valididx $i] && [matchattr [idx2hand $i] $putinfo(-flags) $putinfo(-chan)]} {
				putdebuglog "Putidx: $i $putinfo(-text)"
				Putidx $i $putinfo(-text)
			}
		}
	} else {
		foreach i [ircparty chanlist $putinfo(-chan)] {
			if {$i != $putinfo(-exlcludeidx) && [valididx $i] && [matchattr [idx2hand $i] $putinfo(-flags)]} {
				Putidx $i $putinfo(-text)
			}
		}
	}
}

# This is the putidx command from the core::conn module, without the filter part.
# Sends $text to $idx:
# Note: This is an Eggdrop v1.6 and less command.
proc ::tcldrop::dcc::irc::Putidx {idx text {opts {}}} {
	if {[info exists ::idxlist($idx)]} {
		array set idxinfo $::idxlist($idx)
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

proc ::tcldrop::dcc::irc::PutidxFilter {idx text opts} {
	foreach line [split $text \n] {
		lappend retVal ":${::botnet-nick}![string tolower ${::botnet-nick}]@ircparty. PRIVMSG %${::botnet-nick} :$line"
	}
	 return [join $retVal \n]
}

proc ::tcldrop::dcc::irc::PutRAW {idx msg} {
	Putidx $idx ":${::botnet-nick} $msg"
}

proc ::tcldrop::dcc::irc::PutRAWClient {idx code msg} {
	PutRAW $idx "$code [getidxinfo $idx nickname] $msg"
}

proc ::tcldrop::dcc::irc::ircparty_PING {idx command arg} {
	PutRAW $idx "PONG Tcldrop $arg"
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_PONG {idx command arg} {
	idxinfo $idx timestamp [clock seconds]
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_NICK {idx command arg} {
	putdebuglog "IN ::tcldrop::dcc::irc::NICK $idx $command $arg"
	set nick [string trimleft $arg {: }]
	if {[set handle [idx2hand $idx]] == {*}} { set handle "${nick}" }
	setircparty user $idx nick $nick nickname [set nickname "${handle}@${::botnet-nick}:$idx"]
	PutRAW $idx ":$nick!${idx}@${::botnet-nick} NICK $nickname"
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_USER {idx command arg} {
	putdebuglog "IN ::tcldrop::dcc::irc::USER $idx $command $arg"
	if {[set username [string trim [lindex [set sarg [split $arg]] 0]]] == {}} {
		return 0
	} else {
		if {[string is int [set vhost [string trim [lindex $sarg 1] {""}]]] || [string index $vhost 0] == {+}} { set vhost [getidxinfo $idx remote-hostname] }
		# Set the servers name to the most appropriate value, starting with what the client calls us:
		if {[string length [set server [string trim [lindex $sarg 2] {""}]]] < 5 || ![string match {*.*} $server]} {
			if {[info exists ::my-hostname] && ${::my-hostname} != {}} {
				set server ${::my-hostname}
			} elseif {[info exists ::my-ip] && ${::my-ip} != {}} {
				set server ${::my-ip}
			} else {
				set server ${::botnet-nick}
			}
		}
		setircparty user $idx username $username realname [string range [join [lrange $sarg 3 end]] 1 end] vhost $vhost server $server
		return 1
	}
}

# Funnel all login commands through the OPER command:
proc ::tcldrop::dcc::irc::ircparty_OPER {idx command arg} {
	foreach {handle password} [split $arg] {break}
	if {([validuser $handle] && ![passwdok $handle -] && [passwdok $handle $password]) && (!${::require-p} || [matchattr $handle p])} {
		#setparty $idx handle $handle
		idxinfo $idx handle [getuser $handle handle]
		setircparty user $idx nickname "${handle}@${::botnet-nick}:$idx"
	}
	return 1
}
proc ::tcldrop::dcc::irc::ircparty_LOGIN {idx command arg} { ircparty_OPER $idx $command $arg }
proc ::tcldrop::dcc::irc::ircparty_LOGON {idx command arg} { ircparty_OPER $idx $command $arg }
proc ::tcldrop::dcc::irc::ircparty_PASSWORD {idx command arg} { ircparty_PASS $idx $command $arg }
proc ::tcldrop::dcc::irc::ircparty_PASS {idx command arg} {
	# FixMe: Prevent people from using the PASS command too many times.
	foreach {handle password} [split $arg :] {break}
	ircparty_OPER $idx $command "$handle $password"
}

proc ::tcldrop::dcc::irc::ircparty_QUIT {idx command arg} {
	#unsetparty $idx
	Putidx $idx "ERROR :Client Quit: $arg"
	killidx $idx
	return 1
}


# :irc.choopa.net 324 Tcldrop #Test +tnl 2147483647
# :irc.choopa.net 329 Tcldrop #Test 985416044
proc ::tcldrop::dcc::irc::ircparty_MODE {idx command arg} {
	putdebuglog "in ::tcldrop::dcc::irc::ircparty_MODE $idx $command $arg"
	if {[llength [set sarg [split $arg]]] == 1 && [validchan $arg]} {
		puthelp "$command $arg"
		RAWCapture {324 329} $idx
	} else {

	}
	return 1
}

# :irc.choopa.net 302 Tcldrop :Tcldrop=+Tcldrop@2001:5c0:84dc:7::
proc ::tcldrop::dcc::irc::ircparty_USERHOST {idx command arg} {
	putdebuglog "in ::tcldrop::dcc::irc::ircparty_USERHOST $idx $command $arg"
	array set userinfo [getircparty user $idx]
	switch -- $arg {
		$userinfo(nickname) {
			PutRAW "302 $userinfo(nickname) :$userinfo(nickname)=+$userinfo(username)@$userinfo(vhost)"
		}
		$userinfo(nick) {
			PutRAW "302 $userinfo(nickname) :$userinfo(nick)=+$userinfo(username)@$userinfo(vhost)"
		}
		{default} {

		}
	}
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_VERSION {idx command arg} {
	putdebuglog "in ::tcldrop::dcc::irc::ircparty_VERSION $idx $command $arg"
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_ISON {idx command arg} {
	#putdebuglog "in ::tcldrop::dcc::irc::ircparty_ISON $idx $command $arg"
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_JOIN {idx command arg} {
	# Allow anybody to join any partyline channel.
	# Only allow +vofmn to join already valid channels on irc.
	# Only allow global +mn to join invalid channels. (they'll be [channel add]'d on JOIN)
	putdebuglog "ircparty_JOIN $idx $command $arg"
	array set userinfo [getircparty user $idx]
	array set idxinfo [idxinfo $idx]
	foreach chankey [split $arg ,] {
		foreach {chan key} [split $chankey] {break}
		if {![validchan $chan] && [matchattr $idxinfo(handle) n]} { channel add $chan }
		if {([validchan $chan]) && ([botonchan $chan]) && ([haschanrec $idxinfo(handle) $chan] || [matchattr $idxinfo(handle) ofmn])} {
			setircparty chanuser $chan $idx chan $chan idx $idx
			setircparty chan $chan chan $chan
			putircparty -chan $chan ":$userinfo(nickname)!$userinfo(username)@$userinfo(vhost) JOIN :$chan"
			ircparty_TOPIC $idx $command $chan
			ircparty_NAMES $idx $command $chan
			if {[matchattr $idxinfo(handle) n]} {
				#PutRAW $idx "MODE" .....
			}
		}
	}
	return 1
}

proc ::tcldrop::dcc::irc::ircparty_NAMES {idx command arg} {
	array set userinfo [getircparty user $idx]
	array set idxinfo [idxinfo $idx]
	if {[validchan [set chan [string trimleft $arg :]]]} {
		switch -glob [lindex [split [getchanmode $chan]] 0] {
			{*s*} { set chanflag {@} }
			{*p*} { set chanflag {*} }
			{default} { set chanflag {=} }
		}
		set nicklist [list]
		foreach n [chanlist $chan] {
			if {[isop $n $chan]} {
				lappend nicklist "@$n"
			} elseif {[isvoice $n $chan]} {
				lappend nicklist "+$n"
			} else {
				lappend nicklist "$n"
			}
		}
		# FixMe: Split this into multiple 353's if when the line gets too long:
		PutRAW $idx "353 $userinfo(nickname) $chanflag $chan :[join $nicklist]"
	}
	putdebuglog "ircparty names: [join [ircparty chanlist $chan nickname]]"
	PutRAW $idx "353 $userinfo(nickname) = $chan :[join [ircparty chanlist $chan nickname]]"
	PutRAW $idx "366 $userinfo(nickname) $chan :End of /NAMES list."
}

proc ::tcldrop::dcc::irc::ircparty_TOPIC {idx command arg} {
	array set userinfo [getircparty user $idx]
	if {[validchan [set chan [string trimleft $arg :]]]} {
		PutRAW $idx "332 $userinfo(nickname) $chan :[topic $chan]"
		PutRAW $idx "333 $userinfo(nickname) $chan ${::botnet-nick} [clock seconds]"
	} elseif {[ircparty validchan $chan]} {
	}
}


proc ::tcldrop::dcc::irc::ircparty_PART {idx command arg} {
	return 1
}

# :irc.blessed.net 352 FireTest #Tcldrop Ariel Atlantica.US irc.choopa.net FireEgl H@ :3 Proteus
# :irc.blessed.net 315 FireTest #Tcldrop :End of /WHO list.
proc ::tcldrop::dcc::irc::ircparty_WHO {idx command arg} {
	set chan $arg
	if {[ircparty idxonchan $idx $chan]} {
		set nick [getircparty user $idx nickname]
		foreach n [chanlist $chan] {
			if {[isop $n $chan]} {
				set nickflag {@}
			} elseif {[isvoice $n $chan]} {
				set nickflag {+}
			} else {
				set nickflag {}
			}
			foreach {ident hostname} [split [getchanhost $n $chan] {@}] { break }
			# FixMe: On Eggdrop, create a RAW bind for 352, and store the info for use here.
			#        Alternatively, pass RAW 352's directly.
			PutRAW $idx "352 $nick $chan $ident $hostname ${::botnet-nick} $n H$nickflag :1 $n"
		}
		PutRAW $idx "315 $nick $chan :End of /WHO list."
	}
	return 1
}

# :irc.inter.net.il 311 SafeTcl FireEgl Ariel Atlantica.US * :Proteus
# :irc.inter.net.il 319 SafeTcl FireEgl :@#Tcldrop @+#Windrop @#ubuntu @+#test +#tcl #startrek #StarGate #kubuntu @+#eggtcl @+#debian
# :irc.inter.net.il 312 SafeTcl FireEgl irc.choopa.net :Divided we stand, united we fall
# :irc.inter.net.il 318 SafeTcl FireEgl :End of /WHOIS list.
proc ::tcldrop::dcc::irc::ircparty_WHOIS {idx command arg} {
	#set nick [getircparty user $idx nickname]
	#foreach {ident hostname} [split [getchanhost [lindex [split $arg] 0]] @] {break}
	#if {$ident != {}} {
	#	PutRAW $idx "311 $nick $arg $ident $hostname * :$arg"
	#	PutRAW $idx "312 $nick $arg ${::botnet-nick} :Tcldrop"
	#	PutRAW $idx "318 $nick $arg :End of /WHOIS list."
	#}
	puthelp "$command $arg"
	RAWCapture {401 311 378 307 319 312 338 317 318} $idx
	return 1
}

proc ::tcldrop::dcc::irc::RAWCapture {raws idx} {
	variable RAWCaptures
	foreach r $raws {
		bind raw - $r ::tcldrop::dcc::irc::RAWCapture_RAW
		set RAWCaptures($r,$idx) [list idx $idx last [lindex $raws end]]
	}
	lappend RAWCaptures($r,$idx) raws $raws afterid [after 9999 [list ::tcldrop::dcc::irc::RAWCapture_unset $raws $idx]]
}

proc ::tcldrop::dcc::irc::RAWCapture_unset {raws idx} {
	variable RAWCaptures
	foreach r $raws {
		array unset RAWCaptures $r,$idx
		if {![llength [array names RAWCaptures $r,*]]} { catch { unbind raw - $r ::tcldrop::dcc::irc::RAWCapture_RAW } }
	}
}

proc ::tcldrop::dcc::irc::RAWCapture_RAW {from key arg} {
	variable RAWCaptures
	foreach a [array names RAWCaptures $key,*] {
		array set rawinfo $RAWCaptures($a)
		Putidx $rawinfo(idx) ":$from $key $arg"
		if {[string equal -nocase $rawinfo(last) $key]} {
			after idle [list ::tcldrop::dcc::irc::RAWCapture_unset $rawinfo(raws) $rawinfo(idx)]
			after cancel $rawinfo(afterid)
		}
	}
	return 0
}

proc ::tcldrop::dcc::irc::ircparty_PRIVMSG {idx command arg} {
	# Allow anybody to talk on partyline channels,
	# but only allow people who are +vofmn to talk on channels on the bots irc connection.
	# Also, only allow global +mn to send PRIVMSG's to nicks on irc.
	array set userinfo [getircparty user $idx]
	array set idxinfo [idxinfo $idx]
	# check if we're dealing with PRIVMSG's to a command channel
	if {[string index $arg 0] ne {%}} {
		if {([validchan [set chan [lindex [set sarg [split $arg]] 0]]] && [ircparty idxonchan $idx $chan]) || ([matchattr $idxinfo(handle) nm])} {
			if {[matchattr $idxinfo(handle) z]} {
				puthelp "$command $arg"
			} else {
				puthelp "$command $chan :<$idxinfo(handle)> [string range [string trim [join [lrange $sarg 1 end]]] 1 end]"
			}
		}
		if {[ircparty idxonchan $idx $chan]} { putircparty -chan $chan -excludeidx $idx ":$userinfo(nickname) $command $arg" }
	# This is a command channel
	} else {
		set text [string range $arg [expr {[string first {:} $arg] + 1}] end]
		if {[string index $text 0] eq {.}} {
			dccsimul $idx $text
		} else {
			callparty chat $idx:[idx2hand $idx]@${::botnet-nick} handle [idx2hand $idx] idx $idx bot ${::botnet-nick} line $text
		}
	}
	return 1
}

namespace eval ::tcldrop::dcc::irc {
	# send PING to all clients every once in a while
	# FixMe: make this only send PING if there's no activity, and move it into a proc
	if {![info exists PingTimer]} {
		set PingTimer [utimer 210 { ::tcldrop::dcc::irc::putircparty -chan * "PING :[unixtime]" } -1]
	}
}

::tcldrop::bind sign - * ::tcldrop::dcc::irc::SIGN
proc ::tcldrop::dcc::irc::SIGN {nick host hand chan reason} {
	# Send to ircparty users who are on $chan:
	# FixMe: Change this to a RAW bind, because QUIT's are NOT channel specific.. (We don't want to send a QUIT for every channel $nick was in.)
	putircparty -chan $chan ":$nick!$host QUIT :$reason"
}

::tcldrop::bind splt - * ::tcldrop::dcc::irc::SPLT
proc ::tcldrop::dcc::irc::SPLT {nick host hand chan} {
	# FixMe: Just make this a RAW bind to QUIT.
}

::tcldrop::bind rejn - * ::tcldrop::dcc::irc::REJN
proc ::tcldrop::dcc::irc::REJN {nick host hand chan} {
	# FixMe: Just make this a RAW bind to JOIN.
}

# FixMe: Make this a RAW JOIN bind instead, so rejoins and joins can be treated the same.
::tcldrop::bind join - * ::tcldrop::dcc::irc::JOIN
proc ::tcldrop::dcc::irc::JOIN {nick host hand chan} {
	# Send to ircparty users who are on $chan:
	putircparty -chan $chan ":$nick!$host JOIN :$chan"
}

::tcldrop::bind part - * ::tcldrop::dcc::irc::PART
proc ::tcldrop::dcc::irc::PART {nick host hand chan msg} {
	# Send to ircparty users who are on $chan:
	putircparty -chan $chan ":$nick!$host PART $chan"
}

::tcldrop::bind topc - * ::tcldrop::dcc::irc::TOPC
proc ::tcldrop::dcc::irc::TOPC {nick host hand chan topic} {
	# Send to ircparty users who are on $chan:
	putircparty -chan $chan ":$nick!$host TOPIC $chan :$topic"
}

::tcldrop::bind kick - * ::tcldrop::dcc::irc::KICK
proc ::tcldrop::dcc::irc::KICK {nick host hand chan target reason} {
	# Send to ircparty users who are on $chan:
	putircparty -chan $chan ":$nick!$host KICK $chan $target :$reason"
}

::tcldrop::bind nick - * ::tcldrop::dcc::irc::NICK
proc ::tcldrop::dcc::irc::NICK {nick host hand chan newnick} {
	# Send to ircparty users who are on $chan:
	# FixMe: Make this a RAW bind, because NICK's are NOT channel specific.
	putircparty -chan $chan ":$nick!$host NICK :$newnick"
}

::tcldrop::bind mode - * ::tcldrop::dcc::irc::MODE
proc ::tcldrop::dcc::irc::MODE {nick host hand chan mode {victim {}}} {
	# Send to ircparty users who are on $chan:
	# Note/FixMe: Consider making this a RAW bind, so that the modes will be together instead of split apart.
	putircparty -chan $chan ":$nick!$host MODE $chan $mode $victim"
}

::tcldrop::bind pubm - * ::tcldrop::dcc::irc::PUBM
proc ::tcldrop::dcc::irc::PUBM {nick host hand chan text} {
	# Send to ircparty users who are on $chan:
	putircparty -chan $chan ":$nick!$host PRIVMSG $chan :$text"
}

::tcldrop::bind msgm - * ::tcldrop::dcc::irc::MSGM
proc ::tcldrop::dcc::irc::MSGM {nick host hand text} {
	# Send to all ircparty users with global +n
	putircparty -flags n ":$nick!$host PRIVMSG $::botnick :$text"
}

::tcldrop::bind ctcp - * ::tcldrop::dcc::irc::CTCP
proc ::tcldrop::dcc::irc::CTCP {nick host hand dest keyword text} {
	if {[string equal -nocase $keyword {ACTION}]} {
		# Only send ACTION's, becase ircparty users will try to respond to other CTCP's, which would be bad.
		if {[validchan $dest]} {
			putircparty -chan $dest ":$nick!$host PRIVMSG $dest :\001$keyword $text\001"
		} else {
			putircparty -flags n ":$nick!$host PRIVMSG $dest :\001$keyword $text\001"
		}
	}
}

::tcldrop::bind ctcr - * ::tcldrop::dcc::irc::CTCR
proc ::tcldrop::dcc::irc::CTCR {nick host hand dest keyword text} {
	if {[validchan $dest]} {
		# Send to all ircparty users on channel $dest:
		putircparty -chan $dest ":$nick!$host NOTICE $dest :\001$keyword $text\001"
	} else {
		# Send to all ircparty users with global +n
		putircparty -flags n ":$nick!$host NOTICE $dest :\001$keyword $text\001"
	}
}

::tcldrop::bind notc - * ::tcldrop::dcc::irc::NOTC
proc ::tcldrop::dcc::irc::NOTC {nick host hand text dest} {
	if {[validchan $dest]} {
		# Send to ircparty users who are on $dest:
		putircparty -chan $dest ":$nick!$host NOTICE $dest :$text"
	} else {
		# Send to ircparty users with global +nm:
		putircparty -flags nm ":$nick!$host NOTICE $dest :$text"
	}
}

::tcldrop::bind wall - * ::tcldrop::dcc::irc::WALL
proc ::tcldrop::dcc::irc::WALL {hand msg} {
	# FixMe: Make this send the walls to ircparty users with global +mn
}

proc ::tcldrop::dcc::irc::callircparty {idx command arg} {
	foreach {type flags mask proc} [bindlist ircparty] {
		if {[string match -nocase $mask $command]} {
			if {[catch { $proc $idx $command $arg } err]} {
				putlog "error in script: $proc: $err"
				puterrlog $::errorInfo
			} elseif {[string equal $err {1}]} {
				return 1
			}
			countbind $type $mask $proc
		}
	}
	return 0
}

::tcldrop::bind load - dcc::irc ::tcldrop::dcc::irc::LOAD
proc ::tcldrop::dcc::irc::LOAD {module} {
	setdefault open-telnets 1
	setdefault info-party 0
	setdefault connect-timeout 27
	setdefault use-telnet-banner 0
	setdefault text-path {text}
	setdefault telnet-banner {banner}
	setdefault motd {motd}
	setdefault network {Unknown}
	setdefault require-p 0
	setdefault default-flags {p}
	# Add new listen types (these are used by the [listen] command):
	addlistentype ircparty connect ::tcldrop::dcc::irc::Connect ident 1 dns 1
	bind ircparty - PING ::tcldrop::dcc::irc::ircparty_PING
	bind ircparty - PONG ::tcldrop::dcc::irc::ircparty_PONG
	bind ircparty - NICK ::tcldrop::dcc::irc::ircparty_NICK
	bind ircparty - USER ::tcldrop::dcc::irc::ircparty_USER
	bind ircparty - OPER ::tcldrop::dcc::irc::ircparty_OPER
	bind ircparty - WHO ::tcldrop::dcc::irc::ircparty_WHO
	bind ircparty - LOGIN ::tcldrop::dcc::irc::ircparty_LOGIN
	bind ircparty - LOGON ::tcldrop::dcc::irc::ircparty_LOGON
	bind ircparty - PASSWORD ::tcldrop::dcc::irc::ircparty_PASSWORD
	bind ircparty - PASS ::tcldrop::dcc::irc::ircparty_PASS
	bind ircparty - QUIT ::tcldrop::dcc::irc::ircparty_QUIT
	bind ircparty - MODE ::tcldrop::dcc::irc::ircparty_MODE
	bind ircparty - USERHOST ::tcldrop::dcc::irc::ircparty_USERHOST
	bind ircparty - VERSION ::tcldrop::dcc::irc::ircparty_VERSION
	bind ircparty - ISON ::tcldrop::dcc::irc::ircparty_ISON
	bind ircparty - JOIN ::tcldrop::dcc::irc::ircparty_JOIN
	bind ircparty - PART ::tcldrop::dcc::irc::ircparty_PART
	bind ircparty - NAMES ::tcldrop::dcc::irc::ircparty_NAMES
	bind ircparty - TOPIC ::tcldrop::dcc::irc::ircparty_TOPIC
	bind ircparty - WHOIS ::tcldrop::dcc::irc::ircparty_WHOIS
	bind ircparty - PRIVMSG ::tcldrop::dcc::irc::ircparty_PRIVMSG
	# Don't allow the ircparty module to be unloaded:
	bind unld - ircparty ::tcldrop::dcc::irc::UNLD
	proc ::tcldrop::dcc::irc::UNLD {module} { return 1 }
}
