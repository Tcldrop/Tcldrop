# irc/msg --
#	Handles:
#		* All IRC related MSG binds.
#	Depends: irc.
#
# $Id$
#
# Copyright (C) 2005,2006,2007 Tcldrop Development Team <Tcldrop-Dev>
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

namespace eval ::tcldrop::irc::msg {
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable name {irc::msg}
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {irc channels core::users core}
	variable author {Tcldrop-Dev}
	variable description {All IRC related MSG binds.}
	variable rcsid {$Id$}
}


# FixMe: Finish the procs for these....

# -Atlantica- MSG commands for Atlantica:
# -Atlantica-      ADDHOST    INFO       WHO        IDENT
# -Atlantica-      HELP       VOICE      WHOIS      PASS
# -Atlantica-      OP         INVITE     GO         KEY
# -Atlantica-      DIE        JUMP       MEMORY     SAVE
# -Atlantica-      REHASH          RESET           STATUS
# -Atlantica- For help on a command, /MSG Atlantica HELP <command>
# -Atlantica- You are a master.  Many many more commands are
# -Atlantica- available via dcc chat.
# -Atlantica- Admin: Atlantica <FireEgl@Triton>
# -Atlantica- There may also be additional commands provided by other modules.
proc ::tcldrop::irc::msg::HELP {nick host hand text} {
	if {$text eq {}} { set text {help} }
	foreach {f l} [help msg $text] {  if {(($f eq {-} || [matchattr $hand $f]) && $hand ne {*})} { puthelp "NOTICE $nick :[textsubst $hand $l]" } }
	return 0
}

# ADDHOST <password> <hostmask>
proc ::tcldrop::irc::msg::ADDHOST {nick host hand text} {
	if {[set hostmask [lindex [split $text] end]] == {}} {
		# They neglected to specify the hostmask, so just do a regular IDENT instead.
		::tcldrop::irc::msg::IDENT $nick $host $hand $text
	} elseif {![passwdok $hand -] && [passwdok $hand [lindex [split $text] 0]]} {
		if {[addhost $hand $hostmask]} {
			# FixMe: Tell them it was added successfully. (use lang!)
		} else {
			# FixMe: Tell them it wasn't added, (maybe because it already existed).
		}
	} else {
		# FixMe: Tell them they gave a bad password.
	}
}

# INFO <password> [channel] [an info line]
proc ::tcldrop::irc::msg::INFO {nick host hand text} {

}

# WHO <channel>
proc ::tcldrop::irc::msg::WHO {nick host hand text} {

}

# IDENT <password> [nickname]
proc ::tcldrop::irc::msg::IDENT {nick host hand text} {

}

# FixMe: do more error checking & logging
# VOICE <password> <channel>
proc ::tcldrop::irc::msg::VOICE {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]} {
		foreach chan [lrange $text 1 end] { if {[validchan $chan]} { lappend channels $chan } }
		if {![info exists channels]} { set channels [channels] }
		foreach chan $channels { if {[onchan $nick $chan] && [matchattr $hand nmolv|nmolv $chan]} { pushmode $chan +v $nick } }
	}
}

# WHOIS <hand>
proc ::tcldrop::irc::msg::WHOIS {nick host hand text} {

}

# PASS <password>
# PASS <oldpass> <newpass>
proc ::tcldrop::irc::msg::PASS {nick host hand text} {
	if {$hand eq {*}} {
		putcmdlog "(${nick}!${host}) !${hand}! failed PASS (no such user)"
		return 0
	} elseif {![passwdok $hand -] {
		putcmdlog "(${nick}!${host}) !${hand}! failed PASS (already set)"
		puthelp "PRIVMSG $nick :[lang 0x615]";# You already have a password set.
		return 0
	} elseif {([set numArgs [llength [set arg [split $text]]]] == 1 && [string length $text] < 6) || ($numArgs == 2 && [string length [set newPass [lindex $arg 1]]] < 6)} {
		putcmdlog "(${nick}!${host}) !${hand}! failed PASS (too short)"
		puthelp "PRIVMSG $nick :[lang 0x616]";# Please use at least 6 characters.
		return 0
	} elseif {$numArgs == 1} {
		putcmdlog "(${nick}!${host}) !${hand}! PASS ..."
		chpass $hand $text
		puthelp "PRIVMSG $nick :[lang 0x617] $text";# Password set to:
		return 0
	} elseif {$numArgs == 2 && ![passwdok $hand [lindex $arg 0]]
		putcmdlog "(${nick}!${host}) !${hand}! failed PASS (old password incorrect)"
		puthelp "PRIVMSG $nick :[lang 0x618]";# Incorrect password.
		# return 0
	} elseif {$numArgs == 2} {
		putcmdlog "(${nick}!${host}) !${hand}! PASS ..."
		chpass $hand $newPass
		puthelp "PRIVMSG $nick :[lang 0x619] $newPass";# Password changed to:
		return 0
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed PASS"
		puthelp "PRIVMSG $nick :Usage: /msg $::botnick \[oldpass\] <newpass>"
		return 0
	}
}

# FixMe: do more error checking & logging
# OP <password> [channel]
proc ::tcldrop::irc::msg::OP {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]} {
		foreach c [lrange $text 1 end] { if {[validchan $c]} { lappend channels $c } }
		if {![info exists channels]} { set channels [channels] }
		foreach c $channels { if {[onchan $nick $c] && [matchattr $hand o|o $c]} { pushmode $c +o $nick } }
	}
}

# INVITE <password> <channel>
proc ::tcldrop::irc::msg::INVITE {nick host hand text} {
	if {(![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]) && (([validchan [set chan [lindex $text 1]]] && [botonchan $chan]) && ([botisop $chan] || [botishalfop $chan]))} {
		putcmdlog "(${nick}!${host}) !${hand}! INVITE $chan"
		putserv "INVITE $nick $chan"
		return 0
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed INVITE"
		return 0
	}
}

# GO <channel>
proc ::tcldrop::irc::msg::GO {nick host hand text} {
	if {$hand ne {*}} {
		if {[set chan [lindex $text 1]] eq {}} {
			putcmdlog "(${nick}!${host}) !${hand}! failed GO"
			puthelp "NOTICE $nick :[lang 0x001]: /msg $::botnick go <channel>";# Usage
			return 0
		} elseif {![channel exists $chan]} {
			putcmdlog "(${nick}!${host}) !${hand}! failed GO $chan (no such channel)"
			return 0
		} elseif {[channel get $chan inactive]} {
			putcmdlog "(${nick}!${host}) !${hand}! failed GO $chan (channel is +inactive)"
			return 0
		} elseif {![validchan $chan] || ![botonchan $chan]} {
			putcmdlog "(${nick}!${host}) !${hand}! failed GO $chan (i'm blind)"
			return 0
		# FixMe: do we want to check for halfop here?
		} elseif {[botisop $chan] || [botishalfop $chan]} {
			putcmdlog "(${nick}!${host}) !${hand}! failed GO $chan (i'm chop)"
			return 0
		} else {
			putcmdlog "(${nick}!${host}) !${hand}! GO $chan"
			putserv "PART $chan"
			return 0
		}
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed GO"
		return 0
	}
}

# KEY <password> <channel>
proc ::tcldrop::irc::msg::KEY {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]} {
		if {![botonchan [set chan [lindex $text 1]]]} {
			puthelp "NOTICE $nick :[lang 0x001]: /MSG $::botnick key <pass> <channel>";# Usage
			putcmdlog "(${nick}!${host}) !${hand}! failed KEY"
			return 0
		} else {
			# FixMe: This should only return the key, fix when we figure out a way to parse RAW 005.
			puthelp "NOTICE $nick :${chan} modes: [getchanmode $chan]"
			# puthelp "NOTICE $nick :${chan}: key is $key"
			# puthelp "NOTICE $nick :${chan}: no key set for this channel"
			putcmdlog "(${nick}!${host}) !${hand}! KEY $chan"
			return 0
		}
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed KEY"
		return 0
	}
}

# DIE <password> [message]
proc ::tcldrop::irc::msg::DIE {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]} {
		set CmdLog ""
		if {[set message [lrange $text 1 end]] ne {}} { append CmdLog " $message" } else { append CmdLog " [set message {nyoooooooo...}]" }
		putcmdlog "(${nick}!${host}) !${hand}! DIE${CmdLog}"
		puthelp "NOTICE $nick :[lang 0xb18]";# Bot shut down beginning....
		die $message
		return 0
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed DIE"
		return 0
	}
}

# JUMP <password> [server [port [server password]]]
proc ::tcldrop::irc::msg::JUMP {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]} {
		lassign $text - server port password
		set CmdLog ""
		if {$server ne {}} {
			append CmdLog " $server"
			if {$port ne {}} {
				append CmdLog " $port"
				if {$password ne {}} {
					append CmdLog " ..."
				}
			}
		}
		putcmdlog "(${nick}!${host}) !${hand}!JUMP${CmdLog}"
		puthelp "NOTICE $nick :[lang 0x62b]";# Jumping servers...
		# there's no danger in calling jump with empty args
		jump $server $port $password
		return 0
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed JUMP"
		return 0
	}
}

# FixMe: Either figure out a way to know how much memory we're using or remove this command, or return some other related statistic
# MEMORY <password>
proc ::tcldrop::irc::msg::MEMORY {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [split $text] 0]]} {
		puthelp "NOTICE $nick :I've no idea how much memory I'm using, but I'm happy anyway!"
		return 1
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed MEMORY"
		return 0
	}
}

# SAVE <password>
proc ::tcldrop::irc::msg::SAVE {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [split $text] 0]]} {
		puthelp "NOTICE $nick :Saving user file..."
		save
		return 1
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed SAVE"
		return 0
	}
}

# REHASH <password>
proc ::tcldrop::irc::msg::REHASH {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [split $text] 0]]} {
		puthelp "NOTICE $nick :[lang 0x40f]"; # Rehashing...
		rehash
		return 1
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed REHASH"
		return 0
	}
}

# RESET <password> [channel]
proc ::tcldrop::irc::msg::RESET {nick host hand text} {
	# don't check for botonchan, since that might return 0 on a desynch
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]] && [channel exists [set chan [lindex $text 1]]]} {
		putcmdlog "(${nick}!${host}) !${hand}! RESET $chan"
		# reply put in server queue or Tcldrop won't reply until after resetchan sends /who /topic etc
		putserv "NOTICE $nick :[lang 0x62a]"; # Resetting channel info.
		resetchan $chan
		return 0
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed RESET"
		return 0
	}
}

# STATUS <password>
proc ::tcldrop::irc::msg::STATUS {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [split $text] 0]]} {
		puthelp "NOTICE $nick :I am ${::botnet-nick}, running Tcldrop v$::tcldrop(version): [countusers] users."
		puthelp "NOTICE $nick :Online for [duration [expr { [clock seconds] - $::uptime }]]"
		puthelp "NOTICE $nick :Admin: $::owner"
		puthelp "NOTICE $nick :OS: $::tcl_platform(os) $::tcl_platform(osVersion)"
		puthelp "NOTICE $nick :Online as: $::botname ($::realname)"
		puthelp "NOTICE $nick :Channels: [join [channels] {, }]"; # FixMe: split this into several lines if it's too long
		return 1
	} else {
		putcmdlog "(${nick}!${host}) !${hand}! failed STATUS"
		return 0
	}
}

bind load - irc::msg ::tcldrop::irc::msg::LOAD -priority 1
proc ::tcldrop::irc::msg::LOAD {module} {
	bind msg -|- help ::tcldrop::irc::msg::HELP -priority 100
	bind msg n|n addhost ::tcldrop::irc::msg::ADDHOST
	bind msg -|- info ::tcldrop::irc::msg::INFO
	bind msg -|- who ::tcldrop::irc::msg::WHO
	bind msg -|- ident ::tcldrop::irc::msg::IDENT
	bind msg -|- voice ::tcldrop::irc::msg::VOICE
	bind msg -|- whois ::tcldrop::irc::msg::WHOIS
	bind msg -|- pass ::tcldrop::irc::msg::PASS
	bind msg o|o op ::tcldrop::irc::msg::OP
	bind msg -|- invite ::tcldrop::irc::msg::INVITE
	bind msg -|- go ::tcldrop::irc::msg::GO
	bind msg o|o key ::tcldrop::irc::msg::KEY
	bind msg n|- die ::tcldrop::irc::msg::DIE
	bind msg n|- jump ::tcldrop::irc::msg::JUMP
	bind msg n|- memory ::tcldrop::irc::msg::MEMORY
	bind msg m|n save ::tcldrop::irc::msg::SAVE
	bind msg n|- rehash ::tcldrop::irc::msg::REHASH
	bind msg m|m reset ::tcldrop::irc::msg::RESET
	bind msg m|m status ::tcldrop::irc::msg::STATUS
	loadhelp [file join msg irc.help] msg
}

bind unld - irc::msg ::tcldrop::irc::msg::UNLD -priority 1
proc ::tcldrop::irc::msg::UNLD {module} {
	unbind msg * * ::tcldrop::irc::msg::*
	unloadhelp [file join msg irc.help]
	return 0
}
