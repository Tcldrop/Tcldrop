# irc/msg.tcl --
#	Handles:
#		* All IRC related MSG binds.
#	Depends: irc.
#
# $Id: msg.tcl,v 1.2 2005/04/25 08:09:47 fireegl Exp $
#
# Copyright (C) 2005 Tcldrop Development Team <Tcldrop-Devel>
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
# The author of this project can be reached at FireEgl@Tcldrop.Org
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.

namespace eval ::tcldrop::irc::msg {
	variable version {0.1}
	variable script [info script]
	variable name {irc::msg}
	variable depends {irc channels core::users core}
	variable author {Tcldrop-Dev}
	variable description {All IRC related MSG binds.}
	variable rcsid {$Id: msg.tcl,v 1.2 2005/04/25 08:09:47 fireegl Exp $}
	# Provide the irc::msg module:
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
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
bind msg -|- help ::tcldrop::irc::msg::HELP -priority 100
proc ::tcldrop::irc::msg::HELP {nick host hand text} {
	if {![string equal {*} $hand]} {
		puthelp "NOTICE $nick :No help, yet."
	}
}

# ADDHOST <password> <hostmask>
bind msg n|n addhost ::tcldrop::irc::msg::ADDHOST
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
bind msg -|- info ::tcldrop::irc::msg::INFO
proc ::tcldrop::irc::msg::INFO {nick host hand text} {

}

# WHO <channel>
bind msg -|- who ::tcldrop::irc::msg::WHO
proc ::tcldrop::irc::msg::WHO {nick host hand text} {

}

# IDENT <password> [nickname]
bind msg -|- ident ::tcldrop::irc::msg::IDENT
proc ::tcldrop::irc::msg::IDENT {nick host hand text} {

}

# VOICE <password> <channel>
bind msg -|- voice ::tcldrop::irc::msg::VOICE
proc ::tcldrop::irc::msg::VOICE {nick host hand text} {

}

# WHOIS <hand>
bind msg -|- whois ::tcldrop::irc::msg::WHOIS
proc ::tcldrop::irc::msg::WHOIS {nick host hand text} {

}

# PASS <password>
bind msg -|- pass ::tcldrop::irc::msg::PASS
proc ::tcldrop::irc::msg::PASS {nick host hand text} {

}

# OP <password> [channel]
bind msg o|o op ::tcldrop::irc::msg::OP
proc ::tcldrop::irc::msg::OP {nick host hand text} {
	if {![passwdok $hand -] && [passwdok $hand [lindex [set text [split $text]] 0]]} {
		foreach c [lrange $text 1 end] { if {[validchan $c]} { lappend channels $c } }
		if {![info exists channels]} { set channels [channels] }
		foreach c $channels { if {[onchan $nick $c] && [matchattr $hand o|o $c]} { pushmode $c +o $nick } }
	}
}

# INVITE <password> <channel>
bind msg -|- invite ::tcldrop::irc::msg::INVITE
proc ::tcldrop::irc::msg::INVITE {nick host hand text} {

}

# GO <channel>
bind msg -|- go ::tcldrop::irc::msg::GO
proc ::tcldrop::irc::msg::GO {nick host hand text} {

}

# KEY <password> <channel>
bind msg o|o key ::tcldrop::irc::msg::KEY
proc ::tcldrop::irc::msg::KEY {nick host hand text} {

}

# DIE <password> [message]
bind msg n|- die ::tcldrop::irc::msg::DIE
proc ::tcldrop::irc::msg::DIE {nick host hand text} {

}

# JUMP <password> [server [port [server password]]]
bind msg n|- jump ::tcldrop::irc::msg::JUMP
proc ::tcldrop::irc::msg::JUMP {nick host hand text} {

}

# MEMORY <password>
bind msg n|- memory ::tcldrop::irc::msg::MEMORY
proc ::tcldrop::irc::msg::MEMORY {nick host hand text} {

}

# SAVE <password>
bind msg m|n save ::tcldrop::irc::msg::SAVE
proc ::tcldrop::irc::msg::SAVE {nick host hand text} {

}

# REHASH <password>
bind msg n|- rehash ::tcldrop::irc::msg::REHASH
proc ::tcldrop::irc::msg::REHASH {nick host hand text} {

}

# RESET <password> [channel]
bind msg m|m reset ::tcldrop::irc::msg::RESET
proc ::tcldrop::irc::msg::RESET {nick host hand text} {

}

# STATUS <password>
bind msg m|m status ::tcldrop::irc::msg::STATUS
proc ::tcldrop::irc::msg::STATUS {nick host hand text} {

}

bind - load irc::msg ::tcldrop::irc::msg::LOAD -priority 1
proc ::tcldrop::irc::msg::LOAD {module} {
	loadhelp [file join msg irc.help]
	bind - unld irc::msg ::tcldrop::irc::msg::UNLD -priority 1
}

proc ::tcldrop::irc::msg::UNLD {module} {
	unloadhelp [file join msg irc.help]
	return 0
}
