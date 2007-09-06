# irc/dcc.tcl --
#	Handles:
#		* All IRC related DCC binds.
#	Depends: irc.
#
# $Id: dcc.tcl,v 1.2 2005/04/25 08:09:46 fireegl Exp $
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

namespace eval ::tcldrop::irc::dcc {
	variable version {0.1}
	variable script [info script]
	variable name {irc::dcc}
	variable depends {irc channels core::dcc core}
	variable author {Tcldrop-Dev}
	variable description {All IRC related DCC binds.}
	variable rcsid {$Id: dcc.tcl,v 1.2 2005/04/25 08:09:46 fireegl Exp $}
	# Provide the irc::dcc module:
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
}

proc ::tcldrop::irc::dcc::STATUS {handle idx text} {
	if {${::server-online}} {
		putdcc $idx "    Online as: $::botname ($::realname)"
		putdcc $idx "    Server: $::serveraddress (connected for [duration [expr { [clock seconds] - ${::server-online} }]])"
		foreach chan [channels] {
			if {[botonchan $chan]} {
				set chansets [list]
				if {[botisop $chan]} {
					if {[set chanmode [channel get $chan chanmode]] != {}} { lappend chansets "enforcing \"$chanmode\"" }
					if {[channel get $chan autoop]} { lappend chansets {+autoop} }
					if {[channel get $chan autovoice]} { lappend chansets {+autovoice} }
					if {[channel get $chan bitch]} { lappend chansets {+bitch} }
					if {[channel get $chan enforcebans]} { lappend chansets {+enforcebans} }
					if {[channel get $chan protectops]} { lappend chansets {+protectops} }
					if {[channel get $chan protectfriends]} { lappend chansets {+protectfriends} }
					if {[channel get $chan nodesynch]} { lappend chansets {+nodesynch} }
				}
				if {[channel get $chan revenge]} { lappend chansets {+revenge} }
				if {[channel get $chan revengebot]} { lappend chansets {+revengebot} }
				if {[channel get $chan greet]} { lappend chansets {+greet} }
				if {[channel get $chan seen]} { lappend chansets {+seen} }
				if {[llength $chansets] == 0} { lappend chansets {(lurking)} }
				set status "[llength [chanlist $chan]] members, [join $chansets {, }]"
			} elseif {[channel get $chan inactive]} {
				set status {Channel is set +inactive}
			} elseif {[ischanjuped $chan]} {
				set status {Juped!}
			} else {
				set status {(not on channel)}
			}
			putdcc $idx "    [format {%-20.32s %-2.2s %-40.40s} $chan : $status]"
		}
	} else {
		putdcc $idx "    Server: $::serveraddress (trying to connect)"
		putdcc $idx "    Channels: [join [channels] {, }]"
	}
	return 0
}
bind dcc n status ::tcldrop::irc::dcc::STATUS -priority 3
bind dcc n stat ::tcldrop::irc::dcc::STATUS -priority 3

bind load - irc::dcc ::tcldrop::irc::dcc::LOAD -priority 10
proc ::tcldrop::irc::dcc::LOAD {module} {
	loadhelp irc.help
	bind unld - irc::dcc ::tcldrop::irc::dcc::UNLD -priority 10
}

proc ::tcldrop::irc::dcc::UNLD {module} {
	unbind dcc * * ::tcldrop::irc::dcc::*
	unloadhelp irc.help
	return 0
}
