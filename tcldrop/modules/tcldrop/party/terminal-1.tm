# party/terminal.tcl --
#	Handles:
#		* Provides the terminal (console) interface for users to access the bot.
#	Depends: party.
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
#	party/terminal module for tcldrop.  (REQUIRED)
#
# Note: support for stdin/stdout is also in this module, so it's REQUIRED if you want to use -n -t command line options to login to the bot.

namespace eval ::tcldrop::party::terminal {
	variable name {party::terminal}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {party core::conn core}
	variable author {Tcldrop-Dev}
	variable description {The terminal interface for users to access the bot.}
	variable rcsid {$Id$}
	variable commands [list]
	# Pre-depends on the partyline module:
	checkmodule party
}

# Simulate a telnet/dcc on stdin/stdout:
bind evnt - start ::tcldrop::party::terminal::start
proc ::tcldrop::party::terminal::start {event} {
	if {$::tcldrop(simulate-dcc) && !$::tcldrop(background-mode)} {
		# Turn the console into a simulated DCC session:
		fconfigure stdout -buffering line -blocking 0
		fconfigure stdin -buffering line -blocking 0
		fileevent stdout writable [list ::tcldrop::party::terminal::Write [set idx [assignidx]]]
		fileevent stdin readable [list ::tcldrop::party::terminal::ConsoleRead $idx]
		# Note: Under the right conditions, this logs the person in automatically as the first owner in the $owner setting.
		#if {[set handle [lindex [split $::owner {,}] 0]] == {} || ![validuser $handle] || ![matchattr $handle n] || [passwdok $handle -]} {
		#	set handle {HQ}
		#}
		# Special proc for the console (tclsh/stdin):
		proc ::tcldrop::party::terminal::ConsoleRead {idx} {
			while {[gets stdin line] >= 0} {
				::tcldrop::party::telnet::Read $idx $line
			}
			if {[eof stdin]} { catch { close stdin } }
			if {[eof stdout]} { catch { close stdout } }
			if {[eof stderr]} { catch { close stderr } }
		}
		proc ::tcldrop::party::terminal::Write {idx} {
			fileevent stdout writable {}
			registeridx $idx [list idx $idx sock stdout handle * ident User hostname Console port 0 remote User@Console state TELNET_ID info {Console} other {t-in} timestamp [clock seconds] traffictype partyline nonewline 1 module party::terminal]
			putdcc $idx {### ENTERING DCC CHAT SIMULATION ###}
			::tcldrop::party::telnet::Write $idx
		}

		# This proc isn't used, it's here as a reminder that ::tcldrop::party::telnet::Read is used instead:
		proc ::tcldrop::party::terminal::Read {idx line} { ::tcldrop::party::telnet::Read $idx $line }
		# Turn off logging to PutLogLev, because it's a dcc session now, not a screen:
		unbind log - * ::tcldrop::PutLogLev
		fconfigure stderr -buffering line -blocking 0
	}
}

proc ::tcldrop::party::terminal::EVNT_init {event} {
	if {$::tcldrop(host_env) == {wish}} {
		proc ::tcldrop::stdin {text} { ::tcldrop::party::telnet::Read 1 $text }
		registeridx 1 [list idx 1 sock stdout filter ::tcldrop::party::terminal::IDXFilter handle * ident User hostname Console port 1 remote User@Console state TELNET_ID other {t-in} timestamp [clock seconds] traffictype partyline nonewline 1 module party::terminal]
		putdcc 1 {### ENTERING DCC CHAT SIMULATION ###}
		::tcldrop::party::telnet::Write 1
	}
}

# This is used when we're running in wish:
proc ::tcldrop::party::terminal::IDXFilter {idx text args} { if {[catch { stdout $text }]} { return $text } }

bind load - party::terminal ::tcldrop::party::terminal::LOAD -priority 0
proc ::tcldrop::party::terminal::LOAD {module} {
	bind evnt - init ::tcldrop::party::terminal::EVNT_init -priority 10000
	# Don't allow the module to unload:
	bind unld - party::terminal ::tcldrop::party::terminal::UNLD
	proc ::tcldrop::party::terminal::UNLD {module} { return 1 }
}

