# terminalparty.tcl --
#	Handles:
#		* Provides the telnet interface for users to access the bot.
#	Depends: partyline.
#
# $Id: terminalparty.tcl,v 1.6 2005/05/17 08:19:55 fireegl Exp $
#
# Copyright (C) 2003,2004,2005 FireEgl (Philip Moore) <FireEgl@Tcldrop.Org>
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
#
#	terminalparty module for tcldrop.  (REQUIRED)
#
# Note: support for stdin/stdout is also in this module, so it's REQUIRED if you want to use -n -t command line options to login to the bot.

namespace eval ::tcldrop::terminalparty {
	variable version {0.1}
	variable name {terminalparty}
	variable depends {partyline core::conn core}
	variable author {Tcldrop-Dev}
	variable description {The telnet interface for users to access the bot.}
	variable script [info script]
	variable rcsid {$Id: terminalparty.tcl,v 1.6 2005/05/17 08:19:55 fireegl Exp $}
	variable commands [list]
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the partyline module:
	checkmodule partyline
}

# Simulate a telnet/dcc on stdin/stdout:
bind evnt - start ::tcldrop::terminalparty::start
proc ::tcldrop::terminalparty::start {event} {
	if {$::tcldrop(simulate-dcc) && !$::tcldrop(background-mode)} {
		# Turn the console into a simulated DCC session:
		fconfigure stdout -buffering line -blocking 0
		fconfigure stdin -buffering line -blocking 0
		fileevent stdout writable [list ::tcldrop::terminalparty::Write [set idx [assignidx]]]
		fileevent stdin readable [list ::tcldrop::terminalparty::ConsoleRead $idx]
		# Note: Under the right conditions, this logs the person in automatically as the first owner in the $owner setting.
		#if {[set handle [lindex [split $::owner {,}] 0]] == {} || ![validuser $handle] || ![matchattr $handle n] || [passwdok $handle -]} {
		#	set handle {HQ}
		#}
		# Special proc for the console (tclsh/stdin):
		proc ::tcldrop::terminalparty::ConsoleRead {idx} {
			while {[gets stdin line] >= 0} {
				::tcldrop::telnetparty::Read $idx $line
			}
			if {[eof stdin]} { catch { close stdin } }
			if {[eof stdout]} { catch { close stdout } }
			if {[eof stderr]} { catch { close stderr } }
		}
		proc ::tcldrop::terminalparty::Write {idx} {
			fileevent stdout writable {}
			registeridx $idx [list idx $idx sock stdout handle * ident User hostname Console port 0 remote User@Console state TELNET_ID info {Console} other {t-in} timestamp [clock seconds] traffictype partyline nonewline 1 module terminalparty]
			putdcc $idx {### ENTERING DCC CHAT SIMULATION ###}
			::tcldrop::telnetparty::Write $idx
		}

		# This proc isn't used, it's here as a reminder that ::tcldrop::telnetparty::Read is used instead:
		proc ::tcldrop::terminalparty::Read {idx line} { ::tcldrop::telnetparty::Read $idx $line }
		# Turn off logging to PutLogLev, because it's a dcc session now, not a screen:
		unbind log - * ::tcldrop::PutLogLev
		fconfigure stderr -buffering line -blocking 0
	}
}

proc ::tcldrop::terminalparty::EVNT_init {event} {
	if {$::tcldrop(host_env) == {wish}} {
		proc ::tcldrop::stdin {text} { ::tcldrop::telnetparty::Read 1 $text }
		registeridx 1 [list idx 1 sock stdout filter ::tcldrop::terminalparty::IDXFilter handle * ident User hostname Console port 1 remote User@Console state TELNET_ID other {t-in} timestamp [clock seconds] traffictype partyline nonewline 1 module terminalparty]
		putdcc 1 {### ENTERING DCC CHAT SIMULATION ###}
		::tcldrop::telnetparty::Write 1
	}
}

# This is used when we're running in wish:
proc ::tcldrop::terminalparty::IDXFilter {idx text args} { if {[catch { stdout $text }]} { return $text } }

bind load - terminalparty ::tcldrop::terminalparty::LOAD -priority 0
proc ::tcldrop::terminalparty::LOAD {module} {
	bind evnt - init ::tcldrop::terminalparty::EVNT_init -priority 10000
	# Don't allow the module to unload:
	bind unld - terminalparty ::tcldrop::terminalparty::UNLD
	proc ::tcldrop::terminalparty::UNLD {module} { return 1 }
}

