# bots/dcc.tcl --
#	Handles:
#		* Bot related DCC commands.
#	Depends: bots.
#
# $Id$
#
# Copyright (C) 2005,2006,2007 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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

namespace eval ::tcldrop::bots::dcc {
	variable name {bots::dcc}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {bots core::users core::dcc core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Bot related DCC commands.}
	variable rcsid {$Id$}
	variable commands [list]
	# Pre-depends on the channels module:
	checkmodule bots
}

proc ::tcldrop::bots::dcc::BOTS {handle idx text} {
	putcmdlog "#$handle# bots"
	putdcc $idx "Bots: [join [set bots [bots]] {, }]."
	putdcc $idx "(Total: [llength $bots])"
	return 0
}

proc ::tcldrop::bots::dcc::+BOT {handle idx text} {
	set bot [::tcldrop::slindex $text 0]
	set address [::tcldrop::slindex $text 1]
	set host [::tcldrop::slindex $text 2]
	addbot $bot $address
	addhost $bot $host
	putcmdlog "#$handle# +bot $text"
	putdcc $idx "Added bot '$bot' with address '$address' and hostmask '$host'"
	return 0
}

proc ::tcldrop::bots::dcc::-BOT {handle idx text} {
	if {[matchattr $text b]} {
		-USER $handle $idx $text
	}
	return 0
}

proc ::tcldrop::bots::dcc::LINK {handle idx text} {
	if {[matchattr $text b]} {
		putcmdlog "#$handle# link $text"
		putdcc $idx "Attempting to link to $text..."
		link $text
	}
	return 0
}

proc ::tcldrop::bots::dcc::BOTTREE {handle idx text} {
	# dict get $::bots(saffron) icon
	# dict get $::bots(saffron) tracepath
	foreach bot [bots] { lappend bots [string tolower $bot] }
	set botCount [llength $bots]
	putloglev d * "Bots: $bots"
	lappend botOrder ${::botnet-nick}
	set currentBot 0
	set match 0
	set loops 0; # remove once this works
	while [set len [llength $bots]] {; # <- why doesn't this check work?!
		# putloglev d * "bots len: $len"
		if {[llength $bots] <= 0} { break }; # Fix for the while check not working
		incr loops; # remove once this works
		if {$loops >= 50 } { putloglev d * "Caught infinite loop, breaking"; break }; # remove once this works
		putloglev d * "while. botOrder: $botOrder, currentBot: [lindex $botOrder $currentBot]"
		set pos 0
		foreach bot $bots {
			putloglev d * "  foreach. bot: <${bot}>, pos: $pos, tracepath: [dict get $::bots($bot) tracepath]"
			if {[string equal -nocase [lindex $botOrder $currentBot] [lindex [dict get $::bots($bot) tracepath] end]]} {
				lappend botOrder $bot
				set currentBot [expr {[llength $botOrder]-1}]
				set bots [lreplace $bots $pos $pos]
				putloglev d * "    found match. botOrder: $botOrder, bots: $bots"
				set match 1; break
			}
			incr pos
		}
		if {!$match} { incr currentBot; if {[expr {$currentBot+1}] > [llength $botOrder]} { set currentBot 0 } } else { set match 0 }
		# putloglev d * "  bots: \"${bots}\" llength: [llength $bots]"
	}
	set indent(${::botnet-nick}) 0
	foreach bot [lrange $botOrder 1 end] {
		set indent($bot) [expr {$indent([lindex [dict get $::bots($bot) tracepath] end]) + 1}]
	}
	# Output
	putdcc $idx "${::botnet-nick}"
	foreach bot [lrange $botOrder 1 end] {
		putdcc $idx "[string repeat "  " $indent($bot)]  [dict get $::bots($bot) icon]${bot}"
		# putdcc $idx "<${bot}> [dict get $::bots($bot) tracepath] $indent($bot)"
	}
	# putdcc $idx $botOrder
}

proc ::tcldrop::bots::dcc::VBOTTREE {handle idx text} {
	return 0
}

bind load - bots::dcc ::tcldrop::bots::dcc::LOAD -priority 0
proc ::tcldrop::bots::dcc::LOAD {module} {
	bind dcc nmt bots ::tcldrop::bots::dcc::BOTS
	bind dcc nmt +bot ::tcldrop::bots::dcc::+BOT
	bind dcc nmt -bot ::tcldrop::bots::dcc::-BOT
	bind dcc nmt link ::tcldrop::bots::dcc::LINK
	# FixMe: Add these dcc commands:
	bind dcc nmt botinfo ::tcldrop::bots::dcc::BOTINFO
	bind dcc nmt bottree ::tcldrop::bots::dcc::BOTTREE
	bind dcc nmt vbottree ::tcldrop::bots::dcc::VBOTTREE
	bind dcc nmt botattr ::tcldrop::bots::dcc::BOTATTR
	bind dcc nmt unlink ::tcldrop::bots::dcc::UNLINK
}
