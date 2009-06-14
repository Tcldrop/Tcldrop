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
	putdcc $idx "Bots: [join [bots] {, }]."
	putdcc $idx "(Total: [llength [bots]])"
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
