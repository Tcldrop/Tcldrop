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

# proc by thommey
proc ::tcldrop::bots::dcc::getallbotschildren {} {
	set result [dict create]
	foreach node [bots] {
		dict set result [string tolower $node] [list]
	}
	# now parse the tracepaths to append children to their parent
	foreach node [bots] {
	# every path tells us about the direct parent of a node, the rest is ignored
		dict lappend result [string tolower [lindex [dict get $::bots([set node [string tolower $node]]) tracepath] end]] $node
	}
	return [dict get $result]
}

# proc by thommey
# walk the tree recursively starting with us
proc ::tcldrop::bots::dcc::printtree {idx {version 0} {childrendict {}} {root {}} {indentionlvl -1} {endlvl 0}} {
	if {$root eq {}} { set root ${::botnet-nick} }
	if {$childrendict eq {}} { set childrendict [getallbotschildren] }
	set children [dict get $childrendict [string tolower $root]]
	# output the root of the whole tree
	if {$indentionlvl < 0} {
		if {$version} { putdcc $idx "$root (Tcldrop v[lindex $::version 0])" } else { putdcc $idx $root }
	}
	incr indentionlvl
	set prefix "  "
	for {set i 0} {$i < $indentionlvl} {incr i} {
		if {$i < $endlvl} { append prefix " " } else { append prefix "|" }
		append prefix "    "
	}
	for {set i 0} {$i < [llength $children]} {incr i} {
		set child [lindex $children $i]
		set suffix "[dict get $::bots($child) icon][dict get $::bots($child) handle]"
		if {$version} { append suffix " ([dict get $::bots($child) type] [dict get $::bots($child) numversion])" }; # FixMe: this should display version instead of numversion
		# not last child? "`--", else "|--"
		if {$i < [llength $children]-1} {
			putdcc $idx "$prefix|-$suffix"
		} else {
			putdcc $idx "$prefix`-$suffix"
			incr endlvl
		}
		# and walk on with the recursion for this child
		printtree $idx $version $childrendict $child $indentionlvl $endlvl
	}
}

# proc by thommey
proc ::tcldrop::bots::dcc::avghops {} {
	set totalbots 1; # ourselves, there's no tracepath for us
	set hopstotal 0
	foreach node [bots] {
		set path [dict get $::bots([string tolower $node]) tracepath]
		if {[lindex $path 0] eq ${::botnet-nick}} { set path [lrange $path 1 end] }; # skip us as hop
		incr hopstotal [llength $path]
		incr hopstotal; # but we are a hop, so count us
	}
	return [format %.1f [expr {$hopstotal/(1.0+[llength [bots]])}]]
}


proc ::tcldrop::bots::dcc::BOTTREE {handle idx text} {
	putcmdlog "#$handle# bottree"
	printtree $idx
	putdcc $idx "Total bots: [expr {[llength [bots]]+1}], Avg hops: [avghops]"
}

proc ::tcldrop::bots::dcc::VBOTTREE {handle idx text} {
	putcmdlog "#$handle# vbottree"
	printtree $idx 1
	putdcc $idx "Total bots: [expr {[llength [bots]]+1}], Avg hops: [avghops]"
}

bind load - bots::dcc ::tcldrop::bots::dcc::LOAD -priority 0
proc ::tcldrop::bots::dcc::LOAD {module} {
	bind dcc nmt bots ::tcldrop::bots::dcc::BOTS
	bind dcc nmt +bot ::tcldrop::bots::dcc::+BOT
	bind dcc nmt -bot ::tcldrop::bots::dcc::-BOT
	bind dcc nmt link ::tcldrop::bots::dcc::LINK
	bind dcc nmt bottree ::tcldrop::bots::dcc::BOTTREE
	bind dcc nmt vbottree ::tcldrop::bots::dcc::VBOTTREE
	# FixMe: Add these dcc commands:
	bind dcc nmt botinfo ::tcldrop::bots::dcc::BOTINFO
	bind dcc nmt botattr ::tcldrop::bots::dcc::BOTATTR
	bind dcc nmt unlink ::tcldrop::bots::dcc::UNLINK
}
