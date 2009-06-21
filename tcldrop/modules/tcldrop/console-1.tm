# console/main --
#	Handles:
#		* Provides commands for getting/setting console infos.
#	Depends: core::users, core::dcc.
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

namespace eval ::tcldrop::console {
	variable version {0.5}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable name {console}
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {channels core::users core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Provides commands for getting/setting console infos.}
	variable {$Id$}
	variable commands [list getconsole setconsole echo strip console store initconsole]
	variable script [info script]
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
}

# Stores the console data in the userfile for $idx:
proc ::tcldrop::console::store {idx} {
	array set idxinfo [getidxinfo $idx]
	if {[info exists idxinfo(handle)]} {
		setuser $idxinfo(handle) console [array get idxinfo {console-*}]
		return 1
	} else {
		return 0
	}
}

#  console <idx> [channel] [console-modes]
#    Description: changes a dcc user's console mode, either to an absolute
#      mode (like "mpj") or just adding/removing flags (like "+pj" or "-moc"
#      or "+mp-c"). The user's console channel view can be changed also (as
#      long as the new channel is a valid channel).
#    Returns: a list containing the user's (new) channel view and (new)
#      console modes, or nothing if that user isn't currently on the partyline
#    Module: core
# Note: Eggdrop can take the channel and console-modes arguments in any order, so we'll do the same.
# FixMe: Add support for setting console idx * which should turn on everything and -* which turns off everything
proc ::tcldrop::console::console {idx args} {
	foreach whatever $args {
		if {![validchan $whatever]} {
			switch -- [string index $whatever 0] {
				{ } {}
				{#} - {&} - {!} - {@} - {$} - {%} - {^} - {[} - {]} - {:} - "\"" - "\{" - "\}" { return -code error "Invalid channel/flag: $whatever" }
				{default} {
					# Assuming that we're dealing with levels.
					set levels {}
					set handle [idx2hand $idx]
					foreach c [split [mergeflags $whatever [getconsole $idx levels]] {}] {
						if {[info exists ::console-levels($c)] && [matchattr $handle [set ::console-levels($c)]]} {
							append levels $c
						}
					}
					setconsole $idx levels $levels
				}
			}
		#} elseif {$whatever == {*} || $whatever == {-} || [validchan $whatever] || [haschanrec [idx2hand $idx] $whatever]} {
		} else {
			# Note: * means all channels, - means no channels.
			# They want to change their IRC console channel..
			setconsole $idx channel $whatever
		}
	}
	list [getconsole $idx channel] [getconsole $idx levels]
}

# Gets the console settings for a user from the userfile (if possible),
# otherwise it uses defaults, and puts it in the idxinfo:
proc ::tcldrop::console::initconsole {idx} {
	# Note: console-chan = partyline channel
	#    console-channel = irc channel
	set console [dict create console-echo 0 console-channel - console-levels $::console console-strip - console-chan 0 console-page 0]
	catch { set console [dict merge $console [getuser [set handle [idx2hand $idx]] console]] }
	set levels {}
	foreach c [split [dict get $console console-levels] {}] {
		if {[info exists ::console-levels($c)] && [matchattr $handle [set ::console-levels($c)]]} {
			append levels $c
		}
	}
	dict set console console-levels $levels
	setidxinfo $idx $console
}

#  echo <idx> [status]
#    Description: turns a user's echo on or off; the status has to be a 1 or 0
#    Returns: new value of echo for that user (or the current value, if status
#      was omitted)
#    Module: core
proc ::tcldrop::console::echo {idx {status {-1}}} {
	switch -- $status {
		{0} - {-} - {off} - {no} - {n} - {N} { setconsole $idx echo 0 }
		{1} - {+} - {on} - {yes} - {y} - {Y} { setconsole $idx echo 1 }
		{-1} - {default} { getconsole $idx echo }
	}
}

#  enables you to remove embedded 'attribute' codes from within a
#   section of text. Valid options are:
#       b  remove all boldface codes
#       c  remove all color codes
#       r  remove all reverse video codes
#       u  remove all underline codes
#       a  remove all ansi codes
#       g  remove all ctrl-g (bell) codes
#    the mode can also be a modifier like '+c' or '-bu' or '+ru-c'.  if
#    you omit modes, it will show your current setting.
#  strip <idx> [+/-strip-flags]
#    Description: modifies the strip-flags for a user
#    Returns: new strip-flags for the specified user (or the current
#      flags, if strip-flags was omitted)
#    Module: core
proc ::tcldrop::console::strip {idx {flags {}}} {
	if {$flags != {}} {
		setconsole $idx strip [mergeflags $flags [getconsole $idx strip]]
	} else {
		getconsole $idx strip
	}
}

proc ::tcldrop::console::getconsole {idx setting} {
	if {![catch { idxinfo $idx console-$setting } value]} {
		return $value
	} else {
		return -code error "No such console setting: \"$setting\""
	}
}

proc ::tcldrop::console::setconsole {idx setting value} {
	setidxinfo $idx [list console-$setting $value]
	return $value
}

bind load - console ::tcldrop::console::LOAD -priority 0
proc ::tcldrop::console::LOAD {module} {
	setdefault console {ocmkbxs}
	setdefault console-autosave 1
	setdefault force-channel 0
	variable Levels
	# Define the log levels, and the user-flags that are required to see logs sent to that level.
	# if new log levels are added, they need to be added to the .console dcc command as well.
	array set Levels {
		c n
		o mnt
		x jn
		d n
		r n
		v n
		m n
		p omn
		k omn
		j omn
		b nmt
		h n
		t n
		s n
		w n
		e n
		1 n
		2 n
		3 n
		4 n
		5 n
		6 n
		7 n
		8 n
		9 n
		0 n
		* n
		- n
		+ n
	}
	if {![info exists ::console-levels]} { array set ::console-levels {} }
	array set ::console-levels [concat [array get Levels] [array get ::console-levels]]
	checkmodule console::dcc
}

bind unld - console ::tcldrop::console::UNLD -priority 0
proc ::tcldrop::console::UNLD {module} {
	unloadmodule console::dcc
	return 0
}
