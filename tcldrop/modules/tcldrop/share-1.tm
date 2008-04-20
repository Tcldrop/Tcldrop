# share/main --
#	Handles:
#		* Provides the ability to share anything with other bots.
#
# $Id$
#
# Copyright (C) 2003-2008 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
# Or can be found on IRC (EFNet, FreeNode, or OFTC) as FireEgl.
#
# This uses the database module, so basically anything set in the database can be shared with other bots.

namespace eval ::tcldrop::share {
	variable name {share}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	if {![info exists ::tcldrop]} { return }
	variable depends {core}
	variable author {Tcldrop-Dev}
	variable description {Provides the ability to share anything with other bots.}
	variable rcsid {$Id$}
	variable commands [list share callshare]
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
}

proc ::tcldrop::share::callshare {type command data {options {}}} {
	set count 0
	foreach {bindtype flags mask proc} [bindlist share] {
		if {[string match -nocase $mask $type $options]} {
			if {[catch { $proc $type $command $data } err]} {
				putlog "Error in script: $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $bindtype $mask $proc
			incr count
		}
	}
	return $count
}

proc ::tcldrop::share::share {type command {arguments {}} {options {}}} {
	array set opts [list -share 0]
	array set opts $options
	if {$opts(-share)} {
		callshare $type $command $arguments $options
	} else {
		return 0
	}
}

bind load - share ::tcldrop::share::LOAD -priority 10
proc ::tcldrop::share::LOAD {module} {
	bind database - * ::tcldrop::share::share -priority 10000
	loadhelp share.help
	loadhelp [file join set share.help]
}

bind unld - share ::tcldrop::share::UNLD -priority 10
proc ::tcldrop::tcldrop::share::UNLD {module} {
	unbind database - * ::tcldrop::share::share
	unloadhelp share.help
	unloadhelp [file join set share.help]
	return 0
}
