# encryption/main --
#	Handles:
#		* core encryption support.
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
# This module provides the encpass, encrypt, and decrypt Tcl commands.

namespace eval ::tcldrop::encryption {
	variable version {0.1}
	variable name {encryption}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	variable depends {core::main}
	variable author {Tcldrop-Dev}
	variable description {Provides the core encryption support.}
	variable rcsid {$Id$}
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export encpass Encpass encrypt Encrypt decrypt Decrypt
	variable commands [namespace export]
}

# ToDo/FixMe: Make this a namespace ensemble..
# ToDo/FixMe: Also make the different encryption modules ensembles.

proc ::tcldrop::encryption::register {type info} {
	variable Types
	variable Defaults
	foreach {c n} $info {
		set Types(${type},$c) $n
		set Defaults($c) [linsert $Defaults($c) 0 $type]
	}
}

proc ::tcldrop::encryption::unregister {type} {
	variable Types
	array unset Types ${type},*
	variable Defaults
	foreach c [array names Defaults] {
		if {[set pos [lsearch -exact $Defaults($c) $type]] != -1} {
			set Defaults($c) [lreplace $Defaults($c) $pos $pos]
		}
	}
}

proc ::tcldrop::encryption::default {command {type {}}} {
	variable Defaults
	if {$type == {}} {
		lindex $Defaults($command) 0
	} else {
		if {[set pos [lsearch -exact $Defaults($command) $type]] == -1} {
			set Defaults($command) [linsert $Defaults($command) 0 $type]
		} else {
			set Defaults($command) [lreplace $Defaults($command) $pos $pos]
			set Defaults($command) [linsert $Defaults($command) 0 $type]
		}
		set type
	}
}

proc ::tcldrop::encryption::encpass {password args} { Encpass $password $args }
proc ::tcldrop::encryption::Encpass {password arguments} {
	variable Defaults
	set options(-type) [lindex $Defaults(encpass) 0]
	array set options $arguments
	variable Types
	if {[info exists Types($options(-type),encpass)]} {
		$Types($options(-type),encpass) $password
	} else {
		return -code error "Invalid encryption type \"$options(-type)\".  Available types: [join $Defaults(encpass) {, }]"
	}
}

proc ::tcldrop::encryption::encrypt {key string args} { Encrypt $key $string $args }
proc ::tcldrop::encryption::Encrypt {key string arguments} {
	variable Defaults
	set options(-type) [lindex $Defaults(encrypt) 0]
	array set options $arguments
	variable Types
	if {[info exists Types($options(-type),encrypt)]} {
		$Types($options(-type),encrypt) $key $string
	} else {
		return -code error "Invalid encryption type \"$options(-type)\".  Available types: [join $Defaults(encrypt) {, }]"
	}
}

proc ::tcldrop::encryption::decrypt {key string args} { Decrypt $key $string $args }
proc ::tcldrop::encryption::Decrypt {key string arguments} {
	variable Defaults
	set options(-type) [lindex $Defaults(decrypt) 0]
	array set options $arguments
	variable Types
	if {[info exists Types($options(-type),decrypt)]} {
		$Types($options(-type),decrypt) $key $string
	} else {
		return -code error "Invalid encryption type \"$options(-type)\".  Available types: [join $Defaults(decrypt) {, }]"
	}
}

bind load - encryption ::tcldrop::encryption::LOAD -priority 0
proc ::tcldrop::encryption::LOAD {module} {
	variable Types
	array set Types [list]
	variable Defaults
	array set Defaults [list encpass [list] encrypt [list] decrypt [list]]
}

bind unld - encryption ::tcldrop::encryption::UNLD -priority 0
proc ::tcldrop::encryption::UNLD {module} { return 1 }
