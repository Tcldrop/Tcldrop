# sha1.tcl --
#	Handles:
#		* sha1 hashing.
#	Depends: encryption.
#
# $Id$
#
# Copyright (C) 2003,2004,2005 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
# This module provides the sha1 and encpass Tcl commands.
# (It's not a true encryption module)

namespace eval ::tcldrop::encryption::sha1 {
	variable version {0.2}
	variable script [info script]
	variable name {encryption::sha1}
	# Provide the sha1 module:
	package provide tcldrop::$name 1
	package provide tcldrop::${name}::main 1
	package provide tcldrop::sha1 1
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {SHA1 hashing via the sha1 package.}
	variable rcsid {$Id$}
	variable commands [list]
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	#namespace export {*}$commands
}

package require sha1
putlog "sha1 package test: [::sha1::sha1 string]"

proc ::tcldrop::encryption::sha1::encpass {password} { ::sha1::hmac $password $password }

bind load - encryption::sha1 ::tcldrop::encryption::sha1::LOAD -priority 0
proc ::tcldrop::encryption::sha1::LOAD {module} {
	catch { namespace path [list ::sha1 ::tcldrop::encryption ::tcldrop::core ::tcldrop] }
	::tcldrop::encryption::register sha1 [list encpass ::tcldrop::encryption::sha1::encpass]
	bind unld - encryption::sha1 ::tcldrop::encryption::sha1::UNLD -priority 0
	bind unld - sha1 ::tcldrop::encryption::sha1::UNLD -priority 0
	putlog "sha1 encpass test: [::tcldrop::encryption::sha1::encpass string]"
}

proc ::tcldrop::encryption::sha1::UNLD {module} {
	# FixMe: Unregister the sha1 encryption.
	catch { package forget sha1 }
	return 0
}

