# sha256.tm --
#	Handles:
#		* sha256 hashing.
#	Depends: encryption.
#
# $Id$
#
# Copyright (C) 2008 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
# This module provides the sha256 and encpass Tcl commands.
# (It's not a true encryption module)

namespace eval ::tcldrop::encryption::sha256 {
	variable version {0.2}
	variable script [info script]
	variable name {encryption::sha256}
	# Provide the sha256 module:
	package provide tcldrop::$name 1
	package provide tcldrop::${name}::main 1
	package provide tcldrop::sha256 1
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {SHA256 hashing via the sha256 tcllib package.}
	variable rcsid {$Id$}
	variable commands [list sha256]
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	#namespace export {*}$commands
}

package require sha256
putlog "sha256 package test: [::sha2::sha256 {string}]"

proc ::tcldrop::encryption::sha256::encpass {password} { ::sha2::hmac $password $password }

bind load - encryption::sha256 ::tcldrop::encryption::sha256::LOAD -priority 0
proc ::tcldrop::encryption::sha256::LOAD {module} {
	catch { namespace path [list ::sha2 ::tcldrop::encryption ::tcldrop::core ::tcldrop] }
	::tcldrop::encryption::register sha256 [list encpass ::tcldrop::encryption::sha256::encpass]
	bind unld - encryption::sha256 ::tcldrop::encryption::sha256::UNLD -priority 0
	bind unld - sha256 ::tcldrop::encryption::sha256::UNLD -priority 0
	putlog "sha256 encpass test: [::tcldrop::encryption::sha256::encpass string]"
}

proc ::tcldrop::encryption::sha256::UNLD {module} {
	# FixMe: Unregister the sha256 encryption.
	catch { package forget sha256 }
	return 0
}

