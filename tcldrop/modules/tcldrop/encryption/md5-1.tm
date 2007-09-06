# md5.tcl --
#	Handles:
#		* Provides the [md5] command, as well as md5 password hashing.
#	Depends: encryption.
#
# $Id: md5.tcl,v 1.2 2005/04/25 08:09:45 fireegl Exp $
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

namespace eval ::tcldrop::encryption::md5 {
	variable version {0.1}
	variable name {encryption::md5}
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {Provides the [md5] command as well as md5 password hashing.}
	variable commands [list]
	variable script [info script]
	variable rcsid {$Id: md5.tcl,v 1.2 2005/04/25 08:09:45 fireegl Exp $}
	package provide tcldrop::$name $version
	package provide tcldrop::md5 $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the encryption module:
	checkmodule encryption
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export md5
}

package require md5 1

#  md5 <string>
#    Returns: the 128 bit MD5 message-digest of the specified string
proc ::tcldrop::encryption::md5::md5 {string} { ::md5::md5 $string }
proc ::tcldrop::encryption::md5::encpass {password} { ::md5::hmac $password $password }

bind load - encryption::md5 ::tcldrop::encryption::md5::LOAD -priority 0
bind load - md5 ::tcldrop::encryption::md5::LOAD -priority 0
proc ::tcldrop::encryption::md5::LOAD {module} {
	::tcldrop::encryption::register md5 [list encpass ::tcldrop::encryption::md5::encpass]
	bind unld - encryption::md5 ::tcldrop::encryption::md5::UNLD
	bind unld - md5 ::tcldrop::encryption::md5::UNLD
}

proc ::tcldrop::encryption::md5::UNLD {module} {
	catch { package forget md5 }
	return 0
}
