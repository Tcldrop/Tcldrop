# encryption/null --
#	Handles:
#		* Provides "null" encryption.
#	Depends: encryption.
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

namespace eval ::tcldrop::encryption::null {
	variable version {0.1}
	variable name {encryption::null}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	variable predepends {encryption}
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {Provides "null" encryption.}
	variable commands [list]
	variable rcsid {$Id$}
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	namespace path [list ::tcldrop]
	proc encrypt {key string} { set string }
	proc decrypt {key string} { set string }
	proc encpass {password} { set password }
	namespace path [list ::tcldrop::encryption ::tcldrop]
}

::tcldrop::bind load - encryption::null ::tcldrop::encryption::null::LOAD -priority 0
proc ::tcldrop::encryption::null::LOAD {module} {
	::tcldrop::encryption::register null [list encrypt ::tcldrop::encryption::null::encrypt decrypt ::tcldrop::encryption::null::decrypt encpass ::tcldrop::encryption::null::encpass]
	bind unld - encryption::null ::tcldrop::encryption::null::UNLD -priority 0
}

proc ::tcldrop::encryption::null::UNLD {module} {
	# FixMe: Unregister the null encryption.
	return 0
}

