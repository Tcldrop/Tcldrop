# tea.tcl --
#	Handles:
#		* Provides TEA (Tiny Encryption Algorithm) encryption.
#	Depends: encryption.
#
# $Id$
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

namespace eval ::tcldrop::encryption::tea {
	# Provide the tea module:
	variable version {1.0}
	variable name {encryption::tea}
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {Provides tea encryption via the tea package.}
	variable commands [list]
	variable script [info script]
	variable rcsid {$Id$}
	package provide tcldrop::$name $version
	package provide tcldrop::tea $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the encryption module:
	checkmodule encryption
	proc encrypt {key string} { ::base64::encode -maxlen 0 [::tea::encrypt $key $string] }
	proc decrypt {key string} { ::tea::decrypt $key [::base64::decode $string] }
	proc encpass {password} { ::base64::encode [::tea::encrypt $password $password] }
}

# FixMe: Add some putlog's about what we're package require'ing:
package require tea
package require base64

bind load - encryption::tea ::tcldrop::encryption::tea::LOAD -priority 0
proc ::tcldrop::encryption::tea::LOAD {module} {
	::tcldrop::encryption::register tea [list encrypt ::tcldrop::encryption::tea::encrypt decrypt ::tcldrop::encryption::tea::decrypt encpass ::tcldrop::encryption::tea::encpass]
	bind unld - encryption::tea ::tcldrop::encryption::tea::UNLD -priority 0
}

proc ::tcldrop::encryption::tea::UNLD {module} {
	# FixMe: Unregister the tea encryption.
	catch { package forget tea }
	catch { package forget base64 }
	return 0
}


