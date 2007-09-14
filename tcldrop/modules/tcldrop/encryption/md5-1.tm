# encryption/md5 --
#	Handles:
#		* Provides the [md5] command, as well as md5 password hashing.
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

namespace eval ::tcldrop::encryption::md5 {
	variable version {0.1}
	variable name {encryption::md5}
	package provide tcldrop::$name 1
	package provide tcldrop::md5 1
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {Provides the [md5] command as well as md5 password hashing.}
	variable script [info script]
	variable rcsid {$Id$}
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export md5
	variable commands [namespace export]
}

# Because Eggdrop's ::md5 command returns the md5 in hex, and tcllib's ::md5 command returns the md5 in binary, we have to do this crap:
if {![catch { package require md5 1 }] && [info commands {::md5}] eq {::md5} && [info commands {::md5::real_md5}] ne {::md5::real_md5}} {
	rename ::md5 ::md5::real_md5
	#  md5 <string>
	#    Returns: the 128 bit MD5 message-digest of the specified string
	proc ::tcldrop::encryption::md5::md5 {string} { string tolower [::hex -mode encode -- [::md5::real_md5 -- $string]] }
	# Replace the original ::md5::md5 proc with this one, so it knows about ::md5::real_md5:
	proc ::md5::md5 {string} { string tolower [::hex -mode encode -- [::md5::real_md5 -- $string]] }
}
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
