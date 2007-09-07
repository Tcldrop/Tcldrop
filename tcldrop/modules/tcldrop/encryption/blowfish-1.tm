# blowfish.tcl --
#	Handles:
#		* Provides blowfish encryption via a pure-Tcl package.  (It is NOT Eggdrop compatible!)
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
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.

namespace eval ::tcldrop::encryption::blowfish {
	variable version {0.1}
	variable name {encryption::blowfish}
	package provide tcldrop::$name 1
	package provide tcldrop::blowfish 1
	variable depends {encryption}
	variable author {Tcldrop-Dev}
	variable description {Provides a pure-Tcl blowfish.}
	variable commands [list blowfish]
	variable script [info script]
	variable rcsid {$Id$}
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	::package require blowfish
	::package require base64
	# Pre-depends on the encryption module:
	checkmodule encryption
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
	proc encrypt {key string} { ::base64::encode -maxlen 0 [::blowfish::blowfish -mode ecb -dir encrypt -key $key $string] }
	proc decrypt {key string} { string trimright [::blowfish::blowfish -mode ecb -dir decrypt -key $key [::base64::decode $string]] "\0" }
	proc encpass {password} { ::base64::encode -maxlen 0 [::blowfish::blowfish -mode ecb -dir encrypt -key $password $password] }
	namespace ensemble create -subcommands [list encrypt decrypt encpass]
	namespace ensemble create -command ::tcldrop::blowfish -subcommands [list encrypt decrypt encpass]
	namespace ensemble create -command ::blowfish -subcommands [list encrypt decrypt encpass]
	#namespace unknown unknown
}

bind load - encryption::blowfish ::tcldrop::encryption::blowfish::LOAD -priority 0
bind load - blowfish ::tcldrop::encryption::blowfish::LOAD -priority 0
proc ::tcldrop::encryption::blowfish::LOAD {module} {
	::tcldrop::encryption::register blowfish [list encrypt ::tcldrop::encryption::blowfish::encrypt decrypt ::tcldrop::encryption::blowfish::decrypt encpass ::tcldrop::encryption::blowfish::encpass]
	bind unld - encryption::blowfish ::tcldrop::encryption::blowfish::UNLD
	bind unld - blowfish ::tcldrop::encryption::blowfish::UNLD
}

proc ::tcldrop::encryption::blowfish::UNLD {module} {
	catch { ::package forget blowfish }
	catch { ::package forget base64 }
	return 0
}
