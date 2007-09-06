# blowfishc.tcl --
#	Handles:
#		* Provides blowfish support via a Critcl blowfishc package.
#		* Note, this is NOT compatible with Eggdrop's blowfish.
#	Depends: encryption.
#
# $Id: blowfishc.tcl,v 1.2 2005/04/25 08:09:45 fireegl Exp $
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

namespace eval ::tcldrop::encryption::blowfishc {
	variable version {0.1}
	variable name {encryption::blowfishc}
	variable depends {encryption}
	variable author {Tcldrop-Dev}
	variable description {Provides the blowfishc encryption via a Critcl blowfishc package}
	variable commands [list encpass Encpass encrypt Encrypt decrypt Decrypt]
	variable script [info script]
	variable rcsid {$Id: blowfishc.tcl,v 1.2 2005/04/25 08:09:45 fireegl Exp $}
	package provide tcldrop::$name $version
	package provide tcldrop::blowfishc $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the encryption module:
	checkmodule encryption
	# Export all the commands that should be available to 3rd-party scripters:
	eval namespace export $commands
}

if {[catch { package require blowfishc }] || [catch { package require base64 }]} { putlog "WARNING: blowfishc module can't load." ; return }


proc ::tcldrop::encryption::blowfishc::encrypt {key string} {
	# Note: We convert the binary stuff that blowfish returns into hex..
	#       This is done because Eggdrop scripts expect encrypt to return a plain-text string.
	#binary scan [blowfish encode $string $key] H* string
	#set string
	::base64::encode -maxlen 0 [blowfish encode $string $key]
}
proc ::tcldrop::encryption::blowfishc::decrypt {key string} {
	# Note: We expect $string to be hex..
	#blowfish decode [binary format H* $string] $key
	blowfish decode [::base64::decode $string] $key
}
proc ::tcldrop::encryption::blowfishc::encpass {password} {
	#binary scan [blowfish encode $password $password] H* password
	#set password
	::base64::encode [blowfish encode $password $password]
}

bind load - encryption::blowfishc ::tcldrop::encryption::blowfishc::LOAD -priority 0
proc ::tcldrop::encryption::blowfishc::LOAD {module} {
	::tcldrop::encryption::register blowfishc [list encrypt ::tcldrop::encryption::blowfishc::encrypt decrypt ::tcldrop::encryption::blowfishc::decrypt encpass ::tcldrop::encryption::blowfishc::encpass]
	bind unld - encryption::blowfishc ::tcldrop::encryption::blowfishc::UNLD -priority 0
}

proc ::tcldrop::encryption::blowfishc::UNLD {module} {
	# FixMe: Unregister the encryption.
	catch { package forget blowfishc }
	catch { package forget base64 }
	return 0
}
