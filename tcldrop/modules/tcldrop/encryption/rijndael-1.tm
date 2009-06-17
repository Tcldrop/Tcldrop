# encryption/rijndael.tcl --
# Handles:
#		* Provides the rijndael encryption via the Crypto.mod.
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

namespace eval ::tcldrop::encryption::rijndael {
	variable version {0.1}
	variable name {encryption::rijndael}
	variable depends {encryption}
	variable author {Tcldrop-Dev}
	variable description {Provides rijndael encryption via Crypto.mod.}
	variable commands [list]
	variable script [info script]
	variable rcsid {$Id$}
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the encryption module:
	checkmodule encryption
}

# For now, we require the crypto.mod created by leprechau@EFNet
# which is available from http://TclCryptography.SF.Net/
# He's said that he would see about converting them to work with Critcl though.
# (Using Critcl saves the end user from having to manually compile the encryption lib)
if {[catch { package require Crypto } err] && [catch {load [file join ${::mod-path} lib crypto[info sharedlibextension]]}]} {
	return -code error $err
} else {
	lappend ::protected(commands) rencrypt rdecrypt rencpass
}


proc :tcldrop::encryption::rijndael::encrypt {key string} { rencrypt $key $string }
proc :tcldrop::encryption::rijndael::decrypt {key string} { rdecrypt $key $string }
proc :tcldrop::encryption::rijndael::encpass {password} { rencpass $password }

proc ::tcldrop::encryption::rijndael::UNLD {module} {
	# FixMe: Unregister the rijndael encryption.
	catch { package forget Crypto }
	return 1
}

bind load - encryption::rijndael ::tcldrop::encryption::rijndael::LOAD -priority 0
proc ::tcldrop::encryption::rijndael::LOAD {module} {
	::tcldrop::encryption::register rijndael [list encrypt ::tcldrop::encryption::rijndael::encrypt decrypt ::tcldrop::encryption::rijndael::decrypt encpass ::tcldrop::encryption::rijndael::encpass]
	bind unld - encryption::rijndael ::tcldrop::encryption::rijndael::UNLD -priority 0
}
