# twofish.tcl --
#	Handles:
#		* Provides twofish encryption via Crypto.mod.
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
#
#	twofish module for tcldrop.
#
# This module provides the encpass, encrypt, and decrypt tcl commands.

namespace eval ::tcldrop::encryption::twofish {
	# Provide the twofish module:
	variable version {0.1}
	variable name {encryption::twofish}
	variable depends {encryption}
	variable author {Tcldrop-Dev}
	variable description {Provides twofish encryption via Crypto.mod.}
	variable commands [list]
	variable script [info script]
	variable rcsid {$Id$}
	package provide tcldrop::twofish $version
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	checkmodule encryption
}

# For now, we require the crypto.mod created by leprechau@EFNet
# which is available from http://TclCryptography.SF.Net/
# He's said that he would see about converting them to work with Critcl though.
# (Using Critcl saves the end user from having to manually compile the encryption lib)
if {[catch { package require Crypto } err] && [catch {load [file join ${::mod-path} lib crypto[info sharedlibextension]]}]} {
	return -code error $err
} else {
	lappend ::protected(commands) tencrypt tdecrypt tencpass
}

bind load - encryption::twofish ::tcldrop::encryption::twofish::LOAD -priority 0
proc ::tcldrop::encryption::twofish::LOAD {module} {
	::tcldrop::encryption::register twofish [list encrypt ::tcldrop::encryption::twofish::encrypt decrypt ::tcldrop::encryption::twofish::decrypt encpass ::tcldrop::encryption::twofish::encpass]
	proc encrypt {key string} { tencrypt $key $string }
	proc decrypt {key string} { tdecrypt $key $string }
	proc encpass {password} { tencpass $password }
	bind unld - encryption::twofish ::tcldrop::encryption::twofish::UNLD -priority 0
}

proc ::tcldrop::encryption::twofish::UNLD {module} {
	# FixMe: unregister the twofish encryption.
	# Note: The Crypto package must remain loaded; if it's unloaded a bug in it prevents it from being loaded again.
	catch { package forget Crypto }
	return 1
}
