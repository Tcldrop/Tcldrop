# blowfish.tcl --
#	Handles:
#		* Provides blowfish (Eggdrop style) encryption via Crypto.mod.
#	Depends: encryption.
#
# $Id: blowfish.tcl,v 1.5 2006/05/03 15:06:39 fireegl Exp $
#
# Copyright (C) 2003,2004,2005,2006 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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

# For now, we require the crypto.mod created by leprechau@EFNet
# which is available from http://TclCryptography.SF.Net/
# He's said that he would see about converting them to work with Critcl though.
# (Using Critcl saves the end user from having to manually compile the encryption lib)
namespace eval ::tcldrop::encryption::blowfish {
	variable version {0.1}
	variable name {encryption::blowfish}
	variable depends {encryption}
	variable author {Tcldrop-Dev}
	variable description {Provides blowfish (Eggdrop-style) via Crypto.mod}
	variable commands [list]
	variable script [info script]
	variable rcsid {$Id: blowfish.tcl,v 1.5 2006/05/03 15:06:39 fireegl Exp $}
	package provide tcldrop::$name $version
	package provide tcldrop::blowfish $version
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
	lappend ::protected(commands) bencrypt bdecrypt bencpass
	#rename bencrypt ::tcldrop::encryption::blowfish::bencrypt
	#rename bdecrypt ::tcldrop::encryption::blowfish::bdecrypt
	#rename bencpass ::tcldrop::encryption::blowfish::bencpass
}

proc ::tcldrop::encryption::blowfish::encrypt {key string} { bencrypt $key $string }
proc ::tcldrop::encryption::blowfish::decrypt {key string} { bdecrypt $key $string }
proc ::tcldrop::encryption::blowfish::encpass {password} { bencpass $password }

bind load - encryption::blowfish ::tcldrop::encryption::blowfish::LOAD -priority 0
bind load - blowfish ::tcldrop::encryption::blowfish::LOAD -priority 0
proc ::tcldrop::encryption::blowfish::LOAD {module} {
	::tcldrop::encryption::register blowfish [list encrypt ::tcldrop::encryption::blowfish::encrypt decrypt ::tcldrop::encryption::blowfish::decrypt encpass ::tcldrop::encryption::blowfish::encpass]
	bind unld - encryption::blowfish ::tcldrop::encryption::blowfish::UNLD
	bind unld - blowfish ::tcldrop::encryption::blowfish::UNLD
}

proc ::tcldrop::encryption::blowfish::UNLD {module} {
	catch { package forget Crypto }
	return 1
}
