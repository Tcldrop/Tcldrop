# blowfish.tcl --
#	Handles:
#		* Provides blowfish encryption (Eggdrop style).
#	Depends: encryption.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008 Tcldrop-Dev <Tcldrop-Dev@Tcldrop.US>
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
	variable version {0.2}
	variable name {encryption::blowfish}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name 1
	package provide tcldrop::${name}::main 1
	package provide tcldrop::blowfish 1
	variable predepends {encryption}
	variable depends {encryption}
	variable author {Tcldrop-Dev}
	variable description {Provides a pure-Tcl blowfish.}
	variable commands [list blowfish]
	variable rcsid {$Id$}
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	namespace path [list ::tcldrop]
	# Pre-depends on the encryption module:
	checkmodule encryption
	variable accel
	set accel(cryptomod) 0
	# FixMe: Consider making this check for pre-existing encrypt/decrypt/encpass commands (perhaps ones interp aliased here from Eggdrop).
	if {![catch { package require Crypto } err] || ![catch {load [file join ${::mod-path} lib crypto[info sharedlibextension]]} err]} {
		# Crypto.mod created by leprechau@EFNet is available from http://TclCryptography.SF.Net/
		# He's said that he would see about converting them to work with Critcl though.
		# (Using Critcl saves the end user from having to manually compile the encryption lib)
		lappend ::protected(commands) bencrypt bdecrypt bencpass
		#rename bencrypt ::tcldrop::encryption::blowfish::bencrypt
		#rename bdecrypt ::tcldrop::encryption::blowfish::bdecrypt
		#rename bencpass ::tcldrop::encryption::blowfish::bencpass
		#proc encrypt {key string} { bencrypt $key $string }
		#proc decrypt {key string} { bdecrypt $key $string }
		#proc encpass {password} { bencpass $password }
		interp alias {} ::tcldrop::encryption::blowfish::encrypt {} bencrypt
		interp alias {} ::tcldrop::encryption::blowfish::decrypt {} bdecrypt
		interp alias {} ::tcldrop::encryption::blowfish::encpass {} bencpass
		set accel(cryptomod) 1
	} elseif {![catch { ::package require blowfish } err] && ![catch { ::package require eggbase64 } err]} {
		proc encrypt {key string} { ::eggbase64::encode [::blowfish::blowfish -mode ecb -dir encrypt -key $key $string] }
		proc decrypt {key string} { string trimright [::blowfish::blowfish -mode ecb -dir decrypt -key $key [::eggbase64::decode $string]] "\x00" }
		# These are the SALT1 and SALT2 that Eggdrop uses in blowfish.c for its encpass:
		variable SALT1SALT2 [binary format II {0xdeadd061} {0x23f6b095}]
		proc encpass {password} { variable SALT1SALT2
			return "+[::eggbase64::encode [::blowfish::blowfish -mode ecb -dir encrypt -key $password $SALT1SALT2]]"
		}
	} else {
		putlog "Error loading blowfish: $err"
	}
	namespace ensemble create -subcommands [list encrypt decrypt encpass]
	namespace ensemble create -command ::tcldrop::blowfish -subcommands [list encrypt decrypt encpass]
	#if {[info commands ::blowfish] eq {}} { namespace ensemble create -command ::blowfish -subcommands [list encrypt decrypt encpass] }
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
	#namespace unknown unknown
}

::tcldrop::bind load - encryption::blowfish ::tcldrop::encryption::blowfish::LOAD -priority 0
::tcldrop::bind load - blowfish ::tcldrop::encryption::blowfish::LOAD -priority 0
proc ::tcldrop::encryption::blowfish::LOAD {module} {
	::tcldrop::encryption::register blowfish [list encrypt ::tcldrop::encryption::blowfish::encrypt decrypt ::tcldrop::encryption::blowfish::decrypt encpass ::tcldrop::encryption::blowfish::encpass]
	#if {[info commands ::blowfish] eq {}} { namespace ensemble create -command ::blowfish -subcommands [list encrypt decrypt encpass] }
	bind unld - encryption::blowfish ::tcldrop::encryption::blowfish::UNLD
	bind unld - blowfish ::tcldrop::encryption::blowfish::UNLD
}

proc ::tcldrop::encryption::blowfish::UNLD {module} {
	variable accel
	if {$accel(cryptomod)} {
		catch { ::package forget Crypto }
	} else {
		catch { ::package forget blowfish }
	}
	return 0
}
