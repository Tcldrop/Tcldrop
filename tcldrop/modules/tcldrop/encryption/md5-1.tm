# encryption/md5 --
#	Handles:
#		* Provides the [md5] command, as well as md5 password hashing.
#	Depends: encryption.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
	variable version {0.2}
	variable name {encryption::md5}
	package provide tcldrop::$name 1
	package provide tcldrop::md5 1
	variable depends {encryption core}
	variable author {Tcldrop-Dev}
	variable description {Provides the [md5] command as well as MD5 password hashing.}
	variable script [info script]
	variable rcsid {$Id$}
	# This makes sure we're loading from a Tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export md5
	variable commands [namespace export]

	#  md5 <string>
	#    Returns: the 128 bit MD5 message-digest of the specified string

	# This will find md5 and then make sure the ::md5 command always returns hex in lowercase:
	proc FindMD5 {args} { variable md5ver
		if {[llength [info commands ::md5_orig]]} {
			# Note: We ASSume we've been here and already renamed ::md5 to ::md5_orig, made a replacement ::md5 proc, and that ::md5 returns hex in lowercase.
			return 1
		} elseif {![catch { package require md5 } md5ver] && ![catch { ::md5 "" } Output]} {
			switch -- $Output {
				{d41d8cd98f00b204e9800998ecf8427e} { }
				{D41D8CD98F00B204E9800998ECF8427E} {
					rename ::md5 ::md5_orig
					proc md5 {args} {
						if {[llength $args] == 1} {
							string tolower [::md5_orig [lindex $args 0]]
						} else {
							::md5_orig {*}$args
						}
					}
				}
				{default} {
					rename ::md5 ::md5_orig
					if {[package vsatisfies $md5ver 1]} {
						# Replace the ::md5::md5 proc which tries to use ::md5 with ::md5_orig
						proc ::md5::md5 {msg} { string tolower [::hex -mode encode -- [::md5_orig -- $msg]] }
						proc encpass {password} { ::md5::hmac $password $password }
					} else {
						proc md5 {args} {
							if {[llength $args] == 1} {
								binary scan [::md5_orig [lindex $args 0]] H* args
								return $args
							} else {
								::md5_orig {*}$args
							}
						}
						proc encpass {password} { string tolower [::md5::hmac -hex -key $password $password] }
					}
				}
			}
			return 1
		}
		return 0
	}
	# FixMe: This should check for success or failure to find the md5 package:
	FindMD5
	bind load - encryption::md5 ::tcldrop::encryption::md5::LOAD -priority 0
	bind load - md5 ::tcldrop::encryption::md5::LOAD -priority 0
	proc LOAD {module} {
		::tcldrop::encryption::register md5 [list encpass ::tcldrop::encryption::md5::encpass]
		bind unld - encryption::md5 ::tcldrop::encryption::md5::UNLD
		bind unld - md5 ::tcldrop::encryption::md5::UNLD
		variable md5ver
		putlog "Using tcllib md5 version: $md5ver"
	}
	proc UNLD {module} {
		catch { package forget md5 }
		catch { rename ::md5_orig ::md5 }
		return 0
	}
}
