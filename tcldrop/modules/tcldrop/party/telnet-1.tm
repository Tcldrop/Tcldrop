# party/telnet.tcl --
#	Handles:
#		Telnet interface for users.
#	Depends: party.
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
#
#	party::telnet module for tcldrop.  (REQUIRED)
#
# This module provides the telnet interface for users to access the bot.
# Note: support for stdin/stdout is also in this module, so it's REQUIRED
# if you want to use -n -t command line options to login to the bot.


namespace eval ::tcldrop::party::telnet {
	variable version {0.4}
	variable name {party::telnet}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	variable depends {party channels::main console::main core::users core::dcc core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Provides the telnet interface for users.}
	variable rcsid {$Id$}
	variable commands [list]
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the party module:
	checkmodule party
}

proc ::tcldrop::party::telnet::Connect {idx} { setidxinfo $idx [list -control ::tcldrop::party::telnet::Read -writable ::tcldrop::party::telnet::Write -errors ::tcldrop::party::telnet::Error module party::telnet handle *] }

proc ::tcldrop::party::telnet::Error {idx {error {}}} {
	array set chatinfo [getidxinfo $idx]
	callparty quit ${idx}:$chatinfo(handle)@${::botnet-nick}
}

proc ::tcldrop::party::telnet::Write {idx} {
	setidxinfo $idx [list state TELNET_ID other {t-in} timestamp [clock seconds] traffictype partyline]
	if {[info exists use-telnet-banner] && ${::use-telnet-banner}} { dccdumpfile $idx ${::telnet-banner} }
	if {${::open-telnets} || [countusers] == 0} { putdcc $idx {(If you are new, enter 'NEW' here.)} }
	putdcc $idx {Handle: } -nonewline 1 -flush 1
}

proc ::tcldrop::party::telnet::Read {idx line} {
	if {[info exists ::idxlist($idx)]} { array set chatinfo $::idxlist($idx) } else { killidx $idx }
	switch -- $chatinfo(state) {
		{TELNET_ID} {
			if {[string equal -nocase {new} $line]} {
				# They want to sign-in as a NEW user.
				if {${::open-telnets} || [countusers] == 0} {
					# Let them.
					putdcc $idx {Enter the handle you would like to use: } -nonewline 1 -flush 1
					setidxinfo $idx [list state TELNET_NEW other {new} timestamp [clock seconds]]
				} else {
					# Denied!
					putdcc $idx {You don't have access.  (not accepting 'new' users)}
					return 1
				}
			} else {
				if {([validuser $line]) && (![passwdok $line -]) && (!${::require-p} || [matchattr $line p])} {
					putdcc $idx {Password: } -nonewline 1 -flush 1
					setidxinfo $idx [list handle $line state CHAT_PASS other {pass} timestamp [clock seconds]]
				} else {
					putdcc $idx {You don't have access.}
				}
			}
		}
		{CHAT_PASS} {
			if {[passwdok $chatinfo(handle) $line]} {
				initconsole $idx
				setidxinfo $idx [list state CHAT other {chat} timestamp [clock seconds]]
				if {![info exists ::motd] || ![dccdumpfile $idx ${::motd}]} { putdcc $idx {Welcome!} }
				callchon $chatinfo(handle) $idx
			} else {
				putdcc $idx {Bad password!}
				return 1
			}
		}
		{TELNET_NEW} {
			# Make sure the handle they want isn't already taken..
			if {[validuser $line]} {
				putdcc $idx {Sorry, that handle is taken already.}
				putdcc $idx {Try another one please: } -nonewline 1 -flush 1
			} else {
				putdcc $idx {Okay, now choose and enter a password: } -nonewline 1 -flush 1
				adduser $line *!$chatinfo(remote)
				setidxinfo $idx [list handle $line state TELNET_PW other {newp} timestamp [clock seconds]]
			}
		}
		{TELNET_PW} {
			if {[string length $line] < 4} {
				putdcc $idx {Try to use at least 4 characters in your password.}
				putdcc $idx {Choose and enter a password: } -nonewline 1 -flush 1
				setidxinfo $idx [list timestamp [clock seconds]]
			} else {
				setuser $chatinfo(handle) pass $line
				putdcc $idx {Remember that!  You'll need it next time you log in.}
				putdcc $idx "You now have an account"
				if {[countusers] == 1} {
					chattr $chatinfo(handle) +pnmofvtxj
				} else {
					chattr $chatinfo(handle) ${::default-flags}
				}
				initconsole $idx
				setidxinfo $idx [list state CHAT other {chat} timestamp [clock seconds]]
				callchon $chatinfo(handle) $idx
			}
		}
		{CHAT} {
			if {[string index $line 0] == {.}} {
				dccsimul $idx $line
			} else {
				callparty chat $idx:$chatinfo(handle)@${::botnet-nick} handle $chatinfo(handle) idx $idx bot ${::botnet-nick} line $line
			}
		}
		{default} { return 1 }
	}
	return 0
}

#proc ::tcldrop::party::telnet::CHAT {user chan text} {
#	if {[string match {*@*} $user]} { set text "<${user}> $text" } else { set text "*** (${user}) $text" }
#	foreach {i d} [idxlist [list traffictype partyline module party::telnet state CHAT]] {
#		array set chatinfo $d
#		if {($chatinfo(console-echo) || ![string equal -nocase $user $chatinfo(handle)]) && ([string match -nocase $chatinfo(console-chan) $chan] || [string match -nocase $chan $chatinfo(console-chan)])} {
#			putdcc $chatinfo(idx) "<${user}> $text"
#		}
#	}
#}

#proc ::tcldrop::party::telnet::BCST {bot text} {
#	foreach {i d} [idxlist [list traffictype partyline module party::telnet state CHAT]] {
#		array set chatinfo $d
#		putdcc $chatinfo(idx) "*** ($bot) $text"
#	}
#}

# This has to be a LOAD bind:
bind load - party::telnet ::tcldrop::party::telnet
proc ::tcldrop::party::telnet {module} {
	setdefault open-telnets 1
	setdefault info-party 0
	setdefault connect-timeout 27
	setdefault use-telnet-banner 0
	setdefault text-path {text}
	setdefault telnet-banner {banner}
	setdefault motd {motd}
	setdefault network {Unknown}
	setdefault require-p 0
	setdefault default-flags {p}
	# Add new listen types (these are used by the [listen] command):
	addlistentype telnetparty connect ::tcldrop::party::telnet::Connect ident 1
	addlistentype party::telnet connect ::tcldrop::party::telnet::Connect ident 1
	addlistentype users connect ::tcldrop::party::telnet::Connect ident 1
#	bind chat - * ::tcldrop::party::telnet::CHAT -priority -100
#	bind bcst - * ::tcldrop::party::telnet::BCST -priority -100
}

# Don't allow the party::telnet module to be unloaded:
bind unld - party::telnet ::tcldrop::party::telnet::UNLD
proc ::tcldrop::party::telnet::UNLD {module} { return 1 }
