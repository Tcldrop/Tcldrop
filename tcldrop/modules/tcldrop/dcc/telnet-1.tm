# dcc::telnet --
#	Handles:
#		Telnet interface for users.
#	Depends: party.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008,2009 Tcldrop Development Team <Tcldrop-Dev@Tcldrop.US>
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
#	dcc::telnet module for tcldrop.  (REQUIRED)
#
# This module provides the telnet interface for users to access the bot.
# Note: support for stdin/stdout is also in this module, so it's REQUIRED
# if you want to use -n -t command line options to login to the bot.


namespace eval ::tcldrop::dcc::telnet {
	variable version {0.4}
	variable name {dcc::telnet}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	variable depends {party channels console core::users core::dcc core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Provides the telnet interface for users.}
	variable rcsid {$Id$}
	variable commands [list]
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the party module:
	#checkmodule party
	namespace path [list ::tcldrop ::tcldrop::core]
}

proc ::tcldrop::dcc::telnet::Connect {idx} {
	idxinfo $idx state TELNET_CONN -control ::tcldrop::dcc::telnet::Read -writable ::tcldrop::dcc::telnet::Write -errors ::tcldrop::dcc::telnet::Error module dcc::telnet handle *
}

proc ::tcldrop::dcc::telnet::Error {idx {error {}}} {
	array set chatinfo [idxinfo $idx]
	callparty quit ${idx}:$chatinfo(handle)@${::botnet-nick}
}

proc ::tcldrop::dcc::telnet::Write {idx} {
	if {[getidxinfo $idx state] eq {TELNET_CONN}} {
		idxinfo $idx state TELNET_ID other {t-in} timestamp [clock seconds] traffictype partyline
		if {[info exists use-telnet-banner] && ${::use-telnet-banner}} { dccdumpfile $idx ${::telnet-banner} }
		if {${::open-telnets} || [countusers] == 0} { putdcc $idx "[mc {(If you are new, enter 'NEW' here.)}]" }
		putdcc $idx "[mc {Handle: }]" -nonewline 1 -flush 1
	}
}

proc ::tcldrop::dcc::telnet::Read {idx line} {
	if {[info exists ::idxlist($idx)]} { array set chatinfo $::idxlist($idx) } else { killidx $idx }
	switch -- $chatinfo(state) {
		{TELNET_ID} - {TELNET_CONN} {
			# We process TELNET_CONN here too, because the Read proc can get triggered before the Write.
			# Such as when they connect and send their handle immediately rather than waiting for us to ask for it.
			# In that case, we treat it like we've already sent the Handle: prompt.
			if {[string equal -nocase {new} $line]} {
				# They want to sign-in as a NEW user.
				if {${::open-telnets} || [countusers] == 0} {
					# Let them.
					putdcc $idx "[mc {Enter the handle you would like to use: }]" -nonewline 1 -flush 1
					idxinfo $idx state TELNET_NEW other {new} timestamp [clock seconds]
				} else {
					# Denied!
					putdcc $idx "[mc {You don't have access.  (not accepting 'new' users)}]"
					return 1
				}
			} else {
				if {([validuser $line]) && (![passwdok $line -]) && (!${::require-p} || [matchattr $line p])} {
					putdcc $idx "[mc {Password: }]" -nonewline 1 -flush 1
					idxinfo $idx handle $line state CHAT_PASS other {pass} timestamp [clock seconds]
				} else {
					putdcc $idx "[mc {You don't have access.}]"
				}
			}
		}
		{CHAT_PASS} {
			if {[passwdok $chatinfo(handle) $line]} {
				initconsole $idx
				idxinfo $idx state CHAT other {chat} timestamp [clock seconds]
				if {![info exists ::motd] || ![dccdumpfile $idx ${::motd}]} { putdcc $idx "[mc {Welcome!}]" }
				callchon $chatinfo(handle) $idx
			} else {
				putdcc $idx "[mc {Bad password!}]"
				return 1
			}
		}
		{TELNET_NEW} {
			# Make sure the handle they want isn't already taken..
			if {[validuser $line]} {
				putdcc $idx "[mc {Sorry, that handle is taken already.}]"
				putdcc $idx "[mc {Try another one please: }]" -nonewline 1 -flush 1
			} else {
				putdcc $idx "[mc {Okay, now choose and enter a password: }]" -nonewline 1 -flush 1
				adduser $line *!$chatinfo(remote)
				idxinfo $idx handle $line state TELNET_PW other {newp} timestamp [clock seconds]
			}
		}
		{TELNET_PW} {
			if {[string length $line] < 4} {
				putdcc $idx "[mc {Try to use at least 4 characters in your password.}]"
				putdcc $idx "[mc {Choose and enter a password: }]" -nonewline 1 -flush 1
				idxinfo $idx timestamp [clock seconds]
			} else {
				setuser $chatinfo(handle) pass $line
				putdcc $idx "[mc {Remember that!  You'll need it next time you log in.}]"
				putdcc $idx "[mc {You now have an account}]"
				if {[countusers] == 1} {
					chattr $chatinfo(handle) +pnmofvtxj
				} else {
					chattr $chatinfo(handle) ${::default-flags}
				}
				initconsole $idx
				idxinfo $idx state CHAT other {chat} timestamp [clock seconds]
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

#proc ::tcldrop::dcc::telnet::CHAT {user chan text} {
#	if {[string match {*@*} $user]} { set text "<${user}> $text" } else { set text "*** (${user}) $text" }
#	foreach {i d} [idxlist [list traffictype partyline module dcc::telnet state CHAT]] {
#		array set chatinfo $d
#		if {($chatinfo(console-echo) || ![string equal -nocase $user $chatinfo(handle)]) && ([string match -nocase $chatinfo(console-chan) $chan] || [string match -nocase $chan $chatinfo(console-chan)])} {
#			putdcc $chatinfo(idx) "<${user}> $text"
#		}
#	}
#}

#proc ::tcldrop::dcc::telnet::BCST {bot text} {
#	foreach {i d} [idxlist [list traffictype partyline module dcc::telnet state CHAT]] {
#		array set chatinfo $d
#		putdcc $chatinfo(idx) "*** ($bot) $text"
#	}
#}

# This has to be a LOAD bind:
::tcldrop::bind load - dcc::telnet ::tcldrop::dcc::telnet::LOAD
proc ::tcldrop::dcc::telnet::LOAD {module} {
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
	setdefault ident-timeout 30
	# Add new listen types (these are used by the [listen] command):
	addlistentype telnetparty connect ::tcldrop::dcc::telnet::Connect ident ${::ident-timeout}
	addlistentype dcc::telnet connect ::tcldrop::dcc::telnet::Connect ident ${::ident-timeout}
	addlistentype users connect ::tcldrop::dcc::telnet::Connect ident ${::ident-timeout}
#	bind chat - * ::tcldrop::dcc::telnet::CHAT -priority -100
#	bind bcst - * ::tcldrop::dcc::telnet::BCST -priority -100
}

# Don't allow the dcc::telnet module to be unloaded:
::tcldrop::bind unld - dcc::telnet ::tcldrop::dcc::telnet::UNLD
proc ::tcldrop::dcc::telnet::UNLD {module} { return 1 }
