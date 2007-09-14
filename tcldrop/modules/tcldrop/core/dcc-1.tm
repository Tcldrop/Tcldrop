# core/dcc --
#	Handles:
#		* All core DCC binds and commands.
#	Depends: core::conn.
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
#
#	dcc module for tcldrop.  (REQUIRED)
#
# This module provides all of the telnet/dcc chat related stuff.
# This includes user AND bot AND script connections.

namespace eval ::tcldrop::core::dcc {
	variable name {core::dcc}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	if {![info exists ::tcldrop]} { return }
	variable depends {core core::users core::conn party console}
	variable author {Tcldrop-Dev}
	variable description {All core DCC binds and commands.}
	variable rcsid {$Id$}
	variable commands [list dcclist putdcc putdccraw idx2hand hand2idx killdcc getdccidle calldcc callchon callchof dccdumpfile dccsimul]
	# putdccall getdccaway dccbroadcast putdccbut
	namespace export {*}$commands
}

# Note, the console stuff will have to function differently from Eggdrops,
# for example, Eggdrop requires at least a global +o flag to use the .console
# command.
# Tcldrop should be written for everyone to be able to use the .console command.
#
# A persons irc console channel should only be able to be set to one that
# they have at least one of these flags on: vfomn
# If they don't have the flags, they shouldn't be able to set their console to that channel.
# The console flags should be restricted according to what user flags a person has,
# more strict than Eggdrop anyway.
# The default irc console channel should NOT be the first channel in the [channels], like in Eggdrop.
# It SHOULD be the channel that the user has the highest flags on.

# NOTICE: There's a number of commands like putdcc/putidx, killdcc/killidx, validdcc/valididx, etc..
#         In Tcldrop, "dcc" commands may apply to both local AND remote users.
#         But "idx" commands may only apply to local connections.
#         The notation for a remote idx's is: <idx>:<handle@botname>
#         Eggdrop itself uses that notation, and so that's what we'll use..
#         Here's an example to make it more clear:
#         putdcc 7:FireEgl@Atlantica "Hello there."
#         That will send the text to a remote user.
#         And although putdcc is capable of sending to local users as well,
#         putidx is the preffered command for all local connections.

proc ::tcldrop::core::dcc::putdcc {idx text args} {
	if {[info exists ::idxlist($idx)]} {
		# FixMe: This should support all the %-variables listed in doc/text-substitutions.txt
		array set idxinfo {handle *}
		array set idxinfo $::idxlist($idx)
		array set options [list -subst 0 -substmap [list]]
		array set options $args
		if {$options(-subst)} {
			array set map [list {%B} ${::botnet-nick} {%N} $idxinfo(handle) {%V} "$::tcldrop(name) version $::tcldrop(version)"]
			array set map $options(-substmap)
			set text [string map [array get map] $text]
		}
		if {[set retval [catch { callfiltdcc $idx $text $args } text]]} { return $retval }
		putidx $idx $text $args
	} else {
		return 0
	}
}

proc ::tcldrop::core::dcc::putdccraw {idx size text} {
	putidx $idx [string range $text 0 $size]
}

proc ::tcldrop::core::dcc::killdcc {idx {reason {}}} {
	if {[idx2sock $idx] eq {stdout}} {
		# FixMe: We need a mechanism (killdcc/killidx bind type) to let the party modules close (or not close) the idx.
		after idle [list ::tcldrop::terminalparty::Write $idx]
	} else {
		killidx $idx
	}
}

#  dccsimul <idx> <text>
#    Description: simulates text typed in by the dcc user specified. Note
#      that in v0.9, this only simulated commands; now a command must be
#      preceded by a '.' to be simulated.
#    Returns: nothing
#    Module: core
proc ::tcldrop::core::dcc::dccsimul {idx line} {
	array set chatinfo [getidxinfo $idx]
	# Do the FILT binds:
	foreach {type flags mask proc} [bindlist filt] {
		if {$line eq {}} { break }
		if {[string match -nocase $mask $line] && [matchattr $chatinfo(handle) $flags]} {
			countbind $type $mask $proc
			if {[catch { set line [$proc $idx $line] } err]} {
				putlog "Error in script: $proc: $err"
				puterrlog "$::errorInfo"
			}
		}
	}
	if {[string index $line 0] eq {.}} {
		# Do the DCC binds:
		calldcc $chatinfo(handle) $idx $line
	} elseif {[string trim $line] != {}} {
		# Talk on the partyline:
		callparty chat $chatinfo(idx):$chatinfo(handle)@${::botnet-nick} line $line
	}
	# Update their laston info:
	setlaston $chatinfo(handle) [clock seconds] $chatinfo(traffictype)
}

# Returns the idx that $handle is on:
proc ::tcldrop::core::dcc::hand2idx {handle} {
	# FixMe: Make it return the last active user.
	lindex [listidx [list handle $handle]] 0
}

proc ::tcldrop::core::dcc::idx2hand {idx} { idxinfo $idx handle }

# Works just like the Eggdrop dcclist command,
# it shows the current socket connections.
proc ::tcldrop::core::dcc::dcclist {{type {}}} {
	global idxlist
	set dcclist {}
	foreach i [array names idxlist] {
		array set idxinfo {idx 0 handle * remote ? type ? other ? timestamp 0}
		array set idxinfo $idxlist($i)
		if {$type eq {} || [string match -nocase $type $idxinfo(type)]} {
			lappend dcclist [list $idxinfo(idx) $idxinfo(handle) $idxinfo(remote) $idxinfo(type) $idxinfo(other) $idxinfo(timestamp)]
		}
	}
	return $dcclist
}

# Just like dcclist except it's a flat list (no stupid sublists).
proc ::tcldrop::core::dcc::listdcc {{type {}}} {
	global idxlist
	set dcclist {}
	foreach i [array names idxlist] {
		array set idxinfo {idx 0 handle * remote ? type ? other ? timestamp 0}
		array set idxinfo $idxlist($i)
		if {$type eq {} || [string match -nocase $type $idxinfo(type)]} {
			lappend dcclist $idxinfo(idx) $idxinfo(handle) $idxinfo(remote) $idxinfo(type) $idxinfo(other) $idxinfo(timestamp)
		}
	}
	return $dcclist
}

#  dccdumpfile <idx> <filename>
#    Description: dumps out a file from the text directory to a dcc chat
#      user. The flag matching that's used everywhere else works here, too.
#    Returns: 1 for success, 0 for error.
#    Module: core
proc ::tcldrop::core::dcc::dccdumpfile {idx filename} {
	if {([valididx $idx]) && (![catch { open [file join ${::text-path} $filename] r } fid] || ![catch { open $filename r } fid] || ![catch { open [file join [file tail $filename]] r } fid])} {
		foreach l [split [read -nonewline $fid] \n] {
			putdcc $idx $l -subst 1
		}
		return 1
	} else {
		return 0
	}
}

#  getdccidle <idx>
#    Returns: number of seconds the dcc chat/file system/script user has
#      been idle
#    Module: core
proc ::tcldrop::core::dcc::getdccidle {idx} { expr { [clock seconds] - [idxinfo $idx timestamp] } }

# FILTDCC, filter for things before they're sent to the remote.  (Tcldrop-specific)
# Note: FILT is a filter for things we receive FROM the remote.  (Taken from Eggdrop)
proc ::tcldrop::core::dcc::callfiltdcc {idx text args} {
	foreach {type flags mask proc} [bindlist filtdcc] {
		if {[string equal -nocase $mask "$idx $text"]} {
			# If they return non-zero (error) it means to abort sending the text, otherwise send the new $text they return:
			if {[set retval [catch { $proc $idx $text $args } text]]} { return -code $retval $text }
		}
	}
	return $text
}

proc ::tcldrop::core::dcc::calldcc {handle idx arg} {
	# retval will be the number of binds that were triggered..
	set retval 0
	set arg [split $arg]
	set cmd [string range [lindex $arg 0] 1 end]
	set arg [join [lrange $arg 1 end]]
	foreach {type flags mask proc} [bindlist dcc] {
		if {[string equal -nocase $cmd $mask] && [matchattr $handle $flags]} {
			putloglev d * "tcl: dcc call: $proc $handle $idx $arg"
			incr retval
			if {[catch { $proc $handle $idx $arg } err]} {
				putlog "error in script: $proc: $err"
			} elseif {[string equal $err {1}]} {
				# Abort processing further binds if they return 1.
				break
			}
			countbind $type $mask $proc
		}
	}
	if {$retval == 0} { putidx $idx {What?  You need '.help'} }
	set retval
}

#    (19) CHON (stackable)
#         bind chon <flags> <mask> <proc>
#         proc-name <handle> <idx>
#
#         Description: when someone first enters the party-line area of the
#           bot via dcc chat or telnet, this is triggered before they are
#           connected to a chat channel (so, yes, you can change the channel
#           in a 'chon' proc). mask is matched against the handle and supports
#           wildcards. This is NOT triggered when someone returns from the
#           file area, etc.
#         Module: core
proc ::tcldrop::core::dcc::callchon {handle idx} {
	foreach {type flags mask proc} [bindlist chon] {
		if {[string match -nocase $mask $handle] && [matchattr $handle $flags]} {
			if {[catch { $proc $handle $idx } err]} {
				putlog "error in script: $proc: $err"
				puterrlog $::errorInfo
			}
			countbind $type $mask $proc
		}
	}
}

#    (20) CHOF (stackable)
#         bind chof <flags> <mask> <proc>
#         proc-name <handle> <idx>
#
#         Description: triggered when someone leaves the party line to
#           disconnect from the bot. mask is matched against the handle and
#           can contain wildcards. Note that the connection may have already
#           been dropped by the user, so don't send output to the idx.
#         Module: core
proc ::tcldrop::core::dcc::callchof {handle idx {reason {}}} {
	foreach {type flags mask proc} [bindlist chof] {
		if {[string match -nocase $mask $handle] && [matchattr $handle $flags]} {
			if {[catch { $proc $handle $idx } err]} {
				putlog "error in script: $proc: $err"
			}
			countbind $type $mask $proc
		}
	}
}

# Here goes all the DCC binds:
# These are the CORE dcc commands only!  Any module related dcc commands should be put in their module directory.
proc ::tcldrop::core::dcc::TCL {handle idx text} {
	putcmdlog "#$handle# tcl $text"
	if {[catch { uplevel #0 $text } out]} {
		foreach l [split $out \n] { putdcc $idx "Tcl error: $l" }
	} else {
		putdcc $idx "Tcl: $out"
	}
	return 0
}

proc ::tcldrop::core::dcc::SET {handle idx text} {
	putcmdlog "#$handle# set $text"
	if {[catch { uplevel \#0 [list eval set $text] } out]} {
		set out "Error: $out"
	} else {
		set out "Currently: $out"
	}
	putdcc $idx $out
	return 0
}

proc ::tcldrop::core::dcc::HELP {handle idx text} {
	putcmdlog "#$handle# help $text"
	if {$text eq {}} { set text {help} }
	foreach {f l} [help dcc $text] { if {$f eq {-} || [matchattr $handle $f]} { putdcc $idx $l } }
	return 0
}

proc ::tcldrop::core::dcc::QUIT {handle idx text} {
	putcmdlog "#$handle# quit $text"
	putdcc $idx {Bu-Bye!}
	# FixMe: This should be $nick $uhost instead:
	putlog "[format [lang 0xe16] $handle $idx]"
	if {[set chan [getchan $idx]] != "-1"} { callchpt ${::botnet-nick} $handle $idx $chan $text }
	killdcc $idx $text
	callchof $handle $idx
	return 0
}

proc ::tcldrop::core::dcc::WHOAMI {handle idx text} {
	putcmdlog "#$handle# whoami $text"
	putdcc $idx "You are $handle@${::botnet-nick} on idx $idx"
	return 0
}

proc ::tcldrop::core::dcc::-HOST {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set host [::tcldrop::core::slindex $text 1]
	# FixMe: This should let anyone with higher flags that the person they want to -host remove their host, I think..
	if {(([string equal -nocase $handle $who]) || ([matchattr $handle n])) && ([delhost $who $host])} {
		putcmdlog "#$handle# -host $who $host"
		putdcc $idx "Removed '$host' from $who"
	}
	return 0
}

proc ::tcldrop::core::dcc::NEWPASS {handle idx text} {
	chpass $handle $text
	putcmdlog "#$handle# newpass..."
	putdcc $idx "Changed password to '$text'"
	return 0
}

proc ::tcldrop::core::dcc::+USER {handle idx text} {
	set user [::tcldrop::core::slindex $text 0]
	set hostmask [::tcldrop::core::slindex $text 1]
	adduser $user $hostmask
	putcmdlog "#$handle# +user $text"
	putdcc $idx "Added $user ($hostmask) with [chattr $user] flags"
	return 0
}

proc ::tcldrop::core::dcc::-USER {handle idx text} {
	if {[deluser $text]} {
		putcmdlog "#$handle# -bot $text"
		putdcc $idx "Deleted $text."
	}
	return 0
}

proc ::tcldrop::core::dcc::WHOIS {handle idx text} {
	if {[validuser $text]} {
		putcmdlog "#$handle# whois $text"
		putdcc $idx {HANDLE    PASS FLAGS                        LAST}
		if {[getuser $text PASS] eq {}} { set haspass {no} } else { set haspass {yes} }
		if {[set laston [getuser $text LASTON]] != {}} {
			# FixMe: Make it show a very short time format eg. "11:06" or "05 Oct".
		}
		putdcc $idx [format {%-9.9s %-4.4s %-28.28s %-35.35s} [getuser $text handle] $haspass [getuser $text FLAGS] $laston]
		foreach c [channels] {
			# FixMe: Make it show the laston for the channel:
			putdcc $idx "[format {   %-11.11s %-28.28s %-12.12s} $c [getuser $text FLAGS $c] {}]"
		}
		# FixMe: Make it wrap the hosts like Eggdrop.
		putdcc $idx "  HOSTS: [join [getuser $text HOSTS] {, }]"
		putdcc $idx "  Saved Console Settings:"
		putdcc $idx "    [getuser $text CONSOLE]"
		if {[set comment [getuser $text COMMENT]] != {}} { putdcc $idx "  COMMENT: $comment" }
	} else {
		putdcc $idx {Can't find anyone matching that.}
	}
	return 0
}

proc ::tcldrop::core::dcc::CHADDR {handle idx text} {
	set bot [slindex $text 0]
	if {[matchattr $bot b]} {
		set address [split [slindex $text 1] "\\/:"]
		set botport [lindex $address 1]
		set userport [lindex $address 2]
		if {$userport eq {}} { set userport $botport }
		set address [lindex $address 0]
		setuser $bot BOTADDR [list $address $botport $userport]
		putcmdlog "#$handle# chaddr $text"
		putdcc $idx {Changed bots address.}
	}
	return 0
}

# FixMe: Allow non-owners to use this securely:
proc ::tcldrop::core::dcc::CHPASS {handle idx text} {
	set who [slindex $text 0]
	set pass [slindex $text 1]
	chpass $who $pass
	putcmdlog "#$handle# chpass $who \[something\]"
	putdcc $idx {Changed password.}
	return 0
}

proc ::tcldrop::core::dcc::+HOST {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set host [::tcldrop::core::slindex $text 1]
	# FixMe: This shouldn't be restricted to owners:
	if {[string equal -nocase $who $handle] || [matchattr $handle n]} {
		addhost $who $host
		putcmdlog "#$handle# +host $text"
		putdcc $idx "Added '$host' to $who."
	}
	return 0
}

proc ::tcldrop::core::dcc::CHATTR {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set changes [::tcldrop::core::slindex $text 1]
	set channel [::tcldrop::core::slindex $text 2]
	if {[set chattr [chattr $who $changes $channel]] eq {*}} {
		putdcc {No such user.}
	} else {
		putcmdlog "#$handle# chattr $text"
		putdcc $idx "Global flags for $who are now +[lindex [split $chattr |] 0]"
		if {$channel != {}} {
			putdcc $idx "Channel flags for $who on $channel are now +[lindex [split $chattr |] end]."
		}
	}
	return 0
}

proc ::tcldrop::core::dcc::SAVE {handle idx text} {
	putcmdlog "#$handle# save $text"
	putdcc $idx {Saving...}
	after idle [list after 0 [list callevent save]]
	return 0
}

proc ::tcldrop::core::dcc::UPTIME {handle idx text} {
	putcmdlog "#$handle# uptime $text"
	# FixMe: Add an [uptime] proc that returns the seconds that the bot has been running.
	putdcc $idx "Online for [duration [uptime]]."
	return 0
}

proc ::tcldrop::core::dcc::BACKUP {handle idx text} {
	putcmdlog "#$handle# backup $text"
	putdcc $idx {Backing up data files...}
	# FixMe: Add a [backup] command that calls the "backup" bindings. The bindings should in turn do the backing up of the user/channel files.
	after idle [list backup]
	return 0
}

proc ::tcldrop::core::dcc::COMMENT {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set comment [::tcldrop::core::slindex $text 1]
	setuser $who COMMENT $comment
	putcmdlog "#$handle# comment $text"
	putdcc $idx {Changed comment.}
	return 0
}

proc ::tcldrop::core::dcc::RELOAD {handle idx text} {
	putcmdlog "#$handle# reload $text"
	putdcc $idx {Reloading user file...}
	reload
	return 0
}

proc ::tcldrop::core::dcc::REHASH {handle idx text} {
	putcmdlog "#$handle# rehash $text"
	putdcc $idx {Rehashing..}
	rehash
	return 0
}

proc ::tcldrop::core::dcc::RESTART {handle idx text} {
	putcmdlog "#$handle# restart $text"
	putdcc $idx {Restarting..}
	restart
	return 0
}

proc ::tcldrop::core::dcc::SHUTDOWN {handle idx text} {
	putcmdlog "#$handle# shutdown $text"
	putdcc $idx {Shutting Down...}
	if {$text eq {}} { set text {nyoooooooo...} }
	after idle [list after 0 [list shutdown $text]]
}

proc ::tcldrop::core::dcc::LOADMOD {handle idx text} {
	loadmodule $text
	putcmdlog "#$handle# loadmod $text"
	putdcc $idx "Module loaded: $text"
	return 0
}

proc ::tcldrop::core::dcc::UNLOADMOD {handle idx text} {
	unloadmodule $text
	putcmdlog "#$handle# unloadmod $text"
	putdcc $idx "Module unloaded: $text"
	return 0
}

proc ::tcldrop::core::dcc::DCCSTAT {handle idx text} {
	putcmdlog "#$handle# dccstat $text"
	putdcc $idx {IDX SOCKET NAME       INFORMATION                            TYPE MODULE}
	putdcc $idx {--- ------ ---------- -------------------------------------- ---- ------}
	global idxlist
	foreach i [array names idxlist] {
		array set idxinfo {sock {0} handle {*} other {?} port {0} ident {-telnet} hostname {255.255.255.255} info {} module {script}}
		array set idxinfo $idxlist($i)
		if {$idxinfo(info) eq {}} { set idxinfo(info) "$idxinfo(ident)@$idxinfo(hostname):$idxinfo(port)" }
		putdcc $idx "[format {%-3.3s %-6.6s %-10.10s %-38.38s %-4.4s %-14.14s} $i $idxinfo(sock) $idxinfo(handle) $idxinfo(info) $idxinfo(other) $idxinfo(module)]"
	}
	return 0
}

proc ::tcldrop::core::dcc::TRAFFIC {handle idx text} {
	putcmdlog "#$handle# traffic $text"
	putdcc $idx {Traffic Since Last Restart}
	putdcc $idx {==========================}
	foreach info [traffic $text] {
		putdcc $idx "[lindex $info 0]:"
		# FixMe: Convert bytes to kbytes or mbytes (to make it look like Eggdrop does).
		putdcc $idx "  out: [lindex $info 4] bytes ([lindex $info 3] bytes since last daily reset)"
		putdcc $idx "   in: [lindex $info 2] bytes ([lindex $info 1] bytes since last daily reset)"
	}
	return 0
}

proc ::tcldrop::core::dcc::MODULES {handle idx text} {
	putcmdlog "#$handle# modules $text"
	putdcc $idx {Modules loaded:}
	foreach m [modules] { putdcc $idx "  Module: [lindex $m 0] (v[lindex $m 1])" }
	putdcc $idx {End of modules list.}
	return 0
}

proc ::tcldrop::core::dcc::STATUS {handle idx text} {
	putcmdlog "#$handle# status $text"
	putdcc $idx "I am ${::botnet-nick}, running Tcldrop v$::tcldrop(version): [countusers] users."
	putdcc $idx "Online for [duration [expr { [clock seconds] - $::uptime }]]"
	putdcc $idx "Admin: $::owner"
	putdcc $idx "Config file: $::config"
	putdcc $idx "OS: $::tcl_platform(os) $::tcl_platform(osVersion)"
	putdcc $idx "Tcl library: $::tcl_library"
	putdcc $idx "Tcl version: $::tcl_patchLevel"
	putdcc $idx "Loaded module information: "
	return 0
}

if {[info exists ::tcl::proc_counter]} {
	bind dcc n proc_counter ::tcldrop::core::dcc::PROC_COUNTS
	bind dcc n proc_counts ::tcldrop::core::dcc::PROC_COUNTS
	proc ::tcldrop::core::dcc::PROC_COUNTS {handle idx text} {
		set counts {}
		foreach {p c} [array get ::tcl::proc_counter] {
			if {$c > 1000} {
				lappend counts [list $c $p]
			}
		}
		foreach l [lsort -index 0 -integer $counts] {
			putdcc $idx $l
		}
	}
}

bind load - core::dcc ::tcldrop::core::dcc::LOAD -priority 0
proc ::tcldrop::core::dcc::LOAD {module} {
	bind dcc n tcl ::tcldrop::core::dcc::TCL
	bind dcc n set ::tcldrop::core::dcc::SET
	bind dcc - help ::tcldrop::core::dcc::HELP
	bind dcc - quit ::tcldrop::core::dcc::QUIT
	bind dcc - whoami ::tcldrop::core::dcc::WHOAMI
	bind dcc - -host ::tcldrop::core::dcc::-HOST
	bind dcc - newpass ::tcldrop::core::dcc::NEWPASS
	bind dcc nm +user ::tcldrop::core::dcc::+USER
	bind dcc n -user ::tcldrop::core::dcc::-USER
	bind dcc mntof whois ::tcldrop::core::dcc::WHOIS
	bind dcc nmt chaddr ::tcldrop::core::dcc::CHADDR
	bind dcc n chpass ::tcldrop::core::dcc::CHPASS
	bind dcc - +host ::tcldrop::core::dcc::+HOST
	bind dcc n chattr ::tcldrop::core::dcc::CHATTR
	bind dcc nmto save ::tcldrop::core::dcc::SAVE
	bind dcc nmtof uptime ::tcldrop::core::dcc::UPTIME
	bind dcc nmt backup ::tcldrop::core::dcc::BACKUP
	bind dcc n comment ::tcldrop::core::dcc::COMMENT
	bind dcc n reload ::tcldrop::core::dcc::RELOAD
	bind dcc n rehash ::tcldrop::core::dcc::REHASH
	bind dcc n restart ::tcldrop::core::dcc::RESTART
	bind dcc n die ::tcldrop::core::dcc::SHUTDOWN
	bind dcc n shutdown ::tcldrop::core::dcc::SHUTDOWN
	bind dcc n loadmod ::tcldrop::core::dcc::LOADMOD
	bind dcc n unloadmod ::tcldrop::core::dcc::UNLOADMOD
	bind dcc n dccstat ::tcldrop::core::dcc::DCCSTAT
	bind dcc n traffic ::tcldrop::core::dcc::TRAFFIC
	bind dcc n modules ::tcldrop::core::dcc::MODULES
	bind dcc n status ::tcldrop::core::dcc::STATUS -priority 1
	bind dcc n stat ::tcldrop::core::dcc::STATUS -priority 1
	loadhelp core.help
	loadhelp cmds1.help
	loadhelp cmds2.help
	loadhelp [file join set cmds1.help]
}

bind unld - core::dcc ::tcldrop::core::dcc::UNLD
proc ::tcldrop::core::dcc::UNLD {module} {
	unbind dcc * * ::tcldrop::core::dcc::*
	unloadhelp core.help
	unloadhelp cmds1.help
	unloadhelp cmds2.help
	unloadhelp [file join set cmds1.help]
	return 1
}
