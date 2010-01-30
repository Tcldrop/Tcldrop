# core/dcc --
#	Handles:
#		* All core DCC binds.
#
# $Id$
#
# Copyright (C) 2003-2010 Tcldrop Development Team <Tcldrop-Dev>
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
#	core::dcc module for tcldrop.  (REQUIRED)
#

namespace eval ::tcldrop::core::dcc {
	variable name {core::dcc}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	if {![info exists ::tcldrop]} { return }
	variable depends {core dcc core::users core::conn party console}
	variable predepends {dcc}
	variable author {Tcldrop-Dev}
	variable description {All core DCC binds.}
	variable rcsid {$Id$}
	variable commands [list ]
	namespace path [list ::tcldrop]
	namespace unknown unknown
	# putdccall getdccaway dccbroadcast putdccbut
	namespace export {*}$commands
}

# These are the CORE dcc commands only!
# Any module related dcc commands should be put in their module directory.

# Usage: tcl <command>
proc ::tcldrop::core::dcc::TCL {handle idx text} {
	putcmdlog "#$handle# tcl $text"
	if {[catch { uplevel #0 $text } out]} {
		foreach l [split $out \n] { putdcc $idx "Tcl error: $l" }
	} else {
		putdcc $idx "Tcl: $out"
	}
	return 0
}

# Usage: set <variable> [value]
proc ::tcldrop::core::dcc::SET {handle idx text} {
	putcmdlog "#$handle# set $text"
	if {[catch { uplevel \#0 [list eval set $text] } out]} {
		set out "[mc {Error}]: $out"
	} else {
		set out "[mc {Currently}]: $out"
	}
	putdcc $idx $out
	return 0
}

# Usage: help [command]
#        help all
#        help <module> module
proc ::tcldrop::core::dcc::HELP {handle idx text} {
	putcmdlog "#$handle# help $text"
	if {$text eq {}} { set text {help} }
	foreach {f l} [help dcc $text] { if {$f eq {-} || [matchattr $handle $f]} { putdcc $idx $l -subst 1 } }
	return 0
}

# Usage: quit [comment]
proc ::tcldrop::core::dcc::QUIT {handle idx text} {
	putcmdlog "#$handle# quit $text"
	putdcc $idx [mc_handle $handle {Bu-Bye!}]
	# FixMe: This should be $nick $uhost instead:
	putlog "[mc {DCC connection closed (%s!%s)} $handle $idx]"
	if {[set chan [getchan $idx]] != "-1"} { callchpt ${::botnet-nick} $handle $idx $chan $text }
	killdcc $idx $text
	callchof $handle $idx
	return 0
}

# Usage: whoami
proc ::tcldrop::core::dcc::WHOAMI {handle idx text} {
	putcmdlog "#$handle# whoami $text"
	putdcc $idx "[mc_handle $handle {You are %1$s on idx %2$d.} $handle@${::botnet-nick} $idx]"
	return 0
}

# Usage: -host [handle] <hostmask>
proc ::tcldrop::core::dcc::-HOST {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set host [::tcldrop::core::slindex $text 1]
	if {$text eq {}} {
		# Usage:
		putdcc $idx "[mc_handle $handle {Usage}]: -host \[handle\] <hostmask>"
		return 0
	} elseif {[sllength $text] == 1} {
		set who $handle
		set host [slindex $text 0]
	}
	# FixMe: This should let anyone with higher flags that the person they want to -host remove their host, I think..
	if {(([string equal -nocase $handle $who]) || ([matchattr $handle n])) && ([delhost $who $host])} {
		putcmdlog "#$handle# -host $who $host"
		putdcc $idx "[mc_handle $handle {Removed '%1$s' from %2$s} $host $who]"
	} else {
		# Failed.
		putdcc $idx "[mc_handle $handle {Failed.}]"
	}
	return 0
}

# Usage: newpass <password>
proc ::tcldrop::core::dcc::NEWPASS {handle idx text} {
	chpass $handle $text
	putcmdlog "#$handle# newpass..."
	putdcc $idx "[mc_handle $handle {Changed password to '%s'} $text]"
	return 0
}

# Usage: +user <handle> [hostmask]
proc ::tcldrop::core::dcc::+USER {handle idx text} {
	set user [::tcldrop::core::slindex $text 0]
	set hostmask [::tcldrop::core::slindex $text 1]
	adduser $user $hostmask
	putcmdlog "#$handle# +user $text"
	putdcc $idx "[mc_handle $handle {Added %1$s (%2$s) with %3$s flags} $user $hostmask [chattr $user]]"
	return 0
}

# Usage: -user <handle>
proc ::tcldrop::core::dcc::-USER {handle idx text} {
	if {[deluser $text]} {
		putcmdlog "#$handle# -bot $text"
		putdcc $idx "[mc_handle $handle {Deleted %s.} $text]"
	}
	return 0
}

# Usage: whois <handle>
proc ::tcldrop::core::dcc::WHOIS {handle idx text} {
	if {[validuser $text]} {
		putcmdlog "#$handle# whois $text"
		# FixMe: column width should auto-adjust based on handlen
		putdcc $idx {HANDLE    PASS FLAGS                        LAST}
		if {[getuser $text PASS] eq {}} { set haspass {no} } else { set haspass {yes} }
		if {[set laston [getuser $text LASTON]] != {}} {
			# FixMe: Make it show a very short time format eg. "11:06" or "05 Oct".
			set laston "[lindex $laston 0] ([lindex $laston 1])"
		}
		set flags [getuser $text FLAGS]
		putdcc $idx [format {%-9.9s %-4.4s %-28.28s %-35.35s} [getuser $text handle] $haspass [dict get $flags global] $laston]
		# Discard the global flags now, so we can loop over what's left (the channel flags):
		dict unset flags global
		foreach {c f} [dict get $flags] {
			# FixMe: Make it show the laston for the channel:
			putdcc $idx "[format {   %-11.11s %-28.28s %-12.12s} $c $f {}]"
		}
		if {[matchattr $text b]} {
			lassign [getuser $text botaddr] address botport userport
			putdcc $idx "  ADDRESS: $address"
			putdcc $idx "     users: $userport, bots: $botport"
			unset -nocomplain address botport userport
		}
		# Wrap hosts
		# FixMe: Turn this into a proc if something else uses a similar formatting.
		set hosts [getuser $text HOSTS]
		set block {}
		for {set pos 0} {$pos <= [llength $hosts]} {incr pos} {
			set current [lindex $hosts $pos]
			if {[string length [join [concat $block $current] {, }]] <= 60} {
				lappend block $current
			} else {
				lappend hostsList $block
				set block $current
			}
		}
		lappend hostsList $block
		putdcc $idx "  HOSTS: [join [lindex $hostsList 0] {, }]"
		foreach block [lrange $hostsList 1 end] { putdcc $idx "         [join $block {, }]" }
		unset -nocomplain hosts block pos current hostsList
		if {[set console [getuser $text CONSOLE]] != {}} {
			array set idxinfo $console
			putdcc $idx "  [mc_handle $handle {Saved Console Settings}]:"
			# Channel:
			putdcc $idx "    [mc_handle $handle {Channel:}] $idxinfo(console-channel)"
			# Console flags:  Strip flags:  Echo:
			putdcc $idx "    [mc_handle $handle {Console flags:}] $idxinfo(console-levels), [mc_handle $handle {Strip flags:}] $idxinfo(console-strip), [mc_handle $handle {Echo:}] $idxinfo(console-echo)"
			# Page setting:  Console channel:
			putdcc $idx "    [mc_handle $handle {Page setting:} $idxinfo(console-page), [mc_handle $handle {Console channel:} $idxinfo(console-chan)]"
		}
		if {[set comment [getuser $text COMMENT]] != {}} { putdcc $idx "  COMMENT: $comment" }
	} else {
		putdcc $idx [mc_handle $handle {Can't find anyone matching that.}]
	}
	return 0
}

# Usage: chaddr <bot> <address[:bot port[/user port]]>
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
		putdcc $idx "[mc_handle $handle {Changed bots address.}]"
	}
	return 0
}

# Usage: chpass <handle> [newpassword]
# FixMe: Allow non-owners to use this securely:
proc ::tcldrop::core::dcc::CHPASS {handle idx text} {
	set who [slindex $text 0]
	set pass [slindex $text 1]
	chpass $who $pass
	putcmdlog "#$handle# chpass $who \[something\]"
	putdcc $idx [mc_handle $handle {Changed password.}]
	return 0
}

# Usage: +host [handle] <newhostmask>
proc ::tcldrop::core::dcc::+HOST {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set host [::tcldrop::core::slindex $text 1]
	if {$text eq {}} {
		# Usage:
		putdcc $idx "[mc_handle $handle {Usage}]: +host \[handle\] <newhostmask>"
		return 0
	} elseif {[::tcldrop::core::sllength $text] == 1} {
		set who $handle
		set host [::tcldrop::core::slindex $text 0]
	}
	# FixMe: This shouldn't be restricted to owners:
	if {[string equal -nocase $who $handle] || [matchattr $handle n]} {
		addhost $who $host
		putcmdlog "#$handle# +host $text"
		putdcc $idx "[mc_handle $handle {Added '%1$s' to %2$s.} $host $who]"
	}
	return 0
}

# Usage chattr <handle> [flags] [channel]
proc ::tcldrop::core::dcc::CHATTR {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set changes [::tcldrop::core::slindex $text 1]
	set channel [::tcldrop::core::slindex $text 2]
	if {[set chattr [chattr $who $changes $channel]] eq {*}} {
		putdcc "[mc_handle $handle {No such user.}]"
	} else {
		putcmdlog "#$handle# chattr $text"
		putdcc $idx "[mc_handle $handle {Global flags for %1$s are now +%2$s} $who [lindex [split $chattr |] 0]]"
		if {$channel != {}} {
			putdcc $idx "[mc_handle $handle {Channel flags for %1$s on %2$s are now +%2$s.} $who $channel [lindex [split $chattr |] end]]"
		}
	}
	return 0
}

# Usage: save
proc ::tcldrop::core::dcc::SAVE {handle idx text} {
	putcmdlog "#$handle# save $text"
	putdcc $idx "[mc_handle $handle {Saving...}]"
	afteridle callevent save
	return 0
}

# Usage: uptime
# FixMe: add botnet support for this?
proc ::tcldrop::core::dcc::UPTIME {handle idx text} {
	putcmdlog "#$handle# uptime $text"
	putdcc $idx "[mc_handle $handle {Online for %s.} [duration [uptime]]]"
	return 0
}

# Usage: backup
proc ::tcldrop::core::dcc::BACKUP {handle idx text} {
	putcmdlog "#$handle# backup $text"
	putdcc $idx "[mc_handle $handle {Backing up data files...}]"
	# FixMe: Add a [backup] command that calls the "backup" bindings. The bindings should in turn do the backing up of the user/channel files.
	after idle [list backup]
	return 0
}

# Usage: comment <user> <comment>
proc ::tcldrop::core::dcc::COMMENT {handle idx text} {
	set who [::tcldrop::core::slindex $text 0]
	set comment [::tcldrop::core::slindex $text 1]
	setuser $who COMMENT $comment
	putcmdlog "#$handle# comment $text"
	putdcc $idx [mc_handle $handle {Changed comment.}]
	return 0
}

# Usage: reload
proc ::tcldrop::core::dcc::RELOAD {handle idx text} {
	putcmdlog "#$handle# reload $text"
	putdcc $idx [mc_handle $handle {Reloading user file...}]
	reload
	return 0
}

# Usage: rehash
proc ::tcldrop::core::dcc::REHASH {handle idx text} {
	putcmdlog "#$handle# rehash $text"
	putdcc $idx [mc_handle $handle {Rehashing..}]
	rehash
	return 0
}

# Usage: restart
proc ::tcldrop::core::dcc::RESTART {handle idx text} {
	putcmdlog "#$handle# restart $text"
	putdcc $idx [mc_handle $handle {Restarting..}]
	restart
	return 0
}

# Usage: shutdown [reason]
proc ::tcldrop::core::dcc::SHUTDOWN {handle idx text} {
	putcmdlog "#$handle# shutdown $text"
	putdcc $idx [mc_handle $handle {Shutting Down...}]
	if {$text eq {}} { set text [mc {nyoooooooo...}] }
	afteridle shutdown $text
}

# Usage: loadmod <module>
proc ::tcldrop::core::dcc::LOADMOD {handle idx text} {
	loadmodule $text
	putcmdlog "#$handle# loadmod $text"
	putdcc $idx "[mc_handle $handle {Module loaded: %s} $text]"
	return 0
}

# Usage: unloadmod <module>
proc ::tcldrop::core::dcc::UNLOADMOD {handle idx text} {
	unloadmodule $text
	putcmdlog "#$handle# unloadmod $text"
	putdcc $idx "[mc_handle $handle {Module unloaded: %s} $text]"
	return 0
}

# Usage: dccstat
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

# Usage: traffic
# FixMe: add botnet support for this? Eggdrop doesn't have that but seems good to me
proc ::tcldrop::core::dcc::TRAFFIC {handle idx text} {
	putcmdlog "#$handle# traffic $text"
	putdcc $idx [mc_handle $handle {Traffic since last restart}]
	putdcc $idx {==========================}
	foreach info [lsort [traffic]] {
		switch -nocase -- [lindex $info 0] {
			irc {
				putdcc $idx {IRC:}
			}
			partyline {
				putdcc $idx [mc_handle $handle {Partyline:}]
			}
			misc {
				putdcc $idx [mc_handle $handle {Misc:}]
			}
			total {
				putdcc $idx {---}
				putdcc $idx [mc_handle $handle {Total:}]
			}
			default {
				putdcc $idx "[lindex $info 0]:"
			}
		}
		# FixMe: Convert bytes to kbytes or mbytes (to make it look like Eggdrop does).
		# use this? http://wiki.tcl.tk/1676
		putdcc $idx "  [mc_handle $handle {out: %1$d bytes (%2$d bytes since last daily reset)} [lindex $info 4] [lindex $info 3]]"
		putdcc $idx "   [mc_handle $handle {in: %1$d bytes (%2$d bytes since last daily reset)} [lindex $info 2] [lindex $info 1]]"
	}
	return 0
}

# Usage: modules [botname]
# FixMe: fix botnet part
proc ::tcldrop::core::dcc::MODULES {handle idx text} {
	putcmdlog "#$handle# modules $text"
	putdcc $idx [mc_handle $handle {Modules loaded:}]
	foreach m [modules] { putdcc $idx "  [mc_handle $handle {Module: %1$s (v%2$s)} [lindex $m 0] [lindex $m 1]]" }
	putdcc $idx [mc_handle $handle {End of modules list.}]
	return 0
}

# Usage: status [all]
# FixMe: Add more stuff for the 'all' arg
proc ::tcldrop::core::dcc::STATUS {handle idx text} {
	putcmdlog "#$handle# status $text"
	putdcc $idx "[mc_handle $handle {I am %1$s, running Tcldrop v%1$s: %3$d users.} ${::botnet-nick} $::tcldrop(version) [countusers]]"
	putdcc $idx "[mc_handle $handle {Online for}] [duration [expr { [clock seconds] - $::uptime }]]"
	putdcc $idx "[mc_handle $handle {Admin}]: $::owner"
	putdcc $idx "[mc_handle $handle {Config file}]: $::config"
	putdcc $idx "[mc_handle $handle {OS}]: $::tcl_platform(os) $::tcl_platform(osVersion)"
	putdcc $idx "[mc_handle $handle {Tcl library}]: $::tcl_library"
	putdcc $idx "[mc_handle $handle {Tcl version}]: $::tcl_patchLevel"
	if {[info exists ::tcl_platform(threaded)] && $::tcl_platform(threaded)} { putdcc $idx "[mc_handle $handle {Tcl is threaded.}]" }
	# FixMe: Perhaps move some of these to relevant modules
	# FixMe: If some of these vars are _always_ set, remove the expr.
	if {[string equal -nocase [slindex $text 0] {all}]} {
		putdcc $idx "-"
		putdcc $idx "[mc_handle $handle {Botnet nickname}]: ${::botnet-nick}"
		putdcc $idx "[mc_handle $handle {Databases}]:"
		putdcc $idx "  [mc_handle $handle {Users}]   : [expr {[info exists ::userfile]?$::userfile:"${::database-basename}.users"}]"
		putdcc $idx "  [mc_handle $handle {Channels}]: [expr {[info exists ::userfile]?$::userfile:"${::database-basename}.channels"}]"
		putdcc $idx "  [mc_handle $handle {Ignores}] : [expr {[info exists ::userfile]?$::userfile:"${::database-basename}.ignores"}]"
		putdcc $idx "  [mc_handle $handle {Bans}]    : [expr {[info exists ::userfile]?$::userfile:"${::database-basename}.bans"}]"
		putdcc $idx "  [mc_handle $handle {Exempts}] : [expr {[info exists ::userfile]?$::userfile:"${::database-basename}.exempts"}]"
		putdcc $idx "  [mc_handle $handle {Invites}] : [expr {[info exists ::userfile]?$::userfile:"${::database-basename}.invites"}]"
		putdcc $idx "[mc_handle $handle {Directories}]:"
		putdcc $idx "  [mc_handle $handle {Help}]   : [expr {[info exists ::help-path]?${::help-path}:{}}]"
		putdcc $idx "  [mc_handle $handle {Temp}]   : [expr {[info exists ::temp-path]?${::temp-path}:{}}]"
		putdcc $idx "  [mc_handle $handle {Modules}]: [expr {[info exists ::mod-path]?${::mod-path}:{}}]"
		putdcc $idx "[mc_handle $handle {Motd}]: [file nativename [file join ${::text-path} $::motd]]"
		putdcc $idx "[mc_handle $handle {Telnet Banner}]: [file nativename [file join ${::text-path} ${::telnet-banner}]]"
		putdcc $idx "[mc_handle $handle {New users get flags [%1$s], notify: %2$s} [expr {[info exists ::default-flags]?${::default-flags}:{}}] [expr {[info exists ::notify-newusers]?${::notify-newusers}:0}]]"
		putdcc $idx "[mc_handle $handle {Permanent owner(s)}]: [expr {[info exists ::owner]?${::owner}:{}}]"
		putdcc $idx "[mc_handle $handle {Ignores last %d minutes.} [expr {[info exists ::ignore-time]?${::ignore-time}:0}]]"
	}
	putdcc $idx "[mc_handle $handle {Loaded module information}]: "
	return 0
}

# Usage: motd [botname]
# FixMe: fix botnet part
proc ::tcldrop::core::dcc::MOTD {handle idx text} {
	putcmdlog "#$handle# motd $text"
	if {![info exists ::motd] || ![dccdumpfile $idx ${::motd}]} { putdcc $idx "[mc_handle $handle {Welcome!}]" }
	return 0
}

# Usage: binds [type/match] [all]
# FixMe: needs more formatting love, add support for matching
proc ::tcldrop::core::dcc::BINDS {handle idx text} {
	putcmdlog "#$handle# binds $text"
	putdcc $idx [mc_handle $handle {Command bindings:}]
	putdcc $idx [format {%6s %4s %11s %17s %s}  {TYPE} {FLGS} {COMMAND} {HITS} {BINDING (TCL)}]
	set matches [binds]
	set format {%7s %-8s %-19s %5s %s}
	foreach bind $matches {
		lassign $bind type flags command hits binding
		putdcc $idx [format $format $type $flags $command $hits $binding]
	}
	if {[llength $matches] == 0} {
		putdcc $idx "[mc_handle $handle {No command bindings found that match %s} $text]"
	}
	return 0
}

proc ::tcldrop::core::dcc::PROFILER {handle idx text} {
	if {[set commands [info commands ::profiler::[slindex $text 0]]] ne {}} {
		if {[slrange $text 1 end] eq {}} {
			putdcc $idx [[lindex $commands 0]]
		} else {
			putdcc $idx [[lindex $commands 0] {*}[slrange $text 1 end]]
		}
	} elseif {[info commands ::profiler::print] ne {}} {
		if {$text eq {}} {
			putdcc $idx [::profiler::print]
		} else {
			putdcc $idx [::profiler::print $text]
		}
	}

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

::tcldrop::bind load - core::dcc ::tcldrop::core::dcc::LOAD -priority 0
proc ::tcldrop::core::dcc::LOAD {module} {
	checkmodule console
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
	bind dcc - mot ::tcldrop::core::dcc::MOTD
	bind dcc - motd ::tcldrop::core::dcc::MOTD
	bind dcc nm bind ::tcldrop::core::dcc::BINDS
	bind dcc nm binds ::tcldrop::core::dcc::BINDS
	bind dcc n profiler ::tcldrop::core::dcc::PROFILER
	loadhelp core.help
	loadhelp cmds1.help
	loadhelp cmds2.help
	loadhelp [file join set cmds1.help]
}

::tcldrop::bind unld - core::dcc ::tcldrop::core::dcc::UNLD
proc ::tcldrop::core::dcc::UNLD {module} {
	unbind dcc * * ::tcldrop::core::dcc::*
	unloadhelp core.help
	unloadhelp cmds1.help
	unloadhelp cmds2.help
	unloadhelp [file join set cmds1.help]
	return 1
}
