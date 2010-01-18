# core/dcc --
#	Handles:
#		* All core DCC binds and commands.
#	Depends: core::conn.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008,2009 Tcldrop Development Team <Tcldrop-Dev>
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
	variable commands [list dcclist putdcc putdccraw idx2hand hand2idx killdcc getdccidle getdccaway setdccaway getchan setchan calldcc callfilt callchon callchof dccdumpfile dccsimul]
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
#         putidx is the preferred command for all local connections.

proc ::tcldrop::core::dcc::putdcc {idx text args} {
	if {[valididx $idx]} {
		array set options [list -subst 0]
		array set options $args
		if {$options(-subst)} {
			# output "-" if the line is empty, this is like eggdrop
			if {$text eq {}} {
				set text {-}
			} else {
				set text [textsubst [idx2hand $idx] $text {*}$args]
			}
		}
		if {[set retval [catch { callfiltdcc $idx $text $args } text]]} { return $retval }
		putidx $idx $text $args
	} else {
		return -code error {invalid idx}
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
	if {[set line [callfilt $idx $line]] ne {}} {
		if {[string index $line 0] eq {.}} {
			# Do the DCC binds:
			calldcc [getidxinfo $idx handle] $idx $line
		} else {
			# Talk on the partyline:
			# FixMe: We need to check if chat is off (.chat off) (That's chat channel -1) rather than blindly sending it to the partyline:
			callparty chat ${idx}:[getidxinfo $idx handle]@${::botnet-nick} line $line
		}
		# Update their laston info:
		# FixMe: Should this be done here? (Probably not, because a killdcc/killidx could happen in one of the binds triggered above) Or at all? How often should laston be updated? Only when they join the dcc? Only when they join the partyline? And/Or when they leave the dcc/partyline?
		#setlaston $handle [getidxinfo $idx traffictype] [clock seconds]
	}
}

# Call FILT binds (Taken from Eggdrop):
proc ::tcldrop::core::dcc::callfilt {idx line} {
	# Do the FILT binds, if $line actually contains something::
	if {[string trim $line] ne {}} {
		set handle [getidxinfo $idx handle]
		foreach {type flags mask proc} [bindlist filt] {
			if {[string match -nocase $mask $line] && [matchattr $handle $flags]} {
				countbind $type $mask $proc
				if {[catch { set line [$proc $idx $line] } err]} {
					putlog "[mc {Error in script}]: $proc: $err"
					puterrlog "$::errorInfo"
				}
			}
		}
		return $line
	}
}

# Returns the idx that $handle is on:
proc ::tcldrop::core::dcc::hand2idx {handle} {
	# FixMe: Make it return the last active user.
	if {[set return [lindex [listidx handle $handle] 0]] eq {}} { return -1 } else { return $return }
}

proc ::tcldrop::core::dcc::idx2hand {idx} { getidxinfo $idx handle }

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
		foreach l [textsubst [idx2hand $idx] [read -nonewline $fid] -returnlist 1] {
			putdcc $idx $l
		}
		close $fid
		return 1
	} else {
		catch {close $fid}; # in case valididx = 0 and file open succeeds
		return 0
	}
}

#  getdccidle <idx>
#    Returns: number of seconds the dcc chat/file system/script user has
#      been idle
#    Module: core
proc ::tcldrop::core::dcc::getdccidle {idx} { expr { [clock seconds] - [getidxinfo $idx timestamp] } }


#  getdccaway <idx>
#    Returns: away message for a dcc chat user (or "" if the user is not
#      set away)
#    Module: core
proc ::tcldrop::core::dcc::getdccaway {idx} {
	if {[info exists ::idxlist($idx)] && [dict exists $::idxlist($idx) away]} {
		dict get $::idxlist($idx) away
	}
}

#  setdccaway <idx> <message>
#    Description: sets a party line user's away message and marks them away.
#     If set to "", the user is marked as no longer away.
#    Returns: nothing
#    Module: core
proc ::tcldrop::core::dcc::setdccaway {idx text} {
	if {[info exists ::idxlist($idx)]} {
		dict set ::idxlist($idx) away $text
	}
	# Notify the partyline:
	callparty away ${idx}:*@${::botnet-nick} line $text
}

#  getchan <idx>
#    Returns: the current party line channel for a user on the party line;
#      "0" indicates he's on the group party line, "-1" means he has chat off,
#      and a value from 1 to 99999 is a private channel
#    Module: core
# Note: This command is in this module because it can be used to determine if the partyline is used at all.
#       And because you can't use it to find the partyline chan of a user who's on a remote bot (it only works for LOCAL users).
proc ::tcldrop::core::dcc::getchan {idx} {
	getconsole $idx chan
}

#  setchan <idx> <channel>
#    Description: sets a party line user's channel. The party line user
#      is not notified that she is now on a new channel. A channel name
#      can be used (provided it exists).
#    Returns: nothing
#    Module: core
# -1 means none (Such as when doing ".chat off")
proc ::tcldrop::core::dcc::setchan {idx {channel {-1}}} {
	if {[info exists ::idxlist($idx)]} {
		setconsole $idx chan $channel
		array set idxinfo $::idxlist($idx)
		# FixMe: Need to create a command that can return what "flag" to use based on user-flags
		# flag is one of: * (owner), + (master), @ (op), or % (botnet master).
		# FixMe: Allow hiding the userhost like the nohostwhowhom1.6.16.patch by BarkerJr does, except make it a per-user option and/or a global setting to turn it on/off:
		callparty join $idx:$idxinfo(handle)@${::botnet-nick} bot ${::botnet-nick} handle $idxinfo(handle) idx $idx chan $channel flag "" userhost $idxinfo(remote)
	}
}

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
				putlog "[mc {Error in script}]: $proc: $err"
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
# This is in this module for the same reasons [getchan] and [setchan] are here.
# This is not really a partyline related bind.  As you could trigger this bind without involving the partyline at all.
proc ::tcldrop::core::dcc::callchon {handle idx} {
	foreach {type flags mask proc} [bindlist chon] {
		if {[string match -nocase $mask $handle] && [matchattr $handle $flags]} {
			if {[catch { $proc $handle $idx } err]} {
				putlog "[mc {Error in script}]: $proc: $err"
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
# This is in this module for the same reasons [getchan] and [setchan] are here.
# This is not really a partyline related bind.  As you could trigger this bind without involving the partyline at all.
proc ::tcldrop::core::dcc::callchof {handle idx {reason {}}} {
	foreach {type flags mask proc} [bindlist chof] {
		if {[string match -nocase $mask $handle] && [matchattr $handle $flags]} {
			if {[catch { $proc $handle $idx } err]} {
				putlog "[mc {Error in script}]: $proc: $err"
			}
			countbind $type $mask $proc
		}
	}
}


# FixMe: This code is flawed, perhaps forget it and start over like it never existed:
#bind chon - * ::tcldrop::party::CHON
#proc ::tcldrop::party::CHON {handle idx} {
#	party add ${idx}:${handle}@${::botnet-nick} idx $idx handle $handle bot ${::botnet-nick}
#}
#bind chon - * ::tcldrop::party::CHON -priority 0
#proc ::tcldrop::party::CHON {handle idx} {
#	if {[set chan [getchan $idx]] != -1} {
#		callparty connect ${idx}:${handle}@${::botnet-nick} idx $idx handle $handle bot ${::botnet-nick} chan $chan line {Join.}
#	}
#}
#bind chof - * ::tcldrop::party::CHOF -priority 0
#proc ::tcldrop::party::CHOF {handle idx} {
#	if {[set chan [getchan $idx]] != -1} {
#		callparty disconnect ${idx}:${handle}@${::botnet-nick} idx $idx handle $handle bot ${::botnet-nick} chan $chan line {Join.}
#	}
#}


# Here goes all the DCC binds:
# These are the CORE dcc commands only!  Any module related dcc commands should be put in their module directory.

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

# FixMe: Consider removing this proc and instead have separate log procs for each dcc::* module.
proc ::tcldrop::core::dcc::LOG {levels channel text {tags {}}} {
	global idxlist
	foreach i [array names idxlist] {
		if {[dict exists $idxlist($i) console-channel] && ([string match -nocase $channel [dict get $idxlist($i) console-channel]] || [string match -nocase [dict get $idxlist($i) console-channel] $channel]) && [checkflags $levels [dict get $idxlist($i) console-levels]] && ((![dict exists $tags save] || ![dict get $tags save]) || ![dict get $idxlist($i) console-quiet-save])} {
			switch -- [dict get $idxlist($i) console-log-time] {
				{1} { putdcc $i "[clock format [clock seconds] -format {[%H:%M]}] $text" }
				{2} { putdcc $i "[clock format [clock seconds] -format {[%T]}] $text" }
				{0} - {} - { } { }
				{default} {
					# Use a custom clock format.. Should this use a separate variable instead?
					putdcc $i "[clock format [clock seconds] -format [dict get $idxlist($i) console-log-time]] $text"
				}
			}
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
	# FixMe: Consider removing this bind and instead have separate log binds for each dcc::* module..
	bind log - * ::tcldrop::core::dcc::LOG
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
