# dcc --
#	Handles:
#		* All DCC commands.
#	Depends: core::conn.
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
#	dcc module for tcldrop.  (REQUIRED)
#
# This module provides all of the telnet/dcc chat related stuff.
# This includes user AND bot AND script connections.

namespace eval ::tcldrop::dcc {
	variable name {dcc}
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
	namespace path [list ::tcldrop]
	namespace unknown unknown
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

proc ::tcldrop::dcc::putdcc {idx text args} {
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

proc ::tcldrop::dcc::putdccraw {idx size text} {
	putidx $idx [string range $text 0 $size]
}

proc ::tcldrop::dcc::killdcc {idx {reason {}}} {
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
proc ::tcldrop::dcc::dccsimul {idx line} {
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
proc ::tcldrop::dcc::callfilt {idx line} {
	# Do the FILT binds, if $line actually contains something::
	if {[string trim $line] ne {}} {
		set handle [getidxinfo $idx handle]
		foreach {type flags mask proc} [bindlist filt] {
			if {[bindmatch $mask $line] && [matchattr $handle $flags]} {
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
proc ::tcldrop::dcc::hand2idx {handle} {
	# FixMe: Make it return the last active user.
	if {[set return [lindex [listidx handle $handle] 0]] eq {}} { return -1 } else { return $return }
}

proc ::tcldrop::dcc::idx2hand {idx} { getidxinfo $idx handle }

# Works just like the Eggdrop dcclist command,
# it shows the current socket connections.
proc ::tcldrop::dcc::dcclist {{type {}}} {
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
proc ::tcldrop::dcc::listdcc {{type {}}} {
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
proc ::tcldrop::dcc::dccdumpfile {idx filename} {
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
proc ::tcldrop::dcc::getdccidle {idx} { expr { [clock seconds] - [getidxinfo $idx timestamp] } }


#  getdccaway <idx>
#    Returns: away message for a dcc chat user (or "" if the user is not
#      set away)
#    Module: core
proc ::tcldrop::dcc::getdccaway {idx} {
	if {[info exists ::idxlist($idx)] && [dict exists $::idxlist($idx) away]} {
		dict get $::idxlist($idx) away
	}
}

#  setdccaway <idx> <message>
#    Description: sets a party line user's away message and marks them away.
#     If set to "", the user is marked as no longer away.
#    Returns: nothing
#    Module: core
proc ::tcldrop::dcc::setdccaway {idx text} {
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
proc ::tcldrop::dcc::getchan {idx} {
	getconsole $idx chan
}

#  setchan <idx> <channel>
#    Description: sets a party line user's channel. The party line user
#      is not notified that she is now on a new channel. A channel name
#      can be used (provided it exists).
#    Returns: nothing
#    Module: core
# -1 means none (Such as when doing ".chat off")
proc ::tcldrop::dcc::setchan {idx {channel {-1}}} {
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
proc ::tcldrop::dcc::callfiltdcc {idx text args} {
	foreach {type flags mask proc} [bindlist filtdcc] {
		if {[bindmatch $mask "$idx $text"]} {
			# If they return non-zero (error) it means to abort sending the text, otherwise send the new $text they return:
			if {[set retval [catch { $proc $idx $text $args } text]]} { return -code $retval $text }
		}
	}
	return $text
}

proc ::tcldrop::dcc::calldcc {handle idx arg} {
	# retval will be the number of binds that were triggered..
	set retval 0
	set arg [split $arg]
	set cmd [string range [lindex $arg 0] 1 end]
	set arg [join [lrange $arg 1 end]]
	foreach {type flags mask proc} [bindlist dcc] {
		if {[bindmatch $cmd $mask] && [matchattr $handle $flags]} {
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
proc ::tcldrop::dcc::callchon {handle idx} {
	foreach {type flags mask proc} [bindlist chon] {
		if {[bindmatch $mask $handle] && [matchattr $handle $flags]} {
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
proc ::tcldrop::dcc::callchof {handle idx {reason {}}} {
	foreach {type flags mask proc} [bindlist chof] {
		if {[bindmatch $mask $handle] && [matchattr $handle $flags]} {
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

# FixMe: Consider removing this proc and instead have separate log procs for each dcc::* module.
proc ::tcldrop::dcc::LOG {levels channel text {tags {}}} {
	global idxlist
	foreach i [array names idxlist] {
		if {[dict exists $idxlist($i) console-channel] && ([string match -nocase $channel [dict get $idxlist($i) console-channel]] || [string match -nocase [dict get $idxlist($i) console-channel] $channel]) && [checkflags $levels [dict get $idxlist($i) console-levels]] && ((![dict exists $tags save] || ![dict get $tags save]) || ![dict get $idxlist($i) console-quiet-save])} {
			switch -- [dict get $idxlist($i) console-log-time] {
				{1} { putdcc $i "[clock format [clock seconds] -format {[%H:%M]}] $text" }
				{2} { putdcc $i "[clock format [clock seconds] -format {[%T]}] $text" }
				{0} - {} - { } { putdcc $i "$text" }
				{default} {
					# If it's not 0, 1, or 2, then use the custom timestamp-format:
					putdcc $i "[clock format [clock seconds] -format [dict get $idxlist($i) console-timestamp-format]] $text"
				}
			}
		}
	}
}

::tcldrop::bind load - dcc ::tcldrop::dcc::LOAD -priority 0
proc ::tcldrop::dcc::LOAD {module} {
	checkmodule console
	# FixMe: Consider removing this bind and instead have separate log binds for each dcc::* module..
	bind log - * ::tcldrop::dcc::LOG
	# This module is critical, don't let it be unloaded:
	::tcldrop::bind unld - dcc ::tcldrop::dcc::UNLD -priority 0
	proc ::tcldrop::dcc::UNLD {module} { return 1 }
}

