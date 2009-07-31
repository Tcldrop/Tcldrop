# irc/main --
#	Handles:
#		* Provides all IRC related commands.
#	Depends: server, channels.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007 Tcldrop Development Team <Tcldrop-Dev>
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

namespace eval ::tcldrop::irc {
	variable version {0.6}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable name {irc}
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {server channels core::users core}
	variable author {Tcldrop-Dev}
	variable description {Provides all IRC related commands.}
	variable rcsid {$Id$}
	variable commands [list resetchan onchan dumpfile botonchan nick2hand hand2nick handonchan getchanhost getchanjoin onchansplit chanlist getchanidle getchanmode pushmode flushmode topic ischanjuped botisop botishalfop botisvoice isop ishalfop wasop washalfop isvoice ischanban ischanexempt ischaninvite chanbans chanexempts chaninvites resetbans resetexempts resetinvites callmsgm callpubm callmsg callpub callmode callneed callflud callsign calljoin callpart callsplt callrejn calltopc callnick callkick callnotc +enforcebans callne callneop callnein callneky callnelm callbeub]
	# Pre-depends on these modules:
	checkmodule server
	checkmodule channels
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
}


# Currently supported RAW's:
# MODE
# 311
# JOIN
# 367
# 324
# 331
# 332
# 333
# TOPIC
# NICK
# QUIT
# KICK
# 433
# 352
# 471
# 473
# 474
# 475
# PART
# NOTICE
# PRIVMSG

# Less important RAW's to add:
# irc.nac.net: 319 FireEgl Harley @#pgpnet @#betas @#aeternamtech
# irc.nac.net: 312 FireEgl Harley irc.nac.net Winning popularity contests since 2003
# irc.nac.net: 317 FireEgl Harley 4 1069595760 seconds idle, signon time
# irc.inter.net.il: 353 FireEgl = #Tcldrop @Singles @SafeTcl +horcy +vipy @Slug @FireEgl @I-C-U-P @llength @lindex @lset @Mel0dy @lreplace @Flounder @Mmmmmmmmm @Atlantica @lsearch
# irc.inter.net.il: 353 FireEgl @ #pgpnet @Jax @Parker @Delta` @Dispatch @Lilo @Whore @DJ @Harley @Robin @Skyway @Slut` @Batman FireEgl @pgpbot @Deryl @ardya @pgpkeys
# irc.inter.net.il: 329 FireEgl #pgpnet 1064713818
# irc.inter.net.il: 401 FireEgl Deryl_ No such nick/channel
# irc.choopa.net: 402 FireEgl papillon No such server
# "juped":
# irc.easynews.com 437 Atlantica #SafeTcl :Nick/channel is temporarily unavailable

proc ischanjuped {channel} {
	# FixMe: Complete this.
	return 0
}

proc ::tcldrop::irc::resetchan {channel} {
	putquick "MODE $channel +b"
	putquick "MODE $channel"
	putquick "WHO $channel"
	# I think all servers send the topic by default on join, so explicitly asking for it is probably unnecessary...
	puthelp "TOPIC $channel"
}

# This calls all of the msgm binds:
proc ::tcldrop::irc::callmsgm {nick uhost handle text} {
	foreach {type flags mask proc} [bindlist msgm] {
		if {[string match -nocase $mask $text] && [matchattr $handle $flags]} {
			countbind $type $mask $proc
			if {[catch { $proc $nick $uhost $handle $text } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
		}
	}
}

proc ::tcldrop::irc::callpubm {nick uhost handle channel text} {
	foreach {type flags mask proc} [bindlist pubm] {
		if {[string match -nocase $mask "$channel $text"] && [matchattr $handle $flags $channel]} {
			countbind $type $mask $proc
			if {[catch { $proc $nick $uhost $handle $channel $text } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
		}
	}
}

proc ::tcldrop::irc::callmsg {nick uhost handle command text} {
	set retval 0
	foreach {type flags mask proc} [bindlist msg] {
		if {[string equal -nocase $mask $command] && [matchattr $handle $flags]} {
			countbind $type $mask $proc
			if {[catch { $proc $nick $uhost $handle $text } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			} else {
				if {[string equal {1} $err]} {
					putcmdlog "(${nick}!${uhost}) !$handle! [string toupper $command]"
				}
				set retval 1
			}
		}
	}
	set retval
}

proc ::tcldrop::irc::callpub {nick uhost handle channel command text} {
	set retval 0
	foreach {type flags mask proc} [bindlist pub] {
		if {[string equal -nocase $mask $command] && [matchattr $handle $flags $channel]} {
			countbind $type $mask $proc
			if {[catch { $proc $nick $uhost $handle $channel $text } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			} else {
				if {[string equal {1} $err]} {
					putcmdlog "(${nick}!${uhost}) !$handle! [string toupper $command]" $channel
				}
				set retval 1
			}
		}
	}
	set retval
}

proc ::tcldrop::irc::callpart {nick uhost handle channel {msg {}}} {
	foreach {type flags mask proc} [bindlist part] {
		if {[string match -nocase $mask "$channel $uhost"] && [matchattr $handle $flags $channel]} {
			if {[catch { $proc $nick $uhost $handle $channel $msg } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
	array unset ::channelnicks [string tolower "$channel,$nick"]
	if {![llength [array names ::channelnicks *,[string tolower $nick]]]} {
		array unset ::nicks [string tolower $nick]
	}
}

proc ::tcldrop::irc::callkick {nick uhost handle channel target {reason {}}} {
	foreach {type flags mask proc} [bindlist kick] {
		if {[string match -nocase $mask "$channel $target"]} {
			if {[catch { $proc $nick $uhost $handle $channel $target $reason } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
	array unset ::channelnicks [string tolower "$channel,$nick"]
	if {![llength [array names ::channelnicks *,[string tolower $nick]]]} {
		array unset ::nicks [string tolower $target]
	}
}

proc ::tcldrop::irc::callsign {nick uhost {handle {*}} {chanmask {*}} {msg {Quit}}} {
	# Call all the sign binds:
	foreach {type flags mask proc} [bindlist sign] {
		foreach channel [channels $chanmask] {
			if {[string match -nocase $mask "$channel $nick!$uhost"] && [matchattr $handle $flags $channel]} {
				if {[catch { $proc $nick $uhost $handle $channel $msg } err]} {
					putlog "Error in $proc: $err"
					puterrlog "$::errorInfo"
				}
				countbind $type $mask $proc
			}
		}
	}
	# Forget everything we know about $nick:
	array unset ::nicks [set element [string tolower $nick]]
	array unset ::channelnicks *,$element
}

proc ::tcldrop::irc::callsplt {nick uhost {handle {*}} {chanmask {*}} {msg {net-split}}} {
	global channelnicks
	# Call all the splt binds:
	if {[validchan $chanmask]} { set channels [list $chanmask] } else { set channels [channels] }
	foreach {type flags mask proc} [bindlist splt] {
		foreach channel $channels {
			if {[onchan $nick $channel]} {
				array set channickinfo $channelnicks([set element [string tolower $channel,$nick]])
				array set channickinfo [list split [clock seconds] split-timer [after [expr { ${::wait-split} * 1001 }] [list ::tcldrop::irc::callsign $nick $uhost $handle $channel $msg]]]
				set channelnicks($element) [array get channickinfo]
				if {[string match -nocase $mask "$channel $nick!$uhost"] && [matchattr $handle $flags $channel]} {
					if {[catch { $proc $nick $uhost $handle $channel } err]} {
						putlog "Error in $proc: $err"
						puterrlog "$::errorInfo"
					}
					countbind $type $mask $proc
				}
				unset channickinfo
			}
		}
	}
}

proc ::tcldrop::irc::callrejn {nick uhost handle channel} {
	global channelnicks
	# Call all of the rejn binds:
	foreach {type flags mask proc} [bindlist rejn] {
		if {[string match -nocase $mask "$channel $nick!$uhost"] && [matchattr $handle $flags $channel]} {
			array set channickinfo $channelnicks([set element [string tolower $channel,$nick]])
			array set channickinfo [list split 0 nick $nick op 0 voice 0 halfop 0 wasop 0 washalfop 0 wasvoice 0]
			if {[info exists channickinfo(split-timer)]} {
				after cancel $channickinfo(split-timer)
				unset channickinfo(split-timer)
			}
			set channelnicks($element) [array get channickinfo]
			if {[catch { $proc $nick $uhost $handle $channel } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

proc ::tcldrop::irc::calljoin {nick uhost handle channel} {
	channickinfo $channel $nick idletime [clock seconds] jointime [clock seconds] nick $nick uhost $uhost handle $handle channel $channel op 0 voice 0 halfop 0 wasop 0 washalfop 0 wasvoice 0 split 0
	#set ::channelnicks([string tolower $channel,$nick]) [list nick $nick uhost $uhost handle $handle channel $channel op 0 voice 0 halfop 0 wasop 0 washalfop 0 wasvoice 0 split 0]
	# Call all of the join binds:
	foreach {type flags mask proc} [bindlist join] {
		if {[string match -nocase $mask "$channel $nick!$uhost"] && [matchattr $handle $flags $channel]} {
			if {[catch { $proc $nick $uhost $handle $channel } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

proc ::tcldrop::irc::callmode {nick uhost handle channel mode {victim {}}} {
	foreach {type flags mask proc} [bindlist mode] {
		if {[string match -nocase $mask "$channel $mode"]} {
			if {[catch { $proc $nick $uhost $handle $channel $mode $victim } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

proc ::tcldrop::irc::calltopc {nick uhost handle channel topic} {
	set chaninfo [dict create topic $topic topic-creator $nick!$uhost topic-created [clock seconds]]
	if {[info exists ::channels([set element [string tolower $channel]])]} {
		set ::channels($element) [dict merge $::channels($element) $chaninfo]
	} else {
		set ::channels($element) $chaninfo
	}
	foreach {type flags mask proc} [bindlist topc] {
		if {[string match -nocase $mask "$channel $topic"]} {
			if {[catch { $proc $nick $uhost $handle $channel $topic } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

proc ::tcldrop::irc::callnick {oldnick uhost handle channel nick} {
	foreach {type flags mask proc} [bindlist nick] {
		foreach channel [channels] {
			if {[string match -nocase $mask "$channel $nick"] && [botonchan $channel] && ([onchan $oldnick $channel] || [onchan $nick $channel]) && [matchattr $handle $flags $channel]} {
				countbind $type $mask $proc
				if {[catch { $proc $oldnick $uhost $handle $channel $nick} err]} {
					putlog "Error in $proc: $err"
					puterrlog "$::errorInfo"
				}
			}
		}
	}
}

# Note: Eggdrop supports the following need types: op, unban, invite, limit, and key.
proc ::tcldrop::irc::callneed {channel {need {join}}} {
	# Do the Eggdrop NEED binds:
	foreach {type flags mask proc} [bindlist need] {
		if {[string match -nocase $mask "$channel $need"]} {
			if {[catch { $proc $channel $need } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
	# Do the old-style Eggdrop needs:
	if {[set script [channel get $channel "need-$need"]] != {}} {
		# FixMe: This may require an uplevel #0 to eval $script:
		if {[catch { uplevel #0 $script } err]} {
			putlog "Error in $script: $err"
			puterrlog "$::errorInfo"
		}
	}
	# Finally, do the RacBot-style needs:
	callne [string map {invite in key ky limit lm unban ub} $need] $channel $::botnick
}

# The following are RacBot style need binds:
# Taken from: http://www.racbot.org/docs/tclbinds/irc_channel_event_bindings.html
proc ::tcldrop::irc::callne {type channel {botnick {}}} {
	if {$botnick == {}} { set botnick $::botnick }
	foreach {type flags mask proc} [bindlist "ne$type"] {
		if {[string match -nocase $mask $channel]} {
			if {[catch { $proc $channel $botnick } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

# + NEED CHANNEL OPERATOR STATUS
# Module Required: racirc
# Stackable: Yes
# Usage: "bind neop - <channel> <procedure-name>"
# Variables: $channel $botnick
# Description:
# This binding can be used to perform an action to get Channel
# Operator Status on <channel>. E.g. Login into a ChanServ Bot.
proc ::tcldrop::irc::callneop {channel {botnick {}}} { callne op $channel $botnick }

# + NEED CHANNEL INVITE
# Module Required: racirc
# Stackable: Yes
# Usage: "bind nein - <channel> <procedure-name>"
# Variables: $channel $botnick
# Description:
# This binding can be used to perform an action to get into
# an Invite Only channel called <channel>. E.g. Login into a
# ChanServ Bot.
proc ::tcldrop::irc::callnein {channel {botnick {}}} { callne in $channel $botnick }

# + NEED CHANNEL KEY
# Module Required: racirc
# Stackable: Yes
# Usage: "bind neky - <channel> <procedure-name>"
# Variables: $channel $botnick
# Description:
# This binding can be used to perform an action to get into
# an channel called <channel> that requires a channel key. E.g.
# Login into a ChanServ Bot.
proc ::tcldrop::irc::callneky {channel {botnick {}}} { callne ky $channel $botnick }

# + NEED CHANNEL LIMIT
# Module Required: racirc
# Stackable: Yes
# Usage: "bind nelm - <channel> <procedure-name>"
# Variables: $channel $botnick
# Description:
# This binding can be used to perform an action to get into
# an channel called <channel> that has reached its limit. E.g.
# Login into a ChanServ Bot.
proc ::tcldrop::irc::callnelm {channel {botnick {}}} { callne lm $channel $botnick }

# + NEED CHANNEL UNBAN
# Module Required: racirc
# Stackable: Yes
# Usage: "bind neub - <channel> <procedure-name>"
# Variables: $channel $botnick
# Description:
# This binding can be used to perform an action to get into
# an channel called <channel> that banned the Bot's hostmask. E.g.
# Login into a ChanServ Bot.
proc ::tcldrop::irc::callneub {channel {botnick {}}} { callne ub $channel $botnick }


# Other RacBot binds:
# USER LOGIN
# Module Required: racirc
# Stackable: Yes
# Usage: "bind lgin <flags> <mask> <procedure-name>"
# Variables: $nick $userhost $handle $channel
# Description:
# This binding is called when a user logs into a channel. <Mask> is
# the channel name. Wildcards are allowed. Set the <mask> to "*" if
# you want all channel logins.

# USER LOGOUT
# Module Required: racirc
# Stackable: Yes
# Usage: "bind lgot <flags> <mask> <procedure-name>"
# Variables: $nick $userhost $handle $channel
# Description:
# This binding is called when a user logs out of a channel. <Mask> is
# the channel name. Wildcards are allowed. Set the <mask> to "*" if
# you want all channel logouts.


proc ::tcldrop::irc::callnotc {nick uhost handle text dest} {
	foreach {type flags mask proc} [bindlist notc] {
		if {[string match -nocase $mask $text] && [matchattr $handle $flags]} {
			if {[catch { $proc $nick $uhost $handle $text $dest } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

#    (30) FLUD (stackable)
#         bind flud <flags> <type> <proc>
#         procname <nick> <user@host> <handle> <type> <channel>
#
#         Description: any floods detected through the flood control settings
#           (like 'flood-ctcp') are sent here before processing. If the proc
#           returns 1, no further action is taken on the flood; if the proc
#           returns 0, the bot will do its normal "punishment" for the flood.
#           The flood types are: pub, msg, join, or ctcp (and can be masked to
#           "*" for the bind); flags are ignored.
#         Module: server
proc ::tcldrop::irc::callflud {nick uhost handle type {channel {}}} {
	foreach {type flags mask proc} [bindlist flud] {
		if {[string match -nocase $mask $type]} {
			countbind $type $mask $proc
			if {[catch { $proc $nick $uhost $handle $type $channel } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			} elseif {[string equal {1} $err]} {
				return 1
			}
		}
	}
	return 0
}

# FixMe: These flood related binds should be RAW binds, if $allow-dk-cmds and $trigger-on-ignore settings are to be supported.
bind kick - * ::tcldrop::irc::FLUD_kick -priority 1000
proc ::tcldrop::irc::FLUD_kick {nick uhost handle channel target reason} { if {[detectflood [channel get $channel flood-kick] kick $channel $uhost ]} { callflud $nick $uhost $handle kick $channel } }

bind mode - {* -o} ::tcldrop::irc::FLUD_deop -priority 1000
proc ::tcldrop::irc::FLUD_deop {nick uhost handle channel mode victim} { if {[detectflood [channel get $channel flood-deop] deop $channel $uhost]} { callflud $nick $uhost $handle deop $channel } }

bind join - * ::tcldrop::irc::FLUD_join -priority 1000
proc ::tcldrop::irc::FLUD_join {nick uhost handle channel} { if {[detectflood [channel get $channel flood-join] join $channel $uhost]} { callflud $nick $uhost $handle join $channel } }

bind pubm - * ::tcldrop::irc::FLUD_pub -priority 1000
proc ::tcldrop::irc::FLUD_pub {nick uhost handle channel text} { if {[detectflood [channel get $channel flood-chan] chan $channel $uhost]} { callflud $nick $uhost $handle pub $channel } }

bind msgm - * ::tcldrop::irc::FLUD_msg -priority 1000
# FixMe: Should the channel be "" for msg floods or something else?
proc ::tcldrop::irc::FLUD_msg {nick uhost handle text} { if {[detectflood ${::flood-msg} msg $uhost]} { callflud $nick $uhost $handle msg "" } }

bind ctcp - * ::tcldrop::irc::FLUD_ctcp -priority 1000
proc ::tcldrop::irc::FLUD_ctcp {nick uhost handle dest key text} {
	if {[isbotnick $dest] || $dest eq {}} {
		# FixMe: Should the channel be "" for msg floods or something else?
		if {[detectflood ${::flood-ctcp} ctcp $uhost]} {
			callflud $nick $uhost $handle ctcp ""
			return 1
		}
	} elseif {[validchan $dest] && [detectflood [channel get $dest flood-ctcp] ctcp $dest $uhost]} {
		callflud $nick $uhost $handle ctcp $dest
		return 1
	}
	return 0
}

# irc.choopa.net: 471 Fire_Egl #Tcldrop Cannot join channel (+l)
bind raw - 471 ::tcldrop::irc::471 -priority 100
proc ::tcldrop::irc::471 {from key arg} {
	if {[validchan [set chan [lindex [split $arg] 1]]]} {
		callneed $chan limit
	}
}

# irc.choopa.net: 473 Fire_Egl #Tcldrop Cannot join channel (+i)
bind raw - 473 ::tcldrop::irc::473 -priority 100
proc ::tcldrop::irc::473 {from key arg} {
	if {[validchan [set chan [lindex [split $arg] 1]]]} {
		callneed $chan invite
	}
}

# irc.choopa.net: 474 Fire_Egl #Tcldrop Cannot join channel (+b)
bind raw - 474 ::tcldrop::irc::474 -priority 100
proc ::tcldrop::irc::474 {from key arg} {
	if {[validchan [set chan [lindex [split $arg] 1]]]} {
		callneed $chan unban
	}
}

# irc.inter.net.il: 475 FireEgl #pgpnet Cannot join channel (+k)
bind raw - 475 ::tcldrop::irc::475 -priority 100
proc ::tcldrop::irc::475 {from key arg} { callneed [lindex [split $arg] 1] key }

bind kick - {* *} ::tcldrop::irc::KICK_rejoin -priority 1000
proc ::tcldrop::irc::KICK_rejoin {nick uhost handle channel target reason} { JoinOrPart }

proc ::tcldrop::irc::nickinfo {nick args} {
	if {[info exists ::nicks([set nick [string tolower $nick]])]} {
		set ::nicks($nick) [dict merge $::nicks($nick) $args]
	} else {
		set ::nicks($nick) [dict create {*}$args]
	}
}

proc ::tcldrop::irc::getnickinfo {nick {info {}}} {
	if {[info exists ::nicks([set nick [string tolower $nick]])]} {
		if {$info == {}} {
			return $::nicks($nick)
		} elseif {[dict exists $::nicks($nick) $info]} {
			dict get $::nicks($nick) $info
		}
	}
}

proc ::tcldrop::irc::channickinfo {channel nick args} {
	if {[info exists ::channelnicks([set element [string tolower "$channel,$nick"]])]} {
		set ::channelnicks($element) [dict merge $::channelnicks($element) $args]
	} else {
		set ::channelnicks($element) [dict create {*}$args]
	}
}

proc ::tcldrop::irc::chaninfo {channel args} {
	if {[info exists ::channels([set channel [string tolower $channel]])]} {
		set ::channels($channel) [dict merge $::channels($channel) $args]
	} else {
		set ::channels($channel) [dict create {*}$args]
	}
}


# FixMe: Broken and Unused anywhere else.
#proc ::tcldrop::irc::setchandata {chan {info {}}} {
#	if {[info exists ::channels([set element [string tolower $nick]])]} { array set nickinfo $::nicks($element) }
#	array set nickinfo $info
#	set ::nicks($element) [array get nickinfo]
#}

#proc ::tcldrop::irc::getchandata {nick {info {}}} {
#	if {[info exists ::nicks([set nick [string tolower $nick]])]} {
#		if {$info == {}} {
#			return $::nicks($nick)
#		} else {
#			array set nickinfo $::nicks($nick)
#			if {[info exists nickinfo($info)]} { return $nickinfo($info) }
#		}
#	}
#}


# FixMe: Add support for server modes.
# FixMe: Add support for personal modes. (maybe)
bind raw - MODE ::tcldrop::irc::MODE -priority 100
proc ::tcldrop::irc::MODE {from key arg} {
	set nick [lindex [split $from !] 0]
	set uhost [lindex [split $from !] end]
	set handle [finduser $from]
	set channel [lindex [set arg [split $arg]] 0]
	set modes [string trimleft [lindex $arg 1] :]
	set victims [lrange $arg 2 end]
	putloglev k $channel "${channel}: mode change '$modes ${victims}' by $nick!$uhost"
	# First we process all the modes..
	# For example, this takes a mode like:
	# "+o-o FireEgl FireEgl"
	# and appends it to the $splitmodes list as:
	# {-o FireEgl}
	# (Note the missing +o FireEgl ..since it doesn't actually do anything in the real world it got removed.)
	set v -1
	lappend splitmodes
	foreach m [split $modes {}] {
		switch -- $m {
			{+} - {-} { set plusminus $m }
			{} {}
			{n} - {t} - {i} - {s} - {p} - {m} - {c} - {F} - {f} - {L} - {P} {
				# FixMe: Need isupport (raw 005) so we can detect what modes these are.  (see server module)
				# These are modes that don't have a "victim".
				# Note: +ntispm should be standard on any IRCD.
				#       +FfLPq are some that I noticed on the FreeNode.Net network.
				#       If other networks conflict with these we'll have to redo some stuff here.
				lappend splitmodes $plusminus$m
			}
			{default} {
				# This searches $splitmodes to see if there's already a similar mode already saved:
				if {[set pos [lsearch $splitmodes ?[set mode "$m [lindex $victims [incr v]]"]]] != -1} {
					# A similar mode was found, replace it:
					set splitmodes [lreplace $splitmodes $pos $pos $plusminus$mode]
				} else {
					# No similar mode was found, append to the list:
					lappend splitmodes $plusminus$mode
				}
			}
		}
	}
	foreach m $splitmodes {
		set mode [string range $m 0 1]
		set victim [string range $m 3 end]
		# This switch discards all the modes that don't make any changes on $channel:
		# (Such as a +o on somebody that already had ops)
		switch -- $mode {
			{+o} { if {[isop $victim $channel]} { continue } }
			{-o} { if {![isop $victim $channel]} { continue } }
			{+v} { if {[isvoice $victim $channel]} { continue } }
			{-v} { if {![isvoice $victim $channel]} { continue } }
			{+h} { if {[ishalfop $victim $channel]} { continue } }
			{-h} { if {![ishalfop $victim $channel]} { continue } }
			{default} {
				# FixMe: Check the other modes too.
			}
		}
		# If a "continue" wasn't triggered above,
		# we call all of the mode binds with that mode:
		callmode $nick $uhost $handle $channel $mode $victim
	}
}

# Call this before we call any other mode binds.
bind mode - "* ?o" ::tcldrop::irc::mode -priority -1000
bind mode - "* ?v" ::tcldrop::irc::mode -priority -1000
bind mode - "* ?h" ::tcldrop::irc::mode -priority -1000
# And again after other mode binds so that isop and wasop will be the same.
bind mode - "* ?o" ::tcldrop::irc::mode -priority 1000
bind mode - "* ?v" ::tcldrop::irc::mode -priority 1000
bind mode - "* ?h" ::tcldrop::irc::mode -priority 1000
proc ::tcldrop::irc::mode {nick uhost handle channel mode {victim {}}} {
	if {[info exists ::channelnicks([set element [string tolower "$channel,$victim"]])]} {
		switch -- $mode {
			{+o} {
				dict set ::channelnicks($element) wasop [dict get $::channelnicks($element) op]
				dict set ::channelnicks($element) op 1
			}
			{-o} {
				dict set ::channelnicks($element) wasop [dict get $::channelnicks($element) op]
				dict set ::channelnicks($element) op 0
			}
			{+v} {
				dict set ::channelnicks($element) wasvoice [dict get $::channelnicks($element) voice]
				dict set ::channelnicks($element) voice 1
			}
			{-v} {
				dict set ::channelnicks($element) wasvoice [dict get $::channelnicks($element) voice]
				dict set ::channelnicks($element) voice 0

			}
			{+h} {
				dict set ::channelnicks($element) washalfop [dict get $::channelnicks($element) halfop]
				dict set ::channelnicks($element) halfop 1
			}
			{-h} {
				dict set ::channelnicks($element) washalfop [dict get $::channelnicks($element) halfop]
				dict set ::channelnicks($element) halfop 0
			}
		}
	}
}

# irc.choopa.net: 311 FireEgl FireEgl ~FireEgl adsl-17-134-83.bhm.bellsouth.net * Proteus
# Process results from a WHOIS (this is not something the bot does automatically):
bind raw - 311 ::tcldrop::irc::311 -priority 1000
proc ::tcldrop::irc::311 {from key arg} {
	set larg [split $arg]
	set dest [lindex $larg 0]
	set nick [lindex $larg 1]
	set ident [lindex $larg 2]
	set address [lindex $larg 3]
	if {[string equal $dest $nick]} {
		# We WHOIS'd ourself.. (probably on connect to IRC)
		# So we should update our botname variable:
		set ::botname "$nick!$ident@$address"
	}
	if {[onchan $nick]} {
		nickinfo $nick nick $nick uhost $ident@$address ident $ident address $address realname [string range [join [lrange $larg 5 end]] 1 end] unknown [lindex $larg 4]
	}
}

# irc.choopa.net: 317 FireEgl FireEgl 193 1050624422 seconds idle, signon time
# Process results from a WHOIS:
# Proc by Papillon@EFNet
# FixMe: Untested and unmodified.
# bind raw - 317 ::tcldrop::irc::317 -priority 100
#proc ::tcldrop::irc::317 {from key arg} {
#	# FixMe: This should set the initial idle (lastspoke) time, and join time.
#	set larg [split $arg]
#	set nick [lindex $larg 1]
#	set idle [lindex $larg 2]
#	set jointime [lindex $larg 3]
#	set element [string tolower $nick]
#	if {[info exists ::nicks($element)]} { array set nickinfo $::nicks($element) }
#	array set nickinfo [list idle $idle logon $jointime]
#	set ::nicks($element) [array get nickinfo]
#}

bind evnt - init-server ::tcldrop::irc::Init-Server -priority 100
proc ::tcldrop::irc::Init-Server {type} {
	# Find out our own hostname and whatnot:
	putquick "WHOIS $::botnick"
	# Start trying to join the channels:
	JoinOrPart
}

bind raw - JOIN ::tcldrop::irc::JOIN -priority 1000
proc ::tcldrop::irc::JOIN {from key arg} {
	set arg [string trim $arg]
	if {[validchan [set channel [string range $arg 1 end]]]} {
		# If the bot itself just joined the channel, do a resetchan:
		if {[string equal $from $::botname]} { resetchan $channel }
		set uhost [lindex [split $from !] end]
		if {[onchansplit [set nick [lindex [split $from !] 0]] $channel] && [string equal -nocase [getchanhost $nick $channel] $uhost]} {
			callrejn $nick $uhost [finduser $from] $channel
		} else {
			nickinfo $nick nick $nick uhost $uhost ident [lindex [split $uhost @] 0] address [lindex [split $uhost @] end]
			calljoin $nick $uhost [finduser $from] $channel
		}
		putloglev j $channel "$nick (${uhost}) joined ${channel}."
	} else {
		#puthelp "PART $channel :Why did I join here?"
	}
}

# irc.blessed.net 367 TiCkLe #tclsh host*!*@jaslkgsdg CB-4!surf@80.199.114.234 1053610707
# Process the results of MODE $channel +b:
bind raw - 367 ::tcldrop::irc::367 -priority 1000
proc ::tcldrop::irc::367 {from key arg} {
	set ::bans([string tolower [set channel [lindex [set larg [split $arg]] 1]],[set ban [lindex $larg 2]]]) [list channel $channel ban $ban creator [lindex $larg 3] created [lindex $larg 4]]
}

# irc.wh.verio.net: 368 FireEgl #debian End of Channel Ban List
# FixMe: 368 should trigger the removal of all bans that shouldn't be active on the channel.
bind raw - 368 ::tcldrop::irc::368 -priority 1000
proc ::tcldrop::irc::368 {from key arg} {
	expirebans [lindex [split $arg] 1]
}

# irc.choopa.net: 324 FireEgl #channel +tn
# Process the results from MODE $channel:
bind raw - 324 ::tcldrop::irc::324 -priority 1000
proc ::tcldrop::irc::324 {from key arg} {
	dict set ::channels([string tolower [lindex [set larg [split $arg]] 1]]) chanmodes [join [lrange $larg 2 end]]
}

# irc.choopa.net: 329 FireEgl #channel 1050676546
# Process the results from MODE $channel:
# Proc by Papillon@EFNet
# bind raw - 329 ::tcldrop::irc::329 -priority 100
#proc ::tcldrop::irc::329 {from key arg} {
#	set larg [split $arg]
#	set channel [lindex $larg 1]
#	# This is the unixtime of when the channel was created:
#	set created [lindex $larg end]
#	set element [string tolower $channel]
#	if {![info exists ::channels($element)]} { set ::channels($element) {} }
#	array set chaninfo $::channels($element)
#	array set chaninfo [list created $created]
#	set ::channels($element) [array get chaninfo]
#}

# irc.homelien.no 331 Papillon #channel :No topic is set
# Process the results from TOPIC $channel: if no topic is set
# Proc by Papillon@EFNet
bind raw - 331 ::tcldrop::irc::331 -priority 1000
proc ::tcldrop::irc::331 {from key arg} { 332 $from $key [join [lrange [split $arg] 0 1]] }

# irc.homelien.no 332 Papillon #channel :topic
# Process the results from TOPIC $channel:
bind raw - 332 ::tcldrop::irc::332 -priority 1000
proc ::tcldrop::irc::332 {from key arg} { set larg [split $arg]
	dict set ::channels([string tolower [lindex $larg 1]]) topic [string range [join [lrange $larg 2 end]] 1 end]
	# FixMe: Does Eggdrop call the topc binds when it joins a channel and receives the topic?
}

# irc.choopa.net: 333 FireEgl #tcl FireEgl!Proteus@adsl-17-148-104.bhm.bellsouth.net 1069721590
# Process the results from TOPIC $channel:
# This tells us the creator and created time.
bind raw - 333 ::tcldrop::irc::333 -priority 1000
proc ::tcldrop::irc::333 {from key arg} { set larg [split $arg]
	set channel [string tolower [lindex $larg 1]]
	dict set ::channels($channel) topic-creator [lindex $larg 2]
	dict set ::channels($channel) topic-created [lindex $larg 3]
}

# FireEgl!Proteus@adsl-17-148-104.bhm.bellsouth.net TOPIC #test :blah blah
# Triggered when somebody changes the topic.
bind raw - TOPIC ::tcldrop::irc::TOPIC -priority 1000
proc ::tcldrop::irc::TOPIC {from key arg} {
	# Call all the topc binds:
	calltopc [lindex [split $from !] 0] [lindex [split $from !] end] [finduser $from] [lindex [set larg [split $arg]] 0] [string range [join [lrange $larg 1 end]] 1 end]
}

bind raw - NICK ::tcldrop::irc::NICK -priority 1000
proc ::tcldrop::irc::NICK {from key arg} {
	global nick altnick botnick botname nicks channelnicks
	set oldnick [lindex [split $from !] 0]
	set newnick [string range $arg 1 end]
	if {![string equal -nocase $botnick $nick] && [string equal -nocase $oldnick $nick]} {
		# Try to get our preferred nick back:
		putserv "NICK $nick"
	} elseif {![string equal -nocase $botnick $nick] && ![string equal -nocase $botnick $altnick] && [string equal -nocase $oldnick $altnick]} {
		# Or try to get our altnick:
		putserv "NICK $altnick"
	} elseif {[string equal -nocase $oldnick $botnick]} {
		# Make sure ::botnick and ::botname are up-to-date:
		set botnick $newnick
		set botname "$newnick![lindex [split $from {! }] 1]"
	}

	set loweroldnick [string tolower $oldnick]
	set lowernewnick [string tolower $newnick]
	if {[info exists nicks($loweroldnick)]} {
		set nicks($lowernewnick) $nicks($loweroldnick)
		dict set nicks($lowernewnick) nick $newnick
		unset nicks($loweroldnick)
	}
	# Update the channelnick info with the new nick:
	foreach m [array names ::channelnicks *,$loweroldnick] {
		set lowerchannel [string tolower [dict get $channelnicks($m) channel]]
		set channelnicks($lowerchannel,$lowernewnick) $channelnicks($m)
		dict set channelnicks($lowerchannel,$lowernewnick) nick $newnick
		unset channelnicks($m)
	}

	# Call all the nick binds:
	# FixMe: Is this supposed to use * or should it trigger a nick bind for every channel?
	#        (This should take the Eggdrop behaviour, no matter how stupid it is)
	callnick $oldnick [lindex [split $from !] end] [finduser $from] * $newnick
}

bind nick - * ::tcldrop::irc::FLUD_nick -priority 1000
proc ::tcldrop::irc::FLUD_nick {nick uhost handle channel newnick} { if {[detectflood [channel get $channel flood-nick] nick $channel $uhost]} { callflud $nick $uhost $handle nick $channel } }

bind raw - QUIT ::tcldrop::irc::SIGN -priority 1000
proc ::tcldrop::irc::SIGN {from key arg} {
	if {[string match -nocase {*?.*[a-z][a-z] *?.*[a-z][a-z]} [set msg [string range $arg 1 end]]]} {
		callsplt [lindex [split $from !] 0] [lindex [split $from !] 1] [set handle [finduser $from]] * $msg
		putloglev j * "[lindex [split $from !] 0] ([lindex [split $from !] 1]) got netsplit: $msg"
	} else {
		callsign [lindex [split $from !] 0] [lindex [split $from !] 1] [finduser $from] * $msg
		# FixMe: Make sure this is the same format as Eggdrops:
		putloglev j * "Quit: [lindex [split $from !] 0] [lindex [split $from !] 1] * $msg"
	}
}

bind raw - KICK ::tcldrop::irc::KICK -priority 1000
proc ::tcldrop::irc::KICK {from key arg} {
	# Call all the kick binds:
	lassign [list [lindex [split $from !] 0] [lindex [split $from !] end] [finduser $from] [lindex [set larg [split $arg]] 0] [lindex $larg 1] [string range [join [lrange $larg 2 end]] 1 end]] nick uhost handle channel target reason
	callkick $nick $uhost $handle $channel $target $reason
	putloglev k $channel "$target kicked from $channel by ${nick}: $reason"
}

# irc.choopa.net: 433 FireEgl NewNick Nickname is already in use.
# will choose the alternate nick, if that's taken aswell it will set Lamestbotxx
# where xx is random numbers
bind raw - 433 ::tcldrop::irc::433 -priority 1000
proc ::tcldrop::irc::433 {from key arg} {
	set oldnick [lindex [split $arg] 1]
	if {[string equal $oldnick $::nick]} {
		putserv "NICK $::altnick"
	} elseif {[string equal $oldnick $::altnick]} {
		putserv "NICK $::nick[rand 99]"
	}
}

# irc.choopa.net: 352 FireEgl #channel ~FireEgl adsl-17-134-83.bhm.bellsouth.net irc.choopa.net FireEgl H@ 0 Proteus
# Process the results from WHO $channel:
bind raw - 352 ::tcldrop::irc::352 -priority 1000
# Note: Proc by Papillon@EFNet.
# Note: This command seems to work good now
proc ::tcldrop::irc::352 {from key arg} {
	set larg [split $arg]
	set channel [string tolower [lindex $larg 1]]
	set ident [lindex $larg 2]
	set address [lindex $larg 3]
	#set server [lindex $larg 4]
	set nick [lindex $larg 5]
	set flags [string trimleft [lindex $larg 6] {HG*xXd!}]
	#set hops [string trimleft [lindex $larg 7] :]
	set realname [join [lrange $larg 8 end]]
	set element [string tolower $nick]
	set handle [finduser "$nick!$ident@$address"]
	nickinfo $nick handle $handle nick $nick uhost "$ident@$address" ident $ident address $address realname $realname
	set op [set voice [set halfop 0]]
	foreach f [split $flags {}] {
		switch -- $f {
			{+} { set voice 1 }
			{%} { set halfop 1 }
			{default} { if {[string first $f $::opchars] != -1} { set op 1 } }
		}
	}
	channickinfo $channel $nick handle $handle channel $channel idletime [clock seconds] jointime 0 nick $nick op $op voice $voice halfop $halfop wasop $op washalfop $halfop wasvoice $voice split 0
	return 0
}

# Triggered when somebody leaves a channel:
bind raw - PART ::tcldrop::irc::PART -priority 1000
proc ::tcldrop::irc::PART {from key arg} {
	set larg [split $arg]
	if {[validchan [set channel [lindex $larg 0]]]} {
		set nick [lindex [split $from !] 0]
		set uhost [lindex [split $from !] 1]
		set handle [finduser $from]
		set msg [string range [join [lrange $larg 1 end]] 1 end]
		# Call all the part binds:
		callpart $nick $uhost $handle $channel $msg
		putloglev j $channel "$nick (${uhost}) left ${channel}."
	}
}

# Process the results from NOTICE $channel:
bind raw - NOTICE ::tcldrop::irc::NOTICE -priority 1000
proc ::tcldrop::irc::NOTICE {from key arg} {
	set larg [split $arg]
	set nick [lindex [split $from !] 0]
	set uhost [lindex [split $from !] 1]
	set handle [finduser $from]
	set dest [lindex $larg 0]
	set text [string range [join [lrange $larg 1 end]] 1 end]
	# proc-name <nick> <user@host> <handle> <dest> <keyword> <text>
	if {[string match "\001*\001" $text]} {
		callctcr $nick $uhost $handle $dest [lindex [set text [split [string trim $text "\001"]]] 0] [join [lrange $text 1 end]]
	} else {
		# Call all the notc binds:
		callnotc $nick $uhost $handle $text $dest
		putloglev m - "-$nick (${uhost}- $text"
	}
}

# FixMe: pubm & msgm should be triggered _before_ pub & msg. If a line matches,
# it should only be passed along to pub or msg if exclusive-binds is 1
bind raw - PRIVMSG ::tcldrop::irc::PRIVMSG -priority 1000
proc ::tcldrop::irc::PRIVMSG {from key arg} {
	set nick [lindex [split $from !] 0]
	set uhost [lindex [split $from !] end]
	set handle [finduser $from]
	set larg [split $arg]
	set dest [lindex $larg 0]
	if {[string equal [string index [lindex $larg 1] 1] \001]} {
		# All CTCP binds are called:
		callctcp $nick $uhost $handle $dest [string trimright [string range [lindex $larg 1] 2 end] "\001"] [string trimright [join [lrange $larg 2 end]] "\001"]
		if {[isbotnetnick $dest]} { set dest {-} }
		putloglev m $dest "CTCP [string trimright [string range [lindex $larg 1] 2 end] "\001"] [string trimright [join [lrange $larg 2 end]] "\001"] from $nick (${uhost})"
	} else {
		set text [string range [join [lrange $larg 1 end]] 1 end]
		set ltext [split [string trim $text]]
		set command [lindex $ltext 0]
		set args [string trimleft [join [lrange $ltext 1 end]]]
		if {[isbotnick $dest]} {
			# All MSG binds are called:
			if {![::tcldrop::irc::callmsg $nick $uhost $handle $command $args]} {
				# If callmsg returned 0, do the MSGM binds:
				::tcldrop::irc::callmsgm $nick $uhost $handle $text
				# Only do the msg log if a msg bind wasn't triggered
				putloglev m - "\[$nick!$uhost\] $text"
			}
		} elseif {[validchan $dest]} {
			# All PUB binds are called:
			if {![::tcldrop::irc::callpub $nick $uhost $handle $dest $command $args]} {
				# If callpub returned 0, do the PUBM binds:
				::tcldrop::irc::callpubm $nick $uhost $handle $dest $text
				# Only do the pub log if a pub bind wasn't triggered
				putloglev p $dest "<${nick}> $text"
			}
		}
	}
}

# All this does is clear out all the variables when we disconnect from the server:
bind evnt - disconnect-server ::tcldrop::irc::disconnect-server -priority 1000
proc ::tcldrop::irc::disconnect-server {event} {
	array unset ::channelnicks
	array unset ::channels
	array unset ::nicks
	array unset ::bans
	array unset ::exempts
	array unset ::invites
	# FixMe: It should probably clear the $::botnick and $::botname variables too..
	#        Or should that be done in the server module?
}

# dumpfile <nick> <filename>
#   Description: dumps file from the help/text directory to a user on IRC via
#     msg (one line per msg). The user has no flags, so the flag bindings
#     won't work within the file.
#   Returns: 1 for success, 0 for error.
#   Module: core
proc ::tcldrop::irc::dumpfile {nick filename} {
	if {![catch { open [file join ${::text-path} $filename] r } fid]} {
		# FixMe: Do substitution of %-codes.
		foreach l [split [read -nonewline $fid] \n] {
			puthelp "PRIVMSG $nick :$l"
		}
		return 1
	} else {
		return 0
	}
}

#  ischanban <ban> <channel>
#    Returns: 1 if the specified ban is on the given channel's ban list
#      (not the bot's banlist for the channel)
#    Module: irc
proc ::tcldrop::irc::ischanban {ban channel} {
	info exists ::bans([string tolower "$channel,$ban"])
}

#  ischanexempt <exempt> <channel>
#    Returns: 1 if the specified exempt is on the given channel's exempt
#      list (not the bot's exemptlist for the channel)
#    Module: irc
proc ::tcldrop::irc::ischanexempt {exempt channel} {
	info exists ::exempts([string tolower "$channel,$exempt"])
}

#  ischaninvite <invite> <channel>
#    Returns: 1 if the specified invite is on the given channel's invite
#      list (not the bot's invitelist for the channel)
#    Module: irc
proc ::tcldrop::irc::ischaninvite {invite channel} {
	info exists ::invites([string tolower $channel,$invite])
}

#  chanbans <channel>
#    Returns: a list of the current bans on the channel. Each element is
#      a sublist of the form {<ban> <bywho> <age>}. age is seconds from the
#      bot's POV.
#    Module: irc
proc ::tcldrop::irc::chanbans {channel {banmask {*}}} {
	set banlist [list]
	global bans
	foreach b [array names bans "[string tolower $channel],$banmask"] {
		dict with bans($b) {
			lappend banlist [list $ban $creator $created]
		}
	}
	return $banlist
}

# Similar to chanbans, but this returns all the info on a ban.
proc ::tcldrop::irc::listchanbans {channel {banmask {*}}} {
	set banlist [list]
	global bans
	foreach b [array names bans "[string tolower $channel],$banmask"] { lappend banlist $bans($b) }
	return $banlist
}

#  chanexempts <channel>
#    Returns: a list of the current exempts on the channel. Each element is
#      a sublist of the form {<exempt> <bywho> <age>}. age is seconds from the
#      bot's POV.
#    Module: irc
# FixMe: Add support for the age.
proc ::tcldrop::irc::chanexempts {channel {exemptmask {*}}} {
	set exemptlist [list]
	global exempts
	foreach b [array names exempts "[string tolower $channel],$exemptmask"] {
		dict with exempts($b) {
			lappend exemptlist [list $exempt $creator $created]
		}
	}
	return $exemptlist
}

#  chaninvites <channel>
#    Returns: a list of the current invites on the channel. Each element is
#      a sublist of the form {<invite> <bywho> <age>}. age is seconds from the
#      bot's POV.
#    Module: irc
proc ::tcldrop::irc::chaninvites {channel {invitemask {*}}} {
	set invitelist [list]
	global invites
	foreach b [array names invites "[string tolower $channel],$invitemask"] {
		dict with invites($b) {
			lappend invitelist [list $invite $creator $created]
		}
	}
	return $invitelist
}

# FixMe: This should expire any bans that should no longer be active on the channel:
proc ::tcldrop::irc::expirebans {{channel {*}}} {
}

#  resetbans <channel>
#    Description: removes all bans on the channel that aren't in the bot's
#      ban list and refreshes any bans that should be on the channel but
#      aren't
#    Returns: nothing
#    Module: irc
# FixMe: Complete this.
proc ::tcldrop::irc::resetbans {channel} {
	if {[validchan $channel]} {
		# First remove any active bans that shouldn't be active:
		set ban-time [channel get $channel ban-time]
		global bans
		foreach b [array names bans [string tolower $channel],*] {
			array set baninfo $bans($b)
			if {[isban $baninfo(ban) $channel]} {
				# See if the ban-time has expired..
				if {[expr { ([clock seconds] - $baninfo(created) / 60) >= ${ban-time} }]} {
					# Expired ban-time, remove it.
					pushmode $channel -b $baninfo(ban)
				}
			} else {
				# It wasn't set by the bot, remove it.
				pushmode $channel -b $baninfo(ban)
			}
		}
		# Next set any global or channel bans that are sticky:
		foreach b [concat [listbans $channel] [listbans]] {
			array set baninfo $b
			if {$baninfo(sticky)} { pushmode $channel +b $baninfo(mask) }
		}
		# FixMe: Set any bans that match somebody currently on the channel.
	}
	# Note: Eggdrop gives a Tcl error if it's not a valid channel.
}

#  resetexempts <channel>
#    Description: removes all exempt on the channel that aren't in the bot's
#      exempt list and refreshes any exempts that should be on the channel
#      but aren't
#    Returns: nothing
#    Module: irc
# FixMe: Complete this.
proc ::tcldrop::irc::resetexempts {channel} {
	global exempts
}

#  resetinvites <channel>
#    Description: removes all invites on the channel that aren't in the bot's
#      invite list and refreshes any invites that should be on the channel
#      but aren't
#    Returns: nothing
#    Module: irc
# FixMe: Complete this.
proc ::tcldrop::irc::resetinvites {channel} {
	global invites
}

#  onchan <nickname> [channel]
#    Returns: 1 if someone by that nickname is on the specified channel (or
#      any channel if none is specified); 0 otherwise
proc ::tcldrop::irc::onchan {nick {channel {*}}} {
	if {[llength [array names ::channelnicks [string tolower "$channel,$nick"]]]} {
		return 1
	} else {
		return 0
	}
}

#  botonchan [channel]
#   Returns: 1 if the bot is on the specified channel (or any channel if
#     no channel is specified); 0 otherwise
proc ::tcldrop::irc::botonchan {{channel {*}}} { onchan $::botnick $channel }

#  nick2hand <nickname> [channel]
#    Returns: the handle of a nickname on a channel. If a channel is not
#      specified, the bot will check all of its channels. If the nick is
#      not found, "" is returned. If the nick is found but does not have
#      a handle, "*" is returned.
# Note: Like Eggdrop, Tcldrop ignores $channel.
proc ::tcldrop::irc::nick2hand {nick {channel {*}}} {
	if {[info exists ::nicks([set element [string tolower $nick]])]} {
		array set nickinfo $::nicks($element)
		if {[info exists nickinfo(handle)]} {
			set nickinfo(handle)
		} else {
			finduser $nickinfo(nick)!$nickinfo(uhost)
		}
	}
}

#  hand2nick <handle> [channel]
#    Returns: nickname of the first person on the specified channel (if one
#      is specified) whose nick!user@host matches the given handle; "" is
#      returned if no match is found. If no channel is specified, all channels
#      are checked.
# Note: Eggdrop ignores $channel, and so do we.
proc ::tcldrop::irc::hand2nick {handle {channel {*}}} {
	global nicks
	foreach n [array names nicks] {
		array set nickinfo $nicks($n)
		if {[info exists nickinfo(handle)] && [string equal -nocase $handle $nickinfo(handle)]} {
			return $nickinfo(nick)
		} elseif {[string equal -nocase $handle [finduser $nickinfo(nick)!$nickinfo(uhost)]]} {
			return $nickinfo(nick)
		}
	}
}

#  handonchan <handle> [channel]
#    Returns: 1 if the the nick!user@host for someone on the channel (or any
#      channel if no channel name is specified) matches for the handle given;
#      0 otherwise
proc ::tcldrop::irc::handonchan {handle {channel {*}}} {
	if {[set nick [hand2nick $handle]] != {}} {
		if {[array names ::channelnicks [string tolower $channel,$nick]] != {}} {
			return 1
		} else {
			return 0
		}
	} else {
		return 0
	}
}

#  getchanhost <nickname> [channel]
#    Returns: user@host of the specified nickname (the nickname is not included
#      in the returned host).  Or "" if none found.
# Note: Eggdrop ignores $channel, and so do we.
proc ::tcldrop::irc::getchanhost {nick {channel {*}}} {
	if {[info exists ::nicks([set nick [string tolower $nick]])]} {
		dict get $::nicks($nick) uhost
	}
}

#  getchanjoin <nickname> <channel>
#    Returns: timestamp (unixtime format) of when the specified nickname
#      joined the channel; 0 if the specified user isn't on the channel
proc ::tcldrop::irc::getchanjoin {nick channel} {
	if {[info exists ::channelnicks([set element [string tolower $channel,$nick]])]} {
		dict get $::channelnicks($element) jointime
	} else {
		return 0
	}
}

#  chanlist <channel> [flags[&chanflags]]
#    Description: flags are any global flags; the '&' denotes to look for
#      channel specific flags. Examples:
#        n         (Global Owner)
#        &n        (Channel Owner)
#        o&m       (Global Op, Channel Master)
#      Now you can use even more complex matching of flags, including +&- flags
#      and & or | (and or or) matching.
#    Returns: list of nicknames currently on the bot's channel that have all
#      of the flags specified;. If no flags are given, all of the nicknames
#      are returned. Please note that if you're executing chanlist after a
#      part or sign bind, the gone user will still be listed, so you can
#      check for wasop, isop, etc.
proc ::tcldrop::irc::chanlist {channel {flags {*}}} {
	set chanlist [list]
	global channelnicks
	foreach n [array names channelnicks [string tolower "$channel,*"]] {
		array set channickinfo $channelnicks($n)
		if {$flags == {*}} {
			lappend chanlist $channickinfo(nick)
		} elseif {[matchattr $channickinfo(handle) $flags $channel]} {
			lappend chanlist $channickinfo(nick)
		}
	}
	return $chanlist
}

#  getchanidle <nickname> <channel>
#    Returns: number of minutes that person has been idle; 0 if the specified user isn't on the channel.
proc ::tcldrop::irc::getchanidle {nick channel} {
	if {[info exists ::channelnicks([set element [string tolower $channel,$nick]])]} {
		dict get $::channelnicks($element) idletime
	} else {
		return 0
	}
}

#  getchanmode <channel>
#    Returns: string of the type "+ntik key" for the channel specified
proc ::tcldrop::irc::getchanmode {channel} {
	if {[info exists ::channels([set element [string tolower $channel]])]} {
		dict get $::channels($element) chanmodes
	} else {
		return {}
	}
}

#  pushmode <channel> <mode> [arg]
#    Description: sends out a channel mode change (ex: pushmode #lame +o
#      goober) through the bot's queuing system. All the mode changes will
#      be sent out at once (combined into one line as much as possible) after
#      the script finishes, or when 'flushmode' is called.
#    Returns: nothing
proc ::tcldrop::irc::pushmode {channel mode {arg {}}} {
	if {[validchan $channel] && [botonchan $channel]} {
		variable PushModes
		if {[info exists PushModes([set channel [string tolower $channel]])] && [set pos [lsearch -glob $PushModes($channel) "?[string index $mode 1] $arg"]] != -1} {
			# A conflicting or duplicate mode was found. So we replace it.
			set PushModes($channel) [lreplace $PushModes($channel) $pos $pos "$mode $arg"]
		} else {
			# Otherwise we just lappend to the end:
			lappend PushModes($channel) "$mode $arg"
		}
		after idle [list flushmode $channel]
		return 1
	} else {
		return 0
	}
}

proc ::tcldrop::irc::putkick {channel nicks {comment {}}} {
	# FixMe: Make sure the IRC server can handle stacking $nicks (separated by commas).
	set kicknicks [list]
	foreach nick [split $nicks ,] {
		if {[channel get $channel dontkickops] && [matchattr [nick2hand $nick $channel] o|o]} { continue }
		lappend kicknicks $nick
	}
	if {[llength $kicknicks]} { putquick "KICK $channel [join $kicknicks ,] :$comment" }
}

proc ::tcldrop::irc::GroupModes {modes} {
	set out [list]
	# First, put modes that shouldn't be mixed into separate groups. ($prevent-mixing)
	# FixMe: Needs isupport (RAW 005).  (See server module)
	if {${::prevent-mixing}} {
		foreach m $modes {
			switch -- [string index $m 1] {
				{e} - {I} { set g 2 }
				{default} { set g 1 }
			}
			lappend modegroup($g) $m
		}
	} else {
		set modegroup(0) $modes
	}
	# Process each group separately..
	foreach g [array names modegroup] {
		# Clear the vars from the last group:
		set plusmodes {}
		set minusmodes {}
		set plusvictims [list]
		set minusvictims [list]
		# Process each mode..
		foreach m $modegroup($g) {
			set victim [string range $m 3 end]
			# Separate the + and - modes:
			switch -- [string index $m 0] {
				{+} {
					if {$plusmodes == {}} { set plusmodes {+} }
					append plusmodes [string index $m 1]
					if {$victim != {}} { lappend plusvictims $victim }
				}
				{-} {
					if {$minusmodes == {}} { set minusmodes {-} }
					append minusmodes [string index $m 1]
					if {$victim != {} } { lappend minusvictims $victim }
				}
			}
			if {[string length "[string trimleft $plusmodes +][string trimleft $minusmodes -]"] >= ${::modes-per-line}} {
				# $modes-per-line limit reached..
				if {[llength [set victims [concat $plusvictims $minusvictims]]]} {
					lappend out "$plusmodes$minusmodes [join $victims]"
				} else {
					lappend out "$plusmodes$minusmodes"
				}
				# Clear the vars:
				set plusmodes {}
				set minusmodes {}
				set plusvictims [list]
				set minusvictims [list]
			}
		}
		# Process the leftovers from the group:
		if {[llength [set victims [concat $plusvictims $minusvictims]]]} {
			lappend out "$plusmodes$minusmodes [join $victims]"
		} elseif {[string length "$plusmodes$minusmodes"]} {
			lappend out "$plusmodes$minusmodes"
		}
	}
	return $out
}


#  flushmode [channel]
#    Description: forces all previously pushed channel mode changes to be
#      sent to the server, instead of when the script is finished (just for
#      the channel specified)
#    Returns: nothing
proc ::tcldrop::irc::flushmode {{channel {*}}} {
	variable PushModes
	# Process the modes for each channel array in PushModes:
	foreach c [array names PushModes [string tolower $channel]] {
		set modes [list]
		foreach m $PushModes($c) {
			set victim [string trim [string range $m 3 end]]
			# If the mode we're going to set won't do anything, we continue on to the next mode..
			switch -- [string range $m 0 1] {
				{+o} { if {[isop $victim $c]} { continue } }
				{-o} { if {![isop $victim $c]} { continue } }
				{+v} { if {[isvoice $victim $c]} { continue } }
				{-v} { if {![isvoice $victim $c]} { continue } }
			}
			# Otherwise it's a mode we need to send, so lappend it to modes:
			lappend modes $m
		}
		# GroupModes puts the modes together in an efficient way to send to IRC, and is aware of the $prevent-mixing and $modes-per-lines settings:
		foreach m [GroupModes $modes] { putquick "MODE $c $m" }
	}
}


#  topic <channel>
#    Returns: string containing the current topic of the specified channel
# Proc by Papillon@EFNet
proc ::tcldrop::irc::topic {channel} {
	if {[info exists ::channels([set element [string tolower $channel]])]} {
		if {[dict exists $::channels($element) topic]} { dict get $::channels($element) topic }
	}
}

#  onchansplit <nick> [channel]
#    Returns: 1 if that nick is split from the channel (or any channel if no
#      channel is specified); 0 otherwise
proc ::tcldrop::irc::onchansplit {nick {channel {*}}} { is split $nick $channel }

#  botisop [channel]
#    Returns: 1 if the bot has ops on the specified channel (or any channel
#      if no channel is specified); 0 otherwise
proc ::tcldrop::irc::botisop {{channel {*}}} { is op $::botnick $channel }

#| botishalfop [channel]
#|   Returns: 1 if the bot has halfops on the specified channel (or any channel
#|     if no channel is specified); 0 otherwise
proc ::tcldrop::irc::botishalfop {{channel {*}}} { is halfop $::botnick $channel }

#  botisvoice [channel]
#    Returns: 1 if the bot has a voice on the specified channel (or any
#      channel if no channel is specified); 0 otherwise
proc ::tcldrop::irc::botisvoice {{channel {*}}} { is voice $::botnick $channel }

#  isop <nickname> [channel]
#    Returns: 1 if someone by the specified nickname is on the channel (or
#      any channel if no channel name is specified) and has ops; 0 otherwise
proc ::tcldrop::irc::isop {nick {channel {*}}} { is op $nick $channel }

#| ishalfop <nickname> [channel]
#|   Returns: 1 if someone by the specified nickname is on the channel (or
#|     any channel if no channel name is specified) and has halfops; 0 otherwise
proc ::tcldrop::irc::ishalfop {nick {channel {*}}} { is halfop $nick $channel }

#  isvoice <nickname> [channel]
#    Returns: 1 if someone by that nickname is on the channel (or any
#      channel if no channel is specified) and has voice (+v); 0 otherwise
proc ::tcldrop::irc::isvoice {nick {channel {*}}} { is voice $nick $channel }

#  wasop <nickname> <channel>
#    Returns: 1 if someone that just got opped/deopped in the chan had op
#      before the modechange; 0 otherwise
proc ::tcldrop::irc::wasop {nick channel} { is wasop $nick $channel }

#| washalfop <nickname> <channel>
#|   Returns: 1 if someone that just got halfopped/dehalfopped in the chan
#|     had halfop before the modechange; 0 otherwise
proc ::tcldrop::irc::washalfop {nick channel} { is washalfop $nick $channel }

proc ::tcldrop::irc::wasvoice {nick channel} { is wasvoice $nick $channel }

proc ::tcldrop::irc::is {type nick {channel {*}}} { global channelnicks
	foreach n [array names channelnicks [string tolower "$channel,$nick"]] {
		if {[dict get $channelnicks($n) $type]} { return 1 }
	}
	return 0
}

### Channel settings, enforcement procs:
proc ::tcldrop::irc::+enforcebans {channel {nick {}}} {
	if {[botisop $channel] && [channel get $channel enforcebans]} {
		set nicklist [list]
		if {$nick == {}} {
			set nicklist [chanlist $channel -nmolyaf|-nmolyaf]
		} elseif {[onchan $nick $channel]} {
			set nicklist [list $nick]
		}
		foreach nick $nicklist {
			if {[llength [set banlist [listbans $channel ${nick}![getchanhost $nick $channel]]]]} {
				foreach ban $banlist {
					array set baninfo $ban
					if {![ischanban $baninfo(mask)]} { pushmode $channel +b $baninfo(mask) }
				}
				putkick $channel $nick $baninfo(comment)
			}
		}
	}
}
bind join - * ::tcldrop::irc::JOIN_+enforcebans -priority 100
proc ::tcldrop::irc::JOIN_+enforcebans {nick uhost handle channel} { +enforcebans $channel $nick }
bind nick - {* *} ::tcldrop::irc::NICK_+enforcebans -priority 100
proc ::tcldrop::irc::NICK_+enforcebans {nick uhost handle channel newnick} { +enforcebans $channel $newnick }

proc ::tcldrop::irc::+autovoice {channel {nick {}}} {
	if {[botisop $channel] && [channel get $channel autovoice]} {
		set nicklist [list]
		if {$nick == {}} {
			set nicklist [chanlist $channel v|v]
		} elseif {[onchan $nick $channel] && [matchattr [nick2hand $nick $channel] v|v $channel]} {
			set nicklist [list $nick]
		}
		foreach nick $nicklist { if {![isvoice $nick $channel]} { pushmode $channel +v $nick } }
	}
}
bind join - * ::tcldrop::irc::JOIN_+autovoice -priority 10000
proc ::tcldrop::irc::JOIN_+autovoice {nick uhost handle channel} {
	if {[matchattr $handle g|g $channel]} {
		pushmode $channel +v $nick
	} else {
		+autovoice $channel $nick
	}
}
bind rejn - * ::tcldrop::irc::REJN_+autovoice -priority 10000
proc ::tcldrop::irc::REJN_+autovoice {nick uhost handle channel} {
	if {![isvoice $nick $channel] && [matchattr $handle g|g $channel]} {
		pushmode $channel +v $nick
	} else {
		+autovoice $channel $nick
	}
}

proc ::tcldrop::irc::+autoop {channel {nick {}}} {
	if {[botisop $channel] && [channel get $channel autoop]} {
		set nicklist [list]
		if {$nick == {}} {
			set nicklist [chanlist $channel o|o]
		} elseif {[onchan $nick $channel] && [matchattr $nick o|o $channel]} {
			set nicklist [list $nick]
		}
		foreach nick $nicklist { if {![isop $nick $channel]} { pushmode $channel +o $nick } }
	}
}
bind join - * ::tcldrop::irc::JOIN_+autoop -priority 10000
proc ::tcldrop::irc::JOIN_+autoop {nick uhost handle channel} {
	if {[matchattr $handle a|a $channel]} {
		pushmode $channel +o $nick
	} else {
		+autoop $channel $nick
	}
}
bind rejn - * ::tcldrop::irc::REJN_+autoop -priority 10000
proc ::tcldrop::irc::REJN_+autoop {nick uhost handle channel} {
	if {![isop $nick $channel] && [matchattr $handle a|a $channel]} {
		pushmode $channel +o $nick
	} else {
		+autoop $channel $nick
	}
}

proc ::tcldrop::irc::+bitch {channel {nick {}} {handle {*}}} {
	if {[botisop $channel] && [channel get $channel bitch]} {
		if {$nick == {}} {
			set nicklist [chanlist $channel -o|-o]
		} elseif {[onchan $nick $channel] && ![matchattr $handle o|o $channel]} {
			set nicklist [list $nick]
		}
		foreach nick $nicklist { if {[isop $nick $channel]} { pushmode $channel -o $nick } }
	}
}
proc ::tcldrop::irc::MODE_+bitch {nick uhost handle channel mode victim} { +bitch $channel $nick $handle }
bind mode - {* +o} ::tcldrop::irc::MODE_+bitch -priority 100

proc ::tcldrop::irc::+cycle {channel} {
	if {![botisop $channel] && [llength [chanlist $channel]] == 1} {
		puthelp "PART $channel"
		puthelp "JOIN $channel"
	}
}
proc ::tcldrop::irc::JOIN_+cycle {nick uhost handle channel} {
	if {[string equal $::botnick $nick] && ![botisop $channel]} {
		# We wait a minute before doing the cycle check, so that there's plenty of time for the bot to receive the channel's WHO info.
		# This also keeps the bot from JOIN/PART flooding.
		timer 1 [list ::tcldrop::irc::+cycle $channel]
	}
}
bind join - * ::tcldrop::irc::JOIN_+cycle -priority 1000


bind channel - {remove *} ::tcldrop::irc::CHANNEL_remove -priority 10000
proc ::tcldrop::irc::CHANNEL_remove {command channel data} {
	if {[botonchan $channel]} {
		array unset ::channelnicks [string tolower "$channel,$::botnick"]
		putserv "PART $channel"
	}
}
bind channel - {add *} ::tcldrop::irc::CHANNEL_add -priority 10000
proc ::tcldrop::irc::CHANNEL_add {command channel data} { JoinOrPart }
bind channel - {set * flag inactive *} ::tcldrop::irc::CHANNEL_set -priority 10000
proc ::tcldrop::irc::CHANNEL_set {command channel data} { JoinOrPart }

# This gets called once a minute and joins the channels we need in, and parts the ones we're not supposed to be in.
# It also does a [callneed $channel op] if the bot is in a channel but doesn't have ops.
bind time - {* * * * *} ::tcldrop::irc::JoinOrPart -priority 10000
proc ::tcldrop::irc::JoinOrPart {args} {
	if {${::server-online}} {
		foreach channel [channels] {
			set botonchan [botonchan $channel]
			if {[set inactive [channel get $channel inactive]] && $botonchan} {
				putloglev d $channel "Joined +inactive channel $channel ...Leaving!"
				lappend partchannels $channel
				array unset ::channelnicks [string tolower "$channel,$::botnick"]
			} elseif {!$botonchan && !$inactive} {
				lappend joinchannels $channel
			} elseif {![botisop $channel]} {
				callneed $channel op
				if {![botishalfop $channel]} {
					# FixMe: This should only ask for halfop's on networks that support halfops.
					callneed $channel halfop
					if {![botisvoice $channel]} { callneed $channel voice }
				}
			}
		}
		if {[info exists partchannels]} { putserv "PART [join $partchannels ,]" }
		if {[info exists joinchannels]} { puthelp "JOIN [join $joinchannels ,]" }
	}
}

# FixMe: It needs to exit all channels during a restart, OR it needs to protect all the necessary variables from being removed during restart...

bind load - irc ::tcldrop::irc::LOAD -priority 0
proc ::tcldrop::irc::LOAD {module} {
	# Initialize variables:
	# ::nicks stores the non-channel specific info for each nick:
	# Format: ::nicks($lowernick) {array data}
	# Array data contains the following types:
	# nick, handle, ident, address, and realname (if available).
	array set ::nicks {}

	# ::channelnicks stores the channel specific info for each nick:
	# Format: ::channelnicks($lowerchannel,$lowernick) {array data}
	# Array data contains the following types:
	# op, voice, halfop, jointime, idletime.
	array set ::channelnicks {}

	# ::channels stores the non-nick specific info for each channel:
	# Format: ::channels($lowerchannel) {array data}
	# Array data contains the following types:
	# channel, chanmodes, topic.
	array set ::channels {}

	# ::bans stores the currently active channel bans:
	# Format: ::bans($lowerchannel,$lowerban) {array data}
	# Array data contains the following types:
	# ban, creator, created, channel.
	array set ::bans {}

	array set ::exempts {}

	array set ::invites {}

	# PushModes stores all the modes that need to be pushed to the server when we next hit the Tcl event loop (or when flushmode is called manually).
	variable PushModes
	array set PushModes {}

	# Set default globals:
	setdefault prevent-mixing 1
	setdefault modes-per-line 3
	setdefault network {Unknown}
	setdefault flood-msg 0:0
	setdefault flood-ctcp 0:0
	setdefault text-path {text}
	setdefault wait-split 3
	setdefault opchars {@&~} -protect 1
	loadhelp [file join set irc.help]
	bind unld - irc ::tcldrop::irc::UNLD -priority 0
	checkmodule irc::dcc
	checkmodule irc::msg
	#checkmodule ctcp
}

proc ::tcldrop::irc::UNLD {module} {
	unbind evnt - disconnect-server ::tcldrop::irc::disconnect-server -priority 1000
	unloadhelp [file join set irc.help]
	unloadmodule irc::dcc
	#unloadmodule irc::msg
	return 1
}
