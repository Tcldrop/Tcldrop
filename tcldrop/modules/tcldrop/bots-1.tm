# bots/main --
#	Handles:
#		* The core bot related commands for all bot types.
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
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.
#
#	bots module for tcldrop.
#
# This module provides the core components for bots of all types to talk to each other.

namespace eval ::tcldrop::bots {
	variable name {bots}
	variable version {0.1}
	variable depends {core::conn core::users core}
	variable author {Tcldrop-Dev}
	variable description {Core bot related components.}
	variable commands [list bots putbot putallbots islinked botids link unlink addbottype registerbot unregisterbot setbotinfo getbotinfo delbotinfo callbot calldisc calllink botinfo]
	variable rcsid {$Id$}
	namespace export {*}$commands
	namespace ensemble create -command Bots -subcommands $commands
	namespace unknown unknown
	namespace path [list ::tcldrop]
	variable script [info script]
	set ::modules($name) [list name $name version $version depends $depends author $author description $description rcsid $rcsid commands $commands script $script namespace [namespace current]]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	if {![info exists ::tcldrop]} { return }
}

proc ::tcldrop::bots::addbottype {type args} {
	variable BotTypes
	set BotTypes($type) $args
}

# These commands should provide a way to store and retrieve infos on bots of ALL types:
# At the very least, all botnet modules should use registerbot and unregister bot.
# Note: $botid is used so that every bot can be uniquely identified, even if it's connected more than once, or via more than one botnet module.
# args should be a keyed list, and contain at least "id" and "type" for the bots unique id, and the bots type.
proc ::tcldrop::bots::registerbot {botid args} {
	set ::bots([string tolower $botid]) [dict set args id [string tolower $botid]]
}

proc ::tcldrop::bots::unregisterbot {botid} {
	array unset ::bots [string tolower $botid]
}

proc ::tcldrop::bots::botinfo {botid args} {
	if {[info exists ::bots([set botid [string tolower $botid]])]} {
		if {[llength $args]} {
			# Merge $args into the $::bots($botid) dict:
			set ::bots($botid) [dict merge $::bots($botid) $args]
		} else {
			# Return all we know about $botid:
			return $::bots($botid)
		}
	} else {
		# Just set the info, turning $args into a proper dict while we're at it:
		set ::bots($botid) [dict create {*}$args]
	}
}

# Set one piece of info:
proc ::tcldrop::bots::setbotinfo {botid {info {}} {value {}}} {
	if {[info exists ::bots([set botid [string tolower $botid]])]} {
		dict set ::bots($botid) $info $value
	} else {
		# Just set the $info
		# FixMe: Or should this return an error?
		set ::bots($botid) [dict create $info $value]
	}
}

# Return one piece of info:
proc ::tcldrop::bots::getbotinfo {botid info} {
	if {[info exists ::bots([set botid [string tolower $botid]])]} { dict get $::bots($botid) $info }
	# FixMe: Should this error if the botid doesn't exist?
}

# Delete one piece of info:
proc ::tcldrop::bots::delbotinfo {botid info} {
	if {[info exists ::bots($botid)]} {
		if {[dict exists $::bots($botid) $info]} {
			dict unset ::bots($botid) $info
			return 1
		} else {
			return 0
		}
	} else {
		return 0
	}
}

proc ::tcldrop::bots::callbot {handle cmd arg} {
	foreach {type flags mask proc} [bindlist bot] {
		if {[string equal -nocase $cmd $mask] && [matchattr $handle $flags]} {
			countbind $type $mask $proc
			if {[catch { $proc $handle $cmd $arg } err]} {
				putlog "error in script: $proc: $err"
			} elseif {[string equal $err {1}]} {
				# Abort processing further binds if they return 1.
				#break
			}
		}
	}
}


#    (24) LINK (stackable)
#         bind link <flags> <mask> <proc>
#         proc-name <botname> <via>
#
#         Description: triggered when a bot links into the botnet. botname
#           is the botnetnick of the bot that just linked in; via is the bot
#           it linked through. The mask is checked against the botnetnick of
#           the bot that linked and supports wildcards. flags are ignored.
#         Module: core
# FixMe: Find out if we're supposed to use "" or ${::botnet-nick} for $via if it's a direct connection.
proc ::tcldrop::bots::calllink {bot {via {}}} {
	foreach {type flags mask proc} [bindlist link] {
		if {[string match -nocase $mask $bot]} {
			countbind $type $mask $proc
			if {[catch { $proc $bot $via } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
		}
	}
}

#    (25) DISC (stackable)
#         bind disc <flags> <mask> <proc>
#         proc-name <botname> [reason]
#
#         Description: triggered when a bot disconnects from the botnet for
#           whatever reason. Just like the link bind, flags are ignored; mask
#           is matched against the botnetnick of the bot that unlinked.
#           Wildcards are supported in mask.
#         Module: core
# Support for reason is Tcldrop-only.
proc ::tcldrop::bots::calldisc {bot {reason {}}} {
	foreach {type flags mask proc} [bindlist disc] {
		if {[string match -nocase $mask $bot]} {
			if {[llength [info args $proc]] == 2} {
				if {[catch { $proc $bot $reason } err]} {
					putlog "Error in $proc: $err"
					puterrlog "$::errorInfo"
				}
			} else {
				if {[catch { $proc $bot } err]} {
					putlog "Error in $proc: $err"
					puterrlog "$::errorInfo"
				}
			}
			countbind $type $mask $proc
		}
	}
}


# This should find out what botnet module $botnet is using, using getbotinfo, and then pass $text on to that module's putbot command.
# $bot can refer to the botid OR the bots handle (for compatibility with Eggdrop).
proc ::tcldrop::bots::putbot {bot text} {
	global bots
	if {![info exists bots([set bot [string tolower $bot]])]} {
		foreach b [array names bots] {
			if {[string equal -nocase [dict get $bots($b) handle] $bot]} {
				set bot $b
				break
			}
		}
	}
	# Right now there's 5 ways to handle putbot's.. They're tried in their prefered order, so there shouldn't be any slowdown in supporting all these ways:
	# FixMe: Consider creating a new bind instead of doing this.  The new bind could be called "putbot".
	variable BotTypes
	array set typeinfo $BotTypes([dict get $bots($bot) type])
	if {[info exists typeinfo(putbot)]} {
		$typeinfo(putbot) $bot $text
	} elseif {[info exists typeinfo(namespace)] && [info commands $typeinfo(namespace)::putbot] != {}} {
		$typeinfo(namespace)::putbot $bot $text
	} elseif {[info exists botinfo(putbot)]} {
		$botinfo(putbot) $bot $text
	} elseif {[info exists botinfo(namespace)] && [info commands $botinfo(namespace)::putbot] != {}} {
		$botinfo(namespace)::putbot $bot $text
	} elseif {[info exists botinfo(idx)]} {
		putidx $botinfo(idx) $text
	}
}

proc ::tcldrop::bots::putallbots {text} {
	# FixMe: Consider creating a new bind instead of doing this.  The new bind could be called "putallbots".
	variable BotTypes
	foreach t [array names BotTypes] {
		array set typeinfo $BotTypes($t)
		if {[info exists $typeinfo(putallbots)]} {
			$typeinfo(putallbots) $text
		} elseif {[info exists $typeinfo(namespace)] && [info commands $typeinfo(namespace)::putallbots] != {}} {
			$typeinfo(namespace)::putallbots $text
		} else {
			# Fallback to doing a normal putbot for all bots of this type (in case the bot type doesn't support sending to multiple bots at once):
			global bots
			foreach b [array get bots] {
				if {[string match [dict get $bots($b) type] $t]} { putbot $b $text }
			}
		}
	}
}

# Note: $bot should refer to the botid when possible, but it falls back to using the bots handle.
proc ::tcldrop::bots::islinked {bot {type {}}} {
	global bots
	if {[info exists bots([string tolower $bot])]} {
		return 1
	} else {
		foreach b [array names bots] {
			if {[string equal -nocase [dict get $bots($b) handle] $bot]} { return 1 }
		}
	}
	return 0
}

# Note: For compatibility with Eggdrop, [bots] returns all the bots handles in a sorted list that removes duplicate handles.
# If -type <type> is specified, only bots of that type are returned.
# FixMe: Just supporting -type is too limited.  It should support any key.
proc ::tcldrop::bots::bots {args} {
	global bots
	set type {}
	foreach {o v} $args {
		switch -- $o {
			{-type} { set type $o }
		}
	}
	set BotList [list]
	if {$type eq {}} {
		foreach b [array names bots] {
			lappend BotList [dict get $bots($b) handle]
		}
	} else {
		foreach b [array names bots] {
			if {[string equal -nocase [dict get $bots($b) type] $type]} {
				lappend BotList [dict get $bots($b) handle]
			}
		}
	}
	lsort -unique $BotList
}

# Returns the botid's instead of the handles.
proc ::tcldrop::bots::botids {args} {
	array names ::bots
}

# This will require looking at the userfile [getuser $botname BOTTYPE] to find out what botnet module $botname uses:
# FixMe: Consider creating a new bind instead of doing this.  The new bind could be called "prelink" ("link" is already taken).
proc ::tcldrop::bots::link {viabot {bothandle {}} {type {}}} {
	if {$bothandle == {}} {
		set bothandle $viabot
		set viabot {}
	}
	if {[matchattr $bothandle b]} {
		if {[catch { getuser $bothandle BOTTYPE } bottype]} { set bottype {eggdrop} }
		variable BotTypes
		if {[info exists BotTypes($bottype)]} {
			array set typeinfo $BotTypes($bottype)
			$typeinfo(namespace)::link $bothandle
		}
	}
}

# FixMe: Consider creating a new bind instead of doing this.  The new bind could be called "preunlink".
proc ::tcldrop::bots::unlink {botname {type {}}} {
	array set botinfo [botinfo $botname]
	if {[info exists botinfo(type)]} {
		variable BotTypes
		array set typeinfo $BotTypes($botinfo(type))
		if {[info exists typeinfo(unlink)]} {
			$typeinfo(unlink) $botinfo(id)
		} elseif {[info exists typeinfo(namespace)] && [info commands $typeinfo(namespace)::unlink] != {}} {
			$typeinfo(namespace)::unlink $botinfo(id)
		} elseif {[info exists botinfo(unlink)]} {
			$botinfo(unlink) $botinfo(id)
		} elseif {[info exists botinfo(namespace)] && [info commands $botinfo(namespace)::unlink] != {}} {
			$botinfo(namespace)::unlink $botinfo(id)
		}
	}
	return 0
}

#bind time - {* * * * *} ::tcldrop::bots::AutoLinkBots
proc ::tcldrop::bots::AutoLinkBots {minute hour day month year} {
	foreach b [userlist b] {
		# FixMe: This should only link bots with +h or +a botflags.
		if {![islinked $b]} {
			link $b
		}
	}
}


bind disc - * ::tcldrop::party::DISC -priority -1000
proc ::tcldrop::party::DISC {bot {reason {Unlinked.}}} {
	callparty quit *:*@$bot line $reason
	callparty disc $bot line $reason
}

bind link - * ::tcldrop::party::LINK -priority -1000
proc ::tcldrop::party::LINK {bot via} {
	callparty link $bot handle $bot bot $bot via $via
}


bind load - bots ::tcldrop::bots::LOAD -priority 0
proc ::tcldrop::bots::LOAD {module} {
	variable BotTypes
	array set BotTypes {}
	array set ::bots {}
	bind evnt - loaded ::tcldrop::bots::EVNT_loaded -priority 0
	bind unld - bots ::tcldrop::bots::UNLD -priority 0
	proc ::tcldrop::bots::UNLD {module} {
		return 1
	}
	checkmodule bots::eggdrop
	checkmodule bots::dcc
}

proc ::tcldrop::bots::EVNT_loaded {event} {
	# Do like Eggdrop, and set botnet-nick to $nick (if it's not already set of course):
	setdefault botnet-nick $::nick
}
