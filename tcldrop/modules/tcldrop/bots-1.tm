# bots/main --
#	Handles:
#		* The core bot related commands for all bot types.
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
# info should be a keyed list, and contain at least "id" and "type" for the bots unique id, and the bots type.
proc ::tcldrop::bots::registerbot {botid {info {}}} {
	variable Bots
	array set botinfo $info
	set botid [string tolower $botid]
	if {![info exists botinfo(id)]} { set botinfo(id) $botid }
	set Bots($botid) [array get botinfo]
}

proc ::tcldrop::bots::unregisterbot {botid} {
	variable Bots
	array unset Bots [string tolower $botid]
}

proc ::tcldrop::bots::botinfo {botid {info {}}} {
	variable Bots
	if {[info exists Bots([set botid [string tolower $botid]])]} {
		if {$info eq {}} {
			return $Bots($botid)
		} else {
			array set botinfo $Bots($botid)
			array set botinfo $info
			set Bots($botid) [array get botinfo]
		}
	}
}

proc ::tcldrop::bots::setbotinfo {botid info} {
	# FixMe: This should set a single piece of info about $botname.
}

proc ::tcldrop::bots::getbotinfo {botid info} {
	# FixMe:
}

proc ::tcldrop::bots::delbotinfo {botid info} {
	# FixMe:
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
#         proc-name <botname>
#
#         Description: triggered when a bot disconnects from the botnet for
#           whatever reason. Just like the link bind, flags are ignored; mask
#           is matched against the botnetnick of the bot that unlinked.
#           Wildcards are supported in mask.
#         Module: core
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
	variable Bots
	if {[info exists Bots([set bot [string tolower $bot]])]} {
		array set botinfo $Bots($bot)
	} else {
		foreach b [array names Bots] {
			array set botinfo $Bots($b)
			if {[string equal -nocase $botinfo(handle) $bot]} { break }
		}
	}
	# Right now there's 5 ways to handle putbot's.. They're tried in their prefered order, so there shouldn't be any slowdown in supporting all these ways:
	# FixMe: Consider creating a new bind instead of doing this.  The new bind could be called "putbot".
	variable BotTypes
	array set typeinfo $BotTypes($botinfo(type))
	if {[info exists typeinfo(putbot)]} {
		$typeinfo(putbot) $botinfo(id) $text
	} elseif {[info exists typeinfo(namespace)] && [info commands $typeinfo(namespace)::putbot] != {}} {
		$typeinfo(namespace)::putbot $botinfo(id) $text
	} elseif {[info exists botinfo(putbot)]} {
		$botinfo(putbot) $botinfo(id) $text
	} elseif {[info exists botinfo(namespace)] && [info commands $botinfo(namespace)::putbot] != {}} {
		$botinfo(namespace)::putbot $botinfo(id) $text
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
			variable Bots
			foreach b [array get Bots] {
				array set botinfo $Bots($b)
				if {[string match $botinfo(type) $t]} { putbot $b $text }
			}
		}
	}
}

# Note: $bot should refer to the botid when possible, but it falls back to using the bots handle.
proc ::tcldrop::bots::islinked {bot {type {}}} {
	variable Bots
	if {[info exists Bots([string tolower $bot])]} {
		return 1
	} else {
		foreach b [array names Bots] {
			array set botinfo $Bots($b)
			if {[string equal -nocase $botinfo(handle) $bot]} { return 1 }
		}
	}
	return 0
}

# Note: For compatibility with Eggdrop, [bots] returns all the bots handles in a sorted list that removes duplicate handles.
# If -type <type> is specified, only bots of that type are returned.
proc ::tcldrop::bots::bots {args} {
	switch -- [lindex $args 0] {
		{-type} - {-TYPE} - {-Type} - {} {
			set bots [set type [variable Bots]]
			foreach {o v} $args {
				switch -- $o {
					{-type} { set type $o }
				}
			}
			if {$type == {}} {
				foreach i [array names Bots] {
					array set botinfo $Bots($i)
					lappend bots $botinfo(handle)
				}
			} else {
				foreach i [array names Bots] {
					array set botinfo $Bots($i)
					if {[string equal -nocase $botinfo(type) $type]} { lappend bots $botinfo(handle) }
				}
			}
			lsort -unique $bots
		}
		{default} { Bots {*}$args }
	}
}

# Works just like [bots], except this returns the botid's instead of the handles.
proc ::tcldrop::bots::botids {args} {
	set botids [set type {}]
	foreach {o v} $args {
		switch -- $o {
			{-type} { set type $o }
		}
	}
	if {$type == {}} {
		variable Bots
		array names Bots
	} else {
		foreach i [array names Bots] {
			array set botinfo $Bots($i)
			if {[string equal -nocase $botinfo(type) $type]} { lappend botids $botinfo(id) }
		}
		lsort -unique $botids
	}
}

# This will require looking at the userfile [getuser $botname BOTTYPE] to find out what botnet module $botname uses:
# FixMe: Consider creating a new bind instead of doing this.  The new bind could be called "prelink" ("link" is already taken).
proc ::tcldrop::bots::link {viabot {bothandle {}} {type {}}} {
	if {$bothandle == {}} {
		set bothandle $viabot
		set viabot {}
	}
	if {[matchattr $bothandle b]} {
		if {[set bottype [getuser $bothandle BOTTYPE]] == {}} { set bottype {oldbotnet} }
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

bind load - bots ::tcldrop::bots::LOAD -priority 0
proc ::tcldrop::bots::LOAD {module} {
	variable BotTypes
	array set BotTypes {}
	variable Bots
	array set Bots {}
	bind evnt - loaded ::tcldrop::bots::EVNT_loaded -priority 0
	bind unld - bots ::tcldrop::bots::UNLD -priority 0
	proc ::tcldrop::bots::UNLD {module} {
		return 1
	}
}

proc ::tcldrop::bots::EVNT_loaded {event} {
	# Do like Eggdrop, and set botnet-nick to $nick (if it's not already set of course):
	setdefault botnet-nick $::nick
}
