# core/users --
#	Handles:
#		* All userfile-related Tcl commands.
#	Depends: core.
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
#	core::users module for Tcldrop.  (REQUIRED)

namespace eval ::tcldrop::core::users {
	variable version {0.7}
	variable name {core::users}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	if {![info exists ::tcldrop]} { return }
	variable depends {channels core::database encryption core}
	variable author {Tcldrop-Dev}
	variable description {Provides all userfile-related Tcl commands.}
	variable rcsid {$Id$}
	variable commands [list adduser countusers validuser finduser matchattr matchchanattr userlist passwdok getuser setuser getinfo getchaninfo getting-users chhandle chattr botattr chflags addbot deluser delhost addchanrec delchanrec haschanrec save backup reload chpass setlaston addhost]
	variable aliases [list add adduser count countusers valid validuser isvalid validuser find finduser list userlist get getuser set setuser getting getting-users del deluser + adduser - deluser +user adduser -user deluser +bot addbot -host delhost +host addhost]
	namespace ensemble create -command ::users -map $aliases -subcommands $commands
	namespace ensemble create -command ::user -map $aliases -subcommands $commands
	namespace ensemble create -map $aliases -subcommands $commands
	namespace ensemble create -command [namespace parent]::user -map $aliases -subcommands $commands
	namespace ensemble create -command ::tcldrop::users -map $aliases -subcommands $commands
	namespace ensemble create -command ::tcldrop::user -map $aliases -subcommands $commands
	namespace ensemble create -command user -map $aliases -subcommands $commands
	namespace export {*}$commands
}

#    (41) NKCH (stackable)
#         bind nkch <flags> <mask> <proc>
#         proc-name <oldhandle> <newhandle>
#
#         Description: triggered whenever a local user's handle is changed
#           (in the userfile). mask is matched against the user's old handle
#           and can contain wildcards; flags are ignored.
#         Module: core
proc ::tcldrop::core::users::callnkch {oldhandle newhandle} {
	foreach {type flags mask proc} [bindlist nkch] {
		if {[string match -nocase $mask $oldhandle]} {
			if {[catch { $proc $oldhandle $newhandle } err]} {
				putlog "Error in script: $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

# Returns the number of users in the user database:
proc ::tcldrop::core::users::countusers {} {
	if {[info exists ::database(users)]} {
		dict size $::database(users)
	} else {
		return 0
	}
}

# Checks to see if $handle is a valid user in the user database:
# Returns 1 if they are, 0 if they're not.
proc ::tcldrop::core::users::validuser {handle} { dict exists $::database(users) [string tolower $handle] }

# Searches the user database for the handle that most closely matches $nuhost.
# Returns the matching handle, or "*" if none found.
proc ::tcldrop::core::users::finduser {nuhost} {
	if {![string match {*!*} $nuhost]} { set nuhost "*!$nuhost" }
	foreach u [userlist] { foreach h [getuser $u hosts] { if {[string match -nocase $h $nuhost]} { return $u } } }
	return {*}
}

# Checks $handle for $flags, $channel is optional.
proc ::tcldrop::core::users::matchattr {handle flags {channel {}}} {
	switch -exact -- $flags {
		{*} - {+} - {*|*} - {+|+} - {} {
			# * or + means anybody, return 1.
			return 1
		}
		{default} {
			if {![validuser $handle]} {
				return 0
			} elseif {[checkflags [lindex [split $flags {|}] 0] [getuser $handle flags]] == 1} {
				# Return 1 since we matched one of their global flags.
				return 1
			} elseif {$channel != {} && [string match {*\|*} $flags]} {
				# Try matching a channel flag.
				switch -- $channel {
					{*} - {-all} - {all} { set channels [channels] }
					{default} { if {[validchan $channel]} { set channels [list $channel] } else { return 0 } }
				}
				foreach channel $channels {
					if {[checkflags [lindex [split $flags {|}] end] [lindex [split [getuser $handle flags $channel] {|}] end]] == 1} {
						# Return 1 since we matched one of their channel flags.
						return 1
					}
				}
				return 0
			} else {
				# Return 0 since there was no match.
				return 0
			}
		}
	}
}

# Alias for matchattr:
proc ::tcldrop::core::users::matchchanattr {handle flags {channel {}}} { matchattr $handle $flags $channel }

# Returns all the users matching $flags (flags are optional).
proc ::tcldrop::core::users::userlist {{flags {}} {channel {}}} {
	set userlist [list]
	dict for {handle info} $::database(users) { if {[matchattr $handle $flags $channel]} { lappend userlist [dict get $info handle] } }
	return $userlist
}

proc ::tcldrop::core::users::passwdok {handle {pass {-}} args} {
	if {[validuser $handle]} {
		switch -- $pass {
			{-} - {} { string equal {} [getuser $handle pass] }
			{default} { string equal [Encpass $pass $args] [getuser $handle pass] }
		}
	} else {
		return 0
	}
}

# Gets user related info:
proc ::tcldrop::core::users::getuser {handle args} {
	if {[dict exists $::database(users) [set lowerhandle [string tolower $handle]]]} {
		switch -- [string tolower [lindex $args 0]] {
			{xtra} - {flags} - {console} - {hosts} - {pass} - {comment} {
				# These fields are special, because Eggdrop doesn't return an error if you try to get a non-existant one.
				if {[catch { dict get $::database(users) $lowerhandle {*}[string tolower $args] } return ]} {
					return {}
				} else {
					return $return
				}
			}
			{default} {
				if {[catch { dict get $::database(users) $lowerhandle {*}[string tolower $args] } return]} {
					return -code error "No such info type: $args"
				} else {
					return $return
				}
			}
		}
	} else {
		return -code error "No such user: $handle"
	}
}

# Sets user related info:
# pass <unencrypted pass>
# hosts <*1* host, or blank to clear all hosts>
# flags <flags> [channel]
# botfl <flags> [channel]
# laston <unixtime> [place/channel]
# info <info line> [channel]
# handle <newhandle, or blank to delete the user>
# It returns the new setting.
proc ::tcldrop::core::users::setuser {handle {type {}} {setting {}} {xtra {}} args} {
	global database
	if {![dict exists $database(users) [set lowerhandle [string tolower $handle]]]} {
		switch -- [string tolower $type] {
			{handle} {
				# Add the user:
				return [database users set $lowerhandle handle $setting]
			}
			{} {
				# Add the user:
				return [database users set $lowerhandle handle $handle]
			}
			{default} { return -code error "No such user: $handle" }
		}
	}
	switch -- [set type [string tolower $type]] {
		{pass} {
			if {$setting != {}} {
				database users set $lowerhandle $type [encpass $setting]
			} else {
				# Clear (unset) their password:
				database users unset $lowerhandle $type
			}
		}
		{hosts} {
			#putlog "handle $handle type $type setting $setting xtra $xtra args $args"
			if {$setting != {}} {
				# Add a new host to their list of hostmasks:
				# FixMe: Don't add a duplicate.
				if {[catch { dict get $database(users) $lowerhandle $type } value] || [lsearch -glob $value [set setting [string tolower $setting]]] == -1} {
					database users lappend $lowerhandle $type $setting
				}
			} else {
				# Delete all their hostmasks:
				database users unset $lowerhandle $type
			}
		}
		{flags} - {botfl} - {info} {
			# Note, this REPLACES the flags/botfl/info with the new info (This is what happens in Eggdrop too).
			# So use chflags (or chattr or botattr) if you want to add (merge) to the existing flags.
			if {$xtra != {}} {
				# Set $type for $setting (probably the channel name) to $xtra:
				database users set $lowerhandle $type [string tolower $setting] $xtra
			} elseif {$setting != {}} {
				# Set global $type to $setting:
				database users set $lowerhandle $type $setting
			} else {
				# Delete all data matching $type:
				database users unset $lowerhandle $type
			}
		}
		{laston} {
			if {$xtra != {}} {
				# Set the laston for $xtra (the place) to $setting (the unixtime):
				database users set $lowerhandle $type [string tolower $xtra] $setting
				# Set the global laston to $setting (the unixtime) $xtra (the place):
				database users set $lowerhandle $type [list $setting $xtra]
			} elseif {$setting != {}} {
				# Set the global laston to $setting (the unixtime):
				database users set $lowerhandle $type [list $setting]
			} else {
				# Delete all of the laston data:
				database users unset $lowerhandle $type
			}
		}
		{handle} {
			if {$setting != {}} {
				# Rename the account:
				if {![dict exists database(users) [set lowersetting [string tolower $setting]]]} {
					if {[database users rename $lowerhandle $lowersetting] && [database users set $lowersetting handle $setting]} {
						callnkch $oldhandle $newhandle
						return 1
					} else {
						return 0
					}
				} else {
					return -code error "Another user already has the handle $setting"
				}
			} else {
				# Delete the $lowerhandle account:
				database users unset $lowerhandle
			}
		}
		{xtra} {
			if {$setting eq {}} {
				# You can't clear all the XTRA's at once, Eggdrop doesn't even let you.
				return -code error {wrong # args: should be "setuser handle type key ?value?"}
			} elseif {$xtra != {}} {
				# Add/replace:
				database users set $lowerhandle $type [string tolower $setting] $xtra
			} else {
				# Clear (delete) XTRA $setting:
				database users unset $lowerhandle $type [string tolower $setting]
			}
		}
		{} {
			# Delete the $lowerhandle account:
			database users unset $lowerhandle
		}
		{default} {
			if {$setting eq {}} {
				# They want $type cleared (deleted).
				database users unset $lowerhandle $type
			} else {
				# Set $type to $setting:
				if {[llength $args]} {
					::tcldrop::core::database::Database users set $lowerhandle $type [string tolower $setting] $args
				} else {
					database users set $lowerhandle $type $setting
				}
			}
		}
	}
}

proc ::tcldrop::core::users::setpass {handle password {option {}} {type {}}} { setuser $handle pass $password $option $type }

# This is an eggdrop v1.7.0 command:
proc ::tcldrop::core::users::setlaston {handle when {where {}}} { setuser $handle laston $when $where }

# Gets a users channel INFO line:
proc ::tcldrop::core::users::getchaninfo {handle channel} { getuser $handle info $channel }

# Renames (or deletes) a user account.
proc ::tcldrop::core::users::chhandle {oldhandle {newhandle {}}} { setuser $oldhandle handle $newhandle }

# Returns 1 if we're downloading the userfile from another bot, or 0 if not.
proc ::tcldrop::core::users::getting-users {} {
	# FixMe: This should return 1 if we're downloading the userfile from another bot.
	# FixMe: I'm not sure which module this proc belongs in right now.
	return 0
}

# Adds/removes a users global/channel flags.
proc ::tcldrop::core::users::chflags {handle type {flags {}} {channel {}}} {
	if {[catch { set current [getuser $handle $type] }]} { set current {} }
	if {[set addflags [lindex [split $flags {|}] 0]] != {}} {
		set out [setuser $handle $type [mergeflags $addflags $current]]
	} else {
		set out $current
	}
	if {$channel != {}} {
		if {[catch { set current [getuser $handle $type $channel] }]} { set current {} }
		if {[set addflags [lindex [split $flags {|}] end]] != {}} {
			append out "|[setuser $handle flags [mergeflags $addflags $current] $channel]"
		}
	}
	return $out
}


# Adds/removes a users global/channel flags.
proc ::tcldrop::core::users::chattr {handle {flags {}} {channel {}}} { chflags $handle flags $flags $channel }

# Adds/removes a bots global/channel bot-specific flags.
proc ::tcldrop::core::users::botattr {handle {flags {}} {channel {}}} { if {[matchattr $handle b]} { chflags $handle botfl $flags $channel } }

# Adds a user to the user database, with the optional hostmask:
# Returns 1 for success, 0 for failure.
proc ::tcldrop::core::users::adduser {handle {hostmask {}}} {
	if {$handle ne {} && ![validuser $handle]} {
		setuser $handle
		setuser $handle hosts $hostmask
		setuser $handle console {}
		setuser $handle flags ${::default-flags}
		setlaston $handle 0 {never}
		# Call all of the ADDUSER binds:
		foreach {type flags mask proc} [bindlist adduser] {
			if {[string match -nocase $mask $handle]} {
				countbind $type $mask $proc
				if {[catch { $proc $handle } err]} {
					putlog "Error in script: $proc: $err"
					puterrlog "$::errorInfo"
				}
				# FixMe: Should we care when return value they give?
			}
		}
		return 1
	} else {
		return 0
	}
}

# Adds a bot to the user database, with the optional address and hostmask:
# Returns 1 for success, 0 for failure.
proc ::tcldrop::core::users::addbot {handle {address {}} {hostmask {}} args} {
	if {$handle ne {} && ![validuser $handle]} {
		setuser $handle
		setuser $handle hosts $hostmask
		setuser $handle flags {b}
		setuser $handle console {}
		setuser $handle botaddr [list [lindex [split $address :] 0] [lindex [split $address :/] 1] [lindex [split $address /] end]]
		setlaston $handle 0 {never}
		return 1
	} else {
		return 0
	}
}

# Deletes a user (or bot) from the user database:
# Returns 1 for success, 0 for failure.
proc ::tcldrop::core::users::deluser {handle} {
	# Renaming handle to {} tells setuser to remove the account:
	setuser $handle handle {}
}

# Proc by Papillon@EFNet
# FixMe: Untested and unmodified.
# Removes a single hostmask, or all hostmasks if none is specified.
proc ::tcldrop::core::users::delhost {handle {hostmask {}}} {
	# Specifying an empty host tells setuser to remove all the hosts.  (This is just like Eggdrop)
	if {$hostmask eq {}} {
		setuser $handle hosts {}
	} else {
		# Based this on the Users array and added the use of the maskhost command (gotta love it eh ;) --- Papillon
		if {[set position [lsearch -glob [dict get $::database(users) [set lowerhandle [string tolower $handle]] hosts] $hostmask]] != -1} {
			# FixMe: verify that it was removed?
			database users lremove $lowerhandle hosts $position $position
			return 1
		} else {
			return 0
		}
	}
}

# addhost is in compat.tcl, but I'm adding it here because there's a delhost command here:
proc ::tcldrop::core::users::addhost {handle {hostmask {}}} { setuser $handle HOSTS $hostmask }

# Adds a channel record for a user:
proc ::tcldrop::core::users::addchanrec {handle channel} { setuser $handle $channel flags - }

# Removes a channel record for a user:
proc ::tcldrop::core::users::delchanrec {handle channel} { setuser $handle $channel }

proc ::tcldrop::core::users::haschanrec {handle channel} { dict exists $::database(users) [string tolower $handle] [string tolower $channel] }

# Saves the user database to the hard disk:
proc ::tcldrop::core::users::save {args} {
	if {[info exists ::userfile]} { set filename $::userfile } else { set filename {} }
	if {[info exists ::userfile-perm]} { set perm ${::userfile-perm} } else { set perm ${::database-perm} }
	database users save -file $filename -permissions $perm
}

# Proc by Papillon@EFNet. (untested/unmodified)
proc ::tcldrop::core::users::backup {args} { if {[countusers]} { database users backup } }

# Reload the userfile from disk:
proc ::tcldrop::core::users::reload {} {
	if {[info exists ::userfile]} { set filename $::userfile } else { set filename {} }
	if {![catch { database users reload -file $filename }]} {
		putlog {Userfile loaded.}
	} elseif {![catch { database users create }]} {
		putlog "starting new userfile."
	}
}

# seenuser <nickname>
# Module Required: racseen
# Returns: A seen details list for <nickname>, or nothing
# Description:
# This command returns the last seen details for <nickname>. This is
# the details that <nickname> was using when they were last seen on
# IRC by the Bot. The list is in the format:
# {nickname handle userhost unixtime-last-seen}
# (Taken from http://www.racbot.org/docs/tclcmds/scripting_tcl_commands.html)


# That's it, that's all that we consider "user" commands.
# The ban, exempt, invite and ignore related stuff is in the channels module, since they can be considered channel related.

proc ::tcldrop::core::users::Save {type} {
	if {([set count [countusers]] > 1) || ($count == 1 && ![passwdok [lindex [userlist] 0] {}])} {
		save
	}
}

bind load - core::users ::tcldrop::core::users::LOAD -priority 0
proc ::tcldrop::core::users::LOAD {module} {
	setdefault sort-users 0
	setdefault default-flags {}
	bind evnt - prerestart ::tcldrop::core::users::Save -priority 0
	bind evnt - save ::tcldrop::core::users::Save -priority 1
	bind evnt - hourly-updates ::tcldrop::core::users::Save -priority 2
	bind evnt - die ::tcldrop::core::users::Save -priority 0
	#bind evnt - exit ::tcldrop::core::users::Save -priority 0
}

bind evnt - loaded ::tcldrop::core::users::EVNT_loaded -priority 0
proc ::tcldrop::core::users::EVNT_loaded {event} { reload }

bind unld - core::users ::tcldrop::core::users::UNLD
proc ::tcldrop::core::users::UNLD {module} {
	unbind evnt * * ::tcldrop::core::users::*
	return 1
}
