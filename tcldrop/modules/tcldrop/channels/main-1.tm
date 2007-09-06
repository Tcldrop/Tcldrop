# channels/channels.tcl --
#	Handles:
#		* All channel related commands.  (mainly the ones that translate into database commands)
#
# $Id: channels.tcl,v 1.8 2005/07/31 04:17:25 fireegl Exp $
#
# Copyright (C) 2003,2004,2005 FireEgl (Philip Moore) <FireEgl@Tcldrop.Org>
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
# The author of this project can be reached at FireEgl@Tcldrop.Org
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
#	channels module for tcldrop.
#	Depends on: none.

namespace eval ::tcldrop::channels {
	variable version {0.8}
	variable name {channels}
	variable depends {core::database core}
	variable author {Tcldrop-Dev}
	variable description {All channel related commands.}
	variable commands [list channel channels loadchannels savechannels validchan setudef renudef deludef validudef callchannel countchannels newchanbei newbei stickbei unstickbei killchanbei killbei isbei ischanbei ispermbei isbeisticky matchbei beilist listbeis loadbeis savebeis newchanban newban stick unstick killchanban killban isban ischanban ispermban isbansticky matchban banlist listbans newchanexempt newexempt stickexempt unstickexempt killchanexempt killexempt isexempt ischanexempt ispermexempt isexemptsticky matchexempt exemptlist listexempts newchaninvite newinvite stickinvite unstickinvite killchaninvite killinvite isinvite ischaninvite isperminvite isinvitesticky matchinvite invitelist listinvites newchanignore newignore stickignore unstickignore killchanignore killignore isignore ischanignore ispermignore isignoresticky matchignore ignorelist listignores]
	variable script [info script]
	variable rcsid {$Id: channels.tcl,v 1.8 2005/07/31 04:17:25 fireegl Exp $}
	# Provide the channels module:
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	eval [linsert $commands 0 namespace export]
	# Create ensembles:
	catch { namespace ensemble create -command Channels -subcommands $commands }
}

# Note: - is used in place of a channel name when it applies globally.

proc ::tcldrop::channels::channel {command {channel {}} args} {
	set lowerchannel [string tolower $channel]
	global database
	switch -- [set command [string tolower $command]] {
		{add} {
			# FixMe: I think there's something wrong about this next line:
			if {[llength $args] > 1} { set options $args } else { set options [lindex $args 0] }
			# Add the channel:
			database channels set $lowerchannel name $channel
			SetUdefDefaults
			after idle [list callchannel $command $channel $options]
			# Call ourself again to set the options:
			after idle [list eval [linsert $options 0 channel set $channel]]
		}
		{set} {
			if {![dict exists $database(channels) $lowerchannel]} { return -code error "Invalid Channel: $channel" }
			# In the case of "set", $args is already in the form we can use.
			set setnext 0
			foreach o $args {
				if {$setnext} {
					set setnext 0
					switch -- $type {
						{int} - {integer} {
							# Note, settings such as flood-chan are treated as int's.  Hence the need for using split here:
							database channels set $lowerchannel $name [set o [split $o {:{ }}]]
							after idle [list callchannel $command $channel $type $o]
						}
						{str} - {string} {
							database channels set $lowerchannel $name $o
							after idle [list callchannel $command $channel $type $name $o]
						}
						{list} {
							database channels lappend $lowerchannel $name $o
							after idle [list callchannel $command $channel $type $name $o]
						}
						{flag} {
							# This is so we can support flags being set like:
							# [channel set #channel bitch +]
							# or: [channel set #channel revenge 1]
							# The old way is still supported though. (see below)
							switch -- $o {
								{+} - {1} - {y} - {Y} {
									database channels set $lowerchannel $name 1
									after idle [list callchannel $command $channel $type $name 1]
								}
								{-} - {0} - {n} - {N} {
									database channels set $lowerchannel $name 0
									after idle [list callchannel $command $channel $type $name 0]
								}
								{default} {
									return -code error "invalid value for a channel flag: $o"
								}
							}
						}
						{unknown} - {default} {
							return -code error "Invalid channel option: $name"
						}
					}
				} elseif {$o != {}} {
					switch -- [set type [UdefType [set name [string trimleft $o {+-}]]]] {
						{flag} {
							switch -- [string index $o 0] {
								{+} {
									database channels set $lowerchannel $name 1
									after idle [list callchannel $command $channel $type $name 1]
								}
								{-} {
									database channels set $lowerchannel $name 0
									after idle [list callchannel $command $channel $type $name 0]
								}
								{default} {
									# They must want to set it using a second arg...
									set setnext 1
								}
							}
						}
						{int} - {str} - {list} - {integer} - {string} { set setnext 1 }
						{unknown} - {default} { return -code error "illegal channel option: $name" }
					}
				}
			}
		}
		{info} {
			# COMPATIBILITY WARNING: Because Eggdrop doesn't return the info in any documented or understandable order,
			#                        Tcldrop will return a list of each channel setting and it's value.  This way makes the info MUCH easier to use by Tcl scripters.
			if {[dict exists $database(channels) $lowerchannel]} {
				dict get $database(channels) $lowerchannel
			} else {
				return -code error "no such channel record: $channel"
			}
		}
		{get} {
			if {[dict exists $database(channels) $lowerchannel]} {
				if {[eval [linsert $args 0 dict exists $database(channels) $lowerchannel]]} {
					eval [linsert $args 0 dict get $database(channels) $lowerchannel]
				} else {
					return -code error "Unknown channel setting: $args"
				}
			} else {
				return -code error "no such channel record: $channel"
			}
		}
		{list} {
			set list [list]
			dict for {key value} $database(channels) { lappend list [dict get $value name] }
			return $list
		}
		{count} { dict size $database(channels) }
		{remove} - {rem} - {delete} - {del} {
			if {[dict exists $database(channels) $lowerchannel]} {
				database channels unset $lowerchannel
				after idle [list callchannel $command $channel $args]
			} else {
				return -code error "no such channel record: $channel"
			}
		}
		{default} { return -code error "Unknown channel sub-command \"$command\"." }
	}
}

# Add the [addchanset] RacBot command:
# http://www.racbot.org/docs/tclcmds/channel_setting_change_tcl_commands.html
# + addchanset switch|text|numeric <name>
# Module Required: racchannel
# Returns: nothing
# Description:
# This command adds a new Channel Setting. The setting can be
# either a Channel Switch ("switch"), a Channel Text Setting
# ("text"), or a Channel Numeric Setting ("numeric"). <name> is
# the name of the setting. These channel settings are stored
# within the channel file and will be reloaded at startup (as long
# as the "addchanset" command or Tcl script is always defined in
# the Bot's configuration).

# Just like in Eggdrop, returns the list of channels.
# FixMe: Make [channels] a namespace ensemble if/when they fix it so the -unknown option works properly.
proc ::tcldrop::channels::channels {args} {
	if {$args eq {*} || [llength $args] == 0 || [validchan [lindex $args 0]]} {
		set list [list]
		dict for {key value} $::database(channels) { lappend list [dict get $value name] }
		return $list
	} else {
		eval {Channels} $args
	}
}

# This isn't from Eggdrop, but I'm providing it anyway:
# Works just like [countusers], except this counts how many channels there are.
proc ::tcldrop::channels::countchannels {} { dict size $::database(channels) }

# Saves the channel info to $chanfile:
proc ::tcldrop::channels::savechannels {} {
	if {[info exists ::chanfile]} { set filename $::chanfile } else { set filename {} }
	if {[info exists ::chanfile-perm]} { set perm ${::chanfile-perm} } else { set perm ${::database-perm} }
	database channels save -file $filename -permissions $perm
}

# Loads the channel info from $chanfile:
proc ::tcldrop::channels::loadchannels {} {
	if {[info exists ::chanfile]} { set filename $::chanfile } else { set filename {} }
	if {![catch { database channels reload -file $filename }]} {
		putlog "loading channel database..."
		# FixMe: This shouldn't just dump the file into the Channels array...
		#        It needs to check each channel for udefs that
		#        are no longer in use, and discard them.
	} elseif {![catch { database channels create }]} {
		putlog "no channel database exists..yet"
	}
	SetUdefDefaults
}

# Returns 1 if a channel exists in the channel database, or 0 if it doesn't:
proc ::tcldrop::channels::validchan {channel} { dict exists $::database(channels) [string tolower $channel] }

# Note, types for udef's should be: flag, int, str, and list.
# In the case of lists, the channel command should provide lappend, lreplace, and lremove commands.

proc ::tcldrop::channels::callchannel {command channel args} {
	foreach {type flags mask proc} [bindlist channel] {
		if {[string match -nocase $mask "$command $channel [join $args]"]} {
			countbind $type $mask $proc
			if {[catch { $proc $command $channel $args } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
		}
	}
}

# Defines a new udef:
proc ::tcldrop::channels::setudef {type name {default {}}} {
	# Store the default for this udef:
	variable UdefDefaults
	set name [string tolower $name]
	switch -- $type {
		{flag} {
			switch -- $default {
				{1} - {+} - {y} - {Y} { set UdefDefaults($name) 1 }
				{0} - {-} - {n} - {N} - {default} { set UdefDefaults($name) 0 }
			}
		}
		{int} { if {$default != {}} { set UdefDefaults($name) $default } else { set UdefDefaults($name) 0 } }
		{str} - {list} { set UdefDefaults($name) $default }
		{default} { return -code error "Invalid udef type: $type" }
	}
	# Store the udef itself:
	variable Udefs
	set Udefs($name) $type
	# Apply the default to all channels that don't already have it set:
	SetUdefDefaults $name
}

#  renudef <flag/int> <oldname> <newname>
#    Description: renames a user defined channel flag or integer setting.
#    Returns: nothing
#    Module: channels
proc ::tcldrop::channels::renudef {type oldname newname} {
	variable Udefs
	if {[info exists Udefs($oldname)] && [string equal -nocase $Udefs($oldname) $type]} {
		dict for {key value} $::database(channels) {
			if {[dict exists $value $oldname]} {
				# FixMe: The database module doesn't support this, yet:
				database channels rename $key $oldname $key $newname
			}
		}
		set Udefs($newname) $Udefs($oldname)
		unset Udefs($oldname)
		variable UdefDefaults
		set UdefDefaults($newname) $UdefDefaults($oldname)
		unset Udefs($oldname)
		return 1
	}
	return 0
}

#  deludef <flag/int> <name>
#    Description: deletes a user defined channel flag or integer setting.
#    Returns: nothing
#    Module: channels
# Proc written by Papillon@EFNet.
# FixMe: This proc is untested and unmodified from what he sent me.
proc ::tcldrop::channels::deludef {type name} {
	variable Udefs
	if {[info exists Udefs($oldname)] && [string equal -nocase $Udefs($oldname) $type]} {
		dict for {key value} $::database(channels) { if {[dict exists $value $oldname]} { database channels unset $key $oldname } }
		unset Udefs($oldname)
		variable UdefDefaults
		unset Udefs($oldname)
		return 1
	}
	return 0
}

# Returns 1 if it's a valid (existing) name for a udef, or 0 if it's not:
proc ::tcldrop::channels::validudef {name} {
	variable Udefs
	info exists Udefs($name)
}

#  isdynamic <channel>
#    Returns: 1 if the channel is a dynamic channel; 0 otherwise
#    Module: channels
proc ::tcldrop::channels::isdynamic {channel} { return 1 }

# FixMe: Add these:
# channame2dname <channel-name>
# chandname2name <channel-dname>

proc ::tcldrop::channels::SetUdefDefaults {{name {*}}} {
	variable UdefDefaults
	foreach udef [array names UdefDefaults $name] {
		#dict for {key value} $::database(channels) {
		#	if {![dict exists $value $udef]} {
		#		database channels set $key $value $udef $UdefDefaults($udef)
		#	}
		#}
		foreach channel [channels] {
			if {[catch { channel get $channel $udef }]} {
				channel set $channel $udef $UdefDefaults($udef)
			}
		}
	}
}

# Gives the type of the udef given in $name:
# It returns one of the following: int, flag, str, list, or unknown.
proc ::tcldrop::channels::UdefType {name} {
	variable Udefs
	if {[info exists Udefs($name)]} { return $Udefs($name) } else { return {unknown} }
}

proc ::tcldrop::channels::newchanbei {bei channel mask creator comment {lifetime {-1}} {options {}}} {
	switch -- $lifetime {
		{-1} {
			if {[validchan $channel]} {
				set lifetime [channel get $channel ${bei}-time]
			} elseif {[info exists ::global-${bei}-time]} {
				set lifetime [set ::global-${bei}-time]
			} elseif {[info exists ::${bei}-time]} {
				set lifetime [set ::${bei}-time]
			} else {
				set lifetime 4
			}
			set expires [expr { [clock seconds] + $lifetime }]
		}
		{0} { set expires 0 }
		{default} { set expires [expr { [clock seconds] + $lifetime }] }
	}
	if {[lsearch -exact $options {sticky}] != -1} { set sticky 1 } else { set sticky 0 }
	database ${bei}s set [string tolower $channel] [string tolower $mask] [dict create channel $channel mask $mask creator $creator comment $comment lifetime $lifetime expires $expires created [clock seconds] lastactive 0 sticky $sticky options $options]
}

proc ::tcldrop::channels::newbei {bei mask creator comment {lifetime {-1}} {options {}}} {
	newchanbei $bei - $mask $creator $comment $lifetime $options
}

proc ::tcldrop::channels::stickbei {bei mask {channel {-}}} {
	if {[dict exists $::database(${bei}s) [string tolower $channel] [string tolower $mask]]} {
		if {[catch { database ${bei}s set [string tolower $channel] [string tolower $mask] sticky 1 }]} {
			return 0
		} else {
			return 1
		}
	} else {
		return 0
	}
}

proc ::tcldrop::channels::unstickbei {bei mask {channel {-}}} {
	if {[dict exists $::database(${bei}s) [string tolower $channel] [string tolower $mask]]} {
		if {[catch { database ${bei}s set [string tolower $channel] [string tolower $mask] sticky 0 }]} {
			return 0
		} else {
			return 1
		}
	} else {
		return 0
	}
}

proc ::tcldrop::channels::killchanbei {bei channel mask} {
	if {[dict exists $::database(${bei}s) [string tolower $channel] [string tolower $mask]]} {
		if {[catch { database ${bei}s unset [string tolower $channel] [string tolower $mask] }]} {
			return 0
		} else {
			return 1
		}
	} else {
		return 0
	}
}

proc ::tcldrop::channels::killbei {bei mask {channel {-}}} {
	killchanbei $bei $channel $mask
}

proc ::tcldrop::channels::isbei {bei mask {channel {-}}} {
	dict exists $::database(${bei}s) [string tolower $channel] [string tolower $mask]
}

proc ::tcldrop::channels::ischanbei {bei mask {channel {-}}} {
	isbei $bei $mask $channel
}

proc ::tcldrop::channels::ispermbei {bei mask {channel {-}}} {
	if {[dict exists $::database(${bei}s)s [string tolower $channel] [string tolower $mask] lifetime]} {
		if {[dict get $::database(${bei}s) [string tolower $channel] [string tolower $mask] lifetime] == {0}} {
			return 1
		} else {
			return 0
		}
	} else {
		return -code error "No such ${bei} $channel $mask"
	}
}

proc ::tcldrop::channels::isbeisticky {bei mask {channel {-}}} {
	if {[dict exists $::database(${bei}s) [string tolower $channel] [string tolower $mask] sticky]} {
		dict get $::database(${bei}s) [string tolower $channel] [string tolower $mask] sticky
	} else {
		return -code error "No such ${bei} $channel $mask"
	}
}

proc ::tcldrop::channels::matchbei {bei nuhost {channel {-}}} {
	if {$channel != {-}} {
		if {[dict exists $::database(${bei}s) [string tolower $channel]]} {
			# Note: $key is the mask and $value (a sub-dict) is all the info known about it.
			dict for {key value} [dict get $::database(${bei}s) [string tolower $channel]] {
				if {[string match -nocase [dict get $value mask] $nuhost]} { return 1 }
			}
		}
	}
	if {[dict exists $::database(${bei}s) -]} {
		dict for {key value} [dict get $::database(${bei}s) -] {
			if {[string match -nocase [dict get $value mask] $nuhost]} { return 1 }
		}
	}
	return 0
}

proc ::tcldrop::channels::beilist {bei {channel {-}} {nuhost {}}} {
	set list [list]
	if {[dict exists $::database(${bei}s) [string tolower $channel]]} {
		# Note: $key is the mask and $value (a sub-dict) is all the info known about it.
		dict for {key value} [dict get $::database(${bei}s) [string tolower $channel]] {
			if {$nuhost == {} || [string match -nocase [dict get $value mask] $nuhost]} {
				lappend list [list [dict get $value mask] [dict get $value comment] [dict get $value expires] [dict get $value created] [dict get $value lastactive] [dict get $value creator]]
			}
		}
	}
	return $list
}

proc ::tcldrop::channels::listbeis {bei {channel {-}} {nuhost {}}} {
	set list [list]
	if {[dict exists $::database(${bei}s) [string tolower $channel]]} {
		# Note: $key is the mask and $value (a sub-dict) is all the info known about it.
		dict for {key value} [dict get $::database(${bei}s) [string tolower $channel]] {
			if {$nuhost == {} || [string match -nocase [dict get $value mask] $nuhost]} {
				lappend list $value
			}
		}
	}
	return $list
}

# Saves the bei data to the ${bei}file:
proc ::tcldrop::channels::savebeis {{bei {}}} {
	if {[info exists ::${bei}file]} { set filename [set ::${bei}file] } else { set filename {} }
	if {[info exists ::${bei}file-perm]} { set perm [set ::${bei}file-perm] } else { set perm ${::database-perm}  }
	database ${bei}s save -file $filename -permissions $perm
}

# Loads the bei data from ${bei}file:
proc ::tcldrop::channels::loadbeis {{bei {}}} {
	if {[info exists ::${bei}file]} { set filename [set ::${bei}file] } else { set filename {} }
	if {![catch { database ${bei}s reload -file $filename }]} {
		putlog "loading $bei database..."
	} elseif {![catch { database ${bei}s create }]} {
		putlog "no $bei database file exists..yet."
	} else {
		putlog "error creating $bei database!"
	}
}

proc ::tcldrop::channels::newchanban {channel ban creator comment {lifetime {-1}} {options {}}} {
	newchanbei ban $channel $ban $creator $comment $lifetime $options
}

proc ::tcldrop::channels::newban {ban creator comment {lifetime {-1}} {options {}}} {
	newbei ban $ban $creator $comment $lifetime $options
}

proc ::tcldrop::channels::stickban {banmask {channel {-}}} {
	stickbei ban $banmask $channel
}

proc ::tcldrop::channels::stick {banmask {channel {-}}} {
	stickbei ban $banmask $channel
}

proc ::tcldrop::channels::unstickban {banmask {channel {-}}} {
	unstickbei ban $banmask $channel
}

proc ::tcldrop::channels::unstick {banmask {channel {-}}} {
	unstickbei ban $banmask $channel
}

proc ::tcldrop::channels::killchanban {channel ban} {
	killchanbei ban $channel $ban
}

proc ::tcldrop::channels::killban {ban} {
	killbei ban $ban
}

proc ::tcldrop::channels::isban {ban {channel {-}}} {
	isbei ban $ban $channel
}

proc ::tcldrop::channels::ispermban {ban {channel {-}}} {
	ispermbei ban $ban $channel
}

proc ::tcldrop::channels::isbansticky {ban {channel {-}}} {
	isbeisticky ban $ban $channel
}

proc ::tcldrop::channels::matchban {nuhost {channel {-}}} {
	matchbei ban $nuhost $channel
}

proc ::tcldrop::channels::banlist {{channel {-}} {nuhost {}}} {
	beilist ban $channel $nuhost
}

proc ::tcldrop::channels::listbans {{channel {-}} {nuhost {}}} {
	listbeis ban $channel $nuhost
}



proc ::tcldrop::channels::newchanexempt {channel exempt creator comment {lifetime {-1}} {options {}}} {
	newchanbei exempt $channel $exempt $creator $comment $lifetime $options
}

proc ::tcldrop::channels::newexempt {exempt creator comment {lifetime {-1}} {options {}}} {
	newbei exempt $exempt $creator $comment $lifetime $options
}

proc ::tcldrop::channels::stickexempt {exemptmask {channel {-}}} {
	stickbei exempt $exemptmask $channel
}

proc ::tcldrop::channels::unstickexempt {exemptmask {channel {-}}} {
	unstickbei exempt $exemptmask $channel
}

proc ::tcldrop::channels::killchanexempt {channel exempt} {
	killchanbei exempt $channel $exempt
}

proc ::tcldrop::channels::killexempt {exempt} {
	killbei exempt $exempt
}

proc ::tcldrop::channels::isexempt {exempt {channel {-}}} {
	isbei exempt $exempt $channel
}

proc ::tcldrop::channels::ispermexempt {exempt {channel {-}}} {
	ispermbei exempt $exempt $channel
}

proc ::tcldrop::channels::isexemptsticky {exempt {channel {-}}} {
	isbeisticky exempt $exempt $channel
}

proc ::tcldrop::channels::matchexempt {nuhost {channel {-}}} {
	matchbei exempt $nuhost $channel
}

proc ::tcldrop::channels::exemptlist {{channel {-}} {nuhost {}}} {
	beilist exempt $channel $nuhost
}

proc ::tcldrop::channels::listexempts {{channel {-}} {nuhost {}}} {
	listbeis exempt $channel $nuhost
}


proc ::tcldrop::channels::newchaninvite {channel invite creator comment {lifetime {-1}} {options {}}} {
	newchanbei invite $channel $invite $creator $comment $lifetime $options
}

proc ::tcldrop::channels::newinvite {invite creator comment {lifetime {-1}} {options {}}} {
	newbei invite $invite $creator $comment $lifetime $options
}

proc ::tcldrop::channels::stickinvite {invitemask {channel {-}}} {
	stickbei invite $invitemask $channel
}

proc ::tcldrop::channels::unstickinvite {invitemask {channel {-}}} {
	unstickbei invite $invitemask $channel
}

proc ::tcldrop::channels::killchaninvite {channel invite} {
	killchanbei invite $channel $invite
}

proc ::tcldrop::channels::killinvite {invite} {
	killbei invite $invite
}

proc ::tcldrop::channels::isinvite {invite {channel {-}}} {
	isbei invite $invite $channel
}

proc ::tcldrop::channels::isperminvite {invite {channel {-}}} {
	ispermbei invite $invite $channel
}

proc ::tcldrop::channels::isinvitesticky {invite {channel {-}}} {
	isbeisticky invite $invite $channel
}

proc ::tcldrop::channels::matchinvite {nuhost {channel {-}}} {
	matchbei invite $nuhost $channel
}

proc ::tcldrop::channels::invitelist {{channel {-}} {nuhost {}}} {
	beilist invite $channel $nuhost
}

proc ::tcldrop::channels::listinvites {{channel {-}} {nuhost {}}} {
	listbeis invite $channel $nuhost
}


# Note: I realize that ignores on Eggdrop aren't channel specific,
# but because the commands used to interface with them are so much like
# the ban/exempt/invite commands, I thought that here would be the most
# appopriate place for the ignore support.
# And since we're putting it here, we might as well make it
# support channel-specific ignores. =)
proc ::tcldrop::channels::newchanignore {channel ignore creator comment {lifetime {-1}} {options {}}} {
	newchanbei ignore $channel $ignore $creator $comment $lifetime $options
}

proc ::tcldrop::channels::newignore {ignore creator comment {lifetime {-1}} {options {}}} {
	newbei ignore $ignore $creator $comment $lifetime $options
}

proc ::tcldrop::channels::killchanignore {channel ignore} {
	killchanbei ignore $channel $ignore
}

proc ::tcldrop::channels::killignore {ignore} {
	killbei ignore $ignore
}

proc ::tcldrop::channels::isignore {ignore {channel {-}}} {
	isbei ignore $ignore $channel
}

proc ::tcldrop::channels::ispermignore {ignore {channel {-}}} {
	ispermbei ignore $ignore $channel
}

proc ::tcldrop::channels::isignoresticky {ignore {channel {-}}} {
	isbeisticky ignore $ignore $channel
}

proc ::tcldrop::channels::matchignore {nuhost {channel {-}}} {
	matchbei ignore $nuhost $channel
}

proc ::tcldrop::channels::ignorelist {{channel {-}} {nuhost {}}} {
	beilist ignore $channel $nuhost
}

proc ::tcldrop::channels::listignores {{channel {-}} {nuhost {}}} {
	listbeis ignore $channel $nuhost
}

proc ::tcldrop::channels::Save {type} {
	savebeis ignore
	savechannels
	savebeis ban
	savebeis exempt
	savebeis invite
}

proc ::tcldrop::channels::UNLD {module} {
	# FixMe: Add these unload commands:
	#unloadchannels
	#unloadbeis ignore
	#unloadbeis ban
	#unloadbeis exempt
	#unloadbeis invite
	unloadhelp channels.help
	unloadhelp [file join set channels.help]
	unloadhelp chaninfo.help
	unloadmodule channels::dcc
	return 0
}

proc ::tcldrop::channels::LOAD {module} {
	# Initialize variables:
	variable Udefs
	array set Udefs {}
	# Set the internal defaults:
	setdefault force-expire 0
	setdefault share-greet 0
	setdefault use-info 1
	setdefault global-chanmode {}
	setdefault global-idle-kick 0
	setdefault global-aop-delay [fuzz 1]:[fuzz 9]
	setdefault global-flood-chan [fuzz 9]:[fuzz 2]
	setdefault global-flood-deop 99:[fuzz 1]
	setdefault global-flood-kick 99:[fuzz 1]
	setdefault global-flood-join 99:[fuzz 2]
	setdefault global-flood-ctcp 4:[fuzz 9]
	setdefault global-flood-nick 9:[fuzz 2]
	setdefault global-stopnethack-mode 0
	setdefault global-revenge-mode 0
	setdefault global-ban-time [fuzz 99]
	setdefault global-exempt-time [fuzz 98]
	setdefault global-invite-time [fuzz 97]
	setdefault global-chanset [list -autoop -autovoice -bitch -cycle +dontkickops +dynamicbans +dynamicexempts +dynamicinvites -enforcebans -greet -inactive -nodesynch -protectfriends -protectops -revenge -revengebot -secret -seen +shared -statuslog +userbans +userexempts +userinvites -protecthalfops -autohalfop]
	loadchannels
	bind evnt - save ::tcldrop::channels::Save -priority 3
	bind evnt - hourly-updates ::tcldrop::channels::Save -priority 4
	bind evnt - die ::tcldrop::channels::Save -priority 0
	bind evnt - save ::tcldrop::channels::Save -priority 0
	# Note, these settings are defined here, but their actual functions are in the irc module (or possibly other modules):
	setudef str chanmode ${::global-chanmode}
	setudef int idle-kick ${::global-idle-kick}
	setudef int stopnethack-mode ${::global-stopnethack-mode}
	setudef int revenge-mode ${::global-revenge-mode}
	setudef int ban-time ${::global-ban-time}
	setudef int exempt-time ${::global-exempt-time}
	setudef int invite-time ${::global-invite-time}
	setudef int aop-delay [split ${::global-aop-delay} {:}]
	setudef int flood-chan [split ${::global-flood-chan} {:}]
	setudef int flood-deop [split ${::global-flood-deop} {:}]
	setudef int flood-join [split ${::global-flood-join} {:}]
	setudef int flood-nick [split ${::global-flood-nick} {:}]
	setudef int flood-kick [split ${::global-flood-kick} {:}]
	setudef int flood-ctcp [split ${::global-flood-ctcp} {:}]
	setudef str need-op {}
	setudef str need-unban {}
	setudef str need-invite {}
	setudef str need-key {}
	setudef str need-limit {}
	setudef str need-halfop {}
	setudef str need-voice {}
	# Note/FixMe: global-chanset should be a list, see if it's a list in Eggdrop.
	foreach n ${::global-chanset} { if {$n != {}} { catch { setudef flag [string range $n 1 end] [string index $n 0] } } }
	loadbeis ignore
	loadbeis ban
	loadbeis exempt
	loadbeis invite
	loadhelp channels.help
	loadhelp [file join set channels.help]
	loadhelp chaninfo.help
	bind unld - channels ::tcldrop::channels::UNLD -priority 100
	checkmodule channels::dcc
}
bind load - channels ::tcldrop::channels::LOAD -priority 0
