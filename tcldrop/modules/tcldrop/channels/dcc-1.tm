# channels/dcc.tcl --
#	Handles:
#		* Channel related DCC commands.
#	Depends: channels.
#
# $Id: dcc.tcl,v 1.5 2006/05/19 04:39:40 fireegl Exp $
#
# Copyright (C) 2005,2006 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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

namespace eval ::tcldrop::channels::dcc {
	variable version {0.1}
	variable script [info script]
	variable name {channels::dcc}
	variable depends {channels core::dcc core}
	variable author {Tcldrop-Dev}
	variable description {Channel related DCC commands.}
	variable rcsid {$Id: dcc.tcl,v 1.5 2006/05/19 04:39:40 fireegl Exp $}
	variable commands [list]
	# Provide the channels::dcc module:
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Pre-depends on the channels module:
	checkmodule channels
}

proc ::tcldrop::channels::dcc::+CHAN {handle idx text} {
	if {$text == {}} {
		putdcc $idx "Usage: +chan \[#&!+\]<channel> \[options\]"
		return 0
	}
	set channel [slindex $text 0]
	set options [slrange $text 1 end]
	channel add $channel
	if {$options != {}} {
		foreach o $options {
			if {[catch {channel set $channel $o} error]} {
				putdcc $idx "Invalid channel or channel options."
				break
			}
		}
	}
	putcmdlog "#$handle# +chan $channel"
	return 0
}

proc ::tcldrop::channels::dcc::-CHAN {handle idx text} {
	if {$text == {}} {
		putdcc $idx "Usage: -chan \[#&!+\]<channel>"
		return 0
	}
	set channel [slindex $text 0]
	if {[catch {channel remove $channel} error] && $error == "no such channel record: $channel"} {
		putdcc $idx "That channel doesn't exist!"
		return 0
	}
	putdcc $idx "Channel $channel removed from the bot."
	putdcc $idx "This includes any channel specific bans, invites, exemptions and user records that you set."
	putcmdlog "#$handle# -chan $channel"
	return 0
}


bind load - channels::dcc ::tcldrop::channels::dcc::LOAD -priority 0
proc ::tcldrop::channels::dcc::LOAD {module} {
	bind dcc n +chan ::tcldrop::channels::dcc::+CHAN -priority 1000
	bind dcc n -chan ::tcldrop::channels::dcc::-CHAN -priority 1000
	bind unld - channels::dcc ::tcldrop::channels::dcc::UNLD -priority 0
}

proc ::tcldrop::channels::dcc::UNLD {module} {
	unbind dcc * * ::tcldrop::channels::dcc::*
	return 0
}

# The following are RacBot binds, but should be added to Tcldrop I think:
# Taken from: http://www.racbot.org/docs/tclbinds/miscellaneous_bindings.html

# + CHANNEL SETTING DISPLAY
# Module Required: racchannel
# Stackable: No
# Usage: "bind csdp - <mask> <procedure-name>"
# Variables: $channel $idx $language
# Description:
# This binding is triggered when the DCC command "chaninfo" is
# executed and a Channel Setting that was created using the Tcl
# command "addchanset" requires displaying. <Mask> is the Channel
# Setting name. Wildcards are not allowed. User flags should not be
# specified - they are ignored. $channel is the channel the setting
# is for, $idx is the user's DCC index, and $language is the user's
# language.

# + CHANNEL SETTING CHANGE
# Module Required: racchannel
# Stackable: Yes
# Usage: "bind csch - <mask> <procedure-name>"
# Variables: $channel $idx $setting $language
# Description:
# This binding is triggered when the DCC command "chanset" or the Tcl
# command "channel set" is executed and the Channel Setting was created
# using the Tcl command "addchanset". It is used to do additional
# changes when a Channel setting is changed. (There is no need to set
# the actual setting using this bind unless a translation is required).
# <Mask> is the Channel Setting name. Wildcards are not allowed.
# User flags should not be specified - they are ignored. $channel is
# the channel the setting is for, $idx is the user's DCC index,
# $setting is the new value for the channel setting, and $language is
# the user's language.


