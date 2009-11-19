# notes.tcl --
#	Provides:
#		* notes support.
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
# The author of this project can be reached at Tcldrop-Dev@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.

namespace eval ::tcldrop::notes {
	variable version {1}
	variable script [info script]
	variable name {notes}
	variable depends {core::database core}
	variable author {Tcldrop-Dev}
	variable description {Provides notes support.}
	variable rcsid {$Id$}
	variable commands [list loadnotes savenotes]
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	# Export all the commands that should be available to 3rd-party scripters:
	eval namespace export $commands
}

# Preserving intuitive commands
# Note: or is it better to remove the redundant suffix notes?
proc ::tcldrop::notes::loadnotes {} {
	do loadnotes
}

proc ::tcldrop::notes::Save {} {
	do savenotes
}

proc ::tcldrop::notes::do {command args} {
	
	# Keeping the format of original eggdrop module notes (no need import command then)
	# Recipient Sender Unixtime Message / Example:
	# Sentencia Lamestbot 1232881957 Welcome to eggdrop! :)
	# TODO: my goal is make this module similar to pop3. Mark as read, unread, preserve a history...
	set options [list -from -to -message]
	#foreach {option value} $args {
	#	if {[lsearch -exact $options $option] == -1} {
	#		# stop or continue?
	#		return -code error "$option is not a valid option"
	#		# continue
	#	}
	#	set opt([string trimleft $option {-}]) $value
	#}
	
	switch -- $command {
		{receive} {
			# Description: This shows the first message for the given recipient if exists.
			# Usage: do receive <recipient>
			# Returns: string with whatever is in database if there's something to return
			set recipient [lindex $args 0]
			if {![validuser $recipient]} {
				return -code error "wrong # args: should be \"do $command <user>\""
			}
			#set get [database notes get $recipient]
		}
		{listnum} {
			# Description: This returns how much messages an user has received
			set recipient [lindex $args 0]
			if {![validuser $recipient]} {
				return -code error "wrong # args: should be \"do $command <user>\""
			} else {
				return [llength [database notes keys $recipient]
			}
		}
		{send} {
			# Description: This stores a mesage for the given recipient
			# Usage: do send <from> <to> <message>
			# Returns: the queuesize for the recipient or -1 if can't store it
			set from [lindex $args 0]
			set to [lindex $args 0]
			set message [lindex $args 2]
			if {(![validuser $from]) || (![validuser $to])} {
				return -code error "wrong # args: should be \"do $command <from> <to> <message>\" - the recipient and the sender should be a valid username."
			}
			if {$message eq ""} {
				return -code error "wrong # args: should be \"do $command <from> <to> <message>\" - specify a text to send."
			}
			
		}
		{erase} {
			
		}
		{list} {}
		{loadnotes} {
			# Description: This loads the notes database, bassed on channels module
			# so I'm sure it will work
			if {[info exists ::notefile]} { set filename $::notefile } else { set filename {} }
			if {![catch { database notes reload -file $filename }]} {
				putlog "loading notes database..."
			} elseif {![catch { database notes create }]} {
				putlog "no notes database exists... yet"
			}			
		}
		{savenotes} {
			if {[info exists ::notefile]} { set filename $::notefile } else { set filename {} }
			database notes save -file $filename 
		}
		expire {}
		{default} - {} {
			# FixMe: is this next line smart enough? I don't like it at all
			return -code error "Valid subcommands are: read, send, erase, list, loadnotes, expire"
		}
	}
}


proc ::tcldrop::notes::LOAD {module} {
	
	global nick
	# Set the internal defaults:
	setdefault notefile "${nick}.notes"
	setdefault max-notes 50
	setdefault note-life 60
	setdefault allow-pwd 0
	setdefault notify-users 1
	setdefault notify-onjoin 1
	# Let the sender know when the recepicient read the message
	# setdefault read-time 0
	loadnotes
	bind evnt - save ::tcldrop::notes::Save -priority 3
	bind evnt - hourly-updates ::tcldrop::notes::Save -priority 4
	bind evnt - die ::tcldrop::notes::Save -priority 0	
}
bind load - notes ::tcldrop::notes::LOAD -priority 0
