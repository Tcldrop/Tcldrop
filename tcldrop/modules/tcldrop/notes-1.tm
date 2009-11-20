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

proc ::tcldrop::notes::Save {type} {
	do savenotes
}
proc ::tcldrop::notes::pattern {string} {
	regsub -all -- {(\[|\]|\\)} $text {\\\1}
}
proc ::tcldrop::notes::boolean {data} {
	if {($data eq "true") || ($data == 1)} {
		return 1
	} else {
		return 0
	}
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
			# Returns: string with whatever is in database, nothing if it's empty
			set recipient [lindex $args 0]
			if {$recipient eq ""} {
				return -code error "wrong # args: should be \"do $command <user>\""
			}
			if {![validuser $recipient]} {
				return -code error "$recipient is not a valid user."
			}
			if {[database notes exists $recipient]} {
				return [lindex [lsort -increasing -index 1 [database notes get $recipient]] 0]
			}
		}
		{listnum} {
			# Description: This returns how much messages an user has received
			set recipient [lindex $args 0]
			if {$recipient eq ""} {
				return -code error "wrong # args: should be \"do $command <user>\""
			}			
			if {![validuser $recipient]} {
				return -code error "wrong # args: should be \"do $command <user>\""
			} else {
				if {![database notes exists $recipient]} {
					return 0
				} else {
					return [llength [database notes get $recipient]]
				}
			}
		}
		{send} {
			# Description: This stores a mesage for the given recipient
			# Usage: do send <recipient> <sender> <message>
			# Returns: the queue position (0 or higher) for the recipient or -1 if can't store it
			set recipient [lindex $args 0]
			set sender [lindex $args 1]
			set message [join [lrange $args 2 end]]
			if {![validuser $recipient]} {
				return -code error "wrong # args: should be \"do $command <recipient> <sender> <message>\" - the recipient and the sender must be a valid username."
			}
			if {(![validuser $sender]) && (![boolean $::fakesender])} {
				return -code error "the sender must be a valid username due to admin configuration."
			}
			if {($recipient eq $sender) && (![boolean $::selfnotes])} {
				return -code error "you can't send notes to yourself due to admin configuration."
			}
			if {$message eq ""} {
				return -code error "wrong # args: should be \"do $command <recipient> <sender> <message>\" - specify a text to send."
			}
			if {![database notes exists $recipient]} {
				database notes set $recipient [list [list $sender [unixtime] $message]]
			} else {
				set data [lsearch -all -inline -exact -index 0 [database notes get $recipient] $sender]
				if {[lsearch -exact -index 2 $data $message] != -1} {
					return -1
				} else {
					database notes lappend $recipient [list $sender [unixtime] $message]
			
				}
			}
			return [llength [database notes get $recipient]]
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
		{} - {default} {
			# FixMe: is this next line smart enough? I don't like it at all
			return -code error "unknown or ambiguous subcommand \"$command\": must be receive, listnum, erase, list, loadnotes, savenotes, or expire"
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
	# Allow (1) fake senders? this may be useful for 3rd-party script notifications
	setdefault fakesender 1
	# Do we allow send notes to themselves? (tribute to those people that has no memory)
	setdefault selfnotes 1
	loadnotes
	bind evnt - save ::tcldrop::notes::Save -priority 3
	bind evnt - hourly-updates ::tcldrop::notes::Save -priority 4
	bind evnt - die ::tcldrop::notes::Save -priority 0	
}
bind load - notes ::tcldrop::notes::LOAD -priority 0
