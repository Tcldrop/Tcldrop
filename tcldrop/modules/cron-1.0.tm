# cron -- A cron-like scheduler for scripts.

# Copyright (C) 2010 Tcldrop Development Team <Tcldrop-Dev>

# $Id$

# Usage:
#
# cron valid $cron
# 	Returns 1 if the cron format is valid, 0 otherwise.
# cron parse $cron
# 	Takes a standard cron time format and returns a dict containing all the possible minutes/hours/days/months/weekdays.
# cron match $dict
# 	Takes the dict returned from parse and returns 1 if the current time/date matches, 0 otherwise.
# crontab add $cron $command ...
# 	Add a new crontab, where $cron is a standard cron time format, and $command is the command to run when there's a match.  Returns an ID for the new crontab.
# crontab remove $id
# 	Removes a crontab by ID. 
# crond start
# 	Starts the 1-minute loop necessary for running the crontab commands.
# 	Note: It's started automatically when a crontab is added.
# crond stop
# 	Stops the 1-minute loop.
# 	Note: It's stopped automatically when all the crontabs are removed.

# For the cron syntax, see "man 5 crontab".

namespace eval ::cron {
	package provide cron 1.0
	variable IDCount
	if {![info exists IDCount]} { variable IDCount 0 }
	variable TimerID
	if {![info exists TimerID]} { variable TimerID {} }
	namespace export cron crontab crond
	namespace ensemble create -command cron -subcommands [list match valid parse add remove start stop]
	namespace ensemble create -command crontab -subcommands [list add remove]
	namespace ensemble create -command crond -subcommands [list start stop]
}

# parses $cron and returns a dict containing all the minutes/hours/days/months/weekdays specified by $cron
# Example:
# input: 50-60/2 */4,21 09/2 7-9,december 7-4
# result: minutes {0 50 52 54 56 58} hours {0 4 8 12 16 20 21} days {9 11 13 15 17 19 21 23 25 27 29 31} months {7 8 9 12} weekdays {0 1 2 3 4}
proc ::cron::parse {cron} {
	# Allow special strings:
	switch -- $cron {
		{@hourly} { set cron {0 * * * *} }
		{@midnight} { set cron {0 0 * * *} }
		{@daily} { set cron {0 1 * * *} }
		{@weekly} { set cron {0 0 * * 0} }
		{@monthly} { set cron {0 0 1 * *} }
		{@yearly} - {@annually} { set cron {0 0 1 1 *} }
		{@reboot} { return -code error {@reboot not supported.} }
	}
	foreach info {{minutes 0 59} {hours 0 23} {days 1 31} {months 1 12} {weekdays 0 6}} element $cron {
		lassign $info name min max
		dict set times $name [list]
		# Calls parse_element to turn the cron-syntax into one we can more generically deal with:
		foreach element [parse_element $element $name] {
			lassign $element skip start end
			if {$end eq {}} {
				# Set the end to the highest allowable.
				set end $max
				# Use [scan] to remove any leading 0's (zeros):
			} elseif {![scan $end {%d} end]} {
				# parse_element should've complained about this, but just in case:
				return -code error "\"$end\" is invalid.  (Only decimal integer values are allowed.)"
			} elseif {$end < $min || $end > ($max + 1)} {
				return -code error "$end is outside the allowed range for ${name}: $min-$max"
			}
			if {$start eq {}} {
				# Set the start to the lowest allowable.
				set start $min
				# Use [scan] to remove any leading 0's (zeros):
			} elseif {![scan $start {%d} start]} {
				# parse_element should've complained about this, but just in case:
				return -code error "\"$start\" is invalid.  (Only decimal integer values are allowed.)"
			} elseif {$start < $min || $start > ($max + 1)} {
				return -code error "$start is outside the allowed range for ${name}: $min-$max"
			} elseif {$start == ($max + 1)} {
				# If, for example, the start (and possibly the end) is 7 for the weekday, we'll make it/them 0:
				set start $min
				if {$end == ($max + 1)} { set end $min }
				# Note: Specifying 0-7 will still mean every day of the week.
			}
			# Generate the list of possible values:
			set count $start
			while {$count <= $end} {
				if {$count == ($max + 1)} {
					# If we landed on $max + 1, we set it as $min and break the loop.
					# This makes 60 become 0 for minutes, 24 becomes 0 for hours, 32 becomes 1 for days, 13 becomes 1 for months, and 7 becomes 0 for weekdays..
					dict lappend times $name $min
					break
				}
				dict lappend times $name $count
				incr count $skip
			}
		}
		# Remove duplicates:
		dict set times $name [lsort -unique -integer [dict get $times $name]]
	}
	return $times
}

# Used by [parse]
# Parses a single cron element, returning {{skip start end} ...}
# skip = the skip/step value (defaults to 1)
# start = the start of the range (may not be specified)
# end = the end of the range (may not be specified)
proc ::cron::parse_element {element {name {}}} {
	set retval [list]
	foreach element [split $element {,;:|&}] {
		if {$element eq {*}} {
			# Example: *
			lappend retval [list 1]
		} elseif {[string is digit -strict $element]} {
			# Example: 9
			lappend retval [list 1 $element $element]
		} elseif {[regexp -- {^(\d+)\-(\d+)$} $element -> from to]} {
			# Example: 3-9
			lappend retval [list 1 $from $to]
		} elseif {[regexp -- {^\*/(\d+)$} $element -> div] && $div >= 1} {
			# Example: */2
			lappend retval [list $div]
		} elseif {[regexp -- {^(\d+)\-(\d+)/(\d*)$} $element -> from to div]} {
			# Example: 3-9/2
			lappend retval [list $div $from $to]
		} elseif {[regexp -- {^(\d+)/(\d*)$} $element -> from div] && $div >= 1} {
			# Example: 9/2
			lappend retval [list $div $from]
		} elseif {[string trim $element] ne {}} {
			# Ignore ""
			# Try to deal with names for days/months (ranges and skip/step are not supported in this case):
			switch -- $name {
				{days} {
					switch -nocase -- $element {
						{sun} - {sunday} { set element 0 }
						{mon} - {monday} { set element 1 }
						{tue} - {tuesday} { set element 2 }
						{wed} - {wednesday} { set element 3 }
						{thu} - {thursday} { set element 4 }
						{fri} - {friday} { set element 5 }
						{sat} - {saturday} { set element 6 }
						{default} { return -code error "Failed to process: $element" }
					}
				}
				{months} {
					switch -nocase -- $element {
						{jan} - {january} { set element 1 }
						{feb} - {february} { set element 2 }
						{mar} - {march} { set element 3 }
						{apr} - {april} { set element 4 }
						{may} { set element 5 }
						{jun} - {june} { set element 6 }
						{jul} - {july} { set element 7 }
						{aug} - {august} { set element 8 }
						{sep} - {september} { set element 9 }
						{oct} - {october} { set element 10 }
						{nov} - {november} { set element 11 }
						{dec} - {december} { set element 12 }
						{default} { return -code error "Failed to process: $element" }
					}
				}
				{} - {default} { return -code error "Failed to process: $element" }
			}
			lappend retval [list 1 $element $element]
		}
	}
	# Return the lists of skips/ranges we built up, or return {1} which means a skip of 1 and the full range.
	if {[llength $retval]} { return $retval } else { list 1 }
}

# Takes the dict given by the output of [parse] and returns 1 if the current time/date is a match, else 0.
proc ::cron::match {dict} {
	lassign [clock format [clock seconds] -format {%M %k %e %N %w}] minute hour day month dayofweek
	if {[scan $minute {%d}] in [dict get $dict minutes] && $hour in [dict get $dict hours] && $month in [dict get $dict months] && ($dayofweek in [dict get $dict weekdays] || $day in [dict get $dict days])} { return 1 } else { return 0 }
}

# Checks to see if the supplied cron is valid:
proc ::cron::valid {cron} {
	# This RE consists of 5 instances of the same basic RE. Each instance has
	# "\d+" substituted for a RE that validates the number sequence associated
	# with that instance.
	#
	# Base Regex, explained:
	# Match any number of comma separated items if there are any:
	# (0*\d+|\*)(-0*\d+)?(/\d+)?,)*
	# Match the last item in this instance:
	# (0*\d+|\*)(-0*\d+)?(/\d+)?\s
	regexp -expanded -- {
		# minute (0-59) - ([0-9]|[1-5][0-9])
		^((0*([0-9]|[1-5][0-9])|\*)(-0*([0-9]|[1-5][0-9]))?(/\d+)?,)*
		(0*([0-9]|[1-5][0-9])|\*)(-0*([0-9]|[1-5][0-9]))?(/\d+)?\s
		# hour (0-23) - ([0-9]|1[0-9]|2[0-3])
		((0*([0-9]|1[0-9]|2[0-3])|\*)(-0*([0-9]|1[0-9]|2[0-3]))?(/\d+)?,)*
		(0*([0-9]|1[0-9]|2[0-3])|\*)(-0*([0-9]|1[0-9]|2[0-3]))?(/\d+)?\s
		# day of month (1-31) - ([1-9]|[12][0-9]3[01])
		((0*([1-9]|[12][0-9]3[01])|\*)(-0*([1-9]|[12][0-9]3[01]))?(/\d+)?,)*
		(0*([1-9]|[12][0-9]3[01])|\*)(-0*([1-9]|[12][0-9]3[01]))?(/\d+)?\s
		# month (1-12) - ([1-9]|1[0-2])
		((0*([1-9]|1[0-2])|\*)(-0*([1-9]|1[0-2]))?(/\d+)?,)*
		(0*([1-9]|1[0-2])|\*)(-0*([1-9]|1[0-2]))?(/\d+)?\s
		# day of week (0-7) - [0-7]
		((0*[0-7]|\*)(-0*[0-7])?(/\d+)?,)*
		(0*[0-7]|\*)(-0*[0-7])?(/\d+)?$
	} $cron
}

# Creates a new crontab which will run $args (the script) whenever the current time/date matches, 
# returns an identifier which can be used to remove it.
proc ::cron::add {cron args} {
	if {[llength $args]} {
		variable IDCount
		variable crontabs
		dict set crontabs [incr IDCount] [dict create cron [parse $cron] command $args]
		# Start the 1-minute loop:
		start
		return $IDCount
	}
}

# Removes a crontab:
proc ::cron::remove {id} {
	variable crontabs
	if {[dict exists $crontabs $id]} {
		dict unset crontabs $id
		if {[dict size $crontabs] == 0} {
			# Stop the 1-minute loop if there aren't any crontabs set.
			stop
		}
	}
}

# This is the 1-minute looping proc, it processes all the matching crontabs for this minute:
proc ::cron::DoCron {} {
	# Start another after timer to run this proc again at the start of the next minute + 1 second + 17ms to 126ms:
	variable TimerID [after [expr { 60000 - ([clock milliseconds] % 60000) + 1017 + int(rand() * 127) }] [namespace code DoCron]]
	# Run all the crontabs that match for this minute:
	variable crontabs
	set count 231
	dict for {id info} $crontabs {
		if {[match [dict get $info cron]]} {
			# Run this command 16 to 256ms from now..
			# (Trying to make each command in this batch run 16ms apart.)
			after [expr { 16 + ([incr count 16] % 247) }] [dict get $info command]
		}
	}
	# Notes and logic behind the weird expr's:
	# The extra 1 second added to the 1-minute loop is to avoid any issue with leap seconds...
	# If an extra second is added due to leap seconds, 
	# the loop would start at the 59th second of the minute it already did.
	# (I think, I don't know this for sure.)
	# 
	# Example:
	# An after timer is started at 23:59:00 to run 1000ms in the future.
	# If a leap-second is added, when the script runs after 1000ms is up
	# it will then be 23:59:59 when the script does a [clock format].
	# (Somebody correct me if I'm wrong please.)
	# I may be off by 1 minute..  If the leap second is added at 00:00:00,
	# the script, when run, would see the time as 00:00:59.
	#
	# The extra 16ms between running the commands is so they don't run 
	# all at once, causing the process to use a lot of CPU-time in a short
	# amount of time.  If they're 16ms apart, it spreads the load out.
	# 16ms, on Windows XP at least, is how often a process gets its share of the CPU.
	#
	# There's no good reason for the rand(). =P
	#
	# The effect of all this, is that the command scripts will run at most 1.4 seconds into the minute.
}

# Starts the 1-minute loop if it's not already running:
proc ::cron::start {} {
	variable TimerID
	if {$TimerID eq {}} {
		variable TimerID [after [expr { 60000 - ([clock milliseconds] % 60000) + 1017 + int(rand() * 127) }] [namespace code DoCron]]
	}
}

# Stops the 1-minute loop:
proc ::cron::stop {} {
	variable TimerID
	after cancel $TimerID
	variable TimerID {}
}

# The rest is just used for testing purposes:

#            field          allowed values
#              -----          --------------
#              minute         0-59
#              hour           0-23
#              day of month   1-31
#              month          1-12 (or names, see below)
#              day of week    0-7 (0 or 7 is Sun, or use names)

if {0} {
	namespace eval ::cron {
		puts "now: [clock format [clock seconds] -format {%M %H %d %m %w}]"
		# minute hour day month weekday
		set testval "50-60/2;09 */4,21 09/2 7-9,december *"
		#set testval $argv
		puts "cron input: $testval"
		puts "parse result: [set parseresult [::cron::parse $testval]]"
		puts "match result: [::cron::match $parseresult]"
		puts "parse time: [time { ::cron::parse $testval } 10000]"
		puts "match time: [time { ::cron::match $parseresult } 10000]"
		set matchmax [dict create minutes {0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59} hours {0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23} days {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31} months {1 2 3 4 5 6 7 8 9 10 11 12} weekdays {0 1 2 3 4 5 6}]
		puts "match time (max): [time { ::cron::match $matchmax } 10000]"
		set matchmin [dict create minutes {} hours {} days {} months {} weekdays {}]
		puts "match time (min): [time { ::cron::match $matchmin } 10000]"
		add {*/2} puts test
	}
	after 999999 [list set ::forever 1]
	vwait ::forever
}
