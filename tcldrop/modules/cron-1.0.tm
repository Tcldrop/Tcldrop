#! /usr/bin/tclsh

namespace eval ::cron {
	package provide cron 1.0
	variable IDCount
	if {![info exists IDCount]} { variable IDCount 0 }
	variable TimerID
	if {![info exists TimerID]} { variable TimerID {} }
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
	foreach info {{minutes 0 59} {hours 0 23} {days 1 31} {months 1 12} {weekdays 0 6}} element [split $cron] {
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
		# For speed in the match proc, we see if the requested range is also the complete range, 
		# and if so we replace it with "" and just check for that rather than searching a list
		# for something that always matches:
		if {[llength [dict get $times $name]] >= ($max + 1 - $min)} { dict set times $name [list] }
	}
	return $times
}

# Used by [parse]
# Parses a single cron element, returning {{skip start end} ...}
# skip = the skip/step value
# start = the start of the range (may not be specified)
# end = the end of the range (may not be specified)
proc ::cron::parse_element {element {name {}}} {
	set retval [list]
	foreach element [split $element {,;:|&}] {
		if {$element eq {}} {
			# Ignore.
			continue
		} elseif {$element eq {*}} {
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
		} else {
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
	return $retval
}

# Takes the dict given by the output of [parse] and returns 1 or 0 if the current time/date is a match.
proc ::cron::match {dict} {
	if {[matchtime [dict get $dict minutes] {%M}] && [matchtime [dict get $dict hours] {%H}] && [matchtime [dict get $dict months] {%m}] && ([matchtime [dict get $dict weekdays] {%w}] || [matchtime [dict get $dict days] {%d}])} {
		return 1
	}
	return 0
}

# Used by [match]
# Searches the given $list to see any are a match for the current time.
proc ::cron::matchtime {list format} {
	# If $list is empty, it means always match.
	if {[llength $list] == 0 || [scan [clock format [clock seconds] -format $format] %d] in $list} {
		return 1
	}
	return 0
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
proc ::cron::crontab {cron args} {
	if {[llength $args]} {
		variable IDCount
		variable crontabs
		dict set crontabs [incr IDCount] [dict create cron [parse $cron] command $args]]
		# Start the 1-minute loop:
		crond start
		return $IDCount
	}
}

# Removes a crontab:
proc ::cron::uncrontab {id} {
	variable crontabs
	if {[dict exists $crontabs $id]} {
		dict unset crontabs $id
		if {[dict size $crontabs] == 0} {
			# Stop the 1-minute loop if there aren't any crontabs set.
			crond stop
		}
	}
}

# This is the 1-minute looping proc, it processes all the matching crontabs for this minute:
proc ::cron::DoCron {} {
	variable crontabs
	dict for {id info} $crontabs {
		if {[match [dict get $info cron]]} {
			# Use after idle here, in case the scripts run for over 60 seconds, so we don't miss the next minute:
			after idle [dict get $info command]
		}
	}
	# Start another after timer to do this again at the start of the next minute:
	variable TimerID [after [expr { (60 - ([clock seconds] % 60) ) * 1000 }] [namespace code [list DoCron]]]
}

# Makes starting/stopping the 1-minute looping proc (DoCron) easier:
proc ::cron::crond {{command {}}} {
	switch -- $command {
		{start} - {init} {
			# Start the 1-minute loop if it's not already running:
			variable TimerID
			if {$TimerID eq {}} {
				variable TimerID [after [expr { (60 - ([clock seconds] % 60) ) * 1000 }] [namespace code [list DoCron]]]
			}
		}
		{stop} {
			# Stop the 1-minute loop:
			variable TimerID
			after cancel $TimerID
			variable TimerID {}
		}
	}
}

# The rest is just used for testing purposes:

#            field          allowed values
#              -----          --------------
#              minute         0-59
#              hour           0-23
#              day of month   1-31
#              month          1-12 (or names, see below)
#              day of week    0-7 (0 or 7 is Sun, or use names)

#namespace eval ::cron {
	#puts "now: [clock format [clock seconds] -format {%M %H %d %m %w}]"
	# minute hour day month weekday
	#set testval "50-60/2;09 */4,21 09/2 7-9,december 7-4"
	##set testval $argv
	#puts "cron input: $testval"
	#puts "parse result: [set parseresult [::cron::parse $testval]]"
	#puts "match result: [::cron::match $parseresult]"
	#puts "parse time: [time { ::cron::parse $testval } 10000]"
	#puts "match time: [time { ::cron::match $parseresult } 10000]"
#}
