#! /usr/bin/tclsh8.5

if {[catch { package require Tcl 8.5 }]} {
	puts stderr {Error!  Tcldrop requires Tcl 8.5 or above to run.}
	exit 1
}

proc src {file args} {
  set argv $::argv
  set argc $::argc
  set ::argv $args
  set ::argc [llength $args]
  set code [catch { uplevel [list source $file] } return options]
  set ::argv $argv
  set ::argc $argc
  return -code $code -options $options $return
}

# First try to load tcldrop without adding any tm paths ([package require] has to be done on SOMETHING anyway before the ::tcl::tm::* commands will exist.):
# the tcl::tm::* commands should be loaded as needed, either by package unknown, or by a call to ::tcl::tm::*, ::auto_index(::tcl::tm::path) is set correctly on my system.
if {[catch { package require tcldrop }]} {
	foreach p [list [file join [file dirname [info script]] modules]] {
		if {[file isdirectory $p]} { ::tcl::tm::path add $p }
	}
	# Try to load tcldrop again, allowing an error to show up:
	package require tcldrop
}
