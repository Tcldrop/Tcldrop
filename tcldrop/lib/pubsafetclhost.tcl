# pubsafetclclient.tcl --
#
# Copyright (C) 2004,2005,2006,2007,2008,2009 by Philip Moore <FireEgl@Tcldrop.US>
# This code may be distributed under the same terms as Tcl.
#
# RCS: @(#) $Id$
#
# Provides a safe Tcl interpreter that can be used without the worry of infinite loops.
# See "man n interp" and "man n safe" for more info on safe interpreters.
#
# Usage:
#
#	this is the entry point for the pubsafetcl process. It is used by pubsafetclclient.tcl
#

# Client access the host who executes the commands...

namespace eval pubsafetclhost {

	source [file join [file dirname [info script]] pubsafetcl.tcl]

	variable port 12140

	package require Tcl 8.5
	package require comm
	package require pubsafetcl
	variable version {2.2.0}
	variable author {johannes-kuhn}
	variable description {This is the entry point for the pubsafetcl process}
	variable script [info script]
	# package provide pubsafetcl $version
	package provide pubsafetclhost $version
	variable rcsid {$Id$}
	
	# create an empty interp, used for the public interface (see comm package)
	variable comminterp [interp create]
	variable ids
	# key: interp, value: the id used by comm for the client
	array set ids {}
	variable extraCommands_current
	array set extraCommands_current {}
	variable extraCommands_info
	# Used to restore the original command
	# key: interp, value: dict (key: command, value: (hidden|client|current))
	array set extraCommands_info {}
	
	# delete all commands now. note: I don't clear vars, so be careful that no variable substitution is done
	interp eval $comminterp {
		foreach ns [namespace children] {
		# we don't delete ::tcl here, there are some internal commands that might be required
			if {$ns ne "::tcl"} {namespace delete $ns}
		}
		foreach cmd [info commands] {
			if {$cmd ni {namespace rename if}} {rename $cmd {}}
		}
		namespace delete ::tcl
		rename namespace {}
		rename if {}
		# we don't delete rename, this is required later
	}
	interp hide $comminterp rename
	# We only allow local connections
	::comm::comm new [namespace current]::comm -local 1 -port $port -listen 1 -silent 1 -interp $comminterp
	
	
	proc Exit args {
		comm destroy
		set forever 1
		exit 0
	}
	interp alias $comminterp exit {} [namespace current]::Exit
	
	proc Create {{interp safetcl} args} {
		variable ids
		variable comminterp
		set ids($interp) [comm remoteid]
		::pubsafetcl::create $interp {*}$args
		variable extraCommands_current
		set extraCommands($interp) {}
		interp alias $comminterp $interp {} ::$interp
		interp alias $comminterp ::pubsafetcl::${interp}::extraCommands {} [namespace current]::ExtraCommands $interp
		interp alias $comminterp ::pubsafetcl::${interp}::${interp} {} ::pubsafetcl::${interp}::${interp}
		return $interp
	}
	interp alias $comminterp ::pubsafetcl::create {} [namespace current]::Create
	
	# We have to provide our own Reset. Some people may use that to create an interp
	proc Reset {{interp safetcl}} {
		variable ids
		set ids($interp) [comm remoteid]
		set res [::pubsafetcl::Reset $interp]
		variable extraCommands_current
		set extraCommands($interp) {}
		interp alias $comminterp $interp {} ::$interp
		interp alias $comminterp ::pubsaftcl::${interp}::extraCommands {} [namespace current]::ExtraCommands $interp
		set $res
	}
	interp alias $comminterp ::pubsafetcl::Reset
	
	# I whish I could use a 8.6 feature. But I must not do that: namespace ensemble -parameters
	
	# calling this proc with empty extraCommands and command remove removes all extra commands
	# Todo: check if the command already exist or hidden or target interp...
	# This replaces pubsafetcls extraCommands
	proc ExtraCommands {interp command {extraCommands {}}} {
		variable ids
		variable extraCommands_info
		variable extraCommands_current
		switch $command -- {
			add {
				foreach c $extraCommands {
					if {$c in $extraCommands_current($interp)} continue;
					set info [list]
					# Avoid the glob style info commands
					if {[interp invokehidden $interp namespace which $c] ne ""} {
						lappend info rename
						interp invokehidden $interp rename $c ::tcl::${c}_orig
					}
					if {$c in [interp hidden $interp]} {
						interp expose $interp $c
						lappend info hidden
					} elseif {[namespace which $c] ne ""} {
						interp alias $interp $c {} $c
						lappend info current
					} else {
						interp alias $interp $c {} [namespace current]::comm send $ids($interp) $c
						lappend info client
					}
					dict set extraCommands_info($interp) $c $info
				}
				set extraCommands_current($interp) [lsort -unique [concat $extraCommands $extraCommands_current($interp)]]
			}
			remove {
				if {$extraCommands eq ""} {
					set extraCommands extraCommands_current($interp)
				}
				foreach c $extraCommands {
					set extraCommands_current($interp) [lsearch -exact -not $extraCommands_current($interp) $c]
					set rename 0
					foreach action [dict get $extraCommanfs_info($interp) $c] {
						switch -- $action {
							rename {set rename 1}
							hidden {interp hide $interp $c}
							current - 
							client {interp invokehidden $interp rename ::${c} {}}
						}
						if {$rename} {
							interp invokehidden $interp rename ::tcl::${c}_orig ::${c}
						}
					}
					dict unset extraCommands_info($interp) $c
					interp invokehidden $interp rename $c {}
				}
			}
		}
	}
}

vwait forever