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
	array set ids {}
	
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
	
	# calling this proc with empty extraCommands removes all extra commands
	proc ExtraCommands {interp command {extraCommands {}}} {
		variable ips
		switch $command -- {
			add {
				foreach c $extraCommands {
					interp $interp alias $c {} [namespace current]::comm send $ips($interp) $c
				}
				variable extraCommands_current
				set extraCommands_current($interp) [lsort -unique [concat $extraCommands $extraCommands_current($interp)]]
			}
			remove {
				variable extraCommands_current
				if {$extraCommands eq ""} {
					set extraCommands extraCommands_current($interp)
				}
				foreach c $extraCommands {
					set extraCommands_current($interp) [lsearch -exact -not $extraCommands_current $c]
					interp $interp invokehidden rename $c {}
				}
			}
		}
	}
}

vwait forever