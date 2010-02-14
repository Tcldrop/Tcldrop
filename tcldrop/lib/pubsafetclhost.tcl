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
#	load it, and load the client in an other application
#

# TODO: Add support for extraCommands
namespace eval pubsafetclhost {

	variable port 0

	package require Tcl 8.5
	package require comm
	package require pubsafetcl
	variable version {2.2.0}
	variable author {johannes-kuhn}
	variable description {This is the entry point for the pubsafetcl process}
	variable script [info script]
	package provide pubsafetcl $version
	package provirde pubsafetclhost $version
	variable rcsid {$Id$}
	
	variable comminterp [interp create]
	variable ids
	array set ids {}
	
	interp eval comminterp {
		foreach ns [namespace children] {
		# we don't delete ::tcl here, there are some internal commands that might be required
			if {$ns ne "::tcl"} {namespace delete $ns}
		}
		foreach cmd [info commands] {
			if {$cmd ni {namespace rename}} {rename $cmd {}}
		}
		namespace forget ::tcl
		rename namespace {}
		# we don't delete rename, this is required later
	}
	interp hide comminterp rename
	::comm::comm new [namespace current]::comm -local 1 -port $port -listen 1 -silent 1 -interp $comminterp
	proc Exit args {
		comm destroy
		exit 0
	}
	interp alias $comminterp exit {} [namespace current]::Exit
	proc Create {{interp safetcl} args} {
		variable ids
		set ids($interp) [comm remoteid]
		::pubsafetcl::create $interp $args
		interp alias $comminterp $interp {} ::$interp
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
				
				}
			}
		}
	}
}