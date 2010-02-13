# pubsafetclclient.tcl --
#
# Copyright (C) 2004,2005,2006,2007,2008,2009 by Philip Moore <FireEgl@Tcldrop.US>
# This code may be distributed under the same terms as Tcl.
#
# RCS: @(#) $Id: pubsafetcl.tcl 358 2010-02-11 20:23:58Z johannes-kuhn $
#
# Provides a safe Tcl interpreter that can be used without the worry of infinite loops.
# See "man n interp" and "man n safe" for more info on safe interpreters.
#
# Usage:
#
#	pubsafetcl::create interpname ?-option value? ...
#

# We use the same interface (and provide pubsafetcl) as for the original pubsaftcl, so scripts relying on them won't break.
# The version corresponds to the pubsaftcl-version

# TODO: Add ulimit. I don't know how, and how much.
# TODO: Reset the interp after some sec if it does not response.
namespace eval pubsafetcl {

	# You have to set this in order to use this package
	variable remoteport 0
	# If your executable is not tclsh or wish, than you have to set this.
	variable tclsh [info executable]

	package require Tcl 8.5
	package require comm
	variable exec [catch {package require Tclx}]
	variable version {2.2.0}
	variable name {pubsafetcl}
	variable author {johannes-kuhn}
	variable description {This is used to make pubsafetcl safer. It starts a new process with the pubsafetcl interp}
	variable script [info script]
	package provide pubsafetcl $version
	package provirde pubsafetclhost $version
	variable rcsid {$Id: pubsafetcl.tcl 358 2010-02-11 20:23:58Z johannes-kuhn $}
	variable commands {pubsafetcl Reset}
	namespace export $commands
	
	proc create {{interp safetcl} args} {
		if {[namespace exist [namespace current]::$interp]} {
			Reset $interp
		}
		namespace eval [namespace current]::$interp [list variable interp $interp]
		namespace eval [namespace current]::$interp [list variable args $args]
		namespace eval [namespace current]::$interp {
			namespace path [list [namespace parent]]
			# Because we use namespace path, the variables from the parent namespace are used. After that, they are created.
			variable remoteport $remoteport
			variable exec $exec
			variable extraCommands_current {}
			variable tclsh $tclsh
			# we create an interp
			variable targetinterp [interp create]
			# remove all commands
			interp eval $targetinterp {
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
			interp hide $targetinterp rename
			# Add aliases
			::comm::comm new [namespace current]::comm -listen 1 -interp $targetinterp -local 1
			namespace export $interp
			proc $interp {args} {
				variable interp
				variable remoteport
				comm send $remoteport $interp $args
			}
			namespace ensemble create -command [namespace current]::extraCommands -map {add ExtraCommmandsAdd remove ExtraCommandsRemove}
			proc ExtraCommandsAdd {{extraCommands {}}} {
				variable targetinterp
				foreach c $extraCommands {
					# add this command to the interp
					interp alias $targetinterp $c {} $c
				}
				comm send $remoteport [list ::pubsafetcl::${$interp}::extraCommands add $extraCommands]
			}
			variable extraCommands_current $extraCommands
			}
			proc ExtraCommandsRemove {{extraCommands {}}} {
				variable remoteport
				if {$extraCommands == {}} {
					variable extraCommands_current
					set extraCommands $extraCommands_current
				}
				variable targetinterp
				foreach c $extraCommands {
					interp invokehidden $targetinterp rename $c {}
				}
				comm send $remoteport [list ::pubsafetcl::${interp}::extraCommands remove $extraCommands]
			}
			namespace eval :: [list namespace import -force $interp]
			# We have now to create the host in background - yeah, a litte bit confusing
			variable pid [exec $tclsh [file nativename [file join [file dirname [set [namespace parrent]::script] pubsafetclhost.tcl]]] &]
			# Create the interp remote
			comm $targetport ::pubsafetcl::create $interp $args
			return $interp
		}
	}
	
	# TODO: think again..
	proc Reset {{interp safetcl} {mode 0}} {
		namespace eval [namespace current]::$interp [list variable mode $mode]
		namespace eval [namespace current]::$interp {
			if {$mode == 0} {
				# force the destroy of host process in 3 sec
				set timer [after 3000 [list [namespace parent]::Reset $interp 1]
				send comm $remoteport exit
				catch {after calcel $timer}
				if {$force} {
					# the timer has cleaned up
					return
				}
			} else {
				# this is usually the timer
				# kill the proc
				# I hope, this is good enogh. If not, I've to add -SIGKILL. But I don't like to.
				# The target process should have a changce to cleanup, even tcl is busy. (expr)
				if {!$exec} {
					# use Tclx's kill command
					kill pid
				} else {
					exec kill $pid
				}
			}
			# comm destroy destroys the interp too
			comm destroy
		}
	}
}