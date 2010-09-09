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
#	pubsafetcl::create interpname ?-option value? ...
#

# We use the same interface (and provide pubsafetcl) as for the original pubsaftcl, so scripts relying on them won't break.
# The version corresponds to the pubsaftcl-version

# TODO: Add ulimit. I don't know how, and how much.
# TODO: Reset the interp after some sec if it does not response.
namespace eval pubsafetcl {

	# You have to set this in order to use this package
	variable remoteport 12140
	# If your executable is not tclsh or wish, than you have to set this.
	variable tclsh [info nameofexecutable]

	package require Tcl 8.5
	package require comm
	# If we have to exec kill. Otherwise we use Tclx kill
	variable exec [catch {package require Tclx}]
	variable version {2.2.0}
	variable name {pubsafetcl}
	variable author {johannes-kuhn}
	variable description {This is used to make pubsafetcl safer. It starts a new process with the pubsafetcl interp}
	variable script [info script]
	package provide pubsafetcl $version
	package provide pubsafetclhost $version
	variable rcsid {$Id$}
	variable commands {pubsafetcl Reset}
	namespace export $commands
	
	proc create {{interp safetcl} args} {
		if {[namespace exist [namespace current]::$interp]} {
			Reset2 $interp
		}
		namespace eval [namespace current]::$interp [list variable interp $interp]
		namespace eval [namespace current]::$interp [list variable args $args]
		namespace eval [namespace current]::$interp {
			namespace path [list [namespace parent]]
			variable remoteport [set [namespace parent]::remoteport]
			variable exec [set [namespace parent]::exec]
			variable extraCommands_current {}
			variable tclsh [set [namespace parent]::tclsh]
			# we create an interp for the extra commands
			variable targetinterp [interp create]
			# remove all commands
			interp eval $targetinterp {
				foreach ns [namespace children] {
					# we don't delete ::tcl here, there are some internal commands that might be required
					if {$ns ne "::tcl"} {namespace delete $ns}
				}
				
				foreach cmd [info commands] {
					if {$cmd ni {namespace rename if}} {rename $cmd {}}
				}
				
				namespace forget ::tcl
				rename namespace {}
				rename if {}
				# we don't delete rename, this is required later
			}
			interp hide $targetinterp rename
			# Add aliases
			::comm::comm new [namespace current]::comm -listen 1 -interp $targetinterp -local 1
			namespace export $interp
			proc pubsafetcl {args} {
				variable interp
				variable remoteport
				comm send $remoteport $interp $args
			}
			namespace ensemble create -command [namespace current]::$interp -map {alias {pubsafetcl alias} aliases {pubsafetcl aliases} bgerror {pubsafetcl bgerror} eval PubsafetclEval expose {pubsafetcl expose} hide {pubsafetcl hide} hidden {pubsafetcl hidden} issafe {pubsafetcl issafe} invokehidden {pubsafetcl invokehidden} limit {pubsafetcl limit} marktrusted {pubsafetcl marktrusted} recursionlimit {pubsafetcl recursionlimit} setting PubsafetclSet variable PubsafetclSet option PubsafetclSet configure PubsafetclSet fancyeval PubsafetclFancyeval} -prefixes 1
			namespace export $interp
			
			proc PubsafetclSet {name {value {}}} {
				if {$name eq "extraCommands"} {
					extraCommands remove
					extraCommands add $value
				} else {
					variable $name $value
				}
			}
			
			proc PubsafetclFancyeval {args} {
				variable interp
				variable remoteport
				set timer [after 3000 [list [namespace parent]::Reset $interp]]
				set res [comm send $remoteport [list $interp fancyeval {*}$args]]
				after cancel $timer
				return $res
			}
			
			
			namespace ensemble create -command [namespace current]::extraCommands -map {add ExtraCommandsAdd remove ExtraCommandsRemove}
			proc ExtraCommandsAdd {{extraCommands {}}} {
				variable targetinterp
				variable extraCommands_current
				variable remoteport
				variable interp
				foreach c $extraCommands {
					# add this command to the interp
					interp alias $targetinterp $c {} $c
				}
				comm send $remoteport [list ::pubsafetcl::${interp}::extraCommands add $extraCommands]
				set extraCommands_current [lsort -unique [concat $extraCommands $extraCommands_current]]
			}
			proc ExtraCommandsRemove {{extraCommands {}}} {
				variable remoteport
				variable interp
				variable extraCommands_current
				if {$extraCommands == {}} {
					variable extraCommands_current
					set extraCommands $extraCommands_current
				}
				variable targetinterp
				foreach c $extraCommands {
					if {$c ni $extraCommands_current} continue
					interp invokehidden $targetinterp rename $c {}
					set extraCommands_current [lsearch -all -inline -exact -not $extraCommands_current $c]
				}
				comm send $remoteport [list ::pubsafetcl::${interp}::extraCommands remove $extraCommands]
			}
			namespace eval :: [list namespace import -force [namespace current]::$interp]
			# We have now to create the host in background - yeah, a litte bit confusing
			variable pid [exec $tclsh [file nativename [file join [file dirname [set [namespace parent]::script]] pubsafetclhost.tcl]] &]
			# Create the interp remote
			comm send $remoteport ::pubsafetcl::create $interp {*}$args
			return $interp
		}
	}
	
	proc Reset2 {{interp safetcl} {mode 0}} {
		catch {
			namespace eval [namespace current]::$interp [list variable mode $mode]
			namespace eval [namespace current]::$interp {
				if {$mode == 0} {
					# force the destroy of host process in 3 sec
					set timer [after 3000 [list [namespace parent]::Reset2 $interp 1]]
					# This is a blocking call. If it returns, I think it has succeed
					# Note: this _always_ returns an error
					catch {comm send $remoteport exit}
					catch {after cancel $timer}
					if {$mode} {
						# the timer has cleaned up the rest, nothing to do now
						return 1
					}
				} else {
					# this is usually the timer
					# kill the proc
					# I hope, this is good enogh. If not, I've to add -SIGKILL. But I don't like to.
					# The target process should have a changce to cleanup, even tcl is busy. (expr)
					if {!$exec} {
						# use Tclx's kill command
						kill $pid
					} else {
						exec kill $pid
					}
				}
				# comm destroy destroys the interp too
				comm destroy
			}
			namespace delete [namespace current]::$interp
		}
	}
	
	proc Reset {{interp safetcl}} {
		Reset2 0
		create $interp
	}
}