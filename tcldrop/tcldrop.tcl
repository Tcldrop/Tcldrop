#!/usr/bin/tclsh8.5

if {[catch { package require Tcl 8.5 }]} {
	puts "Error! Tcldrop requires Tcl 8.5 or above to run."
	exit 1
}

# First try to load tcldrop without adding any tm paths ([package require] has to be done on SOMETHING anyway before the ::tcl::tm::* commands will exist.):
# the tcl::tm::* commands should be loaded as they needed. Ethier by package unknown, or by a call to ::tcl::tm::*, ::auto_index(::tcl::tm::path) is set corectly on my system.
if {[catch { package require tcldrop }]} {
	foreach p [list [file join [file dirname [info script]] modules]] {
		if {[file isdirectory $p]} { ::tcl::tm::path add $p }
	}
	# Try to load tcldrop again..
	package require tcldrop
}

namespace eval ::tcldrop {
	# Background and foreground modes only apply to tclsh...
	# If we're running inside an Eggdrop or Wish or some other program then they'll keep us running (no need to do fork or vwait in those cases).
	# FixMe: If TclX or Expect is already loaded, they should be tried first, critcl should be last.
	if {$tcldrop(host_env) eq {tclsh}} {
		if {$tcldrop(background-mode)} {
			# Background mode was requested.
			if {![catch { package require Expect }] && [llength [info commands fork]] && ![catch { fork } pid]} {
				if {$pid != 0} {
					puts "Launched into the background (using Expect)  (pid: $pid)"
					::tcldrop::Exit 0
				} else {
					catch { disconnect }
					vwait ::tcldrop::Exit
					exit $Exit
				}
			} elseif {![catch { package require Tclx }] && [llength [info commands fork]] && ![catch { fork } pid]} {
				if {$pid != 0} {
					puts "Launched into the background (using TclX)  (pid: $pid)"
					::tcldrop::Exit 0
				} else {
					vwait ::tcldrop::Exit
					exit $Exit
				}
			} elseif {![catch { package require critcl }] && ![catch {::critcl::cproc fork {} int { return fork(); }}] && [llength [info commands fork]] && ![catch { fork } pid]} {
				if {$pid != 0} {
					puts "Launched into the background (using Critcl)  (pid: $pid)"
					::tcldrop::Exit 0
				} else {
					vwait ::tcldrop::Exit
					exit $Exit
				}
			} else {
				puts "Running in foreground mode.  (pid: [pid])\n(Install TclX, Expect or Critcl for background mode support.)"
				set tcldrop(background-mode) 0
			}
			# FixMe: Make it run in the foreground when receiving a SIGTTIN or SIGTTOU - http://en.wikipedia.org/wiki/SIGTTIN & http://en.wikipedia.org/wiki/SIGTTOU
			#        Or SIGCONT - http://en.wikipedia.org/wiki/SIGCONT
			#        And SIGTSTP/SIGSTOP should return it to the background. - http://en.wikipedia.org/wiki/SIGTSTP
		}
		# Foreground mode was requested or we're falling back to it.  (otherwise we would have exited before we got to this point).
		puts "${::argv0}: Entering Tcl event-loop (vwait)"
		# FixMe: Basically I want a readline or readline-like interface.  So that at least Tcl-commands can be entered on this interpreter (the bot(s) are running in slave interps).
		# FixMe: In the future I'd like to see a screen-like interface, where each bot has its own "screen" that it runs on.
		# FixMe: This code is untested...
		# I suggest to try to use Tclx command-loop first (if we have Tclx)
		fconfigure stdin -blocking 0 -buffering line
		fconfigure stdout -blocking 0 -buffering line
		fconfigure stderr -blocking 0 -buffering line
		if {[fileevent stdin readable] eq {}} {
			# Next 2 lines of code are a workaround for an Expect bug:
			# Basically, if Expect is loaded in the slave interp, fileevents on stdin in slave interp won't work unless their master has/had a fileevent.
			fileevent stdin readable NOOP
			fileevent stdin readable {}
		}
		if {!$tcldrop(background-mode) && !$tcldrop(simulate-dcc)} {
			if {![catch { package require tclreadline }] && [info commands ::tclreadline::Loop] ne {}} {
				# This is the real tclreadline.
				puts "Starting tclreadline::Loop ..."
				# FixMe: Make it exit when ::tcldrop::Exit is set. Or something. Would rather not have to do a trace on ::tcldrop::Exit to accomplish this.
				::tclreadline::Loop
			} elseif {![catch { package require TclReadLine }] && [info commands ::TclReadLine::interact] ne {}} {
				# This is a Tcl script based Tcl readline.
				puts "Starting TclReadLine::interact ..."
				# FixMe: Make it exit when ::tcldrop::Exit is set.  (Need to modify the modules/TclReadLine-1.1.tm)
				::TclReadLine::interact
			} elseif {![catch { vwait ::tcldrop::Exit } error]} {
				catch { puts "Exiting with error level $Exit ... $error" }
			} else {
				catch { puts "Exiting with error level $Exit ... $error \n$::errorInfo" }
			}
			exit $::tcldrop::Exit
		} elseif {![catch { vwait ::tcldrop::Exit } error]} {
			catch { puts "Exiting with error level $Exit ... $error" }
		} else {
			catch { puts "Exiting with error level $Exit ... $error \n$::errorInfo" }
		}
	}
}
#catch { namespace import ::tcldrop::* }
