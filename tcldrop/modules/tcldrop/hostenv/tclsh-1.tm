# hostenv/tclsh --
#	Handles: 
#		* The interface to tclsh.
#
# $Id$
#
# Copyright (C) 2010 Tcldrop-Dev <Tcldrop-Dev@Tcldrop.US>
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
# Free Software Foundation, Inc., 
# 59 Temple Place - Suite 330, 
# Boston, MA  02111-1307, USA.
# Or visit http://www.GNU.Org/licenses/gpl.html
#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.

package provide tcldrop::hostenv::tclsh 1
namespace eval ::tcldrop::hostenv::tclsh {
	namespace path [list ::tcldrop::hostenv ::tcldrop]
	# Assume we're in a tclsh (or tclsh-like) environment.
	proc PutLogLev {name levels channel text {tags {}}} {
		variable Tcldrop
		if {[array size Tcldrop]} { set pre "Tcldrop/$name: " } else { set pre {} }
		if {![string match {*e*} $levels] || [catch { puts stderr "$pre$text" }]} { catch { puts "$pre$text" } }
	}
	# fileevent handler, calls the stdin command with the line of input:
	proc ::tcldrop::hostenv::tclsh::STDIN {} {
		while {[gets stdin line] >= 0} { stdin $line }
		# catch, because something in the stdin handler may have closed stdin already:
		if {[catch { eof stdin } eof] && $eof} { close stdin }
	}
	fileevent stdin readable [list ::tcldrop::hostenv::tclsh::STDIN]
	#fileevent stdout writable [list ::tcldrop::hostenv::tclsh::STDOUT]
	
	# Start the bot now:
	puts [Tcldrop exec $::tcldrop::tcldrop(argv)]

	# FixMe: When backgrounding, close stdin/stdout/stderr...
	#        See the daemonize proc here: http://wiki.tcl.tk/2224

	# Background and foreground modes only apply to tclsh...
	# If we're running inside an Eggdrop or Wish or some other program then they'll keep us running (no need to do fork or vwait in those cases).
	# FixMe: If TclX or Expect is already loaded, they should be tried first, critcl should be last.
	if {$::tcldrop::tcldrop(background-mode) && $::tcl_platform(platform) eq {unix}} {
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
			set ::tcldrop::tcldrop(background-mode) 0
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
	#fconfigure stdin -blocking 0 -buffering line
	#fconfigure stdout -blocking 0 -buffering line
	#fconfigure stderr -blocking 0 -buffering line

	#		fconfigure stdin -buffering line -blocking 1
	#		fconfigure stdout -buffering line
	#catch { exec stty -raw echo }
	#if {[fileevent stdin readable] eq {}} {
	#	# Next 2 lines of code are a workaround for an Expect bug:
	#	# Basically, if Expect is loaded in the slave interp, fileevents on stdin in slave interp won't work unless their master has/had a fileevent.
	#	fileevent stdin readable NOOP
	#	fileevent stdin readable {}
	#}
	if {!$::tcldrop::tcldrop(background-mode) && !$::tcldrop::tcldrop(simulate-dcc)} {
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
	puts "No longer in vwait! - Exiting.."
}
