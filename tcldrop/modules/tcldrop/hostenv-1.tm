# hostenv --
#	Handles: 
#		* The tclsh/wish/eggdrop/xchat/etc interface.
#
#	Description:
#		The [stdin] command is called whenever a line is received from "stdin".
#		It can be either [::tcldrop::stdin] or [::stdin].
#		And [::tcldrop::hostenv::stdout] should be called 
#		whenever you want to send to "stdout".  
#		As well as [::tcldrop::hostenv::stderr] for "stderr".
#
#	Notes:
#		This runs from the master interp, NOT the bots interp.

package provide tcldrop::hostenv 1
namespace eval ::tcldrop::hostenv {
	namespace ensemble create -command hostenv -map {stdout Stdout stderr Stderr stdin Stdin puts Puts putlog PutLog putloglev PutLogLev} -unknown {Unknown}
	namespace export hostenv
	namespace path [list ::tcldrop]
	variable hostenv
	# Detect our host Tcl environment here, and then load the appropriate package for it:
	if {[llength [info commands ::xchatdirfs]]} {
		# X-Chat
		variable hostenv {xchat}
	} elseif {0} {
		# Tcldrop
		# Yes, this loads support for a Tcldrop inside a Tcldrop.
		# FixMe: Add a test for Tcldrop so this actually works.
		variable hostenv {tcldrop}
	} elseif {[info commands ::putloglev] ne {} && ![catch { package require eggdrop 1.6 } tcldrop(eggdrop_version)]} {
		# Eggdrop
		variable hostenv {eggdrop}
	} elseif {([info exists ::argv0]) && ([string match {*tclsh*} $::argv0] || [string match {*tcldrop*} $::argv0])} {
		# tclsh
		variable hostenv {tclsh}
	} elseif {[info exists tk_version] || ![catch { package require Tk 8.5 } tcldrop(tk_version)]} {
		# Tk (wish)
		# FixMe: Should this try to load Tk or just detect if it's already loaded?
		variable hostenv {wish}
	} else {
		# Unknown
		variable hostenv {unknown}
	}
	# load the package now that we know what we're running from:
	package require "tcldrop::hostenv::$hostenv"
}
