# Tcldrop --
#	Handles:
#		* Initializing Tcldrop from within a Tcl environment.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008,2009,2010 Tcldrop-Dev <Tcldrop-Dev@Tcldrop.US>
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
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# Or visit http://www.GNU.Org/licenses/gpl.html
#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.

# This is the file you would source from other Tcl-enabled applications.
#
# This creates a "tcldrop" namespace under the current one.
# In this new namespace is a command called tcldrop.
#
# The tcldrop command can then be used to start/stop individual Tcldrops
# or eval Tcl commands inside their interpreters.
#
# Usage:
# tcldrop command botname ?arg? ?arg ...?
#
# Sub-command Usage:
# tcldrop parseopts <command-line options/arguments>
#	Parses and returns the command-line options/arguments.
# tcldrop exec <command-line options/arguments>
#	Processes Eggdrop-style command-line options, and starts a bot.
# tcldrop init <botname>
#	Initializes a bot but doesn't "start" it, must use start or eval to start it's "start" proc after this.
# tcldrop start <botname>
#	Starts a bot using a previously set config-file or eval.
# tcldrop start <botname> <config-filename>
#	Starts a bot using a config file.
# tcldrop start <botname> <Tcl commands to eval>
#	Starts a bot and Eval's Tcl commands in its interp.
# tcldrop stop <botname>
#	Stops a bot.
# tcldrop restart <botname>
#	Does stop and restart.
# tcldrop eval <botname> <Tcl commands to eval>
#	Eval's Tcl code on a bot.
# tcldrop stdin <botname> <text>
#	Sends text to a bots "stdin".
# tcldrop signal <botname> <signal>
#	Sends a (faked) signal to a bot.
# tcldrop list
#	Lists the currently running Tcldrops.
#
#
#
# 
#
# command can be start, stop, eval, and restart.
# name should be a unique name for the bot.
# eval should be a whatever you want eval'd in the tcldrop's interp.
#
# Some examples of what your eval can contain:
# set config <config filename>
# set nick <nick>
#
# Note, your eval can take the place of a config file..
# It'll have to be a big eval though..

namespace eval ::tcldrop {
	if {[catch {package require Tcl 8.5}]} { return -code error "Error! Tcldrop requires Tcl 8.5 or above to run." }
	# Append to the ::auto_path some more common paths (if they exist):
	# Note: I do this because when I compile Tcl from source, it doesn't add the Debian/Ubuntu-specific paths..
	foreach p [list /usr/share/tcltk/tcl[info tclversion] /usr/local/lib/tcltk /usr/local/share/tcltk /usr/lib/tcltk /usr/share/tcltk] {
		if {$p ni $::auto_path && [file isdirectory $p]} { lappend ::auto_path $p }
	}
	variable version {0.7.0}
	variable numversion {00070000}
	variable script [info script]
	# This is lowercase becase eggdrop is lowercase in the .modules list on Eggdrop, and because the tcldrop namespace is lowercase:
	variable name {tcldrop}
	variable depends {Tcl}
	variable author {Tcldrop-Dev}
	variable description {Initializes Tcldrop from within a Tcl environment.}
	variable rcsid {$Id: tcldrop-1.tm 380 2010-03-29 15:56:15Z FireEgl $}
	# Provide tcldrop as a package:
	package provide $name 1
	# tcldrop stores info's that each tcldrop needs while starting up:
	variable tcldrop
	array set tcldrop [list name $name config {} config-eval {} background-mode 1 channel-stats 0 simulate-dcc 0 userfile-create 0 console {oe} hostenv {unknown} dirname [file dirname $script] version $version numversion $numversion depends $depends author $author description $description rcsid $rcsid script $script]
	# Tcldrop stores info about each Tcldrop that's running:
	variable Tcldrop
	array set Tcldrop {}
	variable Default {}
	variable input
	array set input {}
	namespace export tcldrop Tcldrop
	variable commands [namespace export]
	namespace ensemble create -command Tcldrop -map {exec Exec run Exec start Start create Start load Start init Start restart Restart reload Restart rehash Restart stop Stop delete Stop die Stop kill Stop close Stop cleanup Stop exit Exit eval Eval tcl Eval stdin Stdin} -unknown Unknown
	# Rename and replace the exit command, so that we can handle exits better:
	if {![llength [info commands ::tcldrop::Exit]]} {
		rename ::exit ::tcldrop::Exit
		proc ::tcldrop::exit {{code {0}}} {
			variable Tcldrop
			# Create a virtual signal called SIGEXIT (named SIGEXIT rather than just EXIT so that a bind can bind to sig* and still see that we're exiting; and also because we already have an exit event that triggers when [exit] is called from the slaves.):
			foreach t [array names Tcldrop] { catch { tcldrop eval $t callevent SIGEXIT } }
			# This is the real exit command:
			::tcldrop::Exit $code
		}
		# The new global exit command is now an alias to our new ::tcldrop::exit proc:
		interp alias {} exit {} ::tcldrop::exit
	}
	# Stores $::argv in the tcldrop array, for possible later use:
	if {![info exists tcldrop(argv)]} { if {[info exists ::argv]} { set tcldrop(argv) $::argv } else { set tcldrop(argv) {} } }
}

# Starts a bot using Eggdrop-style command line options:
proc ::tcldrop::Exec {name {arg {}}} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	# This processes $name as if it were $::argv (command-line options)...
	variable configs [list]
	# FixMe: Need proper command-line option handling.
	foreach a $name {
		switch -glob -- $a {
			{-*} {
				foreach a [split $a {}] {
					switch -- $a {
						{-} { }
						{n} { set tcldrop(background-mode) 0 }
						{c} { set tcldrop(channel-stats) 1 }
						{t} { set tcldrop(simulate-dcc) 1 }
						{m} { set tcldrop(userfile-create) 1 }
						{p} { set tcldrop(profiler) 1 }
						{v} {
							variable Exit 0
							return "Tcldrop v$tcldrop(version)  (C) 2001,2002,2003,2004,2005,2006,2007,2008,2009,2010 Tcldrop-Dev"
						}
						{h} - {?} {
							variable Exit 0
							return "Tcldrop v$tcldrop(version)  (C) 2001,2002,2003,2004,2005,2006,2007,2008,2009,2010 Tcldrop-Dev

		Command line arguments:
		  -h   help
		  -v   print version and exit
		  -n   don't go into the background
		  -c   (with -n) display channel stats every 10 seconds
		  -t   (with -n) use terminal to simulate dcc-chat
		  -m   userfile creation mode
		  -p   source code profiling (debug)
		  -d   Debug mode.
		  optional config filename (default 'tcldrop.conf')\n"
						}
						{d} {
							# Debug mode.
							set tcldrop(debug) 1
							if {![info exists ::env(DEBUG)]} { set ::env(DEBUG) 1 }
							# Also update the console flags to match:
							if {![info exists tcldrop(console)] || ![string match {*d*} $tcldrop(console)]} {
								append tcldrop(console) {d}
							}
						}
						{default} {
							variable Exit 1
							return "Unknown option: -$a"
						}
					}
				}
			}
			{+*} {
				# If an arg starts with + we treat it as console flags:
				append tcldrop(console) [string range $a 1 end]
			}
			{default} {
				# Whatever's left is treated as config filenames:
				lappend configs $a
			}
		}
	}
	# If no config was specified, use the default (tcldrop.conf):
	if {![llength $configs]} { lappend configs {tcldrop.conf} }
	set started [set failed 0]
	foreach config $configs {
		set tcldrop(config) $config
		set name [file tail [file rootname $config]]
		PutLogLev $name * - "*** Attempting to start $name... (configs: $configs)"
		PutLogLev $name * - "Tcldrop v$tcldrop(version)  (C) 2001,2002,2003,2004,2005,2006,2007,2008,2009,2010 Tcldrop-Dev"
		if {[tcldrop start [file tail [file rootname $config]] -config $config]} {
			incr started
			PutLogLev $name * - "*** $name Started."
		} else {
			incr failed
			PutLogLev $name * - "*** $name Failed to Start."
		}
	}
	if {!$started || $failed || ![array size Tcldrop]} { set ::tcldrop::Exit 1 }
	if {!$failed} { return "--- Started $started bot(s)." } else { return "--- Started $started bot(s).  ($failed failed to start)" }		
} ;# end TcldropExec

# Starts a bot using the dict in $arg as the options:
proc ::tcldrop::Start {name {arg {}}} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	array set options [list {-config} {} {-config-eval} {} {-name} $name]
	array set options $arg
	array set tcldrop [list config $options(-config) config-eval $options(-config-eval)]
	set name $options(-name)
	# This starts a single bot named $name with $arg being either the config file to load, or the Tcl-code to be evaluated in place of a config file.
	#if {[file exists [file join [pwd] $arg]]} {
	#	array set tcldrop [list config $arg config-eval {}]
	#} else {
	#	array set tcldrop [list config-eval $arg config {}]
	#}
		if {![catch {
			set Default $name
			# Create the interpreter that the Tcldrop will run in:
			interp create $interpname
			# Initialize the ::tcldrop namespace in the interpreter, and also create the global tcldrop array:
			#puts "$interpname eval [list namespace eval tcldrop [list array set ::tcldrop [array get tcldrop]]]"
			$interpname eval [list namespace eval tcldrop [list array set ::tcldrop [array get tcldrop]]]
			# Make the Tcldrop's ::tcldrop::PutLogLev actually call ::tcldrop::PutLogLev in the parent interp:
			interp alias $interpname ::tcldrop::PutLogLev {} [namespace current]::PutLogLev $name
			interp alias $interpname ::tcldrop::stdout {} [namespace current]::stdout $name
			interp alias $interpname ::tcldrop::tcldrop {} [namespace current]::tcldrop
			interp alias $interpname ::tcldrop::Tcldrop {} [namespace current]::Tcldrop
			# If the Tcldrop runs the exit command, it instead runs the [tcldrop exit] command here:
			interp alias $interpname exit {} [namespace current]::tcldrop exit $name
			# Load the core of the Tcldrop, which in turn will load the required modules, source the config file, etc:
			# FixMe: There's some kind of bug in Tcl that prevents it from loading the ::tcl::tm::* procs until after a package require is done on some other package first.
			# please try to execute set ::auto_index(::tcl::tm::path) and give me the result -- Johannes13
			$interpname eval [list package require http]
			if {![info exists mod-paths]} { set mod-paths [list [file join . modules] $tcldrop(dirname)] }
			# We don't need the {*} here. $mod-paths shoud contain a valid list
			$interpname eval ::tcl::tm::path add ${mod-paths}
			$interpname eval {
				# This is the "last resort" method of loading packages, it allows the modules to be loaded from a remote location:
				# Note: There's also a ::tcldrop::PkgUnknown proc inside modules/tcldrop/core-1.tm that will replace this one once it's loaded.
				# We should call the old pkgUnknown if our lookup failed.
				proc ::tcldrop::PkgUnknown {{name {}} {version {1}}} {
					# FixMe: The limitation here is that we have to know the version in advance.. There's no way for the server to tell us what the latest version is.
					# We could provide an extra file with this information.
					set token [::http::geturl http://tcldrop.svn.sourceforge.net/viewvc/tcldrop/tcldrop/modules/[string map {{::} {/}} $name]-${version}.tm]
					if {[::http::status $token] eq {ok}} { ::uplevel #0 [::http::data $token] } else { set version {} }
					::http::cleanup $token
					return $version
				}
				#::package unknown ::tcldrop::PkgUnknown
			}
			# Load the "core" module now:
			$interpname eval [list package require tcldrop::core]
	} error]} {
		set Tcldrop([string tolower $name]) [list name $name starttime [clock seconds]]
		return 1
	} else {
		# If $interpname calls [exit] from within itself it will delete its own interpreter, which means it closed gracefully and would have done its own putlog's (hopefully) explaining why it when down to fast, so we won't need to do any PutLogLev's here in that case:
		if {[interp exists $interpname]} {
			if {![catch {
				PutLogLev $name e - "Problem Starting: $error"
			}]} {
				catch { PutLogLev $name e - "Problem Starting (Full Error): \n [$interpname eval [list namespace eval tcldrop [list set errorInfo]]]" }
			}
			catch { interp delete $interpname }
		}
		if {![array size Tcldrop]} { set ::tcldrop::Exit 1 }
		return 0
	}
} ;# end TcldropStart

# Stops a bot:
proc ::tcldrop::Stop {name {arg {}}} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	catch { tcldrop eval $name callevent exit }
	array unset Tcldrop [string tolower $name]
	if {![array size Tcldrop]} { set ::tcldrop::Exit 0 }
	if {[catch { interp delete $interpname } error]} {
		PutLogLev $name * - "Error while closing $name: $error"
		return 0
	} else {
		PutLogLev $name * - "Closed Tcldrop: $name"
		return 1
	}
}

# This was Exit, but it should be merged with Stop I think:
proc ::tcldrop::Stop {name {arg {}}} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	# This is an alias to every bots [exit] command, it deletes the interp that $name is running from, and tells the vwait (if there is one) to return if there's no Tcldrop's left running.. $arg is the errorlevel to exit with.
	set return [Tcldrop delete $name]
	if {![array size Tcldrop]} { set ::tcldrop::Exit $arg }
	set return
}

# Restarts a bot. Same as doing stop and start:
proc ::tcldrop::Restart {name {arg {}}} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	# This forcibly stops and starts a bot named $name, $arg will be passed to the start command.
	# Note: This stops (kills) and restarts the bot
	#       Do not confuse this with doing [tcldrop <botname> eval restart]
	if {[Tcldrop stop $name] && [Tcldrop start $name $arg]} { return 1 } else { return 0 }
}

# Eval's Tcl code in a running bots interp:
proc ::tcldrop::Eval {name arg} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	# This evaluates Tcl code in a running bots interpreter.
	$interpname eval $arg
}

proc ::tcldrop::PutLogLev {name levels channel text {tags {}}} {
	variable Tcldrop
	if {[array size Tcldrop]} { set pre "Tcldrop/$name: " } else { set pre {} }
	if {![string match {*e*} $levels] || [catch { puts stderr "$pre$text" }]} { catch { puts "$pre$text" } }
}

# Sends text to a running bots "stdin":
proc ::tcldrop::Stdin {name arg} {
	variable Tcldrop
	variable tcldrop
	variable Default
	variable mod-paths
	if {$name == {-}} {
		# If they used - for the name, we use the name of the last started bot, which is stored in Default.
		set interpname "Tcldrop-[string tolower $Default]"
	} else {
		set interpname "Tcldrop-[string tolower $name]"
	}
	# This sends $arg to the Stdin proc of a running bots interpreter.
	$interpname eval [list ::tcldrop::stdin $arg]
}

# This is called when no sub-command to [tcldrop] matches:
proc ::tcldrop::Unknown {cmd subcmd args} {
	lassign $args name
	PutLogLev $name o - "Unknown command: $command"
	# We return an empty list, so namespace ensemble generates the error. This is also a help.
	return {}
}

# Sets a new default bot to be the one we control or that receives input from "stdin".
proc ::tcldrop::Control {name} {
	variable Default [string tolower $name]
}

proc ::tcldrop::tcldrop {command name args} { Tcldrop $command $name $args }

# Handler for hostenv:
proc ::tcldrop::stdin {text} { Stdin - $text }
# interp alias is more efficient:
interp alias {} ::tcldrop::stdin {} ::tcldrop::Stdin - 

# Detect and load the interface to our host Tcl environment (tclsh/wish/eggdrop/xchat/etc):
package require tcldrop::hostenv

namespace eval ::tcldrop {
	variable tcldrop

	variable Exit
	# Exit right now if something already set Exit:
	if {[info exists Exit]} { exit $Exit }

	# Do the signal handlers:

	# Eggdrop does these signals:
	# SIGBUS "BUS ERROR -- CRASHING!"
	# SIGSEGV "SEGMENT VIOLATION -- CRASHING!"
	# SIGFPE "FLOATING POINT ERROR -- CRASHING!"
	# SIGTERM "TERMINATE SIGNAL -- SIGNING OFF" or "RECEIVED TERMINATE SIGNAL (IGNORING)"
	# SIGQUIT "RECEIVED QUIT SIGNAL (IGNORING)"
	# SIGHUP "HANGUP SIGNAL -- SIGNING OFF" or "Received HUP signal: rehashing..."
	# SIGILL log context and continue
	# SIGALRM used for gethostbyname

	# http://en.wikipedia.org/wiki/Signal_handler#List_of_signals

	# Only attempt to trap signals that are possible to trap on a specific OS:
	# FixMe: from my testing, both Expect and TclX failed to trap SIGHUP, SIGUSR1, SIGUSR2, SIGXCPU, SIGPWR & SIGWINCH on windows.
	# If it is possible to trap these in windows, add them to the list. They both also failed to trap SIGBREAK but I know that's a windows-specific signal.
	# FixMe: Mac OS 8 and 9 identifies as "macintosh". OS X and later as "unix". Add "macintosh" to this list and find out what signals it can trap.
	# Never add SIGCHLD or SIGALRM to this list, and only list the ones we'll use or expect other people to use in Tcldrop:
	switch -exact -- $::tcl_platform(platform) {
		{unix} { variable trapSignals {SIGHUP SIGQUIT SIGTERM SIGINT SIGSEGV SIGBUS SIGFPE SIGILL SIGUSR1 SIGUSR2 SIGABRT SIGXCPU SIGPWR SIGWINCH} }
		{windows} { variable trapSignals {SIGQUIT SIGTERM SIGINT SIGSEGV SIGBUS SIGFPE SIGILL SIGABRT SIGBREAK} }
		{default} { variable trapSignals {SIGHUP SIGQUIT SIGTERM SIGINT SIGSEGV SIGBUS SIGFPE SIGILL SIGUSR1 SIGUSR2 SIGABRT SIGXCPU SIGBREAK SIGPWR SIGWINCH} }
	}
	variable trappedSignals {}
	# Expect seems able to trap more of them than TclX (at least on Windows):
	if {![catch { package require Expect }] && [info commands trap] != {}} {
		# Use trap from Expect.
		proc ::tcldrop::Signal {signal} {
			PutLogLev console o - "Expect Caught Signal: $signal"
			variable Tcldrop
			foreach t [array names Tcldrop] { catch { tcldrop eval $t callevent $signal } }
		}
		foreach Signal $trapSignals {
			if {![catch { trap [list ::tcldrop::Signal [string tolower $Signal]] $Signal } error]} {
				lappend trappedSignals $Signal
				#PutLogLev console o - "Using Expect for trapping signal: $Signal."
			} else {
				#PutLogLev console d - "Expect failed to trap signal $Signal: \"$error\""
			}
		}
		PutLogLev console d - "Using Expect for trapping signals: [join $trappedSignals {, }]."
	}
	# If Expect failed to trap any signals we try to trap them using TclX:
	# FixMe: Really try to trap signals using TclX if Expect failed to trap _ANY_ signals.
	if {[llength $trapSignals] > [llength $trappedSignals] && ![catch { package require Tclx }] && [info commands signal] != {}} {
		# Use signal from TclX.
		proc ::tcldrop::Signal {{signal {default}}} {
			set signal [string tolower $signal]
			PutLogLev console o - "TclX Caught Signal: $signal"
			variable Tcldrop
			foreach t [array names Tcldrop] { catch { tcldrop eval $t callevent $signal } }
		}
		foreach Signal $trapSignals {
			if {[lsearch -exact $trappedSignals $Signal] == -1} {
				if {![catch { signal trap $Signal [list ::tcldrop::Signal %S] } error]} {
					lappend trappedSignals $Signal
					# PutLogLev console o - "Using TclX for trapping signal: $Signal."
				} else {
					#PutLogLev console d - "TclX failed to trap signal $Signal: \"$error\""
				}
			}
		}
		PutLogLev console d - "Using TclX for trapping signals: [join $trappedSignals {, }]."
	}

	foreach Signal $trapSignals { if {[lsearch -exact $trappedSignals $Signal] == -1} { lappend untrappedSignals $Signal } }
	if {[info exists untrappedSignals]} { PutLogLev console d - "Unable to trap these signals: [join $untrappedSignals {, }]." }
	unset -nocomplain trapSignals trappedSignals Signal error untrappedSignals
}
