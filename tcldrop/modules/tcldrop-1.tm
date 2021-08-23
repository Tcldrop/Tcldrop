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
# Example Usage:
# tcldrop start <botname> <config-filename>
# tcldrop start <botname> <Tcl commands to eval>
# tcldrop stop <botname>
# tcldrop eval <botname> <Tcl commands to eval>
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

# This whole .tcl is kind of sloppy.. Probably the most sloppy of all the .tcl's in Tcldrop.
# FixMe: Rewrite this, possibly splitting it up into different .tcl files, possibly one for each Tcl-environment (wish, eggdrop, tclsh, etc.) ...

namespace eval ::tcldrop {
	if {[catch {package require Tcl 8.5}]} { return -code error "Error! Tcldrop requires Tcl 8.5 or above to run." }
	# Append to the ::auto_path some more common paths (if they exist):
	# Note: I do this because when I compile Tcl from source, it doesn't add the Debian/Ubuntu-specific paths..
	foreach p [list /usr/share/tcltk/tcl[info tclversion] /usr/local/lib/tcltk /usr/local/share/tcltk /usr/lib/tcltk /usr/share/tcltk] {
		if {$p ni $::auto_path && [file isdirectory $p]} {
			lappend ::auto_path $p
		}
	}
	variable version {0.6.1}
	variable numversion {00060100}
	variable script [info script]
	# This is lowercase becase eggdrop is lowercase in the .modules list on Eggdrop, and because the tcldrop namespace is lowercase:
	variable name {tcldrop}
	variable depends {Tcl}
	variable author {Tcldrop-Dev}
	variable description {Initializes Tcldrop from within a Tcl environment.}
	variable rcsid {$Id$}
	# Provide tcldrop as a package:
	package provide $name 1
	# Pretend to provide eggdrop:
	if {{eggdrop} ni [package names]} { package provide eggdrop 1.6.20 }
	# tcldrop stores info's that each tcldrop needs while starting up:
	variable tcldrop
	array set tcldrop [list name $name config {} config-eval {} supervision-mode 0 background-mode 1 channel-stats 0 simulate-dcc 0 userfile-create 0 console {oe} host_env {unknown} dirname [file dirname $script] version $version numversion $numversion depends $depends author $author description $description rcsid $rcsid script $script]
	# Tcldrop stores info about each Tcldrop that's running:
	variable Tcldrop
	array set Tcldrop {}
	variable Default {}
	variable input
	array set input {}
	namespace export tcldrop Tcldrop
	variable commands [namespace export]

	# Rename the exit command, so that we can handle exits better:
	if {![llength [info commands ::tcldrop::Exit]]} {
		rename ::exit ::tcldrop::Exit
		proc ::tcldrop::exit {{code {0}}} {
			variable Tcldrop
			# Create a virtual signal called SIGEXIT (named SIGEXIT rather than just EXIT so that a bind can bind to sig* and still see that we're exiting; and also because we already have an exit event that triggers when [exit] is called from the slaves.):
			foreach t [array names Tcldrop] { catch { tcldrop eval $t callevent SIGEXIT } }
			# This is the real exit command:
			::tcldrop::Exit $code
			# We shouldn't ever make it to here..
			return $code
		}
		# The new global exit command is now an alias to our new ::tcldrop::exit proc:
		interp alias {} exit {} ::tcldrop::exit
	}

	# What a huge command...
	namespace ensemble create -command Tcldrop -map {excute TcldropExec run TcldropExec start TcldropStart create TcldropStart load TcldropStart init TcldropStart restart TcldropRestart force-restart TcldropRestart reload TcldropRestart force-reload TcldropRestart rehash TcldropRestart force-rehash TcldropRestart stop TcldropStop delete TcldropStop die TcldropStop kill TcldropStop close TcldropStop cleanup TcldropStop clean-up TcldropStop exit TcldropExit eval TcldropEval tcl TcldropEval stdin TcldropStdin} -unknown TcldropUnknown
	
	proc TcldropExec {name {arg {}}} {
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
							{N} { set tcldrop(supervision-mode) 1 }
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
			  -N   supervision mode - stay in the foreground, and do not use readline.
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
	}
	
	proc TcldropStart {name {arg {}}} {
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
	}
	
	proc TcldropRestart {name {arg {}}} {
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
	
	proc TcldropStop {name {arg {}}} {
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
	
	proc TcldropExit {name {arg {}}} {
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
	
	# TODO: Discusion: arg is not longer optional. This might cause errors
	proc TcldropEval {name arg} {
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
	
	proc TcldropStdin {name arg} {
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
	
	# This is called when no command matches
	proc TcldropUnknown {cmd subcmd args} {
		lassign $args name
		PutLogLev $name o - "Unknown command: $command"
		# We return an empty list, so namespace ensemble generates the error. This is also a help.
		return {}
	}
	

	proc tcldrop {command name args} { Tcldrop $command $name $args }

	if {![info exists tcldrop(argv)]} {
		if {[info exists ::argv]} {
			set tcldrop(argv) $::argv 
		} else {
			set tcldrop(argv) {}
		}
	}

	if {[info commands putloglev] != {}} {
		# This is so we can see the Tcldrop's putlog's in case we're running inside an Eggdrop.
		proc PutLogLev {name levels channel text} {
			if {[array size Tcldrop]} { set pre "Tcldrop/$name: " } else { set pre {Tcldrop: } }
			putloglev $levels $channel "$pre$text"
		}
		catch {
			# This creates a DCC command called .tcldrop if we're running inside an Eggdrop.
			bind dcc n tcldrop ::tcldrop::DCC
			proc ::tcldrop::DCC {handle idx text} {
				set ltext [split $text]
				putcmdlog "#$handle# tcldrop $text"
				if {[catch { Tcldrop [lindex $ltext 0] [lindex $ltext 1] [join [lrange $ltext 2 end]] } error]} { putdcc $idx $error }
			}
			set tcldrop(host_env) {eggdrop}
		}
	} elseif {[info exists tk_version] || ![catch { package require Tk 8.5 } tcldrop(tk_version)]} {
		# We're running from wish.
		array set tcldrop [list host_env wish background-mode 0 simulate-dcc 1 channel-stats 1]
		#option add *Text.wrap word widgetDefault
		if {[tk windowingsystem] == {x11}} {
			option add *borderWidth 1 widgetDefault
			option add *activeBorderWidth 1 widgetDefault
			option add *selectBorderWidth 1 widgetDefault
			option add *font -adobe-helvetica-medium-r-normal-*-12-*-*-*-*-*-*
			option add *padX 2
			option add *padY 4
			option add *Listbox.background white
			option add *Listbox.selectBorderWidth 0
			option add *Listbox.selectForeground white
			option add *Listbox.selectBackground #4a6984
			option add *Entry.background white
			option add *Entry.foreground black
			option add *Entry.selectBorderWidth 0
			option add *Entry.selectForeground white
			option add *Entry.selectBackground #4a6984
			option add *Text.background white
			option add *Text.selectBorderWidth 0
			option add *Text.selectForeground white
			option add *Text.selectBackground #4a6984
			option add *Menu.activeBackground #4a6984
			option add *Menu.activeForeground white
			option add *Menu.activeBorderWidth 0
			option add *Menu.highlightThickness 0
			option add *Menu.borderWidth 2
			option add *Menubutton.activeBackground #4a6984
			option add *Menubutton.activeForeground white
			option add *Menubutton.activeBorderWidth 0
			option add *Menubutton.highlightThickness 0
			option add *Menubutton.borderWidth 0
			option add *highlightThickness 0
			option add *troughColor #bdb6ad
			option add *Labelframe.borderWidth 2
			option add *Frame.borderWidth 2
			# These are from roxirc:
			option add *background #c0c0c0 widgetDefault
			option add *activeBackground #c0c0c0 widgetDefault
			option add *highlightBackground #c0c0c0 widgetDefault
			option add *selectBorderWidth 0 widgetDefault
			option add *selectBackground #999999 widgetDefault
			option add *selectColor navy widgetDefault
			option add *highlightThickness 0 widgetDefault
			option add *Toplevel.borderWidth 1 widgetDefault
			option add *Toplevel.relief raised widgetDefault
			option add *Scrollbar.width 13 widgetDefault
			option add *Scrollbar.borderWidth 0 widgetDefault
			option add *Scrollbar.elementBorderWidth 2 widgetDefault
			option add *Menubutton.borderWidth 1 widgetDefault
			option add *Menu.activeBorderWidth 1 widgetDefault
			option add *Menu*Menu.borderWidth 1 widgetDefault
			option add *Text.cursor left_ptr widgetDefault
			option add *Text.highlightThickness 1 widgetDefault
			option add *Entry.highlightThickness 1 widgetDefault
			option add *Button.borderWidth 1 widgetDefault
			option add *Listbox.borderWidth 1 widgetDefault
			option add *menubar.relief raised widgetDefault
			option add *menubar.borderWidth 1 widgetDefault
			option add *text.borderWidth 1 widgetDefault
			option add *n.label.relief raised widgetDefault
		} elseif {$tcl_platform(platform) == {windows} || [tk windowingsystem] == {win32}} {
			option add *padX 0
			option add *padY 0
			option add *borderWidth 0 widgetDefault
			option add *Button.borderWidth 1 widgetDefault
			option add *Text.cursor arrow
			option add *Button.padX 1m widgetDefault
			option add *Button.width 0 widgetDefault
			option add *Menubutton.padY 4 widgetDefault
			option add *Listbox.highlightThickness 0 widgetDefault
			option add *Textborder.borderWidth 0 widgetDefault
			option add *Textborder*Listbox.borderWidth 0 widgetDefault
			option add *Nickborder.borderWidth 0 widgetDefault
			option add *menubar.relief groove widgetDefault
			option add *menubar.borderWidth 2 widgetDefault
			option add *text.relief flat widgetDefault
			catch {font create fixed -family fixedsys -size 8}
			if {$tcl_platform(osVersion) > 5.0} {
				option add *Menubutton.activeBackground SystemHighlight widgetDefault
				option add *Menubutton.activeForeground SystemHighlightText widgetDefault
				option add *Menubutton.borderWidth 0 widgetDefault
			} else {
				option add *Menubutton.borderWidth 1 widgetDefault
			}
		}
		wm title . {Tcldrop}
		# scroll --
		# Description
		#   Create a scrollable-widget frame
		#
		# Arguments
		#   type : type of  scrollable widget (e.g. listbox)
		#   W    : name for scrollable widget
		#   args : arguments to be passed to creation of scrollable widget
		#
		# Return
		#   name of scrollable widget
		proc scroll {type W args} {
			set w $W.$type
			set x $W.x
			set y $W.y
			array set arg [list -borderwidth 0 -highlightthickness 0 -relief flat -xscrollcommand [list $x set] -yscrollcommand [list $y set]]
			array set arg $args
			frame $W -borderwidth 0 -class Scroll -highlightthickness 0 -relief sunken -takefocus 0 -pady 0 -padx 0
			# create the scrollable widget
			uplevel [linsert [array get arg] 0 $type $w]
			scrollbar $x -borderwidth 0 -elementborderwidth 1 -orient horizontal -takefocus 0 -highlightthickness 0 -command [list $w xview]
			scrollbar $y -borderwidth 0 -elementborderwidth 1 -orient vertical -takefocus 0 -highlightthickness 0 -command [list $w yview]
			grid columnconfigure $W 1 -weight 1
			grid rowconfigure $W 1 -weight 1
			grid $w -column 1 -row 1 -sticky nsew
			grid $x -column 1 -row 2 -sticky nsew
			grid $y -column 2 -row 1 -sticky nsew
			return $w
		}
		namespace eval entry {
			# Original source: http://wiki.tcl.tk/
			proc add? {w} {
				variable $w
				variable n$w
				upvar 0 $w hist
				set s [set ::[$w cget -textvariable]]
				if {$s == {}} { return }
				if [string compare $s [lindex $hist end]] {
					lappend hist $s
					set n$w [llength $hist]
				}
			}
			proc move {w where} {
				variable $w
				variable n$w
				upvar 0 $w hist
				incr n$w $where
				if {[set n$w]<0} { set n$w 0 }
				if {[set n$w]>=[llength $hist]+1} { set n$w [llength $hist] }
				set ::[$w cget -textvar] [lindex $hist [set n$w]]
			}
			proc for {type name args} {
				switch -- $type {
					{entry} {
						uplevel $type $name $args
						set ns [namespace current]
						bind $name <Up> "${ns}::move %W -1"
						bind $name <Down> "${ns}::move %W 1"
						bind $name <Next> "${ns}::move %W 99999"
						bind $name <Return> "${ns}::add? %W"
						bind $name <ButtonPress-3> "${ns}::Event:ButtonPress-3 %W %X %Y"
						variable $name {}
						variable n$name 0
						set name
					}
					default {return -code error "usage: entry::for entry <w> <args>"}
				}
			}
			# Original source: http://wiki.tcl.tk/3404
			proc Copy {win type} {
				if {[$win index end] == 0 || [catch {$win index sel.first}]} { return }
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				clipboard clear -displayof $win
				clipboard append -displayof $win $data
			}
			proc Paste {win type} {
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				$win insert insert $data
			}
			proc Cut {win type} {
				if {[catch {$win index sel.first}]} { return }
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				$win delete sel.first sel.last
				clipboard clear -displayof $win
				clipboard append -displayof $win $data
			}
			proc Append {win type} {
				if {[$win index end] == 0 || [catch {$win index sel.first}]} { return }
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				clipboard append -displayof $win $data
			}
			proc Clear {win} { $win delete 0 end }
			proc Event:ButtonPress-3 {win X Y} {
				destroy $win._popup
				set m [menu $win._popup -tearoff 0]
				set ns [namespace current]
				#valid types are PRIMARY and CLIPBOARD
				$m add command -label Copy -command "${ns}::Copy $win PRIMARY"
				$m add command -label "Paste Primary" -command "${ns}::Paste $win PRIMARY"
				$m add command -label "Paste Clipboard" -command "${ns}::Paste $win CLIPBOARD"
				$m add command -label Cut -command "${ns}::Cut $win PRIMARY"
				$m add command -label Append -command "${ns}::Append $win PRIMARY"
				$m add command -label Clear -command "${ns}::Clear $win"
				tk_popup $m $X $Y
			}
		}
		namespace eval buttonbar {
			# Original source: http://wiki.tcl.tk/
			proc create {w frame} {
				variable buttonbar
				set ns [namespace current]
				frame $w -relief flat -padx 0 -pady 0 -borderwidth 0
				frame $w.middle -relief flat -bd 1 -pady 0 -padx 0
				button $w.left -text {<} -bd 0 -command [list ${ns}::scrollleft $w] -font {Fixedsys 14 bold} -relief flat -highlightthickness 0 -width 0 -padx 0 -state disabled
				button $w.right -text {>} -bd 0 -command [list ${ns}::scrollright $w] -font {Fixedsys 14 bold} -relief flat -highlightthickness 0 -width 0 -padx 0 -state disabled
				button $w.close -text {x} -bd 0 -command [list ${ns}::closecurrent $w] -font {Fixedsys 14 bold} -relief flat -highlightthickness 0 -width 2 -padx 0
				canvas $w.middle.c -height [winfo reqheight $w.right] -xscrollincrement 1 -highlightthickness 0 -relief flat -borderwidth 0
				frame $w.middle.c.f -relief flat -borderwidth 0 -padx 0 -pady 0
				grid $w.left $w.right $w.middle $w.close -sticky nesw -padx 0 -pady 0
				grid columnconfigure $w {0 1 3} -minsize 15 -weight 0
				grid columnconfigure $w 2 -weight 2
				pack $w.middle.c -fill both -padx 0 -pady 0
				$w.middle.c create window 0 0 -anchor nw -window $w.middle.c.f
				bind $w.middle.c.f <Configure> "$w.middle.c configure -scrollregion \[$w.middle.c bbox all\]"
				bind tab <Button-1> "${ns}::showframe $w %W; bind tab <Motion> \"${ns}::tabdrag $w %W\""
				bind tab <ButtonRelease-1> "${ns}::tearoff $w %W %X %Y; bind tab <Motion> {}; after cancel \"${ns}::tabdrag $w %W\""
				bind $w.middle.c <Configure> "${ns}::setscrollstate $w"
				set buttonbar($w) $frame
				return $w
			}
			proc add {w name} {
				variable buttonbar
				frame $buttonbar($w).$name -relief flat -padx 0 -pady 0 -borderwidth 0
				button $w.middle.c.f.$name -font {Fixedsys 14 normal} -text $name -highlightthickness 0 -padx 2 -pady 1 -relief raised
				pack $w.middle.c.f.$name -side left -pady 0 -padx 0 -fill y
				bindtags $w.middle.c.f.$name tab
				showframe $w $name
				after idle [namespace current]::setscrollstate $w
				return $buttonbar($w).$name
			}
			proc name {w tab name} { if {[winfo exists $w.middle.c.f.$tab]} { $w.middle.c.f.$tab configure -text $name } }
			proc tabexists {w tab} { winfo exists $w.middle.c.f.$tab }
			proc scrollright {w} {
				scrollsetleft $w [winfo containing [expr { [winfo rootx $w.middle.c] + [winfo width $w.middle.c] - 1 }] [winfo rooty $w.middle.c]]
				$w.right configure -foreground black -activeforeground black
			}
			proc scrollleft {w} {
				scrollsetright $w [winfo containing [winfo rootx $w.middle.c] [winfo rooty $w.middle.c]]
				$w.left configure -foreground black -activeforeground black
			}
			proc scrollsetleft {w tab} {
				set tab [string map [list $w.middle.c.f {}] $tab]
				if {![winfo exists $w.middle.c.f$tab]} { return }
				$w.middle.c xview scroll [expr { [winfo rootx $w.middle.c.f$tab] - [winfo rootx $w.middle.c] }] units
			}
			proc scrollleft {w} {
				scrollsetright $w [winfo containing [winfo rootx $w.middle.c] [winfo rooty $w.middle.c]]
				$w.left configure -foreground black -activeforeground black
			}
			proc scrollsetleft {w tab} {
				set tab [string map [list $w.middle.c.f {}] $tab]
				if {[winfo exists $w.middle.c.f$tab]} {
					$w.middle.c xview scroll [expr { [winfo rootx $w.middle.c.f$tab] - [winfo rootx $w.middle.c] }] units
				}
			}
			proc scrollsetright {w tab} {
				if {[winfo exists $w.middle.c.f[set tab [string map [list $w.middle.c.f {}] $tab]]]} {
					$w.middle.c xview scroll [expr { -1 * (([winfo rootx $w.middle.c] + [winfo width $w.middle.c]) - ([winfo rootx $w.middle.c.f$tab] + [winfo width $w.middle.c.f$tab])) }] units
				}
			}
			proc closecurrent {w} {
				foreach x [winfo children $w.middle.c.f] {
					if {[$x cget -relief] == "sunken"} {
						variable buttonbar
						destroy $x $buttonbar($w).[string map [list $w.middle.c.f. {}] $x]
						return
					}
				}
			}
			proc hilightbutton {w name} {
				if {![winfo ismapped [set name [winfo toplevel $name]]]} {
					if {[info exists ::info(text,$name)] && [string match *hilight* [$::info(text,$name) tag names end-1l+8c]]} { set color {yellow} } else { set color {red} }
					if {[$w.middle.c.f$name cget -foreground] != "yellow"} {
						$w.middle.c.f$name configure -foreground $color -activeforeground $color
					}
					if {[set view [tabvisibility $name]] < 0 && [$w.left cget -foreground] != "yellow"} {
						$w.left configure -foreground $color -activeforeground $color
					}
					if {$view > 0 && [$w.right cget -foreground] != "yellow"} {
						$w.right configure -foreground $color -activeforeground $color
					}
				}
			}
			proc tabvisibility {w name} {
				if {[set ts [winfo rootx $w.middle.c.f$name]] < [set s [winfo rootx $w.middle.c]]} {
					return -1
				} elseif {$ts + [winfo width $w.middle.c.f$name] > $s + [winfo width $w.middle.c]} {
					return 1
				} else {
					return 0
				}
			}
			proc showframe {w name} {
				if {[$w.middle.c.f.[set name [lindex [split $name .] end]] cget -relief] != "sunken"} {
					variable buttonbar
					foreach x [winfo children $buttonbar($w)] { if {$x != $w} { pack forget $x } }
					foreach x [winfo children $w.middle.c.f] {$x configure -relief raised}
					pack $buttonbar($w).$name -fill both -expand 1 -padx 0 -pady 0
					$w.middle.c.f.$name configure -foreground black -activeforeground black -relief sunken
				}
			}
			proc setscrollstate {w} {
				if {[set width [winfo width $w.middle.c]] > 1 && [winfo width $w.middle.c.f] > $width} {
					$w.left configure -state normal
					$w.right configure -state normal
				} else {
					$w.left configure -foreground black -activeforeground black -state disabled
					$w.right configure -foreground black -activeforeground black -state disabled
				}
			}
			proc tearoff {w tab x y} {
				set rx2 [expr { [set rx1 [winfo rootx $w]] + [winfo width $w] }]
				set ry2 [expr { [set ry1 [winfo rooty $w]] + [winfo height $w] }]
				if {$x < ($rx1 - 20) || $x > ($rx2 + 20) || $y < ($ry1 - 20) || $y > ($ry2 + 20)} {
					variable buttonbar
					set win $buttonbar($w).[string map [list $w.middle.c.f. {}] $tab]
					# add your function here
					#closecurrent $w
				}
			}
			proc tabdrag {w tab} {
				set pointery [winfo pointery $tab]
				set pointerx [winfo pointerx $tab]
				set hi [winfo rooty $w.middle]
				if {$pointery < $hi || $pointery > ($hi + [winfo height $w.middle])} { return }
				set children [winfo children $w.middle.c.f]
				set c [lsearch -exact $children $tab]
				if {$pointerx < [winfo rootx $w.middle.c]} {
					bind tab <Motion> {}
					after 500 [list [namespace current]::tabdrag $w $tab]
					if {[set to [lindex $children [expr { $c - 1 }]]] == {}} { return }
					pack configure $tab -before $to
					lower $tab $to
					update idletasks
					if {[tabvisibility $w [string map [list $w.middle.c.f {}] $tab]] < 0} { scrollsetleft $w $tab }
					return
				} elseif {$pointerx > ([winfo rootx $w.middle.c] + [winfo width $w.middle.c])} {
					bind tab <Motion> {}
					after 500 [list [namespace current]::tabdrag $w $tab]
					if {[set to [lindex $children [expr { $c + 1 }]]] == {}} { return }
					pack configure $tab -after $to
					raise $tab $to
					update idletasks
					if {[tabvisibility $w [string map [list $w.middle.c.f {}] $tab]] > 0} { scrollsetright $w $tab }
					return
				}
				bind tab <Motion> "[namespace current]::tabdrag $w $tab"
				set in [winfo containing $pointerx $pointery]
				if {$tab == $in} { return }
				set i [lsearch -exact $children $in]
				if {$i < 0} {
					pack configure $tab -after [set to [lindex $children end]]
					raise $tab $to
				} elseif {$i < ($c - 1)} {
					pack configure $tab -before [set to [lindex $children [expr { $c - 1 }]]]
					lower $tab $to
				} elseif {$i > ($c + 1)} {
					pack configure $tab -after [set to [lindex $children [expr { $c + 1 }]]]
					raise $tab $to
				}
			}
		}
		proc stdout {name text args} {
			array set options [list -name $name -newline 1]
			array set options $args
			if {![buttonbar::tabexists .tabs $name]} {
				# Create the tab for $name
				buttonbar::add .tabs $name
				pack [scrollbar .windows.$name.sbar -orient vertical -command [list .windows.$name.output yview] -relief flat] -expand n -fill y -side right -pady 0 -padx 0
				pack [text .windows.$name.output -yscrollcommand [list .windows.$name.sbar set] -font {Fixedsys 8 normal} -relief flat -bg black -fg white -insertbackground white -insertofftime 100 -insertontime 100 -insertwidth 2 -borderwidth 0 -pady 0 -padx 0] -expand y -fill both -pady 0 -padx 0
				pack [entry::for entry .windows.$name.input -textvar ::tcldrop::input($name) -takefocus 1 -font {Fixedsys 8 normal} -relief flat -bg AntiqueWhite4 -fg white -insertbackground AntiqueWhite1 -insertofftime 100 -insertontime 100 -borderwidth 0] -fill both -expand n -pady 0 -padx 0
				buttonbar::name .tabs $name $options(-name)
				buttonbar::showframe .tabs $name
				bind .windows.$name.output <ButtonRelease-1> "tk_textCopy .windows.${name}.output; focus .windows.$name.input; break"
				bind .windows.$name.input <Return> "+
					::tcldrop::stdin $name \$::tcldrop::input($name)
					set ::tcldrop::input($name) {}
					.windows.$name.input delete 0 end
				"
				bind .windows.$name.output <Return> "+
					::tcldrop::stdin $name \$::tcldrop::input($name)
					set ::tcldrop::input($name) {}
					.windows.$name.input delete 0 end
				"
				focus .windows.$name.input
				set options(-newline) 0
			}
			if {$options(-newline)} {
				.windows.$name.output insert end "\n$text"
			} else {
				.windows.$name.output insert end "$text"
			}
			.windows.$name.output see end
			update idletasks
		}
		proc PutLogLev {name levels channel text {tags {}}} { stdout $name $text -levels $levels -channel $channel }
		proc stdin {name text} {
			switch -- $name {
				{console} {
					::tcldrop::stdout $name "% $text"
					if {[catch { namespace eval [namespace current] $text } output]} { set output "Tcl error: $output" } else { set output "Tcl: $output" }
					::tcldrop::stdout $name $output
				}
				{default} { ::tcldrop::Tcldrop stdin $name $text }
			}
		}

		# Use winico if it's available (puts Tcldrop in the systray on Windows):
		# FixMe: We need to provide an .ico file, and the winico.dll for this to work for everybody.
		if {[file exists icon.ico] && ![catch {package require Winico}]} {
			variable Icon [winico create icon.ico]
			proc ::tcldrop::TbarHandle {ico msg args} {
				switch -- $msg {
					{WM_LBUTTONDOWN} { wm deiconify . }
					{WM_RBUTTONDOWN} { wm withdraw . }
				}
			}
			winico taskbar add $Icon -callback {::tcldrop::TbarHandle %i %m} -text {Tcldrop}
			winico setwindow . $Icon
			tkwait visibility .
			bind . <Unmap> {wm withdraw .}

			# For more winico stuff see http://wiki.tcl.tk/4090
			#winico taskbar add $winico -callback {winicoCallback %m %x %y} -text $yourAppName
			#proc winicoCallback {t {x 0} {y 0}} {
				#if { $t == "WM_LBUTTONUP" } {
					#wm deiconify .
					#raise .
					#focus .
				#} elseif { $t == "WM_RBUTTONUP" } {
					# Right-click menu..
					#.winicoPopup post $x $y
					#.winicoPopup activate 0
				#}
			#}
		#}
		# This creates the Tcldrop program window and all that good stuff:
		pack [buttonbar::create .tabs .windows] -side top -fill x -padx 0 -pady 0
		pack [frame .windows -padx 0 -pady 0] -side bottom -fill both -expand 1 -padx 0 -pady 0
		stdout console "Tcldrop Console, Ready." -newline 0 -name {Console}
		#wm geometry . [wm geometry .]
		after idle [list wm deiconify .]
		# Catch window being deleted (icon or otherwise):
		wm protocol . WM_DELETE_WINDOW { if {[tk_messageBox -icon question -type yesno -default no -message "Do you want to go?" -title "Quit Application?"] == "yes"} { exit } }


		  #wm title . "Minimize Test"
		  #wm geometry . [winfo screenwidth .]x[winfo screenheight .]+0+0
		  #update idletasks               ;# updates the full-screen
		  #wm overrideredirect . yes      ;# removes window decorations
		  #wm attributes . -topmost yes   ;# stays on top of other windows
#
		  #frame .x -highlightthickness 0 -bg #c8efff
		  #place .x -x 0 -y 0 -relwidth 1 -relheight 1
#
		  #button .x.min -text "Minimize" -bg #ffdbff -font "arial 10 bold" \
		  #  -command x_iconify
		  #place  .x.min -x [expr [winfo screenwidth  .] - 140] -y 10
#
		  #button .x.end -text "Close" -bg #ffdbff -font "arial 10 bold" \
		  #  -command "destroy ."
		  #place  .x.end -x [expr [winfo screenwidth  .] - 60] -y 10
#
		  #bind . <Expose> "wm overrideredirect . yes; focus -force ."
		  #bind . <KeyPress-Escape> x_iconify
#
		  #proc x_iconify {} {
		  #wm overrideredirect . no
		  #wm iconify .
		  #}


		  #wm iconbitmap . "myicon.ico"
 			#wm iconbitmap . -default "myicon.ico"

		stdout console [Tcldrop run [concat [list -n -t] $::argv]]

		# Re-set Exit again so the window will close:
		variable Exit
		if {[info exists Exit]} {
			after 99999 [set ::tcldrop::Exit $Exit]
			stdout console {Closing window in 99 seconds...}
			vwait ::tcldrop::Exit
		}
	} else {
		# Assume we're in a tclsh (or tclsh-like) environment.
		proc PutLogLev {name levels channel text {tags {}}} {
			variable Tcldrop
			if {[array size Tcldrop]} { set pre "Tcldrop/$name: " } else { set pre {} }
			if {![string match {*e*} $levels] || [catch { puts stderr "$pre$text" }]} { catch { puts "$pre$text" } }
		}
		set tcldrop(host_env) {tclsh}
		puts [Tcldrop run $tcldrop(argv)]
	}

	variable Exit
	# Exit right now if something already set Exit:
	if {[info exists Exit]} {
		exit $Exit
	}

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
		default { variable trapSignals {SIGHUP SIGQUIT SIGTERM SIGINT SIGSEGV SIGBUS SIGFPE SIGILL SIGUSR1 SIGUSR2 SIGABRT SIGXCPU SIGBREAK SIGPWR SIGWINCH} }
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
