# core/main --
#	Handles:
#		* Provides all core Tcl commands and bind types.
#		* Initializes the bot by loading the other core modules and sourcing the config file.
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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

# At this point we should be running in our own interpreter..
# We should have variables called config or config-eval (or both).
# config should contain the filename of the config to load.
# config-eval can take the place of a config file. Its contents will be eval'd.
# config-eval if it exists will be eval'd before searching for the config file.
# So if you set the config variable inside config-eval, the config file will be loaded after the eval on config-eval is done.

# FixMe: $config should be loaded first if the variable exists, before we eval $config-eval.

# Tcldrop requires at least Tcl v8.5.
::package require Tcl 8.5
# Cleanup/FixMe: Support/Kludges for Tcl v8.4 are to be removed.  Only Tcl v8.5+ is supported.

# This is just a way to count how many times each proc is called:
if {([info exists ::tcldrop(proc_counter)] && $tcldrop(proc_counter)) || [info exists ::env(proc_counter)] && $::env(proc_counter)} {
	rename proc ::tcl::Proc
	::tcl::Proc proc {name arglist body} { uplevel [list ::tcl::Proc $name $arglist "incr ::tcl::proc_counter([string trimright [namespace current] {:}]::${name})\n$body"] }
}

# It's unusual to start Tcldrop with core/core.tcl directly and without first setting up some procs and variables,
# but in case we were started without this basic stuff, we try to deal with it here:
namespace eval ::tcldrop {
	# Stub commands, in case they don't already exist:
	if {![llength [info commands stdout]]} { proc stdout {text} { if {[catch { puts $text }]} { stderr $text } } }
	if {![llength [info commands stderr]]} { proc stderr {text} { if {[catch { puts stderr $text }]} { stdout $text } } }
	if {![llength [info commands PutLogLev]]} {
		proc PutLogLev {levels channel text} {
			switch -glob -- $levels {
				{*e*} { catch { stderr "[clock format [clock seconds] -format {[%H:%M]}] $channel $text" } }
				{default} { catch { stdout "[clock format [clock seconds] -format {[%H:%M]}] $channel $text" } }
			}
		}
	}
	# FixMe: the Tcldrop/tcldrop commands should be namespace ensembles instead.. Oh, and they need a purpose too. =P
	if {![llength [info commands Tcldrop]]} { proc Tcldrop {args} { namespace eval ::tcldrop $args } }
	if {![llength [info commands tcldrop]]} { proc tcldrop {args} { namespace eval ::tcldrop $args } }
	namespace export Tcldrop tcldrop PutLogLev stdout stderr
	if {![info exists ::tcldrop]} {
		PutLogLev e * "Warning: The ::tcldrop array isn't set!\nPlease start Tcldrop using ./tcldrop or by source'ing tcldrop.tcl from within a Tcl-enabled application or by setting up the ::tcldrop array yourself before source'ing core.tcl.\nSetting up the ::tcldrop array with internal defaults for now...  =/"
		array set ::tcldrop [list botname {} userfile-create 0 dirname . channel-stats 0 config tcldrop.conf background-mode 0 host_env tclsh version $version numversion 0 config-eval {} simulate-dcc 1 author {Tcldrop-Dev} name {Tcldrop} depends {Tcl} description {Tcldrop, the Eggdrop-like IRC bot written in pure-Tcl.} rcsid {} commands [list Tcldrop tcldrop PutLogLev stdout stderr] script {}]
		namespace export {*}$::tcldrop(commands)
	}
	set ::modules(tcldrop) [array get ::tcldrop]
	set ::version [list $::tcldrop(version) $::tcldrop(numversion)]
	namespace unknown unknown
}

# This is helpful to update the screen when running in wish for some reason:
update idletasks

# Here begins the "core" module:
namespace eval ::tcldrop::core {
	variable version {0.6.2}
	variable name {core}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::${name} $version
	package provide tcldrop::${name}::main $version
	variable depends {tcldrop}
	variable author {Tcldrop-Dev}
	variable description {Provides all the core components.}
	variable rcsid {$Id$}
	namespace export addlang addlangsection bgerror bind bindlist binds calldie callevent calltime calltimer callutimer checkflags checkmodule countbind ctime decimal2ip dellang dellangsection detectflood dict die duration timeago encpass exit fuzz getbinds gettimerinfo help ip2decimal isbotnetnick killtimer killutimer lang language lassign loadhelp loadmodule logfile lrepeat maskhost mergeflags moduleloaded modules moduledeps putcmdlog putdebuglog puterrlog putlog putloglev putxferlog rand randstring rehash relang reloadhelp reloadmodule restart setdefault settimerinfo slindex sllength slrange strftime string2list stripcodes timer timerinfo timers timerslist unames unbind unixtime unloadhelp unloadmodule utimer utimers utimerslist validtimer validutimer protected counter unsetdefault isrestart shutdown getlang langsection langloaded defaultlang adddebug uptime know afteridle lprepend ginsu wrapit
	variable commands [namespace export]
	namespace unknown unknown
	namespace import -force {::tcldrop::*}
	namespace path [list ::tcldrop]
	# Setting these variables here protects them from ever being deleted during a restart later:
	array set ::protected {}
	array set ::timers {}
	array set ::binds {}
	array set ::help {}
	array set ::help-files {}
	array set ::lang {}
	variable TimerIDCount 0
	variable Flood
	array set Flood {}
	set ::modules(core) [list name $name version $version depends $depends author $author description $description rcsid $rcsid commands $commands script $script]
	#package prefer latest
	# Set a default mod-path if it's not already set:
	if {![info exists ::mod-path]} { set ::mod-path {./modules} }
	if {![info exists ::mod-paths]} { set ::mod-paths [list [file join / usr lib tcldrop modules] [file join / usr share tcldrop modules] [file join / usr local lib tcldrop modules] [file join / usr local share tcldrop modules] [file join $::env(HOME) lib tcldrop modules] [file join $::env(HOME) share tcldrop modules] [file join . modules] [file join $::tcldrop(dirname) modules] ${::mod-path}] }
	# Add to the paths to search for Tcl Modules:
	foreach m ${::mod-paths} {
		if {[file isdirectory $m]} {
			::tcl::tm::path add $m
			# Set the "official" Tcldrop mod-path:
			set ::mod-path $m
		}
	}
	# Add to the paths to search for Tcl packages:
	foreach m [list lib scripts [file join $::tcldrop(dirname) lib] [file join $::tcldrop(dirname) scripts] [file join $::env(HOME) lib tcldrop lib] [file join $::env(HOME) lib tcldrop scripts] [file join $::env(HOME) share tcldrop lib] [file join $::env(HOME) share tcldrop scripts] [file join / usr local lib tcldrop lib] [file join / usr local lib tcldrop scripts] [file join / usr local share tcldrop lib] [file join / usr local share tcldrop scripts] [file join / usr lib tcldrop lib] [file join / usr lib tcldrop scripts] [file join / usr share tcldrop lib] [file join / usr share tcldrop scripts] [file join / usr share tcltk tcl[info tclversion]] [file join / usr local share tcltk tcl[info tclversion]] [file join / usr local lib tcltk] [file join / usr local share tcltk] [file join / usr lib tcltk] [file join / usr share tcltk]] {
		if {[file isdirectory $m]} { if {$m ni $::auto_path} { lappend ::auto_path $m } }
	}
	unset m
}

# Provided by Dossy@EFNet:
# Contains bugs: 1. It cuts words longer than $maxlen.  2. It only supports maxlen's up to 255.
proc ::tcldrop::core::ginsu {string {maxlen {255}}} { regexp -all -inline -- "\\S.{0,$maxlen}(?!\\S)" $string }

# Provided by leprechau@EFNet:
# Wraps text neatly without breaking words.  (long words can exceed the max line length though)
proc ::tcldrop::core::wrapit {text {len 80}} {
	if {[string length $text] > $len} {
		set list [split $text]
		set x [set y 0]
		for {set i 0} {$i <= [llength $list]} {incr i} {
			if {[string length [set tmp [join [lrange $list $x $y]]]] < $len} {
				incr y
			} else {
				lappend outs $tmp
				set x [incr y]
			}
		}
		if {[info exists outs]} {
			if {[string length $text] != [string length [join $outs]]} { lappend outs $tmp }
			return $outs
		}
	} else {
		list $text
	}
}

# "Let unknown know" from http://wiki.tcl.tk/2776
proc ::tcldrop::core::know {what} {
	if {![info complete $what]} {
		return -code error "Incomplete command(s): $what"
	} else {
		proc unknown {args} "$what\n[info body unknown]"
	}
}
# Command: v: info patchlevel
# Results: v = info patchlevel
#::tcldrop::core::know { if {[regexp -- (.+):$ [lindex $args 0] -> name]} { return [lreplace $args 0 0 $name =] } }
# Command: 1 + 1
# Results: 2
::tcldrop::core::know { if {![catch { expr $args } res]} { return $res } }
# Command: set i 4 ; i--
# Results: 3
::tcldrop::core::know { if {[regexp -- {^([^ ]+)--$} $args -> vname]} { return [uplevel [list incr $vname -1]] } }
# Command: i++
# Results: 4
::tcldrop::core::know { if {[regexp -- {^([^ ]+)\+\+$} $args -> vname]} { return [uplevel [list incr $vname]] } }
# Command: 1..9
# Results: 1 2 3 4 5 6 7 8 9
::tcldrop::core::know {
	if {[regexp {^([0-9]+)\.\.([0-9]+)$} [lindex $args 0] -> from to]} {
		set res {}
		while {$from <= $to} {
			lappend res $from
			incr from
		}
		return $res
	}
}

proc ::tcldrop::core::unixtime {} { clock seconds }

if {![llength [info commands rand]]} { proc ::tcldrop::core::rand {maxint {minint {0}}} { expr { int($minint + rand() * ($maxint - $minint)) } } }

proc ::tcldrop::core::fuzz {number {percent {100}}} { expr { int(rand() * $percent / 100.0 * $number) + $number } }

if {![llength [info commands strftime]]} {
	proc ::tcldrop::core::strftime {format {time {}}} {
		if {$time eq {}} { set time [clock seconds] }
		clock format $time -format $format
	}
}

if {![llength [info commands ctime]]} { proc ::tcldrop::core::ctime {time {format {%a %b %e %T %Y}}} { clock format $time -format $format } }

proc ::tcldrop::core::unames {args} { return "$::tcl_platform(os) $::tcl_platform(osVersion)" }

# Null password encryption.. Used only when an encryption module isn't (yet) loaded:
#proc ::tcldrop::core::encpass {password args} { set password }

proc ::tcldrop::core::protected {type args} { set ::protected($type) [lsort -unique [concat $::protected($type) $args]] }

# Sets global variables, being careful not to overwrite any that may already exist.
# This is mainly used after loading the config file, to fill in the gaps that the config may leave.
proc ::tcldrop::core::setdefault {var {value {}} args} {
	array set options [list -protect 0 -array 0]
	array set options $args
	if {$options(-protect) && [lsearch -exact $::protected(globals) $var] == -1} { lappend ::protected(globals) $var }
	if {$options(-array)} {
		# Set an array to default value:
		array set $var {}
		foreach {n v} $value { if {![info exists $var($n)]} { set var($n) $v } }
	} elseif {[info exists "::$var"]} {
		# Already exists, so just return whatever it's currently set to:
		set "::$var"
	} else {
		# Set the variable to the default value:
		set "::$var" $value
	}
}

# Unsets a global variable:
proc ::tcldrop::core::unsetdefault {var args} {
	if {[set pos [lsearch -exact $::protected(globals) $var]] != -1} { set ::protected(globals) [lreplace $::protected(globals) $pos $pos] }
	#if {[info exists "::$var"]} { unset "::$var" }
}

#proc ::tcldrop::core::islist {s} { expr { ![catch { llength $s }] } }
# Deprecated/FixMe: Replace any/all references to islist with string is list
proc ::tcldrop::core::islist {s} { string is list $s }

# Note: string2list and slindex/slrange/sllength are used so that we can ignore extra white space in strings and still deal with special characters.
#       Please use this carefully, and don't use it anywhere where you need to preserve whitespace.
proc ::tcldrop::core::string2list {string} { regexp -inline -all -- {\S+} $string }
proc ::tcldrop::core::slindex {string index} { lindex [regexp -inline -all -- {\S+} $string] $index }
proc ::tcldrop::core::slrange {string {start {0}} {end {end}}} { lrange [regexp -inline -all -- {\S+} $string] $start $end }
proc ::tcldrop::core::sllength {string} { regexp -all -- {\S+} $string }

proc ::tcldrop::core::randstring {length {chars abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789}} {
	set count [string length $chars]
	# Note: while is faster than for.
	set index 0
	while {$index < $length} {
		append result [string index $chars [expr {int(rand()*$count)}]]
		incr index
	}
	return $result
}

# duration, based on http://wiki.tcl.tk/789, modified by fedex to support years and weeks, added features from http://inferno.slug.org/wiki/Duration with speed tweaks by me (FireEgl).
if {![llength [info commands duration]]} {
	proc ::tcldrop::core::duration {seconds args} {
		# avoid OCTAL interpretation, deal with negatives, split floats, handle things like .3
		lassign [split [string trimleft $seconds {-0}] {.}] seconds fraction
		if {![string length $seconds]} { set seconds 0 }
		set timeatoms [list]
		if {![catch {
			foreach div {31449600 604800 86400 3600 60 1} mod {0 52 7 24 60 60} name {year week day hour minute second} {
				if {[lsearch -glob $args "-no${name}*"] != -1} { break }
				set n [expr {$seconds / $div}]
				if {$mod > 0} { set n [expr {$n % $mod}] }
				if {$n > 1} { lappend timeatoms "$n ${name}s" } elseif {$n == 1} { lappend timeatoms "$n $name" }
			}
		}]} {
			if {[info exists fraction] && [string length $fraction]} { if {!$n} { lappend timeatoms "0.$fraction seconds" } else { set timeatoms [lreplace $timeatoms end end "$n.$fraction seconds"] } }
			if {[llength $timeatoms]} { join $timeatoms {, } } else { return {0 seconds} }
		}
	}
}

# timeago, like in RacBots:
proc ::tcldrop::core::timeago {unixtime} { duration [expr { [clock seconds] - $unixtime }] }

proc ::tcldrop::core::lprepend {varName args} {
	upvar 1 $varName var
	# Ensure that the variable exists and contains a list
	lappend var
	# Now we insert all the arguments in one go (yes, I know this looks crazy, but it's the fastest way in Tcl 8.5, see http://wiki.tcl.tk/1482)
	set var [linsert $var [set var 0] {*}$args]
}

# Sets the default language:
proc ::tcldrop::core::addlang {{language {english}}} {
	# Set this language as the default:
	set ::language $language
	# Support loading multiple languages:
	# If the language was already set,
	if {[set pos [lsearch -exact $::languages $language]] != -1} {
		# then remove it from the list and then prepend it back to the front of the list:
		set ::languages [lreplace [lreplace $::languages $pos $pos] -1 -1 $language]
	} else {
		# else just prepend it:
		set ::languages [lreplace $::languages -1 -1 $language]
	}
	relang [list $language]
}

# Note: This command is obsolete in Eggdrop (it only gives an error in Eggdrop v1.6.18+):
# So we try to make it do something useful, although it should remain undocumented:
# FixMe: Turn this into a namespace ensemble command:
proc ::tcldrop::core::language {command args} {
	switch -- $language {
		{lang} - {-lang} - {getlang} - {-getlang} - {addlang} - {-addlang} - {relang} - {-relang} - {dellang} - {-dellang} - {langloaded} - {-langloaded} - {addlangsection} - {-addlangsection} - {dellangsection} - {-dellangsection} - {defaultlang} - {-defaultlang} - {langsection} - {-langsection} { [string trimleft $command {-}] {*}$args }
		{isloaded} - {-isloaded} - {loaded} - {-loaded} { langloaded [lindex $args 0] }
		{languages} - {-languages} - {all} - {-all} { return $::languages }
		{language} - {-language} - {-default} - {} { return $::language }
		{default} {
			# On old Eggdrops, addlang is all this command used to do (I think):
			addlang $command [lindex $args 0]
		}
	}
}

proc ::tcldrop::core::langloaded {{language {*}}} { if {[llength [array names ::lang *,*,$language]]} { return 1 } else { return 0 } }

# + defaultlang <language>
# Module Required: none
# Returns: nothing
# Description:
# This command sets <language> to be the default language in
# the Bot's language system. It will load any language file
# in $lang-dir that matches the mask
# "<section>.<language>.lang". <section> is any valid section
# defined by the Tcl command "langsection" or builtin sections.
# (Taken from http://www.racbot.org/docs/tclcmds/scripting_tcl_commands.html)
proc ::tcldrop::core::defaultlang {{language {english}}} {
	# Set $language as the default:
	set ::language $language
	if {![langloaded $language]} { addlang $language }
}

# Reloads the previously loaded .lang files:
# (Note: on Eggdrop it doesn't allow any arguments.)
proc ::tcldrop::core::relang {{languages {}} {sections {}}} {
	if {$languages eq {}} { set languages [lreverse $::languages] }
	if {$sections eq {}} { set sections ${::lang-sections} }
	foreach l $languages {
		foreach s $sections {
			dellangsection $s $l
			addlangsection $s $l
		}
	}
}

# Loads a section:
proc ::tcldrop::core::addlangsection {section {language {}}} {
	if {$language eq {}} { set language $::language }
	global lang lang-path lang-sections
	set retval 0
	# Loading Eggdrop's .lang's first, and then ours, so that ours can override Eggdrops if needed.
	foreach langfile [list [file join ${lang-path} eggdrop "${section}.${language}.lang"] [file join ${lang-path} "${section}.${language}.lang"]] {
		if {[file readable $langfile] && ![catch { open $langfile r } fid]} {
			set continued 0
			while {[gets $fid line] >= 0} {
				if {[string equal [string index $line 0] {#}]} {
					# Skip comments.
					continue
				} elseif {[string equal [string index $line end] "\\"]} {
					# Lines ending in \ mean they're continued to the next line.
					set text [subst -nocommands -novariables [string range $line 0 end-1]]
				} else {
					set text [subst -nocommands -novariables $line]
				}
				if {!$continued} {
					set id [string trimright [string range $text 0 [set separator [string first {,} $text]]] {,}]
					set text [string trimleft [string range $text $separator end] {,}]
					set lang($id,$section,$language) $text
				} else {
					append lang($id,$section,$language) [subst -nocommands -novariables $text]
				}
				# Lines ending in \ mean they're continued to the next line.
				set continued [string equal [string index $line end] "\\"]
			}
			close $fid
			set retval 1
		}
	}
	if {$retval} { if {[lsearch -exact ${lang-sections}] == -1} { lappend lang-sections $section } }
	return $retval
}

# Unloads a section:
proc ::tcldrop::core::dellangsection {{section {*}} {language {*}}} {
	array unset ::lang *,$section,$language
	while {[set pos [lsearch -glob ${::lang-sections} $section]] != -1} {
		set ::lang-sections [lreplace ${::lang-sections} $pos $pos]
	}
}

# Removes a language and deletes the sections loaded for it:
proc ::tcldrop::core::dellang {{language {*}} {section {*}}} { dellangsection $section $language }

# Used to display a language id (Tcldrop-specific):
proc ::tcldrop::core::lang {id {section {*}} args} {
	if {[llength $args] == 0} { set args $::languages }
	foreach language $args { foreach a [array names ::lang $id,$section,$language] { return $::lang($a) } }
	return "NOLANG-$id,$section for any of languages: [join $args {, }]"
}

# + getlang <tcl-script>|<module> <index> <language> [<var1> <var2> ...]
# Module Required: none
# Returns: A formatted language line, otherwise "MSG-0-"
# Description:
# This command returns a formatted language line.
# <Tcl-script>|<module> is the section the language line is
# located under, <index> is the language index, and <language>
# is the language you want the line in. You may also be
# required to provide a list of variables (%A etc) to
# substitute into the line.
# (Taken from http://www.racbot.org/docs/tclcmds/scripting_tcl_commands.html)
proc ::tcldrop::core::getlang {section id {language {*}} args} {
	if {$language eq {*}} {
		set languages $::languages
	} elseif {[lsearch -exact $::languages $language] == -1} {
		addlang $language
		set languages [list $language]
	} else {
		set languages [list $language]
	}
	foreach l $languages {
		foreach a [array names ::lang $id,$section,$l] {
			# FixMe: Do % variable substitution here, replacing them with what's in $args
			return $::lang($a)
		}
	}
	return "NOLANG-$id,$section,$language"
}

# + langsection <tcl-script> <section>
# Module Required: none
# Returns: nothing
# Description:
# This command defines a section of language to be loaded
# into the language system. This command is used to load
# language files for Tcl scripts. <Tcl-script> is the Tcl-
# script the language is associated with and <section> is
# the first part of the filename. I.e. <section>.english.lang
# (Taken from http://www.racbot.org/docs/tclcmds/scripting_tcl_commands.html)
proc ::tcldrop::core::langsection {args} { foreach a $args { addlangsection $args } }

# FixMe: Allow format codes to be part of the filename..That way logfile rotation could happen anytime the result of the format changes..
proc ::tcldrop::core::logfile {{levels {*}} {channel {*}} {filename {}}} {
	variable Logfiles
	if {$filename != {}} {
		# See if we're already logging to this file:
		foreach a [array names Logfiles *,*,$filename] {
			# If we're already logging to this filename, just update the levels and channel to match what's specified:
			unbind log [dict get $Logfiles($a) levels] [dict get $Logfiles($a) channel] ::tcldrop::core::LOG
			dict set Logfiles($a) levels $levels
			dict set Logfiles($a) channel $channel
			bind log $levels $channel ::tcldrop::core::LOG
			return $filename
		}
		if {[array size Logfiles] >= ${::max-logs}} {
			return -code error "Maximum number of logs are already open (${::max-logs})."
		} else {
			set Logfiles($levels,[string tolower $channel],$filename) [dict create levels $levels channel $channel filename $filename fileid [open $filename a]]
			bind log $levels $channel ::tcldrop::core::LOG
			return $filename
		}
	} else {
		# Return a list of
		lappend loglist
		foreach a [array names Logfiles $levels,[string tolower $channel]] {
			array set loginfo $Logfiles($a)
			lappend loglist [list $loginfo(levels) $loginfo(channel) $loginfo(filename)]
		}
		set loglist
	}
	# Returns: filename of logfile created, or, if no logfile is specified,
	# a list of logfiles such as: {mco * eggdrop.log} {jp #lame lame.log}
}

# FixMe: Finish writing this.
proc ::tcldrop::core::LOG {levels channel text} {
	variable Logfiles
	foreach a [array names Logfiles $levels,[string tolower $channel],*] {
		switch -glob -- $filename {
			{log://*} {
				# Note: This will be my (FireEgl) new log protocol.. It will likely be UDP based for reduced overhead.
			}
			{ftp://*} {
				# Note: This will log directly to a remote file via FTP.
			}
			{http://*} {
				# Note: This will be a standard HTTP POST.
			}
			{https://*} {
				# Note: This will use https instead of http.
			}
			{default} {
				# Note: This is for local files.
				puts [set fileid [dict get $Logfiles($a) fileid]] $text
				if {!${::quick-logs}} { flush $fileid }
				# FixMe: Should this check the file size every damn time we log something?  Perhaps making this an hourly or even daily event would be good enough..
				# if {!${::keep-all-logs} && [file size [dict get $Logfiles($a) filename]] / 1024 >= ${::max-logsize}} {
					# FixMe.
				# }
			}
		}
	}
}

### These are the levels (aka modes) that are used both by logfiles and consoles:
# c  display user commands (dcc and msg) (n)
# o  display other bot notices [HIGHLY RECOMMENDED] (mnt)
# x  display file transfers and file-area commands (jn)
# d  display debug messages that only coders would care about (n)
# r  display all raw text from the server (if enabled) (n)
# v  display raw text SENT to the server (if enabled) (n)
# m  display private msgs/ctcps to the bot (n)
# p  display public talk and ctcps on the channel (nmofv)
# k  display kicks/bans/mode changes on the channel (nmofv)
# j  display joins/parts/nick changes/signoffs/etc on the channel (nmofv)
# b  display bot links/unlinks/userfile-sharing (ntm)
# h  raw share traffic (n)
# t  raw botnet traffic (nt)
# s  display server messages and connect/disconnects (nm)
# w  display msgs between IRCops (wallops) (n)
# e  display full Tcl errors (n)
# -- There are also 8 user-defined console modes '1' through '8' --

# Sends $text to all the places monitoring $levels.
# Use * for all levels.
# Use * for all channels, or - to specify that it's global-only (non-channel related).
proc ::tcldrop::core::putloglev {levels channel text} {
	switch -- ${::log-time} {
		{1} { set text "[clock format [clock seconds] -format {[%H:%M]}] $text" }
		{2} { set text "[clock format [clock seconds] -format {[%T]}] $text" }
		{0} - {} - { } { }
		{default} {
			# Use a custom clock format.. Should this use a separate variable instead?
			set text "[clock format [clock seconds] -format ${::log-time}] $text"
		}
	}
	# Call all of the LOG binds here:
	foreach {type flags mask proc} [bindlist log] {
		if {[string match -nocase $mask $channel] && [checkflags $flags $levels]} {
			if {[catch { $proc $levels $channel $text } err]} {
				# Note: We log to PutLogLev, because it avoids recursive put*log errors..
				catch { PutLogLev edo $channel "LOG ERROR $proc $levels $channel: $err\n$::errorInfo" }
				# put*log errors are considered fatal errors, so we really should exit:
				if {![catch { exit 1 }]} { update idletasks }
				catch { ::tcldrop::core::Exit 1 }
				return -code error "putloglev $levels $channel error: $err"
			}
			# For speed, we don't count the log binds:
			#countbind $type $mask $proc
		}
	}
	#update idletasks
}

proc ::tcldrop::core::putlog {text {channel {*}}} { putloglev o $channel $text }
proc ::tcldrop::core::putcmdlog {text {channel {*}}} { putloglev c $channel $text }
proc ::tcldrop::core::putxferlog {text {channel {*}}} { putloglev x $channel $text }
proc ::tcldrop::core::puterrlog {text {channel {*}}} { putloglev e $channel $text }
proc ::tcldrop::core::putdebuglog {text {channel {*}}} { putloglev d $channel $text }

# adddebug <output>
# Module Required: none
# Returns: nothing
# Description:
# This command can be used to add a entry to the debug console/log.
# (Taken from http://www.racbot.org/docs/tclcmds/scripting_tcl_commands.html)
proc ::tcldrop::core::adddebug {output} { if {[catch { putdebuglog $output - }]} { catch { PutLogLev deo - "ERROR/DEBUG: $output" } } }

if {![llength [info commands bgerror]]} {
	proc ::tcldrop::core::bgerror {{error {}} args} {
		# Try to report the error to the proper place, with lots of fallbacks:
		if {[catch { putloglev e * [set errorinfo "(bgerror) $error:\n$::errorInfo"] }] && [catch { PutLogLev e - $errorinfo }] && [catch { puts stderr $errorinfo }] && [catch { puts stdout $errorinfo }] && [catch { die $errorinfo }]} {
			# Kill ourself if we can't report the error to anywhere..
			exit 1
		}
	}
	if {[catch { interp bgerror {} ::tcldrop::core::bgerror }]} { interp alias {} bgerror {} ::tcldrop::core::bgerror }
}

# Flag handling, returns 1 if there's a match, 0 if there's not.
# $flags2's flags should match at least one of the flags in $flags1:
# Example: checkflags ab|c&d|fgh zcxd
# Returns: 1
proc ::tcldrop::core::checkflags {flags1 flags2} {
	switch -- $flags1 {
		{+} - {*} - {} - {+|+} - {*|*} - {-} { return 1 }
		{default} {
			set found 0
			foreach o [split $flags1 {|}] {
				foreach a [split $o {&}] {
					set found 0
					foreach f [split $a {}] {
						if {[string match "*$f*" $flags2]} {
							set found 1
							break
						}
					}
					if {!$found} { break }
				}
				if {$found} { return $found }
			}
			return $found
		}
	}
}

# Merge $flags1 with $flags2
# $flags1 should be (eg): +ofv-nm
# $flags2 should be (eg): nfmjxvp
# It Returns what you would expect, (eg): ofjvxp
proc ::tcldrop::core::mergeflags {flags1 flags2} { set add 1
	foreach f [split $flags1 {}] {
		switch -- $f {
			{+} { set add 1 }
			{-} { set add 0 }
			{ } - {} - {	} {}
			{default} {
				if {$add} {
					# Add $f to $flags2 if it's not already there.
					if {![string match "*$f*" $flags2]} { append flags2 $f }
				} else {
					# Remove $f from $flags2 if it is there.
					set flags2 [string map [list $f {}] $flags2]
				}
			}
		}
	}
	set flags2
}

proc ::tcldrop::core::ip2decimal {ip} {
	lassign [split $ip .] a b c d
	format %u 0x[format %02X%02X%02X%02X $a $b $c $d]
}
proc ::tcldrop::core::decimal2ip {ip} {
	set ip [format %08X $ip]
	return "[format %u 0x[string range $ip 0 1]].[format %u 0x[string range $ip 2 3]].[format %u 0x[string range $ip 4 5]].[format %u 0x[string range $ip 6 7]]"
}

proc ::tcldrop::core::stripcodes {strip-flags string} {
	foreach o [split ${strip-flags} {}] {
		switch -- $o {
			{a} { return [regsub -all -- {\003[0-9]{0,2}(,[0-9]{0,2})?|\017|\037|\002|\026|\007} $string {}] }
			{b} { lappend strip "\002" }
			{r} { lappend strip "\026" }
			{u} { lappend strip "\037" }
			{g} { lappend strip "\007" }
			{p} { lappend strip "\017" }
			{c} { lappend strip "\003\[0-9\]{0,2}(,\[0-9\]{0,2})?" }
			{a} { lappend strip "\033\[.*m" }
			{-} - {+} - { } - {	} {}
			{default} { return -code error "Unknown strip option: $o" }
		}
	}
	if {[info exists strip]} {
		regsub -all -- [join $strip {|}] $string {}
	} elseif {![info exists o]} {
		regsub -all -- {\003[0-9]{0,2}(,[0-9]{0,2})?|\017|\037|\002|\026|\007} $string {}
	} else {
		set string
	}
}

# Used to define bind's, works just like Eggdrop's bind command.
# Tcldrop also allows these extra (optional) options in $args:
# -priority <1-99>    This defines the order of priority. (lower gets processed first)
#                     Default is 50.  Priorities <0 and >100 are reserved for Tcldrop internal use.
proc ::tcldrop::core::bind {type flags mask proc args} {
	# Note/FixMe: Eggdrop checks to make sure $type is a valid bind type before accepting it, but currently I don't see why that's such a great idea.
	switch -- $flags {
		{-} - {+} - {*} - {-|-} - {*|*} - {|} - {} - { } - {	} { set flags {+|+} }
		{default} { if {![string match {*|*} $flags]} { set flags "$flags|-" } }
	}
	set options [dict create -priority 50 type $type flags $flags mask $mask proc $proc count 0 {*}$args]
	set ::binds($type,[dict get $options -priority],$proc,$mask) $options
	set mask
}

# FixMe: Make $priority part of $args:
proc ::tcldrop::core::unbind {type flags mask proc {priority {*}} args} {
	array unset ::binds [string tolower $type],$priority,$proc,[string tolower $mask]
}

proc ::tcldrop::core::binds {{typemask {*}} {mask {*}}} {
	set matchbinds {}
	global binds
	# Search by type:
	foreach b [lsort [array names binds [string tolower $typemask],*,*,[string tolower $mask]]] {
		dict with binds($b) { lappend matchbinds [list $type $flags $mask $count $proc] }
	}
	# FixMe: Searching for masks this way is WAY too slow:
	#if {[llength $matchbinds] == 0} {
	#	# If none were found by type, we search by mask:
	#	# FixMe: This is too slow!
	#	foreach b [lsort -dictionary [array names binds]] {
	#		array set bind $binds($b)
	#		if {[string equal -nocase $typemask $bind(mask)]} {
	#			lappend matchbinds [list $bind(type) $bind(flags) $bind(mask) $bind(count) $bind(proc)]
	#		}
	#	}
	#}
	#if {[info exists matchbinds]} { return $matchbinds } else { list }
	return $matchbinds
}

# FixMe: Add support for options, such as -exact 1/0 and -matchmask <arg> (which should match the $bind(mask)).
proc ::tcldrop::core::bindlist {{typemask {*}} {mask {*}}} {
	set matchbinds {}
	global binds
	# Search by type:
	foreach b [lsort [array names binds [string tolower $typemask],*,*,[string tolower $mask]]] {
		dict with binds($b) { lappend matchbinds $type $flags $mask $proc }
	}
	return $matchbinds
}

proc ::tcldrop::core::getbinds {{typemask {*}} {mask {*}}} {
	set matchbinds {}
	global binds
	# Search by type:
	foreach b [lsort [array names binds [string tolower $typemask],*,*,[string tolower $mask]]] { lappend matchbinds $binds($b) }
	return $matchbinds
}

# Counts how many times a bind has been triggered:
proc ::tcldrop::core::countbind {type mask proc {priority {*}}} {
	global binds lastbind
	set lastbind $mask
	foreach name [array names binds [string tolower $type],$priority,$proc,[string tolower $mask]] {
		dict incr binds($name) count
		return 1
	}
	return 0
}

# Provides a unique timerID:
proc ::tcldrop::core::TimerID {args} { variable TimerIDCount
	return "timer[incr TimerIDCount]_[join $args -]"
}

# Executes a command that was set with timer/utimer, and does a killtimer on it (to remove the data from the array)
# Also, if the -repeat option was given to timer/utimer, then we start another timer for it.
proc ::tcldrop::core::DoTimer {timerid} {
	if {[info exists ::timers($timerid)]} {
		foreach initcmd [dict get $::timers($timerid) initcommands] {
			if {[catch { uplevel #0 $initcmd } err]} {
				putlog "Tcl error while running initcmd for '$timerid': $err"
				puterrlog $::errorInfo
			}
		}
		if {[catch { uplevel #0 [dict get $::timers($timerid) fullcommand] } err]} {
			putlog "Tcl error in script for '$timerid':\n$err"
			puterrlog $::errorInfo
			killtimer $timerid
		} else {
			if {[dict get $::timers($timerid) repeat] == 0} {
				# It's not set to repeat, so remove the timers data:
				killtimer $timerid
			} else {
				if {[dict get $::timers($timerid) repeat] > 0} { dict incr ::timers($timerid) repeat -1 }
				dict set ::timers($timerid) executetime [expr {[clock seconds] + ([dict get $::timers($timerid) interval] / 1000)}]
				dict set ::timers($timerid) afterid [after [dict get $::timers($timerid) interval] [list ::tcldrop::core::DoTimer $timerid]]
			}
		}
	}
}

# Called whenever a timer/utimer is _created_:
proc ::tcldrop::core::calltimer {timerinfo} {
	foreach {type flags mask proc} [bindlist timer] {
		countbind $type $mask $proc
		if {[catch { $proc $timerinfo } err]} {
			putlog "Error in $proc: $err"
			puterrlog "$::errorInfo"
		}
	}
}
proc ::tcldrop::core::callutimer {timerinfo} { calltimer $timerinfo }

# if $repeat is 0 (the default) then the command only executes once.
# if $repeat is 1 or higher then the command repeats that many MORE times, every $seconds seconds.
# if $repeat is -1 then it repeats forever.
proc ::tcldrop::core::utimer {seconds command {repeat {0}}} {
	calltimer [set ::timers([set timerid [TimerID $seconds [lindex $command 0] $repeat]]) [dict create timerid $timerid interval [set interval [expr { $seconds * 1000 }]] afterid [after $interval [list ::tcldrop::core::DoTimer $timerid]] executetime [expr { [clock seconds] + $seconds }] repeat $repeat command [lindex $command 0] args [join [lrange $command 1 end]] fullcommand $command initcommands [list]]]
	return $timerid
}

proc ::tcldrop::core::timer {minutes command {repeat {0}}} { utimer [expr {$minutes * 60}] $command $repeat }

# Note, timers and utimers are returned together.
proc ::tcldrop::core::timers {{timerid {*}}} {
	global timers
	set timerlist {}
	foreach t [array names timers $timerid] {
		dict with timers($t) { lappend timerlist [list [expr {($executetime - [clock seconds]) / 60}] $command $timerid] }
	}
	return $timerlist
}

# Note, utimers and timers are returned together.
proc ::tcldrop::core::utimers {{timerid {*}}} {
	global timers
	set utimerlist {}
	foreach t [array names timers $timerid] {
		dict with timers($t) { lappend utimerlist [list [expr {$executetime - [clock seconds]}] $command $timerid] }
	}
	return $utimerlist
}

# Just like [timers], except it returns a flat list (no sub-lists):
proc ::tcldrop::core::timerslist {{timerid {*}}} {
	global timers
	set timerlist {}
	foreach t [array names timers $timerid] {
		dict with timers($t) { lappend timerlist [expr {($executetime - [clock seconds]) / 60}] $command $timerid }
	}
	return $timerlist
}

# Just like [utimers], except it returns a flat list (no sub-lists):
proc ::tcldrop::core::utimerslist {{timerid {*}}} {
	global timers
	set utimerlist {}
	foreach t [array names timers $timerid] {
		dict with timers($t) { lappend utimerlist [expr {$executetime - [clock seconds]}] $command $timerid }
	}
	return $utimerlist
}

proc ::tcldrop::core::killtimer {timerid} {
	if {[info exists ::timers($timerid)]} {
		after cancel [dict get $::timers($timerid) afterid]
		unset ::timers($timerid)
		return 1
	} else {
		return 0
	}
}

#proc ::tcldrop::core::killutimer {timerid} { killtimer $timerid }
interp alias {} ::tcldrop::core::killutimer {} ::tcldrop::core::killtimer

proc ::tcldrop::core::validtimer {timerid} { info exists ::timers($timerid) }

proc ::tcldrop::core::validutimer {timerid} { info exists ::timers($timerid) }

proc ::tcldrop::core::gettimerinfo {timerid} { timerinfo get $timerid }

proc ::tcldrop::core::settimerinfo {timerid info} { timerinfo set $timerid $info }

# FixMe: *yawn* make this use dict commands:
proc ::tcldrop::core::timerinfo {command timerid args} {
	if {[info exists ::timers($timerid)]} { array set timerinfo $::timers($timerid) }
	switch -- $command {
		{get} {
			return $timerinfo([lindex $args 0])
		}
		{set} {
			array set timerinfo $info
			set ::timers($timerid) [array get timerinfo]
		}
		{append} - {lappend} {
			$command timerinfo([lindex $args 0]) [join [lrange $args 1 end]]
			set ::timers($timerid) [array get timerinfo]
		}
		{unset} {
			foreach arg $args { unset timerinfo($arg) }
			if {![info exists arg]} {
				unset ::timers($timerid)
			} else {
				set ::timers($timerid) [array get timerinfo]
			}

		}
		{default} {
			return -code error "Unknown timerinfo sub-command: $command"
		}
	}
}

# Note: This doesn't guaranty that time-specific binds will always be
#       triggered..  For example, if the bot/process is busy for over 60
#       seconds and there was proc was supposed to be trigged during that
#       minute, the proc won't be called.. We'll have skipped over that minute.
#       At any rate, this proc should be as small and as fast as possible...
#       So, I don't believe it should call time binds for times that have
#       already past.  Any script that needs to simply be repeated every so
#       often should use the timer command and its -1 (repeat forever) option.
# FixMe: Add the ability to log the following:
# timer: drift (lastmin=22, now=26)
# timer: drift (lastmin=23, now=26)
# timer: drift (lastmin=24, now=26)
# timer: drift (lastmin=25, now=26)
# (!) timer drift -- spun 4 minutes
proc ::tcldrop::core::calltime {} {
	lassign [set current [clock format [clock seconds] -format {%M %H %e %m %Y}]] minute hour day month year
	foreach {type flags mask proc} [bindlist time] {
		if {[string match $mask $current]} {
			if {[catch { $proc $minute $hour $day $month $year } err]} {
				putlog "Error in $proc: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
		# time binds aren't really time critical, so trigger any other events that are waiting:
		update idletasks
	}
}

# Runs $args after we're truly idle.
proc ::tcldrop::core::afteridle {args} { after idle [list after 0 $args] }

#  maskhost <nick!user@host>
#    Returns: masked hostmask for the string given ("n!u@1.2.3.4" -> "*!u@1.2.3.*",
#      "n!u@lame.com" -> "*!u@lame.com", "n!u@a.b.edu" -> "*!u@*.b.edu")
if {![llength [info commands maskhost]] || [llength [info procs maskhost]]} {
	package require ip
	proc ::tcldrop::core::maskhost {nickuserhost} {
		switch -glob -- $nickuserhost {
			{*!*@*} {
				# Includes the nick.
				set nick {*}
				set user [lindex [split $nickuserhost !@] 1]
				set host [lindex [split $nickuserhost @] end]
			}
			{*@*} {
				set nick {*}
				set user [lindex [split $nickuserhost @] 0]
				set host [lindex [split $nickuserhost @] end]
			}
			{*.*} - {*:*} - {default} {
				set nick {*}
				set user {*}
				set host $nickuserhost
			}
		}
		switch -- [::ip::version $host] {
			{4} {
				# It's an IPv4 IP.
				if {[string match {*.*.*.*} $host]} {
					lassign [split $host .] a b c
					set host "$a.$b.$c.*"
				}
			}
			{6} {
				# It's an IPv6 IP.
				# Note: I don't think it's appropriate to mask an IPv6 IP, since they're almost always static.
			}
			{-1} {
				# It's probably a hostname.
				# Note/FixMe: This doesn't mask long hosts the same as Eggdrop, although it's just as dumb as Eggdrop.
				# Rather than make it exactly like Eggdrop, it should be fixed to be SMARTER than Eggdrop.
				# Like, Eggdrop will mask example.co.uk to *.co.uk
				# But we shouldn't do that!
				if {[string match {*.*.*} $host]} {
					# Only mask those hostnames with at least 2 .'s
					set host "*[string range $host [string first . $host] end]"
				}
			}
			{default} {
				# Unknown response from ::ip::version.
			}
		}
		return "$nick!$user@$host"
	}
}

# FixMe: This may be moved or copied to the bots module:
proc ::tcldrop::core::isbotnetnick {nick} { string equal -nocase $nick ${::botnet-nick} }

# detectflood returns 1 if a flood was detected, or 0 if it wasn't.
# $maxsec is the lines:seconds.
# $args is a way to identify the person.
#
# Examples:
# if {[detectflood 10:60 chan #tcldrop adsl-17-145-128.bhm.bellsouth.net]} { flood detected! }
# if {[detectflood 5:60 dcc 5]} { flood detected! }
#
# Note: Eggdrop supports these flood types: *chan (pub), *msg, *deop, *join, *nick, *kick, *ctcp (channel and personal), dcc, telnet.
proc ::tcldrop::core::detectflood {maxsec args} {
	foreach {max sec} [set maxsec [split $maxsec {: }]] { if {$max == 0 || $sec == 0} { return 0 } }
	variable Flood
	set Flood(maxsec,$args) $maxsec
	if {![info exists Flood(seconds,$args)]} {
		# This is the first time we've seen $args, initialize the seconds list:
		set Flood(seconds,$args) [list [clock seconds]]
		# This basically starts a loop that checks every $sec and deletes old info
		# and eventually will delete the array that stores the info if it
		# doesn't get updated again within $sec seconds:
		after [expr {$sec * 1000 + 1001}] [list ::tcldrop::core::ClearFlood $args]
		return 0
	} else {
		# Detect flood:
		set ndx 0
		foreach s [set Flood(seconds,$args) [lrange [concat $Flood(seconds,$args) [list [set seconds [clock seconds]]]] end-$max end]] {
			if {$sec >= $seconds - $s} {
				set Flood(seconds,$args) [lrange $Flood(seconds,$args) $ndx end]
				break
			} else {
				incr ndx
			}
		}
		if {[llength $Flood(seconds,$args)] >= $max} { return 1 } else { return 0 }
	}
}

proc ::tcldrop::core::ClearFlood {id} {
	variable Flood
	if {[info exists Flood(maxsec,$id)]} {
		set seconds [clock seconds]
		lassign $Flood(maxsec,$id) max sec
		set ndx 0
		foreach s [set Flood(seconds,$id) [lrange $Flood(seconds,$id) end-$max end]] {
			if {[expr { $sec >= $seconds - $s }]} {
				set Flood(seconds,$id) [lrange $Flood(seconds,$id) $ndx end]
				set ndx -1
				break
			} else {
				incr ndx
			}
		}
		if {$ndx == -1} { after [expr {$sec * 1000 + 1001}] [list ::tcldrop::core::ClearFlood $id] } else { array unset Flood *,$id }
	}
}

# The loadmodule and unloadmodule commands MUST be defined here.
# Because package require loads the packages from the global namespace (I think).
# And because [namespace import] imports into the current namespace.
# FixMe: We need to keep up with what modules are loaded, and their versions.
proc ::tcldrop::core::loadmodule {module args} { LoadModule $module $args }
# FixMe: Some parts of this may could be dicts:
proc ::tcldrop::core::LoadModule {module {options {}}} {
	set starttime [clock clicks -milliseconds]
	array set opts [list -version {0} -force {0}]
	array set opts $options
	if {(($opts(-version) > 0) && ([catch { ::package require "tcldrop::${module}" $opts(-version) } err] && [catch { ::package require "tcldrop::${module}::main" $opts(-version) } err])) || ([catch { ::package require "tcldrop::$module" } err] && [catch { ::package require "tcldrop::${module}::main" } err])} {
		putlog "[format [lang 0x209 core]] $module $opts(-version): $err"
		puterrlog "ERROR: $::errorInfo"
		return 0
	} else {
		# Defaults for modinfo:
		array set modinfo [list name $module version $err depends [list] predepends [list] commands [list] rcsid {} script {} author {} description {} namespace "::tcldrop::${module}"]
		# Modules can set ::modules($module) themselves, but for the ones that don't, there's this code:
		foreach i [array names modinfo] { if {[info exists "$modinfo(namespace)::$i"]} { set modinfo($i) [set "$modinfo(namespace)::$i"] } }
		# In case the module set ::modules($module) itself, we use that info:
		if {[info exists ::modules($module)]} { array set modinfo $::modules($module) }
		set ::modules($module) [array get modinfo]
		# predepends modules get loaded before we do any LOAD binds for this module:
		foreach m $modinfo(predepends) { ::tcldrop::core::CheckModule $m $options }
		# depends modules get loaded when we hit the Tcl event-loop next:
		foreach m $modinfo(depends) { if {$m ni $modinfo(predepends)} { after 0 [list ::tcldrop::core::CheckModule $m $options] } }
		if {[namespace exists $modinfo(namespace)]} {
			if {$modinfo(namespace) ni $::protected(namespaces)} { lappend ::protected(namespaces) $modinfo(namespace) }
			namespace eval $modinfo(namespace) {
				# Make sure the module has ::tcldrop in its command search path:
				if {{::tcldrop} ni [set NamespacePath [namespace path]]} {
					namespace path [lappend NamespacePath {::tcldrop}]
					unset NamespacePath
				}
				# Set the unknown command as unqualified (default is ::unknown):
				if {[namespace unknown] eq {}} { namespace unknown unknown }
			}
			# Import the modules' commands into the global namespace.  Modules may run namespace import themselves (either at the end of their script, or from the LOAD bind) (especially if they want to use the -force option), but for the ones that don't, we do it here:
			if {[catch { ::uplevel \#0 [list ::namespace import "$modinfo(namespace)::*"] } err]} { puterrlog "Error importing namespace commands $modinfo(namespace)::* $err" }
			# Import them into the ::tcldrop namespace also, using -force:
			if {[catch { namespace eval ::tcldrop [list namespace import "$modinfo(namespace)::*"] } err]} { puterrlog "Error importing namespace commands $modinfo(namespace)::* $err" }
		}
		# Call the LOAD binds for $module:
		foreach {type flags mask proc} [bindlist load] {
			if {[string match -nocase $mask $module]} {
				if {[catch { $proc $module } err]} {
					putlog "Error in $proc: $err"
					puterrlog "$::errorInfo"
				}
				countbind $type $mask $proc
			}
		}
		# Load the corresponding .lang file:
		# FixMe: Modules should do addlangsections themselves from their own LOAD binds..shouldn't they?
		if {[addlangsection [lindex [split $module :] 0]]} {
			putlog "[format [lang 0x20f core] $module]    (v$modinfo(version), [expr { [clock clicks -milliseconds] - $starttime }]ms)"
		} else {
			putlog "[format [lang 0x210 core] $module]                        (v$modinfo(version), [expr { [clock clicks -milliseconds] - $starttime }]ms)"
		}
		return 1
	}
	return 0
}

proc ::tcldrop::core::checkmodule {module args} { CheckModule $module $args }
proc ::tcldrop::core::CheckModule {module {options {}}} {
	if {[info exists ::modules($module)]} {
		return 1
	} else {
		LoadModule $module $options
	}
}

proc ::tcldrop::core::unloadmodule {{module {*}} args} { UnloadModule $module $args }
proc ::tcldrop::core::UnloadModule {{module {*}} {options {}}} {
	putlog "UnloadModule: $module"
	array set opts [list -force {0}]
	array set opts $options
	set out {}
	global modules
	foreach m [array names modules $module] {
		if {![info exists modules($m)]} { continue }
		foreach d [moduledeps $m] {
			if {![info exists modules($d)]} { continue }
			# FixMe: Prevent loops when 2 modules depend on each other (modules shouldn't depend on each other though).
			switch -- $m {
				$d - {core} - {tcldrop} {}
				{default} { after 0 [list UnloadModule $d $options] }
			}
		}
		set force 0
		foreach {type flags mask proc} [bindlist unld] {
			if {[string match -nocase $mask $m]} {
				if {[catch { $proc $m } force]} {
					putlog "Error in $proc $m: $force"
					puterrlog "$::errorInfo"
					set force 0
				} elseif {[string is int $force]} {
					break
				} else {
					set force 0
				}
				countbind $type $mask $proc
			}
		}
		set success 0
		# If -force is greater or equal, we forget the package (which still leaves it loaded BTW):
		if {$opts(-force) >= $force && ![catch { uplevel #0 [list package forget tcldrop::${m}] }]} {
			lappend msg {package forgot}
			set success 1
		}
		# If -force is greater, we also delete the namespace (which should completely unload it):
		if {$opts(-force) > $force && ![catch { uplevel #0 [list namespace delete "::tcldrop::${m}"] }]} {
			namespace forget "::tcldrop::${m}"
			lappend msg {namespace deleted}
			set success 1
		}
		if {$success} {
			set msg "([join $msg {, }])"
			putlog "[format {%-2.40s %-18.32s %-0.38s} [lang 0x206 core] $m $msg]"
			unset -nocomplain modules($m)
		} else {
			set out [format [lang 0x207 core]]
		}
		set msg [list]
	}
	set out
}

proc ::tcldrop::core::reloadmodule {{module {*}} args} { ReloadModule $module $args }
proc ::tcldrop::core::ReloadModule {{module {*}} {options {}}} {
	UnloadModule $module $options
	LoadModule $module $options
}

proc ::tcldrop::core::modules {{mask {*}}} {
	set modulelist {}
	global modules
	foreach m [array names modules $mask] {
		array set modinfo [list version 0 depends [list]]
		array set modinfo $modules($m)
		lappend modulelist [list $m $modinfo(version) $modinfo(depends)]
	}
	set modulelist
}

proc ::tcldrop::core::moduledeps {module} {
	#putlog "ModuleDeps: $module"
	set deps [list]
	global modules
	foreach m [array names modules] {
		if {[dict exists $modules($m) depends] && [lsearch -exact [dict get $modules($m) depends] $module] != -1} { lappend deps $m }
	}
	return $deps
}

# Tells you if a module is loaded or not:
proc ::tcldrop::core::moduleloaded {module args} { ModuleLoaded $module $args }
proc ::tcldrop::core::ModuleLoaded {module {options {}}} { info exists ::modules($module) }


proc ::tcldrop::core::callevent {event} {
	foreach {type flags mask proc} [bindlist evnt] {
		if {[string equal -nocase $event $mask]} {
			if {[catch { $proc $event } err]} {
				putlog "Error in $proc $event: $err"
				puterrlog "$::errorInfo"
			}
			countbind $type $mask $proc
		}
	}
}

# Loads a .help file.
proc ::tcldrop::core::loadhelp {filename {type {dcc}}} {
	if {![file readable [set filepath [file join ${::help-path} $filename]]] && ![file readable [append filepath .help]]} { return 0 }
	set flags {-}
	set command {unknown}
	global help-path help help-files
	if {![catch { open $filepath r } fid]} {
		putlog "Loading help $filename ..."
		while {[gets $fid line] >= 0} {
			foreach {remove code} [regexp -all -inline -- {\%\{(.*)*?\}} $line] {
				# Process the code inside the %{*}:
				switch -glob -- $code {
					{help=*} { set command [join [lrange [split $code =] 1 end]] }
					{cols=*} { }
					{end} { }
					{[+|-]*} - {?|?} - {?} { set flags [string trimleft $code +] }
					{default} { puterrlog "Unknown help code: $code" }
				}
				# Remove the %{*} stuff from $line
				set line [string map [list $remove {}] $line]
			}
			if {$line != {}} {
				# Store it in the help array:
				lappend help($type,$command,$filename) $flags $line
				if {![info exists help-files($filename)] || [lsearch -exact [set help-files($filename)] "$type,$command,$filename"] == -1} {
					# Associate the $type,$command,$filename with the $filename (so that it can be unloaded):
					lappend help-files($filename) "$type,$command,$filename"
				}
			}
		}
		close $fid
		return 1
	} else {
		return 0
	}
}

# Reloads all the previously loaded .help files.
proc ::tcldrop::core::reloadhelp {args} {
	if {[llength $args] == 0} { set args [array names ::help-files] }
	foreach file $args {
		putlog "Reloading help $file ..."
		if {[unloadhelp $file] && [loadhelp $file]} { putlog "Done!" }
	}
}

# Unloads a .help file.
proc ::tcldrop::core::unloadhelp {args} {
	global help-files help
	if {[llength $args] == 0} { set args [array names help-files] }
	foreach file $args {
		putlog "Unloading help $file ..."
		foreach typecommandfilename [set help-files($file)] {
			array unset help $typecommandfilename
		}
	}
	return 1
}

proc ::tcldrop::core::help {{type {dcc}} {command {help}} {filename {*}}} {
	global help
	set helpinfo [list]
	foreach a [array names help $type,$command,$filename] {
		foreach {f l} $help($a) {
			lappend helpinfo $f $l
		}
	}
	return $helpinfo
}

proc ::tcldrop::core::uptime {} { expr { [clock seconds] - $::uptime } }

# Rename the exit command, so that we can handle exits better:
if {![llength [info commands ::tcldrop::core::Exit]]} {
	rename ::exit ::tcldrop::core::Exit
	proc ::tcldrop::core::exit {{code {0}} {reason {Exit}}} {
		variable Exit $code
		catch { callevent exit }
		catch { ::tcldrop::core::Exit $code }
		catch { file delete -force -- $::pidfile }
		return $code
	}
	interp alias {} exit {} ::tcldrop::core::exit
}

proc ::tcldrop::core::calldie {{reason {die}} {code {0}}} {
	foreach {type flags mask proc} [bindlist die] {
		if {[string match -nocase $mask $reason]} {
			if {[catch { $proc $reason } err]} {
				putlog "Error in $proc $reason: $err"
				puterrlog $::errorInfo
			}
			#countbind $type $mask $proc
		}
	}
}

proc ::tcldrop::core::die {{reason {DIE}} {code {0}}} {
	set ::die $reason
	catch { calldie $reason $code }
	catch { callevent die }
	catch { putlog "* die $code $reason" }
	exit $code
}
# RacBot renamed die to shutdown:
# We use this as a slow/safe shutdown, by allowing idle events to finish first:
proc ::tcldrop::core::shutdown {{reason {SHUTDOWN}} {code {0}}} { after idle [list after 0 [list die $reason $code]] }

# Rehash, just like in Eggdrop, it (re)loads the config.
proc ::tcldrop::core::rehash {{type {}}} {
	set ::rehash $type
	set success 1
	if {$type != {start}} { putlog {Rehashing ...} }
	callevent prerehash
	global tcldrop config
	# tcldrop(config-eval), if it exists, gets eval'd.  This could take the place of, or preceed, the settings in a config file.
	if {$tcldrop(config-eval) != {}} {
		putlog "Evaling \$tcldrop(config-eval) ..."
		if {![catch { uplevel #0 $tcldrop(config-eval) } error]} { set success 1 } else {
			putlog [lang 0x534 core]
			puterrlog $::errorInfo
			set success 0
		}
	}
	if {![info exists config] || $config eq {}} { set config $tcldrop(config) }
	# Check for the config variable, and if it exists, source the config file.
	if {$config != {}} {
		# Search for, and try to load the config file:
		if {[info exists ::env(HOME)]} { set home-path $::env(HOME) } else { set home-path {} }
		set config-path {}
		# These are the search paths that we hope to find $config in:
		foreach p [list {} {.} $tcldrop(dirname) [file join $tcldrop(dirname) ..] [file join $tcldrop(dirname) .. ..] [file join ${home-path} etc] ${home-path} [file join ${home-path} config] [file join ${home-path} configs] [pwd]] {
			if {[file exists [file join $p $config]]} {
				set config-path $p
				break
			}
		}
		if {[file exists [file join ${config-path} $config]]} {
			putlog "Loading Configuration $config ..."
			# The script gets source'd inside the context of this proc without the uplevel command...  O_o
			if {![catch { uplevel \#0 [list source [file join ${config-path} $config]] } error]} { set success 1 } else {
				putlog [lang 0x534 core]
				puterrlog $::errorInfo
				set success 0
			}
		} else {
			putlog [lang 0x534 core]
			set success 0
		}
	}
	if {$success} { callevent rehash } elseif {[catch { die $error 1 }]} { exit 1 }
	unset ::rehash
	set success
}

# Returns 1 if we're in the middle of a restart, 0 if we're not:
proc ::tcldrop::core::isrestart {{type {*}}} { if {[info exists ::restart] && [string match -nocase $type $::restart]} { return 1 } else { return 0 } }

proc ::tcldrop::core::restart {{type {restart}}} {
	variable StartTime
	if {![info exists StartTime] || $type != {start}} { set StartTime [clock clicks -milliseconds] }
	if {[info exists ::restart]} { return $::restart }
	set ::restart $type
	setdefault log-time {2}
	if {$type eq {restart}} { putlog {Restarting ...} }
	setdefault uptime [clock seconds]
	# There's many Eggdrop Tcl scripts that check $::numversion so they can do different things based on the version..
	# So we set numversion to the same as the current Eggdrop v1.6.x version, since that's the version we're most compatible with:
	setdefault numversion {1062003}
	bind evnt - prestart ::tcldrop::core::EVNT_prestart -priority 100000
	bind evnt - prerestart ::tcldrop::core::EVNT_prerestart -priority 10000
	# All modules should have a prerestart bind.
	callevent "pre$type"
	setdefault help-path {help}
	setdefault lang-path {language}
	setdefault lang-sections [list]
	# English is the default language, but users can still override it by adding an addlang command to their config.
	set ::languages [list [set ::language {english}]]
	# Load the language specified in the EGG_LANG env variable (Eggdrop uses this too):
	if {[info exists ::env(EGG_LANG)]} { addlang $::env(EGG_LANG) } else { addlang $::language }
	addlangsection core
	putlog "--- Loading Tcldrop v$::tcldrop(version)  ([clock format [clock seconds] -format {%a %b %e %Y}])"
	bind evnt - init ::tcldrop::core::EVNT_init -priority 0
	bind evnt - sighup ::tcldrop::core::EVNT_signal -priority 10000
	bind evnt - sigint ::tcldrop::core::EVNT_signal -priority 10000
	bind evnt - sigterm ::tcldrop::core::EVNT_signal -priority 10000
	bind evnt - sigquit ::tcldrop::core::EVNT_signal -priority 10000
	setdefault config {}
	setdefault owner {}
	setdefault nick {Tcldrop}
	setdefault fuzz {1}
	setdefault handlen 9
	setdefault die-on-sighup 0
	setdefault die-on-sigterm 1
	setdefault die-on-sigint 1
	setdefault max-logs 25
	setdefault quick-logs 0
	setdefault my-ip {}
	setdefault default-ip {0.0.0.0}
	setdefault timezone [clock format 0 -format {%Z}]
	setdefault hourly-updates "[rand 6][rand 10]"
	setdefault daily-updates "[format %02s [rand 25]]"
	# Load the required modules:
	checkmodule core
	checkmodule core::database
	checkmodule core::users
	checkmodule core::conn
	checkmodule core::dcc
	checkmodule encryption
	#checkmodule encryption::null
	checkmodule encryption::blowfish
	#checkmodule encryption::sha1
	#checkmodule encryption::sha256
	# Loaded because is provides the Eggdrop-style [md5] command:
	checkmodule encryption::md5
	#::tcldrop::encryption::default encrypt tea
	#::tcldrop::encryption::default decrypt tea
	::tcldrop::encryption::default encpass md5
	::tcldrop::encryption::default encrypt blowfish
	::tcldrop::encryption::default decrypt blowfish
	::tcldrop::encryption::default encpass blowfish
	#::tcldrop::encryption::default encpass sha256
	checkmodule bots::eggdrop
	# partyline related modules, aren't required to run, but they're needed if you want a dcc/telnet with the bot, and they're needed to make the bot more like Eggdrop:
	checkmodule party
	checkmodule party::dcc
	checkmodule party::telnet
	checkmodule party::terminal
	# The IRC party module shouldn't be loaded by default in v1.0, should it?
	checkmodule party::irc
	# Loaded by default because it provides the Eggdrop-style [dnslookup] command:
	checkmodule dns
	if {[rehash $type]} {
		# ::botnet-nick needs to be set to $::nick by default at some point, perferably as soon as possible..
		setdefault botnet-nick $::nick -protect 1
		callevent $type
		callevent loaded
		# Every $hourly-updates we call the "hourly-updates" event:
		proc ::tcldrop::core::HourlyUpdates {minute hour day month year} { callevent hourly-updates }
		bind time - "${::hourly-updates} * * * *" ::tcldrop::core::HourlyUpdates
		# Every $daily-updates we call the "daily-updates" event:
		proc ::tcldrop::core::DailyUpdates {minute hour day month year} { callevent daily-updates }
		bind time - "* ${::daily-updates} * * *" ::tcldrop::core::DailyUpdates
		# Start the one-minute loop needed by scripts that use "bind time":
		afteridle timer 1 [list {::tcldrop::core::calltime}] -1
	}
	# Don't allow the core module to be unloaded:
	proc ::tcldrop::core::UNLD {module} { return 1 }
	bind unld - core ::tcldrop::core::UNLD
	if {$type eq {restart}} {
		putlog "Restart Completed in [expr { [clock clicks -milliseconds] - $StartTime } ]ms."
	} else {
		if {(![info exists ::botnet-nick] || ${::botnet-nick} eq {}) && [info exists ::nick]} { set ::botnet-nick $::nick } else { set ::botnet-nick {} }
		# We wanna display how many channels and users there are (like Eggdrop):
		if {[catch { PutLogLev o - "=== ${::botnet-nick}: [countchannels] channels, [countusers] users.  Load Time: [expr { [clock clicks -milliseconds] - $::tcldrop::core::StartTime }]ms" }]} { PutLogLev o - "=== ${::botnet-nick}: [countusers] users.  Load Time: [expr { [clock clicks -milliseconds] - $::tcldrop::core::StartTime }]ms" }
	}
	unset ::restart
	set type
}

proc ::tcldrop::core::EVNT_prestart {event} {
	set ::protected(filechannels) [file channels]
	set ::protected(interps) [interp slaves]
	set ::protected(commands) [info commands]
	set ::protected(namespaces) [concat [namespace eval :: {namespace children}] [namespace eval ::tcldrop {namespace children}] [namespace children]]
	set ::protected(globals) [uplevel #0 {info vars}]
}

proc ::tcldrop::core::EVNT_prerestart {event} {

	# Unload all modules:
	unloadmodule * -force 1

	# Kill all timers/utimers:
	set count 0
	foreach {time proc id} [timerslist] {
		killtimer $id
		incr count
	}
	# Cancel all after events:
	foreach c [after info] {
		after cancel $c
		incr count
	}
	if {$count} { putlog "Timers/Delayed Commands: $count Killed." }

	global protected
	counter start success
	counter start exempt
	foreach c [interp slaves] {
		if {[lsearch -exact $protected(interps) $c] == -1} {
			interp delete $c
			counter incr success
		} else {
			counter incr exempt
		}
	}
	set status {}
	if {[set count [counter end success]]} { lappend status "$count Deleted" }
	if {[set count [counter end exempt]]} { lappend status "$count Exempted" }
	if {[llength $status]} {
		putlog "Sub-interpreters: [join $status {, }]."
		set status {}
	}

	counter start success
	counter start fail
	counter start exempt
	foreach c [concat [namespace eval :: {namespace children}] [namespace eval ::tcldrop {namespace children}] [namespace children]] {
		if {[lsearch -exact $protected(namespaces) $c] == -1} {
			if {([catch { ::package forget [string trimleft $c :] }]) && ([catch { ::namespace forget ${c}::* }] || [catch { ::namespace delete $c }])} {
				counter incr fail
			} else {
				counter incr success
			}
		} else {
			counter incr exempt
		}
	}
	if {[set count [counter end success]]} { lappend status "$count Deleted" }
	if {[set count [counter end fail]]} { lappend status "$count Failed Deletion" }
	if {[set count [counter end exempt]]} { lappend status "$count Exempt" }
	if {[llength $status]} {
		putlog "Packages/Namespaces: [join $status {, }]."
		set status {}
	}

	counter start success
	counter start fail
	counter start exempt
	foreach c [info commands] {
		if {[lsearch -exact $protected(commands) $c] == -1 && [string equal [::namespace origin $c] "::$c"]} {
			if {![catch { ::rename $c {} }]} {
				counter incr success
			} else {
				counter incr fail
			}
		} else {
			counter incr exempt
		}
	}
	if {[set count [counter end success]]} { lappend status "$count Deleted" }
	if {[set count [counter end fail]]} { lappend status "$count Failed Deletion" }
	if {[set count [counter end exempt]]} { lappend status "$count Exempt" }
	if {[llength $status]} {
		putlog "Commands/Procs: [join $status {, }]."
		set status {}
	}

	counter start success
	counter start exempt
	global binds
	foreach c [array names binds] {
		if {[info commands [dict get $binds($c) proc]] eq {}} {
			unset binds($c)
			counter incr success
		} else {
			counter incr exempt
		}
	}
	if {[set count [counter end success]]} { lappend status "$count Cleared" }
	if {[set count [counter end exempt]]} { lappend status "$count Exempt" }
	if {[llength $status]} { putlog "Binds: [join $status {, }]." }
	set status {}

	counter start success
	counter start fail
	counter start exempt
	foreach c [uplevel #0 {info vars}] {
		if {[lsearch -exact $protected(globals) $c] == -1} {
			if {[catch { uplevel \#0 [list unset $c] } err]} {
				counter incr fail
			} else {
				counter incr success
			}
		} else {
			counter incr exempt
		}
	}
	if {[set count [counter end success]]} { lappend status "$count Unset" }
	if {[set count [counter end fail]]} { lappend status "$count Failed Unset" }
	if {[set count [counter end exempt]]} { lappend status "$count Exempt" }
	if {[llength $status]} {
		putlog "Global Variables: [join $status {, }]."
		set status {}
	}
}

# Deprecated, because incr is just as easy to use directly:
proc ::tcldrop::core::counter {command {id {default}} {incr {1}}} {
	variable Counts
	switch -- $command {
		{start} { set Counts($id) 0 }
		{incr} { incr Counts($id) $incr }
		{end} {
			set total $Counts($id)
			unset Counts($id)
			return $total
		}
	}
}

proc ::tcldrop::core::EVNT_init {event} { global pidfile botnet-nick database-basename
	# Create the pid file:
	if {![info exists pidfile] || $pidfile eq {}} {
		if {[info exists botnet-nick] && ${botnet-nick} != {}} {
			set pidfile "pid.${botnet-nick}"
		} elseif {[info exists database-basename] && ${database-basename} != {}} {
			set pidfile "pid.${database-basename}"
		} else {
			set pidfile "pid.[pid]"
		}
	}
	catch {
		puts [set fid [open $pidfile w]] [pid]
		close $fid
		file attributes $pidfile -permissions 0600
	}
}

proc ::tcldrop::core::EVNT_signal {signal} {
	variable Signal
	# (If Signal already exists, it means we're already processing another fatal/exit signal.)
	if {![info exists Signal] || $Signal ne $signal} {
		switch -- [string tolower $signal] {
			{sighup} {
				if {${::die-on-sighup}} {
					shutdown {Caught Signal: SIGHUP (HANGUP SIGNAL) -- SIGNING OFF}
					variable Signal {SIGHUP}
				} else {
					putlog {Received HUP signal: rehashing...}
					rehash $signal
				}
			}
			{sigterm} {
				if {${::die-on-sigterm}} {
					shutdown {Caught Signal: SIGTERM (TERMINATE SIGNAL) -- SIGNING OFF}
					variable Signal {SIGTERM}
				} else {
					putlog {RECEIVED TERMINATE SIGNAL (IGNORING)}
				}
			}
			{sigint} {
				# This is generally a CTRL+C.
				if {[info exists ::die-on-sigint] && ${::die-on-sigint}} {
					shutdown {Caught Signal: SIGINT}
					variable Signal {SIGINT}
				} else {
					putlog {Caught Signal: SIGINT (IGNORING)}
				}
			}
			{sigquit} {
				if {[info exists ::die-on-sigquit] && ${::die-on-sigquit}} {
					die {Caught Signal: SIGQUIT}
					variable Signal {SIGQUIT}
				} else {
					putlog {Caught Signal: RECEIVED QUIT SIGNAL (IGNORING)}
				}
			}
			{sigbus} - {20} {
				die {BUS ERROR -- CRASHING!} 20
				variable Signal {SIGBUS}
				exit 20
			}
			{sigsegv} - {11} {
				die {SEGMENT VIOLATION -- CRASHING!} 11
				variable Signal {SIGSEGV}
				exit 11
			}
			{sigfpe} - {8} {
				die {FLOATING POINT ERROR -- CRASHING!} 8
				variable Signal {SIGFPE}
				exit 8
			}
			{sigill} {
				# log Context..?  EH?
			}
			{sigalrm} - {sigalarm} {
				# eh?
			}
			{0} - {sigpwr} - {sigwinch} - {sigchld} - {sigurg} - {sigcont} - {sigtstp} - {sigttin} - {sigttou} - {sigstop} {
				# Completely ignore these signals.  (They can't be triggered unless they get trapped in ../../../tcldrop.tcl anyway.)
			}
			{default} {
				# Casual shutdown as the default, with exit code 1 because it's an unknown signal.
				shutdown "Caught Signal: $signal  (UNKNOWN SIGNAL - SHUTTING DOWN...)" 1
				variable Signal [string toupper $signal]
			}
		}
		# If we didn't shutdown, this will unset the Signal variable so that we can process another signal 1+ seconds in the future:
		after idle [list after 999 [list unset -nocomplain ::tcldrop::core::Signal]]
	}
}


# Import the core Tcldrop commands into the global namespace:
proc ::tcldrop::core::start {} {
	global restart tcldrop env tcl_interactive tcl_precision
	# Note: If ::restart exists, it means we're already in the middle of a restart (probably just re-source'ing this file.)
	if {![info exists restart]} {
		# This works around a bug in Tcl v8.5+ that gives the wrong results:
		if {[expr {2.55 + 0.9}] != {3.45}} {
			# First set it as high as is allowed by Tcl:
			while {$tcl_precision < 15 && ![catch { incr tcl_precision }]} {}
			# Now back down until it gives the right results:
			while {$tcl_precision >= 3 && [expr {2.55 + 0.9}] != {3.45} && ![catch { incr tcl_precision -1 }]} {}
		}
		# Import the Tcldrop core commands to the ::tcldrop namespace:
		namespace eval ::tcldrop { namespace import -force {::tcldrop::core::*} }
		namespace eval :: {
			# Import the Tcldrop core commands to the global namespace:
			namespace import -force {::tcldrop::core::*}
			# Import the Tcldrop commands to the global namespace:
			namespace import -force {::tcldrop::*}
			# Add ::tcldrop to the global namespace path:
			if {{::tcldrop} ni [namespace path]} { namespace path [concat [namespace path] {::tcldrop}] }
		}
		# The default console flags:
		if {[info exists ::console]} { set console $::console } else { set console {oe} }
		if {([info exists tcldrop(debug)] && $tcldrop(debug)) || ([info exists env(DEBUG)] && $env(DEBUG))} {
			# Add debug flag to the console if requested:
			if {![string match {*d*} $console]} { append console {d} }
			# Setup a log bind that sends logs to the "screen":
			bind log $console * ::tcldrop::PutLogLev -priority 0
			# Setting an error trace, to catch hard to see errors:
			proc ::tcldrop::core::TraceError {var var2 op} { variable LastError
				# Prevent error floods by only showing 1 per second:
				if {![info exists LastError] || [clock seconds] > $LastError} {
					set LastError [clock seconds]
					# Try to report the error to the proper place, with lots of fallbacks:
					if {[catch { putloglev d * [set errorinfo "(TraceError):\n$::errorInfo"] }] && [catch { PutLogLev d - $errorinfo }] && [catch { puts stderr $errorinfo }] && [catch { puts stdout $errorinfo }] && [catch { die $errorinfo }]} {
						# Kill ourself if we can't report the error to anywhere..
						exit 1
					}
				}
			}
			variable LastError 0
			trace add variable ::errorInfo write ::tcldrop::core::TraceError
			# This happens to be our first putlog, so make sure log-time exists..
			setdefault log-time {2}
			# Give notice that we're running in debug mode..
			putdebuglog {Running in debug mode.}
		} else {
			# Setup a log bind that sends logs to the "screen":
			bind log $console * ::tcldrop::PutLogLev -priority 0
		}
		# Tell restart that it's the "start" (first time to start).
		restart start
		# init events are only called after a "start" is complete, and after we've hit the event-loop:
		afteridle callevent init
		if {$tcldrop(background-mode)} {
			# Disable showing logs to the "screen":
			unbind log - * ::tcldrop::PutLogLev
			if {!$tcldrop(simulate-dcc)} {
				# We don't need these sockets open either (do we?):
				#catch { close stderr }
				#catch { close stdout }
				#catch { close stdin }
				set tcl_interactive 0
			}
		}
	}
}

# Setting uptime here isn't important, what is important is having a variable to check on so we know if it's already started or not (This allows this file to be sourced in the future without causing it to try to start "fresh")
if {![info exists ::uptime]} {
	set ::uptime [clock seconds]
	# And now we set everything into motion...  \o/
	::tcldrop::core::start
}
