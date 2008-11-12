# pubsafetcl.tcl --
#
# Copyright (C) 2004,2005,2006,2007,2008 by Philip Moore <FireEgl@Tcldrop.US>
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

if {![info exists ::auto_path]} { set ::auto_path [list] }

namespace eval pubsafetcl {
	variable version {2.2.0}
	variable name {pubsafetcl}
	variable author {FireEgl}
	variable description {Provides a public safe Tcl interpreter, free from infinite loops.}
	variable script [info script]
	package provide pubsafetcl $version
	variable rcsid {$Id$}
	while {$::tcl_precision < 15 && ![catch { incr ::tcl_precision }]} {}
	proc Reset {{interp {safetcl}} args} { return "Reset [create $interp]." }

	# This proc initializes/resets the safe interpreter, and basically does stuff to make it even safer or more complete:
	proc create {{interp {safetcl}} args} {
		if {[interp exists $interp]} {
			catch { safe::interpConfigure $interp -deleteHook {} }
			# Prefer to use safe::interpDelete but fall back to interp delete if safe::interpDelete happens to be broken:
			if {[catch { safe::interpDelete $interp }]} { catch { interp delete $interp } }
		}
		# This is a hack to make it search sub directories.. (Cuz doing -nestedLoadOk doesn't work..)
		set subpaths $::auto_path
		if {[set p [lsearch -exact $subpaths {/usr/lib}]] != -1} { set subpaths [lreplace $subpaths $p $p] }
		foreach p $subpaths { set subpaths [concat $subpaths [glob -directory $p -types {d} -nocomplain *]] }
		if {[catch { safe::interpCreate $interp -deleteHook [list [namespace current]::create $interp] -noStatics -nestedLoadOk -accessPath $subpaths -nested 1 }]} {
			if {[catch { interp create -safe -- $interp }]} {
				return {}
			} else {
				catch { safe::interpInit $interp -deleteHook [list [namespace current]::create $interp] -noStatics -nestedLoadOk -accessPath $subpaths -nested 1 }
			}
		}
		catch { safe::interpConfigure $interp -noStatics -nestedLoadOk -accessPath $subpaths -deleteHook [list [namespace current]::create $interp] -nested 1 }
		#catch { $interp eval { package require tcllib } }
		# We have to hide these commands cuz they let people do nasty things:
		foreach c {after vwait trace} { catch { $interp hide $c } }
		$interp eval {
			# Make some dummy variables:
			array set tcl_platform [list user nobody machine unknown os Unknown osVersion 0.0]
			array set env [list HOME {.} NAME $tcl_platform(user) LOGNAME $tcl_platform(user) USER $tcl_platform(user) USERNAME $tcl_platform(user) TMP {.} PATH {.} HOSTNAME [info hostname] GROUP {nogroup} SHELL {tclsh}]
			set uptime [set server-online [clock seconds]]
			set botname "SafeTcl!$tcl_platform(user)@$env(HOSTNAME)"
			set botnick [set nick {SafeTcl}]
			set altnick {Safe_Tcl}
			set serveraddress [set server "$env(HOSTNAME):6667"]
			set version {1.6.20+cvs 1062003 CVS 1214020204 cvs}
			set numversion {1062003}
			set lastbind {tcl}
			set config {eggdrop.conf}
			# Make some fake commands that don't really do anything:
			if {[info commands load] == {}} { proc load {fileName {packageName {}} {interp {}}} { return -code error "couldn't load file \"$fileName\": [pwd]$fileName: cannot open shared object file: No such file or directory" } }
			if {[info commands source] == {}} { proc source {fileName} { return -code error {source is disabled.} } }
			proc cd {args} { update }
			proc timeout {args} { update }
			proc pwd {args} { set args {/} }
			proc glob {args} {
				switch -- $args {
					{*} { list [file tail [info nameofexecutable]] }
					{} { return -code error {wrong # args: should be "glob ?switches? name ?name ...?"} }
					{default} {}
				}
			}
			proc socket {args} { return -code error {socket command is disabled.} }
			proc fconfigure {channelId {optionName {}} {value {}} {{optionName value} {}}} { -return -code error {fconfigure is disabled.} }
			proc open {fileName {access {}} {permissions {}}} { return -code error {open is disabled.} }
			proc vwait {name} { return -code error {vwait is disabled.} }
			proc after {option args} { return -code error {after is disabled.} }
			proc trace {args} { return -code error {trace is disabled.} }
			# Simulate some common things people try to exec:
			proc exec {args} {
				switch -glob -- $args {
					{arch} - {arch *} { set ::tcl_platform(machine) }
					{basename} { return -code error {basename: too few arguments} }
					{basename --help} { return {Usage: basename NAME} }
					{basename *} { file tail [lindex [split $args] 1] }
					{dirname} { return -code error {dirname: too few arguments} }
					{dirname --help} { return {Usage: dirname NAME} }
					{dirname *} { file dirname [lindex [split $args] 1] }
					{logname} { return -code error {logname: no login name} }
					{logname --help} { return {Usage: logname [OPTION]} }
					{date} { strftime {%a %b %e %T %Z %Y} }
					{false} - {false *} { return -code error {child process exited abnormally} }
					{test} - {test *} { return -code error {} }
					{tty} - {tty *} { return -code error {not a tty} }
					{hostid} { return {00000000} }
					{nice} { return {99} }
					{pwd} { pwd }
					{sync} - {echo} - {true} - {true *} - {users} - {users *} { }
					{sync} { return {sync: ignoring all arguments} }
					{expr} { return -code error {expr: too few arguments} }
					{expr *} { expr { [lrange [split $args] 1 end] } }
					{hostname} { info hostname }
					{hostname -s} { lindex [split [info hostname] .] 0 }
					{hostname -i} { return {127.0.0.1} }
					{hostname -a} - {hostname -d} - {hostname -y} { }
					{hostname -*} { return -code error {hostname: Invalid Option.} }
					{hostname *} { return -code error {hostname: you must be root to change the host name} }
					{ls} { file tail [info nameofexecutable] }
					{tr} { return -code error {tr: two strings must be given when translating} }
					{groups} { set ::env(GROUP) }
					{uname} { set ::tcl_platform(os) }
					{uname -a} { return "$::tcl_platform(os) [info hostname] $::tcl_platform(osVersion) #0 [strftime {%a %b %e %T %Z %Y}] $::tcl_platform(machine) unknown unknown GNU/Linux" }
					{uname -r} { set ::tcl_platform(osVersion) }
					{uname -s} { set ::tcl_platform(os) }
					{uname -n} { info hostname }
					{uname -v} { return "#0 [strftime {%a %b %e %T %Z %Y}]" }
					{uname -m} { set ::tcl_platform(machine) }
					{uname -p} { return {unknown} }
					{uname -i} { return {unknown} }
					{uname -o} { return {GNU/Linux} }
					{uname --help} { return {Usage: uname [OPTION]...} }
					{uname *} { return -code error {Try `uname --help' for more information.} }
					{whoami} { set ::tcl_platform(user) }
					{whoami --help} { return {Usage: whoami [OPTION]...} }
					{whoami *} { return -code error {Try `whoami --help' for more information.} }
					{rm *} - {reboot} - {reboot *} - {shutdown} - {shutdown *} - {su} - {su *} - {sudo *} - {kill *} - {killall *} { return -code error "You can't do \"$args\"   ..Cuz you're BAD!  =P" }
					{} { return -code error {wrong # args: should be "exec ?switches? arg ?arg ...?"} }
					{default} { return -code error "couldn't execute \"[lindex [split $args] 0]\": no such file or directory" }
				}
			}
			proc putlog {text} { putloglev o * $text }
			# Useful procs:
			proc rand {maxint} { expr { int(rand()*$maxint) } }
			proc randstring {length {chars abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789}} {
				set count [string length $chars]
				for {set index 0} {$index < $length} {incr index} { append result [string index $chars [rand $count]] }
				set result
			}
			proc lrandom {L} { lindex $L [expr { int(rand()*[llength $L]) }] }
			proc lrand {list} { lindex $list [expr { int(rand()*[llength $list]) }] }
			proc reverse {s} { puts unknown {Use [string reverse] instead in Tcl v8.5+}
				set l [string length $s]
				while {[incr l -1] >= 0} { append r [string index $s $l] }
				return $r
			}
			if {![llength [info commands lreverse]]} {
				proc lreverse {list} { set length [llength $list]
					while {[incr length -1] >= 0} { lappend newlist [lindex $list $length] }
					return $newlist
				}
			}
			# http://wiki.tcl.tk/14144  http://wiki.tcl.tk/989
			proc urlencode {string} {
				regsub -all -- {([^A-Za-z0-9_-])} $string {%[format {%02lX} [scan "\\\1" {%c}]]} string
				subst -nobackslashes -novariables $string
			}
			proc urldecode {string} {
				regsub -all -- {%([0123456789ABCDEF][0123456789ABCDEF])} $string {[format {%c} {0x\1}]} string
 				subst -nobackslashes -novariables $string
			}
			proc htmlencode {input} { set output {}
				foreach char [split $input {}] { if {![regexp -nocase -- {[a-z0-9&=]} $char]} { append output % [format %X [scan $char %c]] } else { append output $char } }
				return $output
			}
			proc strlwr {string} { string tolower $string }
			proc strupr {string} { string toupper $string }
			proc strcmp {string1 string2} { string compare $string1 $string2 }
			proc stricmp {string1 string2} { string compare [string tolower $string1] [string tolower $string2] }
			proc strlen {string} { string length $string }
			proc stridx {string index} { string index $string $index }
			proc iscommand {command} { if {[info commands $command] != {}} { return 1 } else { return 0 } }
			proc isproc {proc} { if {[info procs $proc] != {}} { return 1 } else { return 0 } }
			proc realtime {what} {
				switch -exact -- $what {
					time { return [strftime {%H:%M}] }
					date { return [strftime {%d %b %Y}] }
					default { return [strftime {%I:%M %P}] }
				}
			}
			proc number_to_number {number} {
				switch -exact -- $number {
					0 { return {Zero} }
					1 { return {One} }
					2 { return {Two} }
					3 { return {Three} }
					4 { return {Four} }
					5 { return {Five} }
					6 { return {Six} }
					7 { return {Seven} }
					8 { return {Eight} }
					9 { return {Nine} }
					10 { return {Ten} }
					11 { return {Eleven} }
					12 { return {Twelve} }
					13 { return {Thirteen} }
					14 { return {Fourteen} }
					15 { return {Fifteen} }
					16 { return {Sixteen} }
					17 { return {Seventeen} }
					18 { return {Eighteen} }
					19 { return {Nineteen} }
					20 { return {Twenty} }
					21 { return {Twenty-One} }
					22 { return {Twenty-Two} }
					23 { return {Twenty-Three} }
					24 { return {Twenty-Four} }
					25 { return {Twenty-Five} }
					30 { return {Thirty} }
					40 { return {Forty} }
					50 { return {Fifty} }
					60 { return {Sixty} }
					70 { return {Seventy} }
					80 { return {Eighty} }
					90 { return {Ninety} }
					default { return $number }
				}
			}
			# proc from leprechau's website, it's not right though.. Fix.
			proc ansicolor {{color {}}} {
				switch -exact -- $color {
					bold { return "[format %c 27][1m" }
					underline { return "[format %c 27][4m" }
					blink { return "[format %c 27][5m" }
					inverted { return "[format %c 27][7m" }
					invisible { return "[format %c 27][8m" }
					fgblack { return "[format %c 27][30m" }
					fgred { return "[format %c 27][31m" }
					fggreen { return "[format %c 27][32m" }
					fgyellow { return "[format %c 27][33m" }
					fgblue { return "[format %c 27][34m" }
					fgmagenta { return "[format %c 27][35m" }
					fgcyan { return "[format %c 27][36m" }
					fgwhite { return "[format %c 27][37m" }
					bgblack { return "[format %c 27][40m" }
					bgred { return "[format %c 27][41m" }
					bggreen { return "[format %c 27][42m" }
					bgyellow { return "[format %c 27][43m" }
					bgblue { return "[format %c 27][44m" }
					bgmagenta { return "[format %c 27][45m" }
					bgcyan { return "[format %c 27][46m" }
					bgwhite { return "[format %c 27][47m" }
					plain - normal - reset - none - default { return "[format %c 27][0m" }
				}
			}
			# duration, based on http://wiki.tcl.tk/789, modified by fedex` to support years and weeks, added features from http://inferno.slug.org/wiki/Duration with speed tweaks by me (FireEgl).
			proc duration2 {seconds args} {
				# avoid OCTAL interpretation, deal with negatives, split floats, handle things like .3
				lassign [split [string trimleft $seconds {-0}] {.}] seconds fraction
				if {![string length $seconds]} { set seconds 0 }
				set timeatoms [list]
				if {![catch {
					# old value was 31449600 for years.
					foreach div {31536000 604800 86400 3600 60 1} mod {0 52 7 24 60 60} name {year week day hour minute second} {
						if {[lsearch -glob $args "-no${name}*"] != -1} { break }
						set n [expr { $seconds / $div }]
						if {$mod > 0} { set n [expr { $n % $mod }] }
						if {$n > 1} { lappend timeatoms "$n ${name}s" } elseif {$n == 1} { lappend timeatoms "$n $name" }
					}
				}]} {
					if {[info exists fraction] && [string length $fraction]} { if {!$n} { lappend timeatoms "0.$fraction seconds" } else { set timeatoms [lreplace $timeatoms end end "$n.$fraction seconds"] } }
					if {[llength $timeatoms]} { join $timeatoms {, } } else { return {0 seconds} }
				}
			}
			# Converts (for example) 2002:4574:9390::1/48 to 69.116.147.144
			# (This is a modified version of Robb's hex_ip proc.)
			proc 6to4toipv4 {6to4ip} {
				set 6to4ip [join [lrange [split $6to4ip :] 1 2] {}]
				foreach a {0 2 4 6} b {1 3 5 7} { lappend ip [format %u 0x[string range $6to4ip $a $b]] }
				join $ip {.}
			}
			proc testip {ip} { regexp -- {^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$} $ip }
			proc validip {ip {type {default}}} {
				switch -- $type {
					{long} - {decimal} { if {[regexp -- {^([0-9]*)([0-9])+$} $ip]) || ($ip < 0) || ($ip > 4294967295)} { return 0 } else { return 1 } }
					{hex} { if {(![regexp -- {^([0-9]*)([0-9])+$} [set ip [format %u 0x$ip]]]) || ($ip < 0) || ($ip > 4294967295)} { return 0 } else { return 1 } }
					{short} - {default} { regexp -- {^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$} $ip }
				}
			}
			proc round {n {c 0}} { if {$c > [string length [lindex [split $n .] 1]]} { return $n } else { format %.${c}f ${n}1 } }
			proc round2 {number digits} { expr { round(pow(10,$digits)*$number)/pow(10,$digits) } }
			proc text2hex {text} {	set hex {}
				foreach c [split $text {}] { append hex "\\x[format %x [scan $c %c]]" }
				return $hex
			}
			proc ip2hex {ip} {
				lassign [split $ip .] a b c d
				format %02x%02x%02x%02x $a $b $c $d
			}
			proc hex2ip {hex} {
				foreach a {0 2 4 6} b {1 3 5 7} { lappend ip [format %u 0x[string range $hex $a $b]] }
				join $ip .
			}
			proc ip2decimal {ip} {
				lassign [split $ip .] a b c d
				format %u 0x[format %02X%02X%02X%02X $a $b $c $d]
			}
			proc decimal2ip {ip} { return "[format %u 0x[string range [set ip [format %08X $ip]] 0 1]].[format %u 0x[string range $ip 2 3]].[format %u 0x[string range $ip 4 5]].[format %u 0x[string range $ip 6 7]]" }
			proc isnumber {string} { string is digit -strict $string }
			proc en_ordinal {n} { if {($n%100) < 10 || ($n%100) > 20} { switch -- [expr { abs($n) % 10 }] { {1} { return "${n}st" } {2} { return "${n}nd" } {3} { return "${n}rd" } {default} { return "${n}th" } } } else { return "${n}th" } }
			proc islonger {word2 word1} { expr { [string length $word1] - [string length $word2] } }
			proc isdigit {string} { regexp -- {^[0-9]+$} $string }
			# by phrek:
			proc gcd {num div} { if {!$div || !$num} { return 0 } elseif {![set res [expr { $num % $div }]]} { return $div } else { gcd $div $res } }
			proc date {args} { strftime {%d %b %Y} }
			#proc time {args} { if {[llength $args]} { return -code error {time is disabled.} } else { strftime {%H:%M} } }
			proc randstring {length {chars abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789}} {
				set count [string length $chars]
				for {set index 0} {$index < $length} {incr index} { append result [string index $chars [rand $count]] }
				set result
			}
			proc commify {number {separator {,}}} { regsub -all -- {\d(?=(\d{3})+($|\.))} $number "\\0$separator" }
			proc israr {name} { regexp -- {((?:part\d{2}.)?rar|r\d{2}|\d{3})$} $name }
			proc u2c {u} { scan $u %u }
			proc c2u {c} { format %c $c }
			proc stripcolors {text} { puts unknown {Use stripcodes instead if you're using Eggdrop v1.6.17 or higher.  http://www.eggheads.org/support/egghtml/1.6.18/tcl-commands.html#misc }
				regsub -all -- {\003[0-9]{0,2}(,[0-9]{0,2})?|\017|\037|\002|\026|\007} $text {}
			}
			#proc stripcodes {strip-flags string} {
			#	foreach o [split ${strip-flags} {}] {
			#		switch -- $o {
			#			{a} { return [regsub -all -- {\003[0-9]{0,2}(,[0-9]{0,2})?|\017|\037|\002|\026|\007} $string {}] }
			#			{b} { lappend strip "\002" }
			#			{r} { lappend strip "\026" }
			#			{u} { lappend strip "\037" }
			#			{g} { lappend strip "\007" }
			#			{p} { lappend strip "\017" }
			#			{c} { lappend strip "\003\[0-9\]{0,2}(,\[0-9\]{0,2})?" }
			#			{a} { lappend strip "\033\[.*m" }
			#			{-} - {+} {}
			#			{default} { return -code error "Unknown strip option: $o" }
			#		}
			#	}
			#	if {[info exists strip]} {
			#		regsub -all -- [join $strip {|}] $string {}
			#	} elseif {![info exists o]} {
			#		regsub -all -- {\003[0-9]{0,2}(,[0-9]{0,2})?|\017|\037|\002|\026|\007} $string {}
			#	} else {
			#		set string
			#	}
			#}
			# Unicode char to \u sequence: simple, but handy when examining Unicode output:
			proc u2x {u} {
				scan $u %c t
				format "\\u%04.4X" $t
			}
			# This is escapes "special" characters, especially ones special to mysql:
			proc addslashes {text} { string map {\\ \\\\ \| \\| \[ \\[ \] \\] \{ \\{ \} \\} $ \\$ \` \\` \' \\' \" \\"} $text }
			proc unhtml {text} { regsub -all -- {(<.+?>)} $text {} }
			proc unhtml2 {text} { regsub -all -- {<([^<])*>} $text {} }
			proc unixtime {} { clock seconds }
			proc average {L} { expr { ([join $L +]) / [llength $L].0 } }
			if {[info tclversion] >= 8.5} {
				proc islist {s} { puts unknown "In Tcl v8.5+ you can use \[string is list\]"
					string is list $s
				}
			} else {
				proc islist {s} { puts unknown "In Tcl v8.5+ you can use \[string is list\]"
					expr { ![catch {llength $s}] }
				}
			}
			proc lconcat {args} {
				set result [list]
				foreach list $args { lappend result {*}$list }
				set result
			}
			proc string2list {string} {
				if {[catch {llength $string}]} {
					foreach i [split $string] { if {$i != {}} { lappend res $i } }
					set res
				} else {
					set string
				}
			}
			proc string2list {string} { regexp -inline -all -- {\S+} $string }
			proc slindex {string index} { lindex [regexp -inline -all -- {\S+} $string] $index }
			proc slrange {string start {end {end}}} { lrange [regexp -inline -all -- {\S+} $string] $start $end }
			proc sllength {string} { regexp -all -- {\S+} $string }
			# removes duplicates without sorting the input list:
			proc luniq {L} {
				set t [list]
				foreach i $L { if {[lsearch -exact $t $i] == -1} { lappend t $i } }
				return $t
			}
			proc scramble {words} {
				set randlist [list]
				foreach word [split $words] { lappend randlist [scrambleword $word] }
				join $randlist
			}
			proc scrambleword {word} {
				set word [split $word {}]
				while {[llength $word] > 0} {
					lappend new [lindex $word [set rand [rand [llength $word]]]]
					set word [lreplace $word $rand $rand]
				}
				join $new {}
			}
			## simple proc to randomize a given list of items
			## usage: mixit $list
			## it really should be a list...if not split it first..
			## example: mixit [split $string]
			## (by leprechau)
			proc mixit {list} {
				foreach x $list {
					lappend mixed [lindex $list [set rindex [expr {int(rand()*[llength $list])}]]]
					set list [lreplace $list $rindex $rindex]
				}
				return $mixed
			}
			proc binaryToDecimal {binaryString} {
				# Pad left side with zeros to form a 32bit bit string.
				set binaryStringLength [string length $binaryString]
				set a 0
				while {$a < [expr { 32 - $binaryStringLength } ]} {
					set binaryString "0$binaryString"
					incr a
				}
				binary scan [binary format B* $binaryString] I* binaryNumber
				if {$binaryNumber != -1} { set binaryNumber } else { LargebinaryToDecimal $binaryString }
			}
			proc LargebinaryToDecimal {myNum} {
				# Must use double percision so the numbers do not wrap.
				set posVal 1.0
				set myResult 0.0
				set totalDigits [string length $myNum]
				set a -1
				while {[incr a] < $totalDigits} {
					set myResult [expr { $myResult + ([string index $myNum end-$a] * $posVal) }]
					set posVal [expr { $posVal * 2.0 }]
				}
				# Don't return the double percision portion (".#"), just a clean integer.
				string range $myResult 0 end-2
			}
			proc fibonacci {x} { if {$x <= 1} { set x 1 } else { expr {[fibonacci [expr {$x-1}]] + [fibonacci [expr {$x-2}]]} } }
			proc showproc {name} { string map { "\n" "\002\0034\;\002\00399" "\011" { } } [info body $name] }
			proc colorize {args} { set out "\0033"
				foreach a [split $args {}] {
					switch -- $a {
						{[} { append out "\0034${a}\0033" }
						{]} { append out "\0034${a}\00399" }
						"\"" { append out "\00311${a}" }
						{$} - {!} - {-} - {+} - {&} - {%} - {|} - {=} { append out "\0038${a}\00399" }
						"\{" { append out "\00312${a}\0033" }
						"\}" { append out "\00312${a}\00399" }
						{default} { append out $a }
					}
				}
				set out
			}
			# This was made by phrek, it's used to convert tabs into spaces while still maintaining the indentation columns.
			proc tabs2spaces {string {tabsize {5}} {spacechar { }}} { while {[string match "*\t*" $string]} { regsub {\t} $string "[string repeat $spacechar [expr { $tabsize - ([string first \t $string] % $tabsize) }]]" string } ; return $string }

			# Provided by Dossy@EFNet:
			# Contains bugs: 1. It cuts words longer than $maxlen.  2. It only supports maxlen's up to 255.
			proc ginsu {string {maxlen {255}}} { regexp -all -inline -- "\\S.{0,$maxlen}(?!\\S)" $string }

			# If $args in the current scope is a list that contains: a b c d e f g
			# Running: args varname1 varname2 args varname3 varname4
			# Will set the following variables in the current scope:
			# varname1 to: a
			# varname2 to: b
			# args to: c d e
			# varname3 to: f
			# varname4 to: g
			proc args {args} { upvar 1 args args_list
				if {![info exists args_list]} {
					return -code error {The variable "args" must already exist.}
				} elseif {[llength $args_list] < [llength $args] - 1} {
					return -code error {Too few required variables specified.}
				} elseif {[set args_start [lsearch -exact $args {args}]] == -1} {
					return -code error {"args" must be one of the specified variables to set.}
				} else {
					# Set the variables to the left of "args":
					if {$args_start > 0} { uplevel 1 [list lassign $args_list {*}[lrange $args 0 ${args_start}-1]] }
					# Set the variables to the right of "args":
					if {[set args_end [lsearch -exact [lreverse $args] {args}]] > 0} { uplevel 1 [list lassign [lreverse $args_list] {*}[lrange [lreverse $args] 0 ${args_end}-1]] }
					# Set "args" to whatever might be leftover:
					set args_list [lrange $args_list $args_start end-$args_end]
				}
			}

			# forward-compatible lset:
			if {[package vcompare [package provide Tcl] 8.4] < 0} {
				proc ::tcl::K {a b} { set a }
				proc lset {listName index val} { upvar $listName list
					set list [lreplace [::tcl::K $list [set list {}]] $index $index $val]
				}
			}
			# forward-compatible dict:
			# Poor man's dict -- a pure tcl [dict] emulation
			# Source: http://wiki.Tcl.Tk/10609  (March 1, 2004)
			# Very slow, but complete.
			#
			# Not all error checks are implemented!
			# e.g. [dict create odd arguments here] will work
			#
			# Implementation is based on lists, [array set/get] and recursion
			if {![llength [info commands dict]]} {
				namespace eval ::tcl {}
				proc dict {cmd args} { uplevel 1 [linsert $args 0 "::tcl::dict_$cmd"] }
				proc ::tcl::dict_get {dv args} {
					if {![llength $args]} { return $dv } else {
						array set dvx $dv
						eval [linsert [lrange $args 1 end] 0 ::tcl::dict_get $dvx([lindex $args 0])]
					}
				}
				proc ::tcl::dict_exists {dv key args} {
					array set dvx $dv
					if {![info exists dvx($key)]} {
						return 0
					} elseif {[llength $args]} {
						eval [linsert $args 0 ::tcl::dict_exists $dvx($key)]
					} else {
						return 1
					}
				}
				proc ::tcl::dict_set {dvar key value args} {
					upvar 1 $dvar dv
					lappend dv
					array set dvx $dv
					if {![llength $args]} {
						set dvx($key) $value
					} else {
						eval [linsert $args 0 ::tcl::dict_set dvx($key) $value]
					}
					set dv [array get dvx]
				}
				proc ::tcl::dict_unset {dvar key args} {
					upvar 1 $dvar mydvar
					if {![info exists mydvar]} { return }
					array set dv $mydvar
					if {![llength $args]} {
						if {[info exists dv($key)]} { unset dv($key) }
					} else {
						eval [linsert $args 0 ::tcl::dict_unset dv($key)]
					}
					set mydvar [array get dv]
					return {}
				}
				proc ::tcl::dict_keys {dv {pat {*}}} {
					array set dvx $dv
					array names dvx $pat
				}
				proc ::tcl::dict_append {dvar key args} {
					upvar 1 $dvar dv
					lappend dv
					array set dvx $dv
					eval [linsert $args 0 append dvx($key)]
					set dv [array get dvx]
				}
				proc ::tcl::dict_create {args} { set args }
				proc ::tcl::dict_filter {dv ftype args} {
					set r [list]
					foreach {globpattern} $args { break }
					foreach {varlist script} $args { break }
					switch $ftype {
						key { foreach {key value} $dv { if {[string match $globpattern $key]} { lappend r $key $value } } }
						value { foreach {key value} $dv { if {[string match $globpattern $value]} { lappend r $key $value } } }
						script {
							foreach {Pkey Pval} $varlist { break }
							upvar 1 $Pkey key $Pval value
							foreach {key value} $dv { if {[uplevel 1 $script]} { lappend r $key $value } }
						}
						default { return -code error {Wrong filter type} }
					}
					set r
				}
				proc ::tcl::dict_for {kv dict body} { uplevel 1 [list foreach $kv $dict $body] }
				proc ::tcl::dict_incr {dvar key {incr 1}} {
					upvar 1 $dvar dv
					lappend dv
					array set dvx $dv
					if {![info exists dvx($key)]} { set dvx($key) 0 }
					incr dvx($key) $incr
					set dv [array get dvx]
				}
				proc ::tcl::dict_info {dv} { return {Dictionary is represented as plain list} }
				proc ::tcl::dict_lappend {dvar key args} {
					upvar 1 $dvar dv
					lappend dv
					array set dvx $dv
					eval [linsert $args 0 lappend dvx($key)]
					set dv [array get dvx]
				}
				proc ::tcl::dict_replace {dv args} {
					foreach {k v} $args { ::tcl::dict_set dv $k $v }
					set dv
				}
				proc ::tcl::dict_remove {dv args} {
					foreach k $args { ::tcl::dict_unset dv $k }
					set dv
				}
				proc ::tcl::dict_size {dv} { expr { [llength $dv] / 2 } }
				proc ::tcl::dict_values {dv {gp *}} {
					set r [list]
					foreach {k v} $dv { if {[string match $gp $v]} { lappend r $v } }
					set r
				}
			}
			# forward-compatible lrepeat:
			if {![llength [info commands lrepeat]]} { proc lrepeat {count args} {string repeat "$args " $count} }
			# forward-compatible lassign:
			if {![llength [info commands lassign]]} {
				proc lassign {values args} {
					while {[llength $values] < [llength $args]} { lappend values {} }
					uplevel 1 [list foreach $args $values {break}]
					lrange $values [llength $args] end
				}
			}
		}

		# We want to make some harmless Eggdrop Tcl commands available:
		foreach c {duration ctime strftime encrypt decrypt encpass unames md5 sha1 getchanlaston dccused getchanidle myip flushmode queuesize traffic inchain haschanrec wasop getting-users botisvoice modules islinked countusers validchan validuser finduser ischanjuped isban ispermban isexempt ispermexempt isinvite isperminvite isbansticky isexemptsticky isinvitesticky matchban matchexempt matchinvite isignore channame2dname chandname2name isbotnick botonchan isop isvoice onchan nick2hand hand2nick handonchan ischanban ischanexempt ischaninvite getchanjoin onchansplit valididx idx2hand maskhost hand2idx washalfop topic getchanhost isdynamic isbotnetnick getinfo realtime stripcodes} {
			if {[info command $c] != {}} { if {[interp eval $interp [list info commands $c]] == {}} { interp alias $interp $c {} $c } else { interp alias $interp "${c}-eggdrop" {} $c } }
		}

		# And these we want to alias to reset:
		foreach c {exit die rehash restart reset quit reload} {
			interp alias $interp $c {} [namespace current]::Reset $interp
		}

		proc Timeout {interp args} { update
			if {[info exists [namespace current]::${interp}::Cancel]} { return -code error "Timed out after [set [namespace current]::${interp}::Cancel]ms." }
		}
		interp alias $interp timeout {} [namespace current]::Timeout $interp

		interp hide $interp info
		proc Info {interp option args} {
			switch -glob -- $option {
				{b*} {
					if {[set code [catch { eval [linsert $args 0 interp invokehidden $interp info $option] } out]]} {
						return -code $code $out
					} elseif {[string match {timeout ; *} $out]} {
						# We take out the "timeout ;" part of the body..
						string range $out 10 end
					} else {
						set out
					}
				}
				{default} { eval [linsert $args 0 interp invokehidden $interp info $option] }
			}
		}
		interp alias $interp info {} [namespace current]::Info $interp

		# We hafta provide the limited file command since safe::interpCreate is b0rked...
		if {[info commands file] == {}} {
			interp hide $interp file
			proc File {interp option name args} {
				switch -glob -- [string tolower "$option"] {
					{ch*} { set option channels }
					{di*} { set option dirname }
					{ext*} { set option extension }
					{j*} { set option join }
					{na*} { set option nativename }
					{no*} { set option normalize }
					{ro*} { set option rootname }
					{se*} { set option separator }
					{sp*} { set option split }
					{ta*} { set option tail }
					{default} { return -code error "file $option is not allowed!" }
				}
				eval [linsert [split $args] 0 interp invokehidden $interp file $option $name]
			}
			interp alias $interp file {} [namespace current]::File $interp
		}

		# This provides reasonable emulation of the puts command:
		# Putting to stdout makes it save it to a variable, which can be processed later.
		interp hide $interp puts
		proc Puts {interp args} {
			switch -- [llength $args] {
				{3} {
					switch -- [lindex $args 0] {
						{-nonewline} { set newline {} }
						{default} { return -code error {wrong # args: should be "puts ?-nonewline? ?channelId? string"} }
					}
					set socket [lindex $args 1]
					set text [lindex $args 2]
				}
				{2} {
					switch -- [lindex $args 0] {
						{-nonewline} {
							set newline {}
							set socket {stdout}
						}
						{default} {
							set newline {}
							set socket [lindex $args 0]
						}
					}
					set text [lindex $args 1]
				}
				{1} {
					set newline "\n"
					set socket {stdout}
					set text [lindex $args 0]
				}
				{default} { return -code error {wrong # args: should be "puts ?-nonewline? ?channelId? string"} }
			}
			if {[string is alnum -strict $socket] && [string length $socket] < 32} {
				switch -glob -- $socket {
					{sock*} - {file*} {
						## Try to send to a real socket.
						#set error [catch { eval [linsert $args 0 interp invokehidden $interp puts] } return]
						#update idletasks
						#return -code $error $return
						return -code error "can not find channel named \"$socket\""
					}
					{default} { lappend [namespace current]::${interp}::puts $socket "$text$newline" }
				}
				return {}
			} else {
				return -code error {bad channelId.  =P}
			}
		}
		interp alias $interp puts {} [namespace current]::Puts $interp

		proc PutLogLev {interp level channel text} {
			if {![string is alnum -strict $level] || [string length $level] > 32} {
				return -code error {Invalid log level. =P}
			} elseif {![string is alnum -strict $channel] || [string length $channel] > 254} {
				return -code error {Bad channel name.  =P}
			} else {
				lappend [namespace current]::${interp}::putloglev $level $channel $text
				return {}
			}
		}
		interp alias $interp putloglev {} [namespace current]::PutLogLev $interp

		interp hide $interp while
		proc While {interp condition body} { interp invokehidden $interp while $condition "timeout ; $body" }
		interp alias $interp while {} [namespace current]::While $interp

		interp hide $interp for
		proc For {interp start test next command} { interp invokehidden $interp for $start $test $next "timeout ; $command" }
		interp alias $interp for {} [namespace current]::For $interp

		# Workaround for bug in 8.4.1 and older that causes Seg Faults:
		# Note: This code is imperfect, so it's been commented out,
		#       be sure you're using a Tcl version > 8.4.1
		#proc Lsearch {interp args} {
		#	switch -- [llength $args] {
		#		{3} {
		#			set options [lindex $args 0]
		#			set list [lindex $args 1]
		#			set pattern [lindex $args 2]
		#		}
		#		{2} {
		#			set options {-glob}
		#			set list [lindex $args 0]
		#			set pattern [lindex $args 1]
		#		}
		#		{default} {
		#			eval interp invokehidden $interp lsearch $args
		#		}
		#	}
		#	if {[info tclversion] < {8.5} && [lsearch [split $options] {-regexp}] != -1 && $list == $pattern} {
		#		interp invokehidden $interp lsearch $options [string range $list 0 end] $pattern
		#	} else {
		#		interp invokehidden $interp lsearch $options $list $pattern
		#	}
		#}
		#interp alias $interp lsearch {} [namespace current]::Lsearch $interp

		interp hide $interp interp
		proc Interp {interp cmd args} {
			switch -glob -- [string tolower $cmd] {
				{alias} { if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft [lindex $args 1] :]] != -1} { return -code error "You can't alias initial commands!  =P" } }
				{is*} { eval [linsert $args 0 interp invokehidden $interp interp $cmd] }
				{default} { return -code error "interp is not allowed! =P" }
			}
			eval [linsert $args 0 interp invokehidden $interp interp $cmd]
		}
		interp alias $interp interp {} [namespace current]::Interp $interp

		interp hide $interp rename
		proc Rename {interp oldName newName} {
			if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft $oldName :]] != -1} {
				return -code error "Attempt to rename command DENIED!  =P"
			} else {
				interp invokehidden $interp rename $oldName $newName
			}
		}
		interp alias $interp rename {} [namespace current]::Rename $interp

		interp hide $interp string
		proc String {interp args} {
			if {[string match {repe*} [lindex $args 0]] && ([set size [expr { [lindex $args end] * [string length [lindex $args end-1]] }]] > 400)} {
				return -code error "You can't repeat a string that long, that many times!  (Needed: $size  Allowed: 400)"
			} else {
				eval [linsert $args 0 interp invokehidden $interp string]
			}
		}
		interp alias $interp string {} [namespace current]::String $interp

		interp hide $interp proc
		proc Proc {interp name arguments body} {
			if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft $name :]] != -1} {
				error "Attempt to (re)define command DENIED!  =P"
			} else {
				interp invokehidden $interp proc $name $arguments "timeout ; $body"
			}
		}
		interp alias $interp proc {} [namespace current]::Proc $interp

		interp hide $interp namespace
		proc Namespace {interp subcommand args} {
			switch -glob -- $subcommand {
				{im*} { if {[lsearch -exact $args {-force}] != -1} { return -code error {Denied! =P  -force not allowed.} } }
				{d*} {
					foreach ns $args {
						switch -- [string trim $ns {: 	}] {
							{} {
								after idle [list after 0 [list [namespace current]::create $interp]]
								return "Resetting $interp ..."
							}
						}
					}
				}
			}
			eval [linsert $args 0 interp invokehidden $interp namespace $subcommand]
		}
		interp alias $interp namespace {} [namespace current]::Namespace $interp

		# Wrapper to prevent a segfault in Tcl <8.4.19 and <8.5.2
		interp hide $interp binary
		proc Binary {interp option args} {
			if {$option eq {format} && [string match -nocase {*x0?*} [lindex $args 0]]} {
				return -code error "Crash attempt DENIED! =P"
			} else {
				eval [linsert $args 0 interp invokehidden $interp binary $option]
			}
		}
		interp alias $interp binary {} [namespace current]::Binary $interp

		interp hide $interp time
		variable TimeNoticeLast 9
		proc Time {interp args} {
			if {[llength $args] == 2 && [lindex $args end] > 10000} {
				return -code error "[lindex $args end] is too high, use 10000 or less."
			} else {
				set notice 0
				set error 0
				switch -glob -- $args {
					{*file *} - {*for *} - {*info *} - {*interp *} - {*namespace *} - {*proc *} - {*puts *} - {*rename *} - {binary *} - {*string *} - {*while *} - {*reset *} - {*die *} - {*exit *} {
						# Always send the annoying notice for these commands.
						set error 1
					}
					{default} {
						# Only send the annoying notice once every 999 seconds otherwise.
						variable TimeNoticeLast
						if {[clock seconds] - $TimeNoticeLast > 99} {
							set TimeNoticeLast [clock seconds]
							set notice 1
						}
					}
				}
				if {$error} {
					interp eval $interp { return -code error {The following commands have extra overhead in pubsafetcl that makes their times inaccurate: file, for, info, interp, namespace, proc, puts, rename, binary, string, time, while.  You should time these commands from your own tclsh instead! } }
				} elseif {$notice} {
					interp eval $interp { puts NOTICE {The following commands have extra overhead in pubsafetcl that makes their times inaccurate: file, for, info, interp, namespace, proc, puts, rename, binary, string, time, while.  You should time these commands from your own tclsh instead!} }
				}
				eval [linsert $args 0 interp invokehidden $interp time]
			}
		}
		interp alias $interp time {} [namespace current]::Time $interp

		# We create a namespace under the current one for storing variables relating to the interp we just created:
		catch { namespace delete [namespace current]::$interp }
		namespace eval [namespace current]::$interp {
			variable minclicks [clock clicks]
			variable Count 0
			variable minclicks [expr { [clock clicks] - $minclicks + 9 }]
		}
		array set options [list -timelimit 1750]
		array set options $args
		namespace eval [namespace current]::$interp [list variable timeLimit $options(-timelimit)]
		namespace eval [namespace current]::$interp [list variable preEval {}]
		namespace eval [namespace current]::$interp [list variable postEval {}]
		namespace eval [namespace current]::$interp [list variable extraCommands {}]

		namespace eval [namespace current]::$interp [list variable InitialCommands [$interp eval {info commands}]]
		# Prevent abuse/confusion by not allowing commands with certain names:
		namespace eval [namespace current]::$interp [list lappend InitialCommands hi fu i it a the it you hello re wb thx see hey well but or please plz pls how what you are of that can do kthx boring yes no may where is typo muaha rtfm google www search com exe ask okay come op voice q it o k ok in thanks thank thx lol jk nah or in bye nn night stfu wtf omg get go howdy example could give me I Hi How Hello What like nah but well If hmm lmao and he eh heh hehe heheh hehehe heheheh ah ha hah haha hahah hahaha hahahah hes You ffs o_O O_o O_O o_o just Oo oO on off oh but yea yeah like u ah aha ask err wow that it lmfao asdf asd as er uh um umm uhm gee duh http www db blah its bleh save timer utimer :) =) =P :P :p =p tcl .tcl safetcl .safetcl .eval im {I'm} {} { } {	} "\n" {?} {:} {;} {/} {.} {..} {...}]
		namespace eval [namespace current]::$interp [list namespace export $interp]

		proc [namespace current]::${interp}::extraCommands {command {extraCommands {}}} {
			switch -- $command {
				{add} {
					variable InitialCommands
					set InitialCommands [lsort -unique [concat $InitialCommands $extraCommands]]
					foreach c $extraCommands {
						if {[catch { pubsafetcl expose $c }]} {
							if {[catch { pubsafetcl invokehidden rename ::$c ::tcl::${c}_orig }]} { catch { pubsafetcl eval rename ::$c ::tcl::${c}_orig } }
							if {[catch { pubsafetcl expose $c }]} { catch { pubsafetcl alias $c $c } }
						}
					}
					variable extraCommands_current $extraCommands
				}
				{remove} {
					if {$extraCommands == {}} {
						variable extraCommands_current
						set extraCommands $extraCommands_current
					}
					foreach c $extraCommands {
						catch { pubsafetcl hide $c }
						#if {[catch { pubsafetcl alias $c {} }]} {  }
						if {[catch { pubsafetcl invokehidden rename ::tcl::${c}_orig ::$c }]} { catch { pubsafetcl eval rename ::tcl::${c}_orig ::$c } }
					}
				}
			}
		}

		set [namespace current]::${interp}::extraCommands_current {}

		catch { rename ::$interp [namespace current]::${interp}::pubsafetcl }

		proc [namespace current]::${interp}::$interp {cmd args} { set namespace [namespace current]
			switch -- $cmd {
				{setting} - {set} - {variable} - {var} - {option} - {opt} - {config} - {configure} { variable [lindex $args 0] [lindex $args 1] }
				{fancyeval} {
					variable timeLimit
					set timerid [after $timeLimit [list set ${namespace}::Cancel $timeLimit]]
					# set arg [encoding convertto iso8859-1 $arg]
					variable preEval
					namespace eval $namespace $preEval
					extraCommands remove
					variable extraCommands
					variable puts [list]
					variable putloglev [list]
					variable minclicks
					#extraCommands add $extraCommands
					# FixMe: Increase the recursionlimit as necessary:
					catch { pubsafetcl recursionlimit 7 }
					# Tcl v8.5's resource limits http://tcl.tk/man/tcl8.5/TclCmd/interp.htm#M45
					catch { pubsafetcl limit time -granularity 1 -milliseconds 2499 -seconds [clock seconds] }
					set errlev [catch { set clicks [clock clicks] ; pubsafetcl eval [join $args] } out]
					set clicks [expr { [clock clicks] - $clicks - $minclicks - 9 }]
					variable Cancel
					if {[info exists Cancel]} { unset Cancel } else { after cancel $timerid }
					variable postEval
					namespace eval $namespace $postEval
					if {$errlev == 1} { set results [string map [list ${namespace}::Proc {proc} ${namespace}::Rename {rename} ${namespace}::While {while} ${namespace}::File {file} ${namespace}::For {for} ${namespace}::Lsearch {lsearch} ${namespace}::Interp {interp} ${namespace}::Info {info} ${namespace}::Timeout {timeout} {::safe::AliasLoad} {load}] $out] } else { set results $out }
					#extraCommands remove $extraCommands
					variable Count
					return [list puts $puts putloglev $putloglev results $results errorlevel $errlev clicks $clicks count [incr Count]]
				}
				{eval} {
					variable timeLimit
					set timerid [after $timeLimit [list set ${namespace}::Cancel $timeLimit]]
					ResourceReset
					# set arg [encoding convertto iso8859-1 $arg]
					set errlev [catch { pubsafetcl eval [join $args] } out]
					variable Cancel
					if {[info exists Cancel]} { unset Cancel } else { after cancel $timerid }
					if {$errlev == 1} { return -code error [string map [list ${namespace}::Proc {proc} ${namespace}::Rename {rename} ${namespace}::While {while} ${namespace}::File {file} ${namespace}::For {for} ${namespace}::Lsearch {lsearch} ${namespace}::Interp {interp} ${namespace}::Info {info} ${namespace}::Timeout {timeout} {::safe::AliasLoad} {load}] $out] } else { return $out }
				}
				{default} { eval [linsert $args 0 "${namespace}::pubsafetcl" $cmd] }
			}
		}
		namespace eval :: [list namespace import -force [namespace current]::${interp}::$interp]
		set interp
	}
}
