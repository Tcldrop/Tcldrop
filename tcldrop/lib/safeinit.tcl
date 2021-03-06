# Splited fropm pubsafetcl
# TODO: fix ident

			# Make some dummy variables:
			array set tcl_platform [list user nobody machine unknown os Unknown osVersion 0.0 pubsafetcl-rcsid {$Id$}]
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
			set rcsid {$Id$}
			proc rcsid {args} { puts unknown {$Id$} }
			# Make some fake commands that don't really do anything:
			if {[info commands load] == {}} {
				proc load {fileName {packageName {}} {interp {}}} { return -code error "couldn't load file \"$fileName\": [pwd]$fileName: cannot open shared object file: No such file or directory" }
			}
			if {[info commands source] == {}} { proc source {fileName} { return -code error {source is disabled.} } }
			proc cd {args} { update }
			proc timeout {args} { update idletasks }
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
			proc randhex {begin end} {
				if {![string match {0x[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]} $begin]} {
					return -code error "Expected hexadecimal number but got \"$begin\""
				} elseif {![string match {0x[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]} $end]} {
					return -code error "Expected hexadecimal number but got \"$end\""
				} elseif {$begin > $end} {
					return -code error {Invalid expression: begin is higher than end}
				}
				return "0x[format %04X [expr { int(rand() * ($end - $begin + 1) + $begin) }]]"
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
			# matchstr, like in Eggdrop:
			#if {![llength [info commands matchstr]]} {
			#	proc matchstr {pattern string args} {
			#		string match -nocase [string map {{[} {\[} "\\" {\\}} $pattern] $string
			#	}
			#}
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
			# added slashes, so it should return at least a var
			proc ansicolor {{color {}}} {
				switch -exact -- $color {
					bold { return "[format %c 27]\[1m" }
					underline { return "[format %c 27]\[4m" }
					blink { return "[format %c 27]\[5m" }
					inverted { return "[format %c 27]\[7m" }
					invisible { return "[format %c 27]\[8m" }
					fgblack { return "[format %c 27]\[30m" }
					fgred { return "[format %c 27]\[31m" }
					fggreen { return "[format %c 27]\[32m" }
					fgyellow { return "[format %c 27]\[33m" }
					fgblue { return "[format %c 27]\[34m" }
					fgmagenta { return "[format %c 27]\[35m" }
					fgcyan { return "[format %c 27]\[36m" }
					fgwhite { return "[format %c 27]\[37m" }
					bgblack { return "[format %c 27]\[40m" }
					bgred { return "[format %c 27]\[41m" }
					bggreen { return "[format %c 27]\[42m" }
					bgyellow { return "[format %c 27]\[43m" }
					bgblue { return "[format %c 27]\[44m" }
					bgmagenta { return "[format %c 27]\[45m" }
					bgcyan { return "[format %c 27]\[46m" }
					bgwhite { return "[format %c 27]\[47m" }
					plain - normal - reset - none - default { return "[format %c 27]\[0m" }
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
			proc addslashes {text} { string map [list \\ \\\\ | \\| \[ \\\[ \] \\\] \{ \\\{ \} \\\} \$ \\\$ ` \\` ' \\' \" \\\"] $text }; # fixed formating, I hope this works for all
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
			proc string2list {string} { lsearch -inline -not -all [split $string] {} }
			proc slindex {string index} { lindex [lsearch -inline -not -all [split $string] {}] $index }
			proc slrange {string {start {0}} {end {end}}} { lrange [lsearch -inline -not -all [split $string] {}] $start $end }
			proc sllength {string} { regexp -all -- {\S+} $string }
			# removes duplicates without sorting the input list:
			# lsort -unique -command {apply {{a b} {expr {-($a!=$b)}}}}
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
			# join [lsort -command {apply {args {expr {2*[rand 1]-1}}}} [split $word {}]] {}
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
			# This proc is defined before the timer is added
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

			# by Pixelz:
			proc fudd {args} { set subs [list {[rl]} w {[RL]} W qu qw Qu Qw {th\y} f {Th\y} F th d Th D {n[.]} {n, uh-hah-hah-hah.}]; foreach word $args { foreach {exp subSpec} $subs { set word [regsub -all -- $exp $word $subSpec] }; lappend retval $word }; return [join $retval] }
			proc pig args { foreach w $args { if {[string match -nocase {[aeiou]*} $w]} { lappend o ${w}way } elseif {[regexp -nocase -- {((?:qu|y)*[^aeiouy]*)(.*)} $w - 1 2]} { lappend o ${2}${1}ay } else { lappend o $w } } ; puts unknown [join $o] }
			proc chef {args} { set subs [list {a([nu])} {u\1} {A([nu])} {U\1} {a\Y} e {A\Y} E {en\y} ee {\Yew} oo {\Ye\y} e-a {\ye} i {\yE} I {\Yf} ff {\Yir} ur {(\w+?)i(\w+?)$} {\1ee\2} {\Yow} oo {\yo} oo {\yO} Oo {^the$} zee {^The$} Zee {th\y} t {\Ytion} shun {\Yu} {oo} {\YU} {Oo} v f V F w w W W {([a-z])[.]} {\1. Bork Bork Bork!}]
				foreach word $args { foreach {exp subSpec} $subs { set word [regsub -all -- $exp $word $subSpec] } ; lappend retval $word } ; puts unknown [join $retval]
			}
			proc rainbow {args} {
				array set col {0 04 1 07 2 08 3 09 4 03 5 10 6 11 7 12 8 02 9 06 10 13 11 05}
				set pos 0
				foreach c [split [join $args] {}] {
					if {$c eq { }} { append r { }; continue }
					append r "\003$col($pos)$c"
					if {$pos == 11} { set pos 0 } else { incr pos }
				}
				puts unknown $r\003
			}

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
			# if {[package vcompare [package provide Tcl] 8.4] < 0} {
				# proc ::tcl::K {a b} { set a }
				# proc lset {listName index val} { upvar $listName list
					# set list [lreplace [::tcl::K $list [set list {}]] $index $index $val]
				# }
			# }
			# forward-compatible dict:
			# Poor man's dict -- a pure tcl [dict] emulation
			# Source: http://wiki.Tcl.Tk/10609  (March 1, 2004)
			# Very slow, but complete.
			#
			# Not all error checks are implemented!
			# e.g. [dict create odd arguments here] will work
			#
			# Implementation is based on lists, [array set/get] and recursion
			# if {![llength [info commands dict]]} {
				# namespace eval ::tcl {}
				# proc dict {cmd args} { uplevel 1 [linsert $args 0 "::tcl::dict_$cmd"] }
				# proc ::tcl::dict_get {dv args} {
					# if {![llength $args]} { return $dv } else {
						# array set dvx $dv
						# eval [linsert [lrange $args 1 end] 0 ::tcl::dict_get $dvx([lindex $args 0])]
					# }
				# }
				# proc ::tcl::dict_exists {dv key args} {
					# array set dvx $dv
					# if {![info exists dvx($key)]} {
						# return 0
					# } elseif {[llength $args]} {
						# eval [linsert $args 0 ::tcl::dict_exists $dvx($key)]
					# } else {
						# return 1
					# }
				# }
				# proc ::tcl::dict_set {dvar key value args} {
					# upvar 1 $dvar dv
					# lappend dv
					# array set dvx $dv
					# if {![llength $args]} {
						# set dvx($key) $value
					# } else {
						# eval [linsert $args 0 ::tcl::dict_set dvx($key) $value]
					# }
					# set dv [array get dvx]
				# }
				# proc ::tcl::dict_unset {dvar key args} {
					# upvar 1 $dvar mydvar
					# if {![info exists mydvar]} { return }
					# array set dv $mydvar
					# if {![llength $args]} {
						# if {[info exists dv($key)]} { unset dv($key) }
					# } else {
						# eval [linsert $args 0 ::tcl::dict_unset dv($key)]
					# }
					# set mydvar [array get dv]
					# return {}
				# }
				# proc ::tcl::dict_keys {dv {pat {*}}} {
					# array set dvx $dv
					# array names dvx $pat
				# }
				# proc ::tcl::dict_append {dvar key args} {
					# upvar 1 $dvar dv
					# lappend dv
					# array set dvx $dv
					# eval [linsert $args 0 append dvx($key)]
					# set dv [array get dvx]
				# }
				# proc ::tcl::dict_create {args} { set args }
				# proc ::tcl::dict_filter {dv ftype args} {
					# set r [list]
					# foreach {globpattern} $args { break }
					# foreach {varlist script} $args { break }
					# switch $ftype {
						# key { foreach {key value} $dv { if {[string match $globpattern $key]} { lappend r $key $value } } }
						# value { foreach {key value} $dv { if {[string match $globpattern $value]} { lappend r $key $value } } }
						# script {
							# foreach {Pkey Pval} $varlist { break }
							# upvar 1 $Pkey key $Pval value
							# foreach {key value} $dv { if {[uplevel 1 $script]} { lappend r $key $value } }
						# }
						# default { return -code error {Wrong filter type} }
					# }
					# set r
				# }
				# proc ::tcl::dict_for {kv dict body} { uplevel 1 [list foreach $kv $dict $body] }
				# proc ::tcl::dict_incr {dvar key {incr 1}} {
					# upvar 1 $dvar dv
					# lappend dv
					# array set dvx $dv
					# if {![info exists dvx($key)]} { set dvx($key) 0 }
					# incr dvx($key) $incr
					# set dv [array get dvx]
				# }
				# proc ::tcl::dict_info {dv} { return {Dictionary is represented as plain list} }
				# proc ::tcl::dict_lappend {dvar key args} {
					# upvar 1 $dvar dv
					# lappend dv
					# array set dvx $dv
					# eval [linsert $args 0 lappend dvx($key)]
					# set dv [array get dvx]
				# }
				# proc ::tcl::dict_replace {dv args} {
					# foreach {k v} $args { ::tcl::dict_set dv $k $v }
					# set dv
				# }
				# proc ::tcl::dict_remove {dv args} {
					# foreach k $args { ::tcl::dict_unset dv $k }
					# set dv
				# }
				# proc ::tcl::dict_size {dv} { expr { [llength $dv] / 2 } }
				# proc ::tcl::dict_values {dv {gp *}} {
					# set r [list]
					# foreach {k v} $dv { if {[string match $gp $v]} { lappend r $v } }
					# set r
				# }
			# }
			# forward-compatible lrepeat:
			# if {![llength [info commands lrepeat]]} { proc lrepeat {count args} {string repeat "$args " $count} }
			# forward-compatible lassign:
			# if {![llength [info commands lassign]]} {
				# proc lassign {values args} {
					# while {[llength $values] < [llength $args]} { lappend values {} }
					# uplevel 1 [list foreach $args $values {break}]
					# lrange $values [llength $args] end
				# }
			# }
