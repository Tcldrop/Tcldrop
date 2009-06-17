# pubsafetcl-bitlbee.tcl v0.1 - by FireEgl@EFNet <SafeTcl@Atlantica.US> (http://Tcldrop.US/safetcl) - 2006

### Description:
# Gives you a public Tcl command interpreter that everybody can use! =)
# It runs the Tcl commands in a tweaked safe interpreter to avoid any exploits.
# See "man n interp" and "man n safe" for more info on safe interpreters.

### Reasons:
# Well I'm an op in #Tcl on EFNet.. So this comes in handy, I think, when helping people.

### Notes:
# If there's any exploits in this script then please tell me..
# I already know it's possible to "lag" the bot with lengthy commands.. And that's expected.
# See the comments inside the reset proc to see what I added/changed in the safe interpreter.
# I recommend using the latest version of Tcl on your bot (currently v8.4.9).

### Examples of Usage:
## All of the following will be evaluated as a Tcl command
## when typed on a channel with the +safetcl channel flag:
#
# ;set test 123
# .safetcl incr test
# [info patchlevel]
# ; unset test
# .tcl set test 2

## This will also work, but if an error is encountered, it won't return the error.
# set test [expr $test * 2]

### TODO:
# 1. Have a list of trusted Tcl commands, that certain trusted people can use.
#    a. It should expose the commands in the safe interpreter before executing their code, and then hide the commands again afterwards.
# 2. See the FixMe's.

# $Id:¤

### Begin Script:
catch { package forget pubsafetcl::bitlbee }
catch { package forget pubsafetcl }
if {[catch { package require pubsafetcl }] && [catch { source [file join scripts pubsafetcl.tcl] }] && [catch { source [file join [file dirname [info script]] pubsafetcl.tcl] }]} { putlog {ERROR: pubsafetcl-bitlbee.tcl won't load without the pubsafetcl.tcl package.} }
namespace eval pubsafetcl::bitlbee {
	### Options:
	## How long in milliseconds to wait for safetcl commands to complete.
	# After this long they're aborted.  (1000ms = 1sec)
	# The higher this is, the more likely your Eggdrop can get lagged off of IRC.
	variable timeLimit 100

	## How long to wait for somebody to paste/type a multi-line script (in seconds)?
	variable multilineWait 9

	# Don't allow more than this many lines of stdout/results:
	variable lineLimit 99

	variable extraCommands
	# These are extra commands that will be available for people with certain flags..
	array set extraCommands {
		n {time encoding fconfigure pid glob pwd loadmodule loadhelp reloadhelp dellang binds addlangsection checkmodule language addlang relang dellangsection putdcc putact putmsg putnotc rehash}
		mn {dcclist putcmdlog iscompressed utimers timers ignorelist backup savechannels save addchanrec getchan}
		tmn {matchbotattr link getaddr}
		jmn {getdccdir getuploads getfilesendtime}
		omn {gethosts getchanmode getchaninfo}
		fomn {handonanychan ispermowner chanlist findnick userlist matchchanattr matchattr chanbans ishalfop banlist exemptlist botisop chanexempts chaninvites invitelist botishalfop channels isinvite}
		ptmn {whom botlist bots getdccidle getdccaway}
	}

	# Initialize/reset the safe interpreter:
	::pubsafetcl::create safetcl -timelimit $timeLimit
	package provide pubsafetcl::bitlbee 0.1

	proc Connect {args} {
		variable Socket
		catch { close $Socket }
		set Socket [socket -async 10.1.7.1 6667]
		fileevent $Socket writable [list [namespace current]::Write $Socket]
		fconfigure $Socket -blocking 0 -buffering line
	}

	proc Write {socket} {
		fileevent $socket writable {}
		puts $socket "NICK SafeTcl\nUSER SafeTcl . . :SafeTcl\nPRIVMSG root :identify Silly123"
		fileevent $socket readable [list [namespace current]::Read $socket]
	}

	proc Read {socket} {
		while {![catch { gets $socket line } error] && $error > 0} {
			putlog "BitlBee: $line"
			if {[string index $line 0] == {:}} {
				#:FireEgl_Jabber!fireegl@jabber.org PRIVMSG SafeTcl :testing 1 2 3
				# Received a PRIVMSG.
				if {[string match {*You can use the * commands to accept/reject this request.*} $line]} { puts $socket "PRIVMSG root :yes" }
				set line [split [string trimleft $line :]]
				set nick [string trimleft [lindex [split [lindex $line 0] !] 0] :]
				if {$nick == {root}} { continue }
				if {$nick == {SafeTcl}} { continue }
				if {$nick == {Melody.Atlantica.US}} { continue }
				if {$nick == {localhost.}} { continue }
				set msg [string range [join [lrange $line 3 end]] 1 end]
				if {[string trim $msg] == {}} { continue }
				#puts $socket "PRIVMSG $nick :You said $msg"
				EggdropPub $nick $nick * $msg
				#PRIVMSG FireEgl_Jabber :test a b c
			} else {
				switch -- [lindex [split $line] 0] {
					{PING} {
						#PING :PinglBee
						#PONG :PinglBee
						puts $socket "PONG :PinglBee"
					}
				}
			}
		}
		if {[eof $socket]} {
			Connect
		}
	}


	# This is used in places where we only get one line at a time, like on IRC or telnet.
	# When it's complete, it will return the total stored code.
	proc completeGet {{id {default}} {delete 1}} {
		variable Completes
		set arg $Completes($id,script)
		if {$delete} { array unset Completes $id,* }
		set arg
	}
	# Returns 0 as long as the Tcl code associated with id is incomplete, or 1 when it is complete.
	proc completeCheck {arg {id {default}}} {
		variable Completes
		variable multilineWait
		# Remove them as their time expires:
		foreach {a d} [array get Completes *,time] {
			if {[expr { [clock seconds] - $d > $multilineWait }]} {
				array unset Completes "[lindex [split $a ,] 0],*"
			}
		}
		if {![info exists Completes($id,time)]} {
			set Completes($id,time) [clock seconds]
		}
		if {[info complete [append Completes($id,script) "$arg\n"]]} {
			return 1
		} else {
			return 0
		}
	}

	proc hassmiley {text} {
		switch -glob -- $text {
			{;(*} - {;)*} - {;>*} - {;<*} - {;\{*} - {;\}*} - {;\**} - {;^*} - {;@*} - {;!*} - {;|*} - {;+*} - {;-*} - {;\\*} - {;/*} - {;\?*} - {;\[*} - {;\]*} - {;D*} - {;P*} { return 1 }
			{default} { return 0 }
		}
	}

	proc safetclEval {arg {errors {1}}} {
		array set evalinfo [list puts {} putloglev {}]
		array set evalinfo [safetcl fancyeval $arg]
		if {!$errors && $evalinfo(errorlevel)} {
			return {}
		} else {
			variable lineLimit
			set linecount 0
			foreach {s t} $evalinfo(puts) {
				if {$s == {unknown}} {
					lappend return "$t"
				} else {
					lappend return "#$evalinfo(count) ($s) $t"
				}
				if {[incr linecount] >= $lineLimit} { break }
			}
			foreach {l c t} $evalinfo(putloglev) {
				lappend return "#$evalinfo(count) (putloglev $l/$c) $t"
				if {[incr linecount] >= $lineLimit} { break }
			}
			if {$linecount == 0 || $evalinfo(results) != {}} {
				if {$evalinfo(errorlevel)} { set extra {Tcl error:} } else { set extra {Tcl:} }
				foreach l [split $evalinfo(results) \n] {
					lappend return "#$evalinfo(count) ($evalinfo(clicks) clicks) $extra $l"
					if {[incr linecount] >= $lineLimit} { break }
				}
			}
			if {$linecount == 0 && $errors} {
				lappend return "#$evalinfo(count) ($evalinfo(clicks) clicks) $extra"
			} elseif {$linecount >= $lineLimit && [set linecount [expr {[llength [concat $evalinfo(puts) [split $evalinfo(results) \n]]] - $linecount - 1}]] >= 1} {
				# FixMe: $evalinfo(puts) and $evalinfo(putloglev) won't be the correct llength.
				lappend return "There's $linecount more lines, but I'm not gonna show you them!  =P"
			}
			return $return
		}
	}

	## Provide the public commands:

	#bind pub n !tcl [namespace current]::EggdropPub

	#variable EggdropPubBinds [list .tcl ,tcl .safetcl ,safetcl .eval ,eval]
	#foreach EggdropPubBind $EggdropPubBinds { bind pub - $EggdropPubBind [namespace current]::EggdropPub }
	#catch { unset EggdropPubBind }
	proc EggdropPub {nick host hand arg {errors {1}}} {
		variable Socket
		if {[matchattr $hand bdkqr|dkqr] || [isbotnick $nick] || [isignore $nick!$host]} {
			return 0
		} else {
			variable extraCommands
			set commands {}
			safetcl setting extraCommands $commands
			array set evalinfo [list puts {} putloglev {}]
			array set evalinfo [safetcl fancyeval $arg]
			if {[matchattr $hand n]} {
				safetcl setting preEval {
					catch { cd /tmp }
				}
				safetcl setting postEval {
					catch { cd [file join $::env(HOME) eggdrop] }
				}
			}
			if {!$errors && $evalinfo(errorlevel)} {
				return 0
			} else {
				variable lineLimit
				set linecount 0
				foreach {s t} $evalinfo(puts) {
					foreach t [split [string trimright $t \n] \n] {
						if {$s == {unknown}} {
							puts $Socket "PRIVMSG $nick :[string map {"\001" {}} [string range $t 0 300]]"
						} else {
							puts $Socket "PRIVMSG $nick :#$evalinfo(count) ($s) [string map {"\001" {}} [string range $t 0 300]]"
						}
						if {[incr linecount] >= $lineLimit} { break }
					}
					if {[incr linecount] >= $lineLimit} { break }
				}
				foreach {l c t} $evalinfo(putloglev) {
					if {![string match -nocase $c -]} {
						puts $Socket "PRIVMSG $nick :#$evalinfo(count) (putloglev $l) [string map {"\001" {}} [string trimright [string range $t 0 300]]]"
						if {[incr linecount] >= $lineLimit} { break }
					}
				}
				if {$linecount == 0 || $evalinfo(results) != {} || !$evalinfo(errorlevel) || $errors} {
					if {$evalinfo(errorlevel)} { set extra {Tcl error:} } else { set extra {Tcl:} }
					foreach l [split $evalinfo(results) \n] {
						puts $Socket "PRIVMSG $nick :#$evalinfo(count) ($evalinfo(clicks) clicks) $extra [string map {"\001" {}} [string range $l 0 300]]"
						if {[incr linecount] >= $lineLimit} { break }
					}
				}
				set linecount [expr {[llength [split $evalinfo(results) \n]]}]
				foreach {s t} $evalinfo(puts) { foreach l [split $t \n] { incr linecount } }
				foreach {l c t} $evalinfo(putloglev) { foreach l [split $t \n] { incr linecount } }
				if {$linecount == 0 && $errors} {
					puts $Socket "PRIVMSG $nick :#$evalinfo(count) ($evalinfo(clicks) clicks) [string map {"\001" {}} $extra]"
				} elseif {$linecount > $lineLimit} {
					puts $Socket "PRIVMSG $nick :$There's $linecount lines, but I'm not gonna show you the rest of them!  =P"
				}
			}
		}
	}
	#bind pubm - * [namespace current]::EggdropPubm
	proc EggdropPubm {nick host hand arg} {
		set errors 0
		if {([string trim $arg ;] == {}) || [hassmiley $arg]} {
			return 0
		} elseif {[string index [set arg [string trim $arg]] 0] == {;} && [string length $arg] > 2} {
			set arg [string trimleft $arg {;}]
			set errors 1
		} elseif {(("[string index $arg 0]" == {[}) && ("[string index $arg end]" == {]}))} {
			set arg [string range $arg 1 end-1]
			set errors 1
		}
		if {[completeCheck $arg "$host"]} {
			EggdropPub $nick $host $hand [completeGet "$host"] $errors
		}
	}
	if {[info commands ispermowner] == {}} { proc ispermowner {hand} { if {(([matchattr $hand n]) && ([lsearch -exact [regsub -all -- {,} "[string tolower $::owner]" {}] [string tolower "$hand"]] != -1))} { return 1 } else { return 0 } } }

	# Auxiliary commands:
	proc slindex {string index} { lindex [string2list $string] $index }
	proc slrange {string start end} { lrange [string2list $string] $start $end }
	proc string2list {string} { if {[catch {llength $string}]} { regexp -inline -all -- {\S+} $string } else { return $string } }

	variable script [file tail [info script]]
	putlog "$script v0.1 - by FireEgl@EFNet <FireEgl@Tcldrop.US> http://www.Tcldrop.US/safetcl - Loaded."
	variable Socket
	if {![info exists Socket] || [lsearch -exact [file channels] $Socket] == -1} {
		catch { close $Socket }
		after idle [list [namespace current]::Connect]
	}
}
