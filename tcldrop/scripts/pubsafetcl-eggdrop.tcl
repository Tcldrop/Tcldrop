# pubsafetcl-eggdrop.tcl v2.2.1 - by FireEgl@EFNet <SafeTcl@Atlantica.US> (http://Tcldrop.Org/safetcl) - May 2006

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

### History:
# v1.0 - First Release.
# v1.1 - Added back access to the for and while commands.
#      - There's now a 10 second time limit on on the execution of for, while, and the body of proc's.
#      - Tabbified the code.
#      - TCL commands can now start with ;
# v1.2 - Fixed some bugs, added back the rename command.. But you can't rename internal tcl commands, only procs.
# v1.3 - Made several more Eggdrop tcl commands available to the safe interpreter.
#      - Added access to the "file" command, but restricted it to only safe options.
#      - TCL commands can now start with [ and end with ]
#      - Added a few procs to the safe interpreter that people might find useful.
#      - .tcl commands typed from dcc/telnet with the bot will be ran from the safe interpreter for non-owners.
#      - Moved a few bits of code around, it's starting to get cluttered.. =/
# 1.4  - Added a +safetcl channel flag; Use ".chanset #YourChannel +safetcl" to enable it on a channel.
#      - Allows multi-line input on +safetcl channels.
#      - Removed the puts proc, as it isn't possible to emulate the real puts.
#      - Only public non-safe Tcl commands will show in the cmdlog.
# 1.5  - Added several procs from Eggdrop's alltools.tcl.
#      - No longer allow any exec commands from the safe interpreter, simulate some common ones instead.
#      - Limit to only +safetcl channels, except for the .tcl command.
#      - Added a few procs from http://Wiki.Tcl.Tk/
#      - Workaround for critical bug in lsearch -regexp.
#      - Telnet access to the safe interpreter.. (emulates a tclsh prompt)
#      - Web access to the safe interpreter. (via a web form)
# 1.6  - Official release, considered stable.
#      - From now on, odd numbered version=unstable, even numbered=stable.
#      - I'm releasing this before I make some major changes.
# 1.7  - Modularized all the procs.
#      - Now loads the script into it's own namespace.
#      - Attempts to use safe::* but falls back to using interp if it fails.  (safe::interpCreate is currently broken as of 2003/02/08, see notes in script)
#      - Added DCC CHAT access to the safe interpreter.
# 2.0  - Split the script into a pubsafetcl package, and an eggdrop script.
# 2.1  - Misc. small changes.
#

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

### Begin Script:
catch { package forget pubsafetcl::eggdrop }
catch { package forget pubsafetcl }
if {[catch { package require pubsafetcl }] && (![file readable [file join scripts pubsafetcl.tcl]] || [catch { source [file join scripts pubsafetcl.tcl] }]) && (![file readable [file join [file dirname [info script]] pubsafetcl.tcl]] || [catch { source [file join [file dirname [info script]] pubsafetcl.tcl] }]) && (![file readable [file join lib pubsafetcl.tcl]] || [catch { source [file join lib pubsafetcl.tcl] }])} { putlog {ERROR: pubsafetcl-eggdrop.tcl won't load without the pubsafetcl.tcl package.} }
namespace eval pubsafetcl::eggdrop {
	### Options:
	## Allow DCC CHAT access on this port (Use 0 to disable):
	# Note: This feature doesn't work at all yet.
	variable dccChatPort 0

	## How long in milliseconds to wait for safetcl commands to complete.
	# After this long they're aborted.  (1000ms = 1sec)
	# The higher this is, the more likely your Eggdrop can get lagged off of IRC.
	variable timeLimit 250

	## How long to wait for somebody to paste/type a multi-line script (in seconds)?
	variable multilineWait 9

	# Don't allow more than this many lines of stdout/results:
	variable lineLimit 4

	variable partyLine 0

	variable extraCommands
	# FEATURE DISABLED.  These are extra commands that will be available for people with certain flags..
	array set extraCommands {
		n {time encoding fconfigure pid glob pwd loadmodule loadhelp reloadhelp dellang binds addlangsection checkmodule language addlang relang dellangsection putdcc putact putmsg putnotc rehash}
		mn {dcclist putcmdlog iscompressed utimers timers ignorelist backup savechannels save addchanrec getchan}
		tmn {matchbotattr link getaddr}
		jmn {getdccdir getuploads getfilesendtime}
		omn {gethosts getchanmode getchaninfo memory}
		fomn {handonanychan ispermowner chanlist findnick userlist matchchanattr matchattr chanbans ishalfop banlist exemptlist botisop chanexempts chaninvites invitelist botishalfop channels}
		ptmn {whom botlist bots getdccidle getdccaway}
	}

	# Initialize/reset the safe interpreter:
	::pubsafetcl::create safetcl -timelimit $timeLimit
	package provide pubsafetcl::eggdrop 2.2.1

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

	proc preferredbot {channel} { variable safetclbots
		if {[info exists safetclbots]} {
			foreach b $safetclbots {
				if {[isbotnetnick $b]} {
					return 1
				} elseif {[handonchan $b $channel]} {
					return 0
				}
			}
		}
		return 1
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
				if {$evalinfo(errorlevel)} {
					if {$evalinfo(errorlevel) != 1} {
						set extra "Tcl error ($evalinfo(errorlevel)):"
					} else {
						set extra {Tcl error:}
					}
				} else {
					set extra {Tcl:}
				}
				foreach l [split $evalinfo(results) \n] {
					#lappend return "#$evalinfo(count) ($evalinfo(clicks) clicks) $extra $l"
					lappend return "#$evalinfo(count) $extra $l"
					if {[incr linecount] >= $lineLimit} { break }
				}
			}
			if {$linecount == 0 && $errors} {
				#lappend return "#$evalinfo(count) ($evalinfo(clicks) clicks) $extra"
				lappend return "#$evalinfo(count) $extra"
			} elseif {$linecount >= $lineLimit && [set linecount [expr {[llength [concat $evalinfo(puts) [split $evalinfo(results) \n]]] - $linecount - 1}]] >= 1} {
				# FixMe: $evalinfo(puts) and $evalinfo(putloglev) won't be the correct llength.
				lappend return "There's $linecount more lines, but I'm not gonna show you them!  =P"
				# lappend return "There's more lines, but I'm not gonna show you them!  =P"
			}
			return $return
		}
	}

	## Provide the public commands:
	variable EggdropPubBinds [list !tcl .tcl ,tcl .safetcl ,safetcl .eval ,eval]
	foreach EggdropPubBind $EggdropPubBinds { bind pub - $EggdropPubBind [namespace current]::EggdropPub }
	catch { unset EggdropPubBind }
	proc EggdropPub {nick host hand chan arg {errors {1}}} {
		if {![channel get $chan safetcl] || [matchattr $hand bdkqr|dkqr] || [isbotnick $nick] || [isignore $nick!$host] || [matchban $nick!$host $chan]} {
			return 0
		} elseif {[preferredbot $chan]} {
			variable extraCommands
			set commands {}
			if {[isop $nick $chan]} { foreach f [array names extraCommands] { if {[matchattr $hand $f|$f $chan]} { set commands [lsort -unique [concat $extraCommands($f) $commands]] } } }
			safetcl setting extraCommands $commands
			array set evalinfo [list puts {} putloglev {}]
			array set evalinfo [safetcl fancyeval $arg]
			if {!$errors && $evalinfo(errorlevel)} {
				return 0
			} else {
				variable lineLimit
				set linecount 0
				foreach {s t} $evalinfo(puts) {
					foreach t [split [string trimright $t \n] \n] {
						if {$s == {unknown}} {
							puthelp "PRIVMSG $chan :$nick\002:\002 [string map {"\001" {}} [string range $t 0 400]]"
						} else {
							puthelp "PRIVMSG $chan :$nick\002:\002 #$evalinfo(count) ($s) [string map {"\001" {}} [string range $t 0 400]]"
						}
						if {[incr linecount] >= $lineLimit} { break }
					}
					if {[incr linecount] >= $lineLimit} { break }
				}
				foreach {l c t} $evalinfo(putloglev) {
					if {[string match -nocase $c $chan]} {
						puthelp "PRIVMSG $chan :$nick\002:\002 #$evalinfo(count) (putloglev $l) [string map {"\001" {}} [string trimright [string range $t 0 400]]]"
						if {[incr linecount] >= $lineLimit} { break }
					}
				}
				if {$linecount == 0 || $evalinfo(results) != {} || !$evalinfo(errorlevel) || $errors} {
					if {$evalinfo(errorlevel)} {
						if {$evalinfo(errorlevel) != 1} {
							set extra "Tcl error ($evalinfo(errorlevel)):"
						} else {
							set extra {Tcl error:}
						}
					} else {
						set extra {Tcl:}
					}
					foreach l [split $evalinfo(results) \n] {
						#puthelp "PRIVMSG $chan :$nick\002:\002 #$evalinfo(count) ($evalinfo(clicks) clicks) $extra [string map {"\001" {}} [string range $l 0 400]]"
						puthelp "PRIVMSG $chan :$nick\002:\002 #$evalinfo(count) $extra [string map {"\001" {}} [string range $l 0 400]]"
						if {[incr linecount] >= $lineLimit} { break }
					}
				}
				set linecount [expr {[llength [split $evalinfo(results) \n]]}]
				foreach {s t} $evalinfo(puts) { foreach l [split $t \n] { incr linecount } }
				foreach {l c t} $evalinfo(putloglev) { foreach l [split $t \n] { incr linecount } }
				if {$linecount == 0 && $errors} {
					#puthelp "PRIVMSG $chan :$nick\002:\002 #$evalinfo(count) ($evalinfo(clicks) clicks) [string map {"\001" {}} $extra]"
					puthelp "PRIVMSG $chan :$nick\002:\002 #$evalinfo(count) [string map {"\001" {}} $extra]"
				} elseif {$linecount > $lineLimit} {
					puthelp "PRIVMSG $chan :$nick\002:\002 There's more lines, but I'm not gonna show you the rest of them!  =P"
				}
			}
		}
	}
	bind pubm - * [namespace current]::EggdropPubm
	proc EggdropPubm {nick host hand chan arg} {
		if {[channel get $chan safetcl] && [preferredbot $chan]} {
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
			if {[completeCheck $arg "$host@$chan"]} {
				EggdropPub $nick $host $hand $chan [completeGet "$host@$chan"] $errors
			}
		}
	}
	## Provides the dcc .tcl command:
	# This is here so that non-perm-owners can have their .tcl commands ran from the safe interpreter. =)
	#bind dcc p tcl [namespace current]::EggdropDCCTcl
	proc EggdropDCCTcl {hand idx arg} {
		if {[ispermowner $hand]} {
			if {[info commands *dcc:tcl] != {}} {
				# This is special for Eggdrop:
				*dcc:tcl $hand $idx $arg
			} elseif {[info commands ::tcldrop::dcc::TCL] != {}} {
				# This is special for Tcldrop:
				::tcldrop::dcc::TCL $hand $idx $arg
			} else {
				# This is for non-Eggdrop bots, like Tcldrop:
				putcmdlog "#$hand# tcl $arg"
				if {[catch { uplevel \#0 $arg } out]} { set out "TCL error: $out" } else { set out "Tcl: $out" }
				putdcc $idx $out
			}
		} else {
			foreach l [safetclEval $arg] { putdcc $idx $l }
		}
	}

	## Provides the dcc .safetcl command:
	bind dcc p safetcl [namespace current]::EggdropDCCSafeTcl
	proc EggdropDCCSafeTcl {hand idx arg} { foreach l [safetclEval $arg] { putdcc $idx $l } }

	if {$partyLine} {
		# Provides the partyline Safe Tcl interpreter:
		bind chat - * [namespace current]::EggdropChat
		proc EggdropChat {hand channel arg} {
			if {[hassmiley $arg]} { return }
			# FixMe: Make this work just like pubm does.
			if {[string index $arg 0] == {;}} { foreach l [safetclEval [string trimleft $arg {;}]] { dccputchan $channel "${hand}: $l" } }
		}
	}

	if {1024 < $dccChatPort && $dccChatPort < 65535} {
		if {[info exists DccChatSock]} {
			putlog "SafeTcl/DCC: Already listening on port ${dccChatPort}."
		} elseif {[catch { socket -server [namespace current]::DccChatServerConnect ${dccChatPort} } DccChatSock]} {
			putlog "SafeTcl/DCC: Can't listen on port (${dccChatPort}): ${DccChatSock}"
			unset DccChatSock
		} else {
			fconfigure ${DccChatSock} -buffering line -blocking 0
			putlog "SafeTcl/DCC: Listening on port ${dccChatPort}."
		}
		proc DccChatServerConnect {sock ip port} { fconfigure $sock -buffering line -blocking 0
			putlog "SafeTcl/DCC: Got connect from $sock ($ip:$port)."
			set timerid [utimer 9 [list [namespace current]::DccChatCheck $sock {DCCUser} {*} $ip $port]]
			fileevent $sock writable [list [namespace current]::DccChatIntro $sock $ip $port {DCCUser} {*} $timerid]
		}
		bind ctcp - DCC [namespace current]::EggdropDccChat
		proc EggdropDccChat {nick host hand dest key text} { putlog "In EggdropDccChat."
			putlog "nick: $nick\n host: $host\n hand: $hand\n dest: $dest\n key: $key\n text: $text"
			if {[isbotnick $dest] && $key == {DCC} && [string toupper [lindex [set text [split $text]] 0]] == {CHAT} && [expr 1024 < [set port [lindex $text end]] < 65535]} {
				set host [lindex [split $host @] end]
				set althost [lindex $text 2]
				DccChatConnect $nick $hand $host $port $althost
			} elseif {[isbotnick $dest] && [regexp -nocase {chat|tclchat|chattcl|safetcl|tcl|dcctcl|tcldcc} "$key"]} {
				variable MyDecimalIP
				variable dccChatPort
				puthelp "PRIVMSG $nick :\001DCC CHAT chat $MyDecimalIP $dccChatPort\001"
			}
			return 1
		}
		proc DccChatConnect {nick hand host port {althost {}}} {
			if {[catch { socket -async $host $port } sock]} {
				putlog "SafeTcl/DCC: Problem connecting to $host: $sock"
				if {$althost != {}} {
					DccChatConnect $nick $hand $althost $port
				} else {
					variable MyDecimalIP
					variable dccChatPort
					puthelp "PRIVMSG $nick :\001DCC CHAT chat $MyDecimalIP $dccChatPort\001"
				}
			} else {
				fconfigure $sock -buffering line -blocking 0
				putlog "SafeTcl/DCC: Attempting to connect to $host:$port..."
				set timerid [utimer 9 [list [namespace current]::DccChatCheck $sock $nick $hand $host $port $althost]]
				fileevent $sock writable [list [namespace current]::DccChatIntro $sock $host $port $nick $hand $timerid $althost]
			}
		}
		proc DccChatCheck {sock nick hand host port {althost {}}} {
			if {[eof $sock]} {
				putlog "SafeTcl/DCC: Got EOF on $sock"
			} elseif {[set error [fconfigure $sock -error]] != {}} {
				putlog "SafeTcl/DCC: Error: $error on $sock"
			}
			DccChatClose $sock $nick $hand $host $port
			if {$althost != {}} { DccChatConnect $nick $hand $althost $port }
		}
		proc DccChatClose {sock nick hand host port} {
			catch { fileevent $sock writable {} }
			catch { fileevent $sock readable {} }
			catch { close $sock }
		}
		# Provides a DCC CHAT'able Safe Tcl interpreter:
		proc DccChatIntro {sock host port nick hand {timerid {}} {althost {}}} {
			fileevent $sock writable {}
			catch { killutimer $timerid }
			if {[set error [fconfigure $sock -error]] == {}} {
				fileevent $sock readable [list [namespace current]::DccChatRead $sock $host $port $nick $hand]
				putlog "SafeTcl/DCC: Sending Intro to $host:$port ($sock)..."
				puts $sock "Hi $nick, you're connected to ${::botnet-nick}, running pubsafetcl.tcl.\nEnter your Tcl commands and I'll respond with the results.. Keep in mind that \"puts\" does NOT work."
			} elseif {$althost != {}} {
				putlog "SafeTcl/DCC: Error to $sock: $error.  Trying $althost..."
				DccChatCheck $sock $nick $hand $host $port $althost
			} else {
				putlog "SafeTcl/DCC: Got error on $sock: $error"
				DccChatClose $sock $nick $hand $host $port
				variable MyDecimalIP
				puthelp "PRIVMSG $nick :\001DCC CHAT chat $MyDecimalIP 44444\001"
			}
		}
		proc DccChatRead {sock host port nick hand} {
			if {[eof $sock] || [set length [gets $sock line]] == -1 || [slindex $line 0] == {exit}} {
				putlog "SafeTcl/DCC: EOF/exit from $host:$port ($sock)"
				DccChatClose $sock $nick $hand $host $port
			} elseif {[completeCheck $line "$sock-$host-$port"]} {
				foreach l [safetclEval [set line [completeGet "$sock-$host-$port"]]] { puts $sock $l }
				flush $sock
				putlog "SafeTcl/DCC: Read $length chars from $host:$port ($sock): $line"
			} elseif {$length == 0 && [completeGet "$sock-$host-$port" 0] == {}} {
				puts $sock {% }
				flush $sock
				putlog "SafeTcl/DCC: Read $host:$port ($sock) - $length chars: $line"
			}
		}

		# FixMe: Some of this may not be needed, as [myip] tells the bots IP in decimal notation.
		variable MyIP
		variable MyHostname
		if {![info exists MyIP]} { set MyIP {127.0.0.1} }
		if {![info exists MyHostname]} { set MyHostname {localhost} }
		bind evnt - init-server [namespace current]::EggdropInitServer
		proc EggdropInitServer {type} {
			catch {
				dnslookup [lindex [split $::botname @] end] [namespace current]::EggdropGetInternetIP
			}
		}
		proc EggdropGetInternetIP {ip host status} {
			if {$status || $ip == $host} {
				variable MyIP $ip
				variable MyDecimalIP [IP2Decimal $ip]
				variable MyHostname $host
			} elseif {$ip == {0.0.0.0}} {
				variable MyIP {127.0.0.1}
				variable MyDecimalIP {2130706433}
				variable MyHostname $host
			}
		}

		proc IP2Decimal {ip} {
			foreach {a b c d} [split $ip .] {}
			format %u 0x[format %02X%02X%02X%02X $a $b $c $d]
		}
	}
	if {[info commands ispermowner] == {}} { proc ispermowner {hand} { if {(([matchattr $hand n]) && ([lsearch -exact [regsub -all -- {,} "[string tolower $::owner]" {}] [string tolower "$hand"]] != -1))} { return 1 } else { return 0 } } }

	# Auxiliary commands:
	proc slindex {string index} { lindex [string2list $string] $index }
	proc slrange {string start end} { lrange [string2list $string] $start $end }
	proc string2list {string} { if {[catch {llength $string}]} { regexp -inline -all -- {\S+} $string } else { return $string } }

	variable script [file tail [info script]]
	putlog "$script v2.2.1 - by FireEgl@EFNet <FireEgl@Tcldrop.US> http://www.Tcldrop.US/safetcl - Loaded."
	setudef flag safetcl
}
