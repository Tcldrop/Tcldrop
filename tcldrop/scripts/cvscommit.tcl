#! /usr/bin/tclsh

# This was written so that cvs commit logs will get written to the Tcldrop support/development channel on irc.

if {[info exists numversion]} {
	# Running from Eggdrop.
	proc cvscheck {args} {
		set subject {}
		set onlogs 0
		set onfiles 0
		set by {Unknown}
		set log {}
		set files {}
		set dir {}
		set module {unknown}
		foreach l ${::commits} {
			switch -glob -- [string trim $l] {
				{} - { } - {  } { }
				{LOGNAME: *} { switch -- [set by [lindex [split $l] 1]] { {fireegl} { set by {FireEgl} } } }
				{SSH_CLIENT: *} { set by "$by@[lindex [split $l] 1]" }
				{From: *} { switch -- [set by [lindex [split $l] 1]] { {fireegl@users.sourceforge.net} { set by {FireEgl@Users.SF.Net} } } }
				{Changes*by:*} { set by [lrange [split $l] 2 end] }
				{Subject: *} {
					set dir "[lindex [split $l] 1]/"
					set module "[lindex [split [lindex [split $l] 1] /] 0]"
				}
				{Modified Files:*} - {Removed Files:*} - {Added Files:*} {
					append files " $l"
					set onfiles 1
				}
				{Update of /cvsroot/tcldrop/*} { set module [lindex [split $l /] 3] }
				{Log Message} - {Log Message:*} { set onlogs 1 }
				{Index:*} - {--- * DELETED ---} - {--- NEW FILE: * ---} - {CVSWeb URLs:*} - {-- EOF --} {
					# End of one email.
					if {[string length $dir] >= 1 && [string length [string trim $files]] >= 1} { lappend dirfiles [string trim "$dir$files"] }
					set onlogs 0
					set onfiles 0
					set files {}
					set dir {}
				}
				{default} {
					if {$onlogs && [lsearch -exact $log $l] == -1} {
						lappend log $l
					} elseif {$onfiles} {
						if {[string length [string trim $files]] >= 1} { append files " [string trim [join [split $l] {, }] {, }]." }
						set onfiles 0
					}
				}
			}
		}
		puthelp "PRIVMSG #Tcldrop :CVS Commit by $by - [join $dirfiles {; }] - Log Message: [join $log {; }]"
		if {[set err [makesnapshot $module]] != {}} { puthelp "PRIVMSG FireEgl :Snapshot NOT created: $err" }
		set ::commits {}
	}
	proc connect:cvscommit {sock address port} {
		if {[string equal {66.35.250.90} $address]} {
			fconfigure $sock -buffering line -blocking 0
			fileevent $sock readable [list read:cvscommit $sock]
			fileevent $sock writable [list write:cvscommit $sock]
			putlog "Connect: $sock ${address}:$port"
		} else {
			close $sock
			putlog "WARNING: Unauthorized port access: ${address}:$port"
		}
	}
	proc write:cvscommit {sock} {
		fileevent $sock writable {}
		fconfigure $sock -buffering line -blocking 0
		catch { killtimer $::cvstimer }
		set ::cvstimer [timer 1 [list cvscheck]]
	}
	proc read:cvscommit {sock} {
		if {[eof $sock]} {
			close $sock
		} else {
			while {[gets $sock line] >= 0} {
				putlog "CVS LOG: $line"
				lappend ::commits $line
			}
		}
	}
	catch { socket -server connect:cvscommit 8181 }

	proc makesnapshot {module} {
		variable LastSnapshot
		if {![info exists LastSnapshot]} { set LastSnapshot 9 }
		if {[clock seconds] - $LastSnapshot > 3000} {
			set LastSnapshot [clock seconds]
			if {![catch { exec ssh fireegl@shell1.sf.net bin/cvscommit $module } err] || ![catch { exec ssh fireegl@shell1.sf.net bin/cvscommit $module & } err]} {
				return {}
			} else {
				return $err
			}
		} else {
			timer 61 [list makesnapshot $module]
			return {}
		}
	}
	bind pub n !makesnap pub:makesnap
	bind pub n !makesnapshot pub:makesnap
	bind pub n !snapshot pub:makesnap
	bind pub n !snap pub:makesnap
	proc pub:makesnap {nick host hand chan text} {
		if {![string equal -nocase $chan {#tcldrop}]} { return }
		if {[set err [makesnapshot tcldrop0.6]] != {} && [string match {*export aborted*} $err]} {
			puthelp "PRIVMSG $chan :Snapshot NOT created: $err"
		} else {
			puthelp "PRIVMSG $chan :Created CVS Snapshot: http://www.Tcldrop.US/snapshots/tcldrop-latest-snapshot.tar.gz"
		}
	}
	putlog "cvscommit.tcl - Loaded."
} elseif {$argv != {}} {
	# Running on the cvs server.
	puts -nonewline {Sending Log to Atlantica...}
	flush stdout
	set sock [socket -async Atlantica.HomeIP.Net 8181]
	proc done {{reason {}}} {
		if {$reason != {}} { puts $reason }
		set ::forever 1
	}
	after 9999 [list done {Timeout while sending log to Atlantica.}]
	fileevent $sock writable [list write:cvscommit $sock]
	fileevent $sock readable [list read:cvscommit $sock]
	proc read:cvscommit {sock} { if {[eof $sock]} { done {EOF.} } }
	fconfigure stdin -buffering line -blocking 0
	fconfigure stdout -buffering line -blocking 0
	proc write:cvscommit {sock} {
		if {[eof stdin]} { done {EOF from stdin.} } else {
			puts $sock "LOGNAME: $::env(LOGNAME)"
			puts $sock "SSH_CLIENT: [lindex [split $::env(SSH_CLIENT)] 0]"
			puts $sock "Subject: [join $::argv]"
			while {[gets stdin line] >= 0} { if {[catch { puts $sock $line } err]} { done "Error: $err" } }
			puts $sock {-- EOF --}
			done {Done.}
		}
	}
	# We wait until we've sent everything on stdin, or the 10 second time limit is up (whichever comes first).
	vwait forever
}

catch { putlog "cvscommit.tcl - Loaded" }
