# rchan 1.0
#
# It's basically a FIFO channel. Whatever you put into it is what you get out.
#
# I'm using it to experiment with refchan's right now..

# But this, or something like it, will be used in Tcldrop as a way 
# for the bot interps to communicate with the master interp.
#
# And I think similar code can be used for inter-bot communication.. 
# (basically fake sockets between multiple bots running in our process)
#
# Note, this is based on the incomplete and buggy code at http://wiki.tcl.tk/17594
#
#
# http://tcl.tk/man/tcl8.5/TclCmd/chan.htm
# http://tcl.tk/man/tcl8.5/TclCmd/refchan.htm
#

package provide rchan 1
namespace eval rchan {
	variable chan
	array set chan {}

	proc create {{modes {read write}}} { chan create $modes rchan }

	proc initialize {chanid modes} {
		puts [info level 0]
		variable chan
		set chan($chanid) [dict create write 0 read 0 buffer {} blocking 0 max 1048576]
		#set map [dict create]
		#dict set map finalize [list ::rchan::finalize $chanid]
		#dict set map watch [list ::rchan::watch $chanid]
		#dict set map seek [list ::rchan::seek $chanid]
		#dict set map write [list ::rchan::write $chanid]
		#dict set map read [list ::rchan::read $chanid]
		#dict set map cget [list ::rchan::cget $chanid]
		#dict set map cgetall [list ::rchan::cgetall $chanid]
		#dict set map configure [list ::rchan::configure $chanid]
		#dict set map blocking [list ::rchan::blocking $chanid]
		#namespace ensemble create -map $map -command ::$chanid
		list initialize finalize watch read write configure cget cgetall blocking
	}

	proc finalize {chanid} {
		puts [info level 0]
		variable chan
		unset chan($chanid)
	}

	proc watch {chanid events} {
		puts [info level 0]
		variable chan
		foreach e {write read} { if {$e in $events} { dict set chan($chanid) $e 1 } else { dict set chan($chanid) $e 0 } }
		# Tell them we're writable now:
		if {[dict get $chan($chanid) write] && [string length [dict get $chan($chanid) buffer]] <= [dict get $chan($chanid) max]} { 
			chan postevent $chanid write
		}
		# inform the app that there's something to read
		if {[dict get $chan($chanid) read] && [dict get $chan($chanid) buffer] ne {}} {
			#puts "post event read"
			chan postevent $chanid read
		}
	}

	# Note:
	# $count is NOT the requested number of bytes like the manual says it is.
	# $count is actually the -buffersize
	proc read {chanid count} {
		puts [info level 0]
		variable chan
		if {[string length [dict get $chan($chanid) buffer]] < $count} {
			set result [dict get $chan($chanid) buffer]
			dict set chan($chanid) buffer {}
		} else {
			set result [string range [dict get $chan($chanid) buffer] 0 ${count}-1]
			dict set chan($chanid) buffer [string range [dict get $chan($chanid) buffer] $count end]
		}
		# implement max buffering ($count is whatever fconfigure set the -buffersize to)
		dict set chan($chanid) max $count
		if {[dict get $chan($chanid) write] && [string length [dict get $chan($chanid) buffer]] <= $count} {
			chan postevent $chanid write
		}
		if {[string length $result]} {
			return $result
		} else {
			return -code error {EAGAIN}
		}
	}

	proc write {chanid data} {
		puts [info level 0]
		variable chan
		set left [expr {[dict get $chan($chanid) max] - [string length [dict get $chan($chanid) buffer]]}] ;# bytes left in buffer
		set dsize [string length $data]
		if {$left >= $dsize} {
			dict append chan($chanid) buffer $data
			if {[dict get $chan($chanid) write] && ([string length [dict get $chan($chanid) buffer]] < [dict get $chan($chanid) max])} {
				# inform the app that it may still write
				chan postevent $chanid write
			}
		} else {
			set dsize $left
			dict append chan($chanid) buffer [string range $data $left]
		}
		# inform the app that there's something to read
		if {[dict get $chan($chanid) read] && [dict get $chan($chanid) buffer] ne {}} {
			#puts "post event read"
			chan postevent $chanid read
		}
		return $dsize ;# number of bytes actually written
	}

	proc blocking {chanid bool} {
		puts [info level 0]
		variable chan
		dict set chan($chanid) blocking $bool
	}

	# Note: Tcl doesn't trigger this proc, ever..I don't think:
	proc cget {chanid option} {
		puts [info level 0]
		variable chan
		dict get $chan($chanid) $option
	}
	# Note: Tcl doesn't trigger this proc, ever..I don't think:
	proc cgetall {chanid} {
		puts [info level 0]
		variable chan
		return $chan($chanid)
	}
	# Note: Tcl doesn't trigger this proc, ever:
	proc configure {chanid option value} {
		puts [info level 0]
		variable chan
		dict set chan($chanid) $option $value
	}

	namespace export -clear *
	namespace ensemble create -subcommands {}
}


# Test code:
if {1} {
	set fd [rchan create]
	chan configure $fd -buffering line -blocking 1 -buffersize 9999 -encoding binary -translation {auto crlf}
	proc PutData {fd} {
		puts [info level 0]
		fileevent $fd writable {}
		puts $fd "Hello Moon! [expr {rand()}]"
	}
	proc GetData {fd} { puts "[info level 0]: [read $fd 10]" }
	fileevent $fd readable [list GetData $fd]
	# Note: Tcl v8.5 segfaults unless you do this AFTER setting the readable fileevent:
	if {[info tclversion] >= 8.6} { fileevent $fd writable [list PutData $fd] } else { PutData $fd }
	after 40 [list set ::forever 1]
	vwait ::forever
	close $fd
}
