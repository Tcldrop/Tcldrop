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
	# 1048576 is the most Tcl allows a -buffersize to be:
	variable buffersize {1048576}
	proc create {{modes {read write}}} {
		puts [info level 0]
		set chanid [chan create $modes rchan]
		variable buffersize
		# Set the default buffersize to the maximum allowed by Tcl:
		chan configure $chanid -buffersize $buffersize -blocking 0
		return $chanid
	}
	# Like create, but it creates 2 channels,
	# whatever you puts into one channel comes out on the other and vice versa.
	proc create2 {{modes {read write}}} {
		puts [info level 0]
		set id1 [create $modes]
		set id2 [create $modes]
		# Set the rchanid channels to each others channel:
		chan configure $id1 rchanid $id2
		chan configure $id2 rchanid $id1
		list $id1 $id2
	}
	# Sets a error on a chanid:
	proc error {chanid {message {Unknown Error}}} {
		puts [info level 0]
		variable chan
		if {[info exists chan($chanid)]} {
			# Prevent us from triggering any more writable fileevents:
			dict set chan($chanid) write 0
			# Set the error message:
			dict set chan($chanid) -error $message
			# Do a readable fileevent, so they can see the error/EOF:
			readable $chanid
		}
	}
	# Tells chanid's writable fileevent it can write as long as rchanid's buffer isn't full (and both chanid's are still valid):
	proc writable {chanid} {
		variable chan
		if {[info exists chan($chanid)]} {
			if {![info exists chan([set rchanid [dict get $chan($chanid) rchanid]])]} {
				# Even though it's unlikely to find the other end missing here, we set the error:
				error $chanid "error: watch ${chanid}: missing rchanid: $rchanid"
			} elseif {[dict get $chan($chanid) write] && [string length [dict get $chan($rchanid) buffer]] < [chan configure $rchanid -buffersize]} { 
				chan postevent $chanid write
			}
		}
	}
	# Tells chanid's readable fileevent it can read if there's something in the buffer:
	proc readable {chanid} {
		variable chan
		if {[info exists chan($chanid)] && [dict get $chan($chanid) read] && ([dict get $chan($chanid) buffer] ne {} || [dict get $chan($chanid) -error] ne {})} {
			chan postevent $chanid read
		}
	}
	proc initialize {chanid modes} {
		puts [info level 0]
		variable chan
		set chan($chanid) [dict create rchanid $chanid write 0 read 0 buffer {} -error {}]
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
		# Tell rchanid about the error too (if it exists), 
		# so it can be closed now rather than later:
		error [dict get $chan($chanid) rchanid] "error: finalize (close) on chanid: $chanid"
		unset chan($chanid)
	}
	proc watch {chanid events} {
		puts [info level 0]
		variable chan
		if {![info exists chan([set rchanid [dict get $chan($chanid) rchanid]])]} {
			# Even though it's unlikely to find the other end missing here, we set the error:
			error $chanid "error: watch ${chanid}: missing rchanid: $rchanid"
			return {}
		} else {
			# Keep track of the events we're supposed to do "chan postevent" for:
			foreach e {write read} { if {$e in $events} { dict set chan($chanid) $e 1 } else { dict set chan($chanid) $e 0 } }
			# Start doing the writable/readable fileevents right away (otherwise we may never start doing them):
			# If the rchanid's buffer isn't full, tell chanid we're writable:
			writable $chanid
			# Tell the chanid's readable there's something to read:
			readable $chanid
		}
	}
	# Note:
	# $count is NOT the requested number of bytes like the manual says it is.
	# $count is actually the -buffersize
	proc read {chanid count} {
		puts [info level 0]
		variable chan
		if {[string length [dict get $chan($chanid) buffer]] < $count} {
			# We have less in the buffer than what was requested by $count, give them all of it:
			set result [dict get $chan($chanid) buffer]
			dict set chan($chanid) buffer {}
		} else {
			# Set the result to however much was requested by $count:
			set result [string range [dict get $chan($chanid) buffer] 0 ${count}-1]
			dict set chan($chanid) buffer [string range [dict get $chan($chanid) buffer] $count end]
			# Tell the readable fileevent there's still more to read:
			readable $chanid
		}
		if {[string length $result]} {
			# Tell rchanid it can write some more:
			writable [dict get $chan($chanid) rchanid]
			# Return whatever's in the buffer:
			return $result
			# Note: We don't check for errors (below) until after we've emptied (read) our buffer.
		} elseif {[chan configure $chanid -error] ne {}} {
			# We set -error from somewhere else, so return "" (Note: "" means EOF):
			return {}
		} elseif {![info exists chan([dict get $chan($chanid) rchanid])]} {
			# rchanid is gone.. Set an error:
			error $chanid "error: read ${chanid}: rchanid ([dict get $chan($chanid) rchanid]) is gone."
			# And return "" (Note: "" means EOF):
			return {}
		} else {
			# If the chanid's buffer isn't full, tell rchanid we're writable:
			writable [dict get $chan($chanid) rchanid]
			# return -code error "EAGAIN" means the buffer is empty (nothing more to read at this time):
			return -code error {EAGAIN}
		}
	}
	proc write {chanid data} {
		puts [info level 0]
		variable chan
		if {![info exists chan([set rchanid [dict get $chan($chanid) rchanid]])]} {
			# The other end is gone, set an error:
			error $chanid "error: write ${chanid}: rchanid (${rchanid}) is missing."
			# Return -1 to signal that the write failed:
			return -1
		} else {
			# Fill the rchanid's buffer with $data:
			if {[set left [expr {[chan configure $rchanid -buffersize] - [string length [dict get $chan($rchanid) buffer]]}]] >= [set dsize [string length $data]]} {
				dict append chan($rchanid) buffer $data
				# If rchanid's buffer isn't full tell chanid's writable fileevent it can keep writing:
				writable $chanid
			} else {
				set dsize $left
				dict append chan($rchanid) buffer [string range $data $left]
			}
			# Tell the rchanid's readable there's something to read:
			readable $rchanid
			# Return how many bytes were written:
			return $dsize
		}
	}
	# Enable blocking:
	proc blocking {chanid bool} {
		puts [info level 0]
		variable chan
		# Note/FixMe: We don't make use of this.
		dict set chan($chanid) blocking $bool
	}
	# Get a single configure setting:
	# Example: [chan configure $chanid rchanid] returns the rchanid.
	proc cget {chanid option} {
		puts [info level 0]
		variable chan
		dict get $chan($chanid) $option
	}
	# Get all the configure settings:
	# Example: [chan configure $chanid]
	proc cgetall {chanid} {
		puts [info level 0]
		variable chan
		return $chan($chanid)
	}
	# Set a configure setting:
	# Example: [chan configure $chanid rchanid $rchanid]
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
	lassign [rchan create2] fd1 fd2
	puts "fd1 $fd1 configure: [chan configure $fd1]"
	puts "fd1 $fd1 rchanid: [chan configure $fd2 rchanid]"
	chan configure $fd2 -buffering line -blocking 1 -buffersize 9999 -encoding binary -translation {auto crlf}
	puts "fd2 $fd2 configure: [chan configure $fd2]"
	puts "fd2 $fd2 rchanid: [chan configure $fd2 rchanid]"
	proc PutData {fd} {
		puts [info level 0]
		fileevent $fd writable {}
		puts $fd "Hello from $fd [expr {rand()}]"
	}
	proc GetData {fd} {
		puts "[info level 0]: [gets $fd line]: $line" 
		if {[eof $fd]} {
			puts "Got eof on $fd - closing.. Error was: [chan configure $fd -error]"
			close $fd
		}
	}
	fileevent $fd1 readable [list GetData $fd1]
	fileevent $fd2 readable [list GetData $fd2]
	# Note: Tcl v8.5 segfaults unless you do this AFTER setting the readable fileevent:
	if {[info tclversion] >= 8.6} { fileevent $fd1 writable [list PutData $fd1] } else { PutData $fd1 }
	if {[info tclversion] >= 8.6} { fileevent $fd2 writable [list PutData $fd2] } else { PutData $fd2 }
	puts A
	after 40 [list set ::forever 1]
	vwait ::forever
	puts B
	puts $fd2 "Test0"
	puts "close: $fd1"
	catch { close $fd1 }
	puts C
	after 40 [list set ::forever 1]
	vwait ::forever
	puts Exit...
}
