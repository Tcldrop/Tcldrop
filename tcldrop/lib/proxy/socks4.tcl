#  socks4.tcl ---
#
#      Package for using the SOCKS4a method for connecting TCP sockets.
#      Only client side.
#
#  (c) 2007  Mats Bengtsson
#
#  This source file is distributed under the BSD license.
#
# $Id$

package provide proxy::socks4 0.2

namespace eval ::proxy::socks4 {

    variable const
    array set const {
	ver                 \x04
	cmd_connect         \x01
	cmd_bind            \x02
	rsp_granted         \x5a
	rsp_failure         \x5b
	rsp_errconnect      \x5c
	rsp_erruserid       \x5d
    }

    # Practical when mapping errors to error codes.
    variable iconst
    array set iconst {
	\x04    ver
	\x01    cmd_connect
	\x02    cmd_bind
	\x5a    rsp_granted
	\x5b    rsp_failure
	\x5c    rsp_errconnect
	\x5c    rsp_erruserid
    }
}

# socks4::init --
#
#       Negotiates with a SOCKS server.
#
# Arguments:
#       sock:       an open socket token to the SOCKS server
#       addr:       the peer address, not SOCKS server
#       port:       the peer's port number
#       args:
#               -command    tclProc {token status}
#               -username   userid
#               -timeout    millisecs
#
# Results:
#       token if -command, else ?.

proc ::proxy::socks4::init {sock addr port args} {
    variable const

    set token [namespace current]::$sock
    variable $token
    upvar 0 $token state

    array set state {
	-command          ""
	-timeout          60000
	-username         ""
	async             0
	bnd_addr          ""
	bnd_port          ""
	trigger           0
    }
    array set state [list     \
      addr          $addr     \
      port          $port     \
      sock          $sock]
    array set state $args

    if {[string length $state(-command)]} {
	set state(async) 1
    }

    # Network byte-ordered port (2 binary-bytes, short)
    set bport [binary format S $port]

    # This corresponds to IP address 0.0.0.x, with x nonzero.
    set bip \x00\x00\x00\x01

    set bdata "$const(ver)$const(cmd_connect)$bport$bip"
    append bdata "$state(-username)\x00$addr\x00"
    fconfigure $sock -translation {binary binary} -blocking 0
    fileevent $sock writable {}
    if {[catch {
	puts -nonewline $sock $bdata
	flush $sock
    } err]} {
	return -code error network-failure
    }

    # Setup timeout timer. !async remains!
    set state(timeoutid)  \
      [after $state(-timeout) [namespace current]::timeout $token]

    if {$state(async)} {
	fileevent $sock readable  \
	  [list [namespace current]::response $token]
	return $token
    } else {

	# We should not return from this proc until finished!
	fileevent $sock readable  \
	  [list [namespace current]::readable $token]
	vwait $token\(trigger)
	return [response $token]
    }
}

proc ::proxy::socks4::response {token} {
    variable $token
    upvar 0 $token state
    variable const
    variable iconst

    puts "socks4::response"

    set sock $state(sock)
    fileevent $sock readable {}

    # Read and parse status.
    if {[catch {read $sock 2} data] || [eof $sock]} {
	finish $token network-failure
	return
    }
    binary scan $data cc null status
    if {![string equal $null \x00]} {
	finish $token err_version
	return
    }
    if {![string equal $status $const(rsp_granted)]} {
	if {[info exists iconst($status)]} {
	    finish $token $iconst($status)
	} else {
	    finish $token error
	}
	return
    }

    # Read and parse port (2 bytes) and ip (4 bytes).
    if {[catch {read $sock 6} data] || [eof $sock]} {
	finish $token network-failure
	return
    }
    binary scan $data ccccS i0 i1 i2 i3 port
    set addr ""
    foreach n [list $i0 $i1 $i2 $i3] {
	# Translate to unsigned!
	append addr [expr ( $n + 0x100 ) % 0x100]
	if {$n <= 2} {
	    append addr .
	}
    }
    # Translate to unsigned!
    set port [expr ( $port + 0x10000 ) % 0x10000]
    set state(bnd_port) $port
    set state(bnd_addr) $addr

    return [finish $token]
}

proc ::proxy::socks4::readable {token} {
    variable $token
    upvar 0 $token state
    puts "socks4::readable"
    incr state(trigger)
}

proc ::proxy::socks4::timeout {token} {
    finish $token timeout
}

proc ::proxy::socks4::getipandport {token} {
    variable $token
    upvar 0 $token state
    return [list $state(bnd_addr) $state(bnd_port)]
}

proc ::proxy::socks4::free {token} {
    variable $token
    upvar 0 $token state
    catch {after cancel $state(timeoutid)}
    unset -nocomplain state
}

# "cleanup" is my standard command name:
interp alias {} ::proxy::socks4::cleanup {} ::proxy::socks4::free
#proc ::proxy::socks4::cleanup {token} { free $token }

proc ::proxy::socks4::finish {token {errormsg ""}} {
    global errorInfo errorCode
    variable $token
    upvar 0 $token state

    puts "socks4::finish token=$token, errormsg=$errormsg"
    parray state

    catch {after cancel $state(timeoutid)}

    # In case of error we do the cleanup.
    if {$state(async)} {
	if {[string length $errormsg]} {
	    catch {close $state(sock)}
	    uplevel #0 $state(-command) [list $token $errormsg]
	    free $token
	} else {
	    uplevel #0 $state(-command) [list $token ok]
	}
    } else {
	if {[string length $errormsg]} {
	    catch {close $state(sock)}
	    free $token
	    return -code error $errormsg
	} else {
	    return
	}
    }
}

# Test
if {0} {
    set s [socket 127.0.0.1 3000]
    set t [socks4::init $s google.com 80 -username mats]
    socks4::free $t
}

