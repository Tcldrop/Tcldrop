# proxy.tcl --
#
#	Provides the base proxy support for Tcl.
#
# Copyright (C) 2003 by Philip Moore <FireEgl@Tcl.no>
# This code may be distributed under the same terms as Tcl.
#
# RCS: @(#) $Id$
#
# This package provides the base proxy support. It talks to the other proxy packages such as https.
#

# Example Usage:
#
# Basically, the ::proxy::connect command takes the place of the "socket" command in your script.
# You're then able to specify the proxy server(s) (and their options) to use to get to $address:$port.
#
# ::proxy::connect address port ?option value? ?option value ...?
# Returns the socket used for the connection, or an error if the "socket" command returns ones.
#
# Allowed options to ::proxy::connect:
# -command <callback>
#	Specifies the command to handle the connection once it's connected to $address:$port.
#	It will be called with the socket used for the connection, plus any other arguments you supplied.
#
# -async, -myaddr, and -myport do the same thing as in the socket command.
#
#
# The ::proxy::proxy command is generally only used internally..
#
# ::proxy::proxy socket ?option value? ?option value ...?
#

# Example Usage:
# The synchronous way is:
# if {[catch { ::proxy::connect https://http.connect.proxy.com:8080/socks5://user:password@socks5.proxy.com:3128/irc.blessed.net:6667 } id]} {
#	puts "Failed"
# } else {
#	puts "Connected to address:port via the proxies."
#	set socket [::proxy::socket $id]
# }
#
# set url https://username:password@http1.connect.proxy.com:8080/socks5://someuser:somepass@socks5.proxy.com/https://http2.connect.proxy.com/irc.blessed.net:6667

namespace eval ::proxy {
	variable version {0.1}
	package provide proxy $version
	variable rcsid {$Id$}
	namespace export proxy connect config
	variable Count
	if {![info exists Count]} { set Count 0 }
}

proc ::proxy::connect {chain args} {
	array set info [list -command {} -readable {} -writable {} -errors {} -user {} -pass {} socket {} -buffering line -blocking 0  -myaddr {} -async 1 -socket-command [list socket]]
	if {$info(-async)} { set async [list {-async}] } else { set async [list] }
	if {$info(-myaddr) != {} && $info(-myaddr) != {0.0.0.0}} { set myaddr [list {-myaddr} $info(-myaddr)] } else { set myaddr [list] }
	array set info [splitchain $chain]
	array set info $args
	array set firstinfo $info(1)
	variable [set info(socket) [eval $info(-socket-command) $async $myaddr {$firstinfo(address)} {$firstinfo(port)}]]
	array set $info(socket) [array get info]
	Chain ::proxy::$info(socket) 1 $info(socket)
	return $info(socket)
}

proc ::proxy::Chain {id {count {0}} {socket {}} {pid {}} {status {ok}} {message {}}} {
	upvar #0 $id info
	# Check for errors on the socket:
	if {[catch { fconfigure $info(socket) -error } error] || [string length $error]} {
		Finish $id {error} $error
	} elseif {[eof $info(socket)]} {
		Finish $id {eof} {EOF}
	} else {
		fileevent $info(socket) writable {}
		fileevent $info(socket) readable {}
		fconfigure $info(socket) -blocking 0 -buffering line
		array set lastinfo $info($count)
		if {[info exists info([incr count])]} {
			array set nextinfo $info($count)
			package require proxy::$lastinfo(type)
			if {[catch { ::proxy::$lastinfo(type)::init $info(socket) $nextinfo(address) $nextinfo(port) -command [list ::proxy::Chain $id $count $info(socket)] -user $lastinfo(user) -pass $lastinfo(pass) } error]} {
				Finish $id {error} $error
			}
		} else {
			Finish $id $status {Connected}
		}
	}
}

proc ::proxy::Finish {id status {reason {}}} {
	upvar #0 $id info
	catch { fileevent $info(socket) writable $info(-writable) }
	catch { fileevent $info(socket) readable $info(-readable) }
	catch { fconfigure $info(socket) -blocking $info(-blocking) -buffering $info(-buffering) }
	switch -- $status {
		{ok} {
			after 0 [list eval $info(-command) $info(socket) $status $id]
		}
		{error} - {eof} - {default} {
			catch { close $info(socket) }
			if {[info exists info(-errors)] && $info(-errors) != {}} {
				after idle [list eval $info(-errors) $status $id $reason]
			} else {
				after idle [list eval $info(-command) $info(socket) $status $id]
			}
		}
	}
	after idle [list after 0 [list unset -nocomplain $id]]
}

proc ::proxy::splitchain {chain} {
	# Forgive me, I suck at regexp. =(
	set count 0
	set proxychain $chain
	while {[regexp -nocase {^(([^:]*)://)?([^@]+@)?([^/:]+)(:([0-9]+))?(/.*)?$} [string trim $proxychain /] {} prefix type user address {} port proxychain]} {
		set info([incr count]) [list type $type pass [join [lrange [split [string range $user 0 end-1] :] 1 end]] user [lindex [split $user :] 0] address $address port [string trimleft $port +]]
	}
	# This processes the leftovers that might be in $proxychain:
	if {[regexp -nocase {^(.*):(.*):(.*)$} [string trim $proxychain /] {} address port extra]} {
		set info([incr count]) [list type {} pass {} user {} address $address port $port extra $extra]
	} else {
		if {![info exists extra]} { set extra {} }
		if {![info exists address]} { set address {} }
		if {![info exists port]} { set port {} }
	}
	array set info [list count $count address $address port $port extra $extra chain $chain]
	array get info
}
