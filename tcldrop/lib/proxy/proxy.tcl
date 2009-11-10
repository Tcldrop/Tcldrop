# proxy.tcl --
#
#	Provides the base proxy support for Tcl.
#
# Copyright (C) 2003-2009 by Philip Moore <FireEgl@Tcldrop.US>
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
#
# The synchronous way is:
# if {[catch { ::proxy::connect https://http.connect.proxy.com:8080/socks5://user:password@socks5.proxy.com:3128/irc.blessed.net:6667 } socket]} {
#	puts "Failed"
# } else {
#	puts "Connected to irc.blessed.net:6667 via the proxies on $socket."
# }
#
#
# https://username:password@http1.connect.proxy.com:8080/socks5://someuser:somepass@socks5.proxy.com/https://http2.connect.proxy.com/irc.blessed.net:6667

# TODO (when Tcl v8.5 becomes more wide-spread):
# Use {*} expands where applicable.
# Use dicts instead of arrays.
# Convert to a Tcl Module (.tm extension).
# Try to simplify the splitchain proc.
# Make it work synchronously.  (right now it requires a -command callback)

namespace eval ::proxy {
	variable version {0.1}
	package provide proxy $version
	variable rcsid {$Id$}
	namespace export proxy connect config
	variable Count
	if {![info exists Count]} { variable Count 0 }
}

proc ::proxy::connect {chain args} {
	array set info [list -command {} -readable {} -writable {} -errors {} socket {} -buffering line -encoding [encoding system] -blocking 0 -myaddr {} -async 1 -ssl 0 -timeout 99999 -socket-command [list socket]]
	array set info $args
	if {$info(-async)} { set async [list {-async}] } else { set async [list] }
	switch -- $info(-myaddr) {
		{0.0.0.0} - {::} - {} { set myaddr [list] }
		{default} { set myaddr [list {-myaddr} $info(-myaddr)] }
	}
	array set info [splitchain $chain]
	array set firstinfo $info(1)
	variable [set info(socket) [eval $info(-socket-command) $async $myaddr [list $firstinfo(address) $firstinfo(port)]]]
	array set $info(socket) [array get info]
	Chain ::proxy::$info(socket) 1 $info(socket)
	return $info(socket)
}

proc ::proxy::Chain {id {count {0}} {socket {}} {pid {}} {status {ok}} {message {}}} {
	upvar #0 $id info
	array set lastinfo $info($count)
	# Call the cleanup proc for the last proxy (their cleanup procs should never raise an error, even if the $pid is invalid):
	if {$pid ne {} && $lastinfo(type) ne {}} { ::proxy::$lastinfo(type)::cleanup $pid }
	# Check for errors on the socket:
	if {[catch { fconfigure $info(socket) -error } error] || [string length $error]} {
		Finish $id {error} $error
	} elseif {[eof $info(socket)]} {
		Finish $id {eof} {EOF}
	} elseif {$status eq {ok}} {
		fileevent $info(socket) writable {}
		fileevent $info(socket) readable {}
		fconfigure $info(socket) -blocking 0 -buffering line
		if {[info exists info([incr count])]} {
			array set nextinfo $info($count)
			package require proxy::$lastinfo(type)
			if {[catch { ::proxy::$lastinfo(type)::init $info(socket) $nextinfo(address) $nextinfo(port) -command [list ::proxy::Chain $id $count $info(socket)] -username $nextinfo(user) -password $nextinfo(pass) } error]} {
				Finish $id {error} $error
			}
		} else {
			# If SSL/TLS was requested, set it up now:
			if {$info(-ssl)} {
				if {[catch {
					::package require tls
					::tls::import $info(socket) -request 0
					fconfigure $info(socket) -buffering none -encoding binary -blocking 1
					::tls::handshake $info(socket)
					#puts "[tls::status $info(socket)]"
				} error]} {
					Finish $id {ssl-error} "TLS/SSL Related Error: $error"
				} else {
					Finish $id $status {Connected with SSL}
				}
			} else {
				Finish $id $status {Connected}
			}
		}
	}
}

proc ::proxy::Finish {id status {reason {}}} {
	if {[info exists $id]} {
		upvar #0 $id info
		switch -- $status {
			{ok} {
				catch { fconfigure $info(socket) -blocking $info(-blocking) -buffering $info(-buffering) -encoding $info(-encoding) }
				catch { fileevent $info(socket) writable $info(-writable) }
				catch { fileevent $info(socket) readable $info(-readable) }
				after 0 [list eval $info(-command) $info(socket) $status $id]
			}
			{error} - {eof} - {default} {
				catch { close $info(socket) }
				if {[info exists info(-errors)] && $info(-errors) != {}} {
					after idle [list eval $info(-errors) $status $id [list $reason]]
				} else {
					after idle [list eval $info(-command) $info(socket) $status $id]
				}
			}
		}
		after 0 [list unset -nocomplain $id]
	}
}

proc ::proxy::splitchain {chain} {
	# Forgive me, I suck at regexp. =(
	set count 0
	set proxychain $chain
	while {[regexp -nocase {^(([^:]*)://)?([^@]+@)?([^/:]+):(\+[0-9]+|[0-9]+)?(/.*)?$} [string trim $proxychain /] {} prefix type user address port proxychain]} {
		if {[string match {+*} $port]} { set ssl 1 } else { set ssl 0 }
		set info([incr count]) [list type $type pass [join [lrange [split [string range $user 0 end-1] :] 1 end]] user [lindex [split $user :] 0] address $address port [string trimleft $port +] ssl $ssl]
	}
	# This processes the leftovers that might be in $proxychain:
	if {[regexp -nocase {^(.*):(.*):(.*)$} [string trim $proxychain /] {} address port extra]} {
		if {[string match {+*} $port]} { set ssl 1 } else { set ssl 0 }
		set info([incr count]) [list type {} pass {} user {} address $address port [string trimleft $port +] extra $extra ssl $ssl]
	} else {
		if {![info exists extra]} { set extra {} }
		if {![info exists address]} { set address {} }
		if {![info exists port]} { set port {} }
		if {![info exists ssl]} { set ssl 0 }
	}
	# The last one in the chain is the final destination, so we set the info about it:
	array set info [list count $count address $address port $port ssl $ssl extra $extra chain $chain]
	array get info
}
