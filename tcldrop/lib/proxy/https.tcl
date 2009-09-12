# https.tcl --
#
#	Provides:
#		* https (HTTP CONNECT) proxy support for Tcl.
#
# Copyright (C) 2003,2004,2005,2009 by Philip Moore <FireEgl@Tcldrop.US>
# This code may be distributed under the same terms as Tcl.
#
# RCS: @(#) $Id$

namespace eval ::proxy::https {
	package require Tcl 8.3
	variable version {0.2}
	package provide proxy::https $version
	variable rcsid {$Id$}
	variable Count
	if {![info exists Count]} { set Count 0 }
}

# FixMe: Prolly a good idea to provide a connect proc, even though it's not necessary to use this package..
proc ::proxy::https::connect {} {
}

# Uses $socket (which should already be connected to the https proxy)
proc ::proxy::https::init {socket address port args} {
	if {[lsearch -exact [file channels] $socket] == -1} {
		return -code error "No such socket: $socket"
	} elseif {[eof $socket]} {
		return -code error "EOF on $socket"
	} elseif {[string length [set error [fconfigure $socket -error]]]} {
		return -code error "Socket error on $socket: $error"
	} else {
		variable Count
		# Create a new token for this connection:
		upvar #0 [set id "::proxy::https::[incr Count]"] info
		set info(sock) $socket
		# Set defaults, and store the current writable, readable, and fconfigure for the socket.
		array set info [list -username {} -password {} -useragent {Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)} -headers {} -timeout {594321} -command {} -fconfigure [fconfigure $info(sock)] -writable [fileevent $info(sock) writable] -readable [fileevent $info(sock) readable] headers {}]
		# Allow $args to override anything set above.
		array set info $args
		# If -username or -password was specified, we'll need to load the base64 package:
		if {[string length "$info(-username)$info(-password)"] && [catch { package require base64 } error]} { return -code error "Can't Load Package base64: $error" }
		fconfigure $info(sock) -translation crlf -buffering line -blocking 0
		fileevent $info(sock) writable [list ::proxy::https::Write $id $address $port]
		fileevent $info(sock) readable [list ::proxy::https::Read $id]
		set info(status) {connecting-proxy}
		set info(timeoutid) [after $info(-timeout) [list ::proxy::https::Finish $id timeout "Timeout after $info(-timeout)ms"]]
		# If -command wasn't specified we have to wait for the connection to $address:$port before returning.
		if {![string length $info(-command)]} { vwait "${id}(results)" }
		return $id
	}
}

proc ::proxy::https::Write {id address port} {
	upvar #0 $id info
	if {[eof $info(sock)] || [string length [set error [fconfigure $info(sock) -error]]]} {
		set info(status) {connect-failed-proxy}
		Finish $id {error} "Socket Error: $error"
		return
	} else {
		set info(status) {connected-proxy}
	}
	fileevent $info(sock) writable {}
	set request "CONNECT ${address}:$port HTTP/1.0\n"
	if {[string length "$info(-username)$info(-password)"]} { append request "Proxy-Authorization: Basic [set userpass [::base64::encode $info(-username):$info(-password)]]\nAuthorization: Basic $userpass\n" }
	if {[llength $info(-headers)]} { foreach {h v} $info(-headers) { append request "${h}: $v\n" } }
	if {![catch { puts $info(sock) "${request}User-Agent: $info(-useragent)\nHost: $address:$port\nContent-Length: 0\nProxy-Connection: Keep-Alive\nConnection: Keep-Alive\nPragma: no-cache\n" } error]} {
		set info(status) {request-sent}
	} else {
		set info(status) {request-failed}
		Finish $id error "Socket Error: $err"
	}
}

proc ::proxy::https::Read {id} {
	upvar #0 $id info
	 if {[string length [set error [fconfigure $info(sock) -error]]]} {
		set info(status) {socket-error-read}
		Finish $id error "Socket Error: $error"
	} else {
		while {[gets $info(sock) l] != -1} {
			switch -- $info(status) {
				{request-sent} {
					if {[regexp {^HTTP\/1\.[01] ([0-9]+)} $l s http_status]} {
						set info(status) {got-http-code}
						if {![string match {2??} $http_status]} {
							# Got an http error code from the proxy server:
							# (Anything not in the 200-299 range is an error)
							Finish $id error "Proxy returned http code $http_status."
							return
						}
					} else {
						# The first line they send back should be the http return code, and since it wasn't, we abort.
						set info(status) {unknown-response}
						Finish $id error "Unknown response from proxy server: $l"
						return
					}
				}
				{got-http-code} {
					if {[string length [string trim $l]] == 0} {
						# This is the end of the http headers.
						set info(status) {connected}
						Finish $id ok {Connected to remote.}
						return
					} else {
						# Store the headers, they might be useful..
						lappend info(headers) [string trim [lindex [set l [split $l {:}]] 0]] [string trim [lindex $l 1]]
					}
				}
			}
		}
		if {[eof $info(sock)]} {
			set info(status) {eof-read}
			Finish $id eof {Got EOF}
		}
	}
}

# This should:
# 1. If there was an error, close the socket.
# 2. Restore the fileevents and fconfigure's that were active before we took over the connection.
# 3. Set info(result) to something (so that the vwait, if there is one, can return).
# 4. Call the callback command.
# 5. Clean up the variables used by $id (in an [after idle]).
proc ::proxy::https::Finish {id status {reason {}}} {
	upvar #0 $id info
	after cancel $info(timeoutid)
	unset info(timeoutid)
	if {$status != {ok}} {
		catch {
			fconfigure $info(sock) -blocking 0
			close $info(sock)
			set info(sock) {}
		}
	} else {
		# Restore the previous fconfigure for the socket:
		foreach {o v} $info(-fconfigure) {
			switch -- $o {
				{-peername} - {-sockname} { }
				{default} { catch { fconfigure $info(sock) $o $v } }
			}
		}
		# Restore the previous writable and readable fileevents for the socket:
		catch {
			fileevent $info(sock) writable $info(-writable)
			fileevent $info(sock) readable $info(-readable)
		}
	}
	# Set info(results), so that the vwait (in ::proxy::https::proxy) can return:
	set info(results) $status
	if {[info exists info(-command)] && $info(-command) != {}} { eval $info(-command) {$id} {$status} {$reason} }
	# FixMe: Should we do the cleanups as an idle event, or make the users call ::proxy::https::cleanup themselves?
	after idle [list after 0 [list ::proxy::https::cleanup $id]]
}

proc ::proxy::https::cleanup {id} { unset -nocomplain $id }

# Convenience proc, returns the HTTP headers from the proxy server:
proc ::proxy::https::headers {id} {
	upvar #0 $id info
	return $info(headers)
}

