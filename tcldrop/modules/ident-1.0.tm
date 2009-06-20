# ident.tcl --
#
#	Ident client for Tcl (See RFC 1413).
#
# Copyright (C) 2003,2004,2005,2006,2007,2008,2009 by Philip Moore <Tcl@FireEgl.Com>
# This code may be distributed under the same terms as Tcl.
#
# RCS: @(#) $Id$
#
# This package provides an ident client (See RFC 1413),
# used for querying a remote system to find the username
# that's connected to a port on the local machine.
#
# Usage:
#
#	::ident::ident -option value ?-option value? ...
#
# The possible options are listed below..
#
# -authport
#	Specifies the identd (auth) port to use on the remote (normally port 113).
#	(optional, defaults to 113)
#
# -sock
#	Specifies the socket that's currently connected to the remote host.
#	(optional, if it's not specified, you must use -lport, -rhost, and -rport)
#	Note, if -sock is used it takes the place of the -lhost, -lport, -rhost, -rport options.
#
# -lhost
#	Specifies the local hostname (or ip) to use for creating the socket to the remote.
#	(optional, defaults to the systems default)
#
# -lport
#	Specifies the local port that's currently connected to the remote.
#	(required, unless you use the -sock option)
#
# -rhost
#	Specifies the remote host that we're currently connected to.
#	(required, unless you use the -sock option)
#
# -rport
#	Specifies the remote port that we're currently connected to.
#	(required, unless you use the -sock option)
#
# -timeout
#	Specifies the timeout (in milliseconds) to use for the ident request.
#	(optional, defaults to 30000)
#
# -command
#	Specifies the command used as a callback when the ident request is complete.
#	(optional, without this, the script blocks until the ident request fails/succeeds)
#	::ident::ident will immediately return a unique ID used to identify
#	this ident request, and will later be passed to your callback command
#	when the lookup fails/succeeds.
#
#	When the ident request completes, your callback command will recieve
#	three arguments in addition to the arguments you specified for the command:
#	<id> <status> <username/or the error message>
#	The status will be either "ok" or "error".
#
#	If -command isn't used, ::ident::ident blocks until the ident request fails or succeeds,
#	and then returns a list with two elements..
#	The first element will be either "ok" or "error" to indicate whether the ident succeeded or failed.
#       If the first element is "ok", the second element will the username on the remote.
#	If the first element is "error", the second element will be the reason it failed.
#
#
# There's also some alias and convenience options...
#	-localhost or -myaddr is the same as -lhost
#	-localport is the same as -lport
#	-remotehost is the same as -rhost
#	-remoteport is the same as -rport
#	-socket is the same as -sock
#	-local can be used to specify the local host and port separated by a colon.
#	-remote can be used to specify the remote host and port separated by a colon.
#

package require Tcl 8.2

namespace eval ::ident {
	variable version {1.0}
	#variable rcsid {$Id$}
	variable Count
	if {![info exists Count]} { variable Count 0 }
	namespace export ident
}

proc ::ident::ident {args} {
	variable Count
	upvar 0 ::ident::[set id ident[incr Count]] info
	foreach {a d} $args {
		switch -- [string tolower $a] {
			{-sock} - {-socket} {
				if {![catch {fconfigure $d -peername} peerinfo]} {
					set info(rhost) [lindex $peerinfo 0]
					set info(rport) [lindex $peerinfo 2]
				} else {
					return -code error "Error getting peer info: $peerinfo"
				}
				if {![catch {fconfigure $d -sockname} sockinfo]} {
					set info(lhost) [lindex $sockinfo 0]
					set info(lport) [lindex $sockinfo 2]
				} else {
					return -code error "Error getting sock info: $sockinfo"
				}
			}
			{-lhost} - {-localhost} - {-chost} - {-clienthost} - {-myaddr} { set info(lhost) $d }
			{-lport} - {-localport} - {-cport} - {-clientport} { set info(lport) $d }
			{-rhost} - {-remotehost} - {-phost} - {-peerhost} - {-shost} - {-serverhost} { set info(rhost) $d }
			{-rport} - {-remoteport} - {-pport} - {-peerport} - {-sport} - {-serverport} { set info(rport) $d }
			{-remote} - {-server} - {-peer} { lassign [split $d :] info(rhost) info(rport) }
			{-local} - {-client} { lassign [split $d :] info(lhost) info(lport) }
			{-command} { set info(command) $d }
			{-timeout} { set info(timeout) $d }
			{-authport} - {-identdport} - {-identport} { set info(authport) $d }
			{default} { return -code error "Unknown option $a, must be -sock, -lhost, -lport, -rhost, -rport, -timeout, -command, -authport." }
		}
	}
	if {![info exists info(lport)]} {
		return -code error {You must specify at least one of these options: -sock, -lport, or -local}
	} elseif {![info exists info(rhost)]} {
		return -code error {You must specify at least one of these options: -sock, -rhost, or -remote}
	} elseif {![info exists info(rport)]} {
		return -code error {You must specify at least one of these options: -sock, -rport, or -remote}
	} elseif {![info exists info(lhost)]} { set info(lhost) {} }
	after 0 [list ::ident::Connect $id]
	if {[info exists info(command)] && $info(command) != {}} {
		set info(ident) {}
		set info(status) {na}
		return $id
	} else {
		# We wait here and return the results when it's ready.
		vwait "::ident::${id}(status)"
		# Returns the status and ident/error:
		list $info(status) $info(ident)
	}
}

proc ::ident::Connect {id} {
	upvar 0 ::ident::$id info
	if {![info exists info(authport)]} { set info(authport) {113} }
	if {$info(lhost) != {}} { set myaddr [list -myaddr $info(lhost)] } else { set myaddr [list] }
	if {[catch { eval {socket} {-async} $myaddr {$info(rhost)} {$info(authport)} } info(sock)]} {
		Finish $id {error} $info(sock)
	} else {
		fconfigure $info(sock) -buffering line -blocking 0
		fileevent $info(sock) writable [list ::ident::Write $id]
		# The RFC (1413) says to "wait at least 30 seconds or longer"
		if {![info exists info(timeout)]} { set info(timeout) 30999 }
		set info(afterid) [after $info(timeout) [list ::ident::Finish $id {error} {CONNECT-TIMEOUT}]]
	}
}

proc ::ident::Write {id} {
	upvar 0 ::ident::$id info
	fileevent $info(sock) writable {}
	fileevent $info(sock) readable [list ::ident::Read $id]
	if {[catch { puts $info(sock) "$info(rport) , $info(lport)" } error]} { Finish $id {error} $error }
}

proc ::ident::Read {id} {
	upvar 0 ::ident::$id info
	fileevent $info(sock) readable {}
	if {[catch { gets $info(sock) line } err]} {
		Finish $id {error} $err
	} elseif {$err == -1} {
		Finish $id {error} {SOCKET-ERROR}
	} else {
		switch -- [string toupper [string trim [lindex [set line [split $line :]] 1]]] {
			{ERROR} { Finish $id {error} [string trim [lindex $line 2]] }
			{USERID} { Finish $id {ok} [string trim [lindex $line 3]] }
			{default} { Finish $id {error} {UNKNOWN-RESPONSE} }
		}
	}
}

proc ::ident::Finish {id status text} {
	upvar 0 ::ident::$id info
	catch { after cancel $info(afterid) }
	catch { close $info(sock) }
	set info(ident) $text
	set info(status) $status
	if {[info exists info(command)] && $info(command) != {}} { uplevel #0 $info(command) [list $id $info(status) $info(ident)] }
	# We'll be done with it after we hit the idle-loop again...
	after idle [list ::ident::cleanup $id]
}

proc ::ident::idinfo {id} { array get ::ident::$id }

proc ::ident::cleanup {id} { catch { unset ::ident::$id } }

package provide ident 1.0
