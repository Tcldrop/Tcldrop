#	socks5.tcl ---
#
#		Package for using the SOCKS5 method for connecting TCP sockets.
#		Some code plus idee from Kerem 'Waster_' HADIMLI.
#		Made from RFC 1928.
#
#	(C) 2000	Kerem 'Waster_' HADIMLI (minor parts)
#	(c) 2003-2007	Mats Bengtsson
#	(C) 2009	FireEgl (Philip Moore) (I mainly fixed the indentations)
#
#	This source file is distributed under the BSD license.
#
# $Id$
#
# TODO:	GSSAPI authentication which is a MUST is missing.
#		Only CMD CONNECT implemented.
#		Do not report English text in callback but rather error keys like
#		rsp_notallowed etc. Client done, server to go.

# Upstream source:
# http://coccinella.svn.sourceforge.net/viewvc/coccinella/trunk/coccinella/contrib/socks5.tcl

package require proxy
namespace eval ::proxy::socks5 {
	# Constants:
	# ver:				Socks version
	# nomatchingmethod:	No matching methods
	# cmd_connect:		Connect command
	# rsv:				Reserved
	# atyp_*:			 Address type
	# auth_*:			 Authorication version
	variable const
	array set const {
		ver						\x05
		auth_no					\x00
		auth_gssapi				\x01
		auth_userpass			\x02
		nomatchingmethod		\xFF
		cmd_connect				\x01
		cmd_bind					\x02
		rsv						\x00
		atyp_ipv4				\x01
		atyp_domainname		\x03
		atyp_ipv6				\x04
		rsp_succeeded			\x00
		rsp_failure				\x01
		rsp_notallowed			\x02
		rsp_netunreachable	\x03
		rsp_hostunreachable	\x04
		rsp_refused				\x05
		rsp_expired				\x06
		rsp_cmdunsupported	\x07
		rsp_addrunsupported	\x08
	}

	# Practical when mapping errors to error codes.
	variable iconst
	array set iconst {
		\x00	rsp_succeeded
		\x01	rsp_failure
		\x02	rsp_notallowed
		\x03	rsp_netunreachable
		\x04	rsp_hostunreachable
		\x05	rsp_refused
		\x06	rsp_expired
		\x07	rsp_cmdunsupported
		\x08	rsp_addrunsupported
	}

	variable ipv4_num_re {([0-9]{1,3}\.){3}[0-9]{1,3}}
	variable ipv6_num_re {([0-9a-fA-F]{4}:){7}[0-9a-fA-F]{4}}

	variable msg
	array set msg {
		1 "General SOCKS server failure"
		2 "Connection not allowed by ruleset"
		3 "Network unreachable"
		4 "Host unreachable"
		5 "Connection refused"
		6 "TTL expired"
		7 "Command not supported"
		8 "Address type not supported"
	}

	variable debug 9
	variable uid 0
}

# socks5::init --
#
#		Negotiates with a SOCKS server.
#
# Arguments:
#		sock:		an open socket token to the SOCKS server
#		addr:		the peer address, not SOCKS server
#		port:		the peer's port number
#		args:
#				-command	tclProc {token status}
#				-username	username
#				-password	password
#				-timeout	millisecs
#
# Results:
#		token if -command, else ?.

proc ::proxy::socks5::init {sock addr port args} {
	variable msg
	variable const
	variable uid

	debug 2 "socks5::init $addr $port $args"

	# Initialize the state variable, an array.	We'll return the
	# name of this array as the token for the transaction.

	set token [namespace current]::[incr uid]
	variable $token
	upvar 0 $token state

	array set state {
		-password	""
		-timeout		60000
		-username	""
		async			0
		auth			0
		bnd_addr		""
		bnd_port		""
		state			""
		status		""
		trigger		0
	}
	array set state [list addr $addr port $port sock $sock]
	array set state $args

	if {[string length $state(-username)] || [string length $state(-password)]} {
		set state(auth) 1
	}
	if {[info exists state(-command)] && [string length $state(-command)]} {
		set state(async) 1
	}
	if {$state(auth)} {
		set methods "$const(auth_no)$const(auth_userpass)"
	} else {
		set methods "$const(auth_no)"
	}
	set nmethods [binary format c [string length $methods]]

	fconfigure $sock -translation {binary binary} -blocking 0
	fileevent $sock writable {}
	debug 2 "\tsend: ver nmethods methods"
	if {[catch {
		puts -nonewline $sock "$const(ver)$nmethods$methods"
		flush $sock
	} err]} {
		return -code error network-failure
	}

	# Setup timeout timer. !async remains!
	set state(timeoutid) [after $state(-timeout) [namespace current]::timeout $token]

	if {$state(async)} {
		fileevent $sock readable [list [namespace current]::response_method $token]
		return $token
	} else {
		# We should not return from this proc until finished!
		fileevent $sock readable [list [namespace current]::readable $token]
		vwait $token\(trigger)
		return [response_method $token]
	}
}

proc ::proxy::socks5::response_method {token} {
	variable $token
	variable const
	upvar 0 $token state

	debug 2 "socks5::response_method"

	set sock $state(sock)

	if {[catch {read $sock 2} data] || [eof $sock]} {
		finish $token network-failure
		return
	}
	set serv_ver ""
	set method $const(nomatchingmethod)
	binary scan $data cc serv_ver smethod
	debug 2 "\tserv_ver=$serv_ver, smethod=$smethod"

	if {![string equal $serv_ver 5]} {
		finish $token err_version
		return
	}

	if {[string equal $smethod 0]} {
		# Now, request address and port.
		request $token
	} elseif {[string equal $smethod 2]} {
		# User/Pass authorization required
		if {$state(auth) == 0} {
			finish $token err_authorization_required
			return
		}
		# Username & Password length (binary 1 byte)
		set ulen [binary format c [string length $state(-username)]]
		set plen [binary format c [string length $state(-password)]]

		debug 2 "\tsend: auth_userpass ulen -username plen -password"
		if {[catch {
			puts -nonewline $sock "$const(auth_userpass)$ulen$state(-username)$plen$state(-password)"
			flush $sock
		} err]} {
			finish $token network-failure
			return
		}

		if {$state(async)} {
			fileevent $sock readable [list [namespace current]::response_auth $token]
			return
		} else {

			# We should not return from this proc until finished!
			fileevent $sock readable [list [namespace current]::readable $token]
			vwait $token\(trigger)
			return [response_auth $token]
		}
	} else {
		finish $token err_unsupported_method
		return
	}
}

proc ::proxy::socks5::response_auth {token} {
	variable $token
	upvar 0 $token state

	debug 2 "socks5::response_auth"

	set sock $state(sock)

	if {[catch {read $sock 2} data] || [eof $sock]} {
		finish $token network-failure
		return
	}
	set auth_ver ""
	set status \x00
	binary scan $data cc auth_ver status
	debug 2 "\tauth_ver=$auth_ver, status=$status"

	if {![string equal $auth_ver 1]} {
		finish $token err_authentication_unsupported
		return
	}
	if {![string equal $status 0]} {
		finish $token err_authorization
		return
	}

	# Now, request address and port.
	return [request $token]
}

proc ::proxy::socks5::request {token} {
	variable $token
	variable const
	variable ipv4_num_re
	variable ipv6_num_re
	upvar 0 $token state

	debug 2 "socks5::request"

	set sock $state(sock)

	# Network byte-ordered port (2 binary-bytes, short)
	set bport [binary format S $state(port)]

	# Figure out type of address given to us.
	if {[regexp $ipv4_num_re $state(addr)]} {
		debug 2 "\tipv4"

		# IPv4 numerical address.
		set atyp_addr_port $const(atyp_ipv4)
		foreach i [split $state(addr) .] {
			append atyp_addr_port [binary format c $i]
		}
		append atyp_addr_port $bport
	} elseif {[regexp $ipv6_num_re $state(addr)]} {
		# todo
	} else {
		debug 2 "\tdomainname"

		# Domain name.
		# Domain length (binary 1 byte)
		set dlen [binary format c [string length $state(addr)]]
		set atyp_addr_port "$const(atyp_domainname)$dlen$state(addr)$bport"
	}

	# We send request for connect
	debug 2 "\tsend: ver cmd_connect rsv atyp_domainname dlen addr port"
	set aconst "$const(ver)$const(cmd_connect)$const(rsv)"
	if {[catch {
		puts -nonewline $sock "$aconst$atyp_addr_port"
		flush $sock
	} err]} {
		finish $token network-failure
		return
	}

	if {$state(async)} {
		fileevent $sock readable [list [namespace current]::response $token]
		return
	} else {
		# We should not return from this proc until finished!
		fileevent $sock readable [list [namespace current]::readable $token]
		vwait $token\(trigger)
		return [response $token]
	}
}

proc ::proxy::socks5::response {token} {
	variable $token
	upvar 0 $token state
	variable iconst

	debug 2 "socks5::response"

	set sock $state(sock)
	fileevent $sock readable {}

	# Start by reading ver+cmd+rsv.
	if {[catch {read $sock 3} data] || [eof $sock]} {
		finish $token network-failure
		return
	}
	set serv_ver ""
	set rep ""
	binary scan $data ccc serv_ver rep rsv

	if {![string equal $serv_ver 5]} {
		finish $token err_version
		return
	}
	if {$rep == 0} {
		# OK
	} elseif {[info exists iconst($rep)]} {
		finish $token $iconst($rep)
		return
	} else {
		finish $token err_unknown
		return
	}

	# Now parse the variable length atyp+addr+host.
	if {[catch {parse_atyp_addr $token addr port} err]} {
		finish $token $err
		return
	}

	# Store in our state array.
	set state(bnd_addr) $addr
	set state(bnd_port) $port

	# And finally let the client know that the bytestream is set up.
	return [finish $token]
}

proc ::proxy::socks5::parse_atyp_addr {token addrVar portVar} {
	variable $token
	variable const
	upvar 0 $token state
	upvar $addrVar addr
	upvar $portVar port

	debug 2 "socks5::parse_atyp_addr"

	set sock $state(sock)

	# Start by reading atyp.
	if {[catch {read $sock 1} data] || [eof $sock]} {
		return -code error network-failure
	}
	set atyp ""
	binary scan $data c atyp
	debug 2 "\tatyp=$atyp"

	# Treat the three address types in order.
	switch -- $atyp {
		1 {
			if {[catch {read $sock 6} data] || [eof $sock]} {
				return -code error network-failure
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
		}
		3 {
			if {[catch {read $sock 1} data] || [eof $sock]} {
				return -code error network-failure
			}
			binary scan $data c len
			debug 2 "\tlen=$len"
			set len [expr ( $len + 0x100 ) % 0x100]
			if {[catch {read $sock $len} data] || [eof $sock]} {
				return -code error network-failure
			}
			set addr $data
			debug 2 "\taddr=$addr"
			if {[catch {read $sock 2} data] || [eof $sock]} {
				return -code error network-failure
			}
			binary scan $data S port
			# Translate to unsigned!
			set port [expr ( $port + 0x10000 ) % 0x10000]
			debug 2 "\tport=$port"
		}
		4 {
			# todo
		}
		default {
			return -code error err_unknown_address_type
		}
	}
}

proc ::proxy::socks5::finish {token {errormsg ""}} {
	global errorInfo errorCode
	variable $token
	upvar 0 $token state

	debug 2 "socks5::finish errormsg=$errormsg"
	catch {after cancel $state(timeoutid)}

	# In case of error we do the cleanup.
	if {$state(async)} {
		if {[string length $errormsg]} {
			catch {close $state(sock)}
			uplevel #0 $state(-command) [list $token $errormsg]
			free $token
		} else {
			# unset the token on our own, eventually:
			after idle [list after 0 [list [namespace current]::free $token]]
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

proc ::proxy::socks5::getipandport {token} {
	variable $token
	upvar 0 $token state
	return [list $state(bnd_addr) $state(bnd_port)]
}

proc ::proxy::socks5::timeout {token} {
	finish $token timeout
}

proc ::proxy::socks5::free {token} {
	variable $token
	upvar 0 $token state
	unset -nocomplain state
}

# "cleanup" is my standard command name:
interp alias {} ::proxy::socks5::cleanup {} ::proxy::socks5::free
#proc ::proxy::socks5::cleanup {token} { free $token }

# socks5::serverinit --
#
#		The SOCKS5 server. Negotiates with a SOCKS5 client.
#		Sets up bytestreams between client and DST.
#
# Arguments:
#		sock:		socket connected to the servers socket
#		ip:		 ip address
#		port:		it's port number
#		command:	tclProc for callabcks {token type args}
#		args:
#				-blocksize	 bytes
#				-bytestream	boolean
#				-opendstsocket boolean
#				-timeout		millisecs
#
# Results:
#		token.

proc ::proxy::socks5::serverinit {sock ip port command args} {
	variable msg
	variable const
	variable uid

	debug 2 "socks5::serverinit"

	# Initialize the state variable, an array.	We'll return the
	# name of this array as the token for the transaction.

	set token [namespace current]::[incr uid]
	variable $token
	upvar 0 $token state

	array set state {
	-blocksize		8192
	-bytestream		1
	-opendstsocket	1
	-timeout			60000
	auth				0
	state			 ""
	status			""
	}
	array set state [list command $command sock $sock]
	array set state $args

	fconfigure $sock -translation {binary binary} -blocking 0
	fileevent $sock writable {}

	# Start by reading the method stuff.
	if {[catch {read $sock 2} data] || [eof $sock]} {
		serv_finish $token network-failure
		return
	}
	set ver ""
	set method $const(nomatchingmethod)
	binary scan $data cc ver nmethods
	set nmethods [expr ( $nmethods + 0x100 ) % 0x100]
	debug 2 "\tver=$ver, nmethods=$nmethods"

	# Error checking. Must have either noauth or userpasswdauth.
	if {![string equal $ver 5]} {
		serv_finish $token "Socks server isn't version 5!"
		return
	}
	for {set i 0} {$i < $nmethods} {incr i} {
		if {[catch {read $sock 1} data] || [eof $sock]} {
			serv_finish $token network-failure
			return
		}
		binary scan $data c method
		set method [expr ( $method + 0x100 ) % 0x100]
		debug 2 "\tmethod=$method"
		if {[string equal $method 0]} {
			set noauthmethod 1
		} elseif {[string equal $method 2]} {
			set userpasswdmethod 1
		}
	}
	set isok 1
	if {[info exists userpasswdmethod]} {
		set ans "$const(ver)$const(auth_userpass)"
		set state(auth) 1
	} elseif {[info exists noauthmethod]} {
		set ans "$const(ver)$const(auth_no)"
	} else {
		set ans "$const(ver)$const(nomatchingmethod)"
		set isok 0
	}

	debug 2 "\tsend: ver method"
	if {[catch {
		puts -nonewline $sock $ans
		flush $sock
	} err]} {
		serv_finish $token $err
		return
	}
	if {!$isok} {
		serv_finish $token "Unrecognized method requested by client"
		return
	}

	if {$state(auth)} {
		fileevent $sock readable [list [namespace current]::serv_auth $token]
	} else {
		fileevent $sock readable [list [namespace current]::serv_request $token]
	}
	return $token
}

proc ::proxy::socks5::serv_auth {token} {
	variable $token
	variable const
	upvar 0 $token state

	debug 2 "socks5::serv_auth"

	set sock $state(sock)
	fileevent $sock readable {}

	if {[catch {read $sock 2} data] || [eof $sock]} {
		serv_finish $token network-failure
		return
	}
	set auth_ver ""
	set method $const(nomatchingmethod)
	binary scan $data cc auth_ver ulen
	set ulen [expr ( $ulen + 0x100 ) % 0x100]
	debug 2 "\tauth_ver=$auth_ver, ulen=$ulen"
	if {![string equal $auth_ver 2]} {
		serv_finish $token "Wrong authorization method"
		return
	}
	if {[catch {read $sock $ulen} data] || [eof $sock]} {
		return -code error network-failure
	}
	set state(username) $data
	debug 2 "\tusername=$data"
	if {[catch {read $sock 1} data] || [eof $sock]} {
		serv_finish $token network-failure
		return
	}
	binary scan $data c plen
	set plen [expr ( $plen + 0x100 ) % 0x100]
	debug 2 "\tplen=$plen"
	if {[catch {read $sock $plen} data] || [eof $sock]} {
		serv_finish $token network-failure
		return
	}
	set state(password) $data
	debug 2 "\tpassword=$data"

	set ans [uplevel #0 $state(command) [list $token authorize -username $state(username) -password $state(password)]]
	if {!$ans} {
		catch { puts -nonewline $state(sock) "\x00\x01" }
		serv_finish $token notauthorized
		return
	}

	# Write auth response.
	if {[catch {
		puts -nonewline $sock "\x01\x00"
		flush $sock
	} err]} {
		serv_finish $token $err
		return
	}
	fileevent $sock readable [list [namespace current]::serv_request $token]
}

proc ::proxy::socks5::serv_request {token} {
	variable $token
	variable const
	variable msg
	variable ipv4_num_re
	variable ipv6_num_re
	upvar 0 $token state

	debug 2 "socks5::serv_request"

	set sock $state(sock)

	# Start by reading ver+cmd+rsv.
	if {[catch {read $sock 3} data] || [eof $sock]} {
		serv_finish $token network-failure
		return
	}
	set ver ""
	set cmd ""
	set rsv ""
	binary scan $data ccc ver cmd rsv
	debug 2 "\tver=$ver, cmd=$cmd, rsv=$rsv"

	if {![string equal $ver 5]} {
		serv_finish $token "Socks server isn't version 5!"
		return
	}
	if {![string equal $cmd 1]} {
		serv_finish $token "Unsuported CMD, must be CONNECT"
		return
	}

	# Now parse the variable length atyp+addr+host.
	if {[catch {parse_atyp_addr $token addr port} err]} {
		serv_finish $token $err
		return
	}

	# Store in our state array.
	set state(dst_addr) $addr
	set state(dst_port) $port

	# Init the SOCKS connection to dst if wanted. Else???
	if {$state(-opendstsocket)} {
		if {[catch {socket -async $addr $port} sock_dst]} {
			serv_finish $token network-failure
			return
		}
		set state(sock_dst) $sock_dst

		# Setup timeout timer.
		set state(timeoutid) [after $state(-timeout) [namespace current]::serv_timeout $token]
		fileevent $sock_dst writable [list [namespace current]::serv_dst_connect $token]
	} else {
		# ???
		uplevel #0 $state(command) [list $token reply]
	}
}

proc ::proxy::socks5::serv_dst_connect {token} {
	variable $token
	upvar 0 $token state

	debug 2 "socks5::serv_dst_connect"
	fileevent $state(sock_dst) writable {}
	after cancel $state(timeoutid)

	set sock_dst $state(sock_dst)
	if {[eof $sock_dst]} {
		serv_finish $token network-failure
		return
	}

	if {[catch {
		fconfigure $sock_dst -translation {binary binary} -blocking 0
		foreach {bnd_ip bnd_addr bnd_port} [fconfigure $sock_dst -sockname] { break }
	} err]} {
		debug 2 "\tfconfigure failed: $err"
		serv_finish $token network-failure
		return
	}
	array set state [list bnd_ip $bnd_ip bnd_addr $bnd_addr bnd_port $bnd_port]
	serv_reply $token
}

proc ::proxy::socks5::serv_reply {token} {
	variable $token
	variable const
	upvar 0 $token state

	debug 2 "socks5:serv_reply"
	set sock $state(sock)
	set bnd_addr $state(bnd_addr)
	set bnd_port $state(bnd_port)
	debug 2 "\tbnd_addr=$bnd_addr, bnd_port=$bnd_port"

	set aconst "$const(ver)$const(rsp_succeeded)$const(rsv)"

	# Domain length (binary 1 byte)
	set dlen [binary format c [string length $bnd_addr]]

	# Network byte-ordered port (2 binary-bytes, short)
	set bport [binary format S $bnd_port]
	set atyp_addr_port \
		"$const(atyp_domainname)$dlen$bnd_addr$bport"

	# We send SOCKS server's reply to client.
	debug 2 "\tsend: ver rep rsv atyp_domainname dlen bnd_addr bnd_port"
	if {[catch {
		puts -nonewline $sock "$aconst$atyp_addr_port"
		flush $sock
	} err]} {
		serv_finish $token $err
		return
	}

	# New we are ready to stream data if wanted.
	if {$state(-bytestream)} {
		establish_bytestreams $token
	} else {
		# ???
		serv_finish $token
	}
}

proc ::proxy::socks5::establish_bytestreams {token} {
	variable $token
	upvar 0 $token state

	debug 2 "socks5::establish_bytestreams"
	set sock $state(sock)
	set sock_dst $state(sock_dst)

	# Forward client stream to dst.
	fileevent $sock readable [list [namespace current]::read_stream $token $sock $sock_dst]

	# Forward dst stream to client.
	fileevent $sock_dst readable [list [namespace current]::read_stream $token $sock_dst $sock]
}

proc ::proxy::socks5::read_stream {token in out} {
	variable $token
	upvar 0 $token state

	set primary [string equal $state(sock) $in]
	debug 3 "::socks5::read_stream primary=$primary: in=$in, out=$out"

	# If any of client (sock) or dst (sock_dst) closes down we shall
	# close down everthing.
	# Only client or dst can determine if a close down is premature.

	if {[catch {eof $in} iseof] || $iseof} {
		serv_finish $token
	} elseif {[catch {eof $out} iseof] || $iseof} {
		serv_finish $token
	} elseif {[catch {read $in} data]} {
		serv_finish $token network-failure
	} else {
		# We could wait here (in the event loop) for channel to be writable
		# to avoid any blocking...
		# BUT, this would keep $data in memory for a while which is a bad idee.
		if {0} {
			fileevent $out writable [list [namespace current]::stream_writeable $token $primary]
			vwait $token\(writetrigger${primary})
		}
		if {[catch {puts -nonewline $out $data; flush $out}]} {
			serv_finish $token network-failure
		}
	}
}

proc ::proxy::socks5::stream_writeable {token primary} {
	variable $token
	upvar 0 $token state
	incr state(writetrigger${primary})
}

proc ::proxy::socks5::serv_finish {token {errormsg ""}} {
	variable $token
	upvar 0 $token state
	debug 2 "socks5::serv_finish"
	if {$state(-bytestream)} {
		catch {close $state(sock)}
		catch {close $state(sock_dst)}
	}
	if {[string length $errormsg]} {
		uplevel #0 $state(command) [list $token $errormsg]
	} else {
		uplevel #0 $state(command) [list $token ok]
	}
	unset state
}

#		Just a trigger for vwait.

proc ::proxy::socks5::readable {token} {
	variable $token
	upvar 0 $token state
	incr state(trigger)
}

proc ::proxy::socks5::serv_timeout {token} {
	variable $token
	upvar 0 $token state
	serv_finish $token timeout
}

proc ::proxy::socks5::debug {num str} {
	variable debug
	if {$num <= $debug} {
		tclLog "$num - $str"
	}
}

# Test code...

if {0} {
	# Server
	proc serv_cmd {token status} {
	puts "server: token=$token, status=$status"
	switch -- $status {
		ok {
		}
		authorize {
			# Here we should check that the username and password is ok.
			return 1
		}
		default {
			puts "error $status"
		}
	}
	}
	proc server_connect {sock ip port} {
		fileevent $sock readable [list socks5::serverinit $sock $ip $port serv_cmd]
	}
	socket -server server_connect 1080

	# Client
	proc cb {token status} {
		global s
		puts "client: token=$token, status=$status"
		if {$status eq "ok"} { fconfigure $s -buffering none }
	}
	proc dump {} {
		puts "dump:"
	}
	set s [socket 127.0.0.1 1080]
	socks5::init $s localhost 3344 -command cb
	#socks5::init $s localhost 3344 -command cb -username xxx -password yyy
}

package provide proxy::socks5 0.2

#-------------------------------------------------------------------------------
