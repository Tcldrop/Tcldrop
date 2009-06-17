#  socks5.tcl ---
#
#	  Package for using the SOCKS5 method for connecting TCP sockets.
#	  Some code plus idee from Kerem 'Waster_' HADIMLI.
#	  Made from RFC 1928.
#
#  (C) 2000 Kerem 'Waster_' HADIMLI (minor parts)
#  (c) 2003  Mats Bengtsson
#  (C) 2005 Philip Moore
#  This source file is distributed under the BSD license.
#
# $Id$
#
# TODO:  GSSAPI authentication which is a MUST is missing.
#		Only CMD CONNECT implemented.
#
# Taken from: http://cvs.sourceforge.net/viewcvs.py/*checkout*/coccinella/coccinella/contrib/socks5.tcl

package provide proxy::socks5 0.2
namespace eval ::proxy::socks5 {
	# Constants:
	# ver: Socks version
	# nomatchingmethod: No matching methods
	# cmd_connect: Connect command
	# rsv: Reserved
	# atyp_*: Address type
	# auth_*: Authorication version
	variable Constants
	array set Constants {
		ver \x05
		auth_no \x00
		auth_gssapi \x01
		auth_userpass \x02
		nomatchingmethod \xFF
		cmd_connect \x01
		cmd_bind \x02
		rsv \x00
		atyp_ipv4 \x01
		atyp_domainname \x03
		atyp_ipv6 \x04
		rsp_succeeded \x00
		rsp_failure \x01
		rsp_notallowed \x02
		rsp_netunreachable \x03
		rsp_hostunreachable \x04
		rsp_refused \x05
		rsp_expired \x06
		rsp_cmdunsupported \x07
		rsp_addrunsupported \x08
	}
	variable IPv4_num_RE {([0-9]{1,3}\.){3}[0-9]{1,3}}
	variable IPv6_num_RE {([0-9a-fA-F]{4}:){7}[0-9a-fA-F]{4}}
	variable ErrorMessages
	array set ErrorMessages {
		1 "General SOCKS server failure"
		2 "Connection not allowed by ruleset"
		3 "Network unreachable"
		4 "Host unreachable"
		5 "Connection refused"
		6 "TTL expired"
		7 "Command not supported"
		8 "Address type not supported"
	}
	variable Debug 3
	variable CountID 0
}

# socks5::init --
#
#	   Negotiates with a SOCKS server.
#
# Arguments:
#	   sock:	   an open socket token to the SOCKS server
#	   addr:	   the peer address, not SOCKS server
#	   port:	   the peer's port number
#	   args:
#			   -command	tclProc {token type args}
#			   -user   username
#			   -pass   password
#			   -timeout	millisecs
#
# Results:
#	   token if -command, else ?.
proc ::proxy::socks5::init {sock addr port args} {
	variable ErrorMessages
	variable Constants
	variable CountID
	debug 2 "socks5::init"
	# Initialize the state variable, an array.  We'll return the
	# name of this array as the token for the transaction.
	set token [namespace current]::[incr CountID]
	variable $token
	upvar 0 $token state
	array set state [list addr $addr port $port sock $sock -pass {} -timeout 999999 -user {} async 0 auth 0 bnd_addr {} bnd_port {} state {} status {} trigger 0]
	array set state $args
	if {[string length $state(-user)] || [string length $state(-pass)]} { set state(auth) 1 }
	if {[info exists state(-command)] && [string length $state(-command)]} { set state(async) 1 }
	if {$state(auth)} { set methods "$Constants(auth_no)$Constants(auth_userpass)" } else { set methods "$Constants(auth_no)" }
	set nmethods [binary format c [string length $methods]]
	fconfigure $sock -translation {binary binary} -blocking 0
	fileevent $sock writable {}
	debug 2 "\tsend: ver nmethods methods"
	if {[catch { puts -nonewline $sock "$Constants(ver)$nmethods$methods" ; flush $sock } err]} { return -code error $err }
	# Setup timeout timer. !async remains!
	set state(timeoutid) [after $state(-timeout) [namespace current]::timeout $token]
	if {$state(async)} {
		fileevent $sock readable [list [namespace current]::response_method $token]
		return $token
	} else {
		# We should not return from this proc until finished!
		fileevent $sock readable [list [namespace current]::readable $token]
		vwait $token\(trigger)
		if {![catch { response_method $token } return]} { return $return } else { return $sock }
	}
}

proc ::proxy::socks5::response_method {token} {
variable $token
	variable Constants
	upvar 0 $token state
	debug 2 "socks5::response_method"
	set sock $state(sock)
	if {[catch {read $sock 2} data] || [eof $sock]} {
		finish $token eof
		return
	}
	set serv_ver ""
	set method $Constants(nomatchingmethod)
	binary scan $data cc serv_ver smethod
	debug 2 "\tserv_ver=$serv_ver, smethod=$smethod"
	if {![string equal $serv_ver 5]} { return [finish $token "Socks server isn't version 5!"] }
	if {[string equal $smethod 0]} {
		# Now, request address and port.
		request $token
	} elseif {[string equal $smethod 2]} {
		# User/Pass authorization required
		if {$state(auth) == 0} { return [finish $token "User/Pass authorization required by Socks Server!"] }
		# Username & Password length (binary 1 byte)
		set ulen [binary format c [string length $state(-user)]]
		set plen [binary format c [string length $state(-pass)]]
		debug 2 "\tsend: auth_userpass ulen -user plen -pass"
		if {[catch { puts -nonewline $sock "$Constants(auth_userpass)$ulen$state(-user)$plen$state(-pass)" ; flush $sock } err]} { return [finish $token $err] }
		if {$state(async)} {
			return [fileevent $sock readable [list [namespace current]::response_auth $token]]
		} else {
			# We should not return from this proc until finished!
			fileevent $sock readable [list [namespace current]::readable $token]
			vwait $token\(trigger)
			return [response_auth $token]
		}
	} else {
		return [finish $token "Method not supported by Socks Server!"]
	}
}

proc ::proxy::socks5::response_auth {token} {
	variable $token
	upvar 0 $token state
	debug 2 "socks5::response_auth"
	set sock $state(sock)
	if {[catch {read $sock 2} data] || [eof $sock]} { return [finish $token eof] }
	set auth_ver ""
	set status \x00
	binary scan $data cc auth_ver status
	debug 2 "\tauth_ver=$auth_ver, status=$status"
	if {![string equal $auth_ver 1]} { return [finish $token "Socks Server's authentication isn't supported!"] }
	if {![string equal $status 0]} { return [finish $token "Wrong username or password!"] }
	# Now, request address and port.
	return [request $token]
}

proc ::proxy::socks5::request {token} {
	variable $token
	variable Constants
	variable IPv4_num_RE
	variable IPv6_num_RE
	upvar 0 $token state
	debug 2 "socks5::request"
	set sock $state(sock)
	# Network byte-ordered port (2 binary-bytes, short)
	set bport [binary format S $state(port)]
	# Figure out type of address given to us.
	if {[regexp $IPv4_num_RE $state(addr)]} {
		debug 2 "\tipv4"
		# IPv4 numerical address.
		set atyp_addr_port $Constants(atyp_ipv4)
		foreach i [split $state(addr) .] { append atyp_addr_port [binary format c $i] }
		append atyp_addr_port $bport
	} elseif {[regexp $IPv6_num_RE $state(addr)]} {
		# todo
	} else {
		debug 2 "\tdomainname"
		# Domain name.
		# Domain length (binary 1 byte)
		set dlen [binary format c [string length $state(addr)]]
		set atyp_addr_port "$Constants(atyp_domainname)$dlen$state(addr)$bport"
	}
	# We send request for connect
	debug 2 "\tsend: ver cmd_connect rsv atyp_domainname dlen addr port"
	set aconst "$Constants(ver)$Constants(cmd_connect)$Constants(rsv)"
	if {[catch {
		puts -nonewline $sock "$aconst$atyp_addr_port"
		flush $sock
	} err]} {
		return [finish $token $err]
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
	variable ErrorMessages
	upvar 0 $token state
	debug 2 "socks5::response"
	set sock $state(sock)
	fileevent $sock readable {}
	# Start by reading ver+cmd+rsv.
	if {[catch {read $sock 3} data] || [eof $sock]} {
		finish $token eof
		return
	}
	set serv_ver ""
	set rep ""
	binary scan $data ccc serv_ver rep rsv
	if {![string equal $serv_ver 5]} {
		finish $token "Socks server isn't version 5!"
		return
	}
	switch -- $rep {
		0 {
			#fconfigure $sock -translation {auto auto}
		}
		1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 {
			finish $token $ErrorMessages($rep)
			return
		}
		default {
			finish $token "Socks server responded: Unknown Error"
			return
		}
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
	finish $token
}

proc ::proxy::socks5::parse_atyp_addr {token addrVar portVar} {
	variable $token
	variable Constants
	upvar 0 $token state
	upvar $addrVar addr
	upvar $portVar port
	debug 2 "socks5::parse_atyp_addr"
	set sock $state(sock)
	# Start by reading atyp.
	if {[catch {read $sock 1} data] || [eof $sock]} { return -code error eof }
	set atyp ""
	binary scan $data c atyp
	debug 2 "\tatyp=$atyp"
	# Treat the three address types in order.
	switch -- $atyp {
		1 {
			if {[catch {read $sock 6} data] || [eof $sock]} { return -code error eof }
			binary scan $data ccccS i0 i1 i2 i3 port
			set addr ""
			foreach n [list $i0 $i1 $i2 $i3] {
				# Translate to unsigned!
				append addr [expr ( $n + 0x100 ) % 0x100]
				if {$n <= 2} { append addr . }
			}
			# Translate to unsigned!
			set port [expr ( $port + 0x10000 ) % 0x10000]
		}
		3 {
			if {[catch {read $sock 1} data] || [eof $sock]} { return -code error eof }
			binary scan $data c len
			debug 2 "\tlen=$len"
			set len [expr ( $len + 0x100 ) % 0x100]
			if {[catch {read $sock $len} data] || [eof $sock]} { return -code error eof }
			set addr $data
			debug 2 "\taddr=$addr"
			if {[catch {read $sock 2} data] || [eof $sock]} { return -code error eof }
			binary scan $data S port
			# Translate to unsigned!
			set port [expr ( $port + 0x10000 ) % 0x10000]
			debug 2 "\tport=$port"
		}
		4 {
			# todo
		}
		default {
			return -code error "Unknown address type"
		}
	}
}

proc ::proxy::socks5::finish {token {errormsg {}}} {
	variable $token
	upvar 0 $token state
	debug 2 "socks5::finish errormsg=$errormsg"
	catch { after cancel $state(timeoutid) }
	if {$state(async)} {
		if {[string length $errormsg]} {
			catch {close $state(sock)}
			uplevel #0 $state(-command) [list $token error $errormsg]
			cleanup $token
		} else {
			uplevel #0 $state(-command) [list $token ok]
		}
	} else {
		if {[string length $errormsg]} {
			catch {close $state(sock)}
			cleanup $token
			return -code error $errormsg
		} else {
			return ""
		}
	}
}

proc ::proxy::socks5::timeout {token} {
	debug 2 "timeout"
	variable $token
	upvar 0 $token state
	after idle [list finish $token timeout]
}

proc ::proxy::socks5::cleanup {token} {
	debug 2 "cleanup"
	variable $token
	upvar 0 $token state
	unset -nocomplain state
}

# socks5::serverinit --
#
#	   The SOCKS5 server. Negotiates with a SOCKS5 client.
#	   Sets up bytestreams between client and DST.
#
# Arguments:
#	   sock:	   socket connected to the servers socket
#	   ip:		 ip address
#	   port:	   it's port number
#	   command:	tclProc for callabcks {token type args}
#	   args:
#			   -blocksize	 bytes
#			   -bytestream	boolean
#			   -opendstsocket boolean
#			   -timeout	   millisecs
#
# Results:
#	   token.

proc ::proxy::socks5::serverinit {sock ip port command args} {
	variable ErrorMessages
	variable Constants
	variable CountID

	debug 2 "socks5::serverinit"

	# Initialize the state variable, an array.  We'll return the
	# name of this array as the token for the transaction.

	set token [namespace current]::[incr CountID]
	variable $token
	upvar 0 $token state

	array set state {
	-blocksize		8192
	-bytestream	   1
	-opendstsocket	1
	-timeout		  60000
	auth			  0
	state			 ""
	status			""
	}
	array set state [list		\
	  command	   $command	 \
	  sock		  $sock]
	array set state $args

	fconfigure $sock -translation {binary binary} -blocking 0
	fileevent $sock writable {}

	# Start by reading the method stuff.
	if {[catch {read $sock 2} data] || [eof $sock]} {
	serv_finish $token eof
	return
	}
	set ver ""
	set method $Constants(nomatchingmethod)
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
		serv_finish $token eof
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
	set ans "$Constants(ver)$Constants(auth_userpass)"
	set state(auth) 1
	} elseif {[info exists noauthmethod]} {
	set ans "$Constants(ver)$Constants(auth_no)"
	} else {
	set ans "$Constants(ver)$Constants(nomatchingmethod)"
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
	fileevent $sock readable  \
	  [list [namespace current]::serv_auth $token]
	} else {
	fileevent $sock readable  \
	  [list [namespace current]::serv_request $token]
	}
	return $token
}

proc ::proxy::socks5::serv_auth {token} {
	variable $token
	variable Constants
	upvar 0 $token state

	debug 2 "socks5::serv_auth"

	set sock $state(sock)
	fileevent $sock readable {}

	if {[catch {read $sock 2} data] || [eof $sock]} {
	serv_finish $token eof
	return
	}
	set auth_ver ""
	set method $Constants(nomatchingmethod)
	binary scan $data cc auth_ver ulen
	set ulen [expr ( $ulen + 0x100 ) % 0x100]
	debug 2 "\tauth_ver=$auth_ver, ulen=$ulen"
	if {![string equal $auth_ver 2]} {
	serv_finish $token "Wrong authorization method"
	return
	}
	if {[catch {read $sock $ulen} data] || [eof $sock]} {
	return -code error eof
	}
	set state(username) $data
	debug 2 "\tusername=$data"
	if {[catch {read $sock 1} data] || [eof $sock]} {
	serv_finish $token eof
	return
	}
	binary scan $data c plen
	set plen [expr ( $plen + 0x100 ) % 0x100]
	debug 2 "\tplen=$plen"
	if {[catch {read $sock $plen} data] || [eof $sock]} {
	serv_finish $token eof
	return
	}
	set state(password) $data
	debug 2 "\tpassword=$data"

	set ans [uplevel #0 $state(command) [list $token authorize \
	  -user $state(username) -pass $state(password)]]
	if {!$ans} {
	catch {
		puts -nonewline $state(sock) "\x00\x01"
	}
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
	fileevent $sock readable  \
	  [list [namespace current]::serv_request $token]
}

proc ::proxy::socks5::serv_request {token} {
	variable $token
	variable Constants
	variable ErrorMessages
	variable IPv4_num_RE
	variable IPv6_num_RE
	upvar 0 $token state

	debug 2 "socks5::serv_request"

	set sock $state(sock)

	# Start by reading ver+cmd+rsv.
	if {[catch {read $sock 3} data] || [eof $sock]} {
	serv_finish $token eof
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
		serv_finish $token eof
		return
	}
	set state(sock_dst) $sock_dst

	# Setup timeout timer.
	set state(timeoutid)  \
	  [after $state(-timeout) [namespace current]::serv_timeout $token]
	fileevent $sock_dst writable  \
	  [list [namespace current]::serv_dst_connect $token]
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
	serv_finish $token eof
	return
	}

	if {[catch {
	fconfigure $sock_dst -translation {binary binary} -blocking 0
	foreach {bnd_ip bnd_addr bnd_port} [fconfigure $sock_dst -sockname] \
	  break
	} err]} {
	debug 2 "\tfconfigure failed: $err"
	serv_finish $token eof
	return
	}
	array set state [list bnd_ip $bnd_ip bnd_addr $bnd_addr bnd_port $bnd_port]
	serv_reply $token
}

proc ::proxy::socks5::serv_reply {token} {
	variable $token
	variable Constants
	upvar 0 $token state

	debug 2 "socks5:serv_reply"
	set sock $state(sock)
	set bnd_addr $state(bnd_addr)
	set bnd_port $state(bnd_port)
	debug 2 "\tbnd_addr=$bnd_addr, bnd_port=$bnd_port"

	set aconst "$Constants(ver)$Constants(rsp_succeeded)$Constants(rsv)"

	# Domain length (binary 1 byte)
	set dlen [binary format c [string length $bnd_addr]]

	# Network byte-ordered port (2 binary-bytes, short)
	set bport [binary format S $bnd_port]
	set atyp_addr_port \
	  "$Constants(atyp_domainname)$dlen$bnd_addr$bport"

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
	fileevent $sock readable  \
	  [list [namespace current]::read_stream $token $sock $sock_dst]

	# Forward dst stream to client.
	fileevent $sock_dst readable  \
	  [list [namespace current]::read_stream $token $sock_dst $sock]
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
	serv_finish $token eof
	} else {

	# We could wait here (in the event loop) for channel to be writable
	# to avoid any blocking...
	# BUT, this would keep $data in memory for a while which is a bad idee.
	if {0} {
		fileevent $out writable  \
		  [list [namespace current]::stream_writeable $token $primary]
		vwait $token\(writetrigger${primary})
	}
	if {[catch {puts -nonewline $out $data; flush $out}]} {
		serv_finish $token eof
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
	uplevel #0 $state(command) [list $token error -error $errormsg]
	} else {
	uplevel #0 $state(command) [list $token ok]
	}
	unset state
}

#	   Just a trigger for vwait.

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
	variable Debug
	if {$num <= $Debug} {
	puts $str
	}
}

# Test code...

if {0} {

	# Server
	proc serv_cmd {token type args} {
	puts "server: token=$token, type=$type, args=$args"
	switch -- $type {
		error {

		}
		ok {

		}
		authorize {
		# Here we should check that the username and password is ok.
		return 1
		}
	}
	}
	proc server_connect {sock ip port} {
	fileevent $sock readable  \
	  [list socks5::serverinit $sock $ip $port serv_cmd]
	}
	socket -server server_connect 1080

	# Client
	proc cb {token type args} {
	global s
	puts "client: token=$token, type=$type, args=$args"
	if {$type == "ok"} {
		fconfigure $s -buffering none
	}
	}
	proc dump {} {
	puts "dump:"
	}
	set s [socket 127.0.0.1 1080]
	socks5::init $s localhost 3344 -command cb
	#socks5::init $s localhost 3344 -command cb -user xxx -pass yyy
}

#-------------------------------------------------------------------------------
