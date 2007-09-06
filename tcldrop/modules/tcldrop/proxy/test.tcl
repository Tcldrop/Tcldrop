
source proxy.tcl

#puts [::proxy::splitchain {https://127.0.0.1:8080/socks5://socks.server.com:3128/irc.choopa.net:6667:SomePassword}]

#puts [::proxy::splitchain {https://127.0.0.1:8080/irc.choopa.net:6667:mypassword}]


#puts [::proxy::splitchain {/irc.choopa.net:6667:mypassword}]


#puts [::proxy::splitchain {irc.choopa.net:6668}]

#puts [::proxy::splitchain {:6668}]


#puts [::proxy::splitchain {irc.choopa.net:6667:mypassword}]

#puts [::proxy::splitchain {irc.choopa.net}]


set my-ip {}
set server-timeout 1234
set inactive-timeout 5678
set connect-timeout 9876

set default-port 6000

proc rand {maxint {minint {0}}} { expr { int($minint + rand() * ($maxint - $minint)) } }

set servers {irc.efnet.net}

proc Server {opts} {
	puts "in Server: $opts"
}
set server-online 0


proc jump {args} {
	puts "in ::tcldrop::server::jump $args"
	#if {${::server-online}} { quit {Changing Servers...} }
	array set options [list -address {} -port {0} -pass {} -buffering line -blocking 0 -myaddr ${::my-ip} -async 1 -connect-timeout ${::server-timeout} -inactive-timeout ${::inactive-timeout} -ssl 0 -proxychain {} -proxyinfo [list]]
	# There's 3 different ways to specify the address and port to connect to...
	# Proxychain style: address:port or http://127.0.0.1:8080/address:port
	# Option type style: -address <address> -port <port>
	# Eggdrop style: address port
	puts "Default options: [array get options]"
	switch -glob -- [lindex $args 0] {
		{*:*} {
			puts "Proxychain style."
			# Proxychain style.
			array set proxychain [::proxy::splitchain [lindex $args 0]]
			lappend args -address $proxychain(address) -port $proxychain(port) -pass $proxychain(extra) -proxychain [lindex $args 0] -proxyinfo [array get proxychain]
			if {[llength [lrange $args 1 end]] % 2} {
				lappend args -pass [lindex $args 1]
				array set options [lrange $args 2 end]
			} else {
				array set options [lrange $args 1 end]
			}
		}
		{-*} {
			puts "Option type style."
			# Option type style.
			array set options $args
		}
		{default} {
			puts "Eggdrop style."
			# Eggdrop style.
			array set options [list -address [lindex $args 0] -port [lindex $args 1]]
			set args [lrange $args 2 end]
			if {[llength [lrange $args 2 end]] % 2} {
				# Odd number, meaning they also specified a password as the 3rd arg.
				lappend args -pass [lindex $args 2]
				array set options [lrange $args 3 end]
			} else {
				array set options [lrange $args 2 end]
			}
		}
	}
	# If -address isn't set, it means a server wasn't specified and we should pick one from $::servers:
	if {$options(-address) == {}} { after idle [list jump [lindex $::servers [rand [llength $::servers]]]] }
	# Make sure the -port option is valid:
	if {![string is int -strict $options(-port)] || 1 > $options(-port) || $options(-port) > 65535 } { set options(-port) ${::default-port} }
	# The -proxychain option is required when passing to the Server command:
	if {![info exists options(-proxychain)] || $options(-proxychain) == {}} { set options(-proxychain) "$options(-address):$options(-port)" }
	# Make sure we didn't leave the password at the end of -proxychain:
	if {$options(-pass) != {} && [string match "*$options(-pass)" $options(-proxychain)]} { set options(-proxychain) [join [lrange [split $options(-proxychain) :] 0 end-1] :] }
	# Make sure the port is at the end of -proxychain:
	if {[string match {*:} $options(-proxychain)]} { append options(-proxychain) $options(-port) }
	puts "Calling Server with: [array get options]"
	array set options [list -control ::tcldrop::server::Read -errors ::tcldrop::server::Error -writable ::tcldrop::server::Write]
	Connect [array get options]
}

proc connect {args} { Connect $args }
proc Connect {opts} {
	puts "in ::tcldrop::core::conn::Connect $opts"
	array set options [list -address {} -port {} -buffering line -blocking 0 -myaddr ${::my-ip} -async 1 -connect-timeout ${::connect-timeout} -inactive-timeout ${::inactive-timeout} -ssl 0 -proxychain {} -proxyinfo [list]]
	# There's 3 different ways to specify the address and port to connect to...
	# Proxychain style: address:port or http://127.0.0.1:8080/address:port
	# Option type style: -address <address> -port <port>
	# Eggdrop style: address port
	switch -glob -- [lindex $opts 0] {
		{*:*} {
			# Proxychain style.
			array set proxychain [::proxy::splitchain [lindex $opts 0]]
			lappend opts -address $proxychain(address) -port $proxychain(port) -proxychain [lindex $opts 0] -proxyinfo [array get proxychain]
			set opts [lrange $opts 1 end]
		}
		{-*} {
			# Option type style.
		}
		{default} {
			# Eggdrop style.
			lappend opts -address [lindex $opts 0] -port [lindex $opts 1]
			set opts [lrange $opts 2 end]
		}
	}
	array set options $opts
	if {$options(-proxychain) == {}} { set options(-proxychain) "$options(-address):$options(-port)" }
	puts "...Connect: [array get options]"
}

#jump https://127.0.0.1:8080/irc.choopa.net:5000
connect https://127.0.0.1:8080
