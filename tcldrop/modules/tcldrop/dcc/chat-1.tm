# dcc::chat --
#
# $Id$
#
# Copyright (C) 2003,2004,2005,2006,2007,2008,2009 Tcldrop Development Team <Tcldrop-Dev@Tcldrop.US>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program (see gpl.txt); if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# Or visit http://www.GNU.Org/licenses/gpl.html
#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
#	dcc::chat module for tcldrop.  (REQUIRED)
#
# This module provides the dcc chat interface for users to access the bot.

namespace eval ::tcldrop::dcc::chat {
	variable version {0.4}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable name {dcc::chat}
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {dcc::telnet channels console core::users core::dcc core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Provides the dcc interface for users.}
	variable commands [list]
	variable rcsid {$Id$}
	# Note: This module depends on the dcc::telnet module, because they're so similar and might as well share code.
	# Export all the commands that should be available to 3rd-party scripters:
	#namespace export {*}$commands
}

# This is when they do a /dcc chat $botnick
bind ctcp op|o DCC ::tcldrop::dcc::chat::DCC -priority 1000
proc ::tcldrop::dcc::chat::DCC {nick host hand dest key text} {
	if {[info exists {::require-p}] && ${::require-p} && ![matchattr p $hand]} { return }
	if {[isbotnick $dest]} {
		if {$key == {DCC} && [string toupper [lindex [set text [split $text]] 0]] == {CHAT} && [expr 1024 < [set port [lindex $text end]] < 65535]} {
			set host [lindex [split $host @] end]
			# FixMe: we should try to connect to $decip first, and if it fails, connect to $host. Currently always connects to $decip
			# FixMe: honor the dcc-sanitycheck conf option
			set decip [lindex $text 2]
			set fail [catch { connect $decip $port -timeout ${::connect-timeout} -myaddr ${::my-ip} -control ::tcldrop::dcc::telnet::Read -errors ::tcldrop::dcc::telnet::Error -writable ::tcldrop::dcc::chat::Write } idx]
			if {!$fail} { idxinfo $idx idx $idx handle $hand remote $host hostname $host dccchatip $decip port $port type TELNET_ID other {t-in} traffictype partyline timestamp [clock seconds] }
		}
	}
	return 1
}

# This is when they do a /ctcp $botnick CHAT
bind ctcp op|o CHAT ::tcldrop::dcc::chat::CHAT -priority 1000
proc ::tcldrop::dcc::chat::CHAT {nick uhost handle dest key text} {
	if {[info exists {::require-p}] && ${::require-p} && ![matchattr p $hand]} { return }
	if {[isbotnick $dest]} {
		# FixMe: is this the log we want this in?
		putlog "CTCP CHAT: from $nick ($uhost)"
		if {![matchattr $handle p]} { return 0 }
		foreach i [array names ::idxlist] {
			array set idxinfo $::idxlist($i)
			if {$idxinfo(type) eq {users}} {
				if {[info exists idxinfo(local-port)]} { break }
			}
		}
		if {[info exists idxinfo(local-port)]} {
			puthelp "PRIVMSG $nick :\001DCC CHAT chat [myip] $idxinfo(local-port)\001"
		} else {
			putdebuglog "Local port was not found in idxlist, CTCP CHAT from $nick failed."
			return 0
		}
	}
	return 1
}

# Note: This proc is only used when people do a /ctcp <bot> CHAT
proc ::tcldrop::dcc::chat::Connect {idx} { idxinfo $idx -control ::tcldrop::dcc::telnet::Read -writable ::tcldrop::dcc::chat::Write -errors ::tcldrop::dcc::telnet::Error module dcc::chat }

# Note: We share code with the telnetparty module, so this proc isn't used and is only here as a reminder to look at the ::tcldrop::dcc::telnet::Error proc.
proc ::tcldrop::dcc::chat::Error {idx {error {}}} { ::tcldrop::dcc::telnet::Error $idx $error }

proc ::tcldrop::dcc::chat::Write {idx} {
	putdcc $idx "Password:"
	if {![dict exists $::idxlist($idx) handle] || [dict get $::idxlist($idx) handle] eq {*}} {
		# Note: We share code with the telnetparty module.
		::tcldrop::dcc::telnet::Write $idx
	} else {
		set ::idxlist($idx) [dict merge $::idxlist($idx) [dict create state CHAT_PASS other pass timestamp [clock seconds] traffictype partyline]]
	}
}

# Note: We share code with the telnetparty module, so this proc isn't used and is only here as a reminder to look at the ::tcldrop::dcc::telnet::Read proc.
proc ::tcldrop::dcc::chat::Read {idx line} { ::tcldrop::dcc::telnet::Read $idx $line }

proc ::tcldrop::dcc::chat::dccchatcodefrompubsafetcl {} {
	if {1024 < $dccChatPort && $dccChatPort < 65535} {
		if {[info exists DccChatSock]} {
			putlog "SafeTcl/DCC: Already listening on port ${dccChatPort}."
		} elseif {[catch { socket -server [namespace current]::DccChatServerConnect ${dccChatPort} } DccChatSock]} {
			putlog "SafeTcl/DCC: Can't listen on port (${dccChatPort}): ${DccChatSock}"
			unset DccChatSock
		} else {
			fconfigure ${DccChatSock} -buffering line -blocking 0
			putlog "SafeTcl/DCC: Listening on port ${dccChatPort}."
		}
		proc DccChatServerConnect {sock ip port} { fconfigure $sock -buffering line -blocking 0
			putlog "SafeTcl/DCC: Got connect from $sock ($ip:$port)."
			set timerid [utimer 9 [list [namespace current]::DccChatCheck $sock {DCCUser} {*} $ip $port]]
			fileevent $sock writable [list [namespace current]::DccChatIntro $sock $ip $port {DCCUser} {*} $timerid]
		}
		bind ctcp - DCC [namespace current]::EggdropDccChat
		proc EggdropDccChat {nick host hand dest key text} { putlog "In EggdropDccChat."
			putlog "nick: $nick\n host: $host\n hand: $hand\n dest: $dest\n key: $key\n text: $text"
			if {[isbotnick $dest] && $key == {DCC} && [string toupper [lindex [set text [split $text]] 0]] == {CHAT} && [expr 1024 < [set port [lindex $text end]] < 65535]} {
				set host [lindex [split $host @] end]
				set althost [lindex $text 2]
				DccChatConnect $nick $hand $host $port $althost
			} elseif {[isbotnick $dest] && [regexp -nocase {chat|tclchat|chattcl|safetcl|tcl|dcctcl|tcldcc} "$key"]} {
				variable MyDecimalIP
				variable dccChatPort
				puthelp "PRIVMSG $nick :\001DCC CHAT chat $MyDecimalIP $dccChatPort\001"
			}
			return 1
		}
		proc DccChatConnect {nick hand host port {althost {}}} {
			if {[catch { socket -async $host $port } sock]} {
				putlog "SafeTcl/DCC: Problem connecting to $host: $sock"
				if {$althost != {}} {
					DccChatConnect $nick $hand $althost $port
				} else {
					variable MyDecimalIP
					variable dccChatPort
					puthelp "PRIVMSG $nick :\001DCC CHAT chat $MyDecimalIP $dccChatPort\001"
				}
			} else {
				fconfigure $sock -buffering line -blocking 0
				putlog "SafeTcl/DCC: Attempting to connect to $host:$port..."
				set timerid [utimer 9 [list [namespace current]::DccChatCheck $sock $nick $hand $host $port $althost]]
				fileevent $sock writable [list [namespace current]::DccChatIntro $sock $host $port $nick $hand $timerid $althost]
			}
		}
		proc DccChatCheck {sock nick hand host port {althost {}}} {
			if {[eof $sock]} {
				putlog "SafeTcl/DCC: Got EOF on $sock"
			} elseif {[set error [fconfigure $sock -error]] != {}} {
				putlog "SafeTcl/DCC: Error: $error on $sock"
			}
			DccChatClose $sock $nick $hand $host $port
			if {$althost != {}} { DccChatConnect $nick $hand $althost $port }
		}
		proc DccChatClose {sock nick hand host port} {
			catch { fileevent $sock writable {} }
			catch { fileevent $sock readable {} }
			catch { close $sock }
		}
		# Provides a DCC CHAT'able Safe Tcl interpreter:
		proc DccChatIntro {sock host port nick hand {timerid {}} {althost {}}} {
			fileevent $sock writable {}
			catch { killutimer $timerid }
			if {[set error [fconfigure $sock -error]] == {}} {
				fileevent $sock readable [list [namespace current]::DccChatRead $sock $host $port $nick $hand]
				putlog "SafeTcl/DCC: Sending Intro to $host:$port ($sock)..."
				puts $sock "Hi $nick, you're connected to ${::botnet-nick}, running pubsafetcl.tcl.\nEnter your Tcl commands and I'll respond with the results.. Keep in mind that \"puts\" does NOT work."
			} elseif {$althost != {}} {
				putlog "SafeTcl/DCC: Error to $sock: $error.  Trying $althost..."
				DccChatCheck $sock $nick $hand $host $port $althost
			} else {
				putlog "SafeTcl/DCC: Got error on $sock: $error"
				DccChatClose $sock $nick $hand $host $port
				variable MyDecimalIP
				puthelp "PRIVMSG $nick :\001DCC CHAT chat $MyDecimalIP 44444\001"
			}
		}
		proc DccChatRead {sock host port nick hand} {
			if {[eof $sock] || [set length [gets $sock line]] == -1 || [slindex $line 0] == {exit}} {
				putlog "SafeTcl/DCC: EOF/exit from $host:$port ($sock)"
				DccChatClose $sock $nick $hand $host $port
			} elseif {[completeCheck $line "$sock-$host-$port"]} {
				foreach l [safetclEval [set line [completeGet "$sock-$host-$port"]]] { puts $sock $l }
				flush $sock
				putlog "SafeTcl/DCC: Read $length chars from $host:$port ($sock): $line"
			} elseif {$length == 0 && [completeGet "$sock-$host-$port" 0] == {}} {
				puts $sock {% }
				flush $sock
				putlog "SafeTcl/DCC: Read $host:$port ($sock) - $length chars: $line"
			}
		}

		# FixMe: Some of this may not be needed, as [myip] tells the bots IP in decimal notation.
		variable MyIP
		variable MyHostname
		if {![info exists MyIP]} { set MyIP {127.0.0.1} }
		if {![info exists MyHostname]} { set MyHostname {localhost} }
		bind evnt - init-server [namespace current]::EggdropInitServer
		proc EggdropInitServer {type} {
			catch {
				dnslookup [lindex [split $::botname @] end] [namespace current]::EggdropGetInternetIP
			}
		}
		proc EggdropGetInternetIP {ip host status} {
			if {$status || $ip == $host} {
				variable MyIP $ip
				variable MyDecimalIP [IP2Decimal $ip]
				variable MyHostname $host
			} elseif {$ip == {0.0.0.0}} {
				variable MyIP {127.0.0.1}
				variable MyDecimalIP {2130706433}
				variable MyHostname $host
			}
		}

		proc IP2Decimal {ip} {
			foreach {a b c d} [split $ip .] {}
			format %u 0x[format %02X%02X%02X%02X $a $b $c $d]
		}
	}
}

bind load - dcc::chat ::tcldrop::dcc::chat::LOAD -priority 0
proc ::tcldrop::dcc::chat::LOAD {module} {
	# Add new listen types (these are used by the [listen] command):
	addlistentype dccparty connect ::tcldrop::dcc::chat::Connect ident 1
	addlistentype dcc::chat connect ::tcldrop::dcc::chat::Connect ident 1
	bind unld - dcc::chat ::tcldrop::dcc::chat::UNLD -priority 0
}

proc ::tcldrop::dcc::chat::UNLD {module} {
	# FixMe: dellistentype here.
	return 1
}
