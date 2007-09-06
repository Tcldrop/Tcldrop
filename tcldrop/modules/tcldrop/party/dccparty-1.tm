# dccparty.tcl --
#
# $Id: dccparty.tcl,v 1.3 2005/05/03 22:47:11 fireegl Exp $
#
# Copyright (C) 2003,2004,2005 FireEgl (Philip Moore) <FireEgl@Tcldrop.Org>
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
# The author of this project can be reached at FireEgl@Tcldrop.Org
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
#	dccparty module for tcldrop.  (REQUIRED)
#
# This module provides the dcc chat interface for users to access the bot.

namespace eval ::tcldrop::dccparty {
	# Provide the dccparty module:
	variable version {0.4}
	package provide tcldrop::dccparty $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable rcsid {$Id: dccparty.tcl,v 1.3 2005/05/03 22:47:11 fireegl Exp $}
	# checkmodule console
	checkmodule partyline
	# Note: This module depends on the telnetparty module, because they're so similar and might as well share code.
	checkmodule telnetparty
	# Export all the commands that should be available to 3rd-party scripters:
	#namespace export
}

# This is when they do a /dcc chat $botnick
bind ctcp p DCC ::tcldrop::dccparty::DCC -priority 1000
proc ::tcldrop::dccparty::DCC {nick host hand dest key text} {
	if {[isbotnick $dest]} {
		if {$key == {DCC} && [string toupper [lindex [set text [split $text]] 0]] == {CHAT} && [expr 1024 < [set port [lindex $text end]] < 65535]} {
			set host [lindex [split $host @] end]
			# FixMe: $althost should be used if we can't connect to $host:
			set althost [lindex $text 2]
			set fail [catch { connect $host $port -timeout ${::connect-timeout} -myaddr ${::my-ip} -control ::tcldrop::telnetparty::Read -errors ::tcldrop::telnetparty::Error -writable ::tcldrop::dccparty::Write } idx]
			if {!$fail} { setidxinfo $idx [list idx $idx handle $hand remote $host hostname $host dccchatip $dccchatip port $port type TELNET_ID other {t-in} traffictype partyline timestamp [clock seconds]] }
		}
	}
	return 1
}

# This is when they do a /ctcp $botnick CHAT
bind ctcp p CHAT ::tcldrop::dccparty::CHAT -priority 1000
# FixMe: This doesn't seem to work yet.
proc ::tcldrop::dccparty::CHAT {nick uhost handle dest key text} {
	if {[isbotnick $dest]} {
		# FixMe: 3232235523 (192.168.0.3) and 7777 is hardcoded for now..
		puthelp "PRIVMSG $nick :\001DCC CHAT chat 3232235523 7777\001"
	}
	return 1
}

# Note: This proc is only used when people do a /ctcp <bot> CHAT
proc ::tcldrop::dccparty::Connect {idx} { setidxinfo $idx [list -control ::tcldrop::telnetparty::Read -writable ::tcldrop::dccparty::Write -errors ::tcldrop::telnetparty::Error module dccparty] }

# Note: We share code with the telnetparty module, so this proc isn't used and is only here as a reminder to look at the ::tcldrop::telnetparty::Error proc.
proc ::tcldrop::dccparty::Error {idx {error {}}} { ::tcldrop::telnetparty::Error $idx $error }

proc ::tcldrop::dccparty::Write {idx} {
	array set chatinfo $::idxlist($idx)
	if {![info exists chatinfo(handle)] || $chatinfo(handle) == {*}} {
		# Note: We share code with the telnetparty module.
		::tcldrop::telnetparty::Write $idx
	} else {
		array set chatinfo [list state CHAT_PASS other pass timestamp [clock seconds] traffictype partyline]
		set ::idxlist($idx) [array get chatinfo]
	}
}

# Note: We share code with the telnetparty module, so this proc isn't used and is only here as a reminder to look at the ::tcldrop::telnetparty::Read proc.
proc ::tcldrop::dccparty::Read {idx line} { ::tcldrop::telnetparty::Read $idx $line }

proc ::tcldrop::dccparty::dccchatcodefrompubsafetcl {} {
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

bind load - dccparty ::tcldrop::dccparty::LOAD -priority 0
proc ::tcldrop::dccparty::LOAD {module} {
	# Add new listen types (these are used by the [listen] command):
	addlistentype dccparty connect ::tcldrop::dccparty::Connect ident 1
	bind unld - dccparty ::tcldrop::dccparty::UNLD -priority 0
}

proc ::tcldrop::dccparty::UNLD {module} {
	# FixMe: dellistentype here.
	return 1
}
