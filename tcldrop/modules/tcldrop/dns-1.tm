# dns.tcl --
#	Handles:
#		* Provides the dnslookup command.
#
# $Id: dns.tcl,v 1.5 2006/04/15 04:21:07 fireegl Exp $
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

namespace eval ::tcldrop::dns {
	variable name {dns}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	if {![info exists ::tcldrop]} { return }
	variable depends {core}
	variable author {Tcldrop-Dev}
	variable description {Provides the dnslookup commands.}
	variable commands [list dnslookup testip]
	variable rcsid {$Id$}
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
}

proc ::tcldrop::dns::testip {ip} { regexp {^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$} $ip }

bind load - dns ::tcldrop::dns::LOAD -priority 0
proc ::tcldrop::dns::LOAD {module} {
	variable Counter
	if {![info exists Counter]} { variable Counter 0 }
	variable Lookups
	array set Lookups {}
	setdefault resolve-timeout 9
	bind unld - dns ::tcldrop::dns::UNLD -priority 0
	proc ::tcldrop::dns::UNLD {module} {
		catch { package forget dns }
		return 0
	}
}
#  dnslookup <ip-address/hostname> <proc> [[arg1] [arg2] ... [argN]]
#    Description: This issues an asynchronous dns lookup request. The
#      command will block if dns module is not loaded; otherwise it will
#      either return immediately or immediately call the specified proc
#      (e.g. if the lookup is already cached).
#
#      As soon as the request completes, the specified proc will be called
#      as follows:
#
#       <proc> <ipaddress> <hostname> <status> [[arg1] [arg2] ... [argN]]
#
#      status is 1 if the lookup was successful and 0 if it wasn't. All
#      additional parameters (called arg1, arg2 and argN above) get
#      appended to the proc's other parameters.
#    Returns: nothing
#    Module: core
if {![catch { package require dns }]} {
	putlog {Using tcllib's dns package for [dnslookup].  (asynchronous)}
	::dns::configure -timeout [expr { [setdefault resolve-timeout 9] * 1001 }] -loglevel error
	proc ::tcldrop::dns::dnslookup {address proc args} {
		variable Lookups
		set Lookups([::dns::resolve $address -command ::tcldrop::dns::Results]) [list $address $proc $args]
	}
	proc ::tcldrop::dns::Results {token} {
		# FixMe: This proc could be done better.
		variable Lookups
		foreach {address proc args} $Lookups($token) {break}
		unset Lookups($token)
		if {![set status [string equal [::dns::status $token] {ok}]]} {
			# Status wasn't "ok".  =(
			if {[testip $address]} {
				set ip [set hostname $address]
			} else {
				set ip {0.0.0.0}
				set hostname $address
			}
		} else {
			if {[testip $address]} {
				set ip $address
				if {[set hostname [lindex [::dns::name $token] 0]] == {} && [set hostname [lindex [::dns::address $token] end]] == {}} {
					set hostname $address
					set status 0
				}
			} else {
				set hostname $address
				if {[set ip [lindex [::dns::address $token] 0]] == {}} {
					set ip {0.0.0.0}
					if {[set hostname [lindex [::dns::name $token] 0]] == {}} { set hostname $address }
					set status 0
				}
			}
		}
		::dns::cleanup $token
		after idle [list $proc $ip $hostname $status {*}$args]
	}
} elseif {![catch { package require Tclx }] && [info commands host_info] != {}} {
	putlog {Using TclX for [dnslookup].  (non-asynchronous.)}
	proc ::tcldrop::dns::dnslookup {address proc args} {
		if {[testip $address]} {
			if {[catch { host_info official_name $address } official_name]} {
				after idle [list $proc $address $address 0 {*}$args]
			} else {
				after idle [list $proc $address $official_name 1 {*}$args]
			}
		} else {
			if {[catch { host_info addresses $address } addresses]} {
				after idle [list $proc {0.0.0.0} $address 0 {*}$args]
			} else {
				after idle [list $proc [lindex $addresses 0] $address 1 {*}$args]
			}
		}
	}
} elseif {[set ::tcldrop::dns::hostexe [auto_execok host]] != {}} {
	# Note: This has only been tested on Debian Linux.
	putlog "Using $::tcldrop::dns::hostexe for \[dnslookup\].  (asynchronous)"
	proc ::tcldrop::dns::dnslookup {address proc args} {
		variable hostexe
		set wanthostname [testip $address]
		if {![catch { open "|$hostexe $address" r } id]} {
			fconfigure $id -blocking 0 -buffering line
			fileevent $id readable [list ::tcldrop::dns::hostRead $id $wanthostname $address $proc $args]
		} else {
			if {$wanthostname} {
				after idle [list $proc $address $address 0 {*}$arg]
			} else {
				after idle [list $proc {0.0.0.0} $address 0 {*}$arg]
			}
		}
	}
	proc ::tcldrop::dns::hostRead {id wanthostname address proc arg} {
		if {[gets $id line] < 1} {
			close $id
			if {$wanthostname} {
				after idle [list $proc $address $address 0 {*}$arg]
			} else {
				after idle [list $proc {0.0.0.0} $address 0 {*}$arg]
			}
		} else {
			close $id
			if {!$wanthostname} {
				if {[testip [set ip [lindex [split $line] end]]]} {
					after idle [list $proc $ip $address 1 {*}$arg]
				} else {
					after idle [list $proc {0.0.0.0} $address 0 {*}$arg]
				}
			} else {
				if {![string match {* not found*} $line]} {
					after idle [list $proc $address [string trimright [lindex [split $line] end] .] 1 {*}$arg]
				} else {
					after idle [list $proc $address $address 0 {*}$arg]
				}
			}
		}
	}
} else {
	putlog {WARNING: Please install TclX or the dns tcllib package for [dnslookup] to work.}
	proc ::tcldrop::dns::dnslookup {address proc args} {
		if {[testip $address]} { set hostname [set ip $address] } else {
			set ip {0.0.0.0}
			set hostname $address
		}
		after idle [list $proc $ip $hostname 0 {*}$args]
	}
}
