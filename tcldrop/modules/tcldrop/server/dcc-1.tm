# server/dcc --
#	Handles:
#		* Server related DCC commands.
#	Depends: server.
#
# $Id$
#
# Copyright (C) 2005,2006,2007 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.

namespace eval ::tcldrop::server::dcc {
	variable name {server::dcc}
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable predepends {server}
	variable depends {core::conn core}
	variable author {Tcldrop-Dev}
	variable description {Server related DCC commands.}
	variable rcsid {$Id$}
	variable commands [list]
}

proc ::tcldrop::server::dcc::JUMP {handle idx text} {
	lassign $text server port password
	set CmdLog ""
	if {$server ne {}} {
		append CmdLog " $server"
		if {$port ne {}} {
			append CmdLog " $port"
			if {$password ne {}} {
				append CmdLog " ..."
			}
		}
	}
	putcmdlog "#$handle# jump${CmdLog}"
	# Jumping servers...
	putdcc $idx [lang 0x62b]
	# there's no danger in calling jump with empty args
	jump $server $port $password
	return 0
}

proc ::tcldrop::server::dcc::DUMP {handle idx text} {
	putcmdlog "#$handle# dump $text"
	putserv $text
	return 0
}

proc ::tcldrop::server::dcc::SERVERS {handle idx text} {
	putcmdlog "#$handle# servers"
	putdcc $idx "[mc_handle $handle {Server list}]:"
	# FixMe: Add support for showing which server in the list we're at when ::idxlist gets turned into a dict
	foreach server $::servers {
		putdcc $idx "  $server"
	}
	putdcc $idx "[mc_handle $handle {End of server list.}]"
	return 0
}

bind load - server::dcc ::tcldrop::server::dcc::LOAD -priority 0
proc ::tcldrop::server::dcc::LOAD {module} {
	bind dcc nm jump ::tcldrop::server::dcc::JUMP -priority 1000
	bind dcc nm dump ::tcldrop::server::dcc::DUMP -priority 1000
	bind dcc nmo servers ::tcldrop::server::dcc::SERVERS -priority 1000
	bind unld - server::dcc ::tcldrop::server::dcc::UNLD -priority 0
	loadhelp server.help
	loadhelp [file join set server.help]
}

proc ::tcldrop::server::dcc::UNLD {module} {
	unbind dcc * * ::tcldrop::server::dcc::*
	unloadhelp server.help
	unloadhelp [file join set server.help]
	return 0
}
