# transfer.tcl --
#
# $Id$
#
# Copyright (C) 2004 phrek <ephrek@gmail.com>
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
# The author of this project can be reached at Tcldrop-Dev@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.

#
#	transfer module for Tcldrop.  (OPTIONAL)
#	Depends on: none.
#
#	The transfer module provides dcc send/get support and userfile transfer
#	support for userfile sharing.
#

#TODO:
#	??userfile sharing??
#	update the idx type during dccsends (ie GET_PENDING)
#	virtual file stuff (look up the virtual file and send the dcc commands to other bots)
#	firewall/natip/proxy stuff
#	tcl bindings for sent, rcvd, lost, tout
#	storing files based on where a user is in their current pwd (if they are in the filesys)

namespace eval ::tcldrop::transfer {
	variable version {0.1}
	variable name {transfer}
	variable depends {core}
	variable author {Tcldrop-Dev}
	variable description {Provides dcc send/get support and userfile transfer support for eggdrop userfile sharing.}
	variable script [info script]
	variable commands [list dccsend getfilesendtime getfileq]
	variable rcsid {$Id$}
	package provide tcldrop::$name $version
	# Export all the commands that should be available to 3rd-party scripters:
	eval namespace export $commands
}

#  dccsend <filename> <ircnick> ["resend"] [callback] [nickofuserwhocalledcommand]
#    Description: attempts to start a dcc file transfer to the given nick;
#      the filename must be specified either by full pathname or in relation
#      to the bot's startup directory
#    Returns:
#      0 success
#      1 the dcc table is full (too many connections)
#      2 can't open a socket for the transfer
#      3 the file doesn't exist
#      4 the file was queued for later transfer, which means that person has
#        too many file transfers going right now
#      5 copy-to-tmp is enabled and the file already exists in the temp
#        directory
#    Module: transfer
#
#	Note: differs from the eggdrop dccsend since it allows an optional protocol argument
#	that must be "resend" (or else resuming is ignored) to send a DCC RESEND instead
#	(used also by file resend of the filesys module)
#	It ALSO allows the specification of a callback procedure to call (specify "0" for the protocol argument if you do not wish a resend)
#	if the transfer fails/succeeds to which it passes four args: filename nick starttime completed
#		file is be the filename of the file that was sent
#		nick the nick it was sent to
#		starttime being the unixtime from when the transfer was started
#		completed being 0 or 1, 0 (completion of the transfer failed) or 1 (the transfer was successfull)
#	Also the nickofuserwhocalledcommand uses the botnick if it is a script (with no calling nick)

proc ::tcldrop::transfer::dccsend {filename nick {protocol ""} {callback ""} {callingnick ""}} {
	variable Transfers
	variable Queue
	if {[string equal $callingnick ""]} {
		set callingnick ${::botnet-nick}
	}
	if {![file exists $filename] || ![file readable $filename]} {
		after idle [list ::tcldrop::transfer::Updatequeue $callingnick]
		return 3
	} elseif {[dccused] >= ${::max-dcc}} {
		after idle [list ::tcldrop::transfer::Updatequeue $callingnick]
		return 1
	} elseif {${::copy-to-tmp} == 1 && !(![file exists "[file separator]tmp[file separator]"] || [file exists "[file separator]tmp[file separator][file tail $filename]"])} {
		after idle [list ::tcldrop::transfer::Updatequeue $callingnick]
		return 5
	} elseif {[::tcldrop::transfer::Getnumberoftransfers $callingnick] >= ${::max-dloads}} {
		#queue the file
		#nickwhoqueuedthefile {{{/full/path/to/file.ext} {nicktosendthefileto} {filequeuetime}} {{/full/path/to/file.ext} {nicktosendthefileto} {filequeuetime}}}
		if {[string equal [array names Queue $nick] ""]} {
			#no files queued so create one for nick
			array set Queue "$callingnick [list [list [list $filename $nick $protocol $callback [clock seconds]]]]"
		} else {
			#ugly split up like this but very necessary
			set tmplist [lindex [array get Queue $callingnick] 1]
			lappend tmplist [list $filename $nick $protocol $callback [clock seconds]]
			array set Queue [list $tmplist]
		}
		return 4
	} else {
		#setup the sock
		if {[info exists {::nat-ip}]} {
			set myip ${::nat-ip}
		}
		if {[info exists ::firewall]} {
			set myip [string trimleft [lindex [split $::firewall ":"] 0] "!"]
			set myport [lindex [split $::firewall ":"] 1]
		}
		if {[info exists {::reserved-portrange}]} {
			set fport [lindex [split ${::reserved-portrange} ":"] 0]
			set lport [lindex [split ${::reserved-portrange} ":"] 1]
			set found 0
			#register dcc idx
			set idx [assignidx]
			registeridx $idx idx $idx sock * handle $callingnick remote irc type "OPENING_SOCK" other "dcc send" timestamp [clock seconds]
			#add to Transfers
			array set Transfers "$idx [list [list $filename $callingnick $nick [clock seconds]]]"
			#find open port
			for { set port $fport } {$port < $lport } {incr port } {
				if {![catch {socket -server "::tcldrop::transfer::Acceptsend$protocol $callingnick [list $filename] $idx 0" $port} sock]} {
					set found 1
					break
				}
			}
			if {$found == 0} {
				#could not use open a port within the specified range
				#unregister idx and delete from Transfers
				unregisteridx $idx
				array unset Transfers $idx
				after idle [list ::tcldrop::transfer::Updatequeue $callingnick]
				return 2
			} else {
				idxinfo $idx sock $sock type "GET_PENDING"
				#TODO: MUST INCLUDE ESCAPE SEQUENCE PROC TO ESCAPE ILLEGAL CHARS according to http://www.irchelp.org/irchelp/rfc/ctcpspec.html
				#TODO: MUST ALSO MAKE SURE USES LONG FORM OF THE NAT/FIREWALL IP TO USE
				timeout start $sock -callback [list ::tcldrop::transfer::Dccsendtimeout $nick $idx {} $sock] -timeout [expr ${::xfer-timeout} * 1000]
				if {[string equal $protocol "resend"]} {
					puthelp "PRIVMSG $nick :\001DCC RESEND [file tail $filename] [myip] [lindex [fconfigure $sock -sockname] end] [file size $filename]\001"
				} else {
					puthelp "PRIVMSG $nick :\001DCC SEND [file tail $filename] [myip] [lindex [fconfigure $sock -sockname] end] [file size $filename]\001"
				}
			}
		} else {
			#register dcc idx
			set idx [assignidx]
			registeridx $idx idx $idx sock * handle $callingnick remote irc type "OPENING_SOCK" other "dcc send" timestamp [clock seconds] callback $callback
			#add to Transfers
			array set Transfers "$idx [list [list $filename $callingnick $nick [clock seconds]]]"
			if {[catch {socket -server "::tcldrop::transfer::Acceptsend$protocol $callingnick [list $filename] $idx 0" 0} sock]} {
				#could not use open a port
				#unregister idx and delete from Transfers
				unregisteridx $idx
				array unset Transfers $idx
				after idle [list ::tcldrop::transfer::Updatequeue $callingnick]
				return 2
			} else {
				idxinfo $idx sock $sock type "GET_PENDING"
				#TODO: MUST INCLUDE ESCAPE SEQUENCE PROC TO ESCAPE ILLEGAL CHARS according to http://www.irchelp.org/irchelp/rfc/ctcpspec.html
				#TODO: MUST ALSO MAKE SURE USES LONG FORM OF THE NAT/FIREWALL IP TO USE
				timeout start $sock -callback [list ::tcldrop::transfer::Dccsendtimeout $nick $idx {} $sock] -timeout [expr ${::xfer-timeout} * 1000]
				if {[string equal $protocol "resend"]} {
					puthelp "PRIVMSG $nick :\001DCC RESEND [file tail $filename] [myip] [lindex [fconfigure $sock -sockname] end] [file size $filename]\001"
				} else {
					puthelp "PRIVMSG $nick :\001DCC SEND [file tail $filename] [myip] [lindex [fconfigure $sock -sockname] end] [file size $filename]\001"
				}
			}
		}
	}
}

#used in dccsend to check if the user is already past his limit (NOT EXPORTED)
proc ::tcldrop::transfer::Getnumberoftransfers {nick} {
	variable Transfers
	set t [array get Transfers]
	set count 0
	for {set i 1} {$i < [llength $t]} {incr i 2} {
		if {[string equal $nick [lindex $t $i 1]]} {
			incr count
		}
	}
	return $count
}

#used to kill idx, close sock, and remove idx from Transfer (NOT EXPORTED)
proc ::tcldrop::transfer::Killandremove {idx} {
	killidx $idx
	array unset ::tcldrop::transfer::Transfers $idx
}

#used by dccsend to handle the reget packet for the resend protocol (waiting on the user)
proc ::tcldrop::transfer::Acceptsendresend {nick filename idx sock host port offset} {
	if {![eof $sock] && ![catch {read $sock} offset]} {
		if {[string length $offset] == 8} {
			if {[string range $offset 0 1] == "\xfe\xab"} {
				#big endian
				binary scan [string range $offset 4 7] I1 offset
				::tcldrop::transfer::Acceptsend $nick $filename $idx $sock $host $port $offset
			} elseif {[string range $offset 0 1] == "\xab\xfe"} {
				#little endian
				binary scan [string range $offset 4 7] i1 offset
				::tcldrop::transfer::Acceptsend $nick $filename $idx $sock $host $port $offset
			}
		}
	}
}

#used by dccsend to handle the initial incoming sock for the beginning of a dcc transfer (NOT EXPORTED)
proc ::tcldrop::transfer::Acceptsend {nick filename idx sock host port offset} {
	variable Transfers
	#kill the timeout timer for the passive socket
	#kill the passive socket
	array set temp [idxinfo $idx]
	timeout cancel $temp(sock)
	idxinfo $idx sock $sock
	catch {close $temp(sock)}
	array unset temp
	if {${::copy-to-tmp} == 1} {
		#TODO: consider revising later
		if {[catch {file copy $filename "[file separator]tmp[file separator]"} err]} {
			#abort, couldn't copy to temp!
			set temp [array get Transfers $idx]
			::tcldrop::transfer::Killandremove $idx
			after idle [list ::tcldrop::transfer::Updatequeue $nick]
			#call callback proc if there exists one
			array set idxinfo $::idxlist($idx)
			if {$idxinfo(callback) != ""} {
				after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
			}
			return 0
		}
		if {[catch {open "[file separator]tmp[file separator]" r} fid]} {
			#abort, could not open file that was just copied
			set temp [array get Transfers $idx]
			::tcldrop::transfer::Killandremove $idx
			after idle [list ::tcldrop::transfer::Updatequeue $nick]
			#call callback proc if there exists one
			array set idxinfo $::idxlist($idx)
			if {$idxinfo(callback) != ""} {
				after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
			}
			return 0
		}
	} else {
		if {[catch {open $filename r} fid]} {
			#abort, could not open file
			set temp [array get Transfers $idx]
			::tcldrop::transfer::Killandremove $idx
			after idle [list ::tcldrop::transfer::Updatequeue $nick]
			#call callback proc if there exists one
			array set idxinfo $::idxlist($idx)
			if {$idxinfo(callback) != ""} {
				after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
			}
			catch {close $fid}
			return 0
		}
	}
	fconfigure $fid -translation binary
	seek $fid $offset
	if {${::dcc-block} == 0} {
		#read entire file
		if {[catch {set buf [read $fid]} err]} {
			#abort, could not read from file
			set temp [array get Transfers $idx]
			::tcldrop::transfer::Killandremove $idx
			after idle [list ::tcldrop::transfer::Updatequeue $nick]
			#call callback proc if there exists one
			array set idxinfo $::idxlist($idx)
			if {$idxinfo(callback) != ""} {
				after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
			}
			catch {close $fid}
			return 0
		}
	} else {
		#read first block
		if {[catch {set buf [read $fid ${::dcc-block}]} err]} {
			#abort, could not read from file
			set temp [array get Transfers $idx]
			::tcldrop::transfer::Killandremove $idx
			after idle [list ::tcldrop::transfer::Updatequeue $nick]
			#call callback proc if there exists one
			array set idxinfo $::idxlist($idx)
			if {$idxinfo(callback) != ""} {
				after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
			}
			catch {close $fid}
			return 0
		}
	}
	fconfigure $sock -blocking 0 -buffering none -translation binary
	if {[catch {puts -nonewline $sock $buf} err]} {
		#could not write to sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
		}
		catch {close $fid}
		return 0
	} else {
		#setup the transfer timeout timer (use the sockid as the timeout id)
		timeout start $sock -callback [list ::tcldrop::transfer::Dccsendtimeout $nick $idx $fid $sock] -timeout [expr ${::xfer-timeout} * 1000]

		#setup the rest of the data transfer's callback
		fileevent $sock readable "::tcldrop::transfer::Dccsendcallback $nick $idx $fid $sock"
		return 1
	}
}

#callback for future fileblocks (NOT EXPORTED)
proc ::tcldrop::transfer::Dccsendcallback {nick idx fid sock} {
	variable Transfers
	timeout reset $sock
	#split up for a reason like this
	if {[eof $sock]} {
		#transfer failed
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
		}
		catch {close $fid}
		return 0
	} elseif {[catch {read $sock 4} len]} {
		#transfer failed (read error)
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
		}
		catch {close $fid}
		return 0
	} elseif {[string length $len] == 0} {
		#transfer failed (sync error)
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
		}
		catch {close $fid}
		return 0
	}
	#entire blocking of dcc protocol rather redundant sort of
	set rcvd 0
	binary scan $len I1 rcvd
	set pos [tell $fid]
	if {[eof $fid] && $pos == $rcvd} {
		success reached eof
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 1]
		}
		catch {close $fid}
		timeout reset $sock
		return 1
	} elseif {$pos == $rcvd && [catch {set buf [read $fid ${::dcc-block}]} err]} {
		#failed, could not read from file
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
		}
		catch {close $fid}
		return 0
	} elseif {$pos == $rcvd && [set red [string length $buf]] == 0} {
		#success, finished at the end of the file
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 1]
		}
		catch {close $fid}
		return 1
	} elseif {$pos == $rcvd && [catch {puts -nonewline $sock $buf} err]} {
		#could not write to sock
		timeout cancel $sock
		set temp [array get Transfers $idx]
		::tcldrop::transfer::Killandremove $idx
		after idle [list ::tcldrop::transfer::Updatequeue $nick]
		#call callback proc if there exists one
		array set idxinfo $::idxlist($idx)
		if {$idxinfo(callback) != ""} {
			after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
		}
		catch {close $fid}
		return 0
	}
	# finished for now -- reset timer to 0
	timeout reset $sock
}

#handle transfer timeouts (NOT EXPORTED)
proc ::tcldrop::transfer::Dccsendtimeout {nick idx fid sock} {
	#the send timed out!
	timeout cancel $sock
	variable Transfers
	set temp [array get Transfers $idx]
	::tcldrop::transfer::Killandremove $idx
	#update the queue if theres something queued for $nick
	after idle [list ::tcldrop::transfer::Updatequeue $nick]
	#call callback proc if there exists one
	array set idxinfo $::idxlist($idx)
	if {$idxinfo(callback) != ""} {
		after idle [list $idxinfo(callback) [lindex $temp 1 0] [lindex $temp 1 2] [lindex $temp 1 3] 0]
	}
	catch {close $fid}
	return 0
}

#Update queue (NOT EXPORTED)
proc ::tcldrop::transfer::Updatequeue {nick} {
	#a transfer initiated by $nick finished so check if any queues for $nick still and if so update queue list
	variable Queue
	#get queue list for $nick to temp
	set temp [lindex [array get Queue $nick] 1]
	if {$temp == ""} {
		#nothing in the queue
		return 0
	}
	if {[llength $temp] == 1} {
		#only one transfer queued
		#remove queue for $nick
		array unset Queue $nick
	} else {
		#more than one transfer queued
		#remove specific queue for $nick
		set temp [lrange $temp 1 end]
		array set Queue "$nick [list $temp]"
	}
	#get the first queue (and maybe only queue)
	#{filename nick {protocol ""} {callback ""} {callingnick ""}} {
	#queue format :nickwhoqueuedthefile {{{/full/path/to/file.ext} {nicktosendthefileto} {protocol} {callback} {filequeuetime}}} {{/full/path/to/file.ext} {nicktosendthefileto} {protocol} {callback} {filequeuetime}}}
	set temp [lindex $temp 0]
	#recall the dccsend to set everything up
	::tcldrop::transfer::dccsend [lindex $temp 0] [lindex $temp 1] [lindex $temp 2] [lindex $temp 3] $nick
	return 1
}

#  getfilesendtime <idx>
#    Returns: the unixtime value from when a file transfer started, or a
#      negative number:
#        -1 no matching transfer with the specified idx was found
#        -2 the idx matches an entry which is not a file transfer
#    Module: transfer
proc ::tcldrop::transfer::getfilesendtime {idx} {
	if {![valididx $idx]} {
		#no matching idx
		return -1
	}
	set idxinfo [idxinfo $idx]
	if {[string equal [lindex $idxinfo(other) 0] "dcc"]} {
		#it is a dcc
		return $idxinfo(timestamp)
	}
	#it is not a dcc transfer
	return -2
}

#  getfileq <handle>
#    Returns: list of files queued by someone; each item in the list will be
#      a sublist with two elements: nickname the file is being sent to and
#      the filename
#    Module: transfer
proc ::tcldrop::transfer::getfileq {handle} {
	variable Transfer
	#transfer format: idx {{/full/path/to/file.ext} {nickwhoqueuedthefile} {nicktosendthefileto} {filetransferstarttime}}
	set filelist ""
	set tlist [array get Transfer]
	for {set i 1} {$i < [llength $tlist]} {incr i 2} {
		if {[string equal [lindex $tlist $i 1] $handle]} {
			#matched handle
			lappend filelist [list [lindex $tlist $i 2] [lindex $tlist $i 0]]
		}
	}
	return $filelist
}

#DCC get request handling (NOT EXPORTED)
bind ctcp - "DCC" ::tcldrop::transfer::Getdccrequest -priority 1000
proc ::tcldrop::transfer::Getdccrequest {nick mask hand dest keyword text} {
	if {[string equal -nocase [lindex $text 0] "send"] || [string equal -nocase [lindex $text 0] "resend"]} {
		if {![string is integer [lindex $text 3]] || ![string is integer [lindex $text 4]]} {
			#illegal port/size
			putxferlog "Refused DCC GET [lindex $text 1] (bad port/file size): $mask"
			putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
		} elseif {[lindex $text 4] > [expr {${::max-filesize} * 1024}] && ![string equal [lindex $text 4] 0]} {
			#filesize larger than allowed
			putxferlog "Refused DCC GET [lindex $text 1] (file size larger than allowed): $mask"
			putxferlog "CTCP DCC: SEND [lrange $text 1 4] from $nick ($mask)"
		} elseif {[dccused] >= ${::max-dcc}} {
			#too many dcc's active
			putxferlog "Refused DCC GET [lindex $text 1] (too many dcc's): $mask"
			putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
		} elseif {[lsearch -dictionary -exact [string tolower [userlist x]] [string tolower $nick]] != -1} {
			set filepath [file normalize "${::incoming-path}[file separator][lindex $text 1]"]
			#set size in bytes
			set size [expr {[lindex $text 4] * 1024}]
			if {[string first [file normalize ${::incoming-path}] $filepath] == 0} {
				if {![file isdirectory [file dirname $filepath]]} {
					putxferlog "Refused DCC GET [lindex $text 1] (incoming-path does not exist): $mask"
					putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
				} elseif {![file exists $filepath] || [string equal -nocase [lindex $text 0] "resend"]} {
					if {[string equal -nocase [lindex $text 0] "resend"]} {
						putxferlog "CTCP DCC: REGET [lrange $text 1 4] from $nick ($mask)"
						::tcldrop::transfer::Acceptget $filepath [lindex $text 2] [lindex $text 3] [lindex $text 4] $nick $hand 1
					} else {
						putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
						::tcldrop::transfer::Acceptget $filepath [lindex $text 2] [lindex $text 3] [lindex $text 4] $nick $hand 0
					}
				} else {
					#filename already exists
					putxferlog "Refused DCC GET [lindex $text 1] (file exists): $mask"
					putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
				}
			} else {
				#filename was illegal
				putxferlog "Refused DCC GET [lindex $text 1] (illegal filename): $mask"
				putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
			}
		} else {
			putxferlog "Refused DCC GET [lindex $text 1] (no access): $mask"
			putxferlog "CTCP DCC: GET [lrange $text 1 4] from $nick ($mask)"
		}
	}
	#always return 1 since this is internal processing
	return 1
}

#DCC get handling (NOT EXPORT)
proc ::tcldrop::transfer::Acceptget {filepath ip port size nick hand resend} {
	if {![catch {socket -async $ip $port} sock]} {
		if {[file exists $filepath]} {
			if {$resend == 1} {
				if {![catch {open $filepath a} fid]} {
					#setup timeout timer
					set idx [assignidx]
					timeout start $sock -callback [list ::tcldrop::transfer::Dccgettimeout $nick $idx {} $sock] -timeout [expr ${::xfer-timeout} * 1000]
					#register dcc idx
					registeridx $idx idx $idx sock $sock port $port handle $nick remote [decimal2ip $ip] type "DATA_TRANSFER" other "dcc get" timestamp [clock seconds]
					#configure file
					fconfigure $fid -translation binary
					#configure sock
					fconfigure $sock -buffering none -blocking 0 -translation binary
					#send the 8 byte resend packet
					puts -nonewline $sock "\xfe\xab\x00\x00[binary format I1 [tell $fid]]"
					fileevent $sock readable "::tcldrop::transfer::Dccgetcallback $sock $fid $size $nick $hand $idx $filepath [tell $fid]"
				}
			}
		} elseif {![catch {open $filepath w} fid]} {
			#setup timeout timer
			set idx [assignidx]
			timeout start $sock -callback [list ::tcldrop::transfer::Dccgettimeout $nick $idx {} $sock] -timeout [expr ${::xfer-timeout} * 1000]
			#register dcc idx
			registeridx $idx idx $idx sock $sock port $port handle $nick remote [decimal2ip $ip] type "DATA_TRANSFER" other "dcc get" timestamp [clock seconds]
			#configure file
			fconfigure $fid -translation binary
			#configure sock
			fconfigure $sock -buffering none -blocking 0 -translation binary
			fileevent $sock readable "::tcldrop::transfer::Dccgetcallback $sock $fid $size $nick $hand $idx $filepath 0"
		}
	}
}

#DCC get data handling (NOT EXPORTED)
proc ::tcldrop::transfer::Dccgetcallback {sock fid size nick hand idx filepath offset} {
	if {![eof $fid]} {
		timeout reset $sock
		if {![catch {set data [read $sock]} err]} {
			# do not check if its oversized unless its over the maximum size
			if {[tell $fid] < [expr {${::max-filesize} * 1024}]} {
				if {![catch {puts -nonewline $fid $data} err]} {
					if {![catch {puts -nonewline $sock [binary format I1 [tell $fid]]} err]} {
						#so far so good
						timeout reset $sock
						return
					} elseif {[tell $fid] == $size} {
						#finished supposedly
						close $fid
						killidx $idx
						putxferlog "CTCP DCC: COMPLETED GET [file tail $filepath] from $nick"
					} else {
						set pos [tell $fid]
						close $fid
						killidx $idx
						putxferlog "CTCP DCC: ERROR DURING GET [file tail $filepath] from $nick (could not write to sock: $err) (${pos}/$size)"
					}
				} else {
					#error writing the data to the file - abort
					set pos [tell $fid]
					close $fid
					killidx $idx
					putxferlog "CTCP DCC: ERROR DURING GET [file tail $filepath] from $nick (failed to write data to file: $err) (${pos}/$size)"
				}
			} else {
				#more data transferred than max filesize allowed
				close $fid
				killidx $idx
				putxferlog "CTCP DCC: ERROR IN GET [file tail $filepath] from $nick (more bytes transferred than allowed by max-filesize)"
			}
		}
		timeout cancel $sock
	} else {
		timeout cancel $sock
		killidx $idx
		catch {close $fid}
		if {[tell $fid] < $size} {
			#incomplete
			putxferlog "CTCP DCC: INCOMPLETE GET [file tail $filepath] from $nick (bytes transferred did not match given size)"
		} elseif {[tell $fid] > $size} {
			#error in transfer (more data transferred than promised somehow)
			putxferlog "CTCP DCC: ERROR IN GET [file tail $filepath] from $nick (more bytes transferred than promised)"
		} else {
			#complete
			putxferlog "CTCP DCC: COMPLETED GET [file tail $filepath] from $nick"
		}
	}
}

#DCC get timeout (NOT EXPORTED)
proc ::tcldrop::transfer::Dccgettimeout {nick idx fid sock} {
	#the send timed out!
	timeout cancel $sock
	killidx $idx
	#call callback proc if there exists one
	#checkbinds&callbackproc user scripting support is todo
	catch {close $fid}
	putxferlog "CTCP DCC: ERROR DURING GET [file tail $filepath] from $nick (could not write to sock: $err) (${pos}/$size)"
	return 0
}

#set default value for xfer-timeout because some pansy didn't bother setting it (NOT EXPORTED)
bind load - transfer ::tcldrop::transfer::LOAD -priority -100
proc ::tcldrop::transfer::LOAD {module} {
	# This variable will store every transfer in progress, in the form: {idx {{/full/path/to/file.ext} {nickwhoqueuedthefile} {nicktosendthefileto} {filetransferstarttime}}}
	variable Transfers
	array set Transfers {}
	# This variable will store every queued transfer, in the form: {nickwhoqueuedthefile {{{/full/path/to/file.ext} {nicktosendthefileto} {protocol} {callback} {filequeuetime}}} {{/full/path/to/file.ext} {nicktosendthefileto} {protocol} {callback} {filequeuetime}}}
	# can not use sub arrays since there could be conflicts
	variable Queue
	array set Queue {}
	setdefault xfer-timeout 120
	setdefault copy-to-tmp 0
	setdefault max-dloads 5
	setdefault dcc-block 8192
}
