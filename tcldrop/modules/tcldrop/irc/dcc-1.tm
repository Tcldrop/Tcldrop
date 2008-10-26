# irc/dcc --
#	Handles:
#		* All IRC related DCC binds.
#	Depends: irc.
#
# $Id$
#
# Copyright (C) 2005,2006,2007,2008 Tcldrop Development Team <Tcldrop-Dev>
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

namespace eval ::tcldrop::irc::dcc {
	variable version {0.1}
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	variable name {irc::dcc}
	package provide tcldrop::$name $version
	# This makes sure we're loading from a tcldrop environment:
	if {![info exists ::tcldrop]} { return }
	variable depends {irc channels core::dcc core}
	variable author {Tcldrop-Dev}
	variable description {All IRC related DCC binds.}
	variable rcsid {$Id$}
}

# FixMe: .status is not part of the irc module in eggdrop, move to core?
proc ::tcldrop::irc::dcc::STATUS {handle idx text} {
	if {${::server-online}} {
		putdcc $idx "    Online as: $::botname ($::realname)"
		putdcc $idx "    Server: $::serveraddress (connected for [duration [expr { [clock seconds] - ${::server-online} }]])"
		foreach chan [channels] {
			if {[botonchan $chan]} {
				set chansets [list]
				if {[botisop $chan]} {
					if {[set chanmode [channel get $chan chanmode]] != {}} { lappend chansets "enforcing \"$chanmode\"" }
					if {[channel get $chan autoop]} { lappend chansets {+autoop} }
					if {[channel get $chan autovoice]} { lappend chansets {+autovoice} }
					if {[channel get $chan bitch]} { lappend chansets {+bitch} }
					if {[channel get $chan enforcebans]} { lappend chansets {+enforcebans} }
					if {[channel get $chan protectops]} { lappend chansets {+protectops} }
					if {[channel get $chan protectfriends]} { lappend chansets {+protectfriends} }
					if {[channel get $chan nodesynch]} { lappend chansets {+nodesynch} }
				}
				if {[channel get $chan revenge]} { lappend chansets {+revenge} }
				if {[channel get $chan revengebot]} { lappend chansets {+revengebot} }
				if {[channel get $chan greet]} { lappend chansets {+greet} }
				if {[channel get $chan seen]} { lappend chansets {+seen} }
				if {[llength $chansets] == 0} { lappend chansets {(lurking)} }
				set status "[llength [chanlist $chan]] members, [join $chansets {, }]"
			} elseif {[channel get $chan inactive]} {
				set status {Channel is set +inactive}
			} elseif {[ischanjuped $chan]} {
				set status {Juped!}
			} else {
				set status {(not on channel)}
			}
			putdcc $idx "    [format {%-20.32s %-2.2s %-40.40s} $chan : $status]"
		}
	} else {
		putdcc $idx "    Server: $::serveraddress (trying to connect)"
		putdcc $idx "    Channels: [join [channels] {, }]"
	}
	return 0
}

# Usage: act [channel] <action>
# FixMe: Add support for console channel once ::idxlist is a dict.
proc ::tcldrop::irc::dcc::ACT {handle idx text} {
	set chan [lindex [split $text] 0]
	set action [lrange [split $text] 1 end]
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: act \[channel\] <action>"
		return 0
	}
	putcmdlog "#$handle# ($chan) act $action"
	if {![botonchan $chan]} {
		putdcc $idx "Cannot act to ${chan}: I'm not on that channel."
		return 0
	# channel is moderated
	} elseif {[string match *m* [getchanmode $chan]]}
		putdcc $idx "[lang 0x001]: act \[channel\] <action>"
		return 0
	} else {
		puthelp "PRIVMSG $chan :\001ACTION ${action}\001"
		putdcc $idx "Action to ${chan}: $action"
		return 1
	}
}

# Usage: say [channel] <message>
# FixMe: Add support for console channel once ::idxlist is a dict.
proc ::tcldrop::irc::dcc::SAY {handle idx text} {
	set chan [lindex [split $text] 0]
	set message [lrange [split $text] 1 end]
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: say \[channel\] <message>"
		return 0
	}
	putcmdlog "#$handle# ($chan) say $message"
	if {![botonchan $chan]} {
		putdcc $idx "Cannot say to ${chan}: I'm not on that channel."
		return 0
	# channel is moderated
	} elseif {[string match *m* [getchanmode $chan]]}
		putdcc $idx "[lang 0x001]: act \[channel\] <action>"
		return 0
	} else {
		puthelp "PRIVMSG $chan :$message"
		putdcc $idx "Said to ${chan}: $message"
		return 1
	}
}

# Usage: msg <nick> <message>
proc ::tcldrop::irc::dcc::MSG {handle idx text} {
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: msg <nick> <message>"
		return 0
	} else {
		set target [lindex [split $text] 0]
		set message [lrange [split $text] 1 end]
		putcmdlog "#$handle# msg $target $message"
		# we don't need to check if the nick is valid since .say is a lower access level than .msg
		puthelp "PRIVMSG $target :$message"
		putdcc $idx "Msg to ${target}: $message"
		return 1
	}
}

# Usage: op <nickname> [channel]
# FixMe: Add support for console channel once ::idxlist is a dict.
# FixMe: If all args are ommited, assume $victim = $handle
proc ::tcldrop::irc::dcc::OP {handle idx text} {
	lassign [split $text] victim chan
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: op <nickname> \[channel\]"
		return 0
	}
	putcmdlog "#$handle# ($chan) op $victim"
	if {![botonchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botisop $chan]} {
		# FixMe: check if we're a halfop that can set +o modes (pretty sure this would never happen but who knows with an error text like this)
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set +o modes."
		return 0
	# user is trying to op himself in a channel where he doesn't have access
	} elseif {![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "Your are not a channel op on $chan"
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on $chan"
		return 0
	# victim is being auto-deopped
	# we could check if the user is being auto-dehalfopped here as well, it might be useful and logical
	} elseif {[matchattr [nick2hand $victim $chan] d|d $chan]} {
		putdcc $idx "$victim is currently being auto-deopped."
		return 0
	# channel is +bitch and user is trying to op someone who doesn't have access
	} elseif {[channel get $chan bitch] && ![matchattr [nick2hand $victim $chan] nmo|nmo $chan]} {
		putdcc $idx "$victim is not a registered op."
		return 0
	} else {
		pushmode $chan +o $victim
		putdcc $idx "Gave op to $victim on ${chan}."
		return 1
	}
}

# Usage: deop <nickname> [channel]
# FixMe: Add support for console channel once ::idxlist is a dict.
proc ::tcldrop::irc::dcc::DEOP {handle idx text} {
	lassign [split $text] victim chan
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: deop <nickname> \[channel\]"
		return 0
	}
	putcmdlog "#$handle# ($chan) deop $victim"
	if {![botonchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botisop $chan]} {
		# FixMe: check if we're a halfop that can set -o modes (pretty sure this would never happen but who knows with an error text like this)
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set -o modes."
		return 0
	# user is trying to deop someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "Your are not a channel op on $chan"
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on $chan"
		return 0
	} elseif {[string equal -nocase $victim $::botnick]} {
		putdcc $idx "I'm not going to deop myself."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] n|n $chan] && ![matchattr $handle n|n $chan]}  {
		putdcc $idx "$victim is an owner for ${chan}."
		return 0
	# if ((chan_master(victim) || glob_master(victim)) && !(chan_owner(user) || glob_owner(user)))
	} elseif {[matchattr [nick2hand $victim $chan] m|m $chan] && ![matchattr $handle n|n] $chan}  {
		putdcc $idx "$victim is a master for ${chan}."
		return 0
	# if ((chan_op(victim) || (glob_op(victim) && !chan_deop(victim))) && !(chan_master(user) || glob_master(user))) {
	} elseif {[matchattr [nick2hand $victim $chan] o|o $chan] && ![matchattr [nick2hand $victim $chan] d|d $chan] && ![matchattr $handle nm|nm $chan]} {
		putdcc $idx "$victim has the op flag for ${chan}."
		return 0
	# eggdrop doesn't check for this but it seems like a good idea to me
	} elseif {![matchattr $handle nm] && [matchattr [nick2hand $victim $chan] b]} {
		putdcc $idx "I'm not going to deop fellow bots."
		return 0
	} else {
		pushmode $chan -o $victim
		putdcc $idx "Took op from $victim on ${chan}."
		return 1
}
# Usage: halfop <nickname> [channel]
# FixMe: Add support for console channel once ::idxlist is a dict.
# FixMe: If all args are ommited, assume $victim = $handle
proc ::tcldrop::irc::dcc::HALFOP {handle idx text} {
	lassign [split $text] victim chan
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: halfop <nickname> \[channel\]"
		return 0
	}
	putcmdlog "#$handle# ($chan) halfop $victim"
	if {![botonchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: check if we're a halfop who can't set +h. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set +h modes."
		return 0
	# user is not an op and is trying to halfop someone else
	} elseif {![matchattr $handle nmo|nmo $chan] && [nick2hand $victim $chan] ne $handle} {
		putdcc $idx "Your are not a channel op on $chan"
		return 0
	# user is trying to halfop himself in a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "Your are not a channel op or halfop on $chan"
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on $chan"
		return 0
	# victim is being auto-dehalfopped
	} elseif {[matchattr [nick2hand $victim $chan] l|l $chan]} {
		putdcc $idx "$victim is currently being auto-dehalfopped."
		return 0
	# channel is +bitch and user is trying to halfop someone who doesn't have access
	} elseif {[channel get $chan bitch] && ![matchattr [nick2hand $victim $chan] nmol|nmol $chan]} {
		putdcc $idx "$victim is not a registered halfop."
		return 0
	} else {
		pushmode $chan +h $victim
		putdcc $idx "Gave halfop to $victim on ${chan}."
		return 1
	}
}

# Usage: dehalfop <nickname> [channel]
# FixMe: Add support for console channel once ::idxlist is a dict.
proc ::tcldrop::irc::dcc::DEHALFOP {handle idx text} {
	lassign [split $text] victim chan
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: dehalfop <nickname> \[channel\]"
		return 0
	}
	putcmdlog "#$handle# ($chan) dehalfop $victim"
	if {![botonchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: check if we're a halfop that can't set -h modes. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set -h modes."
		return 0
	# user is trying to dehalfop someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "Your are not a halfop on $chan"
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on $chan"
		return 0
	} elseif {[string equal -nocase $victim $::botnick]} {
		putdcc $idx "I'm not going to dehalfop myself."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] n|n $chan] && ![matchattr $handle n|n $chan]}  {
		putdcc $idx "$victim is an owner for ${chan}."
		return 0
	# if ((chan_master(victim) || glob_master(victim)) && !(chan_owner(user) || glob_owner(user)))
	} elseif {[matchattr [nick2hand $victim $chan] m|m $chan] && ![matchattr $handle n|n $chan]}  {
		putdcc $idx "$victim is a master for ${chan}."
		return 0
	# if ((chan_op(victim) || (glob_op(victim) && !chan_deop(victim))) && !(chan_master(user) || glob_master(user))) {
	} elseif {[matchattr [nick2hand $victim $chan] o|o $chan] && ![matchattr [nick2hand $victim $chan] d|d $chan] && ![matchattr $handle nm|nm $chan]} {
		putdcc $idx "$victim has the op flag for ${chan}."
		return 0
	# if ((chan_halfop(victim) || (glob_halfop(victim) && !chan_dehalfop(victim))) && !(chan_master(user) || glob_master(user)))
	} elseif {[matchattr [nick2hand $victim $chan] l|l $chan] && ![matchattr [nick2hand $victim $chan] d|d $chan] && ![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "$victim has the halfop flag for ${chan}."
		return 0
	# eggdrop doesn't check for this but it seems like a good idea to me
	} elseif {![matchattr $handle nm] && [matchattr [nick2hand $victim $chan] b]} {
		putdcc $idx "I'm not going to dehalfop fellow bots."
		return 0
	} else {
		pushmode $chan -h $victim
		putdcc $idx "Took halfop from $victim on ${chan}."
		return 1
	}
}

# Usage: voice <nickname> [channel]
# FixMe: Add support for console channel once ::idxlist is a dict.
proc ::tcldrop::irc::dcc::VOICE {handle idx text} {
	lassign [split $text] victim chan
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: voice <nickname> \[channel\]"
		return 0
	}
	putcmdlog "#$handle# ($chan) voice $victim"
	if {![botonchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: Check if we're a halfop who can't set +v. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set +v modes."
		return 0
	# user is not an op or halfop and is trying to voice someone else
	} elseif {![matchattr $handle nmol|nmol $chan] && [nick2hand $victim $chan] ne $handle} {
		putdcc $idx "Your are not a channel op or halfop on $chan"
		return 0
	# user is trying to voice someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmolv|nmolv $chan]} {
		putdcc $idx "Your do not have the voice flag for $chan"
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on $chan"
		return 0
	} else {
		pushmode $chan +v $victim
		putdcc $idx "Gave voice to $victim on ${chan}."
		return 1
	}
}

# Usage: devoice <nickname> [channel]
# FixMe: Add support for console channel once ::idxlist is a dict.
proc ::tcldrop::irc::dcc::DEVOICE {handle idx text} {
	lassign [split $text] victim chan
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: devoice <nickname> \[channel\]"
		return 0
	}
	putcmdlog "#$handle# ($chan) devoice $victim"
	if {![botonchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: check if we're a halfop that can't set -v modes. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set -v modes."
		return 0
	# user is trying to devoice someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "Your are not a channel op or halfop on $chan"
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on $chan"
		return 0
	} elseif {[string equal -nocase $victim $::botnick]} {
		putdcc $idx "I'm not going to devoice myself."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] n|n $chan] && ![matchattr $handle n|n $chan]}  {
		putdcc $idx "$victim is an owner for ${chan}."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] m|m $chan] && ![matchattr $handle n|n $chan]}  {
		putdcc $idx "$victim is a master for ${chan}."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] o|o $chan] && ![matchattr $handle nm|nm $chan]} {
		putdcc $idx "$victim has the op flag for ${chan}."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] l|l $chan] && ![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "$victim has the halfop flag for ${chan}."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] v|v $chan] && ![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "$victim has the voice flag for ${chan}."
		return 0
	} elseif {![matchattr $handle nm] && [matchattr [nick2hand $victim $chan] b]} {
		putdcc $idx "I'm not going to devoice fellow bots."
		return 0
	} else {
		pushmode $chan -v $victim
		putdcc $idx "Took voice from $victim on ${chan}."
		return 1
	}
}





bind load - irc::dcc ::tcldrop::irc::dcc::LOAD -priority 10
proc ::tcldrop::irc::dcc::LOAD {module} {
	bind dcc nmo|nmo act ::tcldrop::irc::dcc::ACT -priority 1000
	bind dcc nmo|nmo say ::tcldrop::irc::dcc::SAY -priority 1000
	# FixMe: Eggdrop binds .msg as o|- but do we want this? Could possibly be abused on networks with services
	bind dcc nmo msg ::tcldrop::irc::dcc::MSG -priority 1000
	bind dcc nmo|nmo op ::tcldrop::irc::dcc::OP -priority 1000
	bind dcc nmo|nmo deop ::tcldrop::irc::dcc::DEOP -priority 1000
	bind dcc nmol|nmol halfop ::tcldrop::irc::dcc::HALFOP -priority 1000
	bind dcc nmol|nmol dehalfop ::tcldrop::irc::dcc::DEHALFOP -priority 1000
	bind dcc nmolv|nmolv voice ::tcldrop::irc::dcc::VOICE -priority 1000
	bind dcc nmolv|nmolv devoice ::tcldrop::irc::dcc::DEVOICE -priority 1000
	# FixMe: What's with these weird priorities?
	bind dcc n status ::tcldrop::irc::dcc::STATUS -priority 3
	bind dcc n stat ::tcldrop::irc::dcc::STATUS -priority 3
	loadhelp irc.help
	bind unld - irc::dcc ::tcldrop::irc::dcc::UNLD -priority 10
}

proc ::tcldrop::irc::dcc::UNLD {module} {
	unbind dcc * * ::tcldrop::irc::dcc::*
	unloadhelp irc.help
	return 0
}
