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
proc ::tcldrop::irc::dcc::ACT {handle idx text} {
	global idxlist
	set args [split $text]
	if {$text eq {}} {
		putdcc $idx "[lang 0x001]: act \[channel\] <action>"
		return 0
	}
	if {![validchan [lindex $args 0]]} {
		set chan [dict get $idxlist($idx) console-channel]
		set action $text
	} else {
		set chan [lindex $args 0]
		set action [lrange $args 1 end]
	}
	putcmdlog "#$handle# ($chan) act $action"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "Cannot act to ${chan}: I'm not on that channel."
		return 0
	} elseif {[string match *m* [getchanmode $chan]]} {
		putdcc $idx "Cannot act to ${chan}: It is moderated."
		return 0
	} else {
		puthelp "PRIVMSG $chan :\001ACTION ${action}\001"
		putdcc $idx "Action to ${chan}: $action"
		return 1
	}
}

# Usage: say [channel] <message>
proc ::tcldrop::irc::dcc::SAY {handle idx text} {
	global idxlist
	set args [split $text]
	if {$text eq {}} {
		putdcc $idx "[lang 0x001]: say \[channel\] <message>"
		return 0
	}
	if {![validchan [lindex $args 0]]} {
		set chan [dict get $idxlist($idx) console-channel]
		set message $text
	} else {
		set chan [lindex $args 0]
		set message [lrange $args 1 end]
	}
	putcmdlog "#$handle# ($chan) say $message"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "Cannot say to ${chan}: I'm not on that channel."
		return 0
	# channel is moderated
	} elseif {[string match *m* [getchanmode $chan]]} {
		putdcc $idx "Cannot say to ${chan}: It is moderated."
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
proc ::tcldrop::irc::dcc::OP {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	# no args were supplied, assume victim = handle
	if {$text eq {}} {
		set victim [hand2nick $handle]
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) op $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	} elseif {![botisop $chan]} {
		# FixMe: check if we're a halfop that can set +o modes (pretty sure this would never happen but who knows with an error text like this)
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set +o modes."
		return 0
	# user is trying to op himself in a channel where he doesn't have access
	} elseif {![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "You are not a channel op on $chan"
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
proc ::tcldrop::irc::dcc::DEOP {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	if {$text eq {}} {
		putdcc $idx "[lang 0x001]: deop <nickname> \[channel\]"
		return 0
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) deop $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	} elseif {![botisop $chan]} {
		# FixMe: check if we're a halfop that can set -o modes (pretty sure this would never happen but who knows with an error text like this)
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set -o modes."
		return 0
	# user is trying to deop someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "You are not a channel op on $chan"
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
	} elseif {[matchattr [nick2hand $victim $chan] m|m $chan] && ![matchattr $handle n|n $chan]}  {
		putdcc $idx "$victim is a master for ${chan}."
		return 0
	# if ((chan_op(victim) || (glob_op(victim) && !chan_deop(victim))) && !(chan_master(user) || glob_master(user)))
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
}
# Usage: halfop <nickname> [channel]
proc ::tcldrop::irc::dcc::HALFOP {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	# no args were supplied, assume victim = handle
	if {$text eq {}} {
		set victim [hand2nick $handle]
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) halfop $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: check if we're a halfop who can't set +h. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set +h modes."
		return 0
	# user is not an op and is trying to halfop someone else
	} elseif {![matchattr $handle nmo|nmo $chan] && [nick2hand $victim $chan] ne $handle} {
		putdcc $idx "You are not a channel op on $chan"
		return 0
	# user is trying to halfop himself in a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "You are not a channel op or halfop on $chan"
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
proc ::tcldrop::irc::dcc::DEHALFOP {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	if {$text eq {}} {
		putdcc $idx "[lang 0x001]: dehalfop <nickname> \[channel\]"
		return 0
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) dehalfop $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: check if we're a halfop that can't set -h modes. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set -h modes."
		return 0
	# user is trying to dehalfop someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "You are not a halfop on $chan"
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
	# if ((chan_op(victim) || (glob_op(victim) && !chan_deop(victim))) && !(chan_master(user) || glob_master(user)))
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
proc ::tcldrop::irc::dcc::VOICE {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	# no args were supplied, assume victim = handle
	if {$text eq {}} {
		set victim [hand2nick $handle]
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) voice $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: Check if we're a halfop who can't set +v. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set +v modes."
		return 0
	# user is not an op or halfop and is trying to voice someone else
	} elseif {![matchattr $handle nmol|nmol $chan] && [nick2hand $victim $chan] ne $handle} {
		putdcc $idx "You are not a channel op or halfop on $chan"
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
proc ::tcldrop::irc::dcc::DEVOICE {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	if {$text eq {}} {
		putdcc $idx "[lang 0x001]: devoice <nickname> \[channel\]"
		return 0
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) devoice $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		# FixMe: check if we're a halfop that can't set -v modes. This worked on unrealircd, needs to be tested on others to verify but I doubt they're different.
		putdcc $idx "I can't help you now because I'm not a chan op or halfop on ${chan}, or halfops cannot set -v modes."
		return 0
	# user is trying to devoice someone in a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "You are not a channel op or halfop on $chan"
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

# Usage: channel [channel-name]
proc ::tcldrop::irc::dcc::CHANNEL {handle idx text} {
	global idxlist nicklen handlen botnick
	if {$text eq {}} {
		set chan [dict get $idxlist($idx) console-channel]
	} else {
		set chan [lindex [split $text] 0]
	}
	putcmdlog "#$handle# ($chan) channel"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	# user is trying to use the command for a channel where he doesn't have access
	} elseif {![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "You are not a channel op on $chan"
		return 0
	}
	set topic [topic $chan]
	set chanlist [chanlist $chan]
	# FixMe: If handlen or nicklen are too long, truncate them
	set format "%-[expr {$nicklen + 1}]s %-[expr {$handlen + 1}]s %-4s %-1.1s%-5s %s"
	
	putdcc $idx "Channel ${chan}, [llength $chanlist] members, mode [getchanmode $chan]"
	if {$topic ne {}} {
		putdcc $idx "Channel Topic: $topic"
	} else {
		putdcc $idx "Channel topic not set"
	}
	# FixMe: Add join time, idle time
	putdcc $idx {(n = owner, m = master, o = op, d = deop, b = bot)}
	putdcc $idx [format $format NICKNAME HANDLE JOIN {} IDLE USER@HOST]
	foreach nick $chanlist {
		# find nick prefix
		set nickprefix {}
		if {[isop $nick $chan]} {
			set nickprefix {@}
		} elseif {[ishalfop $nick $chan]} {
			set nickprefix {%}
		} elseif {[isvoice $nick $chan]} {
			set nickprefix {+}
		} else {
			set nickprefix { }
		}
		# find uhost
		if {[string equal -nocase $nick $botnick]} {
			set uhost {<- it's me!}
		} else {
			set uhost [getchanhost $nick $chan]
		}
		# find handle
		set nickhand [nick2hand $nick $chan]
		# determine status char to use
		if {[matchattr $nickhand b] && [matchattr $nickhand o|o]} {
			set atrflag {B}
		} elseif {[matchattr $nickhand b]} {
			set atrflag {b}
		} elseif {[matchattr $nickhand n]} {
			set atrflag {N}
		} elseif {[matchattr $nickhand |n $chan]} {
			set atrflag {n}
		} elseif {[matchattr $nickhand m]} {
			set atrflag {M}
		} elseif {[matchattr $nickhand |m $chan]} {
			set atrflag {m}
		} elseif {[matchattr $nickhand d]} {
			set atrflag {D}
		} elseif {[matchattr $nickhand |d $chan]} {
			set atrflag {d}
		} elseif {[matchattr $nickhand r]} {
			set atrflag {R}
		} elseif {[matchattr $nickhand |r $chan]} {
			set atrflag {r}
		} elseif {[matchattr $nickhand a]} {
			set atrflag {A}
		} elseif {[matchattr $nickhand |a $chan]} {
			set atrflag {a}
		} elseif {[matchattr $nickhand y]} {
			set atrflag {Y}
		} elseif {[matchattr $nickhand |y $chan]} {
			set atrflag {y}
		} elseif {[matchattr $nickhand o]} {
			set atrflag {O}
		} elseif {[matchattr $nickhand |o $chan]} {
			set atrflag {o}
		} elseif {[matchattr $nickhand l]} {
			set atrflag {L}
		} elseif {[matchattr $nickhand |l $chan]} {
			set atrflag {l}
		} elseif {[matchattr $nickhand q]} {
			set atrflag {Q}
		} elseif {[matchattr $nickhand |q $chan]} {
			set atrflag {q}
		} elseif {[matchattr $nickhand g]} {
			set atrflag {G}
		} elseif {[matchattr $nickhand |g $chan]} {
			set atrflag {g}
		} elseif {[matchattr $nickhand v]} {
			set atrflag {V}
		} elseif {[matchattr $nickhand |v $chan]} {
			set atrflag {v}
		} elseif {[matchattr $nickhand f]} {
			set atrflag {F}
		} elseif {[matchattr $nickhand |f $chan]} {
			set atrflag {f}
		} elseif {[matchattr $nickhand k]} {
			set atrflag {K}
		} elseif {[matchattr $nickhand |k $chan]} {
			set atrflag {k}
		} elseif {[matchattr $nickhand w]} {
			set atrflag {W}
		} elseif {[matchattr $nickhand |w $chan]} {
			set atrflag {w}
		} elseif {[matchattr $nickhand e]} {
			set atrflag {E}
		} elseif {[matchattr $nickhand |e $chan]} {
			set atrflag {e}
		} else {
			set atrflag { }
		}
		# output
		putdcc $idx [format $format ${nickprefix}${nick} $nickhand "---" $atrflag "" $uhost]
	}
	putdcc $idx "End of channel info."
	return 0
}

# Usage: invite <nickname> [channel]
proc ::tcldrop::irc::dcc::INVITE {handle idx text} {
	global idxlist
	lassign [split $text] victim chan
	if {$text eq {}} {
		putdcc $idx "[lang 0x001]: invite <nickname> \[channel\]"
		return 0
	}
	if {![validchan $chan]} {
		set chan [dict get $idxlist($idx) console-channel]
	}
	putcmdlog "#$handle# ($chan) invite $victim"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	# user is trying to use the command for a channel where he doesn't have access
	} elseif {![matchattr $handle nmo|nmo $chan]} {
		putdcc $idx "You are not a channel op on $chan"
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		putdcc $idx "I can't help you now because I'm not a channel op or halfop on ${chan}."
		return 0
	} elseif {[onchan $victim $chan] && ![onchansplit $victim $chan]} {
		putdcc $idx "$victim is already on ${chan}!"
		return 0
	} else {
		putserv "INVITE $victim $chan"
		putdcc $idx "Inviting $victim to ${chan}."
		return 1
	}
}

# Usage: topic [channel] [text]
proc ::tcldrop::irc::dcc::TOPIC {handle idx text} {
	global idxlist
	set UserTopic [lrange [split $text] 1 end]
	if {$text eq {}} {
		set chan [dict get $idxlist($idx) console-channel]
	} else {
		set chan [lindex [split $text] 0]
	}
	putcmdlog "#$handle# ($chan) topic $UserTopic"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	# user is trying to use the command for a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "You are not a channel op or halfop on ${chan}."
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan] && $UserTopic ne {}} {
		putdcc $idx "I can't help you now because I'm not a channel op or halfop on ${chan}."
		return 0
	} elseif {$UserTopic eq {}} {
		set CurrentTopic [topic $chan]
		if {$CurrentTopic eq {}} {
			putdcc $idx "No topic is set for ${chan}."
			return 1
		} else {
			putdcc $idx "The topic for $chan is: $CurrentTopic"
			return 1
		}
	} else {
		putserv "TOPIC $chan :$UserTopic"
		putdcc $idx "Changing topic on $chan to: $UserTopic"
		return 1
	}
}

# Usage: kick [channel] <nickname> [reason]
# FixMe: Add support for console channel
proc ::tcldrop::irc::dcc::KICK {handle idx text} {
	lassign [split $text] victim chan
	set reason [lrange [split $text] 2 end]
	if {[llength [split $text]] < 2} {
		putdcc $idx "[lang 0x001]: kick \[channel\] <nickname> \[reason\]"
		return 0
	}
	if {$reason eq {}} { set reason "requested" }
	putcmdlog "#$handle# ($chan) kick $victim $reason"
	if {![validchan $chan]} {
		putdcc $idx "No such channel."
		return 0
	} elseif {![botonchan $chan]} {
		putdcc $idx "I'm not on $chan right now!"
		return 0
	# user is trying to use the command for a channel where he doesn't have access
	} elseif {![matchattr $handle nmol|nmol $chan]} {
		putdcc $idx "You are not a channel op or halfop on ${chan}."
		return 0
	} elseif {[string equal -nocase $victim $::botnick]} {
		putdcc $idx "I'm not going to kick myself."
		return 0
	} elseif {![botisop $chan] && ![botishalfop $chan]} {
		putdcc $idx "I can't help you now because I'm not a channel op or halfop on ${chan}."
		return 0
	} elseif {![onchan $victim $chan]} {
		putdcc $idx "$victim is not on ${chan}."
		return 0
	} elseif {[botishalfop $chan] && [isop $victim $chan]} {
		putdcc $idx "I can't help you now because halfops cannot kick ops."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] o|o $chan] && ![matchattr [nick2hand $victim $chan] d|d $chan] && ![matchattr $handle nm|nm $chan]} {
		putdcc $idx "$victim is a legal op."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] m|m $chan] && ![matchattr $handle n|n $chan]} {
		putdcc $idx "$victim is a $chan master."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] n|n $chan] && ![matchattr $handle n|n $chan]} {
		putdcc $idx "$victim is a $chan owner."
		return 0
	} elseif {[matchattr [nick2hand $victim $chan] b $chan] && ![matchattr $handle n|n $chan]} {
		putdcc $idx "$victim is another channel bot!"
		return 0
	} else {
		putkick $chan $victim $reason
		putdcc $idx "Okay, done."
		return 1
	}
}

# FixMe: Add the following commands: kickban, resetbans, resetinvites, resetexcempts, adduser, deluser, reset

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
	bind dcc nmo|nmo channel ::tcldrop::irc::dcc::CHANNEL -priority 1000
	# FixMe: Bind .invite to l|l as well?
	bind dcc nmo|nmo invite ::tcldrop::irc::dcc::INVITE -priority 1000
	bind dcc nmol|nmol topic ::tcldrop::irc::dcc::TOPIC -priority 1000
	bind dcc nmol|nmol kick ::tcldrop::irc::dcc::KICK -priority 1000
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
