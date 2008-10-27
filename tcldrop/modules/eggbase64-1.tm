# eggbase64.tcl --
#
# Base64 Encode/Decode strings - but Eggdrop style.
#
# This script provides four different functions that deal with
# Eggdrop's funky implementations of base64.
#
# ::eggbase64::toint <string>
# Converts encoded channel strings used in eggdrop botnet
# protocol back to their corresponding integer values
#
# ::eggbase64::fromint <string>
# Converts integer channel ids to what eggdrop uses in its botnet
# protocol
#
# ::eggbase64::encode <blowfishencryptedbinarydata>
# provides eggdrop compatible base64 encoding used in the blowfish
# module. use blowfish.tcl from tcclib to encrypt your data and use
# this proc to encode it
#
# ::eggbase64::decode <base64encodedstring>
# decodes eggdrop compatible base64 encoded strings back to binary
# data to be then decrypted with blowfish
#
# roc (@efnet) 2008-10-18
# FireEgl@EFNet 2008-10-21  (I only optimized it a bit, roc did all the hard work!  Thank you!)

package require Tcl 8.5
namespace eval ::eggbase64 {
	variable version 1.0
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide eggbase64 $version
	variable EGGDROP_BASE64_SALT [list . / 0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z]
	variable EGGDROP_BASE64_TO_INT_SALT [list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 52 53 54 55 56 57 58 59 60 61 0 0 0 0 0 0 0 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 62 0 63 0 0 0 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	variable EGGDROP_INT_TO_BASE64_SALT [list A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9 \[ \]]
	namespace ensemble create -subcommands [list encode decode toint fromint]
	namespace ensemble create -command ::eggbase64 -subcommands [list encode decode toint fromint]
	namespace ensemble create -command ::tcldrop::eggbase64 -subcommands [list encode decode toint fromint]
	#namespace export eggbase64 encode decode toint fromint
}

proc ::eggbase64::toint {input} {
	variable EGGDROP_BASE64_TO_INT_SALT
	set i 0
	foreach char [split $input {}] {
		set i [expr { ($i << 6) + [lindex $EGGDROP_BASE64_TO_INT_SALT [scan $char %c]] }]
	}
	return $i
}

proc ::eggbase64::fromint {input} {
	if {$input == 0} {
		return {A}
	} elseif {![string is integer -strict $input]} {
		return -code error "\"$input\" is not an integer"
	}
	variable EGGDROP_INT_TO_BASE64_SALT
	while {$input != 0} {
		append output [lindex $EGGDROP_INT_TO_BASE64_SALT [expr { $input & 0x3f }]]
		set input [expr { $input >> 6 }]
	}
	string reverse $output
}

proc ::eggbase64::decode {input} {
	# FixMe: $input needs to be padded.
	set k -1
	variable EGGDROP_BASE64_SALT
	set input [split $input {}]
	while {$k < [llength $input] - 1} {
		set right [set left 0]
		for {set i 0} {$i < 6} {incr i} {
			set right [expr { $right | [lsearch -exact $EGGDROP_BASE64_SALT [lindex $input [incr k]]] << $i * 6 }]
		}
		for {set i 0} {$i < 6} {incr i} {
			set left [expr { $left | [lsearch -exact $EGGDROP_BASE64_SALT [lindex $input [incr k]]] << $i * 6 }]
		}
		for {set i 0} {$i < 4} {incr i} {
			if {[set z [expr { ($left & 0xFF << (3 - $i) * 8) >> (3 - $i) * 8 }]] < 0} { incr z 256 }
			append output [format %c $z]
		}
		for {set i 0} {$i < 4} {incr i} {
			if {[set z [expr { ($right & 0xFF << (3 - $i) * 8) >> (3 - $i) * 8 }]] < 0} { incr z 256 }
			append output [format %c $z]
		}
	}
	return $output
}

proc ::eggbase64::encode {input} {
	# FixMe: $input needs to be padded.
	variable EGGDROP_BASE64_SALT
	set left [set right 0]
	set k -1
	binary scan $input c* input
	while {$k < [llength $input] - 1} {
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		set left [expr { $v << 24 }]
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		set left [expr { $left + ($v << 16) }]
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		set left [expr { $left + ($v << 8) }]
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		incr left $v
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		set right [expr { $v << 24 }]
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		set right [expr { $right + ($v << 16) }]
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		set right [expr { $right + ($v << 8) }]
		if {[set v [lindex $input [incr k]]] < 0 } { incr v 256 }
		incr right $v
		for {set i 0} {$i < 6} {incr i} {
			append output [lindex $EGGDROP_BASE64_SALT [expr { $right & 0x3F }]]
			set right [expr { $right >> 6 }]
		}
		for {set i 0} {$i < 6} {incr i} {
			append output [lindex $EGGDROP_BASE64_SALT [expr { $left & 0x3F }]]
			set left [expr { $left >> 6 }]
		}
	}
	return $output
}

# Before FireEgl's changes:
# fromint: 50.5437 microseconds per iteration: Yaf
# toint: 83.6898 microseconds per iteration: 99999
# encode: 447.522 microseconds per iteration: x.Fxg/JGGEk/wPSWZ0cdqhP0
# decode: 795.1022 microseconds per iteration: V¬-/RÄ¦#¦M+++=ìb

# After FireEgl's changes:
# fromint: 34.1788 microseconds per iteration: Yaf
# toint: 65.2458 microseconds per iteration: 99999
# encode: 184.096 microseconds per iteration: x.Fxg/JGGEk/wPSWZ0cdqhP0
# decode: 564.9768 microseconds per iteration: V¬-/RÄ¦#¦M+++=ìb
