
package provide tcldrop::hostenv::eggdrop
namespace eval ::tcldrop::hostenv::eggdrop {
	# This is so we can see the Tcldrop's putlog's in case we're running inside an Eggdrop.
	proc PutLogLev {name levels channel text} {
		if {[array size Tcldrop]} { set pre "Tcldrop/$name: " } else { set pre {Tcldrop: } }
		putloglev $levels $channel "$pre$text"
	}
	catch {
		# This creates a DCC command called .tcldrop if we're running inside an Eggdrop.
		bind dcc n tcldrop ::tcldrop::DCC
		proc ::tcldrop::DCC {handle idx text} {
			set ltext [split $text]
			putcmdlog "#$handle# tcldrop $text"
			if {[catch { Tcldrop [lindex $ltext 0] [lindex $ltext 1] [join [lrange $ltext 2 end]] } error]} { putdcc $idx $error }
		}
	}
}
