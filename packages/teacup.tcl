
# This is my teacup client, written in pure-Tcl.

# This is as basic as it gets right now...

# Usage:
# source teacup.tcl
#
# Then just [package require] any package you want to load, it will be downloaded and sourced.

# Note/FixMe: This is only working for arch=tcl right now..

package require Tcl 8.2
package require http
package require platform
namespace eval ::tcl::teacup {
	variable Platform [::platform::identify]
	# Update the list of packages:
	proc TEACUP_Update {} {
		# Download the /package/list because I don't know how to decipher the /db/index ...
		set token [::http::geturl "http://teapot.activestate.com/package/list" -timeout 99999]
		foreach l [split [::http::data $token] \n] {
			if {[regexp {<a href="/package/name/(.*)/ver/(.*)/arch/(.*)/details">.*</a>} $l - name ver arch]} {
				if {[SupportedArch $arch]} {
					# FixMe: If we already have this package locally or in the cache use it instead of re-downloading it.
					package ifneeded $name $ver [list ::tcl::teacup::TEACUP_Install $name $ver $arch]
					puts "IFNEEDED: Name: $name Ver: $ver Arch: $arch"
				}
			}
		}
		::http::cleanup $token
	}
	# Install a package:
	proc TEACUP_Install {name {ver {}} {arch {}}} {
		puts "GETTING: http://teapot.activestate.com/package/name/${name}/ver/${ver}/arch/${arch}/file"
		# FixMe: Find an appropriate place to store the cached packages:
		set fid [open "$name-$ver-$arch.tm" w]
		set token [::http::geturl "http://teapot.activestate.com/package/name/${name}/ver/${ver}/arch/${arch}/file" -timeout 99999 -channel $fid]
		::http::cleanup $token
		close $fid
		uplevel #0 [list source "$name-$ver-$arch.tm"]
	}
	proc SupportedArch {arch} {
		variable Platform
		switch -- $arch {
			{tcl} - $Platform {
				return 1
			}
			{default} {
				return 0
			}
		}
	}
	# Update now:
	TEACUP_Update
}
