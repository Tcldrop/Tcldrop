
# This is my teacup client, written in pure-Tcl.

# This is as basic as it gets right now...

# Usage:
# source teacup.tcl
#
# Then just [package require] any package you want to load, it will be downloaded and sourced.

# Note/FixMe: This is only working for arch=tcl right now..

package require Tcl 8.5
package require http
namespace eval ::tcl::teacup {
	# Create a teacup ensemble command:
	namespace ensemble create -command teacup -subcommands [list update install load set help] -map [dict create update TEACUP_Update install TEACUP_Install load TEACUP_Load set TEACUP_Set help TEACUP_Help]
	# Export the teacup command:
	namespace export teacup

	# Default values, these can be changed with the [teacup set] command:
	variable local-repository [file join $env(HOME) .teacup-cache]
	catch {
		variable platforms {tcl}
		::package require platform
		variable platforms [::platform::patterns [::platform::identify]]
	}

	proc TEACUP_Update {} {
		# Download the /package/list because I don't know how to decipher the /db/index ...
		# FixMe: Can check the contents of http://teapot.activestate.com/db/status to see if their database has been updated since our last check.
		set token [::http::geturl "http://teapot.activestate.com.nyud.net/package/list" -timeout 99999]
		set ifneeded {}
		foreach l [split [::http::data $token] \n] {
			#puts "RAW: $l"
			if {[regexp {<a href="/package/name/(.*)/ver/(.*)/arch/(.*)/details">.*</a>} $l - name ver arch]} {
				if {[SupportedArch $arch]} {
					if {![VSatisfies $name $ver]} {
						append ifneeded "package ifneeded $name $ver \[list ::tcl::teacup::TEACUP_Install $name $ver $arch\]\n"
						puts "IFNEEDED: Name: $name Ver: $ver Arch: $arch"
					} else {
						puts "HAVELOCAL: Name: $name Ver: $ver Arch: $arch"
					}
				}
			}
		}
		::http::cleanup $token
		puts "EVAL: $ifneeded"
		# Do the package ifneeded commands last, so only our local packages will be found when doing the VSatisfies command above:
		eval $ifneeded
	}

	# Install a package.  This will also update our cached copy of the package:
	proc TEACUP_Install {name ver arch} {
		puts "GETTING: http://teapot.activestate.com/package/name/${name}/ver/${ver}/arch/${arch}/file"
		variable local-repository
		if {$arch eq {tcl}} { set ext {tm} } else { set ext {zip} }
		set subdir [file dirname [string map {{::} {/}} $name]]
		file mkdir [file join ${local-repository} $arch $subdir]
		set filepath [file join ${local-repository} $arch $subdir "[file tail [string map {{::} {/}} $name]]-${ver}.$ext"]
		set fid [open $filepath w]
		set token [::http::geturl "http://teapot.activestate.com/package/name/${name}/ver/${ver}/arch/${arch}/file" -timeout 99999 -channel $fid]
		close $fid
		if {[::http::ncode $token] eq {200}} {
			if {[string match -nocase {text/plain*} [dict get [::http::meta $token] Content-Type]]} {
				# tcl file = text/plain; charset=UTF-8
				uplevel #0 [list source $filepath]
			} elseif {$arch eq {tcl}} {
				# This is a temporary check to make sure there's nothing that matches this situation..
				puts "ERROR!  Arch = tcl and Content-Type != text/plain -- $name $ver $arch"
				exit 1
			} else {
				# zip file = application/x-zip
				# If it's not text/plain then it's a damn .zip file!  We can't use .zip files, because there's no pure-Tcl way of unzipping them.
				# FixMe: Extract the contents of this .zip and add the directory to the ::auto_path.
				puts "Unhandled: $filepath"
				file delete -force -- $filepath
			}
		} else {
			# ncode was something other than 200, so delete the file (if it exists):
			file delete -force -- $filepath
		}
		::http::cleanup $token
	}

	proc TEACUP_Load {} {
		variable local-repository
		::tcl::tm::add [file join ${local-repository} tcl]
		# FixMe: Add the paths to the pkgIndex.tcl files to the ::auto_path list.
	}

	proc SupportedArch {{arch {tcl}}} {
		variable platforms
		if {[lsearch -exact $platforms $arch] != -1} {
			return 1
		} elseif {[string match -nocase $platforms $arch]} {
			return 1
		} else {
			return 0
		}
	}

	# FixMe: ...
	proc Unzip {file dest} {
		if {![catch { package require Trf }]} {
		} elseif {![catch { package require vfs::zip }]} {
		}
	}

	# Checks to see if our local package is equal to or _NEWER_ than the remote:
	proc VSatisfies {name ver} {
		foreach v [package versions $name] {
			if {[package vsatisfies $v $ver]} { return 1 }
		}
		return 0
	}

	proc TEACUP_Set {var value} {
		variable $var $value
		return $value
	}
}


# Note: These commands will be removed from here later on and left for the end-user to run...

namespace import ::tcl::teacup::teacup

# Change the defaults for..testing purposes:
teacup set local-repository [file join $env(HOME) svn packages teapot]
teacup set platforms {*}

# Load our local (cached) list:
teacup load

# Update packages list from http://teapot.activestate.com/:
teacup update
