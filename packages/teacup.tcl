#! /usr/bin/tclsh8.6

# This is my teacup client, written in pure-Tcl.

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
	variable teacup
	set teacup(local-repository) [file join $env(HOME) .teacup-cache]
	if {$::tcl_platform(os) eq {Linux} && $::tcl_platform(wordSize) == 4} {
		# hard-coded to avoid an annoying bug in the platform package..
		set teacup(platforms) [list linux-glibc2.9-ix86 linux-glibc2.8-ix86 linux-glibc2.7-ix86 linux-glibc2.6-ix86 linux-glibc2.5-ix86 linux-glibc2.4-ix86 linux-glibc2.3-ix86 linux-glibc2.2-ix86 linux-glibc2.1-ix86 linux-glibc2.0-ix86 tcl]
	} elseif {![catch { ::package require platform }]} {
		set teacup(platforms) [::platform::patterns [::platform::identify]]
	} else {
		set teacup(platforms) {tcl}
	}
	array set teacup {
		status-url {http://teapot.activestate.com/db/status}
		list-url {http://teapot.activestate.com/package/list}
		package-url {http://teapot.activestate.com/package/name/@NAME@/ver/@VER@/arch/@ARCH@/file}
		index-url {http://teapot.activestate.com/db/index}
	}

	proc TEACUP_NeedUpdates {} {
		variable teacup
		if {![catch { open [file join $teacup(local-repository) status] r } fid]} {
			set status [read -nonewline $fid]
			tclLog "Old status loaded: $status"
			close $fid
		} else {
			tclLog "local status file doesn't (yet) exist."
			set status {}
		}
		set token []
		if {![catch { ::http::geturl $teacup(status-url) -timeout 99999 } token]} {
			if {[regexp {<pre>(.*)</pre>} [::http::data $token] - newstatus] && $status ne [set newstatus [lindex $newstatus 1]]} {
				set fid [open [file join $teacup(local-repository) status] w]
				puts -nonewline $fid $newstatus
				close $fid
				::http::cleanup $token
				tclLog "Wrote new status: $newstatus"
				return 1
			} else {
				::http::cleanup $token
				tclLog "No need for update."
				return 0
			}
			return 1
		}
		return 1
	}

	proc TEACUP_Update {} {
		variable teacup
		set ifneeded {}
		if {[TEACUP_NeedUpdates]} {
			# Download the /package/list because I don't know how to decipher the /db/index ...
			set token [::http::geturl $teacup(list-url) -timeout 99999]
			foreach l [split [::http::data $token] \n] {
				#tclLog "RAW: $l"
				if {[regexp {<a href="/package/name/(.*)/ver/(.*)/arch/(.*)/details">.*</a>} $l - name ver arch]} {
					if {[SupportedArch $arch]} {
						# Note: $arch = source is unsupported.
						if {![VSatisfies $name $ver]} {
							append ifneeded "package ifneeded $name $ver \[list ::tcl::teacup::TEACUP_Install $name $ver $arch\]\n"
							#tclLog "IFNEEDED: Name: $name Ver: $ver Arch: $arch"
						} else {
							#tclLog "HAVELOCAL: Name: $name Ver: $ver Arch: $arch"
						}
					}
				}
			}
			::http::cleanup $token
		} else {
			# FixMe: Load local copy.
			tclLog "FixMe: Need to load the local cached copy of the package list (or a list of our own format)."
			# Remove next 2 lines once fixed:
			file delete -force -- [file join $teacup(local-repository) status]
			TEACUP_Update
		}
		if {[string length $ifneeded] == 0} {
			# Temporary check.  Remove or replace with something else in the final code.
			tclLog "\$ifneeded is [string length $ifneeded] in length!  o_O"
		} else {
			# Do the package ifneeded commands last, so only our local packages will be found when doing the VSatisfies command above:
			eval $ifneeded
		}
	}

	# Install a package.  This will also update our cached copy of the package:
	proc TEACUP_Install {name ver arch} {
		variable teacup
		set url [string map [list {@NAME@} $name {@VER@} $ver {@ARCH@} $arch] $teacup(package-url)]
		tclLog "GETTING: $url"
		file mkdir [set path [file join $teacup(local-repository) $arch [file dirname [string map {{::} {/}} $name]]]]
		# $filepath is the $path/$filename (minus the extension for now):
		set filepath [file join $path "[file tail [string map {{::} {/}} $name]]-${ver}"]
		# Add a .tmp extension because we don't know what kind of file it is yet:
		set fid [open "${filepath}.tmp" w]
		set token [::http::geturl "$url" -timeout 99999 -channel $fid]
		close $fid
		if {[::http::ncode $token] eq {200}} {
			switch -- [dict get [::http::meta $token] Content-Type] {
				{text/plain; charset=UTF-8} {
					# tcl file = text/plain; charset=UTF-8
					if {$arch eq {tcl}} {
						# We don't know that it's a .tcl file until just now, so rename the file we got:
						file rename -force -- "${filepath}.tmp" [set filepath "${filepath}.tm"]
						uplevel #0 [list source -encoding utf-8 $filepath]
					}
				}
				{application/x-zip} {
					# zip file = application/x-zip
					# We don't know it's a .zip file until just now, so rename the file we got:
					file rename -force -- "${filepath}.tmp" "${filepath}.zip"
					if {[Unzip "${filepath}.zip" $filepath] && [file exists [file join $filepath pkgIndex.tcl]]} {
						# The name "dir" for this variable is important, don't change it..
						set dir $filepath
						# Add this directory to the ::auto_path (it won't work for THIS package require, but it will if we package require it again later):
						# Note: We only need the next higher up directory in the ::auto_path, Tcl itself checks the immediate subdirectories for pkgIndex.tcl files..
						if {[lsearch -exact $::auto_path $path] == -1} { lappend ::auto_path $path }
						# Source the pkgIndex.tcl to replace the existing ifneeded script for this package:
						source [file join $filepath pkgIndex.tcl]
						# eval the new ifneeded script (must be done using uplevel #0):
						uplevel #0 [package ifneeded $name $ver]
					} else {
						tclLog "Unzip failed, or pkgIndex.tcl missing!"
					}
					# Delete the .zip file, we don't need it anymore:
					file delete -force -- "${filepath}.zip"
				}
				{default} {
					tclLog "Unhandled Content-Type: [dict get [::http::meta $token] Content-Type]"
					file delete -force -- "${filepath}.tmp"
					exit 1
				}
			}
		} else {
			# ncode was something other than 200, so delete the file (if it exists):
			file delete -force -- "${filepath}.tmp"
		}
		::http::cleanup $token
	}

	proc TEACUP_Load {} {
		variable teacup
		::tcl::tm::add [file join $teacup(local-repository) tcl]
		# FixMe: Add the paths to the pkgIndex.tcl files to the ::auto_path list.
		foreach p [glob -nocomplain -dir $teacup(local-repository) *] {
			lappend ::auto_path $p
		}
		# FixMe: Find the bug in Tcl that makes it not look for existing packages until AFTER we package require something (it should know what packages are available before they're package required):
		catch { package require asjsdjsdjdfs }
	}

	proc SupportedArch {{arch {tcl}}} {
		variable teacup
		if {[lsearch -exact $teacup(platforms) $arch] != -1} {
			return 1
		} elseif {[string match -nocase $teacup(platforms) $arch]} {
			return 1
		} else {
			return 0
		}
	}

	proc Unzip {file dest} {
		if {![catch { uplevel #0 package require vfs::zip }] && ![catch { uplevel #0 ::vfs::zip::Mount $file $file } mnt]} {
			file mkdir $dest
			file copy -force -- {*}[glob -directory $file *] $dest
			::vfs::zip::Unmount $mnt $file
			tclLog "vfs::zip $file $dest"
			return 1
		} elseif {[auto_execok unzip] ne {} && ![catch { exec unzip -o $file -d $dest }]} {
			tclLog "exec unzip -o $file -d $dest"
			return 1
		} else {
			tclLog "Can't Unzip: $file -> $dest"
			return 0
		}
	}

	# Checks to see if our local package is equal to or _NEWER_ than the remote:
	proc VSatisfies {name ver} {
		foreach v [package versions $name] {
			if {[package vsatisfies $v $ver]} { return 1 }
		}
		return 0
	}

	# Sets a var in the teacup array:
	proc TEACUP_Set {var value} {
		variable teacup
		set teacup($var) $value
	}
}


# Note: These commands will be removed from here later on and left for the end-user to run...

namespace import ::tcl::teacup::teacup

# Change the defaults for..testing purposes:
teacup set local-repository [file join $env(HOME) svn packages teapot]
#teacup set platforms {*}
teacup set status-url {http://teapot.activestate.com/db/status}
teacup set list-url {http://teapot.activestate.com/package/list}
teacup set package-url {http://teapot.activestate.com/package/name/@NAME@/ver/@VER@/arch/@ARCH@/file}

# Load our local (cached) list:
teacup load

# Update packages list from http://teapot.activestate.com/:
teacup update

