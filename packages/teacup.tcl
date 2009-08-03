#! /usr/bin/tclsh8.5

# This is my teacup client, written in pure-Tcl.

# Usage:
# source teacup.tcl
#
# Then just [package require] any package(s) you want to load, they will be downloaded and sourced on-the-fly.

package require Tcl 8.5
package require http
namespace eval ::teacup {
	# Create a teacup ensemble command:
	namespace ensemble create -command teacup -subcommands [list update install load set help] -map [dict create update TEACUP_Update install TEACUP_Install load TEACUP_Load set TEACUP_Set help TEACUP_Help]
	# Export the teacup command:
	namespace export teacup

	# Default values, these can be changed with the [teacup set] command:
	variable teacup
	set teacup(local-repository) [file join $env(HOME) .teacup]
	if {$::tcl_platform(os) eq {Linux} && $::tcl_platform(wordSize) == 4 && [string match {i*86*} $::tcl_platform(machine)]} {
		# hard-coded to avoid an annoying bug in the platform package..
		set teacup(platforms) [list linux-glibc2.9-ix86 linux-glibc2.8-ix86 linux-glibc2.7-ix86 linux-glibc2.6-ix86 linux-glibc2.5-ix86 linux-glibc2.4-ix86 linux-glibc2.3-ix86 linux-glibc2.2-ix86 linux-glibc2.1-ix86 linux-glibc2.0-ix86 tcl]
	} elseif {![catch { ::package require platform }]} {
		set teacup(platforms) [::platform::patterns [::platform::identify]]
	} else {
		set teacup(platforms) {tcl}
	}
	array set teacup {
		geturl-timeout {99999}
		status-url {http://teapot.activestate.com/db/status}
		list-url {http://teapot.activestate.com/package/list}
		package-url {http://teapot.activestate.com/package/name/@NAME@/ver/@VER@/arch/@ARCH@/file}
		index-url {http://teapot.activestate.com/db/index}
		autoupdate-interval {0}
	}

	# Returns 1 when we need to get an updated packages list, 0 when we don't.
	proc TEACUP_NeedUpdates {} {
		variable teacup
		if {![catch { open [file join $teacup(local-repository) status] r } fid]} {
			set status [read -nonewline $fid]
			close $fid
			tclLog "Status: [clock format $status]  ([file join $teacup(local-repository) status])"
		} else {
			set status 0
		}
		if {![catch { ::http::geturl $teacup(status-url) -timeout $teacup(geturl-timeout) } token] && [::http::ncode $token] eq {200} && [regexp -- {<pre>\d+ (\d+) } "[::http::data $token][::http::cleanup $token]" - newstatus]} {
			tclLog "Status: [clock format $newstatus]  ($teacup(status-url))"
			if {$newstatus > $status} {
				# Update our local status file to match the remote:
				if {![catch { file mkdir $teacup(local-repository) }] && ![catch { open [file join $teacup(local-repository) status] w } fid]} {
					puts -nonewline $fid $newstatus
					close $fid
				}
			} else {
				tclLog "No need for package list update."
				return 0
			}
		}
		return 1
	}

	proc TEACUP_Update {{force {0}}} {
		variable teacup
		# Only download the package list if the status file has been updated:
		if {$force || ![file exists [file join $teacup(local-repository) list.html]] || [TEACUP_NeedUpdates]} { Download $teacup(list-url) [file join $teacup(local-repository) list.html] }
		if {![catch { open [file join $teacup(local-repository) list.html] r } fid]} {
			set data [read -nonewline $fid]
			close $fid
			set ifneeded {}
			foreach {html name ver arch} [regexp -all -inline -line -- {<a href="/package/name/([^/]+)/ver/([^/]+)/arch/([^/]+)/details">.*</a>} $data] {
				#tclLog "PROCESSING: $name $ver $arch"
				if {[SupportedArch $arch]} {
					if {([dict exists $teacup(packages) $name $ver $arch downloaded] && ![dict get $teacup(packages) $name $ver $arch downloaded]) || ![VSatisfies $name $ver]} {
						lappend ifneeded "package ifneeded $name $ver \[list ::teacup::TEACUP_Install $name $ver $arch\]"
						dict set teacup(packages) $name $ver $arch downloaded 0
						#tclLog "IFNEEDED: Name: $name Ver: $ver Arch: $arch"
					} else {
						#tclLog "HAVELOCAL: Name: $name Ver: $ver Arch: $arch"
					}
				}
			}
			if {[llength $ifneeded]} {
				# Doing the package ifneeded commands last, so only our local packages will be found when doing the VSatisfies command above:
				eval [join $ifneeded \n]
				tclLog "[llength $ifneeded] packages are available from remote (if needed)."
			} else {
				tclLog "\$ifneeded is [llength $ifneeded] in length!"
			}
		} else {
			tclLog "Can't read from [file join $teacup(local-repository) list.html]"
		}
	}

	# FixMe: Find out why packages like tcllibc don't have their .zip files deleted after extraction.

	# Install a package.  This will also update our cached copy of the package:
	proc TEACUP_Install {name ver arch} {
		variable teacup
		# $path is the directory the file will be put in.
		# $filepath is the $path/$filename (minus the extension for now).
		set filepath [file join [set path [file normalize [file join $teacup(local-repository) $arch [file dirname [string map {{::} {/}} $name]]]]] "[file tail [string map {{::} {/}} $name]]-${ver}"]
		# Add a .tmp extension because we don't know what kind of file it is yet:
		if {![catch { Download [set url [string map [list {@NAME@} $name {@VER@} $ver {@ARCH@} $arch] $teacup(package-url)]] "${filepath}.tmp" } code options] && $code} {
			dict set teacup(packages) $name $ver $arch downloaded 1
			switch -- [dict get $options Content-Type] {
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
					if {([Unzip "${filepath}.zip" $filepath] || [Wobzip "${url}.zip" $filepath]) && [file exists [file join $filepath pkgIndex.tcl]]} {
						# The name "dir" for this variable is important, don't change it..
						set dir $path
						# Add this directory to the ::auto_path (it won't work for THIS package require, but it will if we package require it again later):
						# Note: We only need the next higher up directory in the ::auto_path, Tcl itself checks the immediate subdirectories for pkgIndex.tcl files..
						if {[lsearch -exact $::auto_path $path] == -1} {
							tclLog "Adding $path to ::auto_path"
							lappend ::auto_path $path
						}
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
					tclLog "Unhandled Content-Type: [dict get $options Content-Type]  (Report this as a bug in teacup.tcl)"
					file delete -force -- "${filepath}.tmp"
				}
			}
		}
	}

	proc TEACUP_Load {} {
		variable teacup
		::tcl::tm::add [file normalize [file join $teacup(local-repository) tcl]]
		foreach p [glob -nocomplain -dir $teacup(local-repository) *] {
			lappend ::auto_path [file normalize $p]
		}
		# FixMe: Find the bug in Tcl that makes it not look for existing packages until AFTER we package require something (it should know what packages are available before they're package required):
		catch { package require UpdateAvailablePackages }
	}

	proc SupportedArch {{arch {unknown}}} {
		if {$arch eq {source}} {
			# "source" is unsupported.  (source = source code, which means we'd have to compile it to use it..and that's unlikely.)
			return 0
		}
		variable teacup
		if {[lsearch -exact $teacup(platforms) $arch] != -1} {
			return 1
		} elseif {[string match -nocase $teacup(platforms) $arch]} {
			return 1
		}
		return 0
	}

	# Downloads $url to $destfile, returns 1 for success, 0 for error.
	proc Download {url destfile} {
		if {![catch { file mkdir [file dirname $destfile] ; open ${destfile}.dl wb } fid]} {
			variable DownloadCount
			tclLog "Get:[incr DownloadCount] $url"
			variable teacup
			set downloadtime [clock milliseconds]
			if {![catch { ::http::geturl $url -timeout $teacup(geturl-timeout) -channel $fid } token] && [::http::ncode $token] eq {200}} {
				close $fid
				# If successful, rename to the requested filename:
				file rename -force -- ${destfile}.dl $destfile
				tclLog "Fetched [set size [::http::size $token]] bytes in [set downloadtime [expr { ([clock milliseconds] - $downloadtime) / 1000.0 }]] seconds ([format {%.4f} [expr { ($size / 1024.0) / $downloadtime }]]kB/s)."
				return -options "[::http::meta $token][::http::cleanup $token]" 1
			} else {
				close $fid
				file delete -force -- ${destfile}.dl
				tclLog "Failed to get: $url"
			}
		} else {
			tclLog "Failed to mkdir for or open for writing: ${destfile}.dl"
		}
		return 0
	}

	# Try to unzip $file to $dest by trying different methods:
	proc Unzip {file dest} {
		foreach method {vfs::zip unzip default} {
			switch -- $method {
				{vfs::zip} {
					if {![catch { uplevel #0 {package require vfs::zip} }] && ![catch { uplevel #0 [list ::vfs::zip::Mount $file $file] } mnt]} {
						if {![catch { file mkdir $dest ; file copy -force -- {*}[glob -directory $file *] $dest }]} {
							::vfs::zip::Unmount $mnt $file
							tclLog "vfs::zip $file $dest"
							return 1
						} else {
							::vfs::zip::Unmount $mnt $file
						}
					}
				}
				{unzip} {
					if {[auto_execok unzip] ne {} && ![catch { exec unzip -o $file -d $dest }]} {
						tclLog "exec unzip -o $file -d $dest"
						return 1
					}
				}
				{Trf} {
					# FixMe: Add this method.
				}
				{default} {
					tclLog "Can't Unzip locally: $file -> $dest"
				}
			}
		}
		return 0
	}

	# Uses wobzip.org to get the .zip file and present a list of download links for the files contained in the .zip, and downloads all the files individually to $outDir
	# returns 1 on complete success (all files downloaded), or 0 on error(s) (some or all files failed to download).
	proc Wobzip {url {outDir {.}}} {
		tclLog "Wobzip: Retrieving $url"
		variable teacup
		if {![catch { ::http::geturl "http://wobzip.org/?type=url&url=${url}" -timeout $teacup(geturl-timeout) } token] && [::http::ncode $token] eq {200}} {
			foreach {link file} [regexp -all -inline -- {/get/save_file\.php[^&]+&f=([^']+)} "[::http::data $token][::http::cleanup $token]"] {
				if {![Download "http://wobzip.org${link}" [file join $outDir [string trimleft $file {/}]]]} {
					set retVal 0
				} elseif {![info exists retVal]} {
					# Only set retVal to 1 once, so that it'll only be 1 if we're able to get ALL the files:
					set retVal 1
				}
			}
			if {[info exists retVal]} {
				return $retVal
			} else {
				tclLog "Wobzip: No files downloaded from archive @ $url  (regexp may need updating)."
			}
		} else {
			tclLog "Wobzip: Failed to get zip file list."
		}
		return 0
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

	if {![info exists teacup(packages)]} { set teacup(packages) [dict create] }

	# Note: autoupdate-interval should be in seconds.
	if {$teacup(autoupdate-interval) > 0} {
		proc DoAutoUpdate {{lastupdate {0}}} {
			variable teacup
			# Keep doing autoupdates as long as this is > zero:
			if {$teacup(autoupdate-interval) > 0} {
				if {[clock seconds] - $lastupdate >= $teacup(autoupdate-interval)} {
					# Time for another update:
					if {[TEACUP_NeedUpdates]} { TEACUP_Update 1 }
					after [expr { $teacup(autoupdate-interval) * 1001 }] [list ::teacup::DoAutoUpdate [clock seconds]]
				} else {
					# We didn't wait long enough, try again in a little while:
					after [expr { ([clock seconds] - $lastupdate) * 1001 }] [list ::teacup::DoAutoUpdate $lastupdate]
				}
			}
		}
		after [expr { $teacup(autoupdate-interval) * 1000 }] [list ::teacup::DoAutoUpdate 1]
	}
}
package provide teacup 1.0


# Note: These commands will be removed from here later on and left for the end-user to run...

namespace import ::teacup::teacup

# Load our local (cached) list (This actually just sets the auto_path and tm paths for now):
teacup load

# Update packages list from http://teapot.activestate.com/:
teacup update

after 9999999 [list set ::forever 1]
vwait ::forever
