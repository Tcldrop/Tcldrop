## httpzip.tcl, http based unzip
#
# Usage: ::httpzip::unzip <url> [output directory]
#
# Note: This is just concept code and requires more cleanup / error handling.
# Ultimately I would like a php script that goes along with this so it doesn't depend on a third party website.
#
# Note/FixMe: it seems to take quite a while to download all these small files one by one. Possible solutions:
#     - re-compress the files with a zlib-supported format on the web server, and depend on Tcl 8.6
#     - Use http://wiki.tcl.tk/11060 (Parallel Geturl), untested if it's faster
#

package require Tcl 8.2
package require http

namespace eval ::httpzip {

	# returns 0 on success, 1 on error
	proc unzip {url {outDir .}} {
		# http://wobzip.org/?type=url&url=FILE_URL_HERE&p=PASSWORD_HERE
		if {[Wobzip $url $outDir]} {
			return 0
		} else {
			return 1
		}
	}
	
	# returns 0 on success, 1 on error
	proc Wobzip {url outDir} {
		set retVal 0
		if {![catch { ::http::geturl http://wobzip.org/?type=url&url=${url} -timeout 99999 } token]} {
			set data [::http::data $token]
			::http::cleanup $token
			foreach {link file} [regexp -all -inline -- {/get/save_file\.php[^&]+&f=([^']+)} $data] {
				set file [file join $outDir [string trimleft $file {/}]]
				set dir [file dirname $file]
				puts "$file -> $dir"
				if {![file exists $dir]} {
					puts "$file -> creating $dir"
					file mkdir $dir
				}
				if {![catch { ::http::geturl http://wobzip.org${link} -timeout 99999 } token]} {
					puts "$file -> downloaded"
					set fd [open $file w]
					puts -nonewline $fd [::http::data $token]
					close $fd
					::http::cleanup $token
					puts "$file -> written\n"
				} else {
					set retVal 1
				}
			}
			return $retVal
		}
		return 1
	}
	
}

# ::httpzip::unzip http://teapot.activestate.com/package/name/Expect-sf/ver/0.0.0.2009.05.20.12.29.49/arch/source/file.zip c:/temp/ziptest

