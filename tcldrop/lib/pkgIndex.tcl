package ifneeded Crypto 1.3 [list load [file join $dir crypto[info sharedlibextension]]]
package ifneeded critcl 0.33 [list source [file join $dir critcl.tcl]]
package ifneeded ident 0.1 [list source [file join $dir ident.tcl]]
package ifneeded blowfishc 0.11 [list source [file join $dir blowfishc.tcl]]
package ifneeded pubsafetcl 0.1 [list source [file join $dir pubsafetcl.tcl]]
if {![package vsatisfies [package provide Tcl] 8.2]} {return}
package ifneeded md5 2.0.0 [list source [file join $dir md5x.tcl]]
package ifneeded md5 1.4.3 [list source [file join $dir md5.tcl]]
package ifneeded sha1 1.0.3 [list source [file join $dir sha1.tcl]]
package ifneeded base64   2.2.2 [list source [file join $dir base64.tcl]]
package ifneeded sum 1.1.0   [list source [file join $dir sum.tcl]]
package ifneeded tea 1.0 [list source [file join $dir tea.tcl]]
if {![package vsatisfies [package provide Tcl] 8.3]} {return}
if {[file exists [file join $dir des.tcl]]} {
    package ifneeded tclDES 0.6 [list source [file join $dir des.tcl]]
}
if {[file exists [file join $dir desjr.tcl]]} {
    package ifneeded tclDESjr 0.6 [list source [file join $dir desjr.tcl]]
}

