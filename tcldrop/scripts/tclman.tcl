# $Id$
#

namespace eval ::tclman {
	variable url {http://Tcl.Tk/man/tcl8.5/TclCmd/}
	variable cmds
	array set cmds {
		after after.htm append append.htm array array.htm auto_execok library.htm auto_import library.htm
		auto_load library.htm auto_mkindex library.htm auto_mkindex_old library.htm auto_qualify library.htm
		auto_reset library.htm bgerror bgerror.htm binary binary.htm break break.htm catch catch.htm cd cd.htm
		clock clock.htm close close.htm concat concat.htm continue continue.htm dde dde.htm dict dict.htm encoding encoding.htm
		eof eof.htm error error.htm eval eval.htm exec exec.htm exit exit.htm expr expr.htm fblocked fblocked.htm
		fconfigure fconfigure.htm fcopy fcopy.htm file file.htm fileevent fileevent.htm filename filename.htm
		flush flush.htm for for.htm foreach foreach.htm format format.htm gets gets.htm glob glob.htm global global.htm
		history history.htm http http.htm if if.htm incr incr.htm info info.htm interp interp.htm join join.htm
		lappend lappend.htm lassign lassign.htm lindex lindex.htm linsert linsert.htm list list.htm llength llength.htm load load.htm
		lrange lrange.htm lrepeat lrepeat.htm lreplace lreplace.htm lsearch lsearch.htm lset lset.htm lsort lsort.htm memory memory.htm
		msgcat msgcat.htm namespace namespace.htm open open.htm package package.htm parray library.htm pid pid.htm
		pkg::create packagens.htm pkg_mkIndex pkgMkIndex.htm proc proc.htm puts puts.htm pwd pwd.htm
		re_syntax re_syntax.htm read read.htm regexp regexp.htm registry registry.htm regsub regsub.htm
		rename rename.htm resource resource.htm return return.htm Safe_Base safe.htm {Safe Base} safe.htm safe safe.htm
		scan scan.htm seek seek.htm set set.htm socket socket.htm source source.htm split split.htm string string.htm
		subst subst.htm switch switch.htm {Tcl syntax} Tcl.htm {tcl syntax} Tcl.htm tcl_endOfWord library.htm
		tcl_findLibrary library.htm tcl_startOfNextWord library.htm tcl_startOfPreviousWord library.htm
		tcl_wordBreakAfter library.htm tcl_wordBreakBefore library.htm tcltest tcltest.htm tclvars tclvars.htm
		tell tell.htm time time.htm trace trace.htm unknown unknown.htm unload unload.htm unset unset.htm update update.htm
		uplevel uplevel.htm upvar upvar.htm variable variable.htm vwait vwait.htm while while.htm
		contents contents.htm tclman contents.htm {tcl man} contents.htm {Tcl man} contents.htm {Tcl manual} contents.htm {tcl manual} contents.htm
	}

	proc pubm {nick uhost hand chan text} {
		variable cmds
		if {([string match {*\?} $text] || [string match {\?*} $text]) && [info exists cmds([set text [string trim $text {? }]])] && [channel get $chan tclman]} {
			variable url
			puthelp "PRIVMSG $chan :\002$text\002 -> $url$cmds($text)"
		}
	}
	setudef flag tclman
	bind pubm - * ::tclman::pubm
}
package provide tclman 0.3
putlog {Tcl Manual public search v0.3 by leprechau and FireEgl - LOADED!}