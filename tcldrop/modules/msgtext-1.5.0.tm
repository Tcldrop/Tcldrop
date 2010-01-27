# msgtext.tcl --
#
#	This file defines various procedures which implement a
#	message catalog facility for Tcl programs.  It should be
#	loaded with the command "package require msgtext".
#
# Copyright (c) 1998-2000 by Ajuba Solutions.
# Copyright (c) 1998 by Mark Harrison.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

# Notes:
# This is based on msgcat, and should be compatible with msgcat if used as a msgcat replacement.
# Support for multiple fallback locales was added. (Such as having "bg_bg bg fr en_us en")
# And more is planned...

# TODO:
# 1. Add a parsed or semi-parsed format for .msg files (but use a different extension for the new format).
#	a. Perhaps a dict compatible format for the files, so they can be merged with the current $msgs, like:
#		set msgs [dict merge $msgs [read $fileid]]
# 2. The ability to create/save .msg (or other format) files based on the "C" strings.
#	a. This will serve translators by giving them some files to edit rather than create from scratch.
# 3. Support registering "unknown" handlers in each namespace for strings we can't find a translation for.

package require Tcl 8.5
package provide msgtext 1.5.0
namespace eval ::msgtext {
	namespace export mc mcload mclocale mcmax mcmset mcpreferences mcset mcunknown mcclock msgtext
	#namespace ensemble create -command msgtext -map [list mc mc load load locale mclocale max mcmax preferences mcpreferences set mcset unknown mcunknown]

	# Records the locales passed to mclocale/mclocales:
	variable locales [list]

	# Records the mapping between source strings and translated strings.  The
	# dict key is of the form "<locale> <namespace> <src>", where locale and
	# namespace should be themselves dict values and the value is
	# the translated string.
	variable msgs [dict create]

	# Map of language codes used in Windows registry to those of ISO-639
	if {$::tcl_platform(platform) eq {windows}} {
		variable WinRegToISO639
		array set WinRegToISO639 {
			01 ar 0401 ar_SA 0801 ar_IQ 0c01 ar_EG 1001 ar_LY 1401 ar_DZ
				1801 ar_MA 1c01 ar_TN 2001 ar_OM 2401 ar_YE 2801 ar_SY
				2c01 ar_JO 3001 ar_LB 3401 ar_KW 3801 ar_AE 3c01 ar_BH
				4001 ar_QA
			02 bg 0402 bg_BG
			03 ca 0403 ca_ES
			04 zh 0404 zh_TW 0804 zh_CN 0c04 zh_HK 1004 zh_SG 1404 zh_MO
			05 cs 0405 cs_CZ
			06 da 0406 da_DK
			07 de 0407 de_DE 0807 de_CH 0c07 de_AT 1007 de_LU 1407 de_LI
			08 el 0408 el_GR
			09 en 0409 en_US 0809 en_GB 0c09 en_AU 1009 en_CA 1409 en_NZ
				1809 en_IE 1c09 en_ZA 2009 en_JM 2409 en_GD 2809 en_BZ
				2c09 en_TT 3009 en_ZW 3409 en_PH
			0a es 040a es_ES 080a es_MX 0c0a es_ES@modern 100a es_GT 140a es_CR
				180a es_PA 1c0a es_DO 200a es_VE 240a es_CO 280a es_PE
				2c0a es_AR 300a es_EC 340a es_CL 380a es_UY 3c0a es_PY
				400a es_BO 440a es_SV 480a es_HN 4c0a es_NI 500a es_PR
			0b fi 040b fi_FI
			0c fr 040c fr_FR 080c fr_BE 0c0c fr_CA 100c fr_CH 140c fr_LU
				180c fr_MC
			0d he 040d he_IL
			0e hu 040e hu_HU
			0f is 040f is_IS
			10 it 0410 it_IT 0810 it_CH
			11 ja 0411 ja_JP
			12 ko 0412 ko_KR
			13 nl 0413 nl_NL 0813 nl_BE
			14 no 0414 no_NO 0814 nn_NO
			15 pl 0415 pl_PL
			16 pt 0416 pt_BR 0816 pt_PT
			17 rm 0417 rm_CH
			18 ro 0418 ro_RO
			19 ru
			1a hr 041a hr_HR 081a sr_YU 0c1a sr_YU@cyrillic
			1b sk 041b sk_SK
			1c sq 041c sq_AL
			1d sv 041d sv_SE 081d sv_FI
			1e th 041e th_TH
			1f tr 041f tr_TR
			20 ur 0420 ur_PK 0820 ur_IN
			21 id 0421 id_ID
			22 uk 0422 uk_UA
			23 be 0423 be_BY
			24 sl 0424 sl_SI
			25 et 0425 et_EE
			26 lv 0426 lv_LV
			27 lt 0427 lt_LT
			28 tg 0428 tg_TJ
			29 fa 0429 fa_IR
			2a vi 042a vi_VN
			2b hy 042b hy_AM
			2c az 042c az_AZ@latin 082c az_AZ@cyrillic
			2d eu
			2e wen 042e wen_DE
			2f mk 042f mk_MK
			30 bnt 0430 bnt_TZ
			31 ts 0431 ts_ZA
			33 ven 0433 ven_ZA
			34 xh 0434 xh_ZA
			35 zu 0435 zu_ZA
			36 af 0436 af_ZA
			37 ka 0437 ka_GE
			38 fo 0438 fo_FO
			39 hi 0439 hi_IN
			3a mt 043a mt_MT
			3b se 043b se_NO
			043c gd_UK 083c ga_IE
			3d yi 043d yi_IL
			3e ms 043e ms_MY 083e ms_BN
			3f kk 043f kk_KZ
			40 ky 0440 ky_KG
			41 sw 0441 sw_KE
			42 tk 0442 tk_TM
			43 uz 0443 uz_UZ@latin 0843 uz_UZ@cyrillic
			44 tt 0444 tt_RU
			45 bn 0445 bn_IN
			46 pa 0446 pa_IN
			47 gu 0447 gu_IN
			48 or 0448 or_IN
			49 ta
			4a te 044a te_IN
			4b kn 044b kn_IN
			4c ml 044c ml_IN
			4d as 044d as_IN
			4e mr 044e mr_IN
			4f sa 044f sa_IN
			50 mn
			51 bo 0451 bo_CN
			52 cy 0452 cy_GB
			53 km 0453 km_KH
			54 lo 0454 lo_LA
			55 my 0455 my_MM
			56 gl 0456 gl_ES
			57 kok 0457 kok_IN
			58 mni 0458 mni_IN
			59 sd
			5a syr 045a syr_TR
			5b si 045b si_LK
			5c chr 045c chr_US
			5d iu 045d iu_CA
			5e am 045e am_ET
			5f ber 045f ber_MA
			60 ks 0460 ks_PK 0860 ks_IN
			61 ne 0461 ne_NP 0861 ne_IN
			62 fy 0462 fy_NL
			63 ps
			64 tl 0464 tl_PH
			65 div 0465 div_MV
			66 bin 0466 bin_NG
			67 ful 0467 ful_NG
			68 ha 0468 ha_NG
			69 nic 0469 nic_NG
			6a yo 046a yo_NG
			70 ibo 0470 ibo_NG
			71 kau 0471 kau_NG
			72 om 0472 om_ET
			73 ti 0473 ti_ET
			74 gn 0474 gn_PY
			75 cpe 0475 cpe_US
			76 la 0476 la_VA
			77 so 0477 so_SO
			78 sit 0478 sit_CN
			79 pap 0479 pap_AN
		}
	}
	# Initialize the unknowns list (it's what stores the strings we don't have a translation for):
	variable Unknowns
	if {![info exists Unknowns]} { array set Unknowns {} }
	variable unknown
	if {![info exists unknown]} { array set unknown {} }
}

proc ::msgtext::mcexpand {{locales {}}} {
	# Expand the locales so that, for example, "en_us" will also include "en":
	foreach l $locales {
		set word ""
		foreach part [split $l {_}] {
			# Make sure "$word" is not already in the list, and then find the last match for "$word_*" and insert it after that:
			if {[set word [string trimleft "${word}_${part}" {_}]] ni $locales} {
				set locales [linsert $locales [lindex [lsearch -all -glob $locales "${word}_*"] end]+1 $word]
			}
		}
	}
	return $locales
}

# ::msgtext::mc --
#
#	Find the translation for the given string based on the current
#	locale setting. Check the local namespace first, then look in each
#	parent namespace until the source is found.  If additional args are
#	specified, use the format command to work them into the traslated
#	string.
#
# Arguments:
#	src	The string to translate.
#	args	Args to pass to the format command
#
# Results:
#	Returns the translated string.  Propagates errors thrown by the
#	format command.
proc ::msgtext::mc {src args} {
	variable locales
	mcwithlocales [uplevel 1 [list ::namespace current]] $locales $src {*}$args
}

# Works like mc, but it tells it a list of locales to use:
# FixMe: Pick a better name for this than "mcwithlocales". =P
proc ::msgtext::mcwithlocales {ns locales src args} {
	set origns $ns
	# Check for the src in each namespace starting from the local and ending in the global.
	variable msgs
	while {$ns != {}} {
		foreach l $locales {
			if {[dict exists $msgs $l $ns $src]} {
				if {[llength $args] == 0} {
					return [dict get $msgs $l $ns $src]
				} else {
					return [format [dict get $msgs $l $ns $src] {*}$args]
				}
			}
		}
		set ns [namespace parent $ns]
	}
	# We did not find the translation.
	# Try to find a registered unknown handler:
	variable unknown
	set ns $origns
	while {$ns != {}} {
		if {[info exists unknown($ns)]} {
			return [uplevel 1 [list $unknown($ns) $origns $locales $src {*}$args]]
		}
		set ns [namespace parent $ns]
	}
	# Fallback to the original mcunknown:
	uplevel 1 [list [namespace origin mcunknown] [lindex $locales 0] $src {*}$args]
}

# Adds support for multiple fallback locales to msgtext:
# This replaces any previous locales that are set.
proc ::msgtext::mclocales {args} {
	switch -- [llength $args] {
		{0} {
			# No arguments, just return what we have set:
			variable locales
			return $locales
		}
		{1} {
			# One argument, set locales to it:
			set newlocales [string tolower [lindex $args 0]]
		}
		{default} {
			# Multiple arguments, assume it's a list of locales:
			set newlocales [string tolower $args]
		}
	}
	variable locales {}
	foreach l $newlocales {
		switch -- $l {
			{} - {{}} - {""} - { } {
				# Ignore {}, and other junk people might put in.
			}
			{default} {
				# Safety check + convert locale to a msgtext format + duplicate check:
				if {$l eq [file tail $l] && [set l [convertlocale $l]] ni $locales} {
					lappend locales $l
				}
			}
		}
	}
	set locales [mcexpand $locales]
	# Add "" (empty) to the end of the list:
	lappend locales {}
}

# ::msgtext::mclocale --
#
#	Query or set the current locale.
#
# Arguments:
#	newLocale	(Optional) The new locale string. Locale strings
#			should be composed of one or more sublocale parts
#			separated by underscores (e.g. en_US).
#
# Results:
#	Returns the current locale.
proc ::msgtext::mclocale {args} {
	switch -- [llength $args] {
		{1} {
			# Prepend the locale to the current list of locales and return the first one:
			variable locales
			lindex [mclocales {*}[concat [mcexpand $args] $locales]] 0
		}
		{0} {
			# Just return the first one:
			variable locales
			lindex $locales 0
		}
		{default} {
			# Set all the locales we're given, and return them all (expanded):
			mclocales {*}$args
		}
	}
}

# Does [clock format] with the locale set in LC_ALL LC_TIME or other envs:
proc ::msgtext::mcclock {time args} {
	global env
	foreach v {LC_ALL LC_TIME LANG LANGUAGE LOCALE} {
		if {[info exists env($v)] && $env($v) ne {}} {
			return [clock format $time {*}$args -locale $env($v)]
		}
	}
}


# ::msgtext::mcpreferences --
#
#	Fetch the list of locales used to look up strings, ordered from
#	most preferred to least preferred.
#
# Arguments:
#	None.
#
# Results:
#	Returns an ordered list of the locales preferred by the user.
proc ::msgtext::mcpreferences {} {
	variable locales
	return $locales
}

# ::msgtext::mcload --
#
#	Attempt to load message catalogs for each locale in the
#	preference list from the specified directory.
#
# Arguments:
#	langdir		The directory to search.
#
# Results:
#	Returns the number of message catalogs that were loaded.
proc ::msgtext::mcload {langdir} {
	set count 0
	variable locales
	foreach p $locales {
		if {$p eq {}} { set p {ROOT} }
		if {[file exists [set langfile [file join $langdir "$p.msg"]]]} {
			uplevel 1 [list ::source -encoding utf-8 $langfile]
			incr count
		}
	}
	return $count
}

# ::msgtext::mcset --
#
#	Set the translation for a given string in a specified locale.
#
# Arguments:
#	locale		The locale to use.
#	src		The source string.
#	dest		(Optional) The translated string.  If omitted,
#			the source string is used.
#
# Results:
#	Returns the new locale.
proc ::msgtext::mcset {locale src {dest ""}} {
	variable msgs
	# FixMe: Why [llength [info level 0]] == 3 and not $dest eq "" here?
	if {[llength [info level 0]] == 3} {
		# dest not specified
		set dest $src
	}
	set ns [uplevel 1 [list ::namespace current]]
	set locale [string tolower $locale]
	# create nested dictionaries if they do not exist
	# FixMe: I don't think that's necessary, is it?
	#if {![dict exists $msgs $locale]} { dict set msgs $locale [dict create] }
	#if {![dict exists $msgs $locale $ns]} { dict set msgs $locale $ns [dict create] }
	dict set msgs $locale $ns $src $dest
	return $dest
}

# ::msgtext::mcmset --
#
#	Set the translation for multiple strings in a specified locale.
#
# Arguments:
#	locale		The locale to use.
#	pairs		One or more src/dest pairs (must be even length)
#
# Results:
#	Returns the number of pairs processed
proc ::msgtext::mcmset {locale pairs } {
	variable msgs
	if {[set length [llength $pairs]] % 2} {
		return -code error "Bad translation list: should be \"[lindex [info level 0] 0] locale {src dest ...}\""
	}
	set locale [string tolower $locale]
	set ns [uplevel 1 [list ::namespace current]]
	# create nested dictionaries if they do not exist
	# FixMe: I don't think it's necessary to create empty dicts, is it?
	#if {![dict exists $Msgs $locale]} { dict set Msgs $locale [dict create] }
	#if {![dict exists $Msgs $locale $ns]} { dict set Msgs $locale $ns [dict create] }
	foreach {src dest} $pairs { dict set msgs $locale $ns $src $dest }
	return $length
}

# ::msgtext::mcunknown --
#
#	This routine is called by ::msgtext::mc if a translation cannot
#	be found for a string.  This routine is intended to be replaced
#	by an application specific routine for error reporting
#	purposes.  The default behavior is to return the source string.
#	If additional args are specified, the format command will be used
#	to work them into the traslated string.
#
# Arguments:
#	locale		The current locale.
#	src		The string to be translated.
#	args		Args to pass to the format command
#
# Results:
#	Returns the translated value.
proc ::msgtext::mcunknown {locale src args} { if {[llength $args]} { format $src {*}$args } else { return $src } }

# ::msgtext::mcmax --
#
#	Calculates the maximum length of the translated strings of the given list.
#
# Arguments:
#	args	strings to translate.
#
# Results:
#	Returns the length of the longest translated string.
proc ::msgtext::mcmax {args} {
	set max 0
	foreach string $args {
		set translated [uplevel 1 [list [namespace origin mc] $string]]
		set len [string length $translated]
		if {$len > $max} { set max $len }
	}
	return $max
}

# Convert the locale values stored in environment variables to a form
# suitable for passing to [mclocale]
proc ::msgtext::convertlocale {value} {
	# Assume $value is of form: $language[_$territory][.$codeset][@modifier]
	# Convert to form: $language[_$territory][_$modifier]
	#
	# Comment out expanded RE version -- bugs alleged
	# regexp -expanded {
	#	^		# Match all the way to the beginning
	#	([^_.@]*)	# Match "lanugage"; ends with _, ., or @
	#	(_([^.@]*))?	# Match (optional) "territory"; starts with _
	#	([.]([^@]*))?	# Match (optional) "codeset"; starts with .
	#	(@(.*))?	# Match (optional) "modifier"; starts with @
	#	$		# Match all the way to the end
	# } $value -> language _ territory _ codeset _ modifier
	if {![regexp {^([^_.@]+)(_([^.@]*))?([.]([^@]*))?(@(.*))?$} $value -> language _ territory _ codeset _ modifier]} {
		return -code error "Invalid locale '$value': empty language part"
	}
	set ret $language
	if {[string length $territory]} { append ret "_$territory" }
	if {[string length $modifier]} { append ret "_$modifier" }
	return $ret
}

# Initialize the default locale(s):
proc ::msgtext::mcinit {{locales {}}} {
	# set default locale, try to get from environment global env
	foreach v {LC_ALL LC_MESSAGES LANG LANGUAGE LOCALE LANGUAGES LANGS LINGUAS} {
		if {[info exists ::env($v)] && $::env($v) ne {}} {
			foreach l [split [string tolower $::env($v)] {: ;,}] {
				if {$l ni $locales} { lappend locales $l }
			}
		}
	}
	# On Darwin, fallback to current CFLocale identifier if available.
	if {$::tcl_platform(os) eq "Darwin" && $::tcl_platform(platform) eq "unix" && [info exists ::tcl::mac::locale] && $::tcl::mac::locale ne ""} {
		lappend locales $::tcl::mac::locale
	} elseif { $::tcl_platform(platform) eq "windows" } {
		# On Windows, try to set locale depending on registry settings.
		if {![catch { package require registry }] && ![catch { registry get {HKEY_CURRENT_USER\Control Panel\International} {locale} } locale]} {
			# Keep trying to match against smaller and smaller suffixes
			# of the registry value, since the latter hexadigits appear
			# to determine general language and earlier hexadigits determine
			# more precise information, such as territory.  For example,
			#	 0409 - English - United States
			#	 0809 - English - United Kingdom
			# Add more translations to the WinRegToISO639 array above.
			variable WinRegToISO639
			set locale [string tolower $locale]
			while {[string length $locale]} {
				if {[info exists WinRegToISO639($locale)]} {
					set locales $WinRegToISO639($locale)
					# Found a match, break.
					break
				}
				set locale [string range $locale 1 end]
			}
		}
	}
	# EGG_LANG is a last resort.. (FixMe: Move this back into Tcldrop)
	#variable NamesToISO639
	#if {[info exists env(EGG_LANG)] && $env(EGG_LANG) ne {} && [info exists NamesToISO639([string tolower $env(EGG_LANG)])]} {
	#	if {[set l [string tolower $NamesToISO639([string tolower $env(EGG_LANG)])]] ni $locales} {
	#		lappend locales $l
	#	}
	#}
	# We use all the languages we found:
	mclocales $locales
}

# This will take any unknown locales and put them into msgs/ROOT.msg (or something..undecided).
proc ::msgtext::mcnsunknown {ns locales src args} {
	variable unknowns
	if {$src ni $unknowns} {
		# Log it once, then save it to the unknowns list:
		catch { putdebuglog "::msgtext::mcunknown: $locale $src $args" }
		lappend unknowns $src
	}
	if {[llength $args]} { format $src {*}$args } else { return $src }
}

proc ::msgtext::mcunknowns {} {
	variable unknowns
	return $unknowns
}

::msgtext::mcinit
