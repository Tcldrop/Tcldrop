# pubsafetcl.tcl --
#
# Copyright (C) 2004,2005,2006,2007,2008,2009 by Philip Moore <FireEgl@Tcldrop.US>
# This code may be distributed under the same terms as Tcl.
#
# RCS: @(#) $Id$
#
# Provides a safe Tcl interpreter that can be used without the worry of infinite loops.
# See "man n interp" and "man n safe" for more info on safe interpreters.
#
# Usage:
#
#	pubsafetcl::create interpname ?-option value? ...
#

if {![info exists ::auto_path]} { set ::auto_path [list] }

namespace eval pubsafetcl {
	package require Tcl 8.5
	variable version {2.2.0}
	variable name {pubsafetcl}
	variable author {FireEgl}
	variable description {Provides a public safe Tcl interpreter, free from infinite loops.}
	variable script [info script]
	package provide pubsafetcl $version
	variable rcsid {$Id$}

	proc Reset {{interp {safetcl}} args} { return "Reset [create $interp]." }

	# This proc initializes/resets the safe interpreter, and basically does stuff to make it even safer or more complete:
	# Note: this interp have to be a direct slave, that means not nested.
	# TODO: we should move the procs out of this proc, so they are not redefined if a new interp is created. Maybe we should create a namespace for that, and hide/alias in a foreach, warning: do not hide file
	# TODO: We should discuss if we allow tm. We hide glob...
	proc create {{interp {safetcl}} args} {
		if {[llength $interp] > 1} {
			return -code error "Nested interpreters are not supported"
		}
		if {[interp exists $interp]} {
			catch { safe::interpConfigure $interp -deleteHook {} }
			# Prefer to use safe::interpDelete but fall back to interp delete if safe::interpDelete happens to be broken:
			if {[catch { safe::interpDelete $interp }]} { catch { interp delete $interp } }
		}
		# This is a hack to make it search sub directories.. (Cuz doing -nestedLoadOk doesn't work..) -- -nestedLoadOk is for loading in a slave interp , e.g. load foo.so foo slave
		# And I suggest to use safe::AddSubDirs
		set subpaths $::auto_path
		if {[set p [lsearch -exact $subpaths {/usr/lib}]] != -1} { set subpaths [lreplace $subpaths $p $p] }
		foreach p $subpaths { set subpaths [concat $subpaths [glob -directory $p -types {d} -nocomplain *]] }
		if {[catch { safe::interpCreate $interp -deleteHook [list [namespace current]::create $interp] -noStatics -nestedLoadOk -accessPath $subpaths -nested 1 }]} {
			if {[catch { interp create -safe -- $interp }]} {
				return {}
			} else {
				catch { safe::interpInit $interp -deleteHook [list [namespace current]::create $interp] -noStatics -nestedLoadOk -accessPath $subpaths -nested 1 }
			}
		}
		catch { safe::interpConfigure $interp -noStatics -nestedLoadOk -accessPath $subpaths -deleteHook [list [namespace current]::create $interp] -nested 1 }
		# We shoud build our pkgindex, otherwise it will not work.
		# Note: the user can do this with many calls to for {} {$i < [llength $auto_path]} {incr i} {catch {source [file join [llindex $auto_path] pkgIndex.tcl}}
		#catch { $interp eval { package require tcllib } }
		# We have to hide these commands cuz they let people do nasty things:
		foreach c {after vwait trace} { catch { $interp hide $c } }
		# I moved the code for the interp initalisation to safeinit.tcl
		$interp invokehidden source [file join [file dirname [info script]] safeinit.tcl]
		# We want to make some harmless Eggdrop Tcl commands available:
		foreach c {duration ctime strftime encrypt decrypt encpass unames md5 sha1 getchanlaston dccused getchanidle myip flushmode queuesize traffic inchain haschanrec wasop getting-users botisvoice modules islinked countusers validchan validuser finduser ischanjuped isban ispermban isexempt ispermexempt isinvite isperminvite isbansticky isexemptsticky isinvitesticky matchban matchexempt matchinvite isignore channame2dname chandname2name isbotnick botonchan isop isvoice onchan nick2hand hand2nick handonchan ischanban ischanexempt ischaninvite getchanjoin onchansplit valididx idx2hand maskhost hand2idx washalfop topic getchanhost isdynamic isbotnetnick getinfo realtime stripcodes} {
			if {[info command $c] != {}} { if {[interp eval $interp [list info commands $c]] == {}} { interp alias $interp $c {} $c } else { interp alias $interp "${c}-eggdrop" {} $c } }
		}

		# And these we want to alias to reset: -- exit was an alias to ::safe::interpDelete
		foreach c {exit die rehash restart reset quit reload} {
			interp alias $interp $c {} [namespace current]::Reset $interp
		}

		# This is defined in every proc, but maybe we should set a timelimit
		proc Timeout {interp args} { update
			if {[info exists [namespace current]::${interp}::Cancel]} { return -code error "Timed out after [set [namespace current]::${interp}::Cancel]ms." }
		}
		interp alias $interp timeout {} [namespace current]::Timeout $interp
		
		# This proc is needed as command prefix for our traces
		proc PermissionDenied {interp args} {
			return -code error "Permission denied"
		}
		interp alias $interp permissionDenied {} [namespace current]::PermissionDenied $interp

		# We want to take out the timeout part of the body
		# TODO: Maybe we should replace ::tcl::info::body - since 8.5 info is an ensemble
		#interp hide $interp info
		#proc Info {interp option args} {
			#switch -glob -- $option {
				#{b*} {
					#if {[set code [catch { eval [linsert $args 0 interp invokehidden $interp info $option] } out]]} {
						#return -code $code $out
					#} elseif {[string match {timeout ; *} $out]} {
						# We take out the "timeout ;" part of the body..
						#string range $out 10 end
					#} else {
						#set out
					#}
				#}
				#{default} { eval [linsert $args 0 interp invokehidden $interp info $option] }
			#}
		#}
		#interp alias $interp info {} [namespace current]::Info $interp
		
		interp eval $interp rename ::tcl::info::body ::InfoBody
		interp hide $interp InfoBody
		proc InfoBody {interp procname} {
			if {[set code [catch {interp invokehidden $interp InfoBody $procname} out opt]]} {
				return -code $code -options $opt $out
			} elseif {[string match {timeout ; *} $out]} {
				# We take out the "timeout ;" part of the body..
				string range $out 10 end
			} else {
				set out
			}
		}
		interp alias $interp ::tcl::info::body {} [namespace current]::InfoBody $interp

		# We hafta provide the limited file command since safe::interpCreate is b0rked...
		# This is borked too, [file join [file dirname ~] [file tail ~]] -- Johannes13 
		if {[lsearch -exact [interp hidden $interp] file] == -1} { catch { interp hide $interp file } }
		proc File {interp option args} {
			switch -glob -- [string tolower "$option"] {
				{ch*} { set option channels }
				{di*} { set option dirname }
				{ext*} { set option extension }
				{j*} { set option join }
				{na*} { set option nativename }
				{no*} { set option normalize }
				{ro*} { set option rootname }
				{se*} { set option separator }
				{sp*} { set option split }
				{ta*} { set option tail }
				{default} { return -code error "file $option is not allowed!" }
		}
			eval [linsert [split $args] 0 interp invokehidden $interp file $option]
		}
		interp alias $interp file {} [namespace current]::File $interp

		# This provides reasonable emulation of the puts command:
		# Putting to stdout makes it save it to a variable, which can be processed later.
		# TODO: what about reflected channels? -- Johannes13
		interp hide $interp puts
		proc Puts {interp args} {
			switch -- [llength $args] {
				{3} {
					switch -- [lindex $args 0] {
						{-nonewline} { set newline {} }
						{default} { return -code error {wrong # args: should be "puts ?-nonewline? ?channelId? string"} }
					}
					set socket [lindex $args 1]
					set text [lindex $args 2]
				}
				{2} {
					switch -- [lindex $args 0] {
						{-nonewline} {
							set newline {}
							set socket {stdout}
						}
						{default} {
							set newline {}
							set socket [lindex $args 0]
						}
					}
					set text [lindex $args 1]
				}
				{1} {
					set newline "\n"
					set socket {stdout}
					set text [lindex $args 0]
				}
				{default} { return -code error {wrong # args: should be "puts ?-nonewline? ?channelId? string"} }
			}
			if {[string is alnum -strict $socket] && [string length $socket] < 32} {
				switch -glob -- $socket {
					{sock*} - {file*} {
						## Try to send to a real socket.
						#set error [catch { eval [linsert $args 0 interp invokehidden $interp puts] } return]
						#update idletasks
						#return -code $error $return
						return -code error "can not find channel named \"$socket\""
					}
					{default} { lappend [namespace current]::${interp}::puts $socket "$text$newline" }
				}
				return {}
			} else {
				return -code error {bad channelId.  =P}
			}
		}
		interp alias $interp puts {} [namespace current]::Puts $interp

		proc PutLogLev {interp level channel text} {
			if {![string is alnum -strict $level] || [string length $level] > 32} {
				return -code error {Invalid log level. =P}
			} elseif {![string is alnum -strict $channel] || [string length $channel] > 254} {
				return -code error {Bad channel name.  =P}
			} else {
				lappend [namespace current]::${interp}::putloglev $level $channel $text
				return {}
			}
		}
		interp alias $interp putloglev {} [namespace current]::PutLogLev $interp

		interp hide $interp while
		proc While {interp condition body} { interp invokehidden $interp while $condition "timeout ; $body" }
		interp alias $interp while {} [namespace current]::While $interp

		interp hide $interp for
		proc For {interp start test next command} { interp invokehidden $interp for $start $test $next "timeout ; $command" }
		interp alias $interp for {} [namespace current]::For $interp

		# Workaround for bug in 8.4.1 and older that causes Seg Faults:
		# Note: This code is imperfect, so it's been commented out,
		#       be sure you're using a Tcl version > 8.4.1
		#proc Lsearch {interp args} {
		#	switch -- [llength $args] {
		#		{3} {
		#			set options [lindex $args 0]
		#			set list [lindex $args 1]
		#			set pattern [lindex $args 2]
		#		}
		#		{2} {
		#			set options {-glob}
		#			set list [lindex $args 0]
		#			set pattern [lindex $args 1]
		#		}
		#		{default} {
		#			eval interp invokehidden $interp lsearch $args
		#		}
		#	}
		#	if {[info tclversion] < {8.5} && [lsearch [split $options] {-regexp}] != -1 && $list == $pattern} {
		#		interp invokehidden $interp lsearch $options [string range $list 0 end] $pattern
		#	} else {
		#		interp invokehidden $interp lsearch $options $list $pattern
		#	}
		#}
		#interp alias $interp lsearch {} [namespace current]::Lsearch $interp

		# Interp is safe, even in a safe interp. This means invokehidden and marktrusted etc. are not allowed. Also to change the resource limits
		interp hide $interp interp
		proc Interp {interp cmd args} {
			switch -glob -- [string tolower $cmd] {
				{alias} { if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft [lindex $args 1] :]] != -1} { return -code error "You can't alias initial commands!  =P" } }
				{is*} { eval [linsert $args 0 interp invokehidden $interp interp $cmd] }
				{default} { return -code error "interp is not allowed! =P" }
			}
			eval [linsert $args 0 interp invokehidden $interp interp $cmd]
		}
		interp alias $interp interp {} [namespace current]::Interp $interp

		interp hide $interp rename
		proc Rename {interp oldName newName} {
			if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft $oldName :]] != -1} {
				return -code error "Attempt to rename command DENIED!  =P"
			} elseif {[set size [string length $newName]] > 400} {
				return -code error "You can't have a proc name that long!  (Needed: $size Allowed: 400)"
			} else {
				interp invokehidden $interp rename $oldName $newName
			}
		}
		interp alias $interp rename {} [namespace current]::Rename $interp

		# We should replace ::tcl::string::repeat
		#interp hide $interp string
		#proc String {interp args} {
			#if {[string match {repe*} [lindex $args 0]] && ([set size [expr { [lindex $args end] * [string length [lindex $args end-1]] }]] > 400)} {
				#return -code error "You can't repeat a string that long, that many times!  (Needed: $size  Allowed: 400)"
			#} else {
				#eval [linsert $args 0 interp invokehidden $interp string]
			#}
		#}
		#interp alias $interp string {} [namespace current]::String $interp
		
		# We require Tcl 8.5, so this is safe
		# rename is already replaced, so we have to use invokehidden, because InitialCommands are not set yet.
		interp invokehidden $interp rename ::tcl::string::repeat ::StringRepeat
		interp hide $interp StringRepeat
		proc StringRepeat {interp string count} {
			if {[set size [expr { $count * [string length $string]}]] > 400} {
				return -code error "You can't repeat a string that long, that many times!  (Needed: $size  Allowed: 400)"
			}
			return [interp invokehidden $interp StringRepeat $string $count]
		}
		interp alias $interp ::tcl::string::repeat {} [namespace current]::StringRepeat $interp

		interp hide $interp proc
		proc Proc {interp name arguments body} {
			if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft $name :]] != -1} {
				error "Attempt to (re)define command DENIED!  =P"
			} elseif {[set size [string length $name]] > 400} {
				return -code error "You can't have a proc name that long!  (Needed: $size Allowed: 400)"
			} elseif {[set size [string length [join $arguments]]] > 400} {
				return -code error "You can't have that many arguments!  (Needed: $size Allowed: 400)"
			} elseif {[set size [string length $body]] > 2048} {
				return -code error "You can't have a body that large!  (Needed: $size Allowed: 2048)"
			} else {
				interp invokehidden $interp proc $name $arguments "update idletasks ; $body"
			}
		}
		interp alias $interp proc {} [namespace current]::Proc $interp

		interp hide $interp namespace
		proc Namespace {interp subcommand args} {
			switch -glob -- $subcommand {
				{im*} { if {[lsearch -exact $args {-force}] != -1} { return -code error {Denied! =P  -force not allowed.} } }
				{d*} {
					foreach ns $args {
						switch -- [string trim $ns {: 	}] {
							{} {
								after idle [list after 0 [list [namespace current]::create $interp]]
								return "Resetting $interp ..."
							}
						}
					}
				}
			}
			eval [linsert $args 0 interp invokehidden $interp namespace $subcommand]
		}
		interp alias $interp namespace {} [namespace current]::Namespace $interp

		# Wrapper to prevent a segfault in Tcl <8.4.19 and <8.5.2
		interp hide $interp binary
		proc Binary {interp option args} {
			if {$option eq {format} && [string match -nocase {*x0?*} [lindex $args 0]]} {
				return -code error "Crash attempt DENIED! =P"
			} else {
				eval [linsert $args 0 interp invokehidden $interp binary $option]
			}
		}
		interp alias $interp binary {} [namespace current]::Binary $interp

		interp hide $interp time
		variable TimeNoticeLast 9
		proc Time {interp args} {
			if {[llength $args] == 2 && [lindex $args end] > 10000} {
				return -code error "[lindex $args end] is too high, use 10000 or less."
			} else {
				set notice 0
				set error 0
				switch -glob -- $args {
					{*file *} - {*for *} - {*info *} - {*interp *} - {*namespace *} - {*proc *} - {*puts *} - {*rename *} - {binary *} - {*string *} - {*while *} - {*reset *} - {*die *} - {*exit *} {
						# Always send the annoying notice for these commands.
						set error 1
					}
					{default} {
						# Only send the annoying notice once every 999 seconds otherwise.
						variable TimeNoticeLast
						if {[clock seconds] - $TimeNoticeLast > 99} {
							set TimeNoticeLast [clock seconds]
							set notice 1
						}
					}
				}
				if {$error} {
					interp eval $interp { return -code error {The following commands have extra overhead in pubsafetcl that makes their times inaccurate: file, for, info, interp, namespace, proc, puts, rename, binary, string, time, while.  You should time these commands from your own tclsh instead! } }
				} elseif {$notice} {
					interp eval $interp { puts NOTICE {The following commands have extra overhead in pubsafetcl that makes their times inaccurate: file, for, info, interp, namespace, proc, puts, rename, binary, string, time, while.  You should time these commands from your own tclsh instead!} }
				}
				eval [linsert $args 0 interp invokehidden $interp time]
			}
		}
		interp alias $interp time {} [namespace current]::Time $interp

		interp hide $interp set
		proc Set {interp varName {newValue {}}} {
			if {[set size [string length $varName]] > 100} {
				return -code error "You can't have a variable name that long!  (Needed: $size Allowed: 100)"
			} elseif {[set size [string length $newValue]] > 2700} {
				return -code error "You can't set a variable that long!  (Needed: $size Allowed: 2700)"
			} else {
				if {[llength [info level 0]] <= 3} {
					interp invokehidden $interp set $varName
				} else {
					interp invokehidden $interp set $varName $newValue
				}
			}
		}
		interp alias $interp set {} [namespace current]::Set $interp

		interp hide $interp array
		proc Array {interp option arrayName args} {
			if {[string match -nocase {se*} $option]} {
				if {[set size [string length $arrayName]] > 100} {
					return -code error "You can't have an array name that long!  (Needed: $size Allowed: 100)"
				} elseif {[set size [string length [join $args]]] > 2048} {
					# FixMe: check each key & value pair on their own?
					return -code error "You can't set an array that large!  (Needed: $size Allowed: 2048)"
				}
			}
			eval [linsert $args 0 interp invokehidden $interp array $option $arrayName]
		}
		interp alias $interp array {} [namespace current]::Array $interp

		interp hide $interp append
		proc Append {interp varName args} {
			variable $varName
			if {[info exists $varName]} { set oldLen [string length $varName] } else { set oldLen 0 }
			if {[set size [string length $varName]] > 100} {
				return -code error "You can't have a variable name that long!  (Needed: $size Allowed: 100)"
			} elseif {[set size [expr {[string length [join $args]]+$oldLen}]] > 2048} {
				return -code error "You can't set a variable that long!  (Needed: $size Allowed: 2048)"
			} else {
				eval [linsert $args 0 interp invokehidden $interp append $varName]
			}
		}
		interp alias $interp append {} [namespace current]::Append $interp

		interp hide $interp lappend
		proc Lappend {interp varName args} {
			if {[info exists $varName]} { set oldLen [string length [join $varName]] } else { set oldLen 0 }
			if {[set size [string length $varName]] > 100} {
				return -code error "You can't have a variable name that long!  (Needed: $size Allowed: 100)"
			} elseif {[set size [expr {[string length [join $args]]+$oldLen}]] > 2048} {
				return -code error "You can't set a variable that long!  (Needed: $size Allowed: 2048)"
			} else {
				eval [linsert $args 0 interp invokehidden $interp lappend $varName]
			}
		}
		interp alias $interp lappend {} [namespace current]::Lappend $interp

		interp hide $interp regsub
		proc Regsub {args} {
			set pos 1
			foreach arg [lrange $args 1 end] {
				if {[string index $arg 0] != {-}} {
					break
				} elseif {$arg == {--}} {
					incr pos
					break
				} else {
					incr pos
					lappend switches $arg
				}
			}
			if {![info exists switches]} { set switches {--} }
			switch -exact -- [llength [set list [lrange $args $pos end]]] {
				{3} { lassign $list exp string subSpec; set interp [lindex $args 0]; set varName {} }
				{4} { lassign $list exp string subSpec varName; set interp [lindex $args 0] }
				{default} {
					return -code error {wrong # args: should be "regsub ?switches? exp string subSpec ?varName?"}
				}
			}
			if {[set size [string length $varName]] > 100} {
				return -code error "You can't have a variable name that long!  (Needed: $size Allowed: 100)"
			} elseif {[set size [string length [eval [concat [linsert $switches 0 regsub] [linsert $varName 0 $exp $string $subSpec]]]]] > 2048} {
				return -code error "You can't set a variable that long!  (Needed: $size Allowed: 2048)"
			} else {
				eval [concat [linsert $switches 0 interp invokehidden $interp regsub] [linsert $varName 0 $exp $string $subSpec]]
			}
		}
		interp alias $interp regsub {} [namespace current]::Regsub $interp
		
		interp hide $interp regexp
		proc Regexp {args} {
			set pos 1
			foreach arg [lrange $args 1 end] {
				if {[string index $arg 0] != {-}} {
					break
				} elseif {$arg == {--}} {
					incr pos
					break
				} else {
					incr pos
					lappend switches $arg
				}
			}
			if {![info exists switches]} { set switches {--} }
			switch -exact -- [llength [set list [lrange $args $pos end]]] {
				{0} - {1} { return -code error {wrong # args: should be "regexp ?switches? exp string ?matchVar? ?subMatchVar subMatchVar ...?"} }
				{default} {
					lassign $list exp string
					set interp [lindex $args 0]
					set arg [lrange $list 3 end]
				}
			}
			if {[set size [string length $exp]] > 400} {
				return -code error "Can't evauate a regexp that long.  (Needed: $size Allowed: 400)"
			} elseif {[set size [string length $string]] > 400} {
				return -code error "Can't evaluate a string that long.  (Needed: $size Allowed: 400)"
			} else {
				interp invokehidden $interp regexp {*}$switches $exp $string {*}$arg
			}
		}
		interp alias $interp regexp {} [namespace current]::Regexp $interp

		interp hide $interp scan
		proc Scan {args} {
			lassign $args interp string format
			if {$string eq {} || $format eq {}} {
				return -code error {wrong # args: should be "scan string format ?varName varName ...?"}
			}
			if {[set size [string length $string]] > 2048} {
				return -code error "You can't scan a string that long!  (Needed: $size Allowed: 2048)"
			} else {
				interp invokehidden $interp scan {*}[lrange $args 1 end]
			}
		}
		interp alias $interp scan {} [namespace current]::Scan $interp

		if {[llength [info commands coroutine]]} {
			interp hide $interp coroutine
			proc Coroutine {interp name cmd args} {
				if {[lsearch -exact [set ${interp}::InitialCommands] [string trimleft $name {:}]] != -1 } {
					return -code error {Attempt to rename command DENIED!  =P}
				} elseif {[set size [string length $name]] > 400} {
					return -code error "You can't have a coroutine name that long!  (Needed: $size Allowed: 400)"
				} elseif {[set size [string length $cmd]] > 2048} {
					return -code error "You can't have a coroutine body that long!  (Needed: $size Allowed: 2048)"
				} else {
					interp invokehidden $interp coroutine $name $cmd {*}$args
				}
			}
			interp alias $interp coroutine {} [namespace current]::Coroutine $interp
		}

		# FixMe: Add wrappers for dict, lassign, incr <- possible to increase the size of something with these
		# subst, eval <- possible to eval/subst $a$a$a inside $b$b$b inside $c$c$c etc to create a huge string
		# foreach (poor mans lassign), switch -variable etc can also set vars - yeah -- Johannes13

		# We create a namespace under the current one for storing variables relating to the interp we just created:
		if {[namespace exists [namespace current]::$interp]} { catch { namespace delete [namespace current]::$interp } }
		namespace eval [namespace current]::$interp {
			variable minclicks [clock clicks]
			variable Count 0
			variable minclicks [expr { [clock clicks] - $minclicks + 9 }]
		}
		array set options [list -timelimit 1750]
		array set options $args
		namespace eval [namespace current]::$interp [list variable timeLimit $options(-timelimit)]
		namespace eval [namespace current]::$interp [list variable preEval {}]
		namespace eval [namespace current]::$interp [list variable postEval {}]
		namespace eval [namespace current]::$interp [list variable extraCommands {}]

		namespace eval [namespace current]::$interp [list variable InitialCommands [$interp eval {info commands}]]
		# Prevent abuse/confusion by not allowing commands with certain names:
		# lappend may be already on this list
		namespace eval [namespace current]::$interp [list lappend InitialCommands hi fu i it a the it you hello re wb thx see hey well but or please plz pls how what you are of that can do kthx boring yes no may where is typo muaha rtfm google www search com exe ask okay come op voice q it o k ok in thanks thank thx lol jk nah or in bye nn night stfu wtf omg get go howdy example could give me I Hi How Hello What like nah but well If hmm lmao and he eh heh hehe heheh hehehe heheheh ah ha hah haha hahah hahaha hahahah hes You ffs o_O O_o O_O o_o just Oo oO on off oh but yea yeah like u ah aha ask err wow that it lmfao as er uh um umm uhm gee duh http www db blah its bleh save timer utimer :) =) =P :P :p =p tcl .tcl safetcl .safetcl .eval im {I'm} {} { } {	} "\n" {?} {:} {;} {/} {.} {..} {...} bgerror]
		namespace eval [namespace current]::$interp [list namespace export $interp]

		proc [namespace current]::${interp}::extraCommands {command {extraCommands {}}} {
			switch -- $command {
				{add} {
					variable InitialCommands
					set InitialCommands [lsort -unique [concat $InitialCommands $extraCommands]]
					foreach c $extraCommands {
						if {[catch { pubsafetcl expose $c }]} {
							if {[catch { pubsafetcl invokehidden rename ::$c ::tcl::${c}_orig }]} { catch { pubsafetcl eval rename ::$c ::tcl::${c}_orig } }
							if {[catch { pubsafetcl expose $c }]} { catch { pubsafetcl alias $c $c } }
						}
					}
					variable extraCommands_current $extraCommands
					AddTraces
				}
				{remove} {
					if {$extraCommands == {}} {
						variable extraCommands_current
						set extraCommands $extraCommands_current
					}
					foreach c $extraCommands {
						# First we remove the trace
						catch { pubsafetcl invokehidden -global trace remove command $c {rename delete} permissionDenied }
						catch { pubsafetcl hide $c }
						#if {[catch { pubsafetcl alias $c {} }]} {  }
						if {[catch { pubsafetcl invokehidden rename ::tcl::${c}_orig ::$c }]} { catch { pubsafetcl eval rename ::tcl::${c}_orig ::$c } }
					}
				}
			}
		}

		set [namespace current]::${interp}::extraCommands_current {}

		catch { rename ::$interp [namespace current]::${interp}::pubsafetcl }

		proc [namespace current]::${interp}::$interp {cmd args} { set namespace [namespace current]
			switch -- $cmd {
				{setting} - {set} - {variable} - {var} - {option} - {opt} - {config} - {configure} { variable [lindex $args 0] [lindex $args 1] }
				{fancyeval} {
					variable timeLimit
					set timerid [after $timeLimit [list set ${namespace}::Cancel $timeLimit]]
					# set arg [encoding convertto iso8859-1 $arg]
					variable preEval
					namespace eval $namespace $preEval
					extraCommands remove
					variable extraCommands
					variable puts [list]
					variable putloglev [list]
					variable minclicks
					#extraCommands add $extraCommands
					# FixMe: Increase the recursionlimit as necessary:
					catch { pubsafetcl recursionlimit 7 }
					# Tcl v8.5's resource limits http://tcl.tk/man/tcl8.5/TclCmd/interp.htm#M45
					catch { pubsafetcl limit time -granularity 1 -milliseconds 1000 -seconds [clock seconds] }
					pubsafetcl limit commands -value {}
					# 250 here is the number of commands we'll allow at a time:
					variable CmdCount [expr { [pubsafetcl eval {info cmdcount}] + 250 }]
					catch { pubsafetcl limit commands -value $CmdCount }
					set errlev [catch { set clicks [clock clicks] ; pubsafetcl eval {*}$args } out]
					set clicks [expr { [clock clicks] - $clicks - $minclicks - 9 }]
					variable Cancel
					if {[info exists Cancel]} { unset Cancel } else { after cancel $timerid }
					variable postEval
					namespace eval $namespace $postEval
					if {$errlev == 1} { set results [string map [list ${namespace}::Proc {proc} ${namespace}::Rename {rename} ${namespace}::While {while} ${namespace}::File {file} ${namespace}::For {for} ${namespace}::Lsearch {lsearch} ${namespace}::Interp {interp} ${namespace}::Info {info} ${namespace}::Timeout {timeout} {::safe::AliasLoad} {load}] $out] } else { set results $out }
					#extraCommands remove $extraCommands
					variable Count
					return [list puts $puts putloglev $putloglev results $results errorlevel $errlev clicks $clicks count [incr Count]]
				}
				{eval} {
					variable timeLimit
					set timerid [after $timeLimit [list set ${namespace}::Cancel $timeLimit]]
					# What the hell is ResourceReset? I don't see such a proc
					ResourceReset
					# set arg [encoding convertto iso8859-1 $arg]
					set errlev [catch { pubsafetcl eval {*}$args] } out]
					variable Cancel
					if {[info exists Cancel]} { unset Cancel } else { after cancel $timerid }
					if {$errlev == 1} { return -code error [string map [list ${namespace}::Proc {proc} ${namespace}::Rename {rename} ${namespace}::While {while} ${namespace}::File {file} ${namespace}::For {for} ${namespace}::Lsearch {lsearch} ${namespace}::Interp {interp} ${namespace}::Info {info} ${namespace}::Timeout {timeout} {::safe::AliasLoad} {load}] $out] } else { return $out }
				}
				{default} { eval [linsert $args 0 "${namespace}::pubsafetcl" $cmd] }
			}
		}
		
		# Add traces for each initial command that deny rename and deletion of them
		proc [namespace current]::${interp}::AddTraces args {
			variable InitialCommands
			foreach cmd $InitialCommands {
				# remove first the trace from it
				catch {pubsafetcl invokehidden -global trace remove command $cmd {rename delete} permissionDenied}
				# The command may not exist at this time, so use catch
				catch {pubsafetcl invokehidden -global trace add command $cmd {rename delete} permissionDenied}
			}
		}
		# And now call it
		[namespace current]::${interp}::AddTraces
		namespace eval :: [list namespace import -force [namespace current]::${interp}::$interp]
		set interp
	}
}
