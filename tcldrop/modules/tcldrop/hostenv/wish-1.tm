package provide tcldrop::hostenv::wish 1
	namespace eval ::tcldrop::hostenv::wish {
		namespace path [list ::tcldrop::hostenv ::tcldrop]
		# We're running from wish.
		array set tcldrop [list host_env wish background-mode 0 simulate-dcc 1 channel-stats 1]
		#option add *Text.wrap word widgetDefault
		if {[tk windowingsystem] == {x11}} {
			option add *borderWidth 1 widgetDefault
			option add *activeBorderWidth 1 widgetDefault
			option add *selectBorderWidth 1 widgetDefault
			option add *font -adobe-helvetica-medium-r-normal-*-12-*-*-*-*-*-*
			option add *padX 2
			option add *padY 4
			option add *Listbox.background white
			option add *Listbox.selectBorderWidth 0
			option add *Listbox.selectForeground white
			option add *Listbox.selectBackground #4a6984
			option add *Entry.background white
			option add *Entry.foreground black
			option add *Entry.selectBorderWidth 0
			option add *Entry.selectForeground white
			option add *Entry.selectBackground #4a6984
			option add *Text.background white
			option add *Text.selectBorderWidth 0
			option add *Text.selectForeground white
			option add *Text.selectBackground #4a6984
			option add *Menu.activeBackground #4a6984
			option add *Menu.activeForeground white
			option add *Menu.activeBorderWidth 0
			option add *Menu.highlightThickness 0
			option add *Menu.borderWidth 2
			option add *Menubutton.activeBackground #4a6984
			option add *Menubutton.activeForeground white
			option add *Menubutton.activeBorderWidth 0
			option add *Menubutton.highlightThickness 0
			option add *Menubutton.borderWidth 0
			option add *highlightThickness 0
			option add *troughColor #bdb6ad
			option add *Labelframe.borderWidth 2
			option add *Frame.borderWidth 2
			# These are from roxirc:
			option add *background #c0c0c0 widgetDefault
			option add *activeBackground #c0c0c0 widgetDefault
			option add *highlightBackground #c0c0c0 widgetDefault
			option add *selectBorderWidth 0 widgetDefault
			option add *selectBackground #999999 widgetDefault
			option add *selectColor navy widgetDefault
			option add *highlightThickness 0 widgetDefault
			option add *Toplevel.borderWidth 1 widgetDefault
			option add *Toplevel.relief raised widgetDefault
			option add *Scrollbar.width 13 widgetDefault
			option add *Scrollbar.borderWidth 0 widgetDefault
			option add *Scrollbar.elementBorderWidth 2 widgetDefault
			option add *Menubutton.borderWidth 1 widgetDefault
			option add *Menu.activeBorderWidth 1 widgetDefault
			option add *Menu*Menu.borderWidth 1 widgetDefault
			option add *Text.cursor left_ptr widgetDefault
			option add *Text.highlightThickness 1 widgetDefault
			option add *Entry.highlightThickness 1 widgetDefault
			option add *Button.borderWidth 1 widgetDefault
			option add *Listbox.borderWidth 1 widgetDefault
			option add *menubar.relief raised widgetDefault
			option add *menubar.borderWidth 1 widgetDefault
			option add *text.borderWidth 1 widgetDefault
			option add *n.label.relief raised widgetDefault
		} elseif {$tcl_platform(platform) == {windows} || [tk windowingsystem] == {win32}} {
			option add *padX 0
			option add *padY 0
			option add *borderWidth 0 widgetDefault
			option add *Button.borderWidth 1 widgetDefault
			option add *Text.cursor arrow
			option add *Button.padX 1m widgetDefault
			option add *Button.width 0 widgetDefault
			option add *Menubutton.padY 4 widgetDefault
			option add *Listbox.highlightThickness 0 widgetDefault
			option add *Textborder.borderWidth 0 widgetDefault
			option add *Textborder*Listbox.borderWidth 0 widgetDefault
			option add *Nickborder.borderWidth 0 widgetDefault
			option add *menubar.relief groove widgetDefault
			option add *menubar.borderWidth 2 widgetDefault
			option add *text.relief flat widgetDefault
			catch {font create fixed -family fixedsys -size 8}
			if {$tcl_platform(osVersion) > 5.0} {
				option add *Menubutton.activeBackground SystemHighlight widgetDefault
				option add *Menubutton.activeForeground SystemHighlightText widgetDefault
				option add *Menubutton.borderWidth 0 widgetDefault
			} else {
				option add *Menubutton.borderWidth 1 widgetDefault
			}
		}
		wm title . {Tcldrop}
		# scroll --
		# Description
		#   Create a scrollable-widget frame
		#
		# Arguments
		#   type : type of  scrollable widget (e.g. listbox)
		#   W    : name for scrollable widget
		#   args : arguments to be passed to creation of scrollable widget
		#
		# Return
		#   name of scrollable widget
		proc scroll {type W args} {
			set w $W.$type
			set x $W.x
			set y $W.y
			array set arg [list -borderwidth 0 -highlightthickness 0 -relief flat -xscrollcommand [list $x set] -yscrollcommand [list $y set]]
			array set arg $args
			frame $W -borderwidth 0 -class Scroll -highlightthickness 0 -relief sunken -takefocus 0 -pady 0 -padx 0
			# create the scrollable widget
			uplevel [linsert [array get arg] 0 $type $w]
			scrollbar $x -borderwidth 0 -elementborderwidth 1 -orient horizontal -takefocus 0 -highlightthickness 0 -command [list $w xview]
			scrollbar $y -borderwidth 0 -elementborderwidth 1 -orient vertical -takefocus 0 -highlightthickness 0 -command [list $w yview]
			grid columnconfigure $W 1 -weight 1
			grid rowconfigure $W 1 -weight 1
			grid $w -column 1 -row 1 -sticky nsew
			grid $x -column 1 -row 2 -sticky nsew
			grid $y -column 2 -row 1 -sticky nsew
			return $w
		}
		namespace eval entry {
			# Original source: http://wiki.tcl.tk/
			proc add? {w} {
				variable $w
				variable n$w
				upvar 0 $w hist
				set s [set ::[$w cget -textvariable]]
				if {$s == {}} { return }
				if [string compare $s [lindex $hist end]] {
					lappend hist $s
					set n$w [llength $hist]
				}
			}
			proc move {w where} {
				variable $w
				variable n$w
				upvar 0 $w hist
				incr n$w $where
				if {[set n$w]<0} { set n$w 0 }
				if {[set n$w]>=[llength $hist]+1} { set n$w [llength $hist] }
				set ::[$w cget -textvar] [lindex $hist [set n$w]]
			}
			proc for {type name args} {
				switch -- $type {
					{entry} {
						uplevel $type $name $args
						set ns [namespace current]
						bind $name <Up> "${ns}::move %W -1"
						bind $name <Down> "${ns}::move %W 1"
						bind $name <Next> "${ns}::move %W 99999"
						bind $name <Return> "${ns}::add? %W"
						bind $name <ButtonPress-3> "${ns}::Event:ButtonPress-3 %W %X %Y"
						variable $name {}
						variable n$name 0
						set name
					}
					default {return -code error "usage: entry::for entry <w> <args>"}
				}
			}
			# Original source: http://wiki.tcl.tk/3404
			proc Copy {win type} {
				if {[$win index end] == 0 || [catch {$win index sel.first}]} { return }
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				clipboard clear -displayof $win
				clipboard append -displayof $win $data
			}
			proc Paste {win type} {
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				$win insert insert $data
			}
			proc Cut {win type} {
				if {[catch {$win index sel.first}]} { return }
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				$win delete sel.first sel.last
				clipboard clear -displayof $win
				clipboard append -displayof $win $data
			}
			proc Append {win type} {
				if {[$win index end] == 0 || [catch {$win index sel.first}]} { return }
				if {[catch {selection get -displayof $win -selection $type -type STRING} data]} { return }
				clipboard append -displayof $win $data
			}
			proc Clear {win} { $win delete 0 end }
			proc Event:ButtonPress-3 {win X Y} {
				destroy $win._popup
				set m [menu $win._popup -tearoff 0]
				set ns [namespace current]
				#valid types are PRIMARY and CLIPBOARD
				$m add command -label Copy -command "${ns}::Copy $win PRIMARY"
				$m add command -label "Paste Primary" -command "${ns}::Paste $win PRIMARY"
				$m add command -label "Paste Clipboard" -command "${ns}::Paste $win CLIPBOARD"
				$m add command -label Cut -command "${ns}::Cut $win PRIMARY"
				$m add command -label Append -command "${ns}::Append $win PRIMARY"
				$m add command -label Clear -command "${ns}::Clear $win"
				tk_popup $m $X $Y
			}
		}
		namespace eval buttonbar {
			# Original source: http://wiki.tcl.tk/
			proc create {w frame} {
				variable buttonbar
				set ns [namespace current]
				frame $w -relief flat -padx 0 -pady 0 -borderwidth 0
				frame $w.middle -relief flat -bd 1 -pady 0 -padx 0
				button $w.left -text {<} -bd 0 -command [list ${ns}::scrollleft $w] -font {Fixedsys 14 bold} -relief flat -highlightthickness 0 -width 0 -padx 0 -state disabled
				button $w.right -text {>} -bd 0 -command [list ${ns}::scrollright $w] -font {Fixedsys 14 bold} -relief flat -highlightthickness 0 -width 0 -padx 0 -state disabled
				button $w.close -text {x} -bd 0 -command [list ${ns}::closecurrent $w] -font {Fixedsys 14 bold} -relief flat -highlightthickness 0 -width 2 -padx 0
				canvas $w.middle.c -height [winfo reqheight $w.right] -xscrollincrement 1 -highlightthickness 0 -relief flat -borderwidth 0
				frame $w.middle.c.f -relief flat -borderwidth 0 -padx 0 -pady 0
				grid $w.left $w.right $w.middle $w.close -sticky nesw -padx 0 -pady 0
				grid columnconfigure $w {0 1 3} -minsize 15 -weight 0
				grid columnconfigure $w 2 -weight 2
				pack $w.middle.c -fill both -padx 0 -pady 0
				$w.middle.c create window 0 0 -anchor nw -window $w.middle.c.f
				bind $w.middle.c.f <Configure> "$w.middle.c configure -scrollregion \[$w.middle.c bbox all\]"
				bind tab <Button-1> "${ns}::showframe $w %W; bind tab <Motion> \"${ns}::tabdrag $w %W\""
				bind tab <ButtonRelease-1> "${ns}::tearoff $w %W %X %Y; bind tab <Motion> {}; after cancel \"${ns}::tabdrag $w %W\""
				bind $w.middle.c <Configure> "${ns}::setscrollstate $w"
				set buttonbar($w) $frame
				return $w
			}
			proc add {w name} {
				variable buttonbar
				frame $buttonbar($w).$name -relief flat -padx 0 -pady 0 -borderwidth 0
				button $w.middle.c.f.$name -font {Fixedsys 14 normal} -text $name -highlightthickness 0 -padx 2 -pady 1 -relief raised
				pack $w.middle.c.f.$name -side left -pady 0 -padx 0 -fill y
				bindtags $w.middle.c.f.$name tab
				showframe $w $name
				after idle [namespace current]::setscrollstate $w
				return $buttonbar($w).$name
			}
			proc name {w tab name} { if {[winfo exists $w.middle.c.f.$tab]} { $w.middle.c.f.$tab configure -text $name } }
			proc tabexists {w tab} { winfo exists $w.middle.c.f.$tab }
			proc scrollright {w} {
				scrollsetleft $w [winfo containing [expr { [winfo rootx $w.middle.c] + [winfo width $w.middle.c] - 1 }] [winfo rooty $w.middle.c]]
				$w.right configure -foreground black -activeforeground black
			}
			proc scrollleft {w} {
				scrollsetright $w [winfo containing [winfo rootx $w.middle.c] [winfo rooty $w.middle.c]]
				$w.left configure -foreground black -activeforeground black
			}
			proc scrollsetleft {w tab} {
				set tab [string map [list $w.middle.c.f {}] $tab]
				if {![winfo exists $w.middle.c.f$tab]} { return }
				$w.middle.c xview scroll [expr { [winfo rootx $w.middle.c.f$tab] - [winfo rootx $w.middle.c] }] units
			}
			proc scrollleft {w} {
				scrollsetright $w [winfo containing [winfo rootx $w.middle.c] [winfo rooty $w.middle.c]]
				$w.left configure -foreground black -activeforeground black
			}
			proc scrollsetleft {w tab} {
				set tab [string map [list $w.middle.c.f {}] $tab]
				if {[winfo exists $w.middle.c.f$tab]} {
					$w.middle.c xview scroll [expr { [winfo rootx $w.middle.c.f$tab] - [winfo rootx $w.middle.c] }] units
				}
			}
			proc scrollsetright {w tab} {
				if {[winfo exists $w.middle.c.f[set tab [string map [list $w.middle.c.f {}] $tab]]]} {
					$w.middle.c xview scroll [expr { -1 * (([winfo rootx $w.middle.c] + [winfo width $w.middle.c]) - ([winfo rootx $w.middle.c.f$tab] + [winfo width $w.middle.c.f$tab])) }] units
				}
			}
			proc closecurrent {w} {
				foreach x [winfo children $w.middle.c.f] {
					if {[$x cget -relief] == "sunken"} {
						variable buttonbar
						destroy $x $buttonbar($w).[string map [list $w.middle.c.f. {}] $x]
						return
					}
				}
			}
			proc hilightbutton {w name} {
				if {![winfo ismapped [set name [winfo toplevel $name]]]} {
					if {[info exists ::info(text,$name)] && [string match *hilight* [$::info(text,$name) tag names end-1l+8c]]} { set color {yellow} } else { set color {red} }
					if {[$w.middle.c.f$name cget -foreground] != "yellow"} {
						$w.middle.c.f$name configure -foreground $color -activeforeground $color
					}
					if {[set view [tabvisibility $name]] < 0 && [$w.left cget -foreground] != "yellow"} {
						$w.left configure -foreground $color -activeforeground $color
					}
					if {$view > 0 && [$w.right cget -foreground] != "yellow"} {
						$w.right configure -foreground $color -activeforeground $color
					}
				}
			}
			proc tabvisibility {w name} {
				if {[set ts [winfo rootx $w.middle.c.f$name]] < [set s [winfo rootx $w.middle.c]]} {
					return -1
				} elseif {$ts + [winfo width $w.middle.c.f$name] > $s + [winfo width $w.middle.c]} {
					return 1
				} else {
					return 0
				}
			}
			proc showframe {w name} {
				if {[$w.middle.c.f.[set name [lindex [split $name .] end]] cget -relief] != "sunken"} {
					variable buttonbar
					foreach x [winfo children $buttonbar($w)] { if {$x != $w} { pack forget $x } }
					foreach x [winfo children $w.middle.c.f] {$x configure -relief raised}
					pack $buttonbar($w).$name -fill both -expand 1 -padx 0 -pady 0
					$w.middle.c.f.$name configure -foreground black -activeforeground black -relief sunken
				}
			}
			proc setscrollstate {w} {
				if {[set width [winfo width $w.middle.c]] > 1 && [winfo width $w.middle.c.f] > $width} {
					$w.left configure -state normal
					$w.right configure -state normal
				} else {
					$w.left configure -foreground black -activeforeground black -state disabled
					$w.right configure -foreground black -activeforeground black -state disabled
				}
			}
			proc tearoff {w tab x y} {
				set rx2 [expr { [set rx1 [winfo rootx $w]] + [winfo width $w] }]
				set ry2 [expr { [set ry1 [winfo rooty $w]] + [winfo height $w] }]
				if {$x < ($rx1 - 20) || $x > ($rx2 + 20) || $y < ($ry1 - 20) || $y > ($ry2 + 20)} {
					variable buttonbar
					set win $buttonbar($w).[string map [list $w.middle.c.f. {}] $tab]
					# add your function here
					#closecurrent $w
				}
			}
			proc tabdrag {w tab} {
				set pointery [winfo pointery $tab]
				set pointerx [winfo pointerx $tab]
				set hi [winfo rooty $w.middle]
				if {$pointery < $hi || $pointery > ($hi + [winfo height $w.middle])} { return }
				set children [winfo children $w.middle.c.f]
				set c [lsearch -exact $children $tab]
				if {$pointerx < [winfo rootx $w.middle.c]} {
					bind tab <Motion> {}
					after 500 [list [namespace current]::tabdrag $w $tab]
					if {[set to [lindex $children [expr { $c - 1 }]]] == {}} { return }
					pack configure $tab -before $to
					lower $tab $to
					update idletasks
					if {[tabvisibility $w [string map [list $w.middle.c.f {}] $tab]] < 0} { scrollsetleft $w $tab }
					return
				} elseif {$pointerx > ([winfo rootx $w.middle.c] + [winfo width $w.middle.c])} {
					bind tab <Motion> {}
					after 500 [list [namespace current]::tabdrag $w $tab]
					if {[set to [lindex $children [expr { $c + 1 }]]] == {}} { return }
					pack configure $tab -after $to
					raise $tab $to
					update idletasks
					if {[tabvisibility $w [string map [list $w.middle.c.f {}] $tab]] > 0} { scrollsetright $w $tab }
					return
				}
				bind tab <Motion> "[namespace current]::tabdrag $w $tab"
				set in [winfo containing $pointerx $pointery]
				if {$tab == $in} { return }
				set i [lsearch -exact $children $in]
				if {$i < 0} {
					pack configure $tab -after [set to [lindex $children end]]
					raise $tab $to
				} elseif {$i < ($c - 1)} {
					pack configure $tab -before [set to [lindex $children [expr { $c - 1 }]]]
					lower $tab $to
				} elseif {$i > ($c + 1)} {
					pack configure $tab -after [set to [lindex $children [expr { $c + 1 }]]]
					raise $tab $to
				}
			}
		}
		proc stdout {name text args} {
			array set options [list -name $name -newline 1]
			array set options $args
			if {![buttonbar::tabexists .tabs $name]} {
				# Create the tab for $name
				buttonbar::add .tabs $name
				pack [scrollbar .windows.$name.sbar -orient vertical -command [list .windows.$name.output yview] -relief flat] -expand n -fill y -side right -pady 0 -padx 0
				pack [text .windows.$name.output -yscrollcommand [list .windows.$name.sbar set] -font {Fixedsys 8 normal} -relief flat -bg black -fg white -insertbackground white -insertofftime 100 -insertontime 100 -insertwidth 2 -borderwidth 0 -pady 0 -padx 0] -expand y -fill both -pady 0 -padx 0
				pack [entry::for entry .windows.$name.input -textvar ::tcldrop::input($name) -takefocus 1 -font {Fixedsys 8 normal} -relief flat -bg AntiqueWhite4 -fg white -insertbackground AntiqueWhite1 -insertofftime 100 -insertontime 100 -borderwidth 0] -fill both -expand n -pady 0 -padx 0
				buttonbar::name .tabs $name $options(-name)
				buttonbar::showframe .tabs $name
				bind .windows.$name.output <ButtonRelease-1> "tk_textCopy .windows.${name}.output; focus .windows.$name.input; break"
				bind .windows.$name.input <Return> "+
					::tcldrop::stdin $name \$::tcldrop::input($name)
					set ::tcldrop::input($name) {}
					.windows.$name.input delete 0 end
				"
				bind .windows.$name.output <Return> "+
					::tcldrop::stdin $name \$::tcldrop::input($name)
					set ::tcldrop::input($name) {}
					.windows.$name.input delete 0 end
				"
				focus .windows.$name.input
				set options(-newline) 0
			}
			if {$options(-newline)} {
				.windows.$name.output insert end "\n$text"
			} else {
				.windows.$name.output insert end "$text"
			}
			.windows.$name.output see end
			update idletasks
		}
		proc PutLogLev {name levels channel text {tags {}}} { stdout $name $text -levels $levels -channel $channel }
		proc stdin {name text} {
			switch -- $name {
				{console} {
					::tcldrop::stdout $name "% $text"
					if {[catch { namespace eval [namespace current] $text } output]} { set output "Tcl error: $output" } else { set output "Tcl: $output" }
					::tcldrop::stdout $name $output
				}
				{default} { ::tcldrop::Tcldrop stdin $name $text }
			}
		}

		# Use winico if it's available (puts Tcldrop in the systray on Windows):
		# FixMe: We need to provide an .ico file, and the winico.dll for this to work for everybody.
		if {[file exists icon.ico] && ![catch {package require Winico}]} {
			variable Icon [winico create icon.ico]
			proc ::tcldrop::TbarHandle {ico msg args} {
				switch -- $msg {
					{WM_LBUTTONDOWN} { wm deiconify . }
					{WM_RBUTTONDOWN} { wm withdraw . }
				}
			}
			winico taskbar add $Icon -callback {::tcldrop::TbarHandle %i %m} -text {Tcldrop}
			winico setwindow . $Icon
			tkwait visibility .
			bind . <Unmap> {wm withdraw .}

			# For more winico stuff see http://wiki.tcl.tk/4090
			#winico taskbar add $winico -callback {winicoCallback %m %x %y} -text $yourAppName
			#proc winicoCallback {t {x 0} {y 0}} {
				#if { $t == "WM_LBUTTONUP" } {
					#wm deiconify .
					#raise .
					#focus .
				#} elseif { $t == "WM_RBUTTONUP" } {
					# Right-click menu..
					#.winicoPopup post $x $y
					#.winicoPopup activate 0
				#}
			#}
		#}
		# This creates the Tcldrop program window and all that good stuff:
		pack [buttonbar::create .tabs .windows] -side top -fill x -padx 0 -pady 0
		pack [frame .windows -padx 0 -pady 0] -side bottom -fill both -expand 1 -padx 0 -pady 0
		stdout console "Tcldrop Console, Ready." -newline 0 -name {Console}
		#wm geometry . [wm geometry .]
		after idle [list wm deiconify .]
		# Catch window being deleted (icon or otherwise):
		wm protocol . WM_DELETE_WINDOW { if {[tk_messageBox -icon question -type yesno -default no -message "Do you want to go?" -title "Quit Application?"] == "yes"} { exit } }


		  #wm title . "Minimize Test"
		  #wm geometry . [winfo screenwidth .]x[winfo screenheight .]+0+0
		  #update idletasks               ;# updates the full-screen
		  #wm overrideredirect . yes      ;# removes window decorations
		  #wm attributes . -topmost yes   ;# stays on top of other windows
#
		  #frame .x -highlightthickness 0 -bg #c8efff
		  #place .x -x 0 -y 0 -relwidth 1 -relheight 1
#
		  #button .x.min -text "Minimize" -bg #ffdbff -font "arial 10 bold" \
		  #  -command x_iconify
		  #place  .x.min -x [expr [winfo screenwidth  .] - 140] -y 10
#
		  #button .x.end -text "Close" -bg #ffdbff -font "arial 10 bold" \
		  #  -command "destroy ."
		  #place  .x.end -x [expr [winfo screenwidth  .] - 60] -y 10
#
		  #bind . <Expose> "wm overrideredirect . yes; focus -force ."
		  #bind . <KeyPress-Escape> x_iconify
#
		  #proc x_iconify {} {
		  #wm overrideredirect . no
		  #wm iconify .
		  #}


		  #wm iconbitmap . "myicon.ico"
 			#wm iconbitmap . -default "myicon.ico"

		stdout console [Tcldrop run [concat [list -n -t] $::argv]]

		# Re-set Exit again so the window will close:
		variable Exit
		if {[info exists Exit]} {
			after 99999 [set ::tcldrop::Exit $Exit]
			stdout console {Closing window in 99 seconds...}
			vwait ::tcldrop::Exit
		}
	}

