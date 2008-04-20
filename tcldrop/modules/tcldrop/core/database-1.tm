# core/database --
#	Handles:
#		* Provides all database commands.
#
# $Id$
#
# Copyright (C) 2004,2005,2006,2007,2008 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program (see gpl.txt); if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# Or visit http://www.GNU.Org/licenses/gpl.html
#
# The author of this project can be reached at FireEgl@Tcldrop.US
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
#	core::database module for tcldrop.
#
#	This is to be used by the users, channels, bans, exempts, invites, and ignores modules,
#	and provides a central and standard way for them to store and retrieve their data.
#
#	Note: It's main use is for storing, as the global database array can (and should) be
#	accessed directly for retrieving data using the [dict] command (because that way is faster).
#

namespace eval ::tcldrop::core::database {
	variable name {core::database}
	variable version {0.6}
	variable script [info script]
	variable depends {core}
	variable author {Tcldrop-Dev}
	variable description {Provides all database commands.}
	variable rcsid {$Id$}
	namespace path [list ::tcldrop::core ::tcldrop]
	namespace export Database database calldatabase
	variable commands [namespace export]
	variable namespace [namespace current]
	set ::modules($name) [list name $name version $version depends $depends author $author description $description rcsid $rcsid commands $commands script $script namespace $namespace]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide tcldrop::$name $version
	namespace unknown unknown
	if {![info exists ::tcldrop]} { return }
}

# FixMe: This module needs an overhaul, to take advantage of Tcl v8.5.

# This is mostly a fancy interface to using the [dict] command..
# It allows the dicts to be accessed by their database name and not by the dictionaryValue/dictionaryVariable.
# It adds some commands not available in [dict] and changes the behavior of others.
# Note: It's best to use catch when calling the database command, because it can/will throw errors when it's unable to do something.
proc ::tcldrop::core::database::database {database command args} { Database $database $command $args }
proc ::tcldrop::core::database::Database {database command {arguments {}}} {
	if {[set pos [lsearch -exact $arguments {--}]] == -1} {
		# If they didn't use -- then we assume there were no options.
		array set options {}
	} elseif {[catch { array set options [lrange $arguments 0 [incr pos -1]] } error]} {
		return -code error "database options: $error"
	} else {
		set arguments [lrange $arguments [incr pos 2] end]
	}
	foreach {type flags mask proc} [bindlist filter-database $database] {
		countbind $type $mask $proc
		# This calls all of the database binds (which act as filters on $arguments).
		set arguments [$proc $database $command $arguments [array get options]]
		# Note: This is sensitive to $proc returning -code "return" and "break" and "continue" and "error".  ;)
	}
	# I realize making heavy use of eval like this slows things down, but I don't see any other way..
	switch -- $command {
		{set} {
			# Description: This sets or replaces the value of a key.
			# Usage: database dbName set key ?key ...? value
			# Returns: Whatever dict returns.
			foreach key [lrange $arguments 0 end-1] {
				lappend keys $key
				# This creates all the keys leading up to the value.
				if {![dict exists $::database($database) $key]} {
					eval {dict} {$command} {::database($database)} $keys {[dict create]}
				}
			}
			eval [linsert $arguments 0 dict $command ::database($database)]
			calldatabase $database $command $arguments [array get options]
			lindex $arguments end
		}
		{unset} {
			# Description: This unsets a key (and all it's sub-keys).
			# Usage: database dbName unset key ?key ...?
			# Returns: Whatever dict returns.
			if {![set error [catch { eval [linsert $arguments 0 dict $command ::database($database)] } return]]} {
				calldatabase $database $command $arguments [array get options]
			}
			return -code $error $return
		}
		{lappend} - {append} {
			# Description: This lappends/appends to the value of a key.
			# Usage: database dbName lappend/append key ?key ...? value
			# Returns: Whatever dict returns.
			# Note: lappend and append in the current implementation of [dict] doesn't support (l)append'ing to a value located in a sub-dictionary.  (May 2004)
			if {[catch { set value [eval [linsert [lrange $arguments 0 end-1] 0 dict get $::database($database)]] }]} { set value {} }
			set error [catch { eval {dict} {set} {::database($database)} [lrange $arguments 0 end-1] {[$command value [lindex $arguments end]]} } return]
			if {!$error} { calldatabase $database $command $arguments [array get options] }
			return -code $error $return
		}
		{lreplace} {
			# Description: This replaces a range of elements in a list with a single value.
			# Usage: database dbName lreplace key ?key ...? first last value
			# Returns: Whatever dict returns.
			# Note: There is no lreplace in the current implementation of [dict].  (May 2004)
			if {[catch { set value [eval [linsert [lrange $arguments 0 end-3] 0 dict get $::database($database)]] }]} { set value {} }
			set error [catch { eval {dict} {set} {::database($database)} [lrange $arguments 0 end-3] {[$command $value [lindex $arguments end-2] [lindex $arguments end-1] [lindex $arguments end]]} } return]
			if {!$error} { calldatabase $database $command $arguments [array get options] }
			return -code $error $return
		}
		{lremove} {
			# Description: This removes a range of elements from a list.
			# Usage: database dbName lremove key ?key ...? first last
			# Returns: Whatever dict returns.
			# Note: There is no lremove in the current implementation of [dict].  (May 2004)
			if {[catch { set value [eval [linsert [lrange $arguments 0 end-2] 0 dict get $::database($database)]] }]} { set value {} }
			set error [catch { eval {dict} {set} {::database($database)} [lrange $arguments 0 end-2] {[lreplace $value [lindex $arguments end-1] [lindex $arguments end]]} } return]
			if {!$error} { calldatabase $database $command $arguments [array get options] }
			return -code $error $return
		}
		{incr} - {increment} {
			# Description: This increments the value of a key.
			# Usage: database dbName incr key ?key ...? increment
			#        The amount to increment MUST be specified!
			# Returns: The new value after the increment.
			# Note: incr in the current implementation of [dict] doesn't support incr'ing a value located in a sub-dictionary.  (May 2004)
			if {[catch { set value [eval [linsert [lrange $arguments 0 end-1] 0 dict get $::database($database)]] }]} { set value {0} }
			set error [catch { eval {dict} {set} {::database($database)} [lrange $arguments 0 end-1] {[set incr [$command value [lindex $arguments end]]} } return]
			if {!$error} {
				calldatabase $database $command $arguments [array get options]
				return $incr
			} else {
				return -code $error $return
			}
		}
		{rename} {
			# Description: This renames a key to something else.
			# Usage: database dbName rename oldkey newkey
			# Returns: Whatever dict unset returns, or an error if the key being renamed to already exists.
			# Note: There's no ability to rename a key in the current implementation of [dict].. Is there?  (May 2004)
			# FixMe: This needs to be changed so that keys in sub-dictionaries can be renamed.
			if {![dict exists $::database($database) [lindex $arguments 1]]} {
				eval [linsert [dict get $::database(users) [lindex $arguments 0]] 0 dict set ::database($database) [lindex $arguments 1]]
				set error [catch { dict unset ::database($database) [lindex $arguments 0] } return]
				if {!$error} { calldatabase $database $command $arguments [array get options] }
				return -code $error $return
			} else {
				return -code error "database ${command}: A key named \"[lindex $arguments 1]\" already exists."
			}
		}
		{create} {
			# Description: This creates a new database using the key/value pairs in $arguments.
			# Usage: database dbName create ?key value ...?
			# Returns: Whatever dict returns.
			set error [catch { set ::database($database) [eval [linsert $arguments 0 dict $command]] } return]
			if {!$error} { calldatabase $database $command $arguments [array get options] }
			return -code $error $return
		}
		{init} - {initialize} {
			# Description: This creates a new database if it doesn't already exist using the key/value pairs in $arguments
			#              Or if the database already exists, it sets the $arguments to it.
			# Usage: database dbName init ?key value ...?
			# Returns: Whatever dict returns.
			if {![info exists ::database($database)]} {
				set error [catch { set ::database($database) [eval [linsert $arguments 0 dict create]] } return]
				if {!$error} { calldatabase $database create $arguments [array get options] }
				return -code $error $return
			} elseif {$arguments ne {}} {
				set error [catch { eval [linsert $arguments 0 dict set ::database($database)] } return]
				if {!$error} { calldatabase $database init $arguments [array get options] }
				return -code $error $return
			}
		}
		{exists} - {get} - {keys} - {values} - {size} - {info} - {remove} - {replace} {
			# Description: These commands work much the same as in dict's.
			# Usage: database dbName exists/get/keys/values/size/info/remove/replace ?arguments?
			# Returns: Whatever dict returns.
			eval [linsert $arguments 0 dict $command $::database($database)]
		}
		{for} - {foreach} {
			# Description: This works basically like the dict "for" command.
			# Usage: database dbName for {keyVar valueVar} body
			# Returns: Whatever dict returns.
			eval [linsert [lrange $arguments 1 end] 0 dict for [lindex $arguments 0] $::database($database)]
		}
		{filter} {
			# Description: This works basically like the dict "filter" command.
			# Usage: database dbName filter key/value/script ?args?
			# Returns: Whatever dict returns.
			eval [linsert [lrange $arguments 1 end] 0 dict $command $::database($database) [lindex $arguments 0]]
		}
		{length} {
			# Description: This returns the string length of a value.
			# Usage: database dbName length key ?key ...?
			# Returns: The string length of a value.
			string length [eval [linsert $arguments 0 dict get $::database($database)]]
		}
		{llength} {
			# Description: This returns the llength of a value.
			# Usage: database dbName llength key ?key ...?
			# Returns: The llength of a value.
			llength [eval [linsert $arguments 0 dict get $::database($database)]]
		}
		{options} {
			# Description: This is to set options used when saving and loading a database from a file.
			# Usage: database dbName options ?options?
			#	options should be key/value pairs
			#	The supported options are:
			#	-file <filename>
			#	-permissions <file permissions>
			# Returns: Nothing.
			upvar 1 "::tcldrop::core::database::OPT_$database" opt
			array set opt $arguments
		}
		{reload} - {load} - {open} {
			# Description: This is the command used to (re)load a database from disk.
			# Usage: database dbName reload ?options?
			# Returns: Tcl error on failure.
			upvar 1 "::tcldrop::core::database::OPT_$database" opt
			array set opt $arguments
			if {![info exists opt(-file)] || $opt(-file) eq {}} { set opt(-file) "${::database-basename}.$database" }
			if {![file readable $opt(-file)]} {
				if {![file exists $opt(-file)]} {
					return -code error "database ${command}: The file doesn't exist: $opt(-file)"
				} else {
					return -code error "database ${command}: The file not readable: $opt(-file)"
				}
			} elseif {![catch { set ::database($database) [read [set fid [open $opt(-file) r]] [file size $opt(-file)]][close $fid] } return]} {
				# Note/FixMe: It's possible that there's a line length limit for files, and since the database stores everything on one line, this limit may be reached.
				if {![llength [info commands $database]]} { interp alias {} $database {} ::tcldrop::core::database::database $database }
				if {![llength [info commands ::tcldrop::core::database::$database]]} { interp alias {} ::tcldrop::core::database::$database {} ::tcldrop::core::database::database $database }
			} else {
				return -code error "database ${command}: Unable to load the database: $return"
			}
		}
		{unload} - {close} {
			# Description: This will unload a database from memory entirely (without saving!).
			# Usage: database dbName unload
			# Returns: Nothing.
			array unset ::database $database
			array unset "::tcldrop::core::database::OPT_$database"
		}
		{save} {
			# Description: This will save a database to disk.
			# Usage: database dbName save ?options?
			# Returns: Tcl error for failure.
			upvar 1 "::tcldrop::core::database::OPT_$database" opt
			array set opt $arguments
			if {![info exists opt(-file)] || $opt(-file) eq {}} { set opt(-file) "${::database-basename}.$database" }
			if {![catch { fconfigure [set fid [open $opt(-file) w]] -blocking 0 -buffering full } return]} {
				# Create the database if it doesn't (yet) exist:
				if {![info exists ::database($database)]} { set ::database($database) [list] }
				if {${::quiet-save} == {0}} { putlog "Saving $database ($opt(-file)) ..." }
				puts -nonewline $fid $::database($database)
				close $fid
				if {![info exists opt(-permissions)] || $opt(-permissions) eq {} || $opt(-permissions) == 0 || [catch { file attributes $opt(-file) -permissions $opt(-permissions) }]} {
					file attributes $opt(-file) -permissions ${::database-perm}
				}
			} else {
				return -code error $return
			}
		}
		{backup} {
			upvar 1 "::tcldrop::core::database::OPT_$database" opt
			if {[catch { file copy -force -- $opt(-file) "$opt(-file).bak" }]} {
				return -code error "database backup: Unable to create a backup of $opt(-file)"
			}
			return ;# FixMe: The following code was taken from http://wiki.tcl.tk/1641 and has not yet been tested:
			if {[catch {
				if {[file exists $opt(-file)]} {
					set dir [file dirname $opt(-file)]
					set files [glob -nocomplain -path $opt(-file) ".ba*"]
					set i $opt(-levels)
					while {[incr i -1]} {
						if {[lsearch -exact $files "$opt(-file).ba$i"] > -1} {
							file rename -force "$opt(-file).ba$i" "$opt(-file).ba[incr i]"
							incr i -1
						}
					}
					if {[file exists "$opt(-file).bak"]} { file rename -force "$opt(-file).bak" "$opt(-file).ba2" }
					file rename -force $fname "$opt(-file).bak"
				}
			} err ] } { return -code error "bak($opt(-file) $opt(-levels)): $err" }
		}
		{unknown} - {} - {default} { return -code error "database: Invalid subcommand: \"$command\"" }
	}
}

proc ::tcldrop::core::database::calldatabase {database command {arguments {}} {options {}}} {
	foreach {bindtype flags mask proc} [bindlist database] {
		countbind $bindtype $mask $proc
		if {[string match -nocase $mask $database]} {
			if {[catch { $proc $database $command $options $arguments } err]} {
				putlog "Error in script: $proc: $err"
				puterrlog "$::errorInfo"
			} elseif {[string equal $err {1}]} {
				break
			}
		}
	}
}

bind load - core::database ::tcldrop::core::database::LOAD -priority 0
proc ::tcldrop::core::database::LOAD {module} {
	array set ::database {}
	setdefault database-basename {Database}
	setdefault database-perm {0600}
	setdefault quiet-save 1
	protected globals database database-basename database-perm quiet-save
}

bind unld - core::database ::tcldrop::core::database::UNLD -priority 10
proc ::tcldrop::core::database::UNLD {module} {
	return 1
}
