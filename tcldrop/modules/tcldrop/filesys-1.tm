# filesys.tcl --
#	Provides:
#		* A place where users can store files. With this module, the bot is usable as a file server.
#	Depends: transfer.
#
# $Id: filesys.tcl,v 1.2 2005/04/25 08:09:45 fireegl Exp $
#
# Copyright (C) 2004 phrek <ephrek@gmail.com>
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
# The author of this project can be reached at FireEgl@Tcldrop.Org
# Or can be found on IRC (EFNet or FreeNode) as FireEgl.
#
#	filesys module for Tcldrop.  (OPTIONAL)
#
# TODO:	alot still
#	virtual file handling
#	tcl bindings

namespace eval ::tcldrop::filesys {
	variable version {0.1}
	variable name {filesys}
	variable depends {transfer}
	variable author {phrek}
	variable description {Provides the file server.}
	variable commands [list callfil setpwd getpwd getfiles getdirs dccsend filesend fileresend setdesc getdesc setowner getowner setlink getlink getfileq getfilesendtime mkdir rmdir mv cp getflags setflags]
	variable script [info script]
	variable rcsid {$Id: filesys.tcl,v 1.2 2005/04/25 08:09:45 fireegl Exp $}
	package provide tcldrop::$name $version
	# Export all the commands that should be available to 3rd-party scripters:
	eval namespace export $commands
}

#    (3)  FIL
#         bind fil <flags> <command> <proc>
#         procname <handle> <idx> <text>
#
#         Description: the same as DCC, except this is triggered if the user
#           is in the file area instead of the party line
#         Module: filesys
#NOT IMPLEMENTED YET
proc ::tcldrop::filesys::callfil {handle idx arg} {
	# retval will be the number of binds that were triggered..
	set retval 0
	set arg [split $arg]
	set cmd [string range [lindex $arg 0] 1 end]
	set arg [join [lrange $arg 1 end]]
	foreach {type flags mask proc} [bindlist fil] {
		if {[string equal -nocase $cmd $mask] && [matchattr $handle $flags]} {
			putloglev d * "tcl: fil call: $proc $handle $idx $arg"
			incr retval
			if {[catch { $proc $handle $idx $arg } err]} {
				putlog "error in script: $proc: $err"
			} elseif {[string equal $err {1}]} {
				# Abort processing further binds if they return 1.
				break
			}
			countbind $type $mask $proc
		}
	}
	if {$retval == 0} { putidx $idx {What?  You need '.help'} }
	set retval
}

#  setpwd <idx> <dir>
#    Description: changes the directory of a file system user, in exactly
#      the same way as a 'cd' command would. The directory can be specified
#      relative or absolute.
#    Returns: nothing
#    Module: filesys
proc ::tcldrop::filesys::setpwd {idx dir} {
	variable Users
	foreach u [array names Users [string tolower ${idx}:*@${::botnet-nick}]] {
		if {[string equal [string range $dir 0 0] "/"]} {
			#absolute path
			if {[catch {file normalize "${::incoming-path}[file separator]$dir"} dir]} {
				#could not normalize
				set Users($u) [file normalize ${::incoming-path}]
			} elseif {[string first [file normalize ${::incoming-path}] $dir] == 0} {
				#within vfs
				set Users($u) [file normalize $dir]
			} else {
				#not within vfs
				set Users($u) [file normalize ${::incoming-path}]
			}
		} else {
			#relative path
			if {[catch {file normalize "${::incoming-path}[file separator]$Users($u)[file separator]$dir"} dir]} {
				#could not normalize
				set Users($u) [file normalize ${::incoming-path}]
			} elseif {[string first [file normalize ${::incoming-path}] $dir] == 0} {
				#within vfs
				set Users($u) [file normalize $dir]
			} else {
				#not within vfs
				set Users($u) [file normalize ${::incoming-path}]
			}
		}
	}
}

#  getpwd <idx>
#    Returns: the current directory of a file system user
#    Module: filesys
proc ::tcldrop::filesys::getpwd {idx} {
	variable Users
	foreach u [array names Users [string tolower ${idx}:*@${::botnet-nick}]] {
		return $Users($u)
	}
}

#  getfiles <dir>
#    Returns: a list of files in the directory given; the directory is
#      relative to dcc-path
#    Module: filesys
proc ::tcldrop::filesys::getfiles {dir} {
	if {![catch {file normalize "${::incoming-path}[file separator]$dir"} dir] && [string first [file normalize ${::incoming-path}] $dir] == 0} {
		return [glob -directory $dir -nocomplain -tails -type f -- *]
	}
}

#  getdirs <dir>
#    Returns: a list of subdirectories in the directory given; the directory
#      is relative to dcc-path
#    Module: filesys
proc ::tcldrop::filesys::getdirs {dir} {
	global incoming-path
	if {![catch {file normalize "${::incoming-path}[file separator]$dir"} dir] && [string first [file normalize ${::incoming-path}] $dir] == 0} {
		return [glob -directory $dir -nocomplain -tails -type d -- *]
	}
}

#  filesend <idx> <filename> [ircnick]
#    Description: like dccsend, except it operates for a current filesystem
#      user, and the filename is assumed to be a relative path from that
#      user's current directory
#    Returns: 0 on failure; 1 on success (either an immediate send or a queued
#      send)
#    Module: filesys
proc ::tcldrop::filesys::filesend {idx filename {nick ""}} {
	if {[valididx $idx] == 0} {
		putlog "Tcl error: invalid idx"
		return 0
	}
	variable Users
	foreach u [array names Users [string tolower ${idx}:*@${::botnet-nick}]] {
		#file security for this MUST actually be on this end
		if {[catch {file normalize "${::incoming-path}[file separator]$Users($u)[file separator]"} filename]} {
			#could not normalize path
			return 0
		}
		if {[string first [file normalize ${::incoming-path}] $filename] != 0} {
			#not within the vfs
			return 0
		}
		if {![file exists $filename]} {
			#invalid file
			return 0
		}
		if {[string length $nick] == 0} {
			# check all channels for a nick matching handle otherwise send to the handle
			set hand [idx2hand $idx]
			set nick [hand2nick $hand]
			if {[string length $nick] == 0} {
				return [dccsend $filename $hand]
			} else {
				return [dccsend $filename $nick]
			}
		} else {
			return [dccsend $filename $nick]
		}
	}
}

# fileresend <idx> <filename> [ircnick]
#    Description: functions like filesend, only that it sends a DCC RESEND
#      instead of a DCC SEND, which allows people to resume aborted file
#      transfers if their client supports that protocol. ircII/BitchX/etc.
#      support it; mIRC does not.
#    Returns: 0 on failure; 1 on success (either an immediate send or a queued
#      send)
#    Module: filesys
#

################################################

############### Not sure how to implement this.

#######################################################
proc ::tcldrop::filesys::fileresend {idx filename {nick ""}} {
	if {[valididx $idx] == 0} {
		putlog "Tcl error: invalid idx"
		return 0
	}
	variable Users
	foreach u [array names Users [string tolower ${idx}:*@${::botnet-nick}]] {
		#file security for this MUST actually be on this end
		if {[catch {file normalize "${::incoming-path}[file separator]$Users($u)[file separator]"} filename]} {
			#could not normalize path
			return 0
		}
		if {[string first [file normalize ${::incoming-path}] $filename] != 0} {
			#not within the vfs
			return 0
		}
		if {![file exists $filename]} {
			#invalid file
			return 0
		}
		if {[string length $nick] == 0} {
			# check all channels for a nick matching handle otherwise send to the handle
			set hand [idx2hand $idx]
			set nick [hand2nick $hand]
			if {[string length $nick] == 0} {
				return [dccsend $filename $hand "resend"]
			} else {
				return [dccsend $filename $nick "resend"]
			}
		} else {
			return [dccsend $filename $nick "resend"]
		}
	}
}

#  mkdir <directory> [<required-flags> [channel]]
#    Description: creates a directory in the file system. Only users with the
#      required flags may access it.
#    Returns:
#      0 success
#      1 can't create directory
#      2 directory exists but is not a directory
#    Module: filesys
proc ::tcldrop::filesys::mkdir {dir {flags ""} {chan ""}} {
	if {[string length $flags] == 0 && [string length $chan] == 0} {
		if {[catch {file normalize "${::incoming-path}[file separator]$dir"} dir]} {
			#could not normalize directory - cannot make directory
			return 1
		} else {
			if {[string first [file normalize ${::incoming-path}] $dir] == 0 && [file isdirectory $dir]} {
				#directory already exists
				return 1
			} else {
				#check and see if it is a linked dir
				#the file exists command must come first or else it would err with a nonexistant dir
				if {[string first [file normalize ${::incoming-path}] $dir] == 0 && [file exists $dir] == 1 && [file type $dir] == "link"} {
					return 2
				} elseif {[string first [file normalize ${::incoming-path}] $dir] == 0} {
					#try to create the directory
					if {[catch {file mkdir $dir}]} {
						#failed to make dir
						return 1
					} else {
						#success
						return 0
					}
				} else {
					#outside of path
					return 1
				}
			}
		}
	} else {
		set reldir $dir
		if {[catch {file normalize "${::incoming-path}[file separator]$dir"} dir]} {
			#could not normalize directory - cannot make directory
			return 1
		} else {
			if {[string first [file normalize ${::incoming-path}] $dir] == 0 && [file isdirectory $dir]} {
				#directory already exists, but since flags were passed update the flags for the dir
				if {[setflags $reldir $flags $chan] == 0} {
					#success - changed the flags
					return 0
				} else {
					#failure changing the flags (cannot return immediate result of setflags since it returns -1 on failure)
					return 1
				}
			} else {
				#check and see if it is a linked dir
				#the file exists command must come first or else it would err with a nonexistant dir
				if {[string first [file normalize ${::incoming-path}] $dir] == 0 && [file exists $dir] == 1 && [file type $dir] == "link"} {
					return 2
				} elseif {[string first [file normalize ${::incoming-path}] $dir] == 0} {
					#try to create the directory
					if {[catch {file mkdir $dir}]} {
						#failed to make dir
						return 1
					} else {
						#change the flags (should always work but if it fails for some reason -- this really should still return 0)
						setflags $reldir
						#success
						return 0
					}
				} else {
					#outside of path
					return 1
				}
			}
		}
	}
}

#  rmdir <directory>
#    Description: removes a directory from the file system.
#    Returns: 0 on success; 1 on failure
#    Module: filesys
proc ::tcldrop::filesys::rmdir {dir} {
	#need to normalize the $dir first to make sure they aren't trying to remove something that they shouldn't be able to.
	if {[catch {file normalize "${::incoming-path}[file separator]$dir"} dir]} {
		#could not normalize directory - cannot make directory
		return 1
	} else {
		if {[string first [file normalize ${::incoming-path}] $dir] == 0 && [file isdirectory $dir]} {
			#directory exists
			if {[catch {file delete $dir} err]} {
				return 1
			} else {
				#success
				return 0
			}
		} else {
			return 1
		}
	}
}

#  mv <file> <destination>
#    Description: moves a file from its source to the given destination.
#      The file can also be a mask, such as /incoming/*, provided the
#      destination is a directory.
#    Returns: If the command was successful, the number of files moved will
#      be returned. Otherwise, a negative number will be returned:
#        -1 invalid source file
#        -2 invalid destination
#        -3 destination file exists
#        -4 no matches found
#    Module: filesys
proc ::tcldrop::filesys::mv {file dest} {
	global incoming-path

	#check if a mask was provided as file and a directory as the dest
	if {[string first * [file tail $file]] == -1} {
		#no mask
		if {[catch {file normalize "${incoming-path}[file separator]$dest"} dest]} {
			#could not normalize - invalid destination
			return -2
		} else {
			if {[catch {file normalize "${incoming-path}[file separator]$file"} file]} {
				# could not normalize source for some reason
				return -1
			} else {
				# make sure files are within bounds
				if {[string first [file normalize ${incoming-path}] $file] != 0} {
					return -1
				} elseif {[string first [file normalize ${incoming-path}] $dest] != 0} {
					return -2
				} else {
					# check if source does not exist
					if {![file exists $file]} {
						return -1
					} else {
						#check to see if dest dir is invalid
						if {![file isdirectory [file dirname $dest]]} {
							return -2
						} else {
							if {[catch {file rename $filepath $dest} err]} {
								if {[string equal [string range $err end-5 end] "exists"]} {
									#dest exists already
									return -3
								} else {
									#dest did not exist but could not copy, so cannot access source
									return -1
								}
							} else {
								#success
								return 1
							}
						}
					}
				}
			}
		}
	} else {
		#make sure dest is a dir
		if {[catch {file normalize "${incoming-path}[file separator]$dest"} dest]} {
			#could not normalize - invalid destination
			return -2
		} else {
			# no need to normalize file by itself - just file/mask
			if {[catch {file normalize "${incoming-path}[file separator]$file"} file]} {
				# could not normalize source for some reason
				return -1
			} else {
				# make sure files are within bounds
				if {[string first [file normalize ${incoming-path}] $file] != 0} {
					return -1
				} elseif {[string first [file normalize ${incoming-path}] $dest] != 0} {
					return -2
				} else {
					# now make sure dest dir exists
					if {[file isdirectory $dest]} {
						# get source file list using glob (containing each path for each file)
						set filelist [glob -nocomplain -type f -- $file]
						if {[llength $filelist] == 0} {
							#couldn't find any files matching the file mask
							return -4
						} else {
							set trampling 0
							set mvcount 0
							foreach filepath $filelist {
								if {[catch {file rename $filepath $dest} err]} {
									if {[string equal [string range $err end-5 end] "exists"]} {
										# at least one of the files already existed (skip)
										set trampling 1
									}
								} else {
									incr mvcount
								}
							}
							if {$trampling == 1} {
								#eggdrop is unclear on what it does in this case so it will move as
								#many as possible and tell the user that at least one could not be copied
								return -3
							} else {
								return $mvcount
							}
						}
					} else {
						return -2
					}
				}
			}
		}
	}
}

#  cp <file> <destination>
#    Description: copies a file from its source to the given destination.
#      The file can also be a mask, such as /incoming/*, provided the
#      destination is a directory.
#    Returns: If the command was successful, the number of files copied will
#      be returned. Otherwise, a negative number will be returned:
#        -1 invalid source file
#        -2 invalid destination
#        -3 destination file exists
#        -4 no matches found
#    Module: filesys
proc ::tcldrop::filesys::cp {file dest} {
	global incoming-path

	#check if a mask was provided as file and a directory as the dest
	if {[string first * [file tail $file]] == -1} {
		#no mask
		if {[catch {file normalize "${incoming-path}[file separator]$dest"} dest]} {
			#could not normalize - invalid destination
			return -2
		} else {
			if {[catch {file normalize "${incoming-path}[file separator]$file"} file]} {
				# could not normalize source for some reason
				return -1
			} else {
				# make sure files are within bounds
				if {[string first [file normalize ${incoming-path}] $file] != 0} {
					return -1
				} elseif {[string first [file normalize ${incoming-path}] $dest] != 0} {
					return -2
				} else {
					# check if source does not exist
					if {![file exists $file]} {
						return -1
					} else {
						#check to see if dest dir is invalid
						if {![file isdirectory [file dirname $dest]]} {
							return -2
						} else {
							if {[catch {file copy $filepath $dest} err]} {
								if {[string equal [string range $err end-5 end] "exists"]} {
									#dest exists already
									return -3
								} else {
									#dest did not exist but could not copy, so cannot access source
									return -1
								}
							} else {
								#success
								return 1
							}
						}
					}
				}
			}
		}
	} else {
		#make sure dest is a dir
		if {[catch {file normalize "${incoming-path}[file separator]$dest"} dest]} {
			#could not normalize - invalid destination
			return -2
		} else {
			# no need to normalize file by itself - just file/mask
			if {[catch {file normalize "${incoming-path}[file separator]$file"} file]} {
				# could not normalize source for some reason
				return -1
			} else {
				# make sure files are within bounds
				if {[string first [file normalize ${incoming-path}] $file] != 0} {
					return -1
				} elseif {[string first [file normalize ${incoming-path}] $dest] != 0} {
					return -2
				} else {
					# now make sure dest dir exists
					if {[file isdirectory $dest]} {
						# get source file list using glob (containing each path for each file)
						set filelist [glob -nocomplain -type f -- $file]
						if {[llength $filelist] == 0} {
							#couldn't find any files matching the file mask
							return -4
						} else {
							set trampling 0
							set cpcount 0
							foreach filepath $filelist {
								if {[catch {file copy $filepath $dest} err]} {
									if {[string equal [string range $err end-5 end] "exists"]} {
										# at least one of the files already existed (skip)
										set trampling 1
									}
								} else {
									incr cpcount
								}
							}
							if {$trampling == 1} {
								#eggdrop is unclear on what it does in this case so it will move as
								#many as possible and tell the user that at least one could not be copied
								return -3
							} else {
								return $cpcount
							}
						}
					} else {
						return -2
					}
				}
			}
		}
	}
}

# sets specified attributes for files/directories in the filedb (this proc is not exported)
#
# notes: this filesystem uses a different file structure for the .filedb than eggdrop under the name of: .filedb.{botnet-nick}
# where the botnet-nick is substituted with {botnet-nick} (this ensures compatibility in the instance of multiple bots)
# for a decentralized filedb, it is of this format (in two tcl style arrays):
#   filename {owner desc linkremotebotnick linkremotepath getcount} filename {owner desc linkremotebotnick linkremotepath getcount}
#   dirname {userflags chanflags} dirname {userflags chanflags}
# for a centralized filedb, it is of this format:
#   directory1inthefilesystem					(relative to filesys root)
#   filename {owner desc linkremotebotnick linkremotepath getcount} filename {owner desc linkremotebotnick linkremotepath getcount}
#   dirname {userflags chanflags} dirname {userflags chanflags}
#   directory2inthefilesystem
#   filename {owner desc linkremotebotnick linkremotepath getcount} filename {owner desc linkremotebotnick linkremotepath getcount}
#   dirname {userflags chanflags} dirname {userflags chanflags}
# more notes: the getcount is the number of gets per file (counting handled by dccsend of transfer module)
# the "directoryinthefilesystem" in the centralized db is stored as an ABSOLUTE path for ease of parsing back and forth within this and getAttr
proc ::tcldrop::filesys::Setattr {dir name attr val {val2 ""}} {
	if {[string equal [string trim ${::filedb-path}] ""]} {
		#for a decentralized filedb (return (-1 outside of vfs) (0 failure) (1 success))
		#check if $dir is within vfs
		if {[catch {file normalize "${::incoming-path}[file separator]$dir"} path]} {
			return 0
		} elseif {[string first [file normalize ${::incoming-path}] $file] == 0} {
			return -1
		} else {
			set fdb [file normalize "$path[file separator].${::filedb-path}.${::botnet-nick}"]
			#try to open the filedb in $path/.. for reading
			if {![file exists $fdb] && [catch {open $fdb r} fid]} {
				#could not open filedb for reading
				#try to open it for writing
				if {[catch {open $fdb w} fid]} {
					#could not create file (no access?)
					return 0
				} else {
					switch $attr {
						"owner"	{
								puts $fid "$name [list [list $val {} {} {} {}]]\n"
							}
						"desc"	{
								puts $fid "$name [list [list {} $val {} {} {}]]\n"
							}
						"userflags"	{
								puts $fid "\n$name [list [list $val {}]]"
							}
						"chanflags"	{
								puts $fid "\n$name [list [list {} $val]]"
							}
						"bothflags"	{
								puts $fid "\n$name [list [list $val $val2]]"
							}
						"link"	{
								puts $fid "$name [list [list {} {} $val $val2 {}]]\n"
							}
						"count" {
								puts $fid "$name [list [list {} {} {} {} $val]]\n"
						}
					}
					close $fid
					return 1
				}
			} elseif {[file exists $fdb]} {
				#opened filedb for reading
				array set filearray [gets $fid]
				array set folderarray [gets $fid]
				close $fid
				#open it for writing
				if {[catch {open $fdb w} fid]} {
					#could not create file (no access?)
					return 0
				} else {
					switch $attr {
						"owner"	{
								if {[string equal [array get filearray $name] ""]} {
									array set filearray "$name [list [list $val {} {} {}]]"
								} else {
									array set filearray "$name [list [lreplace [array get filearray $name] 0 0 $val]]"
								}
							}
						"desc"	{
								if {[string equal [array get filearray $name] ""]} {
									array set filearray "$name [list [list {} $val {} {}]]"
								} else {
									array set filearray "$name [list [lreplace [array get filearray $name] 1 1 $val]]"
								}
							}
						"userflags"	{
								if {[string equal [array get folderarray $name] ""]} {
									array set folderarray "$name [list [list $val {}]]"
								} else {
									array set folderarray "$name [list [lreplace [array get folderarray $name] 0 0 $val]]"
								}
						}
						"chanflags"	{
								if {[string equal [array get folderarray $name] ""]} {
									array set folderarray "$name [list [list {} $val]]"
								} else {
									array set folderarray "$name [list [lreplace [array get folderarray $name] 1 1 $val]]"
								}
							}
						"bothflags"	{
								if {[string equal [array get folderarray $name] ""]} {
									array set folderarray "$name [list [list $val {}]]"
								} else {
									array set folderarray "$name [list [lreplace [array get folderarray $name] 0 1 $val $val2]]"
								}
							}
						"link"	{
								if {[string equal [array get filearray $name] ""]} {
									array set folderarray "$name [list [list {} {} $val $val2]]"
								} else {
									array set folderarray "$name [list [lreplace [array get filearray $name] 2 3 $val $val2]]"
								}
							}
						"count"	{
								if {[string equal [array get filearray $name] ""]} {
									array set folderarray "$name [list [list {} {} $val $val2]]"
								} else {
									array set folderarray "$name [list [lreplace [array get filearray $name] 4 4 $val]]"
								}
							}

					}
					puts $fid [array get filearray]
					puts $fid [array get folderarray]
					close $fid
					return 1
				}
			} else {
				return 0
			}
		}
	} else {
		#for a centralized filedb
		#check if $dir is within vfs
		if {[catch {file normalize "${::incoming-path}[file separator]$dir"} path]} {
			return 0
		} elseif {[string first [file normalize ${::incoming-path}] $file] == 0} {
			return -1
		} else {
			set fdb [file normalize "${::filedb-path}[file separator].filedb"]
			#try to open the filedb for reading
			if {![file exists $fdb] && [catch {open $fdb r} fid]} {
				#could not open filedb for reading
				#try to open it for writing
				if {[catch {open $fdb w} fid]} {
					#could not create file (no access?)
					return 0
				} else {
					puts $fid $path
					switch $attr {
						"owner"	{
								puts $fdb "$name [list [list $val {} {} {} {}]]\n"
							}
						"desc"	{
								puts $fdb "$name [list [list {} $val {} {} {}]]\n"
							}
						"userflags"	{
								puts $fdb "\n$name [list [list $val {}]]"
							}
						"chanflags"	{
								puts $fdb "\n$name [list [list {} $val]]"
							}
						"bothflags"	{
								puts $fdb "\n$name [list [list $val $val2]]"
							}
						"link"	{
								puts $fdb "$name [list [list {} {} $val $val2]]\n"
							}
						"count"	{
								puts $fdb "$name [list [list {} {} {} {} $val]]\n"
							}
					}
					close $fid
					return 1
				}
			} elseif {[file exists $fdb]} {
				#opened filedb for reading
				#search for $path
				while {![eof $fid] && ![catch {gets $fid} line]} {
					if {[string equal $path $line] == 0} {
						catch {gets $fid} line
						catch {gets $fid} line
					} else {
						break
					}
				}
				if {[eof $fid]} {
					#could not find path so append to end of file
					close $fid
					#open it for writing
					if {[catch {open $fdb a} fid]} {
						#could not create file (no access?)
						return 0
					} else {
						switch $attr {
							"owner"	{
									if {[string equal [array get filearray $name] ""]} {
										array set filearray "$name [list [list $val {} {} {} {}]]"
									} else {
										array set filearray "$name [list [lreplace [array get filearray $name] 0 0 $val]]"
									}
								}
							"desc"	{
									if {[string equal [array get filearray $name] ""]} {
										array set filearray "$name [list [list {} $val {} {} {}]]"
									} else {
										array set filearray "$name [list [lreplace [array get filearray $name] 1 1 $val]]"
									}
								}
							"userflags"	{
									if {[string equal [array get folderarray $name] ""]} {
										array set folderarray "$name [list [list $val {}]]"
									} else {
										array set folderarray "$name [list [lreplace [array get folderarray $name] 0 0 $val]]"
									}
							}
							"chanflags"	{
									if {[string equal [array get folderarray $name] ""]} {
										array set folderarray "$name [list [list {} $val]]"
									} else {
										array set folderarray "$name [list [lreplace [array get folderarray $name] 1 1 $val]]"
									}
								}
							"bothflags"	{
									if {[string equal [array get folderarray $name] ""]} {
										array set folderarray "$name [list [list $val {}]]"
									} else {
										array set folderarray "$name [list [lreplace [array get folderarray $name] 0 1 $val $val2]]"
									}
								}
							"link"	{
									if {[string equal [array get filearray $name] ""]} {
										array set folderarray "$name [list [list {} {} $val $val2 {}]]"
									} else {
										array set folderarray "$name [list [lreplace [array get filearray $name] 2 3 $val $val2]]"
									}
								}
							"count"	{
									if {[string equal [array get filearray $name] ""]} {
										array set folderarray "$name [list [list {} {} {} {} $val]]"
									} else {
										array set folderarray "$name [list [lreplace [array get filearray $name] 4 4 $val]]"
									}
								}
						}
						puts $fid $path
						puts $fid [array get filearray]
						puts $fid [array get folderarray]
						close $fid
						return 1
					}
				} else {
					#found existing path
					set offset1 [tell $fid]
					array set filearray [gets $fid]
					array set folderarray [gets $fid]
					if {![eof $fid]} {
						set offset2 [tell $fid]
					} else {
						set offset2 -1
					}
					#open it for writing
					if {[catch {open "${fdb}.tmp" w} fid2]} {
						#could not create file (no access?)
						return 0
					} else {
						switch $attr {
							"owner"	{
									if {[string equal [array get filearray $name] ""]} {
										array set filearray "$name [list [list $val {} {} {}]]"
									} else {
										array set filearray "$name [list [lreplace [array get filearray $name] 0 0 $val]]"
									}
								}
							"desc"	{
									if {[string equal [array get filearray $name] ""]} {
										array set filearray "$name [list [list {} $val {} {}]]"
									} else {
										array set filearray "$name [list [lreplace [array get filearray $name] 1 1 $val]]"
									}
								}
							"userflags"	{
									if {[string equal [array get folderarray $name] ""]} {
										array set folderarray "$name [list [list $val {}]]"
									} else {
										array set folderarray "$name [list [lreplace [array get folderarray $name] 0 0 $val]]"
									}
							}
							"chanflags"	{
									if {[string equal [array get folderarray $name] ""]} {
										array set folderarray "$name [list [list {} $val]]"
									} else {
										array set folderarray "$name [list [lreplace [array get folderarray $name] 1 1 $val]]"
									}
								}
							"bothflags"	{
									if {[string equal [array get folderarray $name] ""]} {
										array set folderarray "$name [list [list $val {}]]"
									} else {
										array set folderarray "$name [list [lreplace [array get folderarray $name] 0 1 $val $val2]]"
									}
								}
							"link"	{
									if {[string equal [array get filearray $name] ""]} {
										array set folderarray "$name [list [list {} {} $val $val2]]"
									} else {
										array set folderarray "$name [list [lreplace [array get filearray $name] 2 3 $val $val2]]"
									}
								}
							"count"	{
									if {[string equal [array get filearray $name] ""]} {
										array set folderarray "$name [list [list {} {} {} {} $val]]"
									} else {
										array set folderarray "$name [list [lreplace [array get filearray $name] 4 4 $val]]"
									}
								}
						}
						#copy beginning of original filedb
						seek $fid 0
						puts $fid2 [read $fid $offset1]

						#copy modified entry of filedb
						puts $fid2 $path
						puts $fid2 [array get filearray]
						puts $fid2 [array get folderarray]

						#copy rest of original filedb
						if {$offset2 != -1} {
							seek $fid $offset2
							puts $fid2 [read $fid]
						}

						close $fid
						close $fid2

						if {[catch {file rename -force -- "${fdb}.tmp" $fdb} err]} {
							return 0
						} else {
							return 1
						}
					}
				}
			} else {
				return 0
			}
		}
	}
}

#gets specified attributes for files/directories in the filedb (this proc is not exported)
proc ::tcldrop::filesys::Getattr {dir name attr} {
	if {[string equal [string trim ${::filedb-path}] ""]} {
		#for a decentralized filedb (return (-1 outside of vfs) (0 failure) (1 success))
		#check if $dir is within vfs
		if {[catch {file normalize "${::incoming-path}[file separator]$dir"} path]} {
			return 0
		} elseif {[string first [file normalize ${::incoming-path}] $file] == 0} {
			return -1
		} else {
			set fdb [file normalize "$path[file separator].${::filedb-path}.${::botnet-nick}"]
			#try to open the filedb in $path/.. for reading
			if {[file exists $fdb] && ![catch {open $fdb r} fid] && ![catch {gets $fid} f1] && ![catch {gets $fid} f2]} {
				array set filearray "$f1"
				array set folderarray "$f2"
				catch {close $fid}
				switch $attr {
					"owner"	{
							if {[info exists filearray($name)]} {
								return [lindex $filearray($name) 0]
							} else {
								return ""
							}
						}
					"desc"	{
							if {[info exists filearray($name)]} {
								return [lindex $filearray($name) 1]
							} else {
								return ""
							}
						}
					"bothflags"	{
							if {[info exists folderarray($name)]} {
								return [list [lindex $folderarray($name) 0] [lindex $folderarray($name) 1]]
							} else {
								return ""
							}
						}
					"link"	{
							if {[info exists filearray($name)]} {
								return [list [lindex $filearray($name) 2] [lindex $filearray($name) 3]]
							} else {
								return ""
							}
						}
					"count"	{
							if {[info exists filearray($name)]} {
								return [lindex $filearray($name) 4]
							} else {
								return ""
							}
						}
				}
			}
			return 0
		}
	} else {
		#for a centralized filedb
		#check if $dir is within vfs
		if {[catch {file normalize "${::incoming-path}[file separator]$dir"} path]} {
			return 0
		} elseif {[string first [file normalize ${::incoming-path}] $file] == 0} {
			return -1
		} else {
			set fdb [file normalize "${::filedb-path}[file separator].filedb"]
			#try to open the filedb for reading
			if {[file exists $fdb] && ![catch {open $fdb r} fid]} {
				#opened filedb for reading
				#search for $path
				while {![eof $fid] && ![catch {gets $fid} line]} {
					if {[string equal $path $line] == 0} {
						catch {gets $fid} line
						catch {gets $fid} line
					} else {
						break
					}
				}
				if {![eof $fid] && ![catch {gets $fid} f1] && ![catch {gets $fid} f2]} {
					#found existing path
					array set filearray "$f1"
					array set folderarray "$f2"
					switch $attr {
						"owner"	{
								if {[info exists filearray($name)]} {
									return [lindex $filearray($name) 0]
								} else {
									return ""
								}
							}
						"desc"		{
								if {[info exists filearray($name)]} {
									return [lindex $filearray($name) 1]
								} else {
									return ""
								}
							}
						"bothflags"	{
								if {[info exists folderarray($name)]} {
									return [list [lindex $folderarray($name) 0] [lindex $folderarray($name) 1]]
								} else {
									return ""
								}
						}
						"link"	{
								if {[info exists filearray($name)]} {
									return [list [lindex $filearray($name) 2] [lindex $filearray($name) 3]]
								} else {
									return ""
								}
							}
						"count"	{
								if {[info exists filearray($name)]} {
									return [lindex $filearray($name) 4]
								} else {
									return ""
								}
							}
					}
				}
			}
			return 0
		}
	}
}

#  setdesc <dir> <file> <desc>
#    Description: sets the description for a file in a file system directory;
#      the directory is relative to dcc-path
#    Returns: nothing
#    Module: filesys
proc ::tcldrop::filesys::setdesc {dir file desc} {
	::tcldrop::filesys::Setattr $dir $file "desc" $desc
}

#  setowner <dir> <file> <handle>
#    Description: changes the owner for a file in the file system; the
#      directory is relative to dcc-path
#    Returns: nothing
#    Module: filesys
proc ::tcldrop::filesys::setowner {dir file handle} {
	::tcldrop::filesys::Setattr $dir $file "owner" $desc
}

#  setlink <dir> <file> <link>
#    Description: creates or changes a linked file (a file that actually exists on another bot); the directory is relative to dcc-path
#    Returns: nothing
#    Module: filesys
#
# notes: eggdrop uses format: setlink folder file botnick:/full/path/to/file.ext
#        tcldrop uses format: setlink folder file botnick<|>/full/path/to/file.ext
# where <|> is the "special" separator between the optional botnick (if no botnick is specified, own botnick is used)
proc ::tcldrop::filesys::setlink {dir file link} {
	if {[string first "<|>" $link] == -1} {
		# use default botnick
		::tcldrop::filesys::Setattr $dir $file "link" ${::botnet-nick} $link
	} else {
		::tcldrop::filesys::Setattr $dir $file "link" [string range $link 0 [expr [string first "<|>" $link]-1]] [string range $link [expr [string first "<|>" $link]+3] end]
	}
}

#  setflags <dir> [<flags> [channel]]
#    Description: sets the flags required to access a directory
#    Returns: 0 on success; -1 on failure
#    Module: filesys
proc ::tcldrop::filesys::setflags {dir {args ""}} {
	#flags 'i', 's' not allowed (if they are the only flags in the flags then ignore command)
	if {[string length [lindex $args 0]] != 0 && [string length [string map {"i" "" "s" ""} [lindex $args 0]]] == 0} {
		return -1
	}
	if {[llength $args] == 2} {
		if {[string length [lindex $args 0]] != 0} {
			set args [list "[lindex $args 0]|-"]
		}
		if {[string equal [file tail $dir] ""]} {
			set result [::tcldrop::filesys::Setattr [file dirname $dir] "[file separator][file tail $dir]" "bothflags" "[lindex $args 0]|-" [lindex $args 1]]
		} else {
			set result [::tcldrop::filesys::Setattr [file dirname $dir] [file tail $dir] "bothflags" "[lindex $args 0]|-" [lindex $args 1]]
		}
	} else {
		if {[string length [lindex $args 0]] != 0} {
			set result [set args [list "[lindex $args 0]|-" [lindex $args 1]]]
		}
		if {[string equal [file tail $dir] ""]} {
			set result [::tcldrop::filesys::Setattr [file dirname $dir] "[file separator][file tail $dir]" "userflags" "[lindex $args 0]|-"]
		} else {
			set result [::tcldrop::filesys::Setattr [file dirname $dir] [file tail $dir] "userflags" [lindex $args 0]]
		}
	}
	if {$result == 1} {
		return 0
	} else {
		return -1
	}

}

#  getdesc <dir> <file>
#    Returns: the description for a file in the file system, if one
#      exists
#    Module: filesys
proc ::tcldrop::filesys::getdesc {dir file} {
	return [::tcldrop::filesys::Getattr $dir $file "desc"]
}

#  getowner <dir> <file>
#    Returns: the owner of a file in the file system
#    Module: filesys
proc ::tcldrop::filesys::getowner {dir file} {
	return [::tcldrop::filesys::Getattr $dir $file "owner"]
}

#  getlink <dir> <file>
#    Returns: the link for a linked file, if it exists
#    Module: filesys
proc ::tcldrop::filesys::getlink {dir file} {
	set result [::tcldrop::filesys::Getattr $dir $file "owner"]
	if {[[string length [lindex $result 0]] != 0 && [string length [lindex $result 1]] != 0} {
		return [join $result "<|>"]
	} else {
		return [list "" ""]
	}
}

#  getflags <dir>
#    Returns: the flags required to access a directory
#    Module: filesys
proc ::tcldrop::filesys::getflags {dir} {
	if {[string equal [file tail $dir] ""]} {
		set result [::tcldrop::filesys::Getattr [file dirname $dir] "[file separator][file tail $dir]" "bothflags"]
	} else {
		set result [::tcldrop::filesys::Getattr [file dirname $dir] [file tail $dir] "bothflags"]
	}
	if {[string length [lindex $result 0]] == 0} {
		return [list [string map {"|-" ""} [lindex $result 0]] [lindex $result 1]]
	} else {
		return [list [lindex $result 0] [lindex $result 1]]
	}
}

# increase the number of gets of a file in the vfs (returns 0 on failure, 1 on success) (NOT EXPORTED)
# for use by the transfer module to update number of gets on dcc send completions
proc ::tcldrop::filesys::incrgetcount {dir file} {
	set result [::tcldrop::filesys::Getattr $dir $file "count"]
	if {[string is integer -strict $result] && $result > 0} {
		set result [::tcldrop::filesys::Setattr $dir $file "count" [expr $result + 1]]
	} else {
		set result [::tcldrop::filesys::Setattr $dir $file "count" 1]
	}
	if {$result == 1} {
		return 1
	} else {
		return 0
	}
}

bind load - filesys ::tcldrop::filesys::LOAD -priority -100
proc ::tcldrop::filesys::LOAD {module} {
	# This variable will store every user in the filesys and their current working directory, each array name is in the form: 14:FireEgl@Botname
	variable Users
	array set Users {}
	setdefault files-path "[file dirname [file normalize "blah"]][file separator]filesys"
	setdefault incoming-path "[file dirname [file normalize "blah"]][file separator]filesys[file separator]incoming"
	setdefault upload-to-pwd 0
	setdefault filedb-path ""
	setdefault max-file-users 20
	#in kb
	setdefault max-filesize 1024
}
