# woobie/main --
#	Handles:
#		* This is an example Tcldrop module.
#
# $Id$
#
# Copyright (C) 2007 FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
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
# Or can be found on IRC (EFNet, OFTC, or FreeNode) as FireEgl.

# Create the namespace under ::tcldrop:
namespace eval ::tcldrop::woobie {
	# The name of the module:
	variable name {woobie}
	# The internal version of the module:
	variable version {0.1}
	# Stores the dir/name of the script:
	variable script [info script]
	# This sets the version variable to the same as the version in the filename:
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	# This provides the module so that [package requires] on it will work:
	package provide tcldrop::$name $version
	package provide tcldrop::${name}::main $version
	# If the global tcldrop variable doesn't exist, we're not inside a Tcldrop, so return:
	if {![info exists ::tcldrop]} { return }
	# This is a list of other modules that this module depends on:
	variable depends {core}
	# The main author of this module, or Tcldrop-Dev if there's no primary author:
	variable author {Tcldrop-Dev}
	# The description of this module:
	variable description {An example Tcldrop module.}
	# For CVS repositories, this gets updated (so just leave this alone):
	variable rcsid {$Id$}
	# The list of commands that will be exported:
	variable commands [list ]
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export {*}$commands
}

# FixMe: Complete this example module.
