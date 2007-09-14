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

# Notes:
#
# This is an example module for Tcldrop showing the suggested way to do things..
#

# Create the woobie namespace under ::tcldrop:
namespace eval ::tcldrop::woobie {
	# The name of the module:
	variable name {woobie}
	# The internal version of the module (This can be different or the same as the version number in the filename):
	variable version {0.1}
	# Stores the dir/name of the script:
	variable script [info script]
	# This is a list of other modules that this module predepends on:
	variable predepends {core}
	# This is a list of other modules that this module depends on:
	variable depends {core}
	# The main author of this module, or Tcldrop-Dev if there's no primary author:
	variable author {Tcldrop-Dev}
	# The description of this module:
	variable description {An example Tcldrop module.}
	# For CVS repositories, this gets updated (so just leave this alone):
	variable rcsid {$Id$}
	# Export all the commands that should be available to 3rd-party scripters:
	namespace export woobie
	# Store list of commands that will be exported:
	variable commands [namespace export]
	# Store the namespace in a variable:
	variable namespace [namespace current]
	# Set the namespace search path for this module (All modules should at least have ::tcldrop in their namespace path):
	# This is because there may be some internal commands for use only by Tcldrop modules that won't be in the global namespace but will be in the ::tcldrop namespace.
	namespace path [list ::tcldrop]
	# Set the unknown command to be unqualified (default is ::unknown):
	namespace unknown unknown
	# Set the ::modules($name) variable so that information on this module will be known:
	set ::modules($name) [list name $name version $version depends $depends author $author description $description rcsid $rcsid commands $commands script $script namespace $namespace]
	# This sets the version variable to the same as the version in the filename (This is necessary for package require on Tcl Modules to work):
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	# This provides the module so that [package require] on it will work:
	package provide tcldrop::$name $version
	# Also provide it as ${name}::main because we call the filename main-1.tm:
	package provide tcldrop::${name}::main $version
	# If the global tcldrop variable doesn't exist, we're not inside a Tcldrop, so return:
	if {![info exists ::tcldrop]} { return }
	# Note: The package provide commands above are the only thing that is absolutely required.  The rest is just nice to do.
}


proc ::tcldrop::woobie::woobie {args} {
	return "This is an example."
}

# This proc should do anything that needs to be done to initialize this module:
proc ::tcldrop::woobie::LOAD {module} {
	# This proc should do any cleaning up that's necessary before the module gets unloaded:
	proc ::tcldrop::woobie::UNLD {module} {
		# Return 0 to have the package forgot + namespace deleted.
		# Return 1 to have the package forgot (the namespace will remain intact).
		# Return 2 or higher to prevent the package from being forgot (the namespace will remain intact).
		return 0
	}
	# Call the UNLD proc when this module gets unloaded:
	bind unld - woobie ::tcldrop::woobie::UNLD
}
bind load - woobie ::tcldrop::woobie::LOAD


# FixMe: Complete this example module.
