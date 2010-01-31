# deadline queue -- by FireEgl
#
# Description:
# You fill the queue with items and their deadline time in [clock milliseconds].
# You get items from the queue, always receiving items who've reached their deadline first.
# If no items have reached their deadline, you get however many items you requested (default is 0).

# Usage:
# ::dlqueue::dlqueue $name
#   Creates a new command in the namespace it was called from.
#
# $name put $deadline $item ?$id?
#   Adds a single item to the deadline queue.
#   $deadline should be the [clock milliseconds] of when the deadline is to occur.
#   If $id is not specified, $item will be used as the id.
#   (ids are used to be able to have duplicate items in the queue)

# $name get $count
#   Returns a list of items from the deadline queue..
#   Items whose deadline have been reached will always be returned,
#   regardless of $count (they can exceed $count).
#   If no items have reached their deadlines or if there's not enough items
#   who have reached their deadline, the items nearest their deadlines will
#   be returned, up to $count many.

# $name size
#   Returns how many items/ids there are in total in the queue.

# $name clear
#   Removes all items from the queue.

# $name remove $item/$id
#   Removes a single item from the queue.
#   (Specify the items id if you used an id, it's faster that way.)

# $name destroy
#   Removes everything known about the deadline queue named $name, including the command $name.

# $name peek $count
#   Returns items, like get, but doesn't remove them from the queue.
#   FixMe: Add this subcommand.

# Notes:
# * Items are unique (you can only have one) unless you specify an id
#   for the item as the last arg to the "put" subcommand.
# * If you "put" an existing id/item and one by that id already exists,
#   it'll only be replaced if the deadline for the new item/id is sooner
#   than the existing one.
# * Some of the syntax for this package was borrowed from the struct::prioqueue tcllib package.
#   See http://tcllib.sourceforge.net/doc/prioqueue.html

# I release this script into the public domain.

namespace eval ::dlqueue {
	package provide dlqueue 0.9
	variable deadlines
	variable queues
	variable counter
	if {![info exists counter]} { set counter 0 }
	#namespace export "option" "remove" "clear" "destroy" "get" "peek" "put" "size"
}

# Sets up a queue named $name:
proc ::dlqueue::dlqueue {name} {
	namespace ensemble create -command [uplevel 1 {namespace current}]::$name -map [list {option} [list option $name] {remove} [list remove $name] {clear} [list clear $name] {destroy} [list destroy $name] {get} [list get $name] {peek} [list peek $name] {put} [list put $name] {size} [list size $name]]
	variable queues
	set queues($name) [dict create]
	return $name
}

# Adds items to the queue:
proc ::dlqueue::put {name deadline item {id {}}} {
	if {$id eq {}} { set id $item }
	variable deadlines
	variable counter
	variable queues
	set dlkey "${deadline}.[incr counter]"
	if {![dict exists $queues($name) $id]} {
		# $id doesn't exist, so add it..
		dict set deadlines($name) $dlkey $id
		dict set queues($name) $id [dict create deadline $deadline item $item dlkey $dlkey]
	} elseif {$deadline < [dict get $queues($name) $id deadline]} {
		# $id already exists, so replace it if this one has a sooner deadline..
		# Remove the old deadline key:
		dict unset deadlines($name) [dict get $queues($name) $id dlkey]
		# Add the new deadline key:
		dict set deadlines($name) $dlkey
		dict set queues($name) $id [dict create deadline $deadline item $item dlkey $dlkey]
	}
}

# Gets items from the queue:
proc ::dlqueue::get {name {count {0}}} {
	variable queues
	set result [list]
	# Return all items that have reached their deadline (regardless of how many were requested in the count),
	# and any items whose deadline hasn't been reached until we fill the requested count.
	variable deadlines
	dict for {dlkey id} [lsort -stride 2 $deadlines($name)] {
		if {[clock milliseconds] >= [dict get $queues($name) $id deadline] || $count > 0} {
			lappend result [dict get $queues($name) $id item]
			dict unset queues($name) $id
			dict unset deadlines($name) $dlkey
			incr count -1
		}
	}
	return $result
}

# Returns how many items there are:
proc ::dlqueue::size {name} {
	variable queues
	dict size $queues($name)
}

# Remove all items.
proc ::dlqueue::clear {name} {
	variable queues
	set queues($name) [dict create]
	variable deadlines
	array unset deadlines
}

# Remove a single item.
proc ::dlqueue::remove {name item} {
	variable queues
	variable deadlines
	if {[dict exists queues($name) $item]} {
		# item == id
		dict unset deadlines($name) [dict get $queues($name) $item dlkey]
		dict unset queues($name) $item
	} else {
		# item doesn't match an id, try searching all ids for the item:
		dict for {id iteminfo} $queues($name) {
			if {$item eq [dict get $iteminfo item]} {
				# Note: This removes all items named $item.
				dict unset deadlines($name) [dict get $iteminfo dlkey]
				dict unset queues($name) $id
			}
		}
	}
}

# Remove everything we know about queue $name:
proc ::dlqueue::destroy {name} {
	variable deadlines
	unset -nocomplain deadlines($name)
	variable queues
	unset -nocomplain queues($name)
	#variable options
	#unset -nocomplain options($name)
	rename "::$name" {}
}

proc ::dlqueue::peek {name {count {0}}} {
	# FixMe: Complete this.
}

proc ::dlqueue::option {name} {
	# We need options?
}

# I just stuck this in here in case somebody finds it useful..
# It calculates how often this process gets CPU cycles.
proc ::dlqueue::clockres {{testlength {1000}} {min {1}} {max {100}}} {
	set startms [clock clicks -milliseconds]
	while {[clock clicks -milliseconds] - $startms < $testlength} {
		incr all([expr {-[clock clicks -milliseconds] + [after 1 ; clock clicks -milliseconds]}])
	}
	foreach res [lsort -integer -decreasing [array names all]] {
		# Return the resolution less than 100ms (I think if it's over 100ms then the calculation was wrong due to excessive CPU/System load from other processes.)
		if {$res <= $max} { return $res }
	}
	return $min
}
