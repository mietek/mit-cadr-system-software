Changes in system 45:

A new special form, WITH-OPEN-FILE.  This should be used in most cases
rather than calling OPEN, since it arranges for proper cleanup upon
abnormal exit.
	(WITH-OPEN-FILE (var filename options)
		body1
		body2 ... )
evaluates the forms body1, body2, ... with var bound to a stream
which is the result of (OPEN filename options).  The stream is
closed after evaluating the body.  The stream is also closed if
control is thrown out of the body, and in this case if if the file
is being written the partially-written file is deleted.

New hash-table facility available.  Documentation is in a section
of the file AI:LMMAN;FD.CON >.

If you don't know about the WITH-INPUT-FROM-STRING and WITH-OUTPUT-TO-STRING
special forms, documentation may be found in the file AI:LMMAN;FD.STR >.

Some serious bugs in the process system have been fixed.  The most
noticeable was that the Abort key did not run unwind-protects!

The usual large number of small bug fixes, especially to file names,
processes, and typeahead.

A couple things have been renamed; probably few people were using them.
The :DRAW-FILLED-IN-CHORD message is now :DRAW-FILLED-IN-SECTOR.
If you added anything to TV:WINDOW-TYPES-ITEM-LIST you should
add it to TV:DEFAULT-WINDOW-TYPES-ITEM-LIST instead; the reason is
that this list can be per-screen and per-frame now.  The :STATUS
message returns a new value, :EXPOSED-IN-SUPERIOR.

This system doesn't have a ZMAIL yet; it will soon.
