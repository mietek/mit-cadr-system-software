Programming documentation for the serial I/O interface on the IOB

This does synchronous and asynchronous serial communications,
including data set control.

Interrupts.  Interrupts only exist for input.  The interrupt enable
is bit 7 of location 764112 (the same register that contains the
keyboard, mouse, and clock interrupt enables.)  The vector is 264 .

Programming.  The unibus addresses responded to are 764160-764177.
There is a weird kludge where some locations do different
things on different accesses.  This is reset by bus reset and by
reading location 764166.  Reading from this device always puts
garbage (typically all 1's) in the high-order 8 bits of the
returned value.

764160	(read) Received data.
	(write) Transmit data.

764162	(read) Status as follows:
		200 Data set ready
		100 Carrier Detect
		 40 asynchronous: framing error (break received)
		    synchronous: sync character detected
		 20 receive overrun
		 10 asynchronous: parity error received
		    sychronous: parity error or DLE
		  4 transmit idle or data-set status change
		  2 receive ready with character
		  1 transmit ready for next character
	(write) SYN1 then SYN2 then DLE (writes a different thing
		each time).  These are for synchronous mode only,
		read the data sheet on the Signetics 2651 for details.

764164	(read/write) MODE1 then MODE2 (reads a different thing each time).
	 MODE1	300 asynchronous: number of stop bits (illegal, 1, 1 1/2, 2)
		    synchronous: 200 = single sync char, 100 = "transparent mode"
		 40 even parity
		 20 enable parity
		 14 character length (5, 6, 7, 8) bits + parity(if any)
		  3 mode:
			0 synchronous
			1 asynchronous
			2 asynchronous, divide external clock by 16
			3 asynchronous, divide external clock by 64
	 MODE2	300 unused
		 40 transmit clock (external, internal) (must be 1)
		 20 receive clock (external, internal) (must be 1)
		 17 baud rate for internal clock (both transmit and receive):
		    0 = 50	4 = 150	    10 = 1800	14 = 4800
		    1 = 75	5 = 300	    11 = 2000	15 = 7200
		    2 = 110	6 = 600	    12 = 2400	16 = 9600
		    3 = 134.5	7 = 1200    13 = 3600	17 = 19200

764166	(read/write) Command register as follows:
		300 random modes:
			0 normal
			1 asynchronous: automatic echo
			  synchronous: syn/dle stripping
			2 local loop back (receives what it transmits)
			3 remote loop back (echos what it receives)
		 40 request to send
		 20 reset error flags (write only)
		 10 asynchronous: send break
		    synchronous: send dle
		  4 enable receiver
		  2 data terminal ready
		  1 enable transmitter
