; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 1
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 1	.TITLE "Lisp Machine Marksman Control Microcode"
			; 2	;Based on NEWDSK 31
			; 3	
			; 4	.SEQADR	;Put successive instructions in successive addresses
			; 5	
			; 6	;Field definition
			; 7	; NAME/=J,K,L,M
			; 8	;	J is default value if M is "D"
			; 9	;	K is the field width in bits
			; 10	;	L is 23 minus rightmost bit#, due to backwards pdp10 bit numbering
			; 11	;	M is type, D for defaultable.
			; 12	; followed by value definitions, name=value.
			; 13	
			; 14	;The microcode is a 512x24 PROM.  It is divided into 8 sectors
			; 15	;of 64 words each.  Each sector applies to a different command:
			; 16	;	0	Read
			; 17	;	1	Write
			; 18	;	2	Read All
			; 19	;	3	Write All
			; 20	;	4	2-byte command
			; 21	;	5	1-byte command
			; 22	;	6	not used
			; 23	;	7	not used
			; 24	;Commands 4-7 do not use the memory channel.
			; 25	
			; 26	;When the machine is stopped, the micro instruction and the micro
			; 27	;PC are both zero.  All fields default to zero for this reason.
			; 28	
			; 29	;Microcode field definitions:
			; 30	
			; 31	WRITE GATE/=0,1,0,D		;Write-gate sent to disk
			; 32	
			; 33	READ GATE/=0,1,1,D		;Read-gate sent to disk.  Also selects bit-clock
			; 34					;from read-clock instead of write-clock.  Don't
			; 35					;change this while depending on that clock.
			; 36	
			; 37	LOOP/=0,4,5,D			;Conditionally re-execute the same 
			; 38					; microinstruction.  UIR.LS in hardware.
			; 39	 ;NEVER=0			;Don't loop
			; 40	 ALWAYS=1			;Always (until stopped by DONE or error)
			; 41	 POSC EQ BLOCKSIZE MINUS ONE BYTE=2	;Until almost through a block
			; 42	 ;3 UNUSED
			; 43	 BLOCK CTR EQ BLOCK=4		;Until disk is at correct rotational position
			; 44	 PREAMBLE DETECT=5		;Until RSH contains a start byte
			; 45					; which is 177 octal (see WRITE/ZERO)
			; 46	 POSC EQ ECC FIELD SIZE=6	;Until modulus of error correcting code
			; 47	 END OF BYTE=7			;Until bit counter ends in 7
			; 48	 MK CRDY=10			;Until CRDY from marksman is true
			; 49	 MK ACK=11			;Until CACK from marksman is true
			; 50	 MK STAT AND ACK=12		;Until both CACK and CSTAT from marksman
			; 51	 -MK ACK=13			;Until CACK from marksman is false
			; 52	 -MK STAT=14			;Until CSTAT from marksman is false
			; 53	 ;15,16,17 not used
			; 54	
			; 55	HEADER STROBE/=0,1,6,D		;Set HEADER COMPARE ERROR if byte in RSH does
			; 56					; not agree with disk address register selected
			; 57					; by micro-PC <1:0>
			; 58	
			; 59	DATA FIELD/=0,1,7,D		;1 => We are currently in the data portion of
			; 60					; a disk block.  If reading, the byte in RSH
			; 61					; is transferred to the fifo.  Enables read
			; 62					; compare and internal parity logic to look at
			; 63					; the bits going to and from the disk.
			; 64	
			; 65	GET DATA/=0,1,8,D		;1 => Transfer a byte from fifo to WSH
			; 66					;     for write and/or read-compare.
			; 67	
			; 68	ERR IF START BLOCK/=0,1,9,D	;1 => Error if start block seen, indicating
			; 69					; that we have exceeded a physical sector
			; 70					; boundary.
			; 71	
			; 72	;WS/=0,2,11,D	;Shared field controlling write source and ECC
			; 73	WRITE/=0,2,11,D			;Source of write data sent to disk
			; 74	 ZERO=0		;A byte whose last bit is zero, the rest ones
			; 75	 ONE=1		;All one
			; 76	 SH=2		;Data shifted out from WSH
			; 77	 ECC=3		;Data shifted out from ECC register
			; 78	
			; 79	ECC/=0,2,11,D			;Controls error check and correct logic
			; 80	 FEEDBACK=0	;Input is zero, enable feedback
			; 81	 READ DATA=1	;Input is disk input data (RSH7), enable feedback
			; 82	 WRITE DATA=2	;Input is disk output data, enable feedback
			; 83	 NO FEEDBACK=3	;Bring in zero without feedback.  Use when writing check word
			; 84	
			; 85	DONE TEST/=0,1,12,D		;1 => DONE if memory side is done, i.e.
			; 86			; -MBUSY or (LAST.CCW and CMD.TO.MEMORY and READ [-READ ALL])
			; 87	
			; 88	CLK/=0,2,14,D			;Clock source (CS)
			; 89	 1 USEC=0	;1 microsecond clock from disk
			; 90	 START BLOCK=1	;From disk sector pulse (this is actually the 1 usec
			; 91			; clock, anded with synchronized sector pulse)
			; 92	 BIT=2		;Disk bit clock (CLK.SR)
			; 93	 BYTE=3		;Bit clock divided by 8 (CLK.WD)
			; 94					;Note that the bit clock is faster than the
			; 95					; PROM access time, so it is necessary to
			; 96					; loop at least once when using CLK/BIT.
			; 97	
			; 98	MK REQ/=0,1,15,D
			; 99			;CREQ output to marksman
			; 100	
			; 101	JUMP/=0,2,17,D			;Select instruction to be executed after the
			; 102					; following instruction.  Won't change to
			; 103					; another sector of the PROM.
			; 104					; THIS IS A JUMP-XCT-NEXT OPERATION!
			; 105	 ;NEXT=0	;.+1
			; 106	 START=1	;To start of read or write operation.  Must be used in
			; 107			;conjunction with FUNC/INCREMENT ADDRESS.  Goes to location
			; 108			;0 if a seek or a head-select is required, or to location
			; 109			;12 if we just continue to the following sector.
			; 110	 ECC=2		;to location 72, ecc-decoding routine if an
			; 111			; ECC error is present in the ECC shift register.
			; 112			;If no ECC error, drops through.
			; 113	
			; 114	MK BUS/=0,3,20,D	;Selects output to marksman CBUS
			; 115	 DISK=0		;Let the disk drive it (this is the default)
			; 116	 CMD 1=1	;Misc command byte 1
			; 117	 CMD 2=2	;Misc command byte 2
			; 118	 SEEK 1=3	;Seek command byte 1
			; 119	 SEEK 2=4	;Seek command byte 2
			; 120	 ;5,6,7 unused
			; 121	
			; 122	FUNC/=0,3,23,D			;Miscellaneous functions
			; 123	 ;0 no function
			; 124	 TEST ECC HDR=1		;If ecc non-zero set HEADER ECC ERROR
			; 125	 SET ECC HARD=2		;Set ECC HARD error, which stops machine
			; 126	 TEST ECC=3		;If ecc zero, set ECC SOFT error, which stops machine
			; 127	 CLR ECC+POS=4		;Clear the ECC and POSC registers
			; 128	 CLR RSH=5		;Clear the read shift register
			; 129	 INCREMENT ADDRESS=6	;Increment disk address
			; 130	 ;7 unused
			; 131	î; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 2
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 132	
			; 133	;Macros!!
			; 134	
			; 135	READ GATE		"READ GATE/1"
			; 136	WRITE GATE		"WRITE GATE/1"
			; 137	HEADER STROBE		"HEADER STROBE/1"
			; 138	DATA FIELD		"DATA FIELD/1"
			; 139	GET DATA		"GET DATA/1"
			; 140	ERR IF START BLOCK	"ERR IF START BLOCK/1"
			; 141	DONE TEST		"DONE TEST/1"
			; 142	
			; 143	MK REQ			"MK REQ/1"
			; 144	AWAIT CRDY		"LOOP/MK CRDY"
			; 145	AWAIT ACK		"LOOP/MK ACK"
			; 146	AWAIT NO ACK		"LOOP/-MK ACK"
			; 147	AWAIT NO STAT		"LOOP/-MK STAT"
			; 148	AWAIT STAT ACK		"LOOP/MK STAT AND ACK"
			; 149	CMD 1 TO MK BUS		"MK BUS/CMD 1"
			; 150	CMD 2 TO MK BUS		"MK BUS/CMD 2"
			; 151	SEEK 1 TO MK BUS	"MK BUS/SEEK 1"
			; 152	SEEK 2 TO MK BUS	"MK BUS/SEEK 2"
			; 153	î; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 3
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 154	
			; 155	;1-byte miscellaneous command.  This is good for sequence, recalibrate,
			; 156	;and reading miscellaneous status.  Note that the sequence commands are
			; 157	;automatically generated by hardware when the START switch changes state.
			; 158	;Note that this does not work for head-advance, which we don't use.
			; 159	;The operation is to send the command byte to the microprocessor and
			; 160	;await returned status.
			; 161	
U 0500, 1000,0000	; 162	500:	CLK/1 USEC,AWAIT CRDY		;Wait for microprocessor ready
U 0501, 0000,0010	; 163		CLK/1 USEC,CMD 1 TO MK BUS	;Put command on bus
			; 164		CLK/1 USEC,CMD 1 TO MK BUS,	;Hold command on bus, send
U 0502, 1100,0410	; 165			MK REQ,AWAIT ACK	; request, await acknowledge
U 0503, 1200,0000	; 166		CLK/1 USEC,AWAIT STAT ACK	;Drop command and request, await status
U 0504, 1300,0400	; 167		CLK/1 USEC,MK REQ,AWAIT NO ACK	;Acknowledge status
U 0505, 1400,0000	; 168		CLK/1 USEC,AWAIT NO STAT	;Drop acknowledge, wait for STAT to clear
U 0506, 0000,4000	; 169		CLK/1 USEC,DONE TEST		;Finish
			; 170	
			; 171	;2-byte miscellaneous command.  This is good for seeking, running diagnostics,
			; 172	;and changing the sector length (which you shouldn't do).
			; 173	
U 0400, 1000,0000	; 174	400:	CLK/1 USEC,AWAIT CRDY		;Wait for microprocessor ready
U 0401, 0000,0010	; 175		CLK/1 USEC,CMD 1 TO MK BUS	;Put command on bus
			; 176		CLK/1 USEC,CMD 1 TO MK BUS,	;Hold command on bus, send
U 0402, 1100,0410	; 177			MK REQ,AWAIT ACK	; request, await acknowledge
U 0403, 1300,0000	; 178		CLK/1 USEC,AWAIT NO ACK		;Wait for bus to clear
U 0404, 0000,0020	; 179		CLK/1 USEC,CMD 2 TO MK BUS	;Second byte to bus
			; 180		CLK/1 USEC,CMD 2 TO MK BUS,	;Hold second byte on bus, send
U 0405, 1100,0420	; 181			MK REQ,AWAIT ACK	; request, await acknowledge
U 0406, 1200,0000	; 182		CLK/1 USEC,AWAIT STAT ACK	;Drop command and request, await status
U 0407, 1300,0400	; 183		CLK/1 USEC,MK REQ,AWAIT NO ACK	;Acknowledge status
U 0410, 1400,0000	; 184		CLK/1 USEC,AWAIT NO STAT	;Drop acknowledge, wait for STAT to clear
U 0411, 0000,4000	; 185		CLK/1 USEC,DONE TEST		;Finish
			; 186	
			; 187	;Ignore command 6, the Trident clear-offset command
U 0600, 0000,4000	; 188	600:	CLK/1 USEC,DONE TEST
			; 189	î; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 4
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 190	
			; 191	;Read command.  First step is to do any necessary seek.
			; 192	;Wait until the disk rotates to the start of the desired block, then
			; 193	;read and check the header.  Then read and check the data.  Test for
			; 194	;ECC error.  Loop back to the beginning if the channel command list is 
			; 195	;not exhausted.  Reading (header or data) involves delaying for head
			; 196	;select and PLO synchronization, then waiting for the start byte to
			; 197	;appear, then transferring the data bytes, then transferring the ECC bytes.
			; 198	
U 0000, 1000,0000	; 199	000:	CLK/1 USEC,AWAIT CRDY		;Wait for microprocessor ready
U 0001, 0000,0030	; 200		CLK/1 USEC,SEEK 1 TO MK BUS	;Put seek command on bus
			; 201		CLK/1 USEC,SEEK 1 TO MK BUS,	;Hold command on bus, send
U 0002, 1100,0430	; 202			MK REQ,AWAIT ACK	; request, await acknowledge
U 0003, 1300,0000	; 203		CLK/1 USEC,AWAIT NO ACK		;Wait for bus to clear
U 0004, 0000,0040	; 204		CLK/1 USEC,SEEK 2 TO MK BUS	;Second byte to bus
			; 205		CLK/1 USEC,SEEK 2 TO MK BUS,	;Hold second byte on bus, send
U 0005, 1100,0440	; 206			MK REQ,AWAIT ACK	; request, await acknowledge
U 0006, 1200,0000	; 207		CLK/1 USEC,AWAIT STAT ACK	;Drop command and request, await status
U 0007, 1300,0400	; 208		CLK/1 USEC,MK REQ,AWAIT NO ACK	;Acknowledge status
U 0010, 1400,0000	; 209		CLK/1 USEC,AWAIT NO STAT	;Drop acknowledge, wait for STAT to clear
			; 210			;*** Following line not necessarily supported by documentation ***
U 0011, 1000,0000	; 211		CLK/1 USEC,AWAIT CRDY		;Wait completion of seek
			; 212	;Following instruction must be at location 012, assembler will not check
U 0012, 0400,1000	; 213	12:	CLK/START BLOCK,LOOP/BLOCK CTR EQ BLOCK	;Get to correct rotational position
U 0013, 0000,0000	; 214		CLK/1 USEC			;16 microsecond delay before read gate
U 0014, 0000,0000	; 215		CLK/1 USEC
U 0015, 0000,0000	; 216		CLK/1 USEC
U 0016, 0000,0000	; 217		CLK/1 USEC
U 0017, 0000,0000	; 218		CLK/1 USEC
U 0020, 0000,0000	; 219		CLK/1 USEC
U 0021, 0000,0000	; 220		CLK/1 USEC
U 0022, 0000,0000	; 221		CLK/1 USEC
U 0023, 0000,0000	; 222		CLK/1 USEC
U 0024, 0000,0000	; 223		CLK/1 USEC
U 0025, 0000,0000	; 224		CLK/1 USEC
U 0026, 0000,0000	; 225		CLK/1 USEC
U 0027, 0000,0000	; 226		CLK/1 USEC
U 0030, 0000,0000	; 227		CLK/1 USEC
U 0031, 0000,0000	; 228		CLK/1 USEC
			; 229		CLK/1 USEC,
U 0032, 0000,0005	; 230			FUNC/CLR RSH		;Clear SH so preamble can get detected
			; 231		READ GATE,FUNC/CLR ECC+POS,ERR IF START BLOCK,
U 0033, 2504,2004	; 232			CLK/BIT,LOOP/PREAMBLE DETECT	;Search for start byte
			; 233	;Low order 2 bits of this location must be 00, used by header compare logic.
			; 234	;You better check that this location really belongs at 34, the assembler won't.
			; 235	034:	READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare first header byte
U 0034, 2045,3000	; 236			CLK/BYTE,ERR IF START BLOCK
			; 237		READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare second header byte
U 0035, 2045,3000	; 238			CLK/BYTE,ERR IF START BLOCK
			; 239		READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare third header byte
U 0036, 2045,3000	; 240			CLK/BYTE,ERR IF START BLOCK
			; 241		READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare fourth header byte
U 0037, 2045,3000	; 242			CLK/BYTE,ERR IF START BLOCK
U 0040, 2005,3000	; 243		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK	;Read checkword
U 0041, 2005,3000	; 244		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK
U 0042, 2005,3000	; 245		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK
			; 246		READ GATE,ECC/READ DATA,CLK/BYTE,	;Set header ECC error if so
U 0043, 2005,3001	; 247			FUNC/TEST ECC HDR,ERR IF START BLOCK
U 0044, 0004,0000	; 248		CLK/1 USEC,ERR IF START BLOCK	;Read gate off for 4 microseconds
U 0045, 0004,0000	; 249		CLK/1 USEC,ERR IF START BLOCK	; to resychronize PLO
U 0046, 0004,0000	; 250		CLK/1 USEC,ERR IF START BLOCK
			; 251		CLK/1 USEC,ERR IF START BLOCK,
U 0047, 0004,0005	; 252			FUNC/CLR RSH		;Also, clear SH for preamble detect
			; 253		READ GATE,FUNC/CLR ECC+POS,CLK/BIT,	;Find data preamble
U 0050, 2504,2004	; 254			LOOP/PREAMBLE DETECT,ERR IF START BLOCK
			; 255		READ GATE,FUNC/CLR ECC+POS,CLK/BYTE,	;Read 1 padding byte, and
U 0051, 2014,3004	; 256			GET DATA,ERR IF START BLOCK	; get 1st byte for read-compare
			; 257		READ GATE,DATA FIELD,ECC/READ DATA,	;Transfer the data byte
			; 258			CLK/BYTE,ERR IF START BLOCK,GET DATA,  ;get data is for read compare
U 0052, 2235,3000	; 259			LOOP/POSC EQ BLOCKSIZE MINUS ONE BYTE
			; 260		READ GATE,DATA FIELD,ECC/READ DATA,	;Transfer the last byte
U 0053, 2025,3000	; 261			CLK/BYTE,ERR IF START BLOCK
			; 262		READ GATE,ECC/READ DATA,CLK/BYTE,	;Read the checkword
U 0054, 2005,3000	; 263			ERR IF START BLOCK
U 0055, 2005,3000	; 264		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK
U 0056, 2005,3000	; 265		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK
			; 266		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK,
U 0057, 2005,3200	; 267			JUMP/ECC		;To ECC routine if doesn't check
U 0060, 2000,3000	; 268		READ GATE,ECC/FEEDBACK,CLK/BYTE	;Jump delay
U 0061, 0000,4000	; 269		CLK/1 USEC,DONE TEST		;Stop without inc'ing address if done
			; 270		CLK/1 USEC,FUNC/INCREMENT ADDRESS,
U 0062, 0000,0106	; 271			JUMP/START		;To location 0 or 12 to do next sector
U 0063, 0000,0000	; 272		CLK/1 USEC			;Jump delay
			; 273	
			; 274	;Error-correction.  Run the ECC register the right number of times
			; 275	;to make the cyclic code repeat, then run it through the data field again,
			; 276	;looking for zero.  If found, we have the error bits, if not found
			; 277	;too many bits were in error, set ECC hard.  We leave READ GATE set
			; 278	;so as to get a clock from the disk.
			; 279	
			; 280	072:	READ GATE,CLK/BIT,ECC/FEEDBACK,	;Run ECC until code cycles
U 0072, 2600,2000	; 281			LOOP/POSC EQ ECC FIELD SIZE	;Takes about 3 milliseconds
			; 282		READ GATE,CLK/BIT,ECC/FEEDBACK,	;Scan for error burst
U 0073, 2200,2003	; 283			FUNC/TEST ECC,LOOP/POSC EQ BLOCKSIZE MINUS ONE BYTE
			; 284		READ GATE,CLK/BIT,ECC/FEEDBACK,	;Scan second to last byte (except 1st bit)
U 0074, 2700,2003	; 285			FUNC/TEST ECC,LOOP/END OF BYTE
			; 286		READ GATE,CLK/BIT,ECC/FEEDBACK,	;Scan last byte
U 0075, 2700,2003	; 287			FUNC/TEST ECC,LOOP/END OF BYTE
U 0076, 0000,0002	; 288		CLK/1 USEC,FUNC/SET ECC HARD	;Give up
			; 289	î; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 5
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 290	
			; 291	;Write command.  First step is to do any necessary seek.
			; 292	;Wait until the disk rotates to the start of the desired block, then
			; 293	;read and check the header.  Now write the data preamble, the data,
			; 294	;and the ECC word.  Loop back to the beginning if the channel command list is 
			; 295	;not exhausted.
			; 296	
U 0100, 1000,0000	; 297	100:	CLK/1 USEC,AWAIT CRDY		;Wait for microprocessor ready
U 0101, 0000,0030	; 298		CLK/1 USEC,SEEK 1 TO MK BUS	;Put seek command on bus
			; 299		CLK/1 USEC,SEEK 1 TO MK BUS,	;Hold command on bus, send
U 0102, 1100,0430	; 300			MK REQ,AWAIT ACK	; request, await acknowledge
U 0103, 1300,0000	; 301		CLK/1 USEC,AWAIT NO ACK		;Wait for bus to clear
U 0104, 0000,0040	; 302		CLK/1 USEC,SEEK 2 TO MK BUS	;Second byte to bus
			; 303		CLK/1 USEC,SEEK 2 TO MK BUS,	;Hold second byte on bus, send
U 0105, 1100,0440	; 304			MK REQ,AWAIT ACK	; request, await acknowledge
U 0106, 1200,0000	; 305		CLK/1 USEC,AWAIT STAT ACK	;Drop command and request, await status
U 0107, 1300,0400	; 306		CLK/1 USEC,MK REQ,AWAIT NO ACK	;Acknowledge status
U 0110, 1400,0000	; 307		CLK/1 USEC,AWAIT NO STAT	;Drop acknowledge, wait for STAT to clear
			; 308			;*** Following line not necessarily supported by documentation ***
U 0111, 1000,0000	; 309		CLK/1 USEC,AWAIT CRDY		;Wait completion of seek
			; 310	;Following instruction must be at location 112, assembler will not check.
U 0112, 0400,1000	; 311	112:	CLK/START BLOCK,LOOP/BLOCK CTR EQ BLOCK	;Get to correct rotational position
U 0113, 0000,0000	; 312		CLK/1 USEC			;16 microsecond delay before read gate
U 0114, 0000,0000	; 313		CLK/1 USEC
U 0115, 0000,0000	; 314		CLK/1 USEC
U 0116, 0000,0000	; 315		CLK/1 USEC
U 0117, 0000,0000	; 316		CLK/1 USEC
U 0120, 0000,0000	; 317		CLK/1 USEC
U 0121, 0000,0000	; 318		CLK/1 USEC
U 0122, 0000,0000	; 319		CLK/1 USEC
U 0123, 0000,0000	; 320		CLK/1 USEC
U 0124, 0000,0000	; 321		CLK/1 USEC
U 0125, 0000,0000	; 322		CLK/1 USEC
U 0126, 0000,0000	; 323		CLK/1 USEC
U 0127, 0000,0000	; 324		CLK/1 USEC
U 0130, 0000,0000	; 325		CLK/1 USEC
U 0131, 0000,0000	; 326		CLK/1 USEC
			; 327		CLK/1 USEC,
U 0132, 0000,0005	; 328			FUNC/CLR RSH		;Clear SH so preamble can get detected
			; 329		READ GATE,FUNC/CLR ECC+POS,ERR IF START BLOCK,
U 0133, 2504,2004	; 330			CLK/BIT,LOOP/PREAMBLE DETECT	;Search for start byte
			; 331	;Low order 2 bits of this location must be 00, used by header compare logic.
			; 332	;You better check that this location really belongs at 134, the assembler won't.
			; 333	134:	READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare first header byte
U 0134, 2045,3000	; 334			CLK/BYTE,ERR IF START BLOCK
			; 335		READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare second header byte
U 0135, 2045,3000	; 336			CLK/BYTE,ERR IF START BLOCK
			; 337		READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare third header byte
U 0136, 2045,3000	; 338			CLK/BYTE,ERR IF START BLOCK
			; 339		READ GATE,HEADER STROBE,ECC/READ DATA,	;Compare fourth header byte
U 0137, 2045,3000	; 340			CLK/BYTE,ERR IF START BLOCK
U 0140, 2005,3000	; 341		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK	;Read checkword
U 0141, 2005,3000	; 342		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK
U 0142, 2005,3000	; 343		READ GATE,ECC/READ DATA,CLK/BYTE,ERR IF START BLOCK
			; 344		READ GATE,ECC/READ DATA,CLK/BYTE,	;Set header ECC error if so
U 0143, 2005,3001	; 345			FUNC/TEST ECC HDR,ERR IF START BLOCK
			; 346		CLK/1 USEC,WRITE GATE,WRITE/ONE,	;Switch from read to write
U 0144, 4005,0000	; 347			ERR IF START BLOCK		;Note can't use disk clock
			; 348		;Write an additional 15 bytes of VFO relock
U 0145, 4005,3000	; 349		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0146, 4005,3000	; 350		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0147, 4005,3000	; 351		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0150, 4005,3000	; 352		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0151, 4005,3000	; 353		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0152, 4005,3000	; 354		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0153, 4005,3000	; 355		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0154, 4005,3000	; 356		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0155, 4005,3000	; 357		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0156, 4005,3000	; 358		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0157, 4005,3000	; 359		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0160, 4005,3000	; 360		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0161, 4005,3000	; 361		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0162, 4005,3000	; 362		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0163, 4005,3000	; 363		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK
U 0164, 4004,3000	; 364		WRITE GATE,WRITE/ZERO,CLK/BYTE,ERR IF START BLOCK	;sync byte (177)
			; 365		WRITE GATE,WRITE/ONE,CLK/BYTE,GET DATA,	;Padding byte, get 1st data
U 0165, 4015,3004	; 366			FUNC/CLR ECC+POS,ERR IF START BLOCK
			; 367		WRITE GATE,WRITE/SH,CLK/BYTE,		;Write out the data bytes
			; 368			ECC/WRITE DATA,ERR IF START BLOCK,DATA FIELD,
U 0166, 4236,3000	; 369			GET DATA,LOOP/POSC EQ BLOCKSIZE MINUS ONE BYTE
			; 370		WRITE GATE,WRITE/SH,CLK/BYTE,		;Write last data byte
U 0167, 4026,3000	; 371			ECC/WRITE DATA,ERR IF START BLOCK,DATA FIELD
			; 372		WRITE GATE,WRITE/ECC,CLK/BYTE,		;Write the checkword
U 0170, 4007,3000	; 373			ECC/NO FEEDBACK,ERR IF START BLOCK
U 0171, 4007,3000	; 374		WRITE GATE,WRITE/ECC,CLK/BYTE,ECC/NO FEEDBACK,ERR IF START BLOCK
U 0172, 4007,3000	; 375		WRITE GATE,WRITE/ECC,CLK/BYTE,ECC/NO FEEDBACK,ERR IF START BLOCK
U 0173, 4007,3000	; 376		WRITE GATE,WRITE/ECC,CLK/BYTE,ECC/NO FEEDBACK,ERR IF START BLOCK
			; 377		;Write guard byte so write doesn't turn off in the middle of ECC
U 0174, 4005,7000	; 378		WRITE GATE,WRITE/ONE,CLK/BYTE,ERR IF START BLOCK,DONE TEST
			; 379		CLK/1 USEC,FUNC/INCREMENT ADDRESS,
U 0175, 0000,0106	; 380			JUMP/START		;To location 0 or 12 to do next sector
U 0176, 0000,0000	; 381		CLK/1 USEC			;Jump delay
			; 382	î; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 6
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 383	
			; 384	;Read All operation.  Positions disk to desired cylinder, head,
			; 385	; and rotational position, then simply starts reading.  Keeps on
			; 386	; reading until memory channel is done, actually a little bit
			; 387	; farther due to fifo, then stops; the extra garbage left in the
			; 388	; fifo is discarded.
			; 389	;Note that internal parity checking does not work in the Read All
			; 390	; and Write All operations, because of this extra garbage in the fifo.
			; 391	
U 0200, 1000,0000	; 392	200:	CLK/1 USEC,AWAIT CRDY		;Wait for microprocessor ready
U 0201, 0000,0030	; 393		CLK/1 USEC,SEEK 1 TO MK BUS	;Put seek command on bus
			; 394		CLK/1 USEC,SEEK 1 TO MK BUS,	;Hold command on bus, send
U 0202, 1100,0430	; 395			MK REQ,AWAIT ACK	; request, await acknowledge
U 0203, 1300,0000	; 396		CLK/1 USEC,AWAIT NO ACK		;Wait for bus to clear
U 0204, 0000,0040	; 397		CLK/1 USEC,SEEK 2 TO MK BUS	;Second byte to bus
			; 398		CLK/1 USEC,SEEK 2 TO MK BUS,	;Hold second byte on bus, send
U 0205, 1100,0440	; 399			MK REQ,AWAIT ACK	; request, await acknowledge
U 0206, 1200,0000	; 400		CLK/1 USEC,AWAIT STAT ACK	;Drop command and request, await status
U 0207, 1300,0400	; 401		CLK/1 USEC,MK REQ,AWAIT NO ACK	;Acknowledge status
U 0210, 1400,0000	; 402		CLK/1 USEC,AWAIT NO STAT	;Drop acknowledge, wait for STAT to clear
			; 403			;*** Following line not necessarily supported by documentation ***
U 0211, 1000,0000	; 404		CLK/1 USEC,AWAIT CRDY		;Wait completion of seek
U 0212, 0400,1000	; 405		CLK/START BLOCK,LOOP/BLOCK CTR EQ BLOCK	;Get to correct rotational position
U 0213, 0000,0000	; 406		CLK/1 USEC			;16 microsecond delay before read gate
U 0214, 0000,0000	; 407		CLK/1 USEC
U 0215, 0000,0000	; 408		CLK/1 USEC
U 0216, 0000,0000	; 409		CLK/1 USEC
U 0217, 0000,0000	; 410		CLK/1 USEC
U 0220, 0000,0000	; 411		CLK/1 USEC
U 0221, 0000,0000	; 412		CLK/1 USEC
U 0222, 0000,0000	; 413		CLK/1 USEC
U 0223, 0000,0000	; 414		CLK/1 USEC
U 0224, 0000,0000	; 415		CLK/1 USEC
U 0225, 0000,0000	; 416		CLK/1 USEC
U 0226, 0000,0000	; 417		CLK/1 USEC
U 0227, 0000,0000	; 418		CLK/1 USEC
U 0230, 0000,0000	; 419		CLK/1 USEC
U 0231, 0000,0000	; 420		CLK/1 USEC
			; 421		CLK/1 USEC,
U 0232, 0000,0004	; 422			FUNC/CLR ECC+POS	;Clear pos to sync byte counter
			; 423		READ GATE,DATA FIELD,CLK/BYTE,	;Transfer data to memory, until
U 0233, 2120,7000	; 424			LOOP/ALWAYS,DONE TEST	; it is done
			; 425	î; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 7
; DSK: MOON; MKSMAN 39	01:31:32 22-OCT-79	

			; 426	
			; 427	;Write all operation.  Positions to cylinder, head, and rotational
			; 428	; position then simply firehoses bytes out until the memory channel
			; 429	; is done.  Note that the last 0 to 64 bytes will not get written
			; 430	; on the disk, depending on how full the fifo is when the channel
			; 431	; terminates.  Also, the channel always transfers a multiple of
			; 432	; a page (1024 bytes).  This means you don't have too much control
			; 433	; over exactly how much gets written, but, when formatting, the
			; 434	; only part of the last block in a track that really has to get written
			; 435	; is the header, so you can stop any place in the data field.  This
			; 436	; means this block will get formatted with a bad ECC, but, who cares,
			; 437	; the data in it is not valid anyway.
			; 438	
U 0300, 1000,0000	; 439	300:	CLK/1 USEC,AWAIT CRDY		;Wait for microprocessor ready
U 0301, 0000,0030	; 440		CLK/1 USEC,SEEK 1 TO MK BUS	;Put seek command on bus
			; 441		CLK/1 USEC,SEEK 1 TO MK BUS,	;Hold command on bus, send
U 0302, 1100,0430	; 442			MK REQ,AWAIT ACK	; request, await acknowledge
U 0303, 1300,0000	; 443		CLK/1 USEC,AWAIT NO ACK		;Wait for bus to clear
U 0304, 0000,0040	; 444		CLK/1 USEC,SEEK 2 TO MK BUS	;Second byte to bus
			; 445		CLK/1 USEC,SEEK 2 TO MK BUS,	;Hold second byte on bus, send
U 0305, 1100,0440	; 446			MK REQ,AWAIT ACK	; request, await acknowledge
U 0306, 1200,0000	; 447		CLK/1 USEC,AWAIT STAT ACK	;Drop command and request, await status
U 0307, 1300,0400	; 448		CLK/1 USEC,MK REQ,AWAIT NO ACK	;Acknowledge status
U 0310, 1400,0000	; 449		CLK/1 USEC,AWAIT NO STAT	;Drop acknowledge, wait for STAT to clear
			; 450			;*** Following line not necessarily supported by documentation ***
U 0311, 1000,0000	; 451		CLK/1 USEC,AWAIT CRDY		;Wait completion of seek
			; 452		CLK/START BLOCK,		;Get to correct rotational position
			; 453			LOOP/BLOCK CTR EQ BLOCK,
U 0312, 0400,1004	; 454			FUNC/CLR ECC+POS	;Clear POSC to sync byte counter
			; 455		WRITE GATE,WRITE/ONE,		;Write 8 1's, synchronize clock,
U 0313, 4011,0000	; 456			CLK/1 USEC,GET DATA	;Fetch first data byte
			; 457		WRITE GATE,WRITE/SH,CLK/BYTE,	;Write it all out
			; 458			DATA FIELD,GET DATA,
U 0314, 4132,7000	; 459			LOOP/ALWAYS,DONE TEST
			; 460	
			; 461	;END.
			; 462	


; Number of Micro Words used:
;	D Words= 0
;	U Words= 179

	END
; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 8
; CROSS REFERENCE LISTING

(U) CLK				88 #
	BIT			92 #	232	253	280	282	284	286	330
	BYTE			93 #	236	238	240	242	243	244	245	246	255	258	261
				262	264	265	266	268	334	336	338	340	341	342	343
				344	349	350	351	352	353	354	355	356	357	358	359
				360	361	362	363	364	365	367	370	372	374	375	376
				378	423	457
	START BLOCK		90 #	213	311	405	452
	1 USEC			89 #	162	163	164	166	167	168	169	174	175	176	178
				179	180	182	183	184	185	188	199	200	201	203	204
				205	207	208	209	211	214	215	216	217	218	219	220
				221	222	223	224	225	226	227	228	229	248	249	250
				251	269	270	272	288	297	298	299	301	302	303	305
				306	307	309	312	313	314	315	316	317	318	319	320
				321	322	323	324	325	326	327	346	379	381	392	393
				394	396	397	398	400	401	402	404	406	407	408	409
				410	411	412	413	414	415	416	417	418	419	420	421
				439	440	441	443	444	445	447	448	449	451	456
(U) DATA FIELD			59 #	257	260	368	371	423	458
(U) DONE TEST			85 #	169	185	188	269	378	424	459
(U) ECC				79 #
	FEEDBACK		80 #	268	280	282	284	286
	NO FEEDBACK		83 #	373	374	375	376
	READ DATA		81 #	235	237	239	241	243	244	245	246	257	260	262
				264	265	266	333	335	337	339	341	342	343	344
	WRITE DATA		82 #	368	371
(U) ERR IF START BLOCK		68 #	231	236	238	240	242	243	244	245	247	248	249
				250	251	254	256	258	261	263	264	265	266	329	334
				336	338	340	341	342	343	345	347	349	350	351	352
				353	354	355	356	357	358	359	360	361	362	363	364
				366	368	371	373	374	375	376	378
(U) FUNC			122 #
	CLR ECC+POS		127 #	231	253	255	329	366	422	454
	CLR RSH			128 #	230	252	328
	INCREMENT ADDRESS	129 #	270	379
	SET ECC HARD		125 #	288
	TEST ECC		126 #	283	285	287
	TEST ECC HDR		124 #	247	345
(U) GET DATA			65 #	256	258	365	369	456	458
(U) HEADER STROBE		55 #	235	237	239	241	333	335	337	339
(U) JUMP			101 #
	ECC			110 #	267
	START			106 #	271	380
(U) LOOP			37 #
	ALWAYS			40 #	424	459
	BLOCK CTR EQ BLOCK	43 #	213	311	405	453
	END OF BYTE		47 #	285	287
	MK ACK			49 #	165	177	181	202	206	300	304	395	399	442	446
	MK CRDY			48 #	162	174	199	211	297	309	392	404	439	451
	MK STAT AND ACK		50 #	166	182	207	305	400	447
	POSC EQ BLOCKSIZE M	41 #	259	283	369
	POSC EQ ECC FIELD S	46 #	281
	PREAMBLE DETECT		44 #	232	254	330
	-MK ACK			51 #	167	178	183	203	208	301	306	396	401	443	448
	-MK STAT		52 #	168	184	209	307	402	449
; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 9
; CROSS REFERENCE LISTING

(U) MACRO%
	AWAIT ACK		145 #	165	177	181	202	206	300	304	395	399	442	446
	AWAIT CRDY		144 #	162	174	199	211	297	309	392	404	439	451
	AWAIT NO ACK		146 #	167	178	183	203	208	301	306	396	401	443	448
	AWAIT NO STAT		147 #	168	184	209	307	402	449
	AWAIT STAT ACK		148 #	166	182	207	305	400	447
	CMD 1 TO MK BUS		149 #	163	164	175	176
	CMD 2 TO MK BUS		150 #	179	180
	DATA FIELD		138 #	257	260	368	371	423	458
	DONE TEST		141 #	169	185	188	269	378	424	459
	ERR IF START BLOCK	140 #	231	236	238	240	242	243	244	245	247	248	249
				250	251	254	256	258	261	263	264	265	266	329	334
				336	338	340	341	342	343	345	347	349	350	351	352
				353	354	355	356	357	358	359	360	361	362	363	364
				366	368	371	373	374	375	376	378
	GET DATA		139 #	256	258	365	369	456	458
	HEADER STROBE		137 #	235	237	239	241	333	335	337	339
	MK REQ			143 #	165	167	177	181	183	202	206	208	300	304	306
				395	399	401	442	446	448
	READ GATE		135 #	231	235	237	239	241	243	244	245	246	253	255
				257	260	262	264	265	266	268	280	282	284	286	329
				333	335	337	339	341	342	343	344	423
	SEEK 1 TO MK BUS	151 #	200	201	298	299	393	394	440	441
	SEEK 2 TO MK BUS	152 #	204	205	302	303	397	398	444	445
	WRITE GATE		136 #	346	349	350	351	352	353	354	355	356	357	358
				359	360	361	362	363	364	365	367	370	372	374	375
				376	378	455	457
(U) MK BUS			114 #
	CMD 1			116 #	163	164	175	176
	CMD 2			117 #	179	180
	DISK			115 #
	SEEK 1			118 #	200	201	298	299	393	394	440	441
	SEEK 2			119 #	204	205	302	303	397	398	444	445
(U) MK REQ			98 #	165	167	177	181	183	202	206	208	300	304	306
				395	399	401	442	446	448
(U) READ GATE			33 #	231	235	237	239	241	243	244	245	246	253	255
				257	260	262	264	265	266	268	280	282	284	286	329
				333	335	337	339	341	342	343	344	423
(U) WRITE			73 #
	ECC			77 #	372	374	375	376
	ONE			75 #	346	349	350	351	352	353	354	355	356	357	358
				359	360	361	362	363	365	378	455
	SH			76 #	367	370	457
	ZERO			74 #	364
(U) WRITE GATE			31 #	346	349	350	351	352	353	354	355	356	357	358
				359	360	361	362	363	364	365	367	370	372	374	375
				376	378	455	457
; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 10
; LOCATION / LINE NUMBER INDEX
; DCODE LOC'N	0	1	2	3	4	5	6	7

D 0000		
; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 11
; LOCATION / LINE NUMBER INDEX
; UCODE LOC'N	0	1	2	3	4	5	6	7

U 0000		199	200	202	203	204	206	207	208
U 0010		209	211	213	214	215	216	217	218
U 0020		219	220	221	222	223	224	225	226
U 0030		227	228	230	232	236	238	240	242
U 0040		243	244	245	247	248	249	250	252
U 0050		254	256	259	261	263	264	265	267
U 0060		268	269	271	272				
U 0070				281	283	285	287	288	

U 0100		297	298	300	301	302	304	305	306
U 0110		307	309	311	312	313	314	315	316
U 0120		317	318	319	320	321	322	323	324
U 0130		325	326	328	330	334	336	338	340
U 0140		341	342	343	345	347	349	350	351
U 0150		352	353	354	355	356	357	358	359
U 0160		360	361	362	363	364	366	369	371
U 0170		373	374	375	376	378	380	381	

U 0200		392	393	395	396	397	399	400	401
U 0210		402	404	405	406	407	408	409	410
U 0220		411	412	413	414	415	416	417	418
U 0230		419	420	422	424				
U 0240									
U 0250									
U 0260									
U 0270									

U 0300		439	440	442	443	444	446	447	448
U 0310		449	451	454	456	459			
U 0320									
U 0330									
U 0340									
U 0350									
U 0360									
U 0370									

U 0400		174	175	177	178	179	181	182	183
U 0410		184	185						
U 0420									
U 0430									
U 0440									
U 0450									
U 0460									
U 0470									

U 0500		162	163	165	166	167	168	169	
U 0510									
U 0520									
U 0530									
U 0540									
U 0550									
U 0560									
U 0570									

; DSK: MOON; MKSMAN MCR	02:59:46 25-OCT-79	MICRO 52	LISP MACHINE MARKSMAN CONTROL MICROCODE	PAGE 12
; LOCATION / LINE NUMBER INDEX
; UCODE LOC'N	0	1	2	3	4	5	6	7

U 0600		188

NO ERRORS DETECTED
END OF MICRO CODE ASSEMBLY
USED 1.70 SECONDS
