;;; -*- Mode: Lisp; Base: 8; Package: User -*-

(defun complement (x)
  (logxor 377 x))

(setq bitmap 60)	;60-77 Bit map.  0 if key up, 1 if key down.
;To read from keyboard, P1<4:1> gets column number, then P1<0> gets 0,
;then read back keys from P2, then P1<1> gets 1 again.

;P1<7> = pin 5 on the connector
;P1<6> = pin 3 on the connector	(used for data)
;P1<5> = pin 1 on the connector
;T1    = pin 2 on the connector (used for clock)

;The following are the bit numbers of the keys which are specially
;known about for shifting purposes:
;  mode lock	3
;  caps lock	125
;  alt lock	15
;  repeat	115
;  top		104 / 155
;  greek	44 / 35
;  shift	24 / 25
;  hyper	145 / 175
;  super	5 / 65
;  meta		45 / 165
;  control	20 / 26

; For booting, we know that rubout is 23 and return is 136

(putprop 'ukbd '(
	(= 0)
	(jmp beg)

	(= 100)
beg	(mov a (/# 377))
	(outl p1 a)			;Mainly turn off data out
	(outl p2 a)			;Enable input
	(mov r0 (/# bitmap))		;Clear the bitmap
	(mov r1 (/# 20))
	(clr a)
clear-bitmap-loop
	(mov @r0 a)
	(inc r0)
	(djnz r1 clear-bitmap-loop)
;Scanning Loop.
;R2 is bitmap index in bytes, plus 1
;R0,R1 temporary as usual

scan-keyboard
	(mov r2 (/# 20))
scan-loop
	(mov a (/# (1- bitmap)))
	(add a r2)			;Address of bitmap byte
	(mov r0 a)
	(mov a r2)
	(dec a)
	(orl p1 (/# 37))		;P1<4:1>  R2-1, and disable decoder
	(jb0 scan-loop-p1out-0)
	(anl p1 (/# (complement 2_0)))
scan-loop-p1out-0
	(jb1 scan-loop-p1out-1)
	(anl p1 (/# (complement 2_1)))
scan-loop-p1out-1
	(jb2 scan-loop-p1out-2)
	(anl p1 (/# (complement 2_2)))
scan-loop-p1out-2
	(jb3 scan-loop-p1out-3)
	(anl p1 (/# (complement 2_3)))
scan-loop-p1out-3
	(anl p1 (/# (complement 1)))	;Strobe the decoder
	(in a p2)			;Get row of keys
	(xrl a @r0)			;A  changed bits
	(jnz scan-found)		;Jump if key state changed
	(djnz r2 scan-loop)
	;
	(jmp scan-keyboard)

;R0 address of bit map entry
;R2 bit number
;R3 changed bits
;R4 bit mask
scan-found
	(mov r3 a)
	(mov r4 (/# 1))
	(mov a r2)			;Bit number is byte number times 8
	(dec a)
	(rl a)
	(rl a)
	(rl a)
	(mov r2 a)
scan-bits-loop
	(mov a r4)
	(anl a r3)
	(jnz scan-found-key)
	(inc r2)
	(mov a r4)			;Shift left one bit, A  0 when done
	(add a r4)
	(mov r4 a)
	(jnz scan-bits-loop)
	(jmp scan-keyboard)		;wtf? should have found something

scan-found-key
	(mov a r4)			;Bit mask
	(xrl a @r0)			;Change bitmap bit
	(mov @r0 a)			;Put back in bitmap
	(anl a r4)			;0 if key now up, non-0 if key now down
	(mov r3 (/# 0))			;Assume key down
	(jnz scan-found-key-down)
	(inc r3)			;Key up, middle byte is 1
	;; If this is a shifting key, don't send all-keys-up, send this-key-up.
	;; This is so that with paired shifting keys we know which it is.
	(mov a r0)
	(add a (/# (- 200 bitmap)))	;Point to mask of non-shifting keys
	(movp3 a @a)
	(anl a r4)			;A gets bit from table (0 => shifting)
	(jz scan-found-key-down)	;Shifting => don't send all-keys-up
	;; Look through the bit map and see if all non-shifting keys are now
	;; up.  If so, send an all-keys-up instead.
	(mov r0 (/# bitmap))
	(mov r1 (/# 200))		;P3 table at 1600
	(mov r4 (/# 20))
check-for-all-up
	(mov a r1)
	(movp3 a @a)
	(anl a @r0)
	(jnz scan-found-key-down)
	(inc r0)
	(inc r1)
	(djnz r4 check-for-all-up)
	(call compute-all-up-code)
scan-found-key-down
	(mov r4 (/# 371))		;Source ID 1 (new keyboard)
	(call send)			;Transmit character
	(call check-boot)		;See if request to boot machine
	(jmp scan-keyboard)

;Subroutine to transmit like old-type Knight keyboard
;Send a 24-bit character from R2, R3, R4
;T1    = pin 2 on the connector	(used for clock)
;P1<6> = pin 3 on the connector	(used for data)
;Data output is normally high, we make it low to get the attention of the keyboard
; multiplexor.
;Clock input is normally high, its low-going transition is when we send the next bit.
;A 1 bit is a high.
;R0 has a count

;Timing:	(1 cycle = about 4 microseconds)
;Clock-low to data-change: 4 to 6 cycles
;Data-change to await-clock-high: 1 cycle
;Clock-high to await-clock-low: 6 cycles best case, 15 cycles worst case
;Start-bit to await-clock-low: 7 cycles
;If clock is symmetric, minimum period is 22 cycles (88 microseconds).

send	(anl p1 (/# (complement 1_6)))	;Send a 0 to start
	(mov a r2)
	(call send-byte)
	(mov a r3)
	(call send-byte)
	(mov a r4)
	(call send-byte)
send-1	(jnt1 send-1)			;Await clock high (idle)
	(orl p1 (/# 1_6))		;Restore line to high
	(ret)

send-byte
	(mov r0 (/# 10))
send-byte-1
	(jb0 send-a-1)
send-a-0
	(jt1 send-a-0)			;Await clock low
	(anl p1 (/# (complement 1_6)))	;Send a 0
send-next-bit-0
	(rrc a)
send-await-clock-high-0
	(jnt1 send-await-clock-high-0)
	(djnz r0 send-byte-1)
	(ret)

send-a-1
	(jt1 send-a-1)			;Await clock low
	(orl p1 (/# 1_6))		;Send a 1
send-next-bit-1
	(rrc a)
send-await-clock-high-1
	(jnt1 send-await-clock-high-1)
	(djnz r0 send-byte-1)
	(ret)

	(= 400)
;Return in R2 and R3 the low 16 bits of an all-up key-code.
;This works by checking through the bitmap looking for shift keys
;that are down, and OR'ing in the bits.
compute-all-up-code
	(mov r2 (/# 0))
	(mov r3 (/# 200))		;Start with only bit 15 set
	(mov r5 (/# 0))			;R5 address in P3
	(mov r0 (/# bitmap))		;R0 address in bitmap
cauc-0	(mov r4 (/# 1))			;R4 bit mask
cauc-1	(mov a r5)
	(movp3 a @a)			;Get table entry
	(jb7 cauc-9)			;Jump if this key not a shifter
	(mov r6 a)			;Save bit number
	(mov a r4)			;Check bit in bit map
	(anl a @r0)
	(jz cauc-9)			;Key not pressed
	(mov a r6)			;See if bit number 8 or more
	(add a (/# -8))
	(jb7 cauc-4)			;Jump if less than 8
	(mov r6 a)			;Save bitnumber within middle byte
	(call cauc-sh)
	(orl a r3)
	(mov r3 a)
	(jmp cauc-9)

cauc-4	(call cauc-sh)
	(orl a r2)
	(mov r2 a)
;Done with this key, step to next
cauc-9	(inc r5)			;Advance P3 address
	(mov a r4)			;Shift bit mask left 1
	(add a r4)
	(mov r4 a)
	(jnz cauc-1)			;Jump if more bits this word
	(inc r0)			;Advance bitmap address
	(mov a r0)			;See if done
	(add a (/# (- (+ bitmap 20))))
	(jnz cauc-0)			;Jump if more words in bitmap
	(ret)				;Result is in R2,R3

;Produce in A a bit shifted left by amount in R6
cauc-sh	(inc r6)			;Compensate for DJNZ
	(mov a (/# 200))
cauc-sh-1
	(rl a)
	(djnz r6 cauc-sh-1)
	(ret)

;Is request to boot machine if both controls and both metas are held
;down, along with rubout or return.  We have just sent the key-down codes
;for all of those keys.  We now send a boot character, then delay for 3 seconds
;to give the machine time to load microcode and read the character to see whether
;it is a warm or cold boot, before sending any other characters, such as up-codes.
;  meta		45 / 165
;  control	20 / 26
;  rubout	23
;  return	136
; The locking keys are in bytes 1, 3, and 12, conveniently out of the way
;A boot code:
;  15-10	1
;  9-6		0
;  5-0		46 (octal) if cold, 62 (octal) if warm.

check-boot
	(mov r0 (/# 64))		;Check one meta key
	(mov a @r0)
	(xrl a (/# 1_5))
	(jnz not-boot)
	(mov r0 (/# 76))		;Check other meta key
	(mov a @r0)
	(xrl a (/# 1_5))
	(jnz not-boot)
	(mov r0 (/# 62))		;Check byte containing controls and rubout
	(mov a @r0)
	(xrl a (/# (+ 1_0 1_6 1_3)))
	(jz cold-boot)			;Both controls and rubout => cold-boot
	(xrl a (/# 1_3))
	(jnz not-boot)
	(mov r0 (/# 73))		;Check for return
	(mov a @r0)
	(xrl a (/# 1_6))
	(jnz not-boot)
warm-boot
	(mov r2 (/# 62))
	(jmp send-boot)

cold-boot
	(mov r2 (/# 46))
send-boot
	(mov r3 (/# 374))		;1's in bits 15-10
	(mov r4 (/# 371))		;Source ID 1 (new keyboard)
	(call send)			;Transmit character
	(mov r4 (/# 6))			;Delay 3 seconds
boot-delay-1
	(mov r3 (/# 0))			;Delay 1/2 second
boot-delay-2
	(mov r2 (/# 0))			;Delay 2 milliseconds
boot-delay-3
	(djnz r2 boot-delay-3)
	(djnz r3 boot-delay-2)
	(djnz r4 boot-delay-1)
not-boot
	(ret)

;In P3 (1400-1577) we have a table, indexed by key number, of shifting
;keys.  The byte is 200 for ordinary keys, or the bit number in the
;all-keys-up message for locking and shifting keys.
	(= 1400)
	200	;0
	200
	200
	9.	;3 mode lock
	200
	6.	;5 super
	200
	200

	200	;10
	200
	200
	200
	200
	8.	;15 alt lock
	200
	200

	4.	;20 control
	200
	200
	200
	0.	;24 shift
	0.	;25 shift
	4.	;26 control
	200

	200	;30
	200
	200
	200
	200
	1.	;35 greek
	200
	200

	200	;40
	200
	200
	200
	1.	;44 greek
	5.	;45 meta
	200
	200

	200	;50
	200
	200
	200
	200
	200
	200
	200

	200	;60
	200
	200
	200
	200
	6.	;65 super
	200
	200

	200	;70
	200
	200
	200
	200
	200
	200
	200

	200	;100
	200
	200
	200
	2.	;104 top
	200
	200
	200

	200	;110
	200
	200
	200
	200
	10.	;115 repeat
	200
	200

	200	;120
	200
	200
	200
	200
	3.	;125 caps lock
	200
	200

	200	;130
	200
	200
	200
	200
	200
	200
	200

	200	;140
	200
	200
	200
	200
	7.	;145 hyper
	200
	200

	200	;150
	200
	200
	200
	200
	2.	;155 top
	200
	200

	200	;160
	200
	200
	200
	200
	5.	;165 meta
	200
	200

	200	;170
	200
	200
	200
	200
	7.	;175 hyper
	200
	200

	(= 1600)
;Locations 1600-1617 contain a mask which has 1's for bit-map positions
;which contain non-shifting keys.
	327	;3 and 5
	337	;15
	216	;20, 24, 25, 26
	337	;35
	317	;44, 45
	377
	337	;65
	377	;70
	357	;104
	337	;115
	337	;125
	377
	337	;145
	337	;155
	337	;165
	337	;175

	)
'code)

;Look at kbd and print out any characters that come in
(defun test ()
  (let ((tv-more-processing-global-enable nil))
     (do ((ch)) (nil)
       (process-allow-schedule)
       (and (setq ch (si:kbd-tyi-raw-no-hang))
	    (print (ldb 0027 ch))))))

;This version isn't stoppable, because it just gets it right out of the hardware
;Prevents you from getting screwed by call-processing and the like.
(defun supertest ()
  (let ((tv-more-processing-global-enable nil))
     (do ((ch)) (nil)
       (or si:kbd-buffer (si:kbd-get-hardware-char-if-any))
       (and si:kbd-buffer
	    (progn (setq ch si:kbd-buffer si:kbd-buffer nil)
		   (print (ldb 0027 ch)))))))
     