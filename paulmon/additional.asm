; Device specific SFRs
.equ	ie2, 0xe8		; 2nd interrupt control register
.equ	pwm1, 0xfd		; PWM1 control
.equ	t3, 0xff		; Timer3 control

.equ    locat, 0x2000		; Location for this code

;
;---------------------------------------------------------;
;                                                         ;
;                    Configure system                     ;
;                                                         ;
;---------------------------------------------------------;

.org    locat
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     254,'1',0,0		; id (254=cmd)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "Configure System",0
.org    locat+64		; executable code begins here

; General system config
	mov	ie, #0		; Disable interrupts (0xa8)
	mov	ie2, #0		; Disable interrupts (0xe8)
	mov	pwm1, #0	; PWM1 is connected to the piezo speaker
	orl	pcon, #10h	; Set condition to load watchdog timer
	mov	t3, #0		; Load watchdog timer

; RAM config
	clr	a
	setb	p1.4
	mov	dph, #08h
	movx	@dptr, a	; Clear latch
	mov	dph, #0ch
	movx	@dptr, a	; Clear latch
	clr	p1.4
	setb	p1.0		; PSEN selects RAM
	setb	p1.2		; MOVX selects RAM
	clr	p1.1		; PSEN selects SRAM card (disabled)
	clr	p1.3		; MOVX selects SRAM card (disabled)

; i2c config
	setb	p1.6		; SCL
	setb	p1.7		; SDA

	ret


;
;---------------------------------------------------------;
;                                                         ;
;              Switch to executing from RAM               ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x100
.db     0xA5,0xE5,0xE0,0xA5     ;signiture bytes
.db     254,'!',0,0             ;id (254=cmd)
.db     0,0,0,0                 ;prompt code vector
.db     0,0,0,0                 ;reserved
.db     0,0,0,0                 ;reserved
.db     0,0,0,0                 ;reserved
.db     0,0,0,0                 ;user defined
.db     255,255,255,255         ;length and checksum (255=unused)
.db     "Run RAM #0000",0
.org    locat+0x140             ;executable code begins here

	clr	a
	setb	p1.4
	mov	dph, #08h
	movx	@dptr, a	; Clear latch
	mov	dph, #0ch
	movx	@dptr, a	; Clear latch
	clr	p1.4
	setb	p1.0		; PSEN selects RAM
	setb	p1.2		; MOVX selects RAM
	clr	p1.1		; PSEN selects SRAM card (disabled)
	clr	p1.3		; MOVX selects SRAM card (disabled)
	ljmp	0x8000		; Need A15 enabled to switch PSEN to RAM (doesn't affect RAM access location)
