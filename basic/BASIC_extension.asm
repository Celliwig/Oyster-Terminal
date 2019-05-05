;###############################################################################################################################
;							BASIC Extensions
;###############################################################################################################################

;*****************************************************************************
; Enable startup code
;*****************************************************************************
.org	mcs_basic_locat+0x2001
	.db	0xAA

;*****************************************************************************
; Command/Statement extensions check
;*****************************************************************************
.org	mcs_basic_locat+0x2002
	.db	0x00						; Change to 0x5A if implemented

;*****************************************************************************
; Command/Statement extensions check2 (code sets bit 45)
;*****************************************************************************
.org	mcs_basic_locat+0x2048

;*****************************************************************************
; Command/Statement extension (user vector table)
;*****************************************************************************
.org	mcs_basic_locat+0x2070

;*****************************************************************************
; Command/Statement extension (user lookup table)
;*****************************************************************************
.org	mcs_basic_locat+0x2078

;*****************************************************************************
; Startup code
;
; Extended startup, zeroes extra registers. Initialises internal register
; memory if bootstrapped by PaulMON to avoid blanking need variables.
; Out:
;   A - Zero if the internal register memory has been initialised
;*****************************************************************************
.org	mcs_basic_locat+0x2090
system_startup_extended:
	pop	dph						; Save return address for when we blank the stack
	pop	dpl

; Zero unused registers
	clr	a
	mov	sfr_ie2_80c562, a				; Disable all timer2 interrupts
	mov	sfr_ste_80c562, a				; Disable timer2 compare actions
	mov	sfr_ctcon_80c562, a				; Disable timer2 capture actions
	mov	sfr_rte_80c562, a				; Disable timer2 compare actions

; Configure Timer2
	mov	sfr_t2con_80c562, #0x81				; Timer2: running, Clk src: fosc/12, Prescaler: 1/1, 16-bit overflow interrupt: enabled

; Configure PWM
	mov	sfr_pwm1_80c562, #0x00				; Set so it only takes one instruction to toggle the PWM output
	mov	sfr_pwmp_80c562, #0xFF

	lcall	serial_baudsave_check				; Check whether PaulMON was used to bootstrap BASIC
	mov	a, #0xFF					; Set A to indicate registers not initialised
	jnc	system_startup_extended_set_console

; Initialise internal registers (ignore oysterlib/paulmon locations)
; Init stack_top to 0x1F
	clr	a
	mov	r0, #sys_stack_top				; Start at top of stack
system_startup_init_registers_alt1:
	mov	@r0, a						; Clear register
	inc	r0						; Increment register pointer
	cjne	r0, #0x20, system_startup_init_registers_alt1	; Keep looping until 0x20
; Init 0x22 to 0x4C
	mov	r0, #0x22					; Restart at 0x22
system_startup_init_registers_alt2:
	mov	@r0, a						; Clear register
	inc	r0						; Increment register pointer
	cjne	r0, #0x4D, system_startup_init_registers_alt2	; Keep looping until 0x4D

system_startup_extended_set_console:
	clr	ucon_in						; Make sure the user console driver flags are clear
	clr	ucon_out
	jnb	use_oysterlib, system_startup_extended_finish	; Check whether to use hardware instead of serial
	setb	ucon_in						; Force the use of the user console drivers instead of serial
	setb	ucon_out

system_startup_extended_finish:
	push	dpl						; Restore return address
	push	dph
	ret


;###############################################################################################################################
;							Interrupts
;###############################################################################################################################

;*****************************************************************************
; EXTERNAL INTERRUPT 0
;*****************************************************************************
.org	mcs_basic_locat+0x4003

;*****************************************************************************
; TIMER0 INTERRUPT
;*****************************************************************************
.org	mcs_basic_locat+0x400B

;*****************************************************************************
; EXTERNAL INTERRUPT 1
;*****************************************************************************
.org	mcs_basic_locat+0x4013

;*****************************************************************************
; TIMER1 INTERRUPT
;*****************************************************************************
.org	mcs_basic_locat+0x401B

;*****************************************************************************
; SERIAL PORT (UART) INTERRUPT
;*****************************************************************************
.org	mcs_basic_locat+0x4023

;*****************************************************************************
; SERIAL PORT (I2C) INTERRUPT
;*****************************************************************************
.org	mcs_basic_locat+0x402B

;*****************************************************************************
; Timer2 capture 0 interrupt
;*****************************************************************************
.org	mcs_basic_locat+0x4033

;*****************************************************************************
; Timer2 capture 1 interrupt
;*****************************************************************************
.org	mcs_basic_locat+0x403B

;*****************************************************************************
; Timer2 capture 2 interrupt
;*****************************************************************************
.org	mcs_basic_locat+0x4043

;*****************************************************************************
; Timer2 capture 3 interrupt
;*****************************************************************************
.org	mcs_basic_locat+0x404B

;*****************************************************************************
; ADC completion interrupt
;*****************************************************************************
.org	mcs_basic_locat+0x4053

;*****************************************************************************
; Timer2 compare 0
;*****************************************************************************
.org	mcs_basic_locat+0x405B

;*****************************************************************************
; Timer2 compare 1
;*****************************************************************************
.org	mcs_basic_locat+0x4063

;*****************************************************************************
; Timer2 compare 3
;*****************************************************************************
.org	mcs_basic_locat+0x406B

;*****************************************************************************
; TIMER 2 INTERRUPT
;*****************************************************************************
.org	mcs_basic_locat+0x4073


;###############################################################################################################################
;							User console
;###############################################################################################################################

;*****************************************************************************
; User console output (see UO, MCS BASIC manual page 68)
;*****************************************************************************
.org	mcs_basic_locat+0x4080
user_console_out:
	ajmp	lcd_terminal_handler

;*****************************************************************************
; User console input (see UI, MCS BASIC manual page 67)
;*****************************************************************************
.org	mcs_basic_locat+0x4083
	ajmp	keyboard_character_handler

;*****************************************************************************
; User console status
;*****************************************************************************
.org	mcs_basic_locat+0x4086
	ajmp	keyboard_status_handler

;*****************************************************************************
; User reset
;*****************************************************************************
.org	mcs_basic_locat+0x4089

;*****************************************************************************
; USER PRINT@ OR LIST@ VECTOR
;*****************************************************************************
.org	mcs_basic_locat+0x408C


;###############################################################################################################################
;							CALL statement
;###############################################################################################################################
.org	mcs_basic_locat+0x4100
	ljmp	0x0000								; Reset to Paulmon

;###############################################################################################################################
;							Actual routines
;###############################################################################################################################

lcd_terminal_handler:
	push	dph
	push	dpl
	push	acc
	push	b
	mov	stack_carry_bit, c						; Save Carry to the stack
	push	stack_carry
	mov	a, r5								; Get character to print
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)

lcd_terminal_bell_check:
	cjne	a, #0x07, lcd_terminal_lf_check					; Check for 'bell' character
	mov	a, #0x06
	mov	b, #0x02							; 0.1s
	lcall	piezo_beep
	sjmp	lcd_terminal_finish
lcd_terminal_lf_check:
	cjne	a, #0x0a, lcd_terminal_cr_check					; Check for a line feed character
	lcall	lcd_new_line_scroll_and_clear					; If it's a LF, process it
	sjmp	lcd_terminal_finish
lcd_terminal_cr_check:
	cjne	a, #0x0d, lcd_terminal_bs_check					; Check for a carriage return character
	mov	r0, lcd_start_position						; Get the start position
	anl	rb3r0, #0xE0							; Strip column data
	mov	lcd_start_position, r0						; Save new position
	sjmp	lcd_terminal_finish
lcd_terminal_bs_check:
	cjne	a, #0x08, lcd_terminal_do_character				; Check for a backspace character
	mov	r0, lcd_start_position						; Get the start position
	mov	a, lcd_start_position						; Get the start position
	anl	rb3r0, #0x1F							; Get column data
	anl	a, #0xE0							; Get row data
	cjne	r0, #0x00, lcd_terminal_bs_check_dec_column
	cjne	a, #0x00, lcd_terminal_bs_check_dec_row
	mov	lcd_start_position, #0xFF					; Set to the end of the screen
	sjmp	lcd_terminal_finish
lcd_terminal_bs_check_dec_row:
	swap	a								; Adjust bit position
	rr	a
	dec	a								; So we can decrease the row number
	rl	a
	swap	a								; Then move back
	orl	a, #0x1F							; Set to last column
	mov	lcd_start_position, a						; Save new position
	sjmp	lcd_terminal_finish
lcd_terminal_bs_check_dec_column:
	dec	r0								; Decrease the column position
	orl	a, r0								; Recombine with row position
	mov	lcd_start_position, a
	sjmp	lcd_terminal_finish
lcd_terminal_do_character:
	lcall	lcd_print_character
lcd_terminal_finish:

	anl	psw, #0b11100111						; Restore register bank
	pop	stack_carry							; Restore Carry
	mov	c, stack_carry_bit
	pop	b
	pop	acc
	pop	dpl
	pop	dph
	ret

keyboard_character_handler:
	push	dph
	push	dpl
	push	b
	mov	stack_carry_bit, c						; Save Carry to the stack
	push	stack_carry
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)

	lcall	keyboard_wait_for_keypress
	mov	a, keycode_ascii
	clr	keyboard_new_char

	anl	psw, #0b11100111						; Restore register bank
	pop	stack_carry
	mov	c, stack_carry_bit						; Restore Carry
	pop	b
	pop	dpl
	pop	dph
	ret

keyboard_status_handler:
	push	dph
	push	dpl
	push	acc
	push	b
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)

	lcall	keyboard_scan
	mov	c, keyboard_new_char

	anl	psw, #0b11100111						; Restore register bank
	pop	b
	pop	acc
	pop	dpl
	pop	dph
	ret


;###############################################################################################################################
;							Debug routines
;###############################################################################################################################

debug_print_A:
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'D'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'B'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'G'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, a
	sjmp	debug_print_crlf

debug_print_PSW:
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'D'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'B'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'G'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, psw
	sjmp	debug_print_crlf

debug_print_CMP:
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'D'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'B'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'G'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, a
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, rb0r2
	sjmp	debug_print_crlf

debug_print_1:
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'D'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'B'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'G'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'1'
	sjmp	debug_print_crlf

debug_print_crlf:
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #CR
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #LF
	ret

debug_print_ibuf:
	push	dph
	push	dpl
	push	acc

	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'D'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'B'
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, #'G'
	mov	dptr, #IBUF
	movx	a, @dptr
	jnb	ti, *								; Loop until serial buffer free
	clr	ti
	mov	sbuf, a

	pop	acc
	pop	dpl
	pop	dph
	sjmp	debug_print_crlf

; Dumps the internal registers raw, NOT converted to ASCII
;debug_dump_internal_registers:
;	jnb	ti, *								; Loop until serial buffer free
;	clr	ti
;	mov	sbuf, #'#'							; Move contents of register to serial buffer
;	ret
