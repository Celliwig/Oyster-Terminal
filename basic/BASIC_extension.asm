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
	.db	0x5A						; Change to 0x5A if implemented

;*****************************************************************************
; Command/Statement extensions check2 (code sets bit 0x25.5)
;*****************************************************************************
.org	mcs_basic_locat+0x2048
	setb	extern_prog
	ret

;*****************************************************************************
; Command/Statement extension (user vector table)
;*****************************************************************************
.org	mcs_basic_locat+0x2070
	mov	dptr, #cmd_vector_table
	ret

;*****************************************************************************
; Command/Statement extension (user lookup table)
;*****************************************************************************
.org	mcs_basic_locat+0x2078
	mov	dptr, #cmd_token_table
	ret

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
	jb	lcd_no_scroll, lcd_terminal_finish				; Ignore if no scroll set
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
;							Command Extensions
;###############################################################################################################################

; Command extension vector table
; Provides a list of addresses that correspond to the new commands
cmd_vector_table:
	.dw	basic_lcd_control						; Token: 0x10
	.dw	basic_lcd_set_position						; Token: 0x11
	.dw	basic_lcd_text_attrib						; Token: 0x12
	.dw	basic_lcd_clear							; Token: 0x13
	.dw	basic_lcd_plot							; Token: 0x14

; Command extension token table
; Provides a list of new commands, with their corresponding tokens
cmd_token_table:
	.db	0x10								; Token 1
	.db	"LCDC"								; Command name
	.db	0x00								; End of token indicator

	.db	0x11								; Token 2
	.db	"LCDP"								; Command name
	.db	0x00								; End of token indicator

	.db	0x12								; Token 3
	.db	"LCDT"								; Command name
	.db	0x00								; End of token indicator

	.db	0x13								; Token 4
	.db	"CLS"								; Command name
	.db	0x00								; End of token indicator

	.db	0x14								; Token 5
	.db	"PLOT"								; Command name
	.db	0x00

; Bugfix, needed to avoid problems with variable names
	.db	0xdf								; Dummy token
	.db	0x7f								; Unused dummy char
;----------------------------------------------------------------------------
	.db	0xff								; End of tokenlist indicator


;*****************************************************************************
; LCD commands
;*****************************************************************************

; # Wrapper for OysterLib LCD control commands
; # LCDC <arg> - 0 = off, 1 = on, >1 = init
; ##########################################################################
basic_lcd_control:
	mov	a, #0x39							; Evaluate the first expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	mov	a, r1								; Get LSB of argument (ignore MSB)
basic_lcd_control_off:
	cjne	a, #0x00, basic_lcd_control_on
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	lcall	lcd_off
	anl	psw, #0b11100111						; Restore register bank
	ret
basic_lcd_control_on:
	cjne	a, #0x01, basic_lcd_control_init
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	lcall	lcd_on
	anl	psw, #0b11100111						; Restore register bank
	ret
basic_lcd_control_init:
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	lcall	lcd_init
	anl	psw, #0b11100111						; Restore register bank
	ret

; # Wrapper for OysterLib LCD set position
; # LCDP <column>, <row>
; ##########################################################################
basic_lcd_set_position:
	mov	lcd_start_position, #0x00					; Initially clear position

	mov	a, #0x39							; Evaluate the first expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

	mov	a, #0x40							; Get the next character after the expression
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	cjne	a, #',', basic_lcd_set_position_error				; There's supposed to be 2 arguments

	mov	a, #0x39							; Evaluate the next expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
basic_lcd_set_position_check_y:
	mov	a, r1								; y < 8, so only need LSB
	cjne	a, #0x08, basic_lcd_set_position_check_y_cmp			; Check that y is < 8
basic_lcd_set_position_check_y_cmp:
	jc	basic_lcd_set_position_check_y_save				; If it is, just save for later
	mov	a, #0x07							; Otherwise set at maximum value
basic_lcd_set_position_check_y_save:
	swap	a								; Move row data into the correct position
	rl	a
	orl	lcd_start_position, a						; Save row data

	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
basic_lcd_set_position_check_x:
	mov	a, r1								; x < 32, so only need LSB
	cjne	a, #0x20, basic_lcd_set_position_check_x_cmp			; Check that x is < 32
basic_lcd_set_position_check_x_cmp:
	jc	basic_lcd_set_position_check_x_okay				; If it is, just continue
	mov	a, #0x1f							; Otherwise set at maximum value
basic_lcd_set_position_check_x_okay:
	orl	lcd_start_position, a						; Save column data

	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	lcall	lcd_set_glyph_position
	anl	psw, #0b11100111						; Restore register bank

	ret
basic_lcd_set_position_error:
	mov	a, #0x07							; Carriage return/line feed
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	mov	dptr, #str_arg2_err						; Get the error message
	mov	r3, dph								; Copy address for print function
	mov	r1, dpl
	setb	prnt_rom_or_ram							; Print from ROM
	mov	a, #0x06							; Print string
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	clr	a								; Return to command mode (as this was an error)
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC


; # Sets various LCD print attributes (multiple attributes can be set/unset at once)
; # LCDT <attrib bit>, <state = 0 - clear, >0 - set>
; # Attrib bit:
; #	0 - Double width
; #	1 - Double height
; #	2 - Invert
; #	3 - No scroll
; ##########################################################################
basic_lcd_text_attrib:
	mov	a, #0x39							; Evaluate the first expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

	mov	a, #0x40							; Get the next character after the expression
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	cjne	a, #',', basic_lcd_text_attrib_error				; There's supposed to be 2 arguments

	mov	a, #0x39							; Evaluate the next expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	mov	a, r1								; Get state
	push	acc

	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	pop	acc
	clr	c								; Use Carry to store the attribute state
	jz	basic_lcd_text_attrib_set
	setb	c
basic_lcd_text_attrib_set:
	mov	a, r1								; Get attribute(s)

basic_lcd_text_attrib_bit_double_width:
	jnb	acc.0, basic_lcd_text_attrib_bit_double_height
	clr	lcd_glyph_doublewidth
	jnc	basic_lcd_text_attrib_bit_double_height
	setb	lcd_glyph_doublewidth

basic_lcd_text_attrib_bit_double_height:
	jnb	acc.1, basic_lcd_text_attrib_bit_invert
	clr	lcd_glyph_doubleheight
	jnc	basic_lcd_text_attrib_bit_invert
	setb	lcd_glyph_doubleheight

basic_lcd_text_attrib_bit_invert:
	jnb	acc.2, basic_lcd_text_attrib_bit_no_scroll
	clr	lcd_glyph_invert
	jnc	basic_lcd_text_attrib_bit_no_scroll
	setb	lcd_glyph_invert

basic_lcd_text_attrib_bit_no_scroll:
	jnb	acc.3, basic_lcd_text_attrib_finish
	clr	lcd_no_scroll
	jnc	basic_lcd_text_attrib_finish
	setb	lcd_no_scroll

basic_lcd_text_attrib_finish:
	ret
basic_lcd_text_attrib_error:
	mov	a, #0x07							; Carriage return/line feed
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	mov	dptr, #str_arg2_err						; Get the error message
	mov	r3, dph								; Copy address for print function
	mov	r1, dpl
	setb	prnt_rom_or_ram							; Print from ROM
	mov	a, #0x06							; Print string
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	clr	a								; Return to command mode (as this was an error)
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC


; # Wrapper for OysterLib LCD clear screen
; ##########################################################################
basic_lcd_clear:
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	lcall	lcd_clear_screen
	anl	psw, #0b11100111						; Restore register bank
	ret


; # Wrapper for OysterLib LCD plot
; # PLOT <x>, <y>
; ##########################################################################
basic_lcd_plot:
	mov	a, #0x39							; Evaluate the first expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

	mov	a, #0x40							; Get the next character after the expression
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	cjne	a, #',', basic_lcd_plot_error					; There's supposed to be 2 arguments

	mov	a, #0x39							; Evaluate the next expression, and put on arg stack
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
basic_lcd_plot_check_y:
	mov	a, r1								; y < 64, so only need LSB
	cjne	a, #0x40, basic_lcd_plot_check_y_cmp				; Check that y is < 64
basic_lcd_plot_check_y_cmp:
	jc	basic_lcd_plot_check_y_save					; If it is, just save for later
	mov	a, #0x3f							; Otherwise set at maximum value
basic_lcd_plot_check_y_save:
	push	acc								; Save for later

	mov	a, #0x01							; Pop from arg stack into r3:r1
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
basic_lcd_plot_check_x:
	mov	a, r1								; x < 192, so only need LSB
	cjne	a, #0xc0, basic_lcd_plot_check_x_cmp				; Check that x is < 192
basic_lcd_plot_check_x_cmp:
	jc	basic_lcd_plot_check_x_okay					; If it is, just continue
	mov	a, #0xbf							; Otherwise set at maximum value
basic_lcd_plot_check_x_okay:
	pop	b								; Restore y

	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	lcall	lcd_plot_point
	anl	psw, #0b11100111						; Restore register bank

	ret
basic_lcd_plot_error:
	mov	a, #0x07							; Carriage return/line feed
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	mov	dptr, #str_arg2_err						; Get the error message
	mov	r3, dph								; Copy address for print function
	mov	r1, dpl
	setb	prnt_rom_or_ram							; Print from ROM
	mov	a, #0x06							; Print string
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC
	clr	a								; Return to command mode (as this was an error)
	lcall	mcs_basic_locat+0x7B						; Assembler interface to BASIC

str_arg2_err:	.db	"BAD SYNTAX: 2nd ARG", 0x22				; '"' is the terminator
