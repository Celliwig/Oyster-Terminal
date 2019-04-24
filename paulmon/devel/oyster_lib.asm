; ###############################################################################################################
; # Contains:
; #
; # Libraries:
; #	Console (replaces paulmon functions)
; #	Math
; #	Memory
; #	I2C
; #	RTC
; #	Keyboard
; #	LCD
; #	Misc
; # Command:
; #	System setup
; #	System init
; ###############################################################################################################

; define the storage location
; ##########################################################################
;.equ	paulmon2, 0x0000	;location where paulmon2
.equ	paulmon2, 0x8000	;location where paulmon2
.equ	locat, 0x1000+paulmon2	;location for the library/commands

; These equ lines allow us to call functions within PAULMON2
; by their names.
; ##########################################################################
.equ	cout, 0x78+paulmon2							; Print Acc to Serial Port
.equ    Cin, 0x7a+paulmon2							; Get Acc from serial port
.equ    pHex, 0x7c+paulmon2							; Print Hex value of Acc
.equ    pHex16, 0x7e+paulmon2							; Print Hex value of DPTR
.equ    pStr, 0x80+paulmon2							; Print string pointed to by DPTR,
										;  must be terminated by 0 or a high bit set
										;  pressing ESC will stop the printing
.equ    gHex, 0x82+paulmon2							; Get Hex input into Acc
										;  carry set if ESC has been pressed
.equ    gHex16, 0x84+paulmon2							; Get Hex input into DPTR
										;  carry set if ESC has been pressed
.equ    ESC, 0x86+paulmon2							; Check for ESC key
										;  carry set if ESC has been pressed
.equ    Upper, 0x88+paulmon2							; Convert Acc to uppercase
										;  non-ASCII values are unchanged
.equ    Init, 0x8a+paulmon2							; Initialize serial port
.equ    newline, 0x90+paulmon2							; Print CR/LF (13 and 10)
.equ    lenstr, 0x92+paulmon2							; Return the length of a string @DPTR (in R0)
.equ    pint8u, 0x95+paulmon2							; Print Acc at an integer, 0 to 255
.equ    pint8, 0x98+paulmon2							; Print Acc at an integer, -128 to 127
.equ    pint16u, 0x9b+paulmon2							; Print DPTR as an integer, 0 to 65535

.equ	stack, 0x70								; location of the stack
.equ	baud_save, 0x68								; save baud for warm boot, 4 bytes


; SFR definitions
; ##########################################################################
.equ	sfr_cml0_80c562, 0xA9							; Timer2 compare L register 0
.equ	sfr_cml1_80c562, 0xAA							; Timer2 compare L register 1
.equ	sfr_cml2_80c562, 0xAB							; Timer2 compare L register 2
.equ	sfr_p4_80c562, 0xC0							; Port 4
.equ	sfr_p5_80c562, 0xC4							; Port 5
.equ	sfr_cmh0_80c562, 0xC9							; Timer2 compare H register 0
.equ	sfr_cmh1_80c562, 0xCA							; Timer2 compare H register 1
.equ	sfr_cmh2_80c562, 0xCB							; Timer2 compare H register 2
.equ	sfr_ie2_80c562, 0xE8							; Additional interrupt enable register
.equ	sfr_t2con_80c562, 0xEA							; Timer2 control SFR
.equ	sfr_tl2_80c562, 0xEC							; Timer2 low byte SFR
.equ	sfr_th2_80c562, 0xED							; Timer2 high byte SFR
.equ	sfr_ip2_80c562, 0xF8							; Additional interrupt priority register
.equ	sfr_pwm1_80c562, 0xFD							; PWM1 control
.equ	sfr_pwmp_80c562, 0xFE							; PWM prescaler
.equ	sfr_t3_80c562, 0xFF							; Timer3 control

.flag	i2c_scl, p1.6
.flag	i2c_sda, p1.7


; RAM definitions
; ##########################################################################
.equ	rb0r1, 0x01
.equ	rb0r2, 0x02

; Bit addressable addresses (0x20-0x21 not used by MCS BASIC)
.equ	stack_carry, 0x20							; Save Carry state here to load on the stack
.equ	mem_mode, 0x20								; Bit addressable, memory mode store

; Addresses (0x50-0x67 not used by MCS BASIC/PaulMON, stack = 0x70)
.equ	lcd_font_table_msb, 0x50						; MSB of the address of the font table
.equ	lcd_scroll_offset, 0x51							; LCD start draw line
.equ	lcd_start_position, 0x52						; Start position for LCD operations (b0-b4 - column, b5-7 - row)
.equ	lcd_subscreen_row, 0x53							; Current subscreen row
.equ	lcd_subscreen_column, 0x54						; Current subscreen column
.equ	lcd_properties, 0x55							; Current contrast/backlight value
.equ	keycode_raw, 0x56							; Raw keycode, read from hardware
.equ	keycode_ascii, 0x57							; Processed raw keycode to ASCII keymap
.equ	mem_page_psen, 0x58							; Store for the currently selected PSEN page
.equ	mem_page_rdwr, 0x59							; Store for the currently selected RDWR page
.equ	keymap_offset, 0x5A							; Used to select the correct keymap
.equ	tmp_var, 0x60
.equ	current_year, 0x67							; The current year as an offset from 2000 (needed as the RTC only stores 2 bits for the year)
; 0x68-0x6B used by baud_save
.equ	lcd_props_save, 0x6C							; Used to retain the backlight/contrast settings when the screen is off
.equ	sys_props_save, 0x6D							; System config
										;	0 - key_click
										; 	1 - Main serial port status
.equ	sys_cfg_chksum_inv, 0x6E						; Checksum (inverted) of the system configuration
.equ	sys_cfg_checksum, 0x6F							; Checksum of the system configuration

; Definitions for the system config save/restore rountines
; So there is only one place to update
.equ	sys_config_start, current_year
.equ	sys_config_end, sys_props_save

; Bit RAM definitions
; ##########################################################################
.flag	use_oysterlib, 0x20.0
.flag	key_click, 0x20.1
.flag	mem_mode_psen_ram, 0x20.2						; This bit is not randomly chosen, see memory command
.flag	mem_mode_psen_ram_card, 0x20.3						; This bit is not randomly chosen, see memory command
.flag	tmp_bit, 0x20.4
.flag	stack_carry_bit, 0x20.5
.flag	mem_mode_rdwr_ram, 0x20.6						; This bit is not randomly chosen, see memory command
.flag	mem_mode_rdwr_ram_card, 0x20.7						; This bit is not randomly chosen, see memory command

.flag	lcd_glyph_doublewidth, 0x21.0
.flag	lcd_glyph_doubleheight, 0x21.1
.flag	lcd_glyph_doubleheight_otherhalf, 0x21.2
.flag	lcd_glyph_invert, 0x21.3
.flag	lcd_no_scroll, 0x21.4
.flag	lcd_glyph_use_ram, 0x21.5
.flag	keyboard_key_down, 0x21.6
.flag	keyboard_new_char, 0x21.7


;  Baud rate reload definitions
; ##########################################################################
.equ	BAUD_19200, 0xFE
.equ	BAUD_9600, 0xFC
.equ	BAUD_4800, 0xF8
.equ	BAUD_2400, 0xF0
.equ	BAUD_1200, 0xE0
.equ	BAUD_600, 0xC0
.equ	BAUD_300, 0x80
.equ	BAUD_150, 0x00


.org	locat
; ###############################################################################################################
; #                                                  Library jump table
; ###############################################################################################################
; Not needed for commands in this file, but to allow other commands to access functions

; console functions
console_lib:
	ajmp	oyster_cout							; 0x00
	ajmp	oyster_newline							; 0x02
	ajmp	oyster_cin							; 0x04
; math library functions
math_lib:
	ajmp	packed_bcd_to_hex						; 0x00
	ajmp	hex_to_packed_bcd						; 0x02
; memory library function
memory_lib:
	ajmp	memory_set_page_psen						; 0x00
	ajmp	memory_set_page_rdwr						; 0x02
	ajmp	memory_set_mode_psen						; 0x04
	ajmp	memory_set_mode_rdwr						; 0x06
	ajmp	memory_ramcard_present						; 0x08
	ajmp	memory_ramcard_write_protect					; 0x0a
; i2c library functions
i2c_lib:
	ajmp	i2c_start							; 0x00
	ajmp	i2c_stop_clock_stretch_check					; 0x02
	ajmp	i2c_stop							; 0x04
	ajmp	i2c_write_byte							; 0x06
	ajmp	i2c_ack_check							; 0x08
	ajmp	i2c_write_byte_to_addr						; 0x0a
	ajmp	i2c_write_byte_with_ack_check					; 0x0c
	ajmp	i2c_read_byte							; 0x0e
	ajmp	i2c_master_read_ack						; 0x10
	ajmp	i2c_read_byte_from_addr						; 0x12
	ajmp	i2c_read_device_register					; 0x14
; rtc library functions
rtc_lib:
	ajmp	rtc_get_config							; 0x00
	ajmp	rtc_get_alarm_config						; 0x02
	ajmp	rtc_set_config							; 0x04
	ajmp	rtc_set_alarm_config						; 0x06
	ajmp	rtc_init							; 0x08
	ajmp	rtc_get_datetime						; 0x0a
	ajmp	rtc_set_datetime						; 0x0c
; keyboard library functions
keyboard_lib:
	ajmp	keyboard_scan							; 0x00
	ajmp	keyboard_reset							; 0x02
	ajmp	keyboard_wait_for_keypress					; 0x04
; lcd library functions
lcd_lib:
	ajmp	lcd_off								; 0x00
	ajmp	lcd_init							; 0x02
	ajmp	lcd_clear_screen						; 0x04
	ajmp	lcd_clear_screen_from_position					; 0x06
	ajmp	lcd_new_line_scroll_and_clear					; 0x08
	ajmp	lcd_print_character						; 0x0a
	ajmp	lcd_set_glyph_position						; 0x0c
	ajmp	lcd_pstr							; 0x0e
	ajmp	lcd_set_backlight_on						; 0x10
	ajmp	lcd_set_backlight_off						; 0x12
	ajmp	lcd_set_contrast_inc						; 0x14
	ajmp	lcd_set_contrast_dec						; 0x16
; serial library functions
serial_lib:
	ajmp	serial_mainport_enable						; 0x00
	ajmp	serial_mainport_disable						; 0x02
	ajmp	serial_rts_set							; 0x04
	ajmp	serial_rts_unset						; 0x06
	ajmp	serial_cts_check						; 0x08
	ajmp	serial_baudsave_check						; 0x0a
	ajmp	serial_baudsave_set_reload					; 0x0c
	ajmp	serial_baudsave_set						; 0x0e
; power library functions
power_lib:
	ajmp	power_battery_main_check_charging				; 0x00
	ajmp	power_battery_backup_check_status				; 0x02
	ajmp	power_battery_ramcard_check_status_warn				; 0x04
	ajmp	power_battery_ramcard_check_status_fail				; 0x06
; misc library functions
misc_lib:
	ajmp	piezo_beep							; 0x00
	ajmp	piezo_pwm_sound							; 0x02
	ajmp	sdelay								; 0x04


; ###############################################################################################################
; #                               Console library (substitue functions for paulmon functions)
; ###############################################################################################################

; # oyster_cout
; #
; # Equivalent function to the paulmon cout
; ##########################################################################
oyster_cout:
	mov	tmp_bit, c							; Save Carry
	cjne	a, #0x0a, oyster_cout_cr_check					; Check for a line feed character
	mov	c, tmp_bit
	sjmp	oyster_newline							; If it's a LF, process it
oyster_cout_cr_check:
	cjne	a, #0x0d,oyster_cout_do_character				; Check for a carriage return character
	mov	c, tmp_bit
	sjmp	oyster_cout_finish						; If it's a CR, just drop it
oyster_cout_do_character:
	mov	c, tmp_bit
	jnb	use_oysterlib, oyster_cout_serial
	push	acc
	mov	stack_carry_bit, c						; Save Carry to the stack
	push	stack_carry
	push	psw
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	acall	lcd_print_character
	pop	psw
	pop	stack_carry							; Restore Carry
	mov	c, stack_carry_bit
	pop	acc
oyster_cout_finish:
	ret
oyster_cout_serial:								; Copied from PaulMON
	jnb	ti, oyster_cout_serial
	clr	ti								; clr ti before the mov to sbuf!
	mov	sbuf, a
	ret


; # oyster_newline
; #
; # Equivalent function to the paulmon newline
; ##########################################################################
oyster_newline:
	push	acc
	jnb	use_oysterlib, oyster_newline_serial
	mov	stack_carry_bit, c						; Save Carry to the stack
	push	stack_carry
	push	psw
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	acall	lcd_new_line_scroll_and_clear
	pop	psw
	mov	lcd_start_position, #0xE0					; Reset start position to first character last row
	pop	stack_carry							; Restore Carry
	mov	c, stack_carry_bit
	pop	acc
	ret
oyster_newline_serial:								; Copied from PaulMON
	mov	a, #13
	acall	oyster_cout_serial
	mov	a, #10
	acall	oyster_cout_serial
	pop	acc
	ret


; # oyster_cin
; #
; # Equivalent function to the paulmon cin
; ##########################################################################
oyster_cin:
	jnb	use_oysterlib, oyster_cin_serial
	mov	stack_carry_bit, c						; Save Carry to the stack
	push	stack_carry
	push	psw
	orl	psw, #0b00011000						; Swap to register bank3 (0x18-0x1F)
	acall	keyboard_wait_for_keypress
	pop	psw
	mov	a, keycode_ascii						; Get ascii code
	clr	keyboard_new_char						; Reset flag
	pop	stack_carry							; Restore Carry
	mov	c, stack_carry_bit
	ret
oyster_cin_serial:								; Copied from PaulMON
	jnb	ri, oyster_cin_serial
	clr	ri
	mov	a, sbuf
	ret


; ###############################################################################################################
; #                                                     Math library
; ###############################################################################################################

; # packed_bcd_to_hex
; #
; # A - number to convert from packed BCD to hex
; ##########################################################################
packed_bcd_to_hex:
	push	acc
	swap	a
	anl	a, #0fh
	mov	b, #0ah
	mul	ab
	mov	b, a
	pop	acc
	anl	a, #0fh
	add	a, b
	ret


; # hex_to_packed_bcd
; #
; # A - number to convert from hex to packed BCD
; ##########################################################################
hex_to_packed_bcd:
	mov	b, #0ah
	div	ab
	swap	a
	orl	a, b
	ret


; ###############################################################################################################
; #                                                    Memory library
; #
; # Memory layout:
; #	0x0000 - 0x7FFF ROM
; #	0x0000 - 0x7FFF RAM
; #	0x0000 - 0x7FFF RAM card
; #
; # However both RAM (system/card) and ROM are paged, and swapped out depending on:
; #	U22 latch - @0x0Cxx + P1.4 - MOVX access
; #	U25 latch - @0x08xx + P1.4 - PSEN/MOVC access
; #		0 - RAM/ROM A15
; #		1 - RAM/ROM A16
; #		2 - RAM card A17/ROM Alt A16
; #		3 - RAM card A18
; #		4 - RAM card A19
; #		5 - RAM card A20
; #		6 - RAM card A21
; #		7 - RAM card ??? (A22 ???)
; #	P1.0 - PSEN selects: 1 = RAM / 0 = ROM
; #	P1.1 - PSEN selects: 1 = RAM card / 0 = System RAM
; #	P1.2 - RD|WR selects: 1 = RAM / 0 = ROM
; #	P1.3 - RD|WR selects: 1 = RAM card / 0 = System RAM
; #
; # A15 - Enables the logic above
; #
; ###############################################################################################################

; define latch addresses
.equ	psen_page_latch, 0x08
.equ	rdwr_page_latch, 0x0c

; define memory access types
.equ	mem_sys_rom, 0x00
.equ	mem_sys_ram, 0x01
.equ	mem_ram_card, 0x03


; # memory_register_check
; #
; # Checks and clears the internal registers
; # Out:
; #   Carry - error
; ##########################################################################
memory_register_check:
	mov	r0, #0xff							; We have the upper registers
memory_register_check_loop:
	mov	a, #0xff							; Write 0xff to registers
	mov	@r0, a
	mov	a, @r0
	cjne	a, #0xff, memory_register_check_error
	cpl	a								; Write 0x00 to registers
	mov	@r0, a
	mov	a, @r0
	cjne	a, #0x00, memory_register_check_error
	dec	r0
	cjne	r0, #7, memory_register_check_loop				; Don't clear out the lower registers
	clr	c
	ret
memory_register_check_error:
	setb	c
	ret


; # memory_sram_check
; #
; # Check and clear a page of selected memory (page: 0x8000 - 0xffff)
; # Out
; #   Carry - error
; ##########################################################################
memory_sram_check:
	mov	dptr, #0x8000
memory_sram_check_loop:
	mov	a, #0xff							; Write 0xff to RAM
	movx	@dptr, a
	nop									; Wait for data to settle
	movx	a, @dptr
	cjne	a, #0xff, memory_sram_check_error
	cpl	a								; Write 0x00 to RAM
	movx	@dptr, a
	nop									; Wait for data to settle
	movx	a, @dptr
	cjne	a, #0x00, memory_sram_check_error
	inc	dptr
	mov	a, dph
	cjne	a, #0x00, memory_sram_check_loop				; Keep looping until we reach the end of page
	clr	c
	ret
memory_sram_check_error:
	setb	c
	ret


; # memory_set_page_psen
; #
; # Set the memory page for program operations
; # In:
; #   A - memory page
; ##########################################################################
memory_set_page_psen:
	mov	mem_page_psen, a
	mov	dph, #psen_page_latch
	sjmp	memory_set_page
; # memory_set_page_rdwr
; #
; # Set the memory page for data operations
; # In:
; #   A - memory page
; ##########################################################################
memory_set_page_rdwr:
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
memory_set_page:
	setb	p1.4								; Enable address logic
	movx	@dptr, a
	clr	p1.4
	ret


; # memory_set_mode_psen
; #
; # Selects the memory to access for program operations
; # In:
; #   A - access mode
; ##########################################################################
memory_set_mode_psen:
	jnb	acc.1, memory_set_mode_psen_config				; If we want to select the ram card, check it's there
	acall	memory_ramcard_present
	jc	memory_set_mode_psen_config					; If it's present, continue
	ret									; Otherwise return
memory_set_mode_psen_config:
	anl	a, #3								; Make sure this is valid
	mov	c, acc.0
	mov	p1.0, c
	mov	mem_mode_psen_ram, c
	mov	c, acc.1
	mov	p1.1, c
	mov	mem_mode_psen_ram_card, c
	ret

; # memory_set_mode_rdwr
; #
; # Selects the memory to access for data operations
; # In:
; #   A - access mode
; ##########################################################################
memory_set_mode_rdwr:
	jnb	acc.1, memory_set_mode_rdwr_config				; If we want to select the ram card, check it's there
	acall	memory_ramcard_present
	jc	memory_set_mode_rdwr_config					; If it's present, continue
	ret									; Otherwise return
memory_set_mode_rdwr_config:
	anl	a, #3								; Make sure this is valid
	mov	c, acc.0
	mov	p1.2, c
	mov	mem_mode_rdwr_ram, c
	mov	c, acc.1
	mov	p1.3, c
	mov	mem_mode_rdwr_ram_card, c
	ret


; # memory_ramcard_present
; #
; # Check whether a SRAM card is present
; # Out:
; #   Carry - SRAM card present
; ##########################################################################
memory_ramcard_present:
	mov	a, sfr_p5_80c562						; Read Port 5
	clr	c								; No SRAM card
	jnb	acc.3, memory_ramcard_present_finish
	setb	c								; SRAM card present
memory_ramcard_present_finish:
	ret


; # memory_ramcard_write_protect
; #
; # Returns whether the memory card is write protected
; # Out:
; #   Carry - Write protect status
; ##########################################################################
memory_ramcard_write_protect:
	clr	c								; SRAM card not write protected
	jnb	sfr_p4_80c562.1, memory_ramcard_write_protect_finish
	setb	c								; SRAM card write protected
memory_ramcard_write_protect_finish:
	ret


; ###############################################################################################################
; #                                                     I2C library
; ###############################################################################################################


; # i2c_start
; #
; # Set start condition on i2c bus
; ##########################################################################
i2c_start:
	setb	i2c_scl								; Make sure SCL is set
	nop									; NOP - wait for i2c data to 'settle'
	nop									; NOP - wait for i2c data to 'settle'
	clr	i2c_sda								; Clear SDA first
	nop									; NOP - wait for i2c data to 'settle'
	nop									; NOP - wait for i2c data to 'settle'
	clr	i2c_scl								; Clear SCL
	ret


; # i2c_stop_clock_stretch_check
; #
; # Set stop condition on i2c bus after checking for clock
; # stretching.
; ##########################################################################
i2c_stop_clock_stretch_check:
	setb	i2c_scl								; Release SCL
	jnb	i2c_scl, i2c_stop_clock_stretch_check				; Wait for SCL to be released
	nop									; NOP - wait for i2c data to 'settle'
	nop									; NOP - wait for i2c data to 'settle'
; # i2c_stop
; #
; # Set stop condition on i2c bus
; ##########################################################################
i2c_stop:
	mov	r0, #0								; Setup loop count (255)
i2c_stop_loop:
	clr	i2c_scl								; Clear SCL
	clr	i2c_sda								; Clear SDA
	nop									; NOP - wait for i2c data to 'settle'
	nop									; NOP - wait for i2c data to 'settle'
	setb	i2c_scl								; Set SCL
	setb	i2c_sda								; Set SDA
	jb	i2c_sda, i2c_stop_exit						; Has SDA been released?
	djnz	r0, i2c_stop_loop						; It hasn't, so loop
i2c_stop_exit:
	ret


; # i2c_write_byte
; #
; # Writes a byte to the i2c bus
; # A - byte to write out
; ##########################################################################
i2c_write_byte:
	mov	r0, #8								; Move 8 into r0 (number of bits)
	rlc	a								; Rotate A into Carry
	mov	i2c_sda, c							; Set i2c bit data from Carry
i2c_write_byte_scl_wait:
	setb	i2c_scl								; Set SCL to set next bit
	jnb	i2c_scl, i2c_write_byte_scl_wait				; Check for slave clock stetching
	sjmp	i2c_write_byte_rest
i2c_write_byte_rest_loop:
	mov	i2c_sda, c							; Set i2c bit data
	setb	i2c_scl								; Set SCL to set next bit
i2c_write_byte_rest:
	rlc	a								; Get next bit
	nop									; NOP - wait for i2c data to 'settle'
	clr	i2c_scl								; Clear SCL to ready for next bit
	djnz	r0, i2c_write_byte_rest_loop					; Loop through byte
	setb	i2c_sda								; Release i2c data line
	ret


; # i2c_ack_check
; #
; # Check for ACK/NACK, set Carry if NACKed
; ##########################################################################
i2c_ack_check:
	mov	r0, #0								; Setup loop count (255)
i2c_ack_check_loop:
	jnb	i2c_sda, i2c_ack_check_acked					; Check to see if byte ACKed
	djnz	r0, i2c_ack_check_loop						; Keep checking bit
	setb	c								; Indicate NACKed
	setb	i2c_scl
	ret
i2c_ack_check_acked:
	setb	i2c_scl								; Set SCL for ack bit
	nop									; NOP - wait for i2c data to 'settle'
	nop									; NOP - wait for i2c data to 'settle'
	clr	i2c_scl								; Clear SCL to ready for next bit
	clr	c								; Indicate ACK
	ret


; # i2c_write_byte_to_addr
; #
; # Write a byte of data to an address, set Carry on error
; # A - Data to write
; # B - Address to write to
; ##########################################################################
i2c_write_byte_to_addr:
	push	acc
	acall	i2c_start
	mov	a,b
	acall	i2c_write_byte
	acall	i2c_ack_check
	pop	acc
	jc	i2c_write_byte_return
; ##########################################################################
; # i2c_write_byte_with_ack_check
; #
; # A - Data to write
i2c_write_byte_with_ack_check:
	acall	i2c_write_byte
	acall	i2c_ack_check
	clr	c
i2c_write_byte_return:
	ret


; # i2c_read_byte
; #
; # Read a byte of data from i2c bus
; # A - contains byte read
; ##########################################################################
i2c_read_byte:
	mov	r0, #8								; Move 8 into r0 (num bits)
	clr	a								; Clear A, will store data here
i2c_read_byte_loop:
	setb	i2c_scl								; Set SCL to get next bit
	nop									; NOP - wait for i2c data to 'settle'
	mov	c, i2c_sda							; Copy i2c bit data into Carry
	clr	i2c_scl								; Clear SCL to ready for next bit
	rlc	a								; Rotate Carry in to A to build byte
	djnz	r0, i2c_read_byte_loop						; Loop to build byte
	ret


; # i2c_master_read_ack
; #
; # Generates the necessary ACK to a device read
; ##########################################################################
i2c_master_read_ack:
	clr	i2c_sda
	nop
	nop
	setb	i2c_scl
	nop
	nop
	clr	i2c_scl
	nop
	nop
	setb	i2c_sda
	ret


; # i2c_read_byte_from_addr
; #
; # Read a byte of data from an address
; # A - Address to read from
; ##########################################################################
i2c_read_byte_from_addr:
	acall	i2c_start
	acall	i2c_write_byte
	acall	i2c_ack_check
	jc	i2c_read_byte_from_addr_rtn
	acall	i2c_read_byte
	acall	i2c_stop_clock_stretch_check
	clr	c
i2c_read_byte_from_addr_rtn:
	ret


; # i2c_read_device_register
; #
; # Selects a device register, then reads the register back.
; # Expects additional reads, so connection not closed.
; # A - Register to read
; # B - I2C address
; ##########################################################################
i2c_read_device_register:
	acall	i2c_write_byte_to_addr						; Write register address to device
	jc	i2c_read_byte_from_addr_rtn					; Check for errors
	acall	i2c_start							; I2C restart
	mov	a, b								; Get i2c address
	inc	a								; Read operation
	acall	i2c_write_byte							; Write i2c address (as read operation)
	acall	i2c_ack_check							; Check for errors
	jc	i2c_read_byte_from_addr_rtn
	ajmp	i2c_read_byte							; Read register contents
	clr	c


; ###############################################################################################################
; #                                                     RTC library
; ###############################################################################################################

.equ	rtc_addr1, 0xa0								; 0x50 (bit shifted)
.equ	rtc_addr2, 0xa2								; 0x51 (bit shifted)


; # rtc_get_config
; #
; # Get the selected RTC config
; # In:
; #   B - RTC address
; # Out:
; #   A - RTC config
; #   Carry - error
; ##########################################################################
rtc_get_config:
	clr	a								; Config/status register
	acall	i2c_write_byte_to_addr						; Select register
	jnc	rtc_get_config_restart						; Check for errors
	ajmp	i2c_stop
rtc_get_config_restart:
	acall	i2c_start							; I2C restart
	mov	a, b								; Get i2c address
	inc	a								; Read operation
	acall	i2c_write_byte							; Re-open connection to RTC device
	acall	i2c_ack_check							; Check for errors
	jnc	rtc_get_config_read
	ajmp	i2c_stop
rtc_get_config_read:
	acall	i2c_read_byte							; Get config/status
	acall	i2c_master_read_ack						; ACK read
	clr	c
	ajmp	i2c_stop


; # rtc_get_alarm_config
; #
; # Get the selected RTC alarm config
; # In:
; #   B - RTC address
; # Out:
; #   A - RTC alarm config
; #   Carry - error
; ##########################################################################
rtc_get_alarm_config:
	mov	a, #8								; Alarm control register
	acall	i2c_write_byte_to_addr						; Select register
	jnc	rtc_get_config_restart						; Check for errors
	ajmp	i2c_stop


; # rtc_set_config
; #
; # Configure the selected RTC
; # In:
; #   A - RTC config
; #   B - RTC address
; # Out:
; #   Carry - error
; ##########################################################################
rtc_set_config:
	push	acc								; Save config
	clr	a								; Control/status register
	acall	i2c_write_byte_to_addr						; Select register
	jnc	rtc_set_config_write
	pop	acc								; Pop this so the stack is sound
	ajmp	i2c_stop
rtc_set_config_write:
	pop	acc								; Get config
	acall	i2c_write_byte_with_ack_check
	ajmp	i2c_stop


; # rtc_set_alarm_config
; #
; # Configure the selected RTC alarm
; # In:
; #   A - RTC alarm config
; #   B - RTC address
; # Out:
; #   Carry - error
; ##########################################################################
rtc_set_alarm_config:
	push	acc								; Save config
	mov	a, #8								; Alarm control register
	acall	i2c_write_byte_to_addr						; Select register
	jnc	rtc_set_config_write
	pop	acc								; Pop this so the stack is sound
	ajmp	i2c_stop


; # rtc_init
; #
; # Basic config of the 2 RTC devices
; # Out:
; #   Carry - error
; ##########################################################################
rtc_init:
	mov	a, #20h								; 0b00100000 - Event counter mode, alarm registers disable
	mov	b, #rtc_addr2
	acall	rtc_set_config							; Config RTC2
	jc	rtc_init_finish							; Exit on error

	mov	a, #5								; Timer function: days, Alarm/Timer flag: no iterrupt, no timer alarm, no clock alarm
	mov	b, #rtc_addr1
	acall	rtc_set_alarm_config						; Config RTC1 alarm
	jc	rtc_init_finish							; Exit on error
; # rtc_start
; #
; # Used in conjunction with rtc_stop when the date/time needs to be updated
; ##########################################################################
rtc_start:
	mov	a, #4								; 0b00000100 - Clock mode 32.768 kHz, alarm enabled
	mov	b, #rtc_addr1
	acall	rtc_set_config							; Config RTC1
	jc	rtc_init_finish							; Exit on error

	clr	c
rtc_init_finish:
	ret


; # rtc_stop
; #
; # Stops the RTC ticking
; # Out:
; #   Carry - error
; ##########################################################################
rtc_stop:
	mov	a, #0x80							; Stop counting, disable alarm
	mov	b, #rtc_addr1
	acall	rtc_set_config							; Config RTC1
	jc	rtc_stop_finish							; Exit on error

	clr	c
rtc_stop_finish:
	ret


; # rtc_get_alarm_datetime
; #
; # Returns the alarm date/time registers (don't care about hundreths of a second)
; # Out:
; #   r0 - Seconds
; #   r1 - Minutes
; #   r2 - Hours
; #   r3 - Day of the month
; #   r4 - Month
; #   r5 - Year (0-3)
; #   Carry - error
; ##########################################################################
rtc_get_alarm_datetime:
	mov	a, #0x0a							; Alarm seconds register
	sjmp	rtc_get_datetime_main
; # rtc_get_datetime
; #
; # Returns the date/time registers (don't care about hundreths of a second)
; # Out:
; #   r0 - Seconds
; #   r1 - Minutes
; #   r2 - Hours
; #   r3 - Day of the month
; #   r4 - Month
; #   r5 - Year (0-3)
; #   Carry - error
; ##########################################################################
rtc_get_datetime:
	mov	a, #2								; Seconds register
rtc_get_datetime_main:
	mov	b, #rtc_addr1
	acall	i2c_read_device_register					; Get value
	jc	rtc_get_datetime_finish
	acall	i2c_master_read_ack
;	acall	packed_bcd_to_hex						; Convert for system use
	push	acc								; Store seconds for later (r0 used by i2c_*)
	acall	i2c_read_byte							; Get value
	acall	i2c_master_read_ack
;	acall	packed_bcd_to_hex						; Convert for system use
	mov	r1, a								; Store minutes
	acall	i2c_read_byte							; Get value
	acall	i2c_master_read_ack
	anl	a, #3fh								; Make sure valid value (24h clock)
;	acall	packed_bcd_to_hex						; Convert for system use
	mov	r2, a								; Store hours
	acall	i2c_read_byte							; Get value
	acall	i2c_master_read_ack
	push	acc								; Store for later
	anl	a,#3fh								; Lose year data
;	acall	packed_bcd_to_hex						; Convert for system use
	mov	r3, a								; Store day of the month
	pop	acc								; Get original value
	rl	a
	rl	a
	anl	a, #3								; Get year data
	mov	r5, a								; Store year
	acall	i2c_read_byte							; Get value
	anl	a, #1fh								; Lose weekday data
;	acall	packed_bcd_to_hex						; Convert for system use
	mov	r4, a								; Store month
	clr	c
rtc_get_datetime_finish:
	acall	i2c_stop_clock_stretch_check
	pop	acc
	mov	r0, a
	ret


; # rtc_set_alarm_datetime
; #
; # Set the alarm date/time of the RTC.
; # N.B. Need to disable the RTC before loading new datetime.
; # In:
; #   r0 - Seconds
; #   r1 - Minutes
; #   r2 - Hours
; #   r3 - Day of the month
; #   r4 - Month
; #   r5 - Year (0-3)
; # Out:
; #   Carry - error
; ##########################################################################
rtc_set_alarm_datetime:
	mov	a, r0								; Save for later (r0 used by i2c_*)
	push	acc
	mov	a, #0x09							; Alarm hundreths of a second
	sjmp	rtc_set_datetime_main
; # rtc_set_datetime
; #
; # Set the date/time of the RTC.
; # N.B. Need to disable the RTC before loading new datetime.
; # In:
; #   r0 - Seconds
; #   r1 - Minutes
; #   r2 - Hours
; #   r3 - Day of the month
; #   r4 - Month
; #   r5 - Year (0-3)
; # Out:
; #   Carry - error
; ##########################################################################
rtc_set_datetime:
	mov	a, r0								; Save for later (r0 used by i2c_*)
	push	acc
	mov	a, #1								; Hundreths of a second
rtc_set_datetime_main:
	mov	b, #rtc_addr1
	acall	i2c_write_byte_to_addr						; Select register
	jc	rtc_set_datetime_finish
	mov	a, #0								; Just zero hundreths of a second
	acall	i2c_write_byte_with_ack_check					; Load hundreths of a second
	jc	rtc_set_datetime_finish
	pop	acc
;	anl	a, #3fh								; Make the value reasonable (if not correct)
;	acall	hex_to_packed_bcd						; Convert for RTC register
	acall	i2c_write_byte_with_ack_check					; Load seconds
	jc	rtc_set_datetime_finish
	mov	a, r1
;	anl	a, #3fh								; Make the value reasonable (if not correct)
;	acall	hex_to_packed_bcd						; Convert for RTC register
	acall	i2c_write_byte_with_ack_check					; Load minutes
	jc	rtc_set_datetime_finish
	mov	a, r2
;	anl	a, #1fh								; Make the value reasonable (if not correct)
;	acall	hex_to_packed_bcd						; Convert for RTC register
	acall	i2c_write_byte_with_ack_check					; Load hours
	jc	rtc_set_datetime_finish
	mov	a, r3
;	anl	a, #1fh								; Make the value reasonable (if not correct)
;	acall	hex_to_packed_bcd						; Convert for RTC register
;	anl	a, #3fh								; Make sure valid value
	mov	r0, a								; Store for later
	mov	a, r5
	anl	a, #3								; Make sure valid value
	rr	a
	rr	a								; Shift to correct register position
	orl	a, r0								; Combine with day of the month
	acall	i2c_write_byte_with_ack_check					; Load value
	jc	rtc_set_datetime_finish
	mov	a, r4
;	anl	a, #0fh								; Make the value reasonable (if not correct)
;	acall	hex_to_packed_bcd						; Convert for RTC register
	acall	i2c_write_byte_with_ack_check					; Load month
	jc	rtc_set_datetime_finish
	clr	c
rtc_set_datetime_finish:
	ajmp	i2c_stop


; ###############################################################################################################
; #                                                 Keyboard library
; #
; # Keyboard column select - @0x40xx + P1.4
; # Keyboard row readback - i2c 0x20 + P1.5 irq
; #
; ###############################################################################################################

.equ	keycolumn_addr, 0x04							; Memory address (dph)
.equ	keyrow_addr, 0x40							; I2C address shifted (0x20)


; # keyboard_scan
; #
; # Cycles the column selection looking for a depressed key.
; # Reads the row selection if a key is depressed.
; # Out:
; #   keycode_raw - Raw keycode
; ##########################################################################
keyboard_scan:
	mov	dph, #keycolumn_addr
	setb	p1.4								; Enable address select

	mov	r0, #6								; Number of columns to scan
	mov	a, #0xdf							; Initial column config
keyboard_scan_column:
	movx	@dptr, a							; Load column selection
	nop
	nop										; Wait for signals to propagate
	jnb	p1.5, keyboard_read_key						; Has the row select irq been set
	rr	a								; Next column
	djnz	r0, keyboard_scan_column
	clr	keyboard_key_down						; We haven't found any keys depressed
; # keyboard_reset
; #
; # Resets the column selection, clears the row irq.
; ##########################################################################
keyboard_reset:
	mov	dph, #keycolumn_addr
	setb	p1.4								; Enable address select

	mov	a, #0xff							; Reset column selection
	movx	@dptr, a
keyboard_reset_clear_irq:
	acall	i2c_start
	mov	a, #keyrow_addr
	inc	a								; Read operation
	acall	i2c_write_byte
	acall	i2c_ack_check
	acall	i2c_read_byte
	acall	i2c_master_read_ack
	acall	i2c_stop_clock_stretch_check
	inc	a								; 255 we can't check, 0 we can
	jnz	keyboard_scan_exit
	clr	a
	movx	@dptr, a
keyboard_scan_exit:
	clr	p1.4								; Disable address select
	ret

keyboard_read_key:
	cpl	a								; Double checking the signal was valid
	movx	@dptr, a							; Set inverted signal
	nop
	nop
	nop									; Wait for signals to propagate
	jnb	p1.5, keyboard_reset_clear_irq					; This shouldn't be low, so just clear row irq
	cpl	a
	movx	@dptr, a							; Reset the column selection back
	dec	r0								; Adjust value (1-6 => 0-5)
	mov	a, r0
	swap	a
	rr	a
	mov	r4, a								; Temporarily save data for later (r4 - 0bxxCCCxxx)
	acall	i2c_start
	mov	a, #keyrow_addr
	inc	a								; Read operation
	acall	i2c_write_byte
	acall	i2c_ack_check
	acall	i2c_read_byte
	acall	i2c_master_read_ack
	acall	i2c_stop_clock_stretch_check
	inc	a								; Check for 255
	jz	keyboard_reset							; Clear if invalid
	dec	a
	jnb	psw.0, keyboard_reset_clear_irq					; Sanity check, clear irq if there's an even number of bits set
	mov	r0, #8
keyboard_read_key_row_cycle:
	rl	a								; Row selection bits
	jnb	acc.0, keyboard_read_key_make_keycode
	djnz	r0, keyboard_read_key_row_cycle
	sjmp	keyboard_reset
keyboard_read_key_make_keycode:
	dec	r0								; Adjust value (1-8 => 0-7)
	mov	a, r4								; Get column data (0bxxCCCxxx)
	orl	a, r0								; Combine with row data
	jnb	keyboard_key_down, keyboard_read_key_make_keycode_update	; Update if different key
	sjmp	keyboard_reset							; Otherwise ignore this result
keyboard_read_key_make_keycode_update:
	mov	keycode_raw, a							; Save result (0bxxCCCRRR)
	setb	keyboard_key_down						; Set, so we don't loop on the same key press

	jnb	key_click, keyboard_read_key_make_keycode_check_mode		; Don't emit a key 'click' if disabled
	push	acc
	mov	a, #0x0b							; 2 kHz
	mov	b, #0x01							; 50 ms
	lcall	piezo_beep
	pop	acc
keyboard_read_key_make_keycode_check_mode:
	cjne	a, #0x20, keyboard_read_key_make_keycode_check_mode_lock	; Look for the 'mode' keycode
	mov	a, keymap_offset						; Get the currently selected keymap
	jnb	acc.7, keyboard_read_key_make_keycode_check_mode_inc
	mov	keymap_offset, #0x00						; Select default keymap
	sjmp	keyboard_reset
keyboard_read_key_make_keycode_check_mode_inc:
	add	a, #0x30							; Select next keymap
	mov	keymap_offset, a
	cjne	a, #0x61, keyboard_read_key_make_keycode_check_mode_inc_cmp	; Check if we have cycled through all keymaps
keyboard_read_key_make_keycode_check_mode_inc_cmp:
	jc	keyboard_reset
	mov	keymap_offset, #0x00						; Select default keymap
	ajmp	keyboard_reset
keyboard_read_key_make_keycode_check_mode_lock:
	cjne	a, #0x2C, keyboard_read_key_make_keycode_ascii			; Check for 'DEL' key
	xch	a, keymap_offset
	jnz	keyboard_read_key_make_keycode_mode_lock
	xch	a, keymap_offset
	sjmp	keyboard_read_key_make_keycode_ascii
keyboard_read_key_make_keycode_mode_lock:
	cpl	acc.7								; Toggle 'lock' bit
	xch	a, keymap_offset
	ajmp	keyboard_reset
keyboard_read_key_make_keycode_ascii:
	mov	dptr, #keycode_2_character_table1				; Get conversion table
	mov	dpl, keymap_offset						; Select the correct keymap
	anl	dpl, #0x7F							; Remove the 'lock' bit
	movc	a, @a+dptr							; Get the corresponding character for the raw code
	mov	keycode_ascii, a						; Store for processing
	setb	keyboard_new_char						; Set flag indicating new character
	mov	a, keymap_offset						; Check if an alternate keymap is selected
	jb	acc.7, keyboard_read_key_make_keycode_ascii_finish
	mov	keymap_offset, #0x00						; Select the default keymap
keyboard_read_key_make_keycode_ascii_finish:
	ajmp	keyboard_reset


; # keyboard_wait_for_keypress
; #
; # Waits for a key to be pressed, then returns
; ##########################################################################
keyboard_wait_for_keypress:
	acall	keyboard_scan							; Scan for key press
	jnb	keyboard_new_char, keyboard_wait_for_keypress			; Wait until there is one
	jb	keyboard_key_down, keyboard_wait_for_keypress			; Wait for key to be released
	ret


; # keyboard_click_toggle
; #
; # Toggle key_click
; ##########################################################################
keyboard_click_toggle:
	mov	c, key_click
	cpl	c
; # keyboard_click_set
; #
; # Sets whether a 'click' is emmitted when a key is depressed
; # In:
; #   Carry - When set, key clicks emit a tone
; ##########################################################################
keyboard_click_set:
	mov	a, sys_props_save						; Save configuration
	anl	a, #0xfe
	mov	acc.0, c
	mov	sys_props_save, a
	mov	key_click, c							; Set flag
	ret


; ###############################################################################################################
; #                                                    LCD functions
; ###############################################################################################################


; # lcd_send_data
; #
; # Move A to LCD, delays as the LCD enable line is externally clocked
; ##########################################################################
lcd_send_data:
	nop
	nop
	nop
	nop
	movx	@dptr, a
	ret


; # lcd_get_data
; #
; # Move LCD to A, delays as the LCD enable line is externally clocked
; ##########################################################################
lcd_get_data:
	nop
	nop
	nop
	nop
	movx	a, @dptr
	ret


; # lcd_off
; #
; # Cycle through the subscreens turning them off
; ##########################################################################
lcd_off:
	acall	lcd_set_properties_off						; Turn off backlight, et al.

	setb	p1.4								; Enable clock
	mov	dph, #0x80							; Select LCD (command register)
lcd_off_subscreen:
	mov	a, #0x3e							; LCD command 'Turn off'
	lcall	lcd_send_data							; Send command
	mov	a, dph
	add	a, #0x10							; Select next subscreen
	mov	dph, a
	cjne	a, #0xb0, lcd_off_subscreen					; Disable next subscreen
	clr	p1.4								; Disable clock
	ret


; # lcd_init
; #
; # Initialise the LCD, clear the memory
; ##########################################################################
lcd_init:
	acall	lcd_set_properties_defaults

	setb	p1.4								; Enable clock
	mov	dph, #0x80							; Select LCD (command register)
lcd_init_subscreen:
	mov	a, #0x3f							; LCD command 'Turn on'
	lcall	lcd_send_data							; Send command
	mov	a, #0xc0							; LCD command 'Display start line' #0
	lcall	lcd_send_data							; Send command
	mov	a, dph
	add	a, #0x10							; Select next subscreen
	mov	dph, a
	cjne	a, #0xb0, lcd_init_subscreen					; Initialise next subscreen
	mov	lcd_scroll_offset,#0
; ##########################################################################
; # lcd_clear_screen
lcd_clear_screen:
	mov	dptr, #ascii_font_table
	mov	lcd_font_table_msb, dph
	clr	lcd_glyph_use_ram
	clr	lcd_no_scroll
	clr	lcd_glyph_invert
	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	mov	lcd_start_position,#0
; ##########################################################################
; # lcd_clear_screen_from_position
; #
; # lcd_start_position - Position to start from (b0-b4 - column, b5-7 - row)
lcd_clear_screen_from_position:
	push	dph								; Save registers
	push	b

	setb	p1.4								; Enable clock
	mov	a, lcd_start_position						; Get position to clear from (b0-b4 - column, b5-7 - row)
	anl	a, #0x1f							; Get column data
	mov	b, #6								; Glyph block size
	mul	ab								; Calculate column offset
	push	acc								; Save result
	anl	a, #0xc0							; Get subscreen selection
	rr	a
	rr	a								; Shift into correct bit position
	orl	a, #0x80
	mov	dph, a								; LCD subscreen address
	pop	acc
	anl	a, #0x3f							; Subscreen column position
	mov	lcd_subscreen_column, a
	orl	a, #0x40							; LCD command 'Set Address'
	lcall	lcd_send_data							; Send command
	mov	a, lcd_start_position						; Get position to clear from (b0-b4 - column, b5-7 - row)
	anl	a, #0xe0							; Get row data
	swap	a
	rr	a								; Equivalent to right shift x5
	mov	b, a
	mov	a, lcd_scroll_offset
	rl	a
	swap	a
	add	a, b
	anl	a, #7
	mov	lcd_subscreen_row, a
	orl	a, #0b8h							; LCD command 'Set Page'
	lcall	lcd_send_data							; Send command
	orl	dph, #0x40							; Select LCD (data register)
	mov	a, #0x40
	clr	c
	subb	a, lcd_subscreen_column						; Calculate remaining columns
	mov	lcd_subscreen_column, a
	mov	a, #0
lcd_clear_subscreen_remaining_line:
	lcall	lcd_send_data							; Send command
	djnz	lcd_subscreen_column, lcd_clear_subscreen_remaining_line
lcd_clear_subscreen_check_next:
	anl	dph, #0xbf							; Swap to LCD command register
	mov	a, dph
	add	a, #0x10							; Increment subscreen
	mov	dph, a
	cjne	a, #0xb0, lcd_clear_subscreen_next_start			; Check whether there are subscreens remaining for this line
	mov	a, lcd_subscreen_row
	inc	a								; Increment subscreen row selection
	anl	a, #7
	mov	lcd_subscreen_row, a						; Store incremented subscreen row selection
	mov	dph, #0x80							; Select LCD command register
	mov	a, lcd_start_position
	add	a, #0x20							; Increment stored position (sets carry on screen end)
	jc	lcd_clear_exit
	mov	lcd_start_position, a
lcd_clear_subscreen_next_start:
	mov	a, #0x40							; LCD command 'Set Address' #0
	lcall	lcd_send_data							; Send command
	mov	a, lcd_subscreen_row						; Get subscreen row selection
	orl	a, #0xb8							; LCD command 'Set Page'
	lcall	lcd_send_data							; Send command
	orl	dph, #0x40							; Swap to LCD data register
	mov	lcd_subscreen_column, #0x40					; Number of characters to clear (64 - full line)
	clr	a
lcd_clear_subscreen_remaining_line2:
	lcall	lcd_send_data
	djnz	lcd_subscreen_column, lcd_clear_subscreen_remaining_line2
	sjmp	lcd_clear_subscreen_check_next
lcd_clear_exit:
	clr	p1.4								; Disable clock
	pop	b								; Restore registers
	pop	dph
	ret


; # lcd_new_line_scroll_and_clear
; #
; # lcd_scroll_offset - current line that the lcd starts on
; ##########################################################################
lcd_new_line_scroll_and_clear:
	push	dph								; Save registers

	mov	a, lcd_scroll_offset						; Get current lcd display start line
	add	a, #8								; Move the display up 8 lines
	anl	a, #3fh								; Wrap on >=64
	mov	lcd_scroll_offset, a						; Save result
	setb	p1.4								; Enable clock
	mov	dph, #0x80							; Select LCD command register subscreen 0
lcd_new_line_scroll_and_clear_loop:
	mov	a, lcd_scroll_offset
	orl	a, #0xc0							; LCD command 'display start line'
	lcall	lcd_send_data							; Send command
	mov	a, dph								; Get currently selected subscreen
	add	a, #0x10							; Select next subscreen
	mov	dph, a								; Address subscreen
	cjne	a, #0xb0, lcd_new_line_scroll_and_clear_loop			; Loop until we have iterrated over all subscreens
	mov	r0, lcd_start_position
	mov	lcd_start_position, #0xe0					; Last character line, first character
	lcall	lcd_clear_screen_from_position					; Clear last character line of the lcd
	mov	lcd_start_position, r0

	pop	dph
	ret


; # lcd_print_character
; #
; # Prints a character to the screen
; # In:
; #   A - character to print
; #   lcd_start_position - Glyph position (b0-b4 - column, b5-7 - row)
; ##########################################################################
lcd_print_character:
	anl	a,#7fh								; Force 7-bit ASCII
	push	dph								; Save registers
	push	dpl
	clr	lcd_glyph_doubleheight_otherhalf				; Clear glyph height doubler 'other half' flag
	push	acc
	lcall	lcd_print_glyph_data						; Print glyph
	jnb	lcd_glyph_doubleheight, lcd_print_character_setup_double_width
	mov	a, lcd_start_position						; Get glyph position
	add	a, #0x20							; Increment line number
	jnc	lcd_print_character_other_half
	jb	lcd_no_scroll, lcd_print_character_other_half
	lcall	lcd_new_line_scroll_and_clear
	mov	a, lcd_start_position
lcd_print_character_other_half:
	mov	lcd_start_position, a
	pop	acc
	push	acc
	setb	lcd_glyph_doubleheight_otherhalf
	lcall	lcd_print_glyph_data
lcd_print_character_setup_double_width:
	mov	c, lcd_glyph_doublewidth
	mov	tmp_bit, c
lcd_print_character_inc_char_position:
	inc	lcd_start_position						; Increment character position
	mov	a, lcd_start_position
	anl	a, #0x1f							; Extract column data
	jz	lcd_print_character_inc_character_line				; Off the end of the screen?
	jbc	tmp_bit,lcd_print_character_inc_char_position			; Do again for double width
	sjmp	lcd_print_character_exit
lcd_print_character_inc_character_line:
	mov	a, lcd_start_position						; Get character position
	anl	a, #0xe0							; Extract row data
	swap	a
	rr	a								; Equivalent rr x5
	jnz	lcd_print_character_inc_character_line_have_line_no
	mov	a, #8								; Equivalent line number as we are off the bottom of the screen
lcd_print_character_inc_character_line_have_line_no:
	mov	b, a
	jnb	lcd_glyph_doubleheight, lcd_print_character_inc_character_line_do
	mov	a, lcd_start_position
	add	a, #0x20
	mov	lcd_start_position, a
	inc	b
	inc	b
lcd_print_character_inc_character_line_do:
	jnb	b.3, lcd_print_character_exit
	jnb	lcd_no_scroll, lcd_print_character_generate_new_line
	mov	lcd_start_position, #0
	sjmp	lcd_print_character_exit
lcd_print_character_generate_new_line:
	lcall	lcd_new_line_scroll_and_clear
	mov	a, lcd_start_position
	add	a, #0xe0
	mov	lcd_start_position, a
	dec	b
	jb	b.3, lcd_print_character_generate_new_line
lcd_print_character_exit:
	pop	acc
	pop	dpl
	pop	dph
	ret


; # lcd_print_glyph_data
; #
; # Prints a character to the screen at the given position
; # In:
; #   A - ASCII code
; #   lcd_start_position - Glyph position (b0-b4 - column, b5-7 - row)
; ##########################################################################
lcd_print_glyph_data:
	push	acc
	lcall	lcd_set_glyph_position
	jnb	lcd_glyph_doubleheight_otherhalf, lcd_print_glyph_data_start
	mov	a, lcd_start_position
	add	a, #0xe0
	mov	lcd_start_position,a
lcd_print_glyph_data_start:
	pop	acc								; Pop ASCII character
	orl	dph, #0x40							; We're going to be writing display data (Port 2, bit 6 -> LCD D/I)
	mov	r2, dph								; Store LCD control line cfg in r2
	mov	b, #6								; Number of bytes per character (to calc offset)
	mul	ab								; Offset = ASCII code * bytes per character (A = LSB, B = MSB)
	mov	dpl, a								; Store result as lower half of dptr
	mov	a, b								; Get the MSB of the offset
	add	a, lcd_font_table_msb						; Add MSB and the contents of register lcd_font_table_msb (0x3d)
	mov	r1, a								; During the loop dph is stored in r1
	mov	b, #6								; Number of bytes per character (bytes to copy)
lcd_print_glyph_data_get_byte:
	mov	dph, r1								; r1 contains dph of glyph data
	clr	a
	movc	a, @a+dptr							; Get glyph data
	jnb	lcd_glyph_use_ram, lcd_print_glyph_data_skip_ram_load
	movx	a, @dptr							; Copy data from external RAM
lcd_print_glyph_data_skip_ram_load:
	inc	dptr								; Increment pointer
	mov	r1, dph								; Store dph for later
	jnb	lcd_glyph_doubleheight, lcd_print_glyph_data_is_inverted		; Double the size of the glyph?
	jnb	lcd_glyph_doubleheight_otherhalf, lcd_print_glyph_data_double_size	; Which half of glyph data are we working on?
	swap	a
lcd_print_glyph_data_double_size:
	anl	a, #0x0f
	push	dpl
	mov	dptr, #glyph_double_size_conversion_table
	movc	a, @a+dptr
	pop	dpl
lcd_print_glyph_data_is_inverted:
	jnb	lcd_glyph_invert, lcd_print_glyph_data_processed		; Is the glyph data inverted?
	cpl	a								; Invert glyph data
lcd_print_glyph_data_processed:
	mov	c, lcd_glyph_doublewidth					; Copy the double width parameter so we can iterate with it
	mov	tmp_bit, c
lcd_print_glyph_data_loop_double_width:
	push	acc
	setb	p1.4								; LCD enable high
	mov	p2, r2								; LCD control lines (CSA / CSB / D|I)
	movx	@r0, a								; LCD data
	clr	p1.4								; LCD enable low
	inc	lcd_subscreen_column						; Increment screen x offset store
	mov	r0, lcd_subscreen_column
	cjne	r0, #0x40, lcd_print_glyph_data_continue			; Check whether the 'cursor' is off the current subscreen (64x64 bits)
	mov	a, r2
	jnb	acc.5, lcd_print_glyph_data_inc_subscreen			; Check whether we have moved off the whole screen
	pop	acc
	ret
lcd_print_glyph_data_inc_subscreen:
	add	a, #0x10							; Increment subscreen
	mov	r2, a								; Store LCD control config
lcd_print_glyph_data_continue:
	pop	acc
	jbc	tmp_bit, lcd_print_glyph_data_loop_double_width			; Do we need to repeat the current bit data
	djnz	b, lcd_print_glyph_data_get_byte				; Is there more data to write
	ret


; # lcd_set_glyph_position
; #
; # Sets the cursor position in the correct subscreen
; # In:
; #   lcd_start_position - Glyph position (b0-b4 - column, b5-7 - row)
; ##########################################################################
lcd_set_glyph_position:
	setb	p1.4								; Enable clock
	mov	a, lcd_start_position						; Get glyph position
	anl	a, #0x1f							; Get column data
	mov	b, #6								; Size of glyph
	mul	ab
	mov	dph, a								; Save result for later
	anl	a, #0xc0							; We're only interested in the last 2 bits, subscreen selection
	rr	a								; Shift into correct position
	rr	a								; Shift into correct position
	orl	a, #0x80							; Select LCD
	xch	a, dph								; Store subscreen selection, get original result
	anl	a, #0x3f							; Get column selection
	mov	lcd_subscreen_column, a						; Save subscreen column postion
	orl	a, #0x40							; LCD command 'Set Address'
	lcall	lcd_send_data							; Send command
	mov	a, lcd_start_position						; Get glyph position
	anl	a, #0xe0							; Get row data
	swap	a								; Equivalent of right shift x5
	rr	a
	mov	b, a								; Save row position
	mov	a, lcd_scroll_offset
	rl	a
	swap	a
	add	a, b
	anl	a, #7
	orl	a, #0xb8							; LCD command 'Set Page'
	lcall	lcd_send_data							; Send command
	mov	r0, lcd_subscreen_column
	cjne	r0, #0x36, lcd_set_glyph_position_subscreen_check_overlap	; Check whether this glyph will overlap subscreens (factors double width)
lcd_set_glyph_position_subscreen_check_overlap:
	jc	lcd_set_glyph_position_exit
	xch	a, dph								; Get selected subscreen
	jnb	acc.5, lcd_set_glyph_position_subscreen_config_next		; Check whether we are at the screen edge
	xch	a, dph
lcd_set_glyph_position_exit:
	clr	p1.4								; Disable clock
	ret
lcd_set_glyph_position_subscreen_config_next:
	push	acc
	add	a, #0x10							; Select next screen
	xch	a, dph
	lcall	lcd_send_data							; Send command 'Set Page'
	mov	a, #0x40							; LCD command 'Set Address' (start of screen)
	lcall	lcd_send_data							; Send command
	pop	dph
	clr	p1.4								; Disable clock
	ret


; # lcd_pstr
; #
; # Print a null terminate string
; # In:
; #   dptr - Pointer to string
; #   lcd_start_position - Glyph position (b0-b4 - column, b5-7 - row)
; ##########################################################################
lcd_pstr:
	push    acc
	push	psw
	orl	psw, #0b00011000
lcd_pstr_next_char:
	clr	a
	movc	a, @a+dptr
	inc	dptr
	jz	lcd_pstr_finish
	push	acc
	anl	a, #0x7F
	acall	lcd_print_character
	pop	acc
	jb	acc.7, lcd_pstr_finish
	sjmp	lcd_pstr_next_char
lcd_pstr_finish:
	pop	psw
	pop	acc
	ret


; # lcd_set_properties_off
; #
; # Used in lcd_off, powers off ancillary lcd components
; ##########################################################################
lcd_set_properties_off:
	mov	a, #0x00
	sjmp	lcd_set_properties_do						; This way we don't save the changes
; # lcd_set_properties_defaults
; #
; # Configures backlight/contrast to default values
; ##########################################################################
lcd_set_properties_defaults:
	mov	a, lcd_props_save						; Get existing values
; # lcd_set_properties
; #
; # Updates the backlight/contrast of the LCD display
; # Latch @00xx + p1.4
; # 0bxxfbcccc - b=backlight, c=contrast, f=flash voltage enable
; # In:
; #   A - LCD properties value
; ##########################################################################
lcd_set_properties:
	mov	lcd_props_save, a						; Save config
	anl	lcd_props_save, #0x1f						; Make sure we only save the values we are interested in
lcd_set_properties_do:
	mov	lcd_properties, a
	setb	p1.4
	mov	dph, #0
	movx	@dptr, a
	clr	p1.4
	ret


; # lcd_set_backlight_toggle
; #
; # Toggle backlight
; ##########################################################################
lcd_set_backlight_toggle:
	mov	a, lcd_properties
	mov	c, acc.4
	cpl	c
	sjmp	lcd_set_backlight
; # lcd_set_backlight_on
; #
; # Turn on LCD backlight
; ##########################################################################
lcd_set_backlight_on:
	setb	c
	sjmp	lcd_set_backlight
; # lcd_set_backlight_off
; #
; ##########################################################################
lcd_set_backlight_off:
	clr	c
; # lcd_set_backlight
; #
; # Set the state of the LCD backlight
; # In:
; #   Carry - LCD backlight state
; ##########################################################################
lcd_set_backlight:
	mov	a, lcd_properties						; Get current properties
	anl	a, #0xef							; Remove current backlight value
	jnc	lcd_set_backlight_value						; If not set, just write value
	setb	acc.4								; Set backlight bit
lcd_set_backlight_value:
	acall	lcd_set_properties
lcd_set_backlight_finish:
	ret


; # lcd_set_contrast_inc
; #
; # Increase the contrast of the LCD
; ##########################################################################
lcd_set_contrast_inc:
	mov	a, lcd_properties						; Get current value
	anl	a, #0x0f							; Get just the contrast
	inc	a								; Increment contrast value
	jb	acc.4, lcd_set_contrast_finish					; We're already at maximum contrast
	sjmp	lcd_set_contrast
; # lcd_set_contrast_dec
; #
; # Decrease the contrast of the LCD
; ##########################################################################
lcd_set_contrast_dec:
	mov	a, lcd_properties						; Get current value
	anl	a, #0x0f							; Get just the contrast
	jz	lcd_set_contrast_finish						; Finish if already zero
	dec	a								; Decrease contrast value
; # lcd_set_contrast
; #
; # Set the contrast of the LCD
; # In:
; #   A - LCD contrast
; ##########################################################################
lcd_set_contrast:
	anl	a, #0x0f							; Make sure value valid
	anl	lcd_properties, #0xf0						; Remove current contrast value
	orl	a, lcd_properties						; Merge other properties with new contrast value
	acall	lcd_set_properties						; Set new value
lcd_set_contrast_finish:
	ret


; ###############################################################################################################
; #                                                   Print functions
; ###############################################################################################################


; # print_bcd
; #
; # Prints a BCD encoded digit (prints zeros as well)
; # In:
; #   A - Byte to print
; ##########################################################################
print_bcd_digit:
	push	acc
	anl	a, #0xf0							; First digit
	swap	a
	add	a, #'0'								; Make an ASCII character
	lcall	oyster_cout
	pop	acc
	anl	a, #0x0f							; Second digit
	add	a, #'0'								; Make an ASCII character
	lcall	oyster_cout
	ret


; ###############################################################################################################
; #                                                   Serial functions
; ###############################################################################################################


; # serial_mainport_enable
; #
; # Enables the main serial port (P1) receivers
; ##########################################################################
serial_mainport_enable:
	orl	sys_props_save, #0x02						; Set flag
	sjmp	serial_mainport_set_state
; # serial_mainport_disable
; #
; # Disables the main serial port (P1) receivers
; ##########################################################################
serial_mainport_disable:
	anl	sys_props_save, #0xfd						; Clear flag
serial_mainport_set_state:
	mov	a, sys_props_save
	mov	c, acc.1
	clr	sfr_p4_80c562.7
	jnc	serial_mainport_set_state_finish
	setb	sfr_p4_80c562.7
serial_mainport_set_state_finish:
	ret


; # serial_rts_set
; #
; # Set RTS (Request to Send)
; ##########################################################################
serial_rts_set:
	setb	sfr_p4_80c562.0
	ret


; # serial_rts_unset
; #
; # Unset RTS (Request to Send)
; ##########################################################################
serial_rts_unset:
	clr	sfr_p4_80c562.0
	ret


; # serial_cts_check
; #
; # Check the status of the CTS (Clear to Send) line
; # Out:
; #  Carry - Status of the CTS line
; ##########################################################################
serial_cts_check:
	push	acc
	mov	a, sfr_p5_80c562
	mov	c, acc.2
	pop	acc
	ret


; # serial_baudsave_check
; #
; # Check whether PaulMON 'baud save' structure exists
; # Out:
; #  Carry - set if baud bytes exist
; ##########################################################################
serial_baudsave_check:
	clr	c
	mov	a, baud_save+3							; Get the current baud rate, if it exists
	xrl	baud_save+2, #01010101b
	cjne	a, baud_save+2, serial_baudsave_check_finish
	xrl	baud_save+2, #01010101b						; Redo XOR
	xrl	baud_save+1, #11001100b
	cjne	a, baud_save+1, serial_baudsave_check_finish
	xrl	baud_save+1, #11001100b						; Redo XOR
	xrl	baud_save+0, #00011101b
	cjne	a, baud_save+0, serial_baudsave_check_finish
	xrl	baud_save+0, #00011101b						; Redo XOR
	setb	c
serial_baudsave_check_finish:
	ret


; # serial_baudsave_set_default
; #
; # Sets the serial baud rate to the default value (19200)
; ##########################################################################
serial_baudsave_set_default:
	mov	a, #BAUD_19200
; # serial_baudsave_set_reload
; #
; # Sets the PaulMON 'baud save' bytes, and updates the hardware reload value
; # In:
; #  A - Baud rate reload value
; ##########################################################################
serial_baudsave_set_reload:
	mov	th1, a								; Set the hardware reload value
	mov	tl1, a
; # serial_baudsave_set
; #
; # Sets the PaulMON 'baud save' bytes
; # In:
; #  A - Baud rate reload value
; ##########################################################################
serial_baudsave_set:
	mov	baud_save+3, a							; Store the baud rate for next warm boot.
	mov	baud_save+2, a
	mov	baud_save+1, a
	mov	baud_save+0, a
        xrl	baud_save+2, #01010101b
        xrl	baud_save+1, #11001100b
        xrl	baud_save+0, #00011101b
	ret


; ###############################################################################################################
; #                                                   Power functions
; ###############################################################################################################


; # power_battery_main_check_charging
; #
; # Returns whether the main battery is currently fast charging
; # U2 (MAX713) - FASTCHG output is checked
; # Out:
; #   Carry - charge status
; ##########################################################################
power_battery_main_check_charging:
	clr	c
	jb	sfr_p4_80c562.4, power_battery_main_check_charging_finish
	setb	c
power_battery_main_check_charging_finish:
	ret


; # power_battery_backup_check_status
; #
; # Returns a simple okay/fail for the backup battery
; # Out:
; #   Carry - battery status
; ##########################################################################
power_battery_backup_check_status:
	mov	a, sfr_p5_80c562						; Read Port 5
	clr	c								; Battery low
	jnb	acc.1, power_battery_backup_check_status_finish
	setb	c								; Battery okay
power_battery_backup_check_status_finish:
	ret


; # power_battery_ramcard_check_status_warn
; #
; # Returns a simple okay/warning for memory card battery
; # Out:
; #   Carry - battery status
; ##########################################################################
power_battery_ramcard_check_status_warn:
	clr	c								; SRAM card battery warning
	jnb	sfr_p4_80c562.2, power_battery_ramcard_check_status_warn_finish
	setb	c								; SRAM card battery okay
power_battery_ramcard_check_status_warn_finish:
	ret


; # power_battery_ramcard_check_status_fail
; #
; # Returns a simple okay/fail for memory card battery
; # Out:
; #   Carry - battery status
; ##########################################################################
power_battery_ramcard_check_status_fail:
	clr	c								; SRAM card battery failure
	jnb	sfr_p4_80c562.3, power_battery_ramcard_check_status_fail_finish
	setb	c								; SRAM card battery okay
power_battery_ramcard_check_status_fail_finish:
	ret


; ###############################################################################################################
; #                                                    Misc functions
; ###############################################################################################################


; # piezo_beep
; #
; # Sounds piezo for a set period of time
; # In:
; #   A - Sound from table
; #   B - Delay x 50ms
; ##########################################################################
piezo_beep:
        lcall   piezo_pwm_sound                                 		; Set tone
        mov     a, b
        lcall   sdelay
        mov     a, #255                                         		; Turn off
        lcall   piezo_pwm_sound                                 		; Set tone
	ret


; # piezo_pwm_sound
; #
; # Sounds piezo buzzer using PWM hardware (as opposed to bitbanging)
; # Crystal frequency 7.3728 MHz
; # PWMP = ((fosc / fpwm) / 510) - 1
; # In:
; #   A - Sound from table (255 - disables)
; ##########################################################################
piezo_pwm_sound:
	cjne	a, #255, piezo_pwm_sound_lookup
	mov	sfr_pwm1_80c562, #0
	ret
piezo_pwm_sound_lookup:
	anl	a, #15
	mov	dptr, #piezo_pwm_table
	movc	a, @a+dptr
	mov	sfr_pwmp_80c562, a
	mov	sfr_pwm1_80c562, #128
	ret

piezo_pwm_table:
	.db	143								; 100 Hz
	.db	71								; 200 Hz
	.db	47								; 301 Hz
	.db	35								; 401 Hz
	.db	28								; 498 Hz
	.db	23								; 602 Hz
	.db	20								; 688 Hz
	.db	17								; 803 Hz
	.db	15								; 903 Hz
	.db	13								; 1032 Hz
	.db	9								; 1445 Hz
	.db	6								; 2065 Hz
	.db	4								; 2891 Hz
	.db	3								; 3614 Hz
	.db	1								; 7228 Hz
	.db	0								; 14456 Hz


; # sdelay
; #
; # Software delay
; # Crystal frequency 7.3728 Mhz - instruction cycle = 12/7372800
; # In:
; #   A - x50 milliseconds to delay
; ##########################################################################
sdelay:
	mov	r2, a
sdelay_50ms:									; 50ms delay
	mov	r0, #120
	mov	r1, #128
sdelay1:
	djnz	r0, sdelay1
sdelay2:
	mov	r0, #120
	djnz	r1, sdelay1
	djnz	r2, sdelay_50ms
	ret

; # terminal_esc_cursor_X
; #
; # Manpiulates the terminal's cursor
; # In:
; #   A - number of characters to move
; ##########################################################################
terminal_esc_cursor_up:
	push	acc								; Save cursor count
	mov	a, #esc_char							; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'A'
	ljmp	cout
terminal_esc_cursor_down:
	push	acc								; Save cursor count
	mov	a, #esc_char							; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'B'
	ljmp	cout
terminal_esc_cursor_left:
	push	acc								; Save cursor count
	mov	a, #esc_char							; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'D'
	ljmp	cout
terminal_esc_cursor_right:
	push	acc								; Save cursor count
	mov	a, #esc_char							; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'C'
	ljmp	cout

.equ	esc_char, 0x1b

; ###############################################################################################################
; #                                                    System config functions
; ###############################################################################################################


; # system_config_check
; #
; # Check whether the system config structure exists
; #  Out:
; #   Carry - Set if config exists
; ##########################################################################
system_config_check:
	clr	a
	mov	r0, #sys_config_start
system_config_check_loop:
	add	a, @r0
	cjne	r0, #sys_config_end, system_config_check_loop_cmp
system_config_check_loop_cmp:
	jnc	system_config_check_checksum
	inc	r0
	sjmp	system_config_check_loop
system_config_check_checksum:
	cjne	a, sys_cfg_checksum, system_config_check_error
	cpl	a
	cjne	a, sys_cfg_chksum_inv, system_config_check_error
	setb	c
	ret
system_config_check_error:
	clr	c
	ret


; # system_config_save
; #
; # Saves the current system configuration
; #  Out:
; #   Carry - Set on error
; ##########################################################################
system_config_save:
	clr	a
	mov	r0, #sys_config_start
system_config_save_loop:
	add	a, @r0
	cjne	r0, #sys_config_end, system_config_save_loop_cmp
system_config_save_loop_cmp:
	jnc	system_config_save_checksum
	inc	r0
	sjmp	system_config_save_loop
system_config_save_checksum:
	mov	sys_cfg_checksum, a
	cpl	a
	mov	sys_cfg_chksum_inv, a

system_config_save_2_rtc:
	mov	r1, #sys_config_start						; Register pointer
	mov	a, #sys_config_start						; Write registers to their corresponding address in RTC RAM
	mov	b, #rtc_addr1							; RTC i2c address
	lcall	i2c_write_byte_to_addr
	jc	system_config_save_finish					; Just finish if there's an error
system_config_save_2_rtc_loop:
	mov	a, @r1								; Get a byte of config data
	lcall	i2c_write_byte_with_ack_check					; Write to RTC memory
	jc	system_config_save_finish					; Check for errors
	cjne	r1, #sys_config_end+2, system_config_save_2_rtc_loop_cmp	; See if we have reached the end of the data we're interested in (+2 for the checksum)
system_config_save_2_rtc_loop_cmp:
	jnc	system_config_save_finish					; Will jump when previous operands are equal
	inc	r1								; Increment pointer
	sjmp	system_config_save_2_rtc_loop
system_config_save_finish:
	lcall	i2c_stop
	clr	c
	ret


; # system_config_restore
; #
; # Restores the system configuration, or sets defaults
; ##########################################################################
system_config_restore:
	lcall	system_config_check						; See if system config data exists (Warm boot)
	jc	system_config_restore_do

system_config_restore_from_rtc:							; Try restoring data from RTC memory
	mov	r1, #sys_config_start						; Register pointer
	mov	a, #sys_config_start						; Read registers from their corresponding address in RTC RAM
	mov	b, #rtc_addr1							; RTC i2c address
	lcall	i2c_read_device_register
	jc	system_config_restore_from_rtc_finish				; Just finish if there's an error
system_config_restore_from_rtc_loop:
	lcall	i2c_master_read_ack
	mov	@r1, a								; Save byte
	cjne	r1, #sys_config_end+2, system_config_restore_from_rtc_loop_cmp	; See if we have reached the end of the data we're interested in (+2 for the checksum)
system_config_restore_from_rtc_loop_cmp:
	jnc	system_config_restore_from_rtc_finish				; Will jump when previous operands are equal
	inc	r1								; Increment pointer
	lcall	i2c_read_byte
	sjmp	system_config_restore_from_rtc_loop
system_config_restore_from_rtc_finish:
	lcall	i2c_stop

	lcall	system_config_check						; Has this provided a valid system config (Cold boot)
	jc	system_config_restore_do					; Otherwise set system defaults

	mov	current_year, #0x00						; Equivalent to y2k
	lcall	serial_baudsave_set_default					; Default baud rate
	mov	lcd_props_save, #0x14						; LCD properties defaults, backlight on, contrast=4
	mov	sys_props_save, #0x03						; Key click enabled, Serial port enabled
system_config_restore_do:
	mov	a, sys_props_save
	mov	c, acc.0
	mov	key_click, c							; Restore key click
	lcall	serial_mainport_set_state					; Restore main serial port state
	ret


; ###############################################################################################################
; #                                                     General data
; ###############################################################################################################

.org    locat+0x900

ascii_font_table:
ascii_font_table_char_0:							; Null
	.db	0x41, 0x41, 0x41, 0x41, 0x41, 0x00
ascii_font_table_char_1:							; 
	.db	0x40, 0x5e, 0x45, 0x45, 0x5e, 0x00
ascii_font_table_char_2:							; 
	.db	0x00, 0x7f, 0x3e, 0x1c, 0x08, 0x00
ascii_font_table_char_3:							; 
	.db	0x08, 0x1c, 0x3e, 0x7f, 0x00, 0x00
ascii_font_table_char_4:							; 
	.db	0x40, 0x5f, 0x51, 0x51, 0x4e, 0x00
ascii_font_table_char_5:							; 
	.db	0x40, 0x5f, 0x55, 0x55, 0x51, 0x00
ascii_font_table_char_6:							; 
	.db	0x40, 0x5f, 0x45, 0x45, 0x41, 0x00
ascii_font_table_char_7:							; Bell
	.db	0x48, 0x4e, 0x51, 0x4e, 0x48, 0x00
ascii_font_table_char_8:							; 
	.db	0x08, 0x1c, 0x2a, 0x08, 0x08, 0x00
ascii_font_table_char_9:							; 
	.db	0x08, 0x08, 0x2a, 0x1c, 0x08, 0x00
ascii_font_table_char_10:							; Down arrow
	.db	0x10, 0x20, 0x7f, 0x20, 0x10, 0x00
ascii_font_table_char_11:							; Up arrow
	.db	0x04, 0x02, 0x7f, 0x02, 0x04, 0x00
ascii_font_table_char_12:							; 
	.db	0x40, 0x40, 0x44, 0x40, 0x40, 0x00
ascii_font_table_char_13:							; 
	.db	0x7f, 0x77, 0x63, 0x49, 0x7f, 0x00
ascii_font_table_char_14:							; 
	.db	0x24, 0x12, 0x09, 0x12, 0x24, 0x00
ascii_font_table_char_15:							; 
	.db	0x09, 0x12, 0x24, 0x12, 0x09, 0x00
ascii_font_table_char_16:							; 
	.db	0x4e, 0x59, 0x55, 0x53, 0x4e, 0x00
ascii_font_table_char_17:							; 
	.db	0x40, 0x52, 0x5f, 0x50, 0x40, 0x00
ascii_font_table_char_18:							; 
	.db	0x40, 0x59, 0x55, 0x52, 0x40, 0x00
ascii_font_table_char_19:							; 
	.db	0x40, 0x55, 0x55, 0x4e, 0x40, 0x00
ascii_font_table_char_20:							; 
	.db	0x40, 0x4c, 0x4a, 0x5f, 0x48, 0x00
ascii_font_table_char_21:							; 
	.db	0x40, 0x4f, 0x50, 0x50, 0x4f, 0x00
ascii_font_table_char_22:							; 
	.db	0x43, 0x4c, 0x50, 0x4c, 0x43, 0x00
ascii_font_table_char_23:							; 
	.db	0x4f, 0x50, 0x4c, 0x50, 0x4f, 0x00
ascii_font_table_char_24:							; 
	.db	0x51, 0x4a, 0x44, 0x4a, 0x51, 0x00
ascii_font_table_char_25:							; 
	.db	0x41, 0x42, 0x5c, 0x42, 0x41, 0x00
ascii_font_table_char_26:							; 
	.db	0x51, 0x59, 0x55, 0x53, 0x51, 0x00
ascii_font_table_char_27:							; 
	.db	0x7f, 0x41, 0x55, 0x5d, 0x7f, 0x00
ascii_font_table_char_28:							; 
	.db	0x41, 0x42, 0x44, 0x48, 0x50, 0x00
ascii_font_table_char_29:							; 
	.db	0x40, 0x51, 0x51, 0x5f, 0x40, 0x00
ascii_font_table_char_30:							; 
	.db	0x1e, 0x06, 0x0a, 0x12, 0x20, 0x00
ascii_font_table_char_31:							; 
	.db	0x10, 0x38, 0x54, 0x10, 0x1f, 0x00
ascii_font_table_char_32:							; ' '
	.db	0x00, 0x00, 0x00, 0x00, 0x00, 0x00
ascii_font_table_char_33:							; '!'
	.db	0x00, 0x00, 0x5f, 0x00, 0x00, 0x00
ascii_font_table_char_34:							; '"'
	.db	0x00, 0x07, 0x00, 0x07, 0x00, 0x00
ascii_font_table_char_35:							; '#'
	.db	0x14, 0x7f, 0x14, 0x7f, 0x14, 0x00
ascii_font_table_char_36:							; '$'
	.db	0x24, 0x2a, 0x7f, 0x2a, 0x12, 0x00
ascii_font_table_char_37:							; '%'
	.db	0x23, 0x13, 0x08, 0x64, 0x62, 0x00
ascii_font_table_char_38:							; '&'
	.db	0x36, 0x49, 0x56, 0x20, 0x50, 0x00
ascii_font_table_char_39:							; '''
	.db	0x00, 0x00, 0x07, 0x00, 0x00, 0x00
ascii_font_table_char_40:							; '('
	.db	0x1c, 0x22, 0x41, 0x00, 0x00, 0x00
ascii_font_table_char_41:							; ')'
	.db	0x00, 0x00, 0x41, 0x22, 0x1c, 0x00
ascii_font_table_char_42:							; '*'
	.db	0x22, 0x14, 0x7f, 0x14, 0x22, 0x00
ascii_font_table_char_43:							; '+'
	.db	0x08, 0x08, 0x3e, 0x08, 0x08, 0x00
ascii_font_table_char_44:							; ','
	.db	0x00, 0x50, 0x30, 0x00, 0x00, 0x00
ascii_font_table_char_45:							; '-'
	.db	0x08, 0x08, 0x08, 0x08, 0x08, 0x00
ascii_font_table_char_46:							; '.'
	.db	0x00, 0x60, 0x60, 0x00, 0x00, 0x00
ascii_font_table_char_47:							; '/'
	.db	0x20, 0x10, 0x08, 0x04, 0x02, 0x00
ascii_font_table_char_48:							; '0'
	.db	0x3e, 0x51, 0x49, 0x45, 0x3e, 0x00
ascii_font_table_char_49:							; '1'
	.db	0x00, 0x42, 0x7f, 0x40, 0x00, 0x00
ascii_font_table_char_50:							; '2'
	.db	0x62, 0x51, 0x49, 0x49, 0x46, 0x00
ascii_font_table_char_51:							; '3'
	.db	0x21, 0x41, 0x49, 0x4d, 0x33, 0x00
ascii_font_table_char_52:							; '4'
	.db	0x18, 0x14, 0x12, 0x7f, 0x10, 0x00
ascii_font_table_char_53:							; '5'
	.db	0x27, 0x45, 0x45, 0x45, 0x39, 0x00
ascii_font_table_char_54:							; '6'
	.db	0x3c, 0x4a, 0x49, 0x49, 0x31, 0x00
ascii_font_table_char_55:							; '7'
	.db	0x01, 0x71, 0x09, 0x05, 0x03, 0x00
ascii_font_table_char_56:							; '8'
	.db	0x36, 0x49, 0x49, 0x49, 0x36, 0x00
ascii_font_table_char_57:							; '9'
	.db	0x46, 0x49, 0x49, 0x29, 0x1e, 0x00
ascii_font_table_char_58:							; ':'
	.db	0x00, 0x00, 0x14, 0x00, 0x00, 0x00
ascii_font_table_char_59:							; ';'
	.db	0x00, 0x40, 0x34, 0x00, 0x00, 0x00
ascii_font_table_char_60:							; '<'
	.db	0x08, 0x14, 0x22, 0x41, 0x00, 0x00
ascii_font_table_char_61:							; '='
	.db	0x14, 0x14, 0x14, 0x14, 0x14, 0x00
ascii_font_table_char_62:							; '>'
	.db	0x41, 0x22, 0x14, 0x08, 0x00, 0x00
ascii_font_table_char_63:							; '?'
	.db	0x02, 0x01, 0x59, 0x05, 0x02, 0x00
ascii_font_table_char_64:							; '@'
	.db	0x3e, 0x41, 0x5d, 0x59, 0x4e, 0x00
ascii_font_table_char_65:							; 'A'
	.db	0x7c, 0x12, 0x11, 0x12, 0x7c, 0x00
ascii_font_table_char_66:							; 'B'
	.db	0x7f, 0x49, 0x49, 0x49, 0x36, 0x00
ascii_font_table_char_67:							; 'C'
	.db	0x3e, 0x41, 0x41, 0x41, 0x22, 0x00
ascii_font_table_char_68:							; 'D'
	.db	0x7f, 0x41, 0x41, 0x41, 0x3e, 0x00
ascii_font_table_char_69:							; 'E'
	.db	0x7f, 0x49, 0x49, 0x49, 0x41, 0x00
ascii_font_table_char_70:							; 'F'
	.db	0x7f, 0x09, 0x09, 0x09, 0x01, 0x00
ascii_font_table_char_71:							; 'G'
	.db	0x3e, 0x41, 0x41, 0x51, 0x71, 0x00
ascii_font_table_char_72:							; 'H'
	.db	0x7f, 0x08, 0x08, 0x08, 0x7f, 0x00
ascii_font_table_char_73:							; 'I'
	.db	0x00, 0x41, 0x7f, 0x41, 0x00, 0x00
ascii_font_table_char_74:							; 'J'
	.db	0x20, 0x40, 0x40, 0x40, 0x3f, 0x00
ascii_font_table_char_75:							; 'K'
	.db	0x7f, 0x08, 0x14, 0x22, 0x41, 0x00
ascii_font_table_char_76:							; 'L'
	.db	0x7f, 0x40, 0x40, 0x40, 0x40, 0x00
ascii_font_table_char_77:							; 'M'
	.db	0x7f, 0x02, 0x0c, 0x02, 0x7f, 0x00
ascii_font_table_char_78:							; 'N'
	.db	0x7f, 0x04, 0x08, 0x10, 0x7f, 0x00
ascii_font_table_char_79:							; 'O'
	.db	0x3e, 0x41, 0x41, 0x41, 0x3e, 0x00
ascii_font_table_char_80:							; 'P'
	.db	0x7f, 0x09, 0x09, 0x09, 0x06, 0x00
ascii_font_table_char_81:							; 'Q'
	.db	0x3e, 0x41, 0x51, 0x21, 0x5e, 0x00
ascii_font_table_char_82:							; 'R'
	.db	0x7f, 0x09, 0x19, 0x29, 0x46, 0x00
ascii_font_table_char_83:							; 'S'
	.db	0x26, 0x49, 0x49, 0x49, 0x32, 0x00
ascii_font_table_char_84:							; 'T'
	.db	0x01, 0x01, 0x7f, 0x01, 0x01, 0x00
ascii_font_table_char_85:							; 'U'
	.db	0x3f, 0x40, 0x40, 0x40, 0x3f, 0x00
ascii_font_table_char_86:							; 'V'
	.db	0x1f, 0x20, 0x40, 0x20, 0x1f, 0x00
ascii_font_table_char_87:							; 'W'
	.db	0x7f, 0x20, 0x18, 0x20, 0x7f, 0x00
ascii_font_table_char_88:							; 'X'
	.db	0x63, 0x14, 0x08, 0x14, 0x63, 0x00
ascii_font_table_char_89:							; 'Y'
	.db	0x03, 0x04, 0x78, 0x04, 0x03, 0x00
ascii_font_table_char_90:							; 'Z'
	.db	0x61, 0x51, 0x49, 0x45, 0x43, 0x00
ascii_font_table_char_91:							; '['
	.db	0x7f, 0x7f, 0x41, 0x41, 0x41, 0x00
ascii_font_table_char_92:							; '\'
	.db	0x02, 0x04, 0x08, 0x10, 0x20, 0x00
ascii_font_table_char_93:							; ']'
	.db	0x41, 0x41, 0x41, 0x7f, 0x7f, 0x00
ascii_font_table_char_94:							; '^'
	.db	0x10, 0x08, 0x04, 0x08, 0x10, 0x00
ascii_font_table_char_95:							; '_'
	.db	0x40, 0x40, 0x40, 0x40, 0x40, 0x00
ascii_font_table_char_96:							; '`'
	.db	0x00, 0x01, 0x02, 0x04, 0x00, 0x00
ascii_font_table_char_97:							; 'a'
	.db	0x38, 0x44, 0x44, 0x28, 0x7c, 0x00
ascii_font_table_char_98:							; 'b'
	.db	0x7f, 0x28, 0x44, 0x44, 0x38, 0x00
ascii_font_table_char_99:							; 'c'
	.db	0x38, 0x44, 0x44, 0x44, 0x44, 0x00
ascii_font_table_char_100:							; 'd'
	.db	0x38, 0x44, 0x44, 0x28, 0x7f, 0x00
ascii_font_table_char_101:							; 'e'
	.db	0x38, 0x54, 0x54, 0x54, 0x18, 0x00
ascii_font_table_char_102:							; 'f'
	.db	0x00, 0x08, 0x7e, 0x09, 0x00, 0x00
ascii_font_table_char_103:							; 'g'
	.db	0x08, 0x54, 0x54, 0x54, 0x3c, 0x00
ascii_font_table_char_104:							; 'h'
	.db	0x7f, 0x08, 0x04, 0x04, 0x78, 0x00
ascii_font_table_char_105:							; 'i'
	.db	0x00, 0x48, 0x7a, 0x40, 0x00, 0x00
ascii_font_table_char_106:							; 'j'
	.db	0x20, 0x40, 0x3d, 0x00, 0x00, 0x00
ascii_font_table_char_107:							; 'k'
	.db	0x7f, 0x10, 0x18, 0x24, 0x42, 0x00
ascii_font_table_char_108:							; 'l'
	.db	0x00, 0x41, 0x7f, 0x40, 0x00, 0x00
ascii_font_table_char_109:							; 'm'
	.db	0x78, 0x04, 0x18, 0x04, 0x78, 0x00
ascii_font_table_char_110:							; 'n'
	.db	0x78, 0x08, 0x04, 0x04, 0x78, 0x00
ascii_font_table_char_111:							; 'o'
	.db	0x38, 0x44, 0x44, 0x44, 0x38, 0x00
ascii_font_table_char_112:							; 'p'
	.db	0x7c, 0x14, 0x24, 0x24, 0x18, 0x00
ascii_font_table_char_113:							; 'q'
	.db	0x18, 0x24, 0x14, 0x7c, 0x40, 0x00
ascii_font_table_char_114:							; 'r'
	.db	0x7c, 0x08, 0x04, 0x04, 0x00, 0x00
ascii_font_table_char_115:							; 's'
	.db	0x48, 0x54, 0x54, 0x54, 0x24, 0x00
ascii_font_table_char_116:							; 't'
	.db	0x00, 0x04, 0x3f, 0x44, 0x00, 0x00
ascii_font_table_char_117:							; 'u'
	.db	0x3c, 0x40, 0x40, 0x40, 0x7c, 0x00
ascii_font_table_char_118:							; 'v'
	.db	0x0c, 0x30, 0x40, 0x30, 0x0c, 0x00
ascii_font_table_char_119:							; 'w'
	.db	0x3c, 0x40, 0x20, 0x40, 0x3c, 0x00
ascii_font_table_char_120:							; 'x'
	.db	0x44, 0x28, 0x10, 0x28, 0x44, 0x00
ascii_font_table_char_121:							; 'y'
	.db	0x04, 0x48, 0x30, 0x08, 0x04, 0x00
ascii_font_table_char_122:							; 'z'
	.db	0x44, 0x64, 0x54, 0x4c, 0x44, 0x00
ascii_font_table_char_123:							; '{'
	.db	0x08, 0x36, 0x41, 0x00, 0x00, 0x00
ascii_font_table_char_124:							; '|'
	.db	0x00, 0x00, 0x77, 0x00, 0x00, 0x00
ascii_font_table_char_125:							; '}'
	.db	0x00, 0x00, 0x41, 0x36, 0x08, 0x00
ascii_font_table_char_126:							; '~'
	.db	0x10, 0x08, 0x08, 0x08, 0x04, 0x00
ascii_font_table_char_127:							; DEL
	.db	0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x00

glyph_double_size_conversion_table:
	.db	0x00, 0x03, 0x0c, 0x0f, 0x30, 0x33, 0x3c, 0x3f
	.db	0xc0, 0xc3, 0xcc, 0xcf, 0xf0, 0xf3, 0xfc, 0xff

baud_rate_table:
	.db	BAUD_19200
	.db	BAUD_9600
	.db	BAUD_4800
	.db	BAUD_2400
	.db	BAUD_1200
	.db	BAUD_600
	.db	BAUD_300
	.db	BAUD_150

.org	locat+0xCD0
baud_rate_str_table:
	.db	"19200", 0
	.db	" 9600", 0
	.db	" 4800", 0
	.db	" 2400", 0
	.db	" 1200", 0
	.db	"  600", 0
	.db	"  300", 0
	.db	"  150", 0

.org    locat+0xD00								; location defined so the different keymaps can easily be selected
; Keymap
keycode_2_character_table1:
	.db	'A'	; A
	.db	'E'	; E
	.db	'K'	; K
	.db	'Q'	; Q
	.db	'W'	; W
	.db	0x0b	; Up
	.db	0x0a	; Down
	.db	0x03	; Cancel (Ctrl-C)
	.db	'B'	; B
	.db	'F'	; F
	.db	'L'	; L
	.db	'R'	; R
	.db	'X'	; X
	.db	'7'	; 7
	.db	'4'	; 4
	.db	'1'	; 1
	.db	'C'	; C
	.db	'G'	; G
	.db	'M'	; M
	.db	'S'	; S
	.db	'Y'	; Y
	.db	'8'	; 8
	.db	'5'	; 5
	.db	'2'	; 2
	.db	'D'	; D
	.db	'H'	; H
	.db	'N'	; N
	.db	'T'	; T
	.db	'Z'	; Z
	.db	'9'	; 9
	.db	'6'	; 6
	.db	'3'	; 3
	.db	0xFF	; Mode
	.db	'I'	; I
	.db	'O'	; O
	.db	'U'	; U
	.db	' '	; ' '
	.db	'.'	; .
	.db	'0'	; 0
	.db	'-'	; -
	.db	0xff	;
	.db	'J'	; J
	.db	'P'	; P
	.db	'V'	; V
	.db	0x7f	; Delete
	.db	0x08	; Left
	.db	0x09	; Right
	.db	0x0d	; Enter
keycode_2_character_table2:
	.db	'a'	; A
	.db	'e'	; E
	.db	'k'	; K
	.db	'q'	; Q
	.db	'w'	; W
	.db	0x0b	; Up
	.db	0x0a	; Down
	.db	0x04	; Cancel (Ctrl-D)
	.db	'b'	; B
	.db	'f'	; F
	.db	'l'	; L
	.db	'r'	; R
	.db	'x'	; X
	.db	'&'	; 7
	.db	'$'	; 4
	.db	'!'	; 1
	.db	'c'	; C
	.db	'g'	; G
	.db	'm'	; M
	.db	's'	; S
	.db	'y'	; Y
	.db	'*'	; 8
	.db	'%'	; 5
	.db	'"'	; 2
	.db	'd'	; D
	.db	'h'	; H
	.db	'n'	; N
	.db	't'	; T
	.db	'z'	; Z
	.db	'('	; 9
	.db	'^'	; 6
	.db	0x9C	; 3 (£)
	.db	0xFF	; Mode
	.db	'i'	; I
	.db	'o'	; O
	.db	'u'	; U
	.db	' '	; ' '
	.db	','	; .
	.db	')'	; 0
	.db	'_'	; -
	.db	0xff	;
	.db	'j'	; J
	.db	'p'	; P
	.db	'v'	; V
	.db	0x7f	; Delete
	.db	0x08	; Left
	.db	0x09	; Right
	.db	0x0d	; Enter
keycode_2_character_table3:
	.db	'='	; A
	.db	'}'	; E
	.db	'@'	; K
	.db	'?'	; Q
	.db	0x4F	; W (End, when preceded by a null character)
	.db	0x0b	; Up
	.db	0x0a	; Down
	.db	0x18	; Cancel
	.db	'+'	; B
	.db	'['	; F
	.db	0x27	; L (')
	.db	'|'	; R
	.db	0x1B	; X (Escape)
	.db	0x41	; 7 (F7, when preceded by a null character)
	.db	0x3E	; 4 (F4, when preceded by a null character)
	.db	0x3B	; 1 (F1, when preceded by a null character)
	.db	'/'	; C
	.db	']'	; G
	.db	'~'	; M
	.db	0x5C	; S (\)
	.db	0x49	; Y (PageUp, when preceded by a null character)
	.db	0x42	; 8 (F8, when preceded by a null character)
	.db	0x3F	; 5 (F5, when preceded by a null character)
	.db	0x3C	; 2 (F2, when preceded by a null character)
	.db	'{'	; D
	.db	'?'	; H
	.db	'#'	; N
	.db	0xAC	; T (¬)
	.db	0x51	; Z (PageDown, when preceded by a null character)
	.db	0x43	; 9 (F9, when preceded by a null character)
	.db	0x40	; 6 (F6, when preceded by a null character)
	.db	0x3D	; 3 (F3, when preceded by a null character)
	.db	0xFF	; Mode
	.db	':'	; I
	.db	'<'	; O
	.db	'`'	; U
	.db	' '	; ' '
	.db	0x86	; . (F12, when preceded by a null character)
	.db	0x44	; 0 (F10, when preceded by a null character)
	.db	0x85	; - (F11, when preceded by a null character)
	.db	0xff	;
	.db	';'	; J
	.db	'>'	; P
	.db	0x47	; V (Home, when preceded by a null character)
	.db	0x7f	; Delete
	.db	0x08	; Left
	.db	0x09	; Right
	.db	0x0d	; Enter

str_enabled:		.db	"Enabled ", 0
str_disabled:		.db	"Disabled", 0
str_fail:		.db	"Fail", 0
str_warn:		.db	"Warning", 0
str_okay:		.db	"OK", 0
str_skip:		.db	"Skip", 0
str_present:		.db	"Present", 0
str_na:			.db	"N/A", 0
str_cfg_mem:		.db	"Config memory: ", 0
str_tst_mem:		.db	"Testing memory bank [", 0
str_memory_card:	.db	"Memory Card", 0
str_read_only:		.db	"Read Only", 0
str_rtc:		.db	"RTC init: ", 0
str_timeout:		.db	"Timeout: ", 0
;str_keyb:		.db	"Keyboard init: ", 0
str_backup:		.db	"Backup ", 0
str_battery:		.db	"Battery: ", 0

str_init_header:	.db	" PAULMON ", 0
str_init_select:	.db	"Select console:", 0
str_init_oyster:	.db	"       O - Oyster Terminal", 0
str_init_serial:	.db	"       S - Serial", 0

str_setup_header:	.db	"  SETUP  ", 0
str_setup_screenkey:	.db	"   Screen/Keyboard   ", 0
str_setup_datetime:	.db	"   Set Date/Time   ", 0
str_setup_serial:	.db	"   Configure Serial   ", 0
str_setup_power:	.db	"   Power Status   ", 0
str_setupsk_header:	.db	"Screen/Keyboard", 0
str_setupsk_contrast:	.db	0x0a,"/",0x0b," - Adjust contrast", 0
str_setupsk_backlight:	.db	"  B - Toggle backlight", 0
str_setupsk_keyclick:	.db	"  C - Toggle key click", 0
str_setupdt_header:	.db	"Date/Time", 0
str_setupdt_date:	.db	"(D)ate : ", 0
str_setupdt_time:	.db	"(T)ime : ", 0
str_setupdt_alarm:	.db	"Alar(m)     : ", 0
str_setupdt_adate:	.db	"D(a)te : ", 0
str_setupdt_atime:	.db	"T(i)me : ", 0
str_setupsrl_header:	.db	"Serial", 0
str_setupsrl_mport:	.db	"(M)ain port: ", 0
str_setupsrl_baud:	.db	"(B)aud rate: ", 0
str_setuppwr_header:	.db	"Power Status", 0
str_setuppwr_cstatus:	.db	"Charging Status: ", 0
str_setuppwr_charging:	.db	"Charging", 0
str_setuppwr_charged:	.db	"Charged ", 0


; ###############################################################################################################
; #                                                     OysterLib ISVs
; ###############################################################################################################

; Startup (not used obviously)
.org    locat+0x1000

; External interrupt 0 (INT0)
.org    locat+0x1003

; Timer 0 overflow
.org    locat+0x100B

; External interrupt 1 (INT1)
.org    locat+0x1013

; Timer 1 overflow
.org    locat+0x101B

; SIO0 (UART)
.org    locat+0x1023

; SIO1 (I2C, doesn't exist)
.org    locat+0x102B

; Timer 2 capture 0
.org    locat+0x1033

; Timer 2 capture 1
.org    locat+0x103B

; Timer 2 capture 2
.org    locat+0x1043

; Timer 2 capture 3
.org    locat+0x104B

; ADC completion
.org    locat+0x1053

; Timer 2 compare 0
.org    locat+0x105B

; Timer 2 compare 1
.org    locat+0x1063

; Timer 2 compare 2
.org    locat+0x106B

; Timer 2 overflow
.org    locat+0x1073

; ###############################################################################################################
; #                                                       Commands
; ###############################################################################################################

;---------------------------------------------------------;
;                                                         ;
;                       System setup                      ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x1100
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     253,',',0,0		; id (253=startup)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "System setup",0
.org    locat+0x1140		; executable code begins here

system_setup:
	lcall	newline
system_setup_init_reg:
	mov	c, use_oysterlib						; Save flag state
	clr	keyboard_key_down						; Clear keyboard flag
	clr	keyboard_new_char						; Clear keyboard flag
	mov	a, #0
	mov	keycode_ascii, a						; Clear keyboard buffer
	mov	mem_mode, a
	cpl	a
	mov	keycode_raw, a							; Clear raw keyboard buffer
	mov	use_oysterlib, c						; Restore flag state

system_setup_ram:
	mov	dptr, #str_cfg_mem
	lcall	pstr
;	mov	r7, #mem_sys_rom						; When running from ROM
	mov	r7, #mem_sys_ram						; When running from RAM
	mov	a, r7								; We'll need this when running the memory check
	lcall	memory_set_mode_psen						; Set program access method
	mov	a, #mem_sys_ram
	lcall	memory_set_mode_rdwr						; Set data access method
	mov	a, #0x01
	lcall	memory_set_page_psen						; Set program page
	lcall	memory_set_page_rdwr						; Set data page
	mov	dptr, #str_okay
	lcall	pstr
	lcall	newline

system_setup_check_ram:
	mov	r6, #0
system_setup_check_ram_loop:
	mov	dptr, #str_tst_mem
	lcall	pstr
	mov	a, r6
	lcall	pint8u
	mov	a, #']'
	lcall	cout
	mov	a, #':'
	lcall	cout
	mov	a, #' '
	lcall	cout
	cjne	r6, #1, system_setup_check_ram_do				; If it is the first pasge of memory
	cjne	r7, #mem_sys_ram, system_setup_check_ram_do			; Are we about to erase ourself
	mov	dptr, #str_skip
	sjmp	system_setup_check_ram_result
system_setup_check_ram_do:
	mov	a, r6
	lcall	memory_set_page_rdwr
	lcall	memory_sram_check
	mov	dptr, #str_fail
	jc	system_setup_check_ram_result
	mov	dptr, #str_okay
system_setup_check_ram_result:
	lcall	pstr
	lcall	newline
	inc	r6
	cjne	r6, #4, system_setup_check_ram_loop
	mov	a, #0x01
	lcall	memory_set_page_rdwr						; Reselect the correct page

system_setup_ramcard:
	mov	dptr, #str_memory_card
	lcall	pstr
	mov	a, #':'
	lcall	cout
	mov	a, #' '
	lcall	cout
	lcall	memory_ramcard_present
	mov	dptr, #str_na
	jnc	system_setup_ramcard_print_presence
	lcall	memory_ramcard_write_protect
	mov	dptr, #str_present
	jnc	system_setup_ramcard_print_presence
	mov	dptr, #str_read_only
system_setup_ramcard_print_presence:
	lcall	pstr
	lcall	newline

system_setup_rtc:
	mov	dptr, #str_rtc
	lcall	pstr
	lcall	rtc_init
	mov	dptr, #str_fail
	jc	system_setup_rtc_print
	mov	dptr, #str_okay
system_setup_rtc_print:
	lcall	pstr
	lcall	newline

system_setup_backup_battery:
	mov	dptr, #str_backup
	lcall	pstr
	mov	dptr, #str_battery
	lcall	pstr
	lcall	power_battery_backup_check_status
	mov	dptr, #str_fail
	jnc	system_setup_backup_battery_print_status
	mov	dptr, #str_okay
system_setup_backup_battery_print_status:
	lcall	pstr
	lcall	newline

system_setup_mcard_battery:
	lcall	memory_ramcard_present						; If there is no ram card, don't bother checking it's status!
	jnc	system_setup_continue
	mov	dptr, #str_memory_card
	lcall	pstr
	mov	a, #' '
	lcall	cout
	mov	dptr, #str_battery
	lcall	pstr
	lcall	power_battery_ramcard_check_status_warn
	mov	dptr, #str_fail
	jnc	system_setup_mcard_battery_print_status
	mov	dptr, #str_okay
system_setup_mcard_battery_print_status:
	lcall	pstr

system_setup_continue:
	mov	a, #1								; 200 Hz
	mov	b, #10								; 500ms
	lcall	piezo_beep							; Set tone
	mov	a, #10								; 1445 Hz
	mov	b, #5								; 250ms
	lcall	piezo_beep							; Set tone

	lcall	newline

	ret


;---------------------------------------------------------;
;                                                         ;
;                       System init                       ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x1300
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     249,',',0,0		; id (249=init)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "System init",0
.org    locat+0x1340		; executable code begins here

system_init:
; General system config
	mov	ie, #0								; Disable interrupts (0xa8)
	mov	sfr_ie2_80c562, #0						; Disable interrupts (0xe8)
	mov	sfr_pwm1_80c562, #0						; PWM1 is connected to the piezo speaker
	orl	pcon, #10h							; Set condition to load watchdog timer
	mov	sfr_t3_80c562, #0						; Load watchdog timer
	lcall	serial_mainport_enable						; Make sure main serial port enabled
; i2c config
	setb	p1.6								; SCL
	setb	p1.7								; SDA

	lcall	system_config_restore						; Either restore an existing config, otherwise set defaults

system_init_keyboard:
	lcall	keyboard_reset
	clr	keyboard_new_char
	mov	keycode_raw, #0xff						; Clear buffer (0 not used as it's a valid keycode)
	mov	keymap_offset, #0						; Select first keymap

system_init_lcd:
	lcall	lcd_init

	mov	a, #10								; 1445 Hz
	mov	b, #5
	lcall	piezo_beep							; Beep tone

	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x07					; 1st row, 5th column
	mov	dptr, #str_init_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	clr	lcd_glyph_invert
	mov	lcd_start_position, #0x60					; 3rd row, 1st column
	mov	dptr, #str_init_select
	lcall	lcd_pstr
	mov	lcd_start_position, #0x80					; 4th row, 1st column
	mov	dptr, #str_init_oyster
	lcall	lcd_pstr
	mov	lcd_start_position, #0xa0					; 5th row, 1st column
	mov	dptr, #str_init_serial
	lcall	lcd_pstr

system_init_timeout_loop_setup:
	mov	r5, #0x64							; 5 seconds of delays
	clr	keyboard_new_char
system_init_timeout_loop:
	mov	lcd_start_position, #0xF5					; last row, 22nd column
	mov	dptr, #str_timeout
	lcall	lcd_pstr

	mov	a, r5								; Remaining time delays left
	mov	b, #0x14							; Setup divide by 20
	div	ab								; Seconds left
	inc	a								; Makes the value more understandable
	orl	a, #0x30							; Convert to ASCII number
	lcall	lcd_print_character

	lcall	keyboard_scan							; Keyboard input, if any
	jb	keyboard_new_char, system_init_mode_select			; Continue if key pressed
	mov	a, #1
	lcall	sdelay								; Delay 50ms
	djnz	r5, system_init_timeout_loop					; Loop if we haven't timed out

	mov	keycode_ascii, #'S'						; Set default to serial

system_init_mode_select:
	mov	a, keycode_ascii
system_init_mode_select_serial:
	cjne	a, #'S',system_init_mode_select_oyster
	clr	use_oysterlib
	lcall	lcd_off
	ret
system_init_mode_select_oyster:
	cjne	a, #'O',system_init_timeout_loop_setup
	setb	use_oysterlib
	lcall	lcd_clear_screen
	ret


;---------------------------------------------------------;
;                                                         ;
;                          Setup                          ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x1400
.db     0xA5,0xE5,0xE0,0xA5     ; signiture bytes
.db     254,'!',0,0             ; id (254=cmd)
.db     0,0,0,0                 ; prompt code vector
.db     0,0,0,0                 ; reserved
.db     0,0,0,0                 ; reserved
.db     0,0,0,0                 ; reserved
.db     0,0,0,0                 ; user defined
.db     255,255,255,255         ; length and checksum (255=unused)
.db     "Setup",0
.org    locat+0x1440            ; executable code begins here


; # Menu
; #######
setup:
	push	stack_carry							; Save the use_oysterlib flag
	jb	use_oysterlib, setup_menu_start
	setb	use_oysterlib
	lcall	lcd_init

setup_menu_start:
	lcall	lcd_clear_screen
	mov	tmp_var, #0

	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x07					; 1st row, 5th column
	mov	dptr, #str_setup_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
setup_menu_loop:

	clr	lcd_glyph_invert
	mov	a, tmp_var
	cjne	a, #0, setup_menu_loop_select0
	setb	lcd_glyph_invert
setup_menu_loop_select0:
	mov	lcd_start_position, #0x65					; 4th row, 6th column
	mov	dptr, #str_setup_screenkey
	lcall	lcd_pstr

	clr	lcd_glyph_invert
	mov	a, tmp_var
	cjne	a, #1, setup_menu_loop_select1
	setb	lcd_glyph_invert
setup_menu_loop_select1:
	mov	lcd_start_position, #0x86					; 5th row, 7th column
	mov	dptr, #str_setup_datetime
	lcall	lcd_pstr

	clr	lcd_glyph_invert
	mov	a, tmp_var
	cjne	a, #2, setup_menu_loop_select2
	setb	lcd_glyph_invert
setup_menu_loop_select2:
	mov	lcd_start_position, #0xa4					; 6th row, 5th column
	mov	dptr, #str_setup_serial
	lcall	lcd_pstr

	clr	lcd_glyph_invert
	mov	a, tmp_var
	cjne	a, #3, setup_menu_loop_select3
	setb	lcd_glyph_invert
setup_menu_loop_select3:
	mov	lcd_start_position, #0xc7					; 6th row, 8th column
	mov	dptr, #str_setup_power
	lcall	lcd_pstr

setup_menu_loop_keyboard_scan:
	lcall	keyboard_scan
	jnb	keyboard_new_char, setup_menu_loop_keyboard_scan
	clr	keyboard_new_char

	mov	a, keycode_raw
setup_menu_loop_keyboard_scan_up:
	cjne	a, #0x05, setup_menu_loop_keyboard_scan_down
	mov	a, tmp_var
	jz	setup_menu_loop
	dec	a
	mov	tmp_var, a
	sjmp	setup_menu_loop
setup_menu_loop_keyboard_scan_down:
	cjne	a, #0x06, setup_menu_loop_keyboard_scan_enter
	mov	a, tmp_var
	inc	a
	mov	tmp_var, a
	jnb	acc.2, setup_menu_loop
	mov	tmp_var, #0x03
	sjmp	setup_menu_loop
setup_menu_loop_keyboard_scan_enter:
	cjne	a, #0x2F, setup_menu_loop_keyboard_scan_cancel
	mov	a, tmp_var
setup_menu_loop_keyboard_scan_enter_screenkey:
	cjne	a, #0x00, setup_menu_loop_keyboard_scan_enter_datetime
	acall	setup_screenkey
	ajmp	setup_menu_start
setup_menu_loop_keyboard_scan_enter_datetime:
	cjne	a, #0x01, setup_menu_loop_keyboard_scan_enter_serial
	acall	setup_datetime
	ajmp	setup_menu_start
setup_menu_loop_keyboard_scan_enter_serial:
	cjne	a, #0x02, setup_menu_loop_keyboard_scan_enter_power
	acall	setup_serial
	ajmp	setup_menu_start
setup_menu_loop_keyboard_scan_enter_power:
	cjne	a, #0x03, setup_menu_loop_keyboard_scan_enter_other
	acall	setup_power
	ajmp	setup_menu_start
setup_menu_loop_keyboard_scan_enter_other:
	ajmp	setup_menu_loop
setup_menu_loop_keyboard_scan_cancel:
	cjne	a, #0x07, setup_menu_loop_keyboard_scan
	lcall	system_config_save						; Calculate checksums and save config to RTC memory

	pop	acc								; Get the use_oysterlib flag
	mov	c, acc.0							; Restore use_oysterlib
	mov	use_oysterlib, c
	jb	use_oysterlib, setup_menu_finish
	lcall	lcd_off
	ret
setup_menu_finish:
	lcall	lcd_clear_screen
	ret

; # Screen/Keyboard
; ##################
setup_screenkey:
	lcall	lcd_clear_screen
	mov	tmp_var, #0

	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x00					; 1st row, 1st column
	mov	dptr, #str_setupsk_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	clr	lcd_glyph_invert

	mov	lcd_start_position, #0x64					; 3rd row, 5th column
	mov	dptr, #str_setupsk_contrast
	lcall	lcd_pstr

	mov	lcd_start_position, #0x84					; 4th row, 5th column
	mov	dptr, #str_setupsk_backlight
	lcall	lcd_pstr

	mov	lcd_start_position, #0xA4					; 5th row, 5th column
	mov	dptr, #str_setupsk_keyclick
	lcall	lcd_pstr

setup_screenkey_keyboard_scan:
	lcall	keyboard_scan
	jnb	keyboard_new_char, setup_screenkey_keyboard_scan
	clr	keyboard_new_char

	mov	a, keycode_raw
setup_screenkey_keyboard_scan_up:
	cjne	a, #0x05, setup_screenkey_keyboard_scan_down
	lcall	lcd_set_contrast_inc
	sjmp	setup_screenkey_keyboard_scan
setup_screenkey_keyboard_scan_down:
	cjne	a, #0x06, setup_screenkey_keyboard_scan_b
	lcall	lcd_set_contrast_dec
	sjmp	setup_screenkey_keyboard_scan
setup_screenkey_keyboard_scan_b:
	cjne	a, #0x08, setup_screenkey_keyboard_scan_c
	lcall	lcd_set_backlight_toggle
	sjmp	setup_screenkey_keyboard_scan
setup_screenkey_keyboard_scan_c:
	cjne	a, #0x10, setup_screenkey_keyboard_scan_cancel
	lcall	keyboard_click_toggle
	sjmp	setup_screenkey_keyboard_scan
setup_screenkey_keyboard_scan_cancel:
	cjne	a, #0x07, setup_screenkey_keyboard_scan
	ret

; # Date/Time
; ############
setup_datetime:
	orl	psw, #0b00001000						; Select the second register bank
	lcall	lcd_clear_screen

setup_datetime_loop:
	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x00					; 1st row, 1st column
	mov	dptr, #str_setupdt_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	clr	lcd_glyph_invert

	lcall	rtc_get_datetime

	mov	lcd_start_position, #0x68					; 4th row, 9th column
	mov	dptr, #str_setupdt_date
	lcall	lcd_pstr
	mov	a, r3								; Get day
	lcall	print_bcd_digit
	mov	a, #'/'
	lcall	oyster_cout
	mov	a, r4								; Get month
	lcall	print_bcd_digit
	mov	a, #'/'
	lcall	oyster_cout
	mov	a, current_year							; Get the year we think it is
	anl	a, 0xfc								; Lose the last 2 lsbs
	orl	a, r5								; Add 2 lsbs from the RTC
	lcall	print_bcd_digit

	mov	lcd_start_position, #0x88					; 5th row, 9th column
	mov	dptr, #str_setupdt_time
	lcall	lcd_pstr
	mov	a, r2
	lcall	print_bcd_digit
	mov	a, #':'
	lcall	oyster_cout
	mov	a, r1
	lcall	print_bcd_digit
	mov	a, #':'
	lcall	oyster_cout
	mov	a, r0
	lcall	print_bcd_digit

	mov	lcd_start_position, #0xa3					; 6th row, 4th column
	mov	dptr, #str_setupdt_alarm
	lcall	lcd_pstr
	lcall	rtc_get_alarm_config
	mov	dptr, #str_disabled
	jnb	acc.7, setup_datetime_loop_alarm_sprint
	mov	dptr, #str_enabled
setup_datetime_loop_alarm_sprint:
	lcall	lcd_pstr

	lcall	rtc_get_alarm_datetime

	mov	lcd_start_position, #0xc8					; 7th row, 9th column
	mov	dptr, #str_setupdt_adate
	lcall	lcd_pstr
	mov	a, r3								; Get day
	lcall	print_bcd_digit
	mov	a, #'/'
	lcall	oyster_cout
	mov	a, r4								; Get month
	lcall	print_bcd_digit
	mov	a, #'/'
	lcall	oyster_cout
	mov	a, current_year							; Get the year we think it is
	anl	a, 0xfc								; Lose the last 2 lsbs
	orl	a, r5								; Add 2 lsbs from the RTC
	lcall	print_bcd_digit

	mov	lcd_start_position, #0xe8					; 8th row, 9th column
	mov	dptr, #str_setupdt_atime
	lcall	lcd_pstr
	mov	a, r2
	lcall	print_bcd_digit
	mov	a, #':'
	lcall	oyster_cout
	mov	a, r1
	lcall	print_bcd_digit
	mov	a, #':'
	lcall	oyster_cout
	mov	a, r0
	lcall	print_bcd_digit

setup_datetime_keyboard_scan:
	lcall	keyboard_scan
	jb	keyboard_new_char, setup_datetime_keyboard_scan_new_char
	ajmp	setup_datetime_loop
setup_datetime_keyboard_scan_new_char:
	clr	keyboard_new_char

	mov	a, keycode_raw
;setup_datetime_keyboard_scan_b:
;	cjne	a, #0x08, setup_datetime_keyboard_scan_m
setup_datetime_keyboard_scan_cancel:
	cjne	a, #0x07, setup_datetime_keyboard_scan_other
	anl	psw, #0b11100111						; Return to 1st register bank
	ret
setup_datetime_keyboard_scan_other:
	ajmp	setup_datetime_loop

; # Serial
; #########
setup_serial:
	lcall	lcd_clear_screen
	mov	tmp_var, #0x00

setup_serial_loop:
	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x00					; 1st row, 1st column
	mov	dptr, #str_setupsrl_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	clr	lcd_glyph_invert

	mov	lcd_start_position, #0x67					; 4th row, 8th column
	mov	dptr, #str_setupsrl_mport
	lcall	lcd_pstr
	mov	a, sys_props_save
	anl	a, #0x02
	mov	dptr, #str_disabled
	jz	setup_serial_loop_print_mpstate
	mov	dptr, #str_enabled
setup_serial_loop_print_mpstate:
	lcall	lcd_pstr

	mov	lcd_start_position, #0x87					; 5th row, 8th column
	mov	dptr, #str_setupsrl_baud
	lcall	lcd_pstr
	mov	tmp_var, #0xff							; Baud rate pointer (setup so it rolls over)
	mov	dptr, #baud_rate_table
setup_serial_loop_baudrate_identify:
	inc	tmp_var
	mov	a, tmp_var							; Get current offset
	movc	a, @a+dptr
	subb	a, baud_save+3							; Subtract current baud rate to test if we have a match
	jnz	setup_serial_loop_baudrate_identify
	mov	a, tmp_var
	mov	b, #0x06							; Size of baud strings
	mul	ab
	mov	dptr, #baud_rate_str_table
	add	a, dpl								; Add calculated offset to pointer
	mov	dpl, a
	lcall	lcd_pstr

setup_serial_keyboard_scan:
	lcall	keyboard_scan
	jnb	keyboard_new_char, setup_serial_loop
	clr	keyboard_new_char

	mov	a, keycode_raw
setup_serial_keyboard_scan_b:
	cjne	a, #0x08, setup_serial_keyboard_scan_m
	inc	tmp_var								; Increment baud offset
	mov	a, tmp_var
	cjne	a, #0x08, setup_serial_keyboard_scan_b_set_baud			; Make sure we haven't overrun
	mov	tmp_var, #0x00							; Reset offset
setup_serial_keyboard_scan_b_set_baud:
	mov	a, tmp_var
	mov	dptr, #baud_rate_table
	movc	a, @a+dptr							; Get new baud rate
	lcall	serial_baudsave_set_reload					; Set new baud rate
	sjmp	setup_serial_loop
setup_serial_keyboard_scan_m:
	cjne	a, #0x12, setup_serial_keyboard_scan_cancel
	mov	a, sys_props_save						; Get saved bits
	mov	c, acc.1							; Get the bit we're interested in
	cpl	c								; Invert it
	anl	sys_props_save, #0xfd						; Clear the current setting
	jnc	setup_serial_keyboard_scan_m_set_mpstate
	orl	sys_props_save, #0x02						; Set bit
setup_serial_keyboard_scan_m_set_mpstate:
	lcall	serial_mainport_set_state
	ajmp	setup_serial_loop
setup_serial_keyboard_scan_cancel:
	cjne	a, #0x07, setup_serial_keyboard_scan_other
	ret
setup_serial_keyboard_scan_other:
	ajmp	setup_serial_loop

; # Power
; ########
setup_power:
	lcall	lcd_clear_screen
	mov	tmp_var, #0

setup_power_loop:
	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x00					; 1st row, 1st column
	mov	dptr, #str_setuppwr_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	clr	lcd_glyph_invert

	mov	lcd_start_position, #0x64					; 4th row, 5th column
	mov	dptr, #str_setuppwr_cstatus
	lcall	lcd_pstr
	lcall	power_battery_main_check_charging
	mov	dptr, #str_setuppwr_charged
	jnc	setup_power_loop_print_cstatus
	mov	dptr, #str_setuppwr_charging
setup_power_loop_print_cstatus:
	lcall	lcd_pstr

	mov	lcd_start_position, #0x85					; 4th row, 6th column
	mov	dptr, #str_backup
	lcall	lcd_pstr
	mov	dptr, #str_battery
	lcall	lcd_pstr
	lcall	power_battery_backup_check_status
	mov	dptr, #str_fail
	jnc	setup_power_loop_print_bbattery
	mov	dptr, #str_okay
setup_power_loop_print_bbattery:
	lcall	lcd_pstr

	lcall	memory_ramcard_present						; Check if ram card is present
	jnc	setup_power_keyboard_scan
	mov	lcd_start_position, #0x81					; 4th row, 2nd column
	mov	dptr, #str_memory_card
	lcall	lcd_pstr
	mov	dptr, #str_battery
	lcall	lcd_pstr
	lcall	power_battery_ramcard_check_status_warn
	mov	dptr, #str_okay
	jc	setup_power_loop_print_mcbattery
	lcall	power_battery_ramcard_check_status_fail
	mov	dptr, #str_fail
	jnc	setup_power_loop_print_mcbattery
	mov	dptr, #str_warn
setup_power_loop_print_mcbattery:
	lcall	lcd_pstr

setup_power_keyboard_scan:
	lcall	keyboard_scan
	jnb	keyboard_new_char, setup_power_loop
	clr	keyboard_new_char

	mov	a, keycode_raw
setup_power_keyboard_scan_cancel:
	cjne	a, #0x07, setup_power_keyboard_scan_other
	ret
setup_power_keyboard_scan_other:
	ajmp	setup_power_loop
