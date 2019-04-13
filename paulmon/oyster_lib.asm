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
; #	Memory tool
; #	System setup
; #	System init
; ###############################################################################################################

; define the storage location
; ##########################################################################
.equ	locat, 0x1000		;location for the library/commands
.equ	paulmon2, 0x0000	;location where paulmon2
;.equ	locat, 0x9000		;location for the library/commands
;.equ	paulmon2, 0x8000	;location where paulmon2

; These equ lines allow us to call functions within PAULMON2
; by their names.
; ##########################################################################
.equ	cout, 0x30+paulmon2					; Print Acc to Serial Port
.equ    Cin, 0x32+paulmon2					; Get Acc from serial port
.equ    pHex, 0x34+paulmon2					; Print Hex value of Acc
.equ    pHex16, 0x36+paulmon2					; Print Hex value of DPTR
.equ    pStr, 0x38+paulmon2					; Print string pointed to by DPTR,
								;  must be terminated by 0 or a high bit set
								;  pressing ESC will stop the printing
.equ    gHex, 0x3A+paulmon2					; Get Hex input into Acc
								;  carry set if ESC has been pressed
.equ    gHex16, 0x3C+paulmon2					; Get Hex input into DPTR
								;  carry set if ESC has been pressed
.equ    ESC, 0x3E+paulmon2					; Check for ESC key
								;  carry set if ESC has been pressed
.equ    Upper, 0x40+paulmon2					; Convert Acc to uppercase
								;  non-ASCII values are unchanged
.equ    Init, 0x42+paulmon2					; Initialize serial port
.equ    newline, 0x48+paulmon2					; Print CR/LF (13 and 10)
.equ    lenstr, 0x4A+paulmon2					; Return the length of a string @DPTR (in R0)
.equ    pint8u, 0x4D+paulmon2					; Print Acc at an integer, 0 to 255
.equ    pint8, 0x50+paulmon2					; Print Acc at an integer, -128 to 127
.equ    pint16u, 0x53+paulmon2					; Print DPTR as an integer, 0 to 65535

; RAM definitions
; ##########################################################################
.equ	rb0r1, 0x01
.equ	rb0r2, 0x02

.equ	lcd_font_table_msb, 0x10				; MSB of the address of the font table
.equ	lcd_scroll_offset, 0x11					; LCD start draw line
.equ	lcd_start_position, 0x12				; Start position for LCD operations (b0-b4 - column, b5-7 - row)
.equ	lcd_subscreen_row, 0x13					; Current subscreen row
.equ	lcd_subscreen_column, 0x14				; Current subscreen column
.equ	lcd_properties, 0x15					; Current contrast/backlight value
.equ	keycode_raw, 0x16					; Raw keycode, read from hardware
.equ	keycode_ascii, 0x17					; Processed raw keycode to ASCII keymap
.equ	mem_page_psen, 0x18					; Store for the currently selected PSEN page
.equ	mem_page_rdwr, 0x19					; Store for the currently selected RDWR page
.equ	mem_mode, 0x27						; Bit addressable, memory mode store
.equ	stack_carry, 0x2f					; Save Carry state here to load on the stack

; Bit RAM definitions
; ##########################################################################
.flag	i2c_scl, p1.6
.flag	i2c_sda, p1.7
.flag	lcd_glyph_doublewidth, 0x25.0
.flag	lcd_glyph_doubleheight, 0x25.1
.flag	lcd_glyph_doubleheight_otherhalf, 0x25.2
.flag	lcd_glyph_invert, 0x25.3
.flag	lcd_no_scroll, 0x25.4
.flag	lcd_glyph_use_ram, 0x25.7
.flag	keyboard_key_down, 0x26.0
.flag	keyboard_new_char, 0x26.1
.flag	mem_mode_psen_ram, 0x27.2				; These bits are chosen to make text translation easier
.flag	mem_mode_psen_ram_card, 0x27.3				; See memory command to see how text is selected using them
.flag	mem_mode_rdwr_ram, 0x27.6				; See above
.flag	mem_mode_rdwr_ram_card, 0x27.7				; See above
.flag	use_oysterlib, 0x2f.0
.flag	tmp_bit, 0x2f.6
.flag	stack_carry_bit, 0x2f.7

; SFR definitions
; ##########################################################################
.equ	ie2, 0xe8						; 2nd interrupt control register
.equ	p4, 0xc0						; Port 4
.equ	p5, 0xc4						; Port 5
.equ	pwm1, 0xfd						; PWM1 control
.equ	pwmp, 0xfe						; PWM prescaler
.equ	t3, 0xff						; Timer3 control


.org	locat
; ###############################################################################################################
; #                                                  Library jump table
; ###############################################################################################################
; Not needed for commands in this file, but to allow other commands to access functions

; console functions
	ajmp	oyster_cout
	ajmp	oyster_newline
	ajmp	oyster_cin
; math library functions
	ajmp	packed_bcd_to_hex
	ajmp	hex_to_packed_bcd
; memory library function
	ajmp	memory_set_page_psen
	ajmp	memory_set_page_rdwr
	ajmp	memory_set_mode_psen
	ajmp	memory_set_mode_rdwr
	ajmp	memory_ramcard_present
; i2c library functions
	ajmp	i2c_start
	ajmp	i2c_stop_clock_stretch_check
	ajmp	i2c_stop
	ajmp	i2c_write_byte
	ajmp	i2c_ack_check
	ajmp	i2c_write_byte_to_addr
	ajmp	i2c_write_byte_with_ack_check
	ajmp	i2c_read_byte
	ajmp	i2c_master_read_ack
	ajmp	i2c_read_byte_from_addr
	ajmp	i2c_read_device_register
; rtc library functions
	ajmp	rtc_get_config
	ajmp	rtc_get_alarm_config
	ajmp	rtc_set_config
	ajmp	rtc_set_alarm_config
	ajmp	rtc_init
	ajmp	rtc_get_datetime
	ajmp	rtc_set_datetime
; keyboard library functions
	ajmp	keyboard_scan
	ajmp	keyboard_reset
	ajmp	keyboard_wait_for_keypress
; lcd library functions
	ajmp	lcd_off
	ajmp	lcd_init
	ajmp	lcd_clear_screen
	ajmp	lcd_clear_screen_from_position
	ajmp	lcd_new_line_scroll_and_clear
	ajmp	lcd_print_character
	ajmp	lcd_set_glyph_position
	ajmp	lcd_pstr
	ajmp	lcd_set_backlight_on
	ajmp	lcd_set_backlight_off
	ajmp	lcd_set_contrast_inc
	ajmp	lcd_set_contrast_dec
; misc library functions
	ajmp	piezo_beep
	ajmp	piezo_pwm_sound
	ajmp	sdelay
	ajmp	battery_check_status


; ###############################################################################################################
; #                               Console library (substitue functions for paulmon functions)
; ###############################################################################################################

; # oyster_cout
; #
; # Equivalent function to the paulmon cout
; ##########################################################################
oyster_cout:
	jnb	use_oysterlib, oyster_cout_serial
	push	acc
	mov	stack_carry_bit, c				; Save Carry to the stack
	push	stack_carry
	setb	psw.3						; Swap register bank (0x08-0x0f)
	acall	lcd_print_character
	clr	psw.3						; Restore register bank
	pop	stack_carry					; Restore Carry
	mov	c, stack_carry_bit
	pop	acc
	ret
oyster_cout_serial:						; Copied from PaulMON
	jnb	ti, oyster_cout_serial
	clr	ti						; clr ti before the mov to sbuf!
	mov	sbuf, a
	ret


; # oyster_newline
; #
; # Equivalent function to the paulmon newline
; ##########################################################################
oyster_newline:
	jnb	use_oysterlib, oyster_newline_serial
	push	acc
	mov	stack_carry_bit, c				; Save Carry to the stack
	push	stack_carry
	setb	psw.3						; Swap register bank (0x08-0x0f)
	acall	lcd_new_line_scroll_and_clear
	clr	psw.3						; Restore register bank
	mov	lcd_start_position, #0xE0			; Reset start position to first character last row
	pop	stack_carry					; Restore Carry
	mov	c, stack_carry_bit
	pop	acc
	ret
oyster_newline_serial:						; Copied from PaulMON
	push	acc						; print one newline
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
	mov	stack_carry_bit, c				; Save Carry to the stack
	push	stack_carry
	setb	psw.3						; Swap register bank (0x08-0x0f)
	acall	keyboard_wait_for_keypress
	clr	psw.3						; Restore register bank
	mov	a, keycode_ascii				; Get ascii code
	clr	keyboard_new_char				; Reset flag
	pop	stack_carry					; Restore Carry
	mov	c, stack_carry_bit
	ret
oyster_cin_serial:						; Copied from PaulMON
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
	mov	r0, #0xff					; We have the upper registers
memory_register_check_loop:
	mov	a, #0xff					; Write 0xff to registers
	mov	@r0, a
	mov	a, @r0
	cjne	a, #0xff, memory_register_check_error
	cpl	a						; Write 0x00 to registers
	mov	@r0, a
	mov	a, @r0
	cjne	a, #0x00, memory_register_check_error
	dec	r0
	cjne	r0, #7, memory_register_check_loop		; Don't clear out the lower registers
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
	mov	a, #0xff					; Write 0xff to RAM
	movx	@dptr, a
	nop							; Wait for data to settle
	movx	a, @dptr
	cjne	a, #0xff, memory_sram_check_error
	cpl	a						; Write 0x00 to RAM
	movx	@dptr, a
	nop							; Wait for data to settle
	movx	a, @dptr
	cjne	a, #0x00, memory_sram_check_error
	inc	dptr
	mov	a, dph
	cjne	a, #0x00, memory_sram_check_loop		; Keep looping until we reach the end of page
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
	setb	p1.4						; Enable address logic
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
	anl	a, #3						; Make sure this is valid
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
	anl	a, #3						; Make sure this is valid
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
	mov	a, p5						; Read Port 5
	clr	c						; No SRAM card
	jnb	acc.3, memory_ramcard_present_finish
	setb	c						; SRAM card present
memory_ramcard_present_finish:
	ret


; # memory_ramcard_write_protect
; #
; # Returns whether the memory card is write protected
; # Out:
; #   Carry - Write protect status
; ##########################################################################
memory_ramcard_write_protect:
	clr	c						; SRAM card not write protected
	jnb	p4.1, memory_ramcard_write_protect_finish
	setb	c						; SRAM card write protected
memory_ramcard_write_protect_finish:
	ret


; # memory_ramcard_battery_check_status
; #
; # Returns a simple okay/fail for memory card battery
; # Out:
; #   Carry - battery status
; ##########################################################################
memory_ramcard_battery_check_status:
	clr	c						; SRAM card battery failure
	jnb	p4.2, memory_ramcard_battery_check_status_finish
	setb	c						; SRAM card battery okay
memory_ramcard_battery_check_status_finish:
	ret


; ###############################################################################################################
; #                                                     I2C library
; ###############################################################################################################

; # i2c_start
; #
; # Set start condition on i2c bus
; ##########################################################################
i2c_start:
	setb	i2c_scl						; Make sure SCL is set
	nop							; NOP - wait for i2c data to 'settle'
	nop							; NOP - wait for i2c data to 'settle'
	clr	i2c_sda						; Clear SDA first
	nop							; NOP - wait for i2c data to 'settle'
	nop							; NOP - wait for i2c data to 'settle'
	clr	i2c_scl						; Clear SCL
	ret


; # i2c_stop_clock_stretch_check
; #
; # Set stop condition on i2c bus after checking for clock
; # stretching.
; ##########################################################################
i2c_stop_clock_stretch_check:
	setb	i2c_scl						; Release SCL
	jnb	i2c_scl, i2c_stop_clock_stretch_check		; Wait for SCL to be released
	nop							; NOP - wait for i2c data to 'settle'
	nop							; NOP - wait for i2c data to 'settle'
; # i2c_stop
; #
; # Set stop condition on i2c bus
; ##########################################################################
i2c_stop:
	mov	r0, #0						; Setup loop count (255)
i2c_stop_loop:
	clr	i2c_scl						; Clear SCL
	clr	i2c_sda						; Clear SDA
	nop							; NOP - wait for i2c data to 'settle'
	nop							; NOP - wait for i2c data to 'settle'
	setb	i2c_scl						; Set SCL
	setb	i2c_sda						; Set SDA
	jb	i2c_sda, i2c_stop_exit				; Has SDA been released?
	djnz	r0, i2c_stop_loop				; It hasn't, so loop
i2c_stop_exit:
	ret


; # i2c_write_byte
; #
; # Writes a byte to the i2c bus
; # A - byte to write out
; ##########################################################################
i2c_write_byte:
	mov	r0, #8						; Move 8 into r0 (number of bits)
	rlc	a						; Rotate A into Carry
	mov	i2c_sda, c					; Set i2c bit data from Carry
i2c_write_byte_scl_wait:
	setb	i2c_scl						; Set SCL to set next bit
	jnb	i2c_scl, i2c_write_byte_scl_wait		; Check for slave clock stetching
	sjmp	i2c_write_byte_rest
i2c_write_byte_rest_loop:
	mov	i2c_sda, c					; Set i2c bit data
	setb	i2c_scl						; Set SCL to set next bit
i2c_write_byte_rest:
	rlc	a						; Get next bit
	nop							; NOP - wait for i2c data to 'settle'
	clr	i2c_scl						; Clear SCL to ready for next bit
	djnz	r0, i2c_write_byte_rest_loop			; Loop through byte
	setb	i2c_sda						; Release i2c data line
	ret


; # i2c_ack_check
; #
; # Check for ACK/NACK, set Carry if NACKed
; ##########################################################################
i2c_ack_check:
	mov	r0, #0						; Setup loop count (255)
i2c_ack_check_loop:
	jnb	i2c_sda, i2c_ack_check_acked			; Check to see if byte ACKed
	djnz	r0, i2c_ack_check_loop				; Keep checking bit
	setb	c						; Indicate NACKed
	setb	i2c_scl
	ret
i2c_ack_check_acked:
	setb	i2c_scl						; Set SCL for ack bit
	nop							; NOP - wait for i2c data to 'settle'
	nop							; NOP - wait for i2c data to 'settle'
	clr	i2c_scl						; Clear SCL to ready for next bit
	clr	c						; Indicate ACK
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
	mov	r0, #8						; Move 8 into r0 (num bits)
	clr	a						; Clear A, will store data here
i2c_read_byte_loop:
	setb	i2c_scl						; Set SCL to get next bit
	nop							; NOP - wait for i2c data to 'settle'
	mov	c, i2c_sda					; Copy i2c bit data into Carry
	clr	i2c_scl						; Clear SCL to ready for next bit
	rlc	a						; Rotate Carry in to A to build byte
	djnz	r0, i2c_read_byte_loop				; Loop to build byte
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
	acall	i2c_write_byte_to_addr				; Write register address to device
	jc	i2c_read_byte_from_addr_rtn			; Check for errors
	acall	i2c_start					; I2C restart
	mov	a, b						; Get i2c address
	inc	a						; Read operation
	acall	i2c_write_byte					; Write i2c address (as read operation)
	acall	i2c_ack_check					; Check for errors
	jc	i2c_read_byte_from_addr_rtn
	ajmp	i2c_read_byte					; Read register contents
	clr	c


; ###############################################################################################################
; #                                                     RTC library
; ###############################################################################################################

.equ	rtc_addr1, 0xa0						; 0x50 (bit shifted)
.equ	rtc_addr2, 0xa2						; 0x51 (bit shifted)

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
	clr	a						; Config/status register
	acall	i2c_write_byte_to_addr				; Select register
	jnc	rtc_get_config_restart				; Check for errors
	ajmp	i2c_stop
rtc_get_config_restart:
	acall	i2c_start					; I2C restart
	mov	a, b						; Get i2c address
	inc	a						; Read operation
	acall	i2c_write_byte					; Re-open connection to RTC device
	acall	i2c_ack_check					; Check for errors
	jnc	rtc_get_config_read
	ajmp	i2c_stop
rtc_get_config_read:
	acall	i2c_read_byte					; Get config/status
	acall	i2c_master_read_ack				; ACK read
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
	mov	a, #8						; Alarm control register
	acall	i2c_write_byte_to_addr				; Select register
	jnc	rtc_get_config_restart				; Check for errors
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
	push	acc						; Save config
	clr	a						; Control/status register
	acall	i2c_write_byte_to_addr				; Select register
	jnc	rtc_set_config_write
	pop	acc						; Pop this so the stack is sound
	ajmp	i2c_stop
rtc_set_config_write:
	pop	acc						; Get config
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
	push	acc						; Save config
	mov	a, #8						; Alarm control register
	acall	i2c_write_byte_to_addr				; Select register
	jnc	rtc_set_config_write
	pop	acc						; Pop this so the stack is sound
	ajmp	i2c_stop


; # rtc_init
; #
; # Basic config of the 2 RTC devices
; # Out:
; #   Carry - error
; ##########################################################################
rtc_init:
	mov	a, #4						; 0b00000100 - Clock mode 32.768 kHz, alarm enabled
	mov	b, #rtc_addr1
	acall	rtc_set_config					; Config RTC1
	jc	rtc_init_finish					; Exit on error
	mov	a, #5						; Timer function: days, Alarm/Timer flag: no iterrupt, no timer alarm, no clock alarm
	mov	b, #rtc_addr1
	acall	rtc_set_alarm_config				; Config RTC1 alarm
	jc	rtc_init_finish					; Exit on error
	mov	a, #20h						; 0b00100000 - Event counter mode, alarm registers disable
	mov	b, #rtc_addr2
	acall	rtc_set_config					; Config RTC2
	jc	rtc_init_finish					; Exit on error
	clr	c
rtc_init_finish:
	ret


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
	mov	a, #2						; Seconds register
	mov	b, #rtc_addr1
	acall	i2c_read_device_register			; Get value
	jc	rtc_get_datetime_finish
	acall	i2c_master_read_ack
	acall	packed_bcd_to_hex				; Convert for system use
	push	acc						; Store seconds for later (r0 used by i2c_*)
	acall	i2c_read_byte					; Get value
	acall	i2c_master_read_ack
	acall	packed_bcd_to_hex				; Convert for system use
	mov	r1, a						; Store minutes
	acall	i2c_read_byte					; Get value
	acall	i2c_master_read_ack
	anl	a, #3fh						; Make sure valid value (24h clock)
	acall	packed_bcd_to_hex				; Convert for system use
	mov	r2, a						; Store hours
	acall	i2c_read_byte					; Get value
	acall	i2c_master_read_ack
	push	acc						; Store for later
	anl	a,#3fh						; Lose year data
	acall	packed_bcd_to_hex				; Convert for system use
	mov	r3, a						; Store day of the month
	pop	acc						; Get original value
	rl	a
	rl	a
	anl	a, #3						; Get year data
	mov	r5, a						; Store year
	acall	i2c_read_byte					; Get value
	anl	a, #1fh						; Lose weekday data
	acall	packed_bcd_to_hex				; Convert for system use
	mov	r4, a						; Store month
	clr	c
rtc_get_datetime_finish:
	acall	i2c_stop_clock_stretch_check
	pop	acc
	mov	r0, a
	ret

; # rtc_set_datetime
; #
; # Set the datetime of the RTC.
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
	mov	a, r0						; Save for later (r0 used by i2c_*)
	push	acc
	mov	a, #1						; Hundreths of a second
	mov	b, #rtc_addr1
	acall	i2c_write_byte_to_addr				; Select register
	jc	rtc_set_datetime_finish
	mov	a, #0						; Just zero hundreths of a second
	acall	i2c_write_byte_with_ack_check			; Load hundreths of a second
	jc	rtc_set_datetime_finish
	pop	acc
	anl	a, #3fh						; Make the value reasonable (if not correct)
	acall	hex_to_packed_bcd				; Convert for RTC register
	acall	i2c_write_byte_with_ack_check			; Load seconds
	jc	rtc_set_datetime_finish
	mov	a, r1
	anl	a, #3fh						; Make the value reasonable (if not correct)
	acall	hex_to_packed_bcd				; Convert for RTC register
	acall	i2c_write_byte_with_ack_check			; Load minutes
	jc	rtc_set_datetime_finish
	mov	a, r2
	anl	a, #1fh						; Make the value reasonable (if not correct)
	acall	hex_to_packed_bcd				; Convert for RTC register
	acall	i2c_write_byte_with_ack_check			; Load hours
	jc	rtc_set_datetime_finish
	mov	a, r3
	anl	a, #1fh						; Make the value reasonable (if not correct)
	acall	hex_to_packed_bcd				; Convert for RTC register
	anl	a, #3fh						; Make sure valid value
	mov	r0, a						; Store for later
	mov	a, r5
	anl	a, #3						; Make sure valid value
	rr	a
	rr	a						; Shift to correct register position
	orl	a, r0						; Combine with day of the month
	acall	i2c_write_byte_with_ack_check			; Load value
	jc	rtc_set_datetime_finish
	mov	a, r4
	anl	a, #0fh						; Make the value reasonable (if not correct)
	acall	hex_to_packed_bcd				; Convert for RTC register
	acall	i2c_write_byte_with_ack_check			; Load month
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
	mov	keycode_raw, #0xff						; Clear buffer (0 not used as it's a valid keycode)

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
	cjne	a, keycode_raw, keyboard_read_key_make_keycode_update		; Update if different key
	sjmp	keyboard_reset							; Otherwise ignore this result
keyboard_read_key_make_keycode_update:
	mov	keycode_raw, a							; Save result (0bxxCCCRRR)
	mov	dptr, #keycode_2_character_table1				; Get conversion table
	movc	a, @a+dptr							; Get the corresponding character for the raw code
	mov	keycode_ascii, a						; Store for processing
	setb	keyboard_new_char						; Set flags indicating new character
	setb	keyboard_key_down						;
	sjmp	keyboard_reset


; # keyboard_wait_for_keypress
; #
; # Waits for a key to be pressed, then returns
; ##########################################################################
keyboard_wait_for_keypress:
	acall	keyboard_scan							; Scan for key press
	jnb	keyboard_new_char, keyboard_wait_for_keypress			; Wait until there is one
	jb	keyboard_key_down, keyboard_wait_for_keypress			; Wait for key to be released
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
	setb	p1.4						; Enable clock
	mov	dph, #0x80					; Select LCD (command register)
lcd_off_subscreen:
	mov	a, #0x3e					; LCD command 'Turn off'
	lcall	lcd_send_data					; Send command
	mov	a, dph
	add	a, #0x10					; Select next subscreen
	mov	dph, a
	cjne	a, #0xb0, lcd_off_subscreen			; Disable next subscreen
	clr	p1.4						; Disable clock
	ret


; # lcd_init
; #
; # Initialise the LCD, clear the memory
; ##########################################################################
lcd_init:
	setb	p1.4						; Enable clock
	mov	dph, #0x80					; Select LCD (command register)
lcd_init_subscreen:
	mov	a, #0x3f					; LCD command 'Turn on'
	lcall	lcd_send_data					; Send command
	mov	a, #0xc0					; LCD command 'Display start line' #0
	lcall	lcd_send_data					; Send command
	mov	a, dph
	add	a, #0x10					; Select next subscreen
	mov	dph, a
	cjne	a, #0xb0, lcd_init_subscreen			; Initialise next subscreen
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
	push	dph						; Save registers
	push	b

	setb	p1.4						; Enable clock
	mov	a, lcd_start_position				; Get position to clear from (b0-b4 - column, b5-7 - row)
	anl	a, #0x1f					; Get column data
	mov	b, #6						; Glyph block size
	mul	ab						; Calculate column offset
	push	acc						; Save result
	anl	a, #0xc0					; Get subscreen selection
	rr	a
	rr	a						; Shift into correct bit position
	orl	a, #0x80
	mov	dph, a						; LCD subscreen address
	pop	acc
	anl	a, #0x3f					; Subscreen column position
	mov	lcd_subscreen_column, a
	orl	a, #0x40					; LCD command 'Set Address'
	lcall	lcd_send_data					; Send command
	mov	a, lcd_start_position				; Get position to clear from (b0-b4 - column, b5-7 - row)
	anl	a, #0xe0					; Get row data
	swap	a
	rr	a						; Equivalent to right shift x5
	mov	b, a
	mov	a, lcd_scroll_offset
	rl	a
	swap	a
	add	a, b
	anl	a, #7
	mov	lcd_subscreen_row, a
	orl	a, #0b8h					; LCD command 'Set Page'
	lcall	lcd_send_data					; Send command
	orl	dph, #0x40					; Select LCD (data register)
	mov	a, #0x40
	clr	c
	subb	a, lcd_subscreen_column				; Calculate remaining columns
	mov	lcd_subscreen_column, a
	mov	a, #0
lcd_clear_subscreen_remaining_line:
	lcall	lcd_send_data					; Send command
	djnz	lcd_subscreen_column, lcd_clear_subscreen_remaining_line
lcd_clear_subscreen_check_next:
	anl	dph, #0xbf					; Swap to LCD command register
	mov	a, dph
	add	a, #0x10					; Increment subscreen
	mov	dph, a
	cjne	a, #0xb0, lcd_clear_subscreen_next_start	; Check whether there are subscreens remaining for this line
	mov	a, lcd_subscreen_row
	inc	a						; Increment subscreen row selection
	anl	a, #7
	mov	lcd_subscreen_row, a				; Store incremented subscreen row selection
	mov	dph, #0x80					; Select LCD command register
	mov	a, lcd_start_position
	add	a, #0x20					; Increment stored position (sets carry on screen end)
	jc	lcd_clear_exit
	mov	lcd_start_position, a
lcd_clear_subscreen_next_start:
	mov	a, #0x40					; LCD command 'Set Address' #0
	lcall	lcd_send_data					; Send command
	mov	a, lcd_subscreen_row				; Get subscreen row selection
	orl	a, #0xb8					; LCD command 'Set Page'
	lcall	lcd_send_data					; Send command
	orl	dph, #0x40					; Swap to LCD data register
	mov	lcd_subscreen_column, #0x40			; Number of characters to clear (64 - full line)
	clr	a
lcd_clear_subscreen_remaining_line2:
	lcall	lcd_send_data
	djnz	lcd_subscreen_column, lcd_clear_subscreen_remaining_line2
	sjmp	lcd_clear_subscreen_check_next
lcd_clear_exit:
	clr	p1.4						; Disable clock
	pop	b						; Restore registers
	pop	dph
	ret


; # lcd_new_line_scroll_and_clear
; #
; # lcd_scroll_offset - current line that the lcd starts on
; ##########################################################################
lcd_new_line_scroll_and_clear:
	push	dph						; Save registers

	mov	a, lcd_scroll_offset				; Get current lcd display start line
	add	a, #8						; Move the display up 8 lines
	anl	a, #3fh						; Wrap on >=64
	mov	lcd_scroll_offset, a				; Save result
	setb	p1.4						; Enable clock
	mov	dph, #0x80					; Select LCD command register subscreen 0
lcd_new_line_scroll_and_clear_loop:
	mov	a, lcd_scroll_offset
	orl	a, #0xc0					; LCD command 'display start line'
	lcall	lcd_send_data					; Send command
	mov	a, dph						; Get currently selected subscreen
	add	a, #0x10					; Select next subscreen
	mov	dph, a						; Address subscreen
	cjne	a, #0xb0, lcd_new_line_scroll_and_clear_loop	; Loop until we have iterrated over all subscreens
	mov	r0, lcd_start_position
	mov	lcd_start_position, #0xe0			; Last character line, first character
	lcall	lcd_clear_screen_from_position			; Clear last character line of the lcd
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
	pop	acc
	ret


; # lcd_set_properties
; #
; # Updates the backlight/contrast of the LCD display
; # Latch @00xx + p1.4
; # 0bxxxbcccc - b=backlight, c=contrast
; # In:
; #   A - LCD properties value
; ##########################################################################
lcd_set_properties:
	mov	lcd_properties, a
	setb	p1.4
	mov	dph, #0
	movx	@dptr, a
	clr	p1.4
	ret


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
	mov	a, lcd_properties				; Get current properties
	anl	a, #0xef					; Remove current backlight value
	jnc	lcd_set_backlight_value				; If not set, just write value
	setb	acc.4						; Set backlight bit
lcd_set_backlight_value:
	acall	lcd_set_properties
lcd_set_backlight_finish:
	ret


; # lcd_set_contrast_inc
; #
; # Increase the contrast of the LCD
; ##########################################################################
lcd_set_contrast_inc:
	mov	a, lcd_properties				; Get current value
	anl	a, #0x0f					; Get just the contrast
	inc	a						; Increment contrast value
	jb	acc.4, lcd_set_contrast_finish			; We're already at maximum contrast
	sjmp	lcd_set_contrast
; # lcd_set_contrast_dec
; #
; # Decrease the contrast of the LCD
; ##########################################################################
lcd_set_contrast_dec:
	mov	a, lcd_properties				; Get current value
	anl	a, #0x0f					; Get just the contrast
	jz	lcd_set_contrast_finish				; Finish if already zero
	dec	a						; Decrease contrast value
; # lcd_set_contrast
; #
; # Set the contrast of the LCD
; # In:
; #   A - LCD contrast
; ##########################################################################
lcd_set_contrast:
	anl	a, #0x0f					; Make sure value valid
	anl	lcd_properties, #0xf0				; Remove current contrast value
	orl	a, lcd_properties				; Merge other properties with new contrast value
	acall	lcd_set_properties				; Set new value
lcd_set_contrast_finish:
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
        lcall   piezo_pwm_sound                                 ; Set tone
        mov     a, b
        lcall   sdelay
        mov     a, #255                                         ; Turn off
        lcall   piezo_pwm_sound                                 ; Set tone
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
	mov	pwm1, #0
	ret
piezo_pwm_sound_lookup:
	anl	a, #15
	mov	dptr, #piezo_pwm_table
	movc	a, @a+dptr
	mov	pwmp, a
	mov	pwm1, #128
	ret

piezo_pwm_table:
	.db	143						; 100 Hz
	.db	71						; 200 Hz
	.db	47						; 301 Hz
	.db	35						; 401 Hz
	.db	28						; 498 Hz
	.db	23						; 602 Hz
	.db	20						; 688 Hz
	.db	17						; 803 Hz
	.db	15						; 903 Hz
	.db	13						; 1032 Hz
	.db	9						; 1445 Hz
	.db	6						; 2065 Hz
	.db	4						; 2891 Hz
	.db	3						; 3614 Hz
	.db	1						; 7228 Hz
	.db	0						; 14456 Hz


; # sdelay
; #
; # Software delay
; # Crystal frequency 7.3728 Mhz - instruction cycle = 12/7372800
; # In:
; #   A - x50 milliseconds to delay
; ##########################################################################
sdelay:
	mov	r2, a
sdelay_50ms:							; 50ms delay
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
	push	acc						; Save cursor count
	mov	a, #esc_char					; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'A'
	ljmp	cout
terminal_esc_cursor_down:
	push	acc						; Save cursor count
	mov	a, #esc_char					; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'B'
	ljmp	cout
terminal_esc_cursor_left:
	push	acc						; Save cursor count
	mov	a, #esc_char					; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'D'
	ljmp	cout
terminal_esc_cursor_right:
	push	acc						; Save cursor count
	mov	a, #esc_char					; Send escape character
	lcall	cout
	mov	a, #'['
	lcall	cout
	pop	acc
	lcall	pint8u
	mov	a, #'C'
	ljmp	cout

.equ	esc_char, 0x1b


; # battery_check_status
; #
; # Returns a simple okay/fail for the main battery
; # Out:
; #   Carry - battery status
; ##########################################################################
battery_check_status:
	mov	a, p5						; Read Port 5
	clr	c						; Battery low
	jnb	acc.1, battery_check_status_finish
	setb	c						; Battery okay
battery_check_status_finish:
	ret

; ###############################################################################################################
; #                                                     General data
; ###############################################################################################################

.org    locat+0x700

ascii_font_table:
ascii_font_table_char_0:					; Null
	.db	0x41, 0x41, 0x41, 0x41, 0x41, 0x00
ascii_font_table_char_1:					; 
	.db	0x40, 0x5e, 0x45, 0x45, 0x5e, 0x00
ascii_font_table_char_2:					; 
	.db	0x00, 0x7f, 0x3e, 0x1c, 0x08, 0x00
ascii_font_table_char_3:					; 
	.db	0x08, 0x1c, 0x3e, 0x7f, 0x00, 0x00
ascii_font_table_char_4:					; 
	.db	0x40, 0x5f, 0x51, 0x51, 0x4e, 0x00
ascii_font_table_char_5:					; 
	.db	0x40, 0x5f, 0x55, 0x55, 0x51, 0x00
ascii_font_table_char_6:					; 
	.db	0x40, 0x5f, 0x45, 0x45, 0x41, 0x00
ascii_font_table_char_7:					; Bell
	.db	0x48, 0x4e, 0x51, 0x4e, 0x48, 0x00
ascii_font_table_char_8:					; 
	.db	0x08, 0x1c, 0x2a, 0x08, 0x08, 0x00
ascii_font_table_char_9:					; 
	.db	0x08, 0x08, 0x2a, 0x1c, 0x08, 0x00
ascii_font_table_char_10:					; 
	.db	0x10, 0x20, 0x7f, 0x20, 0x10, 0x00
ascii_font_table_char_11:					; 
	.db	0x04, 0x02, 0x7f, 0x02, 0x04, 0x00
ascii_font_table_char_12:					; 
	.db	0x40, 0x40, 0x44, 0x40, 0x40, 0x00
ascii_font_table_char_13:					; 
	.db	0x7f, 0x77, 0x63, 0x49, 0x7f, 0x00
ascii_font_table_char_14:					; 
	.db	0x24, 0x12, 0x09, 0x12, 0x24, 0x00
ascii_font_table_char_15:					; 
	.db	0x09, 0x12, 0x24, 0x12, 0x09, 0x00
ascii_font_table_char_16:					; 
	.db	0x4e, 0x59, 0x55, 0x53, 0x4e, 0x00
ascii_font_table_char_17:					; 
	.db	0x40, 0x52, 0x5f, 0x50, 0x40, 0x00
ascii_font_table_char_18:					; 
	.db	0x40, 0x59, 0x55, 0x52, 0x40, 0x00
ascii_font_table_char_19:					; 
	.db	0x40, 0x55, 0x55, 0x4e, 0x40, 0x00
ascii_font_table_char_20:					; 
	.db	0x40, 0x4c, 0x4a, 0x5f, 0x48, 0x00
ascii_font_table_char_21:					; 
	.db	0x40, 0x4f, 0x50, 0x50, 0x4f, 0x00
ascii_font_table_char_22:					; 
	.db	0x43, 0x4c, 0x50, 0x4c, 0x43, 0x00
ascii_font_table_char_23:					; 
	.db	0x4f, 0x50, 0x4c, 0x50, 0x4f, 0x00
ascii_font_table_char_24:					; 
	.db	0x51, 0x4a, 0x44, 0x4a, 0x51, 0x00
ascii_font_table_char_25:					; 
	.db	0x41, 0x42, 0x5c, 0x42, 0x41, 0x00
ascii_font_table_char_26:					; 
	.db	0x51, 0x59, 0x55, 0x53, 0x51, 0x00
ascii_font_table_char_27:					; 
	.db	0x7f, 0x41, 0x55, 0x5d, 0x7f, 0x00
ascii_font_table_char_28:					; 
	.db	0x41, 0x42, 0x44, 0x48, 0x50, 0x00
ascii_font_table_char_29:					; 
	.db	0x40, 0x51, 0x51, 0x5f, 0x40, 0x00
ascii_font_table_char_30:					; 
	.db	0x1e, 0x06, 0x0a, 0x12, 0x20, 0x00
ascii_font_table_char_31:					; 
	.db	0x10, 0x38, 0x54, 0x10, 0x1f, 0x00
ascii_font_table_char_32:					; ' '
	.db	0x00, 0x00, 0x00, 0x00, 0x00, 0x00
ascii_font_table_char_33:					; '!'
	.db	0x00, 0x00, 0x5f, 0x00, 0x00, 0x00
ascii_font_table_char_34:					; '"'
	.db	0x00, 0x07, 0x00, 0x07, 0x00, 0x00
ascii_font_table_char_35:					; '#'
	.db	0x14, 0x7f, 0x14, 0x7f, 0x14, 0x00
ascii_font_table_char_36:					; '$'
	.db	0x24, 0x2a, 0x7f, 0x2a, 0x12, 0x00
ascii_font_table_char_37:					; '%'
	.db	0x23, 0x13, 0x08, 0x64, 0x62, 0x00
ascii_font_table_char_38:					; '&'
	.db	0x36, 0x49, 0x56, 0x20, 0x50, 0x00
ascii_font_table_char_39:					; '''
	.db	0x00, 0x00, 0x07, 0x00, 0x00, 0x00
ascii_font_table_char_40:					; '('
	.db	0x1c, 0x22, 0x41, 0x00, 0x00, 0x00
ascii_font_table_char_41:					; ')'
	.db	0x00, 0x00, 0x41, 0x22, 0x1c, 0x00
ascii_font_table_char_42:					; '*'
	.db	0x22, 0x14, 0x7f, 0x14, 0x22, 0x00
ascii_font_table_char_43:					; '+'
	.db	0x08, 0x08, 0x3e, 0x08, 0x08, 0x00
ascii_font_table_char_44:					; ','
	.db	0x00, 0x50, 0x30, 0x00, 0x00, 0x00
ascii_font_table_char_45:					; '-'
	.db	0x08, 0x08, 0x08, 0x08, 0x08, 0x00
ascii_font_table_char_46:					; '.'
	.db	0x00, 0x60, 0x60, 0x00, 0x00, 0x00
ascii_font_table_char_47:					; '/'
	.db	0x20, 0x10, 0x08, 0x04, 0x02, 0x00
ascii_font_table_char_48:					; '0'
	.db	0x3e, 0x51, 0x49, 0x45, 0x3e, 0x00
ascii_font_table_char_49:					; '1'
	.db	0x00, 0x42, 0x7f, 0x40, 0x00, 0x00
ascii_font_table_char_50:					; '2'
	.db	0x62, 0x51, 0x49, 0x49, 0x46, 0x00
ascii_font_table_char_51:					; '3'
	.db	0x21, 0x41, 0x49, 0x4d, 0x33, 0x00
ascii_font_table_char_52:					; '4'
	.db	0x18, 0x14, 0x12, 0x7f, 0x10, 0x00
ascii_font_table_char_53:					; '5'
	.db	0x27, 0x45, 0x45, 0x45, 0x39, 0x00
ascii_font_table_char_54:					; '6'
	.db	0x3c, 0x4a, 0x49, 0x49, 0x31, 0x00
ascii_font_table_char_55:					; '7'
	.db	0x01, 0x71, 0x09, 0x05, 0x03, 0x00
ascii_font_table_char_56:					; '8'
	.db	0x36, 0x49, 0x49, 0x49, 0x36, 0x00
ascii_font_table_char_57:					; '9'
	.db	0x46, 0x49, 0x49, 0x29, 0x1e, 0x00
ascii_font_table_char_58:					; ':'
	.db	0x00, 0x00, 0x14, 0x00, 0x00, 0x00
ascii_font_table_char_59:					; ';'
	.db	0x00, 0x40, 0x34, 0x00, 0x00, 0x00
ascii_font_table_char_60:					; '<'
	.db	0x08, 0x14, 0x22, 0x41, 0x00, 0x00
ascii_font_table_char_61:					; '='
	.db	0x14, 0x14, 0x14, 0x14, 0x14, 0x00
ascii_font_table_char_62:					; '>'
	.db	0x41, 0x22, 0x14, 0x08, 0x00, 0x00
ascii_font_table_char_63:					; '?'
	.db	0x02, 0x01, 0x59, 0x05, 0x02, 0x00
ascii_font_table_char_64:					; '@'
	.db	0x3e, 0x41, 0x5d, 0x59, 0x4e, 0x00
ascii_font_table_char_65:					; 'A'
	.db	0x7c, 0x12, 0x11, 0x12, 0x7c, 0x00
ascii_font_table_char_66:					; 'B'
	.db	0x7f, 0x49, 0x49, 0x49, 0x36, 0x00
ascii_font_table_char_67:					; 'C'
	.db	0x3e, 0x41, 0x41, 0x41, 0x22, 0x00
ascii_font_table_char_68:					; 'D'
	.db	0x7f, 0x41, 0x41, 0x41, 0x3e, 0x00
ascii_font_table_char_69:					; 'E'
	.db	0x7f, 0x49, 0x49, 0x49, 0x41, 0x00
ascii_font_table_char_70:					; 'F'
	.db	0x7f, 0x09, 0x09, 0x09, 0x01, 0x00
ascii_font_table_char_71:					; 'G'
	.db	0x3e, 0x41, 0x41, 0x51, 0x71, 0x00
ascii_font_table_char_72:					; 'H'
	.db	0x7f, 0x08, 0x08, 0x08, 0x7f, 0x00
ascii_font_table_char_73:					; 'I'
	.db	0x00, 0x41, 0x7f, 0x41, 0x00, 0x00
ascii_font_table_char_74:					; 'J'
	.db	0x20, 0x40, 0x40, 0x40, 0x3f, 0x00
ascii_font_table_char_75:					; 'K'
	.db	0x7f, 0x08, 0x14, 0x22, 0x41, 0x00
ascii_font_table_char_76:					; 'L'
	.db	0x7f, 0x40, 0x40, 0x40, 0x40, 0x00
ascii_font_table_char_77:					; 'M'
	.db	0x7f, 0x02, 0x0c, 0x02, 0x7f, 0x00
ascii_font_table_char_78:					; 'N'
	.db	0x7f, 0x04, 0x08, 0x10, 0x7f, 0x00
ascii_font_table_char_79:					; 'O'
	.db	0x3e, 0x41, 0x41, 0x41, 0x3e, 0x00
ascii_font_table_char_80:					; 'P'
	.db	0x7f, 0x09, 0x09, 0x09, 0x06, 0x00
ascii_font_table_char_81:					; 'Q'
	.db	0x3e, 0x41, 0x51, 0x21, 0x5e, 0x00
ascii_font_table_char_82:					; 'R'
	.db	0x7f, 0x09, 0x19, 0x29, 0x46, 0x00
ascii_font_table_char_83:					; 'S'
	.db	0x26, 0x49, 0x49, 0x49, 0x32, 0x00
ascii_font_table_char_84:					; 'T'
	.db	0x01, 0x01, 0x7f, 0x01, 0x01, 0x00
ascii_font_table_char_85:					; 'U'
	.db	0x3f, 0x40, 0x40, 0x40, 0x3f, 0x00
ascii_font_table_char_86:					; 'V'
	.db	0x1f, 0x20, 0x40, 0x20, 0x1f, 0x00
ascii_font_table_char_87:					; 'W'
	.db	0x7f, 0x20, 0x18, 0x20, 0x7f, 0x00
ascii_font_table_char_88:					; 'X'
	.db	0x63, 0x14, 0x08, 0x14, 0x63, 0x00
ascii_font_table_char_89:					; 'Y'
	.db	0x03, 0x04, 0x78, 0x04, 0x03, 0x00
ascii_font_table_char_90:					; 'Z'
	.db	0x61, 0x51, 0x49, 0x45, 0x43, 0x00
ascii_font_table_char_91:					; '['
	.db	0x7f, 0x7f, 0x41, 0x41, 0x41, 0x00
ascii_font_table_char_92:					; '\'
	.db	0x02, 0x04, 0x08, 0x10, 0x20, 0x00
ascii_font_table_char_93:					; ']'
	.db	0x41, 0x41, 0x41, 0x7f, 0x7f, 0x00
ascii_font_table_char_94:					; '^'
	.db	0x10, 0x08, 0x04, 0x08, 0x10, 0x00
ascii_font_table_char_95:					; '_'
	.db	0x40, 0x40, 0x40, 0x40, 0x40, 0x00
ascii_font_table_char_96:					; '`'
	.db	0x00, 0x01, 0x02, 0x04, 0x00, 0x00
ascii_font_table_char_97:					; 'a'
	.db	0x38, 0x44, 0x44, 0x28, 0x7c, 0x00
ascii_font_table_char_98:					; 'b'
	.db	0x7f, 0x28, 0x44, 0x44, 0x38, 0x00
ascii_font_table_char_99:					; 'c'
	.db	0x38, 0x44, 0x44, 0x44, 0x44, 0x00
ascii_font_table_char_100:					; 'd'
	.db	0x38, 0x44, 0x44, 0x28, 0x7f, 0x00
ascii_font_table_char_101:					; 'e'
	.db	0x38, 0x54, 0x54, 0x54, 0x18, 0x00
ascii_font_table_char_102:					; 'f'
	.db	0x00, 0x08, 0x7e, 0x09, 0x00, 0x00
ascii_font_table_char_103:					; 'g'
	.db	0x08, 0x54, 0x54, 0x54, 0x3c, 0x00
ascii_font_table_char_104:					; 'h'
	.db	0x7f, 0x08, 0x04, 0x04, 0x78, 0x00
ascii_font_table_char_105:					; 'i'
	.db	0x00, 0x48, 0x7a, 0x40, 0x00, 0x00
ascii_font_table_char_106:					; 'j'
	.db	0x20, 0x40, 0x3d, 0x00, 0x00, 0x00
ascii_font_table_char_107:					; 'k'
	.db	0x7f, 0x10, 0x18, 0x24, 0x42, 0x00
ascii_font_table_char_108:					; 'l'
	.db	0x00, 0x41, 0x7f, 0x40, 0x00, 0x00
ascii_font_table_char_109:					; 'm'
	.db	0x78, 0x04, 0x18, 0x04, 0x78, 0x00
ascii_font_table_char_110:					; 'n'
	.db	0x78, 0x08, 0x04, 0x04, 0x78, 0x00
ascii_font_table_char_111:					; 'o'
	.db	0x38, 0x44, 0x44, 0x44, 0x38, 0x00
ascii_font_table_char_112:					; 'p'
	.db	0x7c, 0x14, 0x24, 0x24, 0x18, 0x00
ascii_font_table_char_113:					; 'q'
	.db	0x18, 0x24, 0x14, 0x7c, 0x40, 0x00
ascii_font_table_char_114:					; 'r'
	.db	0x7c, 0x08, 0x04, 0x04, 0x00, 0x00
ascii_font_table_char_115:					; 's'
	.db	0x48, 0x54, 0x54, 0x54, 0x24, 0x00
ascii_font_table_char_116:					; 't'
	.db	0x00, 0x04, 0x3f, 0x44, 0x00, 0x00
ascii_font_table_char_117:					; 'u'
	.db	0x3c, 0x40, 0x40, 0x40, 0x7c, 0x00
ascii_font_table_char_118:					; 'v'
	.db	0x0c, 0x30, 0x40, 0x30, 0x0c, 0x00
ascii_font_table_char_119:					; 'w'
	.db	0x3c, 0x40, 0x20, 0x40, 0x3c, 0x00
ascii_font_table_char_120:					; 'x'
	.db	0x44, 0x28, 0x10, 0x28, 0x44, 0x00
ascii_font_table_char_121:					; 'y'
	.db	0x04, 0x48, 0x30, 0x08, 0x04, 0x00
ascii_font_table_char_122:					; 'z'
	.db	0x44, 0x64, 0x54, 0x4c, 0x44, 0x00
ascii_font_table_char_123:					; '{'
	.db	0x08, 0x36, 0x41, 0x00, 0x00, 0x00
ascii_font_table_char_124:					; '|'
	.db	0x00, 0x00, 0x77, 0x00, 0x00, 0x00
ascii_font_table_char_125:					; '}'
	.db	0x00, 0x00, 0x41, 0x36, 0x08, 0x00
ascii_font_table_char_126:					; '~'
	.db	0x10, 0x08, 0x08, 0x08, 0x04, 0x00
ascii_font_table_char_127:					; DEL
	.db	0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x00

glyph_double_size_conversion_table:
	.db	0x00, 0x03, 0x0c, 0x0f, 0x30, 0x33, 0x3c, 0x3f
	.db	0xc0, 0xc3, 0xcc, 0xcf, 0xf0, 0xf3, 0xfc, 0xff

; Normal keymap
keycode_2_character_table1:
	.db	'A'	; A
	.db	'E'	; E
	.db	'K'	; K
	.db	'Q'	; Q
	.db	'W'	; W
	.db	0x0b	; Up
	.db	0x0a	; Down
	.db	0x18	; Cancel
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
	.db	0x80	; Menu
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

str_fail:		.db	"Fail", 0
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
str_keyb:		.db	"Keyboard init: ", 0
str_battery:		.db	"Battery: ", 0
str_bank_header:	.db	"Memory Page Setup:", 0
str_bank_config_psen:	.db	"	1 - Config PSEN", 0
str_bank_config_rdwr:	.db	"	2 - Config RD/WR", 0
str_bank_set_psen:	.db	"	3 - Set PSEN page", 0
str_bank_set_rdwr:	.db	"	4 - Set RD/WR page", 0
str_bank_quit:		.db	"	5 - Quit", 0
str_bank_set_page:	.db	"Enter page bank: ",0
str_bank_set_mode:	.db	"Set mode (0=ROM, 1=RAM, 3=RAM card): ", 0
str_bank_psen_cur_psen:	.db	"Current PSEN config: ", 0
str_bank_psen_cur_rdwr:	.db	"Current RDWR config: ", 0
str_init_header:	.db	" PAULMON ", 0
str_init_select:	.db	"Select console:", 0
str_init_oyster:	.db	"       O - Oyster Terminal", 0
str_init_serial:	.db	"       S - Serial", 0

.org    locat+0xbf0						; Make sure dph is not affected by adding to dpl
str_bank_memory_mode1:	.db	" RO", 0xcd	; 'M' + 0x80	; String table to allow easy selection of memory mode (using msb terminated strings)
str_bank_memory_mode2:	.db	" RA", 0xcd	; 'M' + 0x80
str_bank_memory_mode3:	.db	" IN", 0xd6	; 'V' + 0x80
str_bank_memory_mode4:	.db	" CR", 0xc4	; 'D' + 0x80

; ###############################################################################################################
; #                                                       Commands
; ###############################################################################################################

;---------------------------------------------------------;
;                                                         ;
;                  Set memory page bank                   ;
;                                                         ;
;---------------------------------------------------------;

.org	locat+0xc00
.db	0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db	254,'B',0,0		; id (254=cmd)
.db	0,0,0,0			; prompt code vector
.db	0,0,0,0			; reserved
.db	0,0,0,0			; reserved
.db	0,0,0,0			; reserved
.db	0,0,0,0			; user defined
.db	255,255,255,255		; length and checksum (255=unused)
.db	"Select Memory Bank",0
.org	locat+0xc40		; executable code begins here

memory_config:
	lcall	newline

	mov	dptr, #str_bank_psen_cur_psen			; Print current PSEN config
	lcall	pstr
	mov	a, mem_page_psen
	lcall	phex
	mov	dptr, #str_bank_memory_mode1
	mov	a, 0x27						; Get memory mode selection
	anl	a, #0x0f					; PSEN selection
	add	a, dpl
	mov	dpl, a
	lcall	pstr
	lcall	newline

	mov	dptr, #str_bank_psen_cur_rdwr			; Print current RDWR config
	lcall	pstr
	mov	a, mem_page_rdwr
	lcall	phex
	mov	dptr, #str_bank_memory_mode1
	mov	a, 0x27						; Get memory mode selection
	anl	a, #0xf0					; RDWR selection
	swap	a
	add	a, dpl
	mov	dpl, a
	lcall	pstr
	lcall	newline

memory_config_menu:
	mov	dptr, #str_bank_header				; Print menu
	lcall	pstr
	lcall	newline
;	mov	dptr, #str_bank_config_psen			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	newline
;	mov	dptr, #str_bank_config_rdwr			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	newline
;	mov	dptr, #str_bank_set_psen			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	newline
;	mov	dptr, #str_bank_set_rdwr			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	newline
;	mov	dptr, #str_bank_quit				; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	newline
	lcall	cin						; Get menu selection
	lcall	newline

memory_config_menu_1:
	cjne	a, #'1', memory_config_menu_2
	acall	memory_config_get_mode
	jc	memory_config_menu_else				; Check for invalid value
	lcall	memory_set_mode_psen
	sjmp	memory_config_menu_else
memory_config_menu_2:
	cjne	a, #'2', memory_config_menu_3
	acall	memory_config_get_mode
	jc	memory_config_menu_else				; Check for invalid value
	lcall	memory_set_mode_rdwr
	sjmp	memory_config_menu_else
memory_config_menu_3:
	cjne	a, #'3', memory_config_menu_4
	acall	memory_config_get_page
	lcall	memory_set_page_psen
	sjmp	memory_config_menu_else
memory_config_menu_4:
	cjne	a, #'4', memory_config_menu_5
	acall	memory_config_get_page
	lcall	memory_set_page_rdwr
	sjmp	memory_config_menu_else
memory_config_menu_5:
	cjne	a, #'5', memory_config_menu_else
	ret
memory_config_menu_else:
	ajmp	memory_config

memory_config_get_page:
;	lcall	newline
	mov	dptr, #str_bank_set_page
	lcall	pstr
	lcall	ghex
	lcall	newline
	ret

memory_config_get_mode:
;	lcall	newline
	mov	dptr, #str_bank_set_mode
	lcall	pstr
	lcall	ghex
	lcall	newline
	anl	a, #3						; Make sure it's valid
	cjne	a, #2, memory_config_get_mode_finish		; 2 is invalid, so ignore
	setb	c
	ret
memory_config_get_mode_finish:
	clr	c
	ret


;---------------------------------------------------------;
;                                                         ;
;                       System setup                      ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0xd00
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     253,',',0,0		; id (253=startup)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "System setup",0
.org    locat+0xd40		; executable code begins here

system_setup:
	lcall	newline
system_setup_init_reg:
	clr	keyboard_key_down				; Clear keyboard flag
	clr	keyboard_new_char				; Clear keyboard flag
	mov	a, #0
	mov	keycode_ascii, a				; Clear keyboard buffer
	mov	mem_mode, a
	cpl	a
	mov	keycode_raw, a					; Clear raw keyboard buffer

system_setup_ram:
	mov	dptr, #str_cfg_mem
	lcall	pstr
	mov	r7, #mem_sys_rom				; When running from ROM
;	mov	r7, #mem_sys_ram				; When running from RAM
	mov	a, r7						; We'll need this when running the memory check
	lcall	memory_set_mode_psen				; Set program access method
	mov	a, #mem_sys_ram
	lcall	memory_set_mode_rdwr				; Set data access method
	mov	a, 0
	lcall	memory_set_page_psen				; Set program page
	lcall	memory_set_page_rdwr				; Set data page
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
	cjne	r6, #0, system_setup_check_ram_do		; If it is the first pasge of memory
	cjne	r7, #mem_sys_ram, system_setup_check_ram_do	; Are we about to erase ourself
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
	mov	a, 0
	lcall	memory_set_page_rdwr				; Reselect the first page

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

system_setup_main_battery:
	mov	dptr, #str_battery
	lcall	pstr
	lcall	battery_check_status
	mov	dptr, #str_fail
	jnc	system_setup_main_battery_print_status
	mov	dptr, #str_okay
system_setup_main_battery_print_status:
	lcall	pstr
	lcall	newline

system_setup_mcard_battery:
	lcall	memory_ramcard_present				; If there is no ram card, don't bother checking it's status!
	jnc	system_setup_continue
	mov	dptr, #str_memory_card
	lcall	pstr
	mov	a, #' '
	lcall	cout
	mov	dptr, #str_battery
	lcall	pstr
	lcall	memory_ramcard_battery_check_status
	mov	dptr, #str_fail
	jnc	system_setup_mcard_battery_print_status
	mov	dptr, #str_okay
system_setup_mcard_battery_print_status:
	lcall	pstr

system_setup_continue:
	mov	a, #1						; 200 Hz
	mov	b, #10						; 500ms
	lcall	piezo_beep					; Set tone
	mov	a, #10						; 1445 Hz
	mov	b, #5						; 250ms
	lcall	piezo_beep					; Set tone

	lcall	newline
	ret


;---------------------------------------------------------;
;                                                         ;
;                       System init                       ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0xf00
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     249,',',0,0		; id (249=init)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "System init",0
.org    locat+0xf40		; executable code begins here

system_init:
; General system config
	mov	ie, #0		; Disable interrupts (0xa8)
	mov	ie2, #0		; Disable interrupts (0xe8)
	mov	pwm1, #0	; PWM1 is connected to the piezo speaker
	orl	pcon, #10h	; Set condition to load watchdog timer
	mov	t3, #0		; Load watchdog timer
; i2c config
        setb    p1.6            ; SCL
        setb    p1.7            ; SDA

system_init_keyboard:
	lcall	keyboard_reset
	clr	keyboard_new_char

system_init_lcd:
	mov	a, #4
	lcall	lcd_set_contrast
	lcall	lcd_set_backlight_on
	lcall	lcd_init

	mov	a, #10						; 1445 Hz
	mov	b, #5
	lcall	piezo_beep					; Beep tone

	setb	lcd_glyph_doublewidth
	setb	lcd_glyph_doubleheight
	setb	lcd_glyph_invert
	mov	lcd_start_position, #0x07			; 1st row, 5th column
	mov	dptr, #str_init_header
	lcall	lcd_pstr

	clr	lcd_glyph_doublewidth
	clr	lcd_glyph_doubleheight
	clr	lcd_glyph_invert
	mov	lcd_start_position, #0x60			; 3rd row, 1st column
	mov	dptr, #str_init_select
	lcall	lcd_pstr
	mov	lcd_start_position, #0x80			; 4th row, 1st column
	mov	dptr, #str_init_oyster
	lcall	lcd_pstr
	mov	lcd_start_position, #0xa0			; 5th row, 1st column
	mov	dptr, #str_init_serial
	lcall	lcd_pstr

system_init_timeout_loop_setup:
	mov	r5, #0x64					; 5 seconds of delays
	clr	keyboard_new_char
system_init_timeout_loop:
	mov	lcd_start_position, #0xF5			; last row, 22nd column
	mov	dptr, #str_timeout
	lcall	lcd_pstr

	mov	a, r5						; Remaining time delays left
	mov	b, #0x14					; Setup divide by 20
	div	ab						; Seconds left
	inc	a						; Makes the value more understandable
	orl	a, #0x30					; Convert to ASCII number
	lcall	lcd_print_character

	lcall	keyboard_scan					; Keyboard input, if any
	jb	keyboard_new_char, system_init_mode_select	; Continue if key pressed
	mov	a, #1
	lcall	sdelay						; Delay 50ms
	djnz	r5, system_init_timeout_loop			; Loop if we haven't timed out

	mov	keycode_ascii, #'S'				; Set default to serial

system_init_mode_select:
	mov	a, keycode_ascii
system_init_mode_select_serial:
	cjne	a, #'S',system_init_mode_select_oyster
	clr	use_oysterlib
	lcall	lcd_set_backlight_off
	lcall	lcd_off
	ret
system_init_mode_select_oyster:
	cjne	a, #'O',system_init_timeout_loop_setup
	setb	use_oysterlib
	lcall	lcd_clear_screen
	ret