; ###############################################################################################################
; # Contains:
; #
; # Command:
; #	Memory tool
; ###############################################################################################################

; define the storage location
; ##########################################################################
;.equ	paulmon2, 0x0000					; location where paulmon2
.equ	paulmon2, 0x8000					; location where paulmon2
.equ	oysterlib, paulmon2+0x1080				; location of oysterlib
.equ	locat, 0x4000+paulmon2					; location for the library/commands

; PaulMON functions
; ##########################################################################
;.equ	cout, 0x78+paulmon2					; Print Acc to Serial Port
;.equ    Cin, 0x7a+paulmon2					; Get Acc from serial port
.equ    pHex, 0x7c+paulmon2					; Print Hex value of Acc
.equ    pHex16, 0x7e+paulmon2					; Print Hex value of DPTR
.equ    pStr, 0x80+paulmon2					; Print string pointed to by DPTR,
								;  must be terminated by 0 or a high bit set
								;  pressing ESC will stop the printing
.equ    gHex, 0x82+paulmon2					; Get Hex input into Acc
								;  carry set if ESC has been pressed
.equ    gHex16, 0x84+paulmon2					; Get Hex input into DPTR
								;  carry set if ESC has been pressed
.equ    ESC, 0x86+paulmon2					; Check for ESC key
								;  carry set if ESC has been pressed
.equ    Upper, 0x88+paulmon2					; Convert Acc to uppercase
								;  non-ASCII values are unchanged
.equ    Init, 0x8a+paulmon2					; Initialize serial port
;.equ    newline, 0x90+paulmon2					; Print CR/LF (13 and 10)
.equ    lenstr, 0x92+paulmon2					; Return the length of a string @DPTR (in R0)
.equ    pint8u, 0x95+paulmon2					; Print Acc at an integer, 0 to 255
.equ    pint8, 0x98+paulmon2					; Print Acc at an integer, -128 to 127
.equ    pint16u, 0x9b+paulmon2					; Print DPTR as an integer, 0 to 65535

; OysterLib functions
; ##########################################################################
.equ	console_lib, oysterlib+0x00
.equ	math_lib, oysterlib+0x09
.equ	memory_lib, oysterlib+0x12
.equ	i2c_lib, oysterlib+0x24
.equ	rtc_lib, oysterlib+0x45
.equ	keyboard_lib, oysterlib+0x5a
.equ	lcd_lib, oysterlib+0x63
.equ	serial_lib, oysterlib+0x8d
.equ	power_lib, oysterlib+0xa5
.equ	misc_lib, oysterlib+0xb1

; console lib
.equ	oysterlib_cout, console_lib+0x00
.equ	oysterlib_newline, console_lib+0x03
.equ	oysterlib_cin, console_lib+0x06

; math lib
.equ	packed_bcd_to_hex, math_lib+0x00
.equ	hex_to_packed_bcd, math_lib+0x03
.equ	power_of_2, math_lib+0x06

; memory lib
.equ	memory_set_page_psen, memory_lib+0x00
.equ	memory_set_page_rdwr, memory_lib+0x03
.equ	memory_set_mode_psen, memory_lib+0x06
.equ	memory_set_mode_rdwr, memory_lib+0x09
.equ	memory_ramcard_present, memory_lib+0x0c
.equ	memory_ramcard_write_protect, memory_lib+0x0f

; i2c lib
.equ	i2c_start, i2c_lib+0x00
.equ	i2c_stop_clock_stretch_check, i2c_lib+0x03
.equ	i2c_stop, i2c_lib+0x06
.equ	i2c_write_byte, i2c_lib+0x09
.equ	i2c_ack_check, i2c_lib+0x0c
.equ	i2c_write_byte_to_addr, i2c_lib+0x0f
.equ	i2c_write_byte_with_ack_check, i2c_lib+0x12
.equ	i2c_read_byte, i2c_lib+0x15
.equ	i2c_master_read_ack, i2c_lib+0x18
.equ	i2c_read_byte_from_addr, i2c_lib+0x1b
.equ	i2c_read_device_register, i2c_lib+0x1e

; rtc lib
.equ	rtc_get_config, rtc_lib+0x00
.equ	rtc_get_alarm_config, rtc_lib+0x03
.equ	rtc_set_config, rtc_lib+0x06
.equ	rtc_set_alarm_config, rtc_lib+0x09
.equ	rtc_init, rtc_lib+0x0c
.equ	rtc_get_datetime, rtc_lib+0x0f
.equ	rtc_set_datetime, rtc_lib+0x12

; keyboard lib
.equ	keyboard_scan, keyboard_lib+0x00
.equ	keyboard_reset, keyboard_lib+0x03
.equ	keyboard_wait_for_keypress, keyboard_lib+0x06

; lcd lib
.equ	lcd_off, lcd_lib+0x00
.equ	lcd_on, lcd_lib+0x03
.equ	lcd_init, lcd_lib+0x06
.equ	lcd_clear_screen, lcd_lib+0x09
.equ	lcd_clear_screen_from_position, lcd_lib+0x0c
.equ	lcd_new_line_scroll_and_clear, lcd_lib+0x0f
.equ	lcd_print_character, lcd_lib+0x12
.equ	lcd_set_glyph_position, lcd_lib+0x15
.equ	lcd_plot_point, lcd_lib+0x18
.equ	lcd_pstr, lcd_lib+0x1b
.equ	lcd_set_backlight_on, lcd_lib+0x1e
.equ	lcd_set_backlight_off, lcd_lib+0x21
.equ	lcd_set_contrast_inc, lcd_lib+0x24
.equ	lcd_set_contrast_dec, lcd_lib+0x27

; serial lib
.equ	serial_mainport_enable, serial_lib+0x00
.equ	serial_mainport_disable, serial_lib+0x03
.equ	serial_rts_set, serial_lib+0x06
.equ	serial_rts_unset, serial_lib+0x09
.equ	serial_cts_check, serial_lib+0x0c
.equ	serial_baudsave_check, serial_lib+0x0f
.equ	serial_baudsave_set_reload, serial_lib+0x12
.equ	serial_baudsave_set, serial_lib+0x15

; power lib
.equ	power_battery_main_check_charging, power_lib+0x00
.equ	power_battery_backup_check_status, power_lib+0x03
.equ	power_battery_ramcard_check_status_warn, power_lib+0x06
.equ	power_battery_ramcard_check_status_fail, power_lib+0x09

; misc lib
.equ	piezo_beep, misc_lib+0x00
.equ	piezo_pwm_sound, misc_lib+0x03
.equ	sdelay, misc_lib+0x06

; SFR definitions
; ##########################################################################
.equ	sfr_p4_80c562, 0xc0					; Port 4
.equ	sfr_p5_80c562, 0xc4					; Port 5
.equ	sfr_ie2_80c562, 0xe8					; 2nd interrupt control register
.equ	sfr_t2con_80c562, 0xEA					; Timer2 control SFR
.equ	sfr_tl2_80c562, 0xEC					; Timer2 low byte SFR
.equ	sfr_th2_80c562, 0xED					; Timer2 high byte SFR
.equ	sfr_pwm1_80c562, 0xFD					; PWM1 control
.equ	sfr_pwmp_80c562, 0xFE					; PWM prescaler
.equ	sfr_t3_80c562, 0xff					; Timer3 control

.flag	i2c_scl, p1.6
.flag	i2c_sda, p1.7


; RAM definitions
; ##########################################################################
; Bank 0
.equ	rb0r0, 0x00
.equ	rb0r1, 0x01
.equ	rb0r2, 0x02
.equ	rb0r3, 0x03
.equ	rb0r4, 0x04
.equ	rb0r5, 0x05
.equ	rb0r6, 0x06
.equ	rb0r7, 0x07
; Bank 1
.equ	rb1r0, 0x08
.equ	rb1r1, 0x09
.equ	rb1r2, 0x0a
.equ	rb1r3, 0x0b
.equ	rb1r4, 0x0c
.equ	rb1r5, 0x0d
.equ	rb1r6, 0x0e
.equ	rb1r7, 0x0f
; Bank 2
.equ	rb2r0, 0x10
.equ	rb2r1, 0x11
.equ	rb2r2, 0x12
.equ	rb2r3, 0x13
.equ	rb2r4, 0x14
.equ	rb2r5, 0x15
.equ	rb2r6, 0x16
.equ	rb2r7, 0x17
; Bank 3
.equ	rb3r0, 0x18
.equ	rb3r1, 0x19
.equ	rb3r2, 0x1a
.equ	rb3r3, 0x1b
.equ	rb3r4, 0x1c
.equ	rb3r5, 0x1d
.equ	rb3r6, 0x1e
.equ	rb3r7, 0x1f

; Bit addressable addresses (0x20-0x21 not used by MCS BASIC)
.equ	stack_carry, 0x20					; Save Carry state here to load on the stack
.equ	mem_mode, 0x20						; Bit addressable, memory mode store

; Addresses (0x50-0x67 not used by MCS BASIC/PaulMON, stack = 0x70)
.equ	lcd_font_table_msb, 0x50				; MSB of the address of the font table
.equ	lcd_scroll_offset, 0x51					; LCD start draw line
.equ	lcd_start_position, 0x52				; Start position for LCD operations (b0-b4 - column, b5-7 - row)
.equ	lcd_subscreen_row, 0x53					; Current subscreen row
.equ	lcd_subscreen_column, 0x54				; Current subscreen column
.equ	lcd_properties, 0x55					; Current contrast/backlight value
.equ	keycode_raw, 0x56					; Raw keycode, read from hardware
.equ	keycode_ascii, 0x57					; Processed raw keycode to ASCII keymap
.equ	mem_page_psen, 0x58					; Store for the currently selected PSEN page
.equ	mem_page_rdwr, 0x59					; Store for the currently selected RDWR page
.equ	keymap_offset, 0x5A					; Used to select the correct keymap
.equ	tmp_var, 0x60
.equ	current_year, 0x67					; The current year as an offset from 2000 (needed as the RTC only stores 2 bits for the year)
; 0x68-0x6B used by baud_save
.equ	lcd_props_save, 0x6C					; Used to retain the backlight/contrast settings when the screen is off
.equ	sys_props_save, 0x6D					; System config
								;	0 - key_click
								;	1 - Main serial port status
.equ	sys_cfg_chksum_inv, 0x6E				; Checksum (inverted) of the system configuration
.equ	sys_cfg_checksum, 0x6F					; Checksum of the system configuration

; Definitions for the system config save/restore rountines
; So there is only one place to update
.equ	sys_config_start, current_year
.equ	sys_config_end, sys_props_save

; Bit RAM definitions
; ##########################################################################
.flag	use_oysterlib, 0x20.0
.flag	key_click, 0x20.1
.flag	mem_mode_psen_ram, 0x20.2				; This bit is not randomly chosen, see memory command
.flag	mem_mode_psen_ram_card, 0x20.3				; This bit is not randomly chosen, see memory command
.flag	tmp_bit, 0x20.4
.flag	stack_carry_bit, 0x20.5
.flag	mem_mode_rdwr_ram, 0x20.6				; This bit is not randomly chosen, see memory command
.flag	mem_mode_rdwr_ram_card, 0x20.7				; This bit is not randomly chosen, see memory command

.flag	lcd_glyph_doublewidth, 0x21.0
.flag	lcd_glyph_doubleheight, 0x21.1
.flag	lcd_glyph_doubleheight_otherhalf, 0x21.2
.flag	lcd_glyph_invert, 0x21.3
.flag	lcd_no_scroll, 0x21.4
.flag	lcd_glyph_use_ram, 0x21.5
.flag	keyboard_key_down, 0x21.6
.flag	keyboard_new_char, 0x21.7


; ###############################################################################################################
; #                                                       Commands
; ###############################################################################################################

;---------------------------------------------------------;
;                                                         ;
;                  Set memory page bank                   ;
;                                                         ;
;---------------------------------------------------------;

.org	locat+0x000
.db	0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db	254,'"',0,0		; id (254=cmd)
.db	0,0,0,0			; prompt code vector
.db	0,0,0,0			; reserved
.db	0,0,0,0			; reserved
.db	0,0,0,0			; reserved
.db	0,0,0,0			; user defined
.db	255,255,255,255		; length and checksum (255=unused)
.db	"Select Memory Bank",0
.org	locat+0x040		; executable code begins here

memory_config:
	lcall	oysterlib_newline

	mov	dptr, #str_bank_psen_cur_psen			; Print current PSEN config
	lcall	pstr
	mov	a, mem_page_psen
	lcall	phex
	mov	dptr, #str_bank_memory_mode1
	mov	a, mem_mode					; Get memory mode selection
	anl	a, #0x0c					; PSEN selection
	add	a, dpl
	mov	dpl, a
	lcall	pstr
	lcall	oysterlib_newline

	mov	dptr, #str_bank_psen_cur_rdwr			; Print current RDWR config
	lcall	pstr
	mov	a, mem_page_rdwr
	lcall	phex
	mov	dptr, #str_bank_memory_mode1
	mov	a, mem_mode					; Get memory mode selection
	anl	a, #0xc0					; RDWR selection
	swap	a
	add	a, dpl
	mov	dpl, a
	lcall	pstr
	lcall	oysterlib_newline

memory_config_menu:
	mov	dptr, #str_bank_header				; Print menu
	lcall	pstr
	lcall	oysterlib_newline
;	mov	dptr, #str_bank_config_psen			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	oysterlib_newline
;	mov	dptr, #str_bank_config_rdwr			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	oysterlib_newline
;	mov	dptr, #str_bank_set_psen			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	oysterlib_newline
;	mov	dptr, #str_bank_set_rdwr			; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_menu_quit				; Don't need to set DPTR, printing next string
	lcall	pstr
	lcall	oysterlib_newline
	lcall	oysterlib_cin						; Get menu selection
	lcall	oysterlib_newline

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
	cjne	a, #'4', memory_config_menu_quit
	acall	memory_config_get_page
	lcall	memory_set_page_rdwr
	sjmp	memory_config_menu_else
memory_config_menu_quit:
	cjne	a, #'0', memory_config_menu_else
	ret
memory_config_menu_else:
	ajmp	memory_config

memory_config_get_page:
;	lcall	oysterlib_newline
	mov	dptr, #str_bank_set_page
	lcall	pstr
	lcall	ghex
	lcall	oysterlib_newline
	ret

memory_config_get_mode:
;	lcall	oysterlib_newline
	mov	dptr, #str_bank_set_mode
	lcall	pstr
	lcall	ghex
	lcall	oysterlib_newline
	anl	a, #3						; Make sure it's valid
	cjne	a, #2, memory_config_get_mode_finish		; 2 is invalid, so ignore
	setb	c
	ret
memory_config_get_mode_finish:
	clr	c
	ret


;---------------------------------------------------------;
;                                                         ;
;                          I2C tool                       ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x100
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     254,'~',0,0		; id (254=cmd)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "I2C tool",0
.org    locat+0x140		; executable code begins here


i2c_tool:
	lcall	oysterlib_newline
i2c_tool_menu:
	mov	dptr, #str_i2c_header				; Print menu
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_bus_scan
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_read_reg
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_write_reg
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_dump_regs
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_menu_quit
	lcall	pstr
	lcall	oysterlib_newline
	lcall	oysterlib_cin						; Get menu selection
	lcall	oysterlib_newline

i2c_tool_menu_1:
	cjne	a, #'1', i2c_tool_menu_2
	acall	i2c_tool_bus_scan
	sjmp	i2c_tool_menu_else
i2c_tool_menu_2:
	cjne	a, #'2', i2c_tool_menu_3
	acall	i2c_tool_read_register
	sjmp	i2c_tool_menu_else
i2c_tool_menu_3:
	cjne	a, #'3', i2c_tool_menu_4
	acall	i2c_tool_write_register
	sjmp	i2c_tool_menu_else
i2c_tool_menu_4:
	cjne	a, #'4', i2c_tool_menu_quit
	acall	i2c_tool_dump_registers
	sjmp	i2c_tool_menu_else
i2c_tool_menu_quit:
	cjne	a, #'0', i2c_tool_menu_else
	ret
i2c_tool_menu_else:
	sjmp	i2c_tool

i2c_tool_bus_scan:
	lcall	oysterlib_newline
	mov	r1, #0						; Use r1 to store the i2c address to test
	mov	r2, #16						; Number of addresses to print per line
i2c_tool_bus_scan_print_addr:
	mov	a, r1
	anl	a, 0xf0						; Get upper nibble
	lcall	phex						; Print upper nibble of address
	mov	a, #':'
	lcall	oysterlib_cout
	mov	a, #' '
	lcall	oysterlib_cout
i2c_tool_bus_scan_check_addr:
	mov	a, r1
	rl	a						; Prepare address
	inc	a						; i2c read operation
	lcall	i2c_start
	lcall	i2c_write_byte					; Write address
	lcall	i2c_ack_check					; Check if a device responded
	lcall	i2c_stop					; Finish bus comms

;	push	acc
;	mov	b, a
;	clr	a
;	lcall	i2c_write_byte_to_addr
;	lcall	i2c_stop
;	pop	acc

	jc	i2c_tool_bus_scan_nack
	mov	a, r1
	lcall	phex						; Print the address we've found
	sjmp	i2c_tool_bus_scan_next_addr
i2c_tool_bus_scan_nack:
	mov	a, #'-'						; Device not found, just print '--'
	lcall	oysterlib_cout
	lcall	oysterlib_cout
i2c_tool_bus_scan_next_addr:
	mov	a, #' '
	lcall	oysterlib_cout
	inc	r1						; Increment address
	djnz	r2, i2c_tool_bus_scan_check_addr			; Are we still printing the same line
i2c_tool_bus_scan_next_line:
	lcall	oysterlib_newline
	mov	r2, #16						; New line of addresses
	cjne	r1, #128, i2c_tool_bus_scan_print_addr		; Have we checked all the addresses
	ret

i2c_tool_read_register:
	mov	dptr, #str_i2c_addr
	lcall	pstr
	lcall	ghex						; Get i2c address
	rl	a						; Shift address
	mov	b, a						; Save i2c address
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_read
	lcall	pstr
	mov	dptr, #str_i2c_register
	lcall	pstr
	lcall	ghex						; Get register to read
	lcall	oysterlib_newline
	lcall	i2c_write_byte_to_addr				; Select register
	jnc	i2c_tool_read_register_restart			; Check for errors
	sjmp	i2c_tool_read_register_address_failed
i2c_tool_read_register_restart:
	lcall	i2c_start					; I2C restart
	mov	a, b						; Get i2c address
	inc	a						; Read operation
	lcall	i2c_write_byte					; Re-open connection
	lcall	i2c_ack_check					; Check for errors
	jnc	i2c_tool_read_register_do
	sjmp	i2c_tool_read_register_address_failed
i2c_tool_read_register_do:
	lcall	i2c_read_byte					; Get config/status
	lcall	i2c_master_read_ack				; ACK read
	lcall	i2c_stop
	mov	dptr, #str_i2c_register
	lcall	pstr
	lcall	phex
	ljmp	oysterlib_newline
i2c_tool_read_register_address_failed:
	mov	dptr, #str_i2c_addr
	lcall	pstr
	mov	dptr, #str_fail
	lcall	pstr
	lcall	oysterlib_newline
	ljmp	i2c_stop

i2c_tool_write_register:
	mov	dptr, #str_i2c_addr
	lcall	pstr
	lcall	ghex						; Get i2c address
	rl	a						; Shift address
	mov	b, a						; Save i2c address
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_write
	lcall	pstr
	mov	dptr, #str_i2c_register
	lcall	pstr
	lcall	ghex						; Get register to write
	mov	r0, a						; Save data for later
	lcall	oysterlib_newline
	mov	dptr, #str_i2c_data
	lcall	pstr
	lcall	ghex						; Get data to write
	lcall	oysterlib_newline
	push	acc						; Need this after the register has been selected
	mov	a, r0						; Move register back
	lcall	i2c_write_byte_to_addr				; Select register
	jnc	i2c_tool_write_register_restart			; Check for errors
	pop	acc
	mov	dptr, #str_i2c_addr
	lcall	pstr
	mov	dptr, #str_fail
	lcall	pstr
	lcall	oysterlib_newline
	ljmp	i2c_stop
i2c_tool_write_register_restart:
	pop	acc
	lcall	i2c_write_byte_with_ack_check
	lcall	i2c_stop
	mov	dptr, #str_fail
	jc	i2c_tool_write_register_result
	mov	dptr, #str_okay
i2c_tool_write_register_result:
	lcall	pstr
	ljmp	oysterlib_newline

i2c_tool_dump_registers:
	mov	dptr, #str_i2c_addr
	lcall	pstr
	lcall	ghex						; Get i2c address
	rl	a						; Shift address
	mov	b, a						; Save i2c address
	lcall	oysterlib_newline
	mov	tmp_var, #0x00					; Start with address 0x00
	mov	a, tmp_var
	lcall	i2c_write_byte_to_addr				; Select register
	jnc	i2c_tool_dump_registers_restart			; Check for errors
	ajmp	i2c_tool_read_register_address_failed
i2c_tool_dump_registers_restart:
	lcall	i2c_start					; I2C restart
	mov	a, b						; Get i2c address
	inc	a						; Read operation
	lcall	i2c_write_byte					; Re-open connection
	lcall	i2c_ack_check					; Check for errors
	jnc	i2c_tool_dump_registers_read_reg
	ajmp	i2c_tool_read_register_address_failed
i2c_tool_dump_registers_read_reg:
	mov	a, tmp_var
	anl	a, #0x0f
	jnz	i2c_tool_dump_registers_read_reg_do
	lcall	oysterlib_newline
i2c_tool_dump_registers_read_reg_do:
	lcall	i2c_read_byte					; Get config/status
	lcall	i2c_master_read_ack				; ACK read
	lcall	phex						; Print byte
	mov	a, #0x20
	lcall	oysterlib_cout					; Space
	inc	tmp_var
	mov	a, tmp_var
	cjne	a, 0x00, i2c_tool_dump_registers_read_reg
	lcall	i2c_stop
	ljmp	oysterlib_newline

str_i2c_header:		.db	"I2C Tool:", 0
str_i2c_bus_scan:	.db	"	1 - Bus Scan", 0
str_i2c_read_reg:	.db	"	2 - Read Register", 0
str_i2c_write_reg:	.db	"	3 - Write Register", 0
str_i2c_dump_regs:	.db	"	4 - Dump Registers", 0


;---------------------------------------------------------;
;                                                         ;
;                     Console select                      ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x400
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     254,')',0,0		; id (254=cmd)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "Console select",0
.org    locat+0x440		; executable code begins here


console_select:
	lcall	oysterlib_newline

console_select_menu:
	mov	dptr, #str_screen_header			; Print menu
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_screen_hardware
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_screen_serial
	lcall	pstr
	lcall	oysterlib_newline
;	mov	dptr, #str_menu_quit
;	lcall	pstr
;	lcall	oysterlib_newline
	lcall	oysterlib_cin					; Get menu selection
	lcall	oysterlib_newline

console_select_menu_1u:
	cjne	a, #'H', console_select_menu_1l
	sjmp	console_select_menu_1_do
console_select_menu_1l:
	cjne	a, #'h', console_select_menu_2u
console_select_menu_1_do:
	setb	use_oysterlib
	lcall	lcd_init
	ret
console_select_menu_2u:
	cjne	a, #'S', console_select_menu_2l
	sjmp	console_select_menu_2_do
console_select_menu_2l:
	cjne	a, #'s', console_select_menu_else
console_select_menu_2_do:
	clr	use_oysterlib
	lcall	serial_mainport_enable
	lcall	lcd_off
	ret
;console_select_menu_quit:
;	cjne	a, #'0', console_select_menu_else
;	ret
console_select_menu_else:
	sjmp	console_select

str_screen_header:	.db	"Select console:", 0
str_screen_hardware:	.db	"	H - Hardware", 0
str_screen_serial:	.db	"	S - Serial", 0


;---------------------------------------------------------;
;                                                         ;
;                   Simple flash tool                     ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x500
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     254,'@',0,0		; id (254=cmd)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "Flash tool",0
.org    locat+0x540		; executable code begins here


; # Copied from OysterLib
; #######################################
; define latch addresses
.equ    psen_page_latch, 0x08
.equ    rdwr_page_latch, 0x0c

; # Flash chip definitions
; #######################################
.equ	flash_command_reg_addr1, 0x5555
.equ	flash_command_reg_addr2, 0x2aaa
.equ	flash_command_reg_enable1, 0xaa
.equ	flash_command_reg_enable2, 0x55

.equ	flash_command_reset, 0xf0
.equ	flash_command_autoselect, 0x90
.equ	flash_command_program, 0xa0
.equ	flash_command_erase, 0x80
.equ	flash_command_erase_all, 0x10
.equ	flash_command_erase_sector, 0x30

.equ	base_addr, 0x8000
.equ	flash_tool_addr_fudge, 0x0000				; Fudge to allow code to run while being developed in RAM
;.equ	flash_tool_addr_fudge, 0x8000				; And then run from ROM (which has been copied to RAM)


; # flash_tool
; #
; # Copies the XRAM area 0x0000 to XRAM area 0x8000, with the selected
; # flash ROM page mapped to this area
; ##########################################################################
flash_tool:
	lcall	oysterlib_newline

	mov	dptr, #*					; Check whether we are running from ROM or a RAM dev version
	mov	a, dph
	cjne	a, #0x80, flash_tool_location_check
flash_tool_location_check:
	jnc	flash_tool_actual
	mov	dptr, #str_ft_copy_rom
	lcall	pstr
	lcall	oysterlib_newline
	lcall	flash_tool_copy_rom_to_ram			; Make a working copy of the ROM in RAM

	setb	p1.0						; Select RAM on PSEN in 0x8000 area
	setb	mem_mode_psen_ram
	clr	p1.1
	clr	mem_mode_psen_ram_card

	mov	a, #0x03					; Select page 3 on access to 0x8000 area
	mov	mem_page_psen, a
	mov	dph, #psen_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4
	ljmp	flash_tool_actual+flash_tool_addr_fudge		; Jump to copy in RAM

flash_tool_actual:
	mov	dptr, #str_ft_device				; Print device ID
	lcall	pstr
	acall	flash_get_deviceid+flash_tool_addr_fudge
	lcall	phex
	mov	a, #'/'
	lcall	oysterlib_cout
	mov	a, b
	lcall	phex
	lcall	oysterlib_newline

	mov	dptr, #str_ft_select_page			; Get the desired page to flash
	lcall	pstr
	lcall	ghex
	lcall	oysterlib_newline
	mov	r1, a						; Save selected page
	anl	a, #0x03					; Make sure it's within the desired range
	xrl	a, r1						; And make sure it's what we entered
	jz	flash_tool_actual_confirm
	mov	dptr, #str_ft_err_page
	lcall	pstr
	lcall	oysterlib_newline
	ret

flash_tool_actual_confirm:
	mov	dptr, #str_ft_confirm				; Confirm we want to flash page
	lcall	pstr
	lcall	oysterlib_cin
	lcall	oysterlib_newline
	cjne	a, #'Y', flash_tool_finish			; Check choice

	mov	dptr, #str_ft_erasing_page
	lcall	pstr
	mov	a, r1
	lcall	phex
	mov	a, #':'
	lcall	oysterlib_cout
	mov	a, #' '
	lcall	oysterlib_cout
	acall	flash_erase_page+flash_tool_addr_fudge		; Erase page
	mov	dptr, #str_fail
	jc	flash_tool_actual_erase_result
	mov	dptr, #str_okay
flash_tool_actual_erase_result:
	lcall	pstr
	lcall	oysterlib_newline

	mov	dptr, #str_ft_flashing_page
	lcall	pstr
	mov	a, r1
	lcall	phex
	mov	a, #':'
	lcall	oysterlib_cout
	mov	a, #' '
	lcall	oysterlib_cout
	lcall	flash_tool_flash_page_from_ram+flash_tool_addr_fudge	; Flash page
	mov	dptr, #str_fail
	jc	flash_tool_actual_flash_result
	mov	dptr, #str_okay
flash_tool_actual_flash_result:
	lcall	pstr
	lcall	oysterlib_newline

	mov	a, r1						; If page 0 was written, we need to reset
	jnz	flash_tool_finish
	mov	dptr, #str_ft_reseting_device
	lcall	pstr
	lcall	oysterlib_newline
	ljmp	#0x0000						; Perform a reset as the monitor was overwritten

flash_tool_finish:
	ret


; # flash_tool_copy_rom_to_ram
; #
; # Make a temporary copy of the system ROM in RAM, so as to be able to
; # flash system ROM.
; ##########################################################################
flash_tool_copy_rom_to_ram:
	setb	p1.2						; Select RAM on RD|WR access in 0x8000 area
	setb	mem_mode_rdwr_ram
	clr	p1.3
	clr	mem_mode_rdwr_ram_card

	mov	a, #0x03					; Select page 3 on access to 0x8000 area
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4

	mov	dptr, #0x7fff
flash_tool_copy_rom_to_ram_loop:
	clr	a
	movc	a, @a+dptr					; Read byte of ROM
	orl	dph, #0x80					; Convert to RAM address
	movx	@dptr, a					; Copy ROM byte to RAM
	anl	dph, #0x7f					; Convert back to ROM address

	mov	a, #0xff					; Needed for compare
	dec	dpl						; Decrement address pointer LSB
	cjne	a, dpl, flash_tool_copy_rom_to_ram_loop		; Loop on address pointer LSB
	dec	dph						; Decrement address poiner MSB
	cjne	a, dph, flash_tool_copy_rom_to_ram_loop		; Loop on address pointer MSB

	ret


; # flash_tool_flash_page_from_ram
; #
; # Copies from system RAM page 0 to the specified ROM page
; # In:
; #   r1 - ROM page to select
; # Out:
; #   Carry - Set on error
; ##########################################################################
flash_tool_flash_page_from_ram:
	mov	r2, #0xFF					; Address pointer LSB store
	mov	r3, #0x7F					; Address pointer MSB store

flash_tool_flash_page_from_ram_read_loop:
	setb	p1.2						; Select RAM
	setb	mem_mode_rdwr_ram
	clr	p1.3
	clr	mem_mode_rdwr_ram_card

	mov	a, #0						; Select page 0
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4

	mov	dpl, r2						; Load dptr with next byte to read
	mov	dph, r3
	movx	a, @dptr					; Get next byte

	mov	r0, a						; Set data to program
	acall	flash_program_byte+flash_tool_addr_fudge
	jc	flash_tool_flash_page_from_ram_finish		; On error, exit

	dec	r2
	cjne	r2, #0xff, flash_tool_flash_page_from_ram_read_loop	; Loop on the LSB of address data
	dec	r3
	cjne	r3, #0xff, flash_tool_flash_page_from_ram_read_loop	; Loop on the MSB of address data

	clr	c						; No errors
flash_tool_flash_page_from_ram_finish:
	ret


; # flash_send_command
; #
; # Sends the command register select sequence
; ##########################################################################
flash_send_command:
	clr	p1.2						; Select ROM
	clr	mem_mode_rdwr_ram
	clr	p1.3
	clr	mem_mode_rdwr_ram_card

	mov	a, #0						; Select page 0
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4

flash_send_command_no_setup:
	mov	dptr, #base_addr+flash_command_reg_addr1	; Command register enable address 1
	mov	a,  #0xaa					; Command register enable command byte 1
	movx	@dptr, a

	mov	dptr, #base_addr+flash_command_reg_addr2	; Command register enable address 2
	mov	a,  #0x55					; Command register command byte 2
	movx	@dptr,a

	mov	dptr, #base_addr+flash_command_reg_addr1	; Command register enable address 1

	ret


; # flash_device_reset
; #
; # Resets the flash device back to read operation
; ##########################################################################
flash_device_reset:
	acall	flash_send_command+flash_tool_addr_fudge
	mov	a,  #flash_command_reset			; Reset device operation
	movx	@dptr, a
	ret


; # flash_get_deviceid
; #
; # Gets the flash chip manufacturer/device ID
; # Out:
; #   A - Manufacturer ID
; #   B - Device ID
; ##########################################################################
flash_get_deviceid:
	acall	flash_send_command+flash_tool_addr_fudge
	mov	a,  #flash_command_autoselect			; Device/ID command
	movx	@dptr, a

	mov	dptr, #base_addr
	movx	a, @dptr
	push	acc						; Save manufacturer id

	inc	dptr
	movx	a, @dptr
	push	acc						; Save device id

	acall	flash_device_reset+flash_tool_addr_fudge	; Reset the device back to read operation

	pop	b						; Restore device id
	pop	acc						; Restore manufacturer id

	ret


; # flash_erase_page
; #
; # Erase a page of memory
; # In:
; #   r1 - ROM page to erase (0-3)
; # Out:
; #   Carry - Set on error
; ##########################################################################
flash_erase_page:
	acall	flash_send_command+flash_tool_addr_fudge
	mov	a, #flash_command_erase				; Setup erase
	movx	@dptr, a
	acall	flash_send_command_no_setup+flash_tool_addr_fudge
	mov	a, r1						; Get ROM page to write
	anl	a, #3						; Make sure it's valid
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4
	mov	dptr, #base_addr+0x0000				; Select first sector of page
	mov	a, #flash_command_erase_sector			; Select sector erase
	movx	@dptr, a

flash_erase_page_sector0_loop:
	movx	a, @dptr
	jb	acc.7, flash_erase_page_sector0_loop_finish	; Check whether this sector erase has finished
	jb	acc.5, flash_erase_page_error			; Check whether the sector erase has timed out
	sjmp	flash_erase_page_sector0_loop
flash_erase_page_sector0_loop_finish:

	acall	flash_send_command+flash_tool_addr_fudge
	mov	a, #flash_command_erase				; Setup erase
	movx	@dptr, a
	acall	flash_send_command_no_setup+flash_tool_addr_fudge
	mov	a, r1						; Get ROM page to write
	anl	a, #3						; Make sure it's valid
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4
	mov	dptr, #base_addr+0x4000				; Select second sector of page
	mov	a, #flash_command_erase_sector			; Select sector erase
	movx	@dptr, a

flash_erase_page_sector1_loop:
	movx	a, @dptr
	jb	acc.7, flash_erase_page_sector1_loop_finish	; Check whether this sector erase has finished
	jb	acc.5, flash_erase_page_error			; Check whether the sector erase has timed out
	sjmp	flash_erase_page_sector1_loop
flash_erase_page_sector1_loop_finish:

	clr	c						; There wasn't any errors
	ret
flash_erase_page_error:
	setb	c						; There was an error
	ret


; # flash_program_byte
; #
; # Program a byte of flash memory
; # In:
; #   r0 - Data to write
; #   r1 - ROM page to select
; #   r2 - A7-A0 of address to write data to
; #   r3 - A14-A8 of address to write data to
; # Out:
; #   Carry - Set on error
; ##########################################################################
flash_program_byte:
	acall	flash_send_command+flash_tool_addr_fudge
	mov	a, #flash_command_program			; Program byte
	movx	@dptr, a

	mov	a, r1						; Get ROM page to write
	anl	a, #3						; Make sure it's valid
	mov	mem_page_rdwr, a
	mov	dph, #rdwr_page_latch
	setb	p1.4						; Enable address logic
	movx	@dptr, a
	clr	p1.4

	mov	dph, r3						; Get address MSB
	orl	dph, #0x80					; Make sure we are wrting in the right place
	mov	dpl, r2						; Get address LSB
	mov	a, r0						; Get data
	movx	@dptr, a					; Save data

flash_program_byte_check_loop:
	movx	a, @dptr					; Check if data saved
	mov	b, a						; Save a copy
	xrl	a, r0						; 0 if the data matches
	jz	flash_program_byte_check_loop_finish
	mov	a, b						; Otherwise check for a timeout
	jb	acc.5, flash_program_byte_error
	sjmp	flash_program_byte_check_loop
flash_program_byte_check_loop_finish:

	clr	c
	ret
flash_program_byte_error:
	setb	c
	ret

str_ft_err_page:	.db	"Incorrect page selected.", 0
str_ft_confirm:		.db	"Confirm flash write [Y/N]: ", 0
str_ft_copy_rom:	.db	"Copying system ROM to RAM.", 0
str_ft_device:		.db	"Device: ", 0
str_ft_select_page:	.db	"Select Page To Flash: ", 0
str_ft_erasing_page:	.db	"Erasing Page ", 0
str_ft_flashing_page:	.db	"Flashing Page ", 0
str_ft_reseting_device:	.db	"Reseting device.", 0

.org	locat+0xe00
; ###############################################################################################################
; #                                                       Strings
; ###############################################################################################################

str_okay:		.db	"OK", 0
str_fail:		.db	"Fail", 0

str_menu_quit:		.db	"	0 - Quit", 0

str_i2c_addr:		.db     "I2C Address: ", 0
str_i2c_read:		.db     "Read ", 0
str_i2c_write:		.db     "Write ", 0
str_i2c_register:	.db     "Register: ", 0
str_i2c_data:		.db     "Data: ", 0
str_i2c_enter:		.db     "Enter ", 0

str_bank_header:	.db	"Memory Page Setup:", 0
str_bank_config_psen:	.db	"	1 - Config PSEN", 0
str_bank_config_rdwr:	.db	"	2 - Config RD/WR", 0
str_bank_set_psen:	.db	"	3 - Set PSEN page", 0
str_bank_set_rdwr:	.db	"	4 - Set RD/WR page", 0
str_bank_set_page:	.db	"Enter page bank: ",0
str_bank_set_mode:	.db	"Set mode (0=ROM, 1=RAM, 3=RAM card): ", 0
str_bank_psen_cur_psen:	.db	"Current PSEN config: ", 0
str_bank_psen_cur_rdwr:	.db	"Current RDWR config: ", 0

.org    locat+0xff0                                             ; Make sure dph is not affected by adding to dpl
str_bank_memory_mode1:	.db	" RO", 0xcd	; 'M' + 0x80	; String table to allow easy selection of memory mode (using msb terminated strings)
str_bank_memory_mode2:	.db	" RA", 0xcd	; 'M' + 0x80
str_bank_memory_mode3:	.db	" IN", 0xd6	; 'V' + 0x80
str_bank_memory_mode4:	.db	" CR", 0xc4	; 'D' + 0x80

