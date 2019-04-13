; ###############################################################################################################
; # Contains:
; #
; # Command:
; #	Memory tool
; ###############################################################################################################

; define the storage location
; ##########################################################################
.equ	paulmon2, 0x0000					; location where paulmon2
;.equ	paulmon2, 0x8000					; location where paulmon2
.equ	oysterlib, paulmon2+0x1000				; location of oysterlib
.equ	locat, 0x3000+paulmon2					; location for the library/commands

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
.equ	math_lib, oysterlib+0x06
.equ	memory_lib, oysterlib+0x0A
.equ	i2c_lib, oysterlib+0x14
.equ	rtc_lib, oysterlib+0x2A
.equ	keyboard_lib, oysterlib+0x38
.equ	lcd_lib, oysterlib+0x3E
.equ	misc_lib, oysterlib+0x56

; console lib
.equ	oysterlib_cout, console_lib+0x00
.equ	oysterlib_newline, console_lib+0x02
.equ	oysterlib_cin, console_lib+0x04

; math lib
.equ	packed_bcd_to_hex, math_lib+0x00
.equ	hex_to_packed_bcd, math_lib+0x02

; memory lib
.equ	memory_set_page_psen, memory_lib+0x00
.equ	memory_set_page_rdwr, memory_lib+0x02
.equ	memory_set_mode_psen, memory_lib+0x04
.equ	memory_set_mode_rdwr, memory_lib+0x06
.equ	memory_ramcard_present, memory_lib+0x08

; i2c lib
.equ	i2c_start, i2c_lib+0x00
.equ	i2c_stop_clock_stretch_check, i2c_lib+0x02
.equ	i2c_stop, i2c_lib+0x04
.equ	i2c_write_byte, i2c_lib+0x06
.equ	i2c_ack_check, i2c_lib+0x08
.equ	i2c_write_byte_to_addr, i2c_lib+0x0a
.equ	i2c_write_byte_with_ack_check, i2c_lib+0x0c
.equ	i2c_read_byte, i2c_lib+0x0e
.equ	i2c_master_read_ack, i2c_lib+0x10
.equ	i2c_read_byte_from_addr, i2c_lib+0x12
.equ	i2c_read_device_register, i2c_lib+0x14

; rtc lib
.equ	rtc_get_config, rtc_lib+0x00
.equ	rtc_get_alarm_config, rtc_lib+0x02
.equ	rtc_set_config, rtc_lib+0x04
.equ	rtc_set_alarm_config, rtc_lib+0x06
.equ	rtc_init, rtc_lib+0x08
.equ	rtc_get_datetime, rtc_lib+0x0a
.equ	rtc_set_datetime, rtc_lib+0x0c

; keyboard lib
.equ	keyboard_scan, keyboard_lib+0x00
.equ	keyboard_reset, keyboard_lib+0x02
.equ	keyboard_wait_for_keypress, keyboard_lib+0x04

; lcd lib
.equ	lcd_off, lcd_lib+0x00
.equ	lcd_init, lcd_lib+0x02
.equ	lcd_clear_screen, lcd_lib+0x04
.equ	lcd_clear_screen_from_position, lcd_lib+0x06
.equ	lcd_new_line_scroll_and_clear, lcd_lib+0x08
.equ	lcd_print_character, lcd_lib+0x0a
.equ	lcd_set_glyph_position, lcd_lib+0x0c
.equ	lcd_pstr, lcd_lib+0x0e
.equ	lcd_set_backlight_on, lcd_lib+0x10
.equ	lcd_set_backlight_off, lcd_lib+0x12
.equ	lcd_set_contrast_inc, lcd_lib+0x14
.equ	lcd_set_contrast_dec, lcd_lib+0x16

; misc lib
.equ	piezo_beep, misc_lib+0x00
.equ	piezo_pwm_sound, misc_lib+0x02
.equ	sdelay, misc_lib+0x04
.equ	battery_check_status, misc_lib+0x06

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
.equ	rb0r1, 0x01
.equ	rb0r2, 0x02

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
.equ	keymap_offset, 0x60					; Used to select the correct keymap

; Bit RAM definitions
; ##########################################################################
.flag	use_oysterlib, 0x20.0
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
.db	254,'@',0,0		; id (254=cmd)
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
	cjne	a, #'3', i2c_tool_menu_quit
	acall	i2c_tool_write_register
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

str_i2c_header:		.db	"I2C Tool:", 0
str_i2c_bus_scan:	.db	"	1 - Bus Scan", 0
str_i2c_read_reg:	.db	"	2 - Read Register", 0
str_i2c_write_reg:	.db	"	3 - Write Register", 0


;---------------------------------------------------------;
;                                                         ;
;                       Screen tool                       ;
;                                                         ;
;---------------------------------------------------------;

.org    locat+0x300
.db     0xA5,0xE5,0xE0,0xA5	; signiture bytes
.db     254,'#',0,0		; id (254=cmd)
.db     0,0,0,0			; prompt code vector
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; reserved
.db     0,0,0,0			; user defined
.db     255,255,255,255		; length and checksum (255=unused)
.db     "Screen tool",0
.org    locat+0x340		; executable code begins here


screen_tool:
	lcall	oysterlib_newline

screen_tool_menu:
	mov	dptr, #str_screen_header			; Print menu
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_screen_on
	lcall	pstr
	lcall	oysterlib_newline
	mov	dptr, #str_screen_off
	lcall	pstr
	lcall	oysterlib_newline
;	mov	dptr, #str_menu_quit
;	lcall	pstr
;	lcall	oysterlib_newline
	lcall	oysterlib_cin					; Get menu selection
	lcall	oysterlib_newline

screen_tool_menu_1:
	cjne	a, #'1', screen_tool_menu_2
	lcall	lcd_init
	ljmp	lcd_set_backlight_on
screen_tool_menu_2:
	cjne	a, #'2', screen_tool_menu_else
	lcall	lcd_off
	ljmp	lcd_set_backlight_off
;screen_tool_menu_quit:
;	cjne	a, #'0', screen_tool_menu_else
;	ret
screen_tool_menu_else:
	sjmp	screen_tool

str_screen_header:	.db	"Screen Tool:", 0
str_screen_on:		.db	"	1 - On", 0
str_screen_off:		.db	"	2 - Off", 0


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

