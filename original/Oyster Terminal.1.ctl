;
; D52.CTL - Sample Control File for D52
;
;   Control codes allowed in the CTL file:
;
;  A - Address		Specifies that the word entry is the address of
;			something for which a label should be generated.
;
;  B - Byte binary	Eight bit binary data (db).
;
;  C - Code		Executable code that must be analyzed.
;
;  F - SFR label	Specify name for SFR.
;
;  I - Ignore		Treat as uninitialized space. Will not be dis-
;			assembled as anything unless some other valid
;			code reaches it.
;
;  K - SFR bit		Specify name for SFR bit.
;
;  L - Label		Generate a label for this address.
;
;  M - Memory		Generate a label for bit addressable memory.
;
;  N - No label		Suppress label generation for an address.
;
;  O - Offset		Specify offset to add to addresses.
;
;  P - Patch		Add inline code (macro, for example)
;
;  R - Register		Specify name for register
;			(instead of rb2r5 for example).
;
;  S - Symbol		Generate a symbol for this value.
;
;  T - Text		ASCII text (db).
;
;  W - Word binary	Sixteen bit binary data (dw).
;
;  X - Operand name	Specify special name for operand.
;
;  Y - Operand name	Specify special name for operand, suppress EQU generation.
;
;  # - Comment		Add header comment to output file.
;
;  ! - Inline comment	Add comment to end of line.
;
;  The difference between labels and symbols is that a label refers
;  to a referenced address, whereas a symbol may be used for 8 or 16
;  bit immediate data. For some opcodes (eg: mov r2,#xx) only the symbol
;  table will be searched. Other opcodes (eg: mov dptr,#) will search
;  the label table first and then search the symbol table only if the
;  value is not found in the label table.
;
;  Values may specify ranges, ie:
;
;	A 100-11f specifies a table of addresses starting at address
;		0x100 and continuing to address 0x11f.
;
;	T 100-120 specifies that addresses 0x100 through (and including)
;		address 0x120 contain ascii text data.
;
;  Range specifications are allowed for codes A, B, C, I, T, and W, but
;  obviously don't apply to codes F, K, L, R, and S.
;
;---------------------------------------------------------------------------
;
; labels for interrupt vectors
;
;L 0	reset
;L 3	ie0vec
;L b	tf0vec
;L 13	ie1vec
;L 1b	tf1vec
;L 23	servec
;
; tell D52 to disassemble code at interrupt vector locations
;
;C 3	; external 0 interrupt
;C b	; timer 0 overflow interrupt
;C 13	; external 1 interrupt
;C 1b	; timer 1 overflow interrupt
;C 23	; serial xmit/recv interrupt
;
; end of control file
;

; Memory bits
m	09		21.1_key_map1
m	0a		21.2_key_map2
m	0b		21.3_key_1char		; Swaps case for 1 character
m	0c		21.4_key_ulcse		; Indicates upper or lower case
m	0d		21.5_key???
m	18		23.0_exec_prog		; Running from program memory
m	1a		23.2_no_zero
m	1c		23.4_copy_ram
m	24		24.4_bat_low
m	29		25.1_ram_fail
m	2a		25.2_gly_dhoh		; glyph double height other half
m	3c		27.4_gly_dblwdh		; glyph double width
m	3d		27.5_gly_dblhgt		; glyph double height
m	3e		27.6_gly_invert
m	40		28.0_tmp1

; Registers
r	1c		1ch_lcd_prop		; lcd properties (0bxxxxcccc - c=contrast)
r	20		20h_uart_config
r	3a		3ah_cmd_rtn_val
r	3b		3bh_cmd_rtn_sts
r	3f		3fh_cmdtmp_idxl
r	40		40h_cmdtmp_idxh
r	41		41h_cmdbuf_idxl
r	42		42h_cmdbuf_idxh
r	43		43h_cmdtmp_char		; keyboard buffer current character
r	46		46h_cmdbuf_char		; keyboard buffer current character
r	55		55h_line_offset

; SFR bits
k	96		p1.6_i2c_scl
k	97		p1.7_i2c_sda

#	0		Address that execution resumes after reset
c	0-2
l	0		0000h_start_exec

#	3		Interrupt service routine: External interrupt 0
c	3-9		; external 0 interrupt
l	3		isr_ie0
n	6		; Suppress label generation
l	7		isr_ie0_jmp
i	a		; ignore data

#	b		Interrupt service routine: Timer 0 overflow
c	b-11		; timer 0 overflow interrupt
l	b		isr_tf0
i	12		; ignore data

#	13		Interrupt service routine: External interrupt 1
c	13		; external 1 interrupt
l	13		isr_ie1
l	19		isr_ie1_rtn

#	1b		Interrupt service routine: Timer 1 overflow
i	1b-22		; ignore data

#	23		Interrupt service routine: Serial Tx/Rx interrupt
c	23-41		; serial xmit/recv interrupt
l	23		isr_serial_txrx
l	3b		isr_serial_txrx_1

#	42		External interrupt 1, service routine
l	42		isr_ie1_srv
l	4e		isr_ie1_srv_1

#	51		Interrupt service routine: Serial Rx interrupt
l	51		isr_serial_rx
l	6a		isr_serial_rx_1
l	6f		isr_serial_rx_rtn
l	74		isr_serial_rx_2
l	8c		isr_serial_rx_3
l	8e		isr_serial_rx_4
l	97		isr_serial_rx_5
l	a3		isr_serial_rx_6

#	b9		Timer 0 interrupt, service routine
l	b9		isr_tf0_srv

c	10c-126
l	10c		isr_tf0_srv_keyboard_setup
!	10e		Select alternate register banks
!	113		Keyboard address
!	119		Number of column bits to cycle through
!	11b		Initial config of column matrix
l	11d		isr_tf0_srv_keyboard_column_scan
!	11d		Load config
!	11e		Select next column
!	120		Wait for lines to 'settle'
!	121		p1.5 is connected to the interupt out of the PCF8574 bus expander

c	128-155
l	128		isr_tf0_srv_keyboard_read
!	128		Reverse previous rotate
!	129		Scan was done inverted
!	12a		Select keyboard column matrix
!	12d		Wait for lines to 'settle'
!	137		Save 'adjusted' column selection (rb1r7 - 00cccXXX)
!	13c		I2C (0x41 -> 01000001 -> Addr 0x20, read -> U13 PCF8574 bus expander)
!	153		Number of row bits to cycle through
l	155		isr_tf0_srv_keyboard_read_row_cycle
!	155		Row selection bits

l	173		isr_tf0_srv_exit

l	17c		isr_tf0_srv_keyboard_read_make_keycode
!	17d		rb1r7 - 00cccXXX
!	17f		Add row selection
!	180		rb1r7 - 00cccrrr
l	18a		isr_tf0_srv_keyboard_read_make_keycode_THIS_IS_JUST_FOR_THE_COMPARISON

l	19e		isr_tf0_srv_keyboard_read_reset
l	1a3		isr_tf0_srv_keyboard_read_clear_int
!	1a6		I2C (0x41 -> 01000001 -> Addr 0x20, read -> U13 PCF8574 bus expander)
l	1bb		isr_tf0_srv_keyboard_read_exit

l	2b6		print_character_preserve_A

#	2be	#
#	2be	############################################################
#	2be	# command_buffer_save_current
#	2be	#
l	2be		command_buffer_save_current

#	2c8	#
#	2c8	############################################################
#	2c8	# command_buffer_reload_current
#	2c8	#
l	2c8		command_buffer_reload_current

#	2d2	#
#	2d2	############################################################
#	2d2	# command_buffer_get_char_inc_index
#	2d2	#
l	2d2		command_buffer_get_char_inc_index
!	2d2		Check if we're running from memory
!	2d5		Select temporary command buffer (dph)
!	2d8		Get index in command buffer
!	2da		Get character
l	2de		command_buffer_get_char_inc_index_from_program_store

l	2f4		command_buffer_get_char_inc_index_get_value
!	2f4		Save character to register
!	2f6		Increment index
!	2fa		Check for rollover of dpl
!	2fd		Increment dph
l	2ff		command_buffer_get_char_inc_index_finish

#	340	#
#	340	# CLI - rem string
#	340	#
#	340	############################################################
#	340	# cmd_call_rem
#	340	#
l	340		cmd_call_rem

!	4ee		'$'
!	501		'"'

l	67c		error_print_error
l	67f		str_error
t	67f-683		; ' ERRO'
b	684		; 'R' | 0x80
!	684		'R' | 0x80
l	685		error_print_error_continue

l	6a6		error_print_expr
l	6a9		str_expr
t	6a9-6ac		; 'EXPR'
b	6ad		; '.' | 0x80
!	6ad		'.' | 0x80

l	6b0		error_print_stack
l	6b3		str_stack
t	6b3-6b6		; 'STAC'
b	6b7		; 'K' | 0x80
!	6b7		'K' | 0x80

l	6ba		error_print_err_equ
l	6bd		str_err_equ
t	6bd-6ca		; '!! ERROR =   !'
b	6cb		; '!' | 0x80
!	6cb		'!' | 0x80
n	6d4

l	6de		error_print_syntax
l	6e1		str_syntax
t	6e1-6e5		; 'SYNTA'
b	6e6		; 'X' | 0x80
!	6e6		'X' | 0x80

l	6e9		error_print_overflow
l	6ec		str_overflow
t	6ec-6f2		; 'OVERFLO'
b	6f3		;  'W' | 0x80
!	6f3		'W' | 0x80

l	7d5		str_line
t	7d5-7d8		; 'Line'
b	7d9		; ' ' | 0x80
!	7d9		' ' | 0x80

l	7e0		str_comma
t	7e0		; ','
b	7e1		; ' ' | 0x80
!	7e1		' ' | 0x80

#	853	#
#	853	############################################################
#	853	# process_cmd_start
#	853	#
l	853		process_cmd_start
l	862		process_cmd_start_error
l	865		process_cmd_start_error_cjne
l	86b		process_cmd_next_cmd
!	86e		Increment command index
!	871		Add command size to current offset 
!	874		If there's no carry, just process next command
!	877		Otherwise increment dph
#	87b	############################################################
#	87b	# process_cmd
#	87b	#
#	87b	# r2 - command index
l	87b		process_cmd
!	87b		Clear A
!	87c		Move size of string to check into A
!	87d		Have we reached the end of the table
!	87f		Store size of command string in r1
l	880		process_cmd_char_loop
!	882		Get letter from command string
l	88a		process_cmd_next_char
!	88d		'.'
l	890		process_cmd_jump_table
!	891		Get command index
!	892		Multiply by 2 (size of array items)
!	893		Store for later
!	894		Array of pointers to command handlers
!	897		First byte of address (MSB)
!	898		Store for later
!	899		Increment for next byte
!	89a		Reload offset
!	89b		Second byte of address (LSB)
!	89c		Push LSB on to stack
!	89e		Push MSB on to stack (this will be popped as the PC on the next return)
l	8a3		process_cmd_error

l	8a9		cmd_table
b	8a9		; 4
t	8aa-8ad		; GOTO
b	8ae		; 5
t	8af-8b3		; GOSUB
b	8b4		; 5
t	8b5-8b9		; PRINT
b	8ba		; 2
t	8bb-8bc		; IF
b	8bd		; 5
t	8be-8c2		; INPUT
b	8c3		; 6
t	8c4-8c9		; RETURN
b	8ca		; 4
n	8cd		; Suppress label generation
t	8cb-8ce		; LINK
b	8cf		; 3
t	8d0-8d2		; FOR
b	8d3		; 4
n	8d6		; Suppress label generation
t	8d4-8d7		; NEXT
b	8d8		; 3
n	8d9		; Suppress label generation
n	8da		; Suppress label generation
n	8db		; Suppress label generation
t	8d9-8db		; SDT
b	8dc		; 3
n	8dd		; Suppress label generation
n	8de		; Suppress label generation
t	8dd-8df		; DDT
b	8e0		; 4
n	8e1		; Suppress label generation
n	8e3		; Suppress label generation
t	8e1-8e4		; IRAM
b	8e5		; 3
n	8e6		; Suppress label generation
n	8e8		; Suppress label generation
t	8e6-8e8		; BIT
b	8e9		; 5
n	8ea		; Suppress label generation
n	8ec		; Suppress label generation
n	8ee		; Suppress label generation
t	8ea-8ee		; FLASH
b	8ef		; 3
n	8f0		; Suppress label generation
n	8f2		; Suppress label generation
t	8f0-8f2		; ROM
n	8f3		; Suppress label generation
b	8f3		; 4
n	8f4		; Suppress label generation
n	8f5		; Suppress label generation
n	8f6		; Suppress label generation
n	8f7		; Suppress label generation
t	8f4-8f7		; SRAM
n	8f8		; Suppress label generation
b	8f8		; 3
n	8f9		; Suppress label generation
n	8fb		; Suppress label generation
t	8f9-8fb		; REM
n	8fc		; Suppress label generation
b	8fc		; 2
n	8fd		; Suppress label generation
n	8fe		; Suppress label generation
t	8fd-8fe		; ES

n	8ff		; Suppress label generation
b	8ff		; 2
n	901		; Suppress label generation
t	900-901		; DS
b	902		; 2
n	903		; Suppress label generation
t	903-904		; BS
n	905		; Suppress label generation
b	905		; 3
t	906-908		; CLS
b	909		; 4
n	90d		; Suppress label generation
t	90a-90d		; FILE
b	90e		; 3
n	90f		; Suppress label generation
n	911		; Suppress label generation
t	90f-911		; GET
n	912		; Suppress label generation
b	912		; 3
n	913		; Suppress label generation
n	914		; Suppress label generation
n	915		; Suppress label generation
t	913-915		; PUT
n	916		; Suppress label generation
b	916		; 4
n	917		; Suppress label generation
n	918		; Suppress label generation
n	919		; Suppress label generation
t	917-91a		; COPY
n	91b		; Suppress label generation
b	91b		; 3
n	91c		; Suppress label generation
n	91d		; Suppress label generation
t	91c-91e		; BCD
b	91f		; 4
n	920		; Suppress label generation
n	921		; Suppress label generation
n	922		; Suppress label generation
n	923		; Suppress label generation
t	920-923		; PACK
n	924		; Suppress label generation
b	924		; 6
n	926		; Suppress label generation
n	928		; Suppress label generation
n	929		; Suppress label generation
n	92a		; Suppress label generation
t	925-92a		; UNPACK
n	92b		; Suppress label generation
b	92b		; 2
n	92c		; Suppress label generation
t	92c-92d		; SI
b	92e		; 3
t	92f-931		; CRC
b	932		; 5
t	933-937		; INKEY
b	938		; 6
t	939-93e		; SEARCH
b	93f		; 6
t	940-945		; WAKEUP
b	946		; 3
t	947-949		; LET
b	94a		; 3
t	94b-94d		; END
b	94e		; 5
t	94f-953		; ERASE
b	954		; 5
t	955-959		; POWER
b	95a		; 1
b	95b		; 0xff
b	95c		; 3
t	95d-95f		; RUN
b	960		; 1
b	961		; 0xff
b	962		; 4
t	963-966		; TEST
b	967		; 4
t	968-96b		; TIME
b	96c		; 5
t	96d-971		; SETUP
b	972		; 4
t	973-976		; CALL
b	977		; 7
t	978-97e		; ENDPROC
b	97f		; 2
t	980-981		; ON
b	982		; 6
t	983-988		; LPRINT
b	989		; 5
t	98a-98e		; INIIC
b	98f		; 6
n	993		; Suppress label generation
t	990-995		; OUTIIC
b	996		; 1
b	997		; 0xff
b	998		; 5
t	999-99d		; ENTER
b	99e		; 4
t	99f-9a2		; SORT
b	9a3		; \0

l	9a4		cmd_jump_table
b	9a4-a0b
!	9a5		GOTO
!	9a7		GOSUB
!	9a9		PRINT
!	9ab		IF
!	9ad		INPUT
!	9af		RETURN
!	9b1		LINK
!	9b3		FOR
!	9b5		NEXT
!	9b7		SDT
!	9b9		DDT
!	9bb		IRAM
!	9bd		BIT
!	9bf		FLASH
!	9c1		ROM
!	9c3		SRAM
!	9c5		REM
!	9c7		ES
!	9c9		DS
!	9cb		BS
!	9cd		CLS
!	9cf		FILE
!	9d1		GET
!	9d3		PUT
!	9d5		COPY
!	9d7		BCD
!	9d9		PACK
!	9db		UNPACK
!	9dd		SI
!	9df		CRC
!	9e1		INKEY
!	9e3		SEARCH
!	9e5		WAKEUP
!	9e7		LET
!	9e9		END
!	9eb		ERASE
!	9ed		POWER
!	9ef		N/A
!	9f1		RUN
!	9f3		N/A
!	9f5		TEST
!	9f7		TIME
!	9f9		SETUP
!	9fb		CALL
!	9fd		ENDPROC
!	9ff		ON
!	a01		LPRINT
!	a03		INIIC
!	a05		OUTIIC
!	a07		N/A
!	a09		ENTER
!	a0b		SORT

l	a48		str_ok
t	a48-a49		; 'OK'
b	a4a		; '>' | 0x80
!	a4a		'>' | 0x80

l	b10		cmd_call_let
l	bf4		cmd_call_goto
l	bfa		cmd_call_gosub
l	c00		cmd_call_print
l	ce7		cmd_call_end
l	c79		cmd_call_if
l	ca0		cmd_call_input
l	ca6		cmd_call_link
l	ccf		cmd_call_for
l	cd5		cmd_call_next
l	cdb		cmd_call_sdt
l	ce1		cmd_call_ddt


l	cf3		str_man_code
t	cf3-cfb		; 'MAN CODE:'
b	cfc		; ' ' | 0x80
!	cfc		' ' | 0x80

l	d08		str_dev_code
t	d08-d13		; 'DEVICE CODE:'
b	d14		; ' ' | 0x80
!	d14		' ' | 0x80

l	d30		cmd_call_flash

l	d43		cmd_call_erase
l	d95		print_erasing
l	d98		str_erasing
t	d98-da3		; '!! ERASING !'
b	da4		; '!' | 0x80
!	da4		'!' | 0x80

l	e1e		cmd_call_power
l	e27		cmd_call_run
l	e2d		cmd_call_iram
l	e34		cmd_call_bit
l	e38		cmd_call_sram
l	e3c		cmd_call_rom
l	e46		cmd_call_es
l	e56		cmd_call_ds
l	e5a		cmd_call_bs

l	e69		cmd_call_test
!	e6d		Check the next command character: 'S'

l	e7f		cmd_call_time
l	e87		cmd_call_setup
l	e8d		cmd_call_cls
l	e92		cmd_call_file
l	e98		cmd_call_get
l	e9e		cmd_call_put
l	ea4		cmd_call_copy
l	f9e		cmd_call_bcd
l	fa4		cmd_call_pack
l	faa		cmd_call_unpack
l	fb3		cmd_call_si

l	103e		cmd_call_crc

l	1048		command_buffer_get_next_character_comma_null
!	104a		Check for comma
l	1051		command_buffer_get_next_character_comma_null_rtn

l	1058		cmd_call_search

#	105e	#
#	105e	# CLI - inkey
#	105e	# 
#	105e	# waits for a keypress, returns value
#	105e	############################################################
#	105e	# cmd_call_inkey
#	105e	#
l	105e		cmd_call_inkey
l	1061		cmd_call_return_A

#	1069	#
#	1069	# CLI - wakeup month,day,hour,minute
#	1069	#
#	1069	############################################################
#	1069	# cmd_call_wakeup
l	1069		cmd_call_wakeup
!	106c		0xfb - Alarm - Month
l	107b		cmd_call_wakeup_arg_day
!	107b		0xfc - Alarm - Day of the month
l	1087		cmd_call_wakeup_arg_hour
!	1087		0xfd - Alarm - Hour
l	1091		cmd_call_wakeup_arg_minute
!	1091		0xfe - Alarm - Minute
l	109b		cmd_call_wakeup_rtc_alarm_set
!	109b		Alarm hundredths of a second
!	109d		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	10a0		Select register
!	10a4		Clear hundredths of a second
!	10a8		Clear seconds
!	10ab		Minutes
!	10ae		Get value
!	10af		Convert for RTC register
!	10b2		Save value
!	10b5		0xfd - Hours
!	10b7		Get value
!	10b8		Convert for RTC register
!	10bb		Save value
!	10be		0xfc - Day of the month
!	10c0		Get value
!	10c1		Lose year data
!	10c3		Convert for RTC register
!	10c6		Save value
!	10c9		0xfb - Month
!	10cb		Get value
!	10cc		Convert for RTC register
!	10cf		Save value
!	10d5		Alarm control register
!	10d7		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	10da		Select register
!	10dd		0b10010101 - Alarm interrupt enable, no timer alarm, daily alarm, timer flag: no interrupt, timer function: days
!	10e2		0b10110101 - Alarm interrupt enable, no timer alarm, dated alarm, timer flag: no interrupt, timer function: days
l	10e4		cmd_call_wakeup_rtc_alarm_set_finish
l	10ed		cmd_call_wakeup_arg_save

l	10fb		cmd_call_call
l	1101		cmd_call_endproc
l	1107		cmd_call_on
l	110d		cmd_call_lprint
l	1113		cmd_call_iniic
l	112a		cmd_call_outiic
l	1148		cmd_call_enter
l	1155		cmd_call_sort

!	11ee		'+'
!	11f9		'-'
!	1204		'|'
!	120f		'\'

!	1223		'*'
!	1231		'/'
!	123f		'&'
!	124d		'%'

!	125e		'-'
!	1272		'N'

#       155f
#       155f	############################################################
#       155f	# lcd_init
l	155f		lcd_init
!	155f		Enable clock
!	1561		Select LCD (command register)
l	1564		lcd_init_subscreen
!	1564		LCD command 'Turn on'
!	1566		Send command
!	1569		LCD command 'Display start line' #0
!	156b		Send command
!	1570		Select next subscreen
!	1574		Initialise next subscreen?
#	157a	############################################################
#	157a	# lcd_clear_screen
l	157a		lcd_clear_screen
#	158a	############################################################
#	158a	# lcd_clear_screen_from_position
#	158a	#
#	158a	# rb1r6 - Position to start from (b0-b4 - column, b5-7 - row)
l	158a		lcd_clear_screen_from_position
!	158a		Enable clock
!	158c		Save registers
!	1596		Get position to clear from
!	1598		Get column data
!	159a		Glyph block size
!	159d		Calculate column offset
!	159e		Save result
!	15a0		Get subscreen selection
!	15a3		Shift into correct bit position
!	15a6		LCD subscreen address
!	15aa		Subscreen column position
!	15ac		r6 = subscreen column position
!	15ad		LCD command 'Set Address'
!	15af		Send command
!	15b2		Get position to clear from
!	15b4		Get row data
!	15b7		Equivalent to right shift x5
!	15c2		r7 = subscreen row selection
!	15c3		LCD command 'Set Page'
!	15c5		Send command
!	15c8		Select LCD (data register)
!	15ce		Calculate remaining columns
!	15cf		r6 = remaing columns in subscreen
l	15d2		lcd_clear_subscreen_remaining_line
!	15d2		Send command
l	15d7		lcd_clear_subscreen_check_next
!	15d7		Swap to LCD command register
!	15dc		Increment subscreen
!	15e0		Check whether there are subscreens remaining for this line
!	15e4		Increment subscreen row selection
!	15e7		Store incremented subscreen row selection
!	15e8		Select LCD command register
!	15ed		Increment stored position (sets carry on screen end)
l	15f3		lcd_clear_subscreen_next_start
!	15f3		LCD command 'Set Address' #0
!	15f5		Send command
!	15f8		Get subscreen row selection
!	15f9		LCD command 'Set Page'
!	15fb		Send command
!	15fe		Swap to LCD data register
!	1601		Number of characters to clear (64 - full line)
l	1604		lcd_clear_subscreen_remaining_line2
l	160b		lcd_clear_exit
!	160b		Restore registers
!	1615		Disable clock

c	1618-161d
l	1618		move_A_2_dptr_ExtRAM

c	161e-1623
l	161e		move_dptr_ExtRAM_2_A

#	1624	#
#	1624	############################################################
#	1624	# lcd_new_line_scroll_and_clear
#	1624	#
#	1624	# 55h_line_offset - current line that the lcd starts on
l	1624		lcd_new_line_scroll_and_clear
!	1628		Save registers
!	162a		Get current lcd display start line
!	162c		Move the display up 8 lines
!	162e		Wrap on >=64
!	1630		Save result
!	1632		Enable clock
!	1634		Select LCD command register subscreen 0
l	1637		lcd_new_line_scroll_and_clear_loop
!	1639		LCD command 'display start line'
!	163b		Send command
!	163e		Get currently selected subscreen
!	1640		Select next subscreen
!	1642		Address subscreen
!	1644		Loop until we have iterrated over all subscreens
!	164a		Last character line, first character
!	164d		Clear last character line of the lcd

#	1663	#
#	1663	############################################################
#	1663	# print_character
#	1663	#
#	1663	# A - character to print
#	1663	# rb1r6 - Glyph position (b0-b4 - column, b5-7 - row)
#	1663	# 27.5_gly_dblhgt - Double the height of the glyph
#	1663	# 27.4_gly_dblwdh - Double the width of the glyph
l	1663		print_character
!	1663		Force 7-bit ASCII
l	166b		print_character_wait_loop
l	1670		print_character_alt
!	1670		Force 7-bit ASCII
l	1672		print_character_start
!	167a		Save registers
!	167c		Clear glyph height doubler 'other half' flag
!	1680		Print glyph
!	1686		Get glyph position
!	1688		Increment line number
l	1694		print_character_other_half
l	169f		print_character_setup_double_width
l	16a3		print_character_inc_char_position
!	16a3		Increment character position
!	16a7		Extract column data
!	16a9		Off the end of the screen?
!	16ab		Do again for double width
l	16b0		print_character_inc_character_line
!	16b0		Get character position
!	16b2		Extract row data
!	16b5		Equivalent rr x5
!	16b8		Equivalent line number as we are off the bottom of the screen
l	16ba		print_character_inc_character_line_have_line_no
l	16c9		print_character_inc_character_line_do
l	16d4		print_character_generate_new_line
l	16e2		print_character_exit

#	16ef
#	16ef	############################################################
#	16ef	# print_glyph_data
#	16ef	#
#	16ef	# A - ASCII code
#	16ef	# rb1r6 - Glyph position (b0-b4 - column, b5-7 - row)
#	16ef	# 51h - MSB of pointer to font table
l	16ef		print_glyph_data
l	16fd		print_glyph_data_start
!	16fd		Pop ASCII character
!	16ff		We're going to be writing display data (Port 2, bit 6 -> LCD D/I)
!	1702		Store LCD control line cfg in r2
!	1704		Number of bytes per character (to calc offset)
!	1707		Offset = ASCII code * bytes per character (A = LSB, B = MSB)
!	1708		Store result as lower half of dptr
!	170a		Get the MSB of the offset
!	170c		Add MSB and the contents of register 51h (0x3d)
!	170e		During the loop dph is stored in r1
!	170f		Number of bytes per character (bytes to copy)
l	1712		print_glyph_data_get_byte
!	1712		r1 contains dph of glyph data
!	1715		Get glyph data
;!	1716
!	1719		Copy data from external RAM
l	171a		print_glyph_data_skip_ram_load
!	171a		Increment pointer
!	171b		Store dph for later
!	171d		Double the size of the glyph?
!	1720		Which half of glyph data are we working on?
l	1724		print_glyph_data_double_size
l	172e		print_glyph_data_is_inverted
!	172e		Is the glyph data inverted?
!	1731		Invert glyph data
l	1732		print_glyph_data_processed
!	1732		Copy the double width parameter so we can iterate with it
l	1736		print_glyph_data_loop_double_width
!	1738		LCD enable high
!	173a		LCD control lines (CSA / CSB / D|I)
!	173c		LCD data
!	173d		LCD enable low
!	173f		Increment screen x offset store
!	1740		Check whether the 'cursor' is off the current subscreen (64x64 bits)
!	1744		Check whether we have moved off the whole screen
l	174a		print_glyph_data_inc_subscreen
!	174a		Increment subscreen
!	174c		Store LCD control config
l	174d		print_glyph_data_continue
!	174f		Do we need to repeat the current bit data
!	1752		Is there more data to write

l	1756		conversion_table_double_size
b	1756		; 0x00 '00000000'
b	1757		; 0x03 '00000011'
b	1758		; 0x0c '00001100'
b	1759		; 0x0f '00001111'
b	175a		; 0x30 '00110000'
b	175b		; 0x33 '00110011'
b	175c		; 0x3c '00111100'
b	175d		; 0x3f '00111111'
b	175e		; 0xc0 '11000000'
b	175f		; 0xc3 '11000011'
b	1760		; 0xcc '11001100'
b	1761		; 0xcf '11001111'
b	1762		; 0xf0 '11110000'
b	1763		; 0xf3 '11110011'
b	1764		; 0xfc '11111100'
b	1765		; 0xff '11111111'

#	1766
#	1766	############################################################
#	1766	# lcd_set_glyph_position
#	1766	#
#	1766	# rb1r6 - Glyph position (b0-b4 - column, b5-7 - row)
#	1766	# 55h - line_offset
l	1766		lcd_set_glyph_position
!	1766		Enable clock
!	1768		Get glyph position
!	176a		Get column data
!	176c		Size of glyph
!	1770		Save result for later
!	1772		We're only interested in the last 2 bits, subscreen selection
!	1774		Shift into correct position
!	1775		Shift into correct position
!	1776		Select LCD
!	1778		Store subscreen selection, get original result
!	177a		Get column selection
!	177c		Save subscreen column postion
!	177d		LCD command 'Set Address'
!	177f		Send command
!	1782		Get glyph position
!	1784		Get row data
!	1786		Equivalent of right shift x5
!	1788		Save row position
!	1792		LCD command 'Set Page'
!	1794		Send command
!	1797		Check whether this glyph will overlap subscreens
l	179a		lcd_set_glyph_position_subscreen_check_overlap
!	179c		Get selected subscreen
!	179e		Check whether we are at the screen edge
l	17a3		lcd_set_glyph_position_exit
!	17a3		Disable clock
l	17a6		lcd_set_glyph_position_subscreen_config_next
!	17a8		Select next screen
!	17ac		Send command 'Set Page'
!	17af		LCD command 'Set Address' (start of screen)
!	17b1		Send command
!	17b6		Disable clock

#	1a33	#
#	1a33	############################################################
#	1a33	# save_data_2_i2c_addr_0x30
#	1a33	#
#	1a33	# A - data to write to device at address 0x30 ???
l	1a33		save_data_2_i2c_addr_0x30
!	1a33		Save data
!	1a34		Number of times to retry (250)
l	1a36		save_data_2_i2c_addr_0x30_retry
l	1a39		save_data_2_i2c_addr_0x30_loop_4_
l	1a55		save_data_2_i2c_addr_0x30_write_failed

#	1bbe	#
#	1bbe	############################################################
#	1bbe	# keyboard_process_keycode
#	1bbe	#
l	1bbe		keyboard_process_keycode
l	1bec		keyboard_process_keycode_get_character
l	1bfe		keyboard_process_keycode_check_character
!	1c00		Check for control key code
l	1c06		keyboard_process_keycode_less_than_A
l	1c0b		keyboard_process_keycode_more_than_Z
!	1c0d		Check for 1 character case shift
!	1c10		Shift character case
l	1c12		keyboard_process_keycode_shift_character_case
!	1c12		Check for character case shift
!	1c15		Shift character case
l	1c17		keyboard_process_keycode_finish
l	1c19		keyboard_process_keycode_save_character

#	1e26	#
#	1e26	############################################################
#	1e26	# print_string_term_on_msb
#	1e26	#
#	1e26	# Stack - dptr -> string to print (final character has bit 7 set), followed by code to execute on return
#	1e26	# rb1r6 - Glyph position (b0-b4 - column, b5-7 - row)
l	1e26		print_string_term_on_msb_with_clear_screen
l	1e29		print_string_term_on_msb
!	1e29		Get character pointer
!	1e2e		Get character
!	1e2f		Increment pointer, next character or return address
!	1e32		Save pointer
!	1e3b		Check whether this is the last character

#	1e50	#
#	1e50	############################################################
#	1e50	# print_number_dec_100
#	1e50	#
#	1e50	# A - number to print as decimal (3 digits)
#	1e50	# 23.2_no_zero - suppresses leading zeros
l	1e50		print_number_dec_100__no_leading_zero
l	1e52		print_number_dec_100
#	1e5f	############################################################
#	1e5f	# print_number_hex
#	1e5f	#
#	1e5f	# A - number to print as hexadecimal
#	1e5f	# 23.2_no_zero - suppresses leading zeros
l	1e5f		print_number_hex_no_leading_zero
#	1e66	############################################################
#	1e66	# print_number_dec
#	1e66	#
#	1e66	# A - number to print as decimal
#	1e66	# 23.2_no_zero - suppresses leading zeros
l	1e66		print_number_dec_no_leading_zero
l	1e68		print_number_dec
l	1e6b		print_number_do_conversion
!	1e6e		Print first digit
!	1e71		Get second digit
l	1e75		print_number_single_digit
!	1e75		Make sure value is within range
l	1e7c		print_number_print_character
!	1e7e		Get offset to character in number_2_char_table
!	1e80		Get character represented by number
l	1e87		print_number_exit
l	1e8b		number_2_char_table
t	1e8b-1e9a	; '0123456789ABCDEF'

#	1ef7	#
#	1ef7	############################################################
#	1ef7	# power_battery_status
#	1ef7	#
#	1ef7	# Returns the battery charge status in A
l	1ef7		power_battery_status

!	1f2c		Save registers

!	1f30		I2C (0xA2 -> 10100010 -> Addr 0x51, write -> U4 PCF8583 Clock)
!	1f57		I2C (0xA2 -> 10100010 -> Addr 0x51, write -> U4 PCF8583 Clock)

!	1f8f		Control/status register
!	1f90		I2C (0xA2 -> 10100010 -> Addr 0x51, write -> U4 PCF8583 Clock)
!	1f93		Select register
!	1f96		0b00100100 - Event counter mode, alarm registers enable
!	1f98		Loop count
l	1f9a		system_u4counter_setup_and_clear
!	1f9d		Clear digit registers
!	1fa0		Alarm control register - Alarm flag no interrupt, no timer alarm, no event alarm, timer flag no interrupt, timer function: 1000000
!	1fb0		Restore registers

i	1ffc-1fff
l	1fff		firmware_rev
t	1fff

l	2000		keycode_2_character_table1
b	2000-2001	; 'A'
!	2001		'A'
b	2002-2003	; 'E'
!	2003		'E'
b	2004-2005	; 'K'
!	2005		'K'
b	2006-2007	; 'Q'
!	2007		'Q'
b	2008-2009	; 'W'
!	2009		'W'
b	200a-200b	; Up
!	200b		Up
b	200c-200d	; Down
!	200d		Down
b	200e-200f	; Cancel
!	200f		Cancel
b	2010-2011	; 'B'
!	2011		'B'
b	2012-2013	; 'F'
!	2013		'F'
b	2014-2015	; 'L'
!	2015		'L'
b	2016-2017	; 'R'
!	2017		'R'
b	2018-2019	; 'X'
!	2019		'X'
b	201a-201b	; '7'
!	201b		'7'
b	201c-201d	; '4'
!	201d		'4'
b	201e-201f	; '1'
!	201f		'1'
b	2020-2021	; 'C'
!	2021		'C'
b	2022-2023	; 'G'
!	2023		'G'
b	2024-2025	; 'M'
!	2025		'M'
b	2026-2027	; 'S'
!	2027		'S'
b	2028-2029	; 'Y'
!	2029		'Y'
b	202a-202b	; '8'
!	202b		'8'
b	202c-202d	; '5'
!	202d		'5'
b	202e-202f	; '2'
!	202f		'2'
b	2030-2031	; 'D'
!	2031		'D'
b	2032-2033	; 'H'
!	2033		'H'
b	2034-2035	; 'N'
!	2035		'N'
b	2036-2037	; 'T'
!	2037		'T'
b	2038-2039	; 'Z'
!	2039		'Z'
b	203a-203b	; '9'
!	203b		'9'
b	203c-203d	; '6'
!	203d		'6'
b	203e-203f	; '3'
!	203f		'3'
b	2040-2041	; Menu
!	2041		Menu
b	2042-2043	; 'I'
!	2043		'I'
b	2044-2045	; 'O'
!	2045		'O'
b	2046-2047	; 'U'
!	2047		'U'
b	2048-2049	; ' '
!	2049		' '
b	204a-204b	; '.'
!	204b		'.'
b	204c-204d	; '0'
!	204d		'0'
b	204e-204f	; '-'
!	204f		'-'
b	2050-2051	;
!	2051
b	2052-2053	; 'J'
!	2053		'J'
b	2054-2055	; 'P'
!	2055		'P'
b	2056-2057	; 'V'
!	2057		'V'
b	2058-2059	; Delete
!	2058		Delete
b	205a-205b	; Left
!	205b		Left
b	205c-205d	; Right
!	205d		Right
b	205e-205f	; Enter
!	205f		Enter

l	2060		keycode_2_character_table2
b	2060-2061	; 'A'
!	2061		'A'
b	2062-2063	; 'E'
!	2063		'E'
b	2064-2065	; 'K'
!	2065		'K'
b	2066-2067	; 'Q'
!	2067		'Q'
b	2068-2069	; 'W'
!	2069		'W'
b	206a-206b	; Up
!	206b		Up
b	206c-206d	; Down
!	206d		Down
b	206e-206f	; Cancel
!	206f		Cancel
b	2070-2071	; 'B'
!	2071		'B'
b	2072-2073	; 'F'
!	2073		'F'
b	2074-2075	; 'L'
!	2075		'L'
b	2076-2077	; 'R'
!	2077		'R'
b	2078-2079	; 'X'
!	2079		'X'
b	207a-207b	; '7'
!	207b		'7'
b	207c-207d	; '4'
!	207d		'4'
b	207e-207f	; '1'
!	207f		'1'
b	2080-2081	; 'C'
!	2081		'C'
b	2082-2083	; 'G'
!	2083		'G'
b	2084-2085	; 'M'
!	2085		'M'
b	2086-2087	; 'S'
!	2087		'S'
b	2088-2089	; 'Y'
!	2089		'Y'
b	208a-208b	; '8'
!	208b		'8'
b	208c-208d	; '5'
!	208d		'5'
b	208e-208f	; '2'
!	208f		'2'
b	2090-2091	; 'D'
!	2091		'D'
b	2092-2093	; 'H'
!	2093		'H'
b	2094-2095	; 'N'
!	2095		'N'
b	2096-2097	; 'T'
!	2097		'T'
b	2098-2099	; 'Z'
!	2099		'Z'
b	209a-209b	; '9'
!	209b		'9'
b	209c-209d	; '6'
!	209d		'6'
b	209e-209f	; '3'
!	209f		'3'
b	20a0-20a1	; Menu
!	20a1		Menu
b	20a2-20a3	; 'I'
!	20a3		'I'
b	20a4-20a5	; 'O'
!	20a5		'O'
b	20a6-20a7	; 'U'
!	20a7		'U'
b	20a8-20a9	; ' '
!	20a9		' '
b	20aa-20ab	; '.'
!	20ab		'.'
b	20ac-20ad	; '0'
!	20ad		'0'
b	20ae-20af	; '-'
!	20af		'-'
b	20b0-20b1	;
!	20b1
b	20b2-20b3	; 'J'
!	20b3		'J'
b	20b4-20b5	; 'P'
!	20b5		'P'
b	20b6-20b7	; 'V'
!	20b7		'V'
b	20b8-20b9	; Delete
!	20b8		Delete
b	20ba-20bb	; Left
!	20bb		Left
b	20bc-20bd	; Right
!	20bd		Right
b	20be-20bf	; Enter
!	20bf		Enter

l	20c0		keycode_2_character_table3
b	20c0-20c1	; 'A'
!	20c1		'A'
b	20c2-20c3	; 'E'
!	20c3		'E'
b	20c4-20c5	; 'K'
!	20c5		'K'
b	20c6-20c7	; 'Q'
!	20c7		'Q'
b	20c8-20c9	; 'W'
!	20c9		'W'
b	20ca-20cb	; Up
!	20cb		Up
b	20cc-20cd	; Down
!	20cd		Down
b	20ce-20cf	; Cancel
!	20cf		Cancel
b	20d0-20d1	; 'B'
!	20d1		'B'
b	20d2-20d3	; 'F'
!	20d3		'F'
b	20d4-20d5	; 'L'
!	20d5		'L'
b	20d6-20d7	; 'R'
!	20d7		'R'
b	20d8-20d9	; 'X'
!	20d9		'X'
b	20da-20db	; '7'
!	20db		'7'
b	20dc-20dd	; '4'
!	20dd		'4'
b	20de-20df	; '1'
!	20df		'1'
b	20e0-20e1	; 'C'
!	20e1		'C'
b	20e2-20e3	; 'G'
!	20e3		'G'
b	20e4-20e5	; 'M'
!	20e5		'M'
b	20e6-20e7	; 'S'
!	20e7		'S'
b	20e8-20e9	; 'Y'
!	20e9		'Y'
b	20ea-20eb	; '8'
!	20eb		'8'
b	20ec-20ed	; '5'
!	20ed		'5'
b	20ee-20ef	; '2'
!	20ef		'2'
b	20f0-20f1	; 'D'
!	20f1		'D'
b	20f2-20f3	; 'H'
!	20f3		'H'
b	20f4-20f5	; 'N'
!	20f5		'N'
b	20f6-20f7	; 'T'
!	20f7		'T'
b	20f8-20f9	; 'Z'
!	20f9		'Z'
b	20fa-20fb	; '9'
!	20fb		'9'
b	20fc-20fd	; '6'
!	20fd		'6'
b	20fe-20ff	; '3'
!	20ff		'3'
b	2100-2101	; Menu
!	2101		Menu
b	2102-2103	; 'I'
!	2103		'I'
b	2104-2105	; 'O'
!	2105		'O'
b	2106-2107	; 'U'
!	2107		'U'
b	2108-2109	; ' '
!	2109		' '
b	210a-210b	; '.'
!	210b		'.'
b	210c-210d	; '0'
!	210d		'0'
b	210e-210f	; '-'
!	210f		'-'
b	2110-2111	;
!	2111
b	2112-2113	; 'J'
!	2113		'J'
b	2114-2115	; 'P'
!	2115		'P'
b	2116-2117	; 'V'
!	2117		'V'
b	2118-2119	; Delete
!	2118		Delete
b	211a-211b	; Left
!	211b		Left
b	211c-211d	; Right
!	211d		Right
b	211e-211f	; Enter
!	211f		Enter

b	2120-2124
b	2125-2126

b	2889-2a1a

b	2a1b-2a30

l	2ba0		jmptable1
b	2ba0-2ba7

l	2baa		jmptable1_addr0
l	2bba		jmptable1_addr1
l	2bda		jmptable1_addr2
l	2c0f		jmptable1_addr3
l	2c28		jmptable1_addr4
l	2c11		jmptable1_addr5
l	2c59		jmptable1_addr6
l	2c33		jmptable1_addr7

l	2c81		jmptable2
b	2c81-2c88

l	2c8c		jmptable2_addr0
l	2c9a		jmptable2_addr1
l	2cbe		jmptable2_addr2
l	2cfe		jmptable2_addr3
l	2d0f		jmptable2_addr4
l	2d00		jmptable2_addr5
l	2d1f		jmptable2_addr6_addr7

l	2e20		runtime_args_op_overflow

#	2e3c	#
#	2e3c	############################################################
#	2e3c	# runtime_args_add
#	2e3c	#
#	2e3c	# r1 - pointer to first argument
#	2e3c	# r6 - second argument
#	2e3c	# r7 - second argument
l	2e3c		runtime_args_add

#	2e44	#
#	2e44	############################################################
#	2e44	# runtime_args_minus
#	2e44	#
#	2e44	# r1 - pointer to first argument
#	2e44	# r6 - second argument
#	2e44	# r7 - second argument
l	2e44		runtime_args_minus
!	2e47		Clear carry for subtraction
!	2e48		Get first argument
!	2e49		Subtract second argument
!	2e4a		Store carry flag
!	2e4c		Store result
!	2e4d		Check for roll over of dpl
!	2e50		Increment dph
l	2e52		runtime_args_minus_inc_ptr
!	2e52		Increment dpl
!	2e53		Get first argument
!	2e54		Restore carry flag
!	2e56		Subtract second argument
!	2e57		Store result

#	2e5a	#
#	2e5a	############################################################
#	2e5a	# runtime_args_and
#	2e5a	#
#	2e5a	# r1 - pointer to first argument
#	2e5a	# r6 - second argument
#	2e5a	# r7 - second argument
l	2e5a		runtime_args_and
!	2e5d		Get first argument
!	2e5e		AND with second argument
!	2e5f		Store result
!	2e60		Check for roll over of dpl
!	2e63		Increment dph
l	2e65		runtime_args_and_inc_ptr
!	2e65		Increment argument pointer
!	2e66		Get first argument
!	2e67		AND with second argument
!	2e68		Store result

#	2e6a	#
#	2e6a	############################################################
#	2e6a	# runtime_args_or
#	2e6a	#
#	2e6a	# r1 - pointer to first argument
#	2e6a	# r6 - second argument
#	2e6a	# r7 - second argument
l	2e6a		runtime_args_or
!	2e6d		Get first argument
!	2e6e		OR with second argument
!	2e6f		Store result
!	2e70		Check for roll over of dpl
!	2e73		Increment dph
l	2e75		runtime_args_or_inc_ptr
!	2e75		Increment argument pointer
!	2e76		Get first argument
!	2e77		OR with second argument
!	2e78		Store result

#	2e7a	#
#	2e7a	############################################################
#	2e7a	# runtime_args_xor
#	2e7a	#
#	2e7a	# r1 - pointer to first argument
#	2e7a	# r6 - second argument
#	2e7a	# r7 - second argument
l	2e7a		runtime_args_xor
!	2e7d		Get first argument
!	2e7e		XOR with second argument
!	2e7f		Store result
!	2e80		Check for roll over of dpl
!	2e83		Increment dph
l	2e85		runtime_args_xor_inc_ptr
!	2e85		Increment dpl
!	2e86		Get first argument
!	2e87		XOR with second argument
!	2e88		Store result

#	2eca		Setup routine called after reset
l	2eca		system_setup
!	2eca		Set stack pointer - 0x5d
!	2ecd		Enable external clock input for timer 0
!	2ecf		Disable interrupts
!	2ed5		PWM1 is connected to the piezo speaker
!	2ed8		Set condition to load watchdog timer
!	2edb		Load watchdog timer
!	2ede		Keyboard matrix column select address
!	2ee6		Clear Program Status Word
l	2eec		system_setup_register_clear_7fh_to_00h
!	2eec		Clear the contents of register pointed to by r0 (7f-00)
!	2ef2		Load first byte of data from external RAM
!	2ef3		Save it for comparison
!	2ef4		Invert it
!	2ef5		So when we write it back we test all the RAM bits
!	2ef6		Wait for the data to become ready
!	2ef7		Read it back again
!	2ef8		Invert it back
!	2ef9		Compare the current value with the original
!	2efa		Are they the same?
!	2efc		No, set RAM failed
l	2efe		system_setup_battery_check_status
!	2f16		Enable clock
!	2f22		Enable clock
!	2f24		Keyboard address
!	2f27		Initial column data

#	2f52		External interrupt 0, service routine
l	2f52		isr_ie0_srv	

!	2ffe		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	303a		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)

l	30aa		str_low_battery
t	30aa-30b4	; 'BATTERY LOW'
b	30b5		; '!' | 0x80
!	30b5		; '!' | 0x80

l	30bf		str_ram_failure
t	30bf-30c9	; 'RAM FAILURE'
b	30ca		; '!' | 0x80
!	30ca		; '!' | 0x80

l	30d9		str_mcard_battery_low
t	30d9-30ea	; 'M.CARD BATTERY LOW'
b	30eb		; '!' | 0x80
!	30eb		; '!' | 0x80

l	310d		print_reset_done
l	3110		str_reset_done
t	3110-3118	; 'Reset Don'
b	3119		; 'e' | 0x80
!	3119		'e' | 0x80

l	3143		str_master_reset
t	3143-314d	; 'Master Rese'
b	314e		; 't' | 0x80
!	314e		't' | 0x80

l	3203		ui_test_system
l	3206		str_version
t	3206-3211	; 'VERSION P.04'
b	3212		; ' ' | 0x80
!	3212		' ' | 0x80
l	321e		str_bracket_open
t	321e		; ' '
b	321f		; '[' | 0x80
!	321f		'[' | 0x80
!	3222		Generate a 'hash' starting from 0x0000
!	3225		Temporary storage for hash result
l	3227		ui_test_system_gen_32KB_ROM_hash
!	3228		Get byte of ROM data to hash
!	3229		Add to existing result
!	322a		Store result
!	322b		Increment pointer
!	322e		Test whether we are at the end of the ROM area
l	323a		str_bracket_close
b	323a		; ']' | 0x80
!	323a		']' | 0x80
l	323b		ui_test_system_test_ram

l	326b		print_card_write_protected
l	3271		str_card_write_prot
t	3271-3283	; 'CARD WRITE PROTECTE'
b	3284		; 'D' | 0x80
!	3284		'D' | 0x80

l	3287		print_no_ram_card_present
l	328d		str_no_ram_card
t	328d-329e	; 'NO RAM CARD PRESEN'
b	329f		; 'T' | 0x80
!	329f		'T' | 0x80

l	32ff		str_ram_test
t	32ff-3308	; 'K RAM TEST'
b	3309		; ' ' | 0x80
!	3309		' ' | 0x80

l	330c		print_ram_card
l	330f		str_ram_card
t	330f-3318	; 'K RAM CARD'
b	3319		; ' ' | 0x80
!	3319		' ' | 0x80

l	3362		str_pass
t	3362-3364	; 'PAS'
b	3365		; 'S' | 0x80
!	3365		'S' | 0x80

l	3374		str_aborted
t	3374-3379	; 'ABORTE'
b	337a		; 'D' | 0x80
!	337a		'D' | 0x80

l	337d		print_fail
l	3380		str_fail
t	3380-3382	; 'FAI'
b	3383		; 'L' | 0x80
!	3383		'L' | 0x80

#	3392	#
#	3392	############################################################
#	3392	# math_packed_bcd_to_hex
#	3392	#
#	3392	# A - number to convert from packed BCD to hex
l	3392		math_packed_bcd_to_hex

#	33a4	#
#	33a4	############################################################
#	33a4	# math_hex_to_packed_bcd
#	33a4	#
#	33a4	# A - number to convert from hex to packed BCD
l	33a4		math_hex_to_packed_bcd

#	33ac	#
#	33ac	############################################################
#	33ac	# system_u3rtc_xram_load
#	33ac	#
#	33ac	# Load external RAM with values from RTC
l	33ac		system_u3rtc_xram_load
!	33ac		0xf9 - Hundredths of a second
!	33af		Hundredths register
!	33b1		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	33b4		Get value
!	33bc		Convert for system use
!	33bf		Store value
!	33c0		0xff - Seconds
!	33c3		Get value
!	33c9		Convert for system use
!	33cc		Store value
!	33cd		0xfe - Minutes
!	33cf		Get value
!	33d5		Convert for system use
!	33d8		Store value
!	33d9		0xfd - Hours
!	33db		Get value
!	33e1		Make sure valid value (24h clock)
!	33e3		Convert for system use
!	33e6		Store value
!	33e7		0xfc - Day of the month
!	33e9		Get value
!	33ef		Store for later
!	33f1		Lose year data
!	33f3		Convert for system use
!	33f6		Store value
!	33f7		0xfb
!	33f9		0xfa - Year
!	33fb		Get original value
!	33ff		Get year data
!	3401		Store value
!	3402		0xfb - Month
!	3404		Get value
!	340a		Lose weekday data
!	340c		Convert for system use
!	340f		Store value

l	3411		error_print_syntax_sjmp

#	3414	#
#	3414	############################################################
#	3414	# system_u3rtc_xram_save
#	3414	#
#	3414	# Save external RAM to RTC
l	3414		system_u3rtc_xram_save
!	3414		Control/status register
!	3416		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	3419		Select register
!	341e		Stop counting, disable alarm
!	3420		Send command
!	3423		0xf9 - Hundredths of a second
!	3426		Get value
!	3427		Convert for RTC register
!	342a		Load value
!	342d		0xff - Seconds
!	3430		Get value
!	3431		Convert for RTC register
!	3434		Load value
!	3437		0xfe - Minutes
!	3439		Get value
!	343a		Convert for RTC register
!	343d		Load value
!	3440		0xfd - Hours
!	3442		Get value
!	3443		Convert for RTC register
!	3446		Load value
!	3449		0xfc - Day of the month
!	344b		Get value
!	344c		Make sure valid value (Bug? should be after BCD convert)
!	344e		Convert for RTC register
!	3451		Store for later
!	3452		0xfb
!	3454		0xfa - Year (RTC value 0-3)
!	3456		Get value
!	3457		Make sure valid value
!	345a		Shift to correct register position
!	345b		Combine with day of the month
!	345c		Load value
!	345f		0xfb - Month
!	3461		Get value
!	3462		Convert for RTC register
!	3465		Load value
!	346e		Alarm control register
!	3470		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	3473		Select register
!	3476		Timer function: days, Alarm/Timer flag: no iterrupt, no timer alarm, no clock alarm

#	347f	#
#	347f	############################################################
#	347f	# i2c_read_device_register
#	347f	#
#	347f	# A - Register to read
#	347f	# B - I2C address
l	347f		i2c_read_device_register
!	347f		Write register address to device
!	3482		Check for errors
!	3484		I2C restart
!	3487		Get i2c address
!	3489		Read operation
!	348a		Write i2c address (as read operation)
!	348d		Check for errors
!	3490		Read register contents

#	3493	#
#	3493	############################################################
#	3493	# i2c_write_byte_to_addr
#	3493	#
#	3493	# A - Data to write
#	3493	# B - Address to write to
l	3493		i2c_write_byte_to_addr
#	34a7	############################################################
#	34a7	# i2c_write_byte_with_ack_check
#	34a7	#
#	34a7	# A - Data to write
l	34a7		i2c_write_byte_with_ack_check
l	34ad		i2c_write_byte_return

#	34ae	#
#	34ae	############################################################
#	34ae	# i2c_read_byte
#	34ae	#
#	34ae	# A - contains byte read
c	34ae-34bb
l	34ae		i2c_read_byte
!	34ae		Move 8 into r0 (num bits)
!	34b0		Clear A, will store data here
l	34b1		i2c_read_byte_loop
!	34b1		Set SCL to get next bit
!	34b3		NOP - wait for i2c data to 'settle'
!	34b4		Copy i2c bit data into Carry
!	34b6		Clear SCL to ready for next bit
!	34b8		Rotate Carry in to A to build byte
!	34b9		Loop to build byte

c	34bc-34ca
#	34bc	#
#	34bc	############################################################
#	34bc	# i2c_master_read_ack
#	34bc	#
#	34bc	# Generates the necessary ACK to a device read
l	34bc		i2c_master_read_ack

#	34cb	#
#	34cb	############################################################
#	34cb	# i2c_write_byte
#	34cb	#
#	34cb	# A - byte to write out
c	34cb-34e3
l	34cb		i2c_write_byte
!	34cb		Move 8 into r0 (num bits)
!	34cd		Rotate A into Carry
!	34ce		Set i2c bit data from Carry
l	34d0		i2c_write_byte_scl_wait
!	34d0		Set SCL to set next bit
!	34d2		Check for slave clock stetching
l	34d7		i2c_write_byte_rest_loop
!	34d7		Set i2c bit data
!	34d9		Set SCL to set next bit
l	34db		i2c_write_byte_rest
!	34db		Get next bit
!	34dc		NOP - wait for i2c data to 'settle'
!	34dd		Clear SCL to ready for next bit
!	34df		Loop through byte
!	34e1		Release i2c data line

#	34e4	#
#	34e4	############################################################
#	34e4	# i2c_ack_check
c	34e4-34f6
l	34e4		i2c_ack_check
!	34e4		Setup loop count (255)
l	34e6		i2c_ack_check_loop
!	34e6		Check to see if byte ACKed
!	34e9		Keep checking bit
!	34eb		Indicate NACKed
l	34ef		i2c_ack_check_acked
!	34ef		Set SCL for ack bit
!	34f1		NOP - wait for i2c data to 'settle'
!	34f2		NOP - wait for i2c data to 'settle'
!	34f3		Clear SCL to ready for next bit
!	34f5		Indicate ACK

#	34fb	#
#	34fb	############################################################
#	34fb	# i2c_start
c	34fb-3505
l	34fb		i2c_start
!	34fb		Make sure SCL is set
!	34fd		NOP - wait for i2c data to 'settle'
!	34fe		NOP - wait for i2c data to 'settle'
!	34ff		Clear SDA first
!	3501		NOP - wait for i2c data to 'settle'
!	3502		NOP - wait for i2c data to 'settle'
!	3503		Clear SCL

#	3506	#
#	3506	############################################################
#	3506	# i2c_stop
c	3506-351e
l	3506		i2c_stop_clock_stretch_check
!	3506		Release SCL
!	3508		Wait for SCL to be released
!	350b		NOP - wait for i2c data to 'settle'
!	350c		NOP - wait for i2c data to 'settle'
l	350d		i2c_stop
!	350d		Setup loop count (255)
l	350f		i2c_stop_loop
!	350f		Clear SCL
!	3511		Clear SDA
!	3513		NOP - wait for i2c data to 'settle'
!	3514		NOP - wait for i2c data to 'settle'
!	3515		Set SCL
!	3517		Set SDA
!	3519		Has SDA been released?
!	351c		It hasn't, so loop
l	351e		i2c_stop_exit

#	351f	#
#	351f	############################################################
#	351f	# i2c_read_byte_from_addr
#	351f	#
#	351f	# A - Address to read from
l	351f		i2c_read_byte_from_addr

#	3567	#
#	3567	############################################################
#	3567	# system_u3rtc_enable
#	3567	#
l	3567		system_u3rtc_enable
!	3567		Control/status register
!	3568		I2C (0xA0 -> 10100000 -> Addr 0x50, write -> U3 PCF8583 Clock)
!	356b		Select register
!	356e		0b00000100 - Clock mode 32.768 kHz, alarm enabled

l	3583		keyboard_column_select_clear
!	3583		Enable clock for column select
!	3586		Clear column select
!	3587		Disable clock
l	358f		keyboard_read
!	3592		I2C (0x41 -> 01000001 -> Addr 0x20, read -> U13 PCF8574 bus expander)
!	359a		Return if NACKed
!	359c		Read keyboard row data
l	35a3		keyboard_read_exit

l	35a4		print_month_day
l	35a7		str_month_day
t	35a7-35b5	; 'MONTH **  DAY *'
b	35b6		; '*' | 0x80
!	35b6		'*' | 0x80

l	35bd		str_time
t	35bd-35c7	; 'TIME    :  '
b	35c8		; ':' | 0x80
!	35c8		':' | 0x80

l	36f8		ui_config_serial
!	36f8		Config RAM access
!	36fc		rb1r0 = XRAM 0x9b
!	3700		rb1r1 = XRAM 0x9c
!	3704		50h = XRAM 0x9d
!	3708		rb1r2 = XRAM 0x9e
!	370c		rb1r3 = XRAM 0x9f
l	370e		ui_config_serial_baud
l	3715		str_baud
t	3715-3718	; 'BAUD'
b	3719		; '=' | 0x80
!	3719		'=' | 0x80
l	371a		ui_config_serial_baud_loop
!	371a		Get current baud rate
!	371f		Check that the baud value is between 0-9
l	3724		ui_config_serial_baud_reset
l	3729		ui_config_serial_baud_current
!	3729		Undo the previous add (wrap over 0xff)
!	372b		Size of baud string
!	372e		Calculate offset in string table
!	373a		Increment baud setting
l	373e		ui_config_serial_baud_save
!	374a		Save baud rate
l	374e		ui_config_serial_protocol
l	3751		str_word
t	3751-3754	; 'WORD'
b	3755		; '=' | 0x80
!	3755		'=' | 0x80
!	3756		Get current protocol config
!	375b		Check value between 0-8
l	3760		ui_config_serial_protocol_reset
l	3763		ui_config_serial_protocol_current
!	3763		Get current protocol config
!	3768		Get current protocol config
!	376a		Size of protocol strings
!	376d		Get offset
l	3777		str_stop
t	3777-377a	; ' STO'
b	377b		; 'P' | 0x80
!	377b		'P' | 0x80
!	3781		Increment protocol config
l	3785		ui_config_serial_protocol_save
l	3795		ui_config_progbank
l	3798		str_prog_bank
t	3798-37a5	; 'Prog Bank No.='
b	37a6		; ' ' | 0x80
!	37a6		' ' | 0x80

l	37b6		str_maximum_255
t	37b6-37c1	; 'Maximum = 25'
b	37c2		; '5' | 0x80
!	37c2		'5' | 0x80

l	37cc		str_enter_prog_bank
t	37cc-37dd	; 'Enter Prog Bank No'
b	37de		; '.' | 0x80
!	37de		'.' | 0x80

l	3813		str_prog_addr
t	3813-381c	; 'Prog Addr='
b	381d		; ' ' | 0x80
!	381d		' ' | 0x80

l	382b		str_maximum_32767
t	382b-3838	; 'Maximum = 3276'
b	3839		; '7' | 0x80
!	3839		'7' | 0x80

l	3846		str_enter_addr
t	3846-3850	; 'Enter Addr.'
b	3851		; ' ' | 0x80
!	3851		' ' | 0x80

#	3877	#
#	3877	############################################################
#	3877	# serial_uart_config
#	3877	#
#	3877	# A - Offset to config data
l	3877		serial_uart_config
!	387a		Get config data
!	387b		Mode 3, 9 bit UART, Receive enabled
!	3881		Mode 1, 8 bit UART, Receive enabled
l	3884		serial_uart_config_continue

l	388c		str_rom
t	388c-388e	; 'ROM'
b	388f		; ' ' | 0x80
!	388f		' ' | 0x80

l	3890		str_ram
t	3890-3892	; 'RAM'
b	3893		; ' ' | 0x80
!	3893		' ' | 0x80

l	389e		str_serial_baud_table
t	389e-38a1	; '1920'
b	38a2		; 0xb0
!	38a2		; '0' | 0x80
t	38a3-38a6	; '9600'
b	38a7		; 0xa0
!	38a7		; ' ' | 0x80
t	38a8-38ab	; '4800'
b	38ac		; 0xa0
!	38ac		; ' ' | 0x80
t	38ad-38b0	; '2400'
b	38b1		; 0xa0
!	38b1		; ' ' | 0x80
t	38b2-38b5	; '1200'
b	38b6		; 0xa0
!	38b6		; ' ' | 0x80
t	38b7-38ba	; '600 '
b	38bb		; 0xa0
!	38bb		; ' ' | 0x80
t	38bc-38bf	; '300 '
b	38c0		; 0xa0
!	38c0		; ' ' | 0x80
t	38c1-38c4	; '150 '
b	38c5		; 0xa0
!	38c5		; ' ' | 0x80
n	38c6
t	38c6-38c9	; '110 '
b	38ca		; 0xa0
!	38ca		; ' ' | 0x80
t	38cb-38ce	; '75  '
b	38cf		; 0xa0
!	38cf		; ' ' | 0x80

l	38d0		str_serial_protocol_table
t	38d0-38d4	; '7D+O+'
b	38d5		; '1' | 0x80
!	38d5		'1' | 0x80
t	38d6-38da	; '7D+E+'
b	38db		; '1' | 0x80
!	38db		'1' | 0x80
t	38dc-38e0	; '7D+O+'
b	38e1		; '2' | 0x80
!	38e1		'2' | 0x80
t	38e2-38e6	; '7D+E+'
b	38e7		; '2' | 0x80
!	38e7		'2' | 0x80
t	38e8-38ec	; '  7D+'
b	38ed		; '2' | 0x80
!	38ed		'2' | 0x80
t	38ee-38f2	; '8D+O+'
b	38f3		; '1' | 0x80
!	38f3		'1' | 0x80
t	38f4-38f8	; '8D+E+'
b	38f9		; '1' | 0x80
!	38f9		'1' | 0x80
t	38fa-38fe	; '  8D+'
b	38ff		; '1' | 0x80
!	38ff		'1' | 0x80
t	3900-3904	; '  8D+'
b	3905		; '2' | 0x80
!	3905		'2' | 0x80

l	3906		serial_config_table
b	3906
!	3906		(0bxxxx0111) 7 bit, Odd parity, 1 Stop
b	3907
!	3907		(0bxxxx0110) 7 bit, Even parity, 1 Stop
b	3908
!	3908		(0bxxxx1111) 7 bit, Odd parity, 2 stop
b	3909
!	3909		(0bxxxx1110) 7 bit, Even parity, 2 stop
b	390a
!	390a		(0bxxxx0100) 7 bit, No parity, 2 stop
b	390b
!	390b		(0bxxxx1011) 8 bit, Odd parity, 1 stop
b	390c
!	390c		(0bxxxx1010) 8 bit, Even parity, 1 stop
b	390d
!	390d		(0bxxxx0000) 8 bit, No parity, 1 stop
b	390e
!	390e		(0bxxxx1000) 8 bit, No parity, 2 stop

l	390f		keyboard_wait_for_keypress
!	394e		Check for a control keycode

!	3951		[M] + UP (Contrast+)
!	3954		Get stored contrast value
!	3956		Keep ??? value
!	3958		Increment contrast value
!	395b		Check if we have rolled over the top
!	395d		Restore ??? value
!	395f		Save contrast value
!	3961		LCD properity address
!	3964		Set contrast

!	396c		[M] + DOWN (Contrast-)
!	396f		Get stored contrast value
!	3971		Keep ??? value
!	3975		Check if already zero
!	3977		Decrement contrast value
!	3978		Restore ??? value
!	397a		Save contrast value
!	397c		LCD properties address
!	397f		Set contrast

!	3987		[M] + DEL
!	398e		[M] + SPACE
!	3997		[M]
!	399e		[M] + [M]
!	39a5		[M] + 1
!	39bd		[M] + CANCEL

l	39df		str_reenter
t	39df-39e5	; 'RE-ENTE'
b	39e6		; 'R' | 0x80
!	39e6		'R' | 0x80

#	3a0d	#
#	3a0d	############################################################
#	3a0d	# print_string_from_offset_term_on_msb
#	3a0d	#
#	3a0d	# dptr - Start of string table
#	3a0d	# A - Offset to string
l	3a0d		print_string_from_offset_term_on_msb
!	3a0d		Save offset
!	3a0f		Get character
!	3a10		Save character
!	3a15		Get character
!	3a17		Get offset
!	3a19		Increment offset
!	3a1a		Check for msb term

#	3a1e	#
#	3a1e	############################################################
#	3a1e	# ui_change_parameter
#	3a1e	#
#	3a1e	# Prints 'Change ?' on screen waits for keyboard input,
#	3a1e	# sets Carry on 'Y', clears otherwise
l	3a1e		ui_change_parameter
!	3a1e		Set cursor position
l	3a24		str_change
t	3a24-3a2a	; 'CHANGE '
b	3a2b		; '?' | 0x80
!	3a2b		'?' | 0x80
l	3a36		ui_change_parameter_yes

l	3c30		ui_display_battery_level
!	3c30		Initialise and clear display
!	3c35		Set character position at the start of the display
!	3c38		Set font as inverted
!	3c3a		ASCII character: ' '
!	3c3f		Set font as double width
l	3c44		str_battery_level
t	3c44-3c51	; ' BATTERY LEVEL'
b	3c52		; ' ' | 0x80
!	3c52		' ' | 0x80
!	3c53		Clear double width
!	3c55		ASCII character: ' '
!	3c5a		Clear inverted
!	3c5c		Set character position: row:3 / column:7
!	3c5f		ASCII character: 'E'
!	3c64		ASCII character: 'F'
!	3c66		Set character position: row:3 / column:24
!	3c6c		Enable clock
!	3c6e		LCD command address
!	3c71		r4 - LCD command: 'Set Page' 5
!	3c73		r7 - Quarters
!	3c75		r6 - subscreen column index
!	3c77		r5 - Line data
!	3c79		b - Write iterations
l	3c81		ui_display_battery_level_draw_gauge_next_quarter
l	3c84		ui_display_battery_level_draw_gauge_update_args
!	3c84		Line data (long quarter)
!	3c86		Get quarter
!	3c8b		Line data (short quarter)
l	3c8d		ui_display_battery_level_draw_gauge_update_args_continue
!	3c8d		Send data
!	3c90		Increment subscreen column index
!	3c91		Reset number of iterations
!	3c94		Check if there is another quarter to draw
!	3c96		Disable clock
!	3c9b		Enable clock
!	3c9e		Save battery status as iterations
!	3ca0		LCD command address
!	3ca3		LCD command: 'Set Page' 4
!	3ca5		Reset subscreen column index
!	3ca7		Line data
!	3cac		Disable clock
l	3caf		ui_display_battery_level_draw_gauge
!	3caf		LCD command address
!	3cb2		LCD command: 'Set Address' 44
!	3cb4		Send command
!	3cb7		LCD command: 'Set Page'
!	3cb8		Send command
!	3cbb		LCD data address
!	3cbe		Full line
!	3cc0		Send data
l	3cc3		ui_display_battery_level_draw_gauge_get_data
!	3cc3		Line data
l	3cc4		ui_display_battery_level_draw_gauge_check_iteration
l	3cc8		ui_display_battery_level_draw_gauge_draw_data
!	3cc8		Send data
!	3ccb		Increment column index
!	3ccc		Check we're still on subscreen
!	3ccf		Reset subscreen index
!	3cd3		Get subscreen (command register)
!	3cd5		Increment subscreen
!	3cd9		LCD command: 'Set Address' 0
!	3cdb		Send command
!	3cde		LCD command: 'Set Page'
!	3cdf		Send command
!	3ce2		Swap to LCD data register

l	3d00		ascii_font_table
b	3d00-3fff
n	3d20		; Suppress label generation
n	3d37		; Suppress label generation
n	3d38		; Suppress label generation

b	4000-7fff

c	404b-42f3

c	4352-437d
l	4352		cmd_do_enter

c	43e2-446a

c	446b-446d

;c	4675-468e
c	462a-469d
l	4675		cmd_do_search

c	4739-4769
c	476c-479e
l	476c		cmd_do_file

c	47a0-47cb
l	47a0		cmd_do_get
l	47a9		cmd_do_put

c	4896-48d2
l	4896		cmd_do_bcd
l	48a6		cmd_do_pack
l	48b3		cmd_do_unpack

c	49e0-4a01
l	49e0		cmd_do_lprint

c	4afc-4b1a
l	4afc		cmd_do_goto
l	4b12		cmd_do_gosub

c	4b33-4b5c
l	4b33		cmd_do_for

c	4b61-4b83
l	4b61		cmd_do_next

c	4b98-4bba
l	4b98		cmd_do_sdt
l	4bb9		cmd_do_ddt

c	4be7-4c0d
l	4be7		cmd_do_call

c	4cb4-4d04
l	4cb4		cmd_do_endproc

c	4d02-4d04
l	4d02		cmd_do_on

c	4d51-4d59
l	4d51		cmd_do_input

;c	4a05-4a99
;c	4bbf-4bc0
;c	4bc3-4be3
;c	4c25-4c47

