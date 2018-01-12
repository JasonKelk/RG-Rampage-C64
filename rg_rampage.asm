;
; RG RAMPAGE
;

; Code, graphics and sound effects by Jason Kelk


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/

; build.bat will call ACME to create an assembled file.


; MEMORY MAP
; $8000 - $9fff		code and data
; $a000 - $a7ff		character data (constructed)
; $a800 - $bbff		sprite data (constructed)
; $bc00 - $bfff		screen RAM
; $c000 - $c0ff		multicolour bit pair flip table


; Select an output filename
		!to "rg_rampage.prg",cbm

; If build is $00 then a BASIC startline is added, otherwise it isn't
build		= $00

; Constants
level_max	= $06		; final level number (counts from $00)

rstr1p		= $00
rstr2p		= $f9

screen_col_off	= $1c		; screen to colour RAM high byte offset

; Labels
rn		= $82
sync		= $83

hover_anim_tmr	= $84

draw_stash_1	= $87
draw_stash_2	= $88
sprite_glow_tmr	= $89
ply_run_tmr	= $8a
ply_run_flag	= $8b
char_glow_temp	= $8c
char_glow_tmr	= $8d

grav_power	= $8e
grav_timer	= $8f
grav_active	= $90
respawn_x	= $91
respawn_y	= $92

level		= $93
boxout_col	= $94
boxout_len	= $95

; Title zoom scroll stuff
scrl_cnt	= $96
scrl_tmr	= $97
scrl_buffer	= $98		; $07 bytes used

; Current sprite positions
sprite_pos	= $a0		; $10 bytes used
sprite_dp	= $b0		; $08 bytes used
sprite_flip	= $b8		; $08 bytes used

; Sprite movement positions
sprite_x1	= $c0		; $06 bytes used
sprite_x2	= $c6		; $06 bytes used
sprite_y1	= $cc		; $06 bytes used
sprite_y2	= $d2		; $06 bytes used
sprite_dir	= $d8		; $06 bytes used
coll_temp	= $e0		; $04 bytes used
coll_flag	= $e4

score		= $f0		; $06 bytes used
lives		= $f6		; $01 byte used
bonus		= $f7		; $04 bytes used

; Temporary storage
rt_store_1	= $fc
rt_store_2	= $fd
rt_store_3	= $fe
rt_store_4	= $ff

; Location to build a bit flip table to
bitflip_table	= $c000		; $100 bytes used


; Test :: add a BASIC startline if build is zero
!if build=$00 {
		* = $0801
		!word entry-2
		!byte $00,$00,$9e
		!text "32768"
		!byte $00,$00,$00
}


; Main entry point
		* = $8000
entry		sei
		lda #$33
		sta $01

; Grab the alphanumeric part of the ROM character set
		ldx #$00
		ldy #$00
font_copy	lda $d000,x
		cpy #$02
		bne *+$04
		and #%00000000
		sta $a000,x
		eor #$ff
		sta $a400,x

		lda $d100,x
		cpy #$02
		bne *+$04
		lda #$00
		sta $a100,x
		eor #$ff
		sta $a500,x

		iny
		cpy #$08
		bne *+$04
		ldy #$00

		inx
		bne font_copy

; Pull down the custom tile characters
		ldx #$00
font_modify	lda char_data,x
		sta $a200,x
		inx
		cpx #$88
		bne font_modify

		lda #$36
		sta $01
		cli


; Generate bit pair flip table for the sprites
		ldx #$00
bitflip_gen	txa
		sta rt_store_1
		lda #$00
		asl rt_store_1
		rol
		asl rt_store_1
		rol
		sta rt_store_2

		txa
		and #%00110000
		lsr
		lsr
		sta rt_store_3

		txa
		and #%00001100
		asl
		asl
		sta rt_store_4

		txa
		sta rt_store_1
		lda #$00
		lsr rt_store_1
		ror
		lsr rt_store_1
		ror
		ora rt_store_2
		ora rt_store_3
		ora rt_store_4

		sta bitflip_table,x
		inx
		bne bitflip_gen

; Copy sprites into their appropriate place in RAM
		ldx #$00
sprite_copy	lda sprite_data+$000,x
		sta $aa00,x
		lda sprite_data+$100,x
		sta $a900,x
		sta $ab00,x
		lda sprite_data+$200,x
		sta $ac00,x
		inx
		bne sprite_copy

; Generate flipped sprites
		ldx #$00
sprite_flipper	ldy sprite_data+$002,x
		lda bitflip_table,y
		sta $a800,x
		ldy sprite_data+$001,x
		lda bitflip_table,y
		sta $a801,x
		ldy sprite_data+$000,x
		lda bitflip_table,y
		sta $a802,x

		ldy sprite_data+$042,x
		lda bitflip_table,y
		sta $a840,x
		ldy sprite_data+$041,x
		lda bitflip_table,y
		sta $a841,x
		ldy sprite_data+$040,x
		lda bitflip_table,y
		sta $a842,x

		ldy sprite_data+$082,x
		lda bitflip_table,y
		sta $a880,x
		ldy sprite_data+$081,x
		lda bitflip_table,y
		sta $a881,x
		ldy sprite_data+$080,x
		lda bitflip_table,y
		sta $a882,x

		ldy sprite_data+$0c2,x
		lda bitflip_table,y
		sta $a8c0,x
		ldy sprite_data+$0c1,x
		lda bitflip_table,y
		sta $a8c1,x
		ldy sprite_data+$0c0,x
		lda bitflip_table,y
		sta $a8c2,x

		ldy sprite_data+$102,x
		lda bitflip_table,y
		sta $a900,x
		ldy sprite_data+$101,x
		lda bitflip_table,y
		sta $a901,x
		ldy sprite_data+$100,x
		lda bitflip_table,y
		sta $a902,x

		ldy sprite_data+$142,x
		lda bitflip_table,y
		sta $a940,x
		ldy sprite_data+$141,x
		lda bitflip_table,y
		sta $a941,x
		ldy sprite_data+$140,x
		lda bitflip_table,y
		sta $a942,x
		inx
		inx
		inx
		cpx #$3f
		beq *+$05
		jmp sprite_flipper

; One shot video register inits
		lda #$35
		sta $dd00

		lda #$18
		sta $d016
		lda #$f8
		sta $d018

		lda #$0b
		sta $d020
		lda #$00
		sta $d021

		lda #$0b
		sta $d022
		lda #$0f
		sta $d023

		lda #$0b
		sta $d025
		lda #$0e
		sta $d026

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
		sei
		lda #$01
		sta rn

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda #rstr1p
		sta $d012
		lda #$1b
		sta $d011

		lda #$01
		sta $d019
		sta $d01a

		lda #$35
		sta $01

		cli

; Draw the status bar
		ldx #$00
		lda #$20
clr_status	sta $bf98,x
		inx
		cpx #$50
		bne clr_status

		ldx #$00
set_status	lda status_text+$00,x
		sta $bf98,x
		lda status_text_r,x
		sta $bfa7,x
		lda status_text+$05,x
		sta $bfbb,x

		lda status_text+$0a,x
		sta $bfca,x
		lda status_text+$0f,x
		sta $bfd7,x

		inx
		cpx #$05
		bne set_status

		ldx #$00
set_status_col	lda #$06
		sta $db98,x
		sta $dbc0,x

		lda #$03
		sta $dba7,x

		lda #$02
		sta $dbb6,x
		sta $dbde,x

		lda #$05
		sta $dbca,x
		lda #$04
		sta $dbd4,x
		inx
		cpx #$0a
		bne set_status_col

; Reset the label space
		jsr nuke_labels


; Initialise titles page
title_init	jsr screen_clear
		jsr fire_debounce

; Reset the sound effect driver
		jsr sfx_init

; Set colour RAM for the titles page
		ldx #$00
ttl_colset	lda #$0d
		sta $d918,x
		lda #$0c
		sta $d9b8,x
		inx
		cpx #$a0
		bne ttl_colset

; Main title loop
title_loop	jsr sync_wait

; Move the big scroller
		ldx #$00
ttl_scrl	lda $bd41,x
		sta $bd40,x
		lda $bd69,x
		sta $bd68,x
		lda $bd91,x
		sta $bd90,x
		lda $bdb9,x
		sta $bdb8,x
		lda $bde1,x
		sta $bde0,x
		lda $be09,x
		sta $be08,x
		lda $be31,x
		sta $be30,x
		inx
		cpx #$27
		bne ttl_scrl

; See if we need a new character for the scroller
		ldx scrl_tmr
		dex
		bpl ttl_st_xb

		ldx scrl_cnt
		ldy scrolltext,x
		dey
		tya
		asl
		asl
		asl
		tay

; Copy the character definition
		ldx #$00
ttl_read_char	lda $a008,y
		sta scrl_buffer,x
		iny
		inx
		cpx #$07
		bne ttl_read_char

		ldx scrl_cnt
		inx
		cpx #$19
		bne *+$04
		ldx #$00
		stx scrl_cnt

		ldx #$07
ttl_st_xb	stx scrl_tmr

; Get some bits from the character buffer (skips the blank third row)
		lda #$50
		ldy #$20
		asl scrl_buffer+$00
		bcc *+$03
		tay
		sty $bd67

		ldy #$20
		asl scrl_buffer+$01
		bcc *+$03
		tay
		sty $bd8f

;		ldy #$20		; this line of the char is always blank!
;		asl scrl_buffer+$02
;		bcc *+$03
;		tay
;		sty $bdb7

		ldy #$20
		asl scrl_buffer+$03
		bcc *+$03
		tay
		sty $bddf

		ldy #$20
		asl scrl_buffer+$04
		bcc *+$03
		tay
		sty $be07

		ldy #$20
		asl scrl_buffer+$05
		bcc *+$03
		tay
		sty $be2f

		ldy #$20
		asl scrl_buffer+$06
		bcc *+$03
		tay
		sty $be57

; Check the fire button to see if the game needs to start
		lda $dc00
		and #$10
		beq ttl_exit
		jmp title_loop

ttl_exit	jsr screen_clear

		jsr fire_debounce


; Falling through into the game itself
		jsr nuke_labels

		lda #$00
		sta level

		lda #<level_data
		sta screen_init_rd+$01
		lda #>level_data
		sta screen_init_rd+$02

		lda #$03
		sta lives


; Initialise main game - called when each new life starts
main_init	jsr screen_clear
		jsr level_init

		lda #$02
		sta bonus+$00
		lda #$05
		sta bonus+$01
		lda #$00
		sta bonus+$02
		sta bonus+$03

; Reset the sprite flip table
		ldx #$00
		txa
flip_reset	sta sprite_flip,x
		inx
		cpx #$08
		bne flip_reset

; Player spawn
main_respawn	lda respawn_x
		sta sprite_pos+$00
		lda respawn_y
		sta sprite_pos+$01

		lda #$a0
		sta sprite_dp+$00
		lda #$b0
		sta sprite_dp+$07
		lda #$00
		sta sprite_flip+$00
		sta sprite_flip+$07

		sta grav_power
		sta grav_timer
		sta grav_active

; Trigger the player spawn sound
		ldx #$10
		jsr sfx_call


; Main loop start
main_loop	jsr sync_wait

		jsr player_move
		jsr enemy_move

; Decrease the bonus
		jsr bonus_down

		jsr player_colls
; Check for and process fatal collisions
		lda coll_flag
		cmp #$01
		beq death_init

; Check for and process player to collectable object
		cmp #$02
		bne mb_skip

		jsr score_bump_100
		jsr score_bump_100
		jsr score_bump_100

; Detect the end of a level
		jsr screen_init_rd
		cmp #$ff
		bne *+$05
		jmp levdun_init

		sta sprite_pos+$0e
		jsr screen_init_rd
		sta sprite_pos+$0f

; Trigger the object collection sound
		ldx #$08
		jsr sfx_call

mb_skip		jsr bgnd_manage

		jmp main_loop


; Initialise death sequence
death_init

; Trigger the player death sound
		ldx #$1c
		jsr sfx_call
		ldx #$20
		jsr sfx_call

		lda #$a1
		sta sprite_dp
		ldy #$0c

; Death sequence main loop
death_loop	jsr sync_wait
		jsr sync_wait
		jsr sync_wait
		jsr sync_wait

		lda sprite_flip
		eor #$08
		sta sprite_flip
		dey
		bne death_loop

; Hide the player sprite and wait a second
		lda #$00
		sta sprite_pos+$00

		jsr sync_wait_50

; Decrease the lives, check for zero and call the game over routine
		dec lives
		lda lives
		beq gover_init

; Respawn the player for the next life
		jmp main_respawn


; Initialise game over sequence
gover_init	lda #$02
		sta boxout_col

		ldx #$0d	; X position of box
		ldy #$00	; start of message in status text
		lda #$0e	; end of message in status text
		jsr boxout_draw

; Trigger the game over sound
		ldx #$14
		jsr sfx_call
		ldx #$18
		jsr sfx_call

; Wait for three seconds, then bail to the titles page
		jsr sync_wait_50
		jsr sync_wait_50
		jsr sync_wait_50

		jmp title_init


; Initialise level completion sequence
levdun_init	ldx #$00
ld_copy		lda $bfae,x
		sta boxout_bonus,x
		inx
		cpx #$03
		bne ld_copy

		lda #$04
		sta boxout_col

		ldx #$0c	; X position of box
		ldy #$0c	; start of message in status text
		lda #$1c	; end of message in status text
		jsr boxout_draw

; Trigger the level completion sound
		ldx #$0c
		jsr sfx_call

; Level completion loop
levdun_loop	jsr sync_wait

		lda bonus+$00
		ora bonus+$01
		ora bonus+$02
		beq ld_loop_out

; Add five points for each second on the bonus clock
		jsr bonus_down_3
		jsr score_bump_1
		jsr score_bump_1
		jsr score_bump_1
		jsr score_bump_1
		jsr score_bump_1

		jmp levdun_loop

; Exit level completion sequence
ld_loop_out	jsr sync_wait_50

		ldx level
		inx
		cpx #level_max
		beq gamedun_init
		stx level

		jmp main_init


; Initialise completion sequence
gamedun_init	ldy lives
gd_lives_bonus	jsr score_bump_1000
		dey
		bne gd_lives_bonus

; Completion sequence loop
gamedun_loop	jsr sync_wait
		jsr sync_wait

		inc char_glow_tmr
		lda char_glow_tmr
		and #$0f
		tax
		lda char_glow,x
		sta boxout_col

		ldx #$07	; X position of box
		ldy #$1a	; start of message in status text
		lda #$34	; end of message in status text
		jsr boxout_draw

		lda char_glow_tmr
		and #$0f
		cmp #$0f
		bne gd_no_sfx

; Trigger the game completion sound
		ldx #$24
		jsr sfx_call

; Modify the end sound data to make things more interesting
		lda sd_data_end+$00
		sec
		sbc #$08
		sta sd_data_end+$00

		inc sd_data_end+$01

		lda sd_data_end+$02
		eor #$30
		sta sd_data_end+$02

gd_no_sfx	lda $dc00
		and #$10
		bne gamedun_loop

		jmp title_init


; General purpose screen clear (doesn't clear the bottom two lines)
screen_clear	ldx #$00
		lda #$20
sc_loop		sta $bc00,x
		sta $bd00,x
		sta $be00,x
		sta $bee8-$50,x
		inx
		bne sc_loop

		rts

; Zero all the sprite positions
sprite_reset	ldx #$00
		txa
sr_loop		sta sprite_pos,x
		inx
		cpx #$10
		bne sr_loop
		rts

; Draw a text boxout (Y contains start of text)
boxout_draw	sta boxout_len

bd_loop		lda #$a0
		sta $bd90,x
		sta $bde0,x

		lda boxout_text,y
		ora #$80
		sta $bdb8,x

		lda boxout_col
		sta $d990,x
		sta $d9b8,x
		sta $d9e0,x

		inx
		iny
		cpy boxout_len
		bne bd_loop

		jsr sprite_reset

		rts

; Shared debounce routine for the fire button
fire_debounce	lda $dc00
		and #$10
		beq fire_debounce
		rts

; Nuke the label space
nuke_labels	ldx #$83
		lda #$00
nl_loop		sta $00,x
		inx
		bne nl_loop
		rts


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		beq rout2

; Raster split 1
rout1

; Set up for the second raster split
		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		lda #$ff
		sta $d015
		lda #$7f
		sta $d01c

; Set up the current sprite positions
		ldx #$00
		ldy #$00
xplode		lda sprite_pos,y
		asl
		ror $d010
		sta $d000,y
		lda sprite_pos+$01,y
		sta $d001,y

		lda sprite_dp,x
		clc
		adc sprite_flip,x
		sta $bff8,x
		lda sprite_col,x
		sta $d027,x
		inx
		iny
		iny
		cpx #$08
		bne xplode

; Update the sound effect driver
		jsr sfx_update

; Exit the interrupt
		jmp ea31

; Raster split 2
rout2		lda #$14
		sta $d011

; Wait for the lower border (upper/lower border opening code)
		lda #$fc
		cmp $d012
		bne *-$05

		lda #$1b
		sta $d011
		lda #$00
		sta $d015

; Set up for the first raster split
		lda #$01
		sta rn
		lda #rstr1p
		sta $d012


		lda #$01
		sta sync

; Status bar updates
		ldx #$00
stats_update	lda score+$01,x
		ora #$30
		sta $bfc0,x
		lda high_score+$01,x
		ora #$30
		sta $bfe3,x
		inx
		cpx #$05
		bne stats_update

		ldx #$00
stats_upd_2	lda bonus,x
		ora #$30
		sta $bfae,x
		inx
		cpx #$03
		bne stats_upd_2

		lda lives
		ora #$30
		sta $bfd0

		lda level
		clc
		adc #$31
		sta $bfdd

; Exit interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; 50 frame wait and screen mask for exiting segments of the game
sync_wait_50	ldx #$32
sw50_loop	jsr sync_wait
		dex
		bne sw50_loop
		rts

; Raster synchronisation wait
sync_wait	lda #$00
		sta sync
sw_loop		cmp sync
		beq sw_loop
		rts


; Background event management
bgnd_manage	lda sprite_glow_tmr
		clc
		adc #$01
		and #$1f
		sta sprite_glow_tmr
		lsr
		tax
		lda sprite_glow,x
		sta sprite_col+$07

; Redefine the conveyor belt characters
convey_rl_1	lda $a240
		asl
		bcc convey_rl_1a
		ora #$01
convey_rl_1a	asl
		bcc convey_rl_1b
		ora #$01
convey_rl_1b	sta $a240
		sta $a248
		sta $a257
		sta $a25f

convey_rl_2	lda $a250
		lsr
		bcc convey_rl_2a
		ora #$80
convey_rl_2a	lsr
		bcc convey_rl_2b
		ora #$80
convey_rl_2b	sta $a250
		sta $a258
		sta $a247
		sta $a24f

; Sprite animation - hover droid 2
		inc hover_anim_tmr
		lda hover_anim_tmr
		lsr
		lsr
		and #$07
		tax
		cpx #$04
		bcc *+$04
		ldx #$03
		lda hover_anim_data,x
		sta $a9fd
		sta $abfd

		rts

; Data for hover droid 2
hover_anim_data	!byte %10010110
		!byte %11010111
		!byte %01010101
		!byte %00010100

; Player sprite handling
player_move	lda #$00
		sta ply_run_flag

; Joystick up
		lda $dc00
up		lsr
		bcs down

		ldx grav_active
		bne down

		ldx #$fc
		stx grav_power
		ldx #$ff
		stx grav_timer
		ldx #$01
		sta grav_active

; Trigger the player jump sound
		sta rt_store_1
		ldx #$00
		jsr sfx_call
		ldx #$04
		jsr sfx_call
		lda rt_store_1

; Joystick down - not used in this game
down		lsr

; Joystick left
left		lsr
		bcs right
		jsr player_left

		ldx #$00
		stx sprite_flip+$00
		inc ply_run_flag

; Joystick right
right		lsr
		bcs joy_out
		jsr player_right

		ldx #$08
		stx sprite_flip+$00
		inc ply_run_flag

; Here is where the jump gravity is handled
joy_out		lda sprite_pos+$01
		clc
		adc grav_power
		sta sprite_pos+$01

		ldx grav_timer
		inx
		cpx #$04
		bne jt_xb

		ldx grav_power
		inx
		cpx #$05
		bne *+$04
		ldx #$04
		stx grav_power

		ldx #$00
jt_xb		stx grav_timer

; Process the player's sprite data pointer and animate if needed
		ldy #$a0
		lda ply_run_flag
		beq ply_no_anim
		iny

		lda ply_run_tmr
		adc #$01
		and #$07
		sta ply_run_tmr
		cmp #$04
		bcc ply_no_anim
		iny

ply_no_anim	sty sprite_dp+$00

		lda grav_active
		beq pm_exit
		lda #$a1
		sta sprite_dp+$00

pm_exit		rts

; Player X movement subs, called for joystick and conveyor
player_left	ldx sprite_pos
		dex
		cpx #$0c
		bcc *+$05
		stx sprite_pos
		rts

player_right	ldx sprite_pos
		inx
		cpx #$a3
		bcs *+$05
		stx sprite_pos

		rts

; Player sprite collisions
player_colls	lda #$00
		sta coll_flag

; Vertical height check - values higher than $d5 are fatal!
		lda sprite_pos+$01
		cmp #$d5
		bcc pc_main

		lda #$01
		sta coll_flag
		rts

; Get ready for the collision check loop
pc_main		lda sprite_pos+$00
		sec
		sbc #$06
		sta coll_temp+$00
		clc
		adc #$0d
		sta coll_temp+$01

		lda sprite_pos+$01
		sec
		sbc #$0f
		sta coll_temp+$02
		clc
		adc #$20
		sta coll_temp+$03

; Loop to check the player against each enemy
		ldx #$00
		ldy #$00
psc_loop	lda sprite_pos+$02,x
		cmp coll_temp+$00
		bcc psc_over
		cmp coll_temp+$01
		bcs psc_over

		lda sprite_pos+$03,x
		cmp coll_temp+$02
		bcc psc_over
		cmp coll_temp+$03
		bcs psc_over

		lda #$01
		cpx #$0c
		bne *+$04
		lda #$02
		sta coll_flag

psc_over	iny
		inx
		inx
		cpx #$0e
		bne psc_loop

; Player background collisions
		lda sprite_pos+$01
		cmp #$22
		bcs pcb_go

		rts

; Sprite Y higher than $32 so do background collision checks
pcb_go		sec
		sbc #$1c
		lsr
		lsr
		lsr
		tax
		lda screen_low,x
		sta pcol_read+$01
		lda screen_hi,x
		sta pcol_read+$02

		lda sprite_pos
		sec
		sbc #$09
		lsr
		lsr

		tax
		jsr pcol_read
		sta coll_temp+$00
		inx
		jsr pcol_read
		sta coll_temp+$01

		lda coll_temp+$00
		cmp #$20
		bne grav_kill

		lda coll_temp+$01
		cmp #$20
		bne grav_kill

		lda #$01
		sta grav_active
		jmp grav_end

; Self mod code for player background collision
pcol_read	lda $6464,x
		rts

; The player is standing on something, so stop falling
grav_kill	lda grav_power
		cmp #$80
		bcc *+$03
		rts

		lda #$00
		sta grav_timer
		sta grav_power
		sta grav_active

		lda sprite_pos+$01
		and #$f8
		clc
		adc #$05
		sta sprite_pos+$01

; Conveyor belt movement checks
grav_end	lda coll_temp+$00
		and #$fe

; Check for a left conveyor
conv_chk_1	cmp #$48
		bne conv_chk_2
		jsr player_left
		jmp conv_chk_out

conv_chk_2	cmp #$4a
		bne conv_chk_3
		jsr player_right
		jmp conv_chk_out

; Check for a right conveyor
conv_chk_3	lda coll_temp+$01
		and #$fe

		cmp #$48
		bne conv_chk_4
		jsr player_left
		jmp conv_chk_out

conv_chk_4	cmp #$4a
		bne conv_chk_out
		jsr player_right

conv_chk_out	rts


; Handle enemy movement
enemy_move	ldx #$00
		ldy #$00

em_loop		lda sprite_dir,y
		beq *+$05
		jmp em2_horizontal


; First enemy movement command processor
em1_horizontal	lda sprite_pos+$02,x
		cmp sprite_x1,y
		beq em1_vertical
		bcc em1_right

em1_left	jsr enemy_left
		jmp em1_vertical

em1_right	jsr enemy_right

em1_vertical	lda sprite_pos+$03,x
		cmp sprite_y1,y
		beq em1_out
		bcc em1_down

em1_up		jsr enemy_up
		jmp em1_out

em1_down	jsr enemy_down

em1_out		lda sprite_pos+$02,x
		cmp sprite_x1,y
		bne em1_noswap

		lda sprite_pos+$03,x
		cmp sprite_y1,y
		bne em1_noswap

		lda #$01
		sta sprite_dir,y

em1_noswap	jmp em2_noswap

; Second enemy movement command processor
em2_horizontal	lda sprite_pos+$02,x
		cmp sprite_x2,y
		beq em2_vertical
		bcc em2_right

em2_left	jsr enemy_left
		jmp em2_vertical

em2_right	jsr enemy_right

em2_vertical	lda sprite_pos+$03,x
		cmp sprite_y2,y
		beq em2_out
		bcc em2_down

em2_up		jsr enemy_up
		jmp em2_out

em2_down	jsr enemy_down

em2_out		lda sprite_pos+$02,x
		cmp sprite_x2,y
		bne em2_noswap

		lda sprite_pos+$03,x
		cmp sprite_y2,y
		bne em2_noswap

		lda #$00
		sta sprite_dir,y

em2_noswap	inx
		inx
		iny
		cpy #$06
		beq *+$05
		jmp em_loop

		rts

; Enemy sprite movement routines
enemy_up	dec sprite_pos+$03,x
		dec sprite_pos+$03,x
		rts

enemy_down	inc sprite_pos+$03,x
		inc sprite_pos+$03,x
		rts

enemy_left	dec sprite_pos+$02,x
		lda #$00
		sta sprite_flip+$01,y
		rts

enemy_right	inc sprite_pos+$02,x
		lda #$08
		sta sprite_flip+$01,y
		rts


; Set up the current screen (based on the level number)
level_init	jsr screen_init_rd	; fetch X position
		cmp #$ff
		beq level_init_2

		tax

		jsr screen_init_rd	; fetch Y position
		tay
		lda screen_low,y	; configure self mod
		sta screen_i_wrt1+$01
		sta screen_i_wrt2+$01
		lda screen_hi,y
		sta screen_i_wrt1+$02
		clc
		adc #screen_col_off
		sta screen_i_wrt2+$02

		jsr screen_init_rd	; fetch character
		sta draw_stash_1
		jsr screen_init_rd	; fetch character colour
		sta draw_stash_2

		jsr screen_init_rd
		tay

; Platfomr draw loop with self mod code
screen_draw	lda draw_stash_1
screen_i_wrt1	sta $6464,x
		eor #$01
		sta draw_stash_1
		lda draw_stash_2
screen_i_wrt2	sta $6464,x
		inx
		dey
		bpl screen_draw

		jmp level_init

; Get the player X and Y positions for this screen
level_init_2	jsr screen_init_rd
		sta respawn_x
		jsr screen_init_rd
		sta respawn_y

; Get the enemy data and set everything up
		ldx #$00
		ldy #$00

li2_loop	jsr screen_init_rd
		cmp #$00
		bne li2_go

; Object is invalid so zero it's X/Y and skip to the next
		sta sprite_pos+$02,y
		sta sprite_pos+$03,y
		sta sprite_x1,x
		sta sprite_x2,x
		sta sprite_y1,x
		sta sprite_y2,x

		jmp li2_skip

; Object is valid so initialise it
li2_go		sta sprite_dp+$01,x

		jsr screen_init_rd	; get sprite X - first position
		sta sprite_pos+$02,y
		sta sprite_x1,x

		jsr screen_init_rd	; get sprite Y - first position
		sta sprite_pos+$03,y
		sta sprite_y1,x

		jsr screen_init_rd	; get sprite X - second position
		sta sprite_x2,x

		jsr screen_init_rd	; get sprite Y - second position
		sta sprite_y2,x

		lda #$01		; set initial movement direction
		sta sprite_dir,x

li2_skip	iny
		iny
		inx
		cpx #$06
		bne li2_loop

; Get the first pick-up item's position
		jsr screen_init_rd
		sta sprite_pos+$0e
		jsr screen_init_rd
		sta sprite_pos+$0f

		rts

; Self mod for the screen init
screen_init_rd	lda $6464
		inc screen_init_rd+$01
		bne *+$05
		inc screen_init_rd+$02
		rts


; Bump the score (10 points)
score_bump_1	ldx #$05
		jmp bs_loop

; Bump the score (1000 points)
score_bump_1000	ldx #$02
		jmp bs_loop

; Bump the score (100 points)
score_bump_100	ldx #$03
bs_loop		lda score,x
		clc
		adc #$01
		cmp #$0a
		beq bs_cnt
		sta score,x
		jmp bs_out

bs_cnt		lda #$00
		sta score,x
		dex
		cpx #$ff
		bne bs_loop

; Score to high score comparison
bs_out		ldx #$00
score_scan	lda score,x
		cmp high_score,x
		beq ss_cnt
		bcc ss_out
		bcs hiscore_update
ss_cnt		inx
		cpx #$06
		bne score_scan
ss_out		rts

; Score to high score copy
hiscore_update	ldx #$00
hsu_loop	lda score,x
		sta high_score,x
		inx
		cpx #$06
		bne hsu_loop
		rts

; Drop the bonus by one unit
bonus_down	lda bonus+$00
		ora bonus+$01
		ora bonus+$02
		bne bonus_down_2
		rts

bonus_down_2	ldx bonus+$03
		inx
		cpx #$10
		bne db2_out

bonus_down_3	ldx #$02
db_loop		lda bonus,x
		sec
		sbc #$01
		cmp #$ff
		beq db_cnt
		sta bonus,x
		jmp db_out

db_cnt		lda #$09
		sta bonus,x
		dex
		cpx #$ff
		bne db_loop

db_out		ldx #$00
db2_out		stx bonus+$03
		rts


; The current high score
high_score	!byte $00,$00,$00,$05,$00,$00

; Blocks of text
status_text	!scr "score"
		!scr " high"
		!scr "lives"
		!scr "level"

boxout_text	!scr "  game  over  "
status_text_r	!scr "bonus "
boxout_bonus	!scr "tmr x5  "
		!scr "issue 100 is finished!"

; Titles page scroller
scrolltext	!scr "  rg rampage   by cosine "

; Pick-up sprite colour table
sprite_glow	!byte $09,$02,$08,$0a,$0f,$07,$01,$01
		!byte $01,$0d,$03,$05,$0e,$04,$0b,$06

; Character colour table
char_glow	!byte $00,$06,$02,$04,$05,$03,$07,$01
		!byte $01,$07,$03,$05,$04,$02,$06,$00

; Screen RAM offsets
screen_low	!byte $00,$28,$50,$78,$a0,$c8,$f0,$18
		!byte $40,$68,$90,$b8,$e0,$08,$30,$58
		!byte $80,$a8,$d0,$f8,$20,$48,$70,$98
		!byte $c0

screen_hi	!byte $bc,$bc,$bc,$bc,$bc,$bc,$bc,$bd
		!byte $bd,$bd,$bd,$bd,$bd,$be,$be,$be
		!byte $be,$be,$be,$be,$bf,$bf,$bf,$bf
		!byte $bf

; Sprite colours
sprite_col	!byte $0a,$0f,$0f,$0f,$0f,$0f,$0f,$01


; Platform data:	X start, Y start, character, colour, width

; Char	Colour		Job
; $40	$0f		warning tape
; $42	$0c/$0d		brick 1
; $44	$0c/$0d		brick 2
; $46	$0e		darker brick
; $48	$0a		left conveyor
; $4a	$0a		right conveyor
; $4c	$09		brighter brick
; $4e	$0a		"classic" brick wall

; Enemy patrol data:	object to use, X1, Y1, X2, Y2
; Make sure that both Y positions are either odd or even!

; Level 1
level_data	!byte $04,$02,$4e,$0a,$0d	; platform data
		!byte $16,$02,$4e,$0a,$0d

		!byte $04,$07,$40,$0f,$03
		!byte $20,$07,$40,$0f,$03

		!byte $04,$0c,$46,$0e,$0d
		!byte $16,$0c,$46,$0e,$0d

		!byte $0e,$11,$40,$0f,$03
		!byte $16,$11,$40,$0f,$03


		!byte $04,$16,$42,$0d,$1f
		!byte $ff

		!byte $84,$cd			; player start position

		!byte $a7,$0e,$1d,$0e,$d1	; enemy data
		!byte $a7,$56,$cd,$56,$1d	; enemy data
		!byte $a7,$9e,$1d,$9e,$d1	; enemy data
		!byte $00
		!byte $00
		!byte $00

		!byte $1e,$cd			; pick-up positions
		!byte $46,$cd
		!byte $46,$a5
		!byte $46,$7d
		!byte $1e,$7d
		!byte $1e,$55
		!byte $1e,$2d

		!byte $46,$2d
		!byte $66,$2d
		!byte $8e,$2d
		!byte $8e,$55
		!byte $8e,$7d
		!byte $66,$7d
		!byte $66,$a5
		!byte $66,$cd
		!byte $86,$cd

		!byte $56,$ad
		!byte $56,$85

		!byte $56,$5d
		!byte $56,$25

		!byte $ff

; Level 2
		!byte $00,$16,$4c,$09,$0b	; platform data
		!byte $0c,$16,$4a,$0a,$0f
		!byte $1c,$16,$4c,$09,$0b

		!byte $04,$02,$48,$0a,$15
		!byte $00,$06,$4a,$0a,$0d
		!byte $04,$0a,$48,$0a,$0d
		!byte $00,$0e,$4a,$0a,$0d

		!byte $1d,$02,$4c,$0f,$03
		!byte $24,$06,$4c,$0f,$03
		!byte $1d,$0a,$4c,$0f,$03
		!byte $24,$0e,$4c,$0f,$03
		!byte $1d,$12,$4c,$0f,$03
		!byte $ff

		!byte $0c,$cd			; player start position

		!byte $a7,$90,$15,$90,$cd	; enemy data
		!byte $a7,$74,$ad,$74,$15
		!byte $a4,$30,$cd,$78,$cd
		!byte $a6,$0c,$b1,$1c,$b1
		!byte $00
		!byte $00

		!byte $24,$cd			; pick-up positions
		!byte $82,$ad
		!byte $82,$6d
		!byte $82,$2d
		!byte $34,$2d
		!byte $1c,$2d
		!byte $34,$6d
		!byte $1c,$6d
		!byte $10,$cd
		!byte $9e,$8d
		!byte $9e,$4d
		!byte $1c,$4d
		!byte $34,$4d
		!byte $1c,$8d
		!byte $34,$8d

		!byte $56,$2d
		!byte $ff

; Level 3
		!byte $00,$06,$43,$0d,$04	; platform data
		!byte $23,$06,$42,$0d,$04
		!byte $08,$0a,$46,$0e,$09
		!byte $16,$0a,$46,$0e,$09
		!byte $00,$0e,$43,$0c,$04
		!byte $23,$0e,$42,$0c,$04
		!byte $06,$12,$46,$0e,$0b
		!byte $16,$12,$46,$0e,$0b
		!byte $00,$16,$43,$0d,$27
		!byte $ff

		!byte $98,$4d			; player start position

		!byte $a7,$56,$15,$56,$cd	; enemy data
		!byte $a3,$28,$6d,$48,$6d
		!byte $a3,$64,$6d,$84,$6d
		!byte $a5,$20,$ad,$48,$ad
		!byte $a5,$64,$ad,$8c,$ad
		!byte $00

		!byte $56,$cd			; pick-up positions
		!byte $9c,$4d
		!byte $10,$8d
		!byte $56,$3d
		!byte $9c,$8d
		!byte $10,$4d
		!byte $66,$6d
		!byte $46,$6d
		!byte $9c,$8d
		!byte $9c,$4d
		!byte $10,$4d
		!byte $10,$8d
		!byte $56,$8d
		!byte $ff

; Level 4
		!byte $00,$02,$4e,$0a,$07	; platform data
		!byte $0c,$02,$4e,$0a,$17

		!byte $00,$07,$44,$0c,$1f

		!byte $04,$0c,$4e,$0a,$17
		!byte $20,$0c,$4e,$0a,$07	; platform data

		!byte $08,$11,$44,$0c,$1f

		!byte $00,$16,$4c,$0f,$27
		!byte $ff

		!byte $0c,$cd			; player start position

		!byte $a4,$44,$2d,$88,$2d	; enemy data
		!byte $a5,$0c,$55,$78,$55
		!byte $a4,$25,$7d,$68,$7d
		!byte $a5,$34,$a5,$a0,$a5
		!byte $00
		!byte $00

		!byte $2c,$a5			; pick-up positions
		!byte $3c,$a5
		!byte $80,$55
		!byte $70,$55
		!byte $5c,$7d
		!byte $4c,$7d
		!byte $3c,$7d
		!byte $38,$cd
		!byte $48,$cd
		!byte $58,$cd
		!byte $68,$cd
		!byte $50,$2d
		!byte $40,$2d
		!byte $98,$7d
		!byte $2c,$cd
		!byte $3c,$cd
		!byte $60,$55
		!byte $50,$55
		!byte $40,$55
		!byte $30,$55
		!byte $20,$55
		!byte $ff

; Level 5
		!byte $00,$10,$40,$09,$05	; platform data
		!byte $0b,$10,$40,$09,$05
		!byte $17,$10,$40,$09,$05
		!byte $22,$10,$40,$09,$05
		!byte $ff

		!byte $0c,$9d			; player start position

		!byte $a6,$28,$15,$28,$cd	; enemy data
		!byte $a6,$56,$cd,$56,$19
		!byte $a6,$84,$15,$84,$cd
		!byte $00
		!byte $00
		!byte $00

		!byte $94,$9d			; pick-up positions
		!byte $18,$9d
		!byte $56,$7d
		!byte $94,$9d
		!byte $3e,$9d
		!byte $6e,$9d
		!byte $18,$9d

		!byte $28,$7d
		!byte $56,$7d
		!byte $84,$7d
		!byte $94,$9d
		!byte $6e,$9d
		!byte $3e,$9d
		!byte $18,$9d

		!byte $ff

; Level 6
		!byte $12,$03,$44,$0c,$03	; platform data
		!byte $12,$0b,$44,$0d,$03
		!byte $12,$13,$44,$0c,$03

		!byte $00,$0f,$4a,$0a,$0b
		!byte $1c,$0f,$48,$0a,$0b
		!byte $00,$06,$4a,$0a,$0b
		!byte $1c,$06,$48,$0a,$0b

		!byte $0c,$0f,$40,$0f,$01
		!byte $1a,$0f,$40,$0f,$01
		!byte $0c,$06,$40,$0f,$01
		!byte $1a,$06,$40,$0f,$01
		!byte $ff

		!byte $56,$75			; player start position

		!byte $a4,$0c,$4d,$34,$4d	; enemy data
		!byte $a5,$79,$4d,$a0,$4d
		!byte $a3,$0c,$8d,$33,$8d
		!byte $a4,$78,$8d,$a0,$8d
		!byte $a6,$46,$15,$46,$d5
		!byte $a6,$66,$d5,$66,$15


		!byte $74,$4d			; pick-up positions
		!byte $74,$95
		!byte $56,$b5
		!byte $38,$95
		!byte $38,$4d
		!byte $56,$35

		!byte $56,$75

		!byte $2c,$95
		!byte $80,$4d
		!byte $2c,$4d
		!byte $80,$95
		!byte $56,$75

		!byte $2c,$95
		!byte $2c,$4d
		!byte $80,$4d

		!byte $56,$b5
		!byte $56,$35

		!byte $ff

; Pull in the external source files
		!src "includes/graphics.asm"
		!src "includes/sfx_driver.asm"
