;
; ROUNDASOUND SFX DRIVER
;


; Label assignments
sfx_temp	= $50		; $04 bytes used
ch1_pitch	= $58
ch2_pitch	= $59
ch3_pitch	= $5a

ch1_speed	= $5b
ch2_speed	= $5c
ch3_speed	= $5d

channel_cnt	= $5e

; Zero off the SID and turn on volume
sfx_init	ldx #$00
		txa
sfx_clrsid_lp	sta $d400,x
		inx
		cpx #$1d
		bne sfx_clrsid_lp

		tay
sfx_clrsid_lp2	sta ch1_pitch,y
		iny
		cpy #$07
		bne sfx_clrsid_lp2

		rts

; Initialise a sound
sfx_call	ldy #$00
sc_copy		lda sound_data+$00,x
		sta sfx_temp+$00,y
		inx
		iny
		cpy #$04
		bne sc_copy

		ldy channel_cnt
		iny
		cpy #$03
		bne *+$04
		ldy #$00
		sty channel_cnt
		lda multi_7,y
		tax

		lda #$00		; gate off waveform
		sta $d404,x

		lda sfx_temp+$00	; pitch
		sta $d401,x
		sta ch1_pitch,y

		lda sfx_temp+$03	; set attack/decay
		sta $d405,x

		lda sfx_temp+$02	; set waveform
		sta $d404,x

		lda sfx_temp+$01
		sta ch1_speed,y

		lda #$0f
		sta $d418
		rts

; Update sound effects
sfx_update	ldx #$00
		ldy #$00

sfx_upd_loop	lda ch1_pitch,y
		clc
		adc ch1_speed,y
		sta ch1_pitch,y
		sta $d401,x

		txa
		clc
		adc #$07
		tax
		iny
		cpy #$03
		bne sfx_upd_loop
		rts

; Times seven table
multi_7		!byte $00,$07,$0e

; Sound effect data
; Note, speed, waveform (with gate set), A/D
sound_data	!byte $10,$03,$11,$09	; jump 1
		!byte $28,$03,$11,$09	; jump 2

		!byte $50,$20,$21,$0a	; item collect

		!byte $20,$8c,$11,$bb	; end of level
		!byte $20,$90,$11,$0a	; start of level

		!byte $20,$f8,$21,$0c	; game over 1
		!byte $10,$ff,$11,$0c	; game over 2

		!byte $c0,$7c,$81,$0b	; dead 1
		!byte $80,$c4,$21,$0b	; dead 2

sd_data_end	!byte $80,$14,$11,$bb	; completion
