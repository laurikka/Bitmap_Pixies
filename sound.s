ch1_pos     = $e0
ch2_pos     = $e1
ch3_pos     = $e2
ch1pwl      = $e3
ch2pwl      = $e4
ch3pwl      = $e5
ch1pwh      = $e6
ch2pwh      = $e7
ch3pwh      = $e8
PLAY_FRAME  = $e9

NOTEDELAY   = 4
PWSPEED1    = 9
PWSPEED2    = 7
PWSPEED3    = 5

;    cpu 6510                ; identifier for assembler to target c64
;	org $2000

;    jsr play_init
;    jsr play_start_init
;
;loop
;    ldx $d012               ; load current raster line
;    cpx #$60                ; compare to given number
;    bne loop                ; wait until true
;    lda #1
;    sta $d020,x
;
;    jsr play_start
;
;    lda #0
;    sta $d020,x
;    jmp loop
;
play_init:
    clc
    lda #0
    sta PLAY_FRAME
    sta ch1_pos
    sta ch2_pos
    sta ch3_pos

    ldx #7
    lda #0
:
    sta ch1pwl,x
    dex
    bpl :-

    ldx #20
    lda #0
:
    sta $d400,x
    dex
    bpl :-


    lda #%00001111          ; volume to max
    sta $d418
    rts
    lda #%01000000
    sta $d404
    sta $d40b
    sta $d412
    rts

play_reset:
    lda #0
    ldx #17
:
    sta $d400,x
    dex
    bpl :-
    rts


play_intro:
    jsr pw_update
    lda #%01111110          ; 0-3 decay, 4-7 attack
    sta $d405               ; ch 1
    lda #%10111110          ; 0-3 decay, 4-7 attack
    sta $d40c               ; ch 2
    lda #%11101110          ; 0-3 decay, 4-7 attack
    sta $d413               ; ch 3
    lda #%00001110          ; 0-3 release, 4-7 sustain vol
    sta $d406               ; ch 1
    sta $d40d               ; ch 2
    sta $d414               ; ch 3


    
    inc PLAY_FRAME
    lda PLAY_FRAME
    cmp #4
    bne .skip

    clc
    lda #%01000001
    sta $d404
    sta $d40b
    sta $d412

    lda ch1_pos
    cmp #8
    bne :+
    lda #0
    sta ch1_pos
:
    tay
    lda intro_ch1,y
    tax
    lda notes_lowbyte,x
    sta $d400
    lda notes_highbyte,x
    sta $d401

    lda intro_ch2,y
    tax
    lda notes_lowbyte,x
    sta $d407
    lda notes_highbyte,x
    sta $d408

    lda intro_ch3,y
    tax
    lda notes_lowbyte,x
    sta $d40e
    lda notes_highbyte,x
    sta $d40f

    inc ch1_pos
    lda #0
    sta PLAY_FRAME
.skip
    rts



play_start_init:
    lda #0
    sta PLAY_FRAME
    sta ch1_pos

    lda #%00011010          ; 0-3 decay, 4-7 attack
    sta $d405               ; ch 1
    lda #%00011100          ; 0-3 decay, 4-7 attack
    sta $d40c               ; ch 2
    lda #%00100010          ; 0-3 decay, 4-7 attack
    sta $d413               ; ch 3

    lda #%01001010          ; 0-3 release, 4-7 sustain vol
    sta $d406               ; ch 1
    lda #%01001100          ; 0-3 release, 4-7 sustain vol
    sta $d40d               ; ch 2
    lda #%00100010          ; 0-3 release, 4-7 sustain vol
    sta $d414               ; ch 3

    lda #%01000001
    sta $d404
    sta $d40b

    lda #%01000000
    sta $d412
    rts

play_retrigger_ch1:
    clc
    lda #%01000001
    sta $d404
    rts

play_retrigger_ch2:
    clc
    lda #%01000001
    sta $d40b
    rts

play_retrigger_off:
    clc
    lda #%01000000
    sta $d404
    sta $d40b
    rts

play_start:    

    lda #%01000001
    sta $d412

    jsr pw_update           ; run pulsewidth update

    inc PLAY_FRAME          ; counter to advance
    lda PLAY_FRAME          ; to next note
    cmp #3
    bne .skip_1

    lda ch1_pos             ; current position
    cmp #8                  ; compare
    bne :+                  ; skip if not that
    lda #0                  ; reset to 0
    sta ch1_pos
:
    tay
    lda start_ch1,y
    tax
    lda notes_lowbyte,x     ; get frequency to play
    sta $d400
    lda notes_highbyte,x
    sta $d401

    lda start_ch2,y
    tax
    lda notes_lowbyte,x
    sta $d407
    lda notes_highbyte,x
    sta $d408
    inc ch1_pos

.skip_1
    lda PLAY_FRAME          ; to next note
    cmp #6
    bne .skip

    lda ch1_pos             ; current position
    cmp #8                  ; compare
    bne :+                  ; skip if not that
    lda #0                  ; reset to 0
    sta ch1_pos
:
    tay
    lda start_ch1,y
    tax
    lda notes_lowbyte,x     ; get frequency to play
    sta $d400
    lda notes_highbyte,x
    sta $d401

    lda start_ch2,y
    tax
    lda notes_lowbyte,x
    sta $d407
    lda notes_highbyte,x
    sta $d408
    inc ch1_pos

    lda ch3_pos             ; current position
    cmp #8                  ; compare
    bne :+                  ; skip if not that
    lda #0                  ; reset to 0
    sta ch3_pos
:
    tay
    lda start_ch3,y
    tax
    lda notes_lowbyte,x
    sta $d40e
    lda notes_highbyte,x
    sta $d40f

    lda #%01000000
    sta $d412

    inc ch3_pos
    lda #0
    sta PLAY_FRAME
.skip
    rts

pw_update:
    clc
    lda #PWSPEED1
    adc ch1pwl
    sta ch1pwl
    sta $d402
    lda #0                  ; add carry bit
    adc ch1pwh              ; to pulse high byte
    sta ch1pwh
    sta $d403

    clc
    lda #PWSPEED2
    adc ch2pwl
    sta ch2pwl
    sta $d409
    lda #0
    adc ch2pwh
    sta ch2pwh
    sta $d40a

    clc
    lda #PWSPEED3
    adc ch3pwl
    sta ch3pwl
    sta $d410
    lda #0
    adc ch3pwh
    sta ch3pwh
    sta $d411
    rts

intro_ch1:
    byte  6, 8, 9,11,13,14,15,16
intro_ch2:
    byte 16,18,19,20,21,11,13,14
intro_ch3:
    byte 10,11, 1, 3, 4, 6, 8, 9

start_ch1:
    byte 12,14, 2, 4, 6, 8,10,11
start_ch2:
    byte 16,17,18,20, 8,10,12,14
start_ch3:
    byte  6, 8, 10,12,14,15,16,17

;           1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21
; notes    c-2,d-2,e-2,g-2,a-2,c-3,d-3,e-3,g-3,a-3,c-4,d-4,e-4,g-4,a-4,c-5,d-5,e-5,g-5,a-5,c-6
notes_lowbyte
    byte 0,$54,$DC,$74,$7C,$47,$A8,$B7,$E8,$F8,$8F,$50,$6F,$D0,$F0,$1E,$A0,$DD,$A0,$E1,$3B,$40
notes_highbyte
    byte 0,$04,$04,$05,$06,$07,$08,$09,$0A,$0C,$0E,$11,$13,$15,$19,$1D,$22,$26,$2B,$33,$3A,$45
