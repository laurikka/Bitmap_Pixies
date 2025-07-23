;#################################################################
;##  subroutines                                                ##
;#################################################################

;## set level ############################
set_level:

    lda #0                  ; reset level timers
    sta LEVEL_F
    sta LEVEL_R
    sta TARGET              ; reset target color
    lda #1
    sta $d015               ; reset sprite visibility
    sta PREV_CATCH          ; init previous catch variable
    lda #$ff
    sta SPRITEACTIVE        ; reset active sprites
    ldy #14
    lda (LEVELS_P),y          ; load revealtimer for level
    sta REVEALTIMER
    lda #$61
    sta SCREEN+COLORROW     ; flip to active char for colorrow pos 0
    lda #$60
    sta SCREEN+COLORROW+1   ; reset the other colors
    sta SCREEN+COLORROW+2
    sta SCREEN+COLORROW+3
    sta SCREEN+COLORROW+4
    sta SCREEN+COLORROW+5
    sta SCREEN+COLORROW+6

    ldy #12
    ldx #12
.loop                       ; read sprite coordinates from level data
    clc
    lda (LEVELS_P),y
    asl                     ; multiply x
    sta $d002,x             ; store x
    iny
    lda (LEVELS_P),y
    sta $d002+1,x
    dey
    dey
    dey
    dex
    dex
    bpl .loop

    jsr feedback_bonustime_clear

    ldy #24                 ; 24 -> ready
    jsr feedback_print      ; print text

    ldy SPRITECOLOR+1
    jsr set_borderlinecolor

    lda #180
    sta $d000               ; init hero sprite x location
    lda #140
    sta $d001               ; init hero sprite y location
    lda #0
    sta SPEEDX
    sta SPEEDX+1

;    inc TIMER_D3
    inc SCREEN+40+9
    rts

;## border line color ####################
set_borderlinecolor:
    tya                     ; y should hold spritecolor index
    ldx #0
:                           ;.top
    sta $d800,x
    sta $d800+24*40,x
    inx
    cpx #40
    bcc :-

    ldx #0
:                           ;.sides
    txa
    adc #40
    tax
    tya
    sta $d7ff,x
    sta $d8ef,x
    sta $d9df,x
    sta $dacf,x
    sta $d826,x
    sta $d916,x
    sta $da06,x
    sta $daf6,x
    cpx #240
    bcc :-
    rts

;## BG FX ###########################################################
bgfx:
    ldx #7                  ; run for each row in character
    lda SPEEDX              ; get current speed
    tay                     ; transfer speed to y
.shiftx
    clc                     ; clear carry
    tya                     ; get speed from y
    beq .endx               ; if speed is zero, skip to end
    bmi :++                 ; if speed is negative, skip ahead
:                           ;.posx
    lda FXCHAR,x            ; load current row of fxchar
    rol                     ; shift bits left
    bcs :++                 ; if bit 7 was not empty, jump
    jmp :++++               ; otherwise ready to store
:                           ;.negx
    lda FXCHAR,x
    ror
    bcs :++
    jmp :+++
:                           ;.carryposx
    eor #1                  ; put pixel in first bit
    jmp :++
:                           ;.carrynegx
    eor #$80                ; put pixel in last bit
:                           ;.readyx
    sta FXCHAR,x            ; store shifted row
.endx
    dex
    bpl .shiftx

    lda SPEEDX+1            ; get y-speed
    beq :+++                ; if 0, jump to end
    bmi :++                 ; with negative value, skip ahead
:                           ;.posy  move all rows up wrapping first to last
    lda FXCHAR
    tay
    lda FXCHAR+1
    sta FXCHAR
    lda FXCHAR+2
    sta FXCHAR+1
    lda FXCHAR+3
    sta FXCHAR+2
    lda FXCHAR+4
    sta FXCHAR+3
    lda FXCHAR+5
    sta FXCHAR+4
    lda FXCHAR+6
    sta FXCHAR+5
    lda FXCHAR+7
    sta FXCHAR+6
    tya
    sta FXCHAR+7
    jmp :++
:                           ;.negy, move all down up wrapping last to first
    lda FXCHAR+7
    tay
    lda FXCHAR+6
    sta FXCHAR+7
    lda FXCHAR+5
    sta FXCHAR+6
    lda FXCHAR+4
    sta FXCHAR+5
    lda FXCHAR+3
    sta FXCHAR+4
    lda FXCHAR+2
    sta FXCHAR+3
    lda FXCHAR+1
    sta FXCHAR+2
    lda FXCHAR
    sta FXCHAR+1
    tya
    sta FXCHAR
:                            ; end
    rts

;## clear screen #########################
;## a is character to fill, y is color  ##
clearscreen:
    ldx #$00
:
    sta SCREEN,x
    sta SCREEN+$100,x
    sta SCREEN+$200,x
    sta SCREEN+$300,x
    inx
    bne :-

    tya
:
    sta $d800,x
    sta $d800+$100,x
    sta $d800+$200,x
    sta $d800+$300,x
    inx
    bne :-
    rts

;## freeze action ########################
;## y should be number of frames        ##
freeze:
:
    ldx $D012               ; load current raster line
    cpx #$fd                ; compare to given number
    bne :-                  ; wait until true
    sty VAR0                ; save y to temp var
    jsr bgfx                ; continue bgfx during freeze
    jsr play_start          ; keep playing sound
    ldy VAR0                ; restore y
    dey
    bne :-
    rts

;## sprite copy from memory to vic #######
;## y is how many loops                 ##
;## SPRITEMEM is sprites in memory      ##
;## SPRITEVIC is sprite area in vic     ##
spritecopy:
    lda (SPRITEMEM),y
    sta (SPRITEVIC),y
    dey
    bne spritecopy
    lda (SPRITEMEM),y
    sta (SPRITEVIC),y
    rts

;## feedback routines ####################
feedback_print:             ; print text to screen
    ldx #0
.textloop
    lda feedback,y          ; y is offset to text table
    cmp #$60
    bcc :+
    sbc #$60
:
    sta SCREEN+10*40+16,x   ; text location with row offset
    iny
    inx
    cpx #8
    bne .textloop
    lda #25
    sta FEEDBACK_F
    rts

feedback_points:            ; print points to screen
    lda #$40
    sta SCREEN+10*40+16
    sta SCREEN+10*40+17
    sta SCREEN+10*40+18
    lda #$2b                ; "+"-sign
    sta SCREEN+10*40+19
    tya                     ; points to print should be in y
    clc
    adc #$30
    sta SCREEN+10*40+20
    lda #$40
    sta SCREEN+10*40+21
    sta SCREEN+10*40+22
    sta SCREEN+10*40+23
    lda #15
    sta FEEDBACK_F
    rts

feedback_bonustime:            ; print points to screen
    ldx #0
.textloop
    lda feedback,y          ; y is offset to text table
    cmp #$60
    bcc :+
    sbc #$60
:
    sta SCREEN+12*40+12,x   ; text location with row offset
    iny
    inx
    cpx #11
    bne .textloop
    clc
:
    lda BONUSTIME_D1
    cmp #10
    bcc :+
    clc
    inc BONUSTIME_D2
;    lda BONUSTIME_D1
    sbc #10
    sta BONUSTIME_D1
    clc
    bcs :-
:
    clc
    lda BONUSTIME_D2
    cmp #10
    bcc :+
    clc
    inc BONUSTIME_D3
;    lda BONUSTIME_D2
    sbc #10
    sta BONUSTIME_D2
    clc
    bcs :-
:
    lda BONUSTIME_D3
    adc #$30
    sta SCREEN+12*40+23
    lda BONUSTIME_D2
    adc #$30
    sta SCREEN+12*40+24
    lda BONUSTIME_D1
    adc #$30
    sta SCREEN+12*40+25

    clc
    lda BONUSTIME_D1
    adc TIMER_D1
    sta TIMER_D1
    lda #0
    adc TIMER_D2
    sta TIMER_D2
    clc
    lda BONUSTIME_D2
    adc TIMER_D2
    sta TIMER_D2
    lda #0
    adc TIMER_D3
    sta TIMER_D3
    clc
    lda BONUSTIME_D3
    adc TIMER_D3
    sta TIMER_D3
    clc

    clc
:
    lda TIMER_D1
    cmp #10
    bcc :+
    clc
    inc TIMER_D2
;    lda TIMER_D1
    sbc #10
    sta TIMER_D1
    clc
    bcs :-
:
    clc
    lda TIMER_D2
    cmp #10
    bcc :+
    clc
    inc TIMER_D3
;    lda TIMER_D2
    sbc #10
    sta TIMER_D2
    clc
    bcs :-
:


    lda #0
    sta BONUSTIME_D1
    sta BONUSTIME_D2
    sta BONUSTIME_D3
    rts

feedback_bonustime_clear:
    ldx #13
    lda #$40
:
    sta SCREEN+12*40+12,x          ; text location with row offset
    dex
    bpl :-
    rts

feedback_clear:
    ldx #7
    lda #$40
:
    sta SCREEN+10*40+16,x          ; text location with row offset
    dex
    bpl :-
    rts

feedback_dec:
    lda FEEDBACK_F
    bmi :+++
    beq :+
    jmp :++
:
    jsr feedback_clear
:
    dec FEEDBACK_F
:
    rts

;## game over ############################
game_over:
    clc
    ldx #5
:                           ; copy game score to 4th slot
    lda SCREEN+23*40+33,x
    sta highscore+18,x
    dex
    bpl :-
    ldx #0

.compare3rd                 ; compare 4th to 3rd
    clc
    lda highscore+18,x
    cmp highscore+12,x
    bcc :+                  ; new score not higher, skip equality
    bne :++                 ; if not equal, must be higher
:
    inx
    cpx #6
    bne .compare3rd
    jmp .compare2nd
:                           ; replace 3rd slot with new score
    ldx #5
:
    lda highscore+18,x
    sta highscore+12,x
    dex
    bpl :-

.compare2nd
    inx
    clc
    lda highscore+12,x
    cmp highscore+6,x
    bcc :+
    bne :++
:
    cpx #6
    bne .compare2nd
    jmp .compare1st
:
    ldx #5
:
    lda highscore+6,x       ; swap slots between 2nd and 3rd
    tay
    lda highscore+12,x
    sta highscore+6,x
    tya
    sta highscore+12,x
    dex
    bpl :-

.compare1st
    inx
    clc
    lda highscore+6,x
    cmp highscore,x
    bcc :+
    bne :++
:
    cpx #6
    bne .compare1st
    jmp .end
:
    ldx #5
:
    lda highscore,x
    tay
    lda highscore+6,x
    sta highscore,x
    tya
    sta highscore+6,x
    dex
    bpl :-
.end

    jmp titlescreen
