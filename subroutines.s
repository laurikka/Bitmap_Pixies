;#################################################################
;##  subroutines                                                ##
;#################################################################




gameinit:
    lda #STARTLEVEL
    sta LEVEL

    lda #$C8                ; reset screen control register
    sta $d016

    lda #$40                ; character to fill screen
    ldy #12                 ; color to fill screen
    jsr clearscreen



    lda #<levels            ; indirect 16-bit adress of levels
    sta LEVELS_P            ; location is stored in two bytes
    lda #>levels            ; in zero page
    sta LEVELS_P+1

    ldy #STARTLEVEL
    beq :++
:
    clc
    lda #$10                ; offset for next level
    adc LEVELS_P            ; add to current level offset
    sta LEVELS_P
    lda #0                  ; possible carry add
    adc LEVELS_P+1
    sta LEVELS_P+1
    dey
    beq :+
    bne :-

:
    clc
    lda #SPRITE_MEM         ; vic-relative sprite memory
    adc #8
    sta SPRITE_LEVEL        ; first set of animated sprites

;# sprite init ################################################
    lda #0
    sta $d015               ; sprite enabled
    sta $d017               ; sprite double height
    sta $d01c               ; sprite multicolor
    sta $d01d               ; sprite double width
    sta $d010               ; sprite bit 8
    sta $d01b               ; sprite bg priority
    sta $d020               ; border color
    sta $d021               ; screen color

 ;## top text ############################################
    ldx #39
    ldy #$40
.loop_top
    lda text,x
    cmp #$20                ; if space character replace with fx-char
    bne :+                  ; if not, skip
    tya
    beq :++                 ; skip to commit
:
    cmp #$60
    bcc :+                  ; if carry set, skip ahead
    sbc #$60
:
    sta SCREEN+1*40,x      ; text location with row offset
    dex
    bpl .loop_top

;## highscore ###########################################
    ldy #4
:
    lda highscore,y
    sta SCREEN+(1*40)+34,y
    dey
    bpl :-

;## bottom text #########################################
    ldx #39
    ldy #$40
.loop_bottom:
    lda text+40,x
    cmp #$20
    bne :+
    tya
    beq :++
:
    cmp #$60
    bcc :+
    sbc #$60
:
    sta SCREEN+23*40,x      ; text location with row offset
    dex
    bpl .loop_bottom

;## bottom color row ####################################
    ldx #6
:
    lda #$75
    sta SCREEN+COLORROW,x
    lda spritecolor+1,x
    sta $d800+COLORROW,x    ; store color to text row with x-offset
    dex                     ; decrement x
    bpl :-                  ; if positive, loop back

;## init timers #########################################
    clc
    lda #0
    sta TIMER_D1
    adc #$30
    sta $8000+23*40+8
    clc
    lda #0
    sta TIMER_D2
    adc #$30
    sta $8000+23*40+7
    clc
    lda #2
    sta TIMER_D3
    adc #$30
    sta $8000+23*40+6

;## screen init ###################################
    ldx #38
    lda #$21
:                           ; top
    sta SCREEN,x
    dex
    bne :-
    ldx #38
    lda #$22
:                           ; bottom
    sta SCREEN+24*40,x
    dex
    bne :-
    ldx #0
    ldy #$23
    clc
:                           ; left
    txa
    adc #40
    tax
    tya
    sta SCREEN,x
    sta SCREEN+$f0,x
    sta SCREEN+$1e0,x
    sta SCREEN+$2d0,x
    cpx #240
    bcc :-
    ldx #0
    ldy #$24
    clc
:                           ; right
    txa
    adc #40
    tax
    tya
    sta SCREEN+$27,x
    sta SCREEN+$117,x
    sta SCREEN+$207,x
    sta SCREEN+$2f7,x
    cpx #240
    bcc :-
                            ;corners
    lda #$25
    sta SCREEN
    lda #$26
    sta SCREEN+39
    lda #$27
    sta SCREEN+24*40
    lda #$28
    sta SCREEN+24*40+39

    clc
    lda #0
    ldx #0
:
    sta ANIMF,x
    adc #2
    inx
    cpx #8
    bne :-

    rts

;## set level ############################
set_level:

    lda #0                  ; reset level timers
    sta LEVEL_F
    sta LEVEL_R
    sta COLLIDED
    sta BONUSTIME_D1
    sta BONUSTIME_D2
    sta BONUSTIME_D3
    sta $d010
    lda #1
    sta $d015               ; reset sprite visibility
    sta NEXT                ; init next sprite to catch
    lda #$ff
    sta SPRITEACTIVE        ; reset active sprites
    ldy #14
    lda (LEVELS_P),y        ; load revealtimer for level
    sta REVEALTIMER
    ldy #15
    lda (LEVELS_P),y        ; delay for moving pixies
    sta MOVEDELAY
    lda #$61
    sta SCREEN+COLORROW     ; flip to active char for colorrow pos 0
    lda #$60
    sta SCREEN+COLORROW+1   ; reset the other colors
    sta SCREEN+COLORROW+2
    sta SCREEN+COLORROW+3
    sta SCREEN+COLORROW+4
    sta SCREEN+COLORROW+5
    sta SCREEN+COLORROW+6

    lda SPRITE_LEVEL
    ldx #0
:
    sta SCREEN+$03f8,x      ; set graphics to first sprite sheet
    inx
    cpx #8
    bne :-

    lda #0
    ldx #15
:
    sta SPEEDX,x            ; reset speed
    dex
    bpl :-

    ldy #12
    ldx #6
:                           ; read sprite coordinates from level data
    clc
    lda (LEVELS_P),y
    bpl :+                  ; if over $80 the extra x-position is needed
    lda singlebits+1,x      ; get the current sprite bit pattern
    eor $d010               ; flip the existing status of x-extra bit
    sta $d010
    lda (LEVELS_P),y
    adc #$80                ; take out the extra positin to get remainder pos
:
    asl                     ; multiply x by 2 because it's stored like that
    sta $d002,y             ; in level data to fit in 8 bits
    iny
    lda (LEVELS_P),y
    sta $d002,y
    dey                     ; decrements to get the next sprite to process
    dey
    dey
    dex
    bpl :--

    jsr feedback_bonustime_clear

    ldy #24                 ; 24 -> ready
    jsr feedback_print      ; print text

    lda #180
    sta $d000               ; init hero sprite x location
    lda #140
    sta $d001               ; init hero sprite y location
    lda #0
    sta SPEEDX
    sta SPEEDX+1
    sta CHARFX_SPR_2
    lda #2
    sta CHARFX_ACT_2

    inc SCREEN+40+9
    lda SCREEN+40+9
    cmp #$3a
    bne :+
    inc SCREEN+40+8
    lda #$30
    sta SCREEN+40+9
:
    lda #0
    sta PLAY_OFFSET
    lda #6
    sta PLAY_DELAY
    jsr play_reset          ; to reset sound envelopes
    jsr play_start_init
    rts

;## move pixies ##########################
move_pixies:
    lda LEVEL_R
    and #%00000111
    clc
    rol
    tax
    lda SPEEDX+2,x
    adc sintable,x
    sta SPEEDX+2,x
    lda SPEEDX+3,x
    adc sintable+9,x
    sta SPEEDX+3,x
    rts

;## border line color ####################

borderlinecolor:
    lda spritecolor,y
    tay

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

;## colorrow update ######################
colorrow_update:
    lda SPRITEACTIVE
    bit singlebits+1
    beq :+
    lda #$61
    sta SCREEN+COLORROW
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW
:
    lda SPRITEACTIVE
    bit singlebits+2
    beq :+
    lda #$61
    sta SCREEN+COLORROW+1
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW+1
:
    lda SPRITEACTIVE
    bit singlebits+3
    beq :+
    lda #$61
    sta SCREEN+COLORROW+2
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW+2
:
    lda SPRITEACTIVE
    bit singlebits+4
    beq :+
    lda #$61
    sta SCREEN+COLORROW+3
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW+3
:
    lda SPRITEACTIVE
    bit singlebits+5
    beq :+
    lda #$61
    sta SCREEN+COLORROW+4
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW+4
:
    lda SPRITEACTIVE
    bit singlebits+6
    beq :+
    lda #$61
    sta SCREEN+COLORROW+5
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW+5
:
    lda SPRITEACTIVE
    bit singlebits+7
    beq :+
    lda #$61
    sta SCREEN+COLORROW+6
    bne :++
:
    lda #$60
    sta SCREEN+COLORROW+6
:
    rts


;## BG FX ###########################################################
;## a moving pattern based on updating single character            ##

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

bgfx_2:
    ldx #7                  ; run for each row in character
    lda SPEEDX              ; get current speed
    tay                     ; transfer speed to y
.shiftx
    clc                     ; clear carry
    tya                     ; get speed from y
    beq .endx               ; if speed is zero, skip to end
    bmi :++                 ; if speed is negative, skip ahead
:                           ;.posx
    lda FXCHAR+8,x            ; load current row of fxchar
    rol                     ; shift bits left
    bcs :++                 ; if bit 7 was not empty, jump
    jmp :++++               ; otherwise ready to store
:                           ;.negx
    lda FXCHAR+8,x
    ror
    bcs :++
    jmp :+++
:                           ;.carryposx
    eor #1                  ; put pixel in first bit
    jmp :++
:                           ;.carrynegx
    eor #$80                ; put pixel in last bit
:                           ;.readyx
    sta FXCHAR+8,x            ; store shifted row
.endx
    dex
    bpl .shiftx

    lda SPEEDX+1            ; get y-speed
    beq :+++                ; if 0, jump to end
    bmi :++                 ; with negative value, skip ahead
:                           ;.posy  move all rows up wrapping first to last
    lda FXCHAR+8
    tay
    lda FXCHAR+8+1
    sta FXCHAR+8
    lda FXCHAR+8+2
    sta FXCHAR+8+1
    lda FXCHAR+8+3
    sta FXCHAR+8+2
    lda FXCHAR+8+4
    sta FXCHAR+8+3
    lda FXCHAR+8+5
    sta FXCHAR+8+4
    lda FXCHAR+8+6
    sta FXCHAR+8+5
    lda FXCHAR+8+7
    sta FXCHAR+8+6
    tya
    sta FXCHAR+8+7
    jmp :++
:                           ;.negy, move all down up wrapping last to first
    lda FXCHAR+8+7
    tay
    lda FXCHAR+8+6
    sta FXCHAR+8+7
    lda FXCHAR+8+5
    sta FXCHAR+8+6
    lda FXCHAR+8+4
    sta FXCHAR+8+5
    lda FXCHAR+8+3
    sta FXCHAR+8+4
    lda FXCHAR+8+2
    sta FXCHAR+8+3
    lda FXCHAR+8+1
    sta FXCHAR+8+2
    lda FXCHAR+8
    sta FXCHAR+8+1
    tya
    sta FXCHAR+8
:                            ; end

bgfx_3:
    ldx #7                  ; run for each row in character
    lda SPEEDX              ; get current speed
    tay                     ; transfer speed to y
.shiftx
    clc                     ; clear carry
    tya                     ; get speed from y
    beq .endx               ; if speed is zero, skip to end
    bmi :++                 ; if speed is negative, skip ahead
:
    lda FXCHAR+16,x         ; load current row of fxchar
    rol                     ; shift bits left
    bcs :++                 ; if bit 7 was not empty, jump
    jmp :++++               ; otherwise ready to store
:
    lda FXCHAR+16,x
    ror
    bcs :++
    jmp :+++
:                           ;.carryposx
    eor #1                  ; put pixel in first bit
    jmp :++
:                           ;.carrynegx
    eor #$80                ; put pixel in last bit
:                           ;.readyx
    sta FXCHAR+16,x         ; store shifted row
.endx
    dex
    bpl .shiftx

    lda SPEEDX+1            ; get y-speed
    beq :+++                ; if 0, jump to end
    bmi :++                 ; with negative value, skip ahead
:                           ;.posy  move all rows up wrapping first to last
    lda FXCHAR+16
    tay
    lda FXCHAR+16+1
    sta FXCHAR+16
    lda FXCHAR+16+2
    sta FXCHAR+16+1
    lda FXCHAR+16+3
    sta FXCHAR+16+2
    lda FXCHAR+16+4
    sta FXCHAR+16+3
    lda FXCHAR+16+5
    sta FXCHAR+16+4
    lda FXCHAR+16+6
    sta FXCHAR+16+5
    lda FXCHAR+16+7
    sta FXCHAR+16+6
    tya
    sta FXCHAR+16+7
    jmp :++
:                           ;.negy, move all down up wrapping last to first
    lda FXCHAR+16+7
    tay
    lda FXCHAR+16+6
    sta FXCHAR+16+7
    lda FXCHAR+16+5
    sta FXCHAR+16+6
    lda FXCHAR+16+4
    sta FXCHAR+16+5
    lda FXCHAR+16+3
    sta FXCHAR+16+4
    lda FXCHAR+16+2
    sta FXCHAR+16+3
    lda FXCHAR+16+1
    sta FXCHAR+16+2
    lda FXCHAR+16
    sta FXCHAR+16+1
    tya
    sta FXCHAR+16
:                            ; end
;    rts

bgfx_4:
    ldx #7                  ; run for each row in character
    lda SPEEDX              ; get current speed
    tay                     ; transfer speed to y
.shiftx
    clc                     ; clear carry
    tya                     ; get speed from y
    beq .endx               ; if speed is zero, skip to end
    bmi :++                 ; if speed is negative, skip ahead
:                           ;.posx
    lda FXCHAR+24,x            ; load current row of fxchar
    rol                     ; shift bits left
    bcs :++                 ; if bit 7 was not empty, jump
    jmp :++++               ; otherwise ready to store
:                           ;.negx
    lda FXCHAR+24,x
    ror
    bcs :++
    jmp :+++
:                           ;.carryposx
    eor #1                  ; put pixel in first bit
    jmp :++
:                           ;.carrynegx
    eor #$80                ; put pixel in last bit
:                           ;.readyx
    sta FXCHAR+24,x            ; store shifted row
.endx
    dex
    bpl .shiftx

    lda SPEEDX+1            ; get y-speed
    beq :+++                ; if 0, jump to end
    bmi :++                 ; with negative value, skip ahead
:                           ;.posy  move all rows up wrapping first to last
    lda FXCHAR+24
    tay
    lda FXCHAR+24+1
    sta FXCHAR+24
    lda FXCHAR+24+2
    sta FXCHAR+24+1
    lda FXCHAR+24+3
    sta FXCHAR+24+2
    lda FXCHAR+24+4
    sta FXCHAR+24+3
    lda FXCHAR+24+5
    sta FXCHAR+24+4
    lda FXCHAR+24+6
    sta FXCHAR+24+5
    lda FXCHAR+24+7
    sta FXCHAR+24+6
    tya
    sta FXCHAR+24+7
    jmp :++
:                           ;.negy, move all down up wrapping last to first
    lda FXCHAR+24+7
    tay
    lda FXCHAR+24+6
    sta FXCHAR+24+7
    lda FXCHAR+24+5
    sta FXCHAR+24+6
    lda FXCHAR+24+4
    sta FXCHAR+24+5
    lda FXCHAR+24+3
    sta FXCHAR+24+4
    lda FXCHAR+24+2
    sta FXCHAR+24+3
    lda FXCHAR+24+1
    sta FXCHAR+24+2
    lda FXCHAR+24
    sta FXCHAR+24+1
    tya
    sta FXCHAR+24
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

;## freeze action ###################################################
;## y should be number of frames                                   ##
;####################################################################
freeze:
:
    ldx $D012               ; load current raster line
    cpx #$fd                ; compare to given number
    bne :-                  ; wait until true
    sty VAR0                ; save y to temp var
    jsr bgfx                ; continue bgfx during freeze
    jsr play_call           ; keep playing sound
    jsr sprite_animation    ; keep updating sprites
    lda CHARFX_ACT          ; if charfx is not zero, jump to subroutine
    beq :+
    jsr charfx
    :
    ldy VAR0                ; restore y
    dey
    bne :--
    rts

;## sprite animation ################################################
sprite_animation:
    clc
    ldx #0
:
    lda ANIMF,x
    adc #1
    cmp #20
    bne :+
    lda #0
:
    sta ANIMF,x
    adc SPRITE_LEVEL
    sta SCREEN+$03f8,x      ; set graphics to first sprite sheet
    inx
    cpx #8
    bne :--
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

feedback_bonustime:         ; print points to screen
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
    adc #$f6
    sta BONUSTIME_D1
    inc BONUSTIME_D2
    clc
    bcc :-
:
    lda BONUSTIME_D2
    cmp #10
    bcc :+
    clc
    adc #$f6
    sta BONUSTIME_D2
    inc BONUSTIME_D3
    clc
    bcc :-
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
    clc
    lda BONUSTIME_D2
    adc TIMER_D2
    sta TIMER_D2
    clc
    lda BONUSTIME_D3
    adc TIMER_D3
    sta TIMER_D3
    clc

:
    lda TIMER_D1
    cmp #10
    bcc :+
    clc
    adc #$f6
    sta TIMER_D1
    inc TIMER_D2
    clc
    bcc :-
:
    lda TIMER_D2
    cmp #10
    bcc :+
    clc
    adc #$f6
    sta TIMER_D2
    inc TIMER_D3
    clc
    bcc :-
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

    jsr play_retrigger_off

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

;## game over #######################################################
game_over:
    clc
    ldx #4
:                           ; copy game score to 4th slot
    lda SCREEN+23*40+34,x
    sta highscore+15,x
    dex
    bpl :-

;compare_scores
;## round 1 #########################################################
    clc
    lda highscore+10    ;14
    cmp highscore+15    ;20
    bcc :+
    bne :++
    lda highscore+11    ;14
    cmp highscore+16    ;20
    bcc :+
    bne :++
    lda highscore+12    ;15
    cmp highscore+17    ;21
    bcc :+
    bne :++
    lda highscore+13    ;16
    cmp highscore+18    ;22
    bcc :+
    bne :++
    lda highscore+14    ;17
    cmp highscore+19    ;23
    bcs :++
:
    lda highscore+15    ;20
    sta highscore+10    ;14
    lda highscore+16    ;20
    sta highscore+11    ;14
    lda highscore+17    ;21
    sta highscore+12    ;15
    lda highscore+18    ;22
    sta highscore+13    ;16
    lda highscore+19    ;23
    sta highscore+14    ;17
:;## round 2 ########################################################
    clc
    lda highscore+5     ;8
    cmp highscore+10     ;14
    bcc :+
    bne :++
    lda highscore+6     ;8
    cmp highscore+11     ;14
    bcc :+
    bne :++
    lda highscore+7     ;9
    cmp highscore+12     ;15
    bcc :+
    bne :++
    lda highscore+8     ;10
    cmp highscore+13     ;16
    bcc :+
    bne :++
    lda highscore+9     ;11
    cmp highscore+14     ;17
    bcs :++
:
    ldy highscore+5     ;8
    lda highscore+10     ;14
    sta highscore+5     ;8
    sty highscore+10     ;14

    ldy highscore+6     ;8
    lda highscore+11     ;14
    sta highscore+6     ;8
    sty highscore+11     ;14

    ldy highscore+7     ;9
    lda highscore+12     ;15
    sta highscore+7     ;9
    sty highscore+12     ;15

    ldy highscore+8     ;10
    lda highscore+13     ;16
    sta highscore+8     ;10
    sty highscore+13     ;16

    ldy highscore+9     ;11
    lda highscore+14     ;17
    sta highscore+9     ;11
    sty highscore+14     ;17
:
:;## round 3 ########################################################
    clc
    lda highscore+0     ;2
    cmp highscore+5     ;8
    bcc :+
    bne :++
    lda highscore+1     ;2
    cmp highscore+6     ;8
    bcc :+
    bne :++
    lda highscore+2     ;3
    cmp highscore+7     ;9
    bcc :+
    bne :++
    lda highscore+3     ;4
    cmp highscore+8     ;10
    bcc :+
    bne :++
    lda highscore+4     ;5
    cmp highscore+9     ;11
    bcs :++
:
    ldy highscore+0     ;2
    lda highscore+5     ;8
    sta highscore+0     ;2
    sty highscore+5     ;8

    ldy highscore+1     ;2
    lda highscore+6     ;8
    sta highscore+1     ;2
    sty highscore+6     ;8

    ldy highscore+2     ;3
    lda highscore+7     ;9
    sta highscore+2     ;3
    sty highscore+7     ;9

    ldy highscore+3     ;4
    lda highscore+8     ;10
    sta highscore+3     ;4
    sty highscore+8     ;10

    ldy highscore+4     ;5
    lda highscore+9     ;11
    sta highscore+4     ;5
    sty highscore+9     ;11
:
    rts

;## trailing sprites ################################################
trailing_sprites:

    ldx #1
    ldy #3

    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d002
    iny
    lda posbuffer,y
    sta $d003
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:
    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d004
    iny
    lda posbuffer,y
    sta $d005
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:
    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d006
    iny
    lda posbuffer,y
    sta $d007
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:
    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d008
    iny
    lda posbuffer,y
    sta $d009
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:
    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d00a
    iny
    lda posbuffer,y
    sta $d00b
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:
    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d00c
    iny
    lda posbuffer,y
    sta $d00d
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:
    lda SPRITEACTIVE
    and singlebits,x
    bne :+++
    lda posbuffer,y
    sta $d00e
    iny
    lda posbuffer,y
    sta $d00f
    lda $d010
    and invertbits,x
    sta VAR0
    iny
    lda posbuffer,y
    and #%00000001
    bne :+
    lda #0
    beq :++
:
    lda singlebits,x
:
    eor VAR0
    sta $d010
    iny
    inx
    jmp :++
:
    iny
    iny
    iny
    inx
:

.end
    clc
    rts

;## charfx #######################################################################
;#  calculate hero sprite position in character coords                           #
;#  offset is per-frame offset into pscale and coord lists                       #
;#  the routine goes through the list until it reaches the previous frame offset #
;#  or comes across zero at the end of the list                                  #
;#  pscale is direct offset into character font                                  #
;#  coords is offset from top left position in 9x7 grid in screen space          #
;#################################################################################

charfx:

    cmp #2                  ; accumulator should have the value of CHARFX_ACT
    bne .anim
; calculate the coordinates the first time charfx is called,
; subsequently it can be skipped until next location is needed

    ldx #0                  ; load xreg with sprite number*2 to calculate
    jsr spr_to_char

    stx CHARFX_MEM          
    stx CHARFX_CMEM

    tya
    sta CHARFX_MEM+1
    adc #$94                ; offset from screen to color memory
    sta CHARFX_CMEM+1

    dec CHARFX_ACT          ; no need to recalculate coordinates for the rest of
                            ; the animation so value goes from 2 to 1
    ldy PREV
    lda spritecolor,y
    sta CHARFX_CLR

.anim:
    ldx CHARFX_FRM
    lda charfx_1_offset-1,x
    sta CHARFX_PRV

    lda charfx_1_offset,x
    tax
    bne .animloop           ; if not zero, jump ahead
    sta CHARFX_FRM          ; reset the animation loop
    sta CHARFX_ACT          ; de-activate charfx
    beq .end
.animloop
    dex
    ldy charfx_1_coords,x
    lda (CHARFX_MEM),y
    cmp #$40                ; check upper and lower limit in font
    bcc :+++                ; if not animatable character, jump ahead
    cmp #$45                ; to avoid drawing over borders and text
    bcs :+++
    lda charfx_1_pscale,x
    sta (CHARFX_MEM),y
    cmp #64
    bne :+
    lda #12
    bne :++
    :
    lda CHARFX_CLR
    :
    sta (CHARFX_CMEM),y
    :
    cpx CHARFX_PRV
    bne .animloop
.end
    inc CHARFX_FRM

    rts


charfx_2:

    cmp #2                  ; accumulator should have the value of CHARFX_ACT
    bne .anim
; calculate the coordinates the first time charfx is called,
; subsequently it can be skipped until next location is needed

    ldx CHARFX_SPR_2
    lda spritecolor,x
    sta CHARFX_CLR_2
    txa
    asl
    tax
    jsr spr_to_char

    stx CHARFX_MEM_2
    stx CHARFX_CMEM_2

    tya
    sta CHARFX_MEM_2+1
    adc #$94                ; offset from screen to color memory
    sta CHARFX_CMEM_2+1

    dec CHARFX_ACT_2          ; no need to recalculate coordinates for the rest of
                            ; the animation so value goes from 2 to 1

.anim:
    ldx CHARFX_FRM_2
    lda charfx_2_offset-1,x
    sta CHARFX_PRV_2

    lda charfx_2_offset,x
    tax
    bne .animloop           ; if not zero, jump ahead
    sta CHARFX_FRM_2          ; reset the animation loop
    sta CHARFX_ACT_2          ; de-activate charfx
    beq .end
.animloop
    dex
    ldy charfx_2_coords,x
    lda (CHARFX_MEM_2),y
    cmp #$40                ; check upper and lower limit in font
    bcc :+++                ; if not animatable character, jump ahead
    cmp #$45                ; to avoid drawing over borders and text
    bcs :+++
    lda charfx_2_pscale,x
    sta (CHARFX_MEM_2),y
    cmp #64
    bne :+
    lda #12
    bne :++
    :
    lda CHARFX_CLR_2
    :
    sta (CHARFX_CMEM_2),y
    :
    cpx CHARFX_PRV_2
    bne .animloop
.end
    inc CHARFX_FRM_2

    rts



;####################################################################
;## sprite to character position                                   ##
;## call witx sprite index*2 in x                                  ##
;## will return with x low byte and y high byte of character ram   ##
;####################################################################

spr_to_char:
    lda carrybit,x          ; get sprite x-pos extra bit position
    and $d010               ; leave only active sprite value
    cmp #0                  ; if any other than zero, put 64 to x-pos
    beq :+
    lda #64
:
    sta CHARFX_X            ; store x extra bit
    lda $d000,x             ; get hero x pos
    lsr                     ; divide by 4
    lsr
    adc CHARFX_X            ; add extra bit to get x-pos in range 1-128
    tay                     ; move it to x register
    lda posx,y              ; get value from table that converts x-pos to char pos
    sta CHARFX_X            ; overwrite the previous value with the converted value

    lda $d001,x             ; get hero y pos

    lsr                     ; divide by 4
    lsr
    clc
    tay                     ; move to x
    lda posy,y              ; get character y pos from table

    tay                     ; move char y-pos to x register
    clc
    lda mul40_s_l,y         ; get y-row from table
    adc CHARFX_X            ; add x-pos to row
    tax

    lda mul40_s_h,y
    adc #0                  ; add carry bit only
    tay
    
    rts