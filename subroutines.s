;#################################################################
;##  subroutines                                                ##
;#################################################################

;## set level ############################
set_level:
    clc
    lda LEVEL_CUR           ; get level value

    asl                     ; calculate offset to fetch
    asl                     ; new level data
    asl
    asl
    adc #12                 ; multiply by 16 and add 12
    tay                     ; y is now the offset for current level

    lda #0                  ; reset level timers
    sta LEVEL_F
    sta LEVEL_R
    sta TARGET              ; reset target color
    lda #1
    sta $d015               ; reset sprite visibility
    sta PREV_CATCH          ; init previous catch variable
    lda levels+2,y          ; load revealtimer for level
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

    ldx #12
.loop                       ; read sprite coordinates from level data
    clc
    lda levels,y
    asl                     ; multiply x
    sta $d002,x             ; store x
    lda levels+1,y
    sta $d002+1,x
    dey
    dey
    dex
    dex
    bpl .loop
    
    ldy SPRITECOLOR+1
    jsr set_borderlinecolor

    lda #180
    sta $d000               ; init hero sprite x location
    lda #140
    sta $d001               ; init hero sprite y location
    lda #0
    sta SPEEDX
    sta SPEEDX+1

    inc TIMER_D3
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
    bvc :++++               ; otherwise ready to store
:                           ;.negx
    lda FXCHAR,x
    ror
    bcs :++
    bvc :+++
:                           ;.carryposx
    eor #1                  ; put pixel in first bit
    bvc :++
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
    bvc :++
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
:                            ;.endy
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
