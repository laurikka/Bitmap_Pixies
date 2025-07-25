;#################################################################
;##  titlescreen                                                ##
;#################################################################
titlescreen:

    lda #$20                ; character to fill screen
    ldy #12                 ; color to fill screen
    jsr clearscreen

;# sprite init ################################################

    clc
    lda #0
    sta $c01c               ; sprite multicolor
    sta $d010               ; sprite bit 8
    sta $d020               ; border color
    sta $d017               ; sprite double height
    sta $d01d               ; sprite double width
    sta $d021               ; screen color
    sta $d020               ; border color
    sta $d01b               ; sprites behind bg

    lda #$ff
    sta $d015               ; all sprites visible

    ldy #SPRITE_MEM
    ldx #0
:
    tya
    sta SCREEN+$03f8,x      ; set graphics to first sprite sheet
    iny
    inx
    cpx #8
    bne :-

    lda #80
    sta $d001
    sta $d003
    sta $d005
    sta $d007
    adc #12
    sta $d009
    sbc #12
    sta $d00b
    sta $d00d
    sta $d00f

    clc
    lda #90
    sta $d000
    adc #17
    sta $d002
    adc #18
    sta $d004
    adc #24
    sta $d006
    adc #12
    sta $d008
    adc #28
    sta $d00a
    adc #22
    sta $d00c
    adc #21
    sta $d00e

    lda #%10000000
    sta $d010

;## bottom text #########################################
    ldx #79
.textloop_bottom:
    lda text+80,x
    cmp #$60
    bcc :+
    sbc #$60
:
    sta SCREEN+20*40,x          ; text location with row offset
    dex
    bpl .textloop_bottom

;## high score #########################################
    clc
    ldx #39
.textloop_score:
    lda text+160,x
    cmp #$60
    bcc :+
    sbc #$60
:
    sta SCREEN+12*40,x          ; text location with row offset
    dex
    bpl .textloop_score

    ldy #5
:
    lda highscore,y
    sta SCREEN+(12*40)+20,y
    lda highscore+6,y
    sta SCREEN+(13*40)+20,y
    lda highscore+12,y
    sta SCREEN+(14*40)+20,y
    lda highscore+18,y
    sta SCREEN+(16*40)+20,y
    dey
    bpl :-

    ldx #7
    lda #$4e
:
    adc #2
    sta SCREEN+(18*40)+14,x
    clc
    tay
    lda SPRITECOLOR,x
    sta $d027,x             ; set sprite colors
    sta $d800+(18*40)+14,x  ; store color to text row with x-offset
    tya
    dex                     ; decrement x
    bpl :-                  ; if positive, loop back


    if DEBUG=1
    lda #1
    sta $d020               ; border color
    endif

    if SOUND=1
    jsr play_init
    endif

;#######################################################
;## title main                                        ##
;#######################################################

title_main:

    if DEBUG=1
    lda #2
    sta $d020               ; border color
    endif

    if SOUND=1
    jsr play_intro
    endif

    clc
    lda TITLE_READY
    cmp #1
    bcc :+
    lda $dc00               ; read port A joystick 2 bits
    and #%00001111
    cmp #%00001111
    bne .start_game
:
    ldx #7    
.coloranim
    inc SCREEN+(18*40)+14,x
    lda SCREEN+(18*40)+14,x
    cmp #$60
    bne :+
    lda #$50
:
    sta SCREEN+(18*40)+14,x
    dex                     ; decrement x
    bpl .coloranim          ; if positive, loop back

;## scroller ############################################
    lda SCROLLER_F          ; current value between 0-7
    bpl :++++               ; if not zero, jump ahead
    clc                     ; clear carry
    lda #7
    sta SCROLLER_F          ; reset scroller-timer
    lda #1
    adc SCROLLERPOS         ; increment memory offset
    sta SCROLLERPOS         ; that holds location of scrolltext
    lda #0
    adc SCROLLERPOS+1       ; if carry is set, add it to high
    sta SCROLLERPOS+1       ; byte

    ldy #39
:
    lda (SCROLLERPOS),y     ; indirect location of scrolltext
    beq :++
    cmp #$60
    bcc :+
    sbc #$60
:
    sta SCREEN+24*40,y      ; text location with row offset
    dey
    bpl :--                 ; loop until the whole row is updated
    jmp :++
:
    lda #<scrollertext      ; indirect 16-bit adress of scrolltext
    sta SCROLLERPOS         ; location is stored in two bytes
    lda #>scrollertext      ; in zero page
    sta SCROLLERPOS+1
    lda #0
    sta SCROLLER_F
:

    clc
    ldx TITLE_F
    cpx #36
    bne :+
    ldx #0
    stx TITLE_F
    inc TITLE_READY
:

    if DEBUG=1
    lda #2
    sta $d020               ; border color
    endif
;## sprite movement #####################################
    clc
    lda $d000
    adc sintable+10,x
    sta $d000
    clc
    lda $d002
    adc sintable+12,x
    sta $d002
    clc
    lda $d004
    adc sintable+14,x
    sta $d004
    clc
    lda $d006
    adc sintable+16,x
    sta $d006
    clc
    lda $d008
    adc sintable+18,x
    sta $d008
    clc
    lda $d00a
    adc sintable+20,x
    sta $d00a
    clc
    lda $d00c
    adc sintable+22,x
    sta $d00c

    clc
    lda $d00e
    adc sintable+24,x
    sta $d00e
    bpl :+
    lda #%00000000
    sta $d010
    jmp :++
:
    lda #%10000000
    sta $d010
:

    clc
    lda $d001
    adc sintable,x
    sta $d001
    clc
    lda $d003
    adc sintable+5,x
    sta $d003
    clc
    lda $d005
    adc sintable+10,x
    sta $d005
    clc
    lda $d007
    adc sintable+15,x
    sta $d007
    clc
    lda $d009
    adc sintable+20,x
    sta $d009
    clc
    lda $d00b
    adc sintable+25,x
    sta $d00b
    clc
    lda $d00d
    adc sintable+30,x
    sta $d00d
    clc
    lda $d00f
    adc sintable+35,x
    sta $d00f

    if DEBUG=1
    lda #0
    sta $d020               ; border color
    endif

:
    ldx $D012               ; wait for specific line to
    cpx #$f0                ; use screen control register 
    bne :-

    lda SCROLLER_F          ; use 
    sta $d016


:
    ldx $D012               ; load current raster line
    cpx #$fd                ; compare to given number
    bne :-                  ; wait until true

    lda #$C8                ; reset screen control register
    sta $d016

    if DEBUG=1
    lda #0
    sta $d020               ; border color
    endif

                            ;.ready
    dec SCROLLER_F
    inc TITLE_F
    jmp title_main

.start_game
    lda #0
    sta TITLE_READY
    jmp gameinit
