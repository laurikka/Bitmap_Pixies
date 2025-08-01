    incdir bin

;## directives ##########################################
DEBUG        = 0            ; if 1 includes debug-related stuff
SKIPINTRO    = 0            ; go straight to game

;## constants ###########################################
SCREEN       = $4400        ; screen memory
SCREENSPR    = $4800        ; sprite screen memory
SPRITE_MEM   = $20          ; $20 * $40 equals $800
FONT         = $4000        ; font absolute location
FXCHAR       = FONT+$200    ; character used for moving background FX
COLORROW     = 23*40+16     ; location of color dots at the bottom
MAX_SPEED    = 4            ; max movement speed

LEFT_LIMIT   = 5            ; limits for sprites before they wrap around
RIGHT_LIMIT  = 90
UP_LIMIT     = 24
DOWN_LIMIT   = 250

;## zero page addresses #############################################
VAR0         = $10          ; reusable variables
FEEDBACK_F   = $11          ; delay for clearing feedback from screen
COLLIDED     = $12          ; current collided sprites
LEVEL        = $13          ; hold level number
LEVEL_F      = $14          ; level variable for counting frames
LEVEL_R      = $15          ; level variable for revealing sprites
REVEALTIMER  = $16          ; level variable for delay before reveal
MOVEDELAY    = $17          ; value read from level data
POINTBUFFER  = $18          ; store points before they are processed
NEXT         = $19          ; next sprite to catch
COUNTERX     = $1A          ; countermove for hero sprite movement
COUNTERY     = $1B
CARRY_CUR    = $1C          ; spriteloop temp storage for x extra bit
TEMPY        = $1D          ; spriteloop temp y for processing
X8BIT        = $1E          ; spriteloop temp x 8th bit for processing
COLLECTED    = $1F          ; store collected sprites
BONUSTIME_D1 = $20          ; collect extra time during level
BONUSTIME_D2 = $21
BONUSTIME_D3 = $22
SCROLLERPOS  = $23          ; +$24, pointer to scroller
LEVELS_P     = $25          ; +$26, pointer to level data
SPRITE_LEVEL = $27          ; offset for sprite animation sheet
TITLE_F      = $28          ; framecounter for title screen
TITLE_READY  = $29          ; marker to delay game start immediately after
SCROLLER_F   = $2A          ; counter for character swap
SPRITEACTIVE = $2B          ; which sprites to consider for collisions
TIMER_F      = $2C          ; delays updating timer
TIMER_D1     = $2D          ; three decimal digits for time left
TIMER_D2     = $2E
TIMER_D3     = $2F

ANIMF        = $38          ; $38-3f, current animation frame for sprite
COLLISION    = $40          ; $40-47 bitmasks to compare collided sprites
CARRYBIT     = $48          ; $48-57 for calculating the extra x bit
SINGLEBITS   = $58          ; $58-5f single bit index from low to high

SPEEDX       = $60          ; $60-6f, current speed of sprite
; $e* reserved for sound

;## global init ############################################################

    cpu 6510                ; identifier for assembler to target c64
    org $800                ; program run location

init:
    sei                     ; disable intterrupts

    lda #0
    ldx #$60
:
    sta $10,x               ; clear zeropage between $10-$6f
    dex
    bpl :-                  ; jump to previous anonymous label

    ldx #$20
:                           ; copy to zeropage from tables
    lda to_zeropage,x
    sta $40,x
    dex
    bpl :-

    ldx #35
:
    lda sintable,x          ; duplicate sintable for offsets
    sta sintable+36,x
    dex
    bpl :-

    lda #<scrollertext      ; indirect 16-bit adress of scrolltext
    sta SCROLLERPOS         ; location is stored in two bytes
    lda #>scrollertext      ; in zero page
    sta SCROLLERPOS+1

    lda #%00000010          ; vic bank 1, $4000-$7FFF
    sta $dd00
    lda #%00010000          ; VIC textmode 1, $0800-$0FFF
    sta $d018               ; screen mem 0: $0000-$03FF

    ldx #7
:
    lda spritecolor,x
    sta $d027,x             ; set sprite colors
    dex
    bpl :-

    jsr play_init           ; init sound


;# first go to title screen ###################################

    if SKIPINTRO=0
    jmp titlescreen
    endif

gameinit:
    lda #0
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
    ldy #5
:
    lda highscore,y
    sta SCREEN+(1*40)+33,y
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

;## start level 1 ########################

    jsr set_level           ; subroutine to init level variables



;#################################################################
;#################################################################
;##  main loop                                                  ##
;#################################################################
;#################################################################

main:
    if DEBUG=1
    lda #$a
    sta $d020               ; border color during calculations
    endif

;## loop through sprites #########################################
    clc                     ; clear carry flag
    lda $d010
    sta X8BIT               ; store 8th xbit to zeropage for calculations
    ldx #14                 ; last x-location is 14
spriteloop:
    lda CARRYBIT,x          ; load the 8th bit x-pos for current iteration
    sta CARRY_CUR           ; store it to zero page
    lda $d001,x
    sta TEMPY               ; store current y to zeropage
    lda SPEEDX,x            ; get current speed
    adc COUNTERX            ; add countermove
    clc                     ; clear carry flag
    tay                     ; set negative flag if 7th bit set, y is not used
;sprite_move
    bmi :+                  ; if speed is negative, jump ahead
    adc $d000,x             ; add current speed
    tay                     ; store current x coord
    bcc :+++                ; if carry is not set skip to y
    bcs :++                 ; if carry is set, go flip it
:
    adc $d000,x             ; add current speed
    tay                     ; store current x coord
    bcs :++                 ; if carry is not set skip to y
:
    lda CARRY_CUR           ; get carrybit from zero page
    eor X8BIT               ; flip the current status in the extra x-position
    sta X8BIT               ; store it back
:                           ; move_y
    clc
    lda TEMPY
    adc SPEEDX+1,x
    adc COUNTERY
    sta TEMPY
;check limits
    cpy #RIGHT_LIMIT        ; compare x against limit
    bcc :+                  ; if lower, skip ahead
    lda X8BIT               ; get current 8th bit values
    and CARRY_CUR           ; test against current iteration
    beq :+                  ; if not match, skip over
    eor X8BIT               ; combine with existing pattern
    sta X8BIT               ; store back
    lda #LEFT_LIMIT+1       ; move the sprite to left edge
    tay                     ; store current x coord
    jmp :++                 ; no need to check left
:                           ; checkleft
    cpy #LEFT_LIMIT         ; compare x against limit
    bcs :+
    lda X8BIT
    and CARRY_CUR           
    bne :+
    lda CARRY_CUR
    eor X8BIT              
    sta X8BIT              
    lda #RIGHT_LIMIT   
    tay                     ; store current x coord
:                           ; checktop
    lda TEMPY
    cmp #UP_LIMIT
    bcs :+
    lda #DOWN_LIMIT-1
    sta TEMPY
    jmp :++
:                           ; checkbottom
    cmp #DOWN_LIMIT
    bcc :+
    lda #UP_LIMIT
    sta TEMPY
:                           ; end
    sta $d001,x             ; store y back to hardware
    tya                     ; get currect x coord
    sta $d000,x             ; store back to hardware
    dex                     ; two decrements to go
    dex                     ; to next sprite location
    bpl spriteloop          ; loop back if x not negative

    lda X8BIT
    sta $d010               ; put 8th x bit back to hardware

    jsr sprite_animation

;## collision check #################################################
    if DEBUG=1
    inc $d020               ; border color during calculations
    endif
sprite_collision:
    lda $d01e               ; get the collided sprites
    sta $d01e               ; write back to enable further detection
    and SPRITEACTIVE        ; only consider active sprites
    cmp COLLIDED
    beq .end                ; if same as previous frame, skip ahead
    sta COLLIDED

    ldx #7
:
    lda COLLISION,x         ; load collision bit pattern
    cmp COLLIDED            ; compare against current collision
    beq :+                  ; if equal, jump to commit
    dex
    bne :-
    jmp .end
:                           ; commit
    eor #%11111110          ; flip the bits for collision
        
    and SPRITEACTIVE        ; and with current active sprites
    sta SPRITEACTIVE        ; override old with new active sprites


    ldy #1
:
    lda singlebits,y        ; get the index of collided sprite
    and COLLIDED
    beq :+
    jmp :++
:
    iny
    cpy #8
    bne :--
:
    cpy NEXT                ; compare to bonus point target
    beq :+                  ; if equal, award more points

    inc POINTBUFFER
    clc
    inc BONUSTIME_D1
    inc BONUSTIME_D1
    ldy #1
    jsr feedback_points
    jsr play_retrigger_ch1
    jmp .end

:                           ; five points

    clc
    lda #5
    adc POINTBUFFER
    sta POINTBUFFER
;    clc
    inc BONUSTIME_D1
    inc BONUSTIME_D2

    ldy #1
:
    lda singlebits,y        ; calculate the next bonus target
    and SPRITEACTIVE
    beq :+
    jmp :++
:
    iny
    cpy #8
    bne :--
:
    sty NEXT

    ldy #5
    jsr feedback_points
    jsr play_retrigger_ch2
.end
    jsr colorrow_update
    ldy NEXT
    jsr borderlinecolor
    jsr trailing_sprites

;## enforce maximum speed ###########################################
    if DEBUG=1
    inc $d020               ; border color during calculations
    endif

    ldx #0
:
    lda SPEEDX,x
    bmi :+
    cmp #MAX_SPEED
    bcc :++
    lda #MAX_SPEED
    sta SPEEDX,x
    bcs :++
:                           ;.neg
    cmp #0-MAX_SPEED
    bcs :+
    lda #0-MAX_SPEED
    sta SPEEDX,x
:                           ;.checky
    inx
    cpx #16
    bne :---


;## level reveal ####################################################
    lda LEVEL_F             ; check level timer
    cmp REVEALTIMER         ; compare against reveal delay
    beq :+                  ; if equals, proceed with reveal
    bne :+++                 ; otherwise skip to end
:                           ;.reveal
    ldx LEVEL_R             ; copy sprite count to x
    clc
    cpx #7
    bcs :+
    lda #0
    sta LEVEL_F             ; zero out level timer
    inx                     ; increase revealed sprites count
    stx LEVEL_R
    lda SINGLEBITS,x        ; get sprite bit to activate
    adc $d015
    sta $d015               ; commit it back
    jmp :++
:                           ; end
    inx
    stx LEVEL_R
    lda #0
    sta LEVEL_F             ; zero out level timer
    clc
    cpx MOVEDELAY
    bcc :+
    jsr move_pixies
:
    jsr bgfx


    if DEBUG=1
    lda #0
    sta $d020               ; border color
    endif

;####################################################################
;####################################################################
;##       idle wait 1, mid screed update                           ##
;####################################################################
;####################################################################

idlewait1:
    ldx $d012               ; load current raster line
    cpx #$40                ; compare to given number
    bne idlewait1           ; wait until true

    if DEBUG=1
    lda #6
    sta $d020               ; border color
    endif

    jsr play_start          ; keep sound going

;## joystick read  ##################################################
joystick_read:
    lda LEVEL_R             ; if first sprite is not yet revealed
    beq :+++++              ; skip checking joystick

    lda $dc00               ; read port A joystick 2 bits
:                           ;.read_right
    bit SINGLEBITS+3        ; compare to stored bit pattern
    bne :+                  ; if not active, skip to next
    inc SPEEDX
:                           ;.read_left
    bit SINGLEBITS+2
    bne :+
    dec SPEEDX
:                           ;.read_up
    bit SINGLEBITS
    bne :+
    dec SPEEDX+1            ; sprite 0 y-speed is here
:                           ;.read_down
    bit SINGLEBITS+1
    bne :+
    inc SPEEDX+1
:                           ;.end

;## countermove to move other sprites towards hero sprite ############
    ldx #1                  ; go through twice to get x and y
countermove_calc:
    clc
    lda SPEEDX,x            ; x-speed of hero-sprite
    bmi :++                 ; if negative jump ahead
    cmp #2                  ; compare to value
    bcs :+++                ; if higher than value, jump ahead
:                           ;.zerox
    lda #0
    jmp :+++                ; no countermove
:                           ;.negx
    lda #-2                 ; swap comparison order to get correct carry flag
    cmp SPEEDX,x
    bcc :--                 ; if not higher set zero countermove
    lda #1                  ; counter to positive direction
    jmp :++
:                           ;.posx
    lda #-1                 ; counter to negative direction
:                           ;.storex
    sta COUNTERX,x          ; counterx is x, countery is y
    dex                     ; decrease x
    bpl countermove_calc    ; if not negative, go again

;## empty pointbuffer #################################################
points:
    lda POINTBUFFER         ; get current value in point buffer
    beq .end                ; if zero, skip to end
    dec POINTBUFFER
    ldy #$30                ; screen code for 0
; first digit
    inc SCREEN+23*40+38     ; screen location for score first digit
    lda SCREEN+23*40+38
    cmp #$3a                ; 10 above 0 in screen codes
    bne .end
    sty SCREEN+23*40+38
    inc SCREEN+23*40+37
; second digit
    lda SCREEN+23*40+37     ; screen location for score second digit
    cmp #$3a
    bne .end
    sty SCREEN+23*40+37
    inc SCREEN+23*40+36
.end

;## timer update #####################################################
timer:
    ldy #0
    ldx #9
    lda TIMER_F
    cmp #5                  ; if 5 frames have not passed
    bne .end                ; jump to end
    sty TIMER_F             ; otherwise zero the frame timer
:                           ;.d1
    dec TIMER_D1            ; decrement one from first digit
    bmi :+                  ; if negative, jump to digit 2
    jmp .end                ; otherwise jump to end
:                           ;.d2
    stx TIMER_D1
    dec TIMER_D2
    bmi :+
    jmp .end
:                           ;.d3
    stx TIMER_D2
    dec TIMER_D3            ; if timer digit 3 is negative
    bmi :+                  ; the time has run out
    jmp .end
:                           ;.time_out
    ldy #16                 ; 16 -> time out
    jsr feedback_print      ; print text
    ldy #125    
    jsr freeze
    jsr play_reset
    ldy #2
    jsr freeze
    jsr game_over           ; if time is out game is over
    jmp titlescreen
.end
    lda TIMER_D1
    adc #$30
    sta SCREEN+23*40+8
    clc
    lda TIMER_D2
    adc #$30
    sta SCREEN+23*40+7
    clc
    lda TIMER_D3
    adc #$30
    sta SCREEN+23*40+6

    clc
    lda TIMER_D3
    bne :+
    lda TIMER_D2
    cmp #3
    bcs :+
    ldy FEEDBACK_F
    bpl :+
    ldy #10
    sta FEEDBACK_F
    ldy #8                  ; offset 8 -> "low time"-text
    jsr feedback_print
:

;## update location buffer for previous positions ####################
    if DEBUG=1
    lda #3
    sta $d020               ; border color
    endif
posbuffer_shift:
    ldx #30
:
    lda posbuffer,x
    sta posbuffer+3,x
    lda posbuffer+1,x
    sta posbuffer+4,x
    lda posbuffer+2,x
    sta posbuffer+5,x
    dex
    dex
    dex
    bpl :-

    lda $d000
    sta posbuffer
    lda $d001
    sta posbuffer+1
    lda $d010
    sta posbuffer+2

    if DEBUG=1
    lda #0
    sta $d020               ; border color
    endif

;#################################################################
;##    idle wait 2, things updated after screen update done     ##
;#################################################################
idlewait2:
    ldx $D012               ; load current raster line
    cpx #$fd                ; compare to given number
    bne idlewait2           ; wait until true

    if DEBUG=1
    lda #0
    sta $d020               ; border color
    endif

    jsr feedback_dec

    sta $d01e               ; reset sprite collision
    inc TIMER_F
    inc LEVEL_F


    lda SPRITEACTIVE        ; if spriteactive is 1 then
    cmp #1                  ; all sprites are collected
    bne .ready

    ldy #0                  ; 0 -> level up
    jsr feedback_print      ; print text
    ldy #40
    jsr feedback_bonustime
    ldy #65
    jsr freeze
    clc
    inc LEVEL
    lda LEVEL
    cmp #16
    bne :+
    clc
    lda #<levels            ; indirect 16-bit adress of scrolltext
    sta LEVELS_P            ; location is stored in two bytes
    lda #>levels            ; in zero page
    sta LEVELS_P+1
    jmp :++
:
    clc
    lda #$10                ; offset for next level
    adc LEVELS_P            ; add to current level offset
    sta LEVELS_P
    lda #0                  ; possible carry add
    adc LEVELS_P+1
    sta LEVELS_P+1
:
    clc
    lda SPRITE_LEVEL        ; update pointer for sprites
    adc #20
    bcc :+                  ; if no overflow, skip
    clc
    lda #SPRITE_MEM+8       ; otherwise reset back to first sprite
:
    sta SPRITE_LEVEL
    jsr set_level
.ready
    jmp main

    include titlescreen.s
    include subroutines.s
    include sound.s

;#################################################################
;##  setup variables, to_zeropage is copied to zeropage         ##
;#################################################################

;## bit patterns used for testing conditions
to_zeropage:
collision       byte 0,%00000011,%00000101,%00001001,%00010001,%00100001,%01000001,%10000001
carrybit        byte %00000001,0,%00000010,0,%00000100,0,%00001000,0,%00010000,0,%00100000,0,%01000000,0,%10000000,0
singlebits      byte %00000001,%00000010,%00000100,%00001000,%00010000,%00100000,%01000000,%10000000

;## non-zeropage ################################################
spritecolor     byte 10,2,8,7,5,3,6,4,10
invertbits      byte %11111110,%11111101,%11111011,%11110111,%11101111,%11011111,%10111111,%01111111

text:
    ascii " level 000              hi-score        "
    ascii " time 000                  score 000000 "

    ascii "       collect colors in order          "
    ascii "         for maximum score              "
    ascii "         hi-scores                      "

feedback:
    ascii "level up"
    ascii "low time"
    ascii "time out"
    byte $40
    ascii "ready"
    byte $40,$40
    ascii "  wait  "
    ascii "bonustime +"

scrollertext:
    blk 40,$20              ; 40 spaces before the scroller
    incbin scroller.txt
    blk 40,$20              ; delay at end
    byte 0                  ; terminating byte to start over


;## level data ##################################################
;## x and y coordinates + revealtimer + pad byte               ##
;## x needs to be multplied by 2 to get correct location       ##
;################################################################
levels:
    byte 90, 72, 116, 98, 122, 154, 104, 200, 75, 200, 57, 154, 63, 98, 20, 20
    byte 102, 72, 136, 192, 56, 184, 58, 67, 137, 128, 100, 217, 37, 117, 20, 14
    byte 132, 202, 129, 154, 113, 109, 90, 77, 65, 63, 45, 68, 33, 88, 10, 30
    byte 133, 193, 79, 101, 89, 232, 170, 127, 127, 91, 14, 190, 20, 41, 30, 10
    byte 126, 85, 170, 133, 51, 149, 112, 187, 10, 102, 45, 236, 71, 63, 10, 25
    byte 67, 195, 45, 202, 112, 179, 90, 187, 22, 210, 157, 164, 135, 172, 10, 0
    byte 119, 98, 127, 169, 49, 184, 81, 172, 143, 122, 40, 123, 91, 103, 25, 0
    byte 96, 40, 61, 208, 124, 193, 38, 80, 86, 123, 166, 240, 151, 71, 10, 25
    byte 111, 40, 144, 120, 50, 240, 87, 184, 149, 40, 20, 173, 78, 82, 7, 0
    byte 162, 199, 163, 94, 118, 137, 32, 129, 73, 184, 32, 225, 74, 67, 5, 0
    byte 164, 96, 39, 77, 15, 119, 85, 43, 61, 113, 62, 228, 30, 40, 5, 0
    byte 79, 153, 94, 134, 83, 185, 53, 133, 113, 156, 60, 181, 65, 92, 4, 0
    byte 128, 91, 169, 127, 50, 148, 112, 186, 12, 99, 49, 227, 73, 66, 4, 0
    byte 141, 114, 77, 213, 103, 171, 57, 106, 115, 40, 157, 197, 22, 43, 3, 0
    byte 170, 134, 48, 84, 141, 211, 112, 127, 135, 43, 56, 217, 14, 40, 3, 0
    byte 59, 163, 112, 137, 81, 104, 128, 57, 21, 227, 169, 101, 61, 40, 2, 0


sintable:   ; 36 delta sine values, twice to allow offsets
    byte 1, 2, 2, 1, 1, 1, 1, 0, 1, -1, 0, -1, -1, -1, -1, -2, -2, -1, -1, -2, -2, -1, -1, -1, -1, 0, -1, 1, 0, 1, 1, 1, 1, 2, 2, 1
    blk 36

posbuffer:  ; used to store previous positions of hero sprite
    blk 48

highscore:
    blk 24,$30              ; 4 rows of 6 zeroes

    org FONT
    incbin font.bin             ; 1kb font

    org $4800
    incbin sprite_logo_0.bin    ; 8 slots for logo
    incbin spritesheet_0.bin


