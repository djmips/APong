
    processor 6502
    include vcs.h
    include macro.h

;-------------------------Constants Below---------------------------------

PADDLEHEIGHT    =   20
BALLHEIGHT  =   3
DIGITHEIGHT =   10
PLAYHEIGHT  =   184
DIGITY      =   80


;-------------------------COLOR CONSTANTS (NTSC)--------------------------

GRAY        =   $00
GOLD        =   $10
ORANGE      =   $20
BURNTORANGE =   $30
RED     =   $40
PURPLE      =   $50
PURPLEBLUE  =   $60
BLUE        =   $70
BLUE2       =   $80
LIGHTBLUE   =   $90
TURQUOISE   =   $A0
GREEN       =   $B0
BROWNGREEN  =   $C0
TANGREEN    =   $D0
TAN     =   $E0
BROWN       =   $F0

;--------------------------TIA CONSTANTS----------------------------------

    ;--NUSIZx CONSTANTS
    ;   player:
ONECOPYNORMAL       =   $00
TWOCOPIESCLOSE      =   $01
TWOCOPIESMED        =   $02
THREECOPIESCLOSE    =   $03
TWOCOPIESWIDE       =   $04
ONECOPYDOUBLE       =   $05
THREECOPIESMED      =   $06
ONECOPYQUAD     =   $07
    ;   missile:
SINGLEWIDTHMISSILE  =   $00
DOUBLEWIDTHMISSILE  =   $10
QUADWIDTHMISSILE    =   $20
OCTWIDTHMISSILE     =   $30

    ;---CTRLPF CONSTANTS
    ;   playfield:
REFLECTEDPF     =   %00000001
SCOREPF         =   %00000010
PRIORITYPF      =   %00000100
    ;   ball:
SINGLEWIDTHBALL     =   SINGLEWIDTHMISSILE
DOUBLEWIDTHBALL     =   DOUBLEWIDTHMISSILE
QUADWIDTHBALL       =   QUADWIDTHMISSILE
OCTWIDTHBALL        =   OCTWIDTHMISSILE

    ;---HMxx CONSTANTS
LEFTSEVEN       =   $70
LEFTSIX         =   $60
LEFTFIVE        =   $50
LEFTFOUR        =   $40
LEFTTHREE       =   $30
LEFTTWO         =   $20
LEFTONE         =   $10
NOMOVEMENT      =   $00
RIGHTONE        =   $F0
RIGHTTWO        =   $E0
RIGHTTHREE      =   $D0
RIGHTFOUR       =   $C0
RIGHTFIVE       =   $B0
RIGHTSIX        =   $A0
RIGHTSEVEN      =   $90
RIGHTEIGHT      =   $80

    ;---AUDCx CONSTANTS (P Slocum's naming convention)
SAWSOUND        =   1
ENGINESOUND     =   3
SQUARESOUND     =   4
BASSSOUND       =   6
PITFALLSOUND        =   7
NOISESOUND      =   8
LEADSOUND       =   12
BUZZSOUND       =   15

    ;---SWCHA CONSTANTS (JOYSTICK)
J0RIGHT     =   %10000000
J0LEFT      =   %01000000
J0DOWN      =   %00100000
J0UP        =   %00010000
J1RIGHT     =   %00001000
J1LEFT      =   %00000100
J1DOWN      =   %00000010
J1UP        =   %00000001

    ;---SWCHB CONSTANTS (CONSOLE SWITCHES)
P1DIFF      =   %10000000
P0DIFF      =   %01000000
BWCOLOR     =   %00001000
SELECT      =   %00000010
RESET       =   %00000001

;-------------------------End Constants-----------------------------------

;-----------------------------Macros--------------------------------------

    MAC FILLER
        REPEAT {1}
        .byte {2}
        REPEND
    ENDM



;------------------------------Variables----------------------------------

    SEG.U Variables
    org $80


Counter ds 1


DigitLPtr ds 2
DigitRPtr ds 2

DigitLTemp ds 1
DigitRTemp ds 1

PaddleLTemp ds 1
PaddleRTemp ds 1

PaddleLY ds 1
PaddleRY ds 1

PaddleValue ds 1

BallTemp ds 1
BallX ds 1
BallY ds 1
BallDirX ds 1       ;+ equals right/up, - equals left/down
BallDirY ds 1


ScoreL ds 1
ScoreR ds 1         ;bcd



;-------------------------End Variables-----------------------------------

    SEG Bank0
    org $F000

Start
    CLEAN_START

;--any initial setup

    lda #REFLECTEDPF|DOUBLEWIDTHBALL
    sta CTRLPF

    lda #ONECOPYQUAD|DOUBLEWIDTHMISSILE
    sta NUSIZ0
    lda #ONECOPYQUAD
    sta NUSIZ1

    lda #GRAY+14        ;white
    sta COLUPF
    sta COLUP0
    sta COLUP1




    lda #1
    sta VDELBL
    sta VDELP0
    sta BallDirX
    sta BallDirY

    lda #78
    sta BallX

    lda #79         ;net
    ldx #4
    jsr PositionASpriteSubroutine

    lda #45         ;L digit
    ldx #0
    jsr PositionASpriteSubroutine

    lda #95         ;R digit
    ldx #1
    jsr PositionASpriteSubroutine

    lda #80
    sta BallY



;-------------------------------------------------------------------------
;--------------GAME MAIN LOOP---------------------------------------------
;-------------------------------------------------------------------------

MainGameLoop

    jsr VBLANKRoutine
    jsr KernelRoutine
    jsr OverscanRoutine
    jmp MainGameLoop

;-------------------------------------------------------------------------
;-------------------VBLANK Routine----------------------------------------
;-------------------------------------------------------------------------

VBLANKRoutine
    lda #$82
    sta VBLANK          ;for paddles

    lda #%00001111
VSYNCLoop
    sta WSYNC
    sta VSYNC
    lsr
    bcs VSYNCLoop

    lda #43
    sta TIM64T

    jsr UpdateCountersSubroutine

    jsr SetupDisplayVariablesSubroutine

    jsr MoveBallSubroutine

    sta CXCLR

WaitForVblankEnd
    lda INTIM
    bne WaitForVblankEnd

    sta WSYNC
    sta VBLANK  ;turn off VBLANK - it was turned on by overscan

    rts

;-------------------------------------------------------------------------
;----------------------Kernel Routine-------------------------------------
;-------------------------------------------------------------------------

    ;--Notes on kernel:
    ;   left and right "paddles" are drawn with PF0!
    ;   left and right score digits are drawn with P0 (VDELed) and P1
    ;   center "net" is drawn with BL (VDELed)
    ;   ball is drawn with M0
    ;
    ;   Two-line kernel
    ;       Every line:
    ;           ball is drawn
    ;           "paddles" are drawn
    ;           paddle (INPT0 or INPT1, alternating frames,
    ;               indexed with X) is read
    ;       Every other line:
    ;           "net" is drawn
    ;           left and right score digits are drawn

    align 256

KernelRoutine
    sta WSYNC
    lda #$FF
    sta PF0
    sta PF1
    sta PF2         ;+11    11  draw horizontal line at top of screen
    sta WSYNC
    sta WSYNC

    ldy #PLAYHEIGHT     ;+2  2

    lda Counter
    and #1
    tax         ;+7  9

    lda #255
    sta PaddleValue     ;+5 14  necessary to preload PaddleValue

    SLEEP 45            ;+45    59


    lda #BALLHEIGHT
    dcp BallTemp
    sbc #BALLHEIGHT-2
    sta ENAM0           ;+12    71  draw ball - a little early here.  Oh well.  Quick hack.

    lda #0          ;+2 73
    sta PF2         ;+3 76

;   sta WSYNC
    sta PF0
    sta PF1         ;+6  6  erase horizontal line at top of screen
KernelLoop
    lda #PADDLEHEIGHT
    dcp PaddleLTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0         ;+14    20  draw L "paddle"

    lda #DIGITHEIGHT-1
    dcp DigitLTemp
    bcs DoDrawDigitL
    lda #0
    .byte $2C
DoDrawDigitL
    lda (DigitLPtr),Y
    sta GRP0        ;+18    38  draw L score.  VDELed.  Could use SkipDraw here
                ;       and save 1 cycle.


    lda INPT0,X
    bpl ReadPaddle1
    .byte $2C
ReadPaddle1
    sty PaddleValue     ;+10    48  read paddle

    lda #PADDLEHEIGHT
    dcp PaddleRTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0         ;+14    62  draw right "paddle"

    dey         ;+2 64

    lda #BALLHEIGHT
    dcp BallTemp
    sbc #BALLHEIGHT-2
    sta ENAM0       ;+12    76  draw ball

    lda #PADDLEHEIGHT
    dcp PaddleLTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0         ;+14    14  draw left "paddle"

    lda #DIGITHEIGHT-1
    dcp DigitRTemp
    bcs DoDrawDigitR
    lda #0
    .byte $2C
DoDrawDigitR
    lda (DigitRPtr),Y
    sta GRP1        ;+18    32  draw right score digit
                ;       necessary to use DoDraw (or similar)
                ;       here - because of VDEL GRP1 must be
                ;       written to every time.

    lda INPT0,X
    bpl ReadPaddle2
    .byte $2C
ReadPaddle2
    sty PaddleValue     ;+10    42  read paddle

    lda #PADDLEHEIGHT
    dcp PaddleRTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0         ;+14    56  draw right "paddle"

    tya
    lsr
    sta ENABL       ;+7 63  VDELed - this is the 'net'

    nop         ;+2 65  Two free cycles

    lda #BALLHEIGHT
    dcp BallTemp
    sbc #BALLHEIGHT-2
    sta ENAM0       ;+12     1  draw ball

    dey
    bne KernelLoop      ;+5  6

    lda #$FF
    sta PF0
    sta PF1
    sta PF2         ;       draw horizontal line at bottom of screen



    sta WSYNC
    lda #0
    sta ENAM0
    sta ENABL
    sta GRP0
    sta GRP1        ;+14    14  erase all objects (remember that BL and P0 are VDELed)
    sta WSYNC
    sta WSYNC
    lda #0
    sta PF0
    sta PF1
    sta PF2         ;       erase horizontal line at bottom of screen
    sta WSYNC
    rts

;-------------------------------------------------------------------------
;------------------------Overscan Routine---------------------------------
;-------------------------------------------------------------------------

OverscanRoutine




    lda #2
    sta WSYNC
    sta VBLANK  ;turn on VBLANK
    lda  #34
    sta  TIM64T

    jsr SetPaddleYSubroutine

    jsr CheckCollisionsSubroutine

WaitForOverscanEnd
    lda INTIM
    bne WaitForOverscanEnd
    rts

;-------------------------------------------------------------------------
;----------------------------End Main Routines----------------------------
;-------------------------------------------------------------------------


;*************************************************************************

;-------------------------------------------------------------------------
;----------------------Begin Subroutines----------------------------------
;-------------------------------------------------------------------------

CheckCollisionsSubroutine
    ;--ball to paddle collision is actually a M0-to-PF collision.  Will have to look at ball X
    ;   position to determine which paddle is hit.

    lda CXM0FB      ;M0 to PF in bit 7
    asl
    bcc NoBallToPaddleCollision
    ;--so, we hit a paddle.  for now, just reverse X direction
    lda BallDirX
    eor #$FF
    sta BallDirX

NoBallToPaddleCollision

    rts


;-------------------------------------------------------------------------

SetPaddleYSubroutine

    lda Counter
    and #1
    tax

    lda PaddleValue
    cmp #PLAYHEIGHT-PADDLEHEIGHT
    bcc PaddleNotTooHigh
    lda #PLAYHEIGHT-PADDLEHEIGHT
PaddleNotTooHigh
    sta PaddleLY,X

    rts

;-------------------------------------------------------------------------

MoveBallSubroutine
    lda BallDirX
    bmi MoveBallLeft
    inc BallX
    .byte $2C
MoveBallLeft
    dec BallX

    lda BallDirY
    bmi MoveBallDown
    inc BallY
    .byte $2C
MoveBallDown
    dec BallY

    ;--check boundaries
    ;--if hit any edge, reverse that direction
    ;--if hit left/right edge, increase score of other player
    lda BallX
    cmp #1
    bne BallNotAtLeftEdge
    ;--ball hit left edge
    ;--reverse ball X direction
    lda #1
    sta BallDirX
    ;--increase right score
    sed
    lda ScoreR
    clc
    adc #$01
    sta ScoreR
    cld
BallNotAtLeftEdge
    lda BallX
    cmp #159
    bne BallNotAtRightEdge
    ;--ball hit right edge
    ;--reverse ball X direction
    lda #<-1
    sta BallDirX
    ;--increase left score
    sed
    lda ScoreL
    clc
    adc #$01
    sta ScoreL
    cld
BallNotAtRightEdge

    ;--now check top and bottom bounds
    lda BallY
    cmp #PLAYHEIGHT-2
    bne BallNotAtTopEdge
    ;--ball hit top
    ;--reverse ball Y direction
    lda #<-1
    sta BallDirY
BallNotAtTopEdge
    lda BallY
    cmp #1
    bne BallNotAtBottomEdge
    ;--ball hit bottom
    ;--reverse ball Y direction
    lda #0
    sta BallDirY
BallNotAtBottomEdge


    lda BallX
    ldx #2
    jsr PositionASpriteSubroutine

    rts

;-------------------------------------------------------------------------

SetupDisplayVariablesSubroutine

    lda #PLAYHEIGHT+1
    sec
    sbc PaddleLY
    sta PaddleLTemp

    lda #PLAYHEIGHT+1
    sec
    sbc PaddleRY
    sta PaddleRTemp

    lda #PLAYHEIGHT
    sec
    sbc BallY
    sta BallTemp

    lda #PLAYHEIGHT
    lsr
    sec
    sbc #DIGITY
    clc
    adc #DIGITHEIGHT
    sta DigitLTemp

    lda #PLAYHEIGHT
    lsr
    sec
    sbc #DIGITY
    clc
    adc #DIGITHEIGHT
    sta DigitRTemp


    lda ScoreL
    and #$0F
    asl
    tay
    lda DigitTable,Y
    sec
    sbc #1


;   lda #<Digit3-1
    sec
    sbc #DIGITY*2
    clc
    adc #DIGITHEIGHT*2
    sta DigitLPtr
    lda DigitTable+1,Y
    sta DigitLPtr+1


    lda ScoreR
    and #$0F
    asl
    tay
    lda DigitTable,Y

;   lda #<Digit3
    sec
    sbc #DIGITY*2
    clc
    adc #DIGITHEIGHT*2
    sta DigitRPtr
    lda DigitTable+1,Y
    sta DigitRPtr+1
    rts

;-------------------------------------------------------------------------

UpdateCountersSubroutine

    dec Counter

    rts

;-------------------------------------------------------------------------


PositionASpriteSubroutine   ;call this function with A == horizontal position (0-159)
                ;and X == the object to be positioned (0=P0, 1=P1, 2=M0, etc.)
                ;This function will change A, which
                ;will be the value put into HMxx when returned.
                ;Call this function with at least 14 cycles left in the scanline
                ;(jsr + sec + sta WSYNC + sta HMCLR = 14); it will return 9 cycles
                ;into the second scanline
    sec
    sta HMCLR
    sta WSYNC           ;begin line 1
DivideLoop
    sbc #15
    bcs DivideLoop          ;+4/5    4/ 9.../54

    eor #7              ;+2  6/11.../56
    asl
    asl
    asl
    asl             ;+8 14/19.../64

    sta.wx HMP0,X           ;+5 19/24.../69

    sta RESP0,X         ;+4 23/28/33/38/43/48/53/58/63/68/73
    sta WSYNC           ;+3  0  begin line 2
    sta HMOVE           ;+3
Return                          ;label for time-wasting 'jsr's

    rts



;*************************************************************************

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------
DigitTable
    .word Digit0,Digit1,Digit2,Digit3,Digit4,Digit5,Digit6,Digit7,Digit8,Digit9
    org $FDA0

Digit0
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
Digit1
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
        .byte 0,#%01000000;--
Digit2
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10000000;--
        .byte 0,#%10000000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
Digit3
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%01100000;--
        .byte 0,#%01100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--

        org $FEA0


Digit4
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
Digit5
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10000000;--
        .byte 0,#%10000000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
Digit6
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10000000;--
        .byte 0,#%10000000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
Digit7
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%00100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--

        org $FFA0

Digit8
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
        .byte 0,#%10100000;--
        .byte 0,#%10100000;--
        .byte 0,#%11100000;--
        .byte 0,#%11100000;--
Digit9
    .byte 0
        .byte #%11100000;--
            .byte 0
        .byte #%11100000;--
            .byte 0
        .byte #%00100000;--
            .byte 0
        .byte #%00100000;--
            .byte 0
        .byte #%11100000;--
    .byte 0
            .byte #%11100000;--
    .byte 0
            .byte #%10100000;--
    .byte 0
            .byte #%10100000;--
                .byte 0
    .byte #%11100000;--
    .byte 0
    .byte #%11100000;--
;-------------------------------------------------------------------------
;-------------------------End Data----------------------------------------
;-------------------------------------------------------------------------


    org $FFFC
    .word Start
    .word Start

