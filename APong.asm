
    processor 6502
    include vcs.i
    include macro.i

;-------------------------Constants Below---------------------------------

PADDLEHEIGHT    =   15; 20
BALLHEIGHT  =   3
DIGITHEIGHT =   16
PLAYHEIGHT  =   200 ;184
DIGITY      =   84

SERVECOUNT = 102

; Ball goes from 0 to 182
; Paddle goes from 01 to 169 (184 - PADDLEHEIGHT)


; Ball speeds should be .5 .75 and 1 or $80 $C0 and $100
; or new calc came in at .5 1.0 1.5 or $80 $100 and $180

;-------------------------COLOR CONSTANTS (NTSC)--------------------------

GRAY        =   $00
GOLD        =   $10
ORANGE      =   $20
BURNTORANGE =   $30
RED         =   $40
PURPLE      =   $50
PURPLEBLUE  =   $60
BLUE        =   $70
BLUE2       =   $80
LIGHTBLUE   =   $90
TURQUOISE   =   $A0
GREEN       =   $B0
BROWNGREEN  =   $C0
TANGREEN    =   $D0
TAN         =   $E0
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
ONECOPYQUAD         =   $07
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
PITFALLSOUND    =   7
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


  MAC DEBUG_BRK
    IF DEBUG
      brk                         ;
    ENDIF
  ENDM

  MAC CHECKPAGE
    IF >. != >{1}
      ECHO ""
      ECHO "ERROR: different pages! (", {1}, ",", ., ")"
      ECHO ""
      ERR
    ENDIF
  ENDM


  MAC BIT_B
    .byte   opBIT_B
  ENDM

  MAC BIT_W
    .byte   opBIT_W
  ENDM

;------------------------------Variables----------------------------------

    SEG.U Variables
    org $80


Counter         ds 1
PaddleBase      ds 1


DigitLPtr       ds 2
DigitRPtr       ds 2

DigitLTemp      ds 1
DigitRTemp      ds 1

PaddleLTemp     ds 1
PaddleRTemp     ds 1

PaddleLY        ds 1
PaddleRY        ds 1

PaddleValue     ds 1

BallTemp        ds 1

BallXfrac       ds 1
BallX           ds 1
BallYfrac       ds 1
BallY           ds 1

BallVelXfrac    ds 1
BallVelX        ds 1
BallVelYfrac    ds 1
BallVelY        ds 1


ScoreL          ds 1
ScoreR          ds 1

soundPointer    ds 2
sfxPlay         ds 1
sfxLast         ds 1

gameState       ds 1    ; 0 - Attract - 1 Game
attractLockout  ds 1
serveDelay      ds 1    ; 1.7 seconds is needed for this counter for a count of 102
volleyCounter   ds 1    ; 0-3 slow 4-11 med 12-15 fast

winScore	ds 1    ; 11 or 15 depending on difficulty switch


;-------------------------End Variables-----------------------------------

    SEG Bank0
    org $F000

Start
    CLEAN_START

;--any initial setup

    lda #REFLECTEDPF|SINGLEWIDTHBALL
    sta CTRLPF

    lda #ONECOPYDOUBLE|DOUBLEWIDTHMISSILE
    sta NUSIZ0
    lda #ONECOPYDOUBLE
    sta NUSIZ1

    lda #GRAY+14        ;white
    sta COLUPF
    sta COLUP0
    sta COLUP1


    lda #1
    sta VDELBL
    sta VDELP0


    ; net ranges from 1 to 160
    lda #79         ;net
    ldx #4
    jsr PositionASprite

    lda #38         ;L digit
    ldx #0
    jsr PositionASprite

    ;lda #90        ;R digit
    lda #120
    ldx #1
    jsr PositionASprite

    lda #78
    sta BallX
    lda #80
    sta BallY

    lda #$80
    sta BallYfrac
    sta BallVelXfrac   

    lda #1
    sta BallVelY

    lda #0
    sta ScoreL
    lda #1
    sta ScoreR

    lda #20
    sta attractLockout
    
    lda #0
    sta serveDelay

    jmp MainGameLoop
    
    
Serve
        lda #0
        sta volleyCounter
        lda #1
        sta gameState
        lda #SERVECOUNT
        sta serveDelay
           
        ; reset ball position
        lda #(261/2)
        sta BallXfrac
        
        ; start at slow speed    
        lda #$80
        sta BallVelXfrac
        lda BallVelX
        bmi .neg
        lda #0
        beq .pos
   
.neg    lda #$FF
.pos    sta BallVelX
        
        lda #80
        sta BallX
        rts
    
;-------------------------------------------------------------------------
;--------------GAME MAIN LOOP---------------------------------------------
;-------------------------------------------------------------------------

MainGameLoop

    jsr VBLANKRoutine
    lda sfxPlay         ; NB - this is here to keep the display in check
    bne .nofill         ; if sfxPlay is not true we need to burn a line of the display
    sta WSYNC           ; hence this WSYNC. I stole a line from the start of the Kernel Routine
.nofill                 ; TODO - fix this so it doesn't rely on side effects
    jsr SFXTick
    jsr KernelRoutine
    jsr SFXTick
    jsr OverscanRoutine ; checks paddles and sets new paddle velocity Y
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

    lda SWCHB
    lsr
    bcs .noreset

    lda #0
    sta ScoreL
    sta ScoreR

    jsr Serve
    
.noreset
    ldx serveDelay
    beq .served
    dex
    stx serveDelay  
    bne .serving
.    
.served
    jsr MoveBall
.serving

    jsr UpdateCounters
    jsr SetupDisplayVariables
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
    lda #$00
    sta PF0
    sta PF1
    sta PF2             ;+11    11  draw horizontal line at top of screen
    sta WSYNC
;    sta WSYNC          ; NB - this line was stolen for the main game loop to process an extra SFX tick

    ldy #PLAYHEIGHT     ;+2  2

    lda Counter         ;3
    and #1              ;2
    clc                 ;2
    adc PaddleBase      ;3
    tax                 ;2 +12  14

    lda #255
    sta PaddleValue     ;+5 19  necessary to preload PaddleValue

    SLEEP 40            ;+40    59


    lda #BALLHEIGHT
    dcp BallTemp        ; dec BallTemp cmp BallTemp
    sbc #BALLHEIGHT-2
    sta ENAM0           ;+12    71  draw ball - a little early here.  Oh well.  Quick hack.

    lda #0              ;+2 73
    sta PF2             ;+3 76

;   sta WSYNC
    sta PF0
    sta PF1             ;+6  6  erase horizontal line at top of screen
KernelLoop
    lda #PADDLEHEIGHT
    dcp PaddleLTemp     ; dec PaddleLTemp cmp PaddleLTemp (5 cycles)
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0             ;+14    20  draw L "paddle"

    lda #DIGITHEIGHT-1  ;  2
    dcp DigitLTemp      ;  6    dec DigitLTemp cmp DigitLTemp
    bcs DoDrawDigitL    ;  2/3
    lda #0              ;
    .byte $2C           ;  4 (but covers the next five)
DoDrawDigitL
    lda (DigitLPtr),Y   ;  5
    sta GRP0            ;+18    38  draw L score.  VDELed.  Could use SkipDraw here
                        ;       and save 1 cycle.


    lda INPT0,X         ; 4
    bpl ReadPaddle1     ; 2/3
    .byte $2C           ; 4
ReadPaddle1
    sty PaddleValue     ; 3  (4+3+3) or (4+2+4) =  +10    48  read paddle

    lda #PADDLEHEIGHT
    dcp PaddleRTemp     ; dec PaddleRTemp cmp PaddleRTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0             ;+14    62  draw right "paddle"

    dey                 ;+2 64

    lda #BALLHEIGHT
    dcp BallTemp        ; dec BallTemp cmp BallTemp
    sbc #BALLHEIGHT-2
    sta ENAM0           ;+12    76  draw ball

    lda #PADDLEHEIGHT
    dcp PaddleLTemp     ;dec PaddleLTemp cmp PaddleLTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0             ;+14    14  draw left "paddle"

    lda #DIGITHEIGHT-1
    dcp DigitRTemp      ; dec DigitRtemp cmp DigitRTemp
    bcs DoDrawDigitR
    lda #0
    .byte $2C
DoDrawDigitR
    lda (DigitRPtr),Y
    sta GRP1            ;+18    32  draw right score digit
                        ;       necessary to use DoDraw (or similar)
                        ;       here - because of VDEL GRP1 must be
                        ;       written to every time.

    lda INPT0,X
    bpl ReadPaddle2
    .byte $2C
ReadPaddle2
    sty PaddleValue     ;+10    42  read paddle

    lda #PADDLEHEIGHT
    dcp PaddleRTemp     ; dec PaddleRTemp cmp PaddleRTemp
    sbc #<(PADDLEHEIGHT-64)
    and #$40
    sta PF0             ;+14    56  draw right "paddle"

    tya
    lsr
    sta ENABL           ;+7 63  VDELed - this is the 'net'

    nop                 ;+2 65  Two free cycles

    lda #BALLHEIGHT
    dcp BallTemp        ; dec BallTemp cmp BallTemp
    sbc #BALLHEIGHT-2
    sta ENAM0           ;+12     1  draw ball

    dey
    bne KernelLoop      ;+5  6

    CheckPage KernelRoutine

    lda #$00
    sta PF0
    sta PF1
    sta PF2             ;       draw horizontal line at bottom of screen



    sta WSYNC
    lda #0
    sta ENAM0
    sta ENABL
    sta GRP0
    sta GRP1            ;+14    14  erase all objects (remember that BL and P0 are VDELed)
    sta WSYNC
    sta WSYNC
    lda #0
    sta PF0
    sta PF1
    sta PF2             ;       erase horizontal line at bottom of screen
    sta WSYNC
    rts

;-------------------------------------------------------------------------
;------------------------Overscan Routine---------------------------------
;-------------------------------------------------------------------------

OverscanRoutine subroutine
    lda #2
    sta WSYNC
    sta VBLANK  ;turn on VBLANK
    lda  #34
    sta  TIM64T

    ldx #0
	lda SWCHB
	and #P1DIFF
	bne .setPaddleBase
    ldx #2
.setPaddleBase
    stx PaddleBase

	; Set the winning score to be 11 or 15 depending on Player 0 difficulty switch.
	ldx	#11
	lda SWCHB
	and #P0DIFF
	beq .setWinScore
	ldx #15
.setWinScore
    stx winScore
    stx winScore
    
    ; check collisions first because paddle value will be correct
    jsr CheckCollisions
    lda gameState
    beq .attractMode
    jsr SetPaddleY
    jmp WaitForOverscanEnd
.attractMode
    ldx #201
    stx PaddleLY
    stx PaddleRY
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

CheckCollisions subroutine
;--ball to paddle collision is actually a M0-to-PF collision.  Will have to look at ball X
;   position to determine which paddle is hit.

        lda CXM0FB      ;M0 to PF in bit 7
        asl
        bcc NoBallToPaddleCollision

        ;--so maybe we hit a paddle

        lda BallX
        cmp #80
        bcs .rightSide

        lda BallVelX
        bpl .exit               ; ball already heading correct direction (prevent double trigger)
        
        ldx #00                 ; left paddle
        beq .checkbounce

.rightSide:
       
        lda BallVelX
        bmi .exit               ; ball already heading correct direction (prevent double trigger)
                
        ldx #01                 ; right paddle

.checkbounce
        lda BallY
        clc
        adc #2
        sec
        sbc PaddleLY,X
        bmi .exit
        
.goodl  cmp #16
        bcs .exit
        
.goodh  tax
	
	;lsr
	;sta ScoreR
	;sta ScoreL
	
        lda newvelYl,x
        sta BallVelYfrac
        lda newvelYh,x
        sta BallVelY
       
        sec                     ; 0-velX -> velX
        lda #$00
        sbc BallVelXfrac
        sta BallVelXfrac
        lda #$00
        sbc BallVelX
        sta BallVelX

        
        lda #<PADDLE_HIT
        sta soundPointer
        lda #>PADDLE_HIT
        sta soundPointer+1
        sta sfxPlay             ; starts the SFX
    
       
        ldx volleyCounter
        cpx #4
        beq .med
        
        cpx #12
        beq .fast
        bcs .2
        bcc .1

.fast   lda #$0
        ldy #$1
        bne .updateVel

.med    lda #$c0
        ldy #$0
    
.updateVel
        sta BallVelXfrac
        sty BallVelX        
      
.1      inx      
        stx volleyCounter
        
.2
    
.exit

NoBallToPaddleCollision

    rts

;There are 245 visible lines on the screen in Arcade Pong

; ball is 4 pixels high

; The vertical motion of the ball is influenced by two things, hitting the top or bottom of the screen, or
; hitting one of the paddles. When the ball hits the top or bottom of the screen, it will reverse direction,
; but keep the same speed. When the ball hits the paddle the effect on the vertical motion is determined by
; where on the paddle it hits. The paddle is 16 pixels high which is divided up into 8 regions,
; a hit on these regions affects the vertical position as follows:

; 1 - Up fast
; 2 - Up medium
; 3 - Up slow
; 4 - No vertical motion
; 5 - No vertical motion
; 6 - Down slow
; 7 - Down medium
; 8 - Down fast

; 8 2 pixel high segments


; Hit counter for ball speed increase. Counter determines horizontal ball speed. Counter caps at 15. 
; Counter resets when point scored
;
; HitCounter   H1(11)        H1(3)
; 0-3          0            0
; 4-11         0            1
; 12-15        1            1;
;
;
newvelYl:    .byte $00,$00, $00,$00, $80,$80, $00,$00, $00,$00, $00,$00, $80,$80, $00,$00
newvelYh:    .byte $fe,$fe, $ff,$ff, $ff,$ff, $00,$00, $00,$00, $01,$01, $01,$01, $02,$02
;newvelYl:    .byte $80,$80,$80, $80,$80,$80, $00,$00,$00, $00, $00,$00,$00, $80,$80,$80, $80,$80,$80, $80
;newvelYh:    .byte $fd,$fd,$fd, $fe,$fe,$fe, $ff,$ff,$ff, $00, $01,$01,$01, $01,$01,$01, $02,$02,$02, $02

;-------------------------------------------------------------------------

SetPaddleY subroutine

    lda Counter
    and #1
    tax

    lda PaddleValue
    cmp #PLAYHEIGHT-PADDLEHEIGHT
    bcc PaddleNotTooHigh
    lda #PLAYHEIGHT-PADDLEHEIGHT
;    clc
PaddleNotTooHigh
;    adc PaddleLY,X
;    ror
    sta PaddleLY,X
    rts

;-------------------------------------------------------------------------

MoveBall subroutine

        lda BallVelXfrac
        clc
        adc BallXfrac
        sta BallXfrac
        lda BallX
        adc BallVelX
        sta BallX
        
        lda BallVelYfrac
        clc
        adc BallYfrac
        sta BallYfrac
        lda BallY
        adc BallVelY
        sta BallY

        ;--check boundaries
        ;--if hit any edge, reverse that direction
        ;--if hit left/right edge, increase score of other player
        lda BallX
        sec
        sbc #01
        cmp #(160-1)
        bcc .ballOnScreenX
        
        lda gameState
        bne .gameplay
        
        ;--if attract mode just bounce off left/right side of screen
        ; invert X velocity (2s complement)
        lda BallVelXfrac
        eor #$FF
        clc
        adc #$01
        sta BallVelXfrac
        lda BallVelX
        eor #$FF
        adc #$00
        sta BallVelX
        
        lda BallVelXfrac
        clc
        adc BallXfrac
        sta BallXfrac
        lda BallX
        adc BallVelX
        sta BallX
        
        jmp .ballOnScreenX

.gameplay

        ;--regular gameplay
        ; play off screen sound effect
        lda #<MISSED_BALL
        sta soundPointer
        lda #>MISSED_BALL
        sta soundPointer+1
        sta sfxPlay             ; starts the SFX
        
        jsr Serve
        
        lda BallVelX
        bpl .ballOffRightEdge
        
        ;--ball off left edge
        ;--increase right score
        lda ScoreR
        clc
        adc #$01
        cmp winScore
        bcc .goodscr
        ldx #00
        stx gameState
.goodscr
        sta ScoreR
        jmp .ballOnScreenX
        
.ballOffRightEdge

        ;--ball off right edge
        ;--increase left score
        lda ScoreL
        clc
        adc #$01
        cmp winScore
        bcc .goodscl
        ldx #00
        stx gameState
.goodscl
        sta ScoreL

.ballOnScreenX
        ;--now check top and bottom bounds
        lda BallY
        cmp #PLAYHEIGHT-2
        bcc .ballOnScreenY
        
        ;--ball hit top or bottom
        
        lda gameState
        beq .nosound    ; attract mode has no sound
        
        ; play wall hit sound effect
        lda #<WALL_HIT
        sta soundPointer
        lda #>WALL_HIT
        sta soundPointer+1
        sta sfxPlay             ; starts the SFX
        
.nosound
        ; invert Y velocity (2s complement)
        lda BallVelYfrac
        eor #$FF
        clc
        adc #$01
        sta BallVelYfrac
        lda BallVelY
        eor #$FF
        adc #$00
        sta BallVelY

        ; this should make sure that ball is back in bounds
        lda BallVelYfrac
        clc
        adc BallYfrac
        sta BallYfrac
        lda BallY
        adc BallVelY
        sta BallY
        
.ballOnScreenY

        lda BallX
        ldx #2
        jsr PositionASprite
        
        rts
        
;-------------------------------------------------------------------------

SetupDisplayVariables subroutine

        lda #PLAYHEIGHT+1
        sec
        sbc PaddleLY
        sta PaddleLTemp
        
        lda #PLAYHEIGHT+1
        sec
        sbc PaddleRY
        sta PaddleRTemp
        
        lda #0
        ldx serveDelay
        bne .serving
        
        lda #PLAYHEIGHT
        sec
        sbc BallY
        
.serving    
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
        
        
;       lda #<Digit3-1
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
        
;       lda #<Digit3
        sec
        sbc #DIGITY*2
        clc
        adc #DIGITHEIGHT*2
        sta DigitRPtr
        lda DigitTable+1,Y
        sta DigitRPtr+1
        rts
        
;-------------------------------------------------------------------------

UpdateCounters subroutine

        dec Counter

        rts

;-------------------------------------------------------------------------

    ;align 256

PositionASprite ;call this function with A == horizontal position (0-159)
                ;and X == the object to be positioned (0=P0, 1=P1, 2=M0, etc.)
                ;This function will change A, which
                ;will be the value put into HMxx when returned.
                ;Call this function with at least 14 cycles left in the scanline
                ;(jsr + sec + sta WSYNC + sta HMCLR = 14); it will return 9 cycles
                ;into the second scanline
        sec
        sta HMCLR
        sta WSYNC           ;begin line 1
.divideLoop
        sbc #15
        bcs .divideLoop     ;+4/5    4/ 9.../54
        
        eor #7              ;+2  6/11.../56
        asl
        asl
        asl
        asl                 ;+8 14/19.../64
        
        sta.wx HMP0,X       ;+5 19/24.../69
        
        sta RESP0,X         ;+4 23/28/33/38/43/48/53/58/63/68/73
        sta WSYNC           ;+3  0  begin line 2
        sta HMOVE           ;+3
Return                          ;label for time-wasting 'jsr's

        rts

    CHECKPAGE PositionASprite

;----------------------------------------------------------------------------

;;;;;;;;;; SFX code ;;;;;;;;;;

SFXTick         subroutine

                lda sfxPlay
                beq ExitSFX

                ldy #$00

                lda (soundPointer),y
                cmp sfxLast
                beq .silence
                sta sfxLast
                tax
                beq .silence

                sta AUDF0
                lsr
                lsr
                lsr
                lsr
                lsr
                tay
                lda soundTypeArray,y
                sta AUDC0
.silence
                ldy #$01
                lda (soundPointer),y
                sta AUDV0
                ora sfxLast
                sta sfxPlay

                inc soundPointer
                inc soundPointer
                bne .incrementdone
                inc soundPointer+1
.incrementdone
                rts

ExitSFX         lda #0
                sta AUDV0
                rts


;*************************************************************************

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------


Z_LEAD_B3       = $B4
Z_SQUARE_B4_1   = $1E

soundTypeArray
    byte SQUARESOUND,BASSSOUND,PITFALLSOUND,NOISESOUND,BUZZSOUND,LEADSOUND,SAWSOUND,ENGINESOUND

        align 2

; Ok, the SFX data is stored as audio data byte followed by envelope byte.
; The audio data byte is in Manuel R. format with the top 3 bits index a table to setup the timbre in AUDCx
; and the bottom 5 bits go to setup the frequency in AUDFx
; The envelope byte is just the volume for AUDCx and is done at 120Hz (8.3 ms period)
; (actually it's about 70/30 so it's like 11.6 ms and then 5 ms instead of 8.3/8.3)

; This could be a lot more efficient I realize, but it's just quick and dirty to get the envelope stuff working
MISSED_BALL

    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #8
    .byte #Z_LEAD_B3, #15
    .byte 0,0

;        align 2
;
WALL_HIT
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #15
    .byte #Z_LEAD_B3, #15
    .byte 0,0
;
;
;        align 2
PADDLE_HIT
    .byte #Z_SQUARE_B4_1, #10
    .byte #Z_SQUARE_B4_1, #15
    .byte 0,0


DigitTable
    .word Digit0,Digit1,Digit2,Digit3,Digit4,Digit5,Digit6,Digit7,Digit8,Digit9
    .word Digit10,Digit11,Digit12,Digit13,Digit14,Digit15

; Note all of this digit image data is setup so that the indexing from the kernel
; doesn't cross a page boundary and result in an extra cycle (which would glitch the display)

        org $FA90

Digit0
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
Digit1
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
Digit2
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--

        CHECKPAGE Digit0

        org $FB90

Digit3
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--

Digit4
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
Digit5
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--

enddigit5

        CHECKPAGE Digit3

        org $FC90

Digit6
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--
        .byte 0,#%00001000;--

Digit7
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--

Digit8:
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
enddigit8

        CHECKPAGE Digit6

        org $FD90

Digit9:
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00000001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001001;--
        .byte 0,#%00001111;--
        .byte 0,#%00001111;--

Digit10
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
Digit11
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
enddigit11

        CHECKPAGE Digit9

        org $FE90

Digit12
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
Digit13
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--

Digit14
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        .byte 0,#%10001001;--
        
        
        CHECKPAGE Digit12

        org $FF90
Digit15
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10000001;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001000;--
        .byte 0,#%10001111;--
        .byte 0,#%10001111;--

		CHECKPAGE Digit15
		
;-------------------------------------------------------------------------
;-------------------------End Data----------------------------------------
;-------------------------------------------------------------------------

 ORG $FFF8
 .word 0        ; for supercharger
 .word 0        ; nmi vector
 .word Start    ; start vector
 .word Start    ; brk vector


