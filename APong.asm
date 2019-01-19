
	processor 6502
	include vcs.h
	include macro.h

;-------------------------Constants Below---------------------------------

PADDLEHEIGHT	=	20
BALLHEIGHT	=	3
DIGITHEIGHT	=	10
PLAYHEIGHT	=	184
DIGITY		=	80


;-------------------------COLOR CONSTANTS (NTSC)--------------------------

GRAY		=	$00
GOLD		=	$10
ORANGE		=	$20
BURNTORANGE	=	$30
RED		=	$40
PURPLE		=	$50
PURPLEBLUE	=	$60
BLUE		=	$70
BLUE2		=	$80
LIGHTBLUE	=	$90
TURQUOISE	=	$A0
GREEN		=	$B0
BROWNGREEN	=	$C0
TANGREEN	=	$D0
TAN		=	$E0
BROWN		=	$F0

;--------------------------TIA CONSTANTS----------------------------------

	;--NUSIZx CONSTANTS
	;	player:
ONECOPYNORMAL		=	$00
TWOCOPIESCLOSE		=	$01
TWOCOPIESMED		=	$02
THREECOPIESCLOSE	=	$03
TWOCOPIESWIDE		=	$04
ONECOPYDOUBLE		=	$05
THREECOPIESMED		=	$06
ONECOPYQUAD		=	$07
	;	missile:
SINGLEWIDTHMISSILE	=	$00
DOUBLEWIDTHMISSILE	=	$10
QUADWIDTHMISSILE	=	$20
OCTWIDTHMISSILE		=	$30

	;---CTRLPF CONSTANTS
	;	playfield:
REFLECTEDPF		=	%00000001
SCOREPF			=	%00000010
PRIORITYPF		=	%00000100
	;	ball:
SINGLEWIDTHBALL		=	SINGLEWIDTHMISSILE
DOUBLEWIDTHBALL		=	DOUBLEWIDTHMISSILE
QUADWIDTHBALL		=	QUADWIDTHMISSILE
OCTWIDTHBALL		=	OCTWIDTHMISSILE

	;---HMxx CONSTANTS
LEFTSEVEN		=	$70
LEFTSIX			=	$60
LEFTFIVE		=	$50
LEFTFOUR		=	$40
LEFTTHREE		=	$30
LEFTTWO			=	$20
LEFTONE			=	$10
NOMOVEMENT		=	$00
RIGHTONE		=	$F0
RIGHTTWO		=	$E0
RIGHTTHREE		=	$D0
RIGHTFOUR		=	$C0
RIGHTFIVE		=	$B0
RIGHTSIX		=	$A0
RIGHTSEVEN		=	$90
RIGHTEIGHT		=	$80

	;---AUDCx CONSTANTS (P Slocum's naming convention)
SAWSOUND		=	1
ENGINESOUND		=	3
SQUARESOUND		=	4
BASSSOUND		=	6
PITFALLSOUND		=	7
NOISESOUND		=	8
LEADSOUND		=	12
BUZZSOUND		=	15

	;---SWCHA CONSTANTS (JOYSTICK)
J0RIGHT		=	%10000000
J0LEFT		=	%01000000
J0DOWN		=	%00100000
J0UP		=	%00010000
J1RIGHT		=	%00001000
J1LEFT		=	%00000100
J1DOWN		=	%00000010
J1UP		=	%00000001

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

	lda #GRAY+14		;white
	sta COLUPF
	sta COLUP0
	sta COLUP1


	lda #1
	sta VDELBL
	sta VDELP0

	lda #144
	sta BallX

	lda #79			;net
	ldx #4
	jsr PositionASpriteSubroutine

	lda #45			;L digit
	ldx #0			
	jsr PositionASpriteSubroutine

	lda #95			;R digit
	ldx #1
	jsr PositionASpriteSubroutine

	lda #80
	sta BallY

	lda #<Digit3
	sta DigitLPtr
	sta DigitRPtr
	lda #>Digit3
	sta DigitLPtr+1
	sta DigitRPtr+1

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
	sta VBLANK			;for paddles

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

WaitForVblankEnd
	lda INTIM
	bne WaitForVblankEnd

	sta WSYNC
	sta VBLANK	;turn off VBLANK - it was turned on by overscan

	rts

;-------------------------------------------------------------------------
;----------------------Kernel Routine-------------------------------------
;-------------------------------------------------------------------------

	align 256

KernelRoutine
	sta WSYNC
	lda #$FF
	sta PF0
	sta PF1
	sta PF2			;+11	11
	sta WSYNC
	sta WSYNC

	ldy #PLAYHEIGHT		;+2	 2

	lda Counter
	and #1
	tax			;+7	 9

	lda #255
	sta PaddleValue		;+5	14

	SLEEP 54		;+54	68

	lda #0			;+2	70
	sta PF2			;+3	73
	sta WSYNC
	sta PF0
	sta PF1
KernelLoop
	lda #PADDLEHEIGHT
	dcp PaddleLTemp
	sbc #PADDLEHEIGHT-64
	and #$40
	sta PF0			;+14	20

	lda #DIGITHEIGHT-1
	dcp DigitLTemp
	bcs DoDrawDigitL
	lda #0
	.byte $2C
DoDrawDigitL
	lda (DigitLPtr),Y
	sta GRP0		;+18	38


	lda INPT0,X
	bpl ReadPaddle1
	.byte $2C
ReadPaddle1
	sty PaddleValue		;+10	48

	lda #PADDLEHEIGHT
	dcp PaddleRTemp
	sbc #PADDLEHEIGHT-64
	and #$40
	sta PF0			;+14	62

	dey			;+2	64

	lda #BALLHEIGHT
	dcp BallTemp
	sbc #BALLHEIGHT-2
	sta ENAM0		;+12	76

	lda #PADDLEHEIGHT
	dcp PaddleLTemp
	sbc #PADDLEHEIGHT-64
	and #$40
	sta PF0			;+14	14

	lda #DIGITHEIGHT-1
	dcp DigitRTemp
	bcs DoDrawDigitR
	lda #0
	.byte $2C
DoDrawDigitR
	lda (DigitRPtr),Y
	sta GRP1		;+18	32

	lda INPT0,X
	bpl ReadPaddle2
	.byte $2C
ReadPaddle2
	sty PaddleValue		;+10	42

	lda #PADDLEHEIGHT
	dcp PaddleRTemp
	sbc #PADDLEHEIGHT-64
	and #$40
	sta PF0			;+14	56

	tya
	lsr
	sta ENABL		;+7	63	VDEL

	nop			;+2	65

	lda #BALLHEIGHT
	dcp BallTemp
	sbc #BALLHEIGHT-2
	sta ENAM0		;+12	 1

	dey
	bne KernelLoop		;+5	 6

	lda #$FF
	sta PF0
	sta PF1
	sta PF2



	sta WSYNC
	lda #0
	sta ENAM0
	sta ENABL
	sta GRP0
	sta GRP1		;+14	14
	sta WSYNC
	sta WSYNC
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta WSYNC
	rts

;-------------------------------------------------------------------------
;------------------------Overscan Routine---------------------------------
;-------------------------------------------------------------------------

OverscanRoutine




	lda #2
	sta WSYNC
	sta VBLANK	;turn on VBLANK
	lda  #34
	sta  TIM64T

	jsr SetPaddleYSubroutine




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
	lda Counter
	asl
	bcs MoveBallLeft
	inc BallX
	.byte $2C
MoveBallLeft
	dec BallX

	dec BallY



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

	lda #<Digit3-1
	sec
	sbc #DIGITY*2
	clc
	adc #DIGITHEIGHT*2
	sta DigitLPtr

	lda #<Digit3
	sec
	sbc #DIGITY*2
	clc
	adc #DIGITHEIGHT*2
	sta DigitRPtr

	rts

;-------------------------------------------------------------------------

UpdateCountersSubroutine

	dec Counter

	rts

;-------------------------------------------------------------------------


PositionASpriteSubroutine	;call this function with A == horizontal position (0-159)
				;and X == the object to be positioned (0=P0, 1=P1, 2=M0, etc.)
				;This function will change A, which
				;will be the value put into HMxx when returned.
				;Call this function with at least 14 cycles left in the scanline 
				;(jsr + sec + sta WSYNC + sta HMCLR = 14); it will return 9 cycles
				;into the second scanline
	sec
	sta HMCLR
	sta WSYNC			;begin line 1
DivideLoop
	sbc #15
	bcs DivideLoop			;+4/5	 4/ 9.../54

	eor #7				;+2	 6/11.../56
	asl
	asl
	asl
	asl				;+8	14/19.../64

	sta.wx HMP0,X			;+5	19/24.../69

	sta RESP0,X			;+4	23/28/33/38/43/48/53/58/63/68/73
	sta WSYNC			;+3	 0	begin line 2
	sta HMOVE			;+3
Return							;label for time-wasting 'jsr's

	rts



;*************************************************************************

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------

	org $FEA0

Digit3
	.byte #0
        .byte #%11110000
	.byte #0
        .byte #%11110000
	.byte #0
        .byte #%00010000
	.byte #0
        .byte #%00010000
	.byte #0
        .byte #%01110000
	.byte #0
        .byte #%01110000
	.byte #0
        .byte #%00010000
	.byte #0
        .byte #%00010000
	.byte #0
        .byte #%11110000
	.byte #0
        .byte #%11110000







;-------------------------------------------------------------------------
;-------------------------End Data----------------------------------------
;-------------------------------------------------------------------------


	org $FFFC
	.word Start
	.word Start

