;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/KONEY/"
	SECTION	"Code+PT12",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"Blitter-Register-List.S"	;use if you like ;)
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
w=320		;screen width, height, depth
h=256
bpls=4		;handy values:
bpl=w/16*2	;byte-width of 1 bitplane line (40)
bwid=bpls*bpl	;byte-width of 1 pixel line (all bpls)
POS_TOP=124*bpl
POS_LEFT=16
POS_MID=4
POS_RIGHT=20
POS_BOTTOM=122*bpl
BAND_OFFSET=86*bpl
;*************
SONG_POSITION_JUMP=0	;38
;BLITTER CONSTANTS
bltx	=0
;blty	=0
bltoffs	=210*(w/8)+bltx/8
;blth	=12
;bltw	=320/16
;bltskip	=(320-320)/8

;********** Demo **********	;Demo-specific non-startup code below.
Demo:	;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	move.l	#VBint,$6c(a4)
	move.w	#%1110000000100000,INTENA
	;** SOMETHING INSIDE HERE IS NEEDED TO MAKE MOD PLAY! **
	;move.w	#%1110000000000000,INTENA	; Master and lev6	; NO COPPER-IRQ!

	move.w	#$87c0,DMACON
	;*--- clear screens ---*
	lea	Screen1,a1
	bsr.w	ClearScreen
	lea	Screen2,a1
	bsr.w	ClearScreen
	bsr	WaitBlitter
	;*--- start copper ---*
	lea	Screen1,a0
	moveq	#bpl,d0
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	;PARAMS&ROUTINE
	MOVE.L	#GLITCHBUFFER,GLITCHER_DEST
	MOVE.L	#bpls-2,GLITCHER_DPH
	BSR.W	__FILLRNDBG

	;PARAMS&ROUTINE
	;MOVE.L	#Module1,GLITCHER_SRC
	;MOVE.L	KONEYBG,GLITCHER_DEST
	;MOVE.L	#bpls-2,GLITCHER_DPH
	;BSR.W	__FILLGLITCHBG

	BSR.W	__CREAPATCH		; FILL THE BUFFER
	;BSR.W	__CREATESCROLLSPACE	; NOW WE USE THE BLITTER HERE!

	BSR.W	__InitCopperPalette
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	;---  Call P61_Init  ---
	MOVEM.L	D0-A6,-(SP)
	lea	Module1,a0
	sub.l	a1,a1
	sub.l	a2,a2
	moveq	#0,d0
	;MOVE.W	#20,P61_InitPos	; TRACK START OFFSET
	jsr	P61_Init
	MOVEM.L (SP)+,D0-A6

	MOVE.L	#Copper,$80(a6)

;********************  main loop  ********************
MainLoop:
	move.w	#$12c,d0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	movem.l	DrawBuffer(PC),a2-a3
	exg	a2,a3
	movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	move.l	a3,a0
	move.l	#bpl*256,d0
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1
	;bsr	ClearScreen

	bsr	WaitBlitter
	BSR.W	__SET_PT_VISUALS
	MOVE.L	KONEYBG,DrawBuffer

	; do stuff here :)

	ifne SONG_POSITION_JUMP
	;---  change position  ---
	MOVE.W	P61_Pos,D5
	CMP.W	#1,D5		; seqeunce block position
	BNE.S	.dontJump	; then switch
	MOVEM.L	D0-A6,-(SP)
	;MOVE.W	#$0FF,$180(A6)	; show rastertime left down to $12c
	MOVE.W	#SONG_POSITION_JUMP+1,P61_LAST_POS	; RESET POSITION COUNTER
	CLR.L	D0
	MOVEQ	#SONG_POSITION_JUMP,D0
	JSR	P61_SetPosition
	MOVE.W	#0,P61_FRAMECOUNT	; FORCE NEW FRAME
	MOVEM.L (SP)+,D0-A6
	.dontJump:
	endc

SONG_POSITION_EVENTS:
	;* FOR TIMED EVENTS ON SELECTED FRAME ****
	MOVE.W	P61_Pos,D5
	CMP.W	P61_LAST_POS,D5
	BNE.S	.dontReset
	MOVE.W	#0,P61_FRAMECOUNT
	ADD.W	#1,P61_LAST_POS
	.dontReset:
	MOVE.W	P61_FRAMECOUNT,D4
	ADD.W	#1,D4
	MOVE.W	D4,P61_FRAMECOUNT

	; SHUFFLE ALL SCREENS - JUST A TEST
	;CMPI.W	#13,D4		; BEATS * 14frames
	;BGE.S	.doNothing6
	;ADD.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	;.doNothing6:
	;CMPI.W	#15,D4		; BEATS * 14frames
	;BLO.S	.doNothing7
	;CMPI.W	#27,D4		; BEATS * 14frames
	;BGE.S	.doNothing7
	;SUB.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	;.doNothing7:
	; SHUFFLE ALL SCREENS - JUST A TEST

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#5,D5		; SONG POSITION
	BNE.S	.doNothing0
	CMPI.W	#180,D4		; BEATS * 14frames
	BLO.S	.doNothing0
	CMPI.W	#201,D4	
	BGE.S	.doNothing0
	BSR.W	__HW_DISPLACE

	CMPI.W	#200,D4		; BEATS * 14frames
	BNE.S	.doNothing0

	ADD.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing0:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#15,D5		; SONG POSITION
	BNE.S	.doNothing1

	CMPI.W	#100,D4
	BLO.S	.dontDisplace4
	CMPI.W	#108,D4
	BGE.S	.dontDisplace4
	BSR.W	__HW_DISPLACE
	.dontDisplace4:

	CMPI.W	#190,D4
	BLO.S	.dontDisplace5
	CMPI.W	#196,D4
	BGE.S	.dontDisplace5
	BSR.W	__HW_DISPLACE
	.dontDisplace5:

	CMPI.W	#1,D4		; BEATS * 14frames
	BNE.S	.doNothing1
	ADD.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing1:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#25,D5		; SONG POSITION
	BNE.S	.doNothing2
	CMPI.W	#1,D4		; BEATS * 14frames
	BNE.S	.doNothing2
	ADD.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing2:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#29,D5		; SONG POSITION
	BNE.S	.doNothing5

	CMPI.W	#100,D4
	BLO.S	.dontDisplace1
	CMPI.W	#108,D4
	BGE.S	.dontDisplace1
	BSR.W	__HW_DISPLACE
	.dontDisplace1:

	CMPI.W	#190,D4
	BLO.S	.dontDisplace3
	CMPI.W	#198,D4
	;BGE.S	.dontDisplace3
	BSR.W	__HW_DISPLACE
	.dontDisplace3:

	CMPI.W	#188,D4		; BEATS * 14frames
	BNE.S	.doNothing5
	ADD.L	#bpl*h*2,KONEYBG	; SCROLL 1SCREEN UP x2
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing5:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#32,D5		; SONG POSITION
	BNE.S	.doNothing3
	CMPI.W	#1,D4		; BEATS * 14frames
	BNE.S	.doNothing3
	ADD.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing3:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#37,D5		; SONG POSITION
	BNE.S	.doNothing4
	CMPI.W	#196,D4		; BEATS * 14frames
	BNE.S	.doNothing4
	ADD.L	#bpl*h*2,KONEYBG	; SCROLL 1SCREEN UP x2
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing4:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; TIMED EVENTS ON SELECTED FRAME ****
	; TRIG BG SCROLL
	MOVE.W	#0,BGISSHIFTING
	CMPI.W	#40,D5		; seqeunce block position TEST 41!!
	BNE.S	.dontScroll1	; then switch

	CLR	D5
	MOVE.W	BGSHIFTOFFSET,D5
	CMPI.W	#0,D5		; seqeunce block position
	BEQ.S	.dontScroll2	; then switch
	MOVE.W	#0,AUDIOCHANLEVEL0	; Stop FXs
	MOVE.W	#0,AUDIOCHANLEVEL3	; Stop FXs
	;MOVE.W	#4,P61_visuctr2	; BASS
	MOVE.W	#2,P61_visuctr1	; KICK
	MOVE.W	#1,BGISSHIFTING
	SUB.W	#bpl*h/20,BGSHIFTOFFSET
	ADD.L	#bpl*h/20,KONEYBG	; SCROLL 1PX UP
	BSR.W	__CREAPATCH	; FILL THE BUFFER

	LEA	Palette,A1
	ANDI.W	#1,D4
	BEQ.S	.even
	.odd:
	MOVE.W	#$000,2(A1)	; poke WHITE color now
	BRA.S	.done
	.even:
	MOVE.W	#$555,2(A1)	; poke WHITE color now
	.done:
	.dontScroll2:

	CMPI.W	#82,D4
	BLO.S	.dontResetBg
	LEA	Palette,A1	; RESET BG
	MOVE.W	#$000,2(A1)	; RESET BG
	.dontResetBg:

	CMPI.W	#114,D4
	BLO.S	.dontDisplace0
	CMPI.W	#122,D4
	BGE.S	.dontDisplace0
	BSR.W	__HW_DISPLACE
	.dontDisplace0:

	.dontScroll1:
	; TRIG BG SCROLL

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	; TIMED EVENTS ON SELECTED FRAME ****
	CMPI.W	#40,D5		; seqeunce block position TEST 41!!
	BNE.S	.dontScroll6	; then switch

	;CLR.W	$100		; DEBUG | w 0 100 2
	CMPI.W	#112,D4
	BLO.S	.dontScroll6
	CMPI.W	#121,D4
	BGE.S	.dontScroll6
	BSR.W	__HW_DISPLACE
	.dontScroll6:

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	;CMPI.W	#54,D5		; SONG POSITION
	;BNE.S	.doNothing9
	;CMPI.W	#114,D4
	;BLO.S	.doNothing9
	;CMPI.W	#121,D4
	;BGE.S	.doNothing9
	;BSR.W	__HW_DISPLACE
	;.doNothing9:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#55,D5		; SONG POSITION
	BNE.S	.doNothingA
	CMPI.W	#114,D4
	BLO.S	.doNothingA
	CMPI.W	#122,D4
	BGE.S	.doNothingA
	BSR.W	__HW_DISPLACE
	.doNothingA:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#60,D5		; SONG POSITION
	BNE.S	.doNothing8
	CMPI.W	#1,D4		; BEATS * 14frames
	BNE.S	.doNothing8
	ADD.L	#bpl*h,KONEYBG	; SCROLL 1SCREEN UP
	BSR.W	__CREAPATCH	; FILL THE BUFFER
	BSR.W	__CREATESCROLLSPACE; NOW WE USE THE BLITTER HERE!
	.doNothing8:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING
	CMPI.W	#81,D5		; SONG POSITION
	BNE.S	.doNothing7

	CMPI.W	#112,D4
	BLO.S	.dontDisplace8
	CMPI.W	#121,D4
	BGE.S	.dontDisplace8
	BSR.W	__HW_DISPLACE
	.dontDisplace8:

	CMPI.W	#130,D4		; BEATS * 14frames
	BNE.S	.doNothing7
	MOVEM.L	D0-A6,-(SP)
	JSR	P61_End
	MOVEM.L	(SP)+,D0-A6
	.doNothing7:
	; THIS PART IS REPEATED FOR EVERY POSITION WE WANT TO TRIG SOMETHING

SOUND_TRIGGERED_EVENTS:
	MOVE.W	AUDIOCHANLEVEL0,D2	; GROOVE 2
	CMPI.W	#0,D2		; BEWARE RND ROUTINE WILL RESET D1
	BEQ.S	_noglitch2
	MOVE.W	#0,AUDIOCHANLEVEL3	; Stop FXs
	MOVE.W	#0,GLITCHOFFSET	; #10240 for NEXT BTPL
	BSR.W	__BLIT_GLITCH_PLANE; THIS NEEDS OPTIMIZING
	_noglitch2:

	MOVE.W	AUDIOCHANLEVEL3,D2	; GROOVE 1
	CMPI.W	#0,D2		; BEWARE RND ROUTINE WILL RESET D1
	BEQ.S	_noglitch1
	MOVE.W	#0,GLITCHOFFSET
	BSR.W	__DITHERBGPLANE	; THIS NEEDS OPTIMIZING
	_noglitch1:

ALWAYS_TRIGGERED_EVENTS:
	MOVE.W	BGISSHIFTING,D5
	CMPI.W	#1,D5		; seqeunce block position
	BEQ.S	.dontPlotObjects	; then switch
	BSR.W	__PRINT2X
	MOVE.L	#bpls-1,KONEYLOGO_DPH; RESTORE BITPLANE
	BSR.W	__BLITINPLACE	; FIRST BLITTATA
	BSR.W	__SHIFTTEXT	; SHIFT DATI BUFFER?
	BSR.W	__POPULATETXTBUFFER; PUT SOMETHING
	.dontPlotObjects:

	;*--- main loop end ---*

ENDING_CODE:
	BTST	#6,$BFE001
	BNE.S	.DontShowRasterTime
	MOVE.W	#$FF0,$180(A6)	; show rastertime left down to $12c
	.DontShowRasterTime:
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	;;    ---  Call P61_End  ---
	MOVEM.L	D0-A6,-(SP)
	JSR	P61_End
	MOVEM.L	(SP)+,D0-A6
	RTS

;********** Demo Routines **********
PokePtrs:				; Generic, poke ptrs into copper list
	.bpll:	
	move.l	a0,d2
	swap	d2
	move.w	d2,(a1)		;high word of address
	move.w	a0,4(a1)		;low word of address
	addq.w	#8,a1		;skip two copper instructions
	add.l	d0,a0		;next ptr
	dbf	d1,.bpll
	rts

ClearScreen:			; a1=screen destination address to clear
	bsr	WaitBlitter
	clr.w	$66(a6)		; destination modulo
	move.l	#$01000000,$40(a6)	; set operation type in BLTCON0/1
	move.l	a1,$54(a6)	; destination address
	move.l	#h*bpls*64+bpl/2,$58(a6)	;blitter operation size
	rts

VBint:				; Blank template VERTB interrupt
	movem.l	d0/a6,-(sp)	; Save used registers
	lea	$dff000,a6
	btst	#5,$1f(a6)	; check if it's our vertb int.
	beq.s	.notvb
	;*--- do stuff here ---*
	moveq	#$20,d0		; poll irq bit
	move.w	d0,$9c(a6)
	move.w	d0,$9c(a6)
	.notvb:	
	movem.l	(sp)+,d0/a6	; restore
	rte

__InitCopperPalette:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	LEA.L	PALETTEBUFFERED,A2
	LEA.L	Palette,A3
	MOVE.L	#15,D0
	.FillLoop:
	MOVE.L	(A2)+,(A3)+
	DBRA	D0,.FillLoop
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

; THIS ROUTINE WILL POPULATE A GRAPHIC AREA WITH ANY DATA FROM MEMORY
; NEEDS 3 PARAMS: SOURCE, TARGET, DEPTH (PLANES)
__FILLGLITCHBG:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	GLITCHER_SRC,A3
	MOVE.L	GLITCHER_DEST,A4	; SOURCE DATA
	MOVE.L	GLITCHER_DPH,D1	; UGUALI PER TUTTI I BITPLANE
	.BITPLANESLOOP:
	CLR	D4
	MOVE.B	#h-1,D4		; QUANTE LINEE
	.OUTERLOOP:		; NUOVA RIGA
	CLR	D6
	MOVE.B	#bpl-1,D6		; RESET D6
	.INNERLOOP:
	MOVE.B	(A3)+,(A4)+
	DBRA	D6,.INNERLOOP
	DBRA	D4,.OUTERLOOP
	DBRA	D1,.BITPLANESLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

; FILLS A BUFFER WITH RANDOM DATA
__FILLRNDBG:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	GLITCHER_DEST,A4	; SOURCE DATA
	MOVE.L	GLITCHER_DPH,D1	; UGUALI PER TUTTI I BITPLANE
	.BITPLANESLOOP:
	CLR	D4
	MOVE.B	#h-1,D4		; QUANTE LINEE
	.OUTERLOOP:		; NUOVA RIGA
	CLR	D6
	MOVE.B	#bpl-1,D6		; RESET D6
	.INNERLOOP:
	BSR.S	_RandomWord
	MOVE.B	D5,(A4)+
	DBRA	D6,.INNERLOOP
	DBRA	D4,.OUTERLOOP
	DBRA	D1,.BITPLANESLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

_RandomWord:	bsr	_RandomByte
		rol.w	#8,d5
_RandomByte:	move.b	$dff007,d5	;$dff00a $dff00b for mouse pos
		move.b	$bfd800,d3
		eor.b	d3,d5
		rts

__SET_PT_VISUALS:
	; MOD VISUALIZERS *****
	ifne visuctrs
	MOVEM.L D0-A6,-(SP)

	; GROOVE 2
	lea	P61_visuctr0(PC),a0; which channel? 0-3
	moveq	#14,d0		; maxvalue
	sub.w	(a0),d0		; -#frames/irqs since instrument trigger
	bpl.s	.ok0		; below minvalue?
	moveq	#0,d0		; then set to minvalue
	.ok0:
	MOVE.W	D0,AUDIOCHANLEVEL0	; RESET
	_ok0:

	LEA	Palette,A1
	; BASS
	lea	P61_visuctr2(PC),a0; which channel? 0-3
	moveq	#15,d0		; maxvalue
	sub.w	(a0),d0		; -#frames/irqs since instrument trigger
	bpl.s	.ok2		; below minvalue?
	moveq	#0,d0		; then set to minvalue
	.ok2:
	MOVE.W	D0,AUDIOCHANLEVEL2	; RESET
	MOVE.L	D0,D5
	DIVU.W	#$3,D0		; start from a darker shade
	MOVE.L	D0,D3

	ANDI.W	#1,D5
	BEQ.S	.even
	.odd:
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	#1,D3		; makes color a bit geener
	ADD.L	D3,D0
	ROL.L	#$4,D3
	;ADD.L	#0,D3		; makes color a bit geener
	ADD.L	D3,D0		; expand bits to red
	BRA.S	.done
	.even:
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	#2,D3		; makes color a bit geener
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	#1,D3		; makes color a bit geener
	ADD.L	D3,D0		; expand bits to red
	.done:
	MOVE.W	D0,6(A1)		; poke WHITE color now
	_ok2:

	; KICKDRUM
	lea	P61_visuctr1(PC),a0; which channel? 0-3
	moveq	#15,d0		; maxvalue
	sub.w	(a0),d0		; -#frames/irqs since instrument trigger
	bpl.s	.ok1		; below minvalue?
	moveq	#0,d0		; then set to minvalue
	.ok1:
	MOVE.W	D0,AUDIOCHANLEVEL1	; RESET
	;ADD.W	AUDIOCHANLEVEL2,D0	; KICK BRIGHTER IF BASS PLAYS TOO?
	DIVU.W	#$2,D0		; start from a darker shade
	ADD.W	#$2,D0		; start from a darker shade
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	;ADD.L	#1,D3		; makes color a bit geener
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red
	MOVE.W	D0,14(A1)		; poke WHITE color now
	_ok1:

	; GROOVE 1
	lea	P61_visuctr3(PC),a0; which channel? 0-3
	moveq	#14,d0		; maxvalue
	sub.w	(a0),d0		; -#frames/irqs since instrument trigger
	bpl.s	.ok3		; below minvalue?
	moveq	#0,d0		; then set to minvalue
	.ok3:

	MOVE.W	D0,AUDIOCHANLEVEL3	; RESET
	_ok3:

	MOVEM.L (SP)+,D0-A6
	RTS
	endc
	; MOD VISUALIZERS *****

__PRINT2X:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	KONEYLOGO_DPH,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.W	DISPLACEINDEX,D7
	MOVE.L	KONEYBG,A4
	LEA	DISPLACETABLE,A3
	LEA	PATCH,A0
	.OUTERLOOP:
	LEA	KONEY2X,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#9,D6			
	ADD.W	#POS_TOP,A4	; POSITIONING
	.INNERLOOP:
	ADD.W	#POS_LEFT,A4	; POSITIONING
	MOVE.L	(A0)+,D2		; SALVO SFONDO
	MOVE.L	(A5)+,D3		
	MOVE.L	(A3,D7.W),D5	; FX 1
	ADD.W	#2,D7		; INCREMENTO INDICE TAB
	AND.W	#1024-1,D7	; AND TIRA FUORI SEMPRE FINO A X E POI WRAPPA
	ROL.L	D5,D3		; GLITCH

	EOR.W	D2,D3		; KOMBINO SFONDO+SKRITTA
	MOVE.L	D3,(A4)		
	ADD.W	#POS_MID,A4	; POSITIONING

	MOVE.L	(A0)+,D2		; SALVO SFONDO
	MOVE.L	(A5)+,D3		
	LSR.L	D5,D3		; GLITCH
	EOR.L	D2,D3		; KOMBINO SFONDO+SKRITTA
	MOVE.L	D3,(A4)		
	ADD.W	#POS_RIGHT,A4	; POSITIONING
	DBRA	D6,.INNERLOOP
	ADD.W	#POS_BOTTOM,A4	; POSITIONING
	DBRA	D1,.OUTERLOOP
	MOVE.W	D7,DISPLACEINDEX
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__CREAPATCH:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVEQ	#bpls-1,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.L	KONEYBG,A4
	LEA	PATCH,A5
	.OUTERLOOP:
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#9,D6
	ADD.W	#POS_TOP,A4	; POSITIONING
	.INNERLOOP:
	ADD.W	#POS_LEFT,A4	; POSITIONING
	MOVE.L	(A4),(A5)+	
	ADD.W	#POS_MID,A4	; POSITIONING
	MOVE.L	(A4),(A5)+	
	ADD.W	#POS_RIGHT,A4	; POSITIONING
	DBRA	D6,.INNERLOOP
	ADD.W	#POS_BOTTOM,A4	; POSITIONING
	DBRA	D1,.OUTERLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__CREATESCROLLSPACE:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	MOVEQ	#bpls-1,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.L	KONEYBG,A4
	.OUTERLOOP:
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#10*11-1,D6
	ADD.W	#POS_TOP+BAND_OFFSET,A4	; POSITIONING
	.INNERLOOP:
	MOVE.L	#0,(A4)+	
	DBRA	D6,.INNERLOOP
	ADD.W	#POS_BOTTOM-BAND_OFFSET-bpl,A4	; POSITIONING
	DBF	D1,.OUTERLOOP
	MOVEM.L	(SP)+,D0-D7/A0-A6	; FETCH FROM STACK
	RTS

__BLITINPLACE:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	KONEYBG,A4
	ADD.W	#30720,A4	; NEXT BITPLANE (?)
	ADD.W	#bltoffs+40,A4

	BTST.B	#6,DMACONR	; for compatibility
	bsr	WaitBlitter

	MOVE.L	A4,BLTDPTH
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#$09F0,BLTCON0	; BLTCON0 (usa A+D)
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 lo spiegheremo dopo
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo

	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo

	MOVE.L	#TXTSCROLLBUF,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)

	MOVE.W	#9*64+320/16,BLTSIZE	; BLTSIZE (via al blitter !)
				; adesso, blitteremo una figura di
				; 2 word X 6 linee con una sola
				; blittata coi moduli opportunamente
				; settati per lo schermo.
				; BLTSIZE = (Altezza in righe)
				; * 64 + (Larghezza in pixel)/16 
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__SHIFTTEXT:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	BTST.B	#6,DMACONR	; for compatibility
	bsr	WaitBlitter

	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#%0010100111110000,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
				; sorgente ha le righe consecutive
				; in memoria.

	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo
				; destinazione e` all'interno di un
				; bitplane largo 20 words, ovvero 40
				; bytes. Il rettangolo blittato
				; e` largo 2 words, cioe` 4 bytes.
				; Il valore del modulo e` dato dalla
				; differenza tra le larghezze

	MOVE.L	#_TXTSCROLLBUF-2,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	#_TXTSCROLLBUF-2,BLTDPTH

	MOVE.W	#9*64+320/16,BLTSIZE	; BLTSIZE (via al blitter !)
				; adesso, blitteremo una figura di
				; 2 word X 6 linee con una sola
				; blittata coi moduli opportunamente
				; settati per lo schermo.
				; BLTSIZE = (Altezza in righe)
				; * 64 + (Larghezza in pixel)/16 
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__POPULATETXTBUFFER:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	MOVE.W	FRAMESINDEX,D7
	CMP.W	#4,D7
	BNE.W	.SKIP
	LEA	TXTSCROLLBUF,A4
	LEA	FONT,A5
	LEA	TEXT,A6

	ADD.W	TEXTINDEX,A6
	CMP.L	#_TEXT,A6	; Siamo arrivati all'ultima word della TAB?
	BNE.S	.PROCEED
	MOVE.W	#0,TEXTINDEX	; Riparti a puntare dalla prima word
	.PROCEED:
	MOVE.B	(A6),D2		; Prossimo carattere in d2
	SUB.B	#$20,D2		; TOGLI 32 AL VALORE ASCII DEL CARATTERE, IN
	MULU.W	#8,D2		; MOLTIPLICA PER 8 IL NUMERO PRECEDENTE,
	ADD.W	D2,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#8-1,D6
	.LOOP:
	ADD.W	#38,A4		; POSITIONING
	MOVE.B	(A5)+,(A4)+
	;ADD.W	#1,A4		; POSITIONING
	;ADD.W	#38,A4		; POSITIONING
	MOVE.B	#%00000000,(A4)+
	MOVE.B	#%00000000,(A4)	; WRAPS MORE NICELY?
	;ADD.W	#2,A4		; POSITIONING
	DBRA	D6,.LOOP
	.SKIP:
	SUB.W	#1,D7
	CMP.W	#0,D7
	BEQ.W	.RESET
	MOVE.W	D7,FRAMESINDEX
	MOVEM.L	(SP)+,D0-D7/A0-A6	; FETCH FROM STACK
	RTS
	.RESET:
	ADD.W	#1,TEXTINDEX
	MOVE.W	#4,D7
	MOVE.W	D7,FRAMESINDEX	; OTTIMIZZABILE
	MOVEM.L	(SP)+,D0-D7/A0-A6	; FETCH FROM STACK
	RTS

__DITHERBGPLANE:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	KONEYBG,A3	; Indirizzo del bitplane destinazione in a3
	ADD.W	GLITCHOFFSET,A3	; NEXT BITPLANE (?)
	MOVE.W	AUDIOCHANLEVEL3,D7
	MOVE.W	DITHERFRAMEOFFSET,D2
	ADD.W	#2,D2		; INCREMENTO INDICE TAB
	AND.W	#4-1,D2		; AND TIRA FUORI SEMPRE FINO A X E POI WRAPPA
	MOVE.W	D2,DITHERFRAMEOFFSET
	CMPI.W	#0,D2
	BNE.S	.DontJumpLine
	NOT	D7
	ADD.W	#40*2,A3		; JUMP ONE LINE
	.DontJumpLine:

	MOVE.L	#63,D4		; QUANTE LINEE
	.OUTERLOOP:		; NUOVA RIGA
	move.b	$dff007,d5	; $dff00a $dff00b for mouse pos
	move.b	$bfd800,d1
	eor.b	d1,d5
	ror.L	#8,d5

	;MOVE.L	#%10101010101010101010101010101010,D5	; RESET

	MOVEQ.L	#9,D6		; RESET D6
	.INNERLOOP:		; LOOP KE CICLA LA BITMAP
	MOVE.L	D5,(A3)+
	rol.L	D7,d5
	MOVE.L	D5,(A3)+
	NOT	D7

	DBRA	D6,.INNERLOOP
	ADD.W	#40*2,A3		; JUMP ONE LINE
	DBRA	D4,.OUTERLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__BLIT_GLITCH_PLANE:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	MOVE.L	KONEYBG,A4
	ADD.W	GLITCHOFFSET,A4	; NEXT BITPLANE (?)
	MOVE.W	BLITPLANEOFFSET,D0
	MOVE.W	D0,D2
	ADD.W	#1,D2		; INCREMENTO INDICE TAB
	AND.W	#32-1,D2		; AND TIRA FUORI SEMPRE FINO A X E POI WRAPPA
	MOVE.W	D2,BLITPLANEOFFSET
	MULU.W	#640,D0
	MOVE.L	#GLITCHBUFFER,D1
	ADD.L	D0,D1
	BTST.B	#6,DMACONR	; for compatibility
	bsr	WaitBlitter

	MOVE.L	A4,BLTDPTH
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#$09F0,BLTCON0	; BLTCON0 (usa A+D)
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 lo spiegheremo dopo
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo
	MOVE.L	D1,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.W	#256*64+320/16,BLTSIZE	; BLTSIZE (via al blitter !)
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__HW_DISPLACE:
	MOVEM.L	D0-D7/A0-A6,-(SP)	; SAVE TO STACK
	CLR.L	D2
	MOVE.W	$DFF006,D4	; for bug?
	.waitVisibleRaster:
	MOVE.W	$DFF006,D4
	AND.W	#$FF00,D4	; read vertical beam
	CMP.W	#$3700,D4	; 2C
	BNE.S	.waitVisibleRaster

	.waitNextRaster:
	MOVE.W	$DFF006,D2
	AND.W	#$FF00,D2	; read vertical beam
	CMP.W	D4,D2
	BEQ.S	.waitNextRaster

	MOVE.W	D2,D4
	MOVE.B	$DFF007,D5	; $dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D1
	EOR.B	D1,D5
	MOVE.W	D5,BPLCON1	; 19DEA68E GLITCHA

	MOVE.W	$DFF004,D0	; Read vert most sig. bits
	BTST	#0,D0
	BEQ.S	.waitNextRaster

	CMP.W	#$0A00,D2	; DONT DISPLACE TXT
	BGE.S	.dontSkip		; DONT DISPLACE TXT
	MOVE.W	#0,BPLCON1	; RESET REGISTER
	
	.dontSkip:
	CMP.W	#$2F00,D2	; 12.032
	BNE.S	.waitNextRaster

	MOVE.W	#0,BPLCON1	; RESET REGISTER

	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

;********** Fastmem Data **********
DITHERFRAMEOFFSET:	DC.W 0
GLITCHER_SRC:	DC.L 0
GLITCHER_DEST:	DC.L 0
GLITCHER_DPH:	DC.L 0
KONEYLOGO_DPH:	DC.L bpls-1
ISLOGOFLASHING:	DC.L 0
AUDIOCHANLEVEL0:	DC.W 0
AUDIOCHANLEVEL1:	DC.W 0
AUDIOCHANLEVEL2:	DC.W 0
AUDIOCHANLEVEL3:	DC.W 0
P61_LAST_POS:	DC.W 0
P61_FRAMECOUNT:	DC.W 0

KONEYBG:		DC.L BG1		; INIT BG
DrawBuffer:	DC.L SCREEN2	; pointers to buffers to be swapped
ViewBuffer:	DC.L SCREEN1

DISPLACEINDEX:	DC.W 0
DISPLACETABLE:
	DC.W 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0
	DC.W 0,0,0,3,3,1,1,0,3,0,0,1,0,0,0,0
	DC.W 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,3
	DC.W 1,2,2,2,0,1,0,1,0,2,4,0,3,0,2,1
	DC.W 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1
	DC.W 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0
	DC.W 1,5,0,3,4,0,0,2,4,0,3,1,1,0,1
	DC.W 2,4,0,1,0,1,0,4,1,0,2,1,4,4,0,0

	DC.W 0,0,3,2,0,4,0,1,4,0,0,1,0,5,1
	DC.W 2,1,0,3,0,3,0,3,0,0,5,1,2,1,0,0
	DC.W 1,1,1,1,0,1,2,4,4,0,0,4,0,1,0,0
	DC.W 0,0,0,2,0,1,0,1,0,0,3,0,1,0,1
	DC.W 0,1,0,2,0,1,0,0,0,2,0,2,0,4,1,1
	DC.W 1,2,3,4,2,4,1,5,1,0,4,1,0,3,0,2
	DC.W 1,0,1,0,1,0,1,0,1,0,5,0,1,0,1,0
	DC.W 0,1,0,0,3,0,2,0,0,1,0,2,0,2,1,1

	DC.W 0,1,2,2,0,5,1,4,0,2,4,3,0,1,4,3
	DC.W 0,0,0,3,3,1,1,0,3,0,0,1,0,0,0,0
	DC.W 0,3,4,2,1,0,2,0,1,0,0,1,0,2,0,1
	DC.W 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1
	DC.W 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0
	DC.W 1,2,2,2,0,1,0,1,0,2,4,0,3,0,2,1
	DC.W 0,0,3,4,0,0,0,1,4,0,0,5,0,4,1
	DC.W 2,1,0,1,0,2,0,3,2,0,0,1,2,1,0,3

	DC.W 0,1,0,2,0,0,1,0,3,2,0,3,0,4,1,1
	DC.W 0,0,5,3,0,0,0,0,3,0,0,1,0,0,0,0
	DC.W 0,1,2,2,0,2,1,0,0,2,4,3,0,0,4,1
	DC.W 0,5,0,3,0,0,1,0,3,0,0,1,0,0,0,4
	DC.W 1,3,0,2,0,1,0,3,0,0,2,0,1,0,2,1
	DC.W 0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1
	DC.W 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0
	DC.W 0,2,0,0,3,0,0,2,0,1,0,0,0,2,1,0

	DC.W 0,3,4,2,1,0,2,4,1,0,0,1,0,2,0,1
	DC.W 1,2,0,3,4,0,0,2,4,0,3,4,1,0,1
	DC.W 0,1,0,1,0,1,0,1,0,1,0,1,0,2,0,1
	DC.W 1,0,1,3,1,0,1,0,1,0,1,0,1,0,1,0
	DC.W 0,0,0,2,0,1,0,1,0,0,3,0,1,0,1
	DC.W 1,3,0,2,0,1,2,3,0,2,4,0,4,0,2,0
	DC.W 0,0,3,0,0,0,0,3,5,0,0,1,0,4,1
	DC.W 1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0

	DC.W 1,2,2,1,3,0,0,2,0,2,1,0,0,2,4,0
	DC.W 2,1,0,1,0,2,0,3,0,0,0,1,2,1,0,1

PALETTEBUFFERED:
	DC.W $0180,$0000,$0182,$0000,$0184,$0111,$0186,$0122
	DC.W $0188,$0333,$018A,$0444,$018C,$0555,$018E,$0556
	DC.W $0190,$0666,$0192,$0888,$0194,$0999,$0196,$0AAA
	DC.W $0198,$09AA,$019A,$0FFF,$019C,$0FFF,$019E,$0FFF

GLITCHOFFSET:	DC.W 10240
BLITPLANEOFFSET:	DC.W 0
BGSHIFTCOUNTER0:	DC.W 1
BGSHIFTCOUNTER1:	DC.W 1
BGSHIFTOFFSET:	DC.W bwid*h
BGISSHIFTING:	DC.W 0
PATCH:		DS.B 10*64*bpls	;I need a buffer to save trap BG

TEXTINDEX:	DC.W 0
POS6_REACHED:	DC.B 0
POS16_REACHED:	DC.B 0

	;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
	;*******************************************************************************

KONEY2X:	INCBIN	"koney10x64.raw"

TXTSCROLLBUF:	DS.B (bpl)*9
_TXTSCROLLBUF:

FRAMESINDEX:	DC.W 4

BG1:	
	INCBIN	"onePlane_9.raw"		; POS 0
	INCBIN	"onePlane_8.raw"		; POS 5
	INCBIN	"onePlane_10_2.raw"	; POS 15
	INCBIN	"onePlane_5.raw"		; POS 25
	INCBIN	"onePlane_7.raw"		; POS 29
	INCBIN	"onePlane_11.raw"		; X2
	INCBIN	"onePlane_13.raw"		; POS 32
	INCBIN	"onePlane_6_2.raw"		; POS 37
	; *** FOR JUMPS AT POS 40	**** ??
	INCBIN	"onePlane_3.raw"		; X2
	INCBIN	"onePlane_17.raw"		; POS 40	SHIFT 1
	INCBIN	"onePlane_12.raw"		; POS 40	SHIFT 2
	INCBIN	"onePlane_18.raw"		; POS 40	SHIFT 3
	INCBIN	"BG_METAL_320256_4.raw"	; POS 40	SHIFT 4
					; POS 41
	INCBIN	"onePlane_20.raw"		; POS 60

FONT:	DC.L	0,0	; SPACE CHAR
	INCBIN	"scummfnt_8x752.raw",0
TEXT:
	;DC. "--------------------------------------"		; WIDTH OF 1 SCREEN OF TEXT
	DC.B "! WARNING EPILEPSY DANGER ALERT! RMB "
	DC.B "TO QUIT! WELCOME TO  -=FATAL_DEFRAG=- "
	DC.B "KONEY FIRST AMIGA HARDCORE RELEASE!!! "
	DC.B "BEST VIEWED ON A REAL AMIGA WITH CRT "
	DC.B "AND HUGE LOUDSPEAKERZ!! OK SCROLLTEXT "
	DC.B "STARTS HERE: I ALWAYS WANTED TO CODE "
	DC.B "THE AMIGA BUT I NEVER STARTED BECAUSE "
	DC.B "BACK IN THE DAYS I MOSTLY FOCUSED ON "
	DC.B "MAKING MUSIC. BUT IN 2019 IT HAS BEEN "
	DC.B "30 YEARS SINCE I GOT MY FIRST AMIGA "		; POS 10
	DC.B "(A500 1.2 IN 1989!) AND THESE DAYS I DO "
	DC.B "MY LIVING BY PHP CODING AND MUSIC... "
	DC.B "SIMPLY MY LACK OF AMIGA HARDWARE CODING "
	DC.B "KNOWLEDGE BECAME NO MORE ACCEPTABLE. "
	DC.B "ALSO, ON OCTAMED SOUNDSTUDIO I MADE A "
	DC.B "GOOD AMOUNT OF HARDCORE MODULES WHICH "
	DC.B "WERE RELEASED ON VINYL AND I ALWAYS "
	DC.B "HOPED TO USE THEM FOR SOME AMIGA INTROZ! "
	DC.B "SO I STARTED LEARNING ASM 68K AND BLITTER "
	DC.B "AND COPPER BY READING ONLINE BOOKS AND "	; POS 20
	DC.B "WATCHING YOUTUBE TUTORIALS AND SLOWLY "
	DC.B "BEGAN TO PUT TOGETHER THIS PIECE OF "
	DC.B "CODE HERE. OBVIOUSLY I KNOW IT'S NOT "
	DC.B "SUPER COOL AS DEMOSCENE RELEASES ARE "
	DC.B "THESE DAYS BUT HEY, I'TS MY FIRST INTRO "
	DC.B "EVER AND AMIGA ISN'T THE EASIEST HARDWARE "
	DC.B "FOR A N00B! I'M VERY WELL AWARE I'M JUST "
	DC.B "SCRATCHING THE SURFACE HERE BUT I PLAN "
	DC.B "TO CODE MORE INTROS AND DIG DEEPER INTO "
	DC.B "THE MAZE OF THE AMIGA CHIPSET... "		; POS 30
	DC.B "ABOUT THE MUSIC: IT'S AN INDUSTRIAL HARDCORE "
	DC.B "TRACK GRINDING YOUR BRAIN AT 210 BPM. "
	DC.B "IT WAS PUBLISHED BACK IN 1998 BY ZERO-MUSIC "
	DC.B "HARD, ONE OF THE MOST ICONIC INDUSTRIAL "
	DC.B "HARDCORE LABELS. WELL NOW I'D LIKE TO "
	DC.B "SEND OUT A MESSAGE TO ALL AMIGA CODERS "
	DC.B "IN VENICE AREA, ITALY: LET'S GATHER! "
	DC.B "LET'S MEET AND MAKE AMIGA SHIT TOGETHER!! "
	DC.B " -------------------- "
	DC.B " **  AKNOWLEDGEMENTS: I'D LIKE TO "		; POS 40
	DC.B "MENTION THIS PEOPLE FOR INDIRECTLY "
	DC.B "HELPING ME IN MY AMIGA CODE JOURNEY: "
	DC.B "RANDY/RAMJAM FOR HIS AMIGA ASSEMBLER "
	DC.B "COURSE ON AMINET - PHOTON/SCOOPEX "
	DC.B "FOR HIS AMIGA HARDWARE PROGRAMMING VIDEO "
	DC.B "TUTORIALS ON YOUTUBE - DA JORMAS FOR "
	DC.B "BEING OF BIG INSPIRATION TO ME FOR MORE "
	DC.B "THAN 20 YEARS IN BOTH AMIGA CODING AND "
	DC.B "MUSIC MAKING - OCTAMED SOUNDSTUDIO FOR "
	DC.B "BEING THE BEST MUSIC SOFTWARE EVER!! "		; POS 50
	DC.B "** GREETINGS: AT THIS POINT IT'S A COMMON "
	DC.B "PRACTICE TO SHOUT OUT SOME GREETINGZ "
	DC.B "SO HERE WE ARE! TO FABBROZ/RETROACADEMY "
	DC.B "FOR THE FANTASTIC RETROCOMPUTING EVENTS "
	DC.B "AND FOR BETA-TESTING THIS INTRO ON HIS "
	DC.B "BROKEN A1200 (EH EH!) - TO HEDGEHOG/ "
	DC.B "NAH-KOLOR FOR SHARING WITH ME HIS WISDOM "
	DC.B "DURING MY HARD TIMES WITH ASSEMBLY - "
	DC.B "THE GUYS AT EAB, CODERS.ASM SECTION WHO "	
	DC.B "HELPED A COMPLETE N00B LIKE ME GETTING "	; POS 60
	DC.B "SOMEWHERE BEYOND AN ''HELLO WORLD'' - "
	DC.B "KCMA&T-PLUS AND THE DESTROYER FOR BEING "
	DC.B "DEDICATED AND ACTIVE AMIGACORE ARTISTS - "
	DC.B " ** HOT LINKZ: WWW.KONEY.ORG - "
	DC.B "WWW.RETROACADEMY.IT - ASM SOURCE ON "
	DC.B "GITHUB.COM/KONEY/ ** CODE+GFX+MUSIC BY "
	DC.B "KONEY ** COMPILED IN VENICE, ITALY ON OCT "
	DC.B "2020 ** ALL GRAPHIC GLITCHES ARE ON PURPOSE "
	DC.B " - MUSIC STOPS ON PURPOSE AFTER ONE PLAYBACK"
	DC.B "           .EOF                       "	; POS 70
	DC.B "                                      "
	EVEN
_TEXT:

Copper:
	DC.W $1FC,0	;Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	;238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	;and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	;Standard bitplane dma fetch start
	DC.W $94,$D0	;and stop for standard screen.

	DC.W $106,$0C00	;(AGA compat. if any Dual Playf. mode)
	DC.W $108,0	;bwid-bpl	;modulos
	DC.W $10A,0	;bwid-bpl	;RISULTATO = 80 ?

	DC.W $102,0	;SCROLL REGISTER (AND PLAYFIELD PRI)

Palette:	;Some kind of palette (3 bpls=8 colors)
	DC.W $0180,0,$0182,0,$0184,0,$0186,0
	DC.W $0188,0,$018A,0,$018C,0,$018E,0
	DC.W $0190,0,$0192,0,$0194,0,$0196,0
	DC.W $0198,0,$019A,0,$019C,0,$019e,0

BplPtrs:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $E8,0
	DC.W $EA,0
	DC.W $EC,0
	DC.W $EE,0
	DC.W $F0,0
	DC.W $F2,0
	DC.W $F4,0
	DC.W $F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,BPLS*$1000+$200	;enable bitplanes

COPPERWAITS:
	; HW DISPLACEMENT
	;DC.W $002D,$FFFE
	;DC.W $F102,$A68E
	;DC.W $FE07,$FFFE
	;DC.W $F102,$1F83

	;DC.W $FE07,$FFFE
	;DC.W $0180,$0FFF
	;DC.W $FF07,$FFFE
	;DC.W $0180,$0011	; SCROLLAREA BG COLOR
	;DC.W $0182,$0AAA	; SCROLLING TEXT WHITE ON

	DC.W $FFDF,$FFFE	; allow VPOS>$ff

	;DC.W $0807,$FFFE
	;DC.W $0180,$0FFF
	;DC.W $0907,$FFFE
	;DC.W $0180,$0000
	;DC.W $0182,$0333	; SCROLLING TEXT WHITE OFF

	DC.W $FFFF,$FFFE	;magic value to end copperlist
_Copper:

Module1:	INCBIN	"FatalDefrag_v4.P61"	; code $9104

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B h*bwid	; two buffers
GLITCHBUFFER:	DS.B h*3*bpl	; some free space for glitch

	END