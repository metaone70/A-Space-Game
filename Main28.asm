; 10 SYS (2064)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

*=$0810

START
         SEI
         LDA #$31  ;WE USE THESE
         LDX #$EA  ;ROUTINES HERE TO
         STA $0314 ;TURN OFF THE IRQ
         STX $0315 ;SO THAT THE GAME
         LDA #$00  ;WONT KEEP CRASHING
         STA $D019 ;WHEN RESTARTING
         STA $D01A ;DURING AN IRQ
         LDA #$81  ;ROUTINE PLAYER.
         STA $DC0D ;
         STA $DD0D ;
         LDA #$00  ;NO SOUND
         STA $D418 ;
         LDA #$08  ;TURN OFF SCREEN
         STA $D016 ;MULTICOLOUR

         LDA #$00  ;BLACK COLOR FOR THE BORDER AND
         STA $D021 ;BACKGROUND
         STA $D020 ;
         JSR PLAYMUSIC   ; PLAY INTRO MUSIC

BEGINNING
         LDA #03         ; SET LIVES TO 3
         STA LIVES

         JSR TITLESCREEN         ;DRAW THE BITMAP TITLE SCREEN
         JSR DRAWTEXTSCREEN      ;DRAW THE ENTER THE ARENA SCREEN
         JSR DRAWSCREEN          ;DRAW THE CHARACTER (BATTLE) SCREEN
         JSR INITSPRITEPOS       ;INITIALIZE SPRITE POSITIONS WITH THE TABLE
         JSR SETUPSPRITES        ;INITIALIZE SPRITES
         JSR STOPMUSIC           ;STOP THE MUSIC, SINCE THE BATTLE IS ABOUT TO BEGIN

;---------------------MAIN GAME LOOP-----------------------------------------       
MAINLOOP  
          JSR WAITRASTER        ; WATT FOR RASTERLINE TO SLOW DOWN
          JSR EXPANDSPRITEPOS   ; ADJUST SPRITE POSITONS FOR X > 255
          JSR MAINSHIP          ; WAIT FOR JOYSTICK TO COMMAND AND MOVE MAIN SHIP
          JSR BULLETMOVE        ; MAINSHIP BULLET CHECK AND CONTROL
          JSR ENEMYSHIP         ; MOVE ENEMY SHIP ACCORDING TO THE SINE TABLE

          JSR ENEMYFIRE         ; ENEMYSHIP FIRE CHECK AND CONTROL
          JSR COLLISIONCHECK    ; CHECK IF ANY COLLISION OCCURS
          JMP MAINLOOP          ; CONTINUE THE LOOP

;-----------INITIAL POSITIONS OF SPRITES-------------------
INITSPRITEPOS

         LDX #$00               ; LOAD SPROTE INITIAL COORDINATES
POSSPRTS LDA POSTABLE+$00,X     ; FROM POSITION TABLE
         STA SPRITEPOS+$00,X
         INX
         CPX #$0A             ; 5 SPRITES -> X & Y POS -> 10 BYTES     
         BNE POSSPRTS
         RTS

;---------------------SETTING UP SPRITES----------------------------
SETUPSPRITES
        ; SET SPRITE MULTICOLORS
        LDA #$07        ;--> YELLOW
        STA $D025
        LDA #$02        ;--> RED
        STA $D026

        ; COLORIZE SPRITES
        LDA #$0C        ;-> SPRITE 0 (MAINSHIP) COLOR = GREY 2
        STA $D027
        LDA #$0F        ;-> SPRITE 1 (ENEMY) COLOR = LT.GREY      
        STA $D028
        LDA #$07        ;-> SPRITE 2 (BULLET) COLOR = YELLOW
        STA $D029
        LDA #$07        ;-> SPRITE 3 (ENEMY BULLET) COLOR = YELLOW 
        STA $D02A
        LDA #$07        ;-> SPRITE 4 (EXPLOSION) COLOR = YELLOW
        STA $D02B

        ; SET MULTICOLOR BITS
        LDA #%00010111        ; SPRITES 0,1,2,4 ARE MULTICOLOR
        STA $D01C

        ; SET SCREEN-SPRITE PRIORITY FLAGS
        LDA #$00
        STA $D01B

        ; SET SPRITE POINTERS --> $2000
        LDA #$80        ; MAIN SHIP
        STA $07F8
        LDA #$81        ; ENEMY SHIP
        STA $07F9
        LDA #$82        ; MAIN SHIP BULLET
        STA $07FA
        LDA #$83        ; ENEMY SHIP BULLET
        STA $07FB       
        LDA #$84        ; EXPLOSION
        STA $07FC        

        ; TURN ON SPRITES
        LDA #%00001111        ; TURN ON 4 SPRITES - EXPLOSION NOT INCLUDED
        STA $D015
        RTS

;-----------------EXPANDING SPRITE POSITIONS FOR X>255 -------------------
EXPANDSPRITEPOS  
 
         LDX #$00
XLOOP    LDA SPRITEPOS+$01,X     ; Y COORDINATES
         STA $D001,X
         
         LDA SPRITEPOS+$00,X
         ASL A                  ;X POSITION IS MORE
         ROR $D010              ;THAN 256 PIXELS
         STA $D000,X
         INX
         INX
         CPX #$10
         BNE XLOOP
         RTS

;-----------------DRAW ENTER THE ARENA SCREEN ROTUINE  ------------------------------------
DRAWTEXTSCREEN
                LDA #$00
                STA $FB
                STA $FD         ; $0400 SCREEN RAM
                STA $F7

                LDA #$48        ; $4800 CHARACTER DATA -->  TRANSFER TO $0400
                STA $FC

                LDA #$04
                STA $FE

                LDA #$00        ; $4C00 SCREEN COLOR DATA --> TRANSFER TO $D800
                STA $F9
                LDA #$4C
                STA $FA

                LDA #$D8        ; $D800 --> COLOR RAM
                STA $F8

                LDX #$00
LOOPTEXT           
                LDY #$00
INNERLOOPY
                LDA ($FB),Y
                STA ($FD),Y
                LDA ($F9),Y
                STA ($F7),Y
                INY
                BNE INNERLOOPY

                INC $FC
                INC $FE
                INC $FA
                INC $F8
                INX
                CPX #$04
                BNE LOOPTEXT

INPUTKEY        JSR $FFE4
                BEQ INPUTKEY
                STA KEY
                CMP #$20
                BNE INPUTKEY 
                RTS

;-----------------DRAW SCREEN ROUTINE  ------------------------------------
DRAWSCREEN
                LDA #$00
                STA $FB
                STA $FD         ; $0400 SCREEN RAM
                STA $F7

                LDA #$50        ; $5000 CHARACTER DATA -->  TRANSFER TO $0400
                STA $FC

                LDA #$04
                STA $FE

                LDA #$00        ; $5400 SCREEN COLOR DATA --> TRANSFER TO $D800
                STA $F9
                LDA #$54
                STA $FA

                LDA #$D8        ; $D800 --> COLOR RAM
                STA $F8

                LDX #$00
LOOP           
                LDY #$00
INNERLOOP
                LDA ($FB),Y
                STA ($FD),Y
                LDA ($F9),Y
                STA ($F7),Y
                INY
                BNE INNERLOOP

                INC $FC
                INC $FE
                INC $FA
                INC $F8
                INX
                CPX #$04
                BNE LOOP
                RTS

;---------------------TITLE SCREEN---------------------------------------
TITLESCREEN

      LDA #$3B
      STA $D011
      LDA #$18
      STA $D016
      LDA #$78
      STA $D018
      LDA #$C6
      STA $DD00

      LDA #$00
      STA $D020
      LDA $8710
      STA $D021

      LDX #$FA

LOADIMAGE
      LDA SCRDATA-1,X
      STA SCRRAM-1,X
      LDA SCRDATA+249,X
      STA SCRRAM+249,X
      LDA SCRDATA+499,X
      STA SCRRAM+499,X        
      LDA SCRDATA+749,X
      STA SCRRAM+749,X

      LDA COLDATA-1,X
      STA COLRAM-1,X  
      LDA COLDATA+249,X  
      STA COLRAM+249,X  
      LDA COLDATA+499,X  
      STA COLRAM+499,X  
      LDA COLDATA+749,X  
      STA COLRAM+749,X  
      DEX  
      BNE LOADIMAGE  

INPUTKE JSR $FFE4
        BEQ INPUTKE
        STA KEY
        CMP #$20
        BNE INPUTKE  
          
        LDA #$1B
        STA $D011
        LDA #$C8
        STA $D016                
        LDA #%00011010
        STA $D018
        LDA #$C7
        STA $DD00        

        LDA #$00
        STA $D020
        STA $D021
        RTS

;------------------PLAY TITLE SCREEN MUSIC--------------
PLAYMUSIC
        LDA #$00        ;SELECT IN-GAME TUNE
        STA SOUND
        JSR PLAYSOUND   ;GO AND PLAY THE SELECTED SOUND (HERE-MUSIC)
        RTS

;------------------STOP TITLE SCREEN MUSIC WHEN GAME STARTS --------------
STOPMUSIC
        LDA #$04        ;PLAY ONE OF THE SHORT SOUNDS TO STOP MUSIC
        STA SOUND
        JSR PLAYSOUND
        RTS

;-----------------SLOWING DOWN THE GAME-------------------------------------
WAITRASTER                  ; SLOWDOWN GAME
        LDA #30
        CMP $D012
        BNE WAITRASTER
        RTS

;-----------------MAIN SHIP MOVEMENT W/ JOYSTICK CHECK-----------------------
MAINSHIP

         LDA $DC00        ;READ JOYSTICK
UP       LSR A            ;CHECK UP
         BCS DOWN         ;NOT UP
         LDY SPRITEPOS+$01
         DEY              ;MOVE PLAYER
         DEY              ;UP, UNTIL IT
         CPY #$96         ;REACHES #$96
         BCS SETUP        ;THEN STOP
         LDY #$96         ;MOVING PLAYER
SETUP    STY SPRITEPOS+$01

DOWN     LSR A            ;CHECK DOWN
         BCS LEFT         ;NOT DOWN
         LDY SPRITEPOS+$01;
         INY              ;MOVE PLAYER
         INY              ;DOWN UNTIL IT
         CPY #$DE         ;REACHES #D0
         BCC SETDOWN      ;THEN STOP
         LDY #$DE        ;MOVING PLAYER
SETDOWN  STY SPRITEPOS+$01

LEFT     LSR A            ;READ LEFT
         BCS RIGHT        ;NOT LEFT
         LDY SPRITEPOS+$00
         DEY              ;AS WITH UP
         CPY #$0C         ;AND DOWN, BUT
         BCS SETLEFT      ;MOVING LEFT
         LDY #$0C         ;
SETLEFT  STY SPRITEPOS+$00

RIGHT    LSR A            ;READ RIGHT
         BCS FIRE         ;NOT RIGHT
         LDY SPRITEPOS+$00
         INY              ;AS UP AND
         CPY #$9E         ;DOWN BUT
         BCC SETRIGHT     ;MOVING RIGHT
         LDY #$9E
SETRIGHT STY SPRITEPOS+$00

FIRE     LSR A            ;READ FIRE
         BCS NOJOY        ;NOT FIRE
         LDA FIRELOCKUP   ;
         CMP #$01         ;CHECK
         BEQ NOJOY        ;FIRELOCKUP
                          
        ;IF NOT LOCKED
         LDA SPRITEPOS+$00;PLACE BULLET
         STA SPRITEPOS+$04;ON SHIP AND
         LDA SPRITEPOS+$01;FIRE BULLET
         STA SPRITEPOS+$05
         LDA #4
         STA SOUND
         JSR PLAYSOUND
         LDA #$01         ;LOCK FIRE
         STA FIRELOCKUP   ;UNTIL BULLET
                          ;FINISH ITS
                          ;FUNCTIONAL
                          ;PROCESS
NOJOY    RTS

;-----------------MAIN SHIP BULLET HANDLING-------------------------------          
BULLETMOVE 
         LDA NOMOVEBULL
         CMP #$01
         BEQ NOMOVEBL
        
         LDY SPRITEPOS+$05      ; MOVE THE BULLET
         DEY                    ; DECREMENT Y VAL 
         DEY
         CPY #$30       
         BCS STOPMSB
         LDY #$00
         STY SPRITEPOS+$04
         LDA #$00
         STA FIRELOCKUP
STOPMSB  STY SPRITEPOS+$05

NOMOVEBL RTS

;------------------ENEMY SHIP BULLET --------------------------------------
ENEMYFIRE

        LDA ENEMYBULLET         ; CHECK IF THERE IS ALREADY AN ENEMY BULLET
        CMP #$00
        BNE ENBULMOVE           ; IF BULLET EXISTS, GO TO MOVE SECTION

        ;LDA SPRITEPOS+$00
        ;CMP SPRITEPOS+$02
        ;BNE NOENMOVEBL

        LDY SPRITEPOS+$02       ; COPY ENEMY SHIP COORDINATES
        STY SPRITEPOS+$06       ; TO ENEMY BULLET
        LDY SPRITEPOS+$03     
        STY SPRITEPOS+$07  
        LDY #$01                ; SET ENEMYBULLET TO 1, SO THERE IS A BULLET
        STY ENEMYBULLET         ; ON SCREEN
        LDA #4
        STA SOUND               ; PLAY THE SHOOTING SOUND
        JSR PLAYSOUND
        RTS

ENBULMOVE
         LDY SPRITEPOS+$07      ; MOVE THE BULLET
         INY                    ; DECREMENT Y VAL 
         INY
         CPY #$E5               ; REACHED DOWN THE SCREEN?
         BCC STOPENMSB          ; IF
         LDY #$00               ; IF YES, MAKE THE COORDINATES ZERO 
         STY SPRITEPOS+$06      ; TO GET IT OUT OF SCREEN
         STY SPRITEPOS+$07 
         LDA #$00               ; SET ENEMYBULLET TO 0, SO THERE IS NO
         STA ENEMYBULLET        ; ENEMY BULLET ON SCREEN

STOPENMSB  
         STY SPRITEPOS+$07
         
NOENMOVEBL RTS

;-------------------------ENEMYSHIP MOVEMENT-------------------------------------
ENEMYSHIP 
          LDX BYTECOUNT 
          LDA SINEX,X      ;SINE MOVEMENT X POSITION
          STA SPRITEPOS+$02     
          LDA SINEY,X      ; SINE MOVEMENT Y POSITION
          STA SPRITEPOS+$03
          INC BYTECOUNT
          RTS

;----------------COLLISON CHECK SUBROUTINE------------------------------------
COLLISIONCHECK
          LDA $D01E
          CMP #%00000110        ; CHECK IF MAIN SHIP BULLET AND ENEMY SHIP COLLIDED
          BEQ COLLIDED
          CMP #%00001001        ; CHECK IF ENEMY SHIP BULLET AND MAIN SHIP COLLIDED
          BEQ COLLIDED2
          CMP #%00001100        ; CHECK IF BULLETS COLLIDED
          BEQ COLLIDED3
          RTS
;--------------COLLIDED 3 - BULLETS COLLIDE----------------
COLLIDED3
          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$06      
          STA $D01E
          STA NOMOVEBULL
          STA ENEMYBULLET
          LDA #4
          JSR PLAYSOUND
          RTS
;----------------MAIN SHIP BULLET AND ENEMY SHIP COLLIDED--------------------------
COLLIDED
   
          LDA #%00010001        ; JUST SPRITE 0 (MAINSHIP) AND 
          STA $D015             ; SPRITE 4 (COLLISION SPRITE) IS ON

          ;JSR EXPANDSPRITEPOS

          LDA SPRITEPOS+$02     ; TRANSFER CURRENT ENEMY SHIP POS TO COLLISION COORDINATES
          STA SPRITEPOS+$08
          LDA SPRITEPOS+$03
          STA SPRITEPOS+$09

          LDA #7
          STA SOUND
          JSR PLAYSOUND         ; PLAY COLLOSION SOUND
          JSR ENEMYHIT          ; WRITE "ENEMY HIT" TEXT

          JSR COLLANIM          ; COLLISION ANIMATION

          JSR ENEMYHITCLEAR     ; CLEAR THE BOTTOM TEXT
          
          ; RETURN BACK TO ORIGINAL POSITIONS EXCEPT MAIN SHIP
          LDA #%00001111
          STA $D015              ; FIRST 4 SPRITES ARE ON (AGAIN)        
          
          LDA #$00               ; RESET COLLISION REGISTER
          STA $D01E      
          STA NOMOVEBULL        ; RESET MAINSHIP BULLET REGISTER
          STA NOENMOVEBULL
          STA ENEMYBULLET

          ;JSR INITSPRITEPOS
          
          ;LDX #$00 
          ;STX BYTECOUNT
          ;LDA SINEX,X
          ;STA SPRITEPOS+$02     
          ;LDA SINEY,X
          ;STA SPRITEPOS+$03
          ;INC BYTECOUNT 

          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$05
          STA SPRITEPOS+$06
          STA SPRITEPOS+$07
          
          JSR WRITESCORE        ; JUMP TO WRITE SUBROUTINE
          JMP MAINLOOP          ; RETURN BACK TO THE MAINLOOP

;-------------------ENENMY SHIP BULLET AND MAINSHIP COLLIDED------------------------
COLLIDED2
          
          LDA #%00010010        ; JUST SPRITE 1 (ENEMY SHIP) AND
          STA $D015             ; SPRITE 4 (COLLISION SPRITE) IS ON

          JSR EXPANDSPRITEPOS

          LDA SPRITEPOS+$00     ; TRANSFER CURRENT MAIN SHIP POS TO COLLISIN COORDINATES
          STA SPRITEPOS+$08
          LDA SPRITEPOS+$01
          STA SPRITEPOS+$09

          LDA #7
          STA SOUND
          JSR PLAYSOUND
          JSR YOUAREHIT
          JSR COLLANIM          ; ANIMATE COLLISION
          JSR YOUAREHITCLEAR    ; 
          
          DEC LIVES             ; DECREMENT LIVES VALUE
          JSR UPDATELIVES       ; UPDATE LIVES ON SCREEN
          LDA LIVES             ; CHECK NO. OF LIVES
          BEQ GAMEOVER          ; IF ZERO, THEN JUMP TO END GAME ROUTINE

          LDA #%00001111        ; FIRST 4 SPRITES ARE ON (AGAIN)  
          STA $D015           
          
          LDA #$00              ; RESET COLLISION REGISTER
          STA $D01E      
          STA ENEMYBULLET       ; RESET ENEMY SHIP BULLET REGISTER
     
          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$05
          STA SPRITEPOS+$06
          STA SPRITEPOS+$07          

          JMP MAINLOOP          ; RETURN BACK TO THE MAINLOOP
          
;-------------------END GAME------------------------------
GAMEOVER
        LDX #0
GO      LDA ENDTEXT1,X
        STA SCREEN3,X
        INX
        CPX #9
        BNE GO

        LDX #0
GO2     LDA #1
        STA SCREEN3COL,X
        INX
        CPX #9
        BNE GO2

        LDX #0
GO3     LDA ENDTEXT2,X
        STA SCREEN4,X
        INX
        CPX #21
        BNE GO3

        LDX #0
GO4     LDA #1
        STA SCREEN4COL,X
        INX
        CPX #25
        BNE GO4

        LDA #%00000000        ; TURN OFF SPRITES  
        STA $D015 

        LDA #$00
        STA SOUND
        JSR PLAYSOUND

INPUT   
        JSR COLORANIM1
        JSR COLORANIM2
        JSR $FFE4
        BEQ INPUT
        STA KEY
        CMP #$20
        BNE INPUT       

        JMP BEGINNING
 
;----------- MAIN SHIP HIT MESSAGE-------------------------
YOUAREHIT
        LDX #00
MSH     LDA MAINSHIPHIT,X
        STA SCREEN5,X
        INX
        CPX #32
        BNE MSH          

        LDX #0
MSHC    LDA #1
        STA SCREEN5COL,X
        INX
        CPX #32
        BNE MSHC
        RTS

YOUAREHITCLEAR
        LDX #00
MSH2    LDA #32
        STA SCREEN5,X
        INX
        CPX #36
        BNE MSH2          
        RTS
 ;-----------ENEMY HIT MESSAGE---------------------------------------------
ENEMYHIT
        LDX #00
ESH     LDA ENEMYSHIPHIT,X
        STA SCREEN6,X
        INX
        CPX #27
        BNE ESH          

        LDX #0
ESHC    LDA #1
        STA SCREEN6COL,X
        INX
        CPX #27
        BNE ESHC
        RTS

ENEMYHITCLEAR
        LDX #00
ESH2    LDA #32
        STA SCREEN6,X
        INX
        CPX #36
        BNE ESH2          
        RTS            
;----------------------COLLISION ANIMATIN-------------------------------
COLLANIM

          LDY #00
COLLOOP   LDA #$84        ; SPRITE 4'S POINTER
          STA $07FC       ;
          JSR DRAWCOL
          
          LDA #$85        ; SPRITE 4'S POINTER
          STA $07FC       ;
          JSR DRAWCOL

          LDA #$86        ; SPRITE 4'S POINTER
          STA $07FC       ;
          JSR DRAWCOL

          LDA #$85        ; SPRITE 4'S POINTER
          STA $07FC       ;
          JSR DRAWCOL
          
          INY
          CPY #50
          BNE COLLOOP
          LDA #$00
          STA $D020
          RTS
          
DRAWCOL   LDA SPRITEPOS+$08
          LDA SPRITEPOS+$09
          JSR EXPANDSPRITEPOS
          INC $D020
          JSR DELAY
          RTS
                     
DELAY
WAIT1    LDA #$10
         CMP $D012
         BNE WAIT1
         RTS

;-------------COLOR ANIMATION FOR GAME OVER------------------------
COLORANIM1

            LDX #$00
@LUP1       LDA SCREEN3COL,X
            TAY
            INY
            TYA
            STA SCREEN3COL,X
            INX
            CPX #09
            BNE @LUP1        
            RTS

;-------------COLOR ANIMATION FOR HIT SPACE------------------------
COLORANIM2

            LDX #$00
@LUP2       LDA SCREEN4COL,X
            TAY
            INY
            TYA
            STA SCREEN4COL,X
            INX
            CPX #21
            BNE @LUP2        
            RTS

;---------------------SCORE HANDLING---------------------------------
WRITESCORE      
          SED
          CLC
          LDA SCORE
          ADC #$00
          STA SCORE
          LDA SCORE+1
          ADC #$01
          STA SCORE+1
          LDA SCORE+2
          ADC #0
          STA SCORE+2
          CLD
         
          LDY #5        ; SCREEN OFFSET
          LDX #0        ; SCORE BYTE INDEX
SLOOP   
          LDA SCORE,X
          PHA
          AND #$0F
          JSR PLOTDIGIT 
          PLA
          LSR A
          LSR A
          LSR A 
          LSR A
          JSR PLOTDIGIT
          INX
          CPX #3
          BNE SLOOP
          RTS

PLOTDIGIT
          CLC
          ADC #48       
          STA SCREEN,Y 
          DEY
          RTS
       
;-------------LIVES CHECK-------------------------------------
UPDATELIVES
        LDA LIVES  
        CLC
        ADC #48
        STA SCREEN2
        RTS

;--------------------------PLAY SOUND -------------------
PLAYSOUND

    SEI
    LDA #<IRQ
    LDX #>IRQ
    STA $314
    STX $315
    LDX #$00
    LDY #$7F 
    STX $D012
    STY $DC0D
    LDA #$01
    STA $D01A
    STA $D019
    LDA SOUND
    JSR $3000
    CLI
    RTS
 
IRQ
    LDA #$01
    ASL $D019 
    JSR $3003
    JMP $EA31

;-------------------------------VARIABLES AND TABLES----------------------------------
SCREEN  = $0420
SCREEN2 = $0416
SCREEN3 = $05EF  
SCREEN4 = $0639
SCREEN3COL = $D9EF
SCREEN4COL = $DA39
SCREEN5 = $07C3
SCREEN6 = $07C5
SCREEN5COL = $DBC3
SCREEN6COL = $DBC5

SCRDATA = 32576
COLDATA = 33576          
SCRRAM = 23552
COLRAM = 55296

*=$2800
incbin "NEWFONT.BIN"
   
*=$3000
incbin "MUSIC.BIN",2

*=$6000
incbin "PIXEL_SPACE61.PRG",2
   
SPRITEPOS  = $0370    ;POSITIONS FOR THE SPRITES
COLLISION  = $03F0    ;COLLISION DECTION
FIRELOCKUP = $0330    ;DEBUGGED FIREMODE
GAMEON     = $0331    ;TO CHECK IF GAME IS ON, OR NOT
LEVELCT    = $0332    ;OUR LEVEL COUNTER
MISSLEFT   = $0333    ;MISSILE COUNTER
POINTER    = $0340    ;POINTER FOR MULTI PURPOSE ANIMATION ITH SPRITES
NOMOVEBULL      BYTE 0
NOENMOVEBULL     BYTE 0
DESTROY    = $0350    ;POINTER TO CHECK EXPLOSION ANIM
PLRDESTROY = $0351
GAMEOVR    = $0352


BYTECOUNT       BYTE $00        ; ENEMY SHIP POS COUNTER FOR THE SINE TABLE
SCORE           BYTE 0, 0, 0    ; SCORE VARIABLE FOR 6 DIGITS
BULLET          BYTE $00
ENEMYBULLET     BYTE $00        ; ENEMY BULLET VARIABLE 0=NO BULLET ON SCREEN
LIVES           BYTE $00        ; NUMBER OF LIVES
KEY             BYTE 0          ; GET CHAR. OF KEY PRESSED
KEYTITLE        BYTE 0
SOUND           BYTE 00         ; SOUND SELECTION BYTE
;MUSIC_IN_GAME_TUNE               = $00
;MUSIC_TITLE_TUNE                 = $01
;MUSIC_GET_READY_GAME_OVER_TUNE   = $02
;MUSIC_GAME_END_TUNE              = $03
;MUSIC_PLAYER_SHOOT               = $04
;MUSIC_PLAYER_DIE                 = $05
;MUSIC_PICKUP                     = $06
;MUSIC_ENEMY_DIE                  = $07          

;--------------------TEXT ON SCREENS-------------------------------
ENDTEXT1        BYTE 7,1,13,5,32,15,22,5,18
ENDTEXT2        BYTE 8,9,20,32,19,16,1,3,5,32
                BYTE 20,15,32,3,15,14,20,9,14,21,5

MAINSHIPHIT     BYTE 25,15,21,32,1,18,5,32,4,15,23,14,32,45,32
                BYTE 2,5,32,3,1,18,5,6,21,12,32,16,12,5,1,19,5

ENEMYSHIPHIT    BYTE 5,14,5,13,25,32,19,8,9,16,32,4,15,23,14,32,45,32 
                BYTE 7,15,15,4,32,23,15,18,11
;-----------------------------------------------------------------

; SPRITE INITIAL POSITIONS
POSTABLE BYTE $56,$D0,$55,$3C,$00,$00,$00,$00,$00,$00

;ENEMYSHIP MOVEMENT COORDINATES (SINE TABLES)
SINEX          

        BYTE 85,83,81,79,77,76,74,72,70,68,67,65,63,62,60,58
        BYTE 56,55,53,52,50,48,47,45,44,42,41,39,38,37,35,34
        BYTE 33,31,30,29,28,27,26,25,24,23,22,21,20,19,18,18
        BYTE 17,16,16,15,15,14,14,13,13,13,12,12,12,12,12,12
        BYTE 12,12,12,12,12,12,12,13,13,13,14,14,15,15,16,17
        BYTE 17,18,19,20,20,21,22,23,24,25,26,27,28,30,31,32
        BYTE 33,35,36,37,39,40,42,43,45,46,48,49,51,52,54,56
        BYTE 57,59,61,62,64,66,68,69,71,73,75,76,78,80,82,84
        BYTE 85,87,89,91,93,94,96,98,100,101,103,105,107,108,110,112
        BYTE 113,115,117,118,120,121,123,124,126,127,129,130,132,133,134,136
        BYTE 137,138,139,141,142,143,144,145,146,147,148,149,149,150,151,152
        BYTE 152,153,154,154,155,155,156,156,156,157,157,157,157,157,157,157
        BYTE 157,157,157,157,157,157,156,156,156,155,155,154,154,153,153,152
        BYTE 151,151,150,149,148,147,146,145,144,143,142,141,140,139,138,136
        BYTE 135,134,132,131,130,128,127,125,124,122,121,119,117,116,114,113
        BYTE 111,109,107,106,104,102,101,99,97,95,93,92,90,88,86,85

SINEY
        BYTE 60,60,60,60,60,60,60,60,60,60,61,61,61,61,62,62
        BYTE 62,63,63,63,64,64,65,65,65,66,66,67,68,68,69,69
        BYTE 70,70,71,72,72,73,74,74,75,76,77,77,78,79,80,80
        BYTE 81,82,83,84,85,85,86,87,88,89,90,90,91,92,93,94
        BYTE 95,96,96,97,98,99,100,101,102,102,103,104,105,106,107,107
        BYTE 108,109,110,110,111,112,113,113,114,115,116,116,117,118,118,119
        BYTE 119,120,121,121,122,122,123,123,124,124,125,125,126,126,126,127
        BYTE 127,127,128,128,128,128,129,129,129,129,129,129,129,129,129,129
        BYTE 129,129,129,129,129,129,129,129,129,129,128,128,128,128,127,127
        BYTE 127,126,126,126,125,125,124,124,123,123,122,122,121,121,120,119
        BYTE 119,118,118,117,116,116,115,114,113,113,112,111,110,110,109,108
        BYTE 107,107,106,105,104,103,102,102,101,100,99,98,97,96,96,95
        BYTE 94,93,92,91,90,90,89,88,87,86,85,85,84,83,82,81
        BYTE 80,80,79,78,77,77,76,75,74,74,73,72,72,71,70,70
        BYTE 69,69,68,68,67,66,66,65,65,65,64,64,63,63,63,62
        BYTE 62,62,61,61,61,61,60,60,60,60,60,60,60,60,60,60

*=$2000
; SPRITE #0 --> MAINSHIP
        BYTE $00,$00,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$54
        BYTE $00,$00,$54,$00,$0C,$54,$C0,$0C,$54,$C0,$04,$64,$40,$C4,$64,$4C,$C4,$74,$4C,$45
        BYTE $75,$44,$45,$FD,$44,$5A,$DE,$94,$59,$55,$94,$59,$DD,$94,$00,$DC,$00,$0F,$DF,$C0
        BYTE $0F,$13,$C0
        BYTE 0

; SPRITE #1 --> ENEMY SHIP

        BYTE $00,$3C,$00,$00,$3C,$00,$03,$FF,$C0,$0F,$FF,$F0,$3F,$D7,$FC,$FF,$D7,$FF,$FF,$D7
        BYTE $FF,$FF,$D7,$FF,$3F,$D7,$FC,$3F,$D7,$FC,$0F,$D7,$F0,$0F,$EB,$F0,$0B,$EB,$E0,$0B
        BYTE $EB,$E0,$08,$EB,$20,$08,$EB,$20,$08,$EB,$20,$08,$EB,$20,$00,$28,$00,$00,$28,$00
        BYTE $00,$28,$00
        BYTE 0

; SPRITE #2 --> MAIN SHIP BULLET 
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$82,$00,$00,$82
        BYTE $00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00
        BYTE $C3,$00,$00,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #3 --> ENEMY SHIP BULLET
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00
        BYTE $66,$00,$00,$66,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #4 --> EXPLOSION 1
        BYTE $00,$00,$80,$02,$08,$0C,$00,$00,$3C,$F0,$28,$00,$30,$00,$F2,$00,$20,$00,$0C,$00
        BYTE $C0,$0F,$14,$02,$03,$55,$02,$A0,$5D,$28,$29,$65,$80,$0A,$79,$40,$01,$57,$40,$08
        BYTE $75,$68,$A8,$55,$08,$80,$14,$C2,$03,$00,$C0,$33,$28,$3C,$00,$00,$00,$32,$08,$0C
        BYTE $00,$08,$00
        BYTE 0

; SPRITE #4 --> EXPLOSION 2
        BYTE $00,$00,$00,$80,$08,$02,$00,$00,$00,$02,$00,$00,$00,$20,$00,$00,$20,$C0,$00,$00
        BYTE $C0,$0F,$00,$00,$03,$14,$08,$00,$65,$28,$08,$96,$00,$08,$55,$00,$00,$65,$00,$08
        BYTE $54,$28,$28,$14,$00,$00,$00,$C0,$03,$00,$F0,$03,$28,$30,$00,$08,$02,$20,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #4 --> EXPLOSION 3
        BYTE $00,$00,$00,$00,$00,$0C,$00,$00,$00,$20,$00,$00,$00,$00,$00,$00,$20,$00,$03,$00
        BYTE $C0,$00,$00,$00,$0C,$00,$00,$00,$14,$20,$00,$24,$00,$08,$14,$00,$00,$1C,$00,$00
        BYTE $00,$20,$08,$00,$00,$00,$00,$C0,$03,$00,$C0,$00,$28,$00,$00,$00,$00,$30,$00,$00
        BYTE $00,$00,$02
        BYTE 0

; SCREEN 1 -  SCREEN DATA
*=$5000 
        BYTE    $20,$01,$2D,$13,$10,$01,$03,$05,$2D,$07,$01,$0D,$05,$20,$20,$20,$0C,$09,$16,$05,$13,$3A,$33,$20,$20,$20,$13,$03,$0F,$12,$05,$3A,$30,$30,$30,$30,$30,$30,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20
        BYTE    $20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$57,$20,$20,$56,$20
        BYTE    $20,$20,$56,$20,$5A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20
        BYTE    $20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$57,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$58,$56,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$56,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$57,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$5A,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20
        BYTE    $20,$20,$56,$20,$20,$20,$5A,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$5A,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$5A,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56
        BYTE    $20,$5A,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; Screen 1 -  Colour data
*=$5400
        BYTE    $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$00,$00,$0F,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$01,$01
        BYTE    $00,$00,$0F,$00,$00,$00,$0E,$00,$00,$00,$01,$00,$0E,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$07,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$0E,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$00,$0F,$01
        BYTE    $00,$00,$0E,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$00,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0E,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$0F,$01,$01,$07,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$01,$01,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$0F,$01
        BYTE    $00,$0F,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0E,$0F,$0F,$0E,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$01,$0F,$01,$01,$01,$05,$05,$05,$05,$0F,$01
        BYTE    $00,$01,$01,$00,$00,$0F,$00,$00,$0E,$00,$00,$0F,$00,$0E,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$01,$01,$07,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$07,$01,$01,$01,$01,$05,$01,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$0E,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0E,$00,$00,$00,$00,$0E,$00,$00,$01,$01,$01,$01,$05,$05,$01,$05,$05,$0F,$01
        BYTE    $0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$01,$01,$07,$05,$05,$05,$05,$0E,$05,$01
        BYTE    $00,$00,$0E,$00,$00,$00,$01,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$0E,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$01,$00,$00,$00,$01,$00,$01,$01,$01,$0D,$0D,$0F,$0D,$0D,$0F,$01
        BYTE    $00,$00,$00,$0F,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$0D,$0D,$0D,$0D,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$01,$02,$02,$01,$0F
        BYTE    $00,$0E,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$0E,$01,$01,$01
        BYTE    $00,$00,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$00,$0F,$0F,$0F,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$0F,$0F,$0F,$09,$0F,$0F,$0F,$0F,$0F,$00,$01


; SCREEN 2 -  SCREEN DATA
*=$4800
        BYTE    $20,$E9,$E0,$DF,$48,$E9,$E0,$DF,$20,$E9,$E0,$DF,$20,$E9,$E0,$DF,$20,$E9,$E0,$DF,$20,$E9,$E0,$DF,$48,$E9,$E0,$DF,$20,$E9,$E0,$DF,$20,$E9,$E0,$DF,$20,$E9,$E0,$DF
        BYTE    $20,$E0,$20,$E0,$48,$E0,$20,$20,$20,$E0,$56,$E0,$56,$E0,$20,$E0,$56,$E0,$20,$20,$20,$E0,$20,$56,$48,$E0,$20,$20,$20,$E0,$20,$E0,$20,$E0,$5D,$E0,$20,$E0,$20,$20
        BYTE    $20,$E0,$20,$E0,$48,$E0,$20,$20,$20,$E0,$20,$E0,$20,$E0,$20,$E0,$20,$E0,$20,$20,$20,$E0,$20,$20,$48,$E0,$20,$20,$20,$E0,$20,$E0,$20,$E0,$5D,$E0,$20,$E0,$20,$20
        BYTE    $20,$E0,$E0,$E0,$48,$5F,$E0,$DF,$56,$E0,$E0,$69,$20,$E0,$E0,$E0,$20,$E0,$20,$20,$20,$E0,$E0,$DF,$48,$E0,$E0,$E0,$56,$E0,$E0,$E0,$20,$E0,$5D,$E0,$20,$E0,$E0,$DF
        BYTE    $20,$E0,$20,$E0,$48,$20,$20,$E0,$20,$E0,$20,$20,$20,$E0,$20,$E0,$20,$E0,$20,$20,$20,$E0,$20,$20,$48,$E0,$20,$E0,$20,$E0,$20,$E0,$56,$E0,$5D,$E0,$20,$E0,$20,$20
        BYTE    $20,$E0,$56,$E0,$48,$20,$20,$E0,$20,$E0,$20,$20,$20,$E0,$20,$E0,$20,$E0,$20,$20,$20,$E0,$20,$20,$48,$E0,$20,$E0,$20,$E0,$58,$E0,$20,$E0,$20,$E0,$20,$E0,$20,$20
        BYTE    $20,$69,$20,$5F,$48,$E9,$E0,$69,$20,$69,$20,$20,$20,$69,$56,$5F,$20,$5F,$E0,$69,$20,$5F,$E0,$69,$48,$5F,$E0,$69,$56,$69,$20,$5F,$20,$69,$56,$5F,$20,$5F,$E0,$69
        BYTE    $20,$20,$56,$20,$5A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$17,$05,$0C,$03,$0F,$0D,$05,$20,$14,$0F,$20,$22,$01,$20,$13,$10,$01,$03,$05,$20,$07,$01,$0D,$05,$22,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20
        BYTE    $20,$20,$20,$56,$19,$0F,$15,$20,$17,$09,$0C,$0C,$20,$06,$01,$03,$05,$20,$01,$20,$04,$05,$01,$04,$0C,$19,$20,$05,$0E,$05,$0D,$19,$20,$13,$08,$09,$10,$20,$20,$20
        BYTE    $20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$57,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$09,$0E,$20,$14,$08,$09,$13,$20,$07,$01,$0D,$05,$20,$21,$21,$21,$20,$13,$0F,$20,$02,$05,$20,$10,$12,$05,$10,$01,$12,$05,$04,$2C,$20,$20,$20,$20
        BYTE    $20,$58,$56,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$56,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20
        BYTE    $20,$20,$20,$20,$02,$05,$20,$02,$12,$01,$16,$05,$2E,$2E,$2E,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$57,$20,$20,$20,$20,$56,$20,$20,$20,$20,$58,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56
        BYTE    $20,$20,$20,$20,$10,$0C,$05,$01,$13,$05,$20,$15,$13,$05,$20,$0A,$0F,$19,$13,$14,$09,$03,$0B,$20,$32,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20
        BYTE    $20,$20,$56,$20,$14,$0F,$20,$03,$0F,$0E,$14,$12,$0F,$0C,$20,$19,$0F,$15,$12,$20,$13,$08,$09,$10,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$08,$09,$14,$20,$13,$10,$01,$03,$05,$20,$02,$01,$12,$20,$14,$0F,$20,$05,$0E,$14,$05,$12,$20,$14,$08,$05,$20,$01,$12,$05,$0E,$01,$20,$20,$20,$20
        BYTE    $20,$5A,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$58,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; SCREEN 2 -  COLOUR DATA
*=$4C00 
        BYTE    $00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$07,$0B,$0B,$0B,$00,$0B,$0B,$0B,$06,$0B,$0B,$0B,$07,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$01,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$07,$0B,$0B,$0B
        BYTE    $00,$0C,$07,$0C,$0C,$0C,$07,$07,$00,$0C,$01,$0C,$0E,$0C,$00,$0C,$07,$0C,$00,$00,$07,$0C,$00,$0F,$0C,$0C,$00,$0E,$00,$0C,$0C,$0C,$01,$0C,$0C,$0C,$0E,$0C,$01,$01
        BYTE    $00,$0F,$00,$0F,$0F,$0F,$01,$01,$01,$0F,$01,$0F,$01,$0F,$01,$0F,$01,$0F,$01,$01,$07,$0F,$00,$00,$0F,$0F,$00,$00,$00,$0F,$01,$0F,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$01,$01,$01,$01,$01,$01,$01,$0F,$01,$01,$01,$01,$01,$01,$01,$00,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$01,$01,$01,$01,$01,$01,$01,$07,$01,$01,$01
        BYTE    $00,$01,$07,$01,$01,$07,$07,$01,$00,$01,$00,$00,$00,$01,$00,$01,$01,$01,$00,$00,$07,$01,$00,$00,$01,$01,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$0F,$01,$0F,$01
        BYTE    $00,$07,$01,$07,$07,$01,$01,$07,$01,$07,$07,$01,$01,$07,$01,$07,$01,$07,$01,$01,$07,$07,$00,$00,$07,$07,$00,$07,$00,$07,$0E,$07,$01,$07,$07,$07,$0F,$07,$0F,$01
        BYTE    $00,$07,$00,$07,$07,$07,$07,$07,$00,$07,$00,$00,$00,$07,$01,$07,$00,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$0F,$07,$01,$07,$01,$07,$0F,$07,$0F,$07,$07,$07
        BYTE    $00,$00,$0E,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$01,$00,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$09,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$01,$01
        BYTE    $00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$0F,$01
        BYTE    $00,$0F,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0F,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$01,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$0F,$01
        BYTE    $00,$01,$01,$00,$00,$0F,$00,$00,$0E,$00,$00,$0F,$00,$0E,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$07,$07,$07,$07,$07,$07,$0D,$07,$07,$07,$0D,$07,$07,$07,$07,$07,$07,$07,$07,$0D,$07,$00,$0E,$00,$00,$01,$01,$0F,$01,$05,$05,$01,$05,$05,$0F,$01
        BYTE    $0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$01,$01,$01,$01,$05,$05,$05,$01,$05,$01
        BYTE    $00,$00,$0E,$00,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$0F,$07,$07,$07,$07,$0F,$07,$07,$07,$07,$07,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$02,$02,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$01,$00,$00,$00,$01,$00,$01,$01,$0F,$0D,$0D,$0D,$0D,$0D,$0F,$01
        BYTE    $00,$00,$00,$0F,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$01,$0D,$0D,$0D,$01
        BYTE    $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$01,$01
        BYTE    $00,$0E,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$0E,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$00,$0F,$0F,$0F,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$0F,$0F,$0F,$09,$0F,$0F,$0F,$0F,$0F,$00,$01











