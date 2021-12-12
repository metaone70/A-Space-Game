; 10 SYS (2080):REM (c) 2021 mtsv

*=$0801

        BYTE    $1E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $3a, $8f, $20, $28, $43, $29, $20, $32, $30, $32, $31, $20, $4D, $54, $53, $56, $00, $00, $00

*=$0820

         SEI
         LDA #$00                ;BLACK COLOR FOR THE BORDER AND
         STA $D021               ;BACKGROUND
         STA $D020 
         JSR PLAYMUSIC           ; PLAY INTRO MUSIC

BEGINNING
         LDA #03                 ; SET LIVES TO 3
         STA LIVES
         LDA #$00                ; RESET GAME VARIABLES
         STA BYTECOUNT
         STA FIRELOCKUP
         STA NOMOVEBULL
         STA ENEMYBULLET
         STA ENEMYBULLET2       
         STA ENEMY2DIR
         STA KEY

         JSR TITLESCREEN         ;DRAW THE BITMAP TITLE SCREEN
         JSR DRAWTEXTSCREEN      ;DRAW THE ENTER THE ARENA SCREEN
         JSR SCROLLBOTTOMTEXT    ; WRITE THE SCROLLING TEXT AT THE BOTTOM OF SCREEN
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
          JSR ENEMYSHIP2
          JSR ENEMYFIRE2
          JSR COLLISIONCHECK    ; CHECK IF ANY COLLISION OCCURS
          JMP MAINLOOP          ; CONTINUE THE LOOP

;-----------INITIAL POSITIONS OF SPRITES-------------------
INITSPRITEPOS

         LDX #$00               ; LOAD SPROTE INITIAL COORDINATES
POSSPRTS LDA POSTABLE+$00,X     ; FROM POSITION TABLE
         STA SPRITEPOS+$00,X
         INX
         CPX #$0E             ; 7 SPRITES -> X & Y POS -> 14 BYTES     
         BNE POSSPRTS
         RTS

;---------------------SETTING UP SPRITES----------------------------
SETUPSPRITES
        ; SET SPRITE MULTICOLORS
        LDA #$0F        ;--> LT.GREY
        STA $D025
        LDA #$02        ;--> RED
        STA $D026

        ; COLORIZE SPRITES
        LDA #$0E        ;-> SPRITE 0 (MAINSHIP) COLOR = LT.BLUE
        STA $D027
        LDA #$01        ;-> SPRITE 1 (ENEMY) COLOR = WHITE      
        STA $D028
        LDA #$01        ;-> SPRITE 2 (BULLET) COLOR = WHITE
        STA $D029
        LDA #$0B        ;-> SPRITE 3 (ENEMY BULLET) COLOR = DARK GREY 
        STA $D02A
        LDA #$0B        ;-> SPRITE 4 (ENEMY2) COLOR = DARK GREY
        STA $D02B
        LDA #$07        ;-> SPRITE 5 (EMENY 2 BULLET) COLOR = YELLOW
        STA $D02C
        LDA #$07        ;-> SPRITE 6 (EXPLOSION) COLOR = MED.GREY 
        STA $D02D

        ; SET MULTICOLOR BITS
        LDA #%01111111        ; SPRITES ARE MULTICOLOR
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
        LDA #$84        ; ENEMY SHIP 2
        STA $07FC        
        LDA #$85        ; ENEMY SHIP 2 BULLET
        STA $07FD        
        LDA #$86        ; EXPLOSION
        STA $07FE        

        ; TURN ON SPRITES
        LDA #%00111111        ; TURN ON 6 SPRITES - EXPLOSION NOT INCLUDED
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

;-----------------DRAW ENTER THE ARENA SCREEN ROUTINE  ------------------------------------
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
                RTS

;-----------------DRAW THE ARENA SCREEN ROUTINE  ------------------------------------
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

;--------------------SCROLLTEXT------------------------------
SCROLLBOTTOMTEXT

        LDA #$00        ; character number counter
        STA $04         ; use zp for storage of a variable

LOOPA3             
        LDA #$F0        ; wait until rasterline $C8
LOOPA4  CMP $D012 
        BNE LOOPA4

        LDA $02         ; load value of $2 to scroll x pos register
        STA $D016       ; using zp unused address for faster access

        LDA #$FF        
LOOPA6  CMP $D012       ; wait until rasterline $FF (256)
        BNE LOOPA6

        LDA #$00        ; reset scroll register
        STA $D016 
        LDA $02         ; load zp value
        SEC 
        SBC #$01
        AND #$07        ; 
        STA $02 
        CMP #$07
        BNE LOOPA8  

        LDX #$00        ; replacing characters for row 12
LOOPA9  LDA $07C0,X     ; first character of row 12
        STA $07BF,X     ; last character of row 11
        INX 
        CPX #$27        ; 0-39, one row of characters 
        BNE LOOPA9

        LDX $04 
        LDA SCROLLTEXT,X ; load scroll text character
        STA $07E6        ; write it to row 12 col 39 
        INC $04                   
        LDA #$50         ;         
        CMP $04 
        BNE LOOPA8

        LDA #$00
        STA $04 
  
LOOPA8  JSR $FFE4
        BEQ LOOPA3
        STA KEY
        CMP #$20
        BNE LOOPA3  

        LDA #%00011011           ; $1B -> ENABLE 25 ROWS
        STA $D011
        LDA #%11001000           ;$C8
        STA $D016   
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
          
        LDA #%00011011           ; $1B
        STA $D011
        LDA #%11001000           ;$C8
        STA $D016                
        LDA #%00011010          ; $1A
        STA $D018
        LDA #%11000111          ; $C7
        STA $DD00        

        LDA #$00
        STA $D020
        STA $D021
        RTS

;------------------PLAY TITLE SCREEN MUSIC--------------
PLAYMUSIC
        LDA #$01        ;SELECT IN-GAME TUNE
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
         CPY #$DE         ;REACHES #$DE
         BCC SETDOWN      ;THEN STOP
         LDY #$DE        ;MOVING PLAYER
SETDOWN  STY SPRITEPOS+$01

LEFT     LSR A            ;READ LEFT
         BCS RIGHT        ;NOT LEFT
         LDY SPRITEPOS+$00
         DEY              ;AS WITH UP
         CPY #$1A         ;AND DOWN, BUT
         BCS SETLEFT      ;MOVING LEFT
         LDY #$1A         ;
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
         ;INY
         CPY #$E5               ; REACHED DOWN THE SCREEN?
         BCC STOPENMSB          ; IF
         LDY #$00               ; IF YES, MAKE THE COORDINATES ZERO 
         STY SPRITEPOS+$06      ; TO GET IT OUT OF SCREEN
         STY SPRITEPOS+$07 
         LDA #$00               ; SET ENEMYBULLET TO 0, SO THERE IS NO
         STA ENEMYBULLET        ; ENEMY BULLET ON SCREEN

STOPENMSB  
         STY SPRITEPOS+$07
         
         RTS

;-------------------------ENEMYSHIP MOVEMENT-------------------------------------
ENEMYSHIP 
          LDX BYTECOUNT 
          LDA SINEX,X      ;SINE MOVEMENT X POSITION
          STA SPRITEPOS+$02     
          LDA SINEY,X      ; SINE MOVEMENT Y POSITION
          STA SPRITEPOS+$03
          INC BYTECOUNT
          RTS

;-------------------------ENEMYSHIP 2 MOVEMENT-------------------------------------
ENEMYSHIP2 
          LDA ENEMY2DIR 
          CMP #$00
          BNE GOUP
          
          LDX SPRITEPOS+$09
          INX
          CPX #$E5
          BEQ CDIRUP      
          JMP FINENEMY2

CDIRUP
          INC ENEMY2DIR
          JMP FINENEMY2

GOUP      LDX SPRITEPOS+$09
          DEX
          CPX #$90
          BEQ CDIRDOWN       
          JMP FINENEMY2

CDIRDOWN
          DEC ENEMY2DIR
          JMP FINENEMY2          

FINENEMY2
          STX SPRITEPOS+$09
          RTS

;------------------ENEMY SHIP 2 BULLET --------------------------------------
ENEMYFIRE2

        LDA ENEMYBULLET2         ; CHECK IF THERE IS ALREADY AN ENEMY BULLET 2
        CMP #$00
        BNE ENBULMOVE2           ; IF BULLET 2 EXISTS, GO TO MOVE SECTION

        LDY #$10                ; COPY ENEMY SHIP 2 COORDINATES
        STY SPRITEPOS+$0A       ; TO ENEMY BULLET 2
        LDY SPRITEPOS+$09     
        STY SPRITEPOS+$0B  
        LDY #$01                ; SET ENEMYBULLET2 TO 1, SO THERE IS A BULLET 2
        STY ENEMYBULLET2        ; ON SCREEN
        LDA #4
        STA SOUND               ; PLAY THE SHOOTING SOUND
        JSR PLAYSOUND
        RTS

ENBULMOVE2
         LDY SPRITEPOS+$0A      ; LOAD THE BULLET 2 X POSITION
         INY                    ; INCREMENT X POSITION 
         ;INY
         CPY #156               ; REACHED RIGHT EDGE OF THE SCREEN?
         BCC STOPENMSB2          
         LDY #$00               ; IF YES, MAKE THE COORDINATES ZERO 
         STY SPRITEPOS+$0A      ; TO GET IT OUT OF SCREEN
         STY SPRITEPOS+$0B 
         LDA #$00               ; SET ENEMYBULLET2 TO 0, SO THERE IS NO
         STA ENEMYBULLET2       ; ENEMY BULLET 2 ON SCREEN

STOPENMSB2  
         STY SPRITEPOS+$0A      ; WRITE THE FINAL X POSITION TO VARIABLE
         RTS
;--------------COLLIDED 3 - BULLETS COLLIDE----------------
COLLIDED3
          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$06      
          STA $D01E
          STA NOMOVEBULL
          STA ENEMYBULLET
          RTS

;--------------COLLIDED 4 - BULLETS COLLIDE----------------
COLLIDED4
          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$0A      
          STA $D01E
          STA NOMOVEBULL
          STA ENEMYBULLET2
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
          CMP #%00100001        ; CHECK IF ENEMY SHIP 2 BULLET AND MAIN SHIP COLLIDED
          BEQ COLLIDED2
          CMP #%00100100        ; CHECK IF ENEMY SHIP 2 BULLET AND MAIN SHIP COLLIDED
          BEQ COLLIDED4
          RTS

;----------------MAIN SHIP BULLET AND ENEMY SHIP COLLIDED--------------------------
COLLIDED
   
          LDA #%01010001        ; JUST SPRITE 0 (MAINSHIP) AND 
          STA $D015             ; SPRITE 6 (COLLISION SPRITE) IS ON

          LDA SPRITEPOS+$02     ; TRANSFER CURRENT ENEMY SHIP POS TO COLLISION COORDINATES
          STA SPRITEPOS+$0C
          LDA SPRITEPOS+$03
          STA SPRITEPOS+$0D

          LDA #7
          STA SOUND
          JSR PLAYSOUND         ; PLAY COLLOSION SOUND
          JSR ENEMYHIT          ; WRITE "ENEMY HIT" TEXT

          JSR COLLANIM          ; COLLISION ANIMATION

          JSR ENEMYHITCLEAR     ; CLEAR THE BOTTOM TEXT
          
          ; RETURN BACK TO ORIGINAL POSITIONS EXCEPT MAIN SHIP
          LDA #%00111111
          STA $D015              ; FIRST 4 SPRITES ARE ON (AGAIN)        
          
          LDA #$00               ; RESET COLLISION REGISTER
          STA $D01E      
          STA NOMOVEBULL        ; RESET MAINSHIP BULLET REGISTER
          STA ENEMYBULLET

          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$05
          STA SPRITEPOS+$06
          STA SPRITEPOS+$07
          STA SPRITEPOS+$0A
          STA SPRITEPOS+$0B  
          
          JSR WRITESCORE        ; JUMP TO WRITE SUBROUTINE
          JMP MAINLOOP          ; RETURN BACK TO THE MAINLOOP

;-------------------ENEMY SHIP BULLET AND MAINSHIP COLLIDED------------------------
COLLIDED2
          
          LDA #%01010010        ; JUST SPRITE 1 (ENEMY SHIP), SPRITE 4 (ENEMY SHIP 2 ) AND
          STA $D015             ; SPRITE 6 (COLLISION SPRITE) IS ON

          JSR EXPANDSPRITEPOS

          LDA SPRITEPOS+$00     ; TRANSFER CURRENT MAIN SHIP POS TO COLLISIN COORDINATES
          STA SPRITEPOS+$0C
          LDA SPRITEPOS+$01
          STA SPRITEPOS+$0D

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

          LDA #%00111111        ; FIRST 5 SPRITES ARE ON (AGAIN)  
          STA $D015           
          
          LDA #$00              ; RESET COLLISION REGISTER
          STA $D01E      
          STA ENEMYBULLET       ; RESET ENEMY SHIP BULLET REGISTER
     
          LDA #$00
          STA SPRITEPOS+$04
          STA SPRITEPOS+$05
          STA SPRITEPOS+$06
          STA SPRITEPOS+$07          
          STA SPRITEPOS+$0A
          STA SPRITEPOS+$0B               

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

        LDA #$02
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

         JSR EXPANDSPRITEPOS

         LDY #00
EXPLOOP  LDX #$00
EXPLPLR  LDA EXPLTBL,X
         STA $07FE
         JSR DRAWCOL
         INX
         CPX #$12
         BNE EXPLPLR
         INY
         CPY #20
         BNE EXPLOOP

         LDA #$00
         STA $D020
         RTS

DRAWCOL   
         INC $D020
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
incbin "NEWFONT3.BIN"
   
*=$3000
incbin "MUSIC.BIN",2

*=$6000
incbin "PIXEL_SPACE61.PRG",2
   
SPRITEPOS  = $0370    ;POSITIONS FOR THE SPRITES
FIRELOCKUP = $0330    ;VARIABLE FOR LOCKING UP THE FIRE BUTTON
NOMOVEBULL      BYTE 0

BYTECOUNT       BYTE $00        ; ENEMY SHIP POS COUNTER FOR THE SINE TABLE
SCORE           BYTE 0, 0, 0    ; SCORE VARIABLE FOR 6 DIGITS
ENEMYBULLET     BYTE $00        ; ENEMY BULLET VARIABLE 0=NO BULLET ON SCREEN
ENEMYBULLET2    BYTE $00        ; ENEMY BULLET 2 VARIABLE 0=NO BULLET ON SCREEN
ENEMY2DIR       BYTE $00
LIVES           BYTE $00        ; NUMBER OF LIVES
KEY             BYTE 0          ; GET CHAR. OF KEY PRESSED
KEYB            BYTE 0
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

EXPLTBL  BYTE $86,$86,$86,$87,$87,$87
         BYTE $88,$88,$88,$89,$89,$89
         BYTE $88,$88,$88,$87,$87,$87

SCROLLTEXT      TEXT 'hit space to continue... a-space-game ..'
                TEXT 'design and programming : metesev 2021   '   

; SPRITE INITIAL POSITIONS
POSTABLE BYTE $56,$D0,$55,$3C,$00,$00,$00,$00,$0e,$96,$00,$00,$00,$00

;ENEMYSHIP MOVEMENT COORDINATES (SINE TABLES)
SINEX          
        BYTE 91,89,88,86,85,83,81,80,78,77,75,73,72,70,69,67
        BYTE 66,64,63,61,60,59,57,56,54,53,52,51,49,48,47,46
        BYTE 45,43,42,41,40,39,38,37,36,36,35,34,33,32,32,31
        BYTE 30,30,29,29,28,28,27,27,27,26,26,26,26,26,26,26
        BYTE 26,26,26,26,26,26,26,27,27,27,28,28,28,29,29,30
        BYTE 31,31,32,33,33,34,35,36,37,38,39,40,41,42,43,44
        BYTE 45,46,47,49,50,51,53,54,55,57,58,59,61,62,64,65
        BYTE 67,68,70,71,73,74,76,77,79,81,82,84,85,87,89,90
        BYTE 92,93,95,97,98,100,101,103,105,106,108,109,111,112,114,115
        BYTE 117,118,120,121,123,124,125,127,128,129,131,132,133,135,136,137
        BYTE 138,139,140,141,142,143,144,145,146,147,148,149,149,150,151,151
        BYTE 152,153,153,154,154,154,155,155,155,156,156,156,156,156,156,156
        BYTE 156,156,156,156,156,156,156,155,155,155,154,154,153,153,152,152
        BYTE 151,150,150,149,148,147,146,146,145,144,143,142,141,140,139,137
        BYTE 136,135,134,133,131,130,129,128,126,125,123,122,121,119,118,116
        BYTE 115,113,112,110,109,107,105,104,102,101,99,97,96,94,93,91

SINEY
        BYTE 60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,61
        BYTE 62,62,62,62,63,63,63,64,64,65,65,65,66,66,67,67
        BYTE 68,68,69,69,70,70,71,71,72,72,73,74,74,75,75,76
        BYTE 77,77,78,79,79,80,80,81,82,82,83,84,84,85,86,86
        BYTE 87,88,89,89,90,91,91,92,93,93,94,95,95,96,96,97
        BYTE 98,98,99,100,100,101,101,102,102,103,104,104,105,105,106,106
        BYTE 107,107,108,108,108,109,109,110,110,110,111,111,111,112,112,112
        BYTE 113,113,113,113,113,114,114,114,114,114,114,114,114,114,114,114
        BYTE 114,114,114,114,114,114,114,114,114,114,114,113,113,113,113,113
        BYTE 112,112,112,111,111,111,110,110,110,109,109,108,108,108,107,107
        BYTE 106,106,105,105,104,104,103,102,102,101,101,100,100,99,98,98
        BYTE 97,96,96,95,95,94,93,93,92,91,91,90,89,89,88,87
        BYTE 86,86,85,84,84,83,82,82,81,80,80,79,79,78,77,77
        BYTE 76,75,75,74,74,73,72,72,71,71,70,70,69,69,68,68
        BYTE 67,67,66,66,65,65,65,64,64,63,63,63,62,62,62,62
        BYTE 61,61,61,61,61,60,60,60,60,60,60,60,60,60,60,60

*=$2000
; SPRITE #0 --> MAINSHIP --> POINTER $80
        BYTE $00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$54,$00,$00,$54,$00,$00,$64,$00,$00,$64
        BYTE $00,$01,$65,$00,$31,$45,$30,$31,$45,$30,$11,$45,$10,$11,$65,$10,$11,$65,$10,$55
        BYTE $A9,$54,$56,$AA,$54,$5A,$AA,$94,$C9,$55,$8C,$C9,$DD,$8C,$C0,$DC,$0C,$CF,$CF,$C0
        BYTE $0F,$03,$C0
        BYTE 0

; SPRITE #1 --> ENEMY SHIP --> POINTER $81
        BYTE $00,$3C,$00,$00,$3C,$00,$01,$FF,$40,$05,$FF,$50,$17,$FF,$D4,$5F,$FF,$F5,$5F,$C3
        BYTE $F5,$5F,$C3,$F5,$1F,$C3,$F4,$17,$C3,$D4,$07,$FF,$D0,$05,$BE,$50,$01,$BE,$40,$01
        BYTE $BE,$40,$00,$BE,$00,$00,$BE,$00,$00,$BE,$00,$00,$AA,$00,$00,$28,$00,$00,$28,$00
        BYTE $00,$28,$00
        BYTE 0

; SPRITE #2 --> MAIN SHIP BULLET --> POINTER $82
; Sprite 1
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$41,$00,$00,$41
        BYTE $00,$00,$41,$00,$00,$82,$00,$00,$82,$00,$00,$82,$00,$00,$82,$00,$00,$C3,$00,$00
        BYTE $C3,$00,$00,$C3,$00,$00,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #3 --> ENEMY SHIP BULLET--> POINTER $83
; Sprite 1
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$C3,$00,$00,$C3
        BYTE $00,$00,$C3,$00,$00,$82,$00,$00,$82,$00,$00,$82,$00,$00,$82,$00,$00,$41,$00,$00
        BYTE $41,$00,$00,$41,$00,$00,$41,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0        

; SPRITE #4 --> ENEMY 2 SHIP --> POINTER $84
; Sprite 1
        BYTE $00,$00,$00,$00,$15,$40,$00,$55,$00,$01,$54,$00,$01,$60,$00,$05,$A0,$00,$05,$A0
        BYTE $00,$15,$A0,$00,$D6,$BF,$00,$FA,$BF,$C0,$FA,$BF,$F0,$FA,$BF,$C0,$D6,$BF,$00,$15
        BYTE $A0,$00,$05,$A0,$00,$05,$A0,$00,$01,$60,$00,$01,$54,$00,$00,$55,$00,$00,$15,$40
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #5 --> ENEMY 2 SHIP BULLET --> POINTER $85
; Sprite 1
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$0C,$00,$00,$0F,$D8,$00,$03,$D6,$80,$03,$5A,$A0,$03,$D6,$80,$0F
        BYTE $D8,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0        

; SPRITE #6 --> EXPLOSION 1  --> POINTER $86
        BYTE $00,$00,$80,$02,$08,$0C,$00,$00,$00,$C0,$20,$00,$30,$00,$F2,$00,$20,$00,$0C,$00
        BYTE $C0,$03,$14,$02,$03,$55,$00,$20,$1D,$20,$01,$64,$80,$82,$39,$40,$01,$17,$00,$08
        BYTE $70,$48,$00,$55,$00,$80,$14,$C2,$03,$00,$00,$30,$20,$30,$00,$00,$00,$32,$08,$0C
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 2 --> POINTER $87
        BYTE $00,$00,$00,$02,$08,$00,$00,$00,$00,$00,$00,$00,$30,$00,$C0,$00,$20,$00,$00,$00
        BYTE $00,$00,$14,$00,$03,$45,$00,$80,$1D,$20,$01,$64,$80,$00,$39,$40,$01,$17,$00,$08
        BYTE $70,$48,$00,$54,$00,$80,$00,$C0,$03,$00,$00,$00,$20,$30,$00,$00,$00,$30,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 3 --> POINTER $88
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$00,$00
        BYTE $00,$00,$14,$00,$03,$45,$00,$00,$10,$00,$01,$60,$00,$00,$09,$40,$01,$13,$00,$08
        BYTE $40,$40,$00,$14,$00,$00,$00,$00,$03,$00,$00,$00,$20,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 4 --> POINTER $89
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$14,$00,$00,$45,$00,$00,$10,$00,$00,$60,$00,$00,$09,$00,$00,$10,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SCREEN 1 -  SCREEN DATA
*=$5000 
        BYTE    $20,$01,$2D,$13,$10,$01,$03,$05,$2D,$07,$01,$0D,$05,$20,$20,$20,$0C,$09,$16,$05,$13,$3A,$33,$20,$20,$20,$13,$03,$0F,$12,$05,$3A,$30,$30,$30,$30,$30,$30,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$56,$5C,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5C,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$5C,$20,$20,$56,$20,$20,$20,$20,$20
        BYTE    $20,$20,$56,$4C,$4F,$52,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$5C,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20
        BYTE    $20,$20,$4A,$4D,$50,$53,$4B,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$5C,$20,$20,$56,$20
        BYTE    $20,$20,$56,$4E,$51,$54,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$5C,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$56,$20,$5C,$20,$20,$20
        BYTE    $20,$20,$20,$56,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$58,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20
        BYTE    $20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$57,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$5A,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$58,$56,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$56,$20,$20,$20,$56,$20,$56,$20,$5C,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$57,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20
        BYTE    $20,$20,$20,$5C,$20,$20,$56,$20,$56,$20,$5C,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$5A,$20,$20,$56,$20,$20,$20,$20,$5C,$20,$20,$20,$20,$62,$65,$68,$20,$20,$20,$20,$20,$20
        BYTE    $56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$63,$66,$69,$20,$5C,$20,$56,$20,$20
        BYTE    $20,$20,$56,$20,$20,$20,$5C,$20,$56,$20,$20,$20,$20,$20,$56,$20,$5C,$20,$56,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$64,$67,$6A,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$5A,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$5C,$56,$20,$20,$20,$20,$20,$5A,$20,$20,$20,$20,$5C,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5C,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$5C,$20,$20,$20,$20,$56,$20,$20,$20,$56
        BYTE    $20,$5A,$20,$20,$5C,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; Screen 1 -  Colour data
*=$5400
        BYTE    $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$00,$00,$0F,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$01,$01
        BYTE    $00,$00,$0F,$00,$00,$00,$0E,$00,$00,$00,$01,$00,$0E,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$0F,$0F,$00,$07,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$01,$01,$0F,$01,$0F,$0E,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$01,$0E,$0E,$0E,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$07,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$0E,$0E,$0E,$0E,$0E,$01,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$01,$0F,$0F,$0E,$0F,$00,$0F,$01
        BYTE    $00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$00,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0E,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$0E,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0F,$00,$00,$08,$07,$08,$00,$01,$0F,$01,$01,$07,$0F,$07,$0F,$0F,$01
        BYTE    $00,$00,$00,$01,$01,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$08,$08,$08,$08,$07,$01,$01,$01,$01,$00,$00,$00,$00,$0F,$01
        BYTE    $00,$0F,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$01,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$01,$08,$01,$08,$00,$01,$01,$01,$0F,$0F,$0E,$0F,$0F,$0E,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$07,$00,$00,$00,$00,$01,$0F,$01,$01,$01,$05,$05,$05,$05,$0F,$01
        BYTE    $00,$01,$01,$00,$00,$0F,$00,$00,$0C,$00,$00,$0F,$00,$0E,$00,$00,$00,$0E,$00,$0F,$00,$0F,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$01,$01,$07,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$07,$01,$07,$01,$01,$01,$01,$05,$01,$01
        BYTE    $00,$00,$00,$0F,$00,$00,$0E,$00,$0F,$00,$0B,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$0E,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0E,$00,$00,$00,$00,$0E,$00,$00,$01,$01,$08,$08,$08,$05,$01,$05,$05,$0F,$01
        BYTE    $0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$0E,$01,$08,$08,$08,$05,$07,$05,$0E,$05,$01
        BYTE    $00,$00,$0E,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$0F,$00,$0F,$0E,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$01,$08,$08,$08,$02,$02,$02,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$01,$00,$00,$00,$01,$00,$01,$01,$01,$0D,$0D,$0F,$0D,$0D,$0F,$01
        BYTE    $00,$00,$00,$0F,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$0B,$07,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$0D,$0F,$0D,$0D,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$01,$02,$02,$01,$0F
        BYTE    $00,$0E,$00,$00,$0C,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$0E,$01,$01,$01
        BYTE    $00,$00,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$00,$0F,$0F,$0F,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$0F,$0F,$0F,$09,$0F,$0F,$0F,$0F,$0F,$00,$01

; SCREEN 2 -  SCREEN DATA
*=$4800
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$78,$78,$78,$20,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$20,$78,$78,$78,$78,$78,$78,$78,$20,$20,$20,$78,$78,$78,$78,$20,$78,$20,$20
        BYTE    $20,$20,$78,$20,$78,$20,$78,$20,$20,$78,$20,$78,$78,$20,$78,$78,$20,$20,$78,$20,$20,$20,$78,$20,$20,$78,$20,$78,$78,$78,$20,$78,$78,$78,$20,$20,$20,$78,$20,$20
        BYTE    $20,$20,$78,$78,$78,$20,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$20,$20,$78,$78,$20,$20,$78,$20,$78,$78,$78,$78,$78,$20,$78,$20,$78,$78,$78,$20,$20,$78,$20,$20
        BYTE    $20,$20,$78,$20,$78,$20,$20,$20,$78,$78,$20,$20,$78,$20,$78,$78,$20,$20,$78,$20,$20,$20,$78,$20,$78,$78,$20,$78,$78,$20,$20,$20,$78,$78,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$78,$20,$78,$20,$78,$78,$78,$78,$20,$20,$78,$20,$78,$78,$78,$78,$78,$78,$78,$20,$78,$78,$78,$78,$20,$78,$78,$20,$58,$20,$78,$78,$78,$78,$20,$78,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$56,$20,$5A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$17,$05,$0C,$03,$0F,$0D,$05,$20,$14,$0F,$20,$22,$01,$20,$13,$10,$01,$03,$05,$20,$07,$01,$0D,$05,$22,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20
        BYTE    $20,$20,$20,$56,$19,$0F,$15,$20,$17,$09,$0C,$0C,$20,$06,$01,$03,$05,$20,$04,$05,$01,$04,$0C,$19,$20,$05,$0E,$05,$0D,$19,$20,$13,$08,$09,$10,$13,$20,$20,$20,$20
        BYTE    $20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$57,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$09,$0E,$20,$14,$08,$09,$13,$20,$07,$01,$0D,$05,$20,$21,$21,$21,$20,$13,$0F,$20,$02,$05,$20,$10,$12,$05,$10,$01,$12,$05,$04,$2C,$20,$20,$20,$20
        BYTE    $20,$58,$56,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$56,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20
        BYTE    $20,$20,$20,$20,$02,$05,$20,$02,$12,$01,$16,$05,$2E,$2E,$2E,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$57,$20,$20,$20,$20,$56,$20,$20,$20,$20,$58,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56
        BYTE    $20,$20,$20,$20,$10,$0C,$05,$01,$13,$05,$20,$03,$0F,$0E,$0E,$05,$03,$14,$20,$19,$0F,$15,$12,$20,$0A,$0F,$19,$13,$14,$09,$03,$0B,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20
        BYTE    $20,$20,$56,$20,$14,$0F,$20,$10,$0F,$12,$14,$20,$14,$17,$0F,$20,$14,$0F,$20,$03,$0F,$0E,$14,$12,$0F,$0C,$20,$19,$0F,$15,$12,$20,$13,$08,$09,$10,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$01,$12,$05,$20,$19,$0F,$15,$20,$12,$05,$01,$04,$19,$20,$14,$0F,$20,$05,$0E,$14,$05,$12,$20,$14,$08,$05,$20,$01,$12,$05,$0E,$01,$3F,$20,$20,$20
        BYTE    $20,$5A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; SCREEN 2 -  COLOUR DATA
*=$4C00 
        BYTE    $00,$0B,$0B,$0B,$02,$0B,$0B,$0B,$07,$0B,$0B,$0B,$07,$0B,$0B,$0B,$07,$0B,$0B,$0B,$07,$0B,$0B,$0B,$02,$0B,$0B,$0B,$01,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$07,$0B,$0B,$0B
        BYTE    $00,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$0F,$0F,$0F,$0C,$0C,$0C,$0B,$0B,$0B,$07,$07,$07,$07,$01,$01,$01,$0F,$0C,$0C,$0F,$0F,$0C,$0C,$0C,$07,$07,$01,$01
        BYTE    $00,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$0F,$0F,$0F,$0C,$01,$0C,$0B,$01,$07,$07,$07,$00,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0C,$0F,$0F,$07,$07,$0F,$01
        BYTE    $00,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$0F,$0F,$0F,$0C,$00,$0C,$0B,$0B,$01,$07,$07,$07,$07,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0C,$0C,$0C,$07,$07,$01,$01
        BYTE    $00,$07,$07,$07,$07,$01,$07,$07,$07,$01,$00,$01,$0F,$0F,$0F,$0C,$01,$0C,$0B,$00,$07,$07,$07,$07,$07,$01,$01,$01,$0F,$01,$01,$0F,$0F,$0C,$01,$0C,$07,$07,$0F,$01
        BYTE    $00,$07,$07,$07,$07,$07,$07,$07,$07,$01,$07,$01,$0F,$0F,$0F,$0C,$0C,$0C,$0B,$0B,$0B,$07,$07,$07,$07,$01,$01,$01,$0F,$07,$0E,$0F,$0F,$0C,$0C,$0C,$07,$07,$0F,$01
        BYTE    $00,$07,$00,$07,$02,$07,$07,$07,$07,$07,$00,$00,$00,$07,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$02,$07,$07,$07,$0F,$07,$01,$07,$07,$07,$0F,$07,$0F,$07,$07,$07
        BYTE    $00,$00,$0E,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$01,$00,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$09,$01,$01,$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$01,$01
        BYTE    $00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$0F,$01
        BYTE    $00,$0F,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0F,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$01,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$0F,$01
        BYTE    $00,$01,$01,$00,$00,$0F,$00,$00,$0E,$00,$00,$0F,$00,$0E,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$07,$07,$07,$07,$07,$07,$0D,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$05,$05,$01,$05,$05,$0F,$01
        BYTE    $0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$01,$01,$01,$01,$05,$05,$05,$01,$05,$01
        BYTE    $00,$00,$0E,$00,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$02,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$01,$00,$00,$00,$01,$00,$01,$01,$0F,$0D,$0D,$0D,$0D,$0D,$0F,$01
        BYTE    $00,$00,$00,$0F,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$01,$0D,$0D,$0D,$01
        BYTE    $00,$00,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$01
        BYTE    $00,$0E,$00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01














