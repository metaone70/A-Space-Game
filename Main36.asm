; 10 SYS (2080):REM (c) 2021 mtsv

*=$0801

        BYTE    $1E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $3a, $8f, $20, $28, $43, $29, $20, $32, $30, $32, $31, $20, $4D, $54, $53, $56, $00, $00, $00

*=$0820

         ;SEI

         LDA #$20               ; WHEN RESTORE KEY IS HIT
         STA $0318              ; IT POINTS TO THE START
         LDA #$08               ; OF THIS PROGRAM, WHICH IS 
         STA $0319              ; $0820

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
         STA SCORE
         STA SCORE+1

         JSR TITLESCREEN         ;DRAW THE BITMAP TITLE SCREEN
         JSR DRAWTEXTSCREEN      ;DRAW THE ENTER THE ARENA SCREEN
         JSR SCROLLBOTTOMTEXT    ;WRITE THE SCROLLING TEXT AT THE BOTTOM OF SCREEN
         JSR DRAWSCREEN          ;DRAW THE CHARACTER (BATTLE) SCREEN
         JSR WRITEHISCORE        ;WRITE THE HI-SCORE TO SCREEN
         JSR INITSPRITEPOS       ;INITIALIZE SPRITE POSITIONS WITH THE TABLE
         JSR SETUPSPRITES        ;INITIALIZE SPRITES
         JSR STOPMUSIC           ;STOP THE MUSIC, SINCE THE BATTLE IS ABOUT TO BEGIN


;---------------------MAIN GAME LOOP-----------------------------------------       
MAINLOOP  
          JSR WAITRASTER        ; WAIT FOR RASTERLINE TO SLOW DOWN
          JSR STARANIMATION     ; ANIMATE BACKGROUND STARS
          JSR EXPANDSPRITEPOS   ; ADJUST SPRITE POSITONS FOR X > 255
          JSR MAINSHIP          ; WAIT FOR JOYSTICK TO COMMAND AND MOVE MAIN SHIP
          JSR BULLETMOVE        ; MAINSHIP BULLET CHECK AND CONTROL
          JSR ENEMYSHIP         ; MOVE ENEMY SHIP ACCORDING TO THE SINE TABLE
          JSR ENEMYFIRE         ; ENEMYSHIP FIRE CHECK AND CONTROL
          JSR ENEMYSHIP2        ; MOVE ENEMYSHIP 2 
          JSR ENEMYFIRE2        ; CONTROL ENEMY SHIP 2 BULLET
          JSR COLLISIONCHECK    ; CHECK IF ANY COLLISION OCCURS
          JMP MAINLOOP          ; CONTINUE THE LOOP

;-----------INITIAL POSITIONS OF SPRITES-------------------
INITSPRITEPOS

         LDX #$00               ; LOAD SPRITE INITIAL COORDINATES
POSSPRTS LDA POSTABLE+$00,X     ; FROM POSITION TABLE
         STA SPRITEPOS+$00,X
         INX
         CPX #$0E               ; 7 SPRITES -> X & Y POS -> 14 BYTES     
         BNE POSSPRTS
         RTS

;---------------------SETTING UP SPRITES----------------------------
SETUPSPRITES
        ; SET SPRITE MULTICOLORS
        LDA #$0F                  ;--> LT.GREY
        STA $D025
        LDA #$02                  ;--> RED
        STA $D026

        ; COLORIZE SPRITES
        LDA #$0E                 ;-> SPRITE 0 (MAINSHIP) COLOR = LT.BLUE
        STA $D027
        LDA #$01                 ;-> SPRITE 1 (ENEMY) COLOR = WHITE      
        STA $D028
        LDA #$01                 ;-> SPRITE 2 (BULLET) COLOR = WHITE
        STA $D029
        LDA #$0B                 ;-> SPRITE 3 (ENEMY BULLET) COLOR = DARK GREY 
        STA $D02A
        LDA #$0B                 ;-> SPRITE 4 (ENEMY2) COLOR = DARK GREY
        STA $D02B
        LDA #$07                 ;-> SPRITE 5 (EMENY 2 BULLET) COLOR = YELLOW
        STA $D02C
        LDA #$07                 ;-> SPRITE 6 (EXPLOSION) COLOR = MED.GREY 
        STA $D02D
        
        LDA #%01111111          ; SET MULTICOLOR BITS     
        STA $D01C               ; SPRITES ARE MULTICOLOR
        
        LDA #$00                ; SET SCREEN-SPRITE PRIORITY FLAGS
        STA $D01B

                                ; SET SPRITE POINTERS --> $2000
        LDA #$80                ; MAIN SHIP
        STA $07F8
        LDA #$81                ; ENEMY SHIP
        STA $07F9
        LDA #$82                ; MAIN SHIP BULLET
        STA $07FA
        LDA #$83                ; ENEMY SHIP BULLET
        STA $07FB       
        LDA #$84                ; ENEMY SHIP 2
        STA $07FC        
        LDA #$85                ; ENEMY SHIP 2 BULLET
        STA $07FD        
        LDA #$86                ; EXPLOSION
        STA $07FE        

        LDA #%00111111         ; TURN ON 6 SPRITES - EXPLOSION NOT INCLUDED
        STA $D015
        RTS

;-----------------EXPANDING SPRITE POSITIONS FOR X>255 -------------------
EXPANDSPRITEPOS  
 
                LDX #$00
XLOOP           LDA SPRITEPOS+$01,X     ; Y COORDINATES
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
                STA $D020 
                STA $D021 

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

;-----------------------STARFIELD ANIMATION-----------------------------
STARANIMATION

                SEI
                LDA #<IRQ2                      ; USING IRQ FOR ANIMATION
                STA $314
                LDA #>IRQ2
                STA $315
                LDA #$7F
                STA $DC0D
                AND $D011        
                STA $D011
                LDA $DC0D
                LDA $DD0D
                LDA #$01
                STA $D01A
                CLI
                RTS
 
IRQ2                   
                 LDX #$00
LOOP             DEC STARSDELAYARRAY,X           ; ONLY UPDATE WHEN THE DELAY IS ZERO
                 BEQ OK                         
                 JMP SKIPUPDATE                

OK               LDA STARSSPEEDCOLARRAY,X        ;RESET THE STAR DELAY
                 STA STARSDELAYARRAY,X     

                 STX STARSCURRENTX        
                 LDA STARSYCHARARRAY,X           ; SET THE CURRENT X & Y CHARS
                 STA STARSCURRENTY        
                 INC STARSFRAMEARRAY,X           ; MOVE TO THE NEXT STAR ANIMATION FRAME
                 LDA STARSFRAMEARRAY,X    
                 CMP #94
                 BNE SKIP                   

                 LDA #86                        ; RESET THE STAR FRAME TO STARS 1ST CHARACTER
                 STA STARSFRAMEARRAY,X        

                 LDY STARSCURRENTY              ; ERASE THE CURRENT STAR CHARACTER ON SCREEN
                 LDA SCREENRAMROWSTARTLOW,Y     
                 STA $03 
                 LDA SCREENRAMROWSTARTHIGH,Y   
                 STA $04
                 LDY STARSCURRENTX             

                 LDA #$20                      
                 STA ($03),Y 

                 INC STARSYCHARARRAY,X          ;INCREMENT CHARACTER POSITION
                 LDA STARSYCHARARRAY,X         
                 CMP #$19
                 BNE SKIP                       

                 LDA #$01
                 STA STARSYCHARARRAY,X          

                 INC STARSNEXTSPEED             ; RESET THE SPEED & COLOR    
                 LDA STARSNEXTSPEED            
                 STA STARSSPEEDCOLARRAY,X      
                 CMP #$04
                 BNE SKIP                       
                 LDA #$01                       ; RED & FAST - GETS INCREMENTED TO 2 FIRST TIME AROUND
                 STA STARSNEXTSPEED             

SKIP             LDA STARSYCHARARRAY,X          ; SET THE CURRENT Y CHAR
                 STA STARSCURRENTY           
                 LDA STARSFRAMEARRAY,X          ; SET THE CURRENT FRAME 
                 STA STARSCURRENTFRAME         
                 LDA STARSSPEEDCOLARRAY,X       ; SET THE CURRENT COLOR
                 STA STARSCURRENTCOLOR         

                 LDY STARSCURRENTY 
                 LDA COLORRAMROWSTARTLOW,Y     
                 STA $03 
                 LDA COLORRAMROWSTARTHIGH,Y   
                 STA $04 
                 LDY STARSCURRENTX              
                 LDA STARSCURRENTCOLOR         
                 STA ($03),Y 

                 LDY STARSCURRENTY              ; DRAW THE CURRENT STAR
                 LDA SCREENRAMROWSTARTLOW,Y     
                 STA $03
                 LDA SCREENRAMROWSTARTHIGH,Y     
                 STA $04 
                 LDY STARSCURRENTX              
                 LDA STARSCURRENTFRAME         
                 STA ($03),Y 

SKIPUPDATE       INX                            ; LOOP FOR EACH STAR
                 CPX #$28
                 BEQ FINISHED                  
                 JMP LOOP                      

FINISHED         
                 ASL $D019
                 JMP $EA31                             

                 
;-----------------DRAW THE ARENA SCREEN ROUTINE  ------------------------------------
DRAWSCREEN
                 LDA #$00
                 STA $D020 
                 STA $D021 

                 LDA #$20
                 LDX #$FA
CLEANSCREEN      DEX 
                 STA $0400,X 
                 STA $04FA,X 
                 STA $05F4,X 
                 STA $06EE,X               
                 BNE CLEANSCREEN

                 LDA #$01
                 LDX #$FA
CLEANCOLOR       DEX 
                 STA $D800,X 
                 STA $D8FA,X 
                 STA $D9F4,X 
                 STA $DAEE,X 
                 BNE CLEANCOLOR

                 LDX #$00
TEXTLOOP         
                 LDA ARENATEXT,X
                 STA $0400,X
                 LDA ARENATEXTCOLOR,X
                 STA $D800,X
                 INX
                 CPX #40
                 BNE TEXTLOOP
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
        AND #$07         
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
        LDA #20
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
         JSR MAINSHIPBEEP
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
        JSR ENEMYBEEP           ; PLAY THE SHOOTING SOUND
        RTS

ENBULMOVE
         LDY SPRITEPOS+$07      ; MOVE THE BULLET
         INY                    ; DECREMENT Y VAL 
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
          LDA ENSHIPDIR
          CMP #$00
          BNE R2L

          LDX BYTECOUNT 
          LDA SINEX0,X      ;SINE MOVEMENT X POSITION
          STA SPRITEPOS+$02     
          LDA SINEY,X      ; SINE MOVEMENT Y POSITION
          STA SPRITEPOS+$03
          INC BYTECOUNT
          RTS

R2L       LDX BYTECOUNT 
          LDA SINEX1,X      ;SINE MOVEMENT X POSITION
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
        JSR ENEMYBEEP           ; PLAY THE SHOOTING SOUND
        RTS

ENBULMOVE2
         LDY SPRITEPOS+$0A      ; LOAD THE BULLET 2 X POSITION
         INY                    ; INCREMENT X POSITION 
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
          JSR WRITESCORE        ; GOTO WRITE SCORE SUBROUTINE

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
          STA BYTECOUNT
          STA SPRITEPOS+$04     ; RESET SPRITE POSITIONS
          STA SPRITEPOS+$05
          STA SPRITEPOS+$06
          STA SPRITEPOS+$07
          STA SPRITEPOS+$0A
          STA SPRITEPOS+$0B  

          LDA ENSHIPDIR         ; REVERSE ENEMYSHIP DIRECTION
          EOR #$01
          STA ENSHIPDIR

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
        JSR CHECKHISCORE
        JSR WRITEHISCORE

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
        CPX #40
        BNE MSH          

        LDX #0
MSHC    LDA #1
        STA SCREEN5COL,X
        INX
        CPX #40
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
        CPX #40
        BNE ESH          

        LDX #0
ESHC    LDA #1
        STA SCREEN6COL,X
        INX
        CPX #40
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

         LDX #$00
EXPLPLR  LDA EXPLTBL,X
         STA $07FE
         JSR DELAYANIM
         INX
         CPX #12
         BNE EXPLPLR
         RTS

DELAYANIM   

          LDY #8 
WAIT1              

WAITVB    BIT $D011
          BPL WAITVB

WAITVB2   BIT $D011
          BMI WAITVB2

          DEY 
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

;----------------------CHECK HI-SCORE------------------------------
CHECKHISCORE

            LDA HISCORE+1    
            CMP SCORE+1
            BCc YES
            LDA HISCORE    
            CMP SCORE
            BCC YES
            RTS

YES               
            LDA SCORE
            STA HISCORE    
            LDA SCORE+1
            STA HISCORE+1
            RTS    

;--------------PLOT HI-SCORE-----------------------------
WRITEHISCORE
          LDY #3        ; SCREEN OFFSET
          LDX #0        ; SCORE BYTE INDEX
HILOOP   
          LDA HISCORE,X
          PHA
          AND #$0F      ; 00001111
          JSR PLOTHI 
          PLA
          LSR A
          LSR A
          LSR A 
          LSR A
          JSR PLOTHI
          INX
          CPX #2
          BNE HILOOP
          RTS

PLOTHI
          CLC
          ADC #48       
          STA HISCREEN,Y 
          DEY
          RTS

;---------------------SCORE HANDLING---------------------------------
WRITESCORE              ; WE HAVE 4 DIGITS SCORE
          SED           ; WE ARE SETTING DECIMAL FLAG
          CLC
          LDA SCORE
          ADC #10
          STA SCORE
          LDA SCORE+1
          ADC #$00
          STA SCORE+1
          CLD
         
          LDY #3        ; SCREEN OFFSET
          LDX #0        ; SCORE BYTE INDEX
SLOOP   
          LDA SCORE,X
          PHA
          AND #$0F      ; 00001111
          JSR PLOTDIGIT 
          PLA
          LSR A
          LSR A
          LSR A 
          LSR A
          JSR PLOTDIGIT
          INX
          CPX #2
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

;----------------------SOUND EFFECTS----------------------------
MAINSHIPBEEP    JSR SOUNDEND1
                LDA #53
                STA ATTDEC
                LDA #0
                STA SUSREL
                LDA #15
                STA VOLUME        
                LDA #143
                STA HIFREQ
                LDA #10
                STA LOFREQ
                LDA #32
                STA WAVEFM
                JSR SOUNDGO1
                RTS

ENEMYBEEP       JSR SOUNDEND2
                LDA #53
                STA ATTDEC
                LDA #0
                STA SUSREL
                LDA #15
                STA VOLUME        
                LDA #17
                STA HIFREQ
                LDA #8
                STA LOFREQ
                LDA #32 
                STA WAVEFM
                JSR SOUNDGO2
                RTS


EXPLBEEP        JSR SOUNDEND3
                LDA #25
                STA ATTDEC
                LDA #25
                STA SUSREL
                LDA #15
                STA VOLUME        
                LDA #1
                STA HIFREQ
                LDA #16
                STA LOFREQ
                LDA #128 
                STA WAVEFM
                JSR SOUNDGO3
                RTS


SOUNDGO1        LDA ATTDEC
                STA $D405
                LDA SUSREL
                STA $D406       
                LDA VOLUME
                STA $D418
                LDA HIFREQ
                STA $D400
                LDA LOFREQ
                STA $D401
                LDX WAVEFM
                INX
                TXA
                STA $D404
                RTS
                
SOUNDGO2        LDA ATTDEC
                STA $D40C
                LDA SUSREL
                STA $D40D       
                LDA VOLUME
                STA $D418
                LDA HIFREQ
                STA $D407
                LDA LOFREQ
                STA $D408
                LDX WAVEFM
                INX
                TXA
                STA $D40B
                RTS


SOUNDGO3        LDA ATTDEC
                STA $D413
                LDA SUSREL
                STA $D414       
                LDA VOLUME
                STA $D418
                LDA HIFREQ
                STA $D40E
                LDA LOFREQ
                STA $D40F
                LDX WAVEFM
                INX
                TXA
                STA $D412
                RTS


SOUNDEND1       LDA #0
                STA $D404       ; WF1
                RTS

SOUNDEND2       LDA #0
                STA $D40B       ; WF2
                RTS

SOUNDEND3       LDA #0
                STA $D412       ; WF3
                RTS

;--------------------------PLAY SOUND -------------------
PLAYSOUND

    SEI
    LDA #<IRQ
    LDX #>IRQ
    STA $314
    STX $315
    LDY #$7F 
    STY $DC0D
    LDA #$01
    STA $D01A
    STA $D019 
    LDA SOUND
    JSR $3000
    CLI
    RTS
 
IRQ
    INC $D019
    JSR $3003
    JMP $EA31

;-------------------------------VARIABLES AND TABLES----------------------------------
SCREEN  = $0421         ; SCORE SCREEN LOCATION
HISCREEN = $040A        ; HI-SCORE SCREEN LOCATION
SCREEN2 = $0417         ; LIVES SCREEN LOCATION
SCREEN3 = $05EF         ; GAME OVER TEXT 1 SCREEN LOCATION         
SCREEN4 = $0639         ; GAME OVER TEXT 2 SCREEN LOCATION
SCREEN3COL = $D9EF      ; GAME OVER TEXT 1 COLOR LOCATION 
SCREEN4COL = $DA39      ; GAME OVER TEXT 2 COLOR LOCATION 
SCREEN5 = $07C0         ; YOU ARE DOWN MESSAGE SCREEN LOCATION
SCREEN6 = $07C0         ; ENEMY SHIP DOWN MESSAGE SCREEN LOCATION
SCREEN5COL = $DBC0      ; YOU ARE DOWN MESSAGE COLOR LOCATION
SCREEN6COL = $DBC0      ; ENEMY SHIP DOWN MESSAGE COLOR LOCATION
SCRDATA = 32576         ; BITMAP TITLE DATA - $7F40
COLDATA = 33576         ; BITMAP TITLE COLOR DATA - $8328     
SCRRAM = 23552          ; BITMAP TITLE SCREEN LOCATION - $5C00
COLRAM = 55296          ; BITMAP TITLE COLOR LOCATION - $D800 
COUNTER    = $02
SPRITEPOS  = $0370              ; POSITIONS FOR THE SPRITES
FIRELOCKUP = $0330              ; VARIABLE FOR LOCKING UP THE FIRE BUTTON
NOMOVEBULL      BYTE 0          ; MAINSHIP BULLET MOVEMENT VARIABLE
BYTECOUNT       BYTE $00        ; ENEMY SHIP POS COUNTER FOR THE SINE TABLE
SCORE           BYTE 0, 0       ; SCORE VARIABLE FOR 4 DIGITS
HISCORE         BYTE 0, 0       ; HI-SCORE VARIABLE FOR 4 DIGITS
ENSHIPDIR       BYTE 0          ; 0= LEFT2RIGHT, 1=RIGHT2LEFT
ENEMYBULLET     BYTE $00        ; ENEMY BULLET VARIABLE 0=NO BULLET ON SCREEN
ENEMYBULLET2    BYTE $00        ; ENEMY BULLET 2 VARIABLE 0=NO BULLET ON SCREEN
ENEMY2DIR       BYTE $00
LIVES           BYTE $00        ; NUMBER OF LIVES
KEY             BYTE 0          ; GET CHAR. OF KEY PRESSED

ATTDEC  BYTE 0
SUSREL  BYTE 0
VOLUME  BYTE 0
HIFREQ  BYTE 0
LOFREQ  BYTE 0
WAVEFM  BYTE 0
SOUND           BYTE 00         ; SOUND SELECTION BYTE
;MUSIC_IN_GAME_TUNE               = $00
;MUSIC_TITLE_TUNE                 = $01
;MUSIC_GET_READY_GAME_OVER_TUNE   = $02
;MUSIC_GAME_END_TUNE              = $03
;MUSIC_PLAYER_SHOOT               = $04
;MUSIC_PLAYER_DIE                 = $05
;MUSIC_PICKUP                     = $06
;MUSIC_ENEMY_DIE                  = $07          

*=$2800                 ; NEW FONT
incbin "newfont4.bin"
   
*=$3000                 ; MUSIC AND SOUND FX FILE
incbin "music.bin",2

*=$6000                 ; BITMAP KOALA IMAGE
incbin "pixel_space61.prg",2


;---------------STARFILED VARIABLES AND TABLES -------------------------------------
STARSSPEEDCOLARRAY      BYTE  4,2,4,3,4,3,4,3,4,3
                        BYTE  1,2,4,2,4,2,3,4,2,3
                        BYTE  2,3,4,3,4,1,4,3,1,3
                        BYTE  4,3,4,1,4,2,4,2,3,2

STARSYCHARARRAY         BYTE 15,6,17,1,18,2,4,14,12,5
                        BYTE 13,3,9,7,10,21,5,13,10,23
                        BYTE 11,5,15,1,5,9,7,18,11,2
                        BYTE 12,16,21,9,2,5,16,8,15,2

SCREENRAMROWSTARTLOW    BYTE $00,$28,$50,$78,$A0,$C8,$F0,$18
                        BYTE $40,$68,$90,$B8,$E0,$08,$30,$58      
                        BYTE $80,$A8,$D0,$F8,$20,$48,$70,$98,$C0

SCREENRAMROWSTARTHIGH   BYTE $04,$04,$04,$04,$04,$04,$04,$05
                        BYTE $05,$05,$05,$05,$05,$06,$06,$06
                        BYTE $06,$06,$06,$06,$07,$07,$07,$07,$07      

COLORRAMROWSTARTLOW     BYTE $00,$28,$50,$78,$A0,$C8,$F0,$18
                        BYTE $40,$68,$90,$B8,$E0,$08,$30,$58
                        BYTE $80,$A8,$D0,$F8,$20,$48,$70,$98,$C0


COLORRAMROWSTARTHIGH    BYTE $D8,$D8,$D8,$D8,$D8,$D8,$D8,$D9
                        BYTE $D9,$D9,$D9,$D9,$D9,$DA,$DA,$DA
                        BYTE $DA,$DA,$DA,$DA,$DB,$DB,$DB,$DB,$DB

STARSDELAYARRAY         DCB 40, 1       ; NUM OF COLUMNS AND DELAY
STARSFRAMEARRAY         DCB 40, 86      ; NUM OF COLUMNS AND CHARACTER

STARSCURRENTX           BYTE 0
STARSCURRENTY           BYTE 0
STARSCURRENTFRAME       BYTE 0
STARSCURRENTCOLOR       BYTE 0
STARSNEXTSPEED          BYTE 1

;--------------------TEXT ON SCREENS-------------------------------
ENDTEXT1        BYTE 7,1,13,5,32,15,22,5,18
ENDTEXT2        BYTE 8,9,20,32,19,16,1,3,5,32
                BYTE 20,15,32,3,15,14,20,9,14,21,5

MAINSHIPHIT     TEXT '    you are hit - be careful please     '
ENEMYSHIPHIT    TEXT '   you hit the enemy ship, good work    '
;-----------------------------------------------------------------

EXPLTBL  BYTE $86,$87,$88,$89,$8A,$8B            ; EXPLOSION TABLE THAT STORES
         BYTE $8C,$8D,$8E,$8F,$90,$91            ; THE POINTER TO THE EXPLOSION SPRITE

SCROLLTEXT      
                TEXT 'a-space-game    design and programming :'   
                TEXT 'metesev 2021    hit space to continue...'

ARENATEXT       TEXT ' hi-score:0000   lives:3   score:0000   '
ARENATEXTCOLOR  BYTE 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
                BYTE 15,1,1,1,1,1,1,1,1,1,1,13,13,13,13,13,13,13,13
                BYTE 13,13,13,13,13

; SPRITE INITIAL POSITIONS
POSTABLE BYTE $56,$D0,$55,$3C,$00,$00,$00,$00,$0e,$96,$00,$00,$00,$00

;ENEMYSHIP MOVEMENT COORDINATES (SINE TABLES)
SINEX0  ; X POSITION 1              
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

SINEX1 ; X POSITION 2     
        BYTE 91,93,95,96,98,99,101,102,104,106,107,109,110,112,113,115
        BYTE 116,118,119,121,122,123,125,126,127,129,130,131,132,134,135,136
        BYTE 137,138,139,140,141,142,143,144,145,146,147,148,148,149,150,150
        BYTE 151,152,152,153,153,153,154,154,154,155,155,155,155,155,155,155
        BYTE 155,155,155,155,155,155,155,154,154,154,153,153,152,152,151,151
        BYTE 150,149,149,148,147,146,146,145,144,143,142,141,140,139,138,137
        BYTE 135,134,133,132,131,129,128,127,125,124,123,121,120,118,117,115
        BYTE 114,112,111,109,108,106,105,103,102,100,98,97,95,94,92,91
        BYTE 91,89,87,86,84,83,81,79,78,76,75,73,72,70,69,67
        BYTE 66,64,63,61,60,58,57,56,54,53,52,50,49,48,47,46
        BYTE 44,43,42,41,40,39,38,37,36,35,35,34,33,32,32,31
        BYTE 30,30,29,29,28,28,27,27,27,26,26,26,26,26,26,26
        BYTE 26,26,26,26,26,26,26,27,27,27,28,28,28,29,29,30
        BYTE 31,31,32,33,33,34,35,36,37,38,39,40,41,42,43,44
        BYTE 45,46,47,49,50,51,52,54,55,56,58,59,60,62,63,65
        BYTE 66,68,69,71,72,74,75,77,79,80,82,83,85,86,88,90

SINEY   ; Y POSITION (COMMON)
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
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$82,$00,$00,$82,$00,$00
        BYTE $C3,$00,$00,$C3,$00,$00,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #3 --> ENEMY SHIP BULLET--> POINTER $83
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$C3,$00,$00,$C3,$00,$00,$C3,$00,$00,$82,$00,$00,$82,$00,$00
        BYTE $82,$00,$00,$41,$00,$00,$41,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0        

; SPRITE #4 --> ENEMY 2 SHIP --> POINTER $84
        BYTE $00,$00,$00,$00,$15,$40,$00,$55,$00,$01,$54,$00,$01,$60,$00,$05,$A0,$00,$05,$A0
        BYTE $00,$15,$A0,$00,$D6,$BF,$00,$FA,$BF,$C0,$FA,$BF,$F0,$FA,$BF,$C0,$D6,$BF,$00,$15
        BYTE $A0,$00,$05,$A0,$00,$05,$A0,$00,$01,$60,$00,$01,$54,$00,$00,$55,$00,$00,$15,$40
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #5 --> ENEMY 2 SHIP BULLET --> POINTER $85
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$D6,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $D6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0        

; SPRITE #6 --> EXPLOSION 1  --> POINTER $86
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$80,$01,$02,$00,$01,$02,$00,$02,$6A
        BYTE $40,$00,$6A,$40,$00,$A9,$40,$00,$2A,$40,$00,$2A,$80,$00,$AA,$80,$00,$AA,$80,$02
        BYTE $9A,$A0,$02,$6A,$50,$02,$80,$A0,$02,$00,$A0,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 2 --> POINTER $87
        BYTE $00,$00,$00,$08,$00,$A0,$08,$00,$40,$0A,$20,$40,$01,$A5,$40,$01,$AA,$80,$02,$AA
        BYTE $80,$2A,$AA,$80,$09,$AA,$40,$01,$A9,$40,$01,$69,$40,$01,$6A,$60,$01,$6A,$60,$01
        BYTE $6A,$A0,$02,$AA,$80,$02,$AA,$60,$0A,$55,$60,$08,$99,$60,$28,$00,$08,$20,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 3 --> POINTER $88
        BYTE $20,$02,$80,$28,$0A,$80,$08,$9A,$0A,$09,$95,$68,$01,$A9,$50,$09,$AA,$90,$A9,$AA
        BYTE $90,$A6,$AA,$A0,$05,$6A,$A0,$29,$6A,$A0,$09,$AA,$A0,$29,$AA,$90,$29,$AA,$90,$06
        BYTE $AA,$98,$06,$AA,$98,$06,$96,$90,$06,$65,$50,$04,$25,$60,$28,$25,$28,$A0,$28,$08
        BYTE $80,$20,$0A
        BYTE 0

; SPRITE #6 --> EXPLOSION 4 --> POINTER $89
        BYTE $28,$02,$80,$2A,$9A,$80,$09,$5A,$80,$0A,$A5,$60,$26,$AA,$50,$26,$AA,$90,$AA,$AA
        BYTE $A0,$5A,$AA,$A0,$5A,$6A,$A8,$96,$6A,$9A,$26,$5A,$96,$06,$AA,$A6,$06,$AA,$A8,$06
        BYTE $AA,$A8,$05,$AA,$58,$05,$6A,$50,$05,$5A,$50,$2A,$9A,$A8,$18,$18,$28,$A0,$18,$28
        BYTE $80,$20,$0A
        BYTE 0

; SPRITE #6 --> EXPLOSION 5 --> POINTER $8A
        BYTE $80,$08,$02,$A2,$28,$02,$29,$6A,$8A,$09,$96,$AA,$0A,$A6,$AA,$AA,$A5,$A6,$56,$A9
        BYTE $A4,$56,$69,$A4,$69,$65,$56,$A9,$6A,$A6,$29,$6A,$A6,$09,$9A,$A6,$0A,$96,$A8,$0A
        BYTE $96,$A8,$0A,$AA,$A0,$29,$56,$50,$25,$66,$50,$29,$A5,$68,$A0,$2A,$0A,$A0,$2A,$02
        BYTE $80,$28,$02
        BYTE 0

; SPRITE #6 --> EXPLOSION 6 --> POINTER $8B
        BYTE $02,$A8,$02,$06,$96,$0A,$05,$6D,$B8,$07,$EE,$78,$0B,$EB,$A0,$AA,$6B,$A0,$96,$A7
        BYTE $E0,$A6,$95,$F8,$25,$A6,$B8,$09,$E6,$96,$09,$F5,$AA,$09,$B9,$9A,$09,$BE,$9A,$0A
        BYTE $AA,$98,$09,$DA,$78,$09,$F6,$F8,$2A,$9A,$B8,$28,$14,$94,$28,$28,$A4,$00,$20,$2A
        BYTE $00,$20,$2A
        BYTE 0

; SPRITE #6 --> EXPLOSION 7 --> POINTER $8C
        BYTE $00,$00,$00,$03,$68,$00,$02,$58,$94,$00,$A6,$58,$0A,$F6,$60,$0F,$65,$80,$1F,$55
        BYTE $C0,$16,$A5,$C0,$25,$5E,$E0,$09,$7F,$50,$09,$B7,$90,$0D,$7E,$50,$0F,$7D,$60,$0F
        BYTE $5E,$60,$0F,$69,$40,$07,$96,$80,$05,$9B,$E0,$26,$20,$E0,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 8 --> POINTER $8D
        BYTE $00,$00,$00,$00,$50,$00,$00,$75,$50,$00,$FD,$C0,$01,$5F,$C0,$01,$57,$40,$03,$75
        BYTE $50,$07,$7D,$50,$15,$7D,$F0,$3F,$7D,$74,$1D,$FD,$54,$0D,$5F,$50,$07,$F5,$F0,$05
        BYTE $F5,$D0,$15,$DF,$D0,$01,$FF,$50,$01,$50,$40,$01,$00,$50,$00,$00,$10,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 9 --> POINTER $8E
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$40,$00,$07,$C0,$01,$57
        BYTE $C0,$01,$5F,$40,$03,$D7,$50,$05,$F7,$D0,$05,$FF,$C0,$01,$FD,$40,$01,$75,$40,$05
        BYTE $5F,$40,$07,$FF,$40,$05,$7F,$C0,$01,$F5,$40,$00,$34,$00,$00,$10,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 10 --> POINTER $8F
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1C
        BYTE $00,$00,$14,$00,$00,$55,$00,$01,$DD,$00,$01,$DD,$00,$03,$DD,$00,$01,$5F,$00,$01
        BYTE $57,$00,$00,$F7,$00,$00,$75,$00,$00,$51,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 11 --> POINTER $90
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$11,$00,$00,$15,$00,$00,$55,$00,$00,$55,$00,$00,$55,$40,$00
        BYTE $55,$40,$00,$55,$40,$00,$15,$00,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SPRITE #6 --> EXPLOSION 12 --> POINTER $91
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$54,$00,$00,$14,$00,$00,$55,$00,$00,$54,$00,$00
        BYTE $14,$00,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; SCREEN 1 -  WELCOME SCREEN DATA
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

; SCREEN 1 -  WELCOME SCREEN COLOR
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


