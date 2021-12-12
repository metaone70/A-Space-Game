; 10 SYS (2064)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

*=$0810

START
        SEI
        LDA #<BEGINNING ;POINT SUN/STOP RESTORE TO BEGINNING
        STA $0328
        LDA #>BEGINNING
        STA $0329
        CLI

        LDA #$00        ;SET BORDER AND BACKGROUND COLOR TO BLACK
        STA $D020
        STA $D021

        LDA #03         ; SET LIVES TO 3
        STA LIVES
        JSR PLAYMUSIC   ; PLAY INTRO MUSIC

BEGINNING
        JSR TITLESCREEN         ;DRAW THE BITMAP TITLE SCREEN
        JSR DRAWTEXTSCREEN         ;DRAW THE ENTER THE ARENA SCREEN
        JSR DRAWSCREEN          ;DRAW THE CHARACTER (BATTLE) SCREEN
        JSR SETUPSPRITES        ;INITIALIZE SPRITES
        JSR STOPMUSIC           ;STOP THE MUSIC, SINCE THE BATTLE IS ABOUT TO BEGIN

;---------------------MAIN GAME LOOP-----------------------------------------       
MAINLOOP  
          JSR WAITRASTER        ; WATT FOR RASTERLINE TO SLOW DOWN
          JSR MAINSHIP          ; WAIT FOR JOYSTICK TO COMMAND AND MOVE MAIN SHIP
          JSR ENEMYSHIP         ; MOVE ENEMY SHIP ACCORDING TO THE SINE TABLE
          JSR ENEMYFIRE         ; ENEMYSHIP FIRE CHECK AND CONTROL
          JSR FIRE              ; MAINSHIP BULLET CHECK AND CONTROL
          JSR COLLISIONCHECK    ; CHECK IF ANY COLLISION OCCURS
          JMP MAINLOOP          ; CONTINUE THE LOOP

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
        LDA #$07        ;-> SPRITE 4 (ENEMY BULLET) COLOR = YELLOW 
        STA $D02A
        LDA #$07        ;-> SPRITE 5 (EXPLOSION) COLOR = YELLOW
        STA $D02B

        ; POSITIONING SPRITES
        LDA SPR0POS
        STA $D000       ; #0. MAINSHIP SPRITE X  --> $AC
        LDA SPR0POS+1
        STA $D001       ; #0. SPRITE Y  --> $D0
        LDA SPR2POS
        STA $D004       ; #2. MAIN BULLET SPRITE X --> $00
        LDA SPR2POS+1
        STA $D005       ; #2. SPRITE Y  -->  $00
        LDA SPR3POS
        STA $D006       ; #3. ENEMY BULLET SPRITE X --> $00
        LDA SPR3POS+1
        STA $D007       ; #3. SPRITE Y  -->  $00
        LDA SPR4POS
        STA $D008       ; #4. EXPLOSION SPRITE X --> $00
        LDA SPR4POS+1
        STA $D009       ; #4. SPRITE Y --> $00

        ; SET MULTICOLOR BITS
        LDA #%00010111        ; SPRITES 0,2,4 ARE MULTICOLOR
        STA $D01C

        ; SET SCREEN-SPRITE PRIORITY FLAGS
        LDA #$00
        STA $D01B

        ; SET SPRITE POINTERS
        LDA #$80        ; MAIN SHIP --> $2800 -> $5000
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
          LDA $DC00               ; JOYSTICK CONTROL REGISTER

UP        LSR A                 ; UP CHECK
          BCS DOWN              ; BRANCH ON CARRY SET
          LDY SPR0POS+1         ; CHECK Y POSITION OF MAIN SHIP
          CPY #150              ; COMPARE TO POS Y=150
          BEQ DOWN              ; IF EQUAL TO 150, DON'T GO UP, JUMP TO DOWN CONTROL
          DEC SPR0POS+1         ; OTHERWISE DEC Y POS & GO UP

DOWN      LSR A                 ; DOWN CHECK
          BCS LEFT
          LDY SPR0POS+1
          CPY #$D0              ; CHECK IF IT IS DOWN TO 208
          BEQ LEFT
          INC SPR0POS+1

LEFT      LSR A                
          BCS RIGHT            
          LDY SPR0POS           ; CHECK X POSITION OF MAIN SHIP
          CPY #30               ; COMPARE TO POS X=24
          BEQ RIGHT  
          DEC SPR0POS                 
          
RIGHT     LSR A                  
          BCS BUTTON    
          LDY SPR0POS           ; CHECK X POSITION OF MAIN SHIP
          CPY #245              ; COMPARE TO POS X=250
          BEQ BUTTON
          INC SPR0POS             
          
BUTTON    LSR A                  ;BUTTON CHECK
          BCS CHECKBOUNCE   
          LDA #1        
          STA DEBOUNCE  
          JMP FINALIZE
          
CHECKBOUNCE
          LDA DEBOUNCE  
          BEQ FINALIZE
          LDA #00
          STA DEBOUNCE
          INC BUTTONDOWN
                  
FINALIZE                       ; PUT MAINSHIP INTO POSITION
          LDA SPR0POS      
          STA $D000            
          LDA SPR0POS+1
          STA $D001
          RTS  
 
;-----------------MAIN SHIP BULLET HANDLING-------------------------------          
FIRE
        LDA BULLET          ; DO WE BULLET ON SCREEN?
        BNE MOVE           ; IF YES, GO TO THE MOVE ROTINE
        LDA BUTTONDOWN     ; IF NOT, CHECK IF THE BUTTON IS PRESSED
        BNE CREATEBULLET   ; IF PRESSED, GO AND CREATE A BULLET
        RTS                ; IF NOT, RETURN
MOVE                  
        LDY SPR2POS+1     ; LOAD BULLET'S Y POS
        CPY #50           ; COMPARE IT TO LINE 50 (TOP OF SCREEN)
        BEQ DISAPPEAR    ; IF YES, GO TO "HIDE THE BULLET" ROUTINE
        DEC SPR2POS+1     ; IF NOT, DEC Y POS AND MOVE UP
        JSR DRAWBULLET
        RTS
          
CREATEBULLET
        LDA SPR0POS       ; LOAD THE SHIP X POSITION
        CLC
        ADC #1            ; ADD 1 TO CENTER THE BULLET
        STA SPR2POS       ; STORE IT TO BULLET SPRITE X POS
        LDA SPR0POS+1     ; LOAD THE SHIP'S Y POS
        SBC #18           ; SUBTRACT 18 TO BRING TO THE SHIP'S FRONT
        STA SPR2POS+1     ; STORE IT IN BULLET'S Y POS
        LDA #$01
        STA BULLET        ; INC BULLET VAR, SO NOW WE HAVE A BULLET ON SCREEN
        DEC BUTTONDOWN    ; RESET THE BUTTON STATUS, IT IS OFF
        LDA #04           ; LOAD SOUND NO 4 TO VAR
        STA SOUND        
        JSR PLAYSOUND   
        JSR DRAWBULLET
        RTS
          
DISAPPEAR                 ; SINCE IT REACHED Y=50, LET'S PUT IT OUT OF SCREEN
        LDA #$00          ; MAKE THE X AND Y POS OF BULLET SPRITE
        STA SPR2POS       ; TO ZERO, SO WE DO NOT SEE IT ON SCREEN
        STA SPR2POS+1 
        LDA #$00
        STA BULLET        ; WE DEC (MAKE ZERO) BULLET VAR, SO NO BULLET ON SCREEN
        JSR DRAWBULLET        
        RTS

DRAWBULLET
          LDA SPR2POS           ; DRAW THE MAINSHIP BULLET
          STA $D004     
          LDA SPR2POS+1
          STA $D005
          RTS

;-------------------------ENEMYSHIP MOVEMENT-------------------------------------
ENEMYSHIP 
          LDX BYTECOUNT 
          LDA SINEX,X      ;SINE MOVEMENT X POSITION
          STA SPR1POS     
          STA $D002
          LDA SINEY,X      ; SINE MOVEMENT Y POSITION
          STA SPR1POS+1
          STA $D003
          INC BYTECOUNT
          RTS
;--------------COLLIDED 3 - BULLETS COLLIDE----------------
COLLIDED3
          LDA #$00
          STA SPR2POS
          STA SPR3POS      
          STA $D004
          STA $D006
          STA $D01E
          STA BULLET
          STA ENEMYBULLET
          LDA #4
          JSR PLAYSOUND
          RTS
          
;----------------COLLISON CHECK SUBROUTINE------------------------------------
COLLISIONCHECK
          LDA $D01E
          CMP #%00000110        ; CHECK IF MAIN SHIP BULLET AND ENEMY SHIP COLLIDED
          BEQ COLLIDED
          CMP #%00001001       ; CHECK IF ENEMY SHIP BULLET AND MAIN SHIP COLLIDED
          BEQ COLLIDED2
          CMP #%00001100
          BEQ COLLIDED3
          RTS

;----------------MAIN SHIP BULLET AND ENEMY SHIP COLLIDED--------------------------
COLLIDED
          LDA SPR1POS     ; TRANSFER CURRENT ENEMY SHIP POS TO COLLISIN COORDINATES
          STA SPR4POS
          LDA SPR1POS+1
          STA SPR4POS+1
   
          LDA #%00010001        ; JUST SPRITE 0 (MAINSHIP) AND 
          STA $D015             ; SPRITE 4 (COLLISION SPRITE) IS ON
          LDA #7
          STA SOUND
          JSR PLAYSOUND

          JSR ENEMYHIT         
          JSR COLLANIM 
          JSR ENEMYHITCLEAR
          
          ; RETURN BACK TO ORIGINAL POSITIONS EXCEPT MAIN SHIP
          LDA #%00001111
          STA $D015              ; FIRST 4 SPRITES ARE ON (AGAIN)        
          
          LDA #$00               ; RESET COLLISION REGISTER
          STA $D01E      
          
          LDA #$00              
          STA SPR2POS
          STA SPR2POS+1          
          STA SPR3POS
          STA SPR3POS+1 
          STA SPR4POS             ;RESET EXPLOSON SPRITE POSITIONS
          STA SPR4POS+1
          STA $D004
          STA $D005
          STA $D006
          STA $D007
          STA $D008
          LDA $D009
          
          LDX #$00 
          STX BYTECOUNT
          LDA SINEX,X
          STA SPR1POS     
          STA $D002
          LDA SINEY,X
          STA SPR1POS+1
          STA $D003
          INC BYTECOUNT 
          
          JSR WRITESCORE        ; JUMP TO WRITE SUBROUTINE
          JMP MAINLOOP          ; RETURN BACK TO THE MAINLOOP

;-------------------ENENMY SHIP BULLET AND MAINSHIP COLLIDED------------------------
COLLIDED2
          LDA SPR0POS     ; TRANSFER CURRENT MAIN SHIP POS TO COLLISIN COORDINATES
          STA SPR4POS
          LDA SPR0POS+1
          STA SPR4POS+1
          
          LDA #$00
          STA $D000       ; PLACE THE MAIN SHIP, MAIN BULLET AND THE ENEMY BULLET OUT OF SIGHT
          STA $D001   
          STA $D004
          STA $D005
          STA $D006
          STA $D007

          LDA #%00010010        ; JUST SPRITE 1 (ENEMY SHIP) AND
          STA $D015             ; SPRITE 4 (COLLISION SPRITE) IS ON

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
          
          LDA #$AC              ; RESET SPRITE POSITIONS
          STA SPR0POS
          LDA #$D0
          STA SPR0POS+1
          LDA #$00              
          STA SPR2POS
          STA SPR2POS+1          
          STA SPR3POS
          STA SPR3POS+1 
          STA SPR4POS            
          STA SPR4POS+1
          STA $D002           
          STA $D003   
          STA $D004
          STA $D005
          STA $D006
          STA $D007
          STA $D008
          LDA $D009          
          RTS
          
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
      
        LDA #$00
        STA DEBOUNCE
        STA BULLET
        STA BUTTONDOWN
        STA BYTECOUNT
        STA SCORE
        STA SCORE+1
        STA SCORE+2
        STA ENEMYBULLET
        LDA #3
        STA LIVES
        JSR UPDATELIVES        

        LDA #$AC              ; RESET SPRITE POSITIONS
        STA SPR0POS
        LDA #$D0
        STA SPR0POS+1
        LDA #$00             
        STA SPR2POS
        STA SPR2POS+1          
        STA SPR3POS
        STA SPR3POS+1 
        STA SPR4POS            
        STA SPR4POS+1

        LDX #0
CLEAN   STA $D002,X        ; RESET TO ZERO THROGUH $D002 TO $D009   
        INX
        CPX #8
        BNE CLEAN

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
          
DRAWCOL   LDA SPR4POS
          STA $D008       ; DRAW CURRENT EXPLOISON SPRITE
          LDA SPR4POS+1
          STA $D009  
          INC $D020
          JSR DELAY
          RTS
                     
DELAY
WAIT1    LDA #$FA
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
       
;------------------ENEMY SHIP BULLET --------------------------------------
ENEMYFIRE
        LDA ENEMYBULLET
        CMP #$00                 ; DO WE BULLET ON SCREEN?
        BNE ENEMYBULLETMOVE      ; IF YES, GO TO THE MOVE ROTINE
        LDA SPR1POS             ; LOAD MAINSHIP X POS
        CMP SPR0POS             ; COMPARE IT WITH THE ENEMY SHIP X POS
        BCC CREATEENEMYBULLET   ; IF EQUAL, GO AND CREATE AN ENEMY BULLET
        RTS                     ; OTHERWISE RETURN AND WAIT FOR THE SAME X POS

ENEMYBULLETMOVE                  
        LDY SPR3POS+1                    ; LOAD BULLET'S Y POS
        INY
        ;INY
        CPY #229                         ; COMPARE IT TO LINE 229 (BOTTOM OF SCREEN)
        BEQ ENBULDISAPPEAR               ; IF YES, GO TO "HIDE THE BULLET" ROUTINE
                                        ; IF NOT, INC Y POS AND MOVE DOWN
        STY SPR3POS+1
        JSR FINALIZENEMYBULLET
        RTS
          
CREATEENEMYBULLET
        LDA SPR1POS             ; LOAD ENEMY SHIP X POSITION
        STA SPR3POS             ; AND STORE IT TO ENEMY BULLET X POSITION
        LDA SPR1POS+1           ; LOAD ENEMY SHIP Y POSITION
        STA SPR3POS+1           ; AND STORE IT TO ENEMY BULLET Y POSITION
        LDA #$01                ; ENEMY BULLET CREATED AND IS NOW ON SCREEN
        INC ENEMYBULLET
        LDA #4
        STA SOUND
        JSR PLAYSOUND
        JSR FINALIZENEMYBULLET        
        RTS
 
ENBULDISAPPEAR              
        LDA #$00                 ; MAKE THE X AND Y POS OF ENEMY BULLET SPRITE
        STA SPR3POS              ; TO ZERO, SO WE DO NOT SEE IT ON SCREEN
        STA SPR3POS+1 
        STA ENEMYBULLET         ; WE DEC (MAKE ZERO) BULLET VAR, SO NO BULLET ON SCREEN
        JMP FINALIZENEMYBULLET
       
FINALIZENEMYBULLET
        LDA SPR3POS
        STA $D006
        LDA SPR3POS+1
        STA $D007
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
SCREEN  = $0741
SCREEN2 = $06A3
SCREEN3 = $05EB  
SCREEN4 = $0635
SCREEN3COL = $D9EB
SCREEN4COL = $DA35
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
   
SPR0POS         BYTE $AC,$D0    ; MAINSHIP POS
SPR1POS         BYTE $5F,$5C    ; ENEMY SHIP POS
SPR2POS         BYTE $00,$00    ; BULLET POS (OUT OF SIGHT)
SPR3POS         BYTE $00,$00    ; ENEMY BULLET (OUT OF SIGHT)
SPR4POS         BYTE $00,$00    ; EXPLOSION POS (OUT OF SIGHT)
DEBOUNCE        BYTE $00        ; JOYSTICK BUTTON RELEASE CHECK
BULLET          BYTE $00        ; BULLET VARIABLE 0=NO BULLET ON SCREEN
BUTTONDOWN      BYTE $00        ; JOYSTICK BUTTON PRESSED VARIABLE 0=NOT PRESSED
BYTECOUNT       BYTE $00        ; ENEMY SHIP POS COUNTER FOR THE SINE TABLE
SCORE           BYTE 0, 0, 0    ; SCORE VARIABLE FOR 6 DIGITS
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

ENDTEXT1  BYTE 7,1,13,5,32,15,22,5,18
ENDTEXT2  BYTE 8,9,20,32,19,16,1,3,5,32
          BYTE 20,15,32,3,15,14,20,9,14,21,5

MAINSHIPHIT     BYTE 25,15,21,32,1,18,5,32,4,15,23,14,32,45,32
                BYTE 2,5,32,3,1,18,5,6,21,12,32,16,12,5,1,19,5

ENEMYSHIPHIT    BYTE 5,14,5,13,25,32,19,8,9,16,32,4,15,23,14,32,45,32 
                BYTE 7,15,15,4,32,23,15,18,11

SINEX          
        BYTE 137,134,132,129,126,123,121,118,115,112,110,107,105,102,99,97
        BYTE 94,92,89,87,84,82,79,77,75,73,70,68,66,64,62,60
        BYTE 58,56,54,52,51,49,47,46,44,43,41,40,38,37,36,35
        BYTE 34,33,32,31,30,29,29,28,28,27,27,26,26,26,26,26
        BYTE 26,26,26,26,26,26,27,27,28,28,29,30,31,31,32,33
        BYTE 34,35,37,38,39,40,42,43,45,46,48,50,51,53,55,57
        BYTE 59,61,63,65,67,69,71,74,76,78,81,83,85,88,90,93
        BYTE 95,98,101,103,106,108,111,114,117,119,122,125,127,130,133,136
        BYTE 138,141,144,147,149,152,155,157,160,163,166,168,171,173,176,179
        BYTE 181,184,186,189,191,193,196,198,200,203,205,207,209,211,213,215
        BYTE 217,219,221,223,224,226,228,229,231,232,234,235,236,237,239,240
        BYTE 241,242,243,243,244,245,246,246,247,247,248,248,248,248,248,248
        BYTE 248,248,248,248,248,247,247,246,246,245,245,244,243,242,241,240
        BYTE 239,238,237,236,234,233,231,230,228,227,225,223,222,220,218,216
        BYTE 214,212,210,208,206,204,201,199,197,195,192,190,187,185,182,180
        BYTE 177,175,172,169,167,164,162,159,156,153,151,148,145,142,140,137

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
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$56,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$55,$40,$40,$40,$40,$40,$40,$40,$49
        BYTE    $20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$42,$20,$01,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$42,$20,$13,$10,$01,$03,$05,$20,$48
        BYTE    $20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$42,$20,$20,$07,$01,$0D,$05,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$56,$20,$5A,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$42,$20,$02,$19,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$42,$20,$20,$0D,$14,$13,$16,$20,$48
        BYTE    $20,$20,$20,$56,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$57,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$42,$20,$32,$30,$32,$31,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$58,$56,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$56,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$57,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$56,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$42,$20,$0C,$09,$16,$05,$13,$20,$48
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$5A,$20,$20,$56,$20,$20,$20,$20,$56,$20,$20,$20,$20,$42,$20,$20,$20,$33,$20,$20,$20,$48
        BYTE    $56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$56,$20,$20,$20,$5A,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$56,$20,$20,$42,$20,$13,$03,$0F,$12,$05,$20,$48
        BYTE    $20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$42,$20,$30,$30,$30,$30,$30,$30,$48
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$5A,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$20,$56,$20,$20,$42,$20,$20,$20,$20,$20,$20,$20,$48
        BYTE    $20,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$20,$56,$20,$20,$20,$20,$20,$20,$56,$20,$20,$56,$20,$20,$20,$20,$20,$4A,$46,$46,$46,$46,$46,$46,$46,$4B
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; SCREEN 1 -  COLOUR DATA
*=$5400
        BYTE    $00,$00,$07,$07,$07,$07,$07,$07,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$0F,$00,$00,$00,$0E,$00,$00,$00,$01,$00,$0E,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$07,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$01,$01,$0F,$0F,$0F,$00,$0F,$01
        BYTE    $00,$00,$0E,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$00,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0E,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$01,$01,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$0F,$01
        BYTE    $00,$0F,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$05,$05,$05,$05,$0F,$01
        BYTE    $00,$01,$01,$00,$00,$0F,$00,$00,$0E,$00,$00,$0F,$00,$0E,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$00,$00,$0E,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$05,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$0F,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$01,$00,$00,$0E,$00,$00,$00,$00,$0E,$00,$00,$01,$01,$01,$01,$05,$05,$01,$05,$05,$0F,$01
        BYTE    $0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$01,$01,$01,$05,$05,$05,$05,$05,$05,$01
        BYTE    $00,$00,$0E,$00,$00,$00,$01,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$0E,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$01,$00,$00,$00,$01,$00,$01,$01,$01,$0D,$0D,$0D,$0D,$0D,$0F,$01
        BYTE    $00,$00,$00,$0F,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$0D,$0D,$0D,$0D,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$01,$01
        BYTE    $00,$0E,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$00,$0F,$0F,$0F,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$0F,$0F,$0F,$09,$0F,$0F,$0F,$0F,$0F,$00,$0

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

