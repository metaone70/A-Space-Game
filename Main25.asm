; 10 SYS (2064)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

*=$0810

start
        seı
        lda #<begınnıng ;poınt sun/stop restore to begınnıng
        sta $0328
        lda #>begınnıng
        sta $0329
        clı

        lda #$00        ;set border and background color to black
        sta $D020
        sta $D021

        lda #03         ; set lıves to 3
        sta lıves
        jsr playmusıc   ; play ıntro musıc

begınnıng
        jsr tıtlescreen         ;draw the bıtmap tıtle screen
        jsr drawtextscreen         ;draw the enter the arena screen
        jsr drawscreen          ;draw the character (battle) screen
        jsr setupsprıtes        ;ınıtıalıze sprıtes
        jsr stopmusıc           ;stop the musıc, sınce the battle ıs about to begın

;---------------------maın game loop-----------------------------------------       
maınloop  
          jsr waıtraster        ; watt for rasterlıne to slow down
          jsr maınshıp          ; waıt for joystıck to command and move maın shıp
          jsr enemyshıp         ; move enemy shıp accordıng to the sıne table
          jsr enemyfıre         ; enemyshıp fıre check and control
          jsr fıre              ; maınshıp bullet check and control
          jsr collısıoncheck    ; check ıf any collısıon occurs
          jmp maınloop          ; contınue the loop

;---------------------settıng up sprıtes----------------------------
setupsprıtes
        ; set sprıte multıcolors
        lda #$07        ;--> yellow
        sta $d025
        lda #$02        ;--> Red
        sta $d026

        ; colorıze sprıtes
        lda #$0C        ;-> sprıte 0 (maınshıp) color = grey 2
        sta $d027
        lda #$0f        ;-> sprıte 1 (enemy) color = lt.grey      
        sta $d028
        lda #$07        ;-> sprıte 2 (bullet) color = yellow
        sta $d029
        lda #$07        ;-> sprıte 4 (enemy bullet) color = yellow 
        sta $d02a
        lda #$07        ;-> sprıte 5 (explosıon) color = yellow
        sta $d02b

        ; posıtıonıng sprıtes
        lda spr0pos
        sta $d000       ; #0. maınshıp sprıte X  --> $ac
        lda spr0pos+1
        sta $d001       ; #0. sprıte Y  --> $d0
        lda spr2pos
        sta $d004       ; #2. maın bullet sprıte X --> $00
        lda spr2pos+1
        sta $d005       ; #2. sprıte Y  -->  $00
        lda spr3pos
        sta $d006       ; #3. enemy bullet sprıte X --> $00
        lda spr3pos+1
        sta $d007       ; #3. sprıte Y  -->  $00
        lda spr4pos
        sta $d008       ; #4. explosıon sprıte X --> $00
        lda spr4pos+1
        sta $d009       ; #4. sprıte Y --> $00

        ; set multıcolor bıts
        lda #%00010111        ; sprıtes 0,2,4 are multıcolor
        sta $d01c

        ; set screen-sprıte prıorıty flags
        lda #$00
        sta $d01b

        ; set sprıte poınters
        lda #$80        ; maın shıp --> $2800 -> $5000
        sta $07F8
        lda #$81        ; enemy shıp
        sta $07F9
        lda #$82        ; maın shıp bullet
        sta $07FA
        lda #$83        ; enemy shıp bullet
        sta $07FB       
        lda #$84        ; explosıon
        sta $07FC        

        ; turn on sprıtes
        lda #%00001111        ; turn on 4 sprıtes - explosıon not ıncluded
        sta $d015
        rts

;-----------------draw enter the arena screen rotuıne  ------------------------------------
drawtextscreen
                lda #$00
                sta $fb
                sta $fd         ; $0400 Screen RAM
                sta $f7

                lda #$48        ; $4800 character data -->  transfer to $0400
                sta $fc

                lda #$04
                sta $fe

                lda #$00        ; $4c00 screen color data --> transfer to $d800
                sta $f9
                lda #$4c
                sta $fa

                lda #$d8        ; $d800 --> color RAM
                sta $f8

                ldx #$00
looptext           
                ldy #$00
ınnerloopy
                lda ($fb),y
                sta ($fd),y
                lda ($f9),y
                sta ($f7),y
                ıny
                bne ınnerloopy

                ınc $fc
                ınc $fe
                ınc $fa
                ınc $f8
                ınx
                cpx #$04
                bne looptext

ınputkey        jsr $ffe4
                beq ınputkey
                sta key
                cmp #$20
                bne ınputkey 
                rts

;-----------------draw screen routıne  ------------------------------------
drawscreen
                lda #$00
                sta $fb
                sta $fd         ; $0400 Screen RAM
                sta $f7

                lda #$50        ; $5000 character data -->  transfer to $0400
                sta $fc

                lda #$04
                sta $fe

                lda #$00        ; $5400 screen color data --> transfer to $d800
                sta $f9
                lda #$54
                sta $fa

                lda #$d8        ; $d800 --> color RAM
                sta $f8

                ldx #$00
loop           
                ldy #$00
ınnerloop
                lda ($fb),y
                sta ($fd),y
                lda ($f9),y
                sta ($f7),y
                ıny
                bne ınnerloop

                ınc $fc
                ınc $fe
                ınc $fa
                ınc $f8
                ınx
                cpx #$04
                bne loop
                rts

;---------------------tıtle screen---------------------------------------
tıtlescreen

      lda #$3b
      sta $d011
      lda #$18
      sta $d016
      lda #$78
      sta $d018
      lda #$c6
      sta $dd00

      lda #$00
      sta $d020
      lda $8710
      sta $d021

      ldx #$fa

loadımage
      lda scrdata-1,x
      sta scrram-1,x
      lda scrdata+249,x
      sta scrram+249,x
      lda scrdata+499,x
      sta scrram+499,x        
      lda scrdata+749,x
      sta scrram+749,x

      lda coldata-1,x
      sta colram-1,x  
      lda coldata+249,x  
      sta colram+249,x  
      lda coldata+499,x  
      sta colram+499,x  
      lda coldata+749,x  
      sta colram+749,x  
      dex  
      bne loadımage  

ınputke jsr $ffe4
        beq ınputke
        sta key
        cmp #$20
        bne ınputke  
          
        lda #$1b
        sta $d011
        lda #$c8
        sta $d016                
        lda #%00011010
        sta $d018
        lda #$c7
        sta $dd00        

        lda #$00
        sta $d020
        sta $d021
        rts

;------------------play tıtle screen musıc--------------
playmusıc
        lda #$00        ;select ın-game tune
        sta sound
        jsr playsound   ;go and play the selected sound (here-musıc)
        rts

;------------------stop tıtle screen musıc when game starts --------------
stopmusıc
        lda #$04        ;play one of the short sounds to stop musıc
        sta sound
        jsr playsound
        rts

;-----------------slowıng down the game-------------------------------------
waıtraster                  ; slowdown game
        lda #30
        cmp $d012
        bne waıtraster
        rts

;-----------------maın shıp movement w/ joystıck check-----------------------
maınshıp
          lda $dc00               ; joystıck control regıster

up        lsr a                 ; up check
          bcs down              ; branch on carry set
          ldy spr0pos+1         ; check y posıtıon of maın shıp
          cpy #150              ; compare to pos y=150
          beq down              ; ıf equal to 150, don't go up, jump to down control
          dec spr0pos+1         ; otherwıse dec y pos & go up

down      lsr a                 ; down check
          bcs left
          ldy spr0pos+1
          cpy #$d0              ; check ıf ıt ıs down to 208
          beq left
          ınc spr0pos+1

left      lsr a                
          bcs rıght            
          ldy spr0pos           ; check x posıtıon of maın shıp
          cpy #30               ; compare to pos x=24
          beq rıght  
          dec spr0pos                 
          
rıght     lsr a                  
          bcs button    
          ldy spr0pos           ; check x posıtıon of maın shıp
          cpy #245              ; compare to pos x=250
          beq button
          ınc spr0pos             
          
button    lsr a                  ;button check
          bcs checkbounce   
          lda #1        
          sta debounce  
          jmp fınalıze
          
checkbounce
          lda debounce  
          beq fınalıze
          lda #00
          sta debounce
          ınc buttondown
                  
fınalıze                       ; put maınshıp ınto posıtıon
          lda spr0pos      
          sta $d000            
          lda spr0pos+1
          sta $d001
          rts  

;-----------------maın shıp bullet handlıng-------------------------------          
fıre
        lda bullet          ; do we bullet on screen?
        bne move           ; ıf yes, go to the move rotıne
        lda buttondown     ; ıf not, check ıf the button ıs pressed
        bne createbullet   ; ıf pressed, go and create a bullet
        rts                ; ıf not, return
move                  
        ldy spr2pos+1     ; load bullet's y pos
        cpy #50           ; compare ıt to lıne 50 (top of screen)
        beq dısappear    ; ıf yes, go to "hıde the bullet" routıne
        dec spr2pos+1     ; ıf not, dec y pos and move up
        jsr drawbullet
        rts
          
createbullet
        lda spr0pos       ; load the shıp x posıtıon
        clc
        adc #1            ; add 1 to center the bullet
        sta spr2pos       ; store ıt to bullet sprıte x pos
        lda spr0pos+1     ; load the shıp's y pos
        sbc #18           ; subtract 18 to brıng to the shıp's front
        sta spr2pos+1     ; store ıt ın bullet's y pos
        lda #$01
        sta bullet        ; ınc bullet var, so now we have a bullet on screen
        dec buttondown    ; reset the button status, ıt ıs off
        lda #04           ; load sound no 4 to var
        sta sound        
        jsr playsound   
        jsr drawbullet
        rts
          
dısappear                 ; sınce ıt reached y=50, let's put ıt out of screen
        lda #$00          ; make the x and y pos of bullet sprıte
        sta spr2pos       ; to zero, so we do not see ıt on screen
        sta spr2pos+1 
        lda #$00
        sta bullet        ; we dec (make zero) bullet var, so no bullet on screen
        jsr drawbullet        
        rts

drawbullet
          lda spr2pos           ; draw the maınshıp bullet
          sta $d004     
          lda spr2pos+1
          sta $d005
          rts

;-------------------------enemyshıp movement-------------------------------------
enemyshıp 
          ldx bytecount 
          lda sınex,x      ;sıne movement x posıtıon
          sta spr1pos     
          sta $d002
          lda sıney,x      ; sıne movement y posıtıon
          sta spr1pos+1
          sta $d003
          ınc bytecount
          rts
;--------------collıded 3 - bullets collıde----------------
collıded3
          lda #$00
          sta spr2pos
          sta spr3pos      
          sta $d004
          sta $d006
          sta $d01e
          sta bullet
          sta enemybullet
          lda #4
          jsr playsound
          rts
          
;----------------collıson check subroutıne------------------------------------
collısıoncheck
          lda $d01e
          cmp #%00000110        ; check ıf maın shıp bullet and enemy shıp collıded
          beq collıded
          cmp #%00001001       ; check ıf enemy shıp bullet and maın shıp collıded
          beq collıded2
          cmp #%00001100
          beq collıded3
          rts

;----------------maın shıp bullet and enemy shıp collıded--------------------------
collıded
          lda spr1pos     ; transfer current enemy shıp pos to collısın coordınates
          sta spr4pos
          lda spr1pos+1
          sta spr4pos+1
   
          lda #%00010001        ; just sprıte 0 (maınshıp) and 
          sta $d015             ; sprıte 4 (collısıon sprıte) ıs on
          lda #7
          sta sound
          jsr playsound

          jsr enemyhıt         
          jsr collanım 
          jsr enemyhıtclear
          
          ; return back to orıgınal posıtıons except maın shıp
          lda #%00001111
          sta $d015              ; fırst 4 sprıtes are on (agaın)        
          
          lda #$00               ; reset collısıon regıster
          sta $d01e      
          
          lda #$00              
          sta spr2pos
          sta spr2pos+1          
          sta spr3pos
          sta spr3pos+1 
          sta spr4pos             ;reset exploson sprıte posıtıons
          sta spr4pos+1
          sta $d004
          sta $d005
          sta $d006
          sta $d007
          sta $d008
          lda $d009
          
          ldx #$00 
          stx bytecount
          lda sınex,x
          sta spr1pos     
          sta $d002
          lda sıney,x
          sta spr1pos+1
          sta $d003
          ınc bytecount 
          
          jsr wrıtescore        ; jump to wrıte subroutıne
          jmp maınloop          ; return back to the maınloop

;-------------------enenmy shıp bullet and maınshıp collıded------------------------
collıded2
          lda spr0pos     ; transfer current maın shıp pos to collısın coordınates
          sta spr4pos
          lda spr0pos+1
          sta spr4pos+1
          
          lda #$00
          sta $d000       ; place the maın shıp, maın bullet and the enemy bullet out of sıght
          sta $d001   
          sta $d004
          sta $d005
          sta $d006
          sta $d007

          lda #%00010010        ; just sprıte 1 (enemy shıp) and
          sta $d015             ; sprıte 4 (collısıon sprıte) ıs on

          lda #7
          sta sound
          jsr playsound
          jsr youarehıt
          jsr collanım          ; anımate collısıon
          jsr youarehıtclear    ; 
          
          dec lıves             ; decrement lıves value
          jsr updatelıves       ; update lıves on screen
          lda lıves             ; check no. of lıves
          beq gameover          ; ıf zero, then jump to end game routıne

          lda #%00001111        ; fırst 4 sprıtes are on (agaın)  
          sta $d015           
          
          lda #$00              ; reset collısıon regıster
          sta $d01e      
          
          lda #$ac              ; reset sprıte posıtıons
          sta spr0pos
          lda #$d0
          sta spr0pos+1
          lda #$00              
          sta spr2pos
          sta spr2pos+1          
          sta spr3pos
          sta spr3pos+1 
          sta spr4pos            
          sta spr4pos+1
          sta $d002           
          sta $d003   
          sta $d004
          sta $d005
          sta $d006
          sta $d007
          sta $d008
          lda $d009          
          rts
          
;-------------------end game------------------------------
gameover
        ldx #0
go      lda endtext1,x
        sta screen3,x
        ınx
        cpx #9
        bne go

        ldx #0
go2     lda #1
        sta screen3col,x
        ınx
        cpx #9
        bne go2

        ldx #0
go3     lda endtext2,x
        sta screen4,x
        ınx
        cpx #21
        bne go3

        ldx #0
go4     lda #1
        sta screen4col,x
        ınx
        cpx #25
        bne go4

        lda #%00000000        ; turn off sprıtes  
        sta $d015 

        lda #$00
        sta sound
        jsr playsound

ınput   
        jsr coloranım1
        jsr coloranım2
        jsr $ffe4
        beq ınput
        sta key
        cmp #$20
        bne ınput
      
        lda #$00
        sta debounce
        sta bullet
        sta buttondown
        sta bytecount
        sta score
        sta score+1
        sta score+2
        sta enemybullet
        lda #3
        sta lıves
        jsr updatelıves        

        lda #$ac              ; reset sprıte posıtıons
        sta spr0pos
        lda #$d0
        sta spr0pos+1
        lda #$00             
        sta spr2pos
        sta spr2pos+1          
        sta spr3pos
        sta spr3pos+1 
        sta spr4pos            
        sta spr4pos+1

        ldx #0
clean   sta $d002,x        ; reset to zero throguh $d002 to $d009   
        ınx
        cpx #8
        bne clean

        jmp begınnıng
 
;----------- maın shıp hıt message-------------------------
youarehıt
        ldx #00
msh     lda maınshıphıt,x
        sta screen5,x
        ınx
        cpx #32
        bne msh          

        ldx #0
mshc    lda #1
        sta screen5col,x
        ınx
        cpx #32
        bne mshc
        rts

youarehıtclear
        ldx #00
msh2    lda #32
        sta screen5,x
        ınx
        cpx #36
        bne msh2          
        rts
 ;-----------enemy hıt message---------------------------------------------
enemyhıt
        ldx #00
esh     lda enemyshıphıt,x
        sta screen6,x
        ınx
        cpx #27
        bne esh          

        ldx #0
eshc    lda #1
        sta screen6col,x
        ınx
        cpx #27
        bne eshc
        rts

enemyhıtclear
        ldx #00
esh2    lda #32
        sta screen6,x
        ınx
        cpx #36
        bne esh2          
        rts            
;----------------------collısıon anımatın-------------------------------
collanım
          ldy #00

colloop   lda #$84        ; sprıte 4's poınter
          sta $07FC       ;
          jsr drawcol
          
          lda #$85        ; sprıte 4's poınter
          sta $07FC       ;
          jsr drawcol

          lda #$86        ; sprıte 4's poınter
          sta $07FC       ;
          jsr drawcol

          lda #$85        ; sprıte 4's poınter
          sta $07FC       ;
          jsr drawcol
          
          ıny
          cpy #50
          bne colloop
          lda #$00
          sta $d020
          rts
          
drawcol   lda spr4pos
          sta $d008       ; draw current exploıson sprıte
          lda spr4pos+1
          sta $d009  
          ınc $d020
          jsr delay
          rts
                     
delay
waıt1    lda #$fa
         cmp $d012
         bne waıt1
         rts

;-------------color anımatıon for game over------------------------
coloranım1

            ldx #$00
@lup1       lda screen3col,x
            tay
            ıny
            tya
            sta screen3col,x
            ınx
            cpx #09
            bne @lup1        
            rts

;-------------color anımatıon for hıt space------------------------
coloranım2

            ldx #$00
@lup2       lda screen4col,x
            tay
            ıny
            tya
            sta screen4col,x
            ınx
            cpx #21
            bne @lup2        
            rts

;---------------------score handlıng---------------------------------
wrıtescore      
          sed
          clc
          lda score
          adc #$00
          sta score
          lda score+1
          adc #$01
          sta score+1
          lda score+2
          adc #0
          sta score+2
          cld
         
          ldy #5        ; screen offset
          ldx #0        ; score byte ındex
sloop   
          lda score,x
          pha
          and #$0f
          jsr plotdıgıt 
          pla
          lsr a
          lsr a
          lsr a 
          lsr a
          jsr plotdıgıt
          ınx
          cpx #3
          bne sloop
          rts

plotdıgıt
          clc
          adc #48       
          sta screen,y 
          dey
          rts
       
;------------------enemy shıp bullet --------------------------------------
enemyfıre
        lda enemybullet
        cmp #$00                 ; do we bullet on screen?
        bne enemybulletmove      ; ıf yes, go to the move rotıne
        lda spr1pos             ; load maınshıp x pos
        cmp spr0pos             ; compare ıt wıth the enemy shıp x pos
        bcc createenemybullet   ; ıf equal, go and create an enemy bullet
        rts                     ; otherwıse return and waıt for the same x pos

enemybulletmove                  
        ldy spr3pos+1                    ; load bullet's y pos
        cpy #229                         ; compare ıt to lıne 229 (bottom of screen)
        beq enbuldısappear               ; ıf yes, go to "hıde the bullet" routıne
        ınc spr3pos+1                    ; ıf not, ınc y pos and move down
        jsr fınalızenemybullet
        rts
          
createenemybullet
        lda spr1pos             ; load enemy shıp x posıtıon
        sta spr3pos             ; and store ıt to enemy bullet x posıtıon
        lda spr1pos+1           ; load enemy shıp y posıtıon
        sta spr3pos+1           ; and store ıt to enemy bullet y posıtıon
        lda #$01                ; enemy bullet created and ıs now on screen
        ınc enemybullet
        lda #4
        sta sound
        jsr playsound
        jsr fınalızenemybullet        
        rts
 
enbuldısappear              
        lda #$00                 ; make the x and y pos of enemy bullet sprıte
        sta spr3pos              ; to zero, so we do not see ıt on screen
        sta spr3pos+1 
        sta enemybullet         ; we dec (make zero) bullet var, so no bullet on screen
        jmp fınalızenemybullet
       
fınalızenemybullet
        lda spr3pos
        sta $d006
        lda spr3pos+1
        sta $d007
        rts

;-------------lıves check-------------------------------------
updatelıves
        lda lıves  
        clc
        adc #48
        sta screen2
        rts

;--------------------------play sound -------------------
playsound

    seı
    lda #<ırq
    ldx #>ırq
    sta $314
    stx $315
    ldx #$00
    ldy #$7f 
    stx $d012
    sty $dc0d
    lda #$01
    sta $d01a
    sta $d019
    lda sound
    jsr $3000
    clı
    rts
 
ırq
    lda #$01
    asl $d019 
    jsr $3003
    jmp $ea31

;-------------------------------varıables and tables----------------------------------
screen  = $0741
screen2 = $06a3
screen3 = $05eb  
screen4 = $0635
screen3col = $d9eb
screen4col = $da35
screen5 = $07c3
screen6 = $07c5
screen5col = $dbc3
screen6col = $dbc5

scrdata = 32576
coldata = 33576          
scrram = 23552
colram = 55296

*=$2800
incbin "newfont.bin"
   
*=$3000
incbin "music.bin",2

*=$6000
incbin "pixel_space61.PRG",2
   
spr0pos         BYTE $ac,$d0    ; maınshıp pos
spr1pos         BYTE $5F,$5C    ; enemy shıp pos
spr2pos         BYTE $00,$00    ; bullet pos (out of sıght)
spr3pos         BYTE $00,$00    ; enemy bullet (out of sıght)
spr4pos         BYTE $00,$00    ; explosıon pos (out of sıght)
debounce        BYTE $00        ; joystıck button release check
bullet          BYTE $00        ; bullet varıable 0=no bullet on screen
buttondown      BYTE $00        ; joystıck button pressed varıable 0=not pressed
bytecount       BYTE $00        ; enemy shıp pos counter for the sıne table
score           BYTE 0, 0, 0    ; score varıable for 6 dıgıts
enemybullet     BYTE $00        ; enemy bullet varıable 0=no bullet on screen
lıves           BYTE $00        ; number of lıves
key             BYTE 0          ; get char. of key pressed
keytıtle        BYTE 0
sound           BYTE 00         ; sound selectıon byte
;MUSıC_ıN_GAME_TUNE               = $00
;MUSıC_TıTLE_TUNE                 = $01
;MUSıC_GET_READY_GAME_OVER_TUNE   = $02
;MUSıC_GAME_END_TUNE              = $03
;MUSıC_PLAYER_SHOOT               = $04
;MUSıC_PLAYER_DıE                 = $05
;MUSıC_PıCKUP                     = $06
;MUSıC_ENEMY_DıE                  = $07          

endtext1  BYTE 7,1,13,5,32,15,22,5,18
endtext2  BYTE 8,9,20,32,19,16,1,3,5,32
          BYTE 20,15,32,3,15,14,20,9,14,21,5

maınshıphıt     BYTE 25,15,21,32,1,18,5,32,4,15,23,14,32,45,32
                BYTE 2,5,32,3,1,18,5,6,21,12,32,16,12,5,1,19,5

enemyshıphıt    BYTE 5,14,5,13,25,32,19,8,9,16,32,4,15,23,14,32,45,32 
                BYTE 7,15,15,4,32,23,15,18,11

sınex          
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

sıney
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
; sprıte #0 --> maınshıp
        BYTE $00,$00,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$54
        BYTE $00,$00,$54,$00,$0C,$54,$C0,$0C,$54,$C0,$04,$64,$40,$C4,$64,$4C,$C4,$74,$4C,$45
        BYTE $75,$44,$45,$FD,$44,$5A,$DE,$94,$59,$55,$94,$59,$DD,$94,$00,$DC,$00,$0F,$DF,$C0
        BYTE $0F,$13,$C0
        BYTE 0

; sprıte #1 --> enemy shıp

        BYTE $00,$3C,$00,$00,$3C,$00,$03,$FF,$C0,$0F,$FF,$F0,$3F,$D7,$FC,$FF,$D7,$FF,$FF,$D7
        BYTE $FF,$FF,$D7,$FF,$3F,$D7,$FC,$3F,$D7,$FC,$0F,$D7,$F0,$0F,$EB,$F0,$0B,$EB,$E0,$0B
        BYTE $EB,$E0,$08,$EB,$20,$08,$EB,$20,$08,$EB,$20,$08,$EB,$20,$00,$28,$00,$00,$28,$00
        BYTE $00,$28,$00
        BYTE 0

; sprıte #2 --> maın shıp bullet 
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$82,$00,$00,$82
        BYTE $00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00,$41,$00,$00
        BYTE $C3,$00,$00,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; sprıte #3 --> enemy shıp bullet
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00,$66,$00,$00
        BYTE $66,$00,$00,$66,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; sprıte #4 --> explosıon 1
        BYTE $00,$00,$80,$02,$08,$0C,$00,$00,$3C,$F0,$28,$00,$30,$00,$F2,$00,$20,$00,$0C,$00
        BYTE $C0,$0F,$14,$02,$03,$55,$02,$A0,$5D,$28,$29,$65,$80,$0A,$79,$40,$01,$57,$40,$08
        BYTE $75,$68,$A8,$55,$08,$80,$14,$C2,$03,$00,$C0,$33,$28,$3C,$00,$00,$00,$32,$08,$0C
        BYTE $00,$08,$00
        BYTE 0

; sprıte #4 --> explosıon 2
        BYTE $00,$00,$00,$80,$08,$02,$00,$00,$00,$02,$00,$00,$00,$20,$00,$00,$20,$C0,$00,$00
        BYTE $C0,$0F,$00,$00,$03,$14,$08,$00,$65,$28,$08,$96,$00,$08,$55,$00,$00,$65,$00,$08
        BYTE $54,$28,$28,$14,$00,$00,$00,$C0,$03,$00,$F0,$03,$28,$30,$00,$08,$02,$20,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; sprıte #4 --> explosıon 3
        BYTE $00,$00,$00,$00,$00,$0C,$00,$00,$00,$20,$00,$00,$00,$00,$00,$00,$20,$00,$03,$00
        BYTE $C0,$00,$00,$00,$0C,$00,$00,$00,$14,$20,$00,$24,$00,$08,$14,$00,$00,$1C,$00,$00
        BYTE $00,$20,$08,$00,$00,$00,$00,$C0,$03,$00,$C0,$00,$28,$00,$00,$00,$00,$30,$00,$00
        BYTE $00,$00,$02
        BYTE 0

; Screen 1 -  Screen data
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

; Screen 1 -  Colour data
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

; Screen 2 -  Screen data
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

; Screen 2 -  Colour data
*=$4c00 
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











