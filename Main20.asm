; 10 SYS (2064)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

*=$0810

start
        seı
        lda #147        ; clear screen
        jsr $ffd2

        ; set border and background color to black
        lda #$00
        sta $D020
        sta $D021

        lda #03
        sta lıves

beginning
        jsr playmusıc
        jsr titlescreen
        jsr drawscreen
        jsr setupsprıtes
        jsr stopmusıc

;---------------------maın game loop-----------------------------------------       
maınloop  
          jsr waıtraster        ; watt for rasterlıne to slow down
          jsr maınshıp          ; waıt for joystıck to command and move maın shıp
          jsr enemyshıp         ; move enemy shıp accordıng to the sıne table
          jsr enemyfıre         ; enemyshıp fıre check and control
          jsr fıre              ; maınshıp bullet check and control
          jsr collısıoncheck    ; check ıf any collısıon occurs
          jmp maınloop          ; contınue doıng

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
        lda #$05        ;-> sprıte 1 (enemy) color = green      
        sta $d028
        lda #$07        ;-> sprıte 2 (bullet) color = yellow
        sta $d029
        lda #$05        ;-> sprıte 4 (enemy bullet) color = green 
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
        lda #$15        ; sprıtes 0,2,4 are multıcolor
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

;-----------------draw screen rotıne  ------------------------------------
drawscreen
                lda #$00
                sta $fb
                sta $fd         ; $0400 Screen RAM
                sta $f7

                lda #$c0        ; $c000 character data -->  transfer to $0400
                sta $fc

                lda #$04
                sta $fe

                lda #$00        ; $c100 screen color data --> transfer to $d800
                sta $f9
                lda #$c4
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

;---------------------title screen---------------------------------------
titlescreen

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

loadimage
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
      bne loadimage  

ınputk  jsr $ffe4
        beq ınputk
        sta key
        cmp #$20
        bne ınputk    
          
        lda #$1b
        sta $d011
        lda #$c8
        sta $d016                
        lda #$15
        sta $d018
        lda #$c7
        sta $dd00        

        lda #$00
        sta $d020
        sta $d021
        rts

;------------------play title screen musıc--------------
playmusıc
        lda #$00
        sta sound
        jsr playsound
        rts

;------------------stop title screen musıc when game starts --------------
stopmusıc
        lda #$04        ;play one of the short sounds to stop music
        sta sound
        jsr playsound
        rts

;-----------------slowıng down the game-------------------------------------
waıtraster                        ; slowdown game
        lda #40
        cmp $d012
        bne waıtraster
        rts

;-----------------maın shıp movement w/ joystıck check-----------------------
maınshıp
          lda $dc00               ; joystıck control regıster

up        lsr a                 ; up check
          bcs down             ; branch on carry set
          ldy spr0pos+1         ; check y posıtıon of maın shıp
          cpy #150              ; compare to pos y=130
          beq down             ; branch on result zero (don't go up)
          dec spr0pos+1         ; otherwıse dec y pos & go up

down      lsr a                   ; down check
          bcs left
          ldy spr0pos+1
          cpy #$d0
          beq left
          ınc spr0pos+1

left      lsr a                
          bcs rıght            
          ldy spr0pos           ; check x posıtıon of maın shıp
          cpy #24               ; compare to pos x=24
          beq rıght  
          dec spr0pos                 
          
rıght     lsr a                  
          bcs button    
          ldy spr0pos           ; check x posıtıon of maın shıp
          cpy #250              ; compare to pos x=250
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
                  
fınalıze                       ; put sprıtes ınto theır posıtıons
          lda spr0pos      
          sta $d000             ; maınshıp
          lda spr0pos+1
          sta $d001
          lda spr2pos           ; maınshıp bullet
          sta $d004     
          lda spr2pos+1
          sta $d005
          rts  

;-----------------maın shıp bullet handlıng-------------------------------          
fıre
        lda bullet
        cmp #$01           ; do we bullet on screen?
        beq move           ; ıf yes, go to the move rotıne
        lda buttondown     ; ıf not, check ıf the button ıs pressed
        bne createbullet   ; ıf pressed, go and create a bullet
        rts                ; ıf not, return

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
        rts

move                  
        ldy spr2pos+1     ; load bullet's y pos
        cpy #50           ; compare ıt to lıne 50 (top of screen)
        beq dısappear    ; ıf yes, go to "hıde the bullet" routıne
        dec spr2pos+1     ; ıf not, dec y pos and move up
        rts
 
dısappear              
        lda #$00          ; make the x and y pos of bullet sprıte
        sta spr2pos       ; to zero, so we do not see ıt on screen
        sta spr2pos+1 
        lda #$00
        sta bullet        ; we dec (make zero) bullet var, so no bullet on screen
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

          jsr collanım 

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

          jsr collanım          ; anımate collısıon
          
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

ınput   jsr $ffe4
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

        jmp beginning
          
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
         ldx #$ff
waıt1    cpx $d012
         bne waıt1
         rts
         ınx
waıt2    cpx $d012
         bne waıt2
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
        bcs createenemybullet   ; ıf equal, go and create an enemy bullet
        rts                     ; otherwıse return and waıt for the same x pos
          
createenemybullet
        lda spr1pos             ; load enemy shıp x posıtıon
        sta spr3pos             ; and store ıt to enemy bullet x posıtıon
        lda spr1pos+1           ; load enemy shıp y posıtıon
        sta spr3pos+1           ; and store ıt to enemy bullet y posıtıon
        lda #$01                ; enemy bullet created and ıs now on screen
        sta enemybullet
        lda #4
        sta sound
        jsr playsound
        jmp fınalızenemybullet        

enemybulletmove                  
        ldy spr3pos+1                    ; load bullet's y pos
        cpy #229                         ; compare ıt to lıne 229 (bottom of screen)
        beq enemybulletdısappear         ; ıf yes, go to "hıde the bullet" routıne
        ınc spr3pos+1                    ; ıf not, ınc y pos and move down
        jmp fınalızenemybullet
 
enemybulletdısappear              
        lda #$00                 ; make the x and y pos of enemy bullet sprıte
        sta spr3pos              ; to zero, so we do not see ıt on screen
        sta spr3pos+1 
        lda #$00
        sta enemybullet         ; we dec (make zero) bullet var, so no bullet on screen
       
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
    ;lda #$1b
    ldx #$00
    ldy #$7f 
    ;sta $d011
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
scrdata = 32576
coldata = 33576          
scrram = 23552
colram = 55296

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
keytitle        BYTE 0
sound           BYTE 00         ; sound selection byte
;MUSIC_IN_GAME_TUNE               = $00
;MUSIC_TITLE_TUNE                 = $01
;MUSIC_GET_READY_GAME_OVER_TUNE   = $02
;MUSIC_GAME_END_TUNE              = $03
;MUSIC_PLAYER_SHOOT               = $04
;MUSIC_PLAYER_DIE                 = $05
;MUSIC_PICKUP                     = $06
;MUSIC_ENEMY_DIE                  = $07          

endtext1  BYTE 7,1,13,5,32,15,22,5,18
endtext2  BYTE 8,9,20,32,19,16,1,3,5,32
          BYTE 20,15,32,3,15,14,20,9,14,21,5
sınex          
        BYTE 137,134,132,129,126,123,121,118,115,113,110,107,105,102,100,97
        BYTE 95,92,90,87,85,82,80,78,75,73,71,69,67,64,62,60
        BYTE 58,57,55,53,51,49,48,46,44,43,41,40,39,37,36,35
        BYTE 34,33,32,31,30,29,28,28,27,27,26,26,25,25,25,25
        BYTE 25,25,25,25,25,25,25,26,26,27,27,28,28,29,30,31
        BYTE 32,33,34,35,36,37,39,40,41,43,44,46,48,49,51,53
        BYTE 55,56,58,60,62,64,67,69,71,73,75,78,80,82,85,87
        BYTE 90,92,95,97,100,102,105,107,110,113,115,118,121,123,126,129
        BYTE 131,134,137,140,142,145,148,150,153,156,159,161,164,166,169,172
        BYTE 174,177,179,182,184,187,189,192,194,196,199,201,203,205,207,209
        BYTE 212,214,216,217,219,221,223,225,226,228,230,231,233,234,235,237
        BYTE 238,239,240,241,242,243,244,245,246,246,247,247,248,248,249,249
        BYTE 249,249,249,249,249,249,249,249,249,248,248,247,247,246,246,245
        BYTE 244,243,242,241,240,239,238,237,235,234,233,231,230,228,226,225
        BYTE 223,221,219,218,216,214,212,210,208,205,203,201,199,196,194,192
        BYTE 189,187,185,182,180,177,174,172,169,167,164,161,159,156,153,151
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

*=$2000     ; $2800 -> $5000
; sprıte #0 --> maınshıp
        BYTE $00,$00,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$10,$00,$00,$54
        BYTE $00,$00,$54,$00,$0C,$54,$C0,$0C,$54,$C0,$04,$54,$40,$C4,$54,$4C,$C4,$74,$4C,$45
        BYTE $75,$44,$45,$FD,$44,$5A,$DE,$94,$59,$55,$94,$59,$DD,$94,$00,$DC,$00,$0F,$DF,$C0
        BYTE $0F,$13,$C0
        BYTE 0

; sprıte #1 --> enemy shıp
        BYTE $00,$00,$00,$03,$00,$C0,$03,$00,$C0,$00,$C3,$00,$00,$C3,$00,$03,$FF,$C0,$03,$FF
        BYTE $C0,$0F,$FF,$F0,$0F,$FF,$F0,$3E,$7C,$FC,$3E,$7C,$FC,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        BYTE $FF,$F3,$CF,$FF,$F3,$CC,$00,$33,$CC,$00,$33,$0C,$00,$30,$03,$C3,$C0,$03,$C3,$C0
        BYTE $00,$00,$00
        BYTE 0

; sprıte #2 --> maın shıp bullet 
        ;BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$00,$80,$08,$00
        ;BYTE $80,$04,$00,$40,$04,$00,$40,$04,$00,$40,$04,$00,$40,$04,$00,$40,$04,$00,$40,$0C
        ;BYTE $00,$C0,$0C,$00,$C0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        ;BYTE $00,$00,$00
        ;BYTE 0
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
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$03,$00
        BYTE $C0,$00,$00,$00,$0C,$00,$00,$00,$14,$20,$00,$14,$00,$08,$14,$00,$00,$14,$00,$00
        BYTE $00,$20,$08,$00,$00,$00,$00,$C0,$03,$00,$C0,$00,$28,$00,$00,$00,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; sprıte #4 --> explosıon 2
        BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$00,$20,$C0,$00,$00
        BYTE $C0,$0F,$00,$00,$03,$14,$08,$00,$55,$28,$08,$55,$00,$08,$55,$00,$00,$55,$00,$08
        BYTE $54,$28,$28,$14,$00,$00,$00,$C0,$03,$00,$F0,$03,$28,$30,$00,$08,$00,$00,$00,$00
        BYTE $00,$00,$00
        BYTE 0

; sprıte #4 --> explosıon 3
        BYTE $00,$00,$00,$00,$08,$0C,$00,$08,$3C,$F0,$28,$30,$30,$20,$F0,$3C,$20,$C0,$0C,$00
        BYTE $C0,$0F,$14,$02,$03,$55,$0A,$A0,$55,$28,$29,$55,$40,$09,$55,$40,$01,$55,$40,$08
        BYTE $55,$68,$A8,$55,$0A,$80,$14,$C2,$03,$00,$F0,$3F,$28,$3C,$30,$08,$0C,$30,$08,$0C
        BYTE $00,$08,$00
        BYTE 0


; Screen 1 -  Screen data
*=$C000          
;screendata
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
        BYTE    $20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$70,$40,$40,$40,$40,$40,$40,$40,$6E
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$01,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$2E,$20,$20,$5D,$20,$13,$10,$01,$03,$05,$20,$5D
        BYTE    $20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$07,$01,$0D,$05,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$5D,$20,$20,$02,$19,$20,$20,$20,$5D
        BYTE    $20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$2E,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$0D,$05,$14,$05,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$13,$05,$16,$20,$20,$5D
        BYTE    $20,$20,$20,$2E,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$5D,$20,$32,$30,$32,$31,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$0C,$09,$16,$05,$13,$20,$5D
        BYTE    $20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$5D,$20,$20,$20,$33,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$13,$03,$0F,$12,$05,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$5D,$20,$30,$30,$30,$30,$30,$30,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$2E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$20,$20,$20,$20,$20,$2E,$20,$20,$5D,$20,$20,$20,$20,$20,$20,$20,$5D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$6D,$40,$40,$40,$40,$40,$40,$40,$7D
        BYTE    $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

; Screen 1 -  Colour data
*=$C400
;colordata
        BYTE    $00,$00,$07,$07,$07,$07,$07,$07,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$00,$0E,$00,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$01,$01,$01,$01,$01,$0F,$0F,$0F,$00,$0F,$01
        BYTE    $00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$02,$00,$0E,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$01,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$01,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$05,$05,$05,$05,$0F,$01
        BYTE    $00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$05,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$01
        BYTE    $00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$07,$00,$00,$01,$01,$01,$01,$05,$05,$01,$05,$05,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$05,$05,$05,$05,$05,$05,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$0D,$0D,$0D,$0F,$01
        BYTE    $00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$01,$01,$01,$0D,$0D,$0D,$0D,$0D,$0D,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$01,$01
        BYTE    $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
        BYTE    $00,$00,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$00,$0F,$0F,$0F,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$0F,$0F,$0F,$01,$0F,$0F,$0F,$0F,$0F,$00,$01

