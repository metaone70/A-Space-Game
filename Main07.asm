; 10 SYS2061
*=$0801
        BYTE $0B, $08, $0A, $00, $9E, $32, $30, $36, $31, $00, $00, $00

*=$080d
        
spr1xlo    byte $ac
;spr1xhı    byte $00         
debounce   byte $00


; set to 25 lıne text mode and turn on the screen
        lda #$1B
        sta $D011

        ; dısable SHIFT-Commodore
        lda #$80
        sta $0291

        ; set screen memory ($0400) and charset bıtmap offset ($2000)
        ;lda #$18
        ;sta $D018

        ; set border color
        lda #$00
        sta $D020
        
        ; set background color
        lda #$00
        sta $D021

        ; draw screen
        lda #$00
        sta $fb
        sta $fd
        sta $f7

        lda #$28
        sta $fc

        lda #$04
        sta $fe

        lda #$e8
        sta $f9
        lda #$2b
        sta $fa

        lda #$d8
        sta $f8

        ldx #$00
@loop       
        ldy #$00
@ınnerloop
        lda ($fb),y
        sta ($fd),y
        lda ($f9),y
        sta ($f7),y
        ıny
        bne @ınnerloop

        ınc $fc
        ınc $fe
        ınc $fa
        ınc $f8

        ınx
        cpx #$04
        bne @loop

        ; set sprıte multıcolors
        lda #$0C
        sta $d025
        lda #$02
        sta $d026

        ; colorıze sprıtes
        lda #$07
        sta $d027
        lda #$03
        sta $d028
        lda #$07
        sta $d029

        ; posıtıonıng sprıtes
        lda spr1pos
        sta $d000       ; #0. sprıte X low byte
        lda spr1pos+1
        sta $d001       ; #0. sprıte Y
        lda spr2pos
        sta $d002       ; #1. sprıte X low byte
        lda spr2pos+1
        sta $d003       ; #1. sprıte Y
        lda spr3pos
        sta $d004       ; #2. sprıte X low byte
        lda spr3pos+1
        sta $d005       ; #2. sprıte Y

        ; X coordınate hıgh bıts
        lda #$00
        sta $d010

        ; expand sprıtes
        lda #$00
        sta $d01d
        lda #$00
        sta $d017

        ; set multıcolor flags
        lda #$05
        sta $d01c

        ; set screen-sprıte prıorıty flags
        lda #$00
        sta $d01b

        ; set sprıte poınters
        lda #$28
        sta $07F8
        lda #$29
        sta $07F9
        lda #$2A
        sta $07FA

        ; turn on sprıtes
        lda #$07
        sta $d015

       
maınloop    ; waıt for joystıck
          jsr waıtraster
          jsr maınshıp  
          jsr fıre
          jmp maınloop

maınshıp
          lda $dc00             ; joystıck control regıster
@up       lsr a                 ; up check
          bcs @down             ; branch on carry set
          ldy spr1pos+1         ; check y posıtıon of maın shıp
          cpy #130              ; compare to pos y=130
          beq @down             ; branch on result zero (don't go up)
          dec spr1pos+1         ; otherwıse dec y pos & go up

@down     lsr a                   ; down check
          bcs @left
          ldy spr1pos+1
          cpy #$d0
          beq @left
          ınc spr1pos+1

@left     lsr a                   ;left check
          bcs @rıght
          ldy spr1pos
          cpy #30
          beq @rıght
          dec spr1pos

@rıght    lsr a                   ;rıght check
          bcs @button           
          ldy spr1pos
          cpy #$ff
          beq @rıght
          ınc spr1pos

@button
          lsr a                    ;button check
          bcs @checkbounce   
          lda #1        
          sta debounce  
          jmp @fınalıze
          
@checkbounce
          lda debounce  
          beq @fınalıze
          lda #00
          sta debounce
          lda #$01
          sta buttondown
          jsr fıre
        
@fınalıze                       ; put sprıtes ınto theır posıtıons
          lda spr1pos      
          sta $d000     
          lda spr1pos+1
          sta $d001
          lda spr2pos      
          sta $d002     
          lda spr2pos+1
          sta $d003
          lda spr3pos      
          sta $d004     
          lda spr3pos+1
          sta $d005
          rts  
          
waıtraster
        lda #50
        cmp $d012
        bne waıtraster
        rts

fıre
        lda bullet
        cmp #$01           ; ekranda bullet var mı?
        beq @move          ; varsa harekete gonder
        lda buttondown     ; ekranda bullet yoksa button kontrol et
        cmp #$01           ;basılmıssa  
        beq @createbullet  ;bullet yarat 
        rts                ;basılmamıssa fınalıze'a dogru devam et

@createbullet
        lda spr1pos   ; bullet yok, ılk atesleme anını yarat
        adc #1  
        sta spr3pos   ; gemının x,y pozısyonuna gore yerlestır
        lda spr1pos+1
        sbc #18
        sta spr3pos+1
        ınc bullet      ; bullet ekranda (1)
        lda #$00
        sta buttondown  ; button basımını dusur (0)
        rts

@move                   ; y=50'ye geldıyse yok et, aksı halde yukselt
        ldy spr3pos+1
        cpy #50
        beq @dısappear
        dec spr3pos+1
        rts
          
@dısappear              ; y=50'ye geldıyse koord. 0,0 yap, gozukmesın
        lda #$00
        sta spr3pos
        sta spr3pos+1 
        dec bullet     ; bullet ekranda degıl (0)
        rts        


spr1pos byte $ac,$d0
spr2pos byte $64,$46
spr3pos byte $00,$00
bullet  byte $01          
buttondown byte $00

; Sprıte bıtmaps 3 x 64 bytes
*=$0A00
; sprıte #0
        BYTE $00, $28, $00, $00, $28, $00, $00, $29, $00, $00, $29, $00, $08, $AA, $20, $08, $AA, $20, $08, $96, $64
        BYTE $08, $96, $64, $0A, $96, $A4, $26, $55, $98, $2A, $7D, $A8, $26, $7D, $99, $2A, $7D, $79, $2D, $55, $79
        BYTE $29, $7D, $69, $2D, $FF, $69, $0B, $FF, $E5, $0B, $FF, $E4, $03, $FF, $D4, $03, $C3, $C0, $03, $C3, $C0
        BYTE 0

; sprıte #1
        BYTE $00, $00, $00, $00, $00, $00, $06, $00, $60, $06, $00, $60, $01, $81, $80, $01, $81, $80, $03, $FF, $C0
        BYTE $07, $3C, $E0, $1F, $3C, $F8, $1F, $FF, $F8, $1F, $FF, $F8, $1B, $00, $D8, $1B, $00, $D8, $1B, $00, $D8
        BYTE $1B, $C3, $D8, $03, $C3, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        BYTE 0

; sprıte #2
        BYTE $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $20, $00
        BYTE $00, $20, $00, $00, $24, $00, $00, $24, $00, $00, $24, $00, $00, $24, $00, $00, $24, $00, $00, $24, $00
        BYTE $00, $24, $00, $00, $04, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        BYTE 0


; Character bıtmap defınıtıons 2k
*=$2000
        BYTE    $3C, $66, $6E, $6E, $60, $62, $3C, $00
        BYTE    $00, $00, $3C, $06, $3E, $66, $3E, $00
        BYTE    $00, $60, $60, $7C, $66, $66, $7C, $00
        BYTE    $00, $00, $3C, $60, $60, $60, $3C, $00
        BYTE    $00, $06, $06, $3E, $66, $66, $3E, $00
        BYTE    $00, $00, $3C, $66, $7E, $60, $3C, $00
        BYTE    $00, $0E, $18, $3E, $18, $18, $18, $00
        BYTE    $00, $00, $3E, $66, $66, $3E, $06, $7C
        BYTE    $00, $60, $60, $7C, $66, $66, $66, $00
        BYTE    $00, $18, $00, $38, $18, $18, $3C, $00
        BYTE    $00, $06, $00, $06, $06, $06, $06, $3C
        BYTE    $00, $60, $60, $6C, $78, $6C, $66, $00
        BYTE    $00, $38, $18, $18, $18, $18, $3C, $00
        BYTE    $00, $00, $66, $7F, $7F, $6B, $63, $00
        BYTE    $00, $00, $7C, $66, $66, $66, $66, $00
        BYTE    $00, $00, $3C, $66, $66, $66, $3C, $00
        BYTE    $00, $00, $7C, $66, $66, $7C, $60, $60
        BYTE    $00, $00, $3E, $66, $66, $3E, $06, $06
        BYTE    $00, $00, $7C, $66, $60, $60, $60, $00
        BYTE    $00, $00, $3E, $60, $3C, $06, $7C, $00
        BYTE    $00, $18, $7E, $18, $18, $18, $0E, $00
        BYTE    $00, $00, $66, $66, $66, $66, $3E, $00
        BYTE    $00, $00, $66, $66, $66, $3C, $18, $00
        BYTE    $00, $00, $63, $6B, $7F, $3E, $36, $00
        BYTE    $00, $00, $66, $3C, $18, $3C, $66, $00
        BYTE    $00, $00, $66, $66, $66, $3E, $0C, $78
        BYTE    $00, $00, $7E, $0C, $18, $30, $7E, $00
        BYTE    $3C, $30, $30, $30, $30, $30, $3C, $00
        BYTE    $0C, $12, $30, $7C, $30, $62, $FC, $00
        BYTE    $3C, $0C, $0C, $0C, $0C, $0C, $3C, $00
        BYTE    $00, $18, $3C, $7E, $18, $18, $18, $18
        BYTE    $00, $10, $30, $7F, $7F, $30, $10, $00
        BYTE    $00, $00, $00, $00, $00, $00, $00, $00
        BYTE    $18, $18, $18, $18, $00, $00, $18, $00
        BYTE    $66, $66, $66, $00, $00, $00, $00, $00
        BYTE    $66, $66, $FF, $66, $FF, $66, $66, $00
        BYTE    $18, $3E, $60, $3C, $06, $7C, $18, $00
        BYTE    $62, $66, $0C, $18, $30, $66, $46, $00
        BYTE    $3C, $66, $3C, $38, $67, $66, $3F, $00
        BYTE    $06, $0C, $18, $00, $00, $00, $00, $00
        BYTE    $0C, $18, $30, $30, $30, $18, $0C, $00
        BYTE    $30, $18, $0C, $0C, $0C, $18, $30, $00
        BYTE    $00, $66, $3C, $FF, $3C, $66, $00, $00
        BYTE    $00, $18, $18, $7E, $18, $18, $00, $00
        BYTE    $00, $00, $00, $30, $00, $00, $00, $00
        BYTE    $00, $00, $00, $7E, $00, $00, $00, $00
        BYTE    $00, $00, $00, $00, $04, $00, $00, $00
        BYTE    $00, $03, $06, $0C, $18, $30, $60, $00
        BYTE    $3C, $66, $6E, $76, $66, $66, $3C, $00
        BYTE    $18, $18, $38, $18, $18, $18, $7E, $00
        BYTE    $3C, $66, $06, $0C, $30, $60, $7E, $00
        BYTE    $3C, $66, $06, $1C, $06, $66, $3C, $00
        BYTE    $06, $0E, $1E, $66, $7F, $06, $06, $00
        BYTE    $7E, $60, $7C, $06, $06, $66, $3C, $00
        BYTE    $3C, $66, $60, $7C, $66, $66, $3C, $00
        BYTE    $7E, $66, $0C, $18, $18, $18, $18, $00
        BYTE    $3C, $66, $66, $3C, $66, $66, $3C, $00
        BYTE    $3C, $66, $66, $3E, $06, $66, $3C, $00
        BYTE    $00, $00, $18, $00, $00, $18, $00, $00
        BYTE    $00, $00, $00, $00, $20, $00, $00, $00
        BYTE    $0E, $18, $30, $60, $30, $18, $0E, $00
        BYTE    $00, $00, $7E, $00, $7E, $00, $00, $00
        BYTE    $70, $18, $0C, $06, $0C, $18, $70, $00
        BYTE    $3C, $66, $06, $0C, $18, $00, $18, $00
        BYTE    $00, $00, $00, $FF, $FF, $00, $00, $00
        BYTE    $18, $3C, $66, $7E, $66, $66, $66, $00
        BYTE    $7C, $66, $66, $7C, $66, $66, $7C, $00
        BYTE    $3C, $66, $60, $60, $60, $66, $3C, $00
        BYTE    $78, $6C, $66, $66, $66, $6C, $78, $00
        BYTE    $7E, $60, $60, $78, $60, $60, $7E, $00
        BYTE    $7E, $60, $60, $78, $60, $60, $60, $00
        BYTE    $3C, $66, $60, $6E, $66, $66, $3C, $00
        BYTE    $66, $66, $66, $7E, $66, $66, $66, $00
        BYTE    $3C, $18, $18, $18, $18, $18, $3C, $00
        BYTE    $1E, $0C, $0C, $0C, $0C, $6C, $38, $00
        BYTE    $66, $6C, $78, $70, $78, $6C, $66, $00
        BYTE    $60, $60, $60, $60, $60, $60, $7E, $00
        BYTE    $63, $77, $7F, $6B, $63, $63, $63, $00
        BYTE    $66, $76, $7E, $7E, $6E, $66, $66, $00
        BYTE    $3C, $66, $66, $66, $66, $66, $3C, $00
        BYTE    $7C, $66, $66, $7C, $60, $60, $60, $00
        BYTE    $3C, $66, $66, $66, $66, $3C, $0E, $00
        BYTE    $7C, $66, $66, $7C, $78, $6C, $66, $00
        BYTE    $3C, $66, $60, $3C, $06, $66, $3C, $00
        BYTE    $7E, $18, $18, $18, $18, $18, $18, $00
        BYTE    $66, $66, $66, $66, $66, $66, $3C, $00
        BYTE    $66, $66, $66, $66, $66, $3C, $18, $00
        BYTE    $63, $63, $63, $6B, $7F, $77, $63, $00
        BYTE    $66, $66, $3C, $18, $3C, $66, $66, $00
        BYTE    $66, $66, $66, $3C, $18, $18, $18, $00
        BYTE    $7E, $06, $0C, $18, $30, $60, $7E, $00
        BYTE    $18, $18, $18, $FF, $FF, $18, $18, $18
        BYTE    $C0, $C0, $30, $30, $C0, $C0, $30, $30
        BYTE    $18, $18, $18, $18, $18, $18, $18, $18
        BYTE    $33, $33, $CC, $CC, $33, $33, $CC, $CC
        BYTE    $33, $99, $CC, $66, $33, $99, $CC, $66
        BYTE    $00, $00, $00, $00, $00, $00, $00, $00
        BYTE    $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
        BYTE    $00, $00, $00, $00, $FF, $FF, $FF, $FF
        BYTE    $FF, $00, $00, $00, $00, $00, $00, $00
        BYTE    $00, $00, $00, $00, $00, $00, $00, $FF
        BYTE    $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
        BYTE    $CC, $CC, $33, $33, $CC, $CC, $33, $33
        BYTE    $03, $03, $03, $03, $03, $03, $03, $03
        BYTE    $00, $00, $00, $00, $CC, $CC, $33, $33
        BYTE    $CC, $99, $33, $66, $CC, $99, $33, $66
        BYTE    $03, $03, $03, $03, $03, $03, $03, $03
        BYTE    $18, $18, $18, $1F, $1F, $18, $18, $18
        BYTE    $00, $00, $00, $00, $0F, $0F, $0F, $0F
        BYTE    $18, $18, $18, $1F, $1F, $00, $00, $00
        BYTE    $00, $00, $00, $F8, $F8, $18, $18, $18
        BYTE    $00, $00, $00, $00, $00, $00, $FF, $FF
        BYTE    $00, $00, $00, $1F, $1F, $18, $18, $18
        BYTE    $18, $18, $18, $FF, $FF, $00, $00, $00
        BYTE    $00, $00, $00, $FF, $FF, $18, $18, $18
        BYTE    $18, $18, $18, $F8, $F8, $18, $18, $18
        BYTE    $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
        BYTE    $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0
        BYTE    $07, $07, $07, $07, $07, $07, $07, $07
        BYTE    $FF, $FF, $00, $00, $00, $00, $00, $00
        BYTE    $FF, $FF, $FF, $00, $00, $00, $00, $00
        BYTE    $00, $00, $00, $00, $00, $FF, $FF, $FF
        BYTE    $01, $03, $06, $6C, $78, $70, $60, $00
        BYTE    $00, $00, $00, $00, $F0, $F0, $F0, $F0
        BYTE    $0F, $0F, $0F, $0F, $00, $00, $00, $00
        BYTE    $18, $18, $18, $F8, $F8, $00, $00, $00
        BYTE    $F0, $F0, $F0, $F0, $00, $00, $00, $00
        BYTE    $F0, $F0, $F0, $F0, $0F, $0F, $0F, $0F
        BYTE    $C3, $99, $91, $91, $9F, $99, $C3, $FF
        BYTE    $FF, $FF, $C3, $F9, $C1, $99, $C1, $FF
        BYTE    $FF, $9F, $9F, $83, $99, $99, $83, $FF
        BYTE    $FF, $FF, $C3, $9F, $9F, $9F, $C3, $FF
        BYTE    $FF, $F9, $F9, $C1, $99, $99, $C1, $FF
        BYTE    $FF, $FF, $C3, $99, $81, $9F, $C3, $FF
        BYTE    $FF, $F1, $E7, $C1, $E7, $E7, $E7, $FF
        BYTE    $FF, $FF, $C1, $99, $99, $C1, $F9, $83
        BYTE    $FF, $9F, $9F, $83, $99, $99, $99, $FF
        BYTE    $FF, $E7, $FF, $C7, $E7, $E7, $C3, $FF
        BYTE    $FF, $F9, $FF, $F9, $F9, $F9, $F9, $C3
        BYTE    $FF, $9F, $9F, $93, $87, $93, $99, $FF
        BYTE    $FF, $C7, $E7, $E7, $E7, $E7, $C3, $FF
        BYTE    $FF, $FF, $99, $80, $80, $94, $9C, $FF
        BYTE    $FF, $FF, $83, $99, $99, $99, $99, $FF
        BYTE    $FF, $FF, $C3, $99, $99, $99, $C3, $FF
        BYTE    $FF, $FF, $83, $99, $99, $83, $9F, $9F
        BYTE    $FF, $FF, $C1, $99, $99, $C1, $F9, $F9
        BYTE    $FF, $FF, $83, $99, $9F, $9F, $9F, $FF
        BYTE    $FF, $FF, $C1, $9F, $C3, $F9, $83, $FF
        BYTE    $FF, $E7, $81, $E7, $E7, $E7, $F1, $FF
        BYTE    $FF, $FF, $99, $99, $99, $99, $C1, $FF
        BYTE    $FF, $FF, $99, $99, $99, $C3, $E7, $FF
        BYTE    $FF, $FF, $9C, $94, $80, $C1, $C9, $FF
        BYTE    $FF, $FF, $99, $C3, $E7, $C3, $99, $FF
        BYTE    $FF, $FF, $99, $99, $99, $C1, $F3, $87
        BYTE    $FF, $FF, $81, $F3, $E7, $CF, $81, $FF
        BYTE    $C3, $CF, $CF, $CF, $CF, $CF, $C3, $FF
        BYTE    $F3, $ED, $CF, $83, $CF, $9D, $03, $FF
        BYTE    $C3, $F3, $F3, $F3, $F3, $F3, $C3, $FF
        BYTE    $FF, $E7, $C3, $81, $E7, $E7, $E7, $E7
        BYTE    $FF, $EF, $CF, $80, $80, $CF, $EF, $FF
        BYTE    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        BYTE    $E7, $E7, $E7, $E7, $FF, $FF, $E7, $FF
        BYTE    $99, $99, $99, $FF, $FF, $FF, $FF, $FF
        BYTE    $99, $99, $00, $99, $00, $99, $99, $FF
        BYTE    $E7, $C1, $9F, $C3, $F9, $83, $E7, $FF
        BYTE    $9D, $99, $F3, $E7, $CF, $99, $B9, $FF
        BYTE    $C3, $99, $C3, $C7, $98, $99, $C0, $FF
        BYTE    $00, $00, $00, $00, $10, $00, $00, $00
        BYTE    $F3, $E7, $CF, $CF, $CF, $E7, $F3, $FF
        BYTE    $CF, $E7, $F3, $F3, $F3, $E7, $CF, $FF
        BYTE    $FF, $99, $C3, $00, $C3, $99, $FF, $FF
        BYTE    $FF, $E7, $E7, $81, $E7, $E7, $FF, $FF
        BYTE    $FF, $FF, $FF, $FF, $FF, $E7, $E7, $CF
        BYTE    $FF, $FF, $FF, $81, $FF, $FF, $FF, $FF
        BYTE    $FF, $FF, $FF, $FF, $FF, $E7, $E7, $FF
        BYTE    $FF, $FC, $F9, $F3, $E7, $CF, $9F, $FF
        BYTE    $C3, $99, $91, $89, $99, $99, $C3, $FF
        BYTE    $E7, $E7, $C7, $E7, $E7, $E7, $81, $FF
        BYTE    $C3, $99, $F9, $F3, $CF, $9F, $81, $FF
        BYTE    $C3, $99, $F9, $E3, $F9, $99, $C3, $FF
        BYTE    $F9, $F1, $E1, $99, $80, $F9, $F9, $FF
        BYTE    $81, $9F, $83, $F9, $F9, $99, $C3, $FF
        BYTE    $C3, $99, $9F, $83, $99, $99, $C3, $FF
        BYTE    $81, $99, $F3, $E7, $E7, $E7, $E7, $FF
        BYTE    $C3, $99, $99, $C3, $99, $99, $C3, $FF
        BYTE    $C3, $99, $99, $C1, $F9, $99, $C3, $FF
        BYTE    $FF, $FF, $E7, $FF, $FF, $E7, $FF, $FF
        BYTE    $FF, $FF, $E7, $FF, $FF, $E7, $E7, $CF
        BYTE    $F1, $E7, $CF, $9F, $CF, $E7, $F1, $FF
        BYTE    $FF, $FF, $81, $FF, $81, $FF, $FF, $FF
        BYTE    $8F, $E7, $F3, $F9, $F3, $E7, $8F, $FF
        BYTE    $C3, $99, $F9, $F3, $E7, $FF, $E7, $FF
        BYTE    $FF, $FF, $FF, $00, $00, $FF, $FF, $FF
        BYTE    $E7, $C3, $99, $81, $99, $99, $99, $FF
        BYTE    $83, $99, $99, $83, $99, $99, $83, $FF
        BYTE    $C3, $99, $9F, $9F, $9F, $99, $C3, $FF
        BYTE    $87, $93, $99, $99, $99, $93, $87, $FF
        BYTE    $81, $9F, $9F, $87, $9F, $9F, $81, $FF
        BYTE    $81, $9F, $9F, $87, $9F, $9F, $9F, $FF
        BYTE    $C3, $99, $9F, $91, $99, $99, $C3, $FF
        BYTE    $99, $99, $99, $81, $99, $99, $99, $FF
        BYTE    $C3, $E7, $E7, $E7, $E7, $E7, $C3, $FF
        BYTE    $E1, $F3, $F3, $F3, $F3, $93, $C7, $FF
        BYTE    $99, $93, $87, $8F, $87, $93, $99, $FF
        BYTE    $9F, $9F, $9F, $9F, $9F, $9F, $81, $FF
        BYTE    $9C, $88, $80, $94, $9C, $9C, $9C, $FF
        BYTE    $99, $89, $81, $81, $91, $99, $99, $FF
        BYTE    $C3, $99, $99, $99, $99, $99, $C3, $FF
        BYTE    $83, $99, $99, $83, $9F, $9F, $9F, $FF
        BYTE    $C3, $99, $99, $99, $99, $C3, $F1, $FF
        BYTE    $83, $99, $99, $83, $87, $93, $99, $FF
        BYTE    $C3, $99, $9F, $C3, $F9, $99, $C3, $FF
        BYTE    $81, $E7, $E7, $E7, $E7, $E7, $E7, $FF
        BYTE    $99, $99, $99, $99, $99, $99, $C3, $FF
        BYTE    $99, $99, $99, $99, $99, $C3, $E7, $FF
        BYTE    $9C, $9C, $9C, $94, $80, $88, $9C, $FF
        BYTE    $99, $99, $C3, $E7, $C3, $99, $99, $FF
        BYTE    $99, $99, $99, $C3, $E7, $E7, $E7, $FF
        BYTE    $81, $F9, $F3, $E7, $CF, $9F, $81, $FF
        BYTE    $E7, $E7, $E7, $00, $00, $E7, $E7, $E7
        BYTE    $3F, $3F, $CF, $CF, $3F, $3F, $CF, $CF
        BYTE    $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7
        BYTE    $CC, $CC, $33, $33, $CC, $CC, $33, $33
        BYTE    $CC, $66, $33, $99, $CC, $66, $33, $99
        BYTE    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        BYTE    $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
        BYTE    $FF, $FF, $FF, $FF, $00, $00, $00, $00
        BYTE    $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF
        BYTE    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00
        BYTE    $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
        BYTE    $33, $33, $CC, $CC, $33, $33, $CC, $CC
        BYTE    $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
        BYTE    $FF, $FF, $FF, $FF, $33, $33, $CC, $CC
        BYTE    $33, $66, $CC, $99, $33, $66, $CC, $99
        BYTE    $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
        BYTE    $E7, $E7, $E7, $E0, $E0, $E7, $E7, $E7
        BYTE    $FF, $FF, $FF, $FF, $F0, $F0, $F0, $F0
        BYTE    $E7, $E7, $E7, $E0, $E0, $FF, $FF, $FF
        BYTE    $FF, $FF, $FF, $07, $07, $E7, $E7, $E7
        BYTE    $FF, $FF, $FF, $FF, $FF, $FF, $00, $00
        BYTE    $FF, $FF, $FF, $E0, $E0, $E7, $E7, $E7
        BYTE    $E7, $E7, $E7, $00, $00, $FF, $FF, $FF
        BYTE    $FF, $FF, $FF, $00, $00, $E7, $E7, $E7
        BYTE    $E7, $E7, $E7, $07, $07, $E7, $E7, $E7
        BYTE    $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
        BYTE    $1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
        BYTE    $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
        BYTE    $00, $00, $FF, $FF, $FF, $FF, $FF, $FF
        BYTE    $00, $00, $00, $FF, $FF, $FF, $FF, $FF
        BYTE    $FF, $FF, $FF, $FF, $FF, $00, $00, $00
        BYTE    $FE, $FC, $F9, $93, $87, $8F, $9F, $FF
        BYTE    $FF, $FF, $FF, $FF, $0F, $0F, $0F, $0F
        BYTE    $F0, $F0, $F0, $F0, $FF, $FF, $FF, $FF
        BYTE    $E7, $E7, $E7, $07, $07, $FF, $FF, $FF
        BYTE    $0F, $0F, $0F, $0F, $FF, $FF, $FF, $FF
        BYTE    $0F, $0F, $0F, $0F, $F0, $F0, $F0, $F0

; screen character data
*=$2800
        BYTE    $20, $20, $20, $20, $50, $0C, $01, $19, $05, $12, $20, $31, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $53, $03, $0F, $12, $05, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $3B, $20
        BYTE    $20, $20, $3B, $20, $20, $20, $2C, $20, $20, $20, $2E, $2E, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $3B, $20, $20, $20, $20, $20, $2E, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $3B, $20, $20, $2E, $20, $20, $20, $20, $2C, $20, $20, $3B, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20
        BYTE    $20, $20, $3B, $20, $20, $20, $2E, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $3B, $20, $20, $20
        BYTE    $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $2E, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $2C
        BYTE    $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $3B, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $2E, $2E, $20, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $2E, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $3B, $20, $3B, $20, $20, $20, $2E, $20, $20
        BYTE    $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $2E, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $3B, $20, $20, $2C, $20, $20, $2E, $20, $20, $20, $20, $20
        BYTE    $20, $20, $2E, $2E, $20, $20, $20, $2C, $20, $20, $20, $3B, $20, $20, $20, $3B, $20, $20, $2C, $2C, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2C, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20
        BYTE    $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $2E, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $20, $20, $20, $20, $20, $3B, $20, $20, $20, $2E, $20, $20, $20
        BYTE    $20, $20, $20, $20, $2E, $20, $20, $20, $20, $3B, $20, $20, $3B, $20, $20, $20, $20, $20, $2C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
        BYTE    $20, $20, $53, $10, $01, $03, $05, $20, $47, $01, $0D, $05, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $4D, $05, $14, $05, $20, $53, $05, $16, $20, $20

; screen color data
*=$2be8
        BYTE    $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $08, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $08, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $08, $0E
        BYTE    $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $08, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $08, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $08, $0E, $0E, $0E, $0E, $01, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $01, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $08, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $07, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
        BYTE    $0E, $07, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $07, $07