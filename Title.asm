; 10 SYS2064

*=$0801
        BYTE    $0B, $08, $0A, $00, $9E, $32, $30, $36, $34, $00, $00, $00

*=$0810

         lda $d011       ; enable bitmap mode
        ora #32
        sta $d011

        lda $d016       ; enable multicolor mode
        ora #16
        sta $d016
        
        lda #$78        ; select screen mem -> screenmem is at $1c00
        sta $d018

        lda $dd00       ; switch to VIC bank #1
        and #252
        ora #3-1
        sta $dd00

loadimage
      lda $7f40,x
      sta $5c00,x
      lda $8040,x
      sta $5d00,x
      lda $8140,x
      sta $5e00,x
      lda $8228,x
      sta $5ee8,x
      lda $8328,x
      sta $d800,x
      lda $8428,x
      sta $d900,x
      lda $8528,x
      sta $da00,x
      lda $8610,x
      sta $dae8,x
      ınx
      bne loadimage

ınput   jsr $ffe4
        beq ınput
        sta key
        cmp #$20
        bne ınput     
          
        lda #$1b
        sta $d011

        lda #$c8
        sta$d016                

        lda #$15
        sta $d018

        lda #$c7
        sta $dd00 
        brk

key     byte 0

*=$6000
incbin "pixel_space61.PRG",2


          