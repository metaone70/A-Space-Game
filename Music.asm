; 10 SYS (2064)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

*=$0810

playmusıc
          
    seı
    lda #<ırq
    ldx #>ırq
    sta $314
    stx $315
    lda #$1b
    ldx #$00
    ldy #$7f 
    sta $d011
    stx $d012
    sty $dc0d
    lda #$01
    sta $d01a
    sta $d019
    lda #$03
    jsr $3000
    clı
        rts
 

ırq
    lda #$01
    asl $d019 
    jsr $3003
    jmp $ea31


*=$3000
incbin "music.bin",2


;MUSIC_IN_GAME_TUNE        = $00
;MUSIC_TITLE_TUNE              = $01
;MUSIC_GET_READY_GAME_OVER_TUNE  = $02 ;Also use this for Game Over!
;MUSIC_GAME_END_TUNE     = $03
;MUSIC_PLAYER_SHOOT      = $04
;MUSIC_PLAYER_DIE        = $05
;MUSIC_PICKUP            = $06
;MUSIC_ENEMY_DIE         = $07
