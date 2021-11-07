; 10 SYS (2064)

*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $36, $34, $29, $00, $00, $00

*=$0810


sid = 54272


playsound
          
        ldx #0
loop    lda reg,x
        sta sid,x
        ınx
        cpx #25
        bne loop   

ınput   jsr $ffe4
        beq ınput
        sta key
        cmp #$20
        bne ınput

        lda #0
        sta sid+4
        sta sid+11
        sta sid+18
        
        rts

key     byte 0

reg     byte 38,163,167,251,00,40,218,213,10
        byte 40,242,129,61,58,248,23,190,13,00
        byte 210,238,50,22,115,168




; 0 fine tuning note
; 1 coarse tuning note
; 2 fine adj of pulse width
; 3 coarse adj of pulse width
; 4 waveform select and gate register
; 5 AD envelope
; 6 SR envelope
; 7 - 13 Same order for Voice 2
; 14 - 20 Voice 3
; 21 fine cut off freq
; 22 coarse cut off freq
; 23 resonance
; 24 master volume & mode select
; 25 - 26 paddle
; 27 oscillator random number gen
; 28 envelop gen 3 


