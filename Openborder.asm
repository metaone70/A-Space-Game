; 10 SYS (2080)
*=$0801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $32, $30, $38, $30, $29, $00, $00, $00


*=$820
sprdata = $2200                 ; within 16K address of vic's range based on bank
S = 1024                        ; screen location based on bank used
LB = 1
HB = 2

Init       SEI                  ; set bit, make the CPU ignore interrupt
           LDA #%01111111       ; switch off interrupt signals from CIA-1
           STA $DC0D
           LDA $DC0D            ; acknowledge pending interrupts from CIA-1
           LDA $DD0D            ; acknowledge pending interrupts from CIA-2

           LDX #<IRQ            ; set interrupt vectors routine below
           STX $0314
           LDX #>IRQ
           STX $0315

           LDX #sprdata/64      ; sprite data at $3000 (192 x 64)
           STX S+1016
           INX
           STX S+1017
           INX
           STX S+1018
           INX
           STX S+1019
           INX
           STX S+1020
           INX
           STX S+1021
           INX
           STX S+1022
           INX
           STX S+1023

           LDA #0
           STA slicer           ; in case user re-run
           STA sprindex         ; in case user re-run
           STA _thechar+LB      ; in case user re-run
           STA S+$3BFF          ; clear garbage under border $3FFF
           TAX                  ; make x a zero too
clearloop  STA sprdata,x
           STA sprdata+256,x    ; sprite spans 512bytes
           INX
           BNE clearloop        ; rolled past 255 to zero

           LDX #$2e             ; preset all vic registers
loop       LDA _D000,x          ; the label name
           STA $D000,x          ; the registers
           DEX
           BPL loop             ; until less than zero

           LDA $02a6            ; kernal sets this flag
           BEQ isntsc           ; it is ntsc
           LDA #$f7             ; is equal -1 under PAL system
           STA _palfix+LB
isntsc
           CLI                  ; clear interrupt flag
           RTS

;---------------------The Interupt Service Routine------------------------------

IRQ        ASL $D019            ; acknowledge the interrupt by clearing flag
_nextline  LDX #0-0             ; 249 or 27
           STX $D012            ; set next rasterline where interrupt occur
           BMI restore25        ; branch if bit 7 set
           LDX #249             ; value over 128 is a 'minus'
           STX  _nextline+LB
           LDX #$13             ; set 24 rows, has to be be done on line 249
           STX $D011            ; scrolly trick for no border
           JMP $EA31            ; exit to keyboard scan

restore25  LDX #$1B
           STX $D011            ; undo scroll on any other line
           LDX #50              ; line 50 is just 8 rows below start of sprites
           STX _nextline+LB     ; set rasterline where interrupt shall occu

;--------------------------Main Program-----------------------------------------

runalways  DEC $D000            ; sprite 1
           DEC $D002            ; sprite 2
           DEC $D004            ; sprite 3
           DEC $D006            ; sprite 4
           DEC $D008            ; sprite 5
           DEC $D00A            ; sprite 6
           DEC $D00C            ; sprite 7
           DEC $D00E            ; sprite 8

;----------------------Main Program Slicer Scheduler----------------------------

skip     = $EA81                ; no keyboard scan
taskexit = $EA81                ; no keyboard scan

           LDA slicer           ; goes from 0-47
           TAX                  ; move it to x
           INX                  ; our index counting up
           CPX #48              ; end of list
           BNE notend
           INC counter          ; master counter if a task needs it
           LDX #0
notend     STX slicer           ; set next return
           ASL A                ; double it as table are words
           TAX                  ; move it to x
           LDA jumptable,x
           STA thejump+LB
           LDA jumptable+1,x
           STA thejump+HB
thejump    JMP $1234-0          ; selfmod

;-------------------------------------------------------------------------------

sprsetup   LDA sprindex         ; first entry 321
           LSR A                ; 00000032 (1)
           ROR A                ; 10000003 (2)
           ROR A                ; 21000000 (3) the 3 is used in HB
           STA dest1+LB         ; 0, 64, 128 or 192
           STA dest2+LB
           STA dest3+LB
           LDA #>sprdata        ; get HB of sprite data loc
           ADC #0               ; use (if) carry from the 3
           STA dest1+HB         ; 0, 64, 128 or 192
           STA dest2+HB
           STA dest3+HB
           JMP taskexit

;-------------------------------------------------------------------------------

color      LDX #0-0             ; 0 to 7
           INX
           CPX #8
           BNE _notendcol
           LDX #0
_notendcol STX color+LB
           LDY #7
_colloop   LDA colortable,x
           STA $D027,y        ; sprite0 color + y
           INX
           DEY
           BPL _colloop
           JMP taskexit

colortable BYTE 1,15,12,11,11,12,15,1 , 1,15,12,11,11,12,15 ;repeat 7 bytes

;-------------------------------------------------------------------------------

gettext1   LDX #source1-source3 ; sprite is filled in from bottom right..
           BYTE $2C             ; the BIT opcode skip trick
gettext2   LDX #source2-source3 ; ...to top left so source3 is first
           BYTE $2C
gettext3   LDX #source3-source3 ; yes it would be zero
_thechar   LDY #0-0             ; char pointer
           LDA mytext,y         ; get char
           CMP #$ff             ; end of text so can use 0-to-254 petscii
           BNE _noendtxt        ; replace these two lines w/ BPL for cbm style
           LDA #32              ; replace w/ space, use AND #127 for cbm style
           LDY #-1              ; iny below makes it 0 max 256 charters for now
_noendtxt  INY
           STY _thechar+LB      ; next char on return
           TAY                  ; 87654321
           ASL A                ; 76543210 = x2
           ASL A                ; 65432100 = x4
           ASL A                ; 54321000 = x8
           STA source3+LB,x     ; store it in copyfonts 1 to 3
           TYA                  ; 87654321
           LSR A                ; 08765432
           LSR A                ; 00876543
           LSR A                ; 00087654
           LSR A                ; 00008765
           LSR A                ; 00000876
           ORA #$D0             ; char rom block start at $D000
           STA source3+HB,x     ; store it in copyfonts  1 to 3
           JMP taskexit

;-------------------------------------------------------------------------------

copyfonts  LDA #$33             ; make the CPU see the Character Generator ROM
           STA $01              ; at $D000
           LDX #7               ; copy chars to sprite in one swoop
           LDY #23              ; 3rd column 8th row in a sprite
source3    LDA $d018,x          ; do this while sprite is all under border
dest3      STA $3080,y
           DEY
source2    LDA $d010,x
dest2      STA $3080,y
           DEY
source1    LDA $d008,x
dest1      STA $3080,y
           DEY                  ; third dey
           DEX                  ; one dex
           BPL source3          ; if less than zero stop
           LDA #$37             ; switch in I/O mapped registers again
           STA $01              ; so vic can see them
           JMP taskexit

;-------------------------------------------------------------------------------

removmsb   LDA sprindex         ; after 32 scrolls a sprite crosses below msb
           TAY
           ASL A                ; double it
           TAX
           LDA #95              ; also move most left sprite
           STA $D000,x          ; to the right side as all under border
           LDA $D010
           AND remtable,y
           STA $D010
           JMP taskexit
remtable   BYTE %10111111, %01111111, %11111110, %11111101
           BYTE %11111011, %11110111, %11101111, %11011111

;-------------------------------------------------------------------------------

addmsb     LDA sprindex
           CLC
           ADC #1
           AND #7               ; so increse sprite index 0-7
           STA sprindex
           TAY                  ; backit up to y
           ASL A                ; double it
           TAX                  ; move it to x
_palfix    LDA #$ff             ; $f7 is equal -1 under PAL system
           STA $D000,x          ; x-pos was a $ff value
           LDA $D010            ; so under ntsc nothing change
           ORA addtable,y       ; now move the sprite to the negative range
           STA $D010            ; that is all the way to the left
           TXA
           JMP taskexit
addtable   BYTE %00000001, %00000010, %00000100, %00001000
           BYTE %00010000, %00100000, %01000000, %10000000

;-------------------------------------------------------------------------------
slicer   byte 0
sprindex byte 0
counter  byte 0                ; increases every full slicer

jumptable
 WORD   sprsetup,       color,          skip,           skip            ; 1-4
 WORD   skip,           color,          skip,           skip            ; 5-8
 WORD   gettext1,       color,          skip,           skip            ; 9-12
 WORD   gettext2,       color,          skip,           skip            ; 13-16
 WORD   gettext3,       color,          skip,           skip            ; 17-20
 WORD   copyfonts,      color,          skip,           skip            ; 21-24
 WORD   skip,           color,          skip,           skip            ; 25-28
 WORD   skip,           color,          skip,           removmsb        ; 29-32
 WORD   skip,           color,          skip,           skip            ; 33-36
 WORD   skip,           color,          skip,           skip            ; 37-40
 WORD   skip,           color,          skip,           skip            ; 41-44
 WORD   skip,           color,          skip,           addmsb          ; 45-48


_D000      BYTE 247,42          ; X and Y sprite 1  247 = -1 in pal
_D002      BYTE  47,42          ; X and Y sprite 2
_D004      BYTE  95,42          ; X and Y sprite 3
_D006      BYTE 143,42          ; X and Y sprite 4
_D008      BYTE 191,42          ; X and Y sprite 5
_D00A      BYTE 239,42          ; X and Y sprite 6
_D00C      BYTE  31,42          ; X and Y sprite 7
_D00E      BYTE  79,42          ; X and Y sprite 8
_D010      BYTE %11000001       ; MSBs of X coordinates
_D011      BYTE $1B             ; Control register 1
_D012      BYTE 249             ; Raster counter
_D013      BYTE 0               ; Light Pen X
_D014      BYTE 0               ; Light Pen Y
_D015      BYTE 255             ; Sprite enabled
_D016      BYTE $C8             ; Control register 2
_D017      BYTE 0               ; Sprite Y expansion
_D018      BYTE $14             ; Memory pointers
_D019      BYTE $ff             ; Interrupt register ACK all
_D01A      BYTE 1               ; interrupts enabled
_D01B      BYTE 0               ; Sprite data priority
_D01C      BYTE 0               ; Sprite multicolor
_D01D      BYTE 255             ; Sprite X expansion
_D01E      BYTE 0               ; Sprite-sprite collision
_D01F      BYTE 0               ; Sprite-data collision
_D020      BYTE 14              ; Border color
_D021      BYTE 6               ; Background color 0
_D022      BYTE 0               ; Background color 1
_D023      BYTE 0               ; Background color 2
_D024      BYTE 0               ; Background color 3
_D025      BYTE 0               ; Sprite multicolor 0
_D026      BYTE 0               ; Sprite multicolor 1
_D027      BYTE 1               ; Color sprite 0
_D028      BYTE 1               ; Color sprite 1
_D029      BYTE 1               ; Color sprite 2
_D02A      BYTE 1               ; Color sprite 3
_D02B      BYTE 1               ; Color sprite 4
_D02C      BYTE 1               ; Color sprite 5
_D02D      BYTE 1               ; Color sprite 6
_D02E      BYTE 1               ; Color sprite 7


mytext  TEXT  'HELLO THERE '
 BYTE 78,86,77
 TEXT' THIS IS A SPRITE SCROLLER IN THE BORDER   GO TO LEMON64.COM/FORUM SCENE   '
 TEXT' USES 8 X-EXPANDED SPRITES, A 48 SLICE SCHEDULER RUNS SNIPPETS DURING RASTER IRQ   '
 BYTE 159,32,32,32,32,32, $FF