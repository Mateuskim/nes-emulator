;----------------------------------------------------------------
; constants
;----------------------------------------------------------------

PRG_COUNT = 1 ;1 = 16KB, 2 = 32KB
MIRRORING = %0001 ;%0000 = horizontal, %0001 = vertical, %1000 = four-screen

;----------------------------------------------------------------
; variables
;----------------------------------------------------------------

   .enum $0000

   ;NOTE: declare variables using the DSB and DSW directives, like this:

   ;MyVariable0 .dsb 1
   ;MyVariable1 .dsb 3

   .ende

   ;NOTE: you can also split the variable declarations into individual pages, like this:

   ;.enum $0100
   ;.ende

   ;.enum $0200
   ;.ende

;----------------------------------------------------------------
; iNES header
;----------------------------------------------------------------

   .db "NES", $1a ;identification of the iNES header
   .db PRG_COUNT ;number of 16KB PRG-ROM pages
   .db $01 ;number of 8KB CHR-ROM pages
   .db $00|MIRRORING ;mapper 0 and mirroring
   .dsb 9, $00 ;clear the remaining bytes

;----------------------------------------------------------------
; program bank(s)
;----------------------------------------------------------------

   .base $10000-(PRG_COUNT*$4000)

.org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x    ;move all sprites off screen
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002    ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006    ; write the high byte of $3F00 address
  LDA #$00
  STA $2006    ; write the low byte of $3F00 address
  LDX #$00
LoadPalettesLoop:
  LDA palette, x        ;load palette byte
  STA $2007             ;write to PPU
  INX                   ;set index to next byte
  CPX #$20            
  BNE LoadPalettesLoop  ;if x = $20, 32 bytes copied, all done



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

Forever: 
   JMP Forever     ;jump back to Forever, infinite loop

NMI:

  LDA #$00
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer

LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons

ReadA: 
  LDA $4016       ; player 1 - A
  ; AND #%00000001  ; only look at bit 0
  ; BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
  ;                 ; add instructions here to do something when button IS pressed (1)
  ; LDA $0203       ; load sprite X position
  ; CLC             ; make sure the carry flag is clear
  ; ADC #$01        ; A = A + 1
  ; STA $0203       ; save sprite X position
ReadADone:        ; handling this button is done 

ReadB: 
  LDA $4016       ; player 1 - B
  ; AND #%00000001  ; only look at bit 0
  ; BEQ ReadBDone   ; branch to ReadBDone if button is NOT pressed (0)
  ;                 ; add instructions here to do something when button IS pressed (1)
  ; LDA $0203       ; load sprite X position
  ; SEC             ; make sure carry flag is set
  ; SBC #$01        ; A = A - 1
  ; STA $0203       ; save sprite X position
ReadBDone:        ; handling this button is done

LDA $4016     ; player 1 - Select
LDA $4016     ; player 1 - Start

ReadUP:
  LDA $4016     ; player 1 - UP
  AND #%00000001  ; erase everything but bit 0
  BEQ ReadUPDone   ; branch to ReadUPDone if button is NOT pressed (0)
  LDA $0200   ; load sprite position
  CMP #$07    ; end of up side
  BEQ ReadUPDone ; branch to ReadUPDone if position is end of up side
  ; SEC         ; make sure carry flag is set
  SBC #$01    ; A = A - 1
  STA $0200   ; save sprite position
  ; move the rest of the sprite
  LDA $0204
  SBC #$01    ; A = A - 1
  STA $0204   ; save sprite position
  LDA $0208
  SBC #$01    ; A = A - 1
  STA $0208   ; save sprite position
  LDA $020C
  SBC #$01    ; A = A - 1
  STA $020C   ; save sprite position
ReadUPDone:

ReadDown:
  LDA $4016     ; player 1 - Down
  AND #%00000001  ; erase everything but bit 0
  BEQ ReadDownDone   ; branch to ReadADone if button is NOT pressed (0)
  LDA $0200   ; load sprite position
  CMP #$D7    ; end of down side
  BEQ ReadDownDone ; branch to ReadADone if position is end of down side
  ; SEC         ; make sure carry flag is set
  ADC #$01    ; A = A + 1
  STA $0200   ; save sprite position
  ; move the rest of the sprite
  LDA $0204
  ADC #$01    ; A = A + 1
  STA $0204   ; save sprite position
  LDA $0208
  ADC #$01    ; A = A + 1
  STA $0208   ; save sprite position
  LDA $020C
  ADC #$01    ; A = A + 1
  STA $020C   ; save sprite position
ReadDownDone:

ReadLeft:
  LDA $4016     ; player 1 - Left
  AND #%00000001  ; erase everything but bit 0
  BEQ ReadLeftDone   ; branch to ReadADone if button is NOT pressed (0)
  LDA $0203   ; load sprite position
  CMP #$07    ; end of left side
  BEQ ReadLeftDone ; branch to ReadADone if position is end of left side
  ; SEC         ; make sure carry flag is set
  SBC #$01    ; A = A - 1
  STA $0203   ; save sprite position
  ; move the rest of the sprite
  LDA $0207
  SBC #$01    ; A = A - 1
  STA $0207   ; save sprite position
  LDA $020B
  SBC #$01    ; A = A - 1
  STA $020B   ; save sprite position
  LDA $020F
  SBC #$01    ; A = A - 1
  STA $020F   ; save sprite position

;   LDA $0201
;   CMP #$32
;   BEQ addR
;   JMP subR

; addR:
;   ADC #$04
;   STA $0201
;   LDA $0205
;   ADC #$04
;   STA $0205
;   LDA $0209
;   ADC #$04
;   STA $0209
;   LDA $020D
;   ADC #$04
;   STA $020D
;   JMP ReadLeftDone

; subR:
;   LDA $0201
;   SBC #$04
;   STA $0201
;   LDA $0205
;   ADC #$04
;   STA $0205
;   LDA $0209
;   ADC #$04
;   STA $0209
;   LDA $020D
;   ADC #$04
;   STA $020D
;   JMP ReadLeftDone

ReadLeftDone:

ReadRigth:
  LDA $4016     ; player 1 - Right
  AND #%00000001  ; erase everything but bit 0
  BEQ ReadRigthDone   ; branch to ReadADone if button is NOT pressed (0)
  
  LDA $0203   ; load sprite position
  CMP #$F1    ; end of rigth side
  BEQ ReadRigthDone ; branch to ReadADone if position is end of rigth side
  ; SEC         ; make sure carry flag is set
  ADC #$01     ; A = A + 1
  STA $0203   ; save sprite position
  LDX #$07
MoveRestRigth:
  LDA $0200, X
  ADC #$01     ; A = A + 1
  STA $0200, X
  TXA
  ADC #$04
  TAX
  CMP #$13
  BNE MoveRestRigth
ReadRigthDone:
  
  RTI        ; return from interrupt



IRQ:

   ;NOTE: IRQ code goes here

;----------------------------------------------------------------
; interrupt vectors
;----------------------------------------------------------------

.org $E000
palette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$1C,$15,$14,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3


   .org $fffa

   .dw NMI
   .dw RESET
   .dw IRQ

;----------------------------------------------------------------
; CHR-ROM bank
;----------------------------------------------------------------

.incbin "mario.chr"
