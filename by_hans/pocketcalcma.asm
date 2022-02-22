;  The following program is a pocket calculator                         
;                                              
;  Input/output is either teletype or KIM keyboard and display                 
;                                              
;  Input is given by pressing the keys for a decimal number                   
;  followed by a function key                   
;                                              
;  Functions                                  
;  A = + = add number to result                
;  B = - = subtract number from result         
;  C = * = multiply result by number           
;  D = / = divide result by number             
;  E = c = clear input number                  
;  F = A = clear result                        
; AD = r = remainder of last division          
; DA = i = number stored in memory             
;  + = c = number from memory                  
;  PC = % = calculate percentage               
;  GO = C = clear result                       
;
;  The program makes use of the integer calculation package                 
;  'intcal' partno 770110.                     
;  A 3-byte version is used.                   
;
;  whenever a different number of bytes per                         
;  number is required, the definitions containing                      
;  size, sizea, sizeb, sizec,                  
;  accu, accmsb, acclsb,                       
;  accsav, daclsb and loadad                    
;  have to be changed to the proper value in relation to                 
;  size and the program must be reassembled.                                
;                                              
;  Author Siep de Vries                       
;     Limmen (NH)                              
;     The Netherlands   
;
; Typed in and tested from listing in KIM Kenner (Micro Ade version), 2021-2022 Hans Otten                       
;
;
; Definitions
;
size    = 3
sizea   = size - 1      ; size - 1
sizeb   = size + 3      ; size * 2
sizec   = size + 2      ; (size * 2) - 1 
piadat  = $1740         ; PIA to test KIM/TTY
initrp  = $17fe         ; Non-maskable interrupt trap
nmitrp  = $17fa         ; interrupt break trap
;
; General subroutines
;
scands  = $1f1f         ; display data on LED display
getkey  = $1f6a         ; read data form hex keypad
outch   = $1ea0         ; Print character on TTY
getch   = $1e5a         ; Read character from TTY
crlf    = $1e2F         ; Print Carriage Return/LineFeed
pribyt  = $1e3B         ; Print byte as 2 digits
monitr  = $1c00         ; KIM-1 monitor entry point
;
; character set
;
plus    = $2b
min     = $2d
maal    = $2a           ; multiply
deel    = $2f           ; divide
clrnum  = $43
clrall  = $41
rep     = $52
memin   = $49
memout  = $4f
prcnt   = $25
vraag   = $3f           ; question
;
; Page zero definitions
;
        .org $74
null    .byte $00
        .byte $00
        .byte $00
eenhon  .byte $00
        .byte $01
        .byte $00
memry   .byte $00
        .byte $00
        .byte $00
numcar  .byte $00
prev    .byte $00
accu    .byte $00
accmsb  = accu - 3      ; accu - size 
acclsb  = accu + 3      ; accu + size
daccu   = accu + 6      ; accu + 2*size
daclsb  = daccu + 2     ; accsav + size -1
loadad  = daccu + 6     ; accsav + 2*size
accsav  = loadad + 2
mulind  = accsav + 1
mulcnt  = mulind + 1
count   = mulcnt + 1
data    = $F9 
;
; start of program
;
        .org    $0000
start   cld              ; initialize
        sei
        lda #monitr & 256
        sta initrp
        sta nmitrp
        lda #monitr / 256
        sta initrp + 1
        sta nmitrp
        ldy #$00        ; Result and memory are both zero
        ldx #null
        jsr load
        ldx #memry
        jsr store
getinp  jsr input       ; read number + function
weder   ldx #data
        ldy #$00
        cmp #plus
        bne noadd
;        
        jsr add         ; add operand
        bcs  problm     ; carry set in error
noadd   cmp #min
        bne nomin
;
        jsr sub         ; subtract operand
        bcs problm      ; carry clear is error 
nomin   cmp #maal        ; multiply
        bne nomaal
;
        jsr mpy         ; multiply operand
        bcs problm      ; carry set is error
nomaal  cmp #deel
        bne nodeel
;
        jsr dvi         ; divide by operand
        bcs problm      ; carry clear is error
nodeel  cmp #clrall     
        bne nocler
clear   ldx #null
        jsr load
nocler  cmp #prcnt
        bne noper
        jsr mpy         ; multiply by percentage
        bcs problm
        ldx #eenhon
;
        jsr dvi         ; divide by one hundred
        bcs problm
;
noper   cmp #rep
        bne norep
        ldx #accsav
        jsr load
norep   jmp getinp
;
problm  jsr error
        jmp getinp
;
		 .org 	 $0200
;
; subroutine to input data + function
;
; the result is shown,
; a number can be keyed in
; followed by a non-numerical key
; which causes a return
;
; clear input, store in memory 
; and get from memory are
; processed in this routine
;

input   ldx #data       ; perform output
        ldy #$00        ; first
        jsr store
weronp  jsr output      ; clear digit counter
        lda #$00
        sta numcar
werinp  jsr inchar      ; read next character
        cmp #clrnum     ; clear number 
        beq input       ; re-display result
        cmp #memin
        bne nomemi
        lda data        ; memory in display number
        sta  memry
        lda data + 1
        sta memry + 1
        lda data + 2
        sta memry + 2
        jmp weronp
;
nomemi  cmp #memout     ; number comes from memory
        bne tesdec
        lda memry       ; display it
        sta data
        lda memry + 1
        sta data + 1
        lda memry + 2
        sta data + 2
        jmp weronp

tesdec  cmp #'0'
        bpl godec
nodec   rts             ; leave character unknown
godec   cmp #$3a
        bpl nodec
        ldx numcar      ; it is deciaml
        bne notfrs
        ldy #$00        ; first digit clears number
        sty data
        sty data + 1
        sty data + 2
notfrs  sec
        sbc #'0'        ; obtain value
        rol A           ; digit left positioned 
        rol A           ; in acumulator
        rol A
        rol A
        ldx #$04
rolwer  rol A           ; shift digit in
        rol data 
        rol data + 1
        rol data + 2
        dex
        bne rolwer
        inc numcar
        lda numcar
        cmp #$07    ; test if too many digits
        bmi werinp
        jsr error
        jmp input
;
; subroutine to read a character from KIM or TTY
; output is in ascii
;
inchar  lda #$01
        bit piadat      ; test if KIM 
        beq ttyin
punta   jsr scands      ; wait for no key
        bne punta
puntb   jsr scands      ; wait for a key
        beq puntb
        jsr getkey
        cmp #$15        
        bpl punta
        tax
        lda asctab,x    ; convert to ascii
        rts
ttyin   jsr getch       ; tty is easier
         rts
;
; output number from data area to tty if connected
;
output  lda #$01
        bit piadat
        beq itstty
        rts
itstty  ldx #$02
wtyp    lda data,x
        jsr pribyt
        dex
        bpl wtyp
outyp   jsr crlf
        rts
;
; error subroutine
;
error   lda #$01
        bit piadat
        beq ttyerr
        ldx #sizea
        lda #$ff
setful  sta accu,x
        dex
        bpl setful
        rts
ttyerr  lda #vraag
        jsr outch
        jmp outyp

        .org 	 $0300 
;
; integer package 'INTCAL'
; #770110 
;
; Copyright (C) 1977, 1982
; Westvries Computer Consulting B.V.
; The Netherlands      
;
; Functions provided:
; Load, Store, Add, Subtract, Multiply, Divide
;
begalg  stx loadad      ; General setup and initialize
        sty loadad + 1
        sta accsav
        ldx #sizea
        ldy #$00
clrwer  sty acclsb,x
        dex
        bpl clrwer
        ldx #sizea
        sed
        rts
load    jsr begalg      ; load into accu
nload   lda (loadad),y
        sta accu,y
        iny
        dex
        bpl nload
algout  cld             ; general exit
        ldx loadad      ; restore registers
        ldy loadad + 1
        lda accsav
        rts
store   jsr begalg      ; store from accu
nstor   lda accu,y
        sta (loadad),y
        iny
        dex
        bpl nstor
        bmi algout
add     jsr begalg      ; add into accu
        clc
nadd    lda accu,y
        adc (loadad),y
        sta accu,y
        iny
        dex
        bpl nadd
        bmi algout
sub     jsr begalg      ; subtract from accu
        sec
nsub    lda accu,y
        sbc (loadad),y
        sta accu,y
        iny
        dex
        bpl nsub
        bcs clicar
setcar  sec
        bcs algout
clicar  clc
        bcc algout
mpy     stx loadad
        sty loadad + 1
        sta accsav
        ldx #sizeb
wimpy   lda accu - 1, X
        sta daccu - 1,x
        lda #$00
        sta accu - 1, X
        dex
        bne wimpy
        tay
        lda #size
        sta mulcnt
        sed
outlup  lda (loadad),y
        sta mulind
        beq noad
peuter  clc
        lda #sizeb
        sta count
        ldx #$00
weradd  lda accu,x
        adc daccu,x
        sta accu,x
        inx
        dec count
        bne weradd
ulgo    bcs algout
        lda mulind
        sbc #$00
        sta mulind
        bne peuter
noad    ldx #sizec
wustur  lda daccu - 1,x
        sta daccu,x
        dex
        bne wustur
        stx daccu
        iny
        dec mulcnt
        bne outlup
pulgo   beq clicar
dvi     stx loadad
        sty loadad + 1
        sta accsav
        ldx #size
        stx mulind
        ldx #$05
movdiv  lda accu,x
        sta daccu,x
        lda #$00
        sta accu,x
        dex
        bpl movdiv
divlup  ldy #sizea
        ldx mulind
teswer  lda daclsb,x
        cmp (loadad),y
        bcc findiv
        lda daclsb,x
        beq nocomp
        ldx mulind
        ldy #$00
        lda #size
        sta mulcnt
        sed
divre   lda daccu,x
        sbc (loadad),y
        sta daccu,x
        inx
        iny
        dec mulcnt
        bne divre
        lda accmsb,x
        cmp #$99
        bcs ulgo
        adc #$01
        sta accmsb,x
        bne divlup
nocomp  dex
        dey
        bpl teswer
findiv  dec mulind
        bpl divlup
        jmp clicar

asctab  .byte '0'       ; 0
        .byte '1'       ; 1
        .byte '2'       ; 2
        .byte '3'       ; 3
        .byte '4'       ; 4
        .byte '5'       ; 5
        .byte '6'       ; 6
        .byte '7'       ; 7
        .byte '8'       ; 8
        .byte '9'       ; 9
        .byte plus      ; A
        .byte min       ; B
        .byte maal      ; C
        .byte deel      ; D
        .byte clrnum    ; E
        .byte clrall    ; F
        .byte rep       ; AD
        .byte memin     ; DA
        .byte memout    ; PC
        .byte prcnt     ; +
        .byte clrall    ; GO
       
                .end
