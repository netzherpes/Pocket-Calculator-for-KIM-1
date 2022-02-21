; Target assembler: cc65 v2.18.0 [--target none -C calc_0-ende.bin_cc65.cfg]
;***************************************
;*  THE FOLLOWING PROGRAM IS           *
;*  POCKET CALCULATOR                  *
;*                                     *
;*  INPUT/OUTPUT IS EITHER TELETYPE    *
;*  OR KIM KEYBOARD AND DISPLAY        *
;*                                     *
;*  INPUT IS GIVEN BY PRESSING THE     *
;*  KEYS FOR A DECIMAL NUMBER          *
;*  FOLLOWED BY A FUNCTIONKEY          *
;*                                     *
;*  FUNCTIONS:                         *
;*  A = + = ADD NUMBER TO RESULT       *
;*  B = - = SUBTRACT NUMBER FROM       *
;* RESULT                              *
;*  C = * = MULTIPLY RESULT BY NUMBER  *
;*  D = / = DIVIDE RESULT BY NUMBER    *
;*  E = C = CLEAR INPUT NUMBER         *
;*  F = A = CLEAR RESULT               *
;*  AD = R = REMAINDER OF LAST         *
;* DIVISION                            *
;*  DA = I = NUMBER STORED IN MEMORY   *
;*  + = O = NUMBER FROM MEMORY         *
;*  PC = % = CALCULATE PERCENTAGE      *
;*  GO = C = CLEAR RESULT              *
;*  THE PROGRAM MAKES USE OF THE       *
;*  INTEGER CALCULATION PACKAGE        *
;*  'INTCAL' PARTNO 770110.            *
;*  A 3-BYTE VERSION IS USED.          *
;*  WHENEVER A DIFFERENT               *
;*  NUMBER OF BYTES PER                *
;*  NUMBER IS REQUIRED, THE            *
;*  DEFINITIONS CONTAINING             *
;*  SIZE, SIZEA, SIZEB, SIZEC,         *
;*  ACCU, ACCMSB, ACCLSB,              *
;*  DACCU, DACLSB AND LOADAD           *
;*  HAVE TO BE CHANGED TO THE          *
;*  PROPER VALUE IN RELATION TO        *
;*  SIZE AND THE PROGRAM MUST BE       *
;*  REASSEMBLED.                       *
;*                                     *
;*  AUTHOR: SIEP DE VRIES              *
;*     BRUGSTRAAT 32                   *
;*     LIMMEN (NH)                     *
;*     THE NETHERLANDS                 *
;***************************************
         .setcpu "6502"
;kimroutines
piadat   =       $1740
nmitrp   =       $17fa
initrp   =       $17fe
getch    =       $1e5a
outch    =       $1ea0
scands   =       $1f1f
getkey   =       $1f6a

;memlayout
data     =       $f9
memry 	 =       $7a
numcar	 = 		 memry +3
prev     =  	 numcar+1
accu 	 =		 prev+1
accmsb   = 		 accu-3
acclsb 	 =		 accu+3
daccu 	 =		 accu+6
daclsb 	 =		 daccu+2
loadad 	 =	     daccu+6
accsav 	 = 		 loadad+2
mulind 	 =		 accsav+1
mulcnt   = 	     mulind+1
count 	 =		 mulcnt+1

; definitions
size 	 = 		$03
sizea	 = 		size-1
sizeb	 = 		size+3
sizec    = 		size+2		 
;buttons
plus	 =		$2b
min		 = 		$2d
maal	 =		$2a
deel	 =		$2f
clrnum	 =		$43
clrall	 =		$41
rep 	 = 		$52
memin	 =		$49
memout 	 =		$4f
prcnt	 = 		$25
vraag	 = 		$3f


         .org    $0000
start:   cld
         sei
         lda     #$00
         sta     initrp
         sta     nmitrp
         lda     #$1c
         sta     $17ff
         sta     $17fb
         ldy     #$00
         ldx     #null
         jsr     load
         ldx     #memry
         jsr     store
getinp:  jsr     input
weder:   ldx     #data
         ldy     #$00
         cmp     #plus
         bne     noadd
         jsr     add
         bcs     problm
noadd:   cmp     #min
         bne     nomin
         jsr     sub
         bcs     problm
nomin:   cmp     #maal
         bne     nomaal
         jsr     mpy
         bcs     problm
nomaal:  cmp     #deel
         bne     nodeel
         jsr     dvi
         bcs     problm
nodeel:  cmp     #clrall
         bne     nocler
clear:   ldx     #null
         jsr     load
nocler:  cmp     #prcnt
         bne     noper
         jsr     mpy
         bcs     problm
         ldx     #eenhon
         jsr     dvi
         bcs     problm
noper:   cmp     #rep
         bne     norep
         ldx     #daccu
         jsr     load
norep:   jmp     getinp

problm:  jsr     error
         jmp     getinp

null:    .byte   $00
         .byte   $00
         .byte   $00
eenhon:  .byte   $00
         .byte   $01
         .byte   $00

        
	     .org    $0200
input:   ldx     #data
         ldy     #$00
         jsr     store
weronp:  jsr     output
         lda     #$00
         sta     numcar
werinp:  jsr     inchar
         cmp     #clrnum
         beq     input
         cmp     #$49
         bne     nomemi
         lda     data
         sta     memry
         lda     data+1
         sta     memry+1
         lda     data+2
         sta     accmsb
         jmp     weronp

nomemi:  cmp     #$4f
         bne     tesdec
         lda     memry
         sta     data
         lda     memry+1
         sta     data+1
         lda     accmsb
         sta     data+2
         jmp     weronp

tesdec:  cmp     #$30
         bpl     godec
nodec:   rts

godec:   cmp     #$3a
         bpl     nodec
         ldx     numcar
         bne     notfrs
         ldy     #$00
         sty     data
         sty     data+1
         sty     data+2
notfrs:  sec
         sbc     #$30
         rol     A
         rol     A
         rol     A
         rol     A
         ldx     #$04
rolwer:  rol     A
         rol     data
         rol     data+1
         rol     data+2
         dex
         bne     rolwer
         inc     numcar
         lda     numcar
         cmp     #$07
         bmi     werinp
         jsr     error
         jmp     input

inchar:  lda     #$01
         bit     piadat
         beq     ttyin
punta:   jsr     scands
         bne     punta
puntb:   jsr     scands
         beq     puntb
         jsr     getkey
         cmp     #$15
         bpl     punta
         tax
         lda     asctab,x
         rts

ttyin:   jsr     getch
         rts

output:  lda     #$01
         bit     piadat
         beq     itstty
         rts

itstty:  ldx     #sizea
wtyp:    lda     data,x
         jsr     $1e3b
         dex
         bpl     wtyp
outyp:   jsr     $1e2f
         rts

error:   lda     #$01
         bit     piadat
         beq     ttyerr
         ldx     #sizea
         lda     #$ff
setful:  sta     accu,x
         dex
         bpl     setful
         rts

ttyerr:  lda     #vraag
         jsr     outch
         jmp     outyp

     
		 .org    $0300
begalg:  stx     loadad
         sty     loadad+1
         sta     accsav
         ldx     #sizea
         ldy     #$00
clrwer:  sty     acclsb,x
         dex
         bpl     clrwer
         ldx     #sizea
         sed
         rts

load:    jsr     begalg
nload:   lda     (loadad),y
         sta     accu,y
         iny
         dex
         bpl     nload
algout:  cld
         ldx     loadad
         ldy     loadad+1
         lda     accsav
         rts

store:   jsr     begalg
nstor:   lda     accu,y
         sta     (loadad),y
         iny
         dex
         bpl     nstor
         bmi     algout

add:     jsr     begalg
         clc
nadd:    lda     accu,y
         adc     (loadad),y
         sta     accu,y
         iny
         dex
         bpl     nadd
         bmi     algout

sub:     jsr     begalg
         sec
nsub:    lda     accu,y
         sbc     (loadad),y
         sta     accu,y
         iny
         dex
         bpl     nsub
         bcs     clicar
setcar:  sec
         bcs     algout

clicar:  clc
         bcc     algout

mpy:     stx     loadad
         sty     loadad+1
         sta     accsav
         ldx     #sizeb
wimpy:   lda     prev,x
         sta     acclsb+2,x
         lda     #$00
         sta     prev,x
         dex
         bne     wimpy
         tay
         lda     #size
         sta     mulcnt
         sed
outlup:  lda     (loadad),y
         sta     mulind
         beq     noad
peuter:  clc
         lda     #sizeb
         sta     count
         ldx     #$00
weradd:  lda     accu,x
         adc     daccu,x
         sta     accu,x
         inx
         dec     count
         bne     weradd
ulgo:    bcs     algout
         lda     mulind
         sbc     #$00
         sta     mulind
         bne     peuter
noad:    ldx     #sizec
wustur:  lda     acclsb+2,x
         sta     daccu,x
         dex
         bne     wustur
         stx     daccu
         iny
         dec     mulcnt
         bne     outlup
pulgo:   beq     clicar

dvi:     stx     loadad
         sty     loadad+1
         sta     accsav
         ldx     #size
         stx     mulind
         ldx     #sizec
movdiv:  lda     accu,x
         sta     daccu,x
         lda     #$00
         sta     accu,x
         dex
         bpl     movdiv
divlup:  ldy     #sizea
         ldx     mulind
teswer:  lda     daclsb,x
         cmp     (loadad),y
         bcc     findiv
         lda     daclsb,x
         beq     nocomp
         ldx     mulind
         ldy     #$00
         lda     #size
         sta     mulcnt
         sed
divre:   lda     daccu,x
         sbc     (loadad),y
         sta     daccu,x
         inx
         iny
         dec     mulcnt
         bne     divre
         lda     accmsb,x
         cmp     #$99
         bcs     ulgo
         adc     #$01
         sta     accmsb,x
         bne     divlup
nocomp:  dex
         dey
         bpl     teswer
findiv:  dec     mulind
         bpl     divlup
         jmp     clicar

asctab:  .byte   "0123456789+-*/CARIO%A"
