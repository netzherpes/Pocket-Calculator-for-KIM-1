
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
;*  + = C = NUMBER FROM MEMORY         *
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


piadat   =       $1740
mnitrp   =       $17fa
initrp   =       $17fe
getch    =       $1e5a
dutch    =       $1ea0
scands   =       $1f1f
getkey   =       $1f6a

         .org    $0000
start:   cld
         sei
         lda     #$00
         sta     initrp
         sta     mnitrp
         lda     #$1c
         sta     $17ff
         sta     $17fb
         ldy     #$00
         ldx     #$74
         jsr     load
         ldx     #$7a
         jsr     store
getinp:  jsr     input
weder:   ldx     #$f9
         ldy     #$00
         cmp     #$2b
         bne     noadd
         jsr     add
         bcs     problm
noadd:   cmp     #$2d
         bne     nomin
         jsr     sub
         bcs     problm
nomin:   cmp     #$2a
         bne     nomaal
         jsr     mpy
         bcs     problm
nomaal:  cmp     #$2f
         bne     nodeel
         jsr     dvi
         bcs     problm
nodeel:  cmp     #$41
         bne     nocler
clear:   ldx     #$74
         jsr     load
nocler:  cmp     #$25
         bne     noper
         jsr     mpy
         bcs     problm
         ldx     #$77
         jsr     dvi
         bcs     problm
noper:   cmp     #$52
         bne     norep
         ldx     #$82
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
memry:   .byte   $00
         .byte   $00
         .byte   $00
numcar:  .byte   $00
prev:    .byte   $00
accu:    .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00
         .byte   $00


		 .org 	 $0200
input:   ldx     #$f9
         ldy     #$00
         jsr     store
weronp:  jsr     output
         lda     #$00
         sta     numcar
werinp:  jsr     inchar
         cmp     #$43
         beq     input
         cmp     #$49
         bne     nomemi
         lda     L00F9
         sta     memry
         lda     L00FA
         sta     memry+1
         lda     L00FB
         sta     memry+2
         jmp     weronp

nomemi:  cmp     #$4f
         bne     tesdec
         lda     memry
         sta     L00F9
         lda     memry+1
         sta     L00FA
         lda     memry+2
         sta     L00FB
         jmp     weronp

tesdec:  cmp     #$30
         bpl     godec
nodec:   rts

godec:   cmp     #$3a
         bpl     nodec
         ldx     numcar
         bne     notfrs
         ldy     #$00
         sty     L00F9
         sty     L00FA
         sty     L00FB
notfrs:  sec
         sbc     #$30
         rol     A
         rol     A
         rol     A
         rol     A
         ldx     #$04
rolwer:  rol     A
         rol     L00F9
         rol     L00FA
         rol     L00FB
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

itstty:  ldx     #$02
wtyp:    lda     L00F9,x
         jsr     $1e3b
         dex
         bpl     wtyp
outyp:   jsr     $1e2f
         rts

error:   lda     #$01
         bit     piadat
         beq     ttyerr
         ldx     #$02
         lda     #$ff
setful:  sta     accu,x
         dex
         bpl     setful
         rts

ttyerr:  lda     #$3f
         jsr     dutch
         jmp     outyp

		 .org 	 $0300 

begalg:  stx     L008B
         sty     L008C
         sta     L008D
         ldx     #$02
         ldy     #$00
clrwer:  sty     accu+3,x
         dex
         bpl     clrwer
         ldx     #$02
         sed
         rts

load:    jsr     begalg
nload:   lda     (L008B),y
         sta     accu,y
         iny
         dex
         bpl     nload
algout:  cld
         ldx     L008B
         ldy     L008C
         lda     L008D
         rts

store:   jsr     begalg
nstor:   lda     accu,y
         sta     (L008B),y
         iny
         dex
         bpl     nstor
         bmi     algout

add:     jsr     begalg
         clc
nadd:    lda     accu,y
         adc     (L008B),y
         sta     accu,y
         iny
         dex
         bpl     nadd
         bmi     algout

sub:     jsr     begalg
         sec
nsub:    lda     accu,y
         sbc     (L008B),y
         sta     accu,y
         iny
         dex
         bpl     nsub
         bcs     clicar
setcar:  sec
         bcs     algout

clicar:  clc
         bcc     algout

mpy:     stx     L008B
         sty     L008C
         sta     L008D
         ldx     #$06
wimpy:   lda     prev,x
         sta     L0084,x
         lda     #$00
         sta     prev,x
         dex
         bne     wimpy
         tay
         lda     #$03
         sta     L008F
         sed
outlup:  lda     (L008B),y
         sta     L008E
         beq     noad
peuter:  clc
         lda     #$06
         sta     L0090
         ldx     #$00
weradd:  lda     accu,x
         adc     L0085,x
         sta     accu,x
         inx
         dec     L0090
         bne     weradd
ulgo:    bcs     algout
         lda     L008E
         sbc     #$00
         sta     L008E
         bne     peuter
noad:    ldx     #$05
wustur:  lda     L0084,x
         sta     L0085,x
         dex
         bne     wustur
         stx     L0085
         iny
         dec     L008F
         bne     outlup
pulgo:   beq     clicar

dvi:     stx     L008B
         sty     L008C
         sta     L008D
         ldx     #$03
         stx     L008E
         ldx     #$05
movdiv:  lda     accu,x
         sta     L0085,x
         lda     #$00
         sta     accu,x
         dex
         bpl     movdiv
divlup:  ldy     #$02
         ldx     L008E
teswer:  lda     L0087,x
         cmp     (L008B),y
         bcc     findiv
         lda     L0087,x
         beq     nocomp
         ldx     L008E
         ldy     #$00
         lda     #$03
         sta     L008F
         sed
divre:   lda     L0085,x
         sbc     (L008B),y
         sta     L0085,x
         inx
         iny
         dec     L008F
         bne     divre
         lda     memry+2,x
         cmp     #$99
         bcs     ulgo
         adc     #$01
         sta     memry+2,x
         bne     divlup
nocomp:  dex
         dey
         bpl     teswer
findiv:  dec     L008E
         bpl     divlup
         jmp     clicar

asctab:  .byte   "0123456789+-*/CARIO%A"
