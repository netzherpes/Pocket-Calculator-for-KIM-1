# Pocket-Calculator-for-KIM-1
by Siep de Vries



 INPUT/OUTPUT IS EITHER TELETYPE<br>
 OR KIM KEYBOARD AND DISPLAY<br>

 INPUT IS GIVEN BY PRESSING THE<br>
 KEYS FOR A DECIMAL NUMBER<br>
 FOLLOWED BY A FUNCTIONKEY<br>

 FUNCTIONS:<br>
 A = + = ADD NUMBER TO RESULT<br>
 B = - = SUBTRACT NUMBER FROM RESULT<br>
 C = * = MULTIPLY RESULT BY NUMBER<br>
 D = / = DIVIDE RESULT BY NUMBER<br>
 E = C = CLEAR INPUT NUMBER<br>
 F = A = CLEAR RESULT<br>
 AD = R = REMAINDER OF LAST DIVISION<br>
 DA = I = NUMBER STORED IN MEMORY<br>
 \+ = C = NUMBER FROM MEMORY<br>
 PC = % = CALCULATE PERCENTAGE<br>
 GO = C = CLEAR RESULT<br>

 THE PROGRAM MAKES USE OF THE<br>
 INTEGER CALCULATION PACKAGE<br>
 'INTCAL'	PARTNO 770110.<br>
 A 3-BYTE VERSION IS USED.<br>
 WHENEVER A DIFFERENT<br>
 NUMBER OF BYTES PER<br>
 NUMBER IS REQUIRED, THE<br>
 DEFINITIONS CONTAINING<br>
 SIZE,	SIZEA,	SIZEB,	SIZEC,<br>
 ACCU, ACCMSB,	ACCLSB,<br>
 DACCU, DACLSB AND LOADAD<br>
 HAVE TO BE CHANGED TO THE<br>
 PROPER VALUE IN RELATION TO<br>
 SIZE AND THE PROGRAM MUST BE<br>
 REASSEMBLED.<br>

 AUTHOR: SIEP DE VRIES<br>
  	BRUGSTRAAT 32<br>
    LIMMEN (NH)<br>
    THE NETHERLANDS<br>
