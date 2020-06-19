 processor 6502
	org $800
	; Starting new memory block at $800
	.byte    $0, $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $32,$30,$36,$34
	.byte    $29, $00, $00, $00
	; Ending memory block
EndBlock1
	org $810
	; Starting new memory block at $810
C64Project
	jmp block1
t	dc.w	$00
expression	dc.w	
i	dc.b	
j	dc.b	
k	dc.b	
opcode	dc.b	 
	org opcode+4
opcodeindex	dc.b	
stack	dc.b	
temparray	dc.b	 
	org temparray+256
key	dc.b	
lastkey	dc.b	
saddr	dc.w	 
	org saddr+50
caddr	dc.w	 
	org caddr+50
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4C     ;$59 used for hi-byte
initdiv16x8_dividend = $4E	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4E ;save memory by reusing divident to store the result
divide16x8	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
	rol initdiv16x8_dividend+1
	rol initdiv16x8_remainder	;remainder lb & hb * 2 + msb from carry
	rol initdiv16x8_remainder+1
	lda initdiv16x8_remainder
	sec
	sbc initdiv16x8_divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda initdiv16x8_remainder+1
	sbc initdiv16x8_divisor+1
	bcc skip16	;if carry=0 then divisor didn't fit in yet
	sta initdiv16x8_remainder+1	;else save substraction result as new remainder,
	sty initdiv16x8_remainder
	inc initdiv16x8_result	;and INCrement result cause divisor fit in 1 times
skip16	dex
	bne divloop16
	rts
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4C
mul16x8_num1 = $4E
mul16x8_num2 = $50
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop  ; accumulating multiply entry point (enter with .A=lo, .Y=hi)
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4C
div8x8_d = $4E
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1 rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2 dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end
	rts
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4C
multiplier_a = $4E
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4E
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
initeightbitmul_multiply_eightbit2
	rts
	; ***********  Defining procedure : initgetkey
	;    Procedure type : Built-in function
	;    Requires initialization : no
;jmp c64_getKey
key_columntab
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
 dc.b $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 dc.b $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $60,
 dc.b $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60,
 dc.b $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $50,
 dc.b $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $50, $40,
 dc.b $40, $40, $40, $40, $40, $40, $40, $30, $30, $30, $30, $20, $20, $10, $00, $00,
key_chartab	dc.b $00, $051, $00, $020, $032, $00, $02a, $031
        dc.b $06f, $051, $07d, $00, $072, $03b, $02a, $01c
        dc.b $06c, $040, $07a, $06e, $02d, $04c, $050, $02b
        dc.b $04e, $04f, $04b, $04d, $030, $04a, $049, $039
        dc.b $056, $055, $048, $042, $038, $047, $059, $037
        dc.b $058, $054, $046, $043, $036, $044, $052, $035
        dc.b $00, $045, $053, $05a, $034, $041, $057, $033
        dc.b $00, $051, $00, $00, $00, $00, $08e, $0f7
        dc.b
key_row dc.b 0
key_col dc.b 0
c64_getKey:
    lda #$0
    sta $dc03	; port b ddr (input)
    lda #$ff
    sta $dc02	; port a ddr (output)
    lda #$00
    sta $dc00	; port a
    lda $dc01       ; port b
    cmp #$ff
    beq key_nokey
; got column
    tay
    lda #$7f
    sta key_nokey2+1
    ldx #8
key_nokey2:
    lda #0
    sta $dc00	; port a
    sec
    ror key_nokey2+1
    dex
    bmi key_nokey
    lda $dc01       ; port b
    cmp #$ff
    beq key_nokey2
    ; got row in X
    txa
    asl
    asl
    asl
    tax
    stx key_row
    txa
    lda key_columntab,y
    ror
    ror
    ror
    ror
    sta key_col
    ora key_row
    sta key_row
    lda #64
    sbc key_row
    ;	sec
    tax
    lda key_chartab,x
    jmp key_cont
key_nokey:
    lda #$FF
key_cont:
    rts
	rts
block1
	; 
	; ****** Inline assembler section
	lda #0
			sta $d020
			sta $d021
	
; // PETSCIIdad by gabochi, still a lot to improve...
; //											Screen preparation
screenmemory =  $fe
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$400
	lda #<$400
	ldx #0
	sta saddr,x   ; Address of table
	tya
	sta saddr+1,x
MainProgram_dtloop3
	tay
	lda saddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow4
	iny
MainProgram_dtnooverflow4
	sta saddr,x
	tya
	sta saddr+1,x
	cpx #$30
	bcc MainProgram_dtloop3
	; ----------
	; DefineAddressTable address, StartValue, IncrementValue, TableSize
	ldy #>$d800
	lda #<$d800
	ldx #0
	sta caddr,x   ; Address of table
	tya
	sta caddr+1,x
MainProgram_dtloop5
	tay
	lda caddr,x
	inx
	inx
	clc
	adc #$28
	bcc MainProgram_dtnooverflow6
	iny
MainProgram_dtnooverflow6
	sta caddr,x
	tya
	sta caddr+1,x
	cpx #$30
	bcc MainProgram_dtloop5
	; Clear screen with offset
	lda #$20
	ldx #$fa
MainProgram_clearloop7
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne MainProgram_clearloop7
MainProgram_while8
	; Full binary clause
	; Binary clause: NOTEQUALS
	lda #$1
	; Compare with pure num / var optimization
	cmp #$0;keep
	; BC done
	beq MainProgram_binaryclausefailed631
MainProgram_binaryclausesuccess633
	lda #1; success
	jmp MainProgram_binaryclausefinished632
MainProgram_binaryclausefailed631
	lda #0 ; failed state
MainProgram_binaryclausefinished632
	cmp #1
	beq MainProgram_ConditionalTrueBlock9
	jmp MainProgram_elsedoneblock11
MainProgram_ConditionalTrueBlock9
	
; // 											"Game" loop begins
; //											Cursor effect
	; Assigning single variable : screenmemory
	; ----------
	; AddressTable address, xoffset, yoffset
	ldx #0
	lda saddr,x   ; Address of table lo
	ldy saddr+1,x   ; Address of table hi
	sta screenmemory
	sty screenmemory+1
	; Assigning single variable : screenmemory
	; 8 bit binop
	; Add/sub right value is variable/expression
	; Peek
	ldy opcodeindex
	lda (screenmemory),y
MainProgram_rightvarAddSub_var636 = $54
	sta MainProgram_rightvarAddSub_var636
	lda #$80
	eor MainProgram_rightvarAddSub_var636
	; Calling storevariable
	ldy opcodeindex ; optimized, look out for bugs
	sta (screenmemory),y
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
MainProgram_for637
MainProgram_forLoopFix902
	
; //											Read code, evaluate, write temparray
	; Assigning single variable : expression
	; integer assignment NodeVar
	ldy t+1 ; Next one ; optimized, look out for bugs
	lda t
	; Calling storevariable
	sta expression
	sty expression+1
	; Assigning single variable : j
	lda #$0
	; Calling storevariable
	sta j
MainProgram_for906
MainProgram_forLoopFix1036
	
; //											Four-operation code
	; Assigning single variable : stack
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	lda j
	sec
	sbc #$1
	 ; end add / sub var with constant
	tax
	lda opcode,x
	; Calling storevariable
	sta stack
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$24;keep
	bne MainProgram_elsedoneblock1043
MainProgram_ConditionalTrueBlock1041
	; Assigning single variable : stack
	; integer assignment NodeVar
	ldy t+1 ; Next one ; optimized, look out for bugs
	lda t
	; Calling storevariable
	sta stack
MainProgram_elseblock1042
MainProgram_elsedoneblock1043
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$22;keep
	bne MainProgram_elsedoneblock1049
MainProgram_ConditionalTrueBlock1047
	; Assigning single variable : expression
	; integer assignment NodeVar
	ldy expression+1 ; Next one ; optimized, look out for bugs
	lda expression
MainProgram_tempVarShift_var1054 = $54
	sta MainProgram_tempVarShift_var1054
	sty MainProgram_tempVarShift_var1054+1
	ldx stack ; optimized, look out for bugs
MainProgram_lblShift1055
		lsr MainProgram_tempVarShift_var1054+1
	ror MainProgram_tempVarShift_var1054+0

	dex
	cpx #0
	bne MainProgram_lblShift1055
	lda MainProgram_tempVarShift_var1054
	ldy MainProgram_tempVarShift_var1054+1
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1048
MainProgram_elsedoneblock1049
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$1c;keep
	bne MainProgram_elsedoneblock1059
MainProgram_ConditionalTrueBlock1057
	; Assigning single variable : expression
	; integer assignment NodeVar
	ldy expression+1 ; Next one ; optimized, look out for bugs
	lda expression
MainProgram_tempVarShift_var1064 = $54
	sta MainProgram_tempVarShift_var1064
	sty MainProgram_tempVarShift_var1064+1
	ldx stack ; optimized, look out for bugs
MainProgram_lblShift1065
		asl MainProgram_tempVarShift_var1064+0
	rol MainProgram_tempVarShift_var1064+1

	dex
	cpx #0
	bne MainProgram_lblShift1065
	lda MainProgram_tempVarShift_var1064
	ldy MainProgram_tempVarShift_var1064+1
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1058
MainProgram_elsedoneblock1059
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$11;keep
	bne MainProgram_elsedoneblock1069
MainProgram_ConditionalTrueBlock1067
	; Assigning single variable : expression
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	lda stack
MainProgram_rightvarInteger_var1075 = $54
	sta MainProgram_rightvarInteger_var1075
	sty MainProgram_rightvarInteger_var1075+1
	lda expression+1
	and MainProgram_rightvarInteger_var1075+1
	tay
	lda expression
	and MainProgram_rightvarInteger_var1075
	bcs MainProgram_wordAdd1074
	dey
MainProgram_wordAdd1074
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1068
MainProgram_elsedoneblock1069
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$1f;keep
	bne MainProgram_elsedoneblock1079
MainProgram_ConditionalTrueBlock1077
	; Assigning single variable : expression
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	lda stack
MainProgram_rightvarInteger_var1085 = $54
	sta MainProgram_rightvarInteger_var1085
	sty MainProgram_rightvarInteger_var1085+1
	lda expression+1
	ora MainProgram_rightvarInteger_var1085+1
	tay
	lda expression
	ora MainProgram_rightvarInteger_var1085
	bcs MainProgram_wordAdd1084
	dey
MainProgram_wordAdd1084
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1078
MainProgram_elsedoneblock1079
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$28;keep
	bne MainProgram_elsedoneblock1089
MainProgram_ConditionalTrueBlock1087
	; Assigning single variable : expression
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	lda stack
MainProgram_rightvarInteger_var1095 = $54
	sta MainProgram_rightvarInteger_var1095
	sty MainProgram_rightvarInteger_var1095+1
	lda expression+1
	eor MainProgram_rightvarInteger_var1095+1
	tay
	lda expression
	eor MainProgram_rightvarInteger_var1095
	bcs MainProgram_wordAdd1094
	dey
MainProgram_wordAdd1094
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1088
MainProgram_elsedoneblock1089
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$23;keep
	bne MainProgram_elsedoneblock1099
MainProgram_ConditionalTrueBlock1097
	; Assigning single variable : expression
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	lda stack
MainProgram_rightvarInteger_var1105 = $54
	sta MainProgram_rightvarInteger_var1105
	sty MainProgram_rightvarInteger_var1105+1
	lda expression+1
	sec
	sbc MainProgram_rightvarInteger_var1105+1
	tay
	lda expression
	sec
	sbc MainProgram_rightvarInteger_var1105
	bcs MainProgram_wordAdd1104
	dey
MainProgram_wordAdd1104
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1098
MainProgram_elsedoneblock1099
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx j
	lda opcode,x
	; Compare with pure num / var optimization
	cmp #$1d;keep
	bne MainProgram_elsedoneblock1109
MainProgram_ConditionalTrueBlock1107
	; Assigning single variable : expression
	ldy #0
	; Mul 16x8 setup
	; Integer assignment in nodevar
	lda expression
	ldy expression+1
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda stack
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable
	sta expression
	sty expression+1
MainProgram_elseblock1108
MainProgram_elsedoneblock1109
	
; //											In the meanwhile, check for key interaction 	
	; Assigning single variable : key
	jsr c64_getKey
	; Calling storevariable
	sta key
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$30;keep
	bne MainProgram_elsedoneblock1115
MainProgram_ConditionalTrueBlock1113
	; Assigning single variable : opcodeindex
	lda #$0
	; Calling storevariable
	sta opcodeindex
	; Assigning single variable : lastkey
	lda key
	; Calling storevariable
	sta lastkey
MainProgram_elseblock1114
MainProgram_elsedoneblock1115
	; Binary clause Simplified: NOTEQUALS
	lda key
	; Compare with pure num / var optimization
	cmp lastkey;keep
	beq MainProgram_elsedoneblock1121
MainProgram_ConditionalTrueBlock1119
	; Binary clause: GREATER
	lda key
	; Compare with pure num / var optimization
	cmp #$30;keep
	; BC done
	bcc MainProgram_elseblock1148
	beq MainProgram_elseblock1148
MainProgram_binaryclausesuccess1158
	; Binary clause: LESS
	lda key
	; Compare with pure num / var optimization
	cmp #$5b;keep
	; BC done
	bcs MainProgram_elseblock1148
MainProgram_binaryclausesuccess1160
MainProgram_ConditionalTrueBlock1147
	; Assigning single variable : screenmemory
	; Store Variable simplified optimization : right-hand term is pure
	ldy opcodeindex ; optimized, look out for bugs
	lda key
	sta (screenmemory),y
	; Assigning single variable : lastkey
	; Calling storevariable
	sta lastkey
	; Assigning single variable : opcode
	; 8 bit binop
	; Add/sub where right value is constant number
	sec
	sbc #$30
	 ; end add / sub var with constant
	; Calling storevariable
	ldx opcodeindex ; optimized, look out for bugs
	sta opcode,x
	; Assigning single variable : opcodeindex
	inc opcodeindex
	; Binary clause Simplified: EQUALS
	lda opcodeindex
	; Compare with pure num / var optimization
	cmp #$4;keep
	bne MainProgram_elsedoneblock1165
MainProgram_ConditionalTrueBlock1163
	; Assigning single variable : opcodeindex
	lda #$0
	; Calling storevariable
	sta opcodeindex
MainProgram_elseblock1164
MainProgram_elsedoneblock1165
MainProgram_elseblock1148
MainProgram_elsedoneblock1149
MainProgram_elseblock1120
MainProgram_elsedoneblock1121
	inc j
	lda #$4
	cmp j ;keep
	beq MainProgram_forLoopDone1037
MainProgram_forLoopNotDone1038
	jmp MainProgram_for906
MainProgram_forLoopDone1037
	; Assigning single variable : temparray
	; Store Variable simplified optimization : right-hand term is pure
	ldx i ; optimized, look out for bugs
	; integer assignment NodeVar
	ldy expression+1 ; Next one ; optimized, look out for bugs
	lda expression
	sta temparray,x
	; Assigning single variable : t
	ldy #0
	; WORD optimization: a=a+b
	lda t
	clc
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	adc #$1
	bcc MainProgram_WordAdd1168
	inc t+1
MainProgram_WordAdd1168
	sta t+0
	inc i
	; Integer constant assigning
	ldy #$01
	lda #$00
	cmp i ;keep
	beq MainProgram_forLoopDone903
MainProgram_forLoopNotDone904
	jmp MainProgram_for637
MainProgram_forLoopDone903
	; Assigning single variable : k
	lda #$0
	; Calling storevariable
	sta k
MainProgram_for1169
MainProgram_forLoopFix1189
	; Assigning single variable : j
	lda #$0
	; Calling storevariable
	sta j
MainProgram_for1193
MainProgram_forLoopFix1201
	
; //											Screen write	
	; Assigning single variable : screenmemory
	; ----------
	; AddressTable address, xoffset, yoffset
	; yoffset is complex
	; 8 bit binop
	; Add/sub right value is variable/expression
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda k
	asl
	asl
	asl
MainProgram_rightvarAddSub_var1206 = $54
	sta MainProgram_rightvarAddSub_var1206
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda j
	asl
	asl
	clc
	adc #$4
	 ; end add / sub var with constant
	clc
	adc MainProgram_rightvarAddSub_var1206
	asl ; *2
	tax
	lda saddr,x   ; Address of table lo
	ldy saddr+1,x   ; Address of table hi
	clc
	adc #$4
	bcc MainProgram_dtnooverflow1205
	iny  ; overflow into high byte
MainProgram_dtnooverflow1205
	sta screenmemory
	sty screenmemory+1
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
MainProgram_for1207
	; Assigning single variable : screenmemory
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda j
	asl
	asl
	asl
	asl
	asl
	asl
	asl
	clc
	adc i
	 ; end add / sub var with constant
	tax
	lda temparray,x
	; Calling storevariable
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda i
	lsr
	lsr
	lsr
	lsr
	lsr
	asl
	asl
	asl
	clc
	adc i
	 ; end add / sub var with constant
	tay
	pla
	sta (screenmemory),y
	inc i
	lda #$80
	cmp i ;keep
	bne MainProgram_for1207
MainProgram_forLoopDone1209
	inc j
	lda #$2
	cmp j ;keep
	beq MainProgram_forLoopDone1202
MainProgram_forLoopNotDone1203
	jmp MainProgram_for1193
MainProgram_forLoopDone1202
	inc k
	lda #$2
	cmp k ;keep
	beq MainProgram_forLoopDone1190
MainProgram_forLoopNotDone1191
	jmp MainProgram_for1169
MainProgram_forLoopDone1190
	; Assigning single variable : k
	lda #$0
	; Calling storevariable
	sta k
MainProgram_for1211
MainProgram_forLoopFix1231
	; Assigning single variable : j
	lda #$0
	; Calling storevariable
	sta j
MainProgram_for1235
MainProgram_forLoopFix1243
	; Assigning single variable : screenmemory
	; ----------
	; AddressTable address, xoffset, yoffset
	; yoffset is complex
	; 8 bit binop
	; Add/sub right value is variable/expression
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda k
	asl
	asl
	asl
MainProgram_rightvarAddSub_var1248 = $54
	sta MainProgram_rightvarAddSub_var1248
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda j
	asl
	asl
	clc
	adc #$4
	 ; end add / sub var with constant
	clc
	adc MainProgram_rightvarAddSub_var1248
	asl ; *2
	tax
	lda caddr,x   ; Address of table lo
	ldy caddr+1,x   ; Address of table hi
	clc
	adc #$4
	bcc MainProgram_dtnooverflow1247
	iny  ; overflow into high byte
MainProgram_dtnooverflow1247
	sta screenmemory
	sty screenmemory+1
	; Assigning single variable : i
	lda #$0
	; Calling storevariable
	sta i
MainProgram_for1249
	; Assigning single variable : screenmemory
	; Load Byte array
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda j
	asl
	asl
	asl
	asl
	asl
	asl
	asl
	clc
	adc i
	 ; end add / sub var with constant
	tax
	lda temparray,x
	; Calling storevariable
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	lda i
	lsr
	lsr
	lsr
	lsr
	lsr
	asl
	asl
	asl
	clc
	adc i
	 ; end add / sub var with constant
	tay
	pla
	sta (screenmemory),y
	inc i
	lda #$80
	cmp i ;keep
	bne MainProgram_for1249
MainProgram_forLoopDone1251
	inc j
	lda #$2
	cmp j ;keep
	beq MainProgram_forLoopDone1244
MainProgram_forLoopNotDone1245
	jmp MainProgram_for1235
MainProgram_forLoopDone1244
	inc k
	lda #$2
	cmp k ;keep
	beq MainProgram_forLoopDone1232
MainProgram_forLoopNotDone1233
	jmp MainProgram_for1211
MainProgram_forLoopDone1232
	jmp MainProgram_while8
MainProgram_elseblock10
MainProgram_elsedoneblock11
	jmp * ; loop like (�/%
EndSymbol
	; End of program
	; Ending memory block
EndBlock3
