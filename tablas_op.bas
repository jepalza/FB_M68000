
' para ejecutar por independiente este modulo, descomentar estas lineas
'Dim Shared FALSO As Integer=0
'Dim Shared CIERTO As Integer=1
'Type u8 As UByte
'Type u16 As UShort
'Type u32 As UInteger
'Type bool As UByte
'
'    Enum 'size
'       SizeByte = 0
'       SizeWord = 1
'       SizeLong = 2
'    End Enum

' nota:
' los valores ",0" ",1" y ",2" son sizebyte, word y long
' los valroes ",-1" y ",-2" son FALSO y CIERTO, o sea, 0 y 1

Dim shared opcodes(65535) As string
Dim Shared op As u16 ' para enviar a los core68k_build.... el opcode de la rutina que lo trata

Sub core68k_build_op_with_param(_op As string, _size As Integer, param As integer)
	' hay casos que llevan un FALSO o CIERTO ademas del parametro principal
	Dim As String _param=","+LTrim(RTrim(Str(param))) 'puede ser -1 o -2 falso o cierto
	If param=0 Then _param="" 
    if(_size=SizeByte) Then opcodes(op)="op_"+_op+",0"+_param
    If(_size=SizeWord) Then opcodes(op)="op_"+_op+",1"+_param
    If(_size=SizeLong) Then opcodes(op)="op_"+_op+",2"+_param
End Sub

Sub core68k_build_op(_op As string, _size As integer)
    if(_size=SizeByte) Then opcodes(op)="op_"+_op+",0"
    If(_size=SizeWord) Then opcodes(op)="op_"+_op+",1"
    If(_size=SizeLong) Then opcodes(op)="op_"+_op+",2"
End Sub    

Function CheckEA(ea As u8,valid As String) As Integer 'EA=MMMRRR(mode/reg)
	If ((ea shr 3) and 7) <> 7 Then
        If (valid[(ea shr 3) and 7] <> Asc("1")) Then 
        	 return 0
        else 
          return 1
        End If
    else
        if((ea and 7)>=5) Then
        	 return 0
        else
            Dim count As integer = ea And 7
            'to easier match with documentation
            If (count=2) Then 
            	count=3
            ElseIf(count=3) Then
            	count=4
            ElseIf(count=4) Then
            	count=2
            End If

            If (valid[7+count] <> Asc("1")) Then
            	return 0
            else
        	      return 1
            End If
        End If
        End If
	return 0
End function



Sub ArithmeticQ(opcode As U16)
	Dim As u8 ea=opcode and &h3f
	op=opcode ' para enviar el parametro OPCODE a core68k_build...
   If (CheckEA(ea,"111111111000")<>1) Then exit sub
	Dim as u8 size=(opcode shr 6) and 3
	if(size=SizeByte and ((ea shr 3)=1)) Then exit Sub
    If ((opcode shr 3) and 7)=1 Then
        size=SizeLong:'A reg,word behaves like long
    End If
    Select Case ((opcode shr 8) and 1)
    	case 0: 
    		core68k_build_op("addq", size): exit Sub
    	case 1: 
    		core68k_build_op("subq", size): exit Sub
	End Select
End Sub

Sub ArithmeticI(opcode As U16)
	Dim As u8 ea=opcode and &h3f
	op=opcode ' para enviar el parametro OPCODE a core68k_build...
   If (CheckEA(ea,"101111111000")<>1) Then Exit Sub   
   Dim as u8 size=(opcode shr 6) and 3

	Select Case ((opcode shr 8) and &hf)
		case &h0: core68k_build_op("ori" , size)
		case &h2: core68k_build_op("andi", size)
		case &h4: core68k_build_op("subi", size)
		case &h6: core68k_build_op("addi", size)
		case &ha: core68k_build_op("eori", size)
		case &hc: core68k_build_op("cmpi", size)
	End Select
End Sub

Sub Arithmetico(opcode As u16)
	Dim As u8 opmode=(opcode shr 6) and 7
	op=opcode ' para enviar el parametro OPCODE a core68k_build...
	Dim As u8 ea=opcode And &h3f

	Select Case (opmode And 3)
		case 3:  'ADDA, SUBA, CMPA
            Select Case (opcode shr 12)
                case &h9:
                    if(opmode And 4) Then 
                    	opcodes(opcode)="op_suba,2"
                    else 
                     opcodes(opcode)="op_suba,1"
                    End If
                    exit sub
                case &hB:
                    if(opmode And 4) Then
                    	opcodes(opcode)="op_cmpa,2"
                    else 
                     opcodes(opcode)="op_cmpa,1"
                    End If
                    exit sub
                case &hD:
                    if(opmode and 4) Then 
                    	opcodes(opcode)="op_adda,2"
                    Else
                     opcodes(opcode)="op_adda,1"
                    End If
                    exit sub
            	Case Else
            		exit sub
            End Select
	End Select

	if(opmode And 4) Then 'ea is destination (opmode=4)
      opmode And=3
		Select Case (opcode shr 12)
			case &h8: 'or
                if(CheckEA(ea,"001111111000")<>1) Then exit Sub
                core68k_build_op_with_param("or",opmode,-2)
					Exit sub
			case &h9: 'sub
                if(CheckEA(ea,"001111111000")<>1) Then exit sub
                core68k_build_op_with_param("sub",opmode,-2)
					Exit sub
			case &hB: 'eor
                if(CheckEA(ea,"101111111000")<>1) Then exit sub
                core68k_build_op("eor",opmode)
					Exit sub
			case &hC: 'and
                if(CheckEA(ea,"001111111000")<>1) Then exit sub
                core68k_build_op_with_param("and",opmode,-2)
					Exit sub
			case &hD: 'add
                if(CheckEA(ea,"001111111000")<>1) Then exit sub
                core68k_build_op_with_param("add",opmode,-2)
					exit Sub
			Case Else 
				   Exit sub
		End Select
		
	else'ea is source (opmode<>4)
      opmode And=3
		Select Case (opcode shr 12)
			case &h8: 'or
            If(CheckEA(ea,"101111111111")<>1) Then exit sub
            core68k_build_op_with_param("or",opmode,-1)
				exit sub
			case &h9: 'sub
            if(CheckEA(ea,"111111111111")<>1) Then exit sub
				if(((ea shr 3)=1) and ((opmode and 3)=0)) Then exit sub
            core68k_build_op_with_param("sub",opmode,-1)
				exit sub
			case &hB: 'cmp
            If(CheckEA(ea,"111111111111")<>1) Then exit sub
				if(((ea shr 3)=1) and ((opmode And 3)=0)) Then exit sub
            core68k_build_op("cmp",opmode)
				exit sub
			case &hC: 'and
            If(CheckEA(ea,"101111111111")<>1) Then exit sub
            core68k_build_op_with_param("and",opmode,-1)
				exit sub
			case &hD: 'add
            If(CheckEA(ea,"111111111111")<>1) Then exit sub
				if(((ea shr 3)=1) and ((opmode And 3)=0)) Then exit sub
            core68k_build_op_with_param("add",opmode,-1)
				exit Sub
			Case Else 
			   Exit Sub
		End Select
	End If
	
End Sub



Sub build_optable()
    'interrupt=AUTO_VECTOR
 
   Dim As Integer i,j,k,l,m
    
   For i=0 TO 65535
		opcodes(i)= "op_illegal"
   Next
        
	'LSL,LSR,ASL,ASR reg
	for i=0 TO 7
		for k=0 To 2
			for l=0 To 1
				for m=0 TO 7
					opcodes((i shl 9)+(0 shl 8)+(k shl 6)+(l shl 5)+m+&he008)= "op_xsx,-1,-1,-1"
					opcodes((i shl 9)+(1 shl 8)+(k shl 6)+(l shl 5)+m+&he008)= "op_xsx,-2,-1,-1"
					opcodes((i shl 9)+(0 shl 8)+(k shl 6)+(l shl 5)+m+&he000)= "op_xsx,-1,-1,-2"
					opcodes((i shl 9)+(1 shl 8)+(k shl 6)+(l shl 5)+m+&he000)= "op_xsx,-2,-1,-2"
				Next:Next:Next:Next
				
			
		
	
	'LSL,LSR,ASL,ASR mem
	for j=0 To &h3f
      if(CheckEA(j,"001111111000")<>1) Then Continue For
		opcodes(&he2c0+(0 shl 8)+j)= "op_xsx,-1,-2,-1"
		opcodes(&he2c0+(1 shl 8)+j)= "op_xsx,-2,-2,-1"
		opcodes(&he0c0+(0 shl 8)+j)= "op_xsx,-1,-2,-2"
		opcodes(&he0c0+(1 shl 8)+j)= "op_xsx,-2,-2,-2"
	Next
	
	'ROL,ROR,ROXL,ROXR reg
	for i=0 TO 7
		for k=0 To 2
			for l=0 To 1
				for m=0 TO 7
					opcodes((i shl 9)+(0 shl 8)+(k shl 6)+(l shl 5)+m+&he018)= "op_rox,-1,-1,-1"
					opcodes((i shl 9)+(1 shl 8)+(k shl 6)+(l shl 5)+m+&he018)= "op_rox,-2,-1,-1"
					opcodes((i shl 9)+(0 shl 8)+(k shl 6)+(l shl 5)+m+&he010)= "op_rox,-1,-1,-2"
					opcodes((i shl 9)+(1 shl 8)+(k shl 6)+(l shl 5)+m+&he010)= "op_rox,-2,-1,-2"
				Next:Next:	Next:Next
				
			
		
	
	'ROL,ROR,ROXL,ROXR mem
	for j=0 To &h3f
        if(CheckEA(j,"001111111000")<>1) Then Continue for
		opcodes(&he6c0+(0 shl 8)+j)= "op_rox,-1,-2,-1"
		opcodes(&he6c0+(1 shl 8)+j)= "op_rox,-2,-2,-1"
		opcodes(&he4c0+(0 shl 8)+j)= "op_rox,-1,-2,-2"
		opcodes(&he4c0+(1 shl 8)+j)= "op_rox,-2,-2,-2"
	Next
	
	'BCHG BCLR BSET
    for i=0 TO 7'register
        for j=0 To &h3f'EA
            if(CheckEA(j,"101111111000")<>1) Then Continue For
            opcodes((i shl 9)+j+&h0140)= "op_bchg,-2"
			   opcodes((i shl 9)+j+&h0180)= "op_bclr,-2"
			   opcodes((i shl 9)+j+&h01c0)= "op_bset,-2"
        Next:Next
        
	
    for i=0 To &H3F'EA
        if(CheckEA(i,"101111111000")<>1) Then Continue For
		opcodes(i+&h0840)= "op_bchg,-1"
		opcodes(i+&h0880)= "op_bclr,-1"
		opcodes(i+&h08c0)= "op_bset,-1"
    Next
    
	'BTST
    for i=0 TO 7'register
        for j=0 To &h3f'EA
            if(CheckEA(j,"101111111111")<>1) Then Continue For
            opcodes((i shl 9)+j+&h0100)= "op_btst,-2"
        Next:Next
        
	
    for i=0 To &H3F'EA
        if(CheckEA(i,"101111111110")<>1) Then Continue For:'manual shows immediate before PC indi.
		opcodes(i+&h0800)= "op_btst,-1"
    Next
    
	'CLR
	for i=0 To &H3F
        if(CheckEA(i,"101111111000")<>1) Then Continue For
        opcodes((0 shl 6)+i+&h4200)= "op_clr,0"
        opcodes((1 shl 6)+i+&h4200)= "op_clr,1"
        opcodes((2 shl 6)+i+&h4200)= "op_clr,2"
	Next
	
	'NBCD
	for i=0 To &H3F'EA
        if(CheckEA(i,"101111111000")<>1) Then Continue For
		opcodes(i+&h4800)= "op_nbcd"
	Next
	
	'NEG NEGX
	for i=0 To &H3F				'EA
        if(CheckEA(i,"101111111000")<>1) Then Continue For
        opcodes(((0 shl 10)+(0 shl 6)+i+&h4000))= "op_neg,-2,0"
        opcodes(((0 shl 10)+(1 shl 6)+i+&h4000))= "op_neg,-2,1"
        opcodes(((0 shl 10)+(2 shl 6)+i+&h4000))= "op_neg,-2,2"
        opcodes(((1 shl 10)+(0 shl 6)+i+&h4000))= "op_neg,-1,0"
        opcodes(((1 shl 10)+(1 shl 6)+i+&h4000))= "op_neg,-1,1"
        opcodes(((1 shl 10)+(2 shl 6)+i+&h4000))= "op_neg,-1,2"
	Next
	
	'NOT
	for i=0 To &H3F
        if(CheckEA(i,"101111111000")<>1) Then Continue For
        opcodes((0 shl 6)+i+&h4600)= "op_not,0"
        opcodes((1 shl 6)+i+&h4600)= "op_not,1"
        opcodes((2 shl 6)+i+&h4600)= "op_not,2"
	Next
	
	'SCC
	for i=0 To &H3F'EA
        if(CheckEA(i,"101111111000")<>1) Then Continue For
	    for j=0 To &hF'condition
			opcodes((j shl 8)+i+&h50c0)= "op_scc"
	    Next:Next
	    
	
	'TAS
	for i=0 To &H3F
        if(CheckEA(i,"101111111000")<>1) Then Continue For
		opcodes(i+&h4ac0)= "op_tas"
	Next
	
	'TST
	for i=0 To &H3F
        if(CheckEA(i,"101111111000")<>1) Then Continue For
        opcodes((0 shl 6)+i+&h4a00)= "op_tst,0"
        opcodes((1 shl 6)+i+&h4a00)= "op_tst,1"
        opcodes((2 shl 6)+i+&h4a00)= "op_tst,2"
	Next
	
	'ADD,ADDA,AND,CMP,CMPA,EOR,OR,SUB,SUBA
    for i=&h8 To &hD 'operation
        for j=0 To 7'Dn,An
            for k=0 to 7'opmode
                for l=0 To &h3F'EA
                    Arithmetico((i shl 12)+(j shl 9)+(k shl 6)+l)
                Next:Next:	Next:Next			
			
		
	
    'MULU,MULS,DIVU,DIVS
    for j=0 To &h3f			'EA
      if(CheckEA(j,"101111111111")<>1) Then Continue For
		for i=0 TO 7'register
			opcodes((i shl 9)+j+&hc0c0)= "op_mulu"
			opcodes((i shl 9)+j+&hc1c0)= "op_muls"
			opcodes((i shl 9)+j+&h80c0)= "op_divu"
			opcodes((i shl 9)+j+&h81c0)= "op_divs"
		Next:Next
		
	
	'MOVE
    for j=0 To &h3f 'dest.EA
        Dim as u8 checkJ=((j  and 7) shl 3) or ((j shr 3) and 7)
        if(CheckEA(checkJ,"101111111000")<>1) Then Continue For
        for k=0 To &h3F'source EA
            if(CheckEA(k,"111111111111")<>1) Then Continue For
            if((k shr 3) <> 1) Then
                opcodes((1 shl 12)+(j shl 6)+k)= "op_move,0"
            End If
            opcodes((3 shl 12)+(j shl 6)+k)= "op_move,1"
            opcodes((2 shl 12)+(j shl 6)+k)= "op_move,2"
        Next:Next
        
	'MOVEA
    for j=0 to 7 'dest.reg
        for k=0 To &h3F'source EA
            if(CheckEA(k,"111111111111")<>1) Then Continue For
            opcodes((3 shl 12)+(j shl 9)+k+&h0040)= "op_movea,1"
            opcodes((2 shl 12)+(j shl 9)+k+&h0040)= "op_movea,2"
        Next:Next
        
	'ADDI,ANDI,CMPI,EORI,ORI,SUBI
	for i=0 To &hc'operation
        for j=0 To 2'size
            for k=0 To &h3F'EA
                ArithmeticI((i shl 8)+(j shl 6)+k)
            Next:Next:Next		
		
	
	'ADDQ
	for i=0 TO 7'dato
        for j=0 To 1'ADDQ/SUBQ
            for k=0 To 2'size
                for l=0 To &h3F'EA
                    ArithmeticQ((i shl 9)+(j shl 8)+(k shl 6)+l+&h5000)
                Next:Next:	Next:Next			
			
		
	
	'MOVEQ
    for i=0 To 7
        for j=0 To &hFf
            opcodes((i shl 9)+&h7000+j)= "op_moveq"
        Next:Next
        
	'bcc
	for i=2 To &hf
        for j=0 To &hFf
            opcodes(((i shl 8)+j+&h6000))= "op_bcc"
        Next:Next	
	
	'bra
	for i=0 To &hff
		opcodes(i+&h6000)= "op_bra"
	Next
	
	'bsr
	for i=0 To &hff
		opcodes(i+&h6100)= "op_bsr"
	Next
	
	'dbcc
	for i=0 To &hf'condition
		for j=0 to 7'register
			opcodes((i shl 8)+j+&h50c8)= "op_dbcc"
		Next:Next		
	
	'JMP
	for j=0 To &h3f
      If(CheckEA(j,"001001111011")<>1) Then Continue For
		opcodes(j+&h4ec0)= "op_jmp"
	Next
	
	'JSR
	for j=0 To &h3f
      If(CheckEA(j,"001001111011")<>1) Then Continue For
		opcodes(j+&h4e80)= "op_jsr"
	Next
	
	'LEA
	for k=0 To &h3F				'EA
      If(CheckEA(k,"001001111011")<>1) Then Continue For
		for i=0 TO 7'register
			opcodes((i shl 9)+k+&h41c0)= "op_lea"
		Next:Next	
	
	'PEA
	for j=0 To &h3f
      If(CheckEA(j,"001001111011")<>1) Then Continue For
		opcodes(j+&h4840)= "op_pea"
	Next
	
	'MOVEM
    for k=0 To &h3F'EA
        if(CheckEA(k,"001011111000")) Then
            opcodes((0 shl 10)+(0 shl 6)+k+&h4880)= "op_movem,1,-1"
            opcodes((0 shl 10)+(1 shl 6)+k+&h4880)= "op_movem,2,-1"
        End If
        if(CheckEA(k,"001101111011")) Then
            opcodes((1 shl 10)+(0 shl 6)+k+&h4880)= "op_movem,1,-2"
            opcodes((1 shl 10)+(1 shl 6)+k+&h4880)= "op_movem,2,-2"
        End If
    Next
    
    'ADDX SUBX
    for i=0 TO 7'Rx
        for k=0 To 7'Ry
            opcodes((i shl 9)+(0 shl 3)+(0 shl 6)+k+&hd100)= "op_addx,0,-1"
            opcodes((i shl 9)+(0 shl 3)+(1 shl 6)+k+&hd100)= "op_addx,1,-1"
            opcodes((i shl 9)+(0 shl 3)+(2 shl 6)+k+&hd100)= "op_addx,2,-1"
            opcodes((i shl 9)+(1 shl 3)+(0 shl 6)+k+&hd100)= "op_addx,0,-2"
            opcodes((i shl 9)+(1 shl 3)+(1 shl 6)+k+&hd100)= "op_addx,1,-2"
            opcodes((i shl 9)+(1 shl 3)+(2 shl 6)+k+&hd100)= "op_addx,2,-2"
            opcodes((i shl 9)+(0 shl 3)+(0 shl 6)+k+&h9100)= "op_subx,0,-1"
            opcodes((i shl 9)+(0 shl 3)+(1 shl 6)+k+&h9100)= "op_subx,1,-1"
            opcodes((i shl 9)+(0 shl 3)+(2 shl 6)+k+&h9100)= "op_subx,2,-1"
            opcodes((i shl 9)+(1 shl 3)+(0 shl 6)+k+&h9100)= "op_subx,0,-2"
            opcodes((i shl 9)+(1 shl 3)+(1 shl 6)+k+&h9100)= "op_subx,1,-2"
            opcodes((i shl 9)+(1 shl 3)+(2 shl 6)+k+&h9100)= "op_subx,2,-2"
        Next:Next
        
	'CMPM
    for i=0 TO 7'Ax
        for k=0 To 7'Ay
            opcodes((i shl 9)+(0 shl 6)+k+&hb108)= "op_cmpm,0"
            opcodes((i shl 9)+(1 shl 6)+k+&hb108)= "op_cmpm,1"
            opcodes((i shl 9)+(2 shl 6)+k+&hb108)= "op_cmpm,2"
        Next:Next
        
    'ABCD SBCD
    for i=0 TO 7'Rx
        for k=0 To 7'Ry
            opcodes((i shl 9)+(0 shl 3)+k+&hc100)= "op_abcd,-1"
            opcodes((i shl 9)+(1 shl 3)+k+&hc100)= "op_abcd,-2"
            opcodes((i shl 9)+(0 shl 3)+k+&h8100)= "op_sbcd,-1"
            opcodes((i shl 9)+(1 shl 3)+k+&h8100)= "op_sbcd,-2"
        Next:Next
        
	'ANDI to CCR
	opcodes(&h023c)= "op_andiccr"
	'ANDI to SR
	opcodes(&h027c)= "op_andisr"
	'CHK
	for k=0 To &h3F				'EA
      If(CheckEA(k,"101111111111")<>1) Then Continue For
		for i=0 TO 7'register
			opcodes((i shl 9)+(3 shl 7)+k+&h4000)= "op_chk"
		Next:Next	
	
	'ORI to CCR
	 opcodes(&h003c)= "op_oriccr"
	'ORI to SR
	 opcodes(&h007c)= "op_orisr"
	'EORI to CCR
    opcodes(&h0a3c)= "op_eoriccr"
	'EORI to SR
    opcodes(&h0a7c)= "op_eorisr"
	'MOVE FROM SR
	for i=0 To &H3F
        if(CheckEA(i,"101111111000")<>1) Then Continue For
		  opcodes(i+&h40c0)= "op_movefromsr"
	Next
	
	'MOVE TO CCR
	for i=0 To &H3F
        if(CheckEA(i,"101111111111")<>1) Then Continue For
        opcodes(i+&h44c0)= "op_movetoccr"
	Next
	
	'MOVE TO SR
	for i=0 To &H3F
        if(CheckEA(i,"101111111111")<>1) Then Continue for
		  opcodes(i+&h46c0)= "op_movetosr"
	Next
	
	'EXG
	for i=0 TO 7'Rx
		for j=0 to 7'Ry
            opcodes(&hc100+(i shl 9)+(8 shl 3)+j)= "op_exg,8"
            opcodes(&hc100+(i shl 9)+(9 shl 3)+j)= "op_exg,9"
            opcodes(&hc100+(i shl 9)+(17 shl 3)+j)= "op_exg,17"
		Next:Next	
	
	'EXT
	for i=0 TO 7'Rx
        opcodes(&h4800+(2 shl 6)+i)= "op_ext,-1"
        opcodes(&h4800+(3 shl 6)+i)= "op_ext,-2"
	Next
	
	'LINK
	for i=0 TO 7
		opcodes(&h4e50+i)= "op_link"
	Next
	
	'MOVE FROM(TO)USP
	for i=0 TO 7
        opcodes(&h4e60+(1 shl 3)+i)= "op_moveusp,-2"
        opcodes(&h4e60+(0 shl 3)+i)= "op_moveusp,-1"
	Next
	
	'NOP
	opcodes(&h4e71)= "op_nop"
	'RESET
	opcodes(&h4e70)= "op_reset"
	'RTE
	opcodes(&h4e73)= "op_rte"
	'RTR
	opcodes(&h4e77)= "op_rtr"
	'RTS
	opcodes(&h4e75)= "op_rts"
	'STOP
	opcodes(&h4e72)= "op_stop"
	'Swap
	for i=0 TO 7
		opcodes(&h4840+i)= "op_swap"
	Next
	
	'TRAP
	for i=0 To &hf
      opcodes(&h4e40+i)= "op_trap"
	Next
	
	'TRAPV
	opcodes(&h4e76)= "op_trapv"
	'UNLK
	for i=0 TO 7
		opcodes(&h4e58+i)= "op_unlk"
	Next
	
	'MOVEP
    for i=0 TO 7'Dx
        for k=0 To 7'Ax
            opcodes((i shl 9)+(4 shl 6)+k+&h0008)= "op_movep,4"
            opcodes((i shl 9)+(5 shl 6)+k+&h0008)= "op_movep,5"
            opcodes((i shl 9)+(6 shl 6)+k+&h0008)= "op_movep,6"
            opcodes((i shl 9)+(7 shl 6)+k+&h0008)= "op_movep,7"
        Next:Next
End Sub

build_optable()

' para sacar un listado que poder estudiar
'Dim f As Integer
'Open "opcodes.txt" For Output As 1
'For f=0 To 65535
'	   Print #1,Hex(f,4),opcodes(f)   
'Next
'Close 1

