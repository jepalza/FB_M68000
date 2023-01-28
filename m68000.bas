#Include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB '' para el multikey
#EndIf

Screen 19

Dim Shared CIERTO As Integer=1
Dim Shared FALSO As Integer=0

' solo depuracion (para la rutina DEBUG)
Dim Shared tecla As Integer=1 ' 0 espera teclas, 1 continuo, sin pausas

Type u8   As UByte
Type u16  As UShort
Type u32  As UInteger
Type u64  As uLongint
Type bool As UByte


    Enum 'size
       SizeByte = 0
       SizeWord = 1
       SizeLong = 2
    End Enum
    
    
#Include"declaraciones.bas"
#Include"tablas_op.bas"

Dim Shared executed_cycles As Integer=0
Dim Shared arithmetic As Integer=0
Dim Shared size As Integer=0

Dim Shared RAM(16*(1024*1024)) As u8 ' 16 megas de RAM (maximo permitido???)

Dim Shared As Integer interrupt
Dim Shared As Integer irqPendingLevel
Dim Shared As Integer irqSamplingLevel

Dim Shared As Integer fault_address

Dim Shared As Integer doubleFault 'double fault, needs reset to recover

Dim Shared As Integer rmwCycle
Dim Shared As Integer illegalMode

Dim Shared As Integer eaAddr 

Dim Shared As Integer adm    
Dim Shared As Integer opcode

Dim Shared As Integer reg_irc, reg_ird, reg_ir
Dim Shared As Integer reg_pc, reg_usp, reg_ssp
Dim Shared As Integer extend

Dim Shared As Integer trace, _stop

' regs ##########################################################################

Type reg_state
	Union
	  w As u16
	  Type
	  	l As u8
	  	h As u8
	  End Type
	  Type ' registros de estado CVZNX...
	  	c 					: 1 As bool
	  	v 					: 1 As bool
	  	z 					: 1 As bool
	  	n 					: 1 As bool
	  	x 					: 1 As bool
	  	no_use_bit5 	: 1 As bool
	  	no_use_bit6 	: 1 As bool
	  	no_use_bit7 	: 1 As bool
	  	intr 				: 3 As u8
	  	no_use_bit11	: 1 As bool
	  	no_use_bit12	: 1 As bool
	  	s 					: 1 As bool
	  	no_use_bit14 	: 1 As bool
	  	trace 			: 1 As bool
	  End Type
	End Union
End Type

Type reg_32 
	Union
		d As u32
		Type 
			w As u16
			no_use_w As u16
		End Type
		Type 
			l As u8
			no_use_h As u8
			no_use_lb As u8
			no_use_hb As u8
		End Type
	End Union
End Type

' registros principales
Dim Shared As u32 reg_d(8) 'reg_32
Dim Shared As u32 reg_a(8) 'reg_32
Dim Shared As reg_state reg_s
Dim Shared As reg_32 ptr eareg  'reg_32* 


Enum ' interrupciones
USER_VECTOR, AUTO_VECTOR, SPURIOUS, UNINITIALIZED
End Enum

Enum 
 flag_logical, flag_cmp, flag_add, flag_sub, flag_addx, flag_subx, flag_zn  
End Enum

Enum ADM
 DR_DIRECT 			= 0
 AR_DIRECT 			= 8
 AR_INDIRECT 		= 16
 AR_INDIRECT_INC 	= 24
 AR_INDIRECT_DEC 	= 32
 AR_INDIRECT_D16 	= 40
 AR_INDIRECT_D8 	= 48
 ABS_SHORT 			= 56
 ABS_LONG			= 57
 PC_INDIRECT_D16 	= 58
 PC_INDIRECT_D8 	= 59
 IMMEDIATE 			= 60
 UNSELECT 			= &hff
End Enum

Enum 
	BUS_ERROR, ADDRESS_ERROR
End Enum

Enum 
	ILLEGAL_OPCODE, PRIVILEGE, LINE_A, LINE_F, NONE
End Enum

Type status
        _program     As bool 
        _Read        As bool 
        _instruction As bool 
End Type
Dim Shared As status status_code



' -------------------------- varias funciones de registros --------------------------

Function escero(a As u32) As bool
	' devolvemos "0" ante un valor positivo o negativo, y un "1" ante un cero
	If a=0 Then Return 1 Else Return 0
End Function

Function noescero(a As u32) As bool
	' devolvemos "1" ante un valor 0 , y un "0" ante un valor positivo o negativp
	If a=0 Then Return 0 Else Return 1
End Function

Function getRegA(reg As u32) As u32
        return reg_a(reg And 7)
end Function

Function getRegD(reg As u32) As u32
        return reg_d(reg And 7)
end Function

Sub setRegA(reg As u32, value As u32) 
        reg_a(reg And 7) = value
End Sub 

Sub setRegD(reg As u32, value As u32) 
        reg_d(reg and 7) = value
End Sub 

Function getSR() As u32
        return reg_s.w 
end Function

Sub setSR(valor As u32) 
        reg_s.w = Valor
End Sub 

Function getRegIrd() As u32
        return reg_ird And &hFFFF
end Function

Sub setSSP(valor As u32)
        reg_ssp = Valor
End sub

Sub setCCR(valor As u32)
        reg_s.l = Valor ' long
End Sub 

Sub setZ()
	' no definido aun
End Sub


Function bits(size As U8) As U32
		Select Case size
			case SizeByte
				return 8
			case SizeWord
				return 16
			case SizeLong
				return 32
			Case else 
				return 0
		End Select
end Function

Function msb(size As U8) As U32
		Select Case size
			case SizeByte
			 return &h80
			case SizeWord
			 return &h8000
			case SizeLong
			 return &h80000000
			Case Else
			 return 0
		End select
end Function


Function mask(size As U8) As U32
		Select Case size
			case SizeByte
			 return &hFF
			case SizeWord
			 return &hFFFF
			case SizeLong
			 return &hFFFFFFFF
			Case Else
			 return 0
		End Select
end Function

Function maskval(value As u32, size As u8) As u32
        return value and mask(size)
end Function


Function isMemoryOperand() As bool
        if (adm <> DR_DIRECT and adm <> AR_DIRECT and adm <> IMMEDIATE) Then
           return CIERTO
        Else
           Return FALSO
        End if
end Function

Function isRegisterMode() As bool
        return IIf(eaReg<>0 , CIERTO , FALSO)
end Function

Sub incrementPc()
    reg_pc += 2
End sub
' ###############################################################################














' rotate ##########################################################################
function rotate_left(extend As bool,size As u8,shiftCount As u8, dato As u32) As u32
    Dim As u32 carry=FALSO
    Dim As u32 rotate

    If (extend) Then shiftCount=shiftCount Mod (bits(size)+1)
    If (shiftCount>0) Then
        if(escero(extend) And (shiftCount>bits(size))) Then 
        	   shiftCount And=(bits(size)-1)
        EndIf
        rotate=dato shr (bits(size)-shiftCount)
        carry=rotate and 1
        if(extend) Then
            dato=(((dato shl 1) or (reg_s.x And &Hff)) shl (shiftCount-1)) or (rotate shr 1)
        else
            dato shl=shiftCount
            dato Or=rotate
        End If
    End if
    setFlags(flag_logical,size,dato,0,0)
    If (extend) Then
        If (shiftCount>0) Then reg_s.x=carry
        reg_s.c=reg_s.x
    else
        reg_s.c=carry
    End If
    
    Return dato
End Function

Function rotate_right (extend As bool,size As u8,shiftCount As u8,dato As u32) As u32
    Dim As bool carry=FALSO
    Dim rotate As u32 

    if(extend) Then shiftCount=shiftCount Mod (bits(size)+1)
    if(shiftCount>0) Then
        If (escero(extend) And (shiftCount>bits(size))) Then 
        	   shiftCount And=(bits(size)-1)
        EndIf
        rotate=dato shl (bits(size)-shiftCount)
        carry=noescero(rotate and msb(size))
        if(extend) Then
            dato=(((dato shr 1) or reg_s.x shl (bits(size)-1)) shr (shiftCount-1)) or (rotate shl 1)
        else
            dato Shr=shiftCount
            dato Or=rotate
        End If
    End If
    setFlags(flag_logical,size,dato,0,0)
    if(extend) Then
        if(shiftCount>0) Then reg_s.x=carry
        reg_s.c=reg_s.x
    else
        reg_s.c=carry
    End If

    Return dato
End Function

' shift ##########################################################################
Function shift_left(arithmetic As bool, size As u8, shiftCount As u8, dato As u32) As u32
    Dim As bool carry=falso
    Dim As bool overflow=falso
    Dim As u32 mascara

    if(shiftCount>=bits(size)) Then
        overflow=noescero(dato)
        carry=IIf(shiftcount=bits(size),dato And 1,0)
        dato=0
    elseif(shiftCount>0) Then
        mascara=(mask(size) shl ((bits(size)-1)-shiftCount)) and mask(size)
        overflow=noescero(((dato and mascara)<>mascara) And ((dato and mascara)<>0))
        dato Shl=shiftCount-1
        carry=noescero(dato and msb(size))
        dato Shl=1
    End If
    setFlags(flag_logical,size,dato,0,0)
    reg_s.c=carry
    If (arithmetic) Then reg_s.v=overflow
    If (shiftCount<>0) Then reg_s.x=reg_s.c

    Return dato
End Function


Function shift_right(arithmetic As bool, size As u8, shiftCount As u8, dato As u32) As u32
    Dim As bool carry=falso
    Dim As bool sign=noescero(dato and msb(size))

    if(shiftCount>=bits(size)) Then
        dato=IIf(arithmetic,IIf(sign,mask(size),0),0)
        if(arithmetic) Then 
        	carry=sign
        Else 
      	carry=IIf(shiftCount=bits(size),sign,0)
    	  End if
    ElseIf(shiftCount>0) Then
        dato Shr=shiftCount-1
        carry=dato and 1
        dato shr =1
        if(arithmetic) Then 
        	  dato Or=(mask(size) shl (bits(size)-shiftCount)) and (IIf(sign,mask(size),0))
        EndIf
    End If
    setFlags(flag_logical,size,dato,0,0)
    reg_s.c=carry
    if(shiftCount<>0) Then reg_s.x=reg_s.c
    Return dato
End function
' ############################################################################3




























' opcodes ##########################################################################

' asl,asr,lsl,lsr,rol,ror,roxl,roxr									 .shift/rotate instructions
' bchg,bclr,bset,btst													 .bit manipulation instructions
' clr,nbcd,neg,negx,escero,scc,tas,tst									 .single operand instructions
' add,adda,and,cmp,cmpa,sub,suba,or,eor,mulu,muls,divu,divs	 .standard instructions
' move,movea																 .move instructions
' addi,addq,andi,cmpi,eori,ori,moveq,subi,subq					 .immediate instructions
' bcc,bra,bsr,dbcc														 .specificational instructions
' addx,cmpm,subx,abcd,sbcd												 .multiprecision instructions
' jmp,jsr,lea,pea,movem													 .misc 1 instructions
' andiccr,andisr,chk,eorisr,eoriccr,orisr,oriccr,move from sr.misc 2 instructions
' move to ccr,move to sr,exg,ext,link,moveusp,nop,reset      .misc 3 instructions   
' rte,rtr,rts,stop,swap,trap,trapv,unlk							 .misc 4 instructions
' movep																		 .peripheral instruction


'shift/rotate instruction
Sub op_xsx(izquierda As bool , memory As bool, arithmetic As bool) 

	Dim As u32 Dato
   Dim As u8 size=SizeWord
   Dim As u8 multi, regpos, shiftcount
   Dim As u16 sign,sign2
   Dim As bool carry,ir

	If (memory) Then
		dato=LoadEA(SizeWord,opcode And &h3F, FALSO, CIERTO, FALSO)
      prefetch(FALSO)
		sign=&h8000 and dato
		carry=IIf(izquierda,noescero(&h8000 and dato),dato And 1)
		dato =IIf(izquierda,dato shl 1,dato shr 1)
		if(arithmetic and escero(izquierda)) Then dato Or=sign
		sign2=&h8000 and dato
      setFlags(flag_logical,SizeWord,dato,0,0)
		reg_s.x=carry
		reg_s.c=carry
		if(arithmetic and izquierda) Then reg_s.v=IIf(sign <> sign2,1,0)
	Else
      prefetch(CIERTO)
      size  =(opcode shr 6) and 3
		ir    =(opcode shr 5) and 1
		multi =(opcode shr 9) and 7
		regPos= opcode and 7
      shiftCount=IIf(escero(ir),(IIf(multi=0,8,multi)),reg_d(multi) and 63)
      dato=LoadEA(size,regPos, FALSO, CIERTO, FALSO):'register direct
		dato=IIf(izquierda ,shift_left(arithmetic,size,shiftCount,dato) ,shift_right(arithmetic,size,shiftCount,dato))
		sync(2+(IIf(size=SizeLong,2,0))+shiftCount *2)
	End If	

    writeEA(size,dato,CIERTO)
End Sub

Sub op_rox(izquierda As bool, memory As bool, extend As bool ) 

	Dim As u32 dato
   dim As u8 size=SizeWord
   Dim As u8 multi, regpos, shiftcount
   Dim As u16 sign,sign2
   Dim As bool carry,ir
   
	if(memory) Then
		dato=LoadEA(SizeWord,opcode and &h3F, FALSO, CIERTO, FALSO)
      prefetch(FALSO)
		carry=IIf(izquierda,noescero(&h8000 and dato),dato And 1)
		dato =IIf(izquierda,dato shl 1,dato shr 1)
		If (extend) Then
			dato=IIf(izquierda,dato or (reg_s.x and &hff),dato or (reg_s.x shl 15))
		else
		   dato=IIf(izquierda,dato or (carry   And &hff),dato or (carry shl 15))
		End If
		setFlags(flag_logical,SizeWord,dato,0,0)
		reg_s.c=carry
		If (extend) Then reg_s.x=carry
	Else
      prefetch(CIERTO)
      size =(opcode shr 6) and 3
		ir   =(opcode shr 5) and 1
		multi=(opcode shr 9) and 7
		regPos=opcode and 7
	   shiftCount=IIf(escero(ir),IIf(multi=0,8,multi),reg_d(multi) and 63)
	   dato=LoadEA(size,regPos, FALSO, CIERTO, FALSO)'register direct
		dato=IIf(izquierda ,rotate_left(extend,size,shiftCount,dato) ,rotate_right(extend,size,shiftCount,dato))
		sync(2+iif( size=SizeLong , 2 , 0)+shiftCount *2)
	End If

    writeEA(size,dato,CIERTO)
End Sub

'bit manipulating instructions
Sub op_bchg(dinamic As bool) 


	Dim as u8 reg,bita
	Dim as u8 size=IIf(((opcode shr 3) and 7)=0,SizeLong,SizeByte)
   If(dinamic) Then
        reg=(opcode shr 9) and 7
        bita=IIf((size=SizeLong),reg_d(reg) and 31,reg_d(reg) and 7)
    else
        bita=IIf((size=SizeLong),reg_irc    And 31,reg_irc    And 7)
        readExtensionWord()
   End If
	Dim as u32 dato=LoadEA(size,opcode  and &h3F, FALSO, CIERTO, FALSO)
	dato Xor=(1 shl bita)
	reg_s.z=noescero((dato  and (1 shl bita)) shr bita)
   prefetch(isRegisterMode())
   If(size=SizeLong) Then
        sync(2)
        if(bita>15) Then sync(2)
   End If
   writeEA(size,dato,CIERTO)
End Sub

Sub op_bclr( dinamic as bool) 

	Dim as u8 reg,bita
   Dim as u8 size=IIf(((opcode shr 3) and 7)=0,SizeLong,SizeByte)
   If(dinamic) Then
        reg=(opcode shr 9) and 7
        bita=IIf((size=SizeLong),reg_d(reg) and 31,reg_d(reg) and 7)
   Else
        bita=IIf((size=SizeLong),reg_irc And 31,reg_irc  and 7)
        readExtensionWord()
   End If
	Dim as u32 dato=LoadEA(size,opcode And &h3F, FALSO, CIERTO, FALSO)
	reg_s.z=noescero(1 xor ((dato shr bita) and 1))
	dato And=(1 shl bita)*-1-1 ' equivale a "~"
   prefetch(isRegisterMode())
   If(size=SizeLong) Then
        sync(4)
        if(bita>15) Then sync(2)
   End If
   writeEA(size,dato,CIERTO)
End Sub

sub op_bset( dinamic as bool) 

    Dim as u8 reg,bita
    Dim as u8 size=IIf(((opcode shr 3) and 7)=0,SizeLong,SizeByte)
    if(dinamic) Then
        reg=(opcode shr 9) and 7
        bita= iif( (size=SizeLong) , reg_d(reg) and 31 , reg_d(reg) and 7 )
    else
        bita= iif( (size=SizeLong) , reg_irc  and 31 , reg_irc  and 7 )
        readExtensionWord()
    End If
	 Dim as u32 dato=LoadEA(size,opcode  and &h3F, FALSO, CIERTO, FALSO)
	 reg_s.z=noescero(1 xor ((dato shr bita) and 1))
	 dato Or=(1 shl bita)
    prefetch(isRegisterMode())
    if(size=SizeLong) Then
        sync(2)
        if(bita>15) Then sync(2)
    End If
    writeEA(size,dato,CIERTO)
end Sub

sub op_btst( dinamic as bool) 

    Dim as u8 reg,bita
    Dim as u8 size= iif( ((opcode shr 3) and 7)=0 , SizeLong , SizeByte )
    
    If (dinamic) Then
        reg=(opcode shr 9) and 7
        bita= iif( (size=SizeLong) , reg_d(reg) and 31 , reg_d(reg) and 7 )
    else
        bita= iif( (size=SizeLong) , reg_irc  and 31 , reg_irc and 7 )
        readExtensionWord()
    End If
	 Dim as u32 dato=LoadEA(size,opcode and &h3F, FALSO, CIERTO, FALSO)

	 reg_s.z=noescero(1 xor ((dato shr bita) and 1))
    prefetch(CIERTO)
	 If(size=SizeLong) Then sync(2)
end Sub


'single operand instructions
sub op_clr( size as u8 ) 

	LoadEA(size,opcode and &h3F, FALSO, CIERTO, FALSO)
	reg_s.z=1
	reg_s.n=0
	reg_s.v=0
	reg_s.c=0
   prefetch(isRegisterMode())
   If(adm=DR_DIRECT And size=SizeLong) Then sync(2)
   writeEA(size,0,CIERTO)
end Sub

Sub op_nbcd()

	 Dim as u8 dato=LoadEA(SizeByte,opcode  and &h3F, FALSO, CIERTO, FALSO)
	 Dim as u16 reslo=-(dato And &h0F)-reg_s.x
	 Dim as u16 reshi=-(dato and &hF0)
    Dim as u16 result,tmp_result
      result=resHi+resLo
      tmp_result=result
    if(resLo>9) Then result-=6
	   reg_s.x=IIf((result and &h1F0)>&h90,1,0)
	   reg_s.c=reg_s.x
	 If(reg_s.c) Then result-=&h60
    setFlags(flag_zn,SizeByte,result,0,0)
    reg_s.v=iif(((tmp_result and &h80)=&h80) and ((result and &h80)=0),1,0)
    prefetch(isRegisterMode())
    if(adm=DR_DIRECT) Then sync(2)
    writeEA(SizeByte,result,CIERTO)
End Sub

sub op_neg( negx as bool,size as u8 ) 

	Dim as u8 diff= iif( negx , reg_s.x , 0 )
	Dim as u32 dato=LoadEA(size,opcode  and &h3F, FALSO, CIERTO, FALSO)
   Dim as u64 result=0-CLngInt(dato)-CLngInt(diff)

	if(negx) Then
		setFlags(flag_subx,size,result,dato,0)
		setFlags(flag_zn  ,size,result   ,0,0)
	else
		setFlags(flag_sub ,size,result,dato,0)
	End If 
   prefetch(isRegisterMode())
   If(adm=DR_DIRECT  and size=SizeLong) Then sync(2)
   writeEA(size,result,CIERTO)
end Sub

sub op_not( size as u8 ) 

	Dim as u32 dato=LoadEA(size,opcode And &h3F, FALSO, CIERTO, FALSO)
	dato=dato*-1-1
   setFlags(flag_logical,size,dato,0,0)
   prefetch(isRegisterMode())
   If(adm=DR_DIRECT And size=SizeLong) Then sync(2)
   writeEA(size,dato,CIERTO)
end Sub

Sub op_scc()

	Dim as u8 cc=(opcode shr 8) and &hF
	LoadEA(SizeByte,opcode And &h3F, FALSO, CIERTO, FALSO)
   Dim as u8 dato= iif( conditionalTest(cc) , &hff , 0 )
   prefetch(isRegisterMode())
   If(dato And (adm=DR_DIRECT)) Then sync(2)
   writeEA(SizeByte,dato,CIERTO)
End Sub

Sub op_tas()

    Dim as u32 dato=LoadEA(SizeByte,opcode And &h3F, CIERTO, CIERTO, FALSO)
    If (isMemoryOperand()) Then
        rmwCycle=CIERTO
        dato=readByte(dato,0)
        updateRegAForIndirectAddressing(SizeByte,opcode And 7)
    End If
    dato And=&hff
    setFlags(flag_logical,SizeByte,dato,0,0)
	 dato Or=&h80
    if(adm <> DR_DIRECT) Then sync(2)
    writeEA(SizeByte,dato,CIERTO)
    rmwCycle=FALSO
    prefetch(CIERTO)
End Sub

sub op_tst( size as u8 ) 

	Dim as u32 dato=LoadEA(size,opcode  and &h3F, FALSO, CIERTO, FALSO)
    setFlags(flag_logical,size,dato,0,0)
    prefetch(CIERTO)
End Sub



'Standard Instructions
sub op_add( size as u8,writeEA2 as bool ) 


	Dim as u8 dregPos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(size,opcode and &h3F, FALSO, CIERTO, FALSO)
   Dim as u64 result=CLngInt((maskval(reg_d(dregPos),size)))+CLngInt(dato)

    setFlags(flag_add,size,result,dato,reg_d(dregPos))
    prefetch(escero(writeEA2)) ' revisar, parece ser un escero
    If (escero(writeEA2)) Then
        eaReg= @reg_d(dregPos)
        if(size=SizeLong) Then
            sync(2)
            if(escero(isMemoryOperand())) Then sync(2)
        End If
    End If
    writeEA(size,result,CIERTO)
end Sub

sub op_adda( size as u8 ) 

	 Dim as u8 aregPos=(opcode shr 9) and 7
	 Dim as u32 dato=LoadEA(size,opcode and &h3F, FALSO, CIERTO, FALSO)
    Dim as u32 result
    prefetch(CIERTO)
    if(size=SizeLong) Then
        result=reg_a(aregPos)+dato
        sync(2)
        if(escero(isMemoryOperand())) Then sync(2)
    else
        result=reg_a(aregPos)+cshort(dato and &hffff)
        sync(4)
    End If
	 eaReg= @reg_a(aregPos)
	 writeEA(SizeLong,result,CIERTO)
end Sub

sub op_and( size as u8,writeEA2 as bool ) 

	Dim as u8 dregPos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(size,opcode and &h3F, FALSO, CIERTO, FALSO)
	dato And=reg_d(dregPos)
    setFlags(flag_logical,size,dato,0,0)
    prefetch(escero(writeEA2))
    if(escero(writeEA2)) Then
        eaReg=@reg_d(dregPos) ' ojo con esta direccion
        if(size=SizeLong) Then
            sync(2)
            if(escero(isMemoryOperand())) Then sync(2)
        End If
    end if
    writeEA(size,dato,CIERTO)
end Sub

sub op_cmp( size as u8 ) 

	Dim as u8 dregPos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
   Dim as u64 result=CLngInt(maskval(reg_d(dregPos),size))-CLngInt(dato)
   setFlags(flag_cmp,size,result,dato,reg_d(dregPos))
   prefetch(CIERTO)
   If(size=SizeLong) Then sync(2)
End Sub

sub op_cmpa( size as u8 ) 

	Dim as u8 aregPos=(opcode shr 9) and 7
   Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
   If (size=SizeWord) Then
        dato=cshort(dato And &hFFFF)
   End If
   Dim as u64 result=CLngInt(reg_a(aregPos))-CLngInt(dato)
   setFlags(flag_cmp,SizeLong,result,dato,reg_a(aregPos))
   prefetch(CIERTO)
   sync(2)
end Sub

sub op_sub( size as u8,writeEA2 as bool ) 
	Dim as u8 dregPos=(opcode shr 9) and 7
   Dim as u64 result

	Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
    if(escero(writeEa2)) Then
        result=CLngInt(maskval(reg_d(dregPos),size))-CLngInt(dato)
        setFlags(flag_sub,size,result,dato,reg_d(dregPos))
    Else
        result=CLngInt(dato)-CLngInt(maskval(reg_d(dregPos),size))
        setFlags(flag_sub,size,result,reg_d(dregPos),dato)
    End If
    prefetch(escero(writeEA2))
    if(escero(writeEA2)) Then
        eaReg= @reg_d(dregPos)
        if(size=SizeLong) Then
            sync(2)
            if(escero(isMemoryOperand())) Then sync(2)
        End If
    End if
    writeEA(size,result,CIERTO)
end Sub

sub op_suba( size as u8 ) 

	Dim as u8 aregPos=(opcode shr 9) and 7
   Dim as u64 result
	Dim as u32 dato=LoadEA(size,opcode  and &h3F , FALSO, CIERTO, FALSO)
    prefetch(CIERTO)
    if(size=SizeLong) Then
        result=reg_a(aregPos)-dato
        sync(2)
        if(escero(isMemoryOperand())) Then sync(2)
    else
        result=CLngInt(reg_a(aregPos))-CLngInt(dato and &hffff)
        sync(4)
    End If
	eaReg= @reg_a(aregPos)
	writeEA(SizeLong,result,FALSO)
end Sub

sub op_or( size as u8,writeEA2 as bool ) 

	Dim as u8 dregPos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(size,opcode and &h3F , FALSO, CIERTO, FALSO)
	dato Or=reg_d(dregPos)
	setFlags(flag_logical,size,dato,0,0)
    prefetch(escero(writeEA2))
    if(escero(writeEa2)) Then
        eaReg= @reg_d(dregPos) ' ojo con esta direccion
        if(size=SizeLong) Then
            sync(2)
            if(escero(isMemoryOperand())) Then sync(2)
        End If
    End if
    writeEA(size,dato,CIERTO)
end Sub

sub op_eor( size as u8 ) 

	Dim as u8 dregPos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
	dato Xor=reg_d(dregPos)
	setFlags(flag_logical,size,dato,0,0)
    prefetch(isRegisterMode())
    if((adm=DR_DIRECT) and (size=SizeLong)) Then sync(4)
    writeEA(size,dato,escero(isRegisterMode()))
end Sub

Sub op_mulu()

	Dim as u8 regpos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(SizeWord,opcode And &h3F , FALSO, CIERTO, FALSO)
    dato And=&hFFFF
    Dim as u32 result=dato * (reg_d(regPos) And &hFFFF)
	 setFlags(flag_logical,SizeLong,result,0,0)
    prefetch(CIERTO)
    sync(34):'subtract prefetch time
    while(dato)
        if(dato And 1) Then sync(2)
        dato shr =1
    wend
	eaReg= @reg_d(regPos) ' ojo con esta direccion
	writeEA(SizeLong,result,FALSO)
End Sub

Sub op_muls()

	Dim as u8 regpos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(SizeWord,opcode  and &h3F , FALSO, CIERTO, FALSO)
	
    Dim as u32 result=CShort(dato) * CShort(reg_d(regPos) And &hFFFF)
	 setFlags(flag_logical,SizeLong,result,0,0)
    prefetch(CIERTO)
    sync(34) 'subtract prefetch time
    dato=((dato shl 1) xor dato) and &hffff
    while(dato)
        if(dato and 1) Then sync(2)
        dato shr =1
    wend
    eaReg= @reg_d(regPos) ' ojo con esta direccion
	 writeEA(SizeLong,result,FALSO)
End Sub

Sub op_divu()

    Dim as u8 regpos=(opcode shr 9) and 7
    reg_s.v=0
    reg_s.n=0
    reg_s.c=0
    reg_s.z=0
    Dim as u32 dato=LoadEA(SizeWord,opcode and &h3F , FALSO, CIERTO, FALSO)
    if(dato=0) Then
        sync(8)
        trapException(5)
        Exit Sub
    End If
    Dim as u32 result   =reg_d(regPos)\(dato and &hFFFF)
    Dim As u16 remainder=reg_d(regPos) Mod (dato And &hFFFF)
    sync(getDivu68kCycles(reg_d(regPos),dato And &hFFFF)-4) 'subtract prefetch time
    prefetch(CIERTO)
    if(result>&hFFFF) Then
        reg_s.v=1
        reg_s.n=1
        Exit Sub
    End If
    setFlags(flag_logical,SizeWord,result,0,0)
    result=(result and &hFFFF) or (remainder shl 16)
    eaReg= @reg_d(regPos)
    writeEA(SizeLong,result,FALSO)
End Sub

Sub op_divs()

	Dim as u8 regpos=(opcode shr 9) and 7
	reg_s.v=0
	reg_s.n=0
	reg_s.c=0
	reg_s.z=0

	Dim as u32 dato=LoadEA(SizeWord,opcode And &h3F , FALSO, CIERTO, FALSO)
	 If(dato=0) Then
		sync(8)
      trapException(5)
		Exit Sub
	 End If

    ' importante fallo si A=&h80000000 y b=&hFFFF, da error el freebasic
    ' espero que sea un caso aislado y no algo comun, por que no se como repararlo
    ' solo da error al redondear CSHORT(DATO AND &HFFFF) siendo DATO=&HFFFF
    Dim As Integer result=CInt(reg_d(regPos))  \  CShort(dato and &hFFFF)
    Dim As u16 remainder =cint(reg_d(regPos)) Mod CShort(dato And &hFFFF)

    sync(getDivs68kCycles(cint(reg_d(regPos)),cshort(dato And &hFFFF))-4) 'subtract prefetch time
    prefetch(CIERTO)
	 If(((result and &hffff8000)<>0) And ((result And &hffff8000) <> &hffff8000)) Then 
		reg_s.v=1
		reg_s.n=1
      Exit Sub
	 End If
    setFlags(flag_logical,SizeWord,result,0,0)
    eaReg= @reg_d(regPos)
    writeEA(SizeLong,(result And &hFFFF) or (remainder shl 16),FALSO)
End Sub

sub op_move( size as u8 ) 


    Dim as u8 destEA=(((opcode shr 6) and 7) shl 3) or ((opcode shr 9) and 7)
	 Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
    Dim as bool isClass2=IIf((isMemoryOperand() and (IIf(destEA = ABS_LONG,1,0))),1,0)
    Dim As bool inc_or_dec=IIf(adm=AR_INDIRECT_DEC,1,0)
   
    LoadEA(size,destEA, CIERTO,escero(isClass2),CIERTO)
	 setFlags(flag_logical,size,dato,0,0)
    If inc_or_dec Then prefetch(FALSO) 'dest adm
    writeEA(size,dato,inc_or_dec)

    updateRegAForIndirectAddressing(size,(opcode shr 9) and 7)
    
    If inc_or_dec=0 Then 
    	If isClass2 Then
    		fullprefetch(CIERTO)
    	Else
    		prefetch(CIERTO)
    	EndIf
    EndIf

end Sub

sub op_movea( size as u8 ) 

	Dim as u8 regpos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(size,opcode and &h3F , FALSO, CIERTO, FALSO)
    reg_a(regPos)= iif( size=SizeWord , CShort(dato), dato )
    prefetch(CIERTO)
End Sub


'Immediate instructions
sub op_addi( size as u8 ) 

	Dim as u32 immediate=LoadEA(size,&h3C, FALSO, CIERTO, FALSO)
	Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
    Dim as u64 result=CLngInt(immediate)+CLngInt(dato)
    setFlags(flag_add,size,result,immediate,dato)
    prefetch(isRegisterMode())
    if((size=SizeLong) and (adm=DR_DIRECT)) Then sync(4)
    writeEA(size,result,CIERTO)
end Sub

sub op_addq( size as u8 ) 

	Dim as u8 immediate=(opcode shr 9) And 7
	If (immediate=0) Then immediate=8
	Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
   Dim as u64 result=CLngInt(immediate)+CLngInt(dato)
   
	if(adm <> AR_DIRECT) Then setFlags(flag_add,size,result,immediate,dato)
    prefetch(isRegisterMode())
    if(adm=AR_DIRECT) Then 
    	sync(4)
    else 
      If(size=SizeLong And adm=DR_DIRECT) Then sync(4)
    EndIf
    writeEA(size,result,CIERTO)
end Sub

sub op_subq( size as u8 ) 

    Dim as u8 immediate=(opcode shr 9) and 7
    if(immediate=0) Then immediate=8
    Dim as u32 dato=LoadEA(size,opcode And &h3F , FALSO, CIERTO, FALSO)
    Dim as u64 result=CLngInt(dato)-CLngInt(immediate)
    if(adm <>AR_DIRECT) Then setFlags(flag_sub,size,result,immediate,dato)
    prefetch(isRegisterMode())
    if(adm=AR_DIRECT) Then 
    	sync(4)
    else 
      If(size=SizeLong  and adm=DR_DIRECT) Then sync(4)
    EndIf
    writeEA(size,result,CIERTO)
end Sub

sub op_andi( size as u8 ) 

	Dim as u32 immediate=LoadEA(size,&h3C, FALSO, CIERTO, FALSO)
	Dim as u32 dato=LoadEA(size,opcode and &h3F , FALSO, CIERTO, FALSO)
	 dato And=immediate
    setFlags(flag_logical,size,dato,0,0)
    prefetch(isRegisterMode())
    if((size=SizeLong) and (adm=DR_DIRECT)) Then sync(4)
    writeEA(size,dato,CIERTO)
end Sub

sub op_cmpi( size as u8 ) 

	Dim as u32 immediate=LoadEA(size,&h3C, FALSO, CIERTO, FALSO)
	Dim as u32 dato=LoadEA(size,opcode  and &h3F , FALSO, CIERTO, FALSO)
   Dim as u64 result=CLngInt(dato)-CLngInt(immediate)
   '   If dato=13 Then Cls:Print result, dato:sleep
	 setFlags(flag_cmp,size,result,immediate,dato)
    prefetch(CIERTO)
    if((size=SizeLong) and (adm=DR_DIRECT)) Then sync(2)
end Sub

sub op_eori( size as u8 ) 

	Dim as u32 immediate=LoadEA(size,&h3C, FALSO, CIERTO, FALSO)
	Dim as u32 dato=LoadEA(size,opcode  and &h3F , FALSO, CIERTO, FALSO)
	 dato Xor=immediate
	 setFlags(flag_logical,size,dato,0,0)
    prefetch(isRegisterMode())
    if((size=SizeLong) and (adm=DR_DIRECT)) Then sync(4)
    writeEA(size,dato,CIERTO)
end Sub

sub op_ori( size as u8 ) 

	Dim as u32 immediate=LoadEA(size,&h3C, FALSO, CIERTO, FALSO)
	Dim as u32 dato=LoadEA(size,opcode and &h3F , FALSO, CIERTO, FALSO)
	 dato Or=immediate
	 setFlags(flag_logical,size,dato,0,0)
    prefetch(isRegisterMode())
    If((size=SizeLong) and (adm=DR_DIRECT)) Then sync(4)
    writeEA(size,dato,CIERTO)
end Sub

Sub op_moveq()

	Dim as u8 regpos=(opcode shr 9) and 7
   Dim as u32 dato=cbyte(opcode and &hFF)
    reg_d(regPos)=dato
    setFlags(flag_logical,SizeLong,dato,0,0)
    prefetch(CIERTO)
End Sub

sub op_subi( size as u8 ) 

	Dim as u32 immediate=LoadEA(size,&h3C, FALSO, CIERTO, FALSO)
	Dim as u32 dato=LoadEA(size,opcode and &h3F , FALSO, CIERTO, FALSO)
   Dim as u64 result=CLngInt(dato)-CLngInt(immediate)
	 setFlags(flag_sub,size,result,immediate,dato)
    prefetch(isRegisterMode())
    if((size=SizeLong) and (adm=DR_DIRECT)) Then sync(4)
    writeEA(size,result,CIERTO)
End Sub



'specificational instructions
Sub op_bcc()

	Dim as u8 displacement=opcode  and &hFF
	Dim as u8 cond=(opcode shr 8) and &hF
	
	if(conditionalTest(cond)) Then
        reg_pc+= iif( displacement=0 , cshort(reg_irc) , cbyte(displacement))
		  sync(2)
        fullprefetch(CIERTO)
	Else
		  sync(4)
        if(displacement=0) Then
            readExtensionWord()
        End If
        prefetch(CIERTO)
	End If
	
End Sub

Sub op_bra()
 
	Dim as u8 displacement=opcode and &hFF
    reg_pc+= iif( displacement=0 , cshort(reg_irc) , cbyte(displacement))
	 sync(2)
    fullprefetch(CIERTO)
End Sub

Sub op_bsr()'todo check stacked reg_pc at address error

	Dim as u8 displacement=opcode and &hFF
    sync(2)
    writeStack(IIf (displacement=0 , reg_pc+2 , reg_pc) , FALSO)
    Dim as u32 reg_pc_new= reg_pc+iif(displacement=0 , cshort(reg_irc) , cbyte(displacement))
    reg_irc=readWord(reg_pc_new,FALSO)
    reg_pc=reg_pc_new
    prefetch(CIERTO)
End Sub

Sub op_dbcc()

	Dim as u8 reg = opcode And 7
	Dim as u8 cond=(opcode shr 8) and &hF
	' necesario guardar reg. para no tocar la parte "L"
	Dim As u32 loguardo=reg_d(reg) 
	' solo tocamos la parte W (o sea, WORD, no LONG)
	reg_d(reg) And=&hFFFF
	

	if(escero(conditionalTest(cond))) Then
        sync(2)
		  reg_d(reg)-=1 
        if(CShort(reg_d(reg))<>-1) Then
            reg_pc+=CShort(reg_irc)
            fullprefetch(CIERTO)
				' recuperamos y parte "L" y mezclamos nueva parte "W"
				reg_d(reg)=(loguardo And &hFFFF0000) Or (reg_d(reg) And &hFFFF)
            Exit Sub
        End If
        readWord(reg_pc+2,0):'todo:dummy read
	else
	     sync(4)
	End If

	' recuperamos y parte "L" y mezclamos nueva parte "W"
	reg_d(reg)=(loguardo And &hFFFF0000) Or (reg_d(reg) And &hFFFF)
   incrementPc()
   fullprefetch(CIERTO)
End Sub


'misc 1 instructions
Sub op_jmp()

	Dim as u32 addr=LoadEA(SizeLong,opcode and &h3F , CIERTO, FALSO, FALSO)
    if(adm=AR_INDIRECT_D8 or adm=PC_INDIRECT_D8) Then 
    	sync(4):'todo:dummy read ,  , 'todo:dummy read? )
    ElseIf (adm=AR_INDIRECT_D16 or adm=ABS_SHORT or adm=PC_INDIRECT_D16) Then sync(2)
    End If
    reg_pc=addr
    fullprefetch(CIERTO)
End Sub

Sub op_jsr()

	Dim as u32 addr=LoadEA(SizeLong,opcode and &h3F , CIERTO, FALSO, FALSO)
    if(adm=AR_INDIRECT_D8 or adm=PC_INDIRECT_D8) Then
    	sync(4):'todo:dummy read
    ElseIf((adm=AR_INDIRECT_D16) or (adm=ABS_SHORT) or (adm=PC_INDIRECT_D16)) Then sync(2)
    End if
    dim as u32 pcNextOp=reg_pc
    reg_pc=addr
    fullprefetchFirstStep()
    writeStack(pcNextOp,FALSO)
    prefetch(CIERTO)
End Sub

Sub op_lea()

	 Dim as u8 regpos=(opcode shr 9) and 7
    reg_a(regPos)=LoadEA(SizeLong,opcode and &h3F , CIERTO, CIERTO, FALSO)
	 If((adm=AR_INDIRECT_D8) or (adm=PC_INDIRECT_D8)) Then sync(2)
    prefetch(CIERTO)
End Sub

Sub op_pea()

	 Dim as u32 addr=LoadEA(SizeLong,opcode  and &h3F , CIERTO, CIERTO, FALSO)
	 If((adm=AR_INDIRECT_D8) or (adm=PC_INDIRECT_D8)) Then sync(2)
    Dim As bool isAbs=((adm=ABS_SHORT) or (adm=ABS_LONG))
    if(isAbs) Then
        writeStack(addr,FALSO)
        prefetch(CIERTO)
    else
        prefetch(FALSO)
        writeStack(addr,CIERTO)
    End If
End Sub

sub op_movem(size as u8,memToReg as bool) 

	Dim as u8 regpos=opcode And 7
   Dim as u8 byteCount= iif( size=SizeWord , 2 , 4 )
	Dim as u16 regmask=reg_irc
     readExtensionWord()
   Dim As reg_32 ptr eaReg2
   Dim As bool isBitSet
   
	  LoadEA(size,opcode And &h3F , CIERTO,CIERTO,CIERTO)
     If (memToReg And (size=SizeLong)) Then readWord(eaAddr,0)

   Dim i As integer
   for i=0 To 15
    	  If (escero(regMask)) Then Exit For
        isBitSet=regMask And 1
        regMask shr =1
        If (escero(isBitSet)) Then Continue For
		  If (adm = AR_INDIRECT_DEC) Then 	
		   	eaReg2 = IIf( i>7 , @reg_d(((i-8)*-1-1) and 7) , @reg_a((i*-1-1) and 7) )
		  Else 
		      eaReg2 = iif( i>7 , @reg_a(i-8) , @reg_d(i) )
		  EndIf
		  ' -----------------------------
        Dim As u32 dato=eaReg2->d ' contenido de

        if(adm=AR_INDIRECT_DEC) Then
            If (eaReg2 = @reg_a(regPos)) Then 
            	dato=reg_a(regPos)
            EndIf
            If size=SizeWord then
            	writeWord(eaAddr,dato, FALSO) 
            Else 
            	writeLong(eaAddr,dato, FALSO)
            EndIf
            if(escero(regMask)) Then reg_a(regPos)=eaAddr : Exit For
            eaAddr-=byteCount
        ElseIf (adm=AR_INDIRECT_INC) Then
            If (size=SizeWord) Then
            	eaReg2->w=readWord(eaAddr, FALSO) And &hffff 'dir puntero
            else 
            	eaReg2->d=readLong(eaAddr, FALSO) 'dir puntero
            EndIf
            eaAddr+=byteCount
            If (escero(regMask)) Then reg_a(regPos)=eaAddr: Exit For
        else
            If (memToReg) Then 
            	If (size=SizeWord) Then 
            		eaReg2->w=readWord(eaAddr, FALSO) And &hffff 'dir puntero
            	Else 
            		eaReg2->d=readLong(eaAddr, FALSO) 'dir puntero
            	EndIf
            else 
            	If Size=SizeWord Then 
            		writeWord(eaAddr,eaReg2->w, FALSO) 
            	else 
            		writeLong(eaAddr,eaReg2->d, FALSO)
            	EndIf
            End If
            eaAddr+=byteCount
            If (escero(regMask)) Then Exit For
        End If
	Next i
    if(memToReg  and (size=SizeWord)) Then readWord(eaAddr, FALSO):'dummy read,but can cause a bus error
    prefetch(CIERTO)

End Sub


'multiprecision instructions
sub op_addx( size as u8,MemToMem as bool ) 

	Dim as u8 Ry=opcode  and 7
	Dim as u8 Rx=(opcode shr 9) and 7:'dest
	Dim As u32 mem_src,mem_dest
   Dim as u64 result
    
   If(escero(memTomem)) Then
      result=CLngInt(maskval(reg_d(Rx),size))+CLngInt(maskval(reg_d(Ry),size))+CLngInt(reg_s.x)
		setFlags(flag_addx,size,result,reg_d(Ry),reg_d(Rx))
		eaReg= @reg_d(Rx)
	else
		mem_src =LoadEA(size,(4 shl 3) or Ry, FALSO,CIERTO, FALSO)
		mem_dest=LoadEA(size,(4 shl 3) or Rx ,FALSO,CIERTO,CIERTO)
      result=CLngInt(mem_dest)+CLngInt(mem_src)+CLngInt(reg_s.x)
		setFlags(flag_addx,size,result,mem_src,mem_dest)
   End If
   
	setFlags(flag_zn,size,result,0,0)
	
   If(memTomem And (size=SizeLong)) Then
       writeWord(eaAddr+2,result And &hFFFF,FALSO)
   End If
   
   prefetch(escero(memTomem))
   
   If (escero(memTomem And (size=SizeLong))) Then sync(4)
   
   If (memTomem And (size=SizeLong)) Then
        writeWord(eaAddr,(result shr 16) and &hFFFF,CIERTO)
   Else
        writeEA(size,result,CIERTO)
   End If
end Sub

sub op_cmpm( size as u8 ) 

	Dim as u8  regAy= opcode and 7
	Dim As u8  regAx=(opcode shr 9) and 7
	Dim As u32 src =LoadEA(size,(3 shl 3) or RegAy, FALSO, CIERTO, FALSO)
	Dim As u32 dest=LoadEA(size,(3 shl 3) or RegAx, FALSO, CIERTO, FALSO)
   Dim as u32 result=dest-src
	setFlags(flag_cmp,size,result,src,dest)
   prefetch(CIERTO)
end Sub

sub op_subx( size as u8,MemToMem as bool ) 

	Dim as u8 Rx=opcode  and 7
	Dim as u8 Ry=(opcode shr 9) and 7:'dest
   Dim as u64 result
   Dim As u32 mem_src, mem_dest
   
   If (escero(memTomem)) Then 'dato-dato
      result=clngint(maskval(reg_d(Ry),size))-clngint(maskval(reg_d(Rx),size))-CLngInt(reg_s.x)
		setFlags(flag_subx,size,result,reg_d(Rx),reg_d(Ry))
		eaReg= @reg_d(Ry)
	else
      mem_src =LoadEA(size,(4 shl 3) or Rx, FALSO, CIERTO, FALSO)
      mem_dest=LoadEA(size,(4 shl 3) or Ry ,FALSO, CIERTO, CIERTO)
      result=CLngInt(mem_dest)-CLngInt(mem_src)-CLngInt(reg_s.x)
		setFlags(flag_subx,size,result,mem_src,mem_dest)
   End If
   
   setFlags(flag_zn,size,result,0,0)
   
   If(memTomem  and (size=SizeLong)) Then
        writeWord(eaAddr+2,result  and &hFFFF,FALSO)
   End If
   prefetch(escero(memTomem))
   If (escero(memTomem  and (size=SizeLong))) Then sync(4)
   If (memTomem  and (size=SizeLong)) Then
        writeWord(eaAddr,(result shr 16) and &hFFFF,CIERTO)
   Else
        writeEA(size,result,CIERTO)
   End if
end Sub

sub op_abcd( MemToMem as bool ) 

	Dim as u8 Ry=opcode  and 7
	Dim as u8 Rx=(opcode shr 9) and 7:'dest
	Dim As u32 mem_src,mem_dest
	Dim as u16 result,tmp_result
	Dim as u16 result_lo,result_hi
   If(escero(memTomem)) Then 'dato-dato
      result_lo=(reg_d(Rx) and &h0F)+(reg_d(Ry) and &h0F)+reg_s.x
      result_hi=(reg_d(Rx) and &hF0)+(reg_d(Ry) and &hF0)
		eaReg= @reg_d(Rx) ' direccion de
      prefetch(CIERTO)
		sync(2)
	else
		mem_src =LoadEA(SizeByte,(4 shl 3) or Ry , FALSO, CIERTO, FALSO)
		mem_dest=LoadEA(SizeByte,(4 shl 3) or Rx , FALSO, CIERTO, CIERTO)
      result_lo=(mem_src And &h0F)+(mem_dest And &h0F)+reg_s.x
      result_hi=(mem_src And &hF0)+(mem_dest And &hF0)
      prefetch(FALSO)
   End If
   result=result_hi+result_lo
   tmp_result=result
	if(result_lo>9) Then result+=6
	reg_s.c=IIf((result And &h3f0)>&h90,1,0)
	if(reg_s.c) Then result+=&h60
	reg_s.x=reg_s.c
   reg_s.v=IIf(((tmp_result and &h80)=0) and ((result And &h80)=&h80),1,0)
	setFlags(flag_zn,SizeByte,result,0,0)
   writeEA(SizeByte,result,CIERTO)
end Sub

sub op_sbcd( MemToMem as bool ) 

	Dim as u8 Rx=opcode  and 7
	Dim as u8 Ry=(opcode shr 9) and 7:'dest
	Dim As u32 mem_src,mem_dest
	Dim as u16 result,tmp_result
	Dim as u16 result_lo,result_hi
	
   If(escero(memTomem)) Then
		mem_src=reg_d(Rx)
		mem_dest=reg_d(Ry)
		eaReg= @reg_d(Ry)
      prefetch(CIERTO)
      sync(2)
	else
		mem_src =LoadEA(SizeByte,(4 shl 3) or Rx, FALSO,CIERTO, FALSO)
		mem_dest=LoadEA(SizeByte,(4 shl 3) or Ry ,FALSO,CIERTO,CIERTO)
      prefetch(FALSO)
   End If
   
   result_lo=(mem_dest And &h0F)-(mem_src And &h0F)-reg_s.x
   result_hi=(mem_dest And &hF0)-(mem_src And &hF0)
	result=result_hi+result_lo
	tmp_result=result
	
	Dim As Integer bcd=0
	
	If (result_lo And &hF0) Then result-=6:bcd=6
	If ((((mem_dest And &hFF)-(mem_src  and &hFF)-reg_s.x) and &h100)>&hFF) Then result-=&h60
	reg_s.c=(((mem_dest  and &hFF)-(mem_src  and &hFF)-bcd-reg_s.x) and &h300)>&hFF
	reg_s.x=reg_s.c
   reg_s.v=((tmp_result  and &h80)=&h80) and ((result  and &h80)=0)
	setFlags(flag_zn,SizeByte,result,0,0)
   writeEA(SizeByte,result,CIERTO)
End Sub


'misc 2instructions
Sub op_chk()

	Dim as u8 regpos=(opcode shr 9) and 7
	Dim as u32 dato=LoadEA(SizeWord,opcode  and &h3F , FALSO, CIERTO, FALSO)
	
    prefetch(CIERTO)
    reg_s.v=0
    reg_s.c=0
    reg_s.n=0
    reg_s.z=IIf((reg_d(regPos) and &hffff)=0,1,0)
    sync(4)

    if(CShort(reg_d(regPos))>CShort(dato)) Then
		reg_s.n=0
		sync(4)
      trapException(6)
		Exit Sub
    End If
    sync(2)
    if(CShort(reg_d(regPos))<0) Then
		reg_s.n=1
		sync(4)
      trapException(6)
		Exit Sub
    End If
End Sub

Sub op_eoriccr()

	Dim as u32 dato=LoadEA(SizeByte,7 shl 3 or 4, FALSO, CIERTO, FALSO)
   sync(8)
	reg_s.l Xor=(dato and &h1F)
   readWord(reg_pc+2,0) 'dummy read
   prefetch(CIERTO)
End Sub

Sub op_andiccr()

    Dim as u32 dato=LoadEA(SizeByte,7 shl 3 or 4, FALSO, CIERTO, FALSO)
    sync(8)
    reg_s.l  And=(dato And &h1F)
    readWord(reg_pc+2,0) 'dummy read
    prefetch(CIERTO)
End Sub

Sub op_oriccr()

    Dim as u32 dato=LoadEA(SizeByte,7 shl 3 or 4, FALSO, CIERTO, FALSO)
    sync(8)
    reg_s.l Or=(dato and &h1F)
    readWord(reg_pc+2,0) 'dummy read
    prefetch(CIERTO)
End Sub

Sub op_orisr()

    if(escero(reg_s.s)) Then
        setPrivilegeException()
        Exit Sub
    End If
    Dim as u32 dato=LoadEA(SizeWord,7 shl 3 or 4, FALSO, CIERTO, FALSO)
    sync(8)
    reg_s.w Or=dato And &hFFFF ' mirar el registro REG_S, debe cogerlo ENTERO
    reg_s.w And=&hA71F
    switchToUser()
    readWord(reg_pc+2,0):'dummy read
    prefetch(CIERTO)
End Sub

Sub op_eorisr()

	if(escero(reg_s.s)) Then
        setPrivilegeException()
		Exit Sub
	End If
	Dim as u32 dato=LoadEA(SizeWord,7 shl 3 or 4, FALSO, CIERTO, FALSO)
   sync(8)
	reg_s.w Xor=dato And &hFFFF
	reg_s.w And=&hA71F
   switchToUser()
   readWord(reg_pc+2,0):'dummy read
   prefetch(CIERTO)
End Sub

Sub op_andisr()

    if(escero(reg_s.s)) Then
        setPrivilegeException()
        Exit Sub
    End If
    
    Dim as u32 dato=LoadEA(SizeWord,7 shl 3 or 4, FALSO, CIERTO, FALSO)
    sync(8)
    reg_s.w And=dato And &hFFFF
    reg_s.w And=&hA71F
    switchToUser()
    readWord(reg_pc+2,0) 'dummy read
    prefetch(CIERTO)
End Sub

Sub op_movefromsr()

    LoadEA(SizeWord,opcode and &h3F , FALSO, CIERTO, FALSO)
    prefetch(isRegisterMode())
	 If (adm=DR_DIRECT) Then sync(2)
    writeEA(SizeWord,reg_s.w And &ha71f,CIERTO)
End Sub

Sub op_movetoccr()

	 Dim as u32 dato=LoadEA(SizeWord,opcode and &h3F , FALSO, CIERTO, FALSO)
    sync(4)
	 reg_s.l=dato And &h1f
    readWord(reg_pc+2,0) 'dummy read
    prefetch(CIERTO)
End Sub

Sub op_movetosr()

	if(escero(reg_s.s)) Then
      setPrivilegeException()
		Exit sub
	End If
	Dim as u32 dato=LoadEA(SizeWord,opcode and &h3F , FALSO, CIERTO, FALSO)
    sync(4)
    reg_s.w=dato And &ha71f
    switchToUser()
    readWord(reg_pc+2,0)
    prefetch(CIERTO)
End Sub

sub op_exg( opmode as u8 ) 

	Dim as u8 Rx=(opcode shr 9) and 7
	Dim as u8 Ry=opcode  and 7
	Dim as u32 temp
   prefetch(CIERTO)

	Select Case (opmode)
		case 8: 'date registers
			temp=reg_d(Rx)
			reg_d(Rx)=reg_d(Ry)
			reg_d(Ry)=temp
		case 9: 'address registers
			temp=reg_a(Rx)
			reg_a(Rx)=reg_a(Ry)
			reg_a(Ry)=temp
		case 17: 'date and address register
			temp=reg_d(Rx)
			reg_d(Rx)=reg_a(Ry)
			reg_a(Ry)=temp
	End Select
	sync(2)
end Sub

sub op_ext( LongSize as bool ) 

    Dim As u8 Rx=opcode And 7

    if(longSize) Then
        reg_d(Rx) And=&h0000FFFF
        reg_d(Rx) = reg_d(Rx) or iif(reg_d(Rx) and &h8000 , &hFFFF shl 16 , 0) 
        setFlags(flag_logical,SizeLong,reg_d(Rx),0,0)
    Else
    	  Dim As u32 loguardo=reg_d(rx) And &hFFFF0000 ' guardo solo la palabra sup.
        reg_d(Rx) And=&h00FF
        reg_d(Rx) = reg_d(Rx) or iif(reg_d(Rx) And &h80 , &hFF shl 8 , 0) 
        reg_d(Rx) Or=loguardo ' le sumamos lo que nos hemos cargado
        setFlags(flag_logical,SizeWord,reg_d(Rx),0,0)
    End If
    prefetch(CIERTO)
end Sub

Sub op_link()

	 Dim as u8 regpos=opcode and 7
    Dim As Integer _val=CShort(reg_irc)
    readExtensionWord()
    Dim As u32 _decSP=reg_a(7)-4
    Dim as u32 dato= iif( regPos=7 , _decSP , reg_a(regPos) )
    writeLong(_decSP,dato, FALSO)
    reg_a(7)=_decSP
	 reg_a(regPos)=reg_a(7)
    reg_a(7)+=_val
    prefetch(CIERTO)
End Sub

sub op_moveusp( dr as bool ) 

	if(escero(reg_s.s)) Then
      setPrivilegeException()
		Exit Sub
	End If
	Dim as u8 regpos=opcode and 7
   If (dr) Then
		reg_a(regPos)=reg_usp
	else
		reg_usp=reg_a(regPos)
   End If	
   prefetch(CIERTO)
end Sub

Sub op_nop()
    prefetch(CIERTO)
End Sub

Sub op_reset()

	if(escero(reg_s.s)) Then
      setPrivilegeException()
		Exit Sub
	End If
    sync(128)
    'Print "Estudiar OP_RESET, no existe resetinstruction":Sleep
    ' resetInstruction() ' estudiarlo, no existe
    prefetch(CIERTO)
End Sub

Sub op_rte()

	if(escero(reg_s.s)) Then
      setPrivilegeException()
		Exit Sub
	End If
	reg_s.w=readWord(reg_a(7),0) ' estudiar el .w
	reg_s.w And=&hA71F           ' idem
	reg_a(7)+=2
	reg_pc=readLong(reg_a(7),0)
	reg_a(7)+=4
   switchToUser()
   fullprefetch(CIERTO)
End Sub

Sub op_rtr()

	reg_s.l=readWord(reg_a(7),0) and &h1f
	reg_a(7)+=2
	reg_pc=readLong(reg_a(7),0)
	reg_a(7)+=4
   fullprefetch(CIERTO)
End Sub

Sub op_rts()

	reg_pc=readLong(reg_a(7),0)
	reg_a(7)+=4
   fullprefetch(CIERTO)
End Sub

Sub op_stop()

    if(escero(reg_s.s)) Then
        setPrivilegeException()
        Exit Sub
    End If
    reg_s.w=reg_irc 
    reg_s.w And=&hA71F
    sync(2)
    _stop=CIERTO
    incrementPc()
    sampleIrq()
    sync(2)
End Sub

Sub op_swap()

	Dim as u8 regpos=opcode And 7
   reg_d(regPos)=(reg_d(regPos) shr 16) or ((reg_d(regPos) and &hFFFF) shl 16)
	setFlags(flag_logical,SizeLong,reg_d(regPos),0,0)
   prefetch(CIERTO)
End Sub

Sub op_trap()

	sync(4)
   trapException((opcode  and &hF)+32)
End Sub

Sub op_trapv()

    prefetch(CIERTO)
    if(reg_s.v) Then
        trapException(7)
    End If
End Sub

Sub op_unlk()

	Dim as u8 regpos=opcode  and 7
	reg_a(7)=reg_a(regPos)
	reg_a(regPos)=LoadEA(SizeLong,(2 shl 3) or 7, FALSO, CIERTO, FALSO)
   If(regPos <> 7) Then reg_a(7)+=4
   prefetch(CIERTO)
End Sub

sub op_movep( opmode as u8 ) 

	Dim as u32 dato
	Dim As u8 regposA=opcode and 7
	Dim As u8 regposD=(opcode shr 9) and 7
   LoadEA(SizeByte,5 shl 3 or regPosA, CIERTO, CIERTO, FALSO)

	Select Case (opmode)
		case 4: 'word: mem . reg
         dato=readByte(eaAddr,0):eaAddr+=2
			reg_d(regPosD) Or=(dato And &hFF) Shl 8
         dato=readByte(eaAddr,0)
			reg_d(regPosD) or=(dato And &hFF) 
		case 5: 'long: mem . reg
			dato=readByte(eaAddr,0):eaAddr+=2
			reg_d(regPosD) Or=(dato And &hFF) Shl 24
			dato=readByte(eaAddr,0):eaAddr+=2
			reg_d(regPosD) Or=(dato And &hFF) Shl 16
			dato=readByte(eaAddr,0):eaAddr+=2
			reg_d(regPosD) Or=(dato And &hFF) Shl 8
			dato=readByte(eaAddr,0)
			reg_d(regPosD) or=(dato And &hFF)
		Case 6: 'word: reg . mem
			writeByte(eaAddr,(reg_d(regPosD) And &hFF00) Shr 8,0):eaAddr+=2
			writeByte(eaAddr,(reg_d(regPosD) And &h00FF)      ,0)
		case 7: 'long: reg . mem
			writeByte(eaAddr,(reg_d(regPosD) And &hFF000000) Shr 24,0):eaAddr+=2
			writeByte(eaAddr,(reg_d(regPosD) And &h00FF0000) Shr 16,0):eaAddr+=2
			writeByte(eaAddr,(reg_d(regPosD) And &h0000FF00) Shr  8,0):eaAddr+=2
         writeByte(eaAddr,(reg_d(regPosD) And &h000000FF)       ,0)
	End Select
   prefetch(CIERTO)
end Sub

' PARA LA INSTRUCCION "DIVU" EXCLUSIVAMENTE
function getDivu68kCycles(dividendo As u32, divisor As u32) As Integer
   If((dividendo shr 16)>=divisor) Then return 10:'quotient is bigger than 16bit
   
	Dim As u32 hdivisor=divisor shl 16
	Dim mcycles As Integer =38
	Dim i As Integer
	For i=0 To 14
        if(CInt(dividendo)<0) Then
            dividendo shl =1
            dividendo-=hdivisor
        else
            dividendo shl =1
            mcycles+=2
            if(dividendo>=hdivisor) Then
                dividendo-=hdivisor
                mcycles-=1
            End If
        End if
    Next i
    return mcycles *2
End Function

' PARA LA INSTRUCCION "DIVS" EXCLUSIVAMENTE
Function getDivs68kCycles(dividendo As integer,divisor As short) As Integer
	 Dim mcycles As Integer =6
    if(dividendo<0) Then mcycles+=1
    if((abs(dividendo) shr 16)>=abs(divisor)) Then return(mcycles+2)*2
    
    mcycles+=55
    
    if(divisor>=0) Then
        if(dividendo>=0) Then 
        	 mcycles-=1
        else 
    	    mcycles+=1
    	   End if
    End If
    
   Dim As u32 aquot=abs(dividendo)/abs(divisor)
	Dim i As Integer
	For i=0 To 14
        if(CShort(aquot)>=0) Then mcycles+=1
        aquot shl =1
	Next i
   return mcycles *2

End function

' ####################### FIN DE OPCODES ###################














''''''''''''''''''''''''    EXCEPCIONES E INTERRUPCIONES    '''''''''''''''''''''''''''

' ********************** OP ILEGAL *********************
Sub op_illegal()
   'Print "Instruccion ilegal: ";Hex(opcode,4);" en: ";Hex(reg_pc,8):SLEEP
    Dim as u8 nibble=opcode shr 12:'line A OR line F detection
    illegalMode=ILLEGAL_OPCODE
    if(nibble=&hA) Then
        illegalMode=LINE_A
    ElseIf(nibble=&hF) Then
        illegalMode=LINE_F
    End If
    trace=FALSO
    illegalException(illegalmode)

End Sub
' ********************************************************


' excepciones ##########################################################################
Sub setInterrupt(level as u8)
    irqPendingLevel=level And 7
End Sub

Sub sampleIrq()
    irqSamplingLevel=irqPendingLevel
End Sub

Function interruptException(level as u8) As u32
	 Dim as u8 vector
	 Dim As u16 SR
    irqSamplingLevel=0
    _stop=FALSO
    SR=reg_s.w
    reg_s.intr=level
    switchToSupervisor()
    reg_s.trace=0
    sync(6)
    reg_a(7)-=6
    writeWord(reg_a(7)+4,reg_pc And &hFFFF, FALSO)
    vector=getInterruptVector(level)
    sync(4)
    writeWord(reg_a(7)+0,SR, FALSO)
    writeWord(reg_a(7)+2,(reg_pc shr 16) and &hFFFF, FALSO)
    executeAt(vector)
    Return 0
End Function

Function getInterruptVector(level As u8) As u32
    level And=7
    sync(4)
    Select Case (interrupt)
    	case AUTO_VECTOR
    		return 24 + level
    	Case USER_VECTOR
    		return 0 ' por estudiar -> getUserVector(level) and  &hff
    	Case SPURIOUS
    		return 24
    	Case UNINITIALIZED
    		return 15
    End Select
    Return 0
End Function

Sub setPrivilegeException()
    illegalMode=PRIVILEGE
    trace=FALSO
    reg_pc-=2
End Sub

Sub illegalException(tipo as u8)
    illegalMode=NONE
    Dim As u16 SR=reg_s.w
    switchToSupervisor()
    reg_s.trace=0
    sync(4)
    reg_a(7)-=6
    writeWord(reg_a(7)+4,reg_pc  and &hFFFF, FALSO)
    writeWord(reg_a(7)+0,SR, FALSO)
    writeWord(reg_a(7)+2,(reg_pc shr 16) and &hFFFF, FALSO)
    if(tipo=ILLEGAL_OPCODE) Then
        executeAt(4)
    ElseIf(tipo=LINE_A) Then
        executeAt(10)
    ElseIf(tipo=LINE_F) Then
        executeAt(11)
    ElseIf(tipo=PRIVILEGE) Then
        executeAt(8)
    End If
End Sub

Sub traceException()
    Dim As u16 SR=reg_s.w
    switchToSupervisor()
    reg_s.trace=0
    _stop=FALSO
    sync(4)
    reg_a(7)-=6
    writeWord(reg_a(7)+4,reg_pc  and &hFFFF, FALSO)
    writeWord(reg_a(7)+0,SR, FALSO)
    writeWord(reg_a(7)+2,(reg_pc shr 16) and &hFFFF, FALSO)
    executeAt(9)
End Sub

Sub trapException(vector as u8)
    Dim As u16 SR=reg_s.w
    reg_s.trace=0
    switchToSupervisor()
    reg_a(7)-=6
    writeWord(reg_a(7)+4,reg_pc  and &hFFFF, FALSO)
    writeWord(reg_a(7)+0,SR, FALSO)
    writeWord(reg_a(7)+2,(reg_pc shr 16) and &hFFFF, FALSO)
    executeAt(vector)
End Sub

Sub group0exception(tipo as u8)
    if(doubleFault) Then
    	'Print "Doble IRQ, error grabe de sistema":Sleep:End
    End If
    doubleFault = CIERTO
    Dim As u16 SR=reg_s.w
    Dim As u16 _status 
    _status = iif(status_code._read,16,0) or iif(status_code._instruction,0,8) 
    _status Or= iif(SR and &h2000,4,0) or IIf(status_code._program,2,1)
    reg_s.trace=trace=0
    switchToSupervisor()
    sync(8)
    reg_a(7)-=14
    writeWord(reg_a(7)+12,reg_pc and &hFFFF, FALSO)
    writeWord(reg_a(7)+8,SR, FALSO)
    writeWord(reg_a(7)+10,(reg_pc shr 16) and &hFFFF, FALSO)
    writeWord(reg_a(7)+6,reg_ird, FALSO)
    writeWord(reg_a(7)+4,(fault_address) and &hFFFF, FALSO)
    writeWord(reg_a(7)+0,_status, FALSO)
    writeWord(reg_a(7)+2,((fault_address) shr 16) and &hFFFF, FALSO)
    sync(2)
    executeAt (iif(tipo=BUS_ERROR,2,3))
    doubleFault=FALSO
    ' si todo falla
    'Print "TODO FALLA:Error grabe de sistema":Sleep:End
End Sub

Sub group1exceptions()
    status_code._instruction=FALSO
    If (trace) Then traceException()
    if((irqSamplingLevel=7) or (irqSamplingLevel>reg_s.intr)) Then
        interruptException(irqSamplingLevel)
    End If
    if( illegalMode <> NONE) Then illegalException(illegalMode)
    status_code._instruction=CIERTO
End Sub

Sub executeAt(vector as u8)
    reg_pc=readLong(vector shl 2,0)
    fullprefetchFirstStep()
    sync(2)
    prefetch(CIERTO)
End Sub

Sub switchToSupervisor()
    If (escero(reg_s.s)) Then
        reg_usp=reg_a(7)
        reg_a(7)=reg_ssp
        reg_s.s=CIERTO
    End If
End Sub

Sub switchToUser()
    if (escero(reg_s.s)) Then
        reg_ssp=reg_a(7)
        reg_a(7)=reg_usp
    End If
End Sub 








' ********************************************************
' **************** rutinas varias de CPU *****************
Sub setFlags(Tipo As u8, size As u8, result As u64, src As u32, dest As u32)
	
   'Print "Tipo:";tipo;" size:";size;" result:";Hex(result,8);" src:";Hex(src,8);" dest:";Hex(dest,8)
   
   ' quitamos el bit mas alto del signo (lo dejamos sin signo)
	Dim As bool ResN =IIf((result And msb(size)), 1, 0)
	Dim As bool DestN=IIf((dest   And msb(size)), 1, 0)
	Dim As bool SrcN =IIf((src    And msb(size)), 1, 0)
	
	'Print "resn, destn, srcn:";resn;" ";destn;" ";srcn

	Select Case (tipo)
		case flag_logical
				reg_s.v=0
				reg_s.c=0
            reg_s.z=IIf(maskval(result,size)=0,1,0)
				reg_s.n=ResN
		case flag_sub
            reg_s.z=IIf(maskval(result,size)=0,1,0)
            reg_s.n=ResN
			   reg_s.c=(result shr (bits(size))) and 1
            reg_s.x=reg_s.c
				reg_s.v=(SrcN xor DestN) and (ResN xor DestN)
		case flag_cmp
            reg_s.z=IIf(maskval(result,size)=0,1,0)
            reg_s.n=ResN
			   reg_s.c=(result shr (bits(size))) and 1
			   reg_s.v=(SrcN xor DestN) and (ResN xor DestN)
		case flag_subx
            reg_s.c=(result shr (bits(size))) and 1
            reg_s.x=reg_s.c
            reg_s.v=(SrcN xor DestN) and (ResN xor DestN)
		case flag_add
            reg_s.z=IIf(maskval(result,size)=0,1,0)
				reg_s.n=ResN
				reg_s.c=(result shr (bits(size))) and 1
				reg_s.x=reg_s.c
				reg_s.v=(SrcN xor ResN) And (DestN xor ResN)
		case flag_addx
            reg_s.c=(result shr (bits(size))) and 1
            reg_s.x=reg_s.c
				reg_s.v=(SrcN xor ResN) And (DestN xor ResN)
		case flag_zn
            reg_s.z=reg_s.z And IIf(maskval(result,size)=0,1,0)
				reg_s.n=ResN
	End Select
	
End Sub

Sub writeEA(size as u8,dato As u32,lastBusCycle as bool)

 If deb Then Print "write EA:";Hex(eareg,8),dato,Hex(eaaddr,8)

	If (eaReg) Then
		Select Case (size)
			case SizeByte:
				eaReg->l = dato And &hFF
			case SizeWord:
				eaReg->w = dato And &hFFFF
			Case SizeLong: 
				eaReg->d = dato
		End Select
		Exit Sub
   End If

	Select Case (size)
		case SizeByte:
			writeByte(eaAddr, dato And &hFF  , lastBusCycle)
		case SizeWord:
			writeWord(eaAddr, dato And &hFFFF, lastBusCycle)
		case SizeLong:
			writeLong(eaAddr, dato           , lastBusCycle)
	End Select
	
End Sub	
	
Function LoadEA(size as u8, ea As u8 , noReadFromEA As bool, fetchLastExtension As bool, noSyncDec As bool) As u32
	Dim As u32 operand
   Dim As u32 dispReg
   Dim as u8  displacement
   
	Dim as u8 regpos=ea And 7
	Dim As u8 mode=(ea shr 3) and 7

	eaAddr=0
	eaReg=0
   adm=UNSELECT
   
	Select Case (mode)
		'Register direct
		case 0: 'Dn
		   		If deb Then Print "0: Reg Direct D"
				adm=DR_DIRECT
				eaReg= @reg_d(regPos)
	         Return maskval(reg_d(regPos),size)
		case 1: 'An
				   If deb Then Print "1: Reg Direct A"
				adm=AR_DIRECT
				eaReg= @reg_a(regPos)
	         Return maskval(reg_a(regPos),size)
		'Register indirect
		case 2: '(An)
				   If deb Then Print "2: Reg InDirect (A)"
            adm=AR_INDIRECT
            eaAddr=reg_a(regPos)
		case 3: '(An)+
				   If deb Then Print "3: Reg InDirect (A)+"
            adm=AR_INDIRECT_INC
            eaAddr=reg_a(regPos)
		Case 4: '-(An)
				   If deb Then Print "4: Reg InDirect -(A)"
            adm=AR_INDIRECT_DEC
            eaAddr=reg_a(regPos)
            if(size=SizeByte) Then
                if(regPos=7) Then 
                	eaAddr-=2
                else 
                	eaAddr-=1
                End If
            ElseIf(size=SizeWord) Then
                eaAddr-=2
            else
                eaAddr-=4
            End If
            If (escero(noSyncDec)) Then sync(2)
		case 5: '(d16, An)
				   If deb Then Print "5: Reg InDirect (D,A)"
            adm=AR_INDIRECT_D16
            eaAddr=reg_a(regPos)+CShort(reg_irc)
            if(fetchLastExtension) Then 
            	readExtensionWord()
            Else 
            	incrementPc()
            End If
		case 6: 'Address Register Indirect with Index
			If deb Then Print "6: Address Register Indirect with Index"
			adm=AR_INDIRECT_D8
         eaAddr=reg_a(regPos)
d8Xn: ' no puede ser: UN GOTOOOOOOoooooo...........................................
			displacement=reg_irc  and &hFF
         dispReg= iif( reg_irc  and &h8000 , reg_a((reg_irc  and &h7000) shr 12) , reg_d((reg_irc  and &h7000) shr 12) )
         eaAddr+= iif( escero(reg_irc  and &h800) , cshort(dispReg) , dispReg )
			eaAddr+=CByte(displacement)
			sync(2)
         If(fetchLastExtension) Then 
         	readExtensionWord()
         Else 
            incrementPc()
         End If

		case 7:
			Select Case (regPos)
				case 0:  'absolute Word
				If deb Then Print "7-0: absolute word"
					adm=ABS_SHORT
               eaAddr=CShort(reg_irc)
               If(fetchLastExtension) Then 
               	readExtensionWord()
               Else 
                  incrementPc()
               End If
				case 1: 'absolute Long
				If deb Then Print "7-1: absolute long"
					adm=ABS_LONG
					eaAddr=reg_irc shl 16
               readExtensionWord()
					eaAddr Or=reg_irc
               If(fetchLastExtension) Then 
               	readExtensionWord()
               Else 
               	incrementPc()
               End If
				case 2: 'd16 PC, Program Counter Indirect with Displacement Mode
				If deb Then Print "7-2: PC indirect"
					adm=PC_INDIRECT_D16
               eaAddr=reg_pc+CShort(reg_irc)
               If(fetchLastExtension) Then 
                 readExtensionWord()
               Else 
                 incrementPc()
               End If
				case 3: 'd8 PC Xn, Program Counter Indirect with Index (8-Bit Displacement) Mode
				If deb Then Print "7-3: PC + index"
					adm=PC_INDIRECT_D8
               eaAddr=reg_pc
					goto d8Xn ' EEEEIIIIiiinnnnn!!!!!!!!!!!!!!!!!!! COMOOOOOOOOO.....!!!!
				case 4: 'immediate
				If deb Then Print "7-4: Inmediato"
					adm=IMMEDIATE
					if(size=SizeByte) Then
                  operand=reg_irc And &hff
					ElseIf(size=SizeWord) Then
						operand=reg_irc
					Else
						operand=reg_irc shl 16
                  readExtensionWord()
						operand Or=reg_irc
					End If
               readExtensionWord()
					return operand
			End Select
		End Select
	
   If deb Then Print "Efective Address: ";Hex(eaAddr,8)
   eaAddr And=&H00FFFFFF ' obligatorio para quitar el exceso
   hardware(eaAddr)
   
   If (noReadFromEA) Then return eaAddr
	
	Select Case (size)
		case SizeByte: operand = readByte(eaAddr,0)
		case SizeWord: operand = readWord(eaAddr,0)
		case SizeLong: operand = readLong(eaAddr,0)
	End Select
	
	updateRegAForIndirectAddressing(size,regPos)

   		If deb Then Print "OPERAND: ";Hex(operand,8)
   
	return operand
End Function


Sub updateRegAForIndirectAddressing(size as u8,regpos As u8)
    If (adm=AR_INDIRECT_INC) Then
        if(size=SizeByte) Then
            if(regPos=7) Then 
            	reg_a(regPos)+=2
            else 
            	reg_a(regPos)+=1
            End if
        ElseIf (size=SizeWord) Then
            reg_a(regPos)+=2
        else
            reg_a(regPos)+=4
        End If
    ElseIf (adm=AR_INDIRECT_DEC) Then
        reg_a(regPos)=eaAddr
    End If
End Sub


Function conditionalTest(code as u8) As bool
	 'T          T  true                             1
    'F          F  false                            0
    'HI         HI high                           !C&!Z
    'LS         LS low or same                     C+Z
    'CC         CC carry clear                     !C
    'CS         CS carry set                        C
    'NE         NE not equal                       !Z
    'EQ         EQ equal                            Z
    'VC         VC overflow clear                  !V
    'VS         VS overflow set                     V
    'PL         PL plus                            !N
    'MI         MI minus                            N
    'GE         GE greater or equal            (N&V)+(!N&!V)
    'LT         LT less than                  (N&!V)+(!N&V)
    'GT         GT greater than             (N&V&!Z)+(!N&!V&!Z)
    'LE         LE less or equal            Z+(N&!V)+(!N&V)
	Select Case (code And &hF)
		case  0: return CIERTO
		case  1: return FALSO
		case  2: return escero(reg_s.c) and escero(reg_s.z)
		case  3: return reg_s.c or reg_s.z
		case  4: return escero(reg_s.c)
		case  5: return reg_s.c
		case  6: return escero(reg_s.z)
		case  7: return reg_s.z
		case  8: return escero(reg_s.v)
		case  9: return reg_s.v
		case 10: return escero(reg_s.n)
		case 11: return reg_s.n
		Case 12: return (reg_s.n And reg_s.v) Or (escero(reg_s.n) and escero(reg_s.v))
		Case 13: return (reg_s.n And escero(reg_s.v)) Or (escero(reg_s.n) and reg_s.v)
		case 14: return (reg_s.n and reg_s.v and escero(reg_s.z)) or ( escero(reg_s.n) and escero(reg_s.v) and escero(reg_s.z))
		Case 15: return  reg_s.z or (reg_s.n and escero(reg_s.v)) or ((escero(reg_s.n) and reg_s.v))
	End Select
End Function
	

Sub sync(dato As u8)
   executed_cycles+=dato
End Sub





' ############### LECTURA PREVIA DE MEMORIA ###########################

Sub fullprefetchFirstStep()
    status_code._program=CIERTO
    reg_irc=readWord(reg_pc,0)
    status_code._program=FALSO
End Sub

sub fullprefetch(LastCycle as bool)
    status_code._program=CIERTO
    reg_irc=readWord(reg_pc,0)
    prefetch(lastCycle)
End Sub

Sub prefetch(LastCycle as bool)
    status_code._program=CIERTO
    reg_ir=reg_irc
    reg_irc=readWord(reg_pc+2,lastCycle)
    reg_ird=reg_ir
    status_code._program=FALSO
End Sub

Sub readExtensionWord()
    incrementPc()
    status_code._program=CIERTO
    reg_irc=readWord(reg_pc,0)
    status_code._program=FALSO
End Sub




' #################### MEMORIA #########################
Sub writeStack(valor as u32,LastCycle as bool)
	 reg_a(7)-=4
    writeLong(reg_a(7),valor,lastCycle)
End Sub

Function readByte(addr as u32,LastCycle as bool) As u8
	 Dim as u8 dato

    status_code._read=CIERTO
    fault_address=addr
    sync(2)
    If (lastCycle) Then sampleIrq()
    dato=memByteRead(addr and &hffffff)
    sync(2)
    
	 Return dato
end Function

Function readWord(addr as u32,LastCycle as bool) As u16
	 Dim as u16 word

    status_code._read=CIERTO
    fault_address=addr
    sync(2)
    if((addr And 1)=1) Then
        group0exception(ADDRESS_ERROR) ' error, direccion impar
    End If
    If (lastCycle) Then sampleIrq()
    word=memWordRead(addr and &hffffff)
    sync(2)

	 Return word
End Function

Function readLong(addr as u32,LastCycle as bool) As u32
	Dim as u32 dword=readWord(addr,0) shl 16
   Dword Or=readWord(addr+2,lastCycle) 
	return Dword
end Function

'''''''''''''''''''

Sub writeByte(addr as u32,valor as u8,LastCycle as bool)
    status_code._read=FALSO
    fault_address=addr
    sync(2)
    If lastCycle Then sampleIrq()
    memByteWrite(addr And &hffffff,valor)
    sync(2)
End Sub

Sub writeWord(addr as u32,valor As u16,LastCycle as bool)
    status_code._read=FALSO
    fault_address=addr
    sync(2)
	 If((addr and 1)=1) Then
        group0exception(ADDRESS_ERROR) ' error, direccion impar
	 End If
    If lastCycle Then sampleIrq()
    memWordWrite(addr And &hffffff,valor)
    sync(2)
End Sub

Sub writeLong(addr As u32,valor as u32,LastCycle as bool)
    writeWord(addr,(valor shr 16) and &hFFFF,0)
    writeWord(addr+2,valor and &hFFFF,lastCycle)
End Sub






' ############## PRINCIPAL ###############
Sub exec_ins()

   Dim code As String
   Dim sa As String

   Dim As integer param1, param2, param3
   Dim As u8 ia

   sa=opcodes(opcode)
   ia=InStr(sa,",")
   If ia Then
   	' primer parametro
   	code=Left(sa,ia-1)
   	param1=Val(Mid(sa,ia+1))
   	If param1=-1 Then param1=FALSO
   	If param1=-2 Then param1=CIERTO
   	' segundo, si lo hay
		ia=InStr(ia+1,sa,",")
		If ia Then 
			param2=Val(Mid(sa,ia+1))
			If param2=-1 Then param2=FALSO
			If param2=-2 Then param2=CIERTO
		EndIf
		' tercero, si lo hay
		ia=InStr(ia+1,sa,",")
		If ia Then 
			param3=Val(Mid(sa,ia+1))
			If param3=-1 Then param3=FALSO
			If param3=-2 Then param3=CIERTO
		EndIf
   Else
   	' sin parametros
   	code=sa
   EndIf

	If tecla=1 And ciclosejecutados<>0 Then GoTo no_deb
	 If DEB Then
		Locate 13,1
		Print "REG-PC: ";Hex(reg_pc-2,8);"    OP-CODE: ";Hex(opcode,4);" -- ";sa;"            "
	 End If   
	no_deb:
	
	if code="op_xsx" then        op_xsx( param1 , param2 , param3 ) :Exit sub     
	if code="op_rox" then        op_rox( param1 , param2 , param3 ) :Exit sub 
	if code="op_bchg" then       op_bchg( param1 ) :Exit sub 
	if code="op_bclr" then       op_bclr( param1 ) :Exit sub 
	if code="op_bset" then       op_bset( param1 ) :Exit sub 
	if code="op_btst" then       op_btst( param1 ) :Exit sub 
	if code="op_clr" then        op_clr( param1 ) :Exit sub 
	if code="op_nbcd" then       op_nbcd():Exit sub 
	if code="op_neg" then        op_neg( param1 ,param2 ) :Exit sub  
	if code="op_not" then        op_not( param1 ) :Exit sub 
	if code="op_scc" then        op_scc():Exit sub 
	if code="op_tas" then        op_tas():Exit sub 
	if code="op_tst" then        op_tst( param1 ) :Exit sub 
	if code="op_add" then        op_add( param1 ,param2 ) :Exit sub 
	if code="op_adda" then       op_adda( param1 ) :Exit sub 
	if code="op_and" then        op_and( param1 ,param2 ) :Exit sub 
	if code="op_cmp" then        op_cmp( param1 ) :Exit sub 
	if code="op_cmpa" then       op_cmpa( param1 ) :Exit sub 
	if code="op_sub" then        op_sub( param1 ,param2 ) :Exit sub 
	if code="op_suba" then       op_suba( param1  ) :Exit sub 
	if code="op_or" then         op_or( param1 ,param2 ) :Exit sub 
	if code="op_eor" then        op_eor( param1 ) :Exit sub 
	if code="op_mulu" then       op_mulu() :Exit sub 
	if code="op_muls" then       op_muls() :Exit sub 
	if code="op_divu" then       op_divu() :Exit sub 
	if code="op_divs" then       op_divs() :Exit sub 
	if code="op_move" then       op_move( param1 ) :Exit sub 
	if code="op_movea" then      op_movea( param1 ) :Exit sub 
	if code="op_addi" then       op_addi( param1 ) :Exit sub 
	if code="op_addq" then       op_addq( param1 ) :Exit sub 
	if code="op_subq" then       op_subq( param1 ) :Exit sub 
	if code="op_andi" then       op_andi( param1 ) :Exit sub 
	if code="op_cmpi" then       op_cmpi( param1 ) :Exit sub 
	if code="op_eori" then       op_eori( param1 ) :Exit sub 
	if code="op_ori" then        op_ori( param1 ) :Exit sub 
	if code="op_moveq" then      op_moveq():Exit sub 
	if code="op_subi" then       op_subi( param1 ) :Exit sub 
	if code="op_bcc" then        op_bcc() :Exit sub 
	if code="op_bra" then        op_bra() :Exit sub 
	if code="op_bsr" then        op_bsr() :Exit sub 
	if code="op_dbcc" then       op_dbcc() :Exit sub 
	if code="op_jmp" then        op_jmp() :Exit sub 
	if code="op_jsr" then        op_jsr() :Exit sub 
	if code="op_lea" then        op_lea() :Exit sub 
	if code="op_pea" then        op_pea() :Exit sub 
	if code="op_movem" then      op_movem( param1 ,param2 ):Exit sub  
	if code="op_addx" then       op_addx( param1 ,param2 ) :Exit sub 
	if code="op_cmpm" then       op_cmpm( param1 ) :Exit sub 
	if code="op_subx" then       op_subx( param1 ,param2 ) :Exit sub 
	if code="op_abcd" then       op_abcd( param1 ) :Exit sub 
	if code="op_sbcd" then       op_sbcd( param1 ) :Exit sub 
	if code="op_chk" then        op_chk():Exit sub 
	if code="op_eoriccr" then    op_eoriccr():Exit sub 
	if code="op_andiccr" then    op_andiccr():Exit sub 
	if code="op_oriccr" then     op_oriccr():Exit sub 
	if code="op_orisr" then      op_orisr():Exit sub 
	if code="op_eorisr" then     op_eorisr():Exit sub 
	if code="op_andisr" then     op_andisr():Exit sub 
	if code="op_movefromsr" then op_movefromsr():Exit sub 
	if code="op_movetoccr" then  op_movetoccr():Exit sub 
	if code="op_movetosr" then   op_movetosr():Exit sub 
	if code="op_exg" then        op_exg( param1 ) :Exit sub 
	if code="op_ext" then        op_ext( param1 ) :Exit sub 
	if code="op_link" then       op_link():Exit sub 
	if code="op_moveusp" then    op_moveusp( param1 ) :Exit sub 
	if code="op_nop" then        op_nop():Exit sub 
	if code="op_reset" then      op_reset():Exit sub 
	if code="op_rte" then        op_rte():Exit sub 
	if code="op_rtr" then        op_rtr():Exit sub 
	if code="op_rts" then        op_rts():Exit sub 
	if code="op_stop" then       op_stop():Exit sub 
	if code="op_swap" then       op_swap():Exit sub 
	if code="op_trap" then       op_trap():Exit sub 
	if code="op_trapv" then      op_trapv():Exit sub 
	if code="op_unlk" then       op_unlk():Exit sub 
	if code="op_movep" then      op_movep( param1 ):Exit sub 

 	' resto de casos "op_illegal"
	op_illegal() ' para generar excepciones o IRQs

End Sub


Sub debug ()
	If tecla=1 Then If ciclosejecutados<>0 Then Exit Sub

	Locate 1,1
   Dim i As Integer
   Dim j As Integer 
   
	Print "  D0:";Hex(reg_d(0),8);
	Print "  D1:";Hex(reg_d(1),8);
	Print "  D2:";Hex(reg_d(2),8);
	Print "  D3:";Hex(reg_d(3),8)
	Print "  D4:";Hex(reg_d(4),8);
	Print "  D5:";Hex(reg_d(5),8);
	Print "  D6:";Hex(reg_d(6),8);
	Print "  D7:";Hex(reg_d(7),8)
	Print
	Print "  A0:";Hex(reg_a(0),8);
	Print "  A1:";Hex(reg_a(1),8);
	Print "  A2:";Hex(reg_a(2),8);
	Print "  A3:";Hex(reg_a(3),8)
	Print "  A4:";Hex(reg_a(4),8);
	Print "  A5:";Hex(reg_a(5),8);
	Print "  A6:";Hex(reg_a(6),8);
	Print "  A7:";Hex(reg_a(7),8)
	print
	Print " Supervisor:";Hex(reg_ssp,8),
	Print "    Usuario:";Hex(reg_usp,8)
	Print
	Print "Estado CPU:    --IRQ--"
	Print "T  .  S  .  .  2  1  0  .  .  .  X  N  Z  V  C "	
	For i=0 To 15
		Print Mid(Bin(reg_s.w,16),i+1,1);"  ";
	Next
	Print
		
	   'Locate 14,1
	   'For i=14 To 24:Print Space(52):Next   	
	   'Locate 14,1
		
	' contenido de la pila	
	Locate 19,52:Print "PILA:"
	For i=0 To 15
		Locate i+20,52
		Dim pruebapila As u32=reg_a(7) ' sino, seria esta....
		Print Hex(pruebapila-(i*4),6);
		Print "-";Hex(readLong(pruebapila-(i*4),0),8)
	Next
	
	
	' unas pocas direcciones de RAM
	Dim verram As Integer=&h112 '&h2f80
	Locate 19,90:Print "RAM:"		
	For i=0 To 16
		Locate i+20,69
		Print Hex(verram+(i*8),8);":"
		For j=0 To 7
		 Locate i+20,78+(j*3)
		 Print Hex(ram(verram+(i*8)+j),2);
		Next
	Next
	
	' ciclos ejecutados
   Locate 47,1
   Print "Ciclos:";executed_cycles;
		
End Sub






Sub Initialize(inirom As u32)
    reg_usp=0
    Dim i As Integer

    For i=0 To 7
     reg_d(i)=0
     reg_a(i)=0
    Next
    
    irqPendingLevel=0
    irqSamplingLevel=0
    rmwCycle=FALSO
    _stop=FALSO
    doubleFault=FALSO
    trace=FALSO
    illegalMode=NONE
    reg_s.w=&h2700
    sync(16)
    ' las ROM en un sistema 68000 siempre empiezan con los 4 bytes de la pila A7 (SP)
    ' seguido de los 4 del inicio de arranque, y se leen tal cual (no se invierten bytes)
    ' o sea, si pone "00 40 10 00" no es 0x104000 sino 0x401000"
    ' la CPU al arrancar, considera "0" como direccion de inicio de la ROM y una vez leidos
    ' los 8 primeros bytes, la coloca en su sitio, y deja de ser "0" como inicio
    reg_a(7)=readLong(inirom+0,0) ' localizacion de la pila 
    reg_ssp=reg_a(7)
    reg_pc=readLong(inirom+4,0) ' leemos el inicio del programa
    fullprefetch(FALSO)
End Sub
