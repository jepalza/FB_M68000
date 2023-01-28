
declare Sub hardware(eaAddr As u32) ' aqui metemos las rutinas hard
Declare Sub debug()

Declare function LoadEA(size As u8, ea As u8 , noReadFromEA As bool, fetchLastExtension As bool, noSyncDec As bool) As u32
Declare sub sync(dato As u8)
Declare sub fullprefetch(LastCycle as bool)
Declare Sub fullprefetchFirstStep()
Declare Sub prefetch(LastCycle as bool)
Declare Sub readExtensionWord()

'''''

Declare Function getRegA(reg As u32) As u32
Declare Function getRegD(reg As u32) As u32
Declare Sub setRegA(reg As u32, value As u32) 
Declare Sub setRegD(reg As u32, value As u32) 
Declare Function getSR() As u32
Declare Sub setSR(valor As u32) 
Declare Function getRegIrd() As u32
Declare Sub setSSP(valor As u32)
Declare Sub setCCR(valor As u32)
Declare Function bits_(size As U8) As U32
Declare Function msb_(size As U8) As U32
Declare Function mask_(size As U8) As U32
Declare Function maskVal_(value As u32, size As u8) As u32
Declare Function isMemoryOperand() As bool
Declare Function isRegisterMode() As bool
Declare Sub incrementPc()

' rotaciones
Declare Function rotate_left (extend As bool,size As u8, shiftCount As u8, dato As u32) As u32
Declare Function rotate_right(extend As bool,size As u8, shiftCount As u8, dato As u32) As u32
Declare Function shift_left  (arithmetic As bool, size As u8, shiftCount As u8, dato As u32) As u32
Declare Function shift_right (arithmetic As bool, size As u8, shiftCount As u8, dato As u32) As u32




' ######################  MEMORY #######################################
Declare Function readByte(addr as u32,LastCycle as bool)As u8
Declare Function readWord(addr as u32,LastCycle as bool) As u16
Declare Function readLong(addr as u32,LastCycle as bool) As u32
Declare Sub writeByte(addr as u32,dato as u8 ,LastCycle as bool)
Declare Sub writeWord(addr as u32,dato As u16,LastCycle as bool)
Declare Sub writeLong(addr As u32,dato as u32,LastCycle as bool)
Declare Sub writeStack(datoue as u32,LastCycle as bool)
Declare Function memByteRead(addr As u32) As u8
Declare Function memWordRead(addr As u32) As u16
Declare Sub memByteWrite(addr As u32, valor As u8)
Declare Sub memWordWrite(addr As u32, valor As u16)

' ####################################################################
Declare Sub group1exceptions()
Declare Sub group0exception(tipo as u8)'bus or address error
Declare Sub setInterrupt(level as u8)
Declare Sub sampleIrq()
Declare Sub setPrivilegeException()
Declare Sub illegalException(iType as u8)
Declare Sub traceException()
Declare Sub trapException(vector as u8)'group 2exceptions will triggered within opcode
Declare Sub executeAt(vector as u8)
Declare Sub switchToSupervisor()
Declare Sub process()'execute next opcode
Declare Sub initialize(inirom As u32)
Declare Sub switchToUser()
Declare Sub setFlags(Tipo As u8, size As u8, result As u64, src As u32, dest As u32)
Declare Sub writeEA(size as u8,datoue As u32,lastBusCode as bool)
Declare Sub updateRegAForIndirectAddressing(size as u8,regpos As u8)
Declare function interruptException(level as u8) As u32
Declare Function getInterruptVector(level As u8) As u32
Declare function conditionalTest(code as u8) As u8

' exclusivas de DIVS y DIVU
Declare Function getDivu68kCycles(dividend As u32, divisor As u32) As Integer
Declare Function getDivs68kCycles(dividend As integer, divisor As short) As Integer

' ######################## opcodes ##########################
Declare Sub op_illegal() ' generica para desconocidas
Declare Sub op_xsx(lleft As bool , memory As bool, arithmetic As bool) 
Declare Sub op_rox(lleft As bool, memory As bool, extend As bool ) 
Declare Sub op_bchg(dinamic As bool) 
Declare Sub op_bclr( dinamic as bool) 
Declare sub op_bset( dinamic as bool) 
Declare sub op_btst( dinamic as bool) 
Declare sub op_clr( size as u8 ) 
Declare Sub op_nbcd()
Declare sub op_neg( negx as bool,size as u8 ) 
Declare sub op_not( size as u8 ) 
Declare Sub op_scc()
Declare Sub op_tas()
Declare sub op_tst( size as u8 ) 
Declare sub op_add( size as u8,writeEA as bool) 
Declare sub op_adda( size as u8 ) 
Declare sub op_and( size as u8,writeEA as bool) 
Declare sub op_cmp( size as u8 ) 
Declare sub op_cmpa( size as u8 ) 
Declare sub op_sub( size as u8,writeEA as bool) 
Declare sub op_suba( size as u8 ) 
Declare sub op_or( size as u8,writeEA as bool) 
Declare sub op_eor( size as u8 ) 
Declare Sub op_mulu()
Declare Sub op_muls()
Declare Sub op_divu()
Declare Sub op_divs()
Declare sub op_move( size as u8 ) 
Declare sub op_movea( size as u8 ) 
Declare sub op_addi( size as u8 ) 
Declare sub op_addq( size as u8 ) 
Declare sub op_subq( size as u8 ) 
Declare sub op_andi( size as u8 ) 
Declare sub op_cmpi( size as u8 ) 
Declare sub op_eori( size as u8 ) 
Declare sub op_ori( size as u8 ) 
Declare Sub op_moveq()
Declare sub op_subi( size as u8 ) 
Declare Sub op_bcc()
Declare Sub op_bra()
Declare Sub op_bsr()'todo check stacked reg_pc at address error
Declare Sub op_dbcc()
Declare Sub op_jmp()
Declare Sub op_jsr()
Declare Sub op_lea()
Declare Sub op_pea()
Declare sub op_movem( size as u8,memToReg as bool) 
Declare sub op_addx( size as u8,MemToMem as bool ) 
Declare sub op_cmpm( size as u8 ) 
Declare sub op_subx( size as u8,MemToMem as bool ) 
Declare sub op_abcd( MemToMem as bool ) 
Declare sub op_sbcd( MemToMem as bool ) 
Declare Sub op_chk()
Declare Sub op_eoriccr()
Declare Sub op_andiccr()
Declare Sub op_oriccr()
Declare Sub op_orisr()
Declare Sub op_eorisr()
Declare Sub op_andisr()
Declare Sub op_movefromsr()
Declare Sub op_movetoccr()
Declare Sub op_movetosr()
Declare sub op_exg( opmode as u8 ) 
Declare sub op_ext( LongSize as bool ) 
Declare Sub op_link()
Declare sub op_moveusp( dr as bool) 
Declare Sub op_nop()
Declare Sub op_reset()
Declare Sub op_rte()
Declare Sub op_rtr()
Declare Sub op_rts()
Declare Sub op_stop()
Declare Sub op_swap()
Declare Sub op_trap()
Declare Sub op_trapv()
Declare Sub op_unlk()
Declare sub op_movep( opmode as u8 )


' codigos de operacion OP_CODES, no tocar el orden.....
Enum 'codes
	op_illegal
	op_xsx
	op_rox
	op_bchg
	op_bclr
	op_bset
	op_btst
	op_clr
	op_nbcd
	op_neg
	op_not
	op_scc
	op_tas
	op_tst
	op_add
	op_adda
	op_and
	op_cmp
	op_cmpa
	op_sub
	op_suba
	op_or
	op_eor
	op_mulu
	op_muls
	op_divu
	op_divs
	op_move
	op_movea
	op_addi
	op_addq
	op_subq
	op_andi
	op_cmpi
	op_eori
	op_ori
	op_moveq
	op_subi
	op_bcc
	op_bra
	op_bsr
	op_dbcc
	op_jmp
	op_jsr
	op_lea
	op_pea
	op_movem
	op_addx
	op_cmpm
	op_subx
	op_abcd
	op_sbcd
	op_chk
	op_eoriccr
	op_andiccr
	op_oriccr
	op_orisr
	op_eorisr
	op_andisr
	op_movefromsr
	op_movetoccr
	op_movetosr
	op_exg
	op_ext
	op_link
	op_moveusp
	op_nop
	op_reset
	op_rte
	op_rtr
	op_rts
	op_stop
	op_swap
	op_trap
	op_trapv
	op_unlk
	op_movep
End Enum
