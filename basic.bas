
Dim Shared DEB As Integer=2 ' 1 muestra texto y para en cada INS
Dim Shared ciclosejecutados As Integer=0 ' para impedir que el hard se ejecute en cada cuadro

#Include "m68000.bas"

' #######################################################################
' PRINCIPAL

Dim zona As Integer=0
Dim Shared sc As String

Dim Shared As u8 col,fil
Dim Shared old_regpc As u32
Dim Shared As u8 car
Dim Shared f As Integer=0
Dim Shared rs232 As u8=0
Dim Shared skey As String
Dim Shared inirom As Integer ' inicio de la ROM a ejecutar
Dim Shared load As Integer=0


    

Sub leerom1(nombre As String, inirom As u32)
	Dim a As Integer=1
	Dim b As Integer=0
	Dim sc As String=" "
	
	Open nombre For Binary Access Read As 1
	While Not Eof(1)
		Get #1,a,sc
		RAM(b+inirom)=Asc(sc)
		a+=1
		b+=1 ' vamos de uno en uno
	Wend
	Close 1
	
End Sub
    

Sub leerom2(nombre As String, inirom As u32)
	Dim a As Integer=1
	Dim b As Integer=0
	Dim sc As String=" "
	
	Open nombre For Binary Access Read As 1
	While Not Eof(1)
		Get #1,a,sc
		RAM(b+inirom)=Asc(sc)
		a+=1
		b+=2 ' vamos de dos en dos
	Wend
	Close 1
End Sub


inirom=&h900 ' &hFD0000 --> para el S100

'inirom=0 ' t68k zbug
'inirom=&h400000 ' calculadora ti92


'Open "68k-mon.bin" For Binary Access Read As 1
'Open "zbug\t68k.bin" For Binary Access Read As 1
sc=" "
Open "TBI68K.bin" For Binary Access Read As 1
While Not Eof(1)
	Get #1,zona+1,sc
	RAM(zona)=Asc(sc)
	zona+=1
Wend
Close 1

Initialize(inirom)

	fullprefetchFirstStep

skey=""

Open "con:" For Output As 3
col=1
fil=25
	
reg_pc=&h922 ' para el basic tiny
	
While 1
	'no2:	 
	'old_regpc=reg_pc
	
	If skey="" Then skey=InKey
	'If skey=Chr(13) Then end

   cls
	opcode=readWord(reg_pc,0)
	prefetch(CIERTO)
	incrementpc()
	exec_ins()
	'group0exception(0)
	
	If deb Then debug()
	
	'If reg_pc=&h21b8 And rs232<>3 Then 
   '    rs232=2
	'EndIf
	
		Locate 5,55
		If deb Then Print "Pulsa una tecla para ejecutar (ESC salir)";
	   If tecla=0 Then
	      'sC=""
	      'While skey="": skey=InKey: Wend
	      If skey=Chr(27) Then End
	      'If skey=" " Then tecla=1 ' ejecucion continua
	   Else
	   	'If skey=" " Then tecla=0:deb=1
	   	If skey=Chr(27) Then Close 1,2,3:end
	   End If   
	   Locate 14,1
	
	deb=0
	'If deb Then 
   	If reg_pc=&h4000f8 Then ' 25a2
   	 'reg_pc=&h400266
       'Print "*** ";Hex(old_regpc,8);" ***";:sleep
       'tecla=0
   	End if	
   	If reg_pc=&h4000f8 Then ' 25a2
   	 'reg_pc=&h400126 ' trampa para que avance el emul del VMAC
       'Print "*** ";Hex(old_regpc,8);" ***";:sleep
       'tecla=0
   	End if	
	'End If 
	
		If skey=chr(255) + "k" Then End
		
	'group1exceptions()
Wend


Sub hardware(valor As u32)

   ' para evitar que la rutina hard se muera de lentitud
   'ciclosejecutados+=1
   'If ciclosejecutados < 2 Then Exit Sub
   'ciclosejecutados=0

   
   If skey="!" And load=0  Then 
   	load=1
   	Close 5
   	Open "Tbi68k12\ddjgames.Tbi" For Binary As 5
   	skey=""
   	'ram(&h10041)=2:Exit sub
   EndIf



f+=1
    If load=0 And valor=&H10041 Then ram(valor)=2:Exit Sub
    If load And valor=&H10041 Then ram(valor)=1:Exit sub
    If load And valor=&h10043 Then 
   	  If Eof(5) Then load=0:Exit Sub
   	   	Get #5,load,sc
   	   	f=0
   	   	Open "con:" For Output As 2
   	   	  Print #2,sc;
   	   	Close 2
   	   	load+=1

   	   	'Exit Sub
   	   	If sc=Chr(10) Then 
   	   		Get #5,load,sc
   	   		load+=1
   	   	EndIf
   	   	ram(&h10041)=1
   	   	ram(&h10043)=asc(sc)
   	   	Exit Sub
    EndIf

   If reg_pc=&h1419 Then reg_d(0)=ram(&h10042):ram(&h10042)=0:ram(&h10040)=2:Exit Sub
    
   If skey=Chr(13) Then skey=Chr(13):ram(&h10040)=1:ram(&h10042)=Asc(skey):skey="esperar":Exit Sub  	
	If skey="esperar" Then ram(&h10040)=1:skey="":Exit sub		
   ' si hay un dato esperando, lo imprimimos en pantalla
   If ram(&h10042)<>0 Then ram(&h10040)=2 Else ram(&h10040)=1

	If reg_pc=&h1392 Then '&h976 Then 'mospulsao enter
	'tecla=0:deb=1
      'tecla=0' entramos en debug de nuevo

	End if	



	If reg_pc=&h1156 Then 
      'tecla=0' entramos en debug de nuevo
      ram(&h10040)=2
	End if	
	 
	' si es modo escritura en terminal y tenemos un caracter esperando, cogemos y enviamos a pantalla 
   If reg_pc=&h13fe Then ' And ram(&h10040)=1 Then 'And modoterminal=2 Then n
   	Open "con:" For Output As 2
   	'Dim anterior As Integer = CsrLin
   	Dim As u8 car=ram(&h10042)
   	If car=0 Then ram(&h10040)=2:close 2:Exit sub
   	'Locate fil,col
   	'If car=13 Then fil+=1:col=1
   	Print #2,Chr(car);
   	ram(&h10042)=0 ' una vez enviado a pantall, lo borramos
   	'col+=1
   	'If col=80 Then fil+=1:col=1
   	'If fil>50 Then 
   	'	For fil=25 To 49
   	'		Locate fil,1
   	'		Print Space(80)
   	'	Next
   	'	fil=25:col=1
   	'EndIf
   	'Locate anterior,1
   	ram(&h10040)=2 ' damos permiso de escritura a la cpu
   	'Exit sub
   	Close 2
   	Exit Sub
   EndIf


   ' si es modo lectura y no hay nada para enviar, aprovechamos a mirar si se pulsa una tecla
   If ram(&h10040)=1 Then If ram(&h10042) then ram(&h10040)=2:Exit sub

   
   
   	If skey<>"" And skey<>"esperar" Then ' si pulsamos tecla, ponemos estado lectura
   		ram(&h10040)=1
   		If skey=Chr(13) Then 
   		  'skey=Chr(13)
   		  ram(&h10042)=Asc(skey)
   		Else
   		  ram(&h10042)=Asc(skey)
           skey=""' borramos teclado
   		End If
   	End If
  'EndIf
End Sub


''''''''' rutinas a escribir por el usuario para cada emulacion '''''''''''
Function memWordRead(addr As u32) As u16
    Dim As u16 res = memByteRead(addr) Shl 8
    res or= memByteRead(addr + 1)
    return res
End Function

Sub memWordWrite(addr As u32, valor As u16)
    memByteWrite(addr, valor shr 8)
    memByteWrite(addr + 1, valor and &hff)
End Sub


Function memByteRead(addr As u32) As u8

  	Return RAM(addr)
End Function

Sub memByteWrite(addr As u32, valor As u8)
	'If addr > (&h3fa700-1) And addr<(&h3fa700-1+21888) Then addr=addr-&h3e0000
	If addr > &h900 And addr<&h1300 Then Exit Sub ' estamos en ROM
	'If addr > &h1ffff  And addr<&h3fa700 Then Exit Sub ' estamos fuera de zona RAM disponible
	
	If addr>&hD00000 And addr<&hEFFFFF Then 
   	'Print #3,Hex(addr,8),ram(addr)+1
	EndIf

	RAM(addr)=valor
End Sub
