.386 ; 'Instruction or register not accepted in current CPU mode' error so it was needed to resolve this
.model flat, stdcall

.code

;D:\D2\uasm255_x64\jwasm.exe /c -coff -zzs -Zp8 /nologo /Zi /zt0 /Fo"cpuinfo_i386.obj" /Fl"" /W2 cpuinfo_i386.asm
; cpuidex_asm(result [4]u32, op u32, op2 u32)
_cpuidex_asm PROC
	push edi
	mov edi, [esp+8]	; result
	mov eax, [esp+12]	; op
	mov ecx, [esp+16]	; op2
	cpuid
	mov [edi + 00], eax
	mov [edi + 04], ebx
	mov [edi + 08], ecx
	mov [edi + 12], edx
	pop edi
	ret
_cpuidex_asm ENDP

; xgetbv_asm(result [2]u32, index u32)
_xgetbv_asm PROC
	push edi
	mov edi, [esp+8]	; result
	mov ecx, [esp+12]	; index
	byte 0x0f, 0x01, 0xd0 ;xgetbv
	mov [edi + 00], eax
	mov [edi + 04], edx
	pop edi
	ret
_xgetbv_asm ENDP

; rdtscp_asm(result [4]u32)
_rdtscp_asm PROC
	push edi
	mov edi, [esp+8]	; result
	byte 0x0F, 0x01, 0xF9 ;rdtscp
	mov [edi + 00], eax
	mov [edi + 04], ebx
	mov [edi + 08], ecx
	mov [edi + 12], edx
	pop edi
	ret
_rdtscp_asm ENDP

END