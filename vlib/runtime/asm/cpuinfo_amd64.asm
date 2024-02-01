.x64 ; 'Instruction or register not accepted in current CPU mode' error so it was needed to resolve this
;.model flat, fastcall ; /win64 auto enable flat,fastcall

.code

;D:\D2\uasm255_x64\jwasm.exe /c -Zp8 /nologo /Zi /zt0 /Fo"cpuinfo_amd64.obj" /Fl"" /W2 /win64  cpuinfo_amd64.asm
; cpuidex_asm(result [4]u32, op u32, op2 u32)
cpuidex_asm PROC
	push rdi
	mov rdi, rcx	; result
	mov eax, edx	; op
	mov ecx, r8d	; op2
	cpuid
	mov [rdi + 00], eax
	mov [rdi + 04], ebx
	mov [rdi + 08], ecx
	mov [rdi + 12], edx
	pop rdi
	ret
cpuidex_asm ENDP

; xgetbv_asm(result [2]u32, index u32)
xgetbv_asm PROC
	push rdi
	mov rdi, rcx	; result
	mov rcx, rdx	; index
	xgetbv
	mov [rdi + 00], eax
	mov [rdi + 04], edx
	pop rdi
	ret
xgetbv_asm ENDP

; rdtscp_asm(result [4]u32)
rdtscp_asm PROC
	push rdi
	mov rdi, rcx	; result
	rdtscp
	mov [rdi + 00], eax
	mov [rdi + 04], ebx
	mov [rdi + 08], ecx
	mov [rdi + 12], edx
	pop rdi
	ret
rdtscp_asm ENDP

END