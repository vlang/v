.intel_syntax noprefix
.text
    .globl _start, syscall5

    _start:
        xor rbp,rbp
        pop rdi
        mov rsi,rsp
        and rsp,-16
        call main__main

        mov rdi,rax /* syscall param 1 = rax (ret value of main) */
        mov rax,60 /* SYS_exit */
        syscall

        ret /* should never be reached, but if the OS somehow fails
               to kill us, it will cause a segmentation fault */

    syscall5:
        mov rax,rdi
        mov rdi,rsi
        mov rsi,rdx
        mov rdx,rcx
        mov r10,r8
        mov r8,r9
        syscall
        ret

