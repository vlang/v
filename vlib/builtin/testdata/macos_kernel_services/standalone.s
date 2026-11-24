.section __TEXT,__text,regular,pure_instructions
.globl _start
.p2align 4
_start:
    andq $-16, %rsp
    callq _probe_main
    movl %eax, %edi
    movq $0x2000001, %rax
    syscall
    ud2
