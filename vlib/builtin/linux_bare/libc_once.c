/* Cleanup-only native unwind trampoline. A C++ dependency supplies its usual
 * personality and unwinder; weak references preserve C-only cancellation.
 * Only the V callback changes the once control and thread-local cleanup chain. */
#if defined(__x86_64__)
__asm__(".pushsection .text\n"
        ".weak __gxx_personality_v0\n.weak _Unwind_Resume\n"
        ".global v_pthread_once_invoke\n.type v_pthread_once_invoke,@function\n"
        "v_pthread_once_invoke:\n.Lv_once_start:\n.cfi_startproc\n"
        ".cfi_personality 0x9b,DW.ref.__gxx_personality_v0\n.cfi_lsda 0x1b,.Lv_once_lsda\n"
        "endbr64\npush %rbp\n.cfi_def_cfa_offset 16\n.cfi_offset %rbp,-16\n"
        "mov %rsp,%rbp\n.cfi_def_cfa_register %rbp\n"
        "push %r12\n.cfi_offset %r12,-24\npush %r13\n.cfi_offset %r13,-32\n"
        "mov %rsi,%r12\n.Lv_once_call:\ncall *%rdi\n.Lv_once_return:\n"
        ".cfi_remember_state\npop %r13\n.cfi_restore %r13\npop %r12\n.cfi_restore %r12\n"
        "pop %rbp\n.cfi_def_cfa %rsp,8\n.cfi_restore %rbp\nret\n"
        ".Lv_once_cleanup:\n.cfi_restore_state\nendbr64\nmov %rax,%r13\nmov %r12,%rdi\n"
        "call v_pthread_once_abandon\nmov %r13,%rdi\ncall _Unwind_Resume\n.Lv_once_cleanup_end:\n"
        ".cfi_endproc\n.size v_pthread_once_invoke,.-v_pthread_once_invoke\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text\n"
        ".weak __gxx_personality_v0\n.weak _Unwind_Resume\n"
        ".global v_pthread_once_invoke\n.type v_pthread_once_invoke,%function\n"
        "v_pthread_once_invoke:\n.Lv_once_start:\n.cfi_startproc\n"
        ".cfi_personality 0x9b,DW.ref.__gxx_personality_v0\n.cfi_lsda 0x1b,.Lv_once_lsda\n"
        "bti c\nstp x29,x30,[sp,#-32]!\n.cfi_def_cfa_offset 32\n"
        ".cfi_offset x29,-32\n.cfi_offset x30,-24\nmov x29,sp\n.cfi_def_cfa_register x29\n"
        "stp x19,x20,[sp,#16]\n.cfi_offset x19,-16\n.cfi_offset x20,-8\nmov x19,x1\n"
        ".Lv_once_call:\nblr x0\n.Lv_once_return:\n.cfi_remember_state\n"
        "ldp x19,x20,[sp,#16]\n.cfi_restore x19\n.cfi_restore x20\n"
        "ldp x29,x30,[sp],#32\n.cfi_def_cfa sp,0\n.cfi_restore x29\n.cfi_restore x30\nret\n"
        ".Lv_once_cleanup:\n.cfi_restore_state\nbti j\nmov x20,x0\nmov x0,x19\n"
        "bl v_pthread_once_abandon\nmov x0,x20\nbl _Unwind_Resume\n.Lv_once_cleanup_end:\n"
        ".cfi_endproc\n.size v_pthread_once_invoke,.-v_pthread_once_invoke\n.popsection\n");
#endif
/* The call-site table marks only the initializer call as requiring cleanup.
 * Its action zero is a cleanup, with no exception type or catch handler.
 * The resume region has no landing pad, so phase two can leave this frame. */
__asm__(".pushsection .gcc_except_table,\"a\",%progbits\n"
        ".Lv_once_lsda:\n.byte 0xff,0xff,0x01\n"
        ".uleb128 .Lv_once_cs_end-.Lv_once_cs\n.Lv_once_cs:\n"
        ".uleb128 .Lv_once_call-.Lv_once_start\n"
        ".uleb128 .Lv_once_return-.Lv_once_call\n"
        ".uleb128 .Lv_once_cleanup-.Lv_once_start\n.uleb128 0\n"
        ".uleb128 .Lv_once_cleanup-.Lv_once_start\n"
        ".uleb128 .Lv_once_cleanup_end-.Lv_once_cleanup\n.uleb128 0\n.uleb128 0\n"
        ".Lv_once_cs_end:\n.popsection\n"
        ".hidden DW.ref.__gxx_personality_v0\n.weak DW.ref.__gxx_personality_v0\n"
        ".pushsection .data.rel.local.DW.ref.__gxx_personality_v0,\"awG\",%progbits,DW.ref.__gxx_personality_v0,comdat\n"
        ".p2align 3\n.type DW.ref.__gxx_personality_v0,%object\n"
        ".size DW.ref.__gxx_personality_v0,8\nDW.ref.__gxx_personality_v0:\n"
        ".quad __gxx_personality_v0\n.popsection\n");
