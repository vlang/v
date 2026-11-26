#include "libc_compat.h"
#include "libc_signal_abi.h"
#include <string.h>
/* Native sigaction layout and the kernel's return trampoline are ABI glue. */
struct v_signal_fields { uintptr_t handler,flags,restorer; uint64_t mask; };
void v_signal_action_get(uintptr_t input,uintptr_t output) {
    const struct sigaction *a=(const struct sigaction *)input;
    struct v_signal_fields *f=(struct v_signal_fields *)output;
    f->handler=(uintptr_t)a->sa_handler; f->flags=(unsigned int)a->sa_flags;
    f->restorer=(uintptr_t)a->sa_restorer;
    memcpy(&f->mask,&a->sa_mask,sizeof f->mask);
}
void v_signal_action_set(uintptr_t output,uintptr_t input) {
    const struct v_signal_fields *f=(const struct v_signal_fields *)input;
    struct sigaction *a=(struct sigaction *)output;
    memset(a,0,sizeof *a);
    a->sa_handler=(void (*)(int))f->handler; a->sa_flags=(int)f->flags;
    a->sa_restorer=(void (*)(void))f->restorer;
    memcpy(&a->sa_mask,&f->mask,sizeof f->mask);
}
#if defined(__x86_64__)
/* A naked C function still receives an ordinary compiler-generated FDE. That
 * describes a call frame, whereas this stack contains the kernel's ucontext.
 * Leave this exact kernel trampoline to the native unwinder's signal-frame
 * fallback. The preceding unclaimed byte also keeps a PC-1 lookup outside
 * the previous function's FDE. */
extern void v_signal_return(void);
__asm__(".pushsection .text\n.p2align 4\nnop\n"
        ".hidden v_signal_return\n.global v_signal_return\n.type v_signal_return,@function\n"
        "v_signal_return:\nmov $15,%rax\nsyscall\n"
        ".size v_signal_return,.-v_signal_return\n.popsection\n");
uintptr_t v_signal_restorer(void) { return (uintptr_t)v_signal_return; }
#else
uintptr_t v_signal_restorer(void) { return 0; }
#endif
extern int32_t v_native_sigaction(int32_t,uintptr_t,uintptr_t);
extern uintptr_t v_native_signal(int32_t,uintptr_t);
int sigaction(int number,const struct sigaction *a,struct sigaction *old) { return v_native_sigaction(number,(uintptr_t)a,(uintptr_t)old); }
void (*signal(int number,void (*handler)(int)))(int) { return (void (*)(int))v_native_signal(number,(uintptr_t)handler); }
_Static_assert(sizeof(siginfo_t)==128,"Linux signal-info ABI");
_Static_assert(sizeof(struct timespec)==16,"Linux time64 ABI");
void v_signal_mask_set(uintptr_t output,uint64_t mask) {
    memset((void *)output,0,sizeof(sigset_t));
    memcpy((void *)output,&mask,sizeof mask);
}
void v_signal_queue_info(uintptr_t output,int number,int pid,unsigned int uid,uintptr_t value) {
    siginfo_t *info=(siginfo_t *)output;
    memset(info,0,sizeof *info); info->si_signo=number; info->si_code=SI_QUEUE;
    info->si_pid=pid; info->si_uid=uid; info->si_value.sival_ptr=(void *)value;
}
extern int32_t v_native_sigpending(uintptr_t);
extern int32_t v_native_sigtimedwait(uintptr_t,uintptr_t,uintptr_t);
extern int32_t v_native_sigwait(uintptr_t,uintptr_t);
extern int32_t v_native_sigsuspend(uintptr_t);
extern int32_t v_native_sigqueue(int32_t,int32_t,uintptr_t);
int sigpending(sigset_t *set) { return v_native_sigpending((uintptr_t)set); }
int sigtimedwait(const sigset_t *set,siginfo_t *info,const struct timespec *timeout) {
    return v_native_sigtimedwait((uintptr_t)set,(uintptr_t)info,(uintptr_t)timeout);
}
int sigwaitinfo(const sigset_t *set,siginfo_t *info) { return v_native_sigtimedwait((uintptr_t)set,(uintptr_t)info,0); }
int sigwait(const sigset_t *set,int *number) { return v_native_sigwait((uintptr_t)set,(uintptr_t)number); }
int sigsuspend(const sigset_t *set) { return v_native_sigsuspend((uintptr_t)set); }
int sigqueue(pid_t pid,int number,const union sigval value) { return v_native_sigqueue(pid,number,(uintptr_t)value.sival_ptr); }

static __thread char v_signal_description[96];
uintptr_t v_signal_description_buffer(void) { return (uintptr_t)v_signal_description; }
extern uintptr_t v_native_strsignal(int32_t);
char *strsignal(int number) { return (char *)v_native_strsignal(number); }
int __libc_current_sigrtmin(void) { return 34; }
int __libc_current_sigrtmax(void) { return 64; }

extern int32_t v_native_sigaltstack(uintptr_t, uintptr_t);
extern int32_t v_native_raise(int32_t);
extern int32_t v_native_pthread_kill(uintptr_t, int32_t);
int sigaltstack(const stack_t *stack, stack_t *previous) {
    return v_native_sigaltstack((uintptr_t)stack, (uintptr_t)previous);
}
int raise(int sig) { return v_native_raise(sig); }
int pthread_kill(pthread_t thread, int sig) { return v_native_pthread_kill((uintptr_t)thread, sig); }
