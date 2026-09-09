#ifndef GOROUTINES_CONTEXT_NIX_H
#define GOROUTINES_CONTEXT_NIX_H

#include <ucontext.h>
#include <stdint.h>
#include <stdlib.h>

// Extended context: ucontext_t + entry function info for first-time starts.
typedef struct {
    ucontext_t uctx;
    void (*entry_fn)(void *);
    void *entry_arg;
} goroutine_ctx_t;

// Thread-local pointer to the target goroutine context.
// Set just before swapcontext/setcontext so the trampoline can find its fn/arg.
#if defined(_WIN32) || defined(_WIN64)
static __declspec(thread) goroutine_ctx_t *_goroutine_start_ctx = NULL;
#else
static _Thread_local goroutine_ctx_t *_goroutine_start_ctx = NULL;
#endif

static void *goroutines_context_alloc(void) {
    return calloc(1, sizeof(goroutine_ctx_t));
}

// Zero-argument trampoline. Reads fn/arg from the TLS-stored context.
// This avoids passing arguments through makecontext, which is broken on macOS ARM64.
static void goroutines__context_trampoline(void) {
    goroutine_ctx_t *gctx = _goroutine_start_ctx;
    _goroutine_start_ctx = NULL;
    void (*fn)(void *) = gctx->entry_fn;
    void *arg = gctx->entry_arg;
    // Clear so future context switches to this goroutine don't re-trigger
    gctx->entry_fn = NULL;
    gctx->entry_arg = NULL;
    fn(arg);
}

static void goroutines_context_init(void *ctx_buf, void *stack, int stack_size, void *entry_fn, void *arg) {
    goroutine_ctx_t *gctx = (goroutine_ctx_t *)ctx_buf;
    gctx->entry_fn = (void (*)(void *))entry_fn;
    gctx->entry_arg = arg;
    getcontext(&gctx->uctx);
    gctx->uctx.uc_stack.ss_sp = stack;
    gctx->uctx.uc_stack.ss_size = (size_t)stack_size;
    gctx->uctx.uc_link = NULL;
    makecontext(&gctx->uctx, (void (*)())goroutines__context_trampoline, 0);
}

static void goroutines_context_switch(void *from_buf, void *to_buf) {
    goroutine_ctx_t *from = (goroutine_ctx_t *)from_buf;
    goroutine_ctx_t *to = (goroutine_ctx_t *)to_buf;
    // If switching to a goroutine that hasn't started yet, set TLS for trampoline
    if (to->entry_fn != NULL) {
        _goroutine_start_ctx = to;
    }
    swapcontext(&from->uctx, &to->uctx);
}

static void goroutines_context_set(void *to_buf) {
    goroutine_ctx_t *to = (goroutine_ctx_t *)to_buf;
    if (to->entry_fn != NULL) {
        _goroutine_start_ctx = to;
    }
    setcontext(&to->uctx);
}

#endif
