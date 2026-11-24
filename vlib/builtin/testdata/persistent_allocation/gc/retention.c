#define GC_THREADS 1
#include "retention.h"
#include <gc/gc.h>
#include <pthread.h>
#include <stdint.h>
struct parent { void *child; };
static int finalized;
static void child_finalizer(void *object, void *argument) {
 (void)object; (void)argument; finalized++;
}
static void *create_child(void *address) {
 struct GC_stack_base stack;
 if (GC_get_stack_base(&stack) != GC_SUCCESS) return (void *)(uintptr_t)1;
 int registered = GC_register_my_thread(&stack);
 if (registered != GC_SUCCESS && registered != GC_DUPLICATE) return (void *)(uintptr_t)2;
 struct parent *parent = address;
 unsigned char *child = GC_MALLOC(64);
 child[0] = 173;
 GC_REGISTER_FINALIZER_NO_ORDER(child, child_finalizer, 0, 0, 0);
 parent->child = child;
 if (registered == GC_SUCCESS) GC_unregister_my_thread();
 return 0;
}
int persistent_payload_create(void *address) {
 pthread_t worker;
 void *result = (void *)(uintptr_t)3;
 GC_set_finalize_on_demand(1);
 GC_allow_register_threads();
 if (pthread_create(&worker, 0, create_child, address) != 0) return 3;
 if (pthread_join(worker, &result) != 0) return 4;
 return (int)(uintptr_t)result;
}
__attribute__((noinline)) static void scrub_and_collect(void) {
 volatile uintptr_t erased[8192];
 for (unsigned i = 0; i < sizeof erased / sizeof erased[0]; i++) erased[i] = 0;
 GC_gcollect();
 GC_invoke_finalizers();
}
int persistent_payload_collect(void *address, int clear) {
 struct parent *parent = address;
 if (clear) parent->child = 0;
 for (int attempt = 0; attempt < 20; attempt++) scrub_and_collect();
 /* The collector thread never reads the child address: its only strong root
  * is the scanned parent. The creating worker has exited and unregistered. */
 return clear ? (finalized == 1 ? 0 : 1) : (finalized == 0 ? 0 : 2);
}
