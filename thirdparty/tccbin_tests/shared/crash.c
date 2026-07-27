/* A genuine fault. Confirms the compiler's own crash-reporting/signal
   handling (if any) still catches real faults and doesn't silently
   swallow them - the mirror image of hello.c/gc_alloc.c, which check
   that nothing THAT SHOULDN'T crash does. */
int main(void) {
    int *p = (int *)0;
    return *p;
}
