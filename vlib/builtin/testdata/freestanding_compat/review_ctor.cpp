#include <unistd.h>
extern "C" int __cxa_atexit(void (*)(void *), void *, void *);
extern "C" void __cxa_finalize(void *);
static int first, second, first_identity, second_identity;
static void count(void *value) { ++*static_cast<int *>(value); }
struct Goodbye {
    ~Goodbye() { (void)write(1, "destructor ran\n", 15); }
};
static Goodbye goodbye;
extern "C" int v_review_ctor_probe() {
    if (__cxa_atexit(count,&first,&first_identity) ||
        __cxa_atexit(count,&second,&second_identity)) return 1;
    __cxa_finalize(&first_identity);
    if (first!=1 || second) return 2;
    __cxa_finalize(&first_identity);
    if (first!=1 || second) return 3;
    __cxa_finalize(&second_identity);
    return second==1 ? 0 : 4;
}
#ifdef HOSTED_REVIEW
int main() { return v_review_ctor_probe(); }
#endif
