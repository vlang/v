#include "libc_compat.h"
#include <stdlib.h>
#include <string.h>
#if !defined(__GLIBC__)
struct random_data {
    int32_t *fptr, *rptr, *state;
    int rand_type, rand_deg, rand_sep;
    int32_t *end_ptr;
};
#endif
/* BSD additive-feedback state formats: the word preceding state records the
 * generator type and rear index, allowing setstate_r to restore a sequence. */
static const int v_degrees[] = {0, 7, 15, 31, 63}, v_separations[] = {0, 3, 1, 3, 1};
int random_r(struct random_data *s, int32_t *out) {
    if (!s || !out || !s->state || s->rand_type < 0 || s->rand_type > 4) {
        errno = EINVAL;
        return -1;
    }
    if (!s->rand_type) {
        uint32_t value = ((uint32_t)s->state[0] * 1103515245u + 12345u) & 0x7fffffffu;
        s->state[0] = value;
        *out = value;
    } else {
        uint32_t value = (uint32_t)*s->fptr + (uint32_t)*s->rptr;
        *s->fptr = (int32_t)value;
        *out = (int32_t)(value >> 1);
        if (++s->fptr == s->end_ptr)
            s->fptr = s->state;
        if (++s->rptr == s->end_ptr)
            s->rptr = s->state;
    }
    return 0;
}
int srandom_r(unsigned int seed, struct random_data *s) {
    if (!s || !s->state || s->rand_type < 0 || s->rand_type > 4) {
        errno = EINVAL;
        return -1;
    }
    if (!seed)
        seed = 1;
    s->state[0] = (int32_t)seed;
    if (!s->rand_type)
        return 0;
    int32_t word = (int32_t)seed;
    for (int i = 1; i < s->rand_deg; ++i) {
        int64_t next = 16807LL * (word % 127773) - 2836LL * (word / 127773);
        if (next < 0)
            next += 2147483647;
        word = (int32_t)next;
        s->state[i] = word;
    }
    s->fptr = s->state + s->rand_sep;
    s->rptr = s->state;
    s->end_ptr = s->state + s->rand_deg;
    for (int i = 0; i < 10 * s->rand_deg; ++i) {
        int32_t discard;
        random_r(s, &discard);
    }
    return 0;
}
static void v_random_save(struct random_data *s) {
    if (s->state)
        s->state[-1] = s->rand_type ? s->rand_type + 5 * (int)(s->rptr - s->state) : 0;
}
int initstate_r(unsigned int seed, char *buffer, size_t size, struct random_data *s) {
    if (!buffer || !s || size < 8 || (uintptr_t)buffer % _Alignof(int32_t)) {
        errno = EINVAL;
        return -1;
    }
    int type = size >= 256 ? 4 : size >= 128 ? 3 : size >= 64 ? 2 : size >= 32 ? 1 : 0;
    v_random_save(s);
    s->state = (int32_t *)(void *)buffer + 1;
    s->rand_type = type;
    s->rand_deg = v_degrees[type];
    s->rand_sep = v_separations[type];
    if (srandom_r(seed, s) < 0)
        return -1;
    v_random_save(s);
    return 0;
}
int setstate_r(char *buffer, struct random_data *s) {
    if (!buffer || !s || (uintptr_t)buffer % _Alignof(int32_t)) {
        errno = EINVAL;
        return -1;
    }
    int32_t header = *(int32_t *)(void *)buffer;
    int type = header % 5, rear = header / 5;
    if (header < 0 || type < 0 || type > 4 || (type && rear >= v_degrees[type]) ||
        (!type && rear)) {
        errno = EINVAL;
        return -1;
    }
    v_random_save(s);
    s->state = (int32_t *)(void *)buffer + 1;
    s->rand_type = type;
    s->rand_deg = v_degrees[type];
    s->rand_sep = v_separations[type];
    s->end_ptr = s->state + s->rand_deg;
    s->rptr = s->state + rear;
    s->fptr = s->state + (type ? (rear + s->rand_sep) % s->rand_deg : 0);
    return 0;
}
static int v_random_lock, v_random_ready;
static int32_t v_random_default[32];
static struct random_data v_random_global;
static void v_random_prepare(void) {
    if (!v_random_ready) {
        initstate_r(1, (char *)v_random_default, sizeof v_random_default, &v_random_global);
        v_random_ready = 1;
    }
}
long random(void) {
    v_linux_lock(&v_random_lock);
    v_random_prepare();
    int32_t result;
    random_r(&v_random_global, &result);
    v_linux_unlock(&v_random_lock);
    return result;
}
int rand(void) { return (int)random(); }
void srandom(unsigned int seed) {
    v_linux_lock(&v_random_lock);
    v_random_prepare();
    srandom_r(seed, &v_random_global);
    v_linux_unlock(&v_random_lock);
}
void srand(unsigned int seed) { srandom(seed); }
char *initstate(unsigned int seed, char *buffer, size_t size) {
    v_linux_lock(&v_random_lock);
    v_random_prepare();
    char *old = (char *)(v_random_global.state - 1);
    int result = initstate_r(seed, buffer, size, &v_random_global);
    v_linux_unlock(&v_random_lock);
    return result < 0 ? NULL : old;
}
char *setstate(char *buffer) {
    v_linux_lock(&v_random_lock);
    v_random_prepare();
    char *old = (char *)(v_random_global.state - 1);
    int result = setstate_r(buffer, &v_random_global);
    v_linux_unlock(&v_random_lock);
    return result < 0 ? NULL : old;
}
