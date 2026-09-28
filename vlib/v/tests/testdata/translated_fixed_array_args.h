#ifndef V_TRANSLATED_FIXED_ARRAY_ARGS_H
#define V_TRANSLATED_FIXED_ARRAY_ARGS_H

static inline unsigned char translated_first_byte(unsigned char *values) {
    return values[0];
}

static inline char translated_first_char(char *values) {
    return values[0];
}

static inline unsigned char translated_first_byte_row(unsigned char (*values)[2]) {
    return values[0][0];
}

static inline int translated_sum_ints(int *values) {
    return values[0] + values[1];
}

static inline void translated_mutate_ints(int *values) {
    values[0] = -7;
    values[1] = 42;
}

static inline void translated_increment_int(int *value) {
    ++*value;
}

#endif
