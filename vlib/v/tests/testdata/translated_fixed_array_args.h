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

#endif
