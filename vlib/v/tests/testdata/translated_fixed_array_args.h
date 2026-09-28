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

static inline int translated_alias_ints(int *left, int *right) {
    if (left != right) return 0;
    left[0] = 31;
    if (right[0] != 31) return 0;
    right[1] = 47;
    return 1;
}

static inline int translated_distinct_ints(int *left, int *right) {
    if (left == right) return 0;
    left[0] = 13;
    right[1] = 17;
    return 1;
}

static inline int translated_sum_int_rows(int (*rows)[2]) {
    return rows[0][0] + rows[1][1];
}

static inline void translated_mutate_int_rows(int (*rows)[2]) {
    rows[0][1] = -7;
    rows[1][0] = 42;
}

static inline int translated_alias_int_rows(int (*left)[2], int (*right)[2]) {
    if (left != right) return 0;
    left[0][0] = 31;
    if (right[0][0] != 31) return 0;
    right[1][1] = 47;
    return 1;
}

static inline int translated_deep_int_rows(int (*rows)[2][2]) {
    rows[1][1][0] = -11;
    return rows[0][1][1] + rows[1][0][0];
}

static inline int translated_overlapping_int_rows(int (*rows)[2], int *first, int *last) {
    if (first != rows[0] || last != rows[2]) return 0;
    rows[0][0] = 31;
    if (first[0] != 31) return 0;
    last[1] = 47;
    if (rows[2][1] != 47) return 0;
    rows[1][0] = -7;
    return 1;
}

static inline int translated_overlapping_int_rows_reversed(int *first, int *last, int (*rows)[2]) {
    return translated_overlapping_int_rows(rows, first, last);
}

static inline int translated_partial_int_views(int *left, int *right) {
    if (right != left + 2) return 0;
    left[2] = 31;
    if (right[0] != 31) return 0;
    right[2] = 47;
    return 1;
}

static inline int translated_partial_int_views_reversed(int *right, int *left) {
    return translated_partial_int_views(left, right);
}

#endif
