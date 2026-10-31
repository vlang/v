static int fixed_array_pointer_sum(int values[2]) {
    return values ? values[0] + values[1] : 0;
}

static int fixed_array_pointer_rows(int *values[2]) {
    return *values[0] + *values[1];
}

static int fixed_array_pointer_callbacks(int (*values[2])(int)) {
    return values[0](10) + values[1](20);
}
