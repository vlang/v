void* gen_c_array(int size) {
    unsigned char *c_array = calloc(sizeof(*c_array) * size, size);
    for(int i = 0; i < size; i++) {
        c_array[i] = i;
    }
    return c_array;
}

