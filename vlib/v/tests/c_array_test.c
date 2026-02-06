void* gen_c_array(int size) {
    unsigned char *c_array = malloc(size);
    for(int i = 0; i < size; i++) {
        c_array[i] = i & 0xFF;
    }
    return c_array;
}

void* gen_c_int_array(int size) {
    int *c_array = malloc(size * sizeof(int));
    for(int i = 0; i < size; i++) {
        c_array[i] = i;
    }
    return c_array;
}
