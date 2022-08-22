void* gen_c_array(int size) {
    unsigned char *c_array = malloc(size);
    for(int i = 0; i < size; i++) {
        c_array[i] = i & 0xFF;
    }
    return c_array;
}

