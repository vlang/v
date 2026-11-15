typedef int OrderedBuffer[2];

static void ordered_buffer_set(OrderedBuffer *buffer, int value) {
    (*buffer)[0] = value;
}

static int ordered_buffer_get(OrderedBuffer *buffer) {
    return (*buffer)[0];
}
