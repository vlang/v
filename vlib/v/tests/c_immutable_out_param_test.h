static int immutable_out_param_write_one(int *value) {
    *value = 1;
    return 0;
}

static int immutable_out_param_write_i32(int *value) {
    *value = 1;
    return 0;
}
