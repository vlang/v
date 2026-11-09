#ifndef V_RESULT_IDENTITY_OPAQUE_HANDLE_H
#define V_RESULT_IDENTITY_OPAQUE_HANDLE_H
typedef struct {
    int value;
} multi_return_handle_t;

static multi_return_handle_t result_identity_handle = {7};

static multi_return_handle_t *result_identity_get_handle(void) {
    return &result_identity_handle;
}

static int result_identity_handle_value(multi_return_handle_t *handle) {
    return handle->value;
}
#endif
