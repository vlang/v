// OpaqueHandle stays an incomplete type, like tree-sitter's TSLanguage.
typedef struct OpaqueHandle OpaqueHandle;

static int opaque_handle_storage = 42;

static OpaqueHandle* opaque_handle_get(void) {
	return (OpaqueHandle*)&opaque_handle_storage;
}

static int opaque_handle_value(const OpaqueHandle* h) {
	return *(const int*)h;
}
