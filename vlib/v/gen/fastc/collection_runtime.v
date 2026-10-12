module fastc

// Ordinary FastC programs do not compile the builtin module. These helpers supply
// the collection operations used by the shared expression lowerings.
fn (g &Parser) collection_runtime_type(typ string) string {
	runtime_type := fastc_runtime_c_type(typ)
	return if g.selfhost { runtime_type } else { fastc_output_c_type(runtime_type) }
}

fn (g &Parser) collection_storage_type(typ string) string {
	return if g.selfhost { typ } else { fastc_output_c_type(typ) }
}

const c_collection_runtime = r'
static void *v_fastc_collection_alloc(size_t size) {
	void *data = calloc(1, size ? size : 1);
	if (data == NULL) { fputs("fastc: collection allocation failed\n", stderr); exit(1); }
	return data;
}
static array builtin____new_array(int len, int cap, int element_size) {
	if (len < 0 || cap < 0 || element_size <= 0) exit(1);
	if (cap < len) cap = len;
	array result = {0};
	result.data = v_fastc_collection_alloc((size_t)cap * (size_t)element_size);
	result.len = len; result.cap = cap; result.element_size = element_size;
	return result;
}
static array builtin__new_array_from_c_array(int len, int cap, int element_size, const void *values) {
	array result = builtin____new_array(len, cap, element_size);
	if (len) memcpy(result.data, values, (size_t)len * (size_t)element_size);
	return result;
}
static int builtin__v_fixed_index(i64 index, i64 len) {
	if (index < 0 || index >= len) {
		fputs("array index out of range\n", stderr); exit(1);
	}
	return (int)index;
}
static void *builtin__array_get(array values, i64 index) {
	return (unsigned char *)values.data + (size_t)builtin__v_fixed_index(index, values.len) * (size_t)values.element_size;
}
static array builtin__array_slice(array values, i64 start, i64 end) {
	if (start < 0 || end < start || end > values.len) {
		fputs("array slice out of range\n", stderr); exit(1);
	}
	values.data = (unsigned char *)values.data + (size_t)start * (size_t)values.element_size;
	values.len = (int)(end - start); values.cap = values.len;
	return values;
}
static u8 builtin__string_at(string value, i64 index) {
	return (u8)value[builtin__v_fixed_index(index, (i64)strlen(value ? value : ""))];
}
static bool v_fastc_string_contains(string value, string candidate) {
	return strstr(value ? value : "", candidate ? candidate : "") != NULL;
}
static u64 builtin__map_hash_string(voidptr key) { (void)key; return 0; }
static bool builtin__map_eq_string(voidptr left, voidptr right) {
	return builtin__string_eq(*(string *)left, *(string *)right);
}
static void builtin__map_clone_string(voidptr destination, voidptr source) {
	string value = *(string *)source;
	size_t len = strlen(value ? value : "");
	char *copy = v_fastc_collection_alloc(len + 1);
	memcpy(copy, value ? value : "", len + 1);
	*(string *)destination = copy;
}
static void builtin__map_free_string(voidptr key) { free(*(char **)key); }
static void builtin__map_free_nop(voidptr key) { (void)key; }
#define V_FASTC_MAP_INTEGER_KEY(size) \
static u64 builtin__map_hash_int_##size(voidptr key) { (void)key; return 0; } \
static bool builtin__map_eq_int_##size(voidptr left, voidptr right) { return memcmp(left, right, size) == 0; } \
static void builtin__map_clone_int_##size(voidptr destination, voidptr source) { memcpy(destination, source, size); }
V_FASTC_MAP_INTEGER_KEY(1)
V_FASTC_MAP_INTEGER_KEY(2)
V_FASTC_MAP_INTEGER_KEY(4)
V_FASTC_MAP_INTEGER_KEY(8)
V_FASTC_MAP_INTEGER_KEY(16)
static map builtin__new_map(int key_bytes, int value_bytes, MapHashFn hash_fn, MapEqFn eq_fn, MapCloneFn clone_fn, MapFreeFn free_fn) {
	map result = {v_fastc_collection_alloc(sizeof(VMapData))};
	result.data->key_bytes = key_bytes; result.data->value_bytes = value_bytes;
	result.data->hash_fn = hash_fn; result.data->key_eq_fn = eq_fn;
	result.data->clone_fn = clone_fn; result.data->free_fn = free_fn;
	return result;
}
static void *builtin__map_get_check(map *values, voidptr key) {
	if (values->data == NULL) return NULL;
	VMapData *data = values->data;
	for (int i = 0; i < data->count; i++) {
		if (data->key_eq_fn(data->key_values.keys + (size_t)i * data->key_bytes, key))
			return data->key_values.values + (size_t)i * data->value_bytes;
	}
	return NULL;
}
static void builtin__map_set(map *values, voidptr key, voidptr value) {
	VMapData *data = values->data;
	void *existing = builtin__map_get_check(values, key);
	if (existing != NULL) { memcpy(existing, value, data->value_bytes); return; }
	if (data->count == data->key_values.cap) {
		int cap = data->count ? data->count * 2 : 8;
		u8 *keys = v_fastc_collection_alloc((size_t)cap * data->key_bytes);
		u8 *items = v_fastc_collection_alloc((size_t)cap * data->value_bytes);
		if (data->count) {
			memcpy(keys, data->key_values.keys, (size_t)data->count * data->key_bytes);
			memcpy(items, data->key_values.values, (size_t)data->count * data->value_bytes);
		}
		free(data->key_values.keys); free(data->key_values.values);
		data->key_values.keys = keys; data->key_values.values = items; data->key_values.cap = cap;
	}
	data->clone_fn(data->key_values.keys + (size_t)data->count * data->key_bytes, key);
	memcpy(data->key_values.values + (size_t)data->count * data->value_bytes, value, data->value_bytes);
	data->count++; data->key_values.len = data->count;
}
'
