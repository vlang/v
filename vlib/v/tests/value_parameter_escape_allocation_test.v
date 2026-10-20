struct Req {
	buf []u8
	pos int
	len int
}

// a) `&r.buf[i]` points into the heap storage of the `[]u8`, not into `r`.
fn view(r Req) string {
	return unsafe { tos(&r.buf[r.pos], r.len) }
}

@[noinline]
fn view_local(buf []u8) string {
	r := Req{
		buf: buf
		pos: 4
		len: 11
	}
	return unsafe { tos(&r.buf[r.pos], r.len) }
}

interface Repo {
	count() !int
}

struct MemRepo {}

fn (m MemRepo) count() !int {
	return 7
}

struct UseCase {
	repo Repo
}

// b) a method called through an interface field of a by-value receiver.
fn (u UseCase) count() !int {
	return u.repo.count()
}

struct Frames {
	data []u8
}

struct Row {
	payload []u8
}

struct Iter {
	data []u8
mut:
	i int
}

fn (f &Frames) iter() Iter {
	return Iter{
		data: f.data
	}
}

fn (mut it Iter) next() ?Row {
	if it.i >= it.data.len {
		return none
	}
	it.i++
	return Row{
		payload: it.data
	}
}

fn (r Row) int4(i int) !i32 {
	if i >= r.payload.len {
		return error('no column ${i}')
	}
	return i32(r.payload[i])
}

// c) a `&T` method on a by-value parameter; only an int is returned.
fn first(f Frames) int {
	mut it := f.iter()
	row := it.next() or { panic('expected a row') }
	return int(row.int4(0) or { panic(err) })
}

const rounds = 1_000

fn test_dynamic_backing_does_not_move_its_local_header_to_the_heap() {
	buf := 'GET /index.html HTTP/1.1'.bytes()
	assert view_local(buf) == '/index.html'
	before := gc_heap_usage().total_bytes
	mut length := 0
	for _ in 0 .. rounds {
		length += view_local(buf).len
	}
	allocated := gc_heap_usage().total_bytes - before
	assert allocated == 0
	assert length == rounds * 11
}

fn test_value_parameters_with_separate_backing_do_not_allocate() {
	r := Req{
		buf: 'GET /index.html HTTP/1.1'.bytes()
		pos: 4
		len: 11
	}
	uc := UseCase{
		repo: MemRepo{}
	}
	f := Frames{
		data: [u8(42), 43]
	}
	mut sum := view(r).len + (uc.count() or { 0 }) + first(f) // warm-up

	mut before := gc_heap_usage().total_bytes
	for _ in 0 .. rounds {
		sum += view(r).len
	}
	a := gc_heap_usage().total_bytes - before

	before = gc_heap_usage().total_bytes
	for _ in 0 .. rounds {
		sum += uc.count() or { 0 }
	}
	b := gc_heap_usage().total_bytes - before

	before = gc_heap_usage().total_bytes
	for _ in 0 .. rounds {
		sum += first(f)
	}
	c := gc_heap_usage().total_bytes - before

	assert a == 0
	assert b == 0
	// On 32-bit targets an i32 Result can hold a pointer, so its arguments stay conservative.
	if sizeof(voidptr) > sizeof(i32) {
		assert c == 0
	}
	assert sum == (rounds + 1) * (11 + 7 + 42)
}

struct EncodedAddressValue {
	value int
}

type EncodedAddress = usize

type TaggedAddress = usize | string

@[noinline]
fn encode_local_address(value int) usize {
	local := EncodedAddressValue{value}
	return usize(&local.value)
}

@[noinline]
fn encode_local_address_as_signed(value int) isize {
	local := EncodedAddressValue{value}
	return isize(&local.value)
}

@[noinline]
fn encode_local_address_through_alias(value int) u64 {
	local := EncodedAddressValue{value}
	address := EncodedAddress(&local.value)
	return u64(address)
}

@[noinline]
fn encode_local_address_through_nested_casts(value int) u64 {
	local := EncodedAddressValue{value}
	return u64(usize(&local.value))
}

@[noinline]
fn encode_local_address_through_sum_type(value int) usize {
	local := EncodedAddressValue{value}
	tagged := TaggedAddress(usize(&local.value))
	return tagged as usize
}

fn unavailable_encoded_address() !usize {
	return error('unavailable')
}

@[noinline]
fn encode_local_address_in_or_fallback(value int) usize {
	local := EncodedAddressValue{value}
	return unavailable_encoded_address() or { usize(&local.value) }
}

@[noinline]
fn encode_parameter_address(value EncodedAddressValue) usize {
	return usize(&value.value)
}

@[noinline]
fn encode_local_pointer_alias(value int) usize {
	local := EncodedAddressValue{value}
	pointer := &local.value
	return usize(pointer)
}

@[noinline]
fn encode_local_address_through_option(value int) usize {
	local := EncodedAddressValue{value}
	encoded := ?usize(usize(&local.value))
	return encoded or { usize(0) }
}

fn encoded_unsigned_result(value &EncodedAddressValue) !usize {
	return usize(&value.value)
}

fn encoded_signed_result(value &EncodedAddressValue) !isize {
	return isize(&value.value)
}

fn encoded_alias_result(value &EncodedAddressValue) !EncodedAddress {
	return EncodedAddress(&value.value)
}

@[noinline]
fn encode_parameter_through_unsigned_result(value EncodedAddressValue) usize {
	return encoded_unsigned_result(&value) or { usize(0) }
}

@[noinline]
fn encode_parameter_through_signed_result(value EncodedAddressValue) isize {
	return encoded_signed_result(&value) or { isize(0) }
}

@[noinline]
fn encode_parameter_through_alias_result(value EncodedAddressValue) EncodedAddress {
	return encoded_alias_result(&value) or { EncodedAddress(0) }
}

fn unavailable_integer_address() !int {
	return error('unavailable')
}

@[noinline]
fn encode_local_address_in_integer_fallback(value int) int {
	local := EncodedAddressValue{value}
	return unavailable_integer_address() or { int(&local.value) }
}

fn encoded_u64_result(value &EncodedAddressValue) !u64 {
	return u64(&value.value)
}

fn encoded_i64_result(value &EncodedAddressValue) !i64 {
	return i64(&value.value)
}

fn encoded_int_result(value &EncodedAddressValue) !int {
	return int(&value.value)
}

@[noinline]
fn encode_parameter_through_u64_result(value EncodedAddressValue) u64 {
	return encoded_u64_result(&value) or { u64(0) }
}

@[noinline]
fn encode_parameter_through_i64_result(value EncodedAddressValue) i64 {
	return encoded_i64_result(&value) or { i64(0) }
}

@[noinline]
fn encode_parameter_through_int_result(value EncodedAddressValue) int {
	return encoded_int_result(&value) or { 0 }
}

fn encoded_float_result(value &EncodedAddressValue) !f64 {
	return f64(usize(&value.value))
}

@[noinline]
fn encode_parameter_through_float_result(value EncodedAddressValue) f64 {
	return encoded_float_result(&value) or { f64(0) }
}

fn encoded_halves_result(value &EncodedAddressValue) !(u32, u32) {
	address := u64(usize(&value.value))
	return u32(address), u32(address >> 32)
}

@[noinline]
fn encode_parameter_through_halves_result(value EncodedAddressValue) (u32, u32) {
	return encoded_halves_result(&value) or { return 0, 0 }
}

@[noinline]
fn encoded_address_churn_stack(value int) int {
	mut values := [128]int{}
	for i in 0 .. values.len {
		values[i] = value + i
	}
	return values[127]
}

fn test_integer_casts_keep_the_addressed_local_alive() {
	unsigned := encode_local_address(123)
	signed := encode_local_address_as_signed(234)
	alias := encode_local_address_through_alias(345)
	nested := encode_local_address_through_nested_casts(456)
	tagged := encode_local_address_through_sum_type(567)
	fallback := encode_local_address_in_or_fallback(678)
	parameter := encode_parameter_address(EncodedAddressValue{789})
	pointer_alias := encode_local_pointer_alias(890)
	optional := encode_local_address_through_option(901)
	unsigned_result := encode_parameter_through_unsigned_result(EncodedAddressValue{912})
	signed_result := encode_parameter_through_signed_result(EncodedAddressValue{923})
	alias_result := encode_parameter_through_alias_result(EncodedAddressValue{934})
	integer_fallback := encode_local_address_in_integer_fallback(945)
	u64_result := encode_parameter_through_u64_result(EncodedAddressValue{956})
	i64_result := encode_parameter_through_i64_result(EncodedAddressValue{967})
	int_result := encode_parameter_through_int_result(EncodedAddressValue{978})
	float_result := encode_parameter_through_float_result(EncodedAddressValue{989})
	low, high := encode_parameter_through_halves_result(EncodedAddressValue{990})
	halves := u64(low) | (u64(high) << 32)
	assert encoded_address_churn_stack(10) == 137
	// The integer values encode pointers to storage that outlives the encoding call.
	unsafe {
		assert *(&int(unsigned)) == 123
		assert *(&int(signed)) == 234
		assert *(&int(alias)) == 345
		assert *(&int(nested)) == 456
		assert *(&int(tagged)) == 567
		assert *(&int(fallback)) == 678
		assert *(&int(parameter)) == 789
		assert *(&int(pointer_alias)) == 890
		assert *(&int(optional)) == 901
		assert *(&int(unsigned_result)) == 912
		assert *(&int(signed_result)) == 923
		assert *(&int(alias_result)) == 934
		assert *(&int(integer_fallback)) == 945
		assert *(&int(u64_result)) == 956
		assert *(&int(i64_result)) == 967
		assert *(&int(int_result)) == 978
		assert *(&int(usize(float_result))) == 989
		assert *(&int(usize(halves))) == 990
	}
}
