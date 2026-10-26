@[has_globals]
module main

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

type EncodedAddress = uint

type TaggedAddress = uint | string

@[noinline]
fn encode_local_address(value int) uint {
	local := EncodedAddressValue{value}
	return uint(&local.value)
}

@[noinline]
fn encode_local_address_as_signed(value int) int {
	local := EncodedAddressValue{value}
	return int(&local.value)
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
	return u64(uint(&local.value))
}

@[noinline]
fn encode_local_address_through_sum_type(value int) uint {
	local := EncodedAddressValue{value}
	tagged := TaggedAddress(uint(&local.value))
	return tagged as uint
}

fn unavailable_encoded_address() !uint {
	return error('unavailable')
}

@[noinline]
fn encode_local_address_in_or_fallback(value int) uint {
	local := EncodedAddressValue{value}
	return unavailable_encoded_address() or { uint(&local.value) }
}

@[noinline]
fn encode_parameter_address(value EncodedAddressValue) uint {
	return uint(&value.value)
}

@[noinline]
fn encode_local_pointer_alias(value int) uint {
	local := EncodedAddressValue{value}
	pointer := &local.value
	return uint(pointer)
}

@[noinline]
fn encode_local_address_through_option(value int) uint {
	local := EncodedAddressValue{value}
	encoded := ?uint(uint(&local.value))
	return encoded or { uint(0) }
}

fn encoded_unsigned_result(value &EncodedAddressValue) !uint {
	return uint(&value.value)
}

fn encoded_signed_result(value &EncodedAddressValue) !int {
	return int(&value.value)
}

fn encoded_alias_result(value &EncodedAddressValue) !EncodedAddress {
	return EncodedAddress(&value.value)
}

@[noinline]
fn encode_parameter_through_unsigned_result(value EncodedAddressValue) uint {
	return encoded_unsigned_result(&value) or { uint(0) }
}

@[noinline]
fn encode_parameter_through_signed_result(value EncodedAddressValue) int {
	return encoded_signed_result(&value) or { int(0) }
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
	return f64(uint(&value.value))
}

@[noinline]
fn encode_parameter_through_float_result(value EncodedAddressValue) f64 {
	return encoded_float_result(&value) or { f64(0) }
}

fn encoded_halves_result(value &EncodedAddressValue) !(u32, u32) {
	address := u64(uint(&value.value))
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
		assert *(&int(uint(float_result))) == 989
		assert *(&int(uint(halves))) == 990
	}
}

__global scalar_retained &EncodedAddressValue = unsafe { nil }
__global scalar_retained_bits uint

type ScalarSuccess = bool

@[heap]
struct ScalarResultHolder {
mut:
	value &EncodedAddressValue = unsafe { nil }
}

struct ScalarCarrier {
	value &EncodedAddressValue
}

struct ScalarEncodedCarrier {
	bits uint
}

@[noinline]
fn scalar_retain_global(value &EncodedAddressValue) !bool {
	scalar_retained = value
	return true
}

@[noinline]
fn scalar_retain_heap(mut holder ScalarResultHolder, value &EncodedAddressValue) !i32 {
	holder.value = value
	return 7
}

@[noinline]
fn scalar_retain_alias(value &EncodedAddressValue) !ScalarSuccess {
	scalar_retained = value
	return ScalarSuccess(true)
}

fn scalar_consume_carrier(_value ScalarCarrier) !bool {
	return true
}

@[noinline]
fn scalar_plain_producer(value &EncodedAddressValue) ScalarCarrier {
	scalar_retained = value
	// Deliberately return the borrowed pointer to exercise the caller's provenance tracking.
	return ScalarCarrier{unsafe { value }}
}

fn scalar_consume_wide(_value i64) !bool {
	return true
}

@[noinline]
fn scalar_retain_wide(value &EncodedAddressValue) !i64 {
	scalar_retained = value
	return 0
}

@[noinline]
fn scalar_retain_void(value &EncodedAddressValue) {
	scalar_retained = value
}

@[noinline]
fn scalar_retain_bool(value &EncodedAddressValue) bool {
	scalar_retained = value
	return true
}

fn scalar_pure_reader(value &EncodedAddressValue) !bool {
	return value.value > 0
}

fn scalar_consume(value bool) !bool {
	return !value
}

@[noinline]
fn scalar_retain_carrier(value ScalarCarrier) !bool {
	scalar_retained = value.value
	return true
}

fn scalar_forward_carrier(value ScalarCarrier) !bool {
	return scalar_retain_carrier(value)
}

@[noinline]
fn scalar_retain_encoded(value ScalarEncodedCarrier) !bool {
	scalar_retained_bits = value.bits
	return true
}

@[noinline]
fn scalar_retain_pointee(value &uint) !bool {
	scalar_retained_bits = unsafe { *value }
	return true
}

@[noinline]
fn scalar_global_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	return scalar_retain_global(&local) or { false }
}

@[noinline]
fn scalar_heap_wrapper(mut holder ScalarResultHolder, seed int) i32 {
	local := EncodedAddressValue{seed}
	return scalar_retain_heap(mut holder, &local) or { i32(-1) }
}

@[noinline]
fn scalar_alias_wrapper(seed int) ScalarSuccess {
	local := EncodedAddressValue{seed}
	return scalar_retain_alias(&local) or { ScalarSuccess(false) }
}

@[noinline]
fn scalar_computed_argument_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	return scalar_consume(scalar_retain_global(&local) or { false }) or { false }
}

@[noinline]
fn scalar_wide_computed_argument_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	return scalar_consume_wide(scalar_retain_wide(&local) or { i64(0) }) or { false }
}

@[noinline]
fn scalar_plain_computed_argument_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	return scalar_consume_carrier(scalar_plain_producer(&local)) or { false }
}

@[noinline]
fn scalar_plain_alias_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	carrier := scalar_plain_producer(&local)
	return scalar_consume_carrier(carrier) or { false }
}

@[noinline]
fn scalar_plain_reassigned_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	mut carrier := ScalarCarrier{unsafe { nil }}
	carrier = scalar_plain_producer(&local)
	return scalar_consume_carrier(carrier) or { false }
}

@[noinline]
fn scalar_void_before_reader_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	scalar_retain_void(&local)
	return scalar_pure_reader(&local) or { false }
}

@[noinline]
fn scalar_bool_before_reader_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	stored := scalar_retain_bool(&local)
	return scalar_pure_reader(&local) or { stored }
}

@[noinline]
fn scalar_retain_mut(mut value EncodedAddressValue) !bool {
	scalar_retained = unsafe { &value }
	return true
}

@[noinline]
fn scalar_mut_wrapper(seed int) bool {
	mut local := EncodedAddressValue{seed}
	return scalar_retain_mut(mut local) or { false }
}

@[noinline]
fn scalar_implicit_reference_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	return scalar_retain_global(local) or { false }
}

@[noinline]
fn scalar_closure_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	pointer := &local
	callback := fn [pointer] () {
		scalar_retained = pointer
	}
	callback()
	return scalar_pure_reader(&local) or { false }
}

@[noinline]
fn scalar_closure_alias_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	pointer := &local
	mut callback := fn [pointer] () {
		scalar_retained = pointer
	}
	alias := callback
	alias()
	callback = fn () {}
	callback()
	return scalar_pure_reader(&local) or { false }
}

@[noinline]
fn scalar_carrier_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	carrier := ScalarCarrier{&local}
	return scalar_forward_carrier(carrier) or { false }
}

@[noinline]
fn scalar_encoded_field_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	carrier := ScalarEncodedCarrier{uint(&local.value)}
	return scalar_retain_encoded(carrier) or { false }
}

@[noinline]
fn scalar_encoded_pointee_wrapper(seed int) bool {
	local := EncodedAddressValue{seed}
	bits := uint(&local.value)
	return scalar_retain_pointee(&bits) or { false }
}

fn test_scalar_result_callees_keep_addresses_they_store_alive() {
	assert scalar_global_wrapper(123)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 123

	mut holder := &ScalarResultHolder{}
	assert scalar_heap_wrapper(mut holder, 234) == 7
	assert encoded_address_churn_stack(10) == 137
	assert holder.value.value == 234

	assert scalar_alias_wrapper(345)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 345

	assert !scalar_computed_argument_wrapper(456)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 456

	assert scalar_wide_computed_argument_wrapper(501)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 501

	assert scalar_plain_computed_argument_wrapper(512)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 512

	assert scalar_plain_alias_wrapper(523)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 523

	assert scalar_plain_reassigned_wrapper(534)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 534

	assert scalar_void_before_reader_wrapper(545)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 545

	assert scalar_bool_before_reader_wrapper(556)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 556

	assert scalar_mut_wrapper(557)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 557

	assert scalar_implicit_reference_wrapper(558)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 558

	assert scalar_closure_wrapper(559)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 559

	assert scalar_closure_alias_wrapper(560)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 560

	assert scalar_carrier_wrapper(567)
	assert encoded_address_churn_stack(10) == 137
	assert scalar_retained.value == 567

	assert scalar_encoded_field_wrapper(678)
	assert encoded_address_churn_stack(10) == 137
	unsafe { assert *(&int(scalar_retained_bits)) == 678 }

	assert scalar_encoded_pointee_wrapper(789)
	assert encoded_address_churn_stack(10) == 137
	unsafe { assert *(&int(scalar_retained_bits)) == 789 }
}
