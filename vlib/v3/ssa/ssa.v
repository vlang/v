module ssa

import v3.token

// ValueID aliases value id values used by ssa.
pub type ValueID = int

// TypeID aliases type id values used by ssa.
pub type TypeID = int

// BlockID aliases block id values used by ssa.
pub type BlockID = int

// OpCode lists op code values used by ssa.
pub enum OpCode {
	// Terminators
	ret
	br
	jmp
	switch_ // Multi-way branch: switch_ %val, default_block, [case_val, block]...
	unreachable
	// Binary (integer)
	add
	sub
	mul
	sdiv
	srem
	udiv
	urem
	// Binary (float)
	fadd
	fsub
	fmul
	fdiv
	frem
	// Bitwise
	shl
	ashr
	lshr
	and_
	or_
	xor
	// Memory
	alloca
	load
	store
	get_element_ptr
	heap_alloc // Heap allocate memory for a type (malloc+zero): returns ptr
	fence      // Memory ordering barrier
	cmpxchg    // Atomic compare-exchange
	atomicrmw  // Atomic read-modify-write
	// Comparisons
	lt
	gt
	le
	ge
	ult
	ugt
	ule
	uge
	eq
	ne
	// Other
	call
	call_indirect // Indirect call through function pointer
	call_sret     // Call with struct return (x8 indirect return on ARM64)
	neg
	trunc
	sext
	zext
	fptoui
	fptosi
	uitofp
	sitofp
	bitcast
	phi
	select
	assign             // Copy op used during phi elimination
	inline_string_init // Build a string struct by value: (string){str, len}
	// Concurrency
	go_call    // Launch goroutine: go_call fn_ref, args...
	spawn_call // Launch OS thread: spawn_call fn_ref, args...
	// Aggregate (struct/tuple) operations
	extractvalue
	insertvalue
	struct_init
}

// AtomicOrdering lists atomic ordering values used by ssa.
pub enum AtomicOrdering {
	not_atomic
	unordered
	monotonic
	acquire
	release
	acq_rel
	seq_cst
}

// InlineHint lists inline hint values used by ssa.
pub enum InlineHint {
	none_  // No hint, let optimizer decide
	always // Always inline (e.g. V's [inline] attribute)
	never  // Never inline (e.g. V's [noinline] attribute)
	hint   // Suggest inlining (optimizer may ignore)
}

// TypeKind lists type kind values used by ssa.
pub enum TypeKind {
	void_t
	int_t
	float_t
	ptr_t
	array_t
	struct_t
	func_t
	label_t
	metadata_t
}

// Type represents type data used by ssa.
pub struct Type {
pub:
	kind        TypeKind
	width       int
	is_unsigned bool
	elem_type   TypeID // For Ptr, Array
	len         int    // For Array
	fields      []TypeID
	field_names []string
	params      []TypeID
	ret_type    TypeID
	is_c_struct bool // True for C interop structs (raw field names, typedef to C struct)
	is_union    bool // True for union types (all fields overlap at offset 0)
}

// TypeStore represents type store data used by ssa.
pub struct TypeStore {
pub mut:
	types []Type
	cache map[string]TypeID
}

const recursive_type_slot_size = 256

// new creates a TypeStore value for ssa.
pub fn TypeStore.new() TypeStore {
	mut ts := TypeStore{
		cache: map[string]TypeID{}
	}
	ts.types << Type{
		kind: .void_t
	}
	return ts
}

// get_int returns get int data for TypeStore.
pub fn (mut ts TypeStore) get_int(width int) TypeID {
	key := 'i${width}'
	if id := ts.cache[key] {
		if id > 0 {
			return id
		}
	}
	id := ts.register(Type{ kind: .int_t, width: width })
	ts.cache[key] = id
	return id
}

// get_uint returns get uint data for TypeStore.
pub fn (mut ts TypeStore) get_uint(width int) TypeID {
	key := 'u${width}'
	if id := ts.cache[key] {
		if id > 0 {
			return id
		}
	}
	id := ts.register(Type{ kind: .int_t, width: width, is_unsigned: true })
	ts.cache[key] = id
	return id
}

// get_float returns get float data for TypeStore.
pub fn (mut ts TypeStore) get_float(width int) TypeID {
	key := 'f${width}'
	if id := ts.cache[key] {
		if id > 0 {
			return id
		}
	}
	id := ts.register(Type{ kind: .float_t, width: width })
	ts.cache[key] = id
	return id
}

// get_ptr returns get ptr data for TypeStore.
pub fn (mut ts TypeStore) get_ptr(elem TypeID) TypeID {
	key := 'p${elem}'
	if id := ts.cache[key] {
		if id > 0 {
			return id
		}
	}
	id := ts.register(Type{ kind: .ptr_t, elem_type: elem })
	ts.cache[key] = id
	return id
}

// get_array returns the cached fixed-array type for (elem, length).
pub fn (mut ts TypeStore) get_array(elem TypeID, length int) TypeID {
	key := 'a${elem}_${length}'
	if id := ts.cache[key] {
		if id > 0 {
			return id
		}
	}
	id := ts.register(Type{ kind: .array_t, elem_type: elem, len: length })
	ts.cache[key] = id
	return id
}

// get_tuple returns a cached anonymous struct type holding the given element types.
pub fn (mut ts TypeStore) get_tuple(elem_types []TypeID) TypeID {
	mut key := 'tuple'
	for t in elem_types {
		key += '_${t}'
	}
	if id := ts.cache[key] {
		if id > 0 {
			return id
		}
	}
	id := ts.register(Type{
		kind:   .struct_t
		fields: elem_types
	})
	ts.cache[key] = id
	return id
}

// register supports register handling for TypeStore.
pub fn (mut ts TypeStore) register(t Type) TypeID {
	id := TypeID(ts.types.len)
	ts.types << t
	return id
}

// ValueKind lists value kind values used by ssa.
pub enum ValueKind {
	unknown
	constant
	argument
	global
	instruction
	basic_block
	string_literal   // V string struct literal (by value)
	c_string_literal // C string literal (raw char pointer)
	func_ref
}

// Value represents value data used by ssa.
pub struct Value {
pub mut:
	id    ValueID
	kind  ValueKind
	typ   TypeID
	name  string
	index int
	uses  []ValueID
}

// ConstantData carries the typed payload of a constant value. It mirrors v2's
// representation so backends can recover the original int/float/string value
// instead of re-parsing Value.name.
pub struct ConstantData {
pub:
	int_val   i64
	float_val f64
	str_val   string
}

// Instruction represents instruction data used by ssa.
pub struct Instruction {
pub mut:
	op         OpCode
	operands   []ValueID
	block      BlockID
	typ        TypeID
	pos        token.Pos
	atomic_ord AtomicOrdering
	inline     InlineHint // Inline hint for call instructions
}

// BasicBlock represents basic block data used by ssa.
pub struct BasicBlock {
pub mut:
	id     BlockID
	val_id ValueID // SSA value representing the block (0 in v3's raw-block-id model)
	name   string
	parent int
	instrs []ValueID
	preds  []BlockID
	succs  []BlockID
	// Dominators
	idom     BlockID
	dom_tree []BlockID
}

// CallConv lists call conv values used by ssa.
pub enum CallConv {
	c_decl
	fast_call
	wasm_std
}

// Linkage lists linkage values used by ssa.
pub enum Linkage {
	external
	private
	internal
}

// Function represents function data used by ssa.
pub struct Function {
pub mut:
	id           int
	name         string
	typ          TypeID
	blocks       []BlockID
	params       []ValueID
	is_c_extern  bool // C-language extern function (no V body)
	is_prototype bool // Registered declaration/signature whose body is not materialized yet
	linkage      Linkage
	call_conv    CallConv
}

// GlobalVar represents global var data used by ssa.
pub struct GlobalVar {
pub mut:
	name          string
	typ           TypeID
	linkage       Linkage
	alignment     int
	is_constant   bool
	initial_value i64  // For constants/enums, the initial integer value
	initial_data  []u8 // For constant arrays: serialized element data
}

// TargetData represents target data data used by ssa.
pub struct TargetData {
pub:
	ptr_size      int  = 8
	endian_little bool = true
}

// Module represents module data used by ssa.
@[heap]
pub struct Module {
pub mut:
	name       string
	target     TargetData
	type_store TypeStore
	values     []Value
	instrs     []Instruction
	blocks     []BasicBlock
	funcs      []Function
	globals    []GlobalVar
	// C struct names: TypeID -> C struct name (e.g. for `struct stat`). Used by
	// codegen to emit `typedef struct <name> ...;` and preserve the C layout.
	c_struct_names map[int]string
	// C structs marked @[typedef] — already a C typedef, not a struct tag.
	c_typedef_structs map[int]bool
	// Constant cache: "type:name" -> ValueID for deduplication.
	const_cache map[string]ValueID
}

// new creates a Module value for ssa.
pub fn Module.new() &Module {
	mut m := &Module{
		type_store:        TypeStore.new()
		c_struct_names:    map[int]string{}
		c_typedef_structs: map[int]bool{}
		const_cache:       map[string]ValueID{}
	}
	m.values << Value{
		kind: .unknown
		id:   0
	}
	return m
}

// add_value updates add value state for Module.
pub fn (mut m Module) add_value(kind ValueKind, typ TypeID, name string, index int) ValueID {
	id := ValueID(m.values.len)
	m.values << Value{
		id:    id
		kind:  kind
		typ:   typ
		name:  name
		index: index
	}
	return id
}

// add_instr updates add instr state for Module.
pub fn (mut m Module) add_instr(op OpCode, block BlockID, typ TypeID, operands []ValueID) ValueID {
	instr_idx := m.instrs.len
	m.instrs << Instruction{
		op:       op
		block:    block
		typ:      typ
		operands: operands
	}
	val_id := m.add_value(.instruction, typ, '', instr_idx)
	mut blk := m.blocks[block]
	blk.instrs << val_id
	m.blocks[block] = blk
	for op_id in m.instrs[instr_idx].value_operands() {
		if op_id > 0 && op_id < m.values.len && val_id !in m.values[op_id].uses {
			mut op_val := m.values[op_id]
			op_val.uses << val_id
			m.values[op_id] = op_val
		}
	}
	return val_id
}

// add_instr_front creates an instruction and prepends it to the block (used for
// phi insertion by mem2reg, which requires phis at the top of a block).
pub fn (mut m Module) add_instr_front(op OpCode, block BlockID, typ TypeID, operands []ValueID) ValueID {
	instr_idx := m.instrs.len
	m.instrs << Instruction{
		op:       op
		block:    block
		typ:      typ
		operands: operands
	}
	val_id := m.add_value(.instruction, typ, '', instr_idx)
	mut blk := m.blocks[block]
	blk.instrs.prepend(val_id)
	m.blocks[block] = blk
	for op_id in m.instrs[instr_idx].value_operands() {
		if op_id > 0 && op_id < m.values.len && val_id !in m.values[op_id].uses {
			mut op_val := m.values[op_id]
			op_val.uses << val_id
			m.values[op_id] = op_val
		}
	}
	return val_id
}

// append_phi_operands appends a (val, block_id) pair to a phi instruction.
pub fn (mut m Module) append_phi_operands(instr_idx int, val ValueID, block_id BlockID) {
	mut instr := m.instrs[instr_idx]
	instr.operands << val
	instr.operands << block_id
	m.instrs[instr_idx] = instr
	if val > 0 && val < m.values.len {
		// keep use lists consistent: the phi value uses `val`
	}
}

// add_block updates add block state for Module.
pub fn (mut m Module) add_block(func_id int, name string) BlockID {
	id := BlockID(m.blocks.len)
	unique := '${name}_${id}'
	m.blocks << BasicBlock{
		id:     id
		name:   unique
		parent: func_id
	}
	mut f := m.funcs[func_id]
	f.blocks << id
	f.is_prototype = false
	m.funcs[func_id] = f
	return id
}

// new_function supports new function handling for Module.
pub fn (mut m Module) new_function(name string, ret TypeID) int {
	for i, f in m.funcs {
		if f.name == name {
			return i
		}
	}
	id := m.funcs.len
	m.funcs << Function{
		id:   id
		name: name
		typ:  ret
	}
	return id
}

// --- Safe mutation helpers (avoid chained struct-array mutations) ---

// func_add_param supports func add param handling for Module.
pub fn (mut m Module) func_add_param(func_id int, param_val ValueID) {
	mut f := m.funcs[func_id]
	f.params << param_val
	m.funcs[func_id] = f
}

// func_set_c_extern supports func set c extern handling for Module.
pub fn (mut m Module) func_set_c_extern(func_id int, val bool) {
	mut f := m.funcs[func_id]
	f.is_c_extern = val
	m.funcs[func_id] = f
}

// func_set_prototype supports func set prototype handling for Module.
pub fn (mut m Module) func_set_prototype(func_id int, val bool) {
	mut f := m.funcs[func_id]
	f.is_prototype = val
	m.funcs[func_id] = f
}

// block_add_succ supports block add succ handling for Module.
pub fn (mut m Module) block_add_succ(from BlockID, to BlockID) {
	mut blk := m.blocks[from]
	blk.succs << to
	m.blocks[from] = blk
}

// block_add_pred supports block add pred handling for Module.
pub fn (mut m Module) block_add_pred(to BlockID, from BlockID) {
	mut blk := m.blocks[to]
	blk.preds << from
	m.blocks[to] = blk
}

// add_global updates add global state for Module.
pub fn (mut m Module) add_global(name string, typ TypeID) ValueID {
	id := m.globals.len
	m.globals << GlobalVar{
		name:    name
		typ:     typ
		linkage: .private
	}
	ptr_typ := m.type_store.get_ptr(typ)
	return m.add_value(.global, ptr_typ, name, id)
}

// add_global_with_data registers a private global initialized from raw bytes
// (used for const arrays serialized to element data).
pub fn (mut m Module) add_global_with_data(name string, elem_type TypeID, is_const bool, data []u8) ValueID {
	id := m.globals.len
	m.globals << GlobalVar{
		name:         name
		typ:          elem_type
		linkage:      .private
		is_constant:  is_const
		initial_data: data
	}
	ptr_typ := m.type_store.get_ptr(elem_type)
	return m.add_value(.global, ptr_typ, name, id)
}

// add_external_global registers (or reuses) a global defined outside this module
// (e.g. C runtime globals such as __stdoutp). Returns the global pointer value.
pub fn (mut m Module) add_external_global(name string, typ TypeID) ValueID {
	for v in m.values {
		if v.kind == .global && v.name == name {
			return v.id
		}
	}
	id := m.globals.len
	m.globals << GlobalVar{
		name:    name
		typ:     typ
		linkage: .external
	}
	ptr_typ := m.type_store.get_ptr(typ)
	return m.add_value(.global, ptr_typ, name, id)
}

// get_or_add_const returns get or add const data for Module.
pub fn (mut m Module) get_or_add_const(typ TypeID, name string) ValueID {
	key := '${typ}:${name}'
	if existing := m.const_cache[key] {
		if existing > 0 {
			return existing
		}
	}
	id := m.add_value(.constant, typ, name, 0)
	m.const_cache[key] = id
	return id
}

// get_block_from_val converts a basic-block value operand to its block index.
pub fn (m &Module) get_block_from_val(val_id int) int {
	return m.values[val_id].index
}

// type_size returns the byte size for an SSA type on the current target.
pub fn (m &Module) type_size(typ_id TypeID) int {
	mut visiting := []bool{len: m.type_store.types.len}
	mut cache := []int{len: m.type_store.types.len}
	return m.type_size_inner(typ_id, 0, mut visiting, mut cache)
}

// type_size_inner returns type size inner data for Module.
fn (m &Module) type_size_inner(typ_id TypeID, depth int, mut visiting []bool, mut cache []int) int {
	if typ_id <= 0 || typ_id >= m.type_store.types.len {
		return 0
	}
	if depth > 32 {
		return recursive_type_slot_size
	}
	if cache[typ_id] > 0 {
		return cache[typ_id]
	}
	typ := m.type_store.types[typ_id]
	if typ.width > 0 {
		size := (typ.width + 7) / 8
		cache[typ_id] = size
		return size
	}
	if typ.kind == .array_t {
		if visiting[typ_id] {
			return recursive_type_slot_size
		}
		visiting[typ_id] = true
		elem := m.type_size_inner(typ.elem_type, depth + 1, mut visiting, mut cache)
		visiting[typ_id] = false
		mut total := elem * typ.len
		if total <= 0 {
			total = 0
		}
		cache[typ_id] = total
		return total
	}
	if typ.elem_type > 0 && typ.fields.len == 0 {
		cache[typ_id] = 8
		return 8
	}
	if typ.fields.len == 0 {
		if typ.params.len > 0 || typ.ret_type > 0 {
			cache[typ_id] = 8
			return 8
		}
		return 0
	}
	if typ.fields.len > 256 {
		cache[typ_id] = 8
		return 8
	}
	if visiting[typ_id] {
		return recursive_type_slot_size
	}
	visiting[typ_id] = true
	mut total := 0
	if typ.is_union {
		// Unions: all fields overlap at offset 0; size is the largest field,
		// rounded up to the largest field alignment.
		mut max_size := 0
		mut max_align := 1
		for i in 0 .. typ.fields.len {
			field_typ := typ.fields[i]
			s := m.type_size_inner(field_typ, depth + 1, mut visiting, mut cache)
			if s > max_size {
				max_size = s
			}
			a := m.type_align_for_layout(field_typ)
			if a > max_align {
				max_align = a
			}
		}
		total = if max_align > 1 && max_size % max_align != 0 {
			(max_size + max_align - 1) & ~(max_align - 1)
		} else {
			max_size
		}
	} else {
		mut offset := 0
		mut max_align := 1
		for i in 0 .. typ.fields.len {
			field_typ := typ.fields[i]
			align := m.type_align_for_layout(field_typ)
			if align > max_align {
				max_align = align
			}
			if align > 1 && offset % align != 0 {
				offset = (offset + align - 1) & ~(align - 1)
			}
			offset += m.type_size_inner(field_typ, depth + 1, mut visiting, mut cache)
		}
		total = if max_align > 1 && offset % max_align != 0 {
			(offset + max_align - 1) & ~(max_align - 1)
		} else {
			offset
		}
	}
	visiting[typ_id] = false
	if total <= 0 {
		total = 8
	}
	cache[typ_id] = total
	return total
}

// type_align returns the ABI alignment for an SSA type on the current target.
pub fn (m &Module) type_align(typ_id TypeID) int {
	return m.type_align_for_layout(typ_id)
}

// type_align_for_layout returns type align for layout data for Module.
fn (m &Module) type_align_for_layout(typ_id TypeID) int {
	return m.type_align_for_layout_inner(typ_id, 0)
}

// type_align_for_layout_inner returns type align for layout inner data for Module.
fn (m &Module) type_align_for_layout_inner(typ_id TypeID, depth int) int {
	if typ_id <= 0 || typ_id >= m.type_store.types.len {
		return 1
	}
	if depth > 16 {
		return 8
	}
	typ := m.type_store.types[typ_id]
	if typ.width > 0 {
		size := (typ.width + 7) / 8
		if size >= 8 {
			return 8
		}
		if size >= 4 {
			return 4
		}
		return 1
	}
	if typ.kind == .array_t {
		return m.type_align_for_layout_inner(typ.elem_type, depth + 1)
	}
	if typ.elem_type > 0 && typ.fields.len == 0 {
		return 8
	}
	if typ.fields.len > 0 {
		return 8
	}
	if typ.params.len > 0 || typ.ret_type > 0 {
		return 8
	}
	return 1
}

// replace_uses supports replace uses handling for Module.
pub fn (mut m Module) replace_uses(old_id ValueID, new_id ValueID) {
	if old_id <= 0 || old_id >= m.values.len {
		return
	}
	for user_id in m.values[old_id].uses {
		if user_id <= 0 || user_id >= m.values.len {
			continue
		}
		val := m.values[user_id]
		if val.kind != .instruction {
			continue
		}
		mut instr := m.instrs[val.index]
		for i in 0 .. instr.operands.len {
			if instr.operands[i] == old_id {
				instr.operands[i] = new_id
			}
		}
		m.instrs[val.index] = instr
	}
}

// value_operands returns value operands data for Instruction.
pub fn (i &Instruction) value_operands() []ValueID {
	if i.op == .br {
		if i.operands.len > 0 {
			mut r := []ValueID{}
			r << i.operands[0]
			return r
		}
		return []ValueID{}
	}
	if i.op == .jmp {
		return []ValueID{}
	}
	if i.op == .switch_ {
		// switch_ %val, default_block, [case_val, block]...
		// Only %val and the case values are SSA values; blocks are raw block ids.
		mut r := []ValueID{}
		if i.operands.len > 0 {
			r << i.operands[0]
		}
		for oi := 2; oi < i.operands.len; oi += 2 {
			r << i.operands[oi]
		}
		return r
	}
	if i.op == .phi {
		mut r := []ValueID{}
		for oi := 0; oi < i.operands.len; oi += 2 {
			r << i.operands[oi]
		}
		return r
	}
	return i.operands
}

// struct_field_offset returns the byte offset of a field in a struct type.
pub fn (m &Module) struct_field_offset(typ_id TypeID, field_idx int) int {
	if typ_id <= 0 || typ_id >= m.type_store.types.len {
		return 0
	}
	typ := m.type_store.types[typ_id]
	if typ.kind != .struct_t {
		return 0
	}
	if typ.is_union {
		return 0
	}
	mut visiting := []bool{len: m.type_store.types.len}
	mut cache := []int{len: m.type_store.types.len}
	visiting[typ_id] = true
	mut offset := 0
	for i in 0 .. field_idx {
		if i >= typ.fields.len {
			break
		}
		align := m.type_align_for_layout(typ.fields[i])
		if align > 1 && offset % align != 0 {
			offset = (offset + align - 1) & ~(align - 1)
		}
		offset += m.type_size_inner(typ.fields[i], 1, mut visiting, mut cache)
	}
	if field_idx < typ.fields.len {
		align := m.type_align_for_layout(typ.fields[field_idx])
		if align > 1 && offset % align != 0 {
			offset = (offset + align - 1) & ~(align - 1)
		}
	}
	return offset
}

// struct_field_size returns the byte size of a field in a struct type.
pub fn (m &Module) struct_field_size(typ_id TypeID, field_idx int) int {
	if typ_id <= 0 || typ_id >= m.type_store.types.len {
		return 0
	}
	typ := m.type_store.types[typ_id]
	if typ.kind != .struct_t || field_idx < 0 || field_idx >= typ.fields.len {
		return 0
	}
	mut visiting := []bool{len: m.type_store.types.len}
	mut cache := []int{len: m.type_store.types.len}
	visiting[typ_id] = true
	return m.type_size_inner(typ.fields[field_idx], 1, mut visiting, mut cache)
}
