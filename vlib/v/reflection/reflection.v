@[has_globals]
module reflection

import arrays

__global g_reflection = Reflection{}

@[heap; minify]
pub struct Reflection {
pub mut:
	modules      []Module
	funcs        []Function
	types        []Type
	type_symbols []TypeSymbol
	enums        []Enum
	interfaces   []Interface
	strings      map[int]string
}

pub enum VLanguage {
	v
	c
	js
	amd64 // aka x86_64
	i386
	arm64 // 64-bit arm
	arm32 // 32-bit arm
	rv64  // 64-bit risc-v
	rv32  // 32-bit risc-v
	wasm32
}

type VType = u32

// max of 8
pub enum VTypeFlag {
	option
	result
	variadic
	generic
	shared_f
	atomic_f
}

pub enum VKind {
	placeholder
	void
	voidptr
	byteptr
	charptr
	i8
	i16
	i32
	i64
	int
	isize
	u8
	u16
	u32
	u64
	usize
	f32
	f64
	char
	rune
	bool
	none
	string
	array
	array_fixed
	map
	chan
	any
	struct
	generic_inst
	multi_return
	sum_type
	alias
	enum
	function
	interface
	float_literal
	int_literal
	aggregate
	thread
}

// return true if `flag` is set on `t`
@[inline]
pub fn (t VType) has_flag(flag VTypeFlag) bool {
	return int(t) & (1 << (int(flag) + 24)) > 0
}

@[inline]
pub fn (t VType) idx() int {
	return u16(t) & 0xffff
}

pub fn (t VType) str() string {
	return 'VType(0x${t.hex()} = ${u32(t)})'
}

// return true if `t` is a pointer (nr_muls>0)
@[inline]
pub fn (t VType) is_ptr() bool {
	// any normal pointer, i.e. &Type, &&Type etc;
	// Note: voidptr, charptr and byteptr are NOT included!
	return (int(t) >> 16) & 0xff > 0
}

pub struct ArrayFixed {
pub:
	size      int // array size
	elem_type int // elem type idx
}

pub struct Array {
pub:
	nr_dims   int // nr of dimensions
	elem_type int // elem type idx
}

pub struct Alias {
pub:
	parent_idx int       // parent type idx
	language   VLanguage // language
}

pub struct Interface {
pub:
	name       string        // interface name
	methods    []Function    // methods
	fields     []StructField // fields
	is_generic bool          // is generic?
}

pub struct None {
pub:
	parent_idx int
}

pub struct Enum {
pub:
	vals    []string // enum values
	is_flag bool     // is flag?
}

pub struct StructField {
pub:
	name   string   // field name
	typ    VType    // type
	attrs  []string // field attrs
	is_pub bool     // is pub?
	is_mut bool     // is mut?
}

pub struct Struct {
pub:
	parent_idx int           // parent type
	attrs      []string      // struct attrs
	fields     []StructField // fields
}

pub struct SumType {
pub:
	parent_idx int     // parent type
	variants   []VType // variant type
}

pub struct Map {
pub:
	key_type   VType // key type
	value_type VType // value type
}

pub struct MultiReturn {
pub:
	types []VType // types
}

pub type TypeInfo = Alias
	| Array
	| ArrayFixed
	| Enum
	| Function
	| Interface
	| Map
	| MultiReturn
	| None
	| Struct
	| SumType

pub struct TypeSymbol {
pub:
	name       string     // symbol name
	mod        string     // mod name
	idx        int        // symbol idx
	parent_idx int        // symbol parent idx
	language   VLanguage  // language
	kind       VKind      // kind
	info       TypeInfo   // info
	methods    []Function // methods
}

pub struct Type {
pub:
	name string     // type name
	idx  int        // type idx
	sym  TypeSymbol // type symbol
}

pub struct Module {
pub:
	name string // module name
}

pub struct FunctionArg {
pub:
	name   string // argument name
	typ    VType  // argument type idx
	is_mut bool   // is mut?
}

pub struct Function {
pub:
	mod_name     string        // module name
	name         string        // function/method name
	args         []FunctionArg // function/method args
	file_idx     int           // source file name
	line_start   int           // decl start line
	line_end     int           // decl end line
	is_variadic  bool          // is variadic?
	return_typ   VType         // return type idx
	receiver_typ VType         // receiver type idx (is a method)
	is_pub       bool          // is pub?
}

// API module

pub fn get_string_by_idx(idx int) string {
	return g_reflection.strings[idx]
}

// type_of returns the type info of the passed value
pub fn type_of[T](val T) Type {
	return g_reflection.types.filter(it.idx == typeof[T]().idx & 0xff00ffff)[0]
}

// get_modules returns the module name built with V source
pub fn get_modules() []Module {
	return g_reflection.modules
}

// get_functions returns the functions built with V source
pub fn get_funcs() []Function {
	mut out := g_reflection.funcs.clone()
	out << arrays.flatten[Function](get_types().map(it.sym.methods).filter(it.len != 0))
	return out
}

pub fn get_structs() []Type {
	struct_idxs := g_reflection.type_symbols.filter(it.kind == .struct).map(it.idx)
	return g_reflection.types.filter(it.idx in struct_idxs)
}

// get_types returns the registered types
pub fn get_types() []Type {
	return g_reflection.types
}

// get_enums returns the registered enums
pub fn get_enums() []Type {
	enum_idxs := g_reflection.type_symbols.filter(it.kind == .enum).map(it.idx)
	return g_reflection.types.filter(it.idx in enum_idxs)
}

// get_aliases returns the registered aliases
pub fn get_aliases() []Type {
	alias_idxs := g_reflection.type_symbols.filter(it.kind == .alias).map(it.idx)
	return g_reflection.types.filter(it.idx in alias_idxs)
}

// get_interfaces returns the registered aliases
pub fn get_interfaces() []Interface {
	iface_idxs := g_reflection.type_symbols.filter(it.kind == .interface).map(it.idx)
	return g_reflection.types.filter(it.idx in iface_idxs).map(it.sym.info as Interface)
}

// get_sum_types returns the registered sum types
pub fn get_sum_types() []Type {
	sumtype_idxs := g_reflection.type_symbols.filter(it.kind == .sum_type).map(it.idx)
	return g_reflection.types.filter(it.idx in sumtype_idxs)
}

// get_type_symbol returns the registered type symbols
pub fn get_type_symbols() []TypeSymbol {
	return g_reflection.type_symbols
}

// Type API
pub fn type_name(idx int) string {
	t := g_reflection.types.filter(it.idx == idx)
	return if t.len != 0 { t[0].name } else { '' }
}

pub fn get_type(idx int) ?Type {
	t := g_reflection.types.filter(it.idx == idx)
	return if t.len != 0 { t[0] } else { none }
}

// Type Symbol API
pub fn type_symbol_name(idx int) string {
	t := g_reflection.type_symbols.filter(it.idx == idx)
	return if t.len != 0 { t[0].name } else { '' }
}

pub fn get_type_symbol(idx int) ?TypeSymbol {
	t := g_reflection.type_symbols.filter(it.idx == idx)
	return if t.len != 0 { t[0] } else { none }
}

// V reflection metainfo API (called from backend to fill metadata info)

@[markused]
fn add_module(mod_name string) {
	g_reflection.modules << Module{mod_name}
}

@[markused]
fn add_func(func Function) {
	g_reflection.funcs << func
}

@[markused]
fn add_type(type_ Type) {
	g_reflection.types << Type{
		...type_
		sym: g_reflection.type_symbols.filter(it.idx == type_.idx).first()
	}
}

@[markused]
fn add_type_symbol(typesymbol TypeSymbol) {
	g_reflection.type_symbols << typesymbol
}

@[markused]
fn add_string(str string, idx int) {
	g_reflection.strings[idx] = str
}
