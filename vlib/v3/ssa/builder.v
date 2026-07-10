module ssa

import v3.flat
import v3.types

const arm64_force_external_syms = ['_malloc', '_free', '_calloc', '_realloc', '_exit', '_abort',
	'_memcpy', '_memmove', '_memset', '_memcmp', '___stdoutp', '___stderrp', '_puts', '_printf',
	'_write', '_read', '_open', '_close', '_fwrite', '_fflush', '_fopen', '_fclose', '_putchar',
	'_sprintf', '_snprintf', '_fprintf', '_sscanf', '_mmap', '_munmap', '_getcwd', '_access',
	'_readlink', '_getenv', '_strlen', '_opendir', '_readdir', '_closedir', '_mkdir', '_rmdir',
	'_unlink', '_rename', '_remove', '_stat', '_lstat', '_fstat', '_chmod', '_chdir', '_realpath',
	'_symlink', '_link', '_getpid', '_getuid', '_geteuid', '_fork', '_execve', '_execvp', '_waitpid',
	'_kill', '_system', '_posix_spawn', '_signal', '_atexit', '_fgets', '_fputs', '_fread', '_fseek',
	'_ftell', '_rewind', '_fileno', '_popen', '_pclose', '_dup', '_dup2', '_pipe', '_isatty',
	'_freopen', '_dprintf', '_getc', '_strdup', '_strcmp', '_strncmp', '_strchr', '_strrchr',
	'_strerror', '_strncasecmp', '_strcasecmp', '_atoi', '_atof', '_qsort', '_time', '_localtime_r',
	'_gmtime_r', '_mktime', '_gettimeofday', '_clock', '_clock_gettime_nsec_np',
	'_mach_absolute_time', '_mach_timebase_info', '_nanosleep', '_sleep', '_usleep', '_strftime',
	'_task_info', '_mach_task_self_', '_rand', '_srand', '_isdigit', '_isspace', '_tolower',
	'_toupper', '_setenv', '_unsetenv', '_sysconf', '_uname', '_gethostname', '_pthread_mutex_init',
	'_pthread_mutex_lock', '_pthread_mutex_trylock', '_pthread_mutex_unlock',
	'_pthread_mutex_destroy', '_pthread_self', '_pthread_create', '_pthread_join',
	'_pthread_attr_init', '_pthread_attr_setstacksize', '_pthread_attr_destroy',
	'_pthread_rwlockattr_init', '_pthread_rwlockattr_setpshared', '_pthread_rwlockattr_destroy',
	'_pthread_rwlock_init', '_pthread_rwlock_rdlock', '_pthread_rwlock_wrlock',
	'_pthread_rwlock_tryrdlock', '_pthread_rwlock_trywrlock', '_pthread_rwlock_unlock',
	'_pthread_rwlock_destroy', '_pthread_condattr_init', '_pthread_condattr_setpshared',
	'_pthread_condattr_destroy', '_pthread_cond_init', '_pthread_cond_signal', '_pthread_cond_wait',
	'_pthread_cond_timedwait', '_pthread_cond_destroy', '_arc4random_buf', '_proc_pidpath',
	'_backtrace', '_backtrace_symbols', '_backtrace_symbols_fd', '_dispatch_semaphore_create',
	'_dispatch_semaphore_signal', '_dispatch_semaphore_wait', '_dispatch_time', '_dispatch_release',
	'_setvbuf', '_setbuf', '_memchr', '_getlogin_r', '_getppid', '_getgid', '_getegid', '_ftruncate',
	'_mkstemp', '_statvfs', '_chown', '_sigaction', '_sigemptyset', '_sigaddset', '_sigprocmask',
	'_select', '_kqueue', '_abs', '_tcgetattr', '_tcsetattr', '_ioctl', '_getchar', '_getline',
	'_fdopen', '_feof', '_ferror', '_setpgid', '_ptrace', '_wait', '_timegm', '_clock_gettime',
	'_aligned_alloc', '_utime', '_getlogin', '_environ', '___error', '___stdinp',
	'__dyld_get_image_name', '__dyld_get_image_header', '_cos', '_sin', '_tan', '_acos', '_asin',
	'_atan', '_atan2', '_cosh', '_sinh', '_tanh', '_acosh', '_asinh', '_atanh', '_exp', '_exp2',
	'_log', '_log2', '_log10', '_pow', '_sqrt', '_cbrt', '_ceil', '_floor', '_round', '_trunc',
	'_fmod', '_remainder', '_fabs', '_copysign', '_fmax', '_fmin', '_hypot', '_ldexp', '_frexp',
	'_modf', '_scalbn', '_ilogb', '_logb', '_erf', '_erfc', '_lgamma', '_tgamma', '_j0', '_j1',
	'_jn', '_y0', '_y1', '_yn', '_mprotect', '_sys_icache_invalidate', '_objc_msgSend',
	'_objc_getClass', '_sel_registerName', '_objc_alloc_init', '_objc_autoreleasePoolPush',
	'_objc_autoreleasePoolPop', '_MTLCreateSystemDefaultDevice', '_dlopen', '_dlsym']

const bench_runtime_stub_names = ['current_rss_kb', 'macos_rss_kb', 'linux_rss_kb',
	'bench.current_rss_kb', 'bench.macos_rss_kb', 'bench.linux_rss_kb', 'v3.bench.current_rss_kb',
	'v3.bench.macos_rss_kb', 'v3.bench.linux_rss_kb']

// Builder stores state for SSA construction.
pub struct Builder {
mut:
	m                  &Module            = unsafe { nil }
	a                  &flat.FlatAst      = unsafe { nil }
	tc                 &types.TypeChecker = unsafe { nil }
	used_fns           map[string]bool
	cur_module         string
	cur_func           int
	cur_func_ret_type  string
	cur_block          BlockID
	vars               map[string]ValueID
	var_type_names     map[string]string
	i64_type           TypeID
	i32_type           TypeID
	i8_type            TypeID
	i1_type            TypeID
	u64_type           TypeID
	u32_type           TypeID
	u16_type           TypeID
	u8_type            TypeID
	f32_type           TypeID
	f64_type           TypeID
	void_type          TypeID
	str_type           TypeID
	array_type         TypeID
	map_type           TypeID
	map_state_type     TypeID
	fn_types           map[string]TypeID
	fn_ids             map[string]int
	const_exprs        map[string]flat.NodeId
	struct_types       map[string]TypeID
	struct_field_types map[string]string
	option_types       map[string]TypeID
	sum_type_variants  map[string][]string
	sum_type_canonical map[string]string
	enum_types         map[string]bool
	flag_enum_types    map[string]bool
	enum_values        map[string]int
	enum_member_values map[string]int
	enum_member_dupes  map[string]bool
	c_fn_ids           map[string]int
	c_fn_types         map[string]TypeID
	label_blocks       map[string]BlockID
	defer_body_ids     []flat.NodeId
	break_targets      []BlockID
	continue_targets   []BlockID
	top_level_main     bool
	// --- Build-control machinery (ported from v2 build_all) ---
	// When set, build_functions only materializes this one function body (hot reload).
	hot_fn string
	// When set, build_functions registers signatures only and marks them prototypes.
	skip_fn_bodies bool
	// When non-empty, all functions declared in these modules are skipped (dead
	// code elimination for unused backends/modules).
	skip_modules map[string]bool
	// SSA-level type alias name -> resolved base TypeID. Supplements the checker's
	// alias map (used when building without a checker).
	type_aliases map[string]TypeID
}

// VarBinding represents var binding data used by ssa.
struct VarBinding {
	exists bool
	addr   ValueID
	typ    string
}

// IfGuardState stores if guard state state used by ssa.
struct IfGuardState {
	active    bool
	name      string
	old       VarBinding
	slot      ValueID
	value_ptr ValueID
	typ       TypeID
	typ_name  string
}

// BuildOptions controls which functions a build materializes. The zero value
// reproduces the default whole-program build.
pub struct BuildOptions {
pub:
	hot_fn         string   // build only this function's body (hot reload)
	skip_fn_bodies bool     // register signatures only, mark them prototypes
	skip_modules   []string // skip all functions declared in these modules
}

// build supports build handling for ssa.
pub fn build(a_ &flat.FlatAst) &Module {
	return build_with_used(a_, map[string]bool{}, unsafe { nil })
}

// build_with_used builds with used data for ssa.
pub fn build_with_used(a_ &flat.FlatAst, used_fns map[string]bool, tc &types.TypeChecker) &Module {
	return build_with_options(a_, used_fns, tc, BuildOptions{})
}

// build_with_options builds with options data for ssa.
pub fn build_with_options(a_ &flat.FlatAst, used_fns map[string]bool, tc &types.TypeChecker, opts BuildOptions) &Module {
	mut b := Builder{
		m:                  Module.new()
		a:                  unsafe { a_ }
		tc:                 unsafe { tc }
		used_fns:           used_fns
		vars:               map[string]ValueID{}
		var_type_names:     map[string]string{}
		fn_types:           map[string]TypeID{}
		fn_ids:             map[string]int{}
		const_exprs:        map[string]flat.NodeId{}
		struct_types:       map[string]TypeID{}
		struct_field_types: map[string]string{}
		option_types:       map[string]TypeID{}
		sum_type_variants:  map[string][]string{}
		sum_type_canonical: map[string]string{}
		enum_types:         map[string]bool{}
		flag_enum_types:    map[string]bool{}
		enum_values:        map[string]int{}
		enum_member_values: map[string]int{}
		enum_member_dupes:  map[string]bool{}
		c_fn_ids:           map[string]int{}
		c_fn_types:         map[string]TypeID{}
		label_blocks:       map[string]BlockID{}
		defer_body_ids:     []flat.NodeId{}
		break_targets:      []BlockID{}
		continue_targets:   []BlockID{}
		skip_modules:       map[string]bool{}
		type_aliases:       map[string]TypeID{}
	}
	b.void_type = TypeID(0)
	b.i64_type = b.m.type_store.get_int(64)
	b.i32_type = b.m.type_store.get_int(32)
	b.i8_type = b.m.type_store.get_int(8)
	b.i1_type = b.m.type_store.get_int(1)
	b.u64_type = b.m.type_store.get_uint(64)
	b.u32_type = b.m.type_store.get_uint(32)
	b.u16_type = b.m.type_store.get_uint(16)
	b.u8_type = b.m.type_store.get_uint(8)
	b.f32_type = b.m.type_store.get_float(32)
	b.f64_type = b.m.type_store.get_float(64)
	mut str_fields := []TypeID{}
	str_fields << b.m.type_store.get_ptr(b.i8_type)
	str_fields << b.i32_type
	// `is_lit` occupies the trailing 4-byte slot (offset 12) that the struct is padded
	// to anyway. It must be a named field so `s.is_lit` (e.g. in `string.free`) resolves
	// to offset 12 instead of defaulting to offset 0 (which would read `str`).
	str_fields << b.i32_type
	mut str_field_names := []string{}
	str_field_names << 'str'
	str_field_names << 'len'
	str_field_names << 'is_lit'
	b.str_type = b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      str_fields
		field_names: str_field_names
	})
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut array_fields := []TypeID{}
	array_fields << ptr_i8
	array_fields << b.i32_type
	array_fields << b.i32_type
	array_fields << b.i32_type
	array_fields << b.i32_type
	array_fields << b.i32_type
	mut array_field_names := []string{}
	array_field_names << 'data'
	array_field_names << 'offset'
	array_field_names << 'len'
	array_field_names << 'cap'
	array_field_names << 'flags'
	array_field_names << 'element_size'
	b.array_type = b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      array_fields
		field_names: array_field_names
	})
	mut map_state_fields := []TypeID{}
	map_state_fields << ptr_i8
	map_state_fields << ptr_i8
	map_state_fields << b.i64_type
	map_state_fields << b.i64_type
	map_state_fields << b.i64_type
	map_state_fields << b.i64_type
	mut map_state_field_names := []string{}
	map_state_field_names << 'keys'
	map_state_field_names << 'vals'
	map_state_field_names << 'cap'
	map_state_field_names << 'len'
	map_state_field_names << 'key_size'
	map_state_field_names << 'val_size'
	b.map_state_type = b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      map_state_fields
		field_names: map_state_field_names
	})
	mut map_fields := []TypeID{}
	map_fields << b.m.type_store.get_ptr(b.map_state_type)
	mut map_field_names := []string{}
	map_field_names << 'state'
	b.map_type = b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      map_fields
		field_names: map_field_names
	})
	b.hot_fn = opts.hot_fn
	b.skip_fn_bodies = opts.skip_fn_bodies
	for mname in opts.skip_modules {
		b.skip_modules[mname] = true
	}
	b.m.name = b.main_module_name()
	b.register_type_aliases()
	b.register_types()
	b.register_consts()
	b.register_globals()
	b.register_functions()
	b.build_functions()
	return b.m
}

// register_types updates register types state for ssa.
fn (mut b Builder) register_types() {
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .struct_decl {
			typ_id := b.m.type_store.register(Type{
				kind: .struct_t
			})
			b.register_struct_type_name(node.value, cur_module, typ_id)
		} else if node.kind == .type_decl && node.children_count > 0 {
			typ_id := b.m.type_store.register(Type{
				kind: .struct_t
			})
			b.register_sum_type_name(node.value, cur_module, typ_id)
		} else if node.kind == .enum_decl {
			b.register_enum_values(node, cur_module)
		}
	}

	cur_module = ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .struct_decl {
			mut field_types := []TypeID{}
			mut field_names := []string{}
			for i in 0 .. node.children_count {
				f := b.a.child_node(&node, i)
				field_type := b.resolve_type_in_module(f.typ, cur_module)
				field_type_name := qualify_type_ref_name(f.typ, cur_module)
				field_types << field_type
				field_names << f.value
				b.struct_field_types[node.value + '.' + f.value] = field_type_name
				short_name := node.value.all_after('.')
				b.struct_field_types[short_name + '.' + f.value] = field_type_name
				if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
					b.struct_field_types[cur_module + '.' + short_name + '.' + f.value] = field_type_name
				}
			}
			typ_id := b.struct_type_id_for_decl(node.value, cur_module)
			b.m.type_store.types[typ_id] = Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
			}
		} else if node.kind == .type_decl && node.children_count > 0 {
			sum_name := qualify_type_name(node.value, cur_module)
			variants := b.sum_type_variants[sum_name] or { []string{} }
			mut field_types := []TypeID{}
			mut field_names := []string{}
			field_types << b.i32_type
			field_names << 'typ'
			for variant in variants {
				mut variant_type := b.resolve_type_in_module(variant, cur_module)
				if variant_type > 0 && variant_type < b.m.type_store.types.len
					&& b.m.type_store.types[variant_type].kind == .struct_t {
					variant_type = b.m.type_store.get_ptr(variant_type)
				}
				field_types << variant_type
				field_names << sum_variant_field_name(variant)
			}
			typ_id := b.struct_type_id_for_decl(node.value, cur_module)
			b.m.type_store.types[typ_id] = Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
			}
		}
	}

	cur_module = ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .type_decl || node.children_count == 0 {
			continue
		}
		sum_typ_id := b.struct_type_id_for_decl(node.value, cur_module)
		mut field_types := []TypeID{}
		mut field_names := []string{}
		field_types << b.i64_type
		field_names << 'typ'
		b.register_sum_field_type(node.value, cur_module, 'typ', 'int')
		variants := b.sum_type_variants_for_decl(node, cur_module)
		for variant in variants {
			field_name := sum_variant_field_name(variant)
			mut field_type := b.resolve_type_in_module(variant, cur_module)
			if field_type > 0 && field_type < b.m.type_store.types.len
				&& b.m.type_store.types[field_type].kind == .struct_t {
				field_type = b.m.type_store.get_ptr(field_type)
			}
			field_types << field_type
			field_names << field_name
			b.register_sum_field_type(node.value, cur_module, field_name, variant)
		}
		b.m.type_store.types[sum_typ_id] = Type{
			kind:        .struct_t
			fields:      field_types
			field_names: field_names
		}
	}
	b.register_multi_return_types()
}

// register_multi_return_types updates register multi return types state for ssa.
fn (mut b Builder) register_multi_return_types() {
	if b.tc == unsafe { nil } {
		return
	}
	for _, ret in b.tc.fn_ret_types {
		if ret is types.MultiReturn {
			b.register_multi_return_type(ret)
		}
	}
}

// register_multi_return_type updates register multi return type state for ssa.
fn (mut b Builder) register_multi_return_type(ret types.MultiReturn) TypeID {
	name := b.multi_return_c_type(ret)
	if typ := b.struct_types[name] {
		return typ
	}
	typ_id := b.m.type_store.register(Type{
		kind: .struct_t
	})
	b.struct_types[name] = typ_id
	mut field_types := []TypeID{}
	mut field_names := []string{}
	for i, field_type in ret.types {
		field_types << b.ssa_type_from_checker_type(field_type)
		field_name := 'arg${i}'
		field_names << field_name
		b.struct_field_types[name + '.' + field_name] = field_type.name()
	}
	b.m.type_store.types[typ_id] = Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	}
	return typ_id
}

// multi_return_c_type supports multi return c type handling for Builder.
fn (b &Builder) multi_return_c_type(ret types.MultiReturn) string {
	mut parts := []string{}
	for field_type in ret.types {
		parts << b.tc.c_type(field_type)
	}
	return 'multi_return_${parts.join('_')}'
}

// primitive_type_name supports primitive type name handling for ssa.
fn primitive_type_name(typ types.Primitive) string {
	if typ.props.has(.boolean) {
		return 'bool'
	}
	if typ.props.has(.integer) {
		if typ.props.has(.unsigned) {
			return match typ.size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${typ.size}' }
			}
		}
		return match typ.size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${typ.size}' }
		}
	}
	if typ.props.has(.float) {
		return match typ.size {
			32 { 'f32' }
			else { 'f64' }
		}
	}
	return 'int'
}

// ssa_type_from_checker_type converts ssa type from checker type data for ssa.
fn (mut b Builder) ssa_type_from_checker_type(typ types.Type) TypeID {
	if typ is types.Void {
		return b.void_type
	}
	if typ is types.String {
		return b.str_type
	}
	if typ is types.Char {
		return b.i8_type
	}
	if typ is types.Rune {
		return b.i32_type
	}
	if typ is types.ISize || typ is types.USize {
		return b.i64_type
	}
	if typ is types.Primitive {
		return b.resolve_type(primitive_type_name(typ))
	}
	if typ is types.Array {
		return b.array_type
	}
	if typ is types.ArrayFixed {
		return b.m.type_store.get_ptr(b.ssa_type_from_checker_type(typ.elem_type))
	}
	if typ is types.Map {
		return b.map_type
	}
	if typ is types.Pointer {
		return b.m.type_store.get_ptr(b.ssa_type_from_checker_type(typ.base_type))
	}
	if typ is types.OptionType {
		return b.option_type_id(typ.base_type.name())
	}
	if typ is types.ResultType {
		return b.option_type_id(typ.base_type.name())
	}
	if typ is types.Enum {
		return b.i32_type
	}
	if typ is types.MultiReturn {
		return b.register_multi_return_type(typ)
	}
	if typ is types.Alias {
		return b.ssa_type_from_checker_type(typ.base_type)
	}
	return b.resolve_type(typ.name())
}

// register_enum_values updates register enum values state for ssa.
fn (mut b Builder) register_enum_values(node flat.Node, module_name string) {
	short_name := node.value.all_after('.')
	mut enum_names := []string{}
	enum_names << node.value
	if short_name != node.value {
		enum_names << short_name
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		enum_names << module_name + '.' + short_name
	}
	mut is_flag_enum := false
	for enum_name in enum_names {
		if b.checker_has_flag_enum(enum_name) {
			is_flag_enum = true
			break
		}
	}
	for enum_name in enum_names {
		b.enum_types[enum_name] = true
		if is_flag_enum {
			b.flag_enum_types[enum_name] = true
		}
	}
	mut value := 0
	for i in 0 .. node.children_count {
		field := b.a.child_node(&node, i)
		if field.kind != .enum_field {
			continue
		}
		if field.children_count > 0 {
			if explicit := b.enum_field_expr_value(b.a.child(field, 0)) {
				value = explicit
			}
		}
		for enum_name in enum_names {
			enum_key := enum_name + '.' + field.value
			b.enum_values[enum_key] = value
		}
		if existing := b.enum_member_values[field.value] {
			if existing != value {
				b.enum_member_dupes[field.value] = true
			}
		} else {
			b.enum_member_values[field.value] = value
		}
		value++
	}
}

// register_struct_type_name updates register struct type name state for ssa.
fn (mut b Builder) register_struct_type_name(name string, module_name string, typ_id TypeID) {
	b.struct_types[name] = typ_id
	short_name := name.all_after('.')
	if short_name !in b.struct_types {
		b.struct_types[short_name] = typ_id
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		qualified_name := module_name + '.' + short_name
		b.struct_types[qualified_name] = typ_id
	}
}

// register_sum_type_name updates register sum type name state for ssa.
fn (mut b Builder) register_sum_type_name(name string, module_name string, typ_id TypeID) {
	qualified_name := qualify_type_name(name, module_name)
	b.register_struct_type_name(name, module_name, typ_id)
	mut variants := []string{}
	if b.tc != unsafe { nil } {
		if tc_variants := b.tc.sum_types[qualified_name] {
			variants = tc_variants.clone()
		}
	}
	if variants.len == 0 {
		for node in b.a.nodes {
			if node.kind != .type_decl || node.value != name || node.children_count == 0 {
				continue
			}
			for i in 0 .. node.children_count {
				variant := b.a.child_node(&node, i)
				variants << qualify_type_name(variant.value, module_name)
			}
			break
		}
	}
	b.sum_type_variants[qualified_name] = variants
	b.sum_type_canonical[qualified_name] = qualified_name
	if name != qualified_name {
		b.sum_type_variants[name] = variants
		b.sum_type_canonical[name] = qualified_name
	}
	short_name := qualified_name.all_after('.')
	b.sum_type_variants[short_name] = variants
	b.sum_type_canonical[short_name] = qualified_name
}

// qualify_type_name supports qualify type name handling for ssa.
fn qualify_type_name(name string, module_name string) string {
	if name.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' {
		return name
	}
	return '${module_name}.${name}'
}

// qualify_type_ref_name supports qualify type ref name handling for ssa.
fn qualify_type_ref_name(name string, module_name string) string {
	if name.len == 0 {
		return name
	}
	if name.starts_with('&') {
		return '&' + qualify_type_ref_name(name[1..], module_name)
	}
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return name[..1] + qualify_type_ref_name(name[1..], module_name)
	}
	if name.starts_with('[]') {
		return '[]' + qualify_type_ref_name(name[2..], module_name)
	}
	if name.starts_with('[') {
		idx := name.index_u8(`]`)
		if idx > 0 && idx + 1 < name.len {
			return name[..idx + 1] + qualify_type_ref_name(name[idx + 1..], module_name)
		}
	}
	if name.contains('[') && name.ends_with(']') {
		idx := name.index_u8(`[`)
		if idx > 0 && idx < name.len - 1 {
			return qualify_type_ref_name(name[..idx], module_name) + name[idx..]
		}
	}
	if name.starts_with('map[') {
		key_type, val_type := map_type_parts(name)
		if key_type.len > 0 && val_type.len > 0 {
			return 'map[' + qualify_type_ref_name(key_type, module_name) + ']' +
				qualify_type_ref_name(val_type, module_name)
		}
	}
	if type_ref_is_builtin(name) || name.contains('.') || module_name.len == 0
		|| module_name == 'main' || module_name == 'builtin' {
		return name
	}
	return module_name + '.' + name
}

// type_ref_is_builtin returns type ref is builtin data for ssa.
fn type_ref_is_builtin(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'f32',
		'f64', 'bool', 'string', 'void', 'voidptr', 'rune', 'char', 'array', 'map']
}

// sum_type_variants_for_decl supports sum type variants for decl handling for Builder.
fn (b &Builder) sum_type_variants_for_decl(node flat.Node, module_name string) []string {
	mut variants := []string{}
	for i in 0 .. node.children_count {
		variant := b.a.child_node(&node, i).value
		variants << qualify_type_name(variant, module_name)
	}
	return variants
}

// register_sum_field_type updates register sum field type state for ssa.
fn (mut b Builder) register_sum_field_type(name string, module_name string, field_name string, field_type string) {
	short_name := name.all_after('.')
	b.struct_field_types[name + '.' + field_name] = field_type
	b.struct_field_types[short_name + '.' + field_name] = field_type
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		b.struct_field_types[module_name + '.' + short_name + '.' + field_name] = field_type
	}
}

// struct_type_id_for_decl supports struct type id for decl handling for Builder.
fn (b &Builder) struct_type_id_for_decl(name string, module_name string) TypeID {
	short_name := name.all_after('.')
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		qualified_name := module_name + '.' + short_name
		if typ := b.struct_types[qualified_name] {
			return typ
		}
	}
	if typ := b.struct_types[name] {
		return typ
	}
	if typ := b.struct_types[short_name] {
		return typ
	}
	return TypeID(0)
}

// register_consts updates register consts state for ssa.
fn (mut b Builder) register_consts() {
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .const_decl {
			for i in 0 .. node.children_count {
				field_id := b.a.child(&node, i)
				if int(field_id) < 0 {
					continue
				}
				field := b.a.nodes[int(field_id)]
				if field.kind != .const_field || field.children_count == 0 {
					continue
				}
				expr_id := b.a.child(&field, 0)
				if cur_module.len > 0 && cur_module != 'main' {
					b.const_exprs['${cur_module}.${field.value}'] = expr_id
				} else {
					b.const_exprs[field.value] = expr_id
				}
			}
		}
	}
}

// register_globals updates register globals state for ssa.
fn (mut b Builder) register_globals() {
	for node in b.a.nodes {
		if node.kind == .global_decl {
			for i in 0 .. node.children_count {
				f := b.a.child_node(&node, i)
				typ := b.resolve_type(f.typ)
				b.vars[f.value] = b.m.add_global(f.value, typ)
			}
		}
	}
	b.ensure_runtime_global('g_main_argc', b.i64_type)
	b.ensure_runtime_global('g_main_argv',
		b.m.type_store.get_ptr(b.m.type_store.get_ptr(b.i8_type)))
}

// ensure_runtime_global supports ensure runtime global handling for Builder.
fn (mut b Builder) ensure_runtime_global(name string, typ TypeID) {
	if name in b.vars {
		return
	}
	for v in b.m.values {
		if v.kind == .global && v.name == name {
			b.vars[name] = v.id
			return
		}
	}
	b.vars[name] = b.m.add_global(name, typ)
}

// register_functions updates register functions state for ssa.
fn (mut b Builder) register_functions() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1 := []TypeID{}
	mut p2 := []TypeID{}
	mut p3 := []TypeID{}
	// C library externs
	p3 = []TypeID{}
	p3 << b.i64_type
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('write', b.i64_type, p3)
	b.register_extern('read', b.i64_type, p3)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << b.i64_type
	p3 << b.i64_type
	b.register_extern('open', b.i64_type, p3)
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('putchar', b.i64_type, p1)
	b.register_extern('getchar', b.i64_type, []TypeID{})
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('close', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('puts', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('strlen', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('strerror', ptr_i8, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('feof', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('ferror', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('fclose', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('pclose', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('ftell', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('rewind', b.void_type, p1)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('fputs', b.i64_type, p2)
	b.register_extern('stat', b.i64_type, p2)
	b.register_extern('lstat', b.i64_type, p2)
	b.register_extern('statvfs', b.i64_type, p2)
	b.register_extern('rename', b.i64_type, p2)
	b.register_extern('strcmp', b.i64_type, p2)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << b.i64_type
	b.register_extern('strchr', ptr_i8, p2)
	b.register_extern('strrchr', ptr_i8, p2)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('fopen', ptr_i8, p2)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('popen', ptr_i8, p2)
	mut p4 := []TypeID{}
	p4 << ptr_i8
	p4 << b.i64_type
	p4 << b.i64_type
	p4 << ptr_i8
	b.register_extern('fread', b.i64_type, p4)
	p4 = []TypeID{}
	p4 << ptr_i8
	p4 << b.i64_type
	p4 << b.i64_type
	p4 << ptr_i8
	b.register_extern('fwrite', b.i64_type, p4)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << b.i64_type
	p3 << b.i64_type
	b.register_extern('fseek', b.i64_type, p3)
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('malloc', ptr_i8, p1)
	p2 = []TypeID{}
	p2 << b.i64_type
	p2 << b.i64_type
	b.register_extern('calloc', ptr_i8, p2)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << b.i64_type
	b.register_extern('realloc', ptr_i8, p2)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('memcpy', ptr_i8, p3)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('memmove', ptr_i8, p3)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << b.i64_type
	p3 << b.i64_type
	b.register_extern('memset', ptr_i8, p3)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('memcmp', b.i64_type, p3)
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('exit', b.void_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('fflush', b.void_type, p1)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << b.i64_type
	b.register_extern('access', b.i64_type, p2)
	b.register_extern('mkdir', b.i64_type, p2)
	b.register_extern('chmod', b.i64_type, p2)
	b.register_extern('getcwd', ptr_i8, p2)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('remove', b.i64_type, p1)
	b.register_extern('unlink', b.i64_type, p1)
	b.register_extern('rmdir', b.i64_type, p1)
	b.register_extern('chdir', b.i64_type, p1)
	b.register_extern('getenv', ptr_i8, p1)
	b.register_extern('system', b.i64_type, p1)
	b.register_extern('opendir', ptr_i8, p1)
	b.register_extern('closedir', b.i64_type, p1)
	b.register_extern('fileno', b.i64_type, p1)
	b.register_extern('getc', b.i64_type, p1)
	b.register_extern('strdup', ptr_i8, p1)
	b.register_extern('atoi', b.i64_type, p1)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('readlink', b.i64_type, p3)
	b.register_extern('strncmp', b.i64_type, p3)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('readdir', ptr_i8, p1)
	b.register_extern('pipe', b.i64_type, p1)
	p3 = []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('setenv', b.i64_type, p3)
	p3 = []TypeID{}
	p3 << b.i64_type
	p3 << ptr_i8
	p3 << b.i64_type
	b.register_extern('waitpid', b.i64_type, p3)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('realpath', ptr_i8, p2)
	b.register_extern('pthread_mutex_init', b.i64_type, p2)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('pthread_mutex_lock', b.i64_type, p1)
	b.register_extern('pthread_mutex_trylock', b.i64_type, p1)
	b.register_extern('pthread_mutex_unlock', b.i64_type, p1)
	b.register_extern('pthread_mutex_destroy', b.i64_type, p1)
	b.register_extern('pthread_self', b.i64_type, []TypeID{})
	b.register_extern('getpid', b.i64_type, []TypeID{})
	b.register_extern('clock', b.i64_type, []TypeID{})
	b.register_extern('mach_absolute_time', b.i64_type, []TypeID{})
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('mach_timebase_info', b.void_type, p1)
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('clock_gettime_nsec_np', b.i64_type, p1)
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('time', b.i64_type, p1)
	b.register_extern('mktime', b.i64_type, p1)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('localtime_r', ptr_i8, p2)
	b.register_extern('gmtime_r', ptr_i8, p2)
	p4 = []TypeID{}
	p4 << ptr_i8
	p4 << b.i64_type
	p4 << ptr_i8
	p4 << ptr_i8
	b.register_extern('strftime', b.i64_type, p4)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('gettimeofday', b.i64_type, p2)
	p1 = []TypeID{}
	p1 << b.i64_type
	b.register_extern('sleep', b.i64_type, p1)
	b.register_extern('usleep', b.i64_type, p1)
	b.register_extern('dup', b.i64_type, p1)
	b.register_extern('isatty', b.i64_type, p1)
	p2 = []TypeID{}
	p2 << b.i64_type
	p2 << b.i64_type
	b.register_extern('dup2', b.i64_type, p2)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('nanosleep', b.i64_type, p2)
	p2 = []TypeID{}
	p2 << b.i64_type
	p2 << ptr_i8
	b.register_extern('clock_gettime', b.i64_type, p2)
	b.register_basic_format_stubs()
	b.register_string_plus_stubs()
	b.register_bench_runtime_stubs()
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('free', b.void_type, p1)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('fprintf', b.void_type, p2)

	for node in b.a.nodes {
		if node.kind != .c_fn_decl {
			continue
		}
		ret_type := b.resolve_c_decl_type(node.typ)
		mut param_types := []TypeID{}
		for i in 0 .. node.children_count {
			child := b.a.child_node(&node, i)
			if child.kind == .param {
				param_types << b.resolve_c_decl_type(child.typ)
			}
		}
		b.register_extern(node.value, ret_type, param_types)
		if node.value.starts_with('C.') {
			b.register_extern(node.value[2..], ret_type, param_types)
		}
	}

	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .fn_decl {
			if b.skip_source_fn_in_module(node.value, cur_module) {
				continue
			}
			fn_name := ssa_fn_name_in_module(cur_module, node.value)
			if b.used_fns.len > 0 && !b.source_fn_is_used(node.value, cur_module) {
				continue
			}
			ret_type := b.checker_return_type(node.value, cur_module) or {
				b.resolve_type_in_module(node.typ, cur_module)
			}
			mut param_types := []TypeID{}
			mut param_idx := 0
			for i in 0 .. node.children_count {
				child := b.a.child_node(&node, i)
				if child.kind == .param {
					typ_name := if child.typ.starts_with('&') {
						child.typ
					} else {
						b.checker_param_type_name(node.value, cur_module, param_idx) or {
							child.typ
						}
					}
					param_types << b.resolve_type_in_module(typ_name, cur_module)
					param_idx++
				}
			}
			fn_type := b.m.type_store.register(Type{
				kind:     .func_t
				ret_type: ret_type
				params:   param_types
			})
			b.fn_types[fn_name] = fn_type
			func_id := b.m.new_function(fn_name, ret_type)
			b.fn_ids[fn_name] = func_id
		}
	}
	b.register_top_level_main()
	b.register_wyhash_stubs()
	b.register_string_eq_stub()
	b.register_fast_string_eq_stub()
	b.register_string_lt_stub()
	b.register_string_trim_stubs()
	b.register_string_last_part_stubs()
	b.register_ierror_stubs()
	b.register_array_runtime_stubs()
	b.register_arguments_stub()
	b.register_panic_stub()
	b.register_string_builder_stubs()
	b.register_path_runtime_stubs()
	b.register_map_runtime_stubs()
	b.register_u8_runtime_stubs()
	b.register_heap_tracking_stubs()
	b.register_printing_stubs()
	b.register_at_exit_stub()
	b.register_rand_prng_interface_stubs()
	b.register_pthread_compat_stubs()
	b.register_prealloc_atomic_stubs()
	b.register_atomic_builtin_stubs()
	b.register_process_capture_stubs()
	b.register_file_check_stubs()
	b.register_fd_macro_stubs()
	b.register_signal_macro_stubs()
	b.register_os_stat_stubs()
	b.register_array_string_stubs()
	b.register_fixed_array_contains_stubs()
	b.register_array_contains_stubs()
	b.register_enum_autostr_fns()
}

// register_enum_autostr_fns synthesizes a `<Enum>__autostr` helper per enum decl,
// mirroring cgen's enum_str_defs (gen/c/types.v). The transformer rewrites `${enum}`
// interpolation (when no custom `.str()` exists) into a call to this helper, so the
// ARM64/SSA path must provide it just like the C backend does. The body switches on the
// value and returns the matching field NAME, falling back to the integer rendering.
fn (mut b Builder) register_enum_autostr_fns() {
	mut cur_module := ''
	for node in b.a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.enum_decl {
				qualified := if cur_module.len > 0 && cur_module != 'main'
					&& cur_module != 'builtin' {
					'${cur_module}.${node.value}'
				} else {
					node.value
				}
				fn_name := '${qualified.replace('.', '__')}__autostr'
				if fn_name in b.fn_ids {
					continue
				}
				is_flag := b.is_flag_enum_type_name(qualified)
				mut names := []string{}
				mut values := []int{}
				mut val := 0
				for i in 0 .. node.children_count {
					field := b.a.child_node(&node, i)
					if field.kind != .enum_field {
						continue
					}
					if field.children_count > 0 {
						if explicit := b.enum_field_expr_value(b.a.child(field, 0)) {
							val = explicit
						}
					}
					names << field.value
					// Flag enums store one bit per field, matching `enum_value_for_type`.
					values << if is_flag { 1 << val } else { val }
					val++
				}
				mut p1 := []TypeID{}
				p1 << b.i64_type
				fid := b.register_synthetic_function(fn_name, b.str_type, p1)
				if is_flag {
					// `@[flag]` enums stringify as `Name{.a | .b}` (and `Name{}` for 0),
					// matching the C backend, rather than a single field name.
					b.generate_flag_enum_autostr_body(fid, node.value.all_after_last('.'), names,
						values)
				} else {
					b.generate_enum_autostr_body(fid, names, values)
				}
			}
			else {}
		}
	}
}

// enum_field_expr_value evaluates the constant integer value of an enum-field initializer
// expression, mirroring cgen's enum_field_expr_value (gen/c/types.v). Without this the
// autostr helpers above would only honour bare `.int_literal` initializers and leave a
// field like `a = 1 << 3` / `a = -1` at the previous sequential value, so the generated
// `<Enum>__autostr` would compare against the wrong number.
fn (b &Builder) enum_field_expr_value(id flat.NodeId) ?int {
	if int(id) < 0 {
		return none
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return parse_int_literal(node.value)
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return b.enum_field_expr_value(b.a.child(&node, 0))
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := b.enum_field_expr_value(b.a.child(&node, 0))?
			return match node.op {
				.plus { value }
				.minus { -value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := b.enum_field_expr_value(b.a.child(&node, 0))?
			right := b.enum_field_expr_value(b.a.child(&node, 1))?
			return match node.op {
				.plus {
					left + right
				}
				.minus {
					left - right
				}
				.mul {
					left * right
				}
				.div {
					if right == 0 {
						none
					} else {
						left / right
					}
				}
				.mod {
					if right == 0 {
						none
					} else {
						left % right
					}
				}
				.amp {
					left & right
				}
				.pipe {
					left | right
				}
				.xor {
					left ^ right
				}
				.left_shift {
					int(u64(left) << right)
				}
				.right_shift, .right_shift_unsigned {
					left >> right
				}
				else {
					none
				}
			}
		}
		else {
			return none
		}
	}
}

// generate_enum_autostr_body emits the body of a `<Enum>__autostr` helper: a chain of
// equality tests returning each field's name as a string literal, with an `int_str`
// fallback for out-of-range (or combined-flag) values.
fn (mut b Builder) generate_enum_autostr_body(func_id int, field_names []string, field_values []int) {
	entry := b.m.add_block(func_id, 'entry')
	it := b.func_add_argument(func_id, b.i64_type, 'it')
	mut cur_block := entry
	mut seen := map[int]bool{}
	for i, fname in field_names {
		val := field_values[i]
		// Duplicate values would create unreachable cases; keep the first name.
		if val in seen {
			continue
		}
		seen[val] = true
		const_val := b.m.get_or_add_const(b.i64_type, val.str())
		cmp := b.block_instr2(.eq, cur_block, b.i1_type, it, const_val)
		match_block := b.m.add_block(func_id, 'autostr_match')
		next_block := b.m.add_block(func_id, 'autostr_next')
		b.block_instr3(.br, cur_block, b.void_type, cmp, ValueID(match_block), ValueID(next_block))
		name_val := b.m.add_value(.string_literal, b.str_type, fname, 0)
		b.block_instr1(.ret, match_block, b.void_type, name_val)
		cur_block = next_block
	}
	int_str_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.fn_ids['int_str'])
	default_val := b.block_instr2(.call, cur_block, b.str_type, int_str_ref, it)
	b.block_instr1(.ret, cur_block, b.void_type, default_val)
}

// generate_flag_enum_autostr_body emits the body of a `<FlagEnum>__autostr` helper that
// renders a `@[flag]` value as `Name{.a | .b}` (and `Name{}` for 0), mirroring cgen's
// emit_flag_enum_autostr: start with `Name{`, append `.field` for every set bit (joined
// by ` | `), then append `}`. `field_values` holds each field's bit (1 << index).
fn (mut b Builder) generate_flag_enum_autostr_body(func_id int, short_name string, field_names []string, field_values []int) {
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i1 := b.m.type_store.get_ptr(b.i1_type)
	entry := b.m.add_block(func_id, 'entry')
	it := b.func_add_argument(func_id, b.i64_type, 'it')

	// res = "Name{"; first = true. Both live in stack slots so they can be updated
	// across the per-field branches (this body uses plain control flow, not phis).
	res_slot := b.block_instr0(.alloca, entry, ptr_string)
	first_slot := b.block_instr0(.alloca, entry, ptr_i1)
	open_lit := b.m.add_value(.string_literal, b.str_type, '${short_name}{', 0)
	b.block_instr2(.store, entry, b.void_type, open_lit, res_slot)
	true_const := b.m.get_or_add_const(b.i1_type, '1')
	b.block_instr2(.store, entry, b.void_type, true_const, first_slot)
	false_const := b.m.get_or_add_const(b.i1_type, '0')
	plus_ref := b.m.add_value(.func_ref, b.str_type, 'string__plus', b.fn_ids['string__plus'])

	mut cur_block := entry
	mut seen := map[int]bool{}
	for i, fname in field_names {
		bit := field_values[i]
		// A zero bit can never be "set" (matches the cgen `bit != 0` guard); skip it.
		if bit == 0 || bit in seen {
			continue
		}
		seen[bit] = true
		bit_const := b.m.get_or_add_const(b.i64_type, bit.str())
		masked := b.block_instr2(.and_, cur_block, b.i64_type, it, bit_const)
		is_set := b.block_instr2(.eq, cur_block, b.i1_type, masked, bit_const)
		append_block := b.m.add_block(func_id, 'fe_append')
		after_block := b.m.add_block(func_id, 'fe_after')
		b.block_instr3(.br, cur_block, b.void_type, is_set, ValueID(append_block),
			ValueID(after_block))

		// append_block: if not first, prepend the ` | ` separator first.
		first := b.block_instr1(.load, append_block, b.i1_type, first_slot)
		sep_block := b.m.add_block(func_id, 'fe_sep')
		name_block := b.m.add_block(func_id, 'fe_name')
		b.block_instr3(.br, append_block, b.void_type, first, ValueID(name_block),
			ValueID(sep_block))

		sep_res := b.block_instr1(.load, sep_block, b.str_type, res_slot)
		sep_lit := b.m.add_value(.string_literal, b.str_type, ' | ', 0)
		sep_joined := b.block_instr3(.call, sep_block, b.str_type, plus_ref, sep_res, sep_lit)
		b.block_instr2(.store, sep_block, b.void_type, sep_joined, res_slot)
		b.block_instr1(.jmp, sep_block, b.void_type, ValueID(name_block))

		name_res := b.block_instr1(.load, name_block, b.str_type, res_slot)
		name_lit := b.m.add_value(.string_literal, b.str_type, '.${fname}', 0)
		name_joined := b.block_instr3(.call, name_block, b.str_type, plus_ref, name_res, name_lit)
		b.block_instr2(.store, name_block, b.void_type, name_joined, res_slot)
		b.block_instr2(.store, name_block, b.void_type, false_const, first_slot)
		b.block_instr1(.jmp, name_block, b.void_type, ValueID(after_block))

		cur_block = after_block
	}

	close_res := b.block_instr1(.load, cur_block, b.str_type, res_slot)
	close_lit := b.m.add_value(.string_literal, b.str_type, '}', 0)
	final_res := b.block_instr3(.call, cur_block, b.str_type, plus_ref, close_res, close_lit)
	b.block_instr1(.ret, cur_block, b.void_type, final_res)
}

// ssa_fn_name_in_module supports ssa fn name in module handling for ssa.
fn ssa_fn_name_in_module(module_name string, name string) string {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return module_name + '.' + name
	}
	return name
}

// register_extern updates register extern state for ssa.
fn (mut b Builder) register_extern(name string, ret TypeID, params []TypeID) {
	fn_type := b.m.type_store.register(Type{
		kind:     .func_t
		ret_type: ret
		params:   params
	})
	extern_name := 'C.${name}'
	func_id := b.m.new_function(extern_name, ret)
	b.fn_types[name] = fn_type
	b.fn_ids[name] = func_id
	b.fn_types[extern_name] = fn_type
	b.c_fn_types[name] = fn_type
	b.c_fn_ids[name] = func_id
	mut f := b.m.funcs[func_id]
	f.is_c_extern = true
	f.is_prototype = true
	f.linkage = .external
	b.m.funcs[func_id] = f
}

// register_runtime_extern updates register runtime extern state for ssa.
fn (mut b Builder) register_runtime_extern(name string, ret TypeID, params []TypeID) {
	if b.has_fn_decl(name) {
		return
	}
	b.register_extern(name, ret, params)
}

// has_fn_decl reports whether has fn decl applies in ssa.
fn (b &Builder) has_fn_decl(name string) bool {
	for node in b.a.nodes {
		if node.kind == .fn_decl && node.value == name {
			return true
		}
	}
	return false
}

// skip_source_fn supports skip source fn handling for Builder.
fn (b &Builder) skip_source_fn(name string) bool {
	if name in bench_runtime_stub_names {
		return true
	}
	if name in ['_wymix', 'wyhash', 'wyhash64', 'string__eq', 'string__lt', 'array_new', 'array_get',
		'string__plus', 'string_plus_many', 'string.trim_right', 'array_push', 'array_push_many',
		'array.push_many', 'array_clone', 'panic', 'fast_string_eq', 'strings.new_builder',
		'strings.Builder.write_string', 'strings.Builder.writeln', 'strings.Builder.str',
		'strings.Builder.write_ptr', 'strings.Builder.write_u8', 'strings.Builder.write_runes',
		'strings.Builder.free', 'strings.Builder.last_n', 'Builder.write_string', 'Builder.writeln',
		'Builder.str', 'Builder.write_ptr', 'Builder.write_u8', 'Builder.write_runes', 'Builder.free',
		'Builder.last_n', 'new_map', 'map__set', 'map__get', 'map__exists', 'map__get_check',
		'map__get_or_set', 'map__delete', 'map__clear', 'map__clone', 'map__move', 'map__free',
		'map__reserve', 'map__keys', 'map__values', 'v3_map_find', 'v3_map_set_sized', 'u8.is_digit',
		'u8.is_letter', 'u8.is_alnum', 'u8.is_capital', 'bytestr', '[]u8.bytestr', '[]u8.hex',
		'[]rune.string', 'Array_u8__bytestr', 'Array_u8__hex', 'Array_rune__string',
		'array.repeat_to_depth', 'string.all_before_last', 'string__all_before_last',
		'all_before_last', 'string.all_after_last', 'string__all_after_last', 'all_after_last',
		'_ht_alloc', '_ht_free', 'f32_to_str_l', 'f32_to_str_l_with_dot', 'f64_to_str_l',
		'f64_to_str_l_with_dot', 'print', 'println', 'eprint', 'eprintln', 'arguments', 'at_exit',
		'tos2', 'tos3', 'tos_clone', 'v_prealloc_atomic_add_i32', 'v_prealloc_atomic_load_i32',
		'v_prealloc_atomic_store_i32', 'v_prealloc_atomic_cas_i32', 'FD_ZERO', 'FD_SET', 'FD_ISSET',
		'v_signal_with_handler_cast', 'normalize_path_in_builder', 'check_fwrite', 'check_fread',
		'os.check_fwrite', 'os.check_fread', 'array_eq_raw', 'array_eq_string', 'array_eq_array',
		'fxx_to_str_l_parse', 'fxx_to_str_l_parse_with_dot', 'u8.vstring', 'u8.vstring_with_len',
		'char.vstring', 'char.vstring_with_len', 'byteptr.vstring', 'byteptr.vstring_with_len',
		'charptr.vstring', 'charptr.vstring_with_len', 'u8.vstring_literal',
		'u8.vstring_literal_with_len', 'char.vstring_literal', 'char.vstring_literal_with_len',
		'byteptr.vstring_literal', 'byteptr.vstring_literal_with_len', 'charptr.vstring_literal',
		'charptr.vstring_literal_with_len'] {
		return true
	}
	return name.starts_with('print_backtrace') || name.starts_with('backtrace_')
		|| name.starts_with('map.')
		|| name in ['print_libbacktrace', 'eprint_libbacktrace', 'bsd_backtrace_resolve_atos']
}

// skip_source_fn_in_module supports skip source fn in module handling for Builder.
fn (b &Builder) skip_source_fn_in_module(name string, module_name string) bool {
	if module_name == 'builtin' && name in b.c_fn_ids {
		return true
	}
	if module_name == 'ast' && name in ['Expr.name', 'SelectorExpr.name', '[]Expr.name_list'] {
		return true
	}
	if module_name == 'c' && name.starts_with('Gen.') {
		return true
	}
	if module_name == 'os' && name in ['is_dir', 'is_link', 'ls'] {
		return true
	}
	return b.skip_source_fn(name)
}

// register_top_level_main updates register top level main state for ssa.
fn (mut b Builder) register_top_level_main() {
	if 'main' in b.fn_ids || !b.has_top_level_stmts() {
		return
	}
	fn_type := b.m.type_store.register(Type{
		kind:     .func_t
		ret_type: b.void_type
	})
	b.fn_types['main'] = fn_type
	b.fn_ids['main'] = b.m.new_function('main', b.void_type)
	b.top_level_main = true
}

// has_top_level_stmts reports whether has top level stmts applies in ssa.
fn (b &Builder) has_top_level_stmts() bool {
	return b.top_level_stmt_ids().len > 0
}

// top_level_stmt_ids supports top level stmt ids handling for Builder.
fn (b &Builder) top_level_stmt_ids() []flat.NodeId {
	mut ids := []flat.NodeId{}
	for file_idx, file_node in b.a.nodes {
		if file_idx < b.a.user_code_start || file_node.kind != .file
			|| file_node.children_count == 0 {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := b.a.child(&file_node, i)
			if int(child_id) < b.a.user_code_start {
				continue
			}
			child := b.a.nodes[int(child_id)]
			if b.is_top_level_stmt(child) {
				ids << child_id
			}
		}
	}
	return ids
}

// is_top_level_stmt reports whether is top level stmt applies in ssa.
fn (b &Builder) is_top_level_stmt(node flat.Node) bool {
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt,
		.for_in_stmt, .if_expr, .assert_stmt, .defer_stmt, .block {
			true
		}
		else {
			false
		}
	}
}

// register_wyhash_stubs updates register wyhash stubs state for ssa.
fn (mut b Builder) register_wyhash_stubs() {
	mut p2 := []TypeID{}
	p2 << b.i64_type
	p2 << b.i64_type
	wymix_id := b.register_synthetic_function('_wymix', b.i64_type, p2)
	b.generate_wymix_body(wymix_id)
	wyhash64_id := b.register_synthetic_function('wyhash64', b.i64_type, p2)
	b.generate_wyhash64_body(wyhash64_id)

	ptr_u8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)
	mut p4 := []TypeID{}
	p4 << ptr_u8
	p4 << b.i64_type
	p4 << b.i64_type
	p4 << ptr_u64
	wyhash_id := b.register_synthetic_function('wyhash', b.i64_type, p4)
	b.generate_wyhash_body(wyhash_id)
}

// register_synthetic_function updates register synthetic function state for ssa.
fn (mut b Builder) register_synthetic_function(name string, ret TypeID, params []TypeID) int {
	fn_type := b.m.type_store.register(Type{
		kind:     .func_t
		ret_type: ret
		params:   params
	})
	b.fn_types[name] = fn_type
	func_id := b.m.new_function(name, ret)
	b.fn_ids[name] = func_id
	mut f := b.m.funcs[func_id]
	f.typ = ret
	f.blocks = []BlockID{}
	f.params = []ValueID{}
	f.is_c_extern = false
	b.m.funcs[func_id] = f
	return func_id
}

fn (mut b Builder) register_synthetic_c_function(name string, ret TypeID, params []TypeID) int {
	func_id := b.register_synthetic_function(name, ret, params)
	fn_type := b.fn_types[name]
	extern_name := 'C.${name}'
	b.fn_ids[extern_name] = func_id
	b.fn_types[extern_name] = fn_type
	b.c_fn_ids[name] = func_id
	b.c_fn_types[name] = fn_type
	return func_id
}

// func_add_argument supports func add argument handling for Builder.
fn (mut b Builder) func_add_argument(func_id int, typ TypeID, name string) ValueID {
	mut f := b.m.funcs[func_id]
	param := b.m.add_value(.argument, typ, name, f.params.len)
	f.params << param
	b.m.funcs[func_id] = f
	return param
}

// block_instr0 supports block instr0 handling for Builder.
fn (mut b Builder) block_instr0(op OpCode, block_id BlockID, typ TypeID) ValueID {
	ops := []ValueID{}
	return b.m.add_instr(op, block_id, typ, ops)
}

// block_instr1 supports block instr1 handling for Builder.
fn (mut b Builder) block_instr1(op OpCode, block_id BlockID, typ TypeID, a ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	return b.m.add_instr(op, block_id, typ, ops)
}

// block_instr2 supports block instr2 handling for Builder.
fn (mut b Builder) block_instr2(op OpCode, block_id BlockID, typ TypeID, a ValueID, c ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	ops << c
	return b.m.add_instr(op, block_id, typ, ops)
}

// block_instr3 supports block instr3 handling for Builder.
fn (mut b Builder) block_instr3(op OpCode, block_id BlockID, typ TypeID, a ValueID, c ValueID, d ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	ops << c
	ops << d
	return b.m.add_instr(op, block_id, typ, ops)
}

// block_instr4 supports block instr4 handling for Builder.
fn (mut b Builder) block_instr4(op OpCode, block_id BlockID, typ TypeID, a ValueID, c ValueID, d ValueID, e ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	ops << c
	ops << d
	ops << e
	return b.m.add_instr(op, block_id, typ, ops)
}

// block_struct_field_ptr supports block struct field ptr handling for Builder.
fn (mut b Builder) block_struct_field_ptr(block_id BlockID, base_addr ValueID, typ_id TypeID, field_idx int) ValueID {
	mut builtin_field_type := TypeID(0)
	mut builtin_offset := 0
	mut has_builtin_layout := false
	if typ_id == b.str_type {
		has_builtin_layout = true
		if field_idx == 0 {
			builtin_field_type = b.m.type_store.get_ptr(b.i8_type)
			builtin_offset = 0
		} else if field_idx == 1 {
			builtin_field_type = b.i32_type
			builtin_offset = 8
		} else if field_idx == 2 {
			// is_lit (i32) shares the trailing word with len; it sits at offset 12,
			// matching `struct_field_offset` used by the field readers.
			builtin_field_type = b.i32_type
			builtin_offset = 12
		} else {
			builtin_field_type = b.i64_type
			builtin_offset = field_idx * 8
		}
	} else if typ_id == b.array_type {
		has_builtin_layout = true
		if field_idx == 0 {
			builtin_field_type = b.m.type_store.get_ptr(b.i8_type)
			builtin_offset = 0
		} else {
			builtin_field_type = b.i32_type
			builtin_offset = 8 + (field_idx - 1) * 4
		}
	} else if typ_id == b.map_state_type {
		has_builtin_layout = true
		if field_idx == 0 || field_idx == 1 {
			builtin_field_type = b.m.type_store.get_ptr(b.i8_type)
		} else {
			builtin_field_type = b.i64_type
		}
		builtin_offset = field_idx * 8
	} else if typ_id == b.map_type {
		has_builtin_layout = true
		builtin_field_type = b.m.type_store.get_ptr(b.map_state_type)
		builtin_offset = 0
	}
	if has_builtin_layout {
		off_const := b.m.get_or_add_const(b.i64_type, '${builtin_offset}')
		return b.block_instr2(.get_element_ptr, block_id,
			b.m.type_store.get_ptr(builtin_field_type), base_addr, off_const)
	}
	typ := b.m.type_store.types[typ_id]
	field_type := if field_idx < typ.fields.len { typ.fields[field_idx] } else { b.i64_type }
	offset := b.m.struct_field_offset(typ_id, field_idx)
	off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
	return b.block_instr2(.get_element_ptr, block_id, b.m.type_store.get_ptr(field_type),
		base_addr, off_const)
}

// block_load_array_int_field supports block load array int field handling for Builder.
fn (mut b Builder) block_load_array_int_field(block_id BlockID, arr_ptr ValueID, field_idx int) ValueID {
	field_ptr := b.block_struct_field_ptr(block_id, arr_ptr, b.array_type, field_idx)
	field32 := b.block_instr1(.load, block_id, b.i32_type, field_ptr)
	return b.block_instr1(.zext, block_id, b.i64_type, field32)
}

// register_basic_format_stubs converts register basic format stubs data for ssa.
fn (mut b Builder) register_basic_format_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_i8
	tos2_id := b.register_synthetic_function('tos2', b.str_type, p1_ptr)
	b.generate_tos2_body(tos2_id, 0)
	tos3_id := b.register_synthetic_function('tos3', b.str_type, p1_ptr)
	b.generate_tos2_body(tos3_id, 0)
	tos_clone_id := b.register_synthetic_function('tos_clone', b.str_type, p1_ptr)
	b.generate_tos_clone_body(tos_clone_id)
	b.register_pointer_string_stubs()

	mut p1_i64 := []TypeID{}
	p1_i64 << b.i64_type
	int_str_id := b.register_synthetic_function('int_str', b.str_type, p1_i64)
	b.generate_int_format_body(int_str_id, true, false)

	mut p1_i1 := []TypeID{}
	p1_i1 << b.i1_type
	bool_str_id := b.register_synthetic_function('bool_str', b.str_type, p1_i1)
	b.generate_bool_str_body(bool_str_id)

	mut p1_string := []TypeID{}
	p1_string << b.str_type
	string_int_id := b.register_synthetic_function('string.int', b.i64_type, p1_string)
	b.generate_string_int_body(string_int_id)
	string_int_c_id := b.register_synthetic_function('string__int', b.i64_type, p1_string)
	b.generate_string_int_body(string_int_c_id)

	mut p2_i64 := []TypeID{}
	p2_i64 << b.i64_type
	p2_i64 << b.i64_type
	format_int_id := b.register_synthetic_function('strconv__format_int', b.str_type, p2_i64)
	b.generate_int_format_body(format_int_id, true, true)
	format_uint_id := b.register_synthetic_function('strconv__format_uint', b.str_type, p2_i64)
	b.generate_int_format_body(format_uint_id, false, true)

	b.register_v3_string_format_stubs()

	for name in ['strconv__f32_to_str_l', 'strconv__f32_to_str_l_with_dot', 'f32_to_str_l',
		'f32_to_str_l_with_dot', 'strconv__f64_to_str_l', 'strconv__f64_to_str_l_with_dot',
		'f64_to_str_l', 'f64_to_str_l_with_dot'] {
		float_str_id := b.register_synthetic_function(name, b.str_type, p1_i64)
		b.generate_const_string_body(float_str_id, '0.0')
	}
}

// register_v3_string_format_stubs registers helper calls emitted by the string-interpolation
// transformer. The C backend provides these as inline C helpers; the SSA/native path needs
// synthetic functions so self-hosted ARM64 builds can lower the transformed calls.
fn (mut b Builder) register_v3_string_format_stubs() {
	mut p_pad := []TypeID{}
	p_pad << b.str_type
	p_pad << b.i32_type
	p_pad << b.i32_type
	pad_id := b.register_synthetic_function('v3_string_pad', b.str_type, p_pad)
	b.generate_string_pad_body(pad_id)

	mut p_char := []TypeID{}
	p_char << b.i32_type
	char_id := b.register_synthetic_function('v3_char_string', b.str_type, p_char)
	b.generate_const_string_body(char_id, '?')

	mut p_f64 := []TypeID{}
	p_f64 << b.f64_type
	p_f64 << b.i32_type
	fixed_id := b.register_synthetic_function('v3_f64_fixed', b.str_type, p_f64)
	b.generate_const_string_body(fixed_id, '0.0')

	mut p_int := []TypeID{}
	p_int << b.i32_type
	p_int << b.i32_type
	int_zpad_id := b.register_synthetic_function('v3_int_zpad', b.str_type, p_int)
	b.generate_int_zpad_passthrough_body(int_zpad_id, b.i32_type)

	mut p_i64 := []TypeID{}
	p_i64 << b.i64_type
	p_i64 << b.i32_type
	i64_zpad_id := b.register_synthetic_function('v3_i64_zpad', b.str_type, p_i64)
	b.generate_int_zpad_passthrough_body(i64_zpad_id, b.i64_type)

	mut p_u64 := []TypeID{}
	p_u64 << b.u64_type
	p_u64 << b.i32_type
	u64_zpad_id := b.register_synthetic_function('v3_u64_zpad', b.str_type, p_u64)
	b.generate_const_string_body(u64_zpad_id, '0')
}

// register_pointer_string_stubs updates register pointer string stubs state for ssa.
fn (mut b Builder) register_pointer_string_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_i8
	for name in ['u8.vstring', 'char.vstring', 'byteptr.vstring', 'charptr.vstring',
		'u8.vstring_literal', 'char.vstring_literal', 'byteptr.vstring_literal',
		'charptr.vstring_literal'] {
		func_id := b.register_synthetic_function(name, b.str_type, p1_ptr)
		// The `_literal` wrappers borrow static C data (is_lit=1, never freed); the plain
		// ones return a non-copying view of a runtime buffer (is_lit=0), matching V.
		b.generate_tos2_body(func_id, if name.contains('_literal') { 1 } else { 0 })
	}

	mut p2_ptr_len := []TypeID{}
	p2_ptr_len << ptr_i8
	p2_ptr_len << b.i64_type
	for name in ['u8.vstring_with_len', 'char.vstring_with_len', 'byteptr.vstring_with_len',
		'charptr.vstring_with_len', 'u8.vstring_literal_with_len', 'char.vstring_literal_with_len',
		'byteptr.vstring_literal_with_len', 'charptr.vstring_literal_with_len'] {
		func_id := b.register_synthetic_function(name, b.str_type, p2_ptr_len)
		b.generate_vstring_with_len_body(func_id, if name.contains('_literal') { 1 } else { 0 })
	}
}

// generate_const_string_body supports generate const string body handling for Builder.
fn (mut b Builder) generate_const_string_body(func_id int, value string) {
	entry := b.m.add_block(func_id, 'entry')
	result := b.m.add_value(.string_literal, b.str_type, value, 0)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_bool_str_body supports generate bool str body handling for Builder.
fn (mut b Builder) generate_bool_str_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	value := b.func_add_argument(func_id, b.i1_type, 'value')
	true_block := b.m.add_block(func_id, 'bool_str_true')
	false_block := b.m.add_block(func_id, 'bool_str_false')
	b.block_instr3(.br, entry, b.void_type, value, ValueID(true_block), ValueID(false_block))
	true_val := b.m.add_value(.string_literal, b.str_type, 'true', 0)
	false_val := b.m.add_value(.string_literal, b.str_type, 'false', 0)
	b.block_instr1(.ret, true_block, b.void_type, true_val)
	b.block_instr1(.ret, false_block, b.void_type, false_val)
}

// generate_int_format_body converts generate int format body data for ssa.
fn (mut b Builder) generate_int_format_body(func_id int, is_signed bool, has_radix bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)

	entry := b.m.add_block(func_id, 'entry')
	value := b.func_add_argument(func_id, b.i64_type, 'value')
	if has_radix {
		_ := b.func_add_argument(func_id, b.i64_type, 'radix')
	}

	alloca_n := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_len := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_pos := b.block_instr0(.alloca, entry, ptr_ptr_i8)
	alloca_neg := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.i1_type))

	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	ten := b.m.get_or_add_const(b.i64_type, '10')
	buf_size := b.m.get_or_add_const(b.i64_type, '32')
	last_off := b.m.get_or_add_const(b.i64_type, '31')
	minus_one := b.m.get_or_add_const(b.i64_type, '-1')
	ascii_zero := b.m.get_or_add_const(b.i64_type, '48')
	ascii_minus := b.m.get_or_add_const(b.i64_type, '45')
	false_val := b.m.get_or_add_const(b.i1_type, '0')
	true_val := b.m.get_or_add_const(b.i1_type, '1')

	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	buf := b.block_instr2(.call, entry, ptr_i8, malloc_ref, buf_size)
	end := b.block_instr2(.add, entry, ptr_i8, buf, last_off)
	nul := b.m.get_or_add_const(b.i8_type, '0')
	b.block_instr2(.store, entry, b.void_type, nul, end)
	b.block_instr2(.store, entry, b.void_type, end, alloca_pos)
	b.block_instr2(.store, entry, b.void_type, zero, alloca_len)
	b.block_instr2(.store, entry, b.void_type, false_val, alloca_neg)

	init_block := b.m.add_block(func_id, 'int_str_init')
	neg_block := b.m.add_block(func_id, 'int_str_neg')
	pos_block := b.m.add_block(func_id, 'int_str_pos')
	if is_signed {
		is_neg := b.block_instr2(.lt, entry, b.i1_type, value, zero)
		b.block_instr3(.br, entry, b.void_type, is_neg, ValueID(neg_block), ValueID(pos_block))
		abs_value := b.block_instr2(.sub, neg_block, b.i64_type, zero, value)
		b.block_instr2(.store, neg_block, b.void_type, abs_value, alloca_n)
		b.block_instr2(.store, neg_block, b.void_type, true_val, alloca_neg)
		b.block_instr1(.jmp, neg_block, b.void_type, ValueID(init_block))
		b.block_instr2(.store, pos_block, b.void_type, value, alloca_n)
		b.block_instr1(.jmp, pos_block, b.void_type, ValueID(init_block))
	} else {
		b.block_instr2(.store, entry, b.void_type, value, alloca_n)
		b.block_instr1(.jmp, entry, b.void_type, ValueID(init_block))
	}

	zero_digit_block := b.m.add_block(func_id, 'int_str_zero')
	loop_block := b.m.add_block(func_id, 'int_str_loop')
	sign_block := b.m.add_block(func_id, 'int_str_sign')
	sign_write_block := b.m.add_block(func_id, 'int_str_sign_write')
	done_block := b.m.add_block(func_id, 'int_str_done')

	n_init := b.block_instr1(.load, init_block, b.i64_type, alloca_n)
	is_zero := b.block_instr2(.eq, init_block, b.i1_type, n_init, zero)
	b.block_instr3(.br, init_block, b.void_type, is_zero, ValueID(zero_digit_block),
		ValueID(loop_block))

	pos_zero := b.block_instr1(.load, zero_digit_block, ptr_i8, alloca_pos)
	pos_zero_next := b.block_instr2(.add, zero_digit_block, ptr_i8, pos_zero, minus_one)
	b.block_instr2(.store, zero_digit_block, b.void_type, ascii_zero, pos_zero_next)
	b.block_instr2(.store, zero_digit_block, b.void_type, pos_zero_next, alloca_pos)
	b.block_instr2(.store, zero_digit_block, b.void_type, one, alloca_len)
	b.block_instr1(.jmp, zero_digit_block, b.void_type, ValueID(sign_block))

	n_cur := b.block_instr1(.load, loop_block, b.i64_type, alloca_n)
	rem := b.block_instr2(.srem, loop_block, b.i64_type, n_cur, ten)
	digit := b.block_instr2(.add, loop_block, b.i64_type, rem, ascii_zero)
	pos := b.block_instr1(.load, loop_block, ptr_i8, alloca_pos)
	pos_next := b.block_instr2(.add, loop_block, ptr_i8, pos, minus_one)
	b.block_instr2(.store, loop_block, b.void_type, digit, pos_next)
	b.block_instr2(.store, loop_block, b.void_type, pos_next, alloca_pos)
	old_len := b.block_instr1(.load, loop_block, b.i64_type, alloca_len)
	new_len := b.block_instr2(.add, loop_block, b.i64_type, old_len, one)
	b.block_instr2(.store, loop_block, b.void_type, new_len, alloca_len)
	quot := b.block_instr2(.sdiv, loop_block, b.i64_type, n_cur, ten)
	b.block_instr2(.store, loop_block, b.void_type, quot, alloca_n)
	has_more := b.block_instr2(.gt, loop_block, b.i1_type, quot, zero)
	b.block_instr3(.br, loop_block, b.void_type, has_more, ValueID(loop_block), ValueID(sign_block))

	neg_flag := b.block_instr1(.load, sign_block, b.i1_type, alloca_neg)
	b.block_instr3(.br, sign_block, b.void_type, neg_flag, ValueID(sign_write_block),
		ValueID(done_block))

	pos_sign := b.block_instr1(.load, sign_write_block, ptr_i8, alloca_pos)
	pos_sign_next := b.block_instr2(.add, sign_write_block, ptr_i8, pos_sign, minus_one)
	b.block_instr2(.store, sign_write_block, b.void_type, ascii_minus, pos_sign_next)
	b.block_instr2(.store, sign_write_block, b.void_type, pos_sign_next, alloca_pos)
	len_sign := b.block_instr1(.load, sign_write_block, b.i64_type, alloca_len)
	len_sign_next := b.block_instr2(.add, sign_write_block, b.i64_type, len_sign, one)
	b.block_instr2(.store, sign_write_block, b.void_type, len_sign_next, alloca_len)
	b.block_instr1(.jmp, sign_write_block, b.void_type, ValueID(done_block))

	data := b.block_instr1(.load, done_block, ptr_i8, alloca_pos)
	len := b.block_instr1(.load, done_block, b.i64_type, alloca_len)
	// The digits were written backwards, so `data` points into the middle of `buf`.
	// Shift them to the allocation start (memmove handles the overlap) and NUL-terminate,
	// so the returned owned string's `str` is `buf` itself and free() gets a valid pointer.
	memmove_ref := b.m.add_value(.func_ref, b.void_type, 'memmove', b.fn_ids['memmove'])
	b.block_instr4(.call, done_block, ptr_i8, memmove_ref, buf, data, len)
	buf_term := b.block_instr2(.add, done_block, ptr_i8, buf, len)
	b.block_instr2(.store, done_block, b.void_type, nul, buf_term)
	result := b.emit_make_string(done_block, buf, len, 0)
	b.block_instr1(.ret, done_block, b.void_type, result)
}

// generate_string_int_body supports generate string int body handling for Builder.
fn (mut b Builder) generate_string_int_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, b.str_type, 's')
	s_slot := b.block_instr0(.alloca, entry, ptr_string)
	result_slot := b.block_instr0(.alloca, entry, ptr_i64)
	i_slot := b.block_instr0(.alloca, entry, ptr_i64)
	sign_slot := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, s, s_slot)

	data_ptr := b.block_struct_field_ptr(entry, s_slot, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, s_slot, b.str_type, 1)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)

	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	minus_one := b.m.get_or_add_const(b.i64_type, '-1')
	ten64 := b.m.get_or_add_const(b.i64_type, '10')
	ascii_zero := b.m.get_or_add_const(b.i64_type, '48')
	ascii_minus := b.m.get_or_add_const(b.i8_type, '45')
	b.block_instr2(.store, entry, b.void_type, zero64, result_slot)
	b.block_instr2(.store, entry, b.void_type, zero64, i_slot)
	b.block_instr2(.store, entry, b.void_type, one64, sign_slot)

	check_sign := b.m.add_block(func_id, 'string_int_check_sign')
	sign_body := b.m.add_block(func_id, 'string_int_sign_body')
	loop := b.m.add_block(func_id, 'string_int_loop')
	body := b.m.add_block(func_id, 'string_int_body')
	done := b.m.add_block(func_id, 'string_int_done')
	has_chars := b.block_instr2(.gt, entry, b.i1_type, len, zero64)
	b.block_instr3(.br, entry, b.void_type, has_chars, ValueID(check_sign), ValueID(done))

	first_ch := b.block_instr1(.load, check_sign, b.i8_type, data)
	is_minus := b.block_instr2(.eq, check_sign, b.i1_type, first_ch, ascii_minus)
	b.block_instr3(.br, check_sign, b.void_type, is_minus, ValueID(sign_body), ValueID(loop))

	b.block_instr2(.store, sign_body, b.void_type, minus_one, sign_slot)
	b.block_instr2(.store, sign_body, b.void_type, one64, i_slot)
	b.block_instr1(.jmp, sign_body, b.void_type, ValueID(loop))

	i := b.block_instr1(.load, loop, b.i64_type, i_slot)
	more := b.block_instr2(.lt, loop, b.i1_type, i, len)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))

	body_i := b.block_instr1(.load, body, b.i64_type, i_slot)
	ch_ptr := b.block_instr2(.add, body, ptr_i8, data, body_i)
	ch := b.block_instr1(.load, body, b.i8_type, ch_ptr)
	ch64 := b.block_instr1(.zext, body, b.i64_type, ch)
	digit := b.block_instr2(.sub, body, b.i64_type, ch64, ascii_zero)
	cur := b.block_instr1(.load, body, b.i64_type, result_slot)
	scaled := b.block_instr2(.mul, body, b.i64_type, cur, ten64)
	next := b.block_instr2(.add, body, b.i64_type, scaled, digit)
	b.block_instr2(.store, body, b.void_type, next, result_slot)
	next_i := b.block_instr2(.add, body, b.i64_type, body_i, one64)
	b.block_instr2(.store, body, b.void_type, next_i, i_slot)
	b.block_instr1(.jmp, body, b.void_type, ValueID(loop))

	parsed := b.block_instr1(.load, done, b.i64_type, result_slot)
	sign := b.block_instr1(.load, done, b.i64_type, sign_slot)
	signed := b.block_instr2(.mul, done, b.i64_type, parsed, sign)
	b.block_instr1(.ret, done, b.void_type, signed)
}

// generate_string_pad_body implements the v3 string-interpolation padding helper for SSA.
fn (mut b Builder) generate_string_pad_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'string_pad_entry')
	s := b.func_add_argument(func_id, b.str_type, 's')
	width_arg := b.func_add_argument(func_id, b.i32_type, 'width')
	left_arg := b.func_add_argument(func_id, b.i32_type, 'left')

	s_slot := b.block_instr0(.alloca, entry, ptr_string)
	width_slot := b.block_instr0(.alloca, entry, ptr_i32)
	left_slot := b.block_instr0(.alloca, entry, ptr_i32)
	b.block_instr2(.store, entry, b.void_type, s, s_slot)
	b.block_instr2(.store, entry, b.void_type, width_arg, width_slot)
	b.block_instr2(.store, entry, b.void_type, left_arg, left_slot)

	zero32 := b.m.get_or_add_const(b.i32_type, '0')
	one32 := b.m.get_or_add_const(b.i32_type, '1')
	width_neg := b.block_instr2(.lt, entry, b.i1_type, width_arg, zero32)
	neg_block := b.m.add_block(func_id, 'string_pad_neg_width')
	norm_block := b.m.add_block(func_id, 'string_pad_norm')
	b.block_instr3(.br, entry, b.void_type, width_neg, ValueID(neg_block), ValueID(norm_block))

	neg_width := b.block_instr2(.sub, neg_block, b.i32_type, zero32, width_arg)
	b.block_instr2(.store, neg_block, b.void_type, neg_width, width_slot)
	b.block_instr2(.store, neg_block, b.void_type, one32, left_slot)
	b.block_instr1(.jmp, neg_block, b.void_type, ValueID(norm_block))

	width := b.block_instr1(.load, norm_block, b.i32_type, width_slot)
	data_ptr := b.block_struct_field_ptr(norm_block, s_slot, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(norm_block, s_slot, b.str_type, 1)
	data := b.block_instr1(.load, norm_block, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, norm_block, b.i32_type, len_ptr)
	already_wide := b.block_instr2(.ge, norm_block, b.i1_type, len32, width)
	return_original := b.m.add_block(func_id, 'string_pad_return_original')
	pad_block := b.m.add_block(func_id, 'string_pad_alloc')
	b.block_instr3(.br, norm_block, b.void_type, already_wide, ValueID(return_original),
		ValueID(pad_block))

	b.block_instr1(.ret, return_original, b.void_type, s)

	one64 := b.m.get_or_add_const(b.i64_type, '1')
	width64 := b.block_instr1(.zext, pad_block, b.i64_type, width)
	len64 := b.block_instr1(.zext, pad_block, b.i64_type, len32)
	pad_len32 := b.block_instr2(.sub, pad_block, b.i32_type, width, len32)
	pad_len64 := b.block_instr1(.zext, pad_block, b.i64_type, pad_len32)
	alloc_len := b.block_instr2(.add, pad_block, b.i64_type, width64, one64)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	out_data := b.block_instr2(.call, pad_block, ptr_i8, malloc_ref, alloc_len)
	left := b.block_instr1(.load, pad_block, b.i32_type, left_slot)
	left_aligned := b.block_instr2(.ne, pad_block, b.i1_type, left, zero32)
	copy_left := b.m.add_block(func_id, 'string_pad_copy_left')
	copy_right := b.m.add_block(func_id, 'string_pad_copy_right')
	done := b.m.add_block(func_id, 'string_pad_done')
	b.block_instr3(.br, pad_block, b.void_type, left_aligned, ValueID(copy_left),
		ValueID(copy_right))

	memcpy_ref_left := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, copy_left, ptr_i8, memcpy_ref_left, out_data, data, len64)
	left_pad_dest := b.block_instr2(.add, copy_left, ptr_i8, out_data, len64)
	memset_ref_left := b.m.add_value(.func_ref, b.void_type, 'memset', b.fn_ids['memset'])
	space64 := b.m.get_or_add_const(b.i64_type, '32')
	b.block_instr4(.call, copy_left, ptr_i8, memset_ref_left, left_pad_dest, space64, pad_len64)
	b.block_instr1(.jmp, copy_left, b.void_type, ValueID(done))

	memset_ref_right := b.m.add_value(.func_ref, b.void_type, 'memset', b.fn_ids['memset'])
	b.block_instr4(.call, copy_right, ptr_i8, memset_ref_right, out_data, space64, pad_len64)
	right_text_dest := b.block_instr2(.add, copy_right, ptr_i8, out_data, pad_len64)
	memcpy_ref_right := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, copy_right, ptr_i8, memcpy_ref_right, right_text_dest, data, len64)
	b.block_instr1(.jmp, copy_right, b.void_type, ValueID(done))

	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, done, ptr_i8, out_data, width64)
	b.block_instr2(.store, done, b.void_type, zero8, term_ptr)
	result := b.emit_make_string(done, out_data, width64, 0)
	b.block_instr1(.ret, done, b.void_type, result)
}

// generate_int_zpad_passthrough_body keeps zero-padded helper calls buildable on the
// SSA/native path. C output still uses the full helper implementation.
fn (mut b Builder) generate_int_zpad_passthrough_body(func_id int, value_type TypeID) {
	entry := b.m.add_block(func_id, 'int_zpad_entry')
	value := b.func_add_argument(func_id, value_type, 'value')
	_ := b.func_add_argument(func_id, b.i32_type, 'width')
	mut widened := value
	if value_type != b.i64_type {
		widened = b.block_instr1(.sext, entry, b.i64_type, value)
	}
	int_str_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.fn_ids['int_str'])
	result := b.block_instr2(.call, entry, b.str_type, int_str_ref, widened)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_bench_runtime_stubs updates register bench runtime stubs state for ssa.
fn (mut b Builder) register_bench_runtime_stubs() {
	for name in bench_runtime_stub_names {
		id := b.register_synthetic_function(name, b.i64_type, []TypeID{})
		b.generate_const_i64_body(id, '0')
	}
}

// generate_tos2_body supports generate tos2 body handling for Builder. It backs both the
// non-copying `tos2`/`vstring` wrappers (is_lit=0) and the `vstring_literal` wrappers
// (is_lit=1, the borrowed pointer is static C data that must never be freed).
fn (mut b Builder) generate_tos2_body(func_id int, is_lit int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, ptr_i8, 's')
	strlen_ref := b.m.add_value(.func_ref, b.i64_type, 'strlen', b.fn_ids['strlen'])
	len := b.block_instr2(.call, entry, b.i64_type, strlen_ref, s)
	result := b.emit_make_string(entry, s, len, is_lit)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_tos_clone_body supports generate tos clone body handling for Builder.
fn (mut b Builder) generate_tos_clone_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, ptr_i8, 's')
	strlen_ref := b.m.add_value(.func_ref, b.i64_type, 'strlen', b.fn_ids['strlen'])
	len := b.block_instr2(.call, entry, b.i64_type, strlen_ref, s)
	result := b.emit_make_owned_string(entry, s, len)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_vstring_with_len_body backs `vstring_with_len` (is_lit=0) and
// `vstring_literal_with_len` (is_lit=1; borrowed static C data that must never be freed).
fn (mut b Builder) generate_vstring_with_len_body(func_id int, is_lit int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, ptr_i8, 's')
	len := b.func_add_argument(func_id, b.i64_type, 'len')
	result := b.emit_make_string(entry, s, len, is_lit)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_string_plus_stubs updates register string plus stubs state for ssa.
fn (mut b Builder) register_string_plus_stubs() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	plus_id := b.register_synthetic_function('string__plus', b.str_type, p2)
	b.generate_string_plus_body(plus_id)

	mut p_many := []TypeID{}
	p_many << b.i64_type
	p_many << b.m.type_store.get_ptr(b.str_type)
	many_id := b.register_synthetic_function('string_plus_many', b.str_type, p_many)
	b.generate_string_plus_many_body(many_id)
}

// emit_make_string emits emit make string output for ssa.
// emit_make_string builds a `string{ str: data, len: len64, is_lit: is_lit }` value.
// `is_lit` must be set explicitly because the field shares the trailing struct word and
// is otherwise stack garbage that string.free could misread: pass 1 for a literal /
// borrowed view of static data that must never be freed, and 0 for an owned heap string
// whose `data` is an allocation start (so free() gets the right pointer). For an owned
// copy of a borrowed/interior buffer use emit_make_owned_string.
fn (mut b Builder) emit_make_string(block_id BlockID, data ValueID, len64 ValueID, is_lit int) ValueID {
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	alloca_out := b.block_instr0(.alloca, block_id, ptr_string)
	data_ptr := b.block_struct_field_ptr(block_id, alloca_out, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(block_id, alloca_out, b.str_type, 1)
	is_lit_ptr := b.block_struct_field_ptr(block_id, alloca_out, b.str_type, 2)
	b.block_instr2(.store, block_id, b.void_type, data, data_ptr)
	b.block_instr2(.store, block_id, b.void_type, len64, len_ptr)
	is_lit_const := b.m.get_or_add_const(b.i32_type, is_lit.str())
	b.block_instr2(.store, block_id, b.void_type, is_lit_const, is_lit_ptr)
	return b.block_instr1(.load, block_id, b.str_type, alloca_out)
}

// emit_make_owned_string allocates `len + 1` bytes, copies `len` bytes from `src`,
// NUL-terminates, and returns an owned (non-literal, freeable) string. Use this where a
// helper must return its own buffer rather than a view into a caller-owned one — e.g. a
// substring of a strings.Builder. The returned string is marked non-literal by
// emit_make_string, so returning an interior/aliased pointer here would later hand a
// non-allocation-start pointer to free().
fn (mut b Builder) emit_make_owned_string(block_id BlockID, src ValueID, len ValueID) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	one := b.m.get_or_add_const(b.i64_type, '1')
	alloc_len := b.block_instr2(.add, block_id, b.i64_type, len, one)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	out_data := b.block_instr2(.call, block_id, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, block_id, ptr_i8, memcpy_ref, out_data, src, len)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, block_id, ptr_i8, out_data, len)
	b.block_instr2(.store, block_id, b.void_type, zero8, term_ptr)
	return b.emit_make_string(block_id, out_data, len, 0)
}

// generate_string_plus_body supports generate string plus body handling for Builder.
fn (mut b Builder) generate_string_plus_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	left := b.func_add_argument(func_id, b.str_type, 'left')
	right := b.func_add_argument(func_id, b.str_type, 'right')

	alloca_left := b.block_instr0(.alloca, entry, ptr_string)
	alloca_right := b.block_instr0(.alloca, entry, ptr_string)
	b.block_instr2(.store, entry, b.void_type, left, alloca_left)
	b.block_instr2(.store, entry, b.void_type, right, alloca_right)

	left_data_ptr := b.block_struct_field_ptr(entry, alloca_left, b.str_type, 0)
	left_len_ptr := b.block_struct_field_ptr(entry, alloca_left, b.str_type, 1)
	right_data_ptr := b.block_struct_field_ptr(entry, alloca_right, b.str_type, 0)
	right_len_ptr := b.block_struct_field_ptr(entry, alloca_right, b.str_type, 1)
	left_data := b.block_instr1(.load, entry, ptr_i8, left_data_ptr)
	right_data := b.block_instr1(.load, entry, ptr_i8, right_data_ptr)
	left_len32 := b.block_instr1(.load, entry, b.i32_type, left_len_ptr)
	right_len32 := b.block_instr1(.load, entry, b.i32_type, right_len_ptr)
	left_len := b.block_instr1(.zext, entry, b.i64_type, left_len32)
	right_len := b.block_instr1(.zext, entry, b.i64_type, right_len32)
	total_len := b.block_instr2(.add, entry, b.i64_type, left_len, right_len)
	one := b.m.get_or_add_const(b.i64_type, '1')
	alloc_len := b.block_instr2(.add, entry, b.i64_type, total_len, one)

	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	out_data := b.block_instr2(.call, entry, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref_left := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref_left, out_data, left_data, left_len)
	right_dest := b.block_instr2(.add, entry, ptr_i8, out_data, left_len)
	memcpy_ref_right := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref_right, right_dest, right_data, right_len)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, entry, ptr_i8, out_data, total_len)
	b.block_instr2(.store, entry, b.void_type, zero8, term_ptr)

	result := b.emit_make_string(entry, out_data, total_len, 0)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_string_plus_many_body supports generate string plus many body handling for Builder.
fn (mut b Builder) generate_string_plus_many_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	count := b.func_add_argument(func_id, b.i64_type, 'count')
	input_base := b.func_add_argument(func_id, ptr_string, 'input_base')

	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_total := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_written := b.block_instr0(.alloca, entry, ptr_i64)
	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	stride := b.m.get_or_add_const(b.i64_type, '16')
	b.block_instr2(.store, entry, b.void_type, zero64, alloca_i)
	b.block_instr2(.store, entry, b.void_type, zero64, alloca_total)

	sum_check := b.m.add_block(func_id, 'string_plus_many_sum_check')
	sum_body := b.m.add_block(func_id, 'string_plus_many_sum_body')
	alloc_block := b.m.add_block(func_id, 'string_plus_many_alloc')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(sum_check))

	i_sum := b.block_instr1(.load, sum_check, b.i64_type, alloca_i)
	more_sum := b.block_instr2(.lt, sum_check, b.i1_type, i_sum, count)
	b.block_instr3(.br, sum_check, b.void_type, more_sum, ValueID(sum_body), ValueID(alloc_block))

	sum_item_off := b.block_instr2(.mul, sum_body, b.i64_type, i_sum, stride)
	sum_item_ptr := b.block_instr2(.get_element_ptr, sum_body, ptr_string, input_base, sum_item_off)
	sum_len_ptr := b.block_struct_field_ptr(sum_body, sum_item_ptr, b.str_type, 1)
	sum_len32 := b.block_instr1(.load, sum_body, b.i32_type, sum_len_ptr)
	sum_len := b.block_instr1(.zext, sum_body, b.i64_type, sum_len32)
	old_total := b.block_instr1(.load, sum_body, b.i64_type, alloca_total)
	new_total := b.block_instr2(.add, sum_body, b.i64_type, old_total, sum_len)
	next_i_sum := b.block_instr2(.add, sum_body, b.i64_type, i_sum, one64)
	b.block_instr2(.store, sum_body, b.void_type, new_total, alloca_total)
	b.block_instr2(.store, sum_body, b.void_type, next_i_sum, alloca_i)
	b.block_instr1(.jmp, sum_body, b.void_type, ValueID(sum_check))

	total_len := b.block_instr1(.load, alloc_block, b.i64_type, alloca_total)
	alloc_len := b.block_instr2(.add, alloc_block, b.i64_type, total_len, one64)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	out_data := b.block_instr2(.call, alloc_block, ptr_i8, malloc_ref, alloc_len)
	b.block_instr2(.store, alloc_block, b.void_type, zero64, alloca_i)
	b.block_instr2(.store, alloc_block, b.void_type, zero64, alloca_written)

	copy_check := b.m.add_block(func_id, 'string_plus_many_copy_check')
	copy_body := b.m.add_block(func_id, 'string_plus_many_copy_body')
	done := b.m.add_block(func_id, 'string_plus_many_done')
	b.block_instr1(.jmp, alloc_block, b.void_type, ValueID(copy_check))

	i_copy := b.block_instr1(.load, copy_check, b.i64_type, alloca_i)
	more_copy := b.block_instr2(.lt, copy_check, b.i1_type, i_copy, count)
	b.block_instr3(.br, copy_check, b.void_type, more_copy, ValueID(copy_body), ValueID(done))

	copy_item_off := b.block_instr2(.mul, copy_body, b.i64_type, i_copy, stride)
	copy_item_ptr := b.block_instr2(.get_element_ptr, copy_body, ptr_string, input_base,
		copy_item_off)
	copy_data_ptr := b.block_struct_field_ptr(copy_body, copy_item_ptr, b.str_type, 0)
	copy_len_ptr := b.block_struct_field_ptr(copy_body, copy_item_ptr, b.str_type, 1)
	copy_data := b.block_instr1(.load, copy_body, ptr_i8, copy_data_ptr)
	copy_len32 := b.block_instr1(.load, copy_body, b.i32_type, copy_len_ptr)
	copy_len := b.block_instr1(.zext, copy_body, b.i64_type, copy_len32)
	written := b.block_instr1(.load, copy_body, b.i64_type, alloca_written)
	dest := b.block_instr2(.add, copy_body, ptr_i8, out_data, written)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, copy_body, ptr_i8, memcpy_ref, dest, copy_data, copy_len)
	new_written := b.block_instr2(.add, copy_body, b.i64_type, written, copy_len)
	next_i_copy := b.block_instr2(.add, copy_body, b.i64_type, i_copy, one64)
	b.block_instr2(.store, copy_body, b.void_type, new_written, alloca_written)
	b.block_instr2(.store, copy_body, b.void_type, next_i_copy, alloca_i)
	b.block_instr1(.jmp, copy_body, b.void_type, ValueID(copy_check))

	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, done, ptr_i8, out_data, total_len)
	b.block_instr2(.store, done, b.void_type, zero8, term_ptr)
	result := b.emit_make_string(done, out_data, total_len, 0)
	b.block_instr1(.ret, done, b.void_type, result)
}

// register_array_runtime_stubs updates register array runtime stubs state for ssa.
fn (mut b Builder) register_array_runtime_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)

	mut p3 := []TypeID{}
	p3 << b.i64_type
	p3 << b.i64_type
	p3 << b.i64_type
	array_new_id := b.register_synthetic_function('array_new', b.array_type, p3)
	b.generate_array_new_body(array_new_id)

	mut p2 := []TypeID{}
	p2 << b.array_type
	p2 << b.i64_type
	array_get_id := b.register_synthetic_function('array_get', ptr_i8, p2)
	b.generate_array_get_body(array_get_id)

	p3 = []TypeID{}
	p3 << b.array_type
	p3 << b.i64_type
	p3 << b.i64_type
	array_slice_id := b.register_synthetic_function('array_slice', b.array_type, p3)
	b.generate_array_slice_body(array_slice_id)

	mut p2_array := []TypeID{}
	p2_array << b.array_type
	p2_array << b.array_type
	array_eq_string_id := b.register_synthetic_function('array_eq_string', b.i1_type, p2_array)
	b.generate_array_eq_string_body(array_eq_string_id)

	mut p3_array := []TypeID{}
	p3_array << b.array_type
	p3_array << b.array_type
	p3_array << b.i64_type
	array_eq_raw_id := b.register_synthetic_function('array_eq_raw', b.i1_type, p3_array)
	b.generate_array_eq_raw_body(array_eq_raw_id)

	mut p3_array_depth := []TypeID{}
	p3_array_depth << b.array_type
	p3_array_depth << b.array_type
	p3_array_depth << b.i32_type
	array_eq_array_id := b.register_synthetic_function('array_eq_array', b.i1_type, p3_array_depth)
	b.generate_array_eq_array_body(array_eq_array_id)

	p2 = []TypeID{}
	p2 << ptr_array
	p2 << ptr_i8
	array_push_id := b.register_synthetic_function('array_push', b.void_type, p2)
	b.generate_array_push_body(array_push_id)

	mut p3_push_many := []TypeID{}
	p3_push_many << ptr_array
	p3_push_many << ptr_i8
	p3_push_many << b.i64_type
	array_push_many_id := b.register_synthetic_function('array.push_many', b.void_type,
		p3_push_many)
	b.generate_array_push_many_body(array_push_many_id)
	// `array_push_many_ptr` is the C-macro name the transformer emits for
	// `arr << ptr_value` / fixed-array appends; it has the same
	// (ptr_array, ptr_value, count) signature as `array.push_many`. It needs its
	// own definition so the native linker resolves the `_array_push_many_ptr` symbol.
	array_push_many_ptr_id := b.register_synthetic_function('array_push_many_ptr', b.void_type,
		p3_push_many)
	b.generate_array_push_many_body(array_push_many_ptr_id)

	mut p2_push_many := []TypeID{}
	p2_push_many << ptr_array
	p2_push_many << b.array_type
	array_push_many_wrapper_id := b.register_synthetic_function('array_push_many', b.void_type,
		p2_push_many)
	b.generate_array_push_many_array_body(array_push_many_wrapper_id)

	mut p1 := []TypeID{}
	p1 << b.array_type
	array_clone_id := b.register_synthetic_function('array_clone', b.array_type, p1)
	b.generate_array_clone_body(array_clone_id)

	// The transformer lowers `arr.clone()` to `array__clone(&arr)` (a pointer arg, matching
	// cgen's `array array__clone(array* a)`). Without a synthetic under this exact name, the
	// `__`->`.` fallback resolves it to the builtin `array.clone`, which allocates data past
	// an 8-byte header (alloc_array_data) — incompatible with the synthetic runtime that
	// grows/frees buffers via `realloc(data)`/`free(data)` assuming no header. Provide a
	// header-less (calloc-based) clone taking the array by pointer.
	mut p1_ptr_arr := []TypeID{}
	p1_ptr_arr << b.m.type_store.get_ptr(b.array_type)
	array_clone_ptr_id := b.register_synthetic_function('array__clone', b.array_type, p1_ptr_arr)
	b.generate_array_clone_ptr_body(array_clone_ptr_id)

	mut p3_repeat := []TypeID{}
	p3_repeat << b.array_type
	p3_repeat << b.i64_type
	p3_repeat << b.i64_type
	array_repeat_id := b.register_synthetic_function('array.repeat_to_depth', b.array_type,
		p3_repeat)
	b.generate_array_repeat_to_depth_body(array_repeat_id)
}

// register_panic_stub updates register panic stub state for ssa.
fn (mut b Builder) register_panic_stub() {
	mut p1 := []TypeID{}
	p1 << b.str_type
	panic_id := b.register_synthetic_function('panic', b.void_type, p1)
	b.generate_panic_body(panic_id)
}

// register_printing_stubs updates register printing stubs state for ssa.
fn (mut b Builder) register_printing_stubs() {
	mut p1 := []TypeID{}
	p1 << b.str_type
	print_id := b.register_synthetic_function('print', b.void_type, p1)
	b.generate_print_body(print_id, '1', false)
	println_id := b.register_synthetic_function('println', b.void_type, p1)
	b.generate_print_body(println_id, '1', true)
	eprint_id := b.register_synthetic_function('eprint', b.void_type, p1)
	b.generate_print_body(eprint_id, '2', false)
	eprintln_id := b.register_synthetic_function('eprintln', b.void_type, p1)
	b.generate_print_body(eprintln_id, '2', true)
}

// generate_print_body supports generate print body handling for Builder.
fn (mut b Builder) generate_print_body(func_id int, fd_value string, newline bool) {
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	message := b.func_add_argument(func_id, b.str_type, 'message')
	message_slot := b.block_instr0(.alloca, entry, ptr_string)
	b.block_instr2(.store, entry, b.void_type, message, message_slot)
	data_ptr := b.block_struct_field_ptr(entry, message_slot, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, message_slot, b.str_type, 1)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	fd := b.m.get_or_add_const(b.i64_type, fd_value)
	write_ref := b.m.add_value(.func_ref, b.void_type, 'write', b.fn_ids['write'])
	b.block_instr4(.call, entry, b.i64_type, write_ref, fd, data, len)
	if newline {
		newline_str := b.m.add_value(.string_literal, b.str_type, '\n', 0)
		newline_slot := b.block_instr0(.alloca, entry, ptr_string)
		b.block_instr2(.store, entry, b.void_type, newline_str, newline_slot)
		newline_data_ptr := b.block_struct_field_ptr(entry, newline_slot, b.str_type, 0)
		newline_data := b.block_instr1(.load, entry, ptr_i8, newline_data_ptr)
		one := b.m.get_or_add_const(b.i64_type, '1')
		b.block_instr4(.call, entry, b.i64_type, write_ref, fd, newline_data, one)
	}
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_panic_body supports generate panic body handling for Builder.
fn (mut b Builder) generate_panic_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	message := b.func_add_argument(func_id, b.str_type, 'message')
	if eprintln_id := b.fn_ids['eprintln'] {
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'eprintln', eprintln_id)
		b.block_instr2(.call, entry, b.void_type, fn_ref, message)
	}
	if exit_id := b.fn_ids['exit'] {
		one := b.m.get_or_add_const(b.i64_type, '1')
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'exit', exit_id)
		b.block_instr2(.call, entry, b.void_type, fn_ref, one)
	}
	b.block_instr0(.ret, entry, b.void_type)
}

// register_string_builder_stubs updates register string builder stubs state for ssa.
fn (mut b Builder) register_string_builder_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)

	mut p1 := []TypeID{}
	p1 << b.i64_type
	new_id := b.register_synthetic_function('strings.new_builder', b.array_type, p1)
	b.generate_builder_new_body(new_id)

	mut p2 := []TypeID{}
	p2 << ptr_builder
	p2 << b.str_type
	write_string_id :=
		b.register_synthetic_function('strings.Builder.write_string', b.void_type, p2)
	b.generate_builder_write_string_body(write_string_id, false)
	writeln_id := b.register_synthetic_function('strings.Builder.writeln', b.void_type, p2)
	b.generate_builder_write_string_body(writeln_id, true)

	p1 = []TypeID{}
	p1 << ptr_builder
	str_id := b.register_synthetic_function('strings.Builder.str', b.str_type, p1)
	b.generate_builder_str_body(str_id)
	free_id := b.register_synthetic_function('strings.Builder.free', b.void_type, p1)
	b.generate_builder_free_body(free_id)

	p2 = []TypeID{}
	p2 << ptr_builder
	p2 << b.i8_type
	write_u8_id := b.register_synthetic_function('strings.Builder.write_u8', b.void_type, p2)
	b.generate_builder_write_u8_body(write_u8_id)

	mut p3 := []TypeID{}
	p3 << ptr_builder
	p3 << ptr_i8
	p3 << b.i64_type
	write_ptr_id := b.register_synthetic_function('strings.Builder.write_ptr', b.void_type, p3)
	b.generate_builder_write_ptr_body(write_ptr_id)
	push_many_id := b.register_synthetic_function('strings.Builder.push_many', b.void_type, p3)
	b.generate_builder_write_ptr_body(push_many_id)

	p2 = []TypeID{}
	p2 << ptr_builder
	p2 << b.array_type
	write_runes_id := b.register_synthetic_function('strings.Builder.write_runes', b.void_type, p2)
	b.generate_builder_free_body(write_runes_id)

	p2 = []TypeID{}
	p2 << ptr_builder
	p2 << b.i64_type
	last_n_id := b.register_synthetic_function('strings.Builder.last_n', b.str_type, p2)
	b.generate_builder_last_n_body(last_n_id)
}

// generate_builder_new_body supports generate builder new body handling for Builder.
fn (mut b Builder) generate_builder_new_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	initial_size := b.func_add_argument(func_id, b.i64_type, 'initial_size')
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	builder := b.block_instr4(.call, entry, b.array_type, fn_ref, one, zero, initial_size)
	b.block_instr1(.ret, entry, b.void_type, builder)
}

// emit_builder_append emits emit builder append output for ssa.
fn (mut b Builder) emit_builder_append(func_id int, entry BlockID, builder_ptr ValueID, src_ptr ValueID, add_len ValueID) BlockID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	data_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 3)
	old_len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	old_len := b.block_instr1(.zext, entry, b.i64_type, old_len32)
	cap32 := b.block_instr1(.load, entry, b.i32_type, cap_ptr)
	cap := b.block_instr1(.zext, entry, b.i64_type, cap32)
	needed := b.block_instr2(.add, entry, b.i64_type, old_len, add_len)
	needs_grow := b.block_instr2(.gt, entry, b.i1_type, needed, cap)

	blk_grow := b.m.add_block(func_id, 'builder_append_grow')
	blk_copy := b.m.add_block(func_id, 'builder_append_copy')
	b.block_instr3(.br, entry, b.void_type, needs_grow, ValueID(blk_grow), ValueID(blk_copy))

	two := b.m.get_or_add_const(b.i64_type, '2')
	old_data := b.block_instr1(.load, blk_grow, ptr_i8, data_ptr)
	double_needed := b.block_instr2(.mul, blk_grow, b.i64_type, needed, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, double_needed, two)
	realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.fn_ids['realloc'])
	new_data := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref, old_data, new_cap)
	b.block_instr2(.store, blk_grow, b.void_type, new_data, data_ptr)
	b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_copy))

	data := b.block_instr1(.load, blk_copy, ptr_i8, data_ptr)
	dest := b.block_instr2(.add, blk_copy, ptr_i8, data, old_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_copy, ptr_i8, memcpy_ref, dest, src_ptr, add_len)
	b.block_instr2(.store, blk_copy, b.void_type, needed, len_ptr)
	return blk_copy
}

// generate_builder_write_string_body
// supports helper handling in ssa.
fn (mut b Builder) generate_builder_write_string_body(func_id int, add_newline bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	s := b.func_add_argument(func_id, b.str_type, 's')

	alloca_s := b.block_instr0(.alloca, entry, ptr_string)
	b.block_instr2(.store, entry, b.void_type, s, alloca_s)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	len_off := b.m.get_or_add_const(b.i64_type, '8')
	str_ptr_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_s, zero)
	len_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i32, alloca_s, len_off)
	str_ptr := b.block_instr1(.load, entry, ptr_i8, str_ptr_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len64 := b.block_instr1(.zext, entry, b.i64_type, len32)
	end_block := b.emit_builder_append(func_id, entry, builder_ptr, str_ptr, len64)
	if add_newline {
		nl := b.m.get_or_add_const(b.i8_type, '10')
		alloca_nl := b.block_instr0(.alloca, end_block, ptr_i8)
		b.block_instr2(.store, end_block, b.void_type, nl, alloca_nl)
		one := b.m.get_or_add_const(b.i64_type, '1')
		final_block := b.emit_builder_append(func_id, end_block, builder_ptr, alloca_nl, one)
		b.block_instr0(.ret, final_block, b.void_type)
	} else {
		b.block_instr0(.ret, end_block, b.void_type)
	}
}

// generate_builder_write_ptr_body supports generate builder write ptr body handling for Builder.
fn (mut b Builder) generate_builder_write_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	src_ptr := b.func_add_argument(func_id, ptr_i8, 'ptr')
	len := b.func_add_argument(func_id, b.i64_type, 'len')
	end_block := b.emit_builder_append(func_id, entry, builder_ptr, src_ptr, len)
	b.block_instr0(.ret, end_block, b.void_type)
}

// generate_builder_write_u8_body supports generate builder write u8 body handling for Builder.
fn (mut b Builder) generate_builder_write_u8_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	ch := b.func_add_argument(func_id, b.i8_type, 'ch')
	alloca_ch := b.block_instr0(.alloca, entry, ptr_i8)
	b.block_instr2(.store, entry, b.void_type, ch, alloca_ch)
	one := b.m.get_or_add_const(b.i64_type, '1')
	end_block := b.emit_builder_append(func_id, entry, builder_ptr, alloca_ch, one)
	b.block_instr0(.ret, end_block, b.void_type)
}

// generate_builder_str_body supports generate builder str body handling for Builder.
fn (mut b Builder) generate_builder_str_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	data_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 2)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	// Return an owned copy, not the builder's live buffer (matches strings.Builder.str,
	// which memdups): the result is marked non-literal and may be freed independently.
	result := b.emit_make_owned_string(entry, data, len)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_builder_last_n_body supports generate builder last n body handling for Builder.
fn (mut b Builder) generate_builder_last_n_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	n := b.func_add_argument(func_id, b.i64_type, 'n')
	data_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, builder_ptr, b.array_type, 2)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	start := b.block_instr2(.sub, entry, b.i64_type, len, n)
	src := b.block_instr2(.add, entry, ptr_i8, data, start)
	// Copy out an owned substring (matches strings.Builder.last_n -> spart): the result
	// must not alias the interior of the builder buffer, since string.free would later
	// pass that non-allocation-start pointer to free().
	result := b.emit_make_owned_string(entry, src, n)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_builder_free_body supports generate builder free body handling for Builder.
fn (mut b Builder) generate_builder_free_body(func_id int) {
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	_ := b.func_add_argument(func_id, ptr_builder, 'builder')
	b.block_instr0(.ret, entry, b.void_type)
}

// register_path_runtime_stubs updates register path runtime stubs state for ssa.
fn (mut b Builder) register_path_runtime_stubs() {
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	mut p1 := []TypeID{}
	p1 << ptr_builder
	for name in ['normalize_path_in_builder', 'os.normalize_path_in_builder'] {
		normalize_id := b.register_synthetic_function(name, b.void_type, p1)
		b.generate_builder_free_body(normalize_id)
	}

	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	for name in ['join_path_single', 'os.join_path_single'] {
		join_id := b.register_synthetic_function(name, b.str_type, p2)
		b.generate_join_path_single_body(join_id)
	}
}

// generate_join_path_single_body supports generate join path single body handling for Builder.
fn (mut b Builder) generate_join_path_single_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	base := b.func_add_argument(func_id, b.str_type, 'base')
	elem := b.func_add_argument(func_id, b.str_type, 'elem')
	slash := b.m.add_value(.string_literal, b.str_type, '/', 0)
	plus_ref := b.m.add_value(.func_ref, b.str_type, 'string__plus', b.fn_ids['string__plus'])
	with_sep := b.block_instr3(.call, entry, b.str_type, plus_ref, base, slash)
	result := b.block_instr3(.call, entry, b.str_type, plus_ref, with_sep, elem)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_map_runtime_stubs updates register map runtime stubs state for ssa.
fn (mut b Builder) register_map_runtime_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)

	mut p2 := []TypeID{}
	p2 << ptr_map
	p2 << ptr_i8
	find_id := b.register_synthetic_function('v3_map_find', b.i64_type, p2)
	b.generate_map_find_body(find_id)

	mut p6 := []TypeID{}
	for _ in 0 .. 6 {
		p6 << b.i64_type
	}
	new_id := b.register_synthetic_function('new_map', b.map_type, p6)
	b.generate_new_map_body(new_id)

	mut p3 := []TypeID{}
	p3 << ptr_map
	p3 << ptr_i8
	p3 << ptr_i8

	mut p5 := []TypeID{}
	p5 << ptr_map
	p5 << ptr_i8
	p5 << ptr_i8
	p5 << b.i64_type
	p5 << b.i64_type
	sized_set_id := b.register_synthetic_function('v3_map_set_sized', b.void_type, p5)
	b.generate_map_set_sized_body(sized_set_id)
	set_id := b.register_synthetic_function('map__set', b.void_type, p3)
	b.generate_map_set_default_body(set_id)

	p3 = []TypeID{}
	p3 << ptr_map
	p3 << ptr_i8
	p3 << ptr_i8
	get_id := b.register_synthetic_function('map__get', ptr_i8, p3)
	b.generate_map_get_body(get_id)
	mut p2_map_key := []TypeID{}
	p2_map_key << ptr_map
	p2_map_key << ptr_i8
	get_check_id := b.register_synthetic_function('map__get_check', ptr_i8, p2_map_key)
	b.generate_map_get_check_body(get_check_id)
	get_or_set_id := b.register_synthetic_function('map__get_or_set', ptr_i8, p3)
	b.generate_map_get_body(get_or_set_id)

	p2 = []TypeID{}
	p2 << ptr_map
	p2 << ptr_i8
	exists_id := b.register_synthetic_function('map__exists', b.i1_type, p2)
	b.generate_map_exists_body(exists_id)

	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_map
	clear_id := b.register_synthetic_function('map__clear', b.void_type, p1_ptr)
	b.generate_map_clear_body(clear_id)
	free_id := b.register_synthetic_function('map__free', b.void_type, p1_ptr)
	b.generate_map_free_body(free_id)
	mut p2_arr := []TypeID{}
	p2_arr << ptr_map
	p2_arr << b.i64_type
	keys_id := b.register_synthetic_function('map__keys', b.array_type, p2_arr)
	b.generate_map_keys_values_body(keys_id, 0, 4)
	values_id := b.register_synthetic_function('map__values', b.array_type, p2_arr)
	b.generate_map_keys_values_body(values_id, 1, 5)

	mut p2_reserve := []TypeID{}
	p2_reserve << ptr_map
	p2_reserve << b.u32_type
	reserve_id := b.register_synthetic_function('map__reserve', b.void_type, p2_reserve)
	b.generate_map_reserve_body(reserve_id)

	p2 = []TypeID{}
	p2 << ptr_map
	p2 << ptr_i8
	delete_id := b.register_synthetic_function('map__delete', b.void_type, p2)
	b.generate_map_delete_body(delete_id)

	mut p1_map := []TypeID{}
	p1_map << b.map_type
	clone_id := b.register_synthetic_function('map__clone', b.map_type, p1_map)
	b.generate_map_clone_body(clone_id)

	mut p1_ptr_map := []TypeID{}
	p1_ptr_map << ptr_map
	move_id := b.register_synthetic_function('map__move', b.map_type, p1_ptr_map)
	b.generate_map_move_body(move_id)
}

// generate_map_reserve_body emits a capacity no-op for the simplified SSA map runtime.
fn (mut b Builder) generate_map_reserve_body(func_id int) {
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	_ := b.func_add_argument(func_id, ptr_map, 'm')
	_ := b.func_add_argument(func_id, b.u32_type, 'n')
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_map_clear_body clears the simplified SSA map length.
fn (mut b Builder) generate_map_clear_body(func_id int) {
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	state := b.map_state_ptr(entry, m)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, state, zero_state)
	blk_clear := b.m.add_block(func_id, 'map_clear_state')
	blk_done := b.m.add_block(func_id, 'map_clear_done')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(blk_clear), ValueID(blk_done))

	len_ptr := b.map_state_field_ptr(blk_clear, state, 3)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.block_instr2(.store, blk_clear, b.void_type, zero, len_ptr)
	b.block_instr1(.jmp, blk_clear, b.void_type, ValueID(blk_done))

	b.block_instr0(.ret, blk_done, b.void_type)
}

// generate_map_free_body detaches the simplified SSA map state.
fn (mut b Builder) generate_map_free_body(func_id int) {
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	state_field := b.block_struct_field_ptr(entry, m, b.map_type, 0)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	b.block_instr2(.store, entry, b.void_type, zero_state, state_field)
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_map_delete_body supports generate map delete body handling for Builder.
fn (mut b Builder) generate_map_delete_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'm')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.fn_ids['v3_map_find'])
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)
	blk_found := b.m.add_block(func_id, 'map_delete_found')
	blk_done := b.m.add_block(func_id, 'map_delete_done')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_done))

	state := b.map_state_ptr(blk_found, map_ptr)
	keys_ptr := b.map_state_field_ptr(blk_found, state, 0)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	len_ptr := b.map_state_field_ptr(blk_found, state, 3)
	key_size_ptr := b.map_state_field_ptr(blk_found, state, 4)
	val_size_ptr := b.map_state_field_ptr(blk_found, state, 5)
	len := b.block_instr1(.load, blk_found, b.i64_type, len_ptr)
	one := b.m.get_or_add_const(b.i64_type, '1')
	new_len := b.block_instr2(.sub, blk_found, b.i64_type, len, one)
	deleting_last := b.block_instr2(.eq, blk_found, b.i1_type, idx, new_len)
	blk_compact := b.m.add_block(func_id, 'map_delete_compact')
	blk_store_len := b.m.add_block(func_id, 'map_delete_store_len')
	b.block_instr3(.br, blk_found, b.void_type, deleting_last, ValueID(blk_store_len),
		ValueID(blk_compact))

	keys := b.block_instr1(.load, blk_compact, ptr_i8, keys_ptr)
	vals := b.block_instr1(.load, blk_compact, ptr_i8, vals_ptr)
	key_size := b.block_instr1(.load, blk_compact, b.i64_type, key_size_ptr)
	val_size := b.block_instr1(.load, blk_compact, b.i64_type, val_size_ptr)
	key_dst_off := b.block_instr2(.mul, blk_compact, b.i64_type, idx, key_size)
	val_dst_off := b.block_instr2(.mul, blk_compact, b.i64_type, idx, val_size)
	key_src_off := b.block_instr2(.mul, blk_compact, b.i64_type, new_len, key_size)
	val_src_off := b.block_instr2(.mul, blk_compact, b.i64_type, new_len, val_size)
	key_dst := b.block_instr2(.add, blk_compact, ptr_i8, keys, key_dst_off)
	val_dst := b.block_instr2(.add, blk_compact, ptr_i8, vals, val_dst_off)
	key_src := b.block_instr2(.add, blk_compact, ptr_i8, keys, key_src_off)
	val_src := b.block_instr2(.add, blk_compact, ptr_i8, vals, val_src_off)
	memcpy_ref_key := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	memcpy_ref_val := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_compact, ptr_i8, memcpy_ref_key, key_dst, key_src, key_size)
	b.block_instr4(.call, blk_compact, ptr_i8, memcpy_ref_val, val_dst, val_src, val_size)
	b.block_instr1(.jmp, blk_compact, b.void_type, ValueID(blk_store_len))

	b.block_instr2(.store, blk_store_len, b.void_type, new_len, len_ptr)
	b.block_instr1(.jmp, blk_store_len, b.void_type, ValueID(blk_done))

	b.block_instr0(.ret, blk_done, b.void_type)
}

// generate_map_clone_body supports generate map clone body handling for Builder.
fn (mut b Builder) generate_map_clone_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, b.map_type, 'm')
	map_slot := b.block_instr0(.alloca, entry, ptr_map)
	b.block_instr2(.store, entry, b.void_type, m, map_slot)
	state_field := b.block_struct_field_ptr(entry, map_slot, b.map_type, 0)
	old_state := b.block_instr1(.load, entry, ptr_state, state_field)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, old_state, zero_state)
	blk_clone := b.m.add_block(func_id, 'map_clone_copy')
	blk_empty := b.m.add_block(func_id, 'map_clone_empty')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(blk_clone), ValueID(blk_empty))

	one := b.m.get_or_add_const(b.i64_type, '1')
	state_size := b.m.get_or_add_const(b.i64_type, '48')
	calloc_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.fn_ids['calloc'])
	new_state_raw := b.block_instr3(.call, blk_clone, ptr_i8, calloc_ref, one, state_size)
	new_state := b.block_instr1(.bitcast, blk_clone, ptr_state, new_state_raw)

	old_keys_ptr := b.map_state_field_ptr(blk_clone, old_state, 0)
	old_vals_ptr := b.map_state_field_ptr(blk_clone, old_state, 1)
	old_cap_ptr := b.map_state_field_ptr(blk_clone, old_state, 2)
	old_len_ptr := b.map_state_field_ptr(blk_clone, old_state, 3)
	old_key_size_ptr := b.map_state_field_ptr(blk_clone, old_state, 4)
	old_val_size_ptr := b.map_state_field_ptr(blk_clone, old_state, 5)
	old_keys := b.block_instr1(.load, blk_clone, ptr_i8, old_keys_ptr)
	old_vals := b.block_instr1(.load, blk_clone, ptr_i8, old_vals_ptr)
	cap := b.block_instr1(.load, blk_clone, b.i64_type, old_cap_ptr)
	len := b.block_instr1(.load, blk_clone, b.i64_type, old_len_ptr)
	key_size := b.block_instr1(.load, blk_clone, b.i64_type, old_key_size_ptr)
	val_size := b.block_instr1(.load, blk_clone, b.i64_type, old_val_size_ptr)
	new_keys := b.block_instr3(.call, blk_clone, ptr_i8, calloc_ref, cap, key_size)
	new_vals := b.block_instr3(.call, blk_clone, ptr_i8, calloc_ref, cap, val_size)
	key_bytes := b.block_instr2(.mul, blk_clone, b.i64_type, len, key_size)
	val_bytes := b.block_instr2(.mul, blk_clone, b.i64_type, len, val_size)
	memcpy_ref_keys := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	memcpy_ref_vals := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_clone, ptr_i8, memcpy_ref_keys, new_keys, old_keys, key_bytes)
	b.block_instr4(.call, blk_clone, ptr_i8, memcpy_ref_vals, new_vals, old_vals, val_bytes)

	new_keys_ptr := b.map_state_field_ptr(blk_clone, new_state, 0)
	new_vals_ptr := b.map_state_field_ptr(blk_clone, new_state, 1)
	new_cap_ptr := b.map_state_field_ptr(blk_clone, new_state, 2)
	new_len_ptr := b.map_state_field_ptr(blk_clone, new_state, 3)
	new_key_size_ptr := b.map_state_field_ptr(blk_clone, new_state, 4)
	new_val_size_ptr := b.map_state_field_ptr(blk_clone, new_state, 5)
	b.block_instr2(.store, blk_clone, b.void_type, new_keys, new_keys_ptr)
	b.block_instr2(.store, blk_clone, b.void_type, new_vals, new_vals_ptr)
	b.block_instr2(.store, blk_clone, b.void_type, cap, new_cap_ptr)
	b.block_instr2(.store, blk_clone, b.void_type, len, new_len_ptr)
	b.block_instr2(.store, blk_clone, b.void_type, key_size, new_key_size_ptr)
	b.block_instr2(.store, blk_clone, b.void_type, val_size, new_val_size_ptr)

	result_slot := b.block_instr0(.alloca, blk_clone, ptr_map)
	result_state_field := b.block_struct_field_ptr(blk_clone, result_slot, b.map_type, 0)
	b.block_instr2(.store, blk_clone, b.void_type, new_state, result_state_field)
	result := b.block_instr1(.load, blk_clone, b.map_type, result_slot)
	b.block_instr1(.ret, blk_clone, b.void_type, result)

	b.block_instr1(.ret, blk_empty, b.void_type, m)
}

// generate_map_move_body moves the simplified SSA map header out of `m` and zeroes `m`.
fn (mut b Builder) generate_map_move_body(func_id int) {
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	state_field := b.block_struct_field_ptr(entry, m, b.map_type, 0)
	state := b.block_instr1(.load, entry, ptr_state, state_field)

	result_slot := b.block_instr0(.alloca, entry, ptr_map)
	result_state_field := b.block_struct_field_ptr(entry, result_slot, b.map_type, 0)
	b.block_instr2(.store, entry, b.void_type, state, result_state_field)

	zero_state := b.m.get_or_add_const(ptr_state, '0')
	b.block_instr2(.store, entry, b.void_type, zero_state, state_field)
	result := b.block_instr1(.load, entry, b.map_type, result_slot)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_map_keys_values_body copies one map state storage vector into an array.
fn (mut b Builder) generate_map_keys_values_body(func_id int, data_field int, size_field int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	elem_size_arg := b.func_add_argument(func_id, b.i64_type, 'elem_size')
	state := b.map_state_ptr(entry, m)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, state, zero_state)
	blk_copy := b.m.add_block(func_id, 'map_array_copy')
	blk_empty := b.m.add_block(func_id, 'map_array_empty')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(blk_copy), ValueID(blk_empty))

	src_ptr := b.map_state_field_ptr(blk_copy, state, data_field)
	len_ptr := b.map_state_field_ptr(blk_copy, state, 3)
	elem_size_ptr := b.map_state_field_ptr(blk_copy, state, size_field)
	src := b.block_instr1(.load, blk_copy, ptr_i8, src_ptr)
	len := b.block_instr1(.load, blk_copy, b.i64_type, len_ptr)
	elem_size := b.block_instr1(.load, blk_copy, b.i64_type, elem_size_ptr)
	array_new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	arr := b.block_instr4(.call, blk_copy, b.array_type, array_new_ref, elem_size, len, len)
	arr_slot := b.block_instr0(.alloca, blk_copy, ptr_array)
	b.block_instr2(.store, blk_copy, b.void_type, arr, arr_slot)
	data_ptr := b.block_struct_field_ptr(blk_copy, arr_slot, b.array_type, 0)
	data := b.block_instr1(.load, blk_copy, ptr_i8, data_ptr)
	byte_len := b.block_instr2(.mul, blk_copy, b.i64_type, len, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_copy, ptr_i8, memcpy_ref, data, src, byte_len)
	result := b.block_instr1(.load, blk_copy, b.array_type, arr_slot)
	b.block_instr1(.ret, blk_copy, b.void_type, result)

	zero := b.m.get_or_add_const(b.i64_type, '0')
	empty_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	empty := b.block_instr4(.call, blk_empty, b.array_type, empty_ref, elem_size_arg, zero, zero)
	b.block_instr1(.ret, blk_empty, b.void_type, empty)
}

// register_u8_runtime_stubs updates register u8 runtime stubs state for ssa.
fn (mut b Builder) register_u8_runtime_stubs() {
	mut p1 := []TypeID{}
	p1 << b.i8_type
	for name in ['u8.is_digit', 'u8.is_letter', 'u8.is_alnum', 'u8.is_capital'] {
		func_id := b.register_synthetic_function(name, b.i1_type, p1)
		b.generate_u8_predicate_body(func_id, name)
	}
}

// generate_u8_predicate_body supports generate u8 predicate body handling for Builder.
fn (mut b Builder) generate_u8_predicate_body(func_id int, name string) {
	entry := b.m.add_block(func_id, 'entry')
	c := b.func_add_argument(func_id, b.i8_type, 'c')
	digit := b.u8_in_range(entry, c, `0`, `9`)
	lower := b.u8_in_range(entry, c, `a`, `z`)
	upper := b.u8_in_range(entry, c, `A`, `Z`)
	letter := b.block_instr2(.or_, entry, b.i1_type, lower, upper)
	result := match name {
		'u8.is_digit' {
			digit
		}
		'u8.is_letter' {
			letter
		}
		'u8.is_alnum' {
			b.block_instr2(.or_, entry, b.i1_type, digit, letter)
		}
		else {
			upper
		}
	}

	b.block_instr1(.ret, entry, b.void_type, result)
}

// u8_in_range supports u8 in range handling for Builder.
fn (mut b Builder) u8_in_range(block_id BlockID, c ValueID, low u8, high u8) ValueID {
	low_v := b.m.get_or_add_const(b.i8_type, '${int(low)}')
	high_v := b.m.get_or_add_const(b.i8_type, '${int(high)}')
	ge_low := b.block_instr2(.ge, block_id, b.i1_type, c, low_v)
	le_high := b.block_instr2(.le, block_id, b.i1_type, c, high_v)
	return b.block_instr2(.and_, block_id, b.i1_type, ge_low, le_high)
}

// register_heap_tracking_stubs updates register heap tracking stubs state for ssa.
fn (mut b Builder) register_heap_tracking_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p2 := []TypeID{}
	p2 << ptr_i8
	p2 << b.i64_type
	alloc_id := b.register_synthetic_function('_ht_alloc', b.void_type, p2)
	b.generate_void_noop_body(alloc_id)
	mut p1 := []TypeID{}
	p1 << ptr_i8
	free_id := b.register_synthetic_function('_ht_free', b.void_type, p1)
	b.generate_void_noop_body(free_id)
}

// generate_void_noop_body supports generate void noop body handling for Builder.
fn (mut b Builder) generate_void_noop_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	b.block_instr0(.ret, entry, b.void_type)
}

// register_process_capture_stubs updates register process capture stubs state for ssa.
fn (mut b Builder) register_process_capture_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p3 := []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << ptr_i8
	for name in ['v_os_execute_capture_start', 'v_os_exec_capture_start'] {
		func_id := b.register_synthetic_function(name, b.i64_type, p3)
		b.generate_const_i64_body(func_id, '1')
	}
}

// register_file_check_stubs updates register file check stubs state for ssa.
fn (mut b Builder) register_file_check_stubs() {
	mut p1 := []TypeID{}
	p1 << b.i64_type
	for name in ['check_fwrite', 'os.check_fwrite', 'check_fread', 'os.check_fread'] {
		func_id := b.register_synthetic_function(name, b.i64_type, p1)
		b.generate_identity_i64_body(func_id)
	}
}

// generate_identity_i64_body supports generate identity i64 body handling for Builder.
fn (mut b Builder) generate_identity_i64_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	n := b.func_add_argument(func_id, b.i64_type, 'n')
	b.block_instr1(.ret, entry, b.void_type, n)
}

// register_fd_macro_stubs updates register fd macro stubs state for ssa.
fn (mut b Builder) register_fd_macro_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut fd_set_params := []TypeID{}
	fd_set_params << ptr_i8
	zero_id := b.register_synthetic_function('FD_ZERO', b.void_type, fd_set_params)
	b.generate_void_noop_with_params_body(zero_id, fd_set_params)

	mut fd_check_params := []TypeID{}
	fd_check_params << b.i64_type
	fd_check_params << ptr_i8
	set_id := b.register_synthetic_function('FD_SET', b.void_type, fd_check_params)
	b.generate_void_noop_with_params_body(set_id, fd_check_params)
	isset_id := b.register_synthetic_function('FD_ISSET', b.i64_type, fd_check_params)
	b.generate_const_i64_with_params_body(isset_id, fd_check_params, '0')
}

// register_signal_macro_stubs updates register signal macro stubs state for ssa.
fn (mut b Builder) register_signal_macro_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut params := []TypeID{}
	params << b.i64_type
	params << ptr_i8
	name := 'v_signal_with_handler_cast'
	func_id := b.register_synthetic_function(name, ptr_i8, params)
	if fn_type := b.fn_types[name] {
		b.c_fn_types[name] = fn_type
	}
	b.c_fn_ids[name] = func_id
	b.generate_const_ptr_with_params_body(func_id, params, ptr_i8, '0')
}

// generate_void_noop_with_params_body supports generate_void_noop_with_params_body handling in ssa.
fn (mut b Builder) generate_void_noop_with_params_body(func_id int, params []TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	for i, param in params {
		_ := b.func_add_argument(func_id, param, 'p${i}')
	}
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_const_i64_with_params_body supports generate_const_i64_with_params_body handling in ssa.
fn (mut b Builder) generate_const_i64_with_params_body(func_id int, params []TypeID, value string) {
	entry := b.m.add_block(func_id, 'entry')
	for i, param in params {
		_ := b.func_add_argument(func_id, param, 'p${i}')
	}
	result := b.m.get_or_add_const(b.i64_type, value)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_const_ptr_with_params_body supports generate_const_ptr_with_params_body handling in ssa.
fn (mut b Builder) generate_const_ptr_with_params_body(func_id int, params []TypeID, ret_type TypeID, value string) {
	entry := b.m.add_block(func_id, 'entry')
	for i, param in params {
		_ := b.func_add_argument(func_id, param, 'p${i}')
	}
	result := b.m.get_or_add_const(ret_type, value)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_os_stat_stubs updates register os stat stubs state for ssa.
fn (mut b Builder) register_os_stat_stubs() {
	mut p1 := []TypeID{}
	p1 << b.str_type
	for name in ['is_dir', 'os.is_dir'] {
		is_dir_id := b.register_synthetic_function(name, b.i1_type, p1)
		b.generate_os_stat_kind_body(is_dir_id, 'stat', '16384')
	}
	for name in ['is_link', 'os.is_link'] {
		is_link_id := b.register_synthetic_function(name, b.i1_type, p1)
		b.generate_os_stat_kind_body(is_link_id, 'lstat', '40960')
	}
	ls_result_type := b.option_type_id('[]string')
	for name in ['ls', 'os.ls'] {
		ls_id := b.register_synthetic_function(name, ls_result_type, p1)
		b.generate_os_ls_body(ls_id, ls_result_type)
	}
}

// generate_os_stat_kind_body supports generate os stat kind body handling for Builder.
fn (mut b Builder) generate_os_stat_kind_body(func_id int, stat_fn string, expected_mode string) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	entry := b.m.add_block(func_id, 'entry')
	path := b.func_add_argument(func_id, b.str_type, 'path')
	path_data := b.emit_cstring_from_string(entry, path)

	stat_size := b.m.get_or_add_const(b.i64_type, '256')
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	stat_buf := b.block_instr2(.call, entry, ptr_i8, malloc_ref, stat_size)
	stat_idx := b.c_fn_ids[stat_fn] or {
		false_val := b.m.get_or_add_const(b.i1_type, '0')
		b.block_instr1(.ret, entry, b.void_type, false_val)
		return
	}
	stat_ref := b.m.add_value(.func_ref, b.void_type, stat_fn, stat_idx)
	res := b.block_instr3(.call, entry, b.i64_type, stat_ref, path_data, stat_buf)
	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	ok := b.block_instr2(.eq, entry, b.i1_type, res, zero64)
	ok_block := b.m.add_block(func_id, 'stat_ok')
	fail_block := b.m.add_block(func_id, 'stat_fail')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(ok_block), ValueID(fail_block))

	false_val := b.m.get_or_add_const(b.i1_type, '0')
	b.block_instr1(.ret, fail_block, b.void_type, false_val)

	mode_off := b.m.get_or_add_const(b.i64_type, '4')
	mode_raw_ptr := b.block_instr2(.get_element_ptr, ok_block, ptr_i8, stat_buf, mode_off)
	mode_ptr := b.block_instr1(.bitcast, ok_block, ptr_i32, mode_raw_ptr)
	mode := b.block_instr1(.load, ok_block, b.i32_type, mode_ptr)
	mask := b.m.get_or_add_const(b.i32_type, '61440')
	masked := b.block_instr2(.and_, ok_block, b.i32_type, mode, mask)
	expected := b.m.get_or_add_const(b.i32_type, expected_mode)
	is_kind := b.block_instr2(.eq, ok_block, b.i1_type, masked, expected)
	b.block_instr1(.ret, ok_block, b.void_type, is_kind)
}

// generate_os_ls_body supports generate os ls body handling for Builder.
fn (mut b Builder) generate_os_ls_body(func_id int, result_type TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	path := b.func_add_argument(func_id, b.str_type, 'path')
	path_data := b.emit_cstring_from_string(entry, path)

	str_size := b.m.get_or_add_const(b.i64_type, '16')
	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	cap := b.m.get_or_add_const(b.i64_type, '50')
	array_new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	arr := b.block_instr4(.call, entry, b.array_type, array_new_ref, str_size, zero64, cap)
	arr_alloca := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, arr, arr_alloca)

	opendir_ref := b.m.add_value(.func_ref, b.void_type, 'opendir', b.c_fn_ids['opendir'])
	dir_ptr := b.block_instr2(.call, entry, ptr_i8, opendir_ref, path_data)
	null_ptr := b.m.get_or_add_const(ptr_i8, '0')
	dir_is_null := b.block_instr2(.eq, entry, b.i1_type, dir_ptr, null_ptr)
	blk_loop := b.m.add_block(func_id, 'ls_loop')
	blk_empty := b.m.add_block(func_id, 'ls_empty')
	b.block_instr3(.br, entry, b.void_type, dir_is_null, ValueID(blk_empty), ValueID(blk_loop))

	empty_result := b.block_instr1(.load, blk_empty, b.array_type, arr_alloca)
	empty_wrapped := b.block_option_value(blk_empty, result_type, false, empty_result)
	b.block_instr1(.ret, blk_empty, b.void_type, empty_wrapped)

	readdir_ref := b.m.add_value(.func_ref, b.void_type, 'readdir', b.c_fn_ids['readdir'])
	ent := b.block_instr2(.call, blk_loop, ptr_i8, readdir_ref, dir_ptr)
	ent_is_null := b.block_instr2(.eq, blk_loop, b.i1_type, ent, null_ptr)
	blk_done := b.m.add_block(func_id, 'ls_done')
	blk_check := b.m.add_block(func_id, 'ls_check')
	b.block_instr3(.br, blk_loop, b.void_type, ent_is_null, ValueID(blk_done), ValueID(blk_check))

	name_off := b.m.get_or_add_const(b.i64_type, '21')
	name_ptr := b.block_instr2(.get_element_ptr, blk_check, ptr_i8, ent, name_off)
	first := b.block_instr1(.load, blk_check, b.i8_type, name_ptr)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	has_name := b.block_instr2(.ne, blk_check, b.i1_type, first, zero8)
	blk_push := b.m.add_block(func_id, 'ls_push')
	blk_next := b.m.add_block(func_id, 'ls_next')
	b.block_instr3(.br, blk_check, b.void_type, has_name, ValueID(blk_push), ValueID(blk_next))

	tos_clone_ref := b.m.add_value(.func_ref, b.str_type, 'tos_clone', b.fn_ids['tos_clone'])
	name_str := b.block_instr2(.call, blk_push, b.str_type, tos_clone_ref, name_ptr)
	name_alloca := b.block_instr0(.alloca, blk_push, ptr_string)
	b.block_instr2(.store, blk_push, b.void_type, name_str, name_alloca)
	name_elem := b.block_instr1(.bitcast, blk_push, ptr_i8, name_alloca)
	array_push_ref := b.m.add_value(.func_ref, b.void_type, 'array_push', b.fn_ids['array_push'])
	b.block_instr3(.call, blk_push, b.void_type, array_push_ref, arr_alloca, name_elem)
	b.block_instr1(.jmp, blk_push, b.void_type, ValueID(blk_loop))

	b.block_instr1(.jmp, blk_next, b.void_type, ValueID(blk_loop))

	closedir_ref := b.m.add_value(.func_ref, b.void_type, 'closedir', b.c_fn_ids['closedir'])
	b.block_instr2(.call, blk_done, b.i64_type, closedir_ref, dir_ptr)
	result := b.block_instr1(.load, blk_done, b.array_type, arr_alloca)
	wrapped := b.block_option_value(blk_done, result_type, true, result)
	b.block_instr1(.ret, blk_done, b.void_type, wrapped)
}

// block_option_value supports block option value handling for Builder.
fn (mut b Builder) block_option_value(block_id BlockID, opt_typ TypeID, ok bool, raw_value ValueID) ValueID {
	ptr_opt := b.m.type_store.get_ptr(opt_typ)
	alloca := b.block_instr0(.alloca, block_id, ptr_opt)
	ok_ptr := b.block_struct_field_ptr(block_id, alloca, opt_typ, 0)
	ok_val := b.m.get_or_add_const(b.i1_type, if ok { '1' } else { '0' })
	b.block_instr2(.store, block_id, b.void_type, ok_val, ok_ptr)
	value_typ := b.option_value_type(opt_typ)
	if value_typ != b.void_type {
		value_ptr := b.block_struct_field_ptr(block_id, alloca, opt_typ, 1)
		mut value := raw_value
		if value <= 0 {
			value = b.m.get_or_add_const(value_typ, '0')
		}
		b.block_instr2(.store, block_id, b.void_type, value, value_ptr)
	}
	return b.block_instr1(.load, block_id, opt_typ, alloca)
}

// emit_cstring_from_string converts emit cstring from string data for ssa.
fn (mut b Builder) emit_cstring_from_string(block_id BlockID, value ValueID) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	value_alloca := b.block_instr0(.alloca, block_id, ptr_string)
	b.block_instr2(.store, block_id, b.void_type, value, value_alloca)
	data_ptr := b.block_struct_field_ptr(block_id, value_alloca, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(block_id, value_alloca, b.str_type, 1)
	data := b.block_instr1(.load, block_id, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, block_id, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, block_id, b.i64_type, len32)
	one := b.m.get_or_add_const(b.i64_type, '1')
	alloc_len := b.block_instr2(.add, block_id, b.i64_type, len, one)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	out_data := b.block_instr2(.call, block_id, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, block_id, ptr_i8, memcpy_ref, out_data, data, len)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, block_id, ptr_i8, out_data, len)
	b.block_instr2(.store, block_id, b.void_type, zero8, term_ptr)
	return out_data
}

// generate_const_i64_body supports generate const i64 body handling for Builder.
fn (mut b Builder) generate_const_i64_body(func_id int, value string) {
	entry := b.m.add_block(func_id, 'entry')
	result := b.m.get_or_add_const(b.i64_type, value)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_prealloc_atomic_stubs updates register prealloc atomic stubs state for ssa.
fn (mut b Builder) register_prealloc_atomic_stubs() {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_i64
	load_id := b.register_synthetic_function('v_prealloc_atomic_load_i32', b.i64_type, p1_ptr)
	b.generate_atomic_load_i64_body(load_id)

	mut p2 := []TypeID{}
	p2 << ptr_i64
	p2 << b.i64_type
	add_id := b.register_synthetic_function('v_prealloc_atomic_add_i32', b.i64_type, p2)
	b.generate_atomic_add_i64_body(add_id)
	store_id := b.register_synthetic_function('v_prealloc_atomic_store_i32', b.i64_type, p2)
	b.generate_atomic_store_i64_body(store_id)

	mut p3 := []TypeID{}
	p3 << ptr_i64
	p3 << b.i64_type
	p3 << b.i64_type
	cas_id := b.register_synthetic_function('v_prealloc_atomic_cas_i32', b.i64_type, p3)
	b.generate_atomic_cas_i64_body(cas_id)
}

fn (mut b Builder) register_atomic_builtin_stubs() {
	b.register_atomic_scalar_stubs('byte', b.u8_type)
	b.register_atomic_scalar_stubs('u16', b.u16_type)
	b.register_atomic_scalar_stubs('u32', b.u32_type)
	b.register_atomic_scalar_stubs('u64', b.u64_type)
	b.register_atomic_ptr_stubs()

	mut fence_params := []TypeID{}
	fence_params << b.i64_type
	fence_id := b.register_synthetic_c_function('atomic_thread_fence', b.void_type, fence_params)
	b.generate_void_return_body(fence_id)
	cpu_relax_id := b.register_synthetic_c_function('cpu_relax', b.void_type, []TypeID{})
	b.generate_void_return_body(cpu_relax_id)
}

fn (mut b Builder) register_atomic_scalar_stubs(suffix string, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1 := []TypeID{}
	p1 << ptr_i8
	load_id := b.register_synthetic_c_function('atomic_load_${suffix}', typ, p1)
	b.generate_atomic_load_body(load_id, typ)

	mut p2 := []TypeID{}
	p2 << ptr_i8
	p2 << typ
	store_id := b.register_synthetic_c_function('atomic_store_${suffix}', b.void_type, p2)
	b.generate_atomic_store_body(store_id, typ)
	exchange_id := b.register_synthetic_c_function('atomic_exchange_${suffix}', typ, p2)
	b.generate_atomic_exchange_body(exchange_id, typ)
	fetch_add_id := b.register_synthetic_c_function('atomic_fetch_add_${suffix}', typ, p2)
	b.generate_atomic_fetch_add_body(fetch_add_id, typ)
	fetch_sub_id := b.register_synthetic_c_function('atomic_fetch_sub_${suffix}', typ, p2)
	b.generate_atomic_fetch_sub_body(fetch_sub_id, typ)

	mut p3 := []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << typ
	strong_id := b.register_synthetic_c_function('atomic_compare_exchange_strong_${suffix}',
		b.i1_type, p3)
	b.generate_atomic_compare_exchange_body(strong_id, typ)
	weak_id :=
		b.register_synthetic_c_function('atomic_compare_exchange_weak_${suffix}', b.i1_type, p3)
	b.generate_atomic_compare_exchange_body(weak_id, typ)
}

fn (mut b Builder) register_atomic_ptr_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1 := []TypeID{}
	p1 << ptr_i8
	load_id := b.register_synthetic_c_function('atomic_load_ptr', ptr_i8, p1)
	b.generate_atomic_load_ptr_body(load_id)

	mut p2 := []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	store_id := b.register_synthetic_c_function('atomic_store_ptr', b.void_type, p2)
	b.generate_atomic_store_ptr_body(store_id)
	exchange_id := b.register_synthetic_c_function('atomic_exchange_ptr', ptr_i8, p2)
	b.generate_atomic_exchange_ptr_body(exchange_id)
	fetch_add_id := b.register_synthetic_c_function('atomic_fetch_add_ptr', ptr_i8, p2)
	b.generate_atomic_fetch_add_ptr_body(fetch_add_id)
	fetch_sub_id := b.register_synthetic_c_function('atomic_fetch_sub_ptr', ptr_i8, p2)
	b.generate_atomic_fetch_sub_ptr_body(fetch_sub_id)

	mut p3 := []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	strong_id :=
		b.register_synthetic_c_function('atomic_compare_exchange_strong_ptr', b.i1_type, p3)
	b.generate_atomic_compare_exchange_ptr_body(strong_id)
	weak_id := b.register_synthetic_c_function('atomic_compare_exchange_weak_ptr', b.i1_type, p3)
	b.generate_atomic_compare_exchange_ptr_body(weak_id)
}

fn (mut b Builder) generate_void_return_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	b.block_instr0(.ret, entry, b.void_type)
}

fn (mut b Builder) generate_atomic_load_body(func_id int, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_typ := b.m.type_store.get_ptr(typ)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	ptr := b.block_instr1(.bitcast, entry, ptr_typ, ptr_raw)
	value := b.block_instr1(.load, entry, typ, ptr)
	b.block_instr1(.ret, entry, b.void_type, value)
}

fn (mut b Builder) generate_atomic_store_body(func_id int, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_typ := b.m.type_store.get_ptr(typ)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	value := b.func_add_argument(func_id, typ, 'value')
	ptr := b.block_instr1(.bitcast, entry, ptr_typ, ptr_raw)
	b.block_instr2(.store, entry, b.void_type, value, ptr)
	b.block_instr0(.ret, entry, b.void_type)
}

fn (mut b Builder) generate_atomic_exchange_body(func_id int, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_typ := b.m.type_store.get_ptr(typ)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	value := b.func_add_argument(func_id, typ, 'value')
	ptr := b.block_instr1(.bitcast, entry, ptr_typ, ptr_raw)
	old := b.block_instr1(.load, entry, typ, ptr)
	b.block_instr2(.store, entry, b.void_type, value, ptr)
	b.block_instr1(.ret, entry, b.void_type, old)
}

fn (mut b Builder) generate_atomic_fetch_add_body(func_id int, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_typ := b.m.type_store.get_ptr(typ)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	delta := b.func_add_argument(func_id, typ, 'delta')
	ptr := b.block_instr1(.bitcast, entry, ptr_typ, ptr_raw)
	old := b.block_instr1(.load, entry, typ, ptr)
	new_value := b.block_instr2(.add, entry, typ, old, delta)
	b.block_instr2(.store, entry, b.void_type, new_value, ptr)
	b.block_instr1(.ret, entry, b.void_type, old)
}

fn (mut b Builder) generate_atomic_fetch_sub_body(func_id int, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_typ := b.m.type_store.get_ptr(typ)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	delta := b.func_add_argument(func_id, typ, 'delta')
	ptr := b.block_instr1(.bitcast, entry, ptr_typ, ptr_raw)
	old := b.block_instr1(.load, entry, typ, ptr)
	new_value := b.block_instr2(.sub, entry, typ, old, delta)
	b.block_instr2(.store, entry, b.void_type, new_value, ptr)
	b.block_instr1(.ret, entry, b.void_type, old)
}

fn (mut b Builder) generate_atomic_compare_exchange_body(func_id int, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_typ := b.m.type_store.get_ptr(typ)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	expected_raw := b.func_add_argument(func_id, ptr_i8, 'expected')
	desired := b.func_add_argument(func_id, typ, 'desired')
	ptr := b.block_instr1(.bitcast, entry, ptr_typ, ptr_raw)
	expected := b.block_instr1(.bitcast, entry, ptr_typ, expected_raw)
	old := b.block_instr1(.load, entry, typ, ptr)
	expected_value := b.block_instr1(.load, entry, typ, expected)
	ok := b.block_instr2(.eq, entry, b.i1_type, old, expected_value)
	then_block := b.m.add_block(func_id, 'cas_store')
	else_block := b.m.add_block(func_id, 'cas_fail')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(then_block), ValueID(else_block))
	b.block_instr2(.store, then_block, b.void_type, desired, ptr)
	one := b.m.get_or_add_const(b.i1_type, '1')
	zero := b.m.get_or_add_const(b.i1_type, '0')
	b.block_instr1(.ret, then_block, b.void_type, one)
	b.block_instr2(.store, else_block, b.void_type, old, expected)
	b.block_instr1(.ret, else_block, b.void_type, zero)
}

fn (mut b Builder) generate_atomic_load_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	ptr := b.block_instr1(.bitcast, entry, ptr_ptr_i8, ptr_raw)
	value := b.block_instr1(.load, entry, ptr_i8, ptr)
	b.block_instr1(.ret, entry, b.void_type, value)
}

fn (mut b Builder) generate_atomic_store_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	value := b.func_add_argument(func_id, ptr_i8, 'value')
	ptr := b.block_instr1(.bitcast, entry, ptr_ptr_i8, ptr_raw)
	b.block_instr2(.store, entry, b.void_type, value, ptr)
	b.block_instr0(.ret, entry, b.void_type)
}

fn (mut b Builder) generate_atomic_exchange_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	value := b.func_add_argument(func_id, ptr_i8, 'value')
	ptr := b.block_instr1(.bitcast, entry, ptr_ptr_i8, ptr_raw)
	old := b.block_instr1(.load, entry, ptr_i8, ptr)
	b.block_instr2(.store, entry, b.void_type, value, ptr)
	b.block_instr1(.ret, entry, b.void_type, old)
}

fn (mut b Builder) generate_atomic_fetch_add_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	delta := b.func_add_argument(func_id, ptr_i8, 'delta')
	ptr := b.block_instr1(.bitcast, entry, ptr_ptr_i8, ptr_raw)
	old := b.block_instr1(.load, entry, ptr_i8, ptr)
	new_value := b.block_instr2(.add, entry, ptr_i8, old, delta)
	b.block_instr2(.store, entry, b.void_type, new_value, ptr)
	b.block_instr1(.ret, entry, b.void_type, old)
}

fn (mut b Builder) generate_atomic_fetch_sub_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	delta := b.func_add_argument(func_id, ptr_i8, 'delta')
	ptr := b.block_instr1(.bitcast, entry, ptr_ptr_i8, ptr_raw)
	old := b.block_instr1(.load, entry, ptr_i8, ptr)
	new_value := b.block_instr2(.sub, entry, ptr_i8, old, delta)
	b.block_instr2(.store, entry, b.void_type, new_value, ptr)
	b.block_instr1(.ret, entry, b.void_type, old)
}

fn (mut b Builder) generate_atomic_compare_exchange_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	ptr_raw := b.func_add_argument(func_id, ptr_i8, 'ptr')
	expected_raw := b.func_add_argument(func_id, ptr_i8, 'expected')
	desired_raw := b.func_add_argument(func_id, b.i64_type, 'desired')
	desired := b.block_instr1(.bitcast, entry, ptr_i8, desired_raw)
	ptr := b.block_instr1(.bitcast, entry, ptr_ptr_i8, ptr_raw)
	expected := b.block_instr1(.bitcast, entry, ptr_ptr_i8, expected_raw)
	old := b.block_instr1(.load, entry, ptr_i8, ptr)
	expected_value := b.block_instr1(.load, entry, ptr_i8, expected)
	ok := b.block_instr2(.eq, entry, b.i1_type, old, expected_value)
	then_block := b.m.add_block(func_id, 'cas_store')
	else_block := b.m.add_block(func_id, 'cas_fail')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(then_block), ValueID(else_block))
	b.block_instr2(.store, then_block, b.void_type, desired, ptr)
	one := b.m.get_or_add_const(b.i1_type, '1')
	zero := b.m.get_or_add_const(b.i1_type, '0')
	b.block_instr1(.ret, then_block, b.void_type, one)
	b.block_instr2(.store, else_block, b.void_type, old, expected)
	b.block_instr1(.ret, else_block, b.void_type, zero)
}

// generate_atomic_load_i64_body supports generate atomic load i64 body handling for Builder.
fn (mut b Builder) generate_atomic_load_i64_body(func_id int) {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	entry := b.m.add_block(func_id, 'entry')
	ptr := b.func_add_argument(func_id, ptr_i64, 'ptr')
	value := b.block_instr1(.load, entry, b.i64_type, ptr)
	b.block_instr1(.ret, entry, b.void_type, value)
}

// generate_atomic_store_i64_body supports generate atomic store i64 body handling for Builder.
fn (mut b Builder) generate_atomic_store_i64_body(func_id int) {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	entry := b.m.add_block(func_id, 'entry')
	ptr := b.func_add_argument(func_id, ptr_i64, 'ptr')
	value := b.func_add_argument(func_id, b.i64_type, 'value')
	b.block_instr2(.store, entry, b.void_type, value, ptr)
	b.block_instr1(.ret, entry, b.void_type, value)
}

// generate_atomic_add_i64_body supports generate atomic add i64 body handling for Builder.
fn (mut b Builder) generate_atomic_add_i64_body(func_id int) {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	entry := b.m.add_block(func_id, 'entry')
	ptr := b.func_add_argument(func_id, ptr_i64, 'ptr')
	delta := b.func_add_argument(func_id, b.i64_type, 'delta')
	old := b.block_instr1(.load, entry, b.i64_type, ptr)
	new_value := b.block_instr2(.add, entry, b.i64_type, old, delta)
	b.block_instr2(.store, entry, b.void_type, new_value, ptr)
	b.block_instr1(.ret, entry, b.void_type, new_value)
}

// generate_atomic_cas_i64_body converts generate atomic cas i64 body data for ssa.
fn (mut b Builder) generate_atomic_cas_i64_body(func_id int) {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	entry := b.m.add_block(func_id, 'entry')
	ptr := b.func_add_argument(func_id, ptr_i64, 'ptr')
	expected := b.func_add_argument(func_id, b.i64_type, 'expected')
	desired := b.func_add_argument(func_id, b.i64_type, 'desired')
	old := b.block_instr1(.load, entry, b.i64_type, ptr)
	ok := b.block_instr2(.eq, entry, b.i1_type, old, expected)
	then_block := b.m.add_block(func_id, 'cas_store')
	else_block := b.m.add_block(func_id, 'cas_done')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(then_block), ValueID(else_block))
	b.block_instr2(.store, then_block, b.void_type, desired, ptr)
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.block_instr1(.ret, then_block, b.void_type, one)
	b.block_instr1(.ret, else_block, b.void_type, zero)
}

// register_array_string_stubs updates register array string stubs state for ssa.
fn (mut b Builder) register_array_string_stubs() {
	mut p1 := []TypeID{}
	p1 << b.array_type
	array_str_id := b.register_synthetic_function('Array_str', b.str_type, p1)
	b.generate_const_string_body(array_str_id, '[]')
	for name in ['bytestr', 'Array_u8__bytestr', '[]u8.bytestr'] {
		bytestr_id := b.register_synthetic_function(name, b.str_type, p1)
		b.generate_array_bytestr_body(bytestr_id)
	}
	for name in ['Array_u8__hex', '[]u8.hex'] {
		array_hex_id := b.register_synthetic_function(name, b.str_type, p1)
		b.generate_const_string_body(array_hex_id, '')
	}
	for name in ['Array_rune__string', '[]rune.string'] {
		rune_string_id := b.register_synthetic_function(name, b.str_type, p1)
		b.generate_const_string_body(rune_string_id, '')
	}

	mut p2 := []TypeID{}
	p2 << b.array_type
	p2 << b.str_type
	for name in ['array_string_join', 'Array_string__join'] {
		func_id := b.register_synthetic_function(name, b.str_type, p2)
		b.generate_array_string_join_body(func_id)
	}
}

// generate_array_string_join_body supports generate array string join body handling for Builder.
fn (mut b Builder) generate_array_string_join_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	sep := b.func_add_argument(func_id, b.str_type, 'sep')
	arr_alloca := b.block_instr0(.alloca, entry, ptr_array)
	builder_alloca := b.block_instr0(.alloca, entry, ptr_array)
	i_alloca := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, arr, arr_alloca)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.block_instr2(.store, entry, b.void_type, zero, i_alloca)

	new_ref := b.m.add_value(.func_ref, b.array_type, 'strings.new_builder',
		b.fn_ids['strings.new_builder'])
	initial_cap := b.m.get_or_add_const(b.i64_type, '16')
	builder := b.block_instr2(.call, entry, b.array_type, new_ref, initial_cap)
	b.block_instr2(.store, entry, b.void_type, builder, builder_alloca)

	data_ptr := b.block_struct_field_ptr(entry, arr_alloca, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, arr_alloca, b.array_type, 2)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)

	loop := b.m.add_block(func_id, 'array_join_loop')
	body := b.m.add_block(func_id, 'array_join_body')
	write_sep := b.m.add_block(func_id, 'array_join_write_sep')
	write_elem := b.m.add_block(func_id, 'array_join_write_elem')
	done := b.m.add_block(func_id, 'array_join_done')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(loop))

	i_val := b.block_instr1(.load, loop, b.i64_type, i_alloca)
	more := b.block_instr2(.lt, loop, b.i1_type, i_val, len)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))

	needs_sep := b.block_instr2(.gt, body, b.i1_type, i_val, zero)
	b.block_instr3(.br, body, b.void_type, needs_sep, ValueID(write_sep), ValueID(write_elem))

	write_ref := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_string',
		b.fn_ids['strings.Builder.write_string'])
	b.block_instr3(.call, write_sep, b.void_type, write_ref, builder_alloca, sep)
	b.block_instr1(.jmp, write_sep, b.void_type, ValueID(write_elem))

	elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	offset := b.block_instr2(.mul, write_elem, b.i64_type, i_val, elem_size)
	elem_ptr_raw := b.block_instr2(.add, write_elem, ptr_i8, data, offset)
	elem_ptr := b.block_instr1(.bitcast, write_elem, b.m.type_store.get_ptr(b.str_type),
		elem_ptr_raw)
	elem := b.block_instr1(.load, write_elem, b.str_type, elem_ptr)
	write_ref2 := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_string',
		b.fn_ids['strings.Builder.write_string'])
	b.block_instr3(.call, write_elem, b.void_type, write_ref2, builder_alloca, elem)
	next_i := b.block_instr2(.add, write_elem, b.i64_type, i_val, one)
	b.block_instr2(.store, write_elem, b.void_type, next_i, i_alloca)
	b.block_instr1(.jmp, write_elem, b.void_type, ValueID(loop))

	str_ref := b.m.add_value(.func_ref, b.str_type, 'strings.Builder.str',
		b.fn_ids['strings.Builder.str'])
	result := b.block_instr2(.call, done, b.str_type, str_ref, builder_alloca)
	b.block_instr1(.ret, done, b.void_type, result)
}

// generate_array_bytestr_body supports generate array bytestr body handling for Builder.
fn (mut b Builder) generate_array_bytestr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)
	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 2)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)

	one := b.m.get_or_add_const(b.i64_type, '1')
	alloc_len := b.block_instr2(.add, entry, b.i64_type, len, one)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	out_data := b.block_instr2(.call, entry, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref, out_data, data, len)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, entry, ptr_i8, out_data, len)
	b.block_instr2(.store, entry, b.void_type, zero8, term_ptr)

	result := b.emit_make_string(entry, out_data, len, 0)
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) register_at_exit_stub() {
	mut p1 := []TypeID{}
	p1 << b.resolve_type('FnExitCb')
	result_type := b.option_type_id('void')
	func_id := b.register_synthetic_function('at_exit', result_type, p1)
	b.generate_at_exit_body(func_id, result_type, p1)
}

fn (mut b Builder) generate_at_exit_body(func_id int, result_type TypeID, params []TypeID) {
	entry := b.m.add_block(func_id, 'at_exit_entry')
	for i, param_type in params {
		_ := b.func_add_argument(func_id, param_type, 'arg${i}')
	}
	result := b.block_option_value(entry, result_type, true, ValueID(0))
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) register_pthread_compat_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p_setkind := []TypeID{}
	p_setkind << ptr_i8
	p_setkind << b.i32_type
	setkind_id := b.register_synthetic_c_function('pthread_rwlockattr_setkind_np', b.i32_type,
		p_setkind)
	b.generate_const_body_with_params(setkind_id, b.i32_type, '0', p_setkind)
}

fn (mut b Builder) register_rand_prng_interface_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p_recv := []TypeID{}
	p_recv << ptr_i8
	prng_ptr := b.resolve_type('&rand.PRNG')
	config_type := b.resolve_type('config.PRNGConfigStruct')
	mut p_new_default := []TypeID{}
	p_new_default << config_type
	for name in ['rand.new_default', 'rand__new_default'] {
		func_id := b.register_synthetic_function(name, prng_ptr, p_new_default)
		b.generate_const_body_with_params(func_id, prng_ptr, '0', p_new_default)
	}
	mut p_seed := []TypeID{}
	p_seed << ptr_i8
	p_seed << b.array_type
	for name in ['rand.PRNG.seed', 'rand__PRNG__seed'] {
		func_id := b.register_synthetic_function(name, b.void_type, p_seed)
		b.generate_noop_body(func_id, p_seed)
	}
	for name in ['rand.PRNG.free', 'rand__PRNG__free'] {
		func_id := b.register_synthetic_function(name, b.void_type, p_recv)
		b.generate_noop_body(func_id, p_recv)
	}
	for name in ['rand.PRNG.u8', 'rand__PRNG__u8'] {
		func_id := b.register_synthetic_function(name, b.u8_type, p_recv)
		b.generate_const_body_with_params(func_id, b.u8_type, '0', p_recv)
	}
	for name in ['rand.PRNG.u16', 'rand__PRNG__u16'] {
		func_id := b.register_synthetic_function(name, b.u16_type, p_recv)
		b.generate_const_body_with_params(func_id, b.u16_type, '0', p_recv)
	}
	for name in ['rand.PRNG.u32', 'rand__PRNG__u32'] {
		func_id := b.register_synthetic_function(name, b.u32_type, p_recv)
		b.generate_const_body_with_params(func_id, b.u32_type, '0', p_recv)
	}
	for name in ['rand.PRNG.u64', 'rand__PRNG__u64'] {
		func_id := b.register_synthetic_function(name, b.u64_type, p_recv)
		b.generate_const_body_with_params(func_id, b.u64_type, '0', p_recv)
	}
	for name in ['rand.PRNG.block_size', 'rand__PRNG__block_size'] {
		func_id := b.register_synthetic_function(name, b.i32_type, p_recv)
		b.generate_const_body_with_params(func_id, b.i32_type, '8', p_recv)
	}
}

fn (mut b Builder) generate_noop_body(func_id int, params []TypeID) {
	entry := b.m.add_block(func_id, 'noop_entry')
	for i, param_type in params {
		_ := b.func_add_argument(func_id, param_type, 'arg${i}')
	}
	b.block_instr0(.ret, entry, b.void_type)
}

fn (mut b Builder) generate_const_body_with_params(func_id int, ret TypeID, value string, params []TypeID) {
	entry := b.m.add_block(func_id, 'const_entry')
	for i, param_type in params {
		_ := b.func_add_argument(func_id, param_type, 'arg${i}')
	}
	result := b.m.get_or_add_const(ret, value)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_ierror_stubs updates register ierror stubs state for ssa.
fn (mut b Builder) register_ierror_stubs() {
	ptr_ierror := b.m.type_store.get_ptr(b.i64_type)
	mut p1 := []TypeID{}
	p1 << ptr_ierror
	msg_id := b.register_synthetic_function('IError.msg', b.str_type, p1)
	b.generate_ierror_msg_body(msg_id, ptr_ierror)
	code_id := b.register_synthetic_function('IError.code', b.i64_type, p1)
	b.generate_ierror_code_body(code_id, ptr_ierror)
}

// generate_ierror_msg_body supports generate ierror msg body handling for Builder.
fn (mut b Builder) generate_ierror_msg_body(func_id int, ptr_ierror TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	_ := b.func_add_argument(func_id, ptr_ierror, 'err')
	result := b.m.add_value(.string_literal, b.str_type, '', 0)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_ierror_code_body supports generate ierror code body handling for Builder.
fn (mut b Builder) generate_ierror_code_body(func_id int, ptr_ierror TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	_ := b.func_add_argument(func_id, ptr_ierror, 'err')
	result := b.m.get_or_add_const(b.i64_type, '0')
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_fixed_array_contains_stubs reports register_fixed_array_contains_stubs logic in ssa.
fn (mut b Builder) register_fixed_array_contains_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p3_string := []TypeID{}
	p3_string << ptr_i8
	p3_string << b.i64_type
	p3_string << b.str_type
	contains_string_id := b.register_synthetic_function('fixed_array_contains_string', b.i1_type,
		p3_string)
	b.generate_fixed_array_contains_string_body(contains_string_id)

	mut p3_int := []TypeID{}
	p3_int << ptr_i8
	p3_int << b.i64_type
	p3_int << b.i64_type
	contains_int_id := b.register_synthetic_function('fixed_array_contains_int', b.i1_type, p3_int)
	b.generate_const_bool_body(contains_int_id, false)
}

// generate_fixed_array_contains_string_body
// builds helper data for ssa.
fn (mut b Builder) generate_fixed_array_contains_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, ptr_i8, 'arr')
	len := b.func_add_argument(func_id, b.i64_type, 'len')
	needle := b.func_add_argument(func_id, b.str_type, 'needle')
	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)

	blk_loop := b.m.add_block(func_id, 'fixed_array_contains_string_loop')
	blk_body := b.m.add_block(func_id, 'fixed_array_contains_string_body')
	blk_next := b.m.add_block(func_id, 'fixed_array_contains_string_next')
	blk_found := b.m.add_block(func_id, 'fixed_array_contains_string_found')
	blk_not_found := b.m.add_block(func_id, 'fixed_array_contains_string_not_found')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(blk_loop))

	i := b.block_instr1(.load, blk_loop, b.i64_type, alloca_i)
	in_range := b.block_instr2(.lt, blk_loop, b.i1_type, i, len)
	b.block_instr3(.br, blk_loop, b.void_type, in_range, ValueID(blk_body), ValueID(blk_not_found))

	stride := b.m.get_or_add_const(b.i64_type, '16')
	offset := b.block_instr2(.mul, blk_body, b.i64_type, i, stride)
	slot := b.block_instr2(.add, blk_body, ptr_i8, arr, offset)
	slot_string_ptr := b.block_instr1(.bitcast, blk_body, ptr_string, slot)
	slot_string := b.block_instr1(.load, blk_body, b.str_type, slot_string_ptr)
	eq_ref := b.m.add_value(.func_ref, b.void_type, 'string__eq', b.fn_ids['string__eq'])
	is_eq := b.block_instr3(.call, blk_body, b.i1_type, eq_ref, slot_string, needle)
	b.block_instr3(.br, blk_body, b.void_type, is_eq, ValueID(blk_found), ValueID(blk_next))

	next_i := b.block_instr2(.add, blk_next, b.i64_type, i, one)
	b.block_instr2(.store, blk_next, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, blk_next, b.void_type, ValueID(blk_loop))

	true_value := b.m.get_or_add_const(b.i1_type, '1')
	false_value := b.m.get_or_add_const(b.i1_type, '0')
	b.block_instr1(.ret, blk_found, b.void_type, true_value)
	b.block_instr1(.ret, blk_not_found, b.void_type, false_value)
}

// generate_const_bool_body supports generate const bool body handling for Builder.
fn (mut b Builder) generate_const_bool_body(func_id int, value bool) {
	entry := b.m.add_block(func_id, 'entry')
	result := b.m.get_or_add_const(b.i1_type, if value { '1' } else { '0' })
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_array_contains_stubs reports whether register array contains stubs applies in ssa.
fn (mut b Builder) register_array_contains_stubs() {
	mut p2_string := []TypeID{}
	p2_string << b.array_type
	p2_string << b.str_type
	index_string_id := b.register_synthetic_function('array_index_string', b.i64_type, p2_string)
	b.generate_array_index_string_body(index_string_id)
	contains_string_id := b.register_synthetic_function('array_contains_string', b.i1_type,
		p2_string)
	b.generate_array_contains_from_index_body(contains_string_id, 'array_index_string', b.str_type)

	mut p2_int := []TypeID{}
	p2_int << b.array_type
	p2_int << b.i64_type
	index_int_id := b.register_synthetic_function('array_index_int', b.i64_type, p2_int)
	b.generate_array_index_int_body(index_int_id)
	contains_int_id := b.register_synthetic_function('array_contains_int', b.i1_type, p2_int)
	b.generate_array_contains_from_index_body(contains_int_id, 'array_index_int', b.i64_type)
}

// generate_array_contains_from_index_body
// supports helper handling in ssa.
fn (mut b Builder) generate_array_contains_from_index_body(func_id int, index_name string, needle_type TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	needle := b.func_add_argument(func_id, needle_type, 'needle')
	index_ref := b.m.add_value(.func_ref, b.void_type, index_name, b.fn_ids[index_name])
	idx := b.block_instr3(.call, entry, b.i64_type, index_ref, arr, needle)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)
	b.block_instr1(.ret, entry, b.void_type, found)
}

// generate_array_index_string_body supports generate array index string body handling for Builder.
fn (mut b Builder) generate_array_index_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	needle := b.func_add_argument(func_id, b.str_type, 'needle')
	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)

	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 2)
	elem_size_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 5)
	blk_loop := b.m.add_block(func_id, 'array_index_string_loop')
	blk_body := b.m.add_block(func_id, 'array_index_string_body')
	blk_next := b.m.add_block(func_id, 'array_index_string_next')
	blk_found := b.m.add_block(func_id, 'array_index_string_found')
	blk_not_found := b.m.add_block(func_id, 'array_index_string_not_found')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(blk_loop))

	i := b.block_instr1(.load, blk_loop, b.i64_type, alloca_i)
	len32 := b.block_instr1(.load, blk_loop, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, blk_loop, b.i64_type, len32)
	in_range := b.block_instr2(.lt, blk_loop, b.i1_type, i, len)
	b.block_instr3(.br, blk_loop, b.void_type, in_range, ValueID(blk_body), ValueID(blk_not_found))

	data := b.block_instr1(.load, blk_body, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, blk_body, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, blk_body, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, blk_body, b.i64_type, i, elem_size)
	slot := b.block_instr2(.add, blk_body, ptr_i8, data, offset)
	slot_string_ptr := b.block_instr1(.bitcast, blk_body, ptr_string, slot)
	slot_string := b.block_instr1(.load, blk_body, b.str_type, slot_string_ptr)
	eq_ref := b.m.add_value(.func_ref, b.void_type, 'string__eq', b.fn_ids['string__eq'])
	is_eq := b.block_instr3(.call, blk_body, b.i1_type, eq_ref, slot_string, needle)
	b.block_instr3(.br, blk_body, b.void_type, is_eq, ValueID(blk_found), ValueID(blk_next))

	next_i := b.block_instr2(.add, blk_next, b.i64_type, i, one)
	b.block_instr2(.store, blk_next, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, blk_next, b.void_type, ValueID(blk_loop))

	b.block_instr1(.ret, blk_found, b.void_type, i)
	not_found := b.m.get_or_add_const(b.i64_type, '-1')
	b.block_instr1(.ret, blk_not_found, b.void_type, not_found)
}

// generate_array_index_int_body supports generate array index int body handling for Builder.
fn (mut b Builder) generate_array_index_int_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	needle := b.func_add_argument(func_id, b.i64_type, 'needle')
	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)

	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 2)
	elem_size_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 5)
	blk_loop := b.m.add_block(func_id, 'array_index_int_loop')
	blk_body := b.m.add_block(func_id, 'array_index_int_body')
	blk_next := b.m.add_block(func_id, 'array_index_int_next')
	blk_found := b.m.add_block(func_id, 'array_index_int_found')
	blk_not_found := b.m.add_block(func_id, 'array_index_int_not_found')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(blk_loop))

	i := b.block_instr1(.load, blk_loop, b.i64_type, alloca_i)
	len32 := b.block_instr1(.load, blk_loop, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, blk_loop, b.i64_type, len32)
	in_range := b.block_instr2(.lt, blk_loop, b.i1_type, i, len)
	b.block_instr3(.br, blk_loop, b.void_type, in_range, ValueID(blk_body), ValueID(blk_not_found))

	data := b.block_instr1(.load, blk_body, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, blk_body, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, blk_body, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, blk_body, b.i64_type, i, elem_size)
	slot := b.block_instr2(.add, blk_body, ptr_i8, data, offset)
	slot_i32_ptr := b.block_instr1(.bitcast, blk_body, ptr_i32, slot)
	slot_i32 := b.block_instr1(.load, blk_body, b.i32_type, slot_i32_ptr)
	slot_i64 := b.block_instr1(.sext, blk_body, b.i64_type, slot_i32)
	is_eq := b.block_instr2(.eq, blk_body, b.i1_type, slot_i64, needle)
	b.block_instr3(.br, blk_body, b.void_type, is_eq, ValueID(blk_found), ValueID(blk_next))

	next_i := b.block_instr2(.add, blk_next, b.i64_type, i, one)
	b.block_instr2(.store, blk_next, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, blk_next, b.void_type, ValueID(blk_loop))

	b.block_instr1(.ret, blk_found, b.void_type, i)
	not_found := b.m.get_or_add_const(b.i64_type, '-1')
	b.block_instr1(.ret, blk_not_found, b.void_type, not_found)
}

// emit_map_state_alloc emits emit map state alloc output for ssa.
fn (mut b Builder) emit_map_state_alloc(block_id BlockID, key_size ValueID, val_size ValueID) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	cap := b.m.get_or_add_const(b.i64_type, '8')
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	state_size := b.m.get_or_add_const(b.i64_type, '48')
	calloc_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.fn_ids['calloc'])
	state_raw := b.block_instr3(.call, block_id, ptr_i8, calloc_ref, one, state_size)
	state := b.block_instr1(.bitcast, block_id, ptr_state, state_raw)
	keys := b.block_instr3(.call, block_id, ptr_i8, calloc_ref, cap, key_size)
	vals := b.block_instr3(.call, block_id, ptr_i8, calloc_ref, cap, val_size)

	keys_ptr := b.map_state_field_ptr(block_id, state, 0)
	vals_ptr := b.map_state_field_ptr(block_id, state, 1)
	cap_ptr := b.map_state_field_ptr(block_id, state, 2)
	len_ptr := b.map_state_field_ptr(block_id, state, 3)
	key_size_ptr := b.map_state_field_ptr(block_id, state, 4)
	val_size_ptr := b.map_state_field_ptr(block_id, state, 5)
	b.block_instr2(.store, block_id, b.void_type, keys, keys_ptr)
	b.block_instr2(.store, block_id, b.void_type, vals, vals_ptr)
	b.block_instr2(.store, block_id, b.void_type, cap, cap_ptr)
	b.block_instr2(.store, block_id, b.void_type, zero, len_ptr)
	b.block_instr2(.store, block_id, b.void_type, key_size, key_size_ptr)
	b.block_instr2(.store, block_id, b.void_type, val_size, val_size_ptr)
	return state
}

// map_state_ptr supports map state ptr handling for Builder.
fn (mut b Builder) map_state_ptr(block_id BlockID, map_ptr ValueID) ValueID {
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	state_field_ptr := b.block_struct_field_ptr(block_id, map_ptr, b.map_type, 0)
	return b.block_instr1(.load, block_id, ptr_state, state_field_ptr)
}

// map_state_field_ptr supports map state field ptr handling for Builder.
fn (mut b Builder) map_state_field_ptr(block_id BlockID, state_ptr ValueID, field_idx int) ValueID {
	return b.block_struct_field_ptr(block_id, state_ptr, b.map_state_type, field_idx)
}

// generate_new_map_body supports generate new map body handling for Builder.
fn (mut b Builder) generate_new_map_body(func_id int) {
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	key_size := b.func_add_argument(func_id, b.i64_type, 'key_size')
	val_size := b.func_add_argument(func_id, b.i64_type, 'val_size')
	hash_fn := b.func_add_argument(func_id, b.i64_type, 'hash_fn')
	eq_fn := b.func_add_argument(func_id, b.i64_type, 'eq_fn')
	clone_fn := b.func_add_argument(func_id, b.i64_type, 'clone_fn')
	free_fn := b.func_add_argument(func_id, b.i64_type, 'free_fn')
	_ = hash_fn
	_ = eq_fn
	_ = clone_fn
	_ = free_fn

	alloca_m := b.block_instr0(.alloca, entry, ptr_map)
	state := b.emit_map_state_alloc(entry, key_size, val_size)
	state_ptr := b.block_struct_field_ptr(entry, alloca_m, b.map_type, 0)
	b.block_instr2(.store, entry, b.void_type, state, state_ptr)
	result := b.block_instr1(.load, entry, b.map_type, alloca_m)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_map_find_body supports generate map find body handling for Builder.
fn (mut b Builder) generate_map_find_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')

	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)

	blk_has_map := b.m.add_block(func_id, 'map_find_has_map')
	blk_loop := b.m.add_block(func_id, 'map_find_loop')
	blk_body := b.m.add_block(func_id, 'map_find_body')
	blk_string_cmp := b.m.add_block(func_id, 'map_find_string_cmp')
	blk_mem_cmp := b.m.add_block(func_id, 'map_find_mem_cmp')
	blk_found := b.m.add_block(func_id, 'map_find_found')
	blk_next := b.m.add_block(func_id, 'map_find_next')
	blk_not_found := b.m.add_block(func_id, 'map_find_not_found')
	zero_map := b.m.get_or_add_const(ptr_map, '0')
	has_map := b.block_instr2(.ne, entry, b.i1_type, map_ptr, zero_map)
	b.block_instr3(.br, entry, b.void_type, has_map, ValueID(blk_has_map), ValueID(blk_not_found))

	state := b.map_state_ptr(blk_has_map, map_ptr)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, blk_has_map, b.i1_type, state, zero_state)
	keys_ptr := b.map_state_field_ptr(blk_has_map, state, 0)
	len_ptr := b.map_state_field_ptr(blk_has_map, state, 3)
	key_size_ptr := b.map_state_field_ptr(blk_has_map, state, 4)
	b.block_instr3(.br, blk_has_map, b.void_type, has_state, ValueID(blk_loop),
		ValueID(blk_not_found))

	i := b.block_instr1(.load, blk_loop, b.i64_type, alloca_i)
	len := b.block_instr1(.load, blk_loop, b.i64_type, len_ptr)
	in_range := b.block_instr2(.lt, blk_loop, b.i1_type, i, len)
	b.block_instr3(.br, blk_loop, b.void_type, in_range, ValueID(blk_body), ValueID(blk_not_found))

	keys := b.block_instr1(.load, blk_body, ptr_i8, keys_ptr)
	key_size := b.block_instr1(.load, blk_body, b.i64_type, key_size_ptr)
	offset := b.block_instr2(.mul, blk_body, b.i64_type, i, key_size)
	slot_key := b.block_instr2(.add, blk_body, ptr_i8, keys, offset)
	string_key_size := b.m.get_or_add_const(b.i64_type, '16')
	is_string_key := b.block_instr2(.eq, blk_body, b.i1_type, key_size, string_key_size)
	b.block_instr3(.br, blk_body, b.void_type, is_string_key, ValueID(blk_string_cmp),
		ValueID(blk_mem_cmp))

	slot_string_ptr := b.block_instr1(.bitcast, blk_string_cmp, ptr_string, slot_key)
	key_string_ptr := b.block_instr1(.bitcast, blk_string_cmp, ptr_string, key_ptr)
	slot_string := b.block_instr1(.load, blk_string_cmp, b.str_type, slot_string_ptr)
	key_string := b.block_instr1(.load, blk_string_cmp, b.str_type, key_string_ptr)
	eq_ref := b.m.add_value(.func_ref, b.void_type, 'fast_string_eq', b.fn_ids['fast_string_eq'])
	string_eq := b.block_instr3(.call, blk_string_cmp, b.i1_type, eq_ref, slot_string, key_string)
	b.block_instr3(.br, blk_string_cmp, b.void_type, string_eq, ValueID(blk_found),
		ValueID(blk_next))

	memcmp_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.fn_ids['memcmp'])
	cmp := b.block_instr4(.call, blk_mem_cmp, b.i64_type, memcmp_ref, slot_key, key_ptr, key_size)
	mem_eq := b.block_instr2(.eq, blk_mem_cmp, b.i1_type, cmp, zero)
	b.block_instr3(.br, blk_mem_cmp, b.void_type, mem_eq, ValueID(blk_found), ValueID(blk_next))

	b.block_instr1(.ret, blk_found, b.void_type, i)

	one := b.m.get_or_add_const(b.i64_type, '1')
	next_i := b.block_instr2(.add, blk_next, b.i64_type, i, one)
	b.block_instr2(.store, blk_next, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, blk_next, b.void_type, ValueID(blk_loop))

	not_found := b.m.get_or_add_const(b.i64_type, '-1')
	b.block_instr1(.ret, blk_not_found, b.void_type, not_found)
}

// generate_map_exists_body supports generate map exists body handling for Builder.
fn (mut b Builder) generate_map_exists_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.fn_ids['v3_map_find'])
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)
	b.block_instr1(.ret, entry, b.void_type, found)
}

// generate_map_get_body supports generate map get body handling for Builder.
fn (mut b Builder) generate_map_get_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	zero_ptr := b.func_add_argument(func_id, ptr_i8, 'zero')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.fn_ids['v3_map_find'])
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)

	blk_found := b.m.add_block(func_id, 'map_get_found')
	blk_missing := b.m.add_block(func_id, 'map_get_missing')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_missing))

	state := b.map_state_ptr(blk_found, map_ptr)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	val_size_ptr := b.map_state_field_ptr(blk_found, state, 5)
	vals := b.block_instr1(.load, blk_found, ptr_i8, vals_ptr)
	val_size := b.block_instr1(.load, blk_found, b.i64_type, val_size_ptr)
	offset := b.block_instr2(.mul, blk_found, b.i64_type, idx, val_size)
	result := b.block_instr2(.add, blk_found, ptr_i8, vals, offset)
	b.block_instr1(.ret, blk_found, b.void_type, result)

	b.block_instr1(.ret, blk_missing, b.void_type, zero_ptr)
}

// generate_map_get_check_body supports generate map get check body handling for Builder.
fn (mut b Builder) generate_map_get_check_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.fn_ids['v3_map_find'])
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)

	blk_found := b.m.add_block(func_id, 'map_get_check_found')
	blk_missing := b.m.add_block(func_id, 'map_get_check_missing')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_missing))

	state := b.map_state_ptr(blk_found, map_ptr)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	val_size_ptr := b.map_state_field_ptr(blk_found, state, 5)
	vals := b.block_instr1(.load, blk_found, ptr_i8, vals_ptr)
	val_size := b.block_instr1(.load, blk_found, b.i64_type, val_size_ptr)
	offset := b.block_instr2(.mul, blk_found, b.i64_type, idx, val_size)
	result := b.block_instr2(.add, blk_found, ptr_i8, vals, offset)
	b.block_instr1(.ret, blk_found, b.void_type, result)

	zero_ptr := b.m.get_or_add_const(ptr_i8, '0')
	b.block_instr1(.ret, blk_missing, b.void_type, zero_ptr)
}

// generate_map_set_default_body supports generate map set default body handling for Builder.
fn (mut b Builder) generate_map_set_default_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	val_ptr := b.func_add_argument(func_id, ptr_i8, 'val')
	key_size := b.m.get_or_add_const(b.i64_type, '16')
	val_size := b.m.get_or_add_const(b.i64_type, '8')
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_set_sized',
		b.fn_ids['v3_map_set_sized'])
	mut args := []ValueID{}
	args << fn_ref
	args << map_ptr
	args << key_ptr
	args << val_ptr
	args << key_size
	args << val_size
	b.m.add_instr(.call, entry, b.void_type, args)
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_map_set_sized_body supports generate map set sized body handling for Builder.
fn (mut b Builder) generate_map_set_sized_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	val_ptr := b.func_add_argument(func_id, ptr_i8, 'val')
	key_size_arg := b.func_add_argument(func_id, b.i64_type, 'key_size')
	val_size_arg := b.func_add_argument(func_id, b.i64_type, 'val_size')
	zero := b.m.get_or_add_const(b.i64_type, '0')

	state_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(ptr_state))
	state_field_ptr := b.block_struct_field_ptr(entry, map_ptr, b.map_type, 0)
	old_state := b.block_instr1(.load, entry, ptr_state, state_field_ptr)
	b.block_instr2(.store, entry, b.void_type, old_state, state_slot)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, old_state, zero_state)

	blk_init := b.m.add_block(func_id, 'map_set_init')
	blk_ready := b.m.add_block(func_id, 'map_set_ready')
	blk_update := b.m.add_block(func_id, 'map_set_update')
	blk_insert := b.m.add_block(func_id, 'map_set_insert')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(blk_ready), ValueID(blk_init))

	new_state := b.emit_map_state_alloc(blk_init, key_size_arg, val_size_arg)
	b.block_instr2(.store, blk_init, b.void_type, new_state, state_field_ptr)
	b.block_instr2(.store, blk_init, b.void_type, new_state, state_slot)
	b.block_instr1(.jmp, blk_init, b.void_type, ValueID(blk_ready))

	state := b.block_instr1(.load, blk_ready, ptr_state, state_slot)
	keys_ptr := b.map_state_field_ptr(blk_ready, state, 0)
	vals_ptr := b.map_state_field_ptr(blk_ready, state, 1)
	cap_ptr := b.map_state_field_ptr(blk_ready, state, 2)
	len_ptr := b.map_state_field_ptr(blk_ready, state, 3)
	key_size_ptr := b.map_state_field_ptr(blk_ready, state, 4)
	val_size_ptr := b.map_state_field_ptr(blk_ready, state, 5)

	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.fn_ids['v3_map_find'])
	idx := b.block_instr3(.call, blk_ready, b.i64_type, find_ref, map_ptr, key_ptr)
	found := b.block_instr2(.ge, blk_ready, b.i1_type, idx, zero)
	b.block_instr3(.br, blk_ready, b.void_type, found, ValueID(blk_update), ValueID(blk_insert))

	vals_update := b.block_instr1(.load, blk_update, ptr_i8, vals_ptr)
	val_size_update := b.block_instr1(.load, blk_update, b.i64_type, val_size_ptr)
	update_off := b.block_instr2(.mul, blk_update, b.i64_type, idx, val_size_update)
	update_dest := b.block_instr2(.add, blk_update, ptr_i8, vals_update, update_off)
	memcpy_ref_update := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_update, ptr_i8, memcpy_ref_update, update_dest, val_ptr,
		val_size_update)
	b.block_instr0(.ret, blk_update, b.void_type)

	len := b.block_instr1(.load, blk_insert, b.i64_type, len_ptr)
	cap := b.block_instr1(.load, blk_insert, b.i64_type, cap_ptr)
	needs_grow := b.block_instr2(.ge, blk_insert, b.i1_type, len, cap)
	blk_grow := b.m.add_block(func_id, 'map_set_grow')
	blk_store := b.m.add_block(func_id, 'map_set_store')
	b.block_instr3(.br, blk_insert, b.void_type, needs_grow, ValueID(blk_grow), ValueID(blk_store))

	two := b.m.get_or_add_const(b.i64_type, '2')
	eight := b.m.get_or_add_const(b.i64_type, '8')
	key_size_grow := b.block_instr1(.load, blk_grow, b.i64_type, key_size_ptr)
	val_size_grow := b.block_instr1(.load, blk_grow, b.i64_type, val_size_ptr)
	keys_old := b.block_instr1(.load, blk_grow, ptr_i8, keys_ptr)
	vals_old := b.block_instr1(.load, blk_grow, ptr_i8, vals_ptr)
	double_cap := b.block_instr2(.mul, blk_grow, b.i64_type, cap, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, double_cap, eight)
	key_bytes := b.block_instr2(.mul, blk_grow, b.i64_type, new_cap, key_size_grow)
	val_bytes := b.block_instr2(.mul, blk_grow, b.i64_type, new_cap, val_size_grow)
	realloc_ref_keys := b.m.add_value(.func_ref, b.void_type, 'realloc', b.fn_ids['realloc'])
	realloc_ref_vals := b.m.add_value(.func_ref, b.void_type, 'realloc', b.fn_ids['realloc'])
	keys_new := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref_keys, keys_old, key_bytes)
	vals_new := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref_vals, vals_old, val_bytes)
	b.block_instr2(.store, blk_grow, b.void_type, keys_new, keys_ptr)
	b.block_instr2(.store, blk_grow, b.void_type, vals_new, vals_ptr)
	b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_store))

	keys := b.block_instr1(.load, blk_store, ptr_i8, keys_ptr)
	vals := b.block_instr1(.load, blk_store, ptr_i8, vals_ptr)
	key_size := b.block_instr1(.load, blk_store, b.i64_type, key_size_ptr)
	val_size := b.block_instr1(.load, blk_store, b.i64_type, val_size_ptr)
	key_off := b.block_instr2(.mul, blk_store, b.i64_type, len, key_size)
	val_off := b.block_instr2(.mul, blk_store, b.i64_type, len, val_size)
	key_dest := b.block_instr2(.add, blk_store, ptr_i8, keys, key_off)
	val_dest := b.block_instr2(.add, blk_store, ptr_i8, vals, val_off)
	memcpy_ref_key := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	memcpy_ref_val := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_store, ptr_i8, memcpy_ref_key, key_dest, key_ptr, key_size)
	b.block_instr4(.call, blk_store, ptr_i8, memcpy_ref_val, val_dest, val_ptr, val_size)
	one := b.m.get_or_add_const(b.i64_type, '1')
	new_len := b.block_instr2(.add, blk_store, b.i64_type, len, one)
	b.block_instr2(.store, blk_store, b.void_type, new_len, len_ptr)
	b.block_instr0(.ret, blk_store, b.void_type)
}

// generate_array_new_body supports generate array new body handling for Builder.
fn (mut b Builder) generate_array_new_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	entry := b.m.add_block(func_id, 'entry')
	elem_size := b.func_add_argument(func_id, b.i64_type, 'elem_size')
	len := b.func_add_argument(func_id, b.i64_type, 'len')
	cap := b.func_add_argument(func_id, b.i64_type, 'cap')

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_cap := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_data := b.block_instr0(.alloca, entry, ptr_ptr_i8)
	cap_lt_len := b.block_instr2(.lt, entry, b.i1_type, cap, len)

	blk_use_len := b.m.add_block(func_id, 'array_new_use_len')
	blk_use_cap := b.m.add_block(func_id, 'array_new_use_cap')
	blk_init := b.m.add_block(func_id, 'array_new_init')
	b.block_instr3(.br, entry, b.void_type, cap_lt_len, ValueID(blk_use_len), ValueID(blk_use_cap))

	b.block_instr2(.store, blk_use_len, b.void_type, len, alloca_cap)
	b.block_instr1(.jmp, blk_use_len, b.void_type, ValueID(blk_init))

	b.block_instr2(.store, blk_use_cap, b.void_type, cap, alloca_cap)
	b.block_instr1(.jmp, blk_use_cap, b.void_type, ValueID(blk_init))

	final_cap := b.block_instr1(.load, blk_init, b.i64_type, alloca_cap)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	has_cap := b.block_instr2(.gt, blk_init, b.i1_type, final_cap, zero)
	blk_alloc := b.m.add_block(func_id, 'array_new_alloc')
	blk_empty := b.m.add_block(func_id, 'array_new_empty')
	blk_fields := b.m.add_block(func_id, 'array_new_fields')
	b.block_instr3(.br, blk_init, b.void_type, has_cap, ValueID(blk_alloc), ValueID(blk_empty))

	fn_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.fn_ids['calloc'])
	allocated_data := b.block_instr3(.call, blk_alloc, ptr_i8, fn_ref, final_cap, elem_size)
	b.block_instr2(.store, blk_alloc, b.void_type, allocated_data, alloca_data)
	b.block_instr1(.jmp, blk_alloc, b.void_type, ValueID(blk_fields))

	null_data := b.m.get_or_add_const(ptr_i8, '0')
	b.block_instr2(.store, blk_empty, b.void_type, null_data, alloca_data)
	b.block_instr1(.jmp, blk_empty, b.void_type, ValueID(blk_fields))

	data := b.block_instr1(.load, blk_fields, ptr_i8, alloca_data)
	data_ptr := b.block_struct_field_ptr(blk_fields, alloca_arr, b.array_type, 0)
	offset_ptr := b.block_struct_field_ptr(blk_fields, alloca_arr, b.array_type, 1)
	len_ptr := b.block_struct_field_ptr(blk_fields, alloca_arr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(blk_fields, alloca_arr, b.array_type, 3)
	flags_ptr := b.block_struct_field_ptr(blk_fields, alloca_arr, b.array_type, 4)
	elem_size_ptr := b.block_struct_field_ptr(blk_fields, alloca_arr, b.array_type, 5)
	b.block_instr2(.store, blk_fields, b.void_type, data, data_ptr)
	b.block_instr2(.store, blk_fields, b.void_type, zero, offset_ptr)
	b.block_instr2(.store, blk_fields, b.void_type, len, len_ptr)
	b.block_instr2(.store, blk_fields, b.void_type, final_cap, cap_ptr)
	b.block_instr2(.store, blk_fields, b.void_type, zero, flags_ptr)
	b.block_instr2(.store, blk_fields, b.void_type, elem_size, elem_size_ptr)

	arr := b.block_instr1(.load, blk_fields, b.array_type, alloca_arr)
	b.block_instr1(.ret, blk_fields, b.void_type, arr)
}

// generate_array_get_body supports generate array get body handling for Builder.
fn (mut b Builder) generate_array_get_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	idx := b.func_add_argument(func_id, b.i64_type, 'idx')

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)

	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	elem_size_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 5)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, entry, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, entry, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, entry, b.i64_type, idx, elem_size)
	result := b.block_instr2(.add, entry, ptr_i8, data, offset)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_array_slice_body supports generate array slice body handling for Builder.
fn (mut b Builder) generate_array_slice_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	start := b.func_add_argument(func_id, b.i64_type, 'start')
	end := b.func_add_argument(func_id, b.i64_type, 'end')

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_out := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)

	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	elem_size_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 5)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, entry, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, entry, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, entry, b.i64_type, start, elem_size)
	slice_data := b.block_instr2(.add, entry, ptr_i8, data, offset)
	slice_len := b.block_instr2(.sub, entry, b.i64_type, end, start)
	zero := b.m.get_or_add_const(b.i64_type, '0')

	out_data_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 0)
	out_offset_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 1)
	out_len_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 2)
	out_cap_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 3)
	out_flags_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 4)
	out_elem_size_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 5)
	b.block_instr2(.store, entry, b.void_type, slice_data, out_data_ptr)
	b.block_instr2(.store, entry, b.void_type, zero, out_offset_ptr)
	b.block_instr2(.store, entry, b.void_type, slice_len, out_len_ptr)
	b.block_instr2(.store, entry, b.void_type, slice_len, out_cap_ptr)
	b.block_instr2(.store, entry, b.void_type, zero, out_flags_ptr)
	b.block_instr2(.store, entry, b.void_type, elem_size, out_elem_size_ptr)

	result := b.block_instr1(.load, entry, b.array_type, alloca_out)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_array_eq_string_body implements `[]string == []string` for SSA-native output.
fn (mut b Builder) generate_array_eq_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'array_eq_string_entry')
	left := b.func_add_argument(func_id, b.array_type, 'left')
	right := b.func_add_argument(func_id, b.array_type, 'right')

	left_slot := b.block_instr0(.alloca, entry, ptr_array)
	right_slot := b.block_instr0(.alloca, entry, ptr_array)
	i_slot := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, left, left_slot)
	b.block_instr2(.store, entry, b.void_type, right, right_slot)

	left_len_ptr := b.block_struct_field_ptr(entry, left_slot, b.array_type, 2)
	right_len_ptr := b.block_struct_field_ptr(entry, right_slot, b.array_type, 2)
	left_len32 := b.block_instr1(.load, entry, b.i32_type, left_len_ptr)
	right_len32 := b.block_instr1(.load, entry, b.i32_type, right_len_ptr)
	same_len := b.block_instr2(.eq, entry, b.i1_type, left_len32, right_len32)
	check_empty := b.m.add_block(func_id, 'array_eq_string_check_empty')
	return_false := b.m.add_block(func_id, 'array_eq_string_false')
	return_true := b.m.add_block(func_id, 'array_eq_string_true')
	b.block_instr3(.br, entry, b.void_type, same_len, ValueID(check_empty), ValueID(return_false))

	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	left_len := b.block_instr1(.zext, check_empty, b.i64_type, left_len32)
	is_empty := b.block_instr2(.eq, check_empty, b.i1_type, left_len, zero64)
	loop := b.m.add_block(func_id, 'array_eq_string_loop')
	b.block_instr2(.store, check_empty, b.void_type, zero64, i_slot)
	b.block_instr3(.br, check_empty, b.void_type, is_empty, ValueID(return_true), ValueID(loop))

	left_data_ptr := b.block_struct_field_ptr(loop, left_slot, b.array_type, 0)
	right_data_ptr := b.block_struct_field_ptr(loop, right_slot, b.array_type, 0)
	left_data := b.block_instr1(.load, loop, ptr_i8, left_data_ptr)
	right_data := b.block_instr1(.load, loop, ptr_i8, right_data_ptr)
	i := b.block_instr1(.load, loop, b.i64_type, i_slot)
	more := b.block_instr2(.lt, loop, b.i1_type, i, left_len)
	body := b.m.add_block(func_id, 'array_eq_string_body')
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(return_true))

	stride := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	offset := b.block_instr2(.mul, body, b.i64_type, i, stride)
	left_elem_raw := b.block_instr2(.add, body, ptr_i8, left_data, offset)
	right_elem_raw := b.block_instr2(.add, body, ptr_i8, right_data, offset)
	left_elem_ptr := b.block_instr1(.bitcast, body, ptr_string, left_elem_raw)
	right_elem_ptr := b.block_instr1(.bitcast, body, ptr_string, right_elem_raw)
	left_elem := b.block_instr1(.load, body, b.str_type, left_elem_ptr)
	right_elem := b.block_instr1(.load, body, b.str_type, right_elem_ptr)
	string_eq_ref := b.m.add_value(.func_ref, b.i1_type, 'string__eq', b.fn_ids['string__eq'])
	elem_eq := b.block_instr3(.call, body, b.i1_type, string_eq_ref, left_elem, right_elem)
	next := b.m.add_block(func_id, 'array_eq_string_next')
	b.block_instr3(.br, body, b.void_type, elem_eq, ValueID(next), ValueID(return_false))

	next_i := b.block_instr2(.add, next, b.i64_type, i, one64)
	b.block_instr2(.store, next, b.void_type, next_i, i_slot)
	b.block_instr1(.jmp, next, b.void_type, ValueID(loop))

	false_val := b.m.get_or_add_const(b.i1_type, '0')
	true_val := b.m.get_or_add_const(b.i1_type, '1')
	b.block_instr1(.ret, return_false, b.void_type, false_val)
	b.block_instr1(.ret, return_true, b.void_type, true_val)
}

// generate_array_eq_raw_body implements raw array equality for scalar element arrays.
fn (mut b Builder) generate_array_eq_raw_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'array_eq_raw_entry')
	left := b.func_add_argument(func_id, b.array_type, 'left')
	right := b.func_add_argument(func_id, b.array_type, 'right')
	elem_size := b.func_add_argument(func_id, b.i64_type, 'elem_size')

	left_slot := b.block_instr0(.alloca, entry, ptr_array)
	right_slot := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, left, left_slot)
	b.block_instr2(.store, entry, b.void_type, right, right_slot)
	left_len_ptr := b.block_struct_field_ptr(entry, left_slot, b.array_type, 2)
	right_len_ptr := b.block_struct_field_ptr(entry, right_slot, b.array_type, 2)
	left_len32 := b.block_instr1(.load, entry, b.i32_type, left_len_ptr)
	right_len32 := b.block_instr1(.load, entry, b.i32_type, right_len_ptr)
	same_len := b.block_instr2(.eq, entry, b.i1_type, left_len32, right_len32)
	check_empty := b.m.add_block(func_id, 'array_eq_raw_check_empty')
	return_false := b.m.add_block(func_id, 'array_eq_raw_false')
	return_true := b.m.add_block(func_id, 'array_eq_raw_true')
	b.block_instr3(.br, entry, b.void_type, same_len, ValueID(check_empty), ValueID(return_false))

	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	left_len := b.block_instr1(.zext, check_empty, b.i64_type, left_len32)
	is_empty := b.block_instr2(.eq, check_empty, b.i1_type, left_len, zero64)
	compare := b.m.add_block(func_id, 'array_eq_raw_compare')
	b.block_instr3(.br, check_empty, b.void_type, is_empty, ValueID(return_true), ValueID(compare))

	left_data_ptr := b.block_struct_field_ptr(compare, left_slot, b.array_type, 0)
	right_data_ptr := b.block_struct_field_ptr(compare, right_slot, b.array_type, 0)
	left_data := b.block_instr1(.load, compare, ptr_i8, left_data_ptr)
	right_data := b.block_instr1(.load, compare, ptr_i8, right_data_ptr)
	byte_count := b.block_instr2(.mul, compare, b.i64_type, left_len, elem_size)
	memcmp_ref := b.m.add_value(.func_ref, b.i64_type, 'memcmp', b.fn_ids['memcmp'])
	cmp := b.block_instr4(.call, compare, b.i64_type, memcmp_ref, left_data, right_data, byte_count)
	is_same := b.block_instr2(.eq, compare, b.i1_type, cmp, zero64)
	b.block_instr3(.br, compare, b.void_type, is_same, ValueID(return_true), ValueID(return_false))

	false_val := b.m.get_or_add_const(b.i1_type, '0')
	true_val := b.m.get_or_add_const(b.i1_type, '1')
	b.block_instr1(.ret, return_false, b.void_type, false_val)
	b.block_instr1(.ret, return_true, b.void_type, true_val)
}

// generate_array_eq_array_body keeps nested array equality buildable for SSA-native output.
fn (mut b Builder) generate_array_eq_array_body(func_id int) {
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'array_eq_array_entry')
	left := b.func_add_argument(func_id, b.array_type, 'left')
	right := b.func_add_argument(func_id, b.array_type, 'right')
	_ := b.func_add_argument(func_id, b.i32_type, 'depth')

	left_slot := b.block_instr0(.alloca, entry, ptr_array)
	right_slot := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, left, left_slot)
	b.block_instr2(.store, entry, b.void_type, right, right_slot)
	left_len_ptr := b.block_struct_field_ptr(entry, left_slot, b.array_type, 2)
	right_len_ptr := b.block_struct_field_ptr(entry, right_slot, b.array_type, 2)
	left_elem_size_ptr := b.block_struct_field_ptr(entry, left_slot, b.array_type, 5)
	right_elem_size_ptr := b.block_struct_field_ptr(entry, right_slot, b.array_type, 5)
	left_len := b.block_instr1(.load, entry, b.i32_type, left_len_ptr)
	right_len := b.block_instr1(.load, entry, b.i32_type, right_len_ptr)
	left_elem_size := b.block_instr1(.load, entry, b.i32_type, left_elem_size_ptr)
	right_elem_size := b.block_instr1(.load, entry, b.i32_type, right_elem_size_ptr)
	len_eq := b.block_instr2(.eq, entry, b.i1_type, left_len, right_len)
	elem_size_eq := b.block_instr2(.eq, entry, b.i1_type, left_elem_size, right_elem_size)
	result := b.block_instr2(.and_, entry, b.i1_type, len_eq, elem_size_eq)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_arguments_stub updates register arguments stub state for ssa.
fn (mut b Builder) register_arguments_stub() {
	arguments_id := b.register_synthetic_function('arguments', b.array_type, []TypeID{})
	b.generate_arguments_body(arguments_id)
}

// generate_arguments_body supports generate arguments body handling for Builder.
fn (mut b Builder) generate_arguments_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_ptr_i8 := b.m.type_store.get_ptr(ptr_i8)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)

	entry := b.m.add_block(func_id, 'entry')
	argc_global := b.vars['g_main_argc']
	argv_global := b.vars['g_main_argv']
	argc := b.block_instr1(.load, entry, b.i64_type, argc_global)
	argv := b.block_instr1(.load, entry, ptr_ptr_i8, argv_global)

	elem_size := b.m.get_or_add_const(b.i64_type, '16')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	ptr_size := b.m.get_or_add_const(b.i64_type, '8')
	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	arr := b.block_instr4(.call, entry, b.array_type, new_ref, elem_size, zero, argc)

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)

	loop := b.m.add_block(func_id, 'arguments_loop')
	body := b.m.add_block(func_id, 'arguments_body')
	done := b.m.add_block(func_id, 'arguments_done')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(loop))

	i := b.block_instr1(.load, loop, b.i64_type, alloca_i)
	more := b.block_instr2(.lt, loop, b.i1_type, i, argc)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))

	argv_off := b.block_instr2(.mul, body, b.i64_type, i, ptr_size)
	argv_slot := b.block_instr2(.add, body, ptr_ptr_i8, argv, argv_off)
	cstr := b.block_instr1(.load, body, ptr_i8, argv_slot)
	tos_clone_ref := b.m.add_value(.func_ref, b.str_type, 'tos_clone', b.fn_ids['tos_clone'])
	arg_string := b.block_instr2(.call, body, b.str_type, tos_clone_ref, cstr)

	data_ptr := b.block_struct_field_ptr(body, alloca_arr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(body, alloca_arr, b.array_type, 2)
	data := b.block_instr1(.load, body, ptr_i8, data_ptr)
	elem_off := b.block_instr2(.mul, body, b.i64_type, i, elem_size)
	dest := b.block_instr2(.add, body, ptr_i8, data, elem_off)
	dest_string := b.block_instr1(.bitcast, body, ptr_string, dest)
	b.block_instr2(.store, body, b.void_type, arg_string, dest_string)
	next_i := b.block_instr2(.add, body, b.i64_type, i, one)
	b.block_instr2(.store, body, b.void_type, next_i, len_ptr)
	b.block_instr2(.store, body, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, body, b.void_type, ValueID(loop))

	result := b.block_instr1(.load, done, b.array_type, alloca_arr)
	b.block_instr1(.ret, done, b.void_type, result)
}

// generate_array_clone_body supports generate array clone body handling for Builder.
fn (mut b Builder) generate_array_clone_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_clone := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)

	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 3)
	elem_size_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 5)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	cap32 := b.block_instr1(.load, entry, b.i32_type, cap_ptr)
	cap := b.block_instr1(.zext, entry, b.i64_type, cap32)
	elem_size32 := b.block_instr1(.load, entry, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, entry, b.i64_type, elem_size32)

	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	clone := b.block_instr4(.call, entry, b.array_type, new_ref, elem_size, len, cap)
	b.block_instr2(.store, entry, b.void_type, clone, alloca_clone)

	clone_data_ptr := b.block_struct_field_ptr(entry, alloca_clone, b.array_type, 0)
	clone_data := b.block_instr1(.load, entry, ptr_i8, clone_data_ptr)
	copy_size := b.block_instr2(.mul, entry, b.i64_type, len, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref, clone_data, data, copy_size)
	result := b.block_instr1(.load, entry, b.array_type, alloca_clone)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_array_clone_ptr_body is generate_array_clone_body for the `array__clone(array* a)`
// form the transformer emits: the argument is a pointer to the array, so its fields are read
// directly off the pointer. The clone is allocated header-less via `array_new` (calloc).
fn (mut b Builder) generate_array_clone_ptr_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr_ptr := b.func_add_argument(func_id, ptr_array, 'arr')

	alloca_clone := b.block_instr0(.alloca, entry, ptr_array)

	data_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 3)
	elem_size_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 5)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	cap32 := b.block_instr1(.load, entry, b.i32_type, cap_ptr)
	cap := b.block_instr1(.zext, entry, b.i64_type, cap32)
	elem_size32 := b.block_instr1(.load, entry, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, entry, b.i64_type, elem_size32)

	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	clone := b.block_instr4(.call, entry, b.array_type, new_ref, elem_size, len, cap)
	b.block_instr2(.store, entry, b.void_type, clone, alloca_clone)

	clone_data_ptr := b.block_struct_field_ptr(entry, alloca_clone, b.array_type, 0)
	clone_data := b.block_instr1(.load, entry, ptr_i8, clone_data_ptr)
	copy_size := b.block_instr2(.mul, entry, b.i64_type, len, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref, clone_data, data, copy_size)
	result := b.block_instr1(.load, entry, b.array_type, alloca_clone)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_array_repeat_to_depth_body converts generate array repeat to depth body data for ssa.
fn (mut b Builder) generate_array_repeat_to_depth_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	count := b.func_add_argument(func_id, b.i64_type, 'count')
	_ := b.func_add_argument(func_id, b.i64_type, 'depth')

	alloca_arr := b.block_instr0(.alloca, entry, ptr_array)
	alloca_out := b.block_instr0(.alloca, entry, ptr_array)
	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, arr, alloca_arr)

	data_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 2)
	elem_size_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 5)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	elem_size32 := b.block_instr1(.load, entry, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, entry, b.i64_type, elem_size32)
	total_len := b.block_instr2(.mul, entry, b.i64_type, len, count)

	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.fn_ids['array_new'])
	out := b.block_instr4(.call, entry, b.array_type, new_ref, elem_size, total_len, total_len)
	b.block_instr2(.store, entry, b.void_type, out, alloca_out)

	out_data_ptr := b.block_struct_field_ptr(entry, alloca_out, b.array_type, 0)
	out_data := b.block_instr1(.load, entry, ptr_i8, out_data_ptr)
	chunk_size := b.block_instr2(.mul, entry, b.i64_type, len, elem_size)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)

	loop := b.m.add_block(func_id, 'array_repeat_loop')
	body := b.m.add_block(func_id, 'array_repeat_body')
	done := b.m.add_block(func_id, 'array_repeat_done')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(loop))

	i := b.block_instr1(.load, loop, b.i64_type, alloca_i)
	more := b.block_instr2(.lt, loop, b.i1_type, i, count)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))

	dest_off := b.block_instr2(.mul, body, b.i64_type, i, chunk_size)
	dest := b.block_instr2(.add, body, ptr_i8, out_data, dest_off)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, body, ptr_i8, memcpy_ref, dest, data, chunk_size)
	next_i := b.block_instr2(.add, body, b.i64_type, i, one)
	b.block_instr2(.store, body, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, body, b.void_type, ValueID(loop))

	result := b.block_instr1(.load, done, b.array_type, alloca_out)
	b.block_instr1(.ret, done, b.void_type, result)
}

// generate_array_push_body supports generate array push body handling for Builder.
fn (mut b Builder) generate_array_push_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr_ptr := b.func_add_argument(func_id, ptr_array, 'arr')
	elem_ptr := b.func_add_argument(func_id, ptr_i8, 'elem')

	data_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 3)
	elem_size_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 5)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	cap32 := b.block_instr1(.load, entry, b.i32_type, cap_ptr)
	cap := b.block_instr1(.zext, entry, b.i64_type, cap32)
	needs_grow := b.block_instr2(.ge, entry, b.i1_type, len, cap)

	blk_grow := b.m.add_block(func_id, 'array_push_grow')
	blk_store := b.m.add_block(func_id, 'array_push_store')
	b.block_instr3(.br, entry, b.void_type, needs_grow, ValueID(blk_grow), ValueID(blk_store))

	two := b.m.get_or_add_const(b.i64_type, '2')
	old_data := b.block_instr1(.load, blk_grow, ptr_i8, data_ptr)
	elem_size_grow32 := b.block_instr1(.load, blk_grow, b.i32_type, elem_size_ptr)
	elem_size_grow := b.block_instr1(.zext, blk_grow, b.i64_type, elem_size_grow32)
	double_cap := b.block_instr2(.mul, blk_grow, b.i64_type, cap, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, double_cap, two)
	new_size := b.block_instr2(.mul, blk_grow, b.i64_type, new_cap, elem_size_grow)
	realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.fn_ids['realloc'])
	new_data := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref, old_data, new_size)
	b.block_instr2(.store, blk_grow, b.void_type, new_data, data_ptr)
	b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_store))

	data := b.block_instr1(.load, blk_store, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, blk_store, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, blk_store, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, blk_store, b.i64_type, len, elem_size)
	dest := b.block_instr2(.add, blk_store, ptr_i8, data, offset)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_store, ptr_i8, memcpy_ref, dest, elem_ptr, elem_size)
	one := b.m.get_or_add_const(b.i64_type, '1')
	new_len := b.block_instr2(.add, blk_store, b.i64_type, len, one)
	b.block_instr2(.store, blk_store, b.void_type, new_len, len_ptr)
	b.block_instr0(.ret, blk_store, b.void_type)
}

// generate_array_push_many_body supports generate array push many body handling for Builder.
fn (mut b Builder) generate_array_push_many_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr_ptr := b.func_add_argument(func_id, ptr_array, 'arr')
	src_ptr := b.func_add_argument(func_id, ptr_i8, 'src')
	count := b.func_add_argument(func_id, b.i64_type, 'count')

	zero := b.m.get_or_add_const(b.i64_type, '0')
	has_items := b.block_instr2(.gt, entry, b.i1_type, count, zero)
	blk_check_cap := b.m.add_block(func_id, 'array_push_many_check_cap')
	blk_done := b.m.add_block(func_id, 'array_push_many_done')
	b.block_instr3(.br, entry, b.void_type, has_items, ValueID(blk_check_cap), ValueID(blk_done))

	data_ptr := b.block_struct_field_ptr(blk_check_cap, arr_ptr, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(blk_check_cap, arr_ptr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(blk_check_cap, arr_ptr, b.array_type, 3)
	elem_size_ptr := b.block_struct_field_ptr(blk_check_cap, arr_ptr, b.array_type, 5)
	len32 := b.block_instr1(.load, blk_check_cap, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, blk_check_cap, b.i64_type, len32)
	cap32 := b.block_instr1(.load, blk_check_cap, b.i32_type, cap_ptr)
	cap := b.block_instr1(.zext, blk_check_cap, b.i64_type, cap32)
	new_len := b.block_instr2(.add, blk_check_cap, b.i64_type, len, count)
	needs_grow := b.block_instr2(.gt, blk_check_cap, b.i1_type, new_len, cap)

	blk_grow := b.m.add_block(func_id, 'array_push_many_grow')
	blk_copy := b.m.add_block(func_id, 'array_push_many_copy')
	b.block_instr3(.br, blk_check_cap, b.void_type, needs_grow, ValueID(blk_grow),
		ValueID(blk_copy))

	old_data := b.block_instr1(.load, blk_grow, ptr_i8, data_ptr)
	elem_size_grow32 := b.block_instr1(.load, blk_grow, b.i32_type, elem_size_ptr)
	elem_size_grow := b.block_instr1(.zext, blk_grow, b.i64_type, elem_size_grow32)
	two := b.m.get_or_add_const(b.i64_type, '2')
	new_cap_base := b.block_instr2(.mul, blk_grow, b.i64_type, new_len, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, new_cap_base, two)
	new_size := b.block_instr2(.mul, blk_grow, b.i64_type, new_cap, elem_size_grow)
	realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.fn_ids['realloc'])
	new_data := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref, old_data, new_size)
	b.block_instr2(.store, blk_grow, b.void_type, new_data, data_ptr)
	b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_copy))

	data := b.block_instr1(.load, blk_copy, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, blk_copy, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, blk_copy, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, blk_copy, b.i64_type, len, elem_size)
	dest := b.block_instr2(.add, blk_copy, ptr_i8, data, offset)
	copy_size := b.block_instr2(.mul, blk_copy, b.i64_type, count, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, blk_copy, ptr_i8, memcpy_ref, dest, src_ptr, copy_size)
	b.block_instr2(.store, blk_copy, b.void_type, new_len, len_ptr)
	b.block_instr0(.ret, blk_copy, b.void_type)

	b.block_instr0(.ret, blk_done, b.void_type)
}

// generate_array_push_many_array_body supports generate_array_push_many_array_body handling in ssa.
fn (mut b Builder) generate_array_push_many_array_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr_ptr := b.func_add_argument(func_id, ptr_array, 'arr')
	src := b.func_add_argument(func_id, b.array_type, 'src')

	src_alloca := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, src, src_alloca)
	data_ptr := b.block_struct_field_ptr(entry, src_alloca, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, src_alloca, b.array_type, 2)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	push_many_ref := b.m.add_value(.func_ref, b.void_type, 'array.push_many',
		b.fn_ids['array.push_many'])
	b.block_instr4(.call, entry, b.void_type, push_many_ref, arr_ptr, data, len)
	b.block_instr0(.ret, entry, b.void_type)
}

// register_string_eq_stub updates register string eq stub state for ssa.
fn (mut b Builder) register_string_eq_stub() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	func_id := b.register_synthetic_function('string__eq', b.i1_type, p2)
	b.generate_string_eq_body(func_id)
}

// register_fast_string_eq_stub updates register fast string eq stub state for ssa.
fn (mut b Builder) register_fast_string_eq_stub() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	func_id := b.register_synthetic_function('fast_string_eq', b.i1_type, p2)
	b.generate_string_eq_body(func_id)
}

// register_string_lt_stub updates register string lt stub state for ssa.
fn (mut b Builder) register_string_lt_stub() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	func_id := b.register_synthetic_function('string__lt', b.i1_type, p2)
	b.generate_string_lt_body(func_id)
}

// register_string_trim_stubs updates register string trim stubs state for ssa.
fn (mut b Builder) register_string_trim_stubs() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	trim_right_id := b.register_synthetic_function('string.trim_right', b.str_type, p2)
	b.generate_string_trim_right_body(trim_right_id)
}

// generate_string_trim_right_body supports generate string trim right body handling for Builder.
fn (mut b Builder) generate_string_trim_right_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_i1 := b.m.type_store.get_ptr(b.i1_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, b.str_type, 's')
	cutset := b.func_add_argument(func_id, b.str_type, 'cutset')

	alloca_s := b.block_instr0(.alloca, entry, ptr_string)
	alloca_cutset := b.block_instr0(.alloca, entry, ptr_string)
	alloca_end := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_j := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_found := b.block_instr0(.alloca, entry, ptr_i1)
	b.block_instr2(.store, entry, b.void_type, s, alloca_s)
	b.block_instr2(.store, entry, b.void_type, cutset, alloca_cutset)

	s_data_ptr := b.block_struct_field_ptr(entry, alloca_s, b.str_type, 0)
	s_len_ptr := b.block_struct_field_ptr(entry, alloca_s, b.str_type, 1)
	cut_data_ptr := b.block_struct_field_ptr(entry, alloca_cutset, b.str_type, 0)
	cut_len_ptr := b.block_struct_field_ptr(entry, alloca_cutset, b.str_type, 1)
	s_data := b.block_instr1(.load, entry, ptr_i8, s_data_ptr)
	s_len32 := b.block_instr1(.load, entry, b.i32_type, s_len_ptr)
	s_len := b.block_instr1(.zext, entry, b.i64_type, s_len32)
	cut_data := b.block_instr1(.load, entry, ptr_i8, cut_data_ptr)
	cut_len32 := b.block_instr1(.load, entry, b.i32_type, cut_len_ptr)
	cut_len := b.block_instr1(.zext, entry, b.i64_type, cut_len32)
	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	false_val := b.m.get_or_add_const(b.i1_type, '0')
	true_val := b.m.get_or_add_const(b.i1_type, '1')
	b.block_instr2(.store, entry, b.void_type, s_len, alloca_end)

	loop := b.m.add_block(func_id, 'trim_right_loop')
	body := b.m.add_block(func_id, 'trim_right_body')
	cut_loop := b.m.add_block(func_id, 'trim_right_cut_loop')
	cut_body := b.m.add_block(func_id, 'trim_right_cut_body')
	cut_found := b.m.add_block(func_id, 'trim_right_cut_found')
	cut_next := b.m.add_block(func_id, 'trim_right_cut_next')
	after_cut := b.m.add_block(func_id, 'trim_right_after_cut')
	trim_one := b.m.add_block(func_id, 'trim_right_trim_one')
	done := b.m.add_block(func_id, 'trim_right_done')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(loop))

	end_val := b.block_instr1(.load, loop, b.i64_type, alloca_end)
	has_chars := b.block_instr2(.gt, loop, b.i1_type, end_val, zero64)
	b.block_instr3(.br, loop, b.void_type, has_chars, ValueID(body), ValueID(done))

	last_idx := b.block_instr2(.sub, body, b.i64_type, end_val, one64)
	ch_ptr := b.block_instr2(.add, body, ptr_i8, s_data, last_idx)
	ch := b.block_instr1(.load, body, b.i8_type, ch_ptr)
	b.block_instr2(.store, body, b.void_type, false_val, alloca_found)
	b.block_instr2(.store, body, b.void_type, zero64, alloca_j)
	b.block_instr1(.jmp, body, b.void_type, ValueID(cut_loop))

	j := b.block_instr1(.load, cut_loop, b.i64_type, alloca_j)
	more_cut := b.block_instr2(.lt, cut_loop, b.i1_type, j, cut_len)
	b.block_instr3(.br, cut_loop, b.void_type, more_cut, ValueID(cut_body), ValueID(after_cut))

	cut_ch_ptr := b.block_instr2(.add, cut_body, ptr_i8, cut_data, j)
	cut_ch := b.block_instr1(.load, cut_body, b.i8_type, cut_ch_ptr)
	is_match := b.block_instr2(.eq, cut_body, b.i1_type, ch, cut_ch)
	b.block_instr3(.br, cut_body, b.void_type, is_match, ValueID(cut_found), ValueID(cut_next))

	b.block_instr2(.store, cut_found, b.void_type, true_val, alloca_found)
	b.block_instr1(.jmp, cut_found, b.void_type, ValueID(after_cut))

	next_j := b.block_instr2(.add, cut_next, b.i64_type, j, one64)
	b.block_instr2(.store, cut_next, b.void_type, next_j, alloca_j)
	b.block_instr1(.jmp, cut_next, b.void_type, ValueID(cut_loop))

	found := b.block_instr1(.load, after_cut, b.i1_type, alloca_found)
	b.block_instr3(.br, after_cut, b.void_type, found, ValueID(trim_one), ValueID(done))

	cur_end := b.block_instr1(.load, trim_one, b.i64_type, alloca_end)
	new_end := b.block_instr2(.sub, trim_one, b.i64_type, cur_end, one64)
	b.block_instr2(.store, trim_one, b.void_type, new_end, alloca_end)
	b.block_instr1(.jmp, trim_one, b.void_type, ValueID(loop))

	final_len := b.block_instr1(.load, done, b.i64_type, alloca_end)
	mut result := ValueID(0)
	for substr_name in ['string.substr', 'string__substr', 'substr'] {
		if substr_id := b.fn_ids[substr_name] {
			substr_ref := b.m.add_value(.func_ref, b.str_type, substr_name, substr_id)
			result = b.block_instr4(.call, done, b.str_type, substr_ref, s, zero64, final_len)
			break
		}
	}
	if result == ValueID(0) {
		result = b.emit_make_owned_string(done, s_data, final_len)
	}
	b.block_instr1(.ret, done, b.void_type, result)
}

// register_string_last_part_stubs updates register string last part stubs state for ssa.
fn (mut b Builder) register_string_last_part_stubs() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	for name in ['string.all_before_last', 'string__all_before_last', 'all_before_last'] {
		func_id := b.register_synthetic_function(name, b.str_type, p2)
		b.generate_string_all_last_body(func_id, true)
	}
	for name in ['string.all_after_last', 'string__all_after_last', 'all_after_last'] {
		func_id := b.register_synthetic_function(name, b.str_type, p2)
		b.generate_string_all_last_body(func_id, false)
	}
}

// generate_string_all_last_body supports generate string all last body handling for Builder.
fn (mut b Builder) generate_string_all_last_body(func_id int, before bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, b.str_type, 's')
	sub := b.func_add_argument(func_id, b.str_type, 'sub')
	alloca_s := b.block_instr0(.alloca, entry, ptr_string)
	alloca_sub := b.block_instr0(.alloca, entry, ptr_string)
	alloca_i := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_last := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, s, alloca_s)
	b.block_instr2(.store, entry, b.void_type, sub, alloca_sub)

	s_data_ptr := b.block_struct_field_ptr(entry, alloca_s, b.str_type, 0)
	s_len_ptr := b.block_struct_field_ptr(entry, alloca_s, b.str_type, 1)
	sub_data_ptr := b.block_struct_field_ptr(entry, alloca_sub, b.str_type, 0)
	sub_len_ptr := b.block_struct_field_ptr(entry, alloca_sub, b.str_type, 1)
	s_data := b.block_instr1(.load, entry, ptr_i8, s_data_ptr)
	s_len32 := b.block_instr1(.load, entry, b.i32_type, s_len_ptr)
	s_len := b.block_instr1(.zext, entry, b.i64_type, s_len32)
	sub_data := b.block_instr1(.load, entry, ptr_i8, sub_data_ptr)
	sub_len32 := b.block_instr1(.load, entry, b.i32_type, sub_len_ptr)
	sub_len := b.block_instr1(.zext, entry, b.i64_type, sub_len32)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	minus_one := b.m.get_or_add_const(b.i64_type, '-1')
	b.block_instr2(.store, entry, b.void_type, zero, alloca_i)
	b.block_instr2(.store, entry, b.void_type, minus_one, alloca_last)

	check_size := b.m.add_block(func_id, 'all_last_check_size')
	loop := b.m.add_block(func_id, 'all_last_loop')
	body := b.m.add_block(func_id, 'all_last_body')
	match_block := b.m.add_block(func_id, 'all_last_match')
	next := b.m.add_block(func_id, 'all_last_next')
	done := b.m.add_block(func_id, 'all_last_done')
	return_match := b.m.add_block(func_id, 'all_last_return_match')
	return_original := b.m.add_block(func_id, 'all_last_return_original')
	has_sub := b.block_instr2(.gt, entry, b.i1_type, sub_len, zero)
	b.block_instr3(.br, entry, b.void_type, has_sub, ValueID(check_size), ValueID(return_original))

	sub_fits := b.block_instr2(.ge, check_size, b.i1_type, s_len, sub_len)
	max_start := b.block_instr2(.sub, check_size, b.i64_type, s_len, sub_len)
	b.block_instr3(.br, check_size, b.void_type, sub_fits, ValueID(loop), ValueID(return_original))

	i := b.block_instr1(.load, loop, b.i64_type, alloca_i)
	in_range := b.block_instr2(.le, loop, b.i1_type, i, max_start)
	b.block_instr3(.br, loop, b.void_type, in_range, ValueID(body), ValueID(done))

	slot := b.block_instr2(.add, body, ptr_i8, s_data, i)
	memcmp_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.fn_ids['memcmp'])
	cmp := b.block_instr4(.call, body, b.i64_type, memcmp_ref, slot, sub_data, sub_len)
	is_match := b.block_instr2(.eq, body, b.i1_type, cmp, zero)
	b.block_instr3(.br, body, b.void_type, is_match, ValueID(match_block), ValueID(next))

	b.block_instr2(.store, match_block, b.void_type, i, alloca_last)
	b.block_instr1(.jmp, match_block, b.void_type, ValueID(next))

	next_i := b.block_instr2(.add, next, b.i64_type, i, one)
	b.block_instr2(.store, next, b.void_type, next_i, alloca_i)
	b.block_instr1(.jmp, next, b.void_type, ValueID(loop))

	last := b.block_instr1(.load, done, b.i64_type, alloca_last)
	found := b.block_instr2(.ge, done, b.i1_type, last, zero)
	b.block_instr3(.br, done, b.void_type, found, ValueID(return_match), ValueID(return_original))

	result := if before {
		b.emit_make_owned_string(return_match, s_data, last)
	} else {
		start := b.block_instr2(.add, return_match, b.i64_type, last, sub_len)
		result_data := b.block_instr2(.add, return_match, ptr_i8, s_data, start)
		result_len := b.block_instr2(.sub, return_match, b.i64_type, s_len, start)
		b.emit_make_owned_string(return_match, result_data, result_len)
	}
	b.block_instr1(.ret, return_match, b.void_type, result)
	b.block_instr1(.ret, return_original, b.void_type, s)
}

// generate_string_eq_body supports generate string eq body handling for Builder.
fn (mut b Builder) generate_string_eq_body(func_id int) {
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	entry := b.m.add_block(func_id, 'entry')
	param_a := b.func_add_argument(func_id, b.str_type, 'a')
	param_b := b.func_add_argument(func_id, b.str_type, 'b')

	alloca_a := b.block_instr0(.alloca, entry, ptr_string)
	alloca_b := b.block_instr0(.alloca, entry, ptr_string)
	b.block_instr2(.store, entry, b.void_type, param_a, alloca_a)
	b.block_instr2(.store, entry, b.void_type, param_b, alloca_b)

	zero_64 := b.m.get_or_add_const(b.i64_type, '0')
	len_off := b.m.get_or_add_const(b.i64_type, '8')
	a_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_a, zero_64)
	b_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_b, zero_64)
	a_len_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i32, alloca_a, len_off)
	b_len_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i32, alloca_b, len_off)
	a_str := b.block_instr1(.load, entry, ptr_i8, a_str_ptr)
	b_str := b.block_instr1(.load, entry, ptr_i8, b_str_ptr)
	a_len := b.block_instr1(.load, entry, b.i32_type, a_len_ptr)
	b_len := b.block_instr1(.load, entry, b.i32_type, b_len_ptr)
	len_eq := b.block_instr2(.eq, entry, b.i1_type, a_len, b_len)

	blk_cmp := b.m.add_block(func_id, 'string_eq_cmp')
	blk_false := b.m.add_block(func_id, 'string_eq_false')
	b.block_instr3(.br, entry, b.void_type, len_eq, ValueID(blk_cmp), ValueID(blk_false))

	false_val := b.m.get_or_add_const(b.i1_type, '0')
	b.block_instr1(.ret, blk_false, b.void_type, false_val)

	a_len64 := b.block_instr1(.zext, blk_cmp, b.i64_type, a_len)
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.fn_ids['memcmp'])
	cmp := b.block_instr4(.call, blk_cmp, b.i64_type, fn_ref, a_str, b_str, a_len64)
	is_eq := b.block_instr2(.eq, blk_cmp, b.i1_type, cmp, zero_64)
	b.block_instr1(.ret, blk_cmp, b.void_type, is_eq)
}

// generate_string_lt_body supports generate string lt body handling for Builder.
fn (mut b Builder) generate_string_lt_body(func_id int) {
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	entry := b.m.add_block(func_id, 'entry')
	param_a := b.func_add_argument(func_id, b.str_type, 'a')
	param_b := b.func_add_argument(func_id, b.str_type, 'b')

	alloca_a := b.block_instr0(.alloca, entry, ptr_string)
	alloca_b := b.block_instr0(.alloca, entry, ptr_string)
	alloca_min := b.block_instr0(.alloca, entry, ptr_i32)
	b.block_instr2(.store, entry, b.void_type, param_a, alloca_a)
	b.block_instr2(.store, entry, b.void_type, param_b, alloca_b)

	zero_64 := b.m.get_or_add_const(b.i64_type, '0')
	len_off := b.m.get_or_add_const(b.i64_type, '8')
	a_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_a, zero_64)
	b_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_b, zero_64)
	a_len_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i32, alloca_a, len_off)
	b_len_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i32, alloca_b, len_off)
	a_str := b.block_instr1(.load, entry, ptr_i8, a_str_ptr)
	b_str := b.block_instr1(.load, entry, ptr_i8, b_str_ptr)
	a_len := b.block_instr1(.load, entry, b.i32_type, a_len_ptr)
	b_len := b.block_instr1(.load, entry, b.i32_type, b_len_ptr)
	a_shorter := b.block_instr2(.lt, entry, b.i1_type, a_len, b_len)

	blk_a_min := b.m.add_block(func_id, 'string_lt_a_min')
	blk_b_min := b.m.add_block(func_id, 'string_lt_b_min')
	blk_cmp := b.m.add_block(func_id, 'string_lt_cmp')
	blk_true := b.m.add_block(func_id, 'string_lt_true')
	blk_false := b.m.add_block(func_id, 'string_lt_false')
	blk_cmp_gt := b.m.add_block(func_id, 'string_lt_cmp_gt')
	blk_len := b.m.add_block(func_id, 'string_lt_len')
	b.block_instr3(.br, entry, b.void_type, a_shorter, ValueID(blk_a_min), ValueID(blk_b_min))

	b.block_instr2(.store, blk_a_min, b.void_type, a_len, alloca_min)
	b.block_instr1(.jmp, blk_a_min, b.void_type, ValueID(blk_cmp))

	b.block_instr2(.store, blk_b_min, b.void_type, b_len, alloca_min)
	b.block_instr1(.jmp, blk_b_min, b.void_type, ValueID(blk_cmp))

	min_len := b.block_instr1(.load, blk_cmp, b.i32_type, alloca_min)
	min_len64 := b.block_instr1(.zext, blk_cmp, b.i64_type, min_len)
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.fn_ids['memcmp'])
	cmp := b.block_instr4(.call, blk_cmp, b.i64_type, fn_ref, a_str, b_str, min_len64)
	cmp_lt_zero := b.block_instr2(.lt, blk_cmp, b.i1_type, cmp, zero_64)
	b.block_instr3(.br, blk_cmp, b.void_type, cmp_lt_zero, ValueID(blk_true), ValueID(blk_cmp_gt))

	cmp_gt_zero := b.block_instr2(.gt, blk_cmp_gt, b.i1_type, cmp, zero_64)
	b.block_instr3(.br, blk_cmp_gt, b.void_type, cmp_gt_zero, ValueID(blk_false), ValueID(blk_len))

	true_val := b.m.get_or_add_const(b.i1_type, '1')
	false_val := b.m.get_or_add_const(b.i1_type, '0')
	b.block_instr1(.ret, blk_true, b.void_type, true_val)
	b.block_instr1(.ret, blk_false, b.void_type, false_val)

	len_lt := b.block_instr2(.lt, blk_len, b.i1_type, a_len, b_len)
	b.block_instr1(.ret, blk_len, b.void_type, len_lt)
}

// generate_wymix_body supports generate wymix body handling for Builder.
fn (mut b Builder) generate_wymix_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	param_a := b.func_add_argument(func_id, b.i64_type, 'a')
	param_b := b.func_add_argument(func_id, b.i64_type, 'b')
	result := b.wymix_inline(entry, param_a, param_b)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_wyhash64_body supports generate wyhash64 body handling for Builder.
fn (mut b Builder) generate_wyhash64_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	param_a := b.func_add_argument(func_id, b.i64_type, 'a')
	param_b := b.func_add_argument(func_id, b.i64_type, 'b')

	wyp0 := b.m.get_or_add_const(b.i64_type, '3257665815644502181')
	wyp1 := b.m.get_or_add_const(b.i64_type, '10067880064238660809')

	x := b.block_instr2(.xor, entry, b.i64_type, param_a, wyp0)
	y := b.block_instr2(.xor, entry, b.i64_type, param_b, wyp1)
	result := b.wymix_inline(entry, x, y)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// wymum_pair_inline supports wymum pair inline handling for Builder.
fn (mut b Builder) wymum_pair_inline(block_id BlockID, a ValueID, b_val ValueID) (ValueID, ValueID) {
	mask32 := b.m.get_or_add_const(b.i64_type, '4294967295')
	c32 := b.m.get_or_add_const(b.i64_type, '32')

	lo := b.block_instr2(.mul, block_id, b.i64_type, a, b_val)

	x0 := b.block_instr2(.and_, block_id, b.i64_type, a, mask32)
	x1 := b.block_instr2(.lshr, block_id, b.i64_type, a, c32)
	y0 := b.block_instr2(.and_, block_id, b.i64_type, b_val, mask32)
	y1 := b.block_instr2(.lshr, block_id, b.i64_type, b_val, c32)
	w0 := b.block_instr2(.mul, block_id, b.i64_type, x0, y0)
	x1y0 := b.block_instr2(.mul, block_id, b.i64_type, x1, y0)
	w0_hi := b.block_instr2(.lshr, block_id, b.i64_type, w0, c32)
	t := b.block_instr2(.add, block_id, b.i64_type, x1y0, w0_hi)
	t_lo := b.block_instr2(.and_, block_id, b.i64_type, t, mask32)
	x0y1 := b.block_instr2(.mul, block_id, b.i64_type, x0, y1)
	w1 := b.block_instr2(.add, block_id, b.i64_type, t_lo, x0y1)
	w2 := b.block_instr2(.lshr, block_id, b.i64_type, t, c32)
	x1y1 := b.block_instr2(.mul, block_id, b.i64_type, x1, y1)
	w1_hi := b.block_instr2(.lshr, block_id, b.i64_type, w1, c32)
	hi_tmp := b.block_instr2(.add, block_id, b.i64_type, x1y1, w2)
	hi := b.block_instr2(.add, block_id, b.i64_type, hi_tmp, w1_hi)

	return lo, hi
}

// wymix_inline supports wymix inline handling for Builder.
fn (mut b Builder) wymix_inline(block_id BlockID, a ValueID, b_val ValueID) ValueID {
	lo, hi := b.wymum_pair_inline(block_id, a, b_val)
	return b.block_instr2(.xor, block_id, b.i64_type, hi, lo)
}

// generate_wyhash_body supports generate wyhash body handling for Builder.
fn (mut b Builder) generate_wyhash_body(func_id int) {
	ptr_u8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_u32 := b.m.type_store.get_ptr(b.u32_type)
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)

	entry := b.m.add_block(func_id, 'entry')
	param_key := b.func_add_argument(func_id, ptr_u8, 'key')
	param_len := b.func_add_argument(func_id, b.i64_type, 'len')
	param_seed := b.func_add_argument(func_id, b.i64_type, 'seed')
	_ := b.func_add_argument(func_id, ptr_u64, 'secret')

	wyp0 := b.m.get_or_add_const(b.i64_type, '3257665815644502181')
	wyp1 := b.m.get_or_add_const(b.i64_type, '10067880064238660809')

	seed_xor_s0 := b.block_instr2(.xor, entry, b.i64_type, param_seed, wyp0)
	seed_xor_s0_len := b.block_instr2(.xor, entry, b.i64_type, seed_xor_s0, param_len)
	seed_mix := b.wymix_inline(entry, seed_xor_s0_len, wyp1)
	seed_init := b.block_instr2(.xor, entry, b.i64_type, param_seed, seed_mix)

	alloca_a := b.block_instr0(.alloca, entry, ptr_u64)
	alloca_b := b.block_instr0(.alloca, entry, ptr_u64)
	alloca_seed := b.block_instr0(.alloca, entry, ptr_u64)

	zero_64 := b.m.get_or_add_const(b.i64_type, '0')
	one_64 := b.m.get_or_add_const(b.i64_type, '1')
	two_64 := b.m.get_or_add_const(b.i64_type, '2')
	three_64 := b.m.get_or_add_const(b.i64_type, '3')
	four_64 := b.m.get_or_add_const(b.i64_type, '4')
	eight_64 := b.m.get_or_add_const(b.i64_type, '8')
	sixteen_64 := b.m.get_or_add_const(b.i64_type, '16')
	c32 := b.m.get_or_add_const(b.i64_type, '32')

	b.block_instr2(.store, entry, b.void_type, zero_64, alloca_a)
	b.block_instr2(.store, entry, b.void_type, zero_64, alloca_b)
	b.block_instr2(.store, entry, b.void_type, seed_init, alloca_seed)

	blk_short := b.m.add_block(func_id, 'wyhash_short')
	blk_short_4_16 := b.m.add_block(func_id, 'wyhash_short_4_16')
	blk_short_0_3 := b.m.add_block(func_id, 'wyhash_short_0_3')
	blk_wyr3 := b.m.add_block(func_id, 'wyhash_wyr3')
	blk_long := b.m.add_block(func_id, 'wyhash_long')
	blk_final := b.m.add_block(func_id, 'wyhash_final')

	len_le_16 := b.block_instr2(.le, entry, b.i1_type, param_len, sixteen_64)
	b.block_instr3(.br, entry, b.void_type, len_le_16, ValueID(blk_short), ValueID(blk_long))

	len_ge_4 := b.block_instr2(.ge, blk_short, b.i1_type, param_len, four_64)
	b.block_instr3(.br, blk_short, b.void_type, len_ge_4, ValueID(blk_short_4_16),
		ValueID(blk_short_0_3))

	p_as_u32 := b.block_instr1(.bitcast, blk_short_4_16, ptr_u32, param_key)
	wyr4_0 := b.block_instr1(.load, blk_short_4_16, b.u32_type, p_as_u32)
	wyr4_0_64 := b.block_instr1(.zext, blk_short_4_16, b.i64_type, wyr4_0)

	len_shr3 := b.block_instr2(.lshr, blk_short_4_16, b.i64_type, param_len, three_64)
	off1 := b.block_instr2(.shl, blk_short_4_16, b.i64_type, len_shr3, two_64)

	p_off1 := b.block_instr2(.add, blk_short_4_16, ptr_u8, param_key, off1)
	p_off1_u32 := b.block_instr1(.bitcast, blk_short_4_16, ptr_u32, p_off1)
	wyr4_1 := b.block_instr1(.load, blk_short_4_16, b.u32_type, p_off1_u32)
	wyr4_1_64 := b.block_instr1(.zext, blk_short_4_16, b.i64_type, wyr4_1)

	wyr4_0_shifted := b.block_instr2(.shl, blk_short_4_16, b.i64_type, wyr4_0_64, c32)
	a_val := b.block_instr2(.or_, blk_short_4_16, b.i64_type, wyr4_0_shifted, wyr4_1_64)

	len_m4 := b.block_instr2(.sub, blk_short_4_16, b.i64_type, param_len, four_64)
	p_lm4 := b.block_instr2(.add, blk_short_4_16, ptr_u8, param_key, len_m4)
	p_lm4_u32 := b.block_instr1(.bitcast, blk_short_4_16, ptr_u32, p_lm4)
	wyr4_2 := b.block_instr1(.load, blk_short_4_16, b.u32_type, p_lm4_u32)
	wyr4_2_64 := b.block_instr1(.zext, blk_short_4_16, b.i64_type, wyr4_2)

	lm4_moff1 := b.block_instr2(.sub, blk_short_4_16, b.i64_type, len_m4, off1)
	p_lm4_moff1 := b.block_instr2(.add, blk_short_4_16, ptr_u8, param_key, lm4_moff1)
	p_lm4_moff1_u32 := b.block_instr1(.bitcast, blk_short_4_16, ptr_u32, p_lm4_moff1)
	wyr4_3 := b.block_instr1(.load, blk_short_4_16, b.u32_type, p_lm4_moff1_u32)
	wyr4_3_64 := b.block_instr1(.zext, blk_short_4_16, b.i64_type, wyr4_3)

	wyr4_2_shifted := b.block_instr2(.shl, blk_short_4_16, b.i64_type, wyr4_2_64, c32)
	b_val := b.block_instr2(.or_, blk_short_4_16, b.i64_type, wyr4_2_shifted, wyr4_3_64)

	b.block_instr2(.store, blk_short_4_16, b.void_type, a_val, alloca_a)
	b.block_instr2(.store, blk_short_4_16, b.void_type, b_val, alloca_b)
	b.block_instr1(.jmp, blk_short_4_16, b.void_type, ValueID(blk_final))

	len_gt_0 := b.block_instr2(.gt, blk_short_0_3, b.i1_type, param_len, zero_64)
	b.block_instr3(.br, blk_short_0_3, b.void_type, len_gt_0, ValueID(blk_wyr3), ValueID(blk_final))

	byte_0 := b.block_instr1(.load, blk_wyr3, b.i8_type, param_key)
	byte_0_64 := b.block_instr1(.zext, blk_wyr3, b.i64_type, byte_0)
	byte_0_shifted := b.block_instr2(.shl, blk_wyr3, b.i64_type, byte_0_64, sixteen_64)

	len_shr1 := b.block_instr2(.lshr, blk_wyr3, b.i64_type, param_len, one_64)
	p_mid := b.block_instr2(.add, blk_wyr3, ptr_u8, param_key, len_shr1)
	byte_mid := b.block_instr1(.load, blk_wyr3, b.i8_type, p_mid)
	byte_mid_64 := b.block_instr1(.zext, blk_wyr3, b.i64_type, byte_mid)
	byte_mid_shifted := b.block_instr2(.shl, blk_wyr3, b.i64_type, byte_mid_64, eight_64)

	len_m1 := b.block_instr2(.sub, blk_wyr3, b.i64_type, param_len, one_64)
	p_last := b.block_instr2(.add, blk_wyr3, ptr_u8, param_key, len_m1)
	byte_last := b.block_instr1(.load, blk_wyr3, b.i8_type, p_last)
	byte_last_64 := b.block_instr1(.zext, blk_wyr3, b.i64_type, byte_last)

	a_tmp := b.block_instr2(.or_, blk_wyr3, b.i64_type, byte_0_shifted, byte_mid_shifted)
	a_wyr3 := b.block_instr2(.or_, blk_wyr3, b.i64_type, a_tmp, byte_last_64)

	b.block_instr2(.store, blk_wyr3, b.void_type, a_wyr3, alloca_a)
	b.block_instr1(.jmp, blk_wyr3, b.void_type, ValueID(blk_final))

	p_as_u64 := b.block_instr1(.bitcast, blk_long, ptr_u64, param_key)
	wyr8_first := b.block_instr1(.load, blk_long, b.i64_type, p_as_u64)

	p_plus_8 := b.block_instr2(.add, blk_long, ptr_u8, param_key, eight_64)
	p_plus_8_u64 := b.block_instr1(.bitcast, blk_long, ptr_u64, p_plus_8)
	wyr8_second := b.block_instr1(.load, blk_long, b.i64_type, p_plus_8_u64)

	len_m16 := b.block_instr2(.sub, blk_long, b.i64_type, param_len, sixteen_64)
	p_end_16 := b.block_instr2(.add, blk_long, ptr_u8, param_key, len_m16)
	p_end_16_u64 := b.block_instr1(.bitcast, blk_long, ptr_u64, p_end_16)
	wyr8_end_16 := b.block_instr1(.load, blk_long, b.i64_type, p_end_16_u64)

	len_m8 := b.block_instr2(.sub, blk_long, b.i64_type, param_len, eight_64)
	p_end_8 := b.block_instr2(.add, blk_long, ptr_u8, param_key, len_m8)
	p_end_8_u64 := b.block_instr1(.bitcast, blk_long, ptr_u64, p_end_8)
	wyr8_end_8 := b.block_instr1(.load, blk_long, b.i64_type, p_end_8_u64)

	mix_a := b.block_instr2(.xor, blk_long, b.i64_type, wyr8_first, wyp1)
	seed_cur := b.block_instr1(.load, blk_long, b.i64_type, alloca_seed)
	mix_b := b.block_instr2(.xor, blk_long, b.i64_type, wyr8_second, seed_cur)
	seed_long := b.wymix_inline(blk_long, mix_a, mix_b)

	b.block_instr2(.store, blk_long, b.void_type, seed_long, alloca_seed)
	b.block_instr2(.store, blk_long, b.void_type, wyr8_end_16, alloca_a)
	b.block_instr2(.store, blk_long, b.void_type, wyr8_end_8, alloca_b)
	b.block_instr1(.jmp, blk_long, b.void_type, ValueID(blk_final))

	final_a := b.block_instr1(.load, blk_final, b.i64_type, alloca_a)
	final_b := b.block_instr1(.load, blk_final, b.i64_type, alloca_b)
	final_seed := b.block_instr1(.load, blk_final, b.i64_type, alloca_seed)

	final_a_xor := b.block_instr2(.xor, blk_final, b.i64_type, final_a, wyp1)
	final_b_xor := b.block_instr2(.xor, blk_final, b.i64_type, final_b, final_seed)

	mum_lo, mum_hi := b.wymum_pair_inline(blk_final, final_a_xor, final_b_xor)

	lo_xor_s0 := b.block_instr2(.xor, blk_final, b.i64_type, mum_lo, wyp0)
	lo_xor_s0_len := b.block_instr2(.xor, blk_final, b.i64_type, lo_xor_s0, param_len)
	hi_xor_s1 := b.block_instr2(.xor, blk_final, b.i64_type, mum_hi, wyp1)

	final_result := b.wymix_inline(blk_final, lo_xor_s0_len, hi_xor_s1)
	b.block_instr1(.ret, blk_final, b.void_type, final_result)
}

// build_functions builds functions data for ssa.
fn (mut b Builder) build_functions() {
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .fn_decl {
			if b.skip_source_fn_in_module(node.value, cur_module) {
				continue
			}
			fn_name := ssa_fn_name_in_module(cur_module, node.value)
			// Dead-module elimination: skip every function from a skipped module.
			if b.skip_modules.len > 0 && cur_module in b.skip_modules {
				b.mark_fn_prototype(fn_name)
				continue
			}
			if b.used_fns.len > 0 && !b.source_fn_is_used(node.value, cur_module) {
				continue
			}
			// Hot reload: only the named function's body is materialized.
			if b.hot_fn.len > 0 && node.value != b.hot_fn && fn_name != b.hot_fn {
				b.mark_fn_prototype(fn_name)
				continue
			}
			// Signature-only build: register the prototype, skip the body.
			if b.skip_fn_bodies {
				b.mark_fn_prototype(fn_name)
				continue
			}
			b.build_function(node, cur_module)
		}
	}
	if b.top_level_main && !b.skip_fn_bodies && b.hot_fn.len == 0 {
		b.build_top_level_main()
	}
}

// mark_fn_prototype flags an already-registered function as a declaration whose
// body was intentionally not materialized (hot reload / skip-bodies / skip-module).
fn (mut b Builder) mark_fn_prototype(fn_name string) {
	if fn_id := b.fn_ids[fn_name] {
		b.m.func_set_prototype(fn_id, true)
	}
}

// main_module_name returns the program's primary module name (the first
// non-builtin module_decl), defaulting to 'main'.
fn (b &Builder) main_module_name() string {
	for node in b.a.nodes {
		if node.kind == .module_decl && node.value.len > 0 && node.value != 'builtin' {
			return node.value
		}
	}
	return 'main'
}

// register_type_aliases records `type Foo = Bar` aliases as SSA base TypeIDs so
// resolve_type can resolve them even when no checker is attached.
fn (mut b Builder) register_type_aliases() {
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		// A type_decl with no children and a non-empty `typ` is an alias
		// (`type Foo = Bar`); a type_decl with children is a sum type.
		if node.kind == .type_decl && node.children_count == 0 && node.typ.len > 0 {
			base := b.resolve_type_in_module(node.typ, cur_module)
			if base > 0 {
				b.type_aliases[node.value] = base
				qualified := qualify_type_name(node.value, cur_module)
				b.type_aliases[qualified] = base
			}
		}
	}
}

// checker_param_type_name supports checker param type name handling for Builder.
fn (b &Builder) checker_param_type_name(fn_name string, module_name string, idx int) ?string {
	if b.tc == unsafe { nil } {
		return none
	}
	candidates := checker_fn_name_candidates(fn_name, module_name)
	for candidate in candidates {
		if params := b.tc.fn_param_types[candidate] {
			if idx >= 0 && idx < params.len {
				return params[idx].name()
			}
		}
	}
	return none
}

// checker_fn_name_candidates supports checker fn name candidates handling for ssa.
fn checker_fn_name_candidates(fn_name string, module_name string) []string {
	mut candidates := []string{}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		candidates << module_name + '.' + fn_name
		if fn_name.contains('__') {
			candidates << module_name + '.' + fn_name.replace('__', '.')
		}
		if fn_name.contains('.') {
			candidates << module_name + '.' + fn_name.replace('.', '__')
			short_name := fn_name.all_after('.')
			candidates << module_name + '.' + short_name
		}
	}
	candidates << fn_name
	if fn_name.contains('__') {
		candidates << fn_name.replace('__', '.')
	}
	if fn_name.contains('.') {
		candidates << fn_name.replace('.', '__')
	}
	return candidates
}

// checker_return_type supports checker return type handling for Builder.
fn (mut b Builder) checker_return_type(fn_name string, module_name string) ?TypeID {
	if b.tc == unsafe { nil } {
		return none
	}
	candidates := checker_fn_name_candidates(fn_name, module_name)
	for candidate in candidates {
		if ret := b.tc.fn_ret_types[candidate] {
			return b.ssa_type_from_checker_type(ret)
		}
	}
	return none
}

// fn_is_used supports fn is used handling for Builder.
fn (b &Builder) fn_is_used(name string) bool {
	if name == 'main' {
		return true
	}
	if name in b.used_fns {
		return true
	}
	if name == 'bench.new' || name == 'bench__new' || name.ends_with('.bench.new')
		|| name.ends_with('__bench__new') {
		return true
	}
	if name in ['error_file_not_opened', 'error_size_of_type_0', 'vpopen', 'vpclose'] {
		return true
	}
	if name.contains('__') && name.replace('__', '.') in b.used_fns {
		return true
	}
	if !name.contains('.') {
		for used_name, _ in b.used_fns {
			normalized_used := used_name.replace('__', '.')
			if used_name.ends_with('.${name}') || normalized_used.ends_with('.${name}') {
				return true
			}
		}
	}
	if name.contains('.') {
		normalized_name := name.replace('__', '.')
		short_name := normalized_name.all_after_last('.')
		if short_name in b.used_fns {
			return true
		}
		for used_name, _ in b.used_fns {
			normalized_used := used_name.replace('__', '.')
			if used_name.ends_with('.${name}') || normalized_used.ends_with('.${name}')
				|| normalized_name.ends_with('.${normalized_used}') {
				return true
			}
		}
	}
	if name.starts_with('array_') || name.starts_with('string__') || name.starts_with('strings__')
		|| name.starts_with('strconv__') || name.starts_with('IError.') {
		return true
	}
	if name in ['new_map', 'memdup', 'int_str', 'bool_str', 'print', 'println', 'eprint', 'eprintln',
		'exit', 'arguments', 'tos', 'tos2', 'tos3', 'tos_clone', 'cstring_to_vstring',
		'malloc_noscan', 'isnil', 'error', 'error_with_code', 'join_path_single'] {
		return true
	}
	return false
}

fn (b &Builder) source_fn_is_used(name string, module_name string) bool {
	fn_name := ssa_fn_name_in_module(module_name, name)
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return b.fn_is_used(fn_name)
	}
	return b.fn_is_used(name) || b.fn_is_used(fn_name)
}

// build_function builds function data for ssa.
fn (mut b Builder) build_function(node flat.Node, module_name string) {
	fn_name := ssa_fn_name_in_module(module_name, node.value)
	func_id := b.fn_ids[fn_name]
	b.cur_module = module_name
	b.cur_func = func_id
	b.cur_func_ret_type = qualify_type_ref_name(node.typ, module_name)
	b.vars = map[string]ValueID{}
	b.var_type_names = map[string]string{}
	b.label_blocks = map[string]BlockID{}
	b.defer_body_ids = []flat.NodeId{}

	for v in b.m.values {
		if v.kind == .global {
			b.vars[v.name] = v.id
		}
	}

	entry := b.m.add_block(func_id, 'entry')
	b.cur_block = entry

	for i in 0 .. node.children_count {
		child := b.a.child_node(&node, i)
		if child.kind == .param {
			typ_name := if child.typ.starts_with('&') {
				child.typ
			} else {
				b.checker_param_type_name(node.value, module_name, b.m.funcs[func_id].params.len) or {
					child.typ
				}
			}
			typ := b.resolve_type_in_module(typ_name, module_name)
			param_val := b.m.add_value(.argument, typ, child.value, b.m.funcs[func_id].params.len)
			mut f := b.m.funcs[func_id]
			f.params << param_val
			b.m.funcs[func_id] = f

			alloca := b.emit0(.alloca, b.m.type_store.get_ptr(typ))
			b.emit2(.store, b.void_type, param_val, alloca)
			b.vars[child.value] = alloca
			b.var_type_names[child.value] = typ_name
		}
	}

	body_ids := b.fn_body_ids(node)
	for id in body_ids {
		b.build_stmt(id)
	}

	blk := b.m.blocks[b.cur_block]
	if blk.instrs.len == 0 || !b.is_terminator(blk.instrs.last()) {
		ret_type := b.m.funcs[func_id].typ
		if ret_type == b.void_type {
			b.emit_deferred_stmts()
			b.emit0(.ret, b.void_type)
		}
	}
}

// build_top_level_main builds top level main data for ssa.
fn (mut b Builder) build_top_level_main() {
	func_id := b.fn_ids['main'] or { return }
	b.cur_module = 'main'
	b.cur_func = func_id
	b.cur_func_ret_type = ''
	b.vars = map[string]ValueID{}
	b.var_type_names = map[string]string{}
	b.label_blocks = map[string]BlockID{}
	b.defer_body_ids = []flat.NodeId{}
	for v in b.m.values {
		if v.kind == .global {
			b.vars[v.name] = v.id
		}
	}
	entry := b.m.add_block(func_id, 'entry')
	b.cur_block = entry

	for id in b.top_level_stmt_ids() {
		b.build_stmt(id)
	}

	blk := b.m.blocks[b.cur_block]
	if blk.instrs.len == 0 || !b.is_terminator(blk.instrs.last()) {
		b.emit_deferred_stmts()
		b.emit0(.ret, b.void_type)
	}
}

// fn_body_ids supports fn body ids handling for Builder.
fn (b &Builder) fn_body_ids(node flat.Node) []flat.NodeId {
	mut ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := b.a.child(&node, i)
		child := b.a.nodes[int(child_id)]
		if child.kind != .param && !b.is_declaration_node(child) {
			ids << child_id
		}
	}
	return ids
}

// is_declaration_node reports whether is declaration node applies in ssa.
fn (b &Builder) is_declaration_node(node flat.Node) bool {
	return match node.kind {
		.fn_decl, .c_fn_decl, .struct_decl, .field_decl, .global_decl, .const_decl, .const_field,
		.enum_decl, .enum_field, .type_decl, .interface_decl, .interface_field, .import_decl,
		.module_decl, .directive {
			true
		}
		else {
			false
		}
	}
}

// is_terminator reports whether is terminator applies in ssa.
fn (b &Builder) is_terminator(val_id ValueID) bool {
	if val_id <= 0 || val_id >= b.m.values.len {
		return false
	}
	v := b.m.values[val_id]
	if v.kind != .instruction {
		return false
	}
	instr := b.m.instrs[v.index]
	return instr.op == .ret || instr.op == .br || instr.op == .jmp || instr.op == .unreachable
}

// valid_node_id supports valid node id handling for Builder.
fn (b &Builder) valid_node_id(id flat.NodeId) bool {
	return b.a != unsafe { nil } && int(id) >= 0 && int(id) < b.a.nodes.len
}

// ident_name supports ident name handling for Builder.
fn (b &Builder) ident_name(id flat.NodeId) string {
	if !b.valid_node_id(id) {
		return ''
	}
	node := b.a.nodes[int(id)]
	if node.kind != .ident {
		return ''
	}
	return node.value
}

// save_var_binding updates save var binding state for ssa.
fn (b &Builder) save_var_binding(name string) VarBinding {
	if name.len == 0 {
		return VarBinding{}
	}
	if addr := b.vars[name] {
		return VarBinding{
			exists: true
			addr:   addr
			typ:    b.var_type_names[name] or { '' }
		}
	}
	return VarBinding{}
}

// restore_var_binding supports restore var binding handling for Builder.
fn (mut b Builder) restore_var_binding(name string, binding VarBinding) {
	if name.len == 0 {
		return
	}
	if binding.exists {
		b.vars[name] = binding.addr
		if binding.typ.len > 0 {
			b.var_type_names[name] = binding.typ
		} else {
			b.var_type_names.delete(name)
		}
	} else {
		b.vars.delete(name)
		b.var_type_names.delete(name)
	}
}

// bind_loop_var supports bind loop var handling for Builder.
fn (mut b Builder) bind_loop_var(name string, typ TypeID, typ_name string) ValueID {
	if name.len == 0 {
		return ValueID(0)
	}
	slot := b.emit0(.alloca, b.m.type_store.get_ptr(typ))
	b.vars[name] = slot
	if typ_name.len > 0 {
		b.var_type_names[name] = typ_name
	}
	return slot
}

// for_in_container_type_name supports for in container type name handling for Builder.
fn (b &Builder) for_in_container_type_name(container_id flat.NodeId) string {
	checked := b.checked_expr_type_name(container_id)
	if checked.len > 0 && checked != 'unknown' {
		return checked.trim_left('&')
	}
	if !b.valid_node_id(container_id) {
		return ''
	}
	node := b.a.nodes[int(container_id)]
	if node.typ.len > 0 {
		return node.typ.trim_left('&')
	}
	if node.kind == .ident {
		return (b.var_type_names[node.value] or { '' }).trim_left('&')
	}
	if node.kind == .selector && node.children_count > 0 {
		base_type := b.for_in_container_type_name(b.a.child(&node, 0)).trim_left('&')
		field_key := base_type + '.' + node.value
		if field_type := b.struct_field_types[field_key] {
			return field_type.trim_left('&')
		}
		short_type := base_type.all_after('.')
		short_key := short_type + '.' + node.value
		if field_type := b.struct_field_types[short_key] {
			return field_type.trim_left('&')
		}
	}
	return ''
}

// for_in_array_elem_type_name supports for in array elem type name handling for Builder.
fn (b &Builder) for_in_array_elem_type_name(container_id flat.NodeId, container_type string) string {
	clean := container_type.trim_left('&')
	if clean.starts_with('[]') {
		return clean[2..]
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 && idx + 1 < clean.len {
			return clean[idx + 1..]
		}
	}
	if clean == 'string' {
		return 'u8'
	}
	if b.valid_node_id(container_id) {
		node := b.a.nodes[int(container_id)]
		if node.kind == .array_literal && node.children_count > 0 {
			first_id := b.a.child(&node, 0)
			first_type := b.checked_expr_type_name(first_id)
			if first_type.len > 0 && first_type != 'unknown' {
				return first_type
			}
		}
	}
	return 'int'
}

// build_stmt builds stmt data for ssa.
fn (mut b Builder) build_stmt(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.expr_stmt {
			expr_id := b.a.child(&node, 0)
			expr := b.a.nodes[int(expr_id)]
			if expr.kind == .infix && expr.op == .left_shift && expr.value == 'push' {
				b.build_array_push_expr(expr)
				return
			}
			b.build_expr(expr_id)
		}
		.decl_assign {
			b.build_decl_assign(node)
		}
		.assign {
			b.build_assign(node)
		}
		.selector_assign {
			b.build_selector_assign(node)
		}
		.index_assign {
			b.build_index_assign(node)
		}
		.return_stmt {
			if node.children_count > 0 {
				expr_id := b.a.child(&node, 0)
				ret_type := b.m.funcs[b.cur_func].typ
				mut val := if node.children_count > 1 && b.is_struct_type(ret_type) {
					b.build_multi_return_value(ret_type, node)
				} else {
					// Resolve `return .member` against the declared return type so a
					// duplicated enum-member name is not built as 0 (see build_assign_rhs).
					b.build_field_value(expr_id, b.cur_func_ret_type)
				}
				if b.is_option_type(ret_type) && b.value_type(val) != ret_type {
					expr_node := b.a.nodes[int(expr_id)]
					val = b.build_option_value(ret_type, expr_node.kind != .none_expr, val)
				}
				b.emit_deferred_stmts()
				b.emit1(.ret, b.void_type, val)
			} else {
				b.emit_deferred_stmts()
				b.emit0(.ret, b.void_type)
			}
		}
		.defer_stmt {
			if node.children_count > 0 {
				b.defer_body_ids << b.a.child(&node, 0)
			}
		}
		.for_stmt {
			b.build_for(node)
		}
		.for_in_stmt {
			b.build_for_in(node)
		}
		.break_stmt {
			if b.break_targets.len > 0 {
				target := b.break_targets.last()
				b.emit1(.jmp, b.void_type, ValueID(target))
			}
		}
		.continue_stmt {
			if b.continue_targets.len > 0 {
				target := b.continue_targets.last()
				b.emit1(.jmp, b.void_type, ValueID(target))
			}
		}
		.goto_stmt {
			target := b.label_block(node.value)
			b.emit1(.jmp, b.void_type, ValueID(target))
		}
		.label_stmt {
			target := b.label_block(node.value)
			if !b.current_block_terminated() {
				b.emit1(.jmp, b.void_type, ValueID(target))
			}
			b.cur_block = target
		}
		.if_expr {
			b.build_if(node)
		}
		.match_stmt {
			b.build_match_stmt(node)
		}
		.block {
			for i in 0 .. node.children_count {
				b.build_stmt(b.a.child(&node, i))
			}
		}
		.assert_stmt {
			b.build_assert(node)
		}
		.fn_decl, .c_fn_decl, .struct_decl, .field_decl, .global_decl, .const_decl, .const_field,
		.enum_decl, .enum_field, .type_decl, .interface_decl, .interface_field, .import_decl,
		.module_decl, .directive, .param {}
		.empty {}
		else {
			eprintln('build_stmt: unsupported node kind: ${node.kind}')
		}
	}
}

// build_multi_return_value builds multi return value data for ssa.
fn (mut b Builder) build_multi_return_value(ret_type TypeID, node flat.Node) ValueID {
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(ret_type))
	typ := b.m.type_store.types[ret_type]
	for i in 0 .. node.children_count {
		if i >= typ.fields.len {
			break
		}
		field_ptr := b.block_struct_field_ptr(b.cur_block, alloca, ret_type, i)
		mut value := b.build_expr(b.a.child(&node, i))
		field_type := typ.fields[i]
		if b.is_int_type(b.value_type(value)) && b.is_int_type(field_type) {
			value = b.coerce_int_value(value, field_type)
		}
		b.emit2(.store, b.void_type, value, field_ptr)
	}
	return b.emit1(.load, ret_type, alloca)
}

// emit_deferred_stmts emits emit deferred stmts output for ssa.
fn (mut b Builder) emit_deferred_stmts() {
	for i := b.defer_body_ids.len; i > 0; i-- {
		body_id := b.defer_body_ids[i - 1]
		if !b.valid_node_id(body_id) {
			continue
		}
		body := b.a.nodes[int(body_id)]
		if body.kind == .block {
			for j in 0 .. body.children_count {
				b.build_stmt(b.a.child(&body, j))
				if b.current_block_terminated() {
					return
				}
			}
		} else {
			b.build_stmt(body_id)
			if b.current_block_terminated() {
				return
			}
		}
	}
}

// label_block supports label block handling for Builder.
fn (mut b Builder) label_block(name string) BlockID {
	if block := b.label_blocks[name] {
		return block
	}
	block := b.m.add_block(b.cur_func, 'label_${name}')
	b.label_blocks[name] = block
	return block
}

// build_decl_assign builds decl assign data for ssa.
fn (mut b Builder) build_decl_assign(node flat.Node) {
	mut i := 0
	for i < node.children_count {
		lhs_id := b.a.child(&node, i)
		rhs_id := b.a.child(&node, i + 1)
		lhs := b.a.nodes[int(lhs_id)]
		rhs_val := b.build_expr(rhs_id)
		rhs_type := b.value_type(rhs_val)
		alloca := b.emit0(.alloca, b.m.type_store.get_ptr(rhs_type))
		b.emit2(.store, b.void_type, rhs_val, alloca)
		if lhs.kind == .ident {
			b.vars[lhs.value] = alloca
			declared_type := b.declared_v_type_name(lhs_id, rhs_id)
			rhs_type_name := b.checked_expr_type_name(rhs_id)
			b.var_type_names[lhs.value] = if declared_type.len > 0 {
				declared_type
			} else if rhs_type_name.len > 0 && rhs_type_name != 'unknown' {
				rhs_type_name
			} else {
				node.typ
			}
		}
		i += 2
	}
}

// build_assign builds assign data for ssa.
// build_assign_rhs builds an assignment RHS, resolving bare enum literals (`x = .member`)
// against the LHS's declared type. Without this context a duplicated enum-member name
// (e.g. `number`, which exists in several enums) is built as 0, because the checker types
// the bare literal as `int` and the member lookup is ambiguous. Mirrors struct-init field
// value resolution (`build_field_value`).
fn (mut b Builder) build_assign_rhs(lhs flat.Node, rhs_id flat.NodeId) ValueID {
	mut type_name := ''
	if lhs.kind == .ident {
		type_name = b.var_type_names[lhs.value] or { '' }
	} else if lhs.kind == .selector {
		type_name = b.selector_type_name(lhs) or { '' }
	}
	if type_name.len > 0 {
		return b.build_field_value(rhs_id, type_name)
	}
	return b.build_expr(rhs_id)
}

fn (mut b Builder) build_assign(node flat.Node) {
	mut i := 0
	for i < node.children_count {
		lhs_id := b.a.child(&node, i)
		rhs_id := b.a.child(&node, i + 1)
		lhs := b.a.nodes[int(lhs_id)]

		if lhs.kind == .ident && lhs.value == '_' {
			b.build_expr(rhs_id)
			i += 2
			continue
		}
		if lhs.kind == .ident {
			if addr := b.vars[lhs.value] {
				if node.op == .assign {
					rhs_val := b.build_assign_rhs(lhs, rhs_id)
					b.emit2(.store, b.void_type, rhs_val, addr)
				} else {
					cur := b.emit1(.load, b.deref_type(addr), addr)
					rhs_val := b.build_expr(rhs_id)
					op := b.compound_to_op(node.op)
					result := b.emit2(op, b.value_type(cur), cur, rhs_val)
					b.emit2(.store, b.void_type, result, addr)
				}
			}
		} else if lhs.kind == .selector || lhs.kind == .index {
			addr := if lhs.kind == .selector {
				b.build_selector_addr(lhs)
			} else {
				b.build_lvalue_addr(lhs_id)
			}
			if node.op == .assign {
				rhs_val := b.build_assign_rhs(lhs, rhs_id)
				b.emit2(.store, b.void_type, rhs_val, addr)
			} else {
				field_type := b.deref_type(addr)
				cur := b.emit1(.load, field_type, addr)
				rhs_val := b.build_expr(rhs_id)
				op := b.compound_to_op(node.op)
				result := b.emit2(op, field_type, cur, rhs_val)
				b.emit2(.store, b.void_type, result, addr)
			}
		}
		i += 2
	}
}

// build_selector_assign builds selector assign data for ssa.
fn (mut b Builder) build_selector_assign(node flat.Node) {
	mut i := 0
	for i < node.children_count {
		lhs_id := b.a.child(&node, i)
		rhs_id := b.a.child(&node, i + 1)
		lhs := b.a.nodes[int(lhs_id)]

		if lhs.kind == .selector {
			field_ptr := b.build_selector_addr(lhs)
			if node.op == .assign {
				rhs_val := b.build_assign_rhs(lhs, rhs_id)
				b.emit2(.store, b.void_type, rhs_val, field_ptr)
			} else {
				field_type := b.deref_type(field_ptr)
				cur := b.emit1(.load, field_type, field_ptr)
				rhs_val := b.build_expr(rhs_id)
				op := b.compound_to_op(node.op)
				result := b.emit2(op, field_type, cur, rhs_val)
				b.emit2(.store, b.void_type, result, field_ptr)
			}
		}
		i += 2
	}
}

// build_index_assign builds index assign data for ssa.
fn (mut b Builder) build_index_assign(node flat.Node) {
	mut i := 0
	for i < node.children_count {
		lhs_id := b.a.child(&node, i)
		rhs_id := b.a.child(&node, i + 1)
		addr := b.build_lvalue_addr(lhs_id)
		if node.op == .assign {
			rhs_val := b.build_expr(rhs_id)
			b.emit2(.store, b.void_type, rhs_val, addr)
		} else {
			field_type := b.deref_type(addr)
			cur := b.emit1(.load, field_type, addr)
			rhs_val := b.build_expr(rhs_id)
			op := b.compound_to_op(node.op)
			result := b.emit2(op, field_type, cur, rhs_val)
			b.emit2(.store, b.void_type, result, addr)
		}
		i += 2
	}
}

// build_for builds for data for ssa.
fn (mut b Builder) build_for(node flat.Node) {
	init_id := b.a.child(&node, 0)
	cond_id := b.a.child(&node, 1)
	post_id := b.a.child(&node, 2)
	init_node := b.a.nodes[int(init_id)]
	cond_node := b.a.nodes[int(cond_id)]
	post_node := b.a.nodes[int(post_id)]

	if init_node.kind != .empty {
		b.build_stmt(init_id)
	}

	cond_block := b.m.add_block(b.cur_func, 'for_cond')
	body_block := b.m.add_block(b.cur_func, 'for_body')
	post_block := b.m.add_block(b.cur_func, 'for_post')
	exit_block := b.m.add_block(b.cur_func, 'for_exit')

	b.emit1(.jmp, b.void_type, ValueID(cond_block))
	b.cur_block = cond_block

	if cond_node.kind != .empty {
		cond_val := b.build_expr(cond_id)
		b.emit3(.br, b.void_type, cond_val, ValueID(body_block), ValueID(exit_block))
	} else {
		b.emit1(.jmp, b.void_type, ValueID(body_block))
	}

	b.cur_block = body_block
	b.break_targets << exit_block
	b.continue_targets << if post_node.kind != .empty { post_block } else { cond_block }

	for i in 3 .. node.children_count {
		b.build_stmt(b.a.child(&node, i))
	}
	b.break_targets.delete_last()
	b.continue_targets.delete_last()

	blk := b.m.blocks[b.cur_block]
	if blk.instrs.len == 0 || !b.is_terminator(blk.instrs.last()) {
		if post_node.kind != .empty {
			b.emit1(.jmp, b.void_type, ValueID(post_block))
		} else {
			b.emit1(.jmp, b.void_type, ValueID(cond_block))
		}
	}

	if post_node.kind != .empty {
		b.cur_block = post_block
		b.build_stmt(post_id)
		post_blk := b.m.blocks[b.cur_block]
		if post_blk.instrs.len == 0 || !b.is_terminator(post_blk.instrs.last()) {
			b.emit1(.jmp, b.void_type, ValueID(cond_block))
		}
	}

	b.cur_block = exit_block
}

// build_for_in builds for in data for ssa.
fn (mut b Builder) build_for_in(node flat.Node) {
	if node.children_count < 3 {
		return
	}
	key_id := b.a.child(&node, 0)
	val_id := b.a.child(&node, 1)
	container_id := b.a.child(&node, 2)
	body_start := if node.value == '4' { 4 } else { 3 }
	if body_start == 4 {
		end_id := b.a.child(&node, 3)
		b.build_range_for_in(node, key_id, container_id, end_id, body_start)
		return
	}
	container_type := b.for_in_container_type_name(container_id)
	if container_type == 'string' {
		b.build_string_for_in(node, key_id, val_id, container_id, body_start)
		return
	}
	if container_type.starts_with('map[') || container_type == 'map' || container_type == 'Map' {
		b.build_map_for_in(node, key_id, val_id, container_id, body_start, container_type)
		return
	}
	b.build_array_for_in(node, key_id, val_id, container_id, body_start, container_type)
}

// build_range_for_in builds range for in data for ssa.
fn (mut b Builder) build_range_for_in(node flat.Node, key_id flat.NodeId, start_id flat.NodeId, end_id flat.NodeId, body_start int) {
	start_val := b.build_expr(start_id)
	end_val := b.build_expr(end_id)
	idx_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i64_type))
	b.emit2(.store, b.void_type, start_val, idx_alloca)
	key_name := b.ident_name(key_id)
	old_key := b.save_var_binding(key_name)
	key_alloca := b.bind_loop_var(key_name, b.i64_type, 'int')

	cond_block := b.m.add_block(b.cur_func, 'for_in_range_cond')
	body_block := b.m.add_block(b.cur_func, 'for_in_range_body')
	post_block := b.m.add_block(b.cur_func, 'for_in_range_post')
	exit_block := b.m.add_block(b.cur_func, 'for_in_range_exit')
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.cur_block = cond_block
	idx := b.emit1(.load, b.i64_type, idx_alloca)
	in_range := b.emit2(.lt, b.i1_type, idx, end_val)
	b.emit3(.br, b.void_type, in_range, ValueID(body_block), ValueID(exit_block))

	b.cur_block = body_block
	if key_alloca > 0 {
		b.emit2(.store, b.void_type, idx, key_alloca)
	}
	b.break_targets << exit_block
	b.continue_targets << post_block
	b.build_for_in_body(node, body_start)
	b.break_targets.delete_last()
	b.continue_targets.delete_last()
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(post_block))
	}

	b.cur_block = post_block
	one := b.m.get_or_add_const(b.i64_type, '1')
	cur_idx := b.emit1(.load, b.i64_type, idx_alloca)
	next_idx := b.emit2(.add, b.i64_type, cur_idx, one)
	b.emit2(.store, b.void_type, next_idx, idx_alloca)
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.restore_var_binding(key_name, old_key)
	b.cur_block = exit_block
}

// build_array_for_in builds array for in data for ssa.
fn (mut b Builder) build_array_for_in(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, body_start int, container_type string) {
	arr_val := b.build_expr(container_id)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	arr_alloca := b.emit0(.alloca, ptr_array)
	b.emit2(.store, b.void_type, arr_val, arr_alloca)
	idx_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i64_type))
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.emit2(.store, b.void_type, zero, idx_alloca)

	elem_type_name := b.for_in_array_elem_type_name(container_id, container_type)
	elem_type := b.resolve_type(elem_type_name)
	has_value_var := b.valid_node_id(val_id)
	key_name := b.ident_name(key_id)
	val_name := if has_value_var { b.ident_name(val_id) } else { key_name }
	old_key := b.save_var_binding(key_name)
	old_val := if has_value_var { b.save_var_binding(val_name) } else { VarBinding{} }
	key_alloca := if has_value_var {
		b.bind_loop_var(key_name, b.i64_type, 'int')
	} else {
		ValueID(0)
	}
	val_alloca := b.bind_loop_var(val_name, elem_type, elem_type_name)

	cond_block := b.m.add_block(b.cur_func, 'for_in_array_cond')
	body_block := b.m.add_block(b.cur_func, 'for_in_array_body')
	post_block := b.m.add_block(b.cur_func, 'for_in_array_post')
	exit_block := b.m.add_block(b.cur_func, 'for_in_array_exit')
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.cur_block = cond_block
	idx := b.emit1(.load, b.i64_type, idx_alloca)
	len := b.block_load_array_int_field(cond_block, arr_alloca, 2)
	in_range := b.emit2(.lt, b.i1_type, idx, len)
	b.emit3(.br, b.void_type, in_range, ValueID(body_block), ValueID(exit_block))

	b.cur_block = body_block
	body_idx := b.emit1(.load, b.i64_type, idx_alloca)
	if key_alloca > 0 {
		b.emit2(.store, b.void_type, body_idx, key_alloca)
	}
	if val_alloca > 0 {
		elem_ptr := b.build_array_data_index_addr(arr_alloca, body_idx, elem_type)
		elem_val := b.emit1(.load, elem_type, elem_ptr)
		b.emit2(.store, b.void_type, elem_val, val_alloca)
	}
	b.break_targets << exit_block
	b.continue_targets << post_block
	b.build_for_in_body(node, body_start)
	b.break_targets.delete_last()
	b.continue_targets.delete_last()
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(post_block))
	}

	b.cur_block = post_block
	one := b.m.get_or_add_const(b.i64_type, '1')
	cur_idx := b.emit1(.load, b.i64_type, idx_alloca)
	next_idx := b.emit2(.add, b.i64_type, cur_idx, one)
	b.emit2(.store, b.void_type, next_idx, idx_alloca)
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.restore_var_binding(key_name, old_key)
	if has_value_var {
		b.restore_var_binding(val_name, old_val)
	}
	b.cur_block = exit_block
}

// build_string_for_in builds string for in data for ssa.
fn (mut b Builder) build_string_for_in(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, body_start int) {
	str_val := b.build_expr(container_id)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	str_alloca := b.emit0(.alloca, ptr_string)
	b.emit2(.store, b.void_type, str_val, str_alloca)
	idx_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i64_type))
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.emit2(.store, b.void_type, zero, idx_alloca)

	has_value_var := b.valid_node_id(val_id)
	key_name := b.ident_name(key_id)
	val_name := if has_value_var { b.ident_name(val_id) } else { key_name }
	old_key := b.save_var_binding(key_name)
	old_val := if has_value_var { b.save_var_binding(val_name) } else { VarBinding{} }
	key_alloca := if has_value_var {
		b.bind_loop_var(key_name, b.i64_type, 'int')
	} else {
		ValueID(0)
	}
	val_alloca := b.bind_loop_var(val_name, b.i8_type, 'u8')

	cond_block := b.m.add_block(b.cur_func, 'for_in_string_cond')
	body_block := b.m.add_block(b.cur_func, 'for_in_string_body')
	post_block := b.m.add_block(b.cur_func, 'for_in_string_post')
	exit_block := b.m.add_block(b.cur_func, 'for_in_string_exit')
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.cur_block = cond_block
	idx := b.emit1(.load, b.i64_type, idx_alloca)
	len_ptr := b.get_field_ptr(str_alloca, 'len')
	len32 := b.emit1(.load, b.i32_type, len_ptr)
	len := b.emit1(.zext, b.i64_type, len32)
	in_range := b.emit2(.lt, b.i1_type, idx, len)
	b.emit3(.br, b.void_type, in_range, ValueID(body_block), ValueID(exit_block))

	b.cur_block = body_block
	body_idx := b.emit1(.load, b.i64_type, idx_alloca)
	if key_alloca > 0 {
		b.emit2(.store, b.void_type, body_idx, key_alloca)
	}
	data_ptr := b.get_field_ptr(str_alloca, 'str')
	data := b.emit1(.load, b.m.type_store.get_ptr(b.i8_type), data_ptr)
	ch_ptr := b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), data, body_idx)
	ch := b.emit1(.load, b.i8_type, ch_ptr)
	if val_alloca > 0 {
		b.emit2(.store, b.void_type, ch, val_alloca)
	}
	b.break_targets << exit_block
	b.continue_targets << post_block
	b.build_for_in_body(node, body_start)
	b.break_targets.delete_last()
	b.continue_targets.delete_last()
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(post_block))
	}

	b.cur_block = post_block
	one := b.m.get_or_add_const(b.i64_type, '1')
	cur_idx := b.emit1(.load, b.i64_type, idx_alloca)
	next_idx := b.emit2(.add, b.i64_type, cur_idx, one)
	b.emit2(.store, b.void_type, next_idx, idx_alloca)
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.restore_var_binding(key_name, old_key)
	if has_value_var {
		b.restore_var_binding(val_name, old_val)
	}
	b.cur_block = exit_block
}

// build_map_for_in builds map for in data for ssa.
fn (mut b Builder) build_map_for_in(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, body_start int, container_type string) {
	key_type_name, val_type_name := map_type_parts(container_type)
	key_type := if key_type_name.len > 0 { b.resolve_type(key_type_name) } else { b.str_type }
	val_type := if val_type_name.len > 0 { b.resolve_type(val_type_name) } else { b.i64_type }
	map_val := b.build_expr(container_id)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	map_alloca := b.emit0(.alloca, ptr_map)
	b.emit2(.store, b.void_type, map_val, map_alloca)
	state := b.map_state_ptr(b.cur_block, map_alloca)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.emit2(.ne, b.i1_type, state, zero_state)

	idx_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i64_type))
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.emit2(.store, b.void_type, zero, idx_alloca)

	has_value_var := b.valid_node_id(val_id)
	key_name := b.ident_name(key_id)
	val_name := if has_value_var { b.ident_name(val_id) } else { '' }
	old_key := b.save_var_binding(key_name)
	old_val := if has_value_var { b.save_var_binding(val_name) } else { VarBinding{} }
	key_alloca := b.bind_loop_var(key_name, key_type, key_type_name)
	val_alloca := if has_value_var {
		b.bind_loop_var(val_name, val_type, val_type_name)
	} else {
		ValueID(0)
	}

	cond_block := b.m.add_block(b.cur_func, 'for_in_map_cond')
	body_block := b.m.add_block(b.cur_func, 'for_in_map_body')
	post_block := b.m.add_block(b.cur_func, 'for_in_map_post')
	exit_block := b.m.add_block(b.cur_func, 'for_in_map_exit')
	b.emit3(.br, b.void_type, has_state, ValueID(cond_block), ValueID(exit_block))

	b.cur_block = cond_block
	idx := b.emit1(.load, b.i64_type, idx_alloca)
	len_ptr := b.map_state_field_ptr(cond_block, state, 3)
	len := b.emit1(.load, b.i64_type, len_ptr)
	in_range := b.emit2(.lt, b.i1_type, idx, len)
	b.emit3(.br, b.void_type, in_range, ValueID(body_block), ValueID(exit_block))

	b.cur_block = body_block
	body_idx := b.emit1(.load, b.i64_type, idx_alloca)
	keys_ptr := b.map_state_field_ptr(body_block, state, 0)
	vals_ptr := b.map_state_field_ptr(body_block, state, 1)
	key_size_ptr := b.map_state_field_ptr(body_block, state, 4)
	val_size_ptr := b.map_state_field_ptr(body_block, state, 5)
	keys := b.emit1(.load, b.m.type_store.get_ptr(b.i8_type), keys_ptr)
	vals := b.emit1(.load, b.m.type_store.get_ptr(b.i8_type), vals_ptr)
	key_size := b.emit1(.load, b.i64_type, key_size_ptr)
	val_size := b.emit1(.load, b.i64_type, val_size_ptr)
	key_off := b.emit2(.mul, b.i64_type, body_idx, key_size)
	val_off := b.emit2(.mul, b.i64_type, body_idx, val_size)
	key_raw := b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), keys, key_off)
	val_raw := b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), vals, val_off)
	if key_alloca > 0 {
		key_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(key_type), key_raw)
		key_val := b.emit1(.load, key_type, key_ptr)
		b.emit2(.store, b.void_type, key_val, key_alloca)
	}
	if val_alloca > 0 {
		val_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(val_type), val_raw)
		val_val := b.emit1(.load, val_type, val_ptr)
		b.emit2(.store, b.void_type, val_val, val_alloca)
	}
	b.break_targets << exit_block
	b.continue_targets << post_block
	b.build_for_in_body(node, body_start)
	b.break_targets.delete_last()
	b.continue_targets.delete_last()
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(post_block))
	}

	b.cur_block = post_block
	one := b.m.get_or_add_const(b.i64_type, '1')
	cur_idx := b.emit1(.load, b.i64_type, idx_alloca)
	next_idx := b.emit2(.add, b.i64_type, cur_idx, one)
	b.emit2(.store, b.void_type, next_idx, idx_alloca)
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.restore_var_binding(key_name, old_key)
	if has_value_var {
		b.restore_var_binding(val_name, old_val)
	}
	b.cur_block = exit_block
}

// build_for_in_body builds for in body data for ssa.
fn (mut b Builder) build_for_in_body(node flat.Node, body_start int) {
	for i in body_start .. node.children_count {
		b.build_stmt(b.a.child(&node, i))
	}
}

// build_if builds if data for ssa.
fn (mut b Builder) build_if(node flat.Node) {
	cond_node := b.a.child_node(&node, 0)
	then_block := b.m.add_block(b.cur_func, 'if_then')
	merge_block := b.m.add_block(b.cur_func, 'if_merge')
	mut guard := IfGuardState{}

	if cond_node.kind == .empty {
		b.emit1(.jmp, b.void_type, ValueID(then_block))
	} else {
		mut cond_val := ValueID(0)
		if cond_node.kind == .decl_assign {
			cond_val, guard = b.build_if_guard_condition(*cond_node)
		} else {
			cond_val = b.build_expr(b.a.child(&node, 0))
		}
		if node.children_count > 2 {
			else_block := b.m.add_block(b.cur_func, 'if_else')
			b.emit3(.br, b.void_type, cond_val, ValueID(then_block), ValueID(else_block))

			b.cur_block = else_block
			else_node := b.a.child_node(&node, 2)
			if else_node.kind == .if_expr {
				b.build_if(*else_node)
			} else if else_node.kind == .block {
				for i in 0 .. else_node.children_count {
					b.build_stmt(b.a.child(else_node, i))
				}
			}
			eblk := b.m.blocks[b.cur_block]
			if eblk.instrs.len == 0 || !b.is_terminator(eblk.instrs.last()) {
				b.emit1(.jmp, b.void_type, ValueID(merge_block))
			}
		} else {
			b.emit3(.br, b.void_type, cond_val, ValueID(then_block), ValueID(merge_block))
		}
	}

	b.cur_block = then_block
	if guard.active {
		b.activate_if_guard(guard)
	}
	then_node := b.a.child_node(&node, 1)
	for i in 0 .. then_node.children_count {
		b.build_stmt(b.a.child(then_node, i))
	}
	tblk := b.m.blocks[b.cur_block]
	if tblk.instrs.len == 0 || !b.is_terminator(tblk.instrs.last()) {
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}
	if guard.active {
		b.restore_var_binding(guard.name, guard.old)
	}

	b.cur_block = merge_block
}

// build_if_guard_condition builds if guard condition data for ssa.
fn (mut b Builder) build_if_guard_condition(node flat.Node) (ValueID, IfGuardState) {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i1_type, '1'), IfGuardState{}
	}
	lhs_id := b.a.child(&node, 0)
	rhs_id := b.a.child(&node, 1)
	lhs := b.a.nodes[int(lhs_id)]
	rhs := b.a.nodes[int(rhs_id)]
	if lhs.kind == .ident && rhs.kind == .index && rhs.children_count >= 2 && rhs.value != 'range' {
		ok, cond, guard := b.build_map_index_if_guard(lhs.value, rhs)
		if ok {
			return cond, guard
		}
	}
	if lhs.kind == .ident {
		ok, cond, guard := b.build_option_if_guard(lhs.value, rhs_id)
		if ok {
			return cond, guard
		}
	}
	b.build_decl_assign(node)
	return b.m.get_or_add_const(b.i1_type, '1'), IfGuardState{}
}

// build_option_if_guard builds option if guard data for ssa.
fn (mut b Builder) build_option_if_guard(name string, rhs_id flat.NodeId) (bool, ValueID, IfGuardState) {
	if name.len == 0 {
		return false, b.m.get_or_add_const(b.i1_type, '0'), IfGuardState{}
	}
	opt_val := b.build_expr(rhs_id)
	opt_typ := b.value_type(opt_val)
	value_typ := b.option_value_type(opt_typ)
	if value_typ == TypeID(0) || value_typ == b.void_type {
		return false, b.m.get_or_add_const(b.i1_type, '0'), IfGuardState{}
	}
	opt_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(opt_typ))
	b.emit2(.store, b.void_type, opt_val, opt_alloca)
	ok_ptr := b.get_field_ptr(opt_alloca, 'ok')
	cond := b.emit1(.load, b.i1_type, ok_ptr)
	value_ptr := b.get_field_ptr(opt_alloca, 'value')
	guard := IfGuardState{
		active:    true
		name:      name
		old:       b.save_var_binding(name)
		slot:      b.emit0(.alloca, b.m.type_store.get_ptr(value_typ))
		value_ptr: value_ptr
		typ:       value_typ
		typ_name:  b.option_payload_type_name(rhs_id)
	}
	return true, cond, guard
}

// build_map_index_if_guard builds map index if guard data for ssa.
fn (mut b Builder) build_map_index_if_guard(name string, index_node flat.Node) (bool, ValueID, IfGuardState) {
	base_id := b.a.child(&index_node, 0)
	key_id := b.a.child(&index_node, 1)
	map_type_name := b.expr_type_name_for_map(base_id)
	key_type_name, val_type_name := map_type_parts(map_type_name)
	if name.len == 0 || key_type_name.len == 0 || val_type_name.len == 0 {
		return false, b.m.get_or_add_const(b.i1_type, '0'), IfGuardState{}
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	val_type := b.resolve_type(val_type_name)
	ptr_val_type := b.m.type_store.get_ptr(val_type)
	map_ptr := b.map_expr_ptr(base_id)
	key_val := b.build_expr(key_id)
	key_type := b.resolve_type(key_type_name)
	key_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(key_type))
	b.emit2(.store, b.void_type, key_val, key_alloca)
	key_ptr := if b.value_type(key_alloca) == ptr_i8 {
		key_alloca
	} else {
		b.emit1(.bitcast, ptr_i8, key_alloca)
	}
	get_ref := b.m.add_value(.func_ref, ptr_i8, 'map__get_check', b.fn_ids['map__get_check'])
	value_ptr := b.emit3(.call, ptr_i8, get_ref, map_ptr, key_ptr)
	zero_ptr := b.m.get_or_add_const(ptr_i8, '0')
	found := b.emit2(.ne, b.i1_type, value_ptr, zero_ptr)
	slot := b.emit0(.alloca, ptr_val_type)
	guard := IfGuardState{
		active:    true
		name:      name
		old:       b.save_var_binding(name)
		slot:      slot
		value_ptr: value_ptr
		typ:       val_type
		typ_name:  val_type_name
	}
	return true, found, guard
}

// activate_if_guard supports activate if guard handling for Builder.
fn (mut b Builder) activate_if_guard(guard IfGuardState) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_val_type := b.m.type_store.get_ptr(guard.typ)
	typed_value_ptr := if ptr_val_type == ptr_i8 {
		guard.value_ptr
	} else {
		b.emit1(.bitcast, ptr_val_type, guard.value_ptr)
	}
	value := b.emit1(.load, guard.typ, typed_value_ptr)
	b.emit2(.store, b.void_type, value, guard.slot)
	b.vars[guard.name] = guard.slot
	if guard.typ_name.len > 0 {
		b.var_type_names[guard.name] = guard.typ_name
	}
}

// build_if_value builds if value data for ssa.
fn (mut b Builder) build_if_value(id flat.NodeId, node flat.Node) ValueID {
	if node.children_count < 3 {
		b.build_if(node)
		return b.default_value_for_type(b.if_value_result_type(id, []ValueID{}))
	}
	cond_node := b.a.child_node(&node, 0)
	then_block := b.m.add_block(b.cur_func, 'if_value_then')
	else_block := b.m.add_block(b.cur_func, 'if_value_else')
	merge_block := b.m.add_block(b.cur_func, 'if_value_merge')

	if cond_node.kind == .empty {
		b.emit1(.jmp, b.void_type, ValueID(then_block))
	} else {
		cond_val := b.build_expr(b.a.child(&node, 0))
		b.emit3(.br, b.void_type, cond_val, ValueID(then_block), ValueID(else_block))
	}

	mut incoming_values := []ValueID{}
	mut incoming_blocks := []BlockID{}

	b.cur_block = then_block
	if then_value := b.build_branch_value(b.a.child(&node, 1), b.a.child_node(&node, 1)) {
		then_end := b.cur_block
		if !b.current_block_terminated() {
			incoming_values << then_value
			incoming_blocks << then_end
			b.emit1(.jmp, b.void_type, ValueID(merge_block))
		}
	} else if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}

	b.cur_block = else_block
	if else_value := b.build_branch_value(b.a.child(&node, 2), b.a.child_node(&node, 2)) {
		else_end := b.cur_block
		if !b.current_block_terminated() {
			incoming_values << else_value
			incoming_blocks << else_end
			b.emit1(.jmp, b.void_type, ValueID(merge_block))
		}
	} else if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}

	b.cur_block = merge_block
	result_type := b.if_value_result_type(id, incoming_values)
	if incoming_values.len == 0 {
		return b.default_value_for_type(result_type)
	}
	if incoming_values.len == 1 {
		return incoming_values[0]
	}
	mut operands := []ValueID{}
	for i, value in incoming_values {
		operands << value
		operands << ValueID(incoming_blocks[i])
	}
	return b.m.add_instr(.phi, b.cur_block, result_type, operands)
}

// build_branch_value builds branch value data for ssa.
fn (mut b Builder) build_branch_value(id flat.NodeId, node &flat.Node) ?ValueID {
	if int(id) < 0 {
		return none
	}
	if node.kind == .block {
		if node.children_count == 0 {
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		for i in 0 .. node.children_count - 1 {
			b.build_stmt(b.a.child(node, i))
			if b.current_block_terminated() {
				return none
			}
		}
		last_id := b.a.child(node, node.children_count - 1)
		last := b.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			return b.build_expr(b.a.child(&last, 0))
		}
		if last.kind == .match_stmt {
			return b.build_match_value(last_id, last)
		}
		if last.kind == .if_expr {
			return b.build_if_value(last_id, last)
		}
		if b.is_stmt_kind(last.kind) {
			b.build_stmt(last_id)
			return none
		}
		return b.build_expr(last_id)
	}
	if node.kind == .if_expr {
		return b.build_if_value(id, *node)
	}
	if node.kind == .match_stmt {
		return b.build_match_value(id, *node)
	}
	if b.is_stmt_kind(node.kind) {
		b.build_stmt(id)
		return none
	}
	return b.build_expr(id)
}

// build_match_stmt builds match stmt data for ssa.
fn (mut b Builder) build_match_stmt(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	subject := b.build_expr(b.a.child(&node, 0))
	exit_block := b.m.add_block(b.cur_func, 'match_exit')

	for i in 1 .. node.children_count {
		branch := b.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		is_else := branch.value == 'else'
		body_block := b.m.add_block(b.cur_func, 'match_body')
		next_block := if i + 1 < node.children_count {
			b.m.add_block(b.cur_func, 'match_next')
		} else {
			exit_block
		}
		if is_else {
			b.emit1(.jmp, b.void_type, ValueID(body_block))
		} else {
			cond := b.build_match_condition(subject, *branch)
			b.emit3(.br, b.void_type, cond, ValueID(body_block), ValueID(next_block))
		}

		b.cur_block = body_block
		body_start := b.match_branch_body_start(*branch)
		for j in body_start .. branch.children_count {
			b.build_stmt(b.a.child(branch, j))
			if b.current_block_terminated() {
				break
			}
		}
		if !b.current_block_terminated() {
			b.emit1(.jmp, b.void_type, ValueID(exit_block))
		}
		b.cur_block = next_block
	}
}

// build_match_value builds match value data for ssa.
fn (mut b Builder) build_match_value(id flat.NodeId, node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.default_value_for_type(b.if_value_result_type(id, []ValueID{}))
	}
	subject := b.build_expr(b.a.child(&node, 0))
	merge_block := b.m.add_block(b.cur_func, 'match_merge')
	mut incoming_values := []ValueID{}
	mut incoming_blocks := []BlockID{}

	for i in 1 .. node.children_count {
		branch := b.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		is_else := branch.value == 'else'
		body_block := b.m.add_block(b.cur_func, 'match_value_body')
		next_block := if i + 1 < node.children_count {
			b.m.add_block(b.cur_func, 'match_value_next')
		} else {
			merge_block
		}
		if is_else {
			b.emit1(.jmp, b.void_type, ValueID(body_block))
		} else {
			cond := b.build_match_condition(subject, *branch)
			b.emit3(.br, b.void_type, cond, ValueID(body_block), ValueID(next_block))
		}

		b.cur_block = body_block
		if value := b.build_match_branch_value(*branch) {
			end_block := b.cur_block
			if !b.current_block_terminated() {
				incoming_values << value
				incoming_blocks << end_block
				b.emit1(.jmp, b.void_type, ValueID(merge_block))
			}
		} else if !b.current_block_terminated() {
			b.emit1(.jmp, b.void_type, ValueID(merge_block))
		}
		b.cur_block = next_block
	}

	b.cur_block = merge_block
	result_type := b.if_value_result_type(id, incoming_values)
	if incoming_values.len == 0 {
		return b.default_value_for_type(result_type)
	}
	if incoming_values.len == 1 {
		return incoming_values[0]
	}
	mut operands := []ValueID{}
	for i, value in incoming_values {
		operands << value
		operands << ValueID(incoming_blocks[i])
	}
	return b.m.add_instr(.phi, b.cur_block, result_type, operands)
}

// build_match_branch_value builds match branch value data for ssa.
fn (mut b Builder) build_match_branch_value(branch flat.Node) ?ValueID {
	body_start := b.match_branch_body_start(branch)
	if body_start >= branch.children_count {
		return none
	}
	for i in body_start .. branch.children_count - 1 {
		b.build_stmt(b.a.child(&branch, i))
		if b.current_block_terminated() {
			return none
		}
	}
	last_id := b.a.child(&branch, branch.children_count - 1)
	last := b.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count > 0 {
		return b.build_expr(b.a.child(&last, 0))
	}
	if last.kind == .match_stmt {
		return b.build_match_value(last_id, last)
	}
	if last.kind == .if_expr {
		return b.build_if_value(last_id, last)
	}
	if b.is_stmt_kind(last.kind) {
		b.build_stmt(last_id)
		return none
	}
	return b.build_expr(last_id)
}

// match_branch_body_start supports match branch body start handling for Builder.
fn (b &Builder) match_branch_body_start(branch flat.Node) int {
	if branch.value == 'else' {
		return 0
	}
	return branch.value.int()
}

// build_match_condition builds match condition data for ssa.
fn (mut b Builder) build_match_condition(subject ValueID, branch flat.Node) ValueID {
	n_conds := b.match_branch_body_start(branch)
	mut result := ValueID(0)
	for i in 0 .. n_conds {
		cond_value := b.build_expr(b.a.child(&branch, i))
		cmp := b.emit2(.eq, b.i1_type, subject, cond_value)
		if result == 0 {
			result = cmp
		} else {
			result = b.emit2(.or_, b.i1_type, result, cmp)
		}
	}
	if result == 0 {
		return b.m.get_or_add_const(b.i1_type, '1')
	}
	return result
}

// if_value_result_type supports if value result type handling for Builder.
fn (mut b Builder) if_value_result_type(id flat.NodeId, values []ValueID) TypeID {
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 && name != 'unknown' && name != 'void' {
				return b.resolve_type(name)
			}
		}
	}
	for value in values {
		typ := b.value_type(value)
		if typ != b.void_type {
			return typ
		}
	}
	return b.i64_type
}

// build_assert builds assert data for ssa.
fn (mut b Builder) build_assert(node flat.Node) {
	cond_val := b.build_expr(b.a.child(&node, 0))
	fail_block := b.m.add_block(b.cur_func, 'assert_fail')
	ok_block := b.m.add_block(b.cur_func, 'assert_ok')
	b.emit3(.br, b.void_type, cond_val, ValueID(ok_block), ValueID(fail_block))

	b.cur_block = fail_block
	if exit_fn := b.fn_ids['exit'] {
		one := b.m.get_or_add_const(b.i64_type, '1')
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'exit', exit_fn)
		b.emit2(.call, b.void_type, fn_ref, one)
	}
	b.emit0(.unreachable, b.void_type)

	b.cur_block = ok_block
}

// build_expr builds expr data for ssa.
fn (mut b Builder) build_expr(id flat.NodeId) ValueID {
	if int(id) < 0 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.empty {
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.int_literal {
			return b.m.get_or_add_const(b.i64_type, node.value)
		}
		.float_literal {
			return b.m.get_or_add_const(b.float_literal_type(id, node), node.value)
		}
		.bool_literal {
			val := if node.value == 'true' { '1' } else { '0' }
			return b.m.get_or_add_const(b.i1_type, val)
		}
		.string_literal {
			return b.m.add_value(.string_literal, b.str_type, node.value, 0)
		}
		.char_literal {
			return b.m.get_or_add_const(b.i8_type, '${char_literal_value(node.value)}')
		}
		.string_interp {
			return b.build_string_interp(node)
		}
		.enum_val {
			return b.build_enum_val(id, node)
		}
		.sizeof_expr {
			size := b.m.type_size(b.resolve_type(node.value))
			return b.m.get_or_add_const(b.i64_type, '${size}')
		}
		.ident {
			if addr := b.vars[node.value] {
				addr_val := b.m.values[addr]
				if addr_val.kind == .argument {
					return addr
				}
				if smart_val := b.load_smartcast_sum_value(addr, id) {
					return smart_val
				}
				return b.emit1(.load, b.deref_type(addr), addr)
			}
			if node.value == 'path_separator' {
				return b.m.add_value(.string_literal, b.str_type, '/', 0)
			}
			if expr_id := b.lookup_const_expr(node.value) {
				return b.build_expr(expr_id)
			}
			if !b.enum_member_dupes[node.value] {
				if enum_value := b.enum_member_values[node.value] {
					return b.m.get_or_add_const(b.i64_type, enum_value.str())
				}
			}
			if fn_idx := b.fn_ids[node.value] {
				return b.m.add_value(.func_ref, b.i64_type, node.value, fn_idx)
			}
			qualified_fn_name := ssa_fn_name_in_module(b.cur_module, node.value)
			if qualified_fn_name != node.value {
				if fn_idx := b.fn_ids[qualified_fn_name] {
					return b.m.add_value(.func_ref, b.i64_type, qualified_fn_name, fn_idx)
				}
			}
			match node.value {
				'min_i8' {
					return b.m.get_or_add_const(b.i64_type, '-128')
				}
				'max_i8' {
					return b.m.get_or_add_const(b.i64_type, '127')
				}
				'min_i16' {
					return b.m.get_or_add_const(b.i64_type, '-32768')
				}
				'max_i16' {
					return b.m.get_or_add_const(b.i64_type, '32767')
				}
				'min_i32', 'min_int' {
					return b.m.get_or_add_const(b.i64_type, '-2147483648')
				}
				'max_i32', 'max_int' {
					return b.m.get_or_add_const(b.i64_type, '2147483647')
				}
				'min_i64' {
					return b.m.get_or_add_const(b.i64_type, '-9223372036854775808')
				}
				'max_i64' {
					return b.m.get_or_add_const(b.i64_type, '9223372036854775807')
				}
				'min_u8', 'min_u16', 'min_u32', 'min_u64' {
					return b.m.get_or_add_const(b.i64_type, '0')
				}
				'max_u8' {
					return b.m.get_or_add_const(b.i64_type, '255')
				}
				'max_u16' {
					return b.m.get_or_add_const(b.i64_type, '65535')
				}
				'max_u32' {
					return b.m.get_or_add_const(b.i64_type, '4294967295')
				}
				'max_u64' {
					return b.m.get_or_add_const(b.i64_type, '18446744073709551615')
				}
				else {}
			}

			if expr_id := b.lookup_const_expr(node.value) {
				return b.build_expr(expr_id)
			}

			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.infix {
			return b.build_infix(node)
		}
		.prefix {
			return b.build_prefix(node, id)
		}
		.postfix {
			return b.build_postfix(node)
		}
		.paren {
			return b.build_expr(b.a.child(&node, 0))
		}
		.call {
			return b.build_call(id, node)
		}
		.selector {
			return b.build_selector(node)
		}
		.index {
			return b.build_index(id, node)
		}
		.struct_init {
			return b.build_struct_init(node)
		}
		.array_literal {
			return b.build_array_literal(node)
		}
		.array_init {
			return b.build_array_init(node)
		}
		.map_init {
			return b.build_map_init(node)
		}
		.cast_expr {
			return b.build_cast_expr(node)
		}
		.as_expr {
			return b.build_as_expr(node)
		}
		.is_expr {
			return b.build_is_expr(node)
		}
		.nil_literal {
			return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
		}
		.none_expr {
			return b.default_value_for_type(b.call_expr_result_type(id, node))
		}
		.if_expr {
			return b.build_if_value(id, node)
		}
		.match_stmt {
			return b.build_match_value(id, node)
		}
		.or_expr {
			return b.build_or_expr(id, node)
		}
		.field_init {
			if node.children_count > 0 {
				return b.build_expr(b.a.child(&node, 0))
			}
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.decl_assign {
			b.build_decl_assign(node)
			return b.m.get_or_add_const(b.i1_type, '1')
		}
		.assign, .selector_assign, .index_assign {
			b.build_stmt(id)
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.dump_expr {
			if node.children_count > 0 {
				return b.build_expr(b.a.child(&node, 0))
			}
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.in_expr {
			return b.build_in_expr(node)
		}
		.range {
			return b.build_range_expr(node)
		}
		.block {
			return b.build_block_expr(node)
		}
		else {
			eprintln('build_expr: unsupported expr kind: ${node.kind}')
			return b.m.get_or_add_const(b.i64_type, '0')
		}
	}
}

// build_range_expr builds range expr data for ssa.
fn (mut b Builder) build_range_expr(node flat.Node) ValueID {
	if node.children_count > 0 {
		return b.build_expr(b.a.child(&node, 0))
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

// build_in_expr builds in expr data for ssa.
fn (mut b Builder) build_in_expr(node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i1_type, '0')
	}
	lhs_id := b.a.child(&node, 0)
	rhs_id := b.a.child(&node, 1)
	rhs := b.a.nodes[int(rhs_id)]
	if rhs.kind == .range && rhs.children_count >= 2 {
		lhs := b.build_expr(lhs_id)
		low := b.build_expr(b.a.child(&rhs, 0))
		high := b.build_expr(b.a.child(&rhs, 1))
		ge_low := b.emit2(.ge, b.i1_type, lhs, low)
		lt_high := b.emit2(.lt, b.i1_type, lhs, high)
		return b.emit2(.and_, b.i1_type, ge_low, lt_high)
	}
	map_type_name := b.expr_type_name_for_map(rhs_id)
	key_type_name, _ := map_type_parts(map_type_name)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	map_ptr := b.map_expr_ptr(rhs_id)
	key_val := b.build_expr(lhs_id)
	mut key_type := if key_type_name.len > 0 {
		b.resolve_type(key_type_name)
	} else {
		b.value_type(key_val)
	}
	if key_type == b.void_type {
		key_type = b.value_type(key_val)
	}
	key_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(key_type))
	b.emit2(.store, b.void_type, key_val, key_alloca)
	key_ptr := if b.value_type(key_alloca) == ptr_i8 {
		key_alloca
	} else {
		b.emit1(.bitcast, ptr_i8, key_alloca)
	}
	exists_ref := b.m.add_value(.func_ref, b.i1_type, 'map__exists', b.fn_ids['map__exists'])
	return b.emit3(.call, b.i1_type, exists_ref, map_ptr, key_ptr)
}

// build_enum_val builds enum val data for ssa.
fn (mut b Builder) build_enum_val(id flat.NodeId, node flat.Node) ValueID {
	if node.typ.len > 0 {
		if value := b.enum_value_for_type(node.typ, node.value) {
			return b.m.get_or_add_const(b.i64_type, value.str())
		}
	}
	clean_member0 := node.value.trim_left('.')
	if value := b.enum_values[clean_member0] {
		return b.m.get_or_add_const(b.i64_type, value.str())
	}
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			type_name := typ.name()
			if value := b.enum_value_for_type(type_name, node.value) {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}
	if !b.enum_member_dupes[node.value] {
		if value := b.enum_member_values[node.value] {
			return b.m.get_or_add_const(b.i64_type, value.str())
		}
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

// enum_value_for_type supports enum value for type handling for Builder.
fn (b &Builder) enum_value_for_type(type_name string, member string) ?int {
	if type_name.len == 0 || type_name in ['int', 'unknown'] {
		return none
	}
	clean_member0 := member.trim_left('.')
	if value := b.enum_values[clean_member0] {
		enum_name := clean_member0.all_before_last('.')
		return if b.is_flag_enum_type_name(enum_name) { 1 << value } else { value }
	}
	clean_member := clean_member0.all_after_last('.')
	mut names := []string{}
	names << type_name
	short_type := type_name.all_after('.')
	if short_type != type_name {
		names << short_type
	}
	for name in names {
		key := name + '.' + clean_member
		if value := b.enum_values[key] {
			return if b.is_flag_enum_type_name(name) { 1 << value } else { value }
		}
	}
	return none
}

// is_flag_enum_type_name reports whether is flag enum type name applies in ssa.
fn (b &Builder) is_flag_enum_type_name(type_name string) bool {
	clean := type_name.trim_left('&')
	if clean in b.flag_enum_types {
		return true
	}
	short_name := clean.all_after('.')
	if short_name in b.flag_enum_types {
		return true
	}
	if b.tc != unsafe { nil } {
		if clean in b.tc.flag_enums {
			return true
		}
		if short_name in b.tc.flag_enums {
			return true
		}
		if !clean.contains('.') && 'builtin.${clean}' in b.tc.flag_enums {
			return true
		}
	}
	return clean == 'ArrayFlags'
}

// checker_has_flag_enum converts checker has flag enum data for ssa.
fn (b &Builder) checker_has_flag_enum(type_name string) bool {
	if b.tc == unsafe { nil } {
		return false
	}
	if type_name in b.tc.flag_enums {
		return true
	}
	short_name := type_name.all_after('.')
	return short_name in b.tc.flag_enums
}

// build_enum_val_with_type builds enum val with type data for ssa.
fn (mut b Builder) build_enum_val_with_type(node flat.Node, type_name string) ?ValueID {
	if value := b.enum_value_for_type(type_name, node.value) {
		return b.m.get_or_add_const(b.i64_type, value.str())
	}
	return none
}

// char_literal_value supports char literal value handling for ssa.
fn char_literal_value(value string) int {
	if value.len == 0 {
		return 0
	}
	if value[0] == `\\` && value.len > 1 {
		return match value[1] {
			`n` { 10 }
			`r` { 13 }
			`t` { 9 }
			`0` { 0 }
			`\\` { 92 }
			`'` { 39 }
			else { int(value[1]) }
		}
	}
	return int(value[0])
}

fn parse_int_literal(value string) int {
	clean := value.replace('_', '')
	if clean.len > 2 && clean[0] == `0` && (clean[1] == `x` || clean[1] == `X`) {
		mut n := 0
		for ch in clean[2..] {
			digit := if ch >= `0` && ch <= `9` {
				int(ch - `0`)
			} else if ch >= `a` && ch <= `f` {
				int(ch - `a`) + 10
			} else if ch >= `A` && ch <= `F` {
				int(ch - `A`) + 10
			} else {
				0
			}
			n = n * 16 + digit
		}
		return n
	}
	return clean.int()
}

fn (mut b Builder) build_block_expr(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	for i in 0 .. node.children_count - 1 {
		b.build_stmt(b.a.child(&node, i))
	}
	last_id := b.a.child(&node, node.children_count - 1)
	last := b.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count > 0 {
		return b.build_expr(b.a.child(&last, 0))
	}
	return b.build_expr(last_id)
}

fn (mut b Builder) build_array_literal(node flat.Node) ValueID {
	if b.is_fixed_array_type_name(node.typ) {
		mut fixed_node := node
		fixed_node.value = node.typ
		return b.build_fixed_array_init(fixed_node)
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	mut values := []ValueID{}
	mut elem_type := b.i64_type
	for i in 0 .. node.children_count {
		value := b.build_expr(b.a.child(&node, i))
		if i == 0 {
			elem_type = b.value_type(value)
		}
		values << value
	}
	elem_size := b.m.type_size(elem_type)
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	elem_size_const := b.m.get_or_add_const(b.i64_type, '${actual_elem_size}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	cap_const := b.m.get_or_add_const(b.i64_type, '${node.children_count}')
	new_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.fn_ids['array_new'])
	arr := b.emit4(.call, b.array_type, new_ref, elem_size_const, zero, cap_const)
	arr_alloca := b.emit0(.alloca, ptr_array)
	b.emit2(.store, b.void_type, arr, arr_alloca)
	push_ref := b.m.add_value(.func_ref, b.void_type, 'array_push', b.fn_ids['array_push'])
	for value in values {
		value_type := b.value_type(value)
		value_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(value_type))
		b.emit2(.store, b.void_type, value, value_alloca)
		value_arg := if b.value_type(value_alloca) == ptr_i8 {
			value_alloca
		} else {
			b.emit1(.bitcast, ptr_i8, value_alloca)
		}
		b.emit3(.call, b.void_type, push_ref, arr_alloca, value_arg)
	}
	return b.emit1(.load, b.array_type, arr_alloca)
}

fn (mut b Builder) build_array_init(node flat.Node) ValueID {
	if b.is_fixed_array_type_name(node.value) {
		return b.build_fixed_array_init(node)
	}
	mut elem_type_name := node.value
	mut len_val := b.m.get_or_add_const(b.i64_type, '0')
	mut cap_val := b.m.get_or_add_const(b.i64_type, '0')
	mut init_val := ValueID(0)
	if node.value.starts_with('[') {
		len_text := node.value.all_after('[').all_before(']')
		len_val = b.fixed_array_len_value(len_text)
		cap_val = len_val
		elem_type_name = node.value.all_after(']')
	} else {
		mut has_cap := false
		for i in 0 .. node.children_count {
			child_id := b.a.child(&node, i)
			child := b.a.nodes[int(child_id)]
			if child.kind == .field_init && child.children_count > 0 {
				value := b.build_expr(b.a.child(&child, 0))
				if child.value == 'len' {
					len_val = value
					if !has_cap {
						cap_val = value
					}
				} else if child.value == 'cap' {
					cap_val = value
					has_cap = true
				} else if child.value == 'init' {
					init_val = value
				}
			}
		}
	}
	elem_type := b.resolve_type(elem_type_name)
	elem_size := b.m.type_size(elem_type)
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	elem_size_const := b.m.get_or_add_const(b.i64_type, '${actual_elem_size}')
	new_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.fn_ids['array_new'])
	arr := b.emit4(.call, b.array_type, new_ref, elem_size_const, len_val, cap_val)
	if init_val == ValueID(0) {
		return arr
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	arr_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.array_type))
	i_alloca := b.emit0(.alloca, ptr_i64)
	b.emit2(.store, b.void_type, arr, arr_alloca)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.emit2(.store, b.void_type, zero, i_alloca)
	loop := b.m.add_block(b.cur_func, 'array_init_fill_loop')
	body := b.m.add_block(b.cur_func, 'array_init_fill_body')
	done := b.m.add_block(b.cur_func, 'array_init_fill_done')
	b.emit1(.jmp, b.void_type, ValueID(loop))

	b.cur_block = loop
	i_val := b.emit1(.load, b.i64_type, i_alloca)
	more := b.emit2(.lt, b.i1_type, i_val, len_val)
	b.emit3(.br, b.void_type, more, ValueID(body), ValueID(done))

	b.cur_block = body
	data_ptr := b.get_field_ptr(arr_alloca, 'data')
	data := b.emit1(.load, ptr_i8, data_ptr)
	offset := b.emit2(.mul, b.i64_type, i_val, elem_size_const)
	elem_ptr_raw := b.emit2(.add, ptr_i8, data, offset)
	elem_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr_raw)
	elem_value := if b.is_int_type(b.value_type(init_val)) && b.is_int_type(elem_type) {
		b.coerce_int_value(init_val, elem_type)
	} else {
		init_val
	}
	b.emit2(.store, b.void_type, elem_value, elem_ptr)
	next_i := b.emit2(.add, b.i64_type, i_val, one)
	b.emit2(.store, b.void_type, next_i, i_alloca)
	b.emit1(.jmp, b.void_type, ValueID(loop))

	b.cur_block = done
	return b.emit1(.load, b.array_type, arr_alloca)
}

fn (mut b Builder) fixed_array_len_value(len_text string) ValueID {
	if len_text.len == 0 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	if expr_id := b.lookup_const_expr(len_text) {
		if literal := b.const_int_literal_value(expr_id) {
			return b.m.get_or_add_const(b.i64_type, literal)
		}
	}
	return b.m.get_or_add_const(b.i64_type, len_text)
}

fn (mut b Builder) build_as_expr(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.default_value_for_type(b.resolve_type(node.value))
	}
	child_id := b.a.child(&node, 0)
	child_type_name := b.checked_expr_type_name(child_id)
	sum_name := b.canonical_sum_type_name(child_type_name) or { return b.build_expr(child_id) }
	field_name := sum_variant_field_name(node.value)
	sum_val := b.build_expr(child_id)
	sum_type := b.resolve_type(sum_name)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(sum_type))
	b.emit2(.store, b.void_type, sum_val, alloca)
	field_ptr := b.get_field_ptr(alloca, field_name)
	return b.emit1(.load, b.deref_type(field_ptr), field_ptr)
}

fn (mut b Builder) build_is_expr(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.i1_type, '0')
	}
	child_id := b.a.child(&node, 0)
	child_type_name := b.checked_expr_type_name(child_id)
	sum_name := b.canonical_sum_type_name(child_type_name) or {
		return b.m.get_or_add_const(b.i1_type, '1')
	}
	tag := b.sum_variant_index(sum_name, node.value)
	if tag <= 0 {
		return b.m.get_or_add_const(b.i1_type, '0')
	}
	sum_val := b.build_expr(child_id)
	sum_type := b.resolve_type(sum_name)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(sum_type))
	b.emit2(.store, b.void_type, sum_val, alloca)
	tag_ptr := b.get_field_ptr(alloca, 'typ')
	tag_val := b.emit1(.load, b.deref_type(tag_ptr), tag_ptr)
	tag_const := b.m.get_or_add_const(b.value_type(tag_val), '${tag}')
	return b.emit2(.eq, b.i1_type, tag_val, tag_const)
}

fn (mut b Builder) build_map_init(node flat.Node) ValueID {
	key_type_name, val_type_name := map_type_parts(node.value)
	if key_type_name.len == 0 || val_type_name.len == 0 {
		return b.default_value_for_type(b.map_type)
	}
	key_type := b.resolve_type(key_type_name)
	val_type := b.resolve_type(val_type_name)
	key_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(key_type)}')
	val_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(val_type)}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	mut args := []ValueID{}
	if fn_id := b.fn_ids['new_map'] {
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'new_map', fn_id)
		args << fn_ref
		args << key_size
		args << val_size
		args << zero
		args << zero
		args << zero
		args << zero
	} else {
		return b.default_value_for_type(b.map_type)
	}
	map_val := b.m.add_instr(.call, b.cur_block, b.map_type, args)
	if node.children_count == 0 {
		return map_val
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	map_alloca := b.emit0(.alloca, ptr_map)
	b.emit2(.store, b.void_type, map_val, map_alloca)
	mut i := 0
	for i < node.children_count {
		key_id := b.a.child(&node, i)
		val_id := b.a.child(&node, i + 1)
		key_val := b.build_expr(key_id)
		val_val := b.build_expr(val_id)
		key_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(key_type))
		val_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(val_type))
		b.emit2(.store, b.void_type, key_val, key_alloca)
		b.emit2(.store, b.void_type, val_val, val_alloca)
		key_ptr := if b.value_type(key_alloca) == ptr_i8 {
			key_alloca
		} else {
			b.emit1(.bitcast, ptr_i8, key_alloca)
		}
		val_ptr := if b.value_type(val_alloca) == ptr_i8 {
			val_alloca
		} else {
			b.emit1(.bitcast, ptr_i8, val_alloca)
		}
		if set_id := b.fn_ids['v3_map_set_sized'] {
			set_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_set_sized', set_id)
			mut set_args := []ValueID{}
			set_args << set_ref
			set_args << map_alloca
			set_args << key_ptr
			set_args << val_ptr
			set_args << key_size
			set_args << val_size
			b.m.add_instr(.call, b.cur_block, b.void_type, set_args)
		}
		i += 2
	}
	return b.emit1(.load, b.map_type, map_alloca)
}

fn (b &Builder) is_fixed_array_type_name(type_name string) bool {
	if type_name.starts_with('[') {
		idx := type_name.index_u8(`]`)
		return idx > 1 && idx + 1 < type_name.len
	}
	if type_name.contains('[') && type_name.ends_with(']') {
		idx := type_name.index_u8(`[`)
		return idx > 0 && idx < type_name.len - 1
	}
	return false
}

fn (b &Builder) fixed_array_elem_type_name(type_name string) string {
	if type_name.starts_with('[') {
		idx := type_name.index_u8(`]`)
		if idx > 0 && idx + 1 < type_name.len {
			return type_name[idx + 1..]
		}
	} else if type_name.contains('[') && type_name.ends_with(']') {
		idx := type_name.index_u8(`[`)
		if idx > 0 {
			return type_name[..idx]
		}
	}
	return 'u8'
}

fn (b &Builder) fixed_array_len_text(type_name string) string {
	mut raw_len := ''
	if type_name.starts_with('[') {
		idx := type_name.index_u8(`]`)
		if idx > 1 {
			raw_len = type_name[1..idx]
		}
	} else if type_name.contains('[') && type_name.ends_with(']') {
		idx := type_name.index_u8(`[`)
		if idx > 0 && idx < type_name.len - 1 {
			raw_len = type_name[idx + 1..type_name.len - 1]
		}
	}
	if raw_len.len == 0 {
		return '1'
	}
	clean_len := raw_len.replace('_', '')
	if b.is_decimal_int_text(clean_len) {
		return clean_len
	}
	if expr_id := b.lookup_const_expr(raw_len) {
		if value := b.const_int_literal_value(expr_id) {
			return value.replace('_', '')
		}
	}
	return '1'
}

fn (b &Builder) is_decimal_int_text(text string) bool {
	if text.len == 0 {
		return false
	}
	for ch in text {
		if ch < `0` || ch > `9` {
			return false
		}
	}
	return true
}

fn (mut b Builder) build_fixed_array_init(node flat.Node) ValueID {
	elem_type_name := b.fixed_array_elem_type_name(node.value)
	elem_type := b.resolve_type(elem_type_name)
	ptr_elem := b.m.type_store.get_ptr(elem_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	len_text := b.fixed_array_len_text(node.value)
	len_val := b.m.get_or_add_const(b.i64_type, len_text)
	alloca := b.emit1(.alloca, ptr_elem, len_val)
	data := if ptr_elem == ptr_i8 {
		alloca
	} else {
		b.emit1(.bitcast, ptr_i8, alloca)
	}

	elem_size := b.m.type_size(elem_type)
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	byte_count := b.m.get_or_add_const(b.i64_type, '${len_text.int() * actual_elem_size}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset', b.fn_ids['memset'])
	b.emit4(.call, ptr_i8, memset_ref, data, zero, byte_count)

	mut elem_index := 0
	for i in 0 .. node.children_count {
		child_id := b.a.child(&node, i)
		child := b.a.nodes[int(child_id)]
		if child.kind == .field_init {
			continue
		}
		value := b.build_expr(child_id)
		offset := b.m.get_or_add_const(b.i64_type, '${elem_index * actual_elem_size}')
		slot_i8 := b.emit2(.get_element_ptr, ptr_i8, data, offset)
		slot := if ptr_elem == ptr_i8 {
			slot_i8
		} else {
			b.emit1(.bitcast, ptr_elem, slot_i8)
		}
		b.emit2(.store, b.void_type, value, slot)
		elem_index++
	}
	return alloca
}

fn (mut b Builder) build_or_expr(id flat.NodeId, node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	expr_id := b.a.child(&node, 0)
	expr := b.a.nodes[int(expr_id)]
	if expr.kind == .index && expr.children_count >= 2 && expr.value != 'range' {
		if result := b.build_map_index_or_expr(id, expr, b.a.child(&node, 1)) {
			return result
		}
	}
	if result := b.build_option_or_expr(id, expr_id, b.a.child(&node, 1)) {
		return result
	}
	return b.default_value_for_type(b.or_result_type(id, node.typ))
}

fn (mut b Builder) build_option_or_expr(id flat.NodeId, expr_id flat.NodeId, fallback_id flat.NodeId) ?ValueID {
	opt_val := b.build_expr(expr_id)
	opt_typ := b.value_type(opt_val)
	value_typ := b.option_value_type(opt_typ)
	if value_typ == TypeID(0) {
		return none
	}
	if value_typ == b.void_type {
		return none
	}
	result_type := b.or_result_type(id, b.option_payload_type_name(expr_id))
	result_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(result_type))
	opt_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(opt_typ))
	b.emit2(.store, b.void_type, opt_val, opt_alloca)
	ok_ptr := b.get_field_ptr(opt_alloca, 'ok')
	ok := b.emit1(.load, b.i1_type, ok_ptr)

	then_block := b.m.add_block(b.cur_func, 'or_option_some')
	else_block := b.m.add_block(b.cur_func, 'or_option_else')
	merge_block := b.m.add_block(b.cur_func, 'or_option_merge')
	b.emit3(.br, b.void_type, ok, ValueID(then_block), ValueID(else_block))

	b.cur_block = then_block
	value_ptr := b.get_field_ptr(opt_alloca, 'value')
	mut value := b.emit1(.load, value_typ, value_ptr)
	if value_typ != result_type {
		value = b.coerce_int_value(value, result_type)
	}
	b.emit2(.store, b.void_type, value, result_alloca)
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}

	b.cur_block = else_block
	fallback_value := b.build_or_fallback_value(fallback_id, result_type)
	if !b.current_block_terminated() {
		b.emit2(.store, b.void_type, fallback_value, result_alloca)
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}

	b.cur_block = merge_block
	return b.emit1(.load, result_type, result_alloca)
}

fn (mut b Builder) build_map_index_or_expr(id flat.NodeId, index_node flat.Node, fallback_id flat.NodeId) ?ValueID {
	base_id := b.a.child(&index_node, 0)
	key_id := b.a.child(&index_node, 1)
	map_type_name := b.expr_type_name_for_map(base_id)
	key_type_name, val_type_name := map_type_parts(map_type_name)
	if key_type_name.len == 0 || val_type_name.len == 0 {
		return none
	}
	result_type := b.or_result_type(id, val_type_name)
	ptr_result := b.m.type_store.get_ptr(result_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	result_alloca := b.emit0(.alloca, ptr_result)
	map_ptr := b.map_expr_ptr(base_id)
	key_val := b.build_expr(key_id)
	key_type := b.resolve_type(key_type_name)
	key_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(key_type))
	b.emit2(.store, b.void_type, key_val, key_alloca)
	key_ptr := if b.value_type(key_alloca) == ptr_i8 {
		key_alloca
	} else {
		b.emit1(.bitcast, ptr_i8, key_alloca)
	}
	get_ref := b.m.add_value(.func_ref, ptr_i8, 'map__get_check', b.fn_ids['map__get_check'])
	value_ptr := b.emit3(.call, ptr_i8, get_ref, map_ptr, key_ptr)
	zero_ptr := b.m.get_or_add_const(ptr_i8, '0')
	found := b.emit2(.ne, b.i1_type, value_ptr, zero_ptr)

	then_block := b.m.add_block(b.cur_func, 'or_map_found')
	else_block := b.m.add_block(b.cur_func, 'or_map_else')
	merge_block := b.m.add_block(b.cur_func, 'or_map_merge')
	b.emit3(.br, b.void_type, found, ValueID(then_block), ValueID(else_block))

	b.cur_block = then_block
	typed_value_ptr := if ptr_result == ptr_i8 {
		value_ptr
	} else {
		b.emit1(.bitcast, ptr_result, value_ptr)
	}
	found_value := b.emit1(.load, result_type, typed_value_ptr)
	b.emit2(.store, b.void_type, found_value, result_alloca)
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}

	b.cur_block = else_block
	fallback_value := b.build_or_fallback_value(fallback_id, result_type)
	if !b.current_block_terminated() {
		b.emit2(.store, b.void_type, fallback_value, result_alloca)
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}

	b.cur_block = merge_block
	return b.emit1(.load, result_type, result_alloca)
}

fn (mut b Builder) map_expr_ptr(id flat.NodeId) ValueID {
	node := b.a.nodes[int(id)]
	if node.kind == .ident || node.kind == .selector {
		addr := b.build_lvalue_addr(id)
		if b.value_type(addr) == b.m.type_store.get_ptr(b.map_type) {
			return addr
		}
	}
	val := b.build_expr(id)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.value_type(val)))
	b.emit2(.store, b.void_type, val, alloca)
	return alloca
}

fn (mut b Builder) build_or_fallback_value(id flat.NodeId, result_type TypeID) ValueID {
	if int(id) < 0 {
		return b.default_value_for_type(result_type)
	}
	node := b.a.nodes[int(id)]
	if node.kind != .block {
		return b.build_expr(id)
	}
	if node.children_count == 0 {
		return b.default_value_for_type(result_type)
	}
	for i in 0 .. node.children_count {
		child_id := b.a.child(&node, i)
		child := b.a.nodes[int(child_id)]
		is_last := i == node.children_count - 1
		if is_last {
			if child.kind == .expr_stmt && child.children_count > 0 {
				return b.build_expr(b.a.child(&child, 0))
			}
			if b.is_stmt_kind(child.kind) {
				b.build_stmt(child_id)
				return b.default_value_for_type(result_type)
			}
			return b.build_expr(child_id)
		}
		b.build_stmt(child_id)
		if b.current_block_terminated() {
			return b.default_value_for_type(result_type)
		}
	}
	return b.default_value_for_type(result_type)
}

fn (mut b Builder) or_result_type(id flat.NodeId, fallback_type string) TypeID {
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 && name != 'unknown' {
				return b.resolve_type(name)
			}
		}
	}
	return b.resolve_type(fallback_type)
}

fn (mut b Builder) default_value_for_type(typ TypeID) ValueID {
	if b.is_option_type(typ) {
		return b.build_option_value(typ, false, ValueID(0))
	}
	if typ == b.str_type {
		return b.m.add_value(.string_literal, b.str_type, '', 0)
	}
	if typ == b.array_type {
		fn_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.fn_ids['array_new'])
		zero := b.m.get_or_add_const(b.i64_type, '0')
		one := b.m.get_or_add_const(b.i64_type, '1')
		return b.emit4(.call, b.array_type, fn_ref, one, zero, zero)
	}
	if typ > 0 && typ < b.m.type_store.types.len && b.m.type_store.types[typ].kind == .ptr_t {
		return b.m.get_or_add_const(typ, '0')
	}
	return b.m.get_or_add_const(typ, '0')
}

fn (mut b Builder) build_option_value(opt_typ TypeID, ok bool, raw_value ValueID) ValueID {
	ptr_opt := b.m.type_store.get_ptr(opt_typ)
	alloca := b.emit0(.alloca, ptr_opt)
	ok_ptr := b.block_struct_field_ptr(b.cur_block, alloca, opt_typ, 0)
	ok_val := b.m.get_or_add_const(b.i1_type, if ok { '1' } else { '0' })
	b.emit2(.store, b.void_type, ok_val, ok_ptr)
	value_typ := b.option_value_type(opt_typ)
	if value_typ != b.void_type {
		value_ptr := b.block_struct_field_ptr(b.cur_block, alloca, opt_typ, 1)
		mut value := raw_value
		if !ok || value <= 0 {
			value = b.default_value_for_type(value_typ)
		}
		value_type := b.value_type(value)
		if value_type != value_typ {
			if b.is_pointer_type(value_typ) && b.is_pointer_type(value_type) {
				value = b.emit1(.bitcast, value_typ, value)
			} else {
				value = b.coerce_int_value(value, value_typ)
				if b.value_type(value) != value_typ {
					value = b.default_value_for_type(value_typ)
				}
			}
		}
		b.emit2(.store, b.void_type, value, value_ptr)
	}
	return b.emit1(.load, opt_typ, alloca)
}

fn (b &Builder) is_stmt_kind(kind flat.NodeKind) bool {
	return kind in [.expr_stmt, .decl_assign, .assign, .selector_assign, .index_assign, .return_stmt,
		.defer_stmt, .for_stmt, .for_in_stmt, .break_stmt, .continue_stmt, .if_expr, .match_stmt,
		.block, .assert_stmt, .goto_stmt, .label_stmt]
}

fn (b &Builder) current_block_terminated() bool {
	if b.cur_block < 0 || b.cur_block >= b.m.blocks.len {
		return true
	}
	blk := b.m.blocks[b.cur_block]
	return blk.instrs.len > 0 && b.is_terminator(blk.instrs.last())
}

fn (mut b Builder) build_infix(node flat.Node) ValueID {
	if node.op == .logical_and || node.op == .logical_or {
		return b.build_short_circuit(node)
	}
	if node.op == .left_shift && node.value == 'push' {
		return b.build_array_push_expr(node)
	}
	if result := b.build_qualified_infix_call(node) {
		return result
	}
	lhs_id := b.a.child(&node, 0)
	rhs_id := b.a.child(&node, 1)
	lhs_node := b.a.nodes[int(lhs_id)]
	rhs_node := b.a.nodes[int(rhs_id)]
	lhs_type_name := b.checked_expr_type_name(lhs_id)
	rhs_type_name := b.checked_expr_type_name(rhs_id)
	lhs := if lhs_node.kind == .enum_val && rhs_type_name.len > 0 {
		b.build_enum_val_with_type(lhs_node, rhs_type_name) or { b.build_expr(lhs_id) }
	} else {
		b.build_expr(lhs_id)
	}
	rhs := if rhs_node.kind == .enum_val && lhs_type_name.len > 0 {
		b.build_enum_val_with_type(rhs_node, lhs_type_name) or { b.build_expr(rhs_id) }
	} else {
		b.build_expr(rhs_id)
	}
	if b.value_type(lhs) == b.str_type || b.value_type(rhs) == b.str_type {
		return b.build_string_infix(node.op, lhs, rhs)
	}
	lhs_type := b.value_type(lhs)
	mut op := OpCode.add
	if b.is_float_type(lhs_type) {
		op = OpCode.fadd
		match node.op {
			.plus { op = OpCode.fadd }
			.minus { op = OpCode.fsub }
			.mul { op = OpCode.fmul }
			.div { op = OpCode.fdiv }
			.mod { op = OpCode.frem }
			.eq { op = OpCode.eq }
			.ne { op = OpCode.ne }
			.lt { op = OpCode.lt }
			.gt { op = OpCode.gt }
			.le { op = OpCode.le }
			.ge { op = OpCode.ge }
			else {}
		}
	} else {
		op = OpCode.add
		match node.op {
			.plus {
				op = OpCode.add
			}
			.minus {
				op = OpCode.sub
			}
			.mul {
				op = OpCode.mul
			}
			.div {
				op = if b.is_unsigned_type(lhs_type) { OpCode.udiv } else { OpCode.sdiv }
			}
			.mod {
				op = if b.is_unsigned_type(lhs_type) { OpCode.urem } else { OpCode.srem }
			}
			.amp {
				op = OpCode.and_
			}
			.pipe {
				op = OpCode.or_
			}
			.xor {
				op = OpCode.xor
			}
			.left_shift {
				op = OpCode.shl
			}
			.right_shift {
				op = if b.is_unsigned_type(lhs_type) { OpCode.lshr } else { OpCode.ashr }
			}
			.eq {
				op = OpCode.eq
			}
			.ne {
				op = OpCode.ne
			}
			.lt {
				op = if b.is_unsigned_type(lhs_type) { OpCode.ult } else { OpCode.lt }
			}
			.gt {
				op = if b.is_unsigned_type(lhs_type) { OpCode.ugt } else { OpCode.gt }
			}
			.le {
				op = if b.is_unsigned_type(lhs_type) { OpCode.ule } else { OpCode.le }
			}
			.ge {
				op = if b.is_unsigned_type(lhs_type) { OpCode.uge } else { OpCode.ge }
			}
			else {
				op = OpCode.add
			}
		}
	}
	result_type := if node.op in [.eq, .ne, .lt, .gt, .le, .ge] { b.i1_type } else { lhs_type }
	return b.emit2(op, result_type, lhs, rhs)
}

fn (mut b Builder) build_qualified_infix_call(node flat.Node) ?ValueID {
	if node.children_count < 2 || node.op !in [.dot, .amp] {
		return none
	}
	lhs_id := b.a.child(&node, 0)
	rhs_id := b.a.child(&node, 1)
	lhs_name := b.qualified_expr_name(lhs_id)
	if lhs_name.len == 0 {
		return none
	}
	if _ := b.vars[lhs_name] {
		return none
	}
	rhs := b.a.nodes[int(rhs_id)]
	if rhs.kind != .call || rhs.children_count == 0 {
		return none
	}
	fn_node := b.a.child_node(&rhs, 0)
	if fn_node.kind != .ident || fn_node.value.len == 0 {
		return none
	}
	for candidate in ['${lhs_name}.${fn_node.value}', '${lhs_name}__${fn_node.value}'] {
		if candidate in b.fn_ids {
			return b.build_direct_call(candidate, rhs)
		}
	}
	return none
}

fn (mut b Builder) build_direct_call(resolved_name string, node flat.Node) ValueID {
	fn_idx := b.fn_ids[resolved_name] or { panic('ssa: unknown function `${resolved_name}`') }
	fn_ref := b.m.add_value(.func_ref, b.void_type, resolved_name, fn_idx)
	ret_type := b.m.funcs[fn_idx].typ
	mut param_types := []TypeID{}
	if ft_id := b.fn_types[resolved_name] {
		ft := b.m.type_store.types[ft_id]
		param_types = ft.params.clone()
	}
	mut args := []ValueID{}
	args << fn_ref
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		param_idx := i - 1
		if param_idx < param_types.len {
			param_type := param_types[param_idx]
			if b.is_option_type(param_type) {
				arg_node := b.a.nodes[int(arg_id)]
				if arg_node.kind == .none_expr {
					args << b.default_value_for_type(param_type)
					continue
				}
				mut arg := b.build_expr(arg_id)
				if b.value_type(arg) != param_type {
					arg = b.build_option_value(param_type, true, arg)
				}
				args << arg
				continue
			}
			pt := b.m.type_store.types[param_type]
			if pt.kind == .ptr_t {
				arg_node := b.a.nodes[int(arg_id)]
				if arg_node.kind == .ident {
					if addr := b.vars[arg_node.value] {
						if b.should_pass_ident_addr_for_ptr_param(addr, param_type) {
							args << addr
							continue
						}
					}
				} else if arg_node.kind == .selector {
					addr := b.build_selector_addr(arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type) {
						args << addr
						continue
					}
				} else if arg_node.kind == .index {
					addr := b.build_index_addr(arg_id, arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type) {
						args << addr
						continue
					}
				}
			}
		}
		args << b.build_expr(arg_id)
	}
	return b.m.add_instr(.call, b.cur_block, ret_type, args)
}

fn (b &Builder) qualified_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := b.qualified_expr_name(b.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		else {
			return ''
		}
	}
}

fn (mut b Builder) build_string_infix(op flat.Op, lhs ValueID, rhs ValueID) ValueID {
	match op {
		.plus {
			return b.emit_runtime_call('string__plus', b.str_type, [lhs, rhs])
		}
		.eq {
			return b.emit_runtime_call('string__eq', b.i1_type, [lhs, rhs])
		}
		.ne {
			eq := b.emit_runtime_call('string__eq', b.i1_type, [lhs, rhs])
			zero := b.m.get_or_add_const(b.i1_type, '0')
			return b.emit2(.eq, b.i1_type, eq, zero)
		}
		.lt {
			return b.emit_runtime_call('string__lt', b.i1_type, [lhs, rhs])
		}
		.gt {
			return b.emit_runtime_call('string__lt', b.i1_type, [rhs, lhs])
		}
		.le {
			gt := b.emit_runtime_call('string__lt', b.i1_type, [rhs, lhs])
			zero := b.m.get_or_add_const(b.i1_type, '0')
			return b.emit2(.eq, b.i1_type, gt, zero)
		}
		.ge {
			lt := b.emit_runtime_call('string__lt', b.i1_type, [lhs, rhs])
			zero := b.m.get_or_add_const(b.i1_type, '0')
			return b.emit2(.eq, b.i1_type, lt, zero)
		}
		else {}
	}

	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (mut b Builder) emit_runtime_call(name string, ret_type TypeID, values []ValueID) ValueID {
	if fn_idx := b.fn_ids[name] {
		fn_ref := b.m.add_value(.func_ref, ret_type, name, fn_idx)
		mut args := []ValueID{}
		args << fn_ref
		args << values
		return b.m.add_instr(.call, b.cur_block, ret_type, args)
	}
	return b.m.get_or_add_const(ret_type, '0')
}

fn (mut b Builder) build_cast_expr(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	target_name := if node.typ.len > 0 { node.typ } else { node.value }
	if target_name.len == 0 {
		return b.build_expr(b.a.child(&node, 0))
	}
	if value := b.build_sum_type_cast(target_name, b.a.child(&node, 0)) {
		return value
	}
	value := b.build_expr(b.a.child(&node, 0))
	target_type := b.resolve_type(target_name)
	from_type := b.value_type(value)
	if target_type == from_type {
		return value
	}
	if b.is_pointer_type(target_type) || b.is_pointer_type(from_type) {
		return b.emit1(.bitcast, target_type, value)
	}
	if b.is_int_type(from_type) && b.is_int_type(target_type) {
		from_width := b.scalar_type_width(from_type)
		to_width := b.scalar_type_width(target_type)
		if to_width > 0 && from_width > 0 && to_width < from_width {
			return b.emit1(.trunc, target_type, value)
		}
		if to_width > from_width {
			if b.is_unsigned_type(from_type) {
				return b.emit1(.zext, target_type, value)
			}
			return b.emit1(.sext, target_type, value)
		}
		return b.emit1(.bitcast, target_type, value)
	}
	if b.is_float_type(from_type) && b.is_int_type(target_type) {
		if b.is_unsigned_type(target_type) {
			return b.emit1(.fptoui, target_type, value)
		}
		return b.emit1(.fptosi, target_type, value)
	}
	if b.is_int_type(from_type) && b.is_float_type(target_type) {
		if b.is_unsigned_type(from_type) {
			return b.emit1(.uitofp, target_type, value)
		}
		return b.emit1(.sitofp, target_type, value)
	}
	return b.emit1(.bitcast, target_type, value)
}

fn (mut b Builder) build_sum_type_cast(target_name string, value_expr flat.NodeId) ?ValueID {
	sum_name := b.canonical_sum_type_name(target_name) or { return none }
	sum_type := b.resolve_type(sum_name)
	if sum_type <= 0 {
		return none
	}
	value := b.build_expr(value_expr)
	variant := b.sum_variant_for_expr(sum_name, value_expr, value) or { return none }
	tag := b.sum_variant_index(sum_name, variant)
	if tag <= 0 {
		return none
	}
	return b.wrap_sum_value(sum_type, variant, tag, value)
}

fn (mut b Builder) wrap_sum_value(sum_type TypeID, variant string, tag int, value ValueID) ValueID {
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(sum_type))
	tag_ptr := b.get_field_ptr(alloca, 'typ')
	tag_val := b.m.get_or_add_const(b.i32_type, tag.str())
	b.emit2(.store, b.void_type, tag_val, tag_ptr)
	field_ptr := b.get_field_ptr(alloca, sum_variant_field_name(variant))
	field_type := b.deref_type(field_ptr)
	field_val := b.coerce_store_value(value, field_type)
	b.emit2(.store, b.void_type, field_val, field_ptr)
	return b.emit1(.load, sum_type, alloca)
}

fn (mut b Builder) heap_copy_value(value ValueID, value_type TypeID) ValueID {
	if b.is_pointer_type(value_type) {
		return value
	}
	ptr_type := b.m.type_store.get_ptr(value_type)
	alloca := b.emit0(.alloca, ptr_type)
	b.emit2(.store, b.void_type, value, alloca)
	size := b.m.type_size(value_type)
	size_const := b.m.get_or_add_const(b.i64_type, '${size}')
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	src := b.emit1(.bitcast, ptr_i8, alloca)
	memdup_ref := b.m.add_value(.func_ref, ptr_i8, 'memdup', b.fn_ids['memdup'])
	copied := b.emit3(.call, ptr_i8, memdup_ref, src, size_const)
	return b.emit1(.bitcast, ptr_type, copied)
}

fn (b &Builder) canonical_sum_type_name(name string) ?string {
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified := '${b.cur_module}.${name}'
		if canonical := b.sum_type_canonical[qualified] {
			return canonical
		}
	}
	if canonical := b.sum_type_canonical[name] {
		return canonical
	}
	if name.contains('.') {
		return none
	}
	short_name := name.all_after('.')
	if canonical := b.sum_type_canonical[short_name] {
		return canonical
	}
	return none
}

fn (mut b Builder) sum_variant_for_expr(sum_name string, expr_id flat.NodeId, value ValueID) ?string {
	mut type_name := ''
	if b.tc != unsafe { nil } && int(expr_id) >= 0 && int(expr_id) < b.a.nodes.len {
		if typ := b.tc.expr_type(expr_id) {
			type_name = typ.name()
		}
		if type_name.len == 0 || type_name == 'unknown' {
			type_name = b.tc.resolve_type(expr_id).name()
		}
	}
	if type_name.len == 0 || type_name == 'unknown' {
		type_name = b.infer_v_type(expr_id)
	}
	if variant := b.find_sum_variant(sum_name, type_name) {
		return variant
	}
	value_type := b.value_type(value)
	variants := b.sum_type_variants[sum_name] or { []string{} }
	for variant in variants {
		variant_type := b.resolve_type(variant)
		if variant_type == value_type {
			return variant
		}
	}
	return none
}

fn (b &Builder) find_sum_variant(sum_name string, type_name string) ?string {
	if type_name.len == 0 {
		return none
	}
	short_type := type_name.all_after('.')
	variants := b.sum_type_variants[sum_name] or { []string{} }
	for variant in variants {
		short_variant := variant.all_after('.')
		if variant == type_name || short_variant == short_type {
			return variant
		}
	}
	return none
}

fn (b &Builder) sum_variant_index(sum_name string, variant string) int {
	short_variant := variant.all_after('.')
	variants := b.sum_type_variants[sum_name] or { []string{} }
	for i, candidate in variants {
		short_candidate := candidate.all_after('.')
		if candidate == variant || short_candidate == short_variant {
			return i + 1
		}
	}
	return 0
}

fn (b &Builder) sum_variant_references_sum(variant string, sum_name string) bool {
	if b.tc == unsafe { nil } {
		return false
	}
	mut visited := map[string]bool{}
	return b.sum_variant_references_sum_inner(variant, sum_name, mut visited)
}

fn (b &Builder) sum_variant_references_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	if variant in visited {
		return false
	}
	visited[variant] = true
	variant_module := if variant.contains('.') { variant.all_before_last('.') } else { '' }
	fields := b.tc.structs[variant] or {
		short := variant.all_after('.')
		b.tc.structs[short] or { []types.StructField{} }
	}
	for field in fields {
		if b.type_name_matches_sum(field.typ.name(), sum_name, variant_module) {
			return true
		}
		field_name := field.typ.name().trim_left('[]&')
		if nested := b.find_sum_variant(sum_name, field_name) {
			if b.sum_variant_references_sum_inner(nested, sum_name, mut visited) {
				return true
			}
		}
	}
	return false
}

fn (b &Builder) type_name_matches_sum(type_name string, sum_name string, module_name string) bool {
	mut clean := type_name
	for clean.starts_with('[]') || clean.starts_with('&') || clean.starts_with('?')
		|| clean.starts_with('!') {
		if clean.starts_with('[]') {
			clean = clean[2..]
		} else {
			clean = clean[1..]
		}
	}
	if clean == sum_name || clean.all_after('.') == sum_name.all_after('.') {
		return true
	}
	if !clean.contains('.') && module_name.len > 0 {
		return '${module_name}.${clean}' == sum_name
	}
	return false
}

fn (b &Builder) is_sum_type_id(typ_id TypeID) bool {
	if typ_id <= 0 {
		return false
	}
	for sum_name, _ in b.sum_type_variants {
		if canonical := b.sum_type_canonical[sum_name] {
			if sum_typ := b.struct_types[canonical] {
				if sum_typ == typ_id {
					return true
				}
			}
		}
	}
	return false
}

fn (b &Builder) sum_type_has_field(typ_id TypeID, field_name string) bool {
	if !b.is_sum_type_id(typ_id) {
		return false
	}
	for sum_name, variants in b.sum_type_variants {
		if canonical := b.sum_type_canonical[sum_name] {
			if sum_typ := b.struct_types[canonical] {
				if sum_typ != typ_id {
					continue
				}
				for variant in variants {
					if sum_variant_field_name(variant) == field_name {
						return true
					}
				}
			}
		}
	}
	return false
}

fn sum_variant_field_name(variant string) string {
	if variant.starts_with('[]') {
		return '_Array_${ssa_c_name(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${ssa_c_name(variant[4..].replace(']', '_'))}'
	}
	return match variant {
		'int' { '_int' }
		'i8' { '_i8' }
		'i16' { '_i16' }
		'i64' { '_i64' }
		'u8', 'byte' { '_u8' }
		'u16' { '_u16' }
		'u32' { '_u32' }
		'u64' { '_u64' }
		'f32' { '_f32' }
		'f64' { '_f64' }
		'bool' { '_bool' }
		'string' { '_string' }
		else { ssa_c_name(variant) }
	}
}

fn ssa_c_name(name string) string {
	return name.replace('.', '__').replace('[]', 'Array_').replace('[', '_').replace(']', '_').replace('&',
		'ptr_').replace(' ', '_').replace(',', '_')
}

fn (mut b Builder) build_array_push_expr(node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	lhs_id := b.a.child(&node, 0)
	rhs_id := b.a.child(&node, 1)
	arr_ptr := b.build_lvalue_addr(lhs_id)
	rhs_val := b.build_expr(rhs_id)
	if node.value == 'push_many' || b.value_type(rhs_val) == b.array_type {
		if fn_id := b.fn_ids['array_push_many'] {
			fn_ref := b.m.add_value(.func_ref, b.void_type, 'array_push_many', fn_id)
			return b.emit3(.call, b.void_type, fn_ref, arr_ptr, rhs_val)
		}
	}
	elem_type := b.value_type(rhs_val)
	elem_ptr := b.emit0(.alloca, b.m.type_store.get_ptr(elem_type))
	b.emit2(.store, b.void_type, rhs_val, elem_ptr)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	elem_arg := if b.value_type(elem_ptr) == ptr_i8 {
		elem_ptr
	} else {
		b.emit1(.bitcast, ptr_i8, elem_ptr)
	}
	if fn_id := b.fn_ids['array_push'] {
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'array_push', fn_id)
		return b.emit3(.call, b.void_type, fn_ref, arr_ptr, elem_arg)
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (mut b Builder) build_short_circuit(node flat.Node) ValueID {
	result_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i1_type))
	lhs := b.build_expr(b.a.child(&node, 0))
	b.emit2(.store, b.void_type, lhs, result_alloca)

	rhs_block := b.m.add_block(b.cur_func, 'sc_rhs')
	merge_block := b.m.add_block(b.cur_func, 'sc_merge')

	if node.op == .logical_and {
		b.emit3(.br, b.void_type, lhs, ValueID(rhs_block), ValueID(merge_block))
	} else {
		b.emit3(.br, b.void_type, lhs, ValueID(merge_block), ValueID(rhs_block))
	}

	b.cur_block = rhs_block
	rhs := b.build_expr(b.a.child(&node, 1))
	b.emit2(.store, b.void_type, rhs, result_alloca)
	b.emit1(.jmp, b.void_type, ValueID(merge_block))

	b.cur_block = merge_block
	return b.emit1(.load, b.i1_type, result_alloca)
}

fn (mut b Builder) build_prefix(node flat.Node, _id flat.NodeId) ValueID {
	child_id := b.a.child(&node, 0)
	child := b.a.nodes[int(child_id)]
	if node.op == .amp && child.kind == .struct_init {
		return b.build_heap_struct_init(child)
	}
	if node.op == .amp && child.kind == .ident {
		if addr := b.vars[child.value] {
			return addr
		}
		if expr_id := b.lookup_const_expr(child.value) {
			if addr := b.const_global_addr(child.value, expr_id) {
				return addr
			}
		}
	}
	if node.op == .amp && child.kind == .selector {
		return b.build_selector_addr(child)
	}
	if node.op == .amp && child.kind == .index {
		return b.build_index_addr(child_id, child)
	}
	val := b.build_expr(child_id)
	match node.op {
		.minus {
			zero := b.m.get_or_add_const(b.i64_type, '0')
			return b.emit2(.sub, b.value_type(val), zero, val)
		}
		.not {
			zero := b.m.get_or_add_const(b.i1_type, '0')
			return b.emit2(.eq, b.i1_type, val, zero)
		}
		.bit_not {
			minus_one := b.m.get_or_add_const(b.i64_type, '-1')
			return b.emit2(.xor, b.value_type(val), val, minus_one)
		}
		.amp {
			return val
		}
		.mul {
			return b.emit1(.load, b.deref_type(val), val)
		}
		else {
			return val
		}
	}
}

fn (mut b Builder) const_global_addr(name string, expr_id flat.NodeId) ?ValueID {
	if int(expr_id) < 0 || int(expr_id) >= b.a.nodes.len {
		return none
	}
	expr := b.a.nodes[int(expr_id)]
	if expr.kind != .struct_init || expr.children_count != 0 {
		return none
	}
	typ_id, _ := b.struct_literal_type(expr.value)
	if typ_id <= 0 {
		return none
	}
	qualified_name := if name.contains('.') || b.cur_module.len == 0 || b.cur_module == 'main' {
		name
	} else {
		b.cur_module + '.' + name
	}
	global_name := '__const_' + ssa_c_name(qualified_name)
	for v in b.m.values {
		if v.kind == .global && v.name == global_name {
			return v.id
		}
	}
	return b.m.add_global(global_name, typ_id)
}

fn (mut b Builder) build_postfix(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	child_id := b.a.child(&node, 0)
	addr := b.build_lvalue_addr(child_id)
	value_type := b.deref_type(addr)
	if value_type <= 0 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	cur := b.emit1(.load, value_type, addr)
	one := b.m.get_or_add_const(value_type, '1')
	op := if node.op == .inc { OpCode.add } else { OpCode.sub }
	result := b.emit2(op, value_type, cur, one)
	b.emit2(.store, b.void_type, result, addr)
	return cur
}

fn (mut b Builder) build_lvalue_addr(id flat.NodeId) ValueID {
	if int(id) < 0 {
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.ident {
			if addr := b.vars[node.value] {
				elem_type := b.deref_type(addr)
				if elem_type > 0 && elem_type < b.m.type_store.types.len {
					elem := b.m.type_store.types[elem_type]
					if elem.kind == .ptr_t {
						return b.emit1(.load, elem_type, addr)
					}
				}
				return addr
			}
		}
		.selector {
			return b.build_selector_addr(node)
		}
		.index {
			return b.build_index_addr(id, node)
		}
		else {}
	}

	val := b.build_expr(id)
	val_type := b.value_type(val)
	tmp := b.emit0(.alloca, b.m.type_store.get_ptr(val_type))
	b.emit2(.store, b.void_type, val, tmp)
	return tmp
}

fn (mut b Builder) build_selector_addr(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	base_id := b.a.child(&node, 0)
	base := b.a.nodes[int(base_id)]
	if node.op == .arrow {
		base_ptr := b.build_arrow_selector_base_ptr(base_id, base)
		return b.get_field_ptr(base_ptr, node.value)
	}
	if base.kind == .ident {
		if addr := b.vars[base.value] {
			if field_ptr := b.smartcast_sum_selector_addr(addr, base_id, node.value) {
				return field_ptr
			}
			return b.get_field_ptr(addr, node.value)
		}
	}
	if base.kind == .selector {
		base_addr := b.build_selector_addr(base)
		return b.get_field_ptr(base_addr, node.value)
	}
	if base.kind == .index {
		base_addr := b.build_index_addr(base_id, base)
		return b.get_field_ptr(base_addr, node.value)
	}
	base_val := b.build_expr(base_id)
	base_typ := b.value_type(base_val)
	if base_typ > 0 && base_typ < b.m.type_store.types.len {
		base_type_kind := b.m.type_store.types[base_typ].kind
		if base_type_kind == .struct_t {
			alloca := b.emit0(.alloca, b.m.type_store.get_ptr(base_typ))
			b.emit2(.store, b.void_type, base_val, alloca)
			return b.get_field_ptr(alloca, node.value)
		}
		if base_type_kind == .ptr_t {
			// The base is already a pointer to the struct, e.g. a `&Struct(ptr)` cast
			// or `(&Struct(ptr)).field` lvalue. Index straight off the pointer; if the
			// pointee struct type is known the cast keeps it, otherwise re-derive it.
			ptr_val := b.coerce_struct_ptr_for_field(base_val, base_id)
			return b.get_field_ptr(ptr_val, node.value)
		}
	}
	return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
}

// coerce_struct_ptr_for_field makes sure a pointer used as a selector base carries the
// struct pointee type, so get_field_ptr can find field offsets. A bare `voidptr`/`&u8`
// (e.g. from a `&Struct(rawptr)` cast that lowered to a generic pointer) is bitcast to a
// pointer to the cast's struct type when that type can be recovered from the AST.
fn (mut b Builder) coerce_struct_ptr_for_field(ptr_val ValueID, base_id flat.NodeId) ValueID {
	ptr_typ := b.value_type(ptr_val)
	if ptr_typ > 0 && ptr_typ < b.m.type_store.types.len {
		elem := b.m.type_store.types[ptr_typ].elem_type
		if elem > 0 && elem < b.m.type_store.types.len
			&& b.m.type_store.types[elem].kind == .struct_t {
			return ptr_val
		}
	}
	struct_name := b.pointer_cast_struct_name(base_id) or { return ptr_val }
	if struct_id := b.struct_types[struct_name] {
		want := b.m.type_store.get_ptr(struct_id)
		if want != ptr_typ {
			return b.emit1(.bitcast, want, ptr_val)
		}
	}
	return ptr_val
}

// pointer_cast_struct_name returns the struct type name targeted by a `&Struct(...)`
// pointer cast expression, stripping the leading `&`.
fn (b &Builder) pointer_cast_struct_name(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := b.a.nodes[int(id)]
	mut name := ''
	if node.kind == .cast_expr {
		name = node.value
	} else if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child := b.a.nodes[int(b.a.child(&node, 0))]
		if child.kind == .cast_expr {
			name = child.value
		}
	}
	if name.len == 0 {
		return none
	}
	name = name.trim_left('&')
	if name in b.struct_types {
		return name
	}
	return none
}

fn (mut b Builder) build_arrow_selector_base_ptr(base_id flat.NodeId, base flat.Node) ValueID {
	if base.kind == .ident {
		if addr := b.vars[base.value] {
			return b.emit1(.load, b.deref_type(addr), addr)
		}
	}
	if base.kind == .selector && b.selector_has_addressable_root(&base) {
		base_addr := b.build_selector_addr(base)
		return b.emit1(.load, b.deref_type(base_addr), base_addr)
	}
	return b.build_expr(base_id)
}

fn (mut b Builder) build_call(id flat.NodeId, node flat.Node) ValueID {
	fn_node_id := b.a.child(&node, 0)
	fn_node := b.a.nodes[int(fn_node_id)]
	fn_name := fn_node.value

	mut is_method := false
	mut is_c_call := false
	mut base_id := flat.NodeId(0)
	mut actual_name := fn_name
	if fn_node.kind == .selector {
		base_id = b.a.child(&fn_node, 0)
		base := b.a.nodes[int(base_id)]
		if base.kind == .ident && base.value == 'C' {
			actual_name = fn_node.value
			is_c_call = true
		} else {
			mut found_name := fn_node.value
			mut resolved_handled := false
			if builder_name := b.builder_method_name_for_base(base, fn_node.value) {
				found_name = builder_name
				is_method = true
				resolved_handled = true
			}
			if !resolved_handled {
				if resolved := b.resolved_call_name(id) {
					found_name = resolved
					if b.resolved_selector_has_receiver(resolved, node) {
						is_method = true
					}
					resolved_handled = true
				}
			}
			if !resolved_handled {
				if typed_name := b.typed_receiver_method_name(base_id, fn_node.value) {
					found_name = typed_name
					is_method = true
					resolved_handled = true
				}
			}
			if !resolved_handled && base.kind == .ident {
				mut candidates := []string{}
				full_name := '${base.value}.${fn_node.value}'
				candidates << full_name
				candidates << full_name.replace('.', '__')
				if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
					qualified_name := '${b.cur_module}.${base.value}.${fn_node.value}'
					candidates << qualified_name
					candidates << qualified_name.replace('.', '__')
				}
				for candidate in candidates {
					if candidate in b.fn_ids {
						found_name = candidate
						break
					}
				}
			}
			if !resolved_handled && base.kind == .selector {
				qualified_base := b.selector_qualified_name(base)
				if qualified_base.len > 0 {
					full_name := '${qualified_base}.${fn_node.value}'
					c_name := full_name.replace('.', '__')
					if full_name in b.fn_ids {
						found_name = full_name
					} else if c_name in b.fn_ids {
						found_name = c_name
					}
				}
			}
			if found_name == fn_node.value {
				base_is_array := b.call_base_is_array(base_id)
				for fname, _ in b.fn_ids {
					if fname.ends_with('.${fn_node.value}') || fname.ends_with('__${fn_node.value}') {
						if fname.starts_with('array.') && !base_is_array {
							continue
						}
						found_name = fname
						is_method = true
						break
					}
				}
			}
			actual_name = found_name
		}
	} else {
		if resolved := b.resolved_call_name(id) {
			actual_name = resolved
		}
	}
	if fn_node.kind == .selector && !is_c_call && actual_name !in b.fn_ids {
		if typed_name := b.typed_receiver_method_name(base_id, fn_node.value) {
			actual_name = typed_name
			is_method = true
		}
	}
	if actual_name.starts_with('C.') {
		actual_name = actual_name[2..]
		is_c_call = true
	}

	if fn_node.kind == .selector && fn_node.value in ['set', 'clear', 'has'] {
		if result := b.build_flag_enum_method_call(base_id, fn_node.value, node) {
			return result
		}
	}
	if fn_node.kind == .selector && fn_node.value == 'delete_last' {
		return b.build_array_len_mutator_call(base_id, fn_node.value)
	}
	if fn_node.kind == .selector && b.call_base_is_array(base_id) {
		if fn_node.value == 'pop' {
			return b.build_array_pop_call(id, base_id)
		}
		if fn_node.value in ['delete_last', 'clear'] {
			return b.build_array_len_mutator_call(base_id, fn_node.value)
		}
		if fn_node.value in ['first', 'last'] {
			return b.build_array_first_last_call(id, base_id, fn_node.value)
		}
		if fn_node.value == 'index' && node.children_count > 1 {
			return b.build_array_index_call(base_id, b.a.child(&node, 1))
		}
		if fn_node.value == 'join' && node.children_count > 1 {
			return b.build_array_join_call(base_id, b.a.child(&node, 1))
		}
		if fn_node.value == 'repeat' && node.children_count > 1 {
			return b.build_array_repeat_call(base_id, b.a.child(&node, 1), flat.NodeId(-1))
		}
		if fn_node.value == 'repeat_to_depth' && node.children_count > 2 {
			return b.build_array_repeat_call(base_id, b.a.child(&node, 1), b.a.child(&node, 2))
		}
	}
	if fn_node.kind == .selector && fn_node.value == 'delete' && node.children_count > 1
		&& b.expr_type_name_for_map(base_id).trim_left('&').starts_with('map[') {
		return b.build_map_delete_call(base_id, b.a.child(&node, 1))
	}
	if fn_node.kind == .selector && fn_node.value == 'clone'
		&& b.expr_type_name_for_map(base_id).trim_left('&').starts_with('map[') {
		return b.build_map_clone_call(base_id)
	}
	if actual_name == 'FILE' {
		if node.children_count > 1 {
			return b.build_expr(b.a.child(&node, 1))
		}
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	if actual_name == 'IError' {
		if node.children_count > 1 {
			return b.build_expr(b.a.child(&node, 1))
		}
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	if actual_name in ['ast.Expr.name', 'Expr.name', 'ast.SelectorExpr.name', 'SelectorExpr.name',
		'ast.[]Expr.name_list', '[]Expr.name_list', 'name_list', 'types__Type__str'] {
		return b.m.add_value(.string_literal, b.str_type, '', 0)
	}
	if actual_name == 'join_path' || actual_name == 'os.join_path' {
		return b.build_join_path_call(node)
	}
	if fn_node.kind == .selector && node.children_count == 2 {
		base := b.a.child_node(fn_node, 0)
		if base.kind == .ident && base.value == 'C' && actual_name !in b.fn_ids {
			return b.build_expr(b.a.child(&node, 1))
		}
	}
	if fn_node.kind == .selector && fn_node.value in ['has', 'all'] && node.children_count == 2 {
		base_type_name := b.checked_expr_type_name(base_id).trim_left('&')
		if b.is_flag_enum_type_name(base_type_name) {
			base_val := b.build_expr(base_id)
			arg_id := b.a.child(&node, 1)
			arg_node := b.a.nodes[int(arg_id)]
			arg := if arg_node.kind == .enum_val && base_type_name.len > 0 {
				b.build_enum_val_with_type(arg_node, base_type_name) or { b.build_expr(arg_id) }
			} else if arg_node.kind == .infix && base_type_name.len > 0 {
				b.build_enum_expr_with_type(arg_node, base_type_name) or { b.build_expr(arg_id) }
			} else {
				b.build_expr(arg_id)
			}
			masked := b.emit2(.and_, b.value_type(base_val), base_val, arg)
			if fn_node.value == 'has' {
				zero := b.m.get_or_add_const(b.value_type(masked), '0')
				return b.emit2(.ne, b.i1_type, masked, zero)
			}
			return b.emit2(.eq, b.i1_type, masked, arg)
		}
	}
	if node.children_count == 2 && actual_name !in b.fn_ids {
		mut cast_name := actual_name
		if fn_node.kind == .selector {
			base := b.a.child_node(fn_node, 0)
			if base.kind == .ident {
				full_name := '${base.value}.${fn_node.value}'
				if _ := b.canonical_sum_type_name(full_name) {
					cast_name = full_name
				}
			}
		}
		if value := b.build_sum_type_cast(cast_name, b.a.child(&node, 1)) {
			return value
		}
		if actual_name in ['TypeID', 'ValueID', 'BlockID', 'NodeId'] {
			return b.build_expr(b.a.child(&node, 1))
		}
	}
	mut resolved_name := actual_name
	if resolved_name !in b.fn_ids && !resolved_name.contains('.') {
		qualified_name := ssa_fn_name_in_module(b.cur_module, resolved_name)
		if qualified_name in b.fn_ids {
			resolved_name = qualified_name
		} else if resolved_name == 'new' && node.children_count == 1 && 'bench.new' in b.fn_ids {
			resolved_name = 'bench.new'
		} else if fn_node.kind == .ident {
			if imported_name := b.unqualified_call_candidate(resolved_name, node.children_count - 1) {
				resolved_name = imported_name
			}
		}
	}
	if resolved_name !in b.fn_ids && resolved_name.contains('.') {
		qualified_name := ssa_fn_name_in_module(b.cur_module, resolved_name)
		if qualified_name in b.fn_ids {
			resolved_name = qualified_name
			if fn_node.kind == .selector {
				if has_receiver := b.fn_signature_has_receiver(resolved_name,
					node.children_count - 1)
				{
					is_method = has_receiver
				}
			}
		}
		c_name := resolved_name.replace('.', '__')
		if c_name in b.fn_ids {
			resolved_name = c_name
		} else if is_method {
			moduleless_name := drop_first_fn_qualifier(resolved_name)
			if moduleless_name != resolved_name && moduleless_name in b.fn_ids {
				resolved_name = moduleless_name
			}
		}
		if resolved_name !in b.fn_ids && is_method {
			unqualified_method := resolved_name.all_after('.')
			if unqualified_method in b.fn_ids {
				resolved_name = unqualified_method
			}
		}
	}
	if resolved_name !in b.fn_ids && resolved_name.contains('__') {
		dotted_name := resolved_name.replace('__', '.')
		if dotted_name in b.fn_ids {
			resolved_name = dotted_name
		} else if is_method {
			unqualified_method := dotted_name.all_after('.')
			if unqualified_method in b.fn_ids {
				resolved_name = unqualified_method
			}
		}
	}
	if resolved_name !in b.fn_ids && resolved_name.contains('.') {
		method_name := resolved_name.all_after_last('.')
		if method_name in ['vstring', 'vstring_with_len', 'vstring_literal',
			'vstring_literal_with_len'] {
			u8_name := 'u8.${method_name}'
			if u8_name in b.fn_ids {
				resolved_name = u8_name
			}
		}
	}
	if fn_node.kind == .selector && fn_node.value == 'str' && !is_c_call
		&& resolved_name !in b.fn_ids {
		base_type_name := b.checked_expr_type_name(base_id).trim_left('&')
		if autostr_name := b.enum_autostr_fn_name(base_type_name) {
			resolved_name = autostr_name
			is_method = true
		}
	}

	// Final receiver classification: once the call target is resolved, trust the
	// signature's parameter count. A `Type.method(args)` selector whose param count
	// already equals the explicit arg count is a static type-function (no receiver),
	// so the base must NOT be prepended. The early heuristics (and
	// `resolved_selector_has_receiver`'s default-true) otherwise pass the type
	// selector as a phantom receiver — e.g. cross-module
	// `token.Token.from_string_tinyv(lit)` shifted its string arg into the wrong
	// registers, corrupting `name.len`.
	if fn_node.kind == .selector && !is_c_call {
		if has_receiver := b.fn_signature_has_receiver(resolved_name, node.children_count - 1) {
			is_method = has_receiver
		}
	}

	mut fn_idx := 0
	if is_c_call {
		if idx := b.c_fn_ids[resolved_name] {
			fn_idx = idx
		} else if idx := b.fn_ids[resolved_name] {
			fn_idx = idx
		} else {
			panic('ssa: unknown function `${actual_name}`')
		}
	} else if idx := b.fn_ids[resolved_name] {
		fn_idx = idx
	} else {
		if fn_node.kind == .ident {
			if _ := b.vars[fn_node.value] {
				return b.build_indirect_call(id, node, fn_node_id)
			}
		} else if fn_node.kind == .selector && b.is_function_value_expr(fn_node_id) {
			return b.build_indirect_call(id, node, fn_node_id)
		}
		panic('ssa: unknown function `${actual_name}`')
	}
	fn_ref := b.m.add_value(.func_ref, b.void_type, resolved_name, fn_idx)
	ret_type := b.m.funcs[fn_idx].typ

	mut param_types := []TypeID{}
	if is_c_call {
		if ft_id := b.c_fn_types[resolved_name] {
			ft := b.m.type_store.types[ft_id]
			param_types = ft.params.clone()
		}
	} else if ft_id := b.fn_types[resolved_name] {
		ft := b.m.type_store.types[ft_id]
		param_types = ft.params.clone()
	}

	if resolved_name == 'map__set' {
		return b.build_map_set_call(node)
	}

	mut args := []ValueID{}
	args << fn_ref
	if resolved_name == 'error_posix' && node.children_count == 1 && param_types.len > 0 {
		args << b.default_system_error_value(param_types[0])
		return b.m.add_instr(.call, b.cur_block, ret_type, args)
	}
	if is_method {
		if param_types.len > 0 {
			pt := b.m.type_store.types[param_types[0]]
			if pt.kind == .ptr_t {
				base_node := b.a.nodes[int(base_id)]
				if base_node.kind == .ident {
					if addr := b.vars[base_node.value] {
						if b.should_pass_ident_addr_for_ptr_param(addr, param_types[0]) {
							args << addr
						} else {
							args << b.build_expr(base_id)
						}
					} else {
						args << b.build_expr(base_id)
					}
				} else if base_node.kind == .selector {
					addr := b.build_selector_addr(base_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_types[0]) {
						args << addr
					} else {
						args << b.build_expr(base_id)
					}
				} else if base_node.kind == .index {
					addr := b.build_index_addr(base_id, base_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_types[0]) {
						args << addr
					} else {
						args << b.build_expr(base_id)
					}
				} else {
					args << b.build_expr(base_id)
				}
			} else {
				args << b.coerce_value_for_param(b.build_expr(base_id), param_types[0])
			}
		} else {
			args << b.build_expr(base_id)
		}
	}
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		param_idx := if is_method { i } else { i - 1 }
		if resolved_name == 'fixed_array_contains_string' && param_idx == 0 {
			if arg := b.build_const_string_array_arg(arg_id) {
				args << arg
				continue
			}
		}
		if param_idx < param_types.len {
			param_type := param_types[param_idx]
			if b.is_option_type(param_type) {
				arg_node := b.a.nodes[int(arg_id)]
				if arg_node.kind == .none_expr {
					args << b.default_value_for_type(param_type)
					continue
				}
				mut arg := b.build_expr(arg_id)
				if b.value_type(arg) != param_type {
					arg = b.build_option_value(param_type, true, arg)
				}
				args << arg
				continue
			}
			pt := b.m.type_store.types[param_type]
			if pt.kind == .ptr_t {
				arg_node := b.a.nodes[int(arg_id)]
				if arg_node.kind == .ident {
					if addr := b.vars[arg_node.value] {
						if b.should_pass_ident_addr_for_ptr_param(addr, param_type) {
							args << addr
							continue
						}
					}
				} else if arg_node.kind == .selector {
					addr := b.build_selector_addr(arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type) {
						args << addr
						continue
					}
				} else if arg_node.kind == .index {
					addr := b.build_index_addr(arg_id, arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type) {
						args << addr
						continue
					}
				}
			}
		}
		args << b.build_expr(arg_id)
	}
	if resolved_name in ['map__keys', 'map__values'] && args.len == 2 && param_types.len == 2 {
		if elem_size := b.map_array_elem_size_arg(id, node) {
			args << elem_size
		} else {
			args << b.m.get_or_add_const(b.i64_type, '1')
		}
	}
	return b.m.add_instr(.call, b.cur_block, ret_type, args)
}

fn (mut b Builder) map_array_elem_size_arg(id flat.NodeId, node flat.Node) ?ValueID {
	mut typ_name := b.checked_expr_type_name(id)
	if typ_name.len == 0 || typ_name == 'unknown' {
		typ_name = node.typ
	}
	if !typ_name.starts_with('[]') {
		return none
	}
	elem_type := b.resolve_type(typ_name[2..])
	elem_size := b.m.type_size(elem_type)
	actual_size := if elem_size > 0 { elem_size } else { 1 }
	return b.m.get_or_add_const(b.i64_type, actual_size.str())
}

fn (b &Builder) unqualified_call_candidate(name string, arg_count int) ?string {
	mut fallback := ''
	for candidate, _ in b.fn_ids {
		if !candidate.ends_with('.${name}') && !candidate.ends_with('__${name}') {
			continue
		}
		if !b.fn_param_count_matches(candidate, arg_count) {
			continue
		}
		if is_single_qualified_name(candidate) {
			return candidate
		}
		if fallback.len == 0 {
			fallback = candidate
		}
	}
	if fallback.len > 0 {
		return fallback
	}
	return none
}

fn (b &Builder) fn_param_count_matches(name string, arg_count int) bool {
	if ft_id := b.fn_types[name] {
		if ft_id > 0 && ft_id < b.m.type_store.types.len {
			return b.m.type_store.types[ft_id].params.len == arg_count
		}
	}
	return false
}

fn is_single_qualified_name(name string) bool {
	mut dots := 0
	for ch in name {
		if ch == `.` {
			dots++
		} else if ch == `_` {
			break
		}
	}
	return dots == 1
}

fn (mut b Builder) coerce_value_for_param(value ValueID, param_type TypeID) ValueID {
	value_type := b.value_type(value)
	if value_type > 0 && value_type < b.m.type_store.types.len {
		typ := b.m.type_store.types[value_type]
		if typ.kind == .ptr_t && typ.elem_type == param_type {
			return b.emit1(.load, param_type, value)
		}
	}
	return value
}

fn (mut b Builder) build_flag_enum_method_call(base_id flat.NodeId, method string, node flat.Node) ?ValueID {
	flag_type_name := b.flag_enum_expr_type_name(base_id)
	if flag_type_name.len == 0 || !b.is_flag_enum_type_name(flag_type_name) {
		return none
	}
	base_addr := b.build_lvalue_addr(base_id)
	flag_type := b.deref_type(base_addr)
	mut flag := if node.children_count > 1 {
		arg_id := b.a.child(&node, 1)
		arg_node := b.a.nodes[int(arg_id)]
		if value := b.enum_const_value_with_type(arg_node, flag_type_name) {
			b.m.get_or_add_const(flag_type, value.str())
		} else {
			b.build_expr(arg_id)
		}
	} else {
		b.m.get_or_add_const(flag_type, '0')
	}
	flag = b.coerce_int_value(flag, flag_type)
	current := b.emit1(.load, flag_type, base_addr)
	if method == 'has' {
		masked := b.emit2(.and_, flag_type, current, flag)
		zero := b.m.get_or_add_const(flag_type, '0')
		return b.emit2(.ne, b.i1_type, masked, zero)
	}
	result := if method == 'set' {
		b.emit2(.or_, flag_type, current, flag)
	} else {
		all_bits := b.m.get_or_add_const(flag_type, '-1')
		inverted := b.emit2(.xor, flag_type, flag, all_bits)
		b.emit2(.and_, flag_type, current, inverted)
	}
	b.emit2(.store, b.void_type, result, base_addr)
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (b &Builder) is_flag_enum_expr(id flat.NodeId) bool {
	return b.is_flag_enum_type_name(b.flag_enum_expr_type_name(id))
}

fn (b &Builder) flag_enum_expr_type_name(id flat.NodeId) string {
	name := b.checked_expr_type_name(id)
	if b.is_flag_enum_type_name(name) {
		return name
	}
	if int(id) < 0 {
		return ''
	}
	node := b.a.nodes[int(id)]
	if node.typ.len > 0 && b.is_flag_enum_type_name(node.typ) {
		return node.typ
	}
	if node.kind == .selector && node.children_count > 0 {
		base_type := b.receiver_type_name(b.a.child(&node, 0)).trim_left('&')
		field_type := b.field_type_name(base_type, node.value)
		if field_type.len > 0 {
			if b.is_flag_enum_type_name(field_type) {
				return field_type
			}
		}
		if node.value == 'flags'
			&& (base_type == 'array' || base_type == 'builtin.array' || base_type.starts_with('[]')) {
			return 'ArrayFlags'
		}
	}
	return ''
}

fn (mut b Builder) coerce_int_value(value ValueID, to_type TypeID) ValueID {
	from_type := b.value_type(value)
	if from_type == to_type || !b.is_int_type(from_type) || !b.is_int_type(to_type) {
		return value
	}
	from_width := b.scalar_type_width(from_type)
	to_width := b.scalar_type_width(to_type)
	if to_width > 0 && from_width > 0 && to_width < from_width {
		return b.emit1(.trunc, to_type, value)
	}
	if to_width > from_width {
		if b.is_unsigned_type(from_type) {
			return b.emit1(.zext, to_type, value)
		}
		return b.emit1(.sext, to_type, value)
	}
	return b.emit1(.bitcast, to_type, value)
}

fn (mut b Builder) build_array_len_mutator_call(base_id flat.NodeId, method string) ValueID {
	arr_addr := b.build_lvalue_addr(base_id)
	len_ptr := b.get_field_ptr(arr_addr, 'len')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	if method == 'delete_last' {
		len32 := b.emit1(.load, b.i32_type, len_ptr)
		len64 := b.emit1(.zext, b.i64_type, len32)
		one := b.m.get_or_add_const(b.i64_type, '1')
		new_len := b.emit2(.sub, b.i64_type, len64, one)
		new_len32 := b.emit1(.trunc, b.i32_type, new_len)
		b.emit2(.store, b.void_type, new_len32, len_ptr)
	} else {
		zero32 := b.m.get_or_add_const(b.i32_type, '0')
		b.emit2(.store, b.void_type, zero32, len_ptr)
	}
	return zero
}

fn (mut b Builder) build_array_pop_call(id flat.NodeId, base_id flat.NodeId) ValueID {
	arr_addr := b.build_lvalue_addr(base_id)
	base := b.emit1(.load, b.array_type, arr_addr)
	elem_type := b.array_method_elem_type(id, base_id)
	len_ptr := b.get_field_ptr(arr_addr, 'len')
	len32 := b.emit1(.load, b.i32_type, len_ptr)
	len64 := b.emit1(.zext, b.i64_type, len32)
	one := b.m.get_or_add_const(b.i64_type, '1')
	index := b.emit2(.sub, b.i64_type, len64, one)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.fn_ids['array_get'])
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
	value := b.emit1(.load, elem_type, typed_ptr)
	new_len32 := b.emit1(.trunc, b.i32_type, index)
	b.emit2(.store, b.void_type, new_len32, len_ptr)
	return value
}

fn (mut b Builder) build_array_first_last_call(id flat.NodeId, base_id flat.NodeId, method string) ValueID {
	base := b.build_expr(base_id)
	elem_type := b.array_method_elem_type(id, base_id)
	index := if method == 'last' {
		len := b.load_struct_field_from_value(base, b.array_type, 'len')
		one := b.m.get_or_add_const(b.i64_type, '1')
		b.emit2(.sub, b.i64_type, len, one)
	} else {
		b.m.get_or_add_const(b.i64_type, '0')
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.fn_ids['array_get'])
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
	return b.emit1(.load, elem_type, typed_ptr)
}

fn (mut b Builder) build_array_index_call(base_id flat.NodeId, needle_id flat.NodeId) ValueID {
	base := b.build_expr(base_id)
	elem_type := b.array_receiver_elem_type(base_id)
	fn_name := if elem_type == b.str_type { 'array_index_string' } else { 'array_index_int' }
	needle_type := if elem_type == b.str_type { b.str_type } else { b.i64_type }
	mut needle := b.build_expr(needle_id)
	needle = b.coerce_int_value(needle, needle_type)
	fn_ref := b.m.add_value(.func_ref, b.i64_type, fn_name, b.fn_ids[fn_name])
	return b.emit3(.call, b.i64_type, fn_ref, base, needle)
}

fn (mut b Builder) build_array_join_call(base_id flat.NodeId, sep_id flat.NodeId) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	base := b.build_expr(base_id)
	sep := b.build_expr(sep_id)
	arr_alloca := b.emit0(.alloca, ptr_array)
	builder_alloca := b.emit0(.alloca, ptr_array)
	i_alloca := b.emit0(.alloca, ptr_i64)
	b.emit2(.store, b.void_type, base, arr_alloca)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	b.emit2(.store, b.void_type, zero, i_alloca)

	new_ref := b.m.add_value(.func_ref, b.array_type, 'strings.new_builder',
		b.fn_ids['strings.new_builder'])
	initial_cap := b.m.get_or_add_const(b.i64_type, '16')
	builder := b.emit2(.call, b.array_type, new_ref, initial_cap)
	b.emit2(.store, b.void_type, builder, builder_alloca)

	data_ptr := b.get_field_ptr(arr_alloca, 'data')
	len_ptr := b.get_field_ptr(arr_alloca, 'len')
	data := b.emit1(.load, ptr_i8, data_ptr)
	len32 := b.emit1(.load, b.i32_type, len_ptr)
	len := b.emit1(.zext, b.i64_type, len32)

	loop := b.m.add_block(b.cur_func, 'array_join_loop')
	body := b.m.add_block(b.cur_func, 'array_join_body')
	write_sep := b.m.add_block(b.cur_func, 'array_join_write_sep')
	write_elem := b.m.add_block(b.cur_func, 'array_join_write_elem')
	done := b.m.add_block(b.cur_func, 'array_join_done')
	b.emit1(.jmp, b.void_type, ValueID(loop))

	b.cur_block = loop
	i_val := b.emit1(.load, b.i64_type, i_alloca)
	more := b.emit2(.lt, b.i1_type, i_val, len)
	b.emit3(.br, b.void_type, more, ValueID(body), ValueID(done))

	b.cur_block = body
	needs_sep := b.emit2(.gt, b.i1_type, i_val, zero)
	b.emit3(.br, b.void_type, needs_sep, ValueID(write_sep), ValueID(write_elem))

	b.cur_block = write_sep
	write_ref := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_string',
		b.fn_ids['strings.Builder.write_string'])
	b.emit3(.call, b.void_type, write_ref, builder_alloca, sep)
	b.emit1(.jmp, b.void_type, ValueID(write_elem))

	b.cur_block = write_elem
	elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	offset := b.emit2(.mul, b.i64_type, i_val, elem_size)
	elem_ptr_raw := b.emit2(.add, ptr_i8, data, offset)
	elem_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(b.str_type), elem_ptr_raw)
	elem := b.emit1(.load, b.str_type, elem_ptr)
	write_ref2 := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_string',
		b.fn_ids['strings.Builder.write_string'])
	b.emit3(.call, b.void_type, write_ref2, builder_alloca, elem)
	next_i := b.emit2(.add, b.i64_type, i_val, one)
	b.emit2(.store, b.void_type, next_i, i_alloca)
	b.emit1(.jmp, b.void_type, ValueID(loop))

	b.cur_block = done
	str_ref := b.m.add_value(.func_ref, b.str_type, 'strings.Builder.str',
		b.fn_ids['strings.Builder.str'])
	return b.emit2(.call, b.str_type, str_ref, builder_alloca)
}

fn (mut b Builder) build_array_repeat_call(base_id flat.NodeId, count_id flat.NodeId, depth_id flat.NodeId) ValueID {
	base := b.build_expr(base_id)
	mut count := b.build_expr(count_id)
	count = b.coerce_int_value(count, b.i64_type)
	depth := if int(depth_id) >= 0 {
		mut depth_value := b.build_expr(depth_id)
		depth_value = b.coerce_int_value(depth_value, b.i64_type)
		depth_value
	} else {
		b.m.get_or_add_const(b.i64_type, '0')
	}
	fn_ref := b.m.add_value(.func_ref, b.array_type, 'array.repeat_to_depth',
		b.fn_ids['array.repeat_to_depth'])
	return b.emit4(.call, b.array_type, fn_ref, base, count, depth)
}

fn (mut b Builder) array_receiver_elem_type(base_id flat.NodeId) TypeID {
	base_name := b.receiver_type_name(base_id)
	if base_name.starts_with('[]') {
		return b.resolve_type(base_name[2..])
	}
	return b.i64_type
}

fn (mut b Builder) array_method_elem_type(id flat.NodeId, base_id flat.NodeId) TypeID {
	ret_name := b.checked_expr_type_name(id)
	if ret_name.len > 0 && ret_name != 'unknown' && !ret_name.starts_with('[]') {
		return b.resolve_type(ret_name)
	}
	base_name := b.receiver_type_name(base_id)
	if base_name.starts_with('[]') {
		return b.resolve_type(base_name[2..])
	}
	return b.i64_type
}

fn (b &Builder) call_base_is_array(base_id flat.NodeId) bool {
	if int(base_id) < 0 {
		return false
	}
	base_name := b.receiver_type_name(base_id)
	if base_name.starts_with('[]') {
		return true
	}
	base_node := b.a.nodes[int(base_id)]
	if base_node.kind == .ident {
		if addr := b.vars[base_node.value] {
			if b.storage_points_to_array(addr) {
				return true
			}
		}
	}
	return false
}

fn (b &Builder) storage_points_to_array(addr ValueID) bool {
	typ := b.deref_type(addr)
	if typ == b.array_type {
		return true
	}
	if typ > 0 && typ < b.m.type_store.types.len {
		t := b.m.type_store.types[typ]
		return t.kind == .ptr_t && t.elem_type == b.array_type
	}
	return false
}

fn (b &Builder) receiver_type_name(base_id flat.NodeId) string {
	if int(base_id) < 0 {
		return ''
	}
	base_node := b.a.nodes[int(base_id)]
	if base_node.kind == .ident {
		if var_name := b.var_type_names[base_node.value] {
			return var_name
		}
	} else if base_node.kind == .selector && base_node.children_count > 0 {
		base_type := b.receiver_type_name(b.a.child(&base_node, 0)).trim_left('&')
		field_type := b.field_type_name(base_type, base_node.value)
		if field_type.len > 0 {
			return field_type
		}
	}
	return b.checked_expr_type_name(base_id)
}

fn (b &Builder) typed_receiver_method_name(base_id flat.NodeId, method string) ?string {
	receiver_name := b.receiver_type_name(base_id).trim_left('&')
	if receiver_name.len == 0 {
		return none
	}
	if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin'
		&& !receiver_name.contains('.') {
		module_name := '${b.cur_module}.${receiver_name}.${method}'
		if module_name in b.fn_ids {
			return module_name
		}
		module_c_name := module_name.replace('.', '__')
		if module_c_name in b.fn_ids {
			return module_c_name
		}
	}
	mut candidates := []string{}
	candidates << '${receiver_name}.${method}'
	candidates << '${receiver_name.replace('.', '__')}__${method}'
	short_name := receiver_name.all_after_last('.')
	if short_name != receiver_name {
		candidates << '${short_name}.${method}'
		candidates << '${short_name}__${method}'
	}
	for candidate in candidates {
		if candidate in b.fn_ids {
			return candidate
		}
	}
	return none
}

fn (mut b Builder) build_indirect_call(id flat.NodeId, node flat.Node, callee_id flat.NodeId) ValueID {
	callee := b.build_expr(callee_id)
	mut args := []ValueID{}
	args << callee
	for i in 1 .. node.children_count {
		args << b.build_expr(b.a.child(&node, i))
	}
	return b.m.add_instr(.call_indirect, b.cur_block, b.call_expr_result_type(id, node), args)
}

fn (b &Builder) is_function_value_expr(id flat.NodeId) bool {
	typ := b.checked_expr_type_name(id).trim_space()
	return typ.starts_with('fn(') || typ.starts_with('fn (')
}

fn (mut b Builder) call_expr_result_type(id flat.NodeId, node flat.Node) TypeID {
	checked := b.checked_expr_type_name(id)
	if checked.len > 0 && checked != 'unknown' {
		return b.resolve_type(checked)
	}
	if node.typ.len > 0 && node.typ != 'unknown' {
		return b.resolve_type(node.typ)
	}
	return b.i64_type
}

fn (mut b Builder) build_const_string_array_arg(id flat.NodeId) ?ValueID {
	if int(id) < 0 {
		return none
	}
	expr_id := b.const_string_array_expr_id(id) or { return none }
	expr := b.a.nodes[int(expr_id)]
	if expr.kind != .array_literal || expr.children_count == 0 {
		return none
	}
	for i in 0 .. expr.children_count {
		child := b.a.nodes[int(b.a.child(&expr, i))]
		if child.kind != .string_literal {
			return none
		}
	}
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	count := b.m.get_or_add_const(b.i64_type, '${expr.children_count}')
	alloca := b.emit1(.alloca, ptr_string, count)
	stride := 16
	for i in 0 .. expr.children_count {
		child_id := b.a.child(&expr, i)
		value := b.build_expr(child_id)
		off := b.m.get_or_add_const(b.i64_type, '${i * stride}')
		slot := b.emit2(.get_element_ptr, ptr_string, alloca, off)
		b.emit2(.store, b.void_type, value, slot)
	}
	return b.emit1(.bitcast, ptr_i8, alloca)
}

fn (b &Builder) const_string_array_expr_id(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := b.a.nodes[int(id)]
	if node.kind == .array_literal {
		return id
	}
	if node.kind == .cast_expr && node.children_count > 0 {
		return b.const_string_array_expr_id(b.a.child(&node, 0))
	}
	if node.kind == .ident {
		return b.lookup_const_expr(node.value)
	}
	if node.kind == .selector {
		if node.value == 'data' && node.children_count > 0 {
			return b.const_string_array_expr_id(b.a.child(&node, 0))
		}
		name := b.selector_qualified_name(node)
		if name.len > 0 {
			return b.lookup_const_expr(name)
		}
	}
	return none
}

fn (mut b Builder) default_system_error_value(typ_id TypeID) ValueID {
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(typ_id))
	typ := b.m.type_store.types[typ_id]
	for fi, fname in typ.field_names {
		offset := b.m.struct_field_offset(typ_id, fi)
		off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
		field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
		field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type), alloca,
			off_const)
		default_value := if fname == 'msg' {
			b.m.add_value(.string_literal, b.str_type, '', 0)
		} else if fname == 'code' {
			b.m.get_or_add_const(field_type, '-1')
		} else {
			b.default_value_for_type(field_type)
		}
		b.emit2(.store, b.void_type, default_value, field_ptr)
	}
	return b.emit1(.load, typ_id, alloca)
}

fn (mut b Builder) build_const_i32_array_arg(id flat.NodeId) ?ValueID {
	if int(id) < 0 {
		return none
	}
	node := b.a.nodes[int(id)]
	if node.kind != .ident {
		return none
	}
	expr_id := b.lookup_const_expr(node.value) or { return none }
	expr := b.a.nodes[int(expr_id)]
	if expr.kind != .array_literal || expr.children_count == 0 {
		return none
	}
	mut values := []string{}
	for i in 0 .. expr.children_count {
		child_id := b.a.child(&expr, i)
		value := b.const_int_literal_value(child_id) or { return none }
		values << value
	}
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	count := b.m.get_or_add_const(b.i64_type, '${values.len}')
	alloca := b.emit1(.alloca, ptr_i32, count)
	for i, value in values {
		const_val := b.m.get_or_add_const(b.i32_type, value)
		off := b.m.get_or_add_const(b.i64_type, '${i * b.m.type_size(b.i32_type)}')
		slot := b.emit2(.get_element_ptr, ptr_i32, alloca, off)
		b.emit2(.store, b.void_type, const_val, slot)
	}
	return alloca
}

fn (b &Builder) const_int_literal_value(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return node.value
		}
		.ident {
			if expr_id := b.lookup_const_expr(node.value) {
				return b.const_int_literal_value(expr_id)
			}
		}
		.paren, .cast_expr {
			if node.children_count > 0 {
				return b.const_int_literal_value(b.a.child(&node, 0))
			}
		}
		.prefix {
			if node.children_count > 0 && node.op == .minus {
				if value := b.const_int_literal_value(b.a.child(&node, 0)) {
					return '-' + value
				}
			}
		}
		.infix {
			if node.children_count >= 2 {
				left := b.const_int_literal_value(b.a.child(&node, 0)) or { return none }
				right := b.const_int_literal_value(b.a.child(&node, 1)) or { return none }
				l := left.i64()
				r := right.i64()
				return match node.op {
					.plus {
						'${l + r}'
					}
					.minus {
						'${l - r}'
					}
					.mul {
						'${l * r}'
					}
					.div {
						if r != 0 {
							'${l / r}'
						} else {
							return none
						}
					}
					.mod {
						if r != 0 {
							'${l % r}'
						} else {
							return none
						}
					}
					.left_shift {
						if r >= 0 && r < 64 {
							'${i64(u64(l) << u64(r))}'
						} else {
							return none
						}
					}
					.right_shift {
						if r >= 0 && r < 64 {
							'${l >> u64(r)}'
						} else {
							return none
						}
					}
					.amp {
						'${l & r}'
					}
					.pipe {
						'${l | r}'
					}
					.xor {
						'${l ^ r}'
					}
					else {
						return none
					}
				}
			}
		}
		.call {
			if node.children_count == 2 {
				return b.const_int_literal_value(b.a.child(&node, 1))
			}
		}
		else {}
	}

	return none
}

fn (b &Builder) lookup_const_expr(name string) ?flat.NodeId {
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main' {
		qualified := '${b.cur_module}.${name}'
		if expr := b.const_exprs[qualified] {
			return expr
		}
	}
	if expr := b.const_exprs[name] {
		return expr
	}
	return none
}

fn (mut b Builder) float_literal_type(id flat.NodeId, node flat.Node) TypeID {
	checked := b.checked_expr_type_name(id)
	if checked == 'f32' || node.typ == 'f32' {
		return b.f32_type
	}
	return b.f64_type
}

fn (mut b Builder) build_map_set_call(node flat.Node) ValueID {
	fn_idx := b.fn_ids['v3_map_set_sized']
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_set_sized', fn_idx)
	mut args := []ValueID{}
	args << fn_ref
	for i in 1 .. node.children_count {
		args << b.build_expr(b.a.child(&node, i))
	}
	key_size, val_size := b.map_set_call_sizes(node)
	args << b.m.get_or_add_const(b.i64_type, '${key_size}')
	args << b.m.get_or_add_const(b.i64_type, '${val_size}')
	return b.m.add_instr(.call, b.cur_block, b.void_type, args)
}

fn (mut b Builder) build_map_delete_call(base_id flat.NodeId, key_id flat.NodeId) ValueID {
	map_type := b.expr_type_name_for_map(base_id).trim_left('&')
	key_type_name, _ := map_type_parts(map_type)
	key_type := if key_type_name.len > 0 { b.resolve_type(key_type_name) } else { b.str_type }
	map_ptr := b.map_expr_ptr(base_id)
	key_val := b.build_expr(key_id)
	key_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(key_type))
	b.emit2(.store, b.void_type, key_val, key_alloca)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	key_ptr := if b.value_type(key_alloca) == ptr_i8 {
		key_alloca
	} else {
		b.emit1(.bitcast, ptr_i8, key_alloca)
	}
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'map__delete', b.fn_ids['map__delete'])
	return b.emit3(.call, b.void_type, fn_ref, map_ptr, key_ptr)
}

fn (mut b Builder) build_map_clone_call(base_id flat.NodeId) ValueID {
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'map__clone', b.fn_ids['map__clone'])
	base := b.coerce_value_for_param(b.build_expr(base_id), b.map_type)
	return b.emit2(.call, b.map_type, fn_ref, base)
}

fn (mut b Builder) map_set_call_sizes(node flat.Node) (int, int) {
	if node.children_count <= 1 {
		return 16, 8
	}
	map_type := b.map_arg_type_name(b.a.child(&node, 1))
	key_type, val_type := map_type_parts(map_type)
	if key_type.len == 0 || val_type.len == 0 {
		return 16, 8
	}
	key_size := b.m.type_size(b.resolve_type(key_type))
	val_size := b.m.type_size(b.resolve_type(val_type))
	actual_key_size := if key_size > 0 { key_size } else { 8 }
	actual_val_size := if val_size > 0 { val_size } else { 8 }
	return actual_key_size, actual_val_size
}

fn (b &Builder) map_arg_type_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := b.a.nodes[int(id)]
	if node.kind == .prefix && node.children_count > 0 && node.op == .amp {
		return b.expr_type_name_for_map(b.a.child(&node, 0)).trim_left('&')
	}
	return b.expr_type_name_for_map(id).trim_left('&')
}

fn (b &Builder) expr_type_name_for_map(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := b.a.nodes[int(id)]
	if node.kind == .ident {
		if var_name := b.var_type_names[node.value] {
			if var_name.len > 0 {
				return var_name
			}
		}
	}
	checked := b.checked_expr_type_name(id)
	if checked.len > 0 && checked != 'unknown' {
		return checked
	}
	if node.typ.len > 0 {
		return node.typ
	}
	if node.kind == .prefix && node.children_count > 0 && node.op == .amp {
		inner := b.expr_type_name_for_map(b.a.child(&node, 0))
		if inner.len > 0 {
			return '&' + inner
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_type := b.expr_type_name_for_map(b.a.child(&node, 0)).trim_left('&')
		field_key := base_type + '.' + node.value
		if field_type := b.struct_field_types[field_key] {
			return field_type
		}
		short_type := base_type.all_after('.')
		short_key := short_type + '.' + node.value
		if field_type := b.struct_field_types[short_key] {
			return field_type
		}
	}
	return ''
}

fn map_type_parts(map_type string) (string, string) {
	clean := map_type.trim_left('&')
	if !clean.starts_with('map[') {
		return '', ''
	}
	mut depth := 0
	for i in 4 .. clean.len {
		c := clean[i]
		if c == `[` {
			depth++
		} else if c == `]` {
			if depth == 0 {
				return clean[4..i], clean[i + 1..]
			}
			depth--
		}
	}
	return '', ''
}

fn indexed_elem_type_name(container_type string) string {
	clean := container_type.trim_left('&')
	if clean.starts_with('[]') {
		return clean[2..]
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 && idx + 1 < clean.len {
			return clean[idx + 1..]
		}
	}
	if clean.starts_with('map[') {
		_, val_type := map_type_parts(clean)
		return val_type
	}
	if clean == 'string' {
		return 'u8'
	}
	return ''
}

fn (mut b Builder) build_join_path_call(node flat.Node) ValueID {
	if node.children_count <= 1 {
		return b.m.add_value(.string_literal, b.str_type, '', 0)
	}
	mut result := b.build_expr(b.a.child(&node, 1))
	if node.children_count == 2 {
		return result
	}
	mut fn_idx := 0
	mut found_fn := false
	if idx := b.fn_ids['join_path_single'] {
		fn_idx = idx
		found_fn = true
	}
	if !found_fn {
		if idx := b.fn_ids['os.join_path_single'] {
			fn_idx = idx
			found_fn = true
		}
	}
	if !found_fn {
		panic('ssa: unknown function `join_path_single`')
	}
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'join_path_single', fn_idx)
	for i in 2 .. node.children_count {
		arg := b.build_expr(b.a.child(&node, i))
		mut args := []ValueID{}
		args << fn_ref
		args << result
		args << arg
		result = b.m.add_instr(.call, b.cur_block, b.str_type, args)
	}
	return result
}

fn (b &Builder) declared_v_type_name(lhs_id flat.NodeId, rhs_id flat.NodeId) string {
	if int(rhs_id) >= 0 {
		rhs := b.a.nodes[int(rhs_id)]
		if rhs.kind == .array_init && b.is_fixed_array_type_name(rhs.value) {
			return rhs.value
		}
		if rhs.kind == .call && rhs.children_count > 0 {
			fn_node := b.a.child_node(&rhs, 0)
			if fn_node.kind == .selector && fn_node.children_count > 0
				&& fn_node.value == 'new_builder' {
				base := b.a.child_node(fn_node, 0)
				if base.kind == .ident && base.value == 'strings' {
					return 'strings.Builder'
				}
			}
		}
		if rhs.typ.len > 0 && rhs.typ != 'unknown' {
			return rhs.typ
		}
	}
	if int(lhs_id) >= 0 {
		lhs := b.a.nodes[int(lhs_id)]
		if lhs.typ.len > 0 {
			return lhs.typ
		}
	}
	lhs_type := b.checked_expr_type_name(lhs_id)
	if lhs_type.len > 0 && lhs_type != 'unknown' {
		return lhs_type
	}
	rhs_type := b.checked_expr_type_name(rhs_id)
	if rhs_type.len > 0 && rhs_type != 'unknown' {
		return rhs_type
	}
	return ''
}

fn (b &Builder) checked_expr_type_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := b.a.nodes[int(id)]
	if node.kind == .selector {
		if typ := b.selector_type_name(node) {
			return typ
		}
	}
	if node.kind == .index && node.children_count > 0 {
		base_type := b.checked_expr_type_name(b.a.child(&node, 0))
		elem_type := indexed_elem_type_name(base_type)
		if elem_type.len > 0 {
			return elem_type
		}
	}
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			if typ is types.MultiReturn {
				return b.multi_return_c_type(typ)
			}
			name := typ.name()
			if name.len > 0 && name != 'unknown' {
				return name
			}
		}
	}
	if node.typ.len > 0 && node.typ != 'unknown' {
		return node.typ
	}
	if node.kind == .ident {
		if typ := b.var_type_names[node.value] {
			return typ
		}
	}
	return ''
}

fn (b &Builder) selector_type_name(node flat.Node) ?string {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := b.a.child(&node, 0)
	base_type := b.checked_expr_type_name(base_id)
	if base_type.len == 0 {
		return none
	}
	if sum_name := b.canonical_sum_type_name(base_type) {
		if variants := b.sum_type_variants[sum_name] {
			for variant in variants {
				if sum_variant_field_name(variant) == node.value {
					return variant
				}
			}
		}
	}
	field_type := b.field_type_name(base_type, node.value)
	if field_type.len > 0 {
		return field_type
	}
	return none
}

fn (b &Builder) builder_method_name_for_base(base flat.Node, method string) ?string {
	if base.kind != .ident {
		return none
	}
	if !is_builder_method(method) {
		return none
	}
	mut receiver_type := b.var_type_names[base.value] or { '' }
	if receiver_type.starts_with('&') {
		receiver_type = receiver_type[1..]
	}
	if receiver_type != 'strings.Builder' && receiver_type != 'Builder' {
		return none
	}
	name := 'strings.Builder.${method}'
	if name in b.fn_ids {
		return name
	}
	return none
}

fn is_builder_method(method string) bool {
	return method in ['write_string', 'writeln', 'str', 'free', 'write_u8', 'write_ptr',
		'write_runes', 'last_n', 'push_many']
}

fn (b &Builder) resolved_call_name(id flat.NodeId) ?string {
	if b.tc == unsafe { nil } {
		return none
	}
	if name := b.tc.resolved_call_name(id) {
		if name.len > 0 {
			return name
		}
	}
	return none
}

fn drop_first_fn_qualifier(name string) string {
	idx := name.index('.') or { return name }
	if idx + 1 >= name.len {
		return name
	}
	return name[idx + 1..]
}

fn (b &Builder) resolved_selector_has_receiver(resolved string, node flat.Node) bool {
	explicit_arg_count := node.children_count - 1
	if has_receiver := b.fn_signature_has_receiver(resolved, explicit_arg_count) {
		return has_receiver
	}
	if resolved.contains('.') {
		c_name := resolved.replace('.', '__')
		if has_receiver := b.fn_signature_has_receiver(c_name, explicit_arg_count) {
			return has_receiver
		}
		unqualified := resolved.all_after('.')
		if has_receiver := b.fn_signature_has_receiver(unqualified, explicit_arg_count) {
			return has_receiver
		}
	}
	if resolved.contains('__') {
		dotted := resolved.replace('__', '.')
		if has_receiver := b.fn_signature_has_receiver(dotted, explicit_arg_count) {
			return has_receiver
		}
		unqualified := dotted.all_after('.')
		if has_receiver := b.fn_signature_has_receiver(unqualified, explicit_arg_count) {
			return has_receiver
		}
	}
	return true
}

fn (b &Builder) fn_signature_has_receiver(name string, explicit_arg_count int) ?bool {
	if ft_id := b.fn_types[name] {
		ft := b.m.type_store.types[ft_id]
		if ft.params.len == explicit_arg_count {
			return false
		}
		if ft.params.len < explicit_arg_count {
			return false
		}
		if ft.params.len == explicit_arg_count + 1 {
			return true
		}
	}
	return none
}

fn (b &Builder) should_pass_ident_addr_for_ptr_param(addr ValueID, param_type TypeID) bool {
	if param_type <= 0 || param_type >= b.m.type_store.types.len {
		return false
	}
	param := b.m.type_store.types[param_type]
	if param.kind != .ptr_t {
		return false
	}
	arg_type := b.deref_type(addr)
	if arg_type <= 0 || arg_type >= b.m.type_store.types.len {
		return true
	}
	arg := b.m.type_store.types[arg_type]
	return arg.kind != .ptr_t
}

fn (mut b Builder) build_selector(node flat.Node) ValueID {
	base_id := b.a.child(&node, 0)
	base := b.a.nodes[int(base_id)]
	field_name := node.value

	if base.kind == .ident {
		if value := b.enum_value_for_type(base.value, field_name) {
			return b.m.get_or_add_const(b.i64_type, value.str())
		}
		if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
			if value := b.enum_value_for_type(b.cur_module + '.' + base.value, field_name) {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}
	if base.kind == .selector && base.children_count > 0 {
		enum_base := b.selector_qualified_name(base)
		if enum_base.len > 0 {
			if value := b.enum_value_for_type(enum_base, field_name) {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}

	if base.kind == .ident && base.value == 'os' {
		if field_name == 'args' {
			if fn_id := b.fn_ids['arguments'] {
				fn_ref := b.m.add_value(.func_ref, b.array_type, 'arguments', fn_id)
				return b.emit1(.call, b.array_type, fn_ref)
			}
		}
		if field_name == 'path_separator' {
			return b.m.add_value(.string_literal, b.str_type, '/', 0)
		}
	}

	if base.kind == .ident && base.value == 'C' {
		match field_name {
			'SEEK_SET' {
				return b.m.get_or_add_const(b.i64_type, '0')
			}
			'SEEK_CUR' {
				return b.m.get_or_add_const(b.i64_type, '1')
			}
			'SEEK_END' {
				return b.m.get_or_add_const(b.i64_type, '2')
			}
			else {}
		}
	}

	if base.kind == .ident {
		const_name := base.value + '.' + field_name
		if expr_id := b.lookup_const_expr(const_name) {
			return b.build_expr(expr_id)
		}
		if addr := b.vars[base.value] {
			if b.is_option_like_var(base.value) {
				field_ptr := b.get_field_ptr(addr, field_name)
				return b.emit1(.load, b.deref_type(field_ptr), field_ptr)
			}
			if field_name == 'len' && b.deref_type(addr) == b.map_type {
				return b.load_map_len(addr)
			}
			if field := b.load_smartcast_sum_selector(addr, base_id, field_name) {
				return field
			}
			field_ptr := b.get_field_ptr(addr, field_name)
			return b.load_selector_field(node, b.deref_type(addr), field_ptr)
		}
	}
	if base.kind == .selector && b.selector_has_addressable_root(&base) {
		if field_name == 'len' {
			base_addr := b.build_selector_addr(base)
			if b.deref_type(base_addr) == b.map_type {
				return b.load_map_len(base_addr)
			}
		}
		field_ptr := b.build_selector_addr(node)
		return b.emit1(.load, b.deref_type(field_ptr), field_ptr)
	}
	base_val := b.build_expr(base_id)
	base_typ := b.value_type(base_val)
	if field_name == 'len' && base_typ == b.map_type {
		alloca := b.emit0(.alloca, b.m.type_store.get_ptr(base_typ))
		b.emit2(.store, b.void_type, base_val, alloca)
		return b.load_map_len(alloca)
	}
	if base_typ > 0 && base_typ < b.m.type_store.types.len {
		base_type := b.m.type_store.types[base_typ]
		if base_type.kind == .ptr_t && base_type.elem_type > 0
			&& base_type.elem_type < b.m.type_store.types.len
			&& b.m.type_store.types[base_type.elem_type].kind == .struct_t {
			field_ptr := b.get_field_ptr(base_val, field_name)
			return b.load_selector_field(node, base_type.elem_type, field_ptr)
		}
	}
	if base_typ > 0 && base_typ < b.m.type_store.types.len
		&& b.m.type_store.types[base_typ].kind == .struct_t {
		alloca := b.emit0(.alloca, b.m.type_store.get_ptr(base_typ))
		b.emit2(.store, b.void_type, base_val, alloca)
		field_ptr := b.get_field_ptr(alloca, field_name)
		return b.load_selector_field(node, base_typ, field_ptr)
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (mut b Builder) load_smartcast_sum_value(sum_addr ValueID, expr_id flat.NodeId) ?ValueID {
	sum_type := b.deref_type(sum_addr)
	if !b.is_sum_type_id(sum_type) {
		return none
	}
	checked_name := b.checked_expr_type_name(expr_id)
	if checked_name.len == 0 || b.resolve_type(checked_name) == sum_type {
		return none
	}
	sum_name := b.sum_name_for_type_id(sum_type) or { return none }
	variant := b.find_sum_variant(sum_name, checked_name) or { return none }
	variant_type := b.resolve_type(variant)
	if variant_type <= 0 || variant_type >= b.m.type_store.types.len {
		return none
	}
	payload_ptr := b.get_field_ptr(sum_addr, sum_variant_field_name(variant))
	payload_type := b.deref_type(payload_ptr)
	if payload_type > 0 && payload_type < b.m.type_store.types.len {
		payload_layout := b.m.type_store.types[payload_type]
		if payload_layout.kind == .ptr_t {
			variant_addr := b.emit1(.load, payload_type, payload_ptr)
			return b.emit1(.load, variant_type, variant_addr)
		}
	}
	return b.emit1(.load, variant_type, payload_ptr)
}

fn (mut b Builder) load_smartcast_sum_selector(sum_addr ValueID, base_id flat.NodeId, field_name string) ?ValueID {
	field_ptr := b.smartcast_sum_selector_addr(sum_addr, base_id, field_name) or { return none }
	return b.emit1(.load, b.deref_type(field_ptr), field_ptr)
}

fn (mut b Builder) smartcast_sum_selector_addr(sum_addr ValueID, base_id flat.NodeId, field_name string) ?ValueID {
	sum_type := b.deref_type(sum_addr)
	if !b.is_sum_type_id(sum_type) {
		return none
	}
	base_type_name := b.checked_expr_type_name(base_id)
	if base_type_name.len == 0 {
		return none
	}
	if b.resolve_type(base_type_name) == sum_type {
		return none
	}
	sum_name := b.sum_name_for_type_id(sum_type) or { return none }
	variant := b.find_sum_variant(sum_name, base_type_name) or { return none }
	variant_type := b.resolve_type(variant)
	if variant_type <= 0 || variant_type >= b.m.type_store.types.len {
		return none
	}
	variant_layout := b.m.type_store.types[variant_type]
	if variant_layout.kind != .struct_t {
		return none
	}
	mut has_field := false
	for fname in variant_layout.field_names {
		if fname == field_name {
			has_field = true
			break
		}
	}
	if !has_field {
		return none
	}
	payload_ptr := b.get_field_ptr(sum_addr, sum_variant_field_name(variant))
	payload_type := b.deref_type(payload_ptr)
	mut variant_addr := payload_ptr
	if payload_type > 0 && payload_type < b.m.type_store.types.len {
		payload_layout := b.m.type_store.types[payload_type]
		if payload_layout.kind == .ptr_t {
			variant_addr = b.emit1(.load, payload_type, payload_ptr)
		}
	}
	return b.get_field_ptr(variant_addr, field_name)
}

fn (b &Builder) sum_name_for_type_id(typ_id TypeID) ?string {
	if typ_id <= 0 {
		return none
	}
	for sum_name, _ in b.sum_type_variants {
		if canonical := b.sum_type_canonical[sum_name] {
			if sum_typ := b.struct_types[canonical] {
				if sum_typ == typ_id {
					return sum_name
				}
			}
		}
	}
	return none
}

fn (b &Builder) selector_qualified_name(node flat.Node) string {
	mut cur := node
	mut parts := []string{}
	for _ in 0 .. 128 {
		if cur.kind == .ident {
			if cur.value.len > 0 {
				parts << cur.value
			}
			break
		}
		if cur.kind != .selector {
			break
		}
		if cur.value.len > 0 {
			parts << cur.value
		}
		if cur.children_count == 0 {
			break
		}
		next_id := b.a.child(&cur, 0)
		if int(next_id) < 0 || int(next_id) >= b.a.nodes.len {
			break
		}
		cur = b.a.nodes[int(next_id)]
	}
	if parts.len == 0 {
		return ''
	}
	mut out := ''
	mut i := parts.len - 1
	for i >= 0 {
		if out.len > 0 {
			out += '.'
		}
		out += parts[i]
		if i == 0 {
			break
		}
		i--
	}
	return out
}

fn (mut b Builder) load_map_len(map_ptr ValueID) ValueID {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	result_slot := b.emit0(.alloca, ptr_i64)
	state := b.map_state_ptr(b.cur_block, map_ptr)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.emit2(.ne, b.i1_type, state, zero_state)
	blk_load := b.m.add_block(b.cur_func, 'map_len_state')
	blk_empty := b.m.add_block(b.cur_func, 'map_len_empty')
	blk_done := b.m.add_block(b.cur_func, 'map_len_done')
	b.emit3(.br, b.void_type, has_state, ValueID(blk_load), ValueID(blk_empty))

	b.cur_block = blk_load
	len_ptr := b.map_state_field_ptr(blk_load, state, 3)
	len := b.emit1(.load, b.i64_type, len_ptr)
	b.emit2(.store, b.void_type, len, result_slot)
	b.emit1(.jmp, b.void_type, ValueID(blk_done))

	b.cur_block = blk_empty
	zero_len := b.m.get_or_add_const(b.i64_type, '0')
	b.emit2(.store, b.void_type, zero_len, result_slot)
	b.emit1(.jmp, b.void_type, ValueID(blk_done))

	b.cur_block = blk_done
	return b.emit1(.load, b.i64_type, result_slot)
}

fn (mut b Builder) load_selector_field(node flat.Node, struct_typ_id TypeID, field_ptr ValueID) ValueID {
	field := b.emit1(.load, b.deref_type(field_ptr), field_ptr)
	if node.typ.len == 0 || !b.sum_type_has_field(struct_typ_id, node.value) {
		return field
	}
	field_typ := b.value_type(field)
	if field_typ <= 0 || field_typ >= b.m.type_store.types.len {
		return field
	}
	field_type := b.m.type_store.types[field_typ]
	if field_type.kind != .ptr_t {
		return field
	}
	expected := b.resolve_type(node.typ)
	if expected > 0 && expected == field_type.elem_type {
		return b.emit1(.load, expected, field)
	}
	return field
}

fn (b &Builder) is_option_like_var(name string) bool {
	typ := b.var_type_names[name] or { return false }
	return typ.len > 0 && (typ[0] == `?` || typ[0] == `!` || typ == 'Optional')
}

fn (b &Builder) selector_has_addressable_root(node &flat.Node) bool {
	match node.kind {
		.ident {
			if _ := b.vars[node.value] {
				return true
			}
		}
		.selector {
			if node.children_count > 0 {
				return b.selector_has_addressable_root(b.a.child_node(node, 0))
			}
		}
		else {}
	}

	return false
}

fn (mut b Builder) build_index(id flat.NodeId, node flat.Node) ValueID {
	base_id := b.a.child(&node, 0)
	if node.children_count >= 2 {
		if base := b.build_const_i32_array_arg(base_id) {
			index := b.build_expr(b.a.child(&node, 1))
			elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.i32_type)}')
			offset := b.emit2(.mul, b.i64_type, index, elem_size)
			elem_ptr := b.emit2(.add, b.m.type_store.get_ptr(b.i32_type), base, offset)
			return b.emit1(.load, b.i32_type, elem_ptr)
		}
	}
	base := b.build_expr(base_id)
	base_typ := b.value_type(base)
	if node.value == 'range' {
		mut start := b.m.get_or_add_const(b.i64_type, '0')
		if node.children_count > 1 {
			start_node := b.a.child_node(&node, 1)
			if start_node.kind == .empty {
				start = b.m.get_or_add_const(b.i64_type, '0')
			} else {
				start = b.build_expr(b.a.child(&node, 1))
			}
		}
		mut end := b.m.get_or_add_const(b.i64_type, '0')
		if node.children_count > 2 {
			end_node := b.a.child_node(&node, 2)
			if end_node.kind == .empty {
				if base_typ == b.str_type {
					len32 := b.load_struct_field_from_value(base, b.str_type, 'len')
					end = b.emit1(.zext, b.i64_type, len32)
				} else {
					end = b.load_struct_field_from_value(base, b.array_type, 'len')
				}
			} else {
				end = b.build_expr(b.a.child(&node, 2))
			}
		} else {
			if base_typ == b.str_type {
				len32 := b.load_struct_field_from_value(base, b.str_type, 'len')
				end = b.emit1(.zext, b.i64_type, len32)
			} else {
				end = b.load_struct_field_from_value(base, b.array_type, 'len')
			}
		}
		if base_typ == b.str_type {
			for substr_name in ['string.substr', 'string__substr', 'substr'] {
				if substr_id := b.fn_ids[substr_name] {
					substr_ref := b.m.add_value(.func_ref, b.str_type, substr_name, substr_id)
					return b.emit4(.call, b.str_type, substr_ref, base, start, end)
				}
			}
			data := b.load_struct_field_from_value(base, b.str_type, 'str')
			slice_data := b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), data, start)
			slice_len := b.emit2(.sub, b.i64_type, end, start)
			return b.emit_make_owned_string(b.cur_block, slice_data, slice_len)
		}
		fn_ref := b.m.add_value(.func_ref, b.array_type, 'array_slice', b.fn_ids['array_slice'])
		return b.emit4(.call, b.array_type, fn_ref, base, start, end)
	}
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	index := b.build_expr(b.a.child(&node, 1))
	if base_typ == b.str_type {
		data := b.load_struct_field_from_value(base, b.str_type, 'str')
		elem_ptr := b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), data, index)
		return b.emit1(.load, b.i8_type, elem_ptr)
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	map_type_name := b.expr_type_name_for_map(base_id)
	if map_type_name.starts_with('map[') {
		key_type_name, val_type_name := map_type_parts(map_type_name)
		if key_type_name.len > 0 && val_type_name.len > 0 {
			key_type := b.resolve_type(key_type_name)
			val_type := b.resolve_type(val_type_name)
			key_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(key_type))
			b.emit2(.store, b.void_type, index, key_alloca)
			key_ptr := if b.value_type(key_alloca) == ptr_i8 {
				key_alloca
			} else {
				b.emit1(.bitcast, ptr_i8, key_alloca)
			}
			default_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(val_type))
			b.emit2(.store, b.void_type, b.default_value_for_type(val_type), default_alloca)
			default_ptr := if b.value_type(default_alloca) == ptr_i8 {
				default_alloca
			} else {
				b.emit1(.bitcast, ptr_i8, default_alloca)
			}
			map_ptr := b.map_expr_ptr(base_id)
			get_ref := b.m.add_value(.func_ref, ptr_i8, 'map__get', b.fn_ids['map__get'])
			value_ptr := b.emit4(.call, ptr_i8, get_ref, map_ptr, key_ptr, default_ptr)
			typed_value_ptr := if b.m.type_store.get_ptr(val_type) == ptr_i8 {
				value_ptr
			} else {
				b.emit1(.bitcast, b.m.type_store.get_ptr(val_type), value_ptr)
			}
			return b.emit1(.load, val_type, typed_value_ptr)
		}
	}
	if base_typ > 0 && base_typ < b.m.type_store.types.len {
		base_type := b.m.type_store.types[base_typ]
		if base_type.kind == .ptr_t {
			if base_type.elem_type == b.array_type {
				elem_type := b.index_elem_type(id, node)
				elem_ptr := b.build_array_data_index_addr(base, index, elem_type)
				return b.emit1(.load, elem_type, elem_ptr)
			}
			elem_type := if base_type.elem_type > 0 { base_type.elem_type } else { b.i8_type }
			elem_size := b.m.type_size(elem_type)
			offset := if elem_size > 1 {
				elem_size_const := b.m.get_or_add_const(b.i64_type, '${elem_size}')
				b.emit2(.mul, b.i64_type, index, elem_size_const)
			} else {
				index
			}
			elem_ptr := b.emit2(.add, base_typ, base, offset)
			return b.emit1(.load, elem_type, elem_ptr)
		}
	}
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.fn_ids['array_get'])
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	elem_type := b.index_elem_type(id, node)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
	return b.emit1(.load, elem_type, typed_ptr)
}

fn (mut b Builder) build_index_addr(id flat.NodeId, node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	// `a[lo..hi]` (an index node tagged `range`) is a slice expression producing a fresh
	// array value, not an addressable element. Its address must point at the materialized
	// slice value; computing an element pointer would make the slice's fields read as
	// garbage (len 0). Affects `&a[lo..hi]`, `array__clone(&a[lo..hi])`, `&[]T` args, etc.
	if node.value == 'range' {
		slice_val := b.build_expr(id)
		slice_type := b.value_type(slice_val)
		tmp := b.emit0(.alloca, b.m.type_store.get_ptr(slice_type))
		b.emit2(.store, b.void_type, slice_val, tmp)
		return tmp
	}
	base_id := b.a.child(&node, 0)
	if base := b.build_const_i32_array_arg(base_id) {
		index := b.build_expr(b.a.child(&node, 1))
		elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.i32_type)}')
		offset := b.emit2(.mul, b.i64_type, index, elem_size)
		return b.emit2(.add, b.m.type_store.get_ptr(b.i32_type), base, offset)
	}
	base := b.build_expr(base_id)
	index := b.build_expr(b.a.child(&node, 1))
	base_typ := b.value_type(base)
	if base_typ == b.str_type {
		data := b.load_struct_field_from_value(base, b.str_type, 'str')
		return b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), data, index)
	}
	if base_typ > 0 && base_typ < b.m.type_store.types.len {
		base_type := b.m.type_store.types[base_typ]
		if base_type.kind == .ptr_t {
			if base_type.elem_type == b.array_type {
				elem_type := b.index_elem_type(id, node)
				return b.build_array_data_index_addr(base, index, elem_type)
			}
			elem_type := if base_type.elem_type > 0 { base_type.elem_type } else { b.i8_type }
			elem_size := b.m.type_size(elem_type)
			offset := if elem_size > 1 {
				elem_size_const := b.m.get_or_add_const(b.i64_type, '${elem_size}')
				b.emit2(.mul, b.i64_type, index, elem_size_const)
			} else {
				index
			}
			return b.emit2(.add, base_typ, base, offset)
		}
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.fn_ids['array_get'])
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	elem_type := b.index_elem_type(id, node)
	return b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
}

fn (mut b Builder) build_array_data_index_addr(array_ptr ValueID, index ValueID, elem_type TypeID) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	data_ptr := b.get_field_ptr(array_ptr, 'data')
	data := b.emit1(.load, ptr_i8, data_ptr)
	elem_size := b.m.type_size(elem_type)
	offset := if elem_size > 1 {
		elem_size_const := b.m.get_or_add_const(b.i64_type, '${elem_size}')
		b.emit2(.mul, b.i64_type, index, elem_size_const)
	} else {
		index
	}
	elem_ptr := b.emit2(.add, ptr_i8, data, offset)
	ptr_elem := b.m.type_store.get_ptr(elem_type)
	if ptr_elem == ptr_i8 {
		return elem_ptr
	}
	return b.emit1(.bitcast, ptr_elem, elem_ptr)
}

fn (mut b Builder) load_struct_field_from_value(value ValueID, typ TypeID, field_name string) ValueID {
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(typ))
	b.emit2(.store, b.void_type, value, alloca)
	field_ptr := b.get_field_ptr(alloca, field_name)
	field := b.emit1(.load, b.deref_type(field_ptr), field_ptr)
	if typ == b.array_type && field_name in ['offset', 'len', 'cap', 'flags', 'element_size'] {
		return b.emit1(.zext, b.i64_type, field)
	}
	if typ == b.str_type && field_name == 'len' {
		return b.emit1(.zext, b.i64_type, field)
	}
	return field
}

fn (mut b Builder) index_elem_type(id flat.NodeId, node flat.Node) TypeID {
	if node.children_count > 0 {
		base_type := b.checked_expr_type_name(b.a.child(&node, 0))
		elem_type := indexed_elem_type_name(base_type)
		if elem_type.len > 0 {
			return b.resolve_type(elem_type)
		}
	}
	if node.typ != '' && !node.typ.starts_with('[]') {
		return b.resolve_type(node.typ)
	}
	checked := b.checked_expr_type_name(id)
	if checked.len > 0 && checked != 'unknown' && !checked.starts_with('[]') && checked[0] != `?`
		&& checked[0] != `!` {
		return b.resolve_type(checked)
	}
	return b.i64_type
}

fn (mut b Builder) build_struct_init(node flat.Node) ValueID {
	typ_id, struct_name := b.struct_literal_type(node.value)
	if typ_id > 0 {
		alloca := b.emit0(.alloca, b.m.type_store.get_ptr(typ_id))
		typ := b.m.type_store.types[typ_id]
		mut initialized := map[string]bool{}
		for i in 0 .. node.children_count {
			field_node := b.a.child_node(&node, i)
			field_expr_id := b.a.child(field_node, 0)
			for fi, fname in typ.field_names {
				if fname == field_node.value {
					offset := b.m.struct_field_offset(typ_id, fi)
					off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
					field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
					field_type_name := b.field_type_name(struct_name, fname)
					field_val := b.coerce_store_value(b.build_field_value(field_expr_id,
						field_type_name), field_type)
					field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type),
						alloca, off_const)
					b.emit2(.store, b.void_type, field_val, field_ptr)
					initialized[fname] = true
					break
				}
			}
		}
		zero := b.m.get_or_add_const(b.i64_type, '0')
		for fi, fname in typ.field_names {
			if fname !in initialized {
				offset := b.m.struct_field_offset(typ_id, fi)
				off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
				field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
				field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type), alloca,
					off_const)
				default_value := b.default_field_value(struct_name, fname, zero)
				b.emit2(.store, b.void_type, default_value, field_ptr)
			}
		}
		return b.emit1(.load, typ_id, alloca)
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (mut b Builder) coerce_store_value(value ValueID, target_type TypeID) ValueID {
	mut result := value
	value_type := b.value_type(value)
	if b.is_pointer_type(target_type) && !b.is_pointer_type(value_type) {
		result = b.heap_copy_value(value, value_type)
	} else if target_type != value_type && b.is_pointer_type(target_type)
		&& b.is_pointer_type(value_type) {
		result = b.emit1(.bitcast, target_type, value)
	}
	return result
}

fn (mut b Builder) build_field_value(expr_id flat.NodeId, field_type_name string) ValueID {
	if int(expr_id) >= 0 {
		expr := b.a.nodes[int(expr_id)]
		if expr.kind == .enum_val && field_type_name.len > 0 {
			return b.build_enum_val_with_type(expr, field_type_name) or { b.build_expr(expr_id) }
		}
		if expr.kind == .infix && field_type_name.len > 0 && b.is_enum_type_name(field_type_name) {
			return b.build_enum_expr_with_type(expr, field_type_name) or { b.build_expr(expr_id) }
		}
	}
	return b.build_expr(expr_id)
}

fn (mut b Builder) build_enum_expr_with_type(node flat.Node, type_name string) ?ValueID {
	if node.kind == .enum_val {
		return b.build_enum_val_with_type(node, type_name)
	}
	if node.kind != .infix || node.children_count < 2 {
		return none
	}
	lhs := b.a.nodes[int(b.a.child(&node, 0))]
	rhs := b.a.nodes[int(b.a.child(&node, 1))]
	lval := b.enum_const_value_with_type(lhs, type_name) or { return none }
	rval := b.enum_const_value_with_type(rhs, type_name) or { return none }
	mut result := 0
	match node.op {
		.pipe { result = lval | rval }
		.amp { result = lval & rval }
		.xor { result = lval ^ rval }
		else { return none }
	}

	return b.m.get_or_add_const(b.i64_type, result.str())
}

fn (b &Builder) enum_const_value_with_type(node flat.Node, type_name string) ?int {
	if node.kind == .enum_val {
		return b.enum_value_for_type(type_name, node.value)
	}
	if node.kind == .infix && node.children_count >= 2 {
		lhs := b.a.nodes[int(b.a.child(&node, 0))]
		rhs := b.a.nodes[int(b.a.child(&node, 1))]
		lval := b.enum_const_value_with_type(lhs, type_name) or { return none }
		rval := b.enum_const_value_with_type(rhs, type_name) or { return none }
		match node.op {
			.pipe { return lval | rval }
			.amp { return lval & rval }
			.xor { return lval ^ rval }
			else { return none }
		}
	}
	return none
}

fn (mut b Builder) default_field_value(struct_name string, field_name string, zero ValueID) ValueID {
	if struct_name.ends_with('SystemError') {
		if field_name == 'msg' {
			return b.m.add_value(.string_literal, b.str_type, '', 0)
		}
		if field_name == 'code' {
			return b.m.get_or_add_const(b.i32_type, '-1')
		}
	}
	field_type := b.field_type_name(struct_name, field_name)
	if field_type.starts_with('[]') {
		elem_name := field_type[2..]
		elem_type := b.resolve_type(elem_name)
		elem_size := b.m.type_size(elem_type)
		elem_size_const := b.m.get_or_add_const(b.i64_type, '${elem_size}')
		if fn_id := b.fn_ids['array_new'] {
			fn_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', fn_id)
			return b.emit4(.call, b.array_type, fn_ref, elem_size_const, zero, zero)
		}
	}
	if field_type.starts_with('map[') {
		key_type, val_type := map_type_parts(field_type)
		key_size := b.m.type_size(b.resolve_type(key_type))
		val_size := b.m.type_size(b.resolve_type(val_type))
		actual_key_size := if key_size > 0 { key_size } else { 8 }
		actual_val_size := if val_size > 0 { val_size } else { 8 }
		key_size_const := b.m.get_or_add_const(b.i64_type, '${actual_key_size}')
		val_size_const := b.m.get_or_add_const(b.i64_type, '${actual_val_size}')
		if fn_id := b.fn_ids['new_map'] {
			fn_ref := b.m.add_value(.func_ref, b.void_type, 'new_map', fn_id)
			mut args := []ValueID{}
			args << fn_ref
			args << key_size_const
			args << val_size_const
			args << zero
			args << zero
			args << zero
			args << zero
			return b.m.add_instr(.call, b.cur_block, b.map_type, args)
		}
	}
	if field_type.len > 0 {
		return b.default_value_for_type(b.resolve_type(field_type))
	}
	return zero
}

fn (b &Builder) field_type_name(struct_name string, field_name string) string {
	key := struct_name + '.' + field_name
	if typ := b.struct_field_types[key] {
		return typ
	}
	short_name := struct_name.all_after('.')
	short_key := short_name + '.' + field_name
	if typ := b.struct_field_types[short_key] {
		return typ
	}
	return ''
}

fn (b &Builder) struct_literal_type(name string) (TypeID, string) {
	short_name := name.all_after('.')
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified_name := b.cur_module + '.' + short_name
		if typ := b.struct_types[qualified_name] {
			return typ, qualified_name
		}
	}
	if typ := b.struct_types[name] {
		return typ, name
	}
	if typ := b.struct_types[short_name] {
		return typ, short_name
	}
	return TypeID(0), name
}

fn (mut b Builder) build_heap_struct_init(node flat.Node) ValueID {
	typ_id, struct_name := b.struct_literal_type(node.value)
	if typ_id > 0 {
		alloca := b.emit0(.alloca, b.m.type_store.get_ptr(typ_id))
		typ := b.m.type_store.types[typ_id]
		mut initialized := map[string]bool{}
		for i in 0 .. node.children_count {
			field_node := b.a.child_node(&node, i)
			field_expr_id := b.a.child(field_node, 0)
			for fi, fname in typ.field_names {
				if fname == field_node.value {
					offset := b.m.struct_field_offset(typ_id, fi)
					off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
					field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
					field_type_name := b.field_type_name(struct_name, fname)
					field_val := b.coerce_store_value(b.build_field_value(field_expr_id,
						field_type_name), field_type)
					field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type),
						alloca, off_const)
					b.emit2(.store, b.void_type, field_val, field_ptr)
					initialized[fname] = true
					break
				}
			}
		}
		zero := b.m.get_or_add_const(b.i64_type, '0')
		for fi, fname in typ.field_names {
			if fname !in initialized {
				offset := b.m.struct_field_offset(typ_id, fi)
				off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
				field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
				field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type), alloca,
					off_const)
				default_value := b.default_field_value(struct_name, fname, zero)
				b.emit2(.store, b.void_type, default_value, field_ptr)
			}
		}
		size := b.m.type_size(typ_id)
		size_const := b.m.get_or_add_const(b.i64_type, '${size}')
		ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
		src_cast := b.emit1(.bitcast, ptr_i8, alloca)
		memdup_ref := b.m.add_value(.func_ref, ptr_i8, 'memdup', b.fn_ids['memdup'])
		result := b.emit3(.call, ptr_i8, memdup_ref, src_cast, size_const)
		return b.emit1(.bitcast, b.m.type_store.get_ptr(typ_id), result)
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (mut b Builder) build_string_interp(node flat.Node) ValueID {
	n := node.children_count
	if n == 0 {
		return b.m.add_value(.string_literal, b.str_type, '', 0)
	}
	mut parts := []ValueID{}
	for i in 0 .. n {
		child_id := b.a.child(&node, i)
		child := b.a.nodes[int(child_id)]
		if child.kind == .string_literal {
			parts << b.m.add_value(.string_literal, b.str_type, child.value, 0)
		} else {
			vtype := b.infer_v_type(child_id)
			if vtype == 'string' {
				parts << b.build_expr(child_id)
			} else {
				val := b.build_expr(child_id)
				int_str_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.fn_ids['int_str'])
				parts << b.emit2(.call, b.str_type, int_str_ref, val)
			}
		}
	}
	count_const := b.m.get_or_add_const(b.i64_type, '${parts.len}')
	alloca := b.emit1(.alloca, b.m.type_store.get_ptr(b.str_type), count_const)
	for i, part in parts {
		off_const := b.m.get_or_add_const(b.i64_type, '${i * 16}')
		ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(b.str_type), alloca, off_const)
		b.emit2(.store, b.void_type, part, ptr)
	}
	fn_ref := b.m.add_value(.func_ref, b.str_type, 'string_plus_many', b.fn_ids['string_plus_many'])
	return b.emit3(.call, b.str_type, fn_ref, count_const, alloca)
}

fn (mut b Builder) get_field_ptr(base_addr ValueID, field_name string) ValueID {
	mut struct_typ_id := TypeID(0)
	base_val := b.m.values[base_addr]
	base_type_id := base_val.typ
	if base_type_id > 0 && base_type_id < b.m.type_store.types.len {
		base_type := b.m.type_store.types[base_type_id]
		if base_type.kind == .ptr_t {
			elem_type := b.m.type_store.types[base_type.elem_type]
			if elem_type.kind == .ptr_t {
				struct_typ_id = elem_type.elem_type
			} else if elem_type.kind == .struct_t {
				struct_typ_id = base_type.elem_type
			}
		}
	}

	if struct_typ_id > 0 {
		typ := b.m.type_store.types[struct_typ_id]
		for fi in 0 .. 512 {
			if fi >= typ.field_names.len || fi >= typ.fields.len {
				break
			}
			fname := typ.field_names[fi]
			if fname == field_name {
				offset := b.m.struct_field_offset(struct_typ_id, fi)
				off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
				field_type := typ.fields[fi]
				ptr_type := b.m.type_store.get_ptr(field_type)

				if b.m.type_store.types[b.m.values[base_addr].typ].kind == .ptr_t {
					inner := b.m.type_store.types[b.m.values[base_addr].typ]
					if inner.kind == .ptr_t && b.m.type_store.types[inner.elem_type].kind == .ptr_t {
						loaded := b.emit1(.load, inner.elem_type, base_addr)
						return b.emit2(.get_element_ptr, ptr_type, loaded, off_const)
					}
				}
				return b.emit2(.get_element_ptr, ptr_type, base_addr, off_const)
			}
		}
	}

	off_const := b.m.get_or_add_const(b.i64_type, '0')
	ptr_type := b.m.type_store.get_ptr(b.i64_type)
	return b.emit2(.get_element_ptr, ptr_type, base_addr, off_const)
}

fn (mut b Builder) resolve_type(name string) TypeID {
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return b.option_type_id(name[1..])
	}
	if name.starts_with('&') {
		inner := b.resolve_type(name[1..])
		return b.m.type_store.get_ptr(inner)
	}
	clean_name := normalize_primitive_type_name(name)
	if clean_name != name {
		return b.resolve_type(clean_name)
	}
	if typ := b.primitive_type_id(name) {
		return typ
	}
	if alias := b.type_alias_target(name) {
		if alias != name {
			return b.resolve_type(alias)
		}
	}
	// SSA-level alias fallback (used when building without a checker).
	if alias_typ := b.type_aliases[name] {
		if alias_typ > 0 {
			return alias_typ
		}
	}
	if b.is_fixed_array_type_name(name) {
		elem_type := b.resolve_type(b.fixed_array_elem_type_name(name))
		return b.m.type_store.get_ptr(elem_type)
	}
	if name.starts_with('[]') || name == 'array' || name == 'Array' {
		return b.array_type
	}
	if name == 'strings.Builder' {
		return b.array_type
	}
	if name.starts_with('map[') || name == 'map' || name == 'Map' {
		return b.map_type
	}
	if b.is_enum_type_name(name) {
		return b.i32_type
	}
	if canonical := b.canonical_sum_type_name(name) {
		if typ := b.struct_types[canonical] {
			return typ
		}
	}
	short_name := name.all_after('.')
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified_name := b.cur_module + '.' + short_name
		if qualified_name in b.enum_types {
			return b.i32_type
		}
		if typ := b.struct_types[qualified_name] {
			return typ
		}
	}
	if typ := b.struct_types[name] {
		return typ
	}
	if typ := b.struct_types[short_name] {
		return typ
	}
	if name == 'Builder' {
		return b.array_type
	}
	return b.i64_type
}

fn (mut b Builder) primitive_type_id(name string) ?TypeID {
	return match name {
		'int' {
			b.i32_type
		}
		'i8' {
			b.i8_type
		}
		'i16' {
			b.i32_type
		}
		'i32' {
			b.i32_type
		}
		'i64' {
			b.i64_type
		}
		'u8', 'byte' {
			b.u8_type
		}
		'u16' {
			b.u16_type
		}
		'u32' {
			b.u32_type
		}
		'u64' {
			b.u64_type
		}
		'f32' {
			b.f32_type
		}
		'f64' {
			b.f64_type
		}
		'bool' {
			b.i1_type
		}
		'string' {
			b.str_type
		}
		'void', '' {
			b.void_type
		}
		'voidptr' {
			b.m.type_store.get_ptr(b.i8_type)
		}
		else {
			none
		}
	}
}

fn normalize_primitive_type_name(name string) string {
	short_name := name.all_after('.')
	return match short_name {
		'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool',
		'string', 'void', 'voidptr', '' {
			short_name
		}
		else {
			name
		}
	}
}

fn (mut b Builder) option_type_id(base_name string) TypeID {
	key := '?' + base_name
	if typ := b.option_types[key] {
		return typ
	}
	base_typ := b.resolve_type(base_name)
	mut fields := []TypeID{}
	mut field_names := []string{}
	fields << b.i1_type
	field_names << 'ok'
	if base_typ != b.void_type {
		fields << base_typ
		field_names << 'value'
	}
	typ_id := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      fields
		field_names: field_names
	})
	b.option_types[key] = typ_id
	b.struct_types[key] = typ_id
	b.struct_types['!' + base_name] = typ_id
	return typ_id
}

fn (b &Builder) option_value_type(typ_id TypeID) TypeID {
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return TypeID(0)
	}
	typ := b.m.type_store.types[typ_id]
	if typ.kind != .struct_t || typ.field_names.len == 0 || typ.field_names[0] != 'ok' {
		return TypeID(0)
	}
	if typ.field_names.len > 1 && typ.field_names[1] == 'value' && typ.fields.len > 1 {
		return typ.fields[1]
	}
	return b.void_type
}

fn (b &Builder) option_payload_type_name(id flat.NodeId) string {
	name := b.checked_expr_type_name(id)
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return name[1..]
	}
	return name
}

fn (b &Builder) is_option_type(typ_id TypeID) bool {
	return b.option_value_type(typ_id) != TypeID(0)
}

fn (b &Builder) is_enum_type_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if name in b.enum_types {
		return true
	}
	short_name := name.all_after('.')
	return short_name in b.enum_types
}

fn (b &Builder) enum_autostr_fn_name(type_name string) ?string {
	if type_name.len == 0 {
		return none
	}
	clean := type_name.trim_left('&')
	mut candidates := []string{}
	candidates << clean
	if !clean.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		candidates << '${b.cur_module}.${clean}'
	}
	candidates << clean.all_after_last('.')
	for candidate in candidates {
		if !b.is_enum_type_name(candidate) {
			continue
		}
		fn_name := '${ssa_c_name(candidate)}__autostr'
		if fn_name in b.fn_ids {
			return fn_name
		}
	}
	return none
}

fn (mut b Builder) resolve_type_in_module(name string, module_name string) TypeID {
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return b.option_type_id(name[1..])
	}
	if name.starts_with('&') {
		inner := b.resolve_type_in_module(name[1..], module_name)
		return b.m.type_store.get_ptr(inner)
	}
	if !name.contains('.') && module_name.len > 0 && module_name != 'main'
		&& module_name != 'builtin' {
		qualified_name := module_name + '.' + name
		if qualified_name in b.enum_types {
			return b.i32_type
		}
		if typ := b.struct_types[qualified_name] {
			return typ
		}
		if alias := b.type_alias_target(qualified_name) {
			if alias != qualified_name {
				return b.resolve_type_in_module(alias, module_name)
			}
		}
	}
	return b.resolve_type(name)
}

fn (mut b Builder) resolve_c_decl_type(name string) TypeID {
	if name.starts_with('...') {
		return b.m.type_store.get_ptr(b.i8_type)
	}
	if name.starts_with('C.') {
		return b.resolve_type(name[2..])
	}
	return b.resolve_type(name)
}

fn (b &Builder) type_alias_target(name string) ?string {
	if b.tc == unsafe { nil } || name.len == 0 {
		return none
	}
	if target := b.tc.type_aliases[name] {
		return target
	}
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified_name := b.cur_module + '.' + name
		if target := b.tc.type_aliases[qualified_name] {
			return target
		}
	}
	return none
}

fn (b &Builder) value_type(val_id ValueID) TypeID {
	if val_id <= 0 || val_id >= b.m.values.len {
		return b.i64_type
	}
	v := b.m.values[val_id]
	if v.typ > 0 {
		return v.typ
	}
	return b.i64_type
}

fn (b &Builder) is_float_type(typ TypeID) bool {
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return false
	}
	return b.m.type_store.types[typ].kind == .float_t
}

fn (b &Builder) is_struct_type(typ TypeID) bool {
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return false
	}
	return b.m.type_store.types[typ].kind == .struct_t
}

fn (b &Builder) is_int_type(typ TypeID) bool {
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return false
	}
	return b.m.type_store.types[typ].kind == .int_t
}

fn (b &Builder) is_unsigned_type(typ TypeID) bool {
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return false
	}
	t := b.m.type_store.types[typ]
	return t.kind == .int_t && t.is_unsigned
}

fn (b &Builder) is_pointer_type(typ TypeID) bool {
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return false
	}
	return b.m.type_store.types[typ].kind == .ptr_t
}

fn (b &Builder) scalar_type_width(typ TypeID) int {
	if typ <= 0 || typ >= b.m.type_store.types.len {
		return 0
	}
	return b.m.type_store.types[typ].width
}

fn (b &Builder) deref_type(ptr_val ValueID) TypeID {
	if ptr_val <= 0 || ptr_val >= b.m.values.len {
		return b.i64_type
	}
	v := b.m.values[ptr_val]
	if v.typ > 0 && v.typ < b.m.type_store.types.len {
		t := b.m.type_store.types[v.typ]
		if t.kind == .ptr_t {
			return t.elem_type
		}
	}
	if v.kind == .instruction {
		instr := b.m.instrs[v.index]
		if instr.op == .alloca || instr.op == .get_element_ptr || instr.op == .bitcast {
			if v.typ > 0 && v.typ < b.m.type_store.types.len {
				t := b.m.type_store.types[v.typ]
				if t.kind == .ptr_t {
					return t.elem_type
				}
			}
		}
	}
	if v.kind == .global {
		if v.typ > 0 && v.typ < b.m.type_store.types.len {
			t := b.m.type_store.types[v.typ]
			if t.kind == .ptr_t {
				return t.elem_type
			}
		}
	}
	return b.i64_type
}

fn (b &Builder) infer_v_type(id flat.NodeId) string {
	if int(id) < 0 {
		return 'int'
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.string_literal, .string_interp {
			return 'string'
		}
		.int_literal {
			return 'int'
		}
		.bool_literal {
			return 'bool'
		}
		.ident {
			if addr := b.vars[node.value] {
				val := b.m.values[addr]
				if val.typ == b.str_type || (val.typ > 0 && val.typ < b.m.type_store.types.len
					&& b.m.type_store.types[val.typ].kind == .ptr_t
					&& b.m.type_store.types[val.typ].elem_type == b.str_type) {
					return 'string'
				}
			}
			return 'int'
		}
		.call {
			fn_node := b.a.child_node(&node, 0)
			if fn_node.value == 'int_str' {
				return 'string'
			}
			if fn_idx := b.fn_ids[fn_node.value] {
				ret := b.m.funcs[fn_idx].typ
				if ret == b.str_type {
					return 'string'
				}
			}
			return 'int'
		}
		.infix {
			lt := b.infer_v_type(b.a.child(&node, 0))
			if lt == 'string' {
				return 'string'
			}
			return lt
		}
		else {
			return 'int'
		}
	}
}

fn (mut b Builder) emit0(op OpCode, typ TypeID) ValueID {
	return b.m.add_instr(op, b.cur_block, typ, []ValueID{})
}

fn (mut b Builder) emit1(op OpCode, typ TypeID, a ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	return b.m.add_instr(op, b.cur_block, typ, ops)
}

fn (mut b Builder) emit2(op OpCode, typ TypeID, a ValueID, c ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	ops << c
	return b.m.add_instr(op, b.cur_block, typ, ops)
}

fn (mut b Builder) emit3(op OpCode, typ TypeID, a ValueID, c ValueID, d ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	ops << c
	ops << d
	return b.m.add_instr(op, b.cur_block, typ, ops)
}

fn (mut b Builder) emit4(op OpCode, typ TypeID, a ValueID, c ValueID, d ValueID, e ValueID) ValueID {
	mut ops := []ValueID{}
	ops << a
	ops << c
	ops << d
	ops << e
	return b.m.add_instr(op, b.cur_block, typ, ops)
}

fn (b &Builder) compound_to_op(op flat.Op) OpCode {
	return match op {
		.plus_assign { .add }
		.minus_assign { .sub }
		.mul_assign { .mul }
		.div_assign { .sdiv }
		.mod_assign { .srem }
		.amp_assign { .and_ }
		.pipe_assign { .or_ }
		.xor_assign { .xor }
		.left_shift_assign { .shl }
		.right_shift_assign { .ashr }
		else { .add }
	}
}
