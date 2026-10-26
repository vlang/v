module ssa

import math
import os
import v.flat
import v.gen.c.naming { sum_field_name }
import v.token
import v.types

const arm64_force_external_syms = ['_malloc', '_free', '_calloc', '_realloc', '_exit', '_abort',
	'_memcpy', '_memmove', '_memset', '_memcmp', '___stdoutp', '___stderrp', '_puts', '_printf',
	'_write', '_read', '_open', '_close', '_fwrite', '_fflush', '_fopen', '_fclose', '_putchar',
	'_sprintf', '_snprintf', '_fprintf', '_sscanf', '_mmap', '_munmap', '_getcwd', '_access',
	'_readlink', '_getenv', '_strlen', '_opendir', '_readdir', '_closedir', '_mkdir', '_rmdir',
	'_unlink', '_rename', '_remove', '_stat', '_lstat', '_fstat', '_chmod', '_chdir', '_realpath',
	'_symlink', '_link', '_getpid', '_getuid', '_geteuid', '_fork', '_execve', '_execvp', '_waitpid',
	'_kill', '_system', '_posix_spawn', '_signal', '_atexit', '_fgets', '_fputs', '_fread', '_fseek',
	'_ftell', '_rewind', '_fileno', '_popen', '_pclose', '_dup', '_dup2', '_pipe', '_isatty', '_freopen',
	'_dprintf', '_getc', '_strdup', '_strcmp', '_strncmp', '_strchr', '_strrchr', '_strerror',
	'_strncasecmp', '_strcasecmp', '_atoi', '_atof', '_qsort', '_time', '_localtime_r', '_gmtime_r',
	'_mktime', '_gettimeofday', '_clock', '_clock_gettime_nsec_np', '_mach_absolute_time',
	'_mach_timebase_info', '_nanosleep', '_sleep', '_usleep', '_strftime', '_task_info',
	'_mach_task_self', '_mach_task_self_', '_proc_pid_rusage', '_rand', '_srand', '_isdigit', '_isspace',
	'_tolower', '_toupper', '_setenv', '_unsetenv', '_sysconf', '_uname', '_gethostname',
	'_pthread_mutex_init', '_pthread_mutex_lock', '_pthread_mutex_trylock', '_pthread_mutex_unlock',
	'_pthread_mutex_destroy', '_pthread_self', '_pthread_create', '_pthread_join', '_pthread_detach',
	'_pthread_attr_init', '_pthread_attr_setstacksize', '_pthread_attr_destroy',
	'_pthread_rwlockattr_init', '_pthread_rwlockattr_setpshared', '_pthread_rwlockattr_destroy',
	'_pthread_rwlock_init', '_pthread_rwlock_rdlock', '_pthread_rwlock_wrlock',
	'_pthread_rwlock_tryrdlock', '_pthread_rwlock_trywrlock', '_pthread_rwlock_unlock',
	'_pthread_rwlock_destroy', '_pthread_condattr_init', '_pthread_condattr_setpshared',
	'_pthread_condattr_destroy', '_pthread_cond_init', '_pthread_cond_signal', '_pthread_cond_wait',
	'_pthread_cond_timedwait', '_pthread_cond_destroy', '_arc4random_buf', '_proc_pidpath', '_backtrace',
	'_backtrace_symbols', '_backtrace_symbols_fd', '_dispatch_semaphore_create',
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
	'_jn', '_y0', '_y1', '_yn', '_getrusage', '_mprotect', '_sys_icache_invalidate', '_objc_msgSend',
	'_objc_getClass', '_sel_registerName', '_objc_alloc_init', '_objc_autoreleasePoolPush',
	'_objc_autoreleasePoolPop', '_MTLCreateSystemDefaultDevice', '_dlopen', '_dlsym']

const wasm_bench_runtime_stub_names = ['bench.current_rss_kb', 'bench.macos_rss_kb',
	'bench.linux_rss_kb', 'v.bench.current_rss_kb', 'v.bench.macos_rss_kb', 'v.bench.linux_rss_kb']

const ssa_array_source_type = flat.source_type_id('array')
const ssa_map_source_type = flat.source_type_id('map')
const ssa_thread_source_type = flat.source_type_id('thread')
const ssa_nominal_array_source_type = flat.source_type_id('Array')
const ssa_nominal_map_source_type = flat.source_type_id('Map')
const ssa_i128_source_type = flat.source_type_id('i128')
const ssa_u128_source_type = flat.source_type_id('u128')
const ssa_option_source_type = flat.source_type_id('__v_option')
const ssa_builtin_array_source_type = flat.source_type_id('builtin.array')
const ssa_array_flags_source_type = flat.source_type_id('ArrayFlags')

const bench_runtime_stub_names = ['current_rss_kb', 'macos_rss_kb', 'linux_rss_kb',
	'bench.current_rss_kb', 'bench.macos_rss_kb', 'bench.linux_rss_kb', 'v.bench.current_rss_kb',
	'v.bench.macos_rss_kb', 'v.bench.linux_rss_kb']

// Builder stores state for SSA construction.
pub struct Builder {
mut:
	m                        &Module            = unsafe { nil }
	a                        &flat.FlatAst      = unsafe { nil }
	tc                       &types.TypeChecker = unsafe { nil }
	used_fns                 map[string]bool
	used_fn_normalized       map[string]bool
	used_fn_suffixes         map[string]bool
	cur_module               string
	cur_func                 int
	cur_func_ret_type        u32
	cur_block                BlockID
	vars                     map[string]ValueID
	global_vars              map[string]ValueID
	global_type_names        flat.TypeTextBindings
	global_imports           map[string]string
	global_aliases           map[string]string
	var_type_names           flat.TypeTextBindings
	mut_pointer_params       map[string]TypeID
	mut_param_names          map[string]bool
	sum_guard_variants       map[ValueID]u32
	i64_type                 TypeID
	i32_type                 TypeID
	i8_type                  TypeID
	i1_type                  TypeID
	u64_type                 TypeID
	u32_type                 TypeID
	u16_type                 TypeID
	u8_type                  TypeID
	f32_type                 TypeID
	f64_type                 TypeID
	void_type                TypeID
	str_type                 TypeID
	array_type               TypeID
	map_type                 TypeID
	map_state_type           TypeID
	wide_int_type            TypeID
	fn_types                 map[string]TypeID
	fn_ids                   map[string]int
	const_exprs              map[string]flat.NodeId
	native_initializers      map[string]NativeInitializer
	native_initializer_order []string
	// Declaration and emitted C names stay in their own symbol namespace.
	struct_types             map[string]TypeID
	multi_return_types       map[u32]TypeID
	multi_return_field_types map[u32]map[string]u32
	ssa_type_names           map[TypeID]string
	interface_types          map[string]TypeID
	interface_type_names     map[TypeID]string
	sum_types_registering    map[TypeID]bool
	struct_field_types       flat.TypeTextBindings
	struct_field_defaults    map[string]NativeFieldDefault
	struct_embeds            map[TypeID][]int
	default_struct_types     map[TypeID]bool
	option_types             map[u32]TypeID
	sum_type_variants        flat.TypeTextListBindings
	sum_type_canonical       flat.TypeTextBindings
	enum_types               map[string]bool
	flag_enum_types          map[string]bool
	enum_values              map[string]int
	enum_member_values       map[string]int
	enum_member_dupes        map[string]bool
	c_fn_ids                 map[string]int
	c_fn_types               map[string]TypeID
	label_blocks             map[string]BlockID
	defer_body_ids           []flat.NodeId
	defer_active_slots       map[flat.NodeId]ValueID
	pending_loop_label       string
	loop_targets             []LoopTargets
	top_level_main           bool
	thread_stack_size        int
	source_fn_names          map[string]bool
	runtime_fn_ids           map[string]int
	runtime_fn_types         map[string]TypeID
	break_targets            []BlockID
	continue_targets         []BlockID
	break_labels             map[string]BlockID
	continue_labels          map[string]BlockID
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
	// File-scoped names preserve distinct nested modules that share a leaf name.
	source_modules         map[string]string
	source_imports         map[string]map[string]string
	exact_used_fns         bool
	allow_missing_fn_stubs bool
	use_native_c_sources   bool
}

// VarBinding represents var binding data used by ssa.
struct VarBinding {
	exists bool
	addr   ValueID
	typ    u32
}

struct BuilderScope {
	active     bool
	vars       map[string]ValueID
	type_names flat.TypeTextBindings
}

// IfGuardState stores if guard state state used by ssa.
struct IfGuardState {
	active    bool
	name      string
	old       VarBinding
	slot      ValueID
	value_ptr ValueID
	typ       TypeID
	typ_name  u32
}

// BuildOptions controls which functions a build materializes. The zero value
// reproduces the default whole-program build.
pub struct BuildOptions {
pub:
	hot_fn                 string   // build only this function's body (hot reload)
	skip_fn_bodies         bool     // register signatures only, mark them prototypes
	skip_modules           []string // skip all functions declared in these modules
	track_uses             bool = true            // build optimizer/verifier use lists
	thread_stack_size      int  = 8 * 1024 * 1024 // zero uses the pthread default
	test_files             []string                     // driver-selected test file paths; enables a test entrypoint
	test_run_only          []string                     // bare or qualified function-name globs
	target                 TargetData                   // target memory layout, defaults to 64-bit pointers
	source_modules         map[string]string            // source filename -> full module path
	source_imports         map[string]map[string]string // source filename -> import alias -> full module path
	exact_used_fns         bool                         // materialize only source functions listed in used_fns
	allow_missing_fn_stubs bool                         // native compile-only fallback for checker-validated calls
	use_native_c_sources   bool                         // link V-shipped C implementations instead of SSA replacements
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
	progress := os.getenv('V_SSA_PROGRESS') != ''
	if progress {
		eprintln('ssa: create builder')
	}
	mut b := Builder{
		m:                        Module.new()
		a:                        unsafe { a_ }
		tc:                       unsafe { tc }
		used_fns:                 used_fns
		used_fn_normalized:       map[string]bool{}
		used_fn_suffixes:         map[string]bool{}
		vars:                     map[string]ValueID{}
		global_vars:              map[string]ValueID{}
		global_type_names:        flat.TypeTextBindings{}
		global_imports:           map[string]string{}
		var_type_names:           flat.TypeTextBindings{}
		mut_pointer_params:       map[string]TypeID{}
		mut_param_names:          map[string]bool{}
		fn_types:                 map[string]TypeID{}
		fn_ids:                   map[string]int{}
		const_exprs:              map[string]flat.NodeId{}
		struct_types:             map[string]TypeID{}
		multi_return_types:       map[u32]TypeID{}
		multi_return_field_types: map[u32]map[string]u32{}
		ssa_type_names:           map[TypeID]string{}
		interface_types:          map[string]TypeID{}
		interface_type_names:     map[TypeID]string{}
		sum_types_registering:    map[TypeID]bool{}
		struct_field_types:       flat.TypeTextBindings{}
		option_types:             map[u32]TypeID{}
		sum_type_variants:        flat.TypeTextListBindings{}
		sum_type_canonical:       flat.TypeTextBindings{}
		enum_types:               map[string]bool{}
		flag_enum_types:          map[string]bool{}
		enum_values:              map[string]int{}
		enum_member_values:       map[string]int{}
		enum_member_dupes:        map[string]bool{}
		c_fn_ids:                 map[string]int{}
		c_fn_types:               map[string]TypeID{}
		label_blocks:             map[string]BlockID{}
		defer_body_ids:           []flat.NodeId{}
		loop_targets:             []LoopTargets{}
		skip_modules:             map[string]bool{}
		type_aliases:             map[string]TypeID{}
		source_modules:           opts.source_modules
		source_imports:           opts.source_imports
		exact_used_fns:           opts.exact_used_fns
		allow_missing_fn_stubs:   opts.allow_missing_fn_stubs
		use_native_c_sources:     opts.use_native_c_sources
	}
	b.m.track_uses = opts.track_uses
	b.m.target = opts.target
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
	// Keep `is_lit` named so string ownership checks resolve its target-specific offset.
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
	mut map_state_field_names := []string{}
	if b.m.target.ptr_size == 8 {
		dense_fields := [b.i32_type, b.i32_type, b.i32_type, b.i32_type, b.u32_type, b.u8_type,
			b.u8_type, ptr_i8, ptr_i8, ptr_i8]
		dense_names := ['key_bytes', 'value_bytes', 'cap', 'len', 'deletes', 'key_alignment',
			'value_alignment', 'all_deleted', 'keys', 'values']
		dense_type := b.m.type_store.register(Type{
			kind:        .struct_t
			fields:      dense_fields
			field_names: dense_names
		})
		map_state_fields = [b.i32_type, b.i32_type, b.u32_type, b.u8_type, b.u8_type, dense_type,
			b.m.type_store.get_ptr(b.u32_type), b.u32_type, b.i8_type, ptr_i8, ptr_i8, ptr_i8,
			ptr_i8, b.i32_type, ptr_i8, b.i64_type, b.i64_type]
		map_state_field_names = ['key_bytes', 'value_bytes', 'even_index', 'cached_hashbits', 'shift',
			'key_values', 'metas', 'extra_metas', 'has_string_keys', 'hash_fn', 'eq_fn', 'clone_fn',
			'free_fn', 'count', 'v3_hash_slots', 'v3_hash_cap', 'v3_indexed_len']
	} else {
		map_state_fields = [ptr_i8, ptr_i8, b.i64_type, b.i64_type, b.i64_type, b.i64_type]
		map_state_field_names = ['keys', 'vals', 'cap', 'len', 'key_size', 'val_size']
	}
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
	b.thread_stack_size = opts.thread_stack_size
	b.skip_fn_bodies = opts.skip_fn_bodies
	for mname in opts.skip_modules {
		b.skip_modules[mname] = true
	}
	b.prepare_used_fn_lookups()
	b.m.name = b.main_module_name()
	if progress {
		eprintln('ssa: register declarations')
	}
	b.register_consts()
	b.register_type_aliases()
	b.register_types()
	b.register_struct_field_defaults()
	b.register_globals()
	b.global_vars = b.vars.clone()
	b.register_functions()
	if progress {
		eprintln('ssa: build function bodies (${b.fn_ids.len} registered)')
	}
	if b.m.target.ptr_size == 8 { b.register_native_initializers() }
	b.build_functions()
	if progress {
		eprintln('ssa: build native entrypoints')
	}
	if b.m.target.ptr_size == 8 {
		b.build_native_test_main(opts.test_files, opts.test_run_only)
		b.build_native_initialization()
	}
	if b.m.target.ptr_size == 4 { b.canonicalize_internal_function_refs() }
	return b.m
}

const ssa_strings_builder_source_type = flat.source_type_id('strings.Builder')
const ssa_builder_source_type = flat.source_type_id('Builder')
const ssa_strings_builder_ref_source_type = flat.source_type_id('&strings.Builder')
const ssa_builder_ref_source_type = flat.source_type_id('&Builder')

// canonicalize_internal_function_refs keeps backend symbol lookup consistent with
// the selected SSA function, including runtime helpers renamed around source functions.
fn (mut b Builder) canonicalize_internal_function_refs() {
	for i in 0 .. b.m.values.len {
		value := b.m.values[i]
		if value.kind != .func_ref || value.index < 0 || value.index >= b.m.funcs.len {
			continue
		}
		f := b.m.funcs[value.index]
		if !f.is_c_extern && value.name != f.name {
			mut reference := value
			reference.name = f.name
			b.m.values[i] = reference
		}
	}
}

// register_types updates register types state for ssa.
fn (mut b Builder) register_types() {
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
			continue
		}
		if node.kind == .struct_decl {
			typ_id := b.m.type_store.register(Type{
				kind: .struct_t
			})
			b.register_struct_type_name(node.value_text(), cur_module, typ_id)
		} else if is_sum_type_decl(node) {
			typ_id := b.m.type_store.register(Type{
				kind: .struct_t
			})
			b.register_sum_type_name(node.value_text(), cur_module, typ_id)
		} else if node.kind == .enum_decl {
			b.register_enum_values(node, cur_module)
		}
	}

	cur_module = ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
			continue
		}
		if node.kind == .struct_decl {
			if b.tc != unsafe { nil } && node.value.contains('[') {
				checker_fields := b.tc.struct_fields_for_type(flat.source_type_id(node.value))
				if checker_fields.len > 0 {
					typ_id := b.struct_type_id_for_decl(node.value, cur_module)
					mut concrete_fields := []TypeID{cap: checker_fields.len}
					mut concrete_names := []string{cap: checker_fields.len}
					for field in checker_fields {
						field_type := b.ssa_type_from_checker_type(field.typ)
						concrete_fields << field_type
						concrete_names << field.name
						b.struct_field_types.set_id(node.value + '.' + field.name,
							field.typ.source_type_id())
					}
					b.m.type_store.types[typ_id] = Type{
						kind:        .struct_t
						fields:      concrete_fields
						field_names: concrete_names
					}
					continue
				}
			}
			mut field_types := []TypeID{}
			mut field_names := []string{}
			mut embeds := []int{}
			for i in 0 .. node.children_count {
				f := b.a.child_node(&node, i)
				metadata := f.generic_params()
				if f.typ == 0 || (metadata.len > 0 && metadata[0].contains('e')) {
					embeds << i
				}
				// Embedded fields with defaults carry their type in the field name.
				declared_type := if f.typ == 0 { f.source_value_id() } else { f.typ }
				field_type_name_id := native_c_struct_field_type(node.value_text(), f.value) or {
					qualify_type_ref_name(declared_type, cur_module)
				}
				field_type := b.resolve_struct_storage_type(field_type_name_id, cur_module)
				field_types << field_type
				field_names << f.value
				b.struct_field_types.set_id(node.value_text() + '.' + f.value, field_type_name_id)
				short_name := node.value_text().all_after('.')
				b.struct_field_types.set_id(short_name + '.' + f.value, field_type_name_id)
				if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
					b.struct_field_types.set_id(cur_module + '.' + short_name + '.' + f.value, field_type_name_id)
				}
			}
			if abi := native_c_struct_abi(node.value_text(), b.m.target.ptr_size) {
				field_names = abi.field_names.clone()
				field_types = []TypeID{cap: abi.field_types.ids.len}
				for field_type in abi.field_types.ids {
					field_types << b.resolve_struct_storage_type(field_type, cur_module)
				}
				for i, name in field_names {
					b.struct_field_types.set_id(node.value_text() + '.' + name, abi.field_types.ids[i])
					b.struct_field_types.set_id(node.value_text().all_after('.') + '.' + name, abi.field_types.ids[i])
				}
			}
			if node.value_text() == 'C.tm' && 'tm_zone' !in field_names {
				field_names << 'tm_zone'
				field_types << b.resolve_type(flat.source_type_id('&char'))
				b.struct_field_types.set('C.tm.tm_zone', '&char')
				b.struct_field_types.set('tm.tm_zone', '&char')
			}
			typ_id := b.struct_type_id_for_decl(node.value_text(), cur_module)
			b.struct_embeds[typ_id] = embeds
			b.m.type_store.types[typ_id] = Type{
				kind:        .struct_t
				fields:      field_types
				field_names: field_names
				is_union:    'union' in node.typ_text().split(',')
				is_packed:   'packed' in node.typ_text().split(',')
				alignment:   struct_decl_alignment(node)
			}
		}
	}

	cur_module = ''
	for node in b.a.nodes {
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
			continue
		}
		if !is_sum_type_decl(node) {
			continue
		}
		sum_typ_id := b.struct_type_id_for_decl(node.value_text(), cur_module)
		mut field_types := []TypeID{}
		mut field_names := []string{}
		mut has_pointer_variant := false

		variants := b.sum_type_variants_for_decl(node, cur_module)
		for variant in variants {
			field_name := sum_variant_field_name(flat.source_type_name(variant), cur_module)
			field_type := b.resolve_struct_storage_type(variant, cur_module)
			has_pointer_variant = has_pointer_variant || b.is_pointer_type(field_type)
			field_types << field_type
			field_names << field_name
			b.register_sum_field_type(node.value_text(), cur_module, field_name, variant)
		}
		payload := b.m.type_store.register(Type{
			kind:        .struct_t
			fields:      field_types
			field_names: field_names
			is_union:    true
		})
		mut sum_fields := [b.i32_type]
		mut sum_names := ['typ']
		if has_pointer_variant {
			sum_fields << b.i32_type
			sum_names << '_pointer_variant_is_owned'
			b.register_sum_field_type(node.value_text(), cur_module, '_pointer_variant_is_owned', flat.source_type_u32)
		}
		sum_fields << payload
		sum_names << '_payload'
		b.m.type_store.types[sum_typ_id] = Type{
			kind:        .struct_t
			fields:      sum_fields
			field_names: sum_names
		}
		b.register_sum_field_type(node.value_text(), cur_module, 'typ', flat.source_type_i32)
	}
	mut generic_struct_names := []string{}
	for name, _ in b.struct_types {
		if name.contains('[') {
			generic_struct_names << name
		}
	}
	for name in generic_struct_names {
		_ := b.register_checker_generic_struct(name)
	}
	b.register_multi_return_types()
}

fn struct_decl_alignment(node flat.Node) int {
	for attribute in node.typ_text().split(',') {
		clean := attribute.trim_space()
		if clean == 'aligned' {
			return 16
		}
		if clean.starts_with('aligned=') {
			return clean.all_after('=').int()
		}
	}
	return 0
}

fn (mut b Builder) resolve_struct_storage_type(name_id u32, module_name string) TypeID {
	if b.is_fixed_array_type_name(name_id) {
		elem_name := qualify_type_ref_name(b.fixed_array_elem_type_name(name_id), module_name)
		elem_type := b.resolve_struct_storage_type(elem_name, module_name)
		previous_module := b.cur_module
		b.cur_module = module_name
		length := b.fixed_array_len_text(name_id).int()
		b.cur_module = previous_module
		return b.m.type_store.get_array(elem_type, length)
	}
	return b.resolve_type_in_module(name_id, module_name)
}

struct NativeCStructAbi {
	field_names []string
	field_types flat.TypeTextList
}

fn native_c_struct_abi(struct_name string, pointer_size int) ?NativeCStructAbi {
	if struct_name == 'C.utsname' && pointer_size == 8 {
		$if linux {
			// Native ABI overrides follow the compiler host OS. Linux includes
			// domainname even when the V declaration exposes only five fields.
			return NativeCStructAbi{
				field_names: ['sysname', 'nodename', 'release', 'version', 'machine', 'domainname']
				field_types: flat.TypeTextList.from_names(['[65]char', '[65]char', '[65]char',
					'[65]char', '[65]char', '[65]char'])
			}
		}
		// Darwin stores five inline char[256] fields. The V declaration uses
		// &char so selectors have C array-to-pointer semantics.
		return NativeCStructAbi{
			field_names: ['sysname', 'nodename', 'release', 'version', 'machine']
			field_types: flat.TypeTextList.from_names(['[256]char', '[256]char', '[256]char',
				'[256]char', '[256]char'])
		}
	}
	if struct_name == 'C.pthread_mutex_t' {
		return NativeCStructAbi{
			field_names: ['opaque']
			field_types: flat.TypeTextList.from_names(['[8]u64'])
		}
	}
	if struct_name == 'C.pthread_rwlock_t' {
		return NativeCStructAbi{
			field_names: ['opaque']
			field_types: flat.TypeTextList.from_names(['[25]u64'])
		}
	}
	if struct_name == 'C.pthread_rwlockattr_t' {
		return NativeCStructAbi{
			field_names: ['opaque']
			field_types: flat.TypeTextList.from_names(['[3]u64'])
		}
	}
	if struct_name == 'C.pthread_cond_t' {
		return NativeCStructAbi{
			field_names: ['opaque']
			field_types: flat.TypeTextList.from_names(['[6]u64'])
		}
	}
	if struct_name == 'C.pthread_condattr_t' {
		return NativeCStructAbi{
			field_names: ['opaque']
			field_types: flat.TypeTextList.from_names(['[2]u64'])
		}
	}
	if struct_name == 'C.task_basic_info' {
		// `task_info` writes the complete macOS mach_task_basic_info ABI value even
		// though runtime.used_memory exposes only resident_size in its V declaration.
		return NativeCStructAbi{
			field_names: ['virtual_size', 'resident_size', 'resident_size_max', 'user_time_seconds',
				'user_time_microseconds', 'system_time_seconds', 'system_time_microseconds', 'policy',
				'suspend_count']
			field_types: flat.TypeTextList.from_names(['u64', 'u64', 'u64', 'i32', 'i32', 'i32',
				'i32', 'i32', 'i32'])
		}
	}
	if struct_name == 'C.rusage' {
		// getrusage fills two 16-byte timeval values followed by fourteen longs.
		return NativeCStructAbi{
			field_names: ['ru_utime_sec', 'ru_utime_usec', 'ru_stime_sec', 'ru_stime_usec', 'ru_maxrss',
				'ru_ixrss', 'ru_idrss', 'ru_isrss', 'ru_minflt', 'ru_majflt', 'ru_nswap', 'ru_inblock',
				'ru_oublock', 'ru_msgsnd', 'ru_msgrcv', 'ru_nsignals', 'ru_nvcsw', 'ru_nivcsw']
			field_types: flat.TypeTextList.from_names(['i64', 'i64', 'i64', 'i64', 'i64', 'i64',
				'i64', 'i64', 'i64', 'i64', 'i64', 'i64', 'i64', 'i64', 'i64', 'i64', 'i64', 'i64'])
		}
	}
	return none
}

fn native_c_struct_field_type(struct_name string, field_name string) ?u32 {
	if struct_name == 'C.tm' && field_name == 'tm_gmtoff' {
		return flat.source_type_i64
	}
	return none
}

// register_multi_return_types updates register multi return types state for ssa.
fn (mut b Builder) register_multi_return_types() {
	if b.tc == unsafe { nil } {
		return
	}
	for _, ret in b.tc.fn_ret_types {
		mut payload := ret
		if ret is types.OptionType {
			payload = ret.base_type
		} else if ret is types.ResultType {
			payload = ret.base_type
		}
		if payload is types.MultiReturn {
			b.register_multi_return_type(payload)
		}
	}
}

// register_multi_return_type updates register multi return type state for ssa.
fn (mut b Builder) register_multi_return_type(ret types.MultiReturn) TypeID {
	name := b.multi_return_c_type(ret)
	source_id := types.Type(ret).source_type_id()
	if typ := b.struct_types[name] {
		b.multi_return_types[source_id] = typ
		return typ
	}
	typ_id := b.m.type_store.register(Type{
		kind: .struct_t
	})
	b.struct_types[name] = typ_id
	b.multi_return_types[source_id] = typ_id
	b.multi_return_field_types[source_id] = map[string]u32{}
	mut field_types := []TypeID{}
	mut field_names := []string{}
	for i, field_type in ret.types {
		field_types << b.ssa_type_from_checker_type(field_type)
		field_name := 'arg${i}'
		field_names << field_name
		b.struct_field_types.set_id(name + '.' + field_name, field_type.source_type_id())
		b.multi_return_field_types[source_id][field_name] = field_type.source_type_id()
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
fn primitive_type_name(typ types.Primitive) u32 {
	if typ.props.has(.boolean) {
		return flat.source_type_bool
	}
	if typ.props.has(.integer) {
		if typ.props.has(.unsigned) {
			return match typ.size {
				8 { flat.source_type_u8 }
				16 { flat.source_type_u16 }
				32 { flat.source_type_u32 }
				64 { flat.source_type_u64 }
				else { flat.source_type_id('u${typ.size}') }
			}
		}
		return match typ.size {
			0 { flat.source_type_int }
			8 { flat.source_type_i8 }
			16 { flat.source_type_i16 }
			32 { flat.source_type_i32 }
			64 { flat.source_type_i64 }
			else { flat.source_type_id('i${typ.size}') }
		}
	}
	if typ.props.has(.float) {
		return match typ.size {
			32 { flat.source_type_f32 }
			else { flat.source_type_f64 }
		}
	}
	return flat.source_type_int
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
		return b.u32_type
	}
	if typ is types.FnType {
		mut params := []TypeID{cap: typ.params.len}
		for param in typ.params {
			params << b.ssa_type_from_checker_type(param.typ)
		}
		return b.m.type_store.register(Type{
			kind:     .func_t
			params:   params
			ret_type: b.ssa_type_from_checker_type(typ.return_type)
		})
	}
	if typ is types.ISize || typ is types.USize {
		if b.m.target.ptr_size == 4 {
			return if typ is types.USize { b.u32_type } else { b.i32_type }
		}
		return b.i64_type
	}
	if typ is types.Primitive {
		return b.resolve_type(primitive_type_name(typ))
	}
	if typ is types.Array {
		return b.array_type
	}
	if typ is types.ArrayFixed {
		return b.m.type_store.get_array(b.ssa_type_from_checker_type(typ.elem_type), typ.len)
	}
	if typ is types.Map {
		return b.map_type
	}
	if typ is types.Pointer {
		return b.m.type_store.get_ptr(b.ssa_type_from_checker_type(typ.base_type))
	}
	if typ is types.OptionType {
		payload := b.ssa_type_from_checker_type(typ.base_type)
		return b.option_type_id_with_payload(typ.base_type.source_type_id(), payload, false)
	}
	if typ is types.ResultType {
		payload := b.ssa_type_from_checker_type(typ.base_type)
		return b.option_type_id_with_payload(typ.base_type.source_type_id(), payload, true)
	}
	if typ is types.Enum {
		return b.i32_type
	}
	if typ is types.MultiReturn {
		return b.register_multi_return_type(typ)
	}
	if typ is types.SumType {
		return b.register_checker_sum_type(typ.source_type_id())
	}
	if typ is types.Interface {
		if typ_id := b.register_checker_interface(typ.source_type_id()) {
			return typ_id
		}
	}
	if typ is types.Struct {
		name_id := typ.source_type_id()
		if b.source_is_sum_type(flat.source_type_name(name_id)) {
			return b.register_checker_sum_type(name_id)
		}
	}
	if typ is types.Alias {
		return b.ssa_type_from_checker_type(typ.base_type)
	}
	return b.resolve_type(typ.source_type_id())
}

fn checker_type_is_function(typ types.Type) bool {
	if typ is types.FnType {
		return true
	}
	if typ is types.Alias {
		return checker_type_is_function(typ.base_type)
	}
	return false
}

fn (mut b Builder) register_checker_sum_type(name_id u32) TypeID {
	name := flat.source_type_name(name_id)
	existing_id := b.struct_types[name] or { TypeID(0) }
	if existing_id > 0 && existing_id < b.m.type_store.types.len
		&& b.m.type_store.types[existing_id].fields.len > 0 {
		return existing_id
	}
	if existing_id > 0 && b.sum_types_registering[existing_id] {
		return existing_id
	}
	typ_id := if existing_id > 0 {
		existing_id
	} else {
		b.m.type_store.register(Type{
			kind: .struct_t
		})
	}
	b.struct_types[name] = typ_id
	b.ssa_type_names[typ_id] = name
	b.sum_types_registering[typ_id] = true
	mut variants := b.sum_type_variants.type_ids(name) or { []u32{} }
	if variants.len == 0 {
		variants = b.sum_type_variants.type_ids(name.all_after('.')) or { []u32{} }
	}
	if variants.len == 0 {
		b.sum_types_registering.delete(typ_id)
		return typ_id
	}
	mut field_types := []TypeID{cap: variants.len}
	mut field_names := []string{cap: variants.len}
	for variant in variants {
		field_types << b.resolve_type(variant)
		field_names << sum_variant_field_name(flat.source_type_name(variant),
			name.all_before_last('.'))
	}
	payload := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
		is_union:    true
	})
	b.m.type_store.types[typ_id] = Type{
		kind:        .struct_t
		fields:      [b.i32_type, payload]
		field_names: ['typ', '_payload']
	}
	b.sum_types_registering.delete(typ_id)
	return typ_id
}

fn (b &Builder) source_is_sum_type(name string) bool {
	if (b.sum_type_variants.type_ids(name) or { []u32{} }).len > 0 {
		return true
	}
	short := name.all_after('.')
	if short != name && (b.sum_type_variants.type_ids(short) or { []u32{} }).len > 0 {
		return true
	}
	if b.tc != unsafe { nil } {
		if (b.tc.sum_types.type_ids(name) or { []u32{} }).len > 0 {
			return true
		}
		if short != name && (b.tc.sum_types.type_ids(short) or { []u32{} }).len > 0 {
			return true
		}
	}
	return false
}

// register_enum_values updates register enum values state for ssa.
fn (mut b Builder) register_enum_values(node flat.Node, module_name string) {
	short_name := node.value_text().all_after('.')
	mut enum_names := []string{}
	enum_names << node.value_text()
	if short_name != node.value_text() {
		enum_names << short_name
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		enum_names << module_name + '.' + short_name
	}
	mut is_flag_enum := false
	for enum_name in enum_names {
		if b.checker_has_flag_enum(flat.source_type_id(enum_name)) {
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
			if explicit := b.enum_field_expr_value(b.a.child(field, 0), flat.source_type_id(enum_names.last())) {
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
	if typ_id > 0 {
		b.ssa_type_names[typ_id] = name
	}
	if existing := b.struct_types[name] {
		if existing > 0 && existing < b.m.type_store.types.len
			&& b.m.type_store.types[existing].fields.len > 0 {
			return
		}
	}
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

// register_checker_interface materializes the runtime interface box used by
// native method dispatch: {object pointer, concrete type id, boxed ownership}.
fn (mut b Builder) register_checker_interface(name_id u32) ?TypeID {
	if b.tc == unsafe { nil } || name_id == 0 {
		return none
	}
	raw_name := flat.source_type_name(name_id).trim_left('&').trim_space()
	if raw_name.len == 0 {
		return none
	}
	short_name := raw_name.all_after_last('.')
	qualified_name := qualify_type_name(raw_name, b.cur_module)
	mut interface_name := ''
	for candidate in [raw_name, qualified_name, short_name] {
		if candidate in b.tc.interface_names {
			interface_name = candidate
			break
		}
	}
	if interface_name.len == 0 {
		return none
	}
	if typ_id := b.interface_types[interface_name] {
		b.interface_types[raw_name] = typ_id
		b.interface_types[short_name] = typ_id
		return typ_id
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut interface_fields := []TypeID{}
	interface_fields << ptr_i8
	interface_fields << b.i32_type
	interface_fields << b.i1_type
	mut interface_field_names := ['_object', '_typ', '_object_is_boxed']
	for field in b.tc.interface_field_list(flat.source_type_id(interface_name)) {
		interface_fields << b.ssa_type_from_checker_type(field.typ)
		interface_field_names << field.name
	}
	typ_id := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      interface_fields
		field_names: interface_field_names
	})
	b.interface_types[interface_name] = typ_id
	b.interface_types[raw_name] = typ_id
	b.interface_types[short_name] = typ_id
	b.interface_type_names[typ_id] = interface_name
	b.ssa_type_names[typ_id] = interface_name
	return typ_id
}

fn (b &Builder) is_interface_type_id(typ_id TypeID) bool {
	return typ_id in b.interface_type_names
}

fn (b &Builder) is_interface_related_type(typ_id TypeID) bool {
	if b.is_interface_type_id(typ_id) {
		return true
	}
	if typ_id > 0 && typ_id < b.m.type_store.types.len {
		typ := b.m.type_store.types[typ_id]
		return typ.kind == .ptr_t && b.is_interface_type_id(typ.elem_type)
	}
	return false
}

fn (mut b Builder) interface_name_for_type(typ_id TypeID) ?u32 {
	name := b.interface_type_names[typ_id] or { return none }
	return flat.source_type_id(name)
}

fn (b &Builder) interface_concrete_type_id(source_type_id u32) TypeID {
	name := flat.source_type_name(source_type_id).trim_left('&')
	short_name := name.all_after_last('.')
	for candidate in [name, short_name] {
		if typ_id := b.struct_types[candidate] {
			return typ_id
		}
	}
	if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
		if typ_id := b.struct_types['${b.cur_module}.${short_name}'] {
			return typ_id
		}
	}
	return b.resolve_type(source_type_id)
}

fn (b &Builder) interface_dispatch_id(interface_type TypeID, source_type_id u32) int {
	if b.tc != unsafe { nil } {
		if interface_name := b.interface_type_names[interface_type] {
			ids := b.tc.interface_type_ids(flat.source_type_id(interface_name))
			name := flat.source_type_name(source_type_id).trim_left('&')
			if id := ids[source_type_id] {
				return id
			}
			name_id := flat.source_type_id(name)
			if id := ids[name_id] {
				return id
			}
			short_name := name.all_after_last('.')
			for impl_id, id in ids {
				if flat.source_type_name(impl_id).all_after_last('.') == short_name {
					return id
				}
			}
		}
	}
	return b.interface_concrete_type_id(source_type_id)
}

fn (mut b Builder) interface_init_concrete_type(node flat.Node, interface_type TypeID) ?int {
	for index in 0 .. node.children_count {
		field := b.a.child_node(&node, index)
		if field.value != '_object' || field.children_count == 0 {
			continue
		}
		object_expr_id := b.a.child(field, 0)
		if name_id := b.interface_object_concrete_type_name(object_expr_id) {
			return b.interface_dispatch_id(interface_type, name_id)
		}
	}
	return none
}

fn (b &Builder) interface_object_concrete_type_name(id flat.NodeId) ?u32 {
	if !b.valid_node_id(id) {
		return none
	}
	mut name_id := b.checked_expr_type_name(id)
	mut name := flat.source_type_name(name_id).trim_space()
	for name.starts_with('&') || name.starts_with('?') || name.starts_with('!') {
		name = name[1..].trim_space()
	}
	if name.len > 0 && name !in ['void', 'voidptr', 'IError', 'unknown'] {
		candidate := flat.source_type_id(name)
		typ_id := b.resolve_type(candidate)
		if typ_id > 0 && typ_id < b.m.type_store.types.len
			&& b.m.type_store.types[typ_id].kind == .struct_t {
			return candidate
		}
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.paren, .prefix {
			if node.children_count > 0 {
				return b.interface_object_concrete_type_name(b.a.child(&node, 0))
			}
		}
		.cast_expr {
			target_name := node.value_text().trim_left('&?')
			if target_name.len > 0 && target_name !in ['void', 'voidptr', 'IError'] {
				return flat.source_type_id(target_name)
			}
			if node.children_count > 0 {
				return b.interface_object_concrete_type_name(b.a.child(&node, 0))
			}
		}
		.struct_init {
			if node.source_value_id() != 0 {
				return flat.source_type_id(node.value_text().trim_left('&?'))
			}
		}
		else {}
	}
	return none
}

// register_sum_type_name updates register sum type name state for ssa.
fn (mut b Builder) register_sum_type_name(name string, module_name string, typ_id TypeID) {
	qualified_name := qualify_type_name(name, module_name)
	b.ssa_type_names[typ_id] = qualified_name
	qualified_id := flat.source_type_id(qualified_name)
	b.register_struct_type_name(name, module_name, typ_id)
	mut variants := []u32{}
	if b.tc != unsafe { nil } {
		if tc_variants := b.tc.sum_types.type_ids(qualified_name) {
			variants = tc_variants.clone()
		}
	}
	if variants.len == 0 {
		for node in b.a.nodes {
			if node.kind != .type_decl || node.value_text() != name || !is_sum_type_decl(node) {
				continue
			}
			variants = b.sum_type_variants_for_decl(node, module_name)
			break
		}
	}
	b.sum_type_variants.set_ids(qualified_name, variants)
	b.sum_type_canonical.set_id(qualified_name, qualified_id)
	if name != qualified_name {
		b.sum_type_variants.set_ids(name, variants)
		b.sum_type_canonical.set_id(name, qualified_id)
	}
	short_name := qualified_name.all_after('.')
	b.sum_type_variants.set_ids(short_name, variants)
	b.sum_type_canonical.set_id(short_name, qualified_id)
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
fn qualify_type_ref_name(name_id u32, module_name string) u32 {
	if name_id == 0 { return 0 }
	name := flat.source_type_name(name_id)
	qualified := qualify_type_ref_name_bytes(name, module_name)
	return if qualified == name { name_id } else { flat.source_type_id(qualified) }
}

fn qualify_type_ref_name_bytes(name string, module_name string) string {
	if name.len == 0 {
		return name
	}
	if name.starts_with('fn(') || name.starts_with('fn (') {
		return name
	}
	if name.starts_with('&') {
		return '&' + qualify_type_ref_name_bytes(name[1..], module_name)
	}
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return name[..1] + qualify_type_ref_name_bytes(name[1..], module_name)
	}
	if name.starts_with('[]') {
		return '[]' + qualify_type_ref_name_bytes(name[2..], module_name)
	}
	if name.starts_with('[') {
		idx := name.index_u8(`]`)
		if idx > 0 && idx + 1 < name.len {
			return name[..idx + 1] + qualify_type_ref_name_bytes(name[idx + 1..], module_name)
		}
	}
	if name.contains('[') && name.ends_with(']') {
		idx := name.index_u8(`[`)
		if idx > 0 && idx < name.len - 1 {
			return qualify_type_ref_name_bytes(name[..idx], module_name) + name[idx..]
		}
	}
	if name.starts_with('map[') {
		key_type, val_type := map_type_parts_bytes(name)
		if key_type.len > 0 && val_type.len > 0 {
			return 'map[' + qualify_type_ref_name_bytes(key_type, module_name) + ']' +
				qualify_type_ref_name_bytes(val_type, module_name)
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
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'string', 'void', 'voidptr', 'rune', 'char', 'array', 'map']
}

// sum_type_variants_for_decl supports sum type variants for decl handling for Builder.
fn (b &Builder) sum_type_variants_for_decl(node flat.Node, module_name string) []u32 {
	mut variants := []u32{cap: node.children_count}
	if node.children_count > 0 {
		for i in 0 .. node.children_count {
			variant_id := b.a.child_node(&node, i).source_value_id()
			variant := flat.source_type_name(variant_id)
			qualified := qualify_type_name(variant, module_name)
			variants << if qualified == variant {
				variant_id
			} else {
				flat.source_type_id(qualified)
			}
		}
		return variants
	}
	for variant in node.typ_text().split('|') {
		trimmed := variant.trim_space()
		if trimmed.len > 0 {
			variants << flat.source_type_id(qualify_type_name(trimmed, module_name))
		}
	}
	return variants
}

// is_sum_type_decl reports whether a type declaration is a sum type. Older
// flattened declarations retain variant children, while compact declarations
// can retain only their `A | B` source spelling.
fn is_sum_type_decl(node flat.Node) bool {
	return node.kind == .type_decl
		&& (node.children_count > 0 || node.typ_text().contains('|'))
}

// sum_variant_field_name builds the payload field spelling used by source
// smartcasts. The checker qualifies variants with their declaring module, while
// smartcasts use the short source spelling (for example `string` rather than
// `strconv.string`).
fn sum_variant_field_name(variant string, module_name string) string {
	mut name := variant
	if module_name.len > 0 && module_name !in ['main', 'builtin'] {
		prefix := module_name + '.'
		if name.starts_with(prefix) {
			name = name[prefix.len..]
		}
	}
	return sum_field_name(name)
}

// register_sum_field_type updates register sum field type state for ssa.
fn (mut b Builder) register_sum_field_type(name string, module_name string, field_name string, field_type_id u32) {
	short_name := name.all_after('.')
	b.struct_field_types.set_id(name + '.' + field_name, field_type_id)
	b.struct_field_types.set_id(short_name + '.' + field_name, field_type_id)
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		b.struct_field_types.set_id(module_name + '.' + short_name + '.' + field_name, field_type_id)
	}
}

// registered_struct_type looks up an existing layout without resolving aliases or
// changing module precedence. Source composites use spelling IDs; declaration and
// emitted C names remain in the symbol index.
fn (b &Builder) registered_struct_type(name_id u32) ?TypeID {
	if typ := b.option_types[name_id] {
		return typ
	}
	if typ := b.multi_return_types[name_id] {
		return typ
	}
	if typ := b.struct_types[flat.source_type_name(name_id)] {
		return typ
	}
	return none
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
	mut cur_module := 'main'
	for node in b.a.nodes {
		if node.kind == .file {
			// A script can omit its module declaration after imported module files.
			cur_module = 'main'
			continue
		}
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
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
				if cur_module.len > 0 {
					b.const_exprs['${cur_module}.${field.value}'] = expr_id
				} else {
					b.const_exprs['main.${field.value}'] = expr_id
				}
				if cur_module.len == 0 || cur_module == 'main' {
					b.const_exprs[field.value] = expr_id
				}
			}
		}
	}
}

// register_globals updates register globals state for ssa.
fn c_standard_stream_symbol(name string) string {
	$if linux {
		return name
	} $else {
		return '__${name}p'
	}
}

fn (mut b Builder) register_globals() {
	mut cur_module := 'main'
	for node in b.a.nodes {
		if node.kind == .file {
			cur_module = 'main'
			continue
		}
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
			continue
		}
		if node.kind == .import_decl {
			alias := if node.typ != 0 {
				node.typ_text()
			} else {
				node.value_text().all_after_last('.')
			}
			b.global_imports[cur_module + '#' + alias] = node.value_text()
			continue
		}
		if node.kind == .global_decl {
			for i in 0 .. node.children_count {
				f := b.a.child_node(&node, i)
				if b.m.target.ptr_size == 4 {
					mut typ := b.resolve_type(f.typ)
					if b.m.target.ptr_size == 4 && f.typ == 0 && f.children_count > 0 {
						typ = b.global_constant_type(b.a.child(f, 0))
					}
					name := if b.source_modules.len > 0 {
						ssa_fn_name_in_module(cur_module, f.value)
					} else {
						f.value
					}
					addr := b.m.add_global(name, typ)
					b.vars[name] = addr
					if b.m.target.ptr_size == 4 && f.children_count > 0 {
						b.cur_module = cur_module
						b.initialize_global(b.m.values[addr].index, b.a.child(f, 0))
					}
				} else {
					type_name := (if f.typ != 0 {
						f.typ
					} else if f.children_count > 0 {
						expr_id := b.a.child(f, 0)
						checked_source_id := b.checked_expr_type_name(expr_id)
						checked_source_id_7 := checked_source_id

						expr := b.a.node(expr_id)
						if checked_source_id != 0 {
							checked_source_id_7
						} else if expr.kind == .struct_init || expr.kind == .array_init {
							expr.source_value_id()
						} else if expr.kind == .array_literal {
							flat.source_type_id('[]int')
						} else if expr.kind == .map_init {
							flat.source_type_id('map[string]int')
						} else {
							b.infer_v_type(expr_id)
						}
					} else {
						flat.source_type_int
					})
					b.cur_module = cur_module
					name := native_global_name(cur_module, f.value)
					typ := match name {
						'g_main_argc' { b.i64_type }
						'g_main_argv' { b.m.type_store.get_ptr(b.m.type_store.get_ptr(b.i8_type)) }
						else {
							b.resolve_struct_storage_type(type_name, cur_module)
						}
					}
					if f.value.starts_with('C.') {
						if f.value == 'C.errno' {
							continue
						}
						symbol := match f.value {
							'C.stdin' { c_standard_stream_symbol('stdin') }
							'C.stdout' { c_standard_stream_symbol('stdout') }
							'C.stderr' { c_standard_stream_symbol('stderr') }
							else { f.value.all_after('C.') }
						}
						b.vars[f.value] = b.m.add_external_global(symbol, typ)
						b.global_type_names.set_id(f.value, type_name)
						continue
					}
					b.vars[name] = b.m.add_global(ssa_c_name(name), typ)
					b.global_type_names.set_id(name, type_name)
					b.global_aliases[cur_module + '.' + f.value] = name
				}
			}
		}
	}
	b.ensure_runtime_global('g_main_argc', b.i64_type)
	b.ensure_runtime_global('g_main_argv',
		b.m.type_store.get_ptr(b.m.type_store.get_ptr(b.i8_type)))
}

fn (mut b Builder) initialize_global(index int, id flat.NodeId) {
	typ_id := b.m.globals[index].typ
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return
	}
	typ := b.m.type_store.types[typ_id]
	if typ.kind == .float_t {
		value := b.global_constant_float(id, 0)
		bits := if typ.width == 32 { u64(math.f32_bits(f32(value))) } else { math.f64_bits(value) }
		mut data := []u8{len: b.m.type_size(typ_id)}
		for i in 0 .. data.len {
			data[i] = u8(bits >> (i * 8))
		}
		b.m.globals[index].initial_data = data
	} else if typ.kind == .int_t {
		b.m.globals[index].initial_value = b.narrow_global_constant(b.global_constant_int(id,
			0), typ_id)
	}
}

fn (mut b Builder) global_constant_type(id flat.NodeId) TypeID {
	if b.tc != unsafe { nil } {
		return b.ssa_type_from_checker_type(b.tc.resolve_type(id))
	}
	node := b.a.nodes[int(id)]
	if node.kind == .cast_expr {
		return b.resolve_type(node.source_value_id())
	}
	if node.kind == .float_literal {
		return b.f64_type
	}
	if node.kind == .bool_literal {
		return b.i1_type
	}
	return b.i32_type
}

fn (b &Builder) narrow_global_constant(value i64, typ_id TypeID) i64 {
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return value
	}
	typ := b.m.type_store.types[typ_id]
	if typ.kind != .int_t || typ.width <= 1 || typ.width >= 64 {
		return value
	}
	mask := (u64(1) << typ.width) - 1
	bits := u64(value) & mask
	if !typ.is_unsigned && bits & (u64(1) << (typ.width - 1)) != 0 {
		return i64(bits | ~mask)
	}
	return i64(bits)
}

fn (b &Builder) global_constant_expr(node flat.Node) ?flat.NodeId {
	if node.kind == .ident {
		return b.lookup_const_expr(node.value_text())
	}
	if name := b.source_selector_name(node) {
		return b.lookup_const_expr(name)
	}
	return none
}

fn (mut b Builder) global_constant_int(id flat.NodeId, depth int) i64 {
	if !b.valid_node_id(id) || depth > 64 {
		return 0
	}
	node := b.a.nodes[int(id)]
	if expr := b.global_constant_expr(node) {
		return b.global_constant_int(expr, depth + 1)
	}
	match node.kind {
		.int_literal {
			return global_integer_literal(node.value_text())
		}
		.float_literal {
			return i64(node.value_text().f64())
		}
		.bool_literal {
			return i64(int(node.value_text() == 'true'))
		}
		.char_literal {
			return i64(char_literal_value(node.value_text()))
		}
		.paren {
			return b.global_constant_int(b.a.child(&node, 0), depth + 1)
		}
		.block {
			if node.children_count == 0 {
				return 0
			}
			return b.global_constant_int(b.a.child(&node, node.children_count - 1), depth + 1)
		}
		.expr_stmt {
			if node.children_count == 0 {
				return 0
			}
			return b.global_constant_int(b.a.child(&node, 0), depth + 1)
		}
		.if_expr {
			if node.children_count < 3 {
				return 0
			}
			condition := b.global_constant_int(b.a.child(&node, 0), depth + 1)
			branch := if condition != 0 { 1 } else { 2 }
			return b.global_constant_int(b.a.child(&node, branch), depth + 1)
		}
		.sizeof_expr {
			if node.children_count > 0 {
				operand := b.a.child(&node, 0)
				return b.sizeof_type_name(b.checked_expr_type_name(operand))
			}
			return b.sizeof_type_name(node.source_value_id())
		}
		.selector {
			if node.value == 'len' && node.children_count > 0 {
				base := b.a.child_node(&node, 0)
				if base.kind == .array_literal {
					return base.children_count
				}
				if base.kind == .ident {
					if expr_id := b.lookup_const_expr(base.value) {
						expr := b.a.nodes[int(expr_id)]
						if expr.kind == .array_literal || expr.kind == .array_init {
							return expr.children_count
						}
					}
				}
			}
			return 0
		}
		.cast_expr {
			target := b.resolve_type(node.source_value_id())
			if b.m.type_store.types[target].kind == .float_t {
				return i64(b.global_constant_float(id, depth + 1))
			}
			value := b.global_constant_int(b.a.child(&node, 0), depth + 1)
			return b.narrow_global_constant(value, target)
		}
		.prefix {
			value := b.global_constant_int(b.a.child(&node, 0), depth + 1)
			return match node.op {
				.minus { -value }
				.bit_not { ~value }
				.not { i64(int(value == 0)) }
				else { value }
			}
		}
		.infix {
			expr_type := b.global_constant_type(id)
			if b.m.type_store.types[expr_type].kind == .float_t {
				return i64(b.global_constant_float(id, depth + 1))
			}
			left_id := b.a.child(&node, 0)
			right_id := b.a.child(&node, 1)
			left := b.global_constant_int(left_id, depth + 1)
			right := b.global_constant_int(right_id, depth + 1)
			left_type := b.global_constant_type(left_id)
			right_type := b.global_constant_type(right_id)
			unsigned := b.is_unsigned_type(left_type) || b.is_unsigned_type(right_type)
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
				width := int_max(32, b.scalar_type_width(left_type))
				if u64(right) >= u64(width) {
					return 0
				}
				mut bits := u64(left)
				if node.op == .right_shift_unsigned && b.scalar_type_width(left_type) < 32 {
					bits &= (u64(1) << b.scalar_type_width(left_type)) - 1
				}
				shifted := if node.op == .left_shift {
					i64(bits << u64(right))
				} else if node.op == .right_shift_unsigned || b.is_unsigned_type(left_type) {
					i64(bits >> u64(right))
				} else {
					left >> u64(right)
				}
				return b.narrow_global_constant(shifted, expr_type)
			}
			value := match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div {
					if right == 0 {
						i64(0)
					} else if unsigned {
						i64(u64(left) / u64(right))
					} else {
						left / right
					}
				}
				.mod {
					if right == 0 {
						i64(0)
					} else if unsigned {
						i64(u64(left) % u64(right))
					} else {
						left % right
					}
				}
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				.eq { i64(int(left == right)) }
				.ne { i64(int(left != right)) }
				.lt { i64(int(if unsigned { u64(left) < u64(right) } else { left < right })) }
				.le { i64(int(if unsigned { u64(left) <= u64(right) } else { left <= right })) }
				.gt { i64(int(if unsigned { u64(left) > u64(right) } else { left > right })) }
				.ge { i64(int(if unsigned { u64(left) >= u64(right) } else { left >= right })) }
				else { i64(0) }
			}
			return b.narrow_global_constant(value, expr_type)
		}
		else { return 0 }
	}
}

fn (mut b Builder) global_constant_float(id flat.NodeId, depth int) f64 {
	if !b.valid_node_id(id) || depth > 64 {
		return 0
	}
	node := b.a.nodes[int(id)]
	if expr := b.global_constant_expr(node) {
		return b.global_constant_float(expr, depth + 1)
	}
	match node.kind {
		.float_literal { return node.value_text().f64() }
		.int_literal { return f64(u64(global_integer_literal(node.value_text()))) }
		.paren { return b.global_constant_float(b.a.child(&node, 0), depth + 1) }
		.prefix {
			value := b.global_constant_float(b.a.child(&node, 0), depth + 1)
			return if node.op == .minus { -value } else { value }
		}
		.cast_expr {
			target := b.resolve_type(node.source_value_id())
			if b.m.type_store.types[target].kind != .float_t {
				return f64(b.global_constant_int(id, depth + 1))
			}
			value := b.global_constant_float(b.a.child(&node, 0), depth + 1)
			return if target == b.f32_type { f64(f32(value)) } else { value }
		}
		.infix {
			left := b.global_constant_float(b.a.child(&node, 0), depth + 1)
			right := b.global_constant_float(b.a.child(&node, 1), depth + 1)
			return match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div {
					if right != 0 { left / right } else { f64(0) }
				}
				else { f64(0) }
			}
		}
		.call {
			if node.children_count == 2 {
				callee := b.a.child_node(&node, 0)
				name := if callee.kind == .selector {
					b.selector_qualified_name(callee)
				} else {
					callee.value
				}
				if name in ['sqrt', 'math.sqrt'] {
					return math.sqrt(b.global_constant_float(b.a.child(&node, 1), depth + 1))
				}
			}
			return f64(0)
		}
		else { return f64(b.global_constant_int(id, depth + 1)) }
	}
}

fn global_integer_literal(value string) i64 {
	clean := value.replace('_', '')
	mut digits := clean
	mut base := u64(10)
	if clean.starts_with('0x') || clean.starts_with('0X') {
		digits = clean[2..]
		base = 16
	} else if clean.starts_with('0b') || clean.starts_with('0B') {
		digits = clean[2..]
		base = 2
	} else if clean.starts_with('0o') || clean.starts_with('0O') {
		digits = clean[2..]
		base = 8
	}
	mut result := u64(0)
	for ch in digits {
		digit := if ch >= `0` && ch <= `9` {
			u64(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			u64(ch - `a` + 10)
		} else {
			u64(ch - `A` + 10)
		}
		result = result * base + digit
	}
	return i64(result)
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
	b.register_extern('access', b.i32_type, p2)
	b.register_extern('mkdir', b.i32_type, p2)
	b.register_extern('chmod', b.i32_type, p2)
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
	b.register_extern('pthread_create', b.i32_type, [
		b.m.type_store.get_ptr(b.u64_type),
		ptr_i8,
		ptr_i8,
		ptr_i8,
	])
	b.register_extern('pthread_join', b.i32_type, [b.u64_type, b.m.type_store.get_ptr(ptr_i8)])
	b.register_extern('pthread_detach', b.i32_type, [b.u64_type])
	b.register_extern('pthread_attr_init', b.i32_type, [ptr_i8])
	b.register_extern('pthread_attr_setstacksize', b.i32_type, [ptr_i8, b.u64_type])
	b.register_extern('pthread_attr_destroy', b.i32_type, [ptr_i8])
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
	if b.m.target.ptr_size == 8 {
		b.register_basic_format_stubs()
		b.register_string_plus_stubs()
		b.register_native_string_format_helpers()
		b.register_bench_runtime_stubs()
	}
	p1 = []TypeID{}
	p1 << ptr_i8
	b.register_extern('free', b.void_type, p1)
	p2 = []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	b.register_extern('fprintf', b.i32_type, p2)
	b.register_ssa_memdup_helper()

	for node in b.a.nodes {
		if node.kind != .c_fn_decl {
			continue
		}
		ret_type := b.resolve_c_decl_type(node.typ)
		mut param_types := []TypeID{}
		mut variadic_start := -1
		for i in 0 .. node.children_count {
			child := b.a.child_node(&node, i)
			if child.kind == .param {
				if flat.source_type_shape(child.typ).has(.variadic) {
					variadic_start = param_types.len
					continue
				}
				param_types << b.resolve_c_decl_type(child.typ)
			}
		}
		b.register_extern(node.value_text(), ret_type, param_types)
		if variadic_start >= 0 {
			b.set_extern_variadic(node.value_text(), variadic_start)
		}
		if node.value_text().starts_with('C.') {
			b.register_extern(node.value_text()[2..], ret_type, param_types)
			if variadic_start >= 0 {
				b.set_extern_variadic(node.value_text()[2..], variadic_start)
			}
		}
	}

	c_variant_names := b.c_variant_function_names()
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .file {
			cur_module = ''
			continue
		}
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
			continue
		}
		if node.kind == .fn_decl {
			if file := b.a.source_files[node.pos.id] {
				fn_name := ssa_fn_name_in_module(cur_module, node.value_text())
				if c_variant_names[fn_name] && !file.name.ends_with('.c.v') {
					continue
				}
			}
			if b.skip_source_fn_in_module(node.value_text(), cur_module) {
				continue
			}
			fn_name := ssa_fn_name_in_module(cur_module, node.value_text())
			if node.value_text() in native_runtime_override_names && node.value_text() in b.fn_ids {
				continue
			}
			if (b.exact_used_fns || b.used_fns.len > 0) && !b.source_fn_is_used(node.value_text(), cur_module) {
				continue
			}
			ret_type := b.checker_return_type(node.value_text(), cur_module) or {
				b.resolve_type_in_module(node.typ, cur_module)
			}
			mut param_types := []TypeID{}
			mut param_idx := 0
			for i in 0 .. node.children_count {
				child := b.a.child_node(&node, i)
				if child.kind == .param {
					typ_name_source_id := b.source_param_type_name(child, node.value_text(), cur_module, param_idx)

					mut param_type := b.resolve_type_in_module(typ_name_source_id, cur_module)
					if checked := b.checker_param_type(node.value, cur_module, param_idx) {
						if checker_type_is_function(checked) {
							param_type = b.ssa_type_from_checker_type(checked)
						}
					}
					param_types << param_type
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
			b.source_fn_names[fn_name] = true
			if cur_module == 'builtin' {
				b.runtime_fn_ids[fn_name] = func_id
				b.runtime_fn_types[fn_name] = fn_type
			}
		}
	}
	b.register_top_level_main()
	if b.m.target.ptr_size == 4 {
		b.register_basic_format_stubs()
		b.register_string_plus_stubs()
		b.register_bench_runtime_stubs()
	}
	b.register_wyhash_stubs()
	b.register_string_eq_stub()
	b.register_fast_string_eq_stub()
	b.register_string_lt_stub()
	b.register_string_trim_stubs()
	b.register_string_last_part_stubs()
	b.register_ierror_stubs()
	b.register_array_runtime_stubs()
	b.register_arguments_stub()
	b.register_printing_stubs()
	b.register_panic_stub()
	b.register_string_builder_stubs()
	b.register_path_runtime_stubs()
	b.register_map_runtime_stubs()
	if b.m.target.ptr_size == 8 { b.register_native_map_format_helpers() }
	b.register_u8_runtime_stubs()
	b.register_heap_tracking_stubs()
	if b.m.target.ptr_size == 4 { b.register_at_exit_stub() } else { b.register_at_exit_runtime() }
	b.register_embed_file_interface_stubs()
	b.register_pthread_compat_stubs()
	b.register_prealloc_allocator_stubs()
	b.register_prealloc_atomic_stubs()
	b.register_atomic_builtin_stubs()
	b.register_process_capture_helpers()
	b.register_file_check_stubs()
	b.register_modulecache_stubs()
	b.register_fd_macro_stubs()
	b.register_signal_macro_stubs()
	b.register_os_stat_stubs()
	b.register_array_string_stubs()
	b.register_fixed_array_contains_stubs()
	b.register_array_contains_stubs()
	b.register_enum_autostr_fns()
	if b.m.target.ptr_size == 8 { b.register_native_runtime_helpers() }
	if b.m.target.ptr_size == 8 { b.register_native_allocation_helpers() }
	if b.m.target.ptr_size == 8 { b.register_native_header_helpers() }
	if b.m.target.ptr_size == 8 { b.register_native_signal_helpers() }
	if b.m.target.ptr_size == 8 { b.register_native_worker_helpers() }
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
				cur_module = b.source_module_name(node)
			}
			.enum_decl {
				qualified := if cur_module.len > 0 && cur_module != 'main'
					&& cur_module != 'builtin' {
					'${cur_module}.${node.value_text()}'
				} else {
					node.value_text()
				}
				fn_name := '${qualified.replace('.', '__')}__autostr'
				if fn_name in b.fn_ids {
					continue
				}
				is_flag := b.is_flag_enum_type_name(flat.source_type_id(qualified))
				mut names := []string{}
				mut values := []int{}
				mut val := 0
				for i in 0 .. node.children_count {
					field := b.a.child_node(&node, i)
					if field.kind != .enum_field {
						continue
					}
					if registered := b.enum_values['${qualified}.${field.value}'] {
						val = registered
					}
					names << field.value
					// Flag enums store one bit per field, matching `enum_value_for_type`.
					values << if is_flag { 1 << val } else { val }
					val++
				}
				mut p1 := []TypeID{}
				p1 << b.i64_type
				fid := b.register_runtime_function(fn_name, b.str_type, p1) or { continue }
				if is_flag {
					// `@[flag]` enums stringify as `Name{.a | .b}` (and `Name{}` for 0),
					// matching the C backend, rather than a single field name.
					b.generate_flag_enum_autostr_body(fid, node.value_text().all_after_last('.'), names,
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
// expression. References resolve against already registered fields of the same enum,
// preserving exact declaration names before trying an escaped keyword alternative.
fn (b &Builder) enum_field_expr_value(id flat.NodeId, enum_name_id u32) ?int {
	if int(id) < 0 || int(id) >= b.a.nodes.len {
		return none
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return parse_int_literal(node.value_text())
		}
		.ident, .enum_val, .selector {
			name := if node.kind == .selector {
				b.qualified_expr_name(id)
			} else {
				node.value_text()
			}
			return b.enum_field_value_for_type(enum_name_id, name, false)
		}
		.paren, .cast_expr {
			if node.children_count == 0 {
				return none
			}
			return b.enum_field_expr_value(b.a.child(&node, 0), enum_name_id)
		}
		.call {
			if node.children_count != 2 {
				return none
			}
			cast_name := b.qualified_expr_name(b.a.child(&node, 0))
			if cast_name !in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64',
				'uint', 'rune'] {
				return none
			}
			return b.enum_field_expr_value(b.a.child(&node, 1), enum_name_id)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := b.enum_field_expr_value(b.a.child(&node, 0), enum_name_id)?
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
			left := b.enum_field_expr_value(b.a.child(&node, 0), enum_name_id)?
			right := b.enum_field_expr_value(b.a.child(&node, 1), enum_name_id)?
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
	int_str_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.runtime_fn_id('int_str'))
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
	plus_ref := b.m.add_value(.func_ref, b.str_type, 'string__plus', b.runtime_fn_id('string__plus'))

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

fn (b &Builder) source_module_name(node flat.Node) string {
	if file := b.a.source_files[node.pos.id] {
		if name := b.source_modules[file.name] {
			if name.len > 0 {
				return name
			}
		}
	}
	return node.value_text()
}

fn (b &Builder) source_import_name(node flat.Node, alias string) ?string {
	file := b.a.source_files[node.pos.id] or { return none }
	return b.source_imports[file.name][alias] or { return none }
}

fn (b &Builder) source_selector_name(node flat.Node) ?string {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := b.a.child_node(&node, 0)
	if base.kind != .ident || base.value_text() in b.vars {
		return none
	}
	module_name := b.source_import_name(node, base.value_text()) or { return none }
	return ssa_fn_name_in_module(module_name, node.value_text())
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
	b.runtime_fn_types[name] = fn_type
	b.runtime_fn_ids[name] = func_id
	mut f := b.m.funcs[func_id]
	f.is_c_extern = true
	f.is_prototype = true
	f.linkage = .external
	b.m.funcs[func_id] = f
	// Synthetic runtime helpers are emitted before source declarations. Their
	// libc function references must already carry Darwin's variadic ABI metadata.
	fixed := match name.trim_string_left('C.') {
		'printf', 'wprintf' { 1 }
		'fprintf', 'dprintf', 'sprintf', 'sscanf', 'fcntl', 'open' { 2 }
		'snprintf' { 3 }
		else { -1 }
	}
	if fixed >= 0 {
		b.set_extern_variadic(name, fixed)
	}
}

fn (mut b Builder) set_extern_variadic(name string, fixed int) {
	if fn_id := b.fn_ids[name] {
		b.m.funcs[fn_id].is_variadic = true
		b.m.funcs[fn_id].variadic_start = fixed
	}
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
	mut module_name := ''
	for node in b.a.nodes {
		if node.kind == .file {
			module_name = ''
		} else if node.kind == .module_decl {
			module_name = b.source_module_name(node)
		} else if node.kind == .fn_decl
			&& ssa_fn_name_in_module(module_name, node.value_text()) == name
			&& !b.skip_source_fn_in_module(node.value_text(), module_name)
			&& (!(b.exact_used_fns || b.used_fns.len > 0)
				|| b.source_fn_is_used(node.value_text(), module_name)) {
			return true
		}
	}
	return false
}

// These checker-visible placeholders stand in for compiler-native helpers.
// Calls must resolve to the synthetic implementation, not a fixture's stub body.
const native_runtime_override_names = ['v3_string_zpad', 'v3_f64_zpad', 'v3_int_zpad', 'v3_i64_zpad',
	'v3_u64_zpad', 'v3_string_upper_ascii', 'v3_string_plus_sign', 'v3_string_rpad_zero',
	'v3_char_string', 'v3_f64_fixed', 'v3_f64_exp', 'v3_f64_general', 'v3_f64_trimmed',
	'v3_aligned_memdup', 'v3_heap_array', 'v3_map_str', 'print', 'println', 'eprint', 'eprintln',
	'error', 'error_with_code']

// skip_source_fn supports skip source fn handling for Builder.
fn (b &Builder) skip_source_fn(name string) bool {
	if name in ['_wymix', 'wyhash', 'wyhash64', 'string__eq', 'string__lt', 'array_new', 'array_get',
		'string__plus', 'string_plus_many', 'string.trim_right', 'array_push', 'array_push_many',
		'array.push_many', 'array_clone', 'array.reverse', 'array__reverse', 'panic', 'panic_debug',
		'fast_string_eq', 'strings.new_builder', 'strings.Builder.write_string',
		'strings.Builder.writeln', 'strings.Builder.str', 'strings.Builder.write_ptr',
		'strings.Builder.write_u8', 'strings.Builder.write_byte', 'strings.Builder.write_rune',
		'strings.Builder.free', 'strings.Builder.last_n', 'Builder.write_string', 'Builder.writeln',
		'Builder.str', 'Builder.write_ptr', 'Builder.write_u8', 'Builder.write_byte',
		'Builder.write_rune', 'Builder.free', 'Builder.last_n', 'new_map', 'map__set', 'map__get',
		'map__exists', 'map__get_check', 'map__get_key_check', 'map__get_or_set', 'map__delete',
		'map__clear', 'map__clone', 'map__move', 'map__free', 'map__reserve', 'map__keys', 'map__values',
		'v3_map_find', 'v3_map_set_sized', 'u8.is_digit', 'u8.is_letter', 'u8.is_alnum', 'u8.is_capital',
		'bytestr', '[]u8.bytestr', '[]u8.hex', '[]rune.string', 'Array_u8__bytestr', 'Array_u8__hex',
		'Array_rune__string', 'array.repeat_to_depth', 'string.all_before_last',
		'string__all_before_last', 'all_before_last', 'string.all_after_last', 'string__all_after_last',
		'all_after_last', '_ht_alloc', '_ht_free', 'f32_to_str_l', 'f32_to_str_l_with_dot',
		'f64_to_str_l', 'f64_to_str_l_with_dot', 'print', 'println', 'eprint', 'eprintln', 'arguments',
		'at_exit', 'tos2', 'tos3', 'tos_clone', 'v_prealloc_atomic_add_i32',
		'v_prealloc_atomic_load_i32', 'v_prealloc_atomic_store_i32', 'v_prealloc_atomic_cas_i32',
		'FD_ZERO', 'FD_SET', 'FD_ISSET', 'v_signal_with_handler_cast', 'normalize_path_in_builder',
		'check_fwrite', 'check_fread', 'os.check_fwrite', 'os.check_fread', '__v3_array_eq_raw',
		'__v3_array_eq_string', '__v3_array_eq_array', 'u8.vstring', 'u8.vstring_with_len',
		'char.vstring', 'char.vstring_with_len', 'byteptr.vstring', 'byteptr.vstring_with_len',
		'charptr.vstring', 'charptr.vstring_with_len', 'u8.vstring_literal',
		'u8.vstring_literal_with_len', 'char.vstring_literal', 'char.vstring_literal_with_len',
		'byteptr.vstring_literal', 'byteptr.vstring_literal_with_len', 'charptr.vstring_literal',
		'charptr.vstring_literal_with_len', 'prealloc_malloc', 'prealloc_malloc_align',
		'prealloc_calloc', 'prealloc_realloc', 'prealloc_scope_begin', 'v_realloc', 'map_map_eq',
		'v3_string_zpad', 'v3_f64_zpad', 'v3_int_zpad', 'v3_i64_zpad', 'v3_u64_zpad', 'v3_heap_array',
		'v3_aligned_memdup', 'v3_string_upper_ascii', 'v3_string_plus_sign', 'v3_string_rpad_zero',
		'v3_f64_fixed', 'v3_f64_exp', 'v3_f64_general', 'v3_f64_trimmed', 'v3_char_string', 'v3_map_str',
		'v3_map_str_piece'] {
		return true
	}
	return name.starts_with('print_backtrace') || name.starts_with('backtrace_')
		|| name.starts_with('map.')
		|| name in ['map_map_eq', 'map_map_ne', 'VMapData.free']
		|| name in ['print_libbacktrace', 'eprint_libbacktrace', 'bsd_backtrace_resolve_atos']
}

// skip_source_fn_in_module supports skip source fn in module handling for Builder.
fn (b &Builder) skip_source_fn_in_module(name string, module_name string) bool {
	if module_name in ['bench', 'v.bench']
		&& ssa_fn_name_in_module(module_name, name) in bench_runtime_stub_names {
		return true
	}
	if module_name == 'builtin' && name.starts_with('VMapData.') {
		// The native map stubs replace the C runtime's copy-on-write implementation.
		return true
	}
	if module_name == 'builtin' {
		return name in b.c_fn_ids || b.skip_source_fn(name)
			|| name in ['int_str', 'bool_str', 'string.int', 'string__int', 'array_slice',
				'array__clone', 'array_push_many_ptr', 'IError.msg', 'IError.code', 'Array_str',
				'error', 'error_with_code', '__v3_array_index_string', '__v3_array_index_int',
				'__v3_array_contains_string', '__v3_array_contains_int',
				'__v3_fixed_array_contains_string', '__v3_fixed_array_contains_int', 'v3_string_pad',
				'v3_char_string', 'v3_f64_fixed', 'v3_int_zpad', 'v3_i64_zpad', 'v3_u64_zpad']
			|| name.starts_with('v3_array_sort_')
	}
	if module_name in ['ast', 'v.ast']
		&& name in ['Expr.name', 'SelectorExpr.name', '[]Expr.name_list'] {
		return true
	}
	if module_name in ['c', 'v.gen.c'] && name.starts_with('Gen.') {
		return true
	}
	if module_name == 'strings' {
		return b.skip_source_fn(ssa_fn_name_in_module(module_name, name))
			|| name.starts_with('Builder.') && b.skip_source_fn(name)
			|| name == 'Builder.push_many'
	}
	if module_name == 'os' {
		return name in ['is_dir', 'is_link', 'ls', 'normalize_path_in_builder', 'check_fwrite',
			'check_fread', 'join_path_single']
	}
	if module_name == 'strconv' {
		return name in ['f32_to_str_l', 'f32_to_str_l_with_dot', 'f64_to_str_l', 'f64_to_str_l_with_dot']
	}
	if module_name == 'rand' {
		return name in ['PRNG.seed', 'PRNG.free', 'PRNG.u8', 'PRNG.u16', 'PRNG.u32', 'PRNG.u64',
			'PRNG.block_size']
	}
	if module_name == 'embed_file' {
		return name == 'Decoder.decompress'
	}
	return false
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
	if wymix_id := b.register_runtime_c_function('_wymix', b.i64_type, p2) {
		b.generate_wymix_body(wymix_id)
	}
	if wyhash64_id := b.register_runtime_c_function('wyhash64', b.i64_type, p2) {
		b.generate_wyhash64_body(wyhash64_id)
	}

	ptr_u8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)
	mut p4 := []TypeID{}
	p4 << ptr_u8
	p4 << b.i64_type
	p4 << b.i64_type
	p4 << ptr_u64
	if wyhash_id := b.register_runtime_c_function('wyhash', b.i64_type, p4) {
		b.generate_wyhash_body(wyhash_id)
	}
}

// register_runtime_function keeps Wasm source and runtime names independent.
fn (mut b Builder) register_runtime_function(name string, ret TypeID, params []TypeID) ?int {
	if b.m.target.ptr_size == 8 { return b.register_synthetic_function(name, ret, params) }
	if id := b.runtime_fn_ids[name] {
		if !b.m.funcs[id].is_c_extern && b.m.funcs[id].blocks.len > 0 {
			return none
		}
	}
	// Preserve selected source signatures and bodies while keeping an independent
	// implementation for compiler-generated runtime calls with the same spelling.
	mut synthetic_name := name
	if b.source_fn_names[name] {
		synthetic_name = '__ssa_runtime_${name}'
		for (synthetic_name in b.fn_ids) {
			synthetic_name += '_'
		}
	}
	fn_type := b.m.type_store.register(Type{
		kind:     .func_t
		ret_type: ret
		params:   params
	})
	b.fn_types[synthetic_name] = fn_type
	func_id := b.m.new_function(synthetic_name, ret)
	b.fn_ids[synthetic_name] = func_id
	b.runtime_fn_ids[name] = func_id
	b.runtime_fn_types[name] = fn_type
	mut f := b.m.funcs[func_id]
	f.typ = ret
	f.blocks = []BlockID{}
	f.params = []ValueID{}
	f.is_c_extern = false
	b.m.funcs[func_id] = f
	return func_id
}

fn (mut b Builder) register_runtime_c_function(name string, ret TypeID, params []TypeID) ?int {
	if b.m.target.ptr_size == 8 { return b.register_synthetic_c_function(name, ret, params) }
	// Keep the C implementation separate when a V function owns the bare name.
	synthetic_name := if b.source_fn_names[name] { 'C.${name}' } else { name }
	func_id := b.register_runtime_function(synthetic_name, ret, params)?
	fn_type := b.runtime_fn_types[synthetic_name]
	extern_name := 'C.${name}'
	b.fn_ids[extern_name] = func_id
	b.fn_types[extern_name] = fn_type
	b.c_fn_ids[name] = func_id
	b.c_fn_types[name] = fn_type
	b.runtime_fn_ids[name] = func_id
	b.runtime_fn_types[name] = fn_type
	return func_id
}

// runtime_fn_lookup resolves compiler-generated calls independently of source names.
fn (b &Builder) runtime_fn_lookup(name string) ?int {
	if b.m.target.ptr_size == 8 { return b.fn_ids[name] or { none } }
	if id := b.runtime_fn_ids[name] {
		return id
	}
	if id := b.c_fn_ids[name] {
		return id
	}
	return b.fn_ids[name] or { none }
}

fn (b &Builder) runtime_fn_id(name string) int {
	return b.runtime_fn_lookup(name) or { 0 }
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
	ops << if op == .store { b.coerce_scalar_store_value(a, c, block_id) } else { a }
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
	if b.m.target.ptr_size == 8 {
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
			builtin_field_type = b.m.type_store.types[typ_id].fields[field_idx]
			builtin_offset = if b.m.target.ptr_size == 8 {
				b.m.struct_field_offset(typ_id, field_idx)
			} else {
				field_idx * 8
			}
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
	if tos2_id := b.register_runtime_function('tos2', b.str_type, p1_ptr) {
		b.generate_tos2_body(tos2_id, 0)
	}
	if tos3_id := b.register_runtime_function('tos3', b.str_type, p1_ptr) {
		b.generate_tos2_body(tos3_id, 0)
	}
	if tos_clone_id := b.register_runtime_function('tos_clone', b.str_type, p1_ptr) {
		b.generate_tos_clone_body(tos_clone_id)
	}
	b.register_pointer_string_stubs()

	mut p1_i64 := []TypeID{}
	p1_i64 << b.i64_type
	if int_str_id := b.register_runtime_function('int_str', b.str_type, p1_i64) {
		b.generate_int_format_body(int_str_id, true, false)
	}

	mut p1_i1 := []TypeID{}
	p1_i1 << b.i1_type
	if bool_str_id := b.register_runtime_function('bool_str', b.str_type, p1_i1) {
		b.generate_bool_str_body(bool_str_id)
	}

	mut p1_string := []TypeID{}
	p1_string << b.str_type
	if string_int_id := b.register_runtime_function('string.int', b.i64_type, p1_string) {
		b.generate_string_int_body(string_int_id)
	}
	if string_int_c_id := b.register_runtime_function('string__int', b.i64_type, p1_string) {
		b.generate_string_int_body(string_int_c_id)
	}

	mut p2_i64 := []TypeID{}
	p2_i64 << b.i64_type
	p2_i64 << b.i64_type
	if format_int_id := b.register_runtime_function('strconv__format_int', b.str_type, p2_i64) {
		b.generate_int_format_body(format_int_id, true, true)
	}
	if format_uint_id := b.register_runtime_function('strconv__format_uint', b.str_type, p2_i64) {
		b.generate_int_format_body(format_uint_id, false, true)
	}

	b.register_v3_string_format_stubs()
	if b.m.target.ptr_size == 4 {
		for name in ['strconv__f32_to_str_l', 'strconv__f32_to_str_l_with_dot', 'f32_to_str_l',
			'f32_to_str_l_with_dot', 'strconv__f64_to_str_l', 'strconv__f64_to_str_l_with_dot',
			'f64_to_str_l', 'f64_to_str_l_with_dot'] {
			if float_str_id := b.register_runtime_function(name, b.str_type, p1_i64) {
				b.generate_const_string_body(float_str_id, '0.0')
			}
		}
	}
}

// generate_plus_sign_body prepends '+' to a numeric string that carries no sign yet,
// satisfying the `+` interpolation flag. It mirrors generate_native_plus_sign_body but
// builds the concatenation itself, because the wasm stub set registers `string__plus`
// only after this fragment runs.
fn (mut b Builder) generate_plus_sign_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'plus_sign_entry')
	s := b.func_add_argument(func_id, b.str_type, 's')
	s_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.str_type))
	b.block_instr2(.store, entry, b.void_type, s, s_slot)
	len_ptr := b.block_struct_field_ptr(entry, s_slot, b.str_type, 1)
	data_ptr := b.block_struct_field_ptr(entry, s_slot, b.str_type, 0)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	nonempty := b.block_instr2(.gt, entry, b.i1_type, len, zero)
	check_sign := b.m.add_block(func_id, 'plus_sign_check')
	append := b.m.add_block(func_id, 'plus_sign_add')
	b.block_instr3(.br, entry, b.void_type, nonempty, ValueID(check_sign), ValueID(append))
	first := b.block_instr1(.load, check_sign, b.i8_type, data)
	minus := b.m.get_or_add_const(b.i8_type, '45')
	plus := b.m.get_or_add_const(b.i8_type, '43')
	is_minus := b.block_instr2(.eq, check_sign, b.i1_type, first, minus)
	is_plus := b.block_instr2(.eq, check_sign, b.i1_type, first, plus)
	has_sign := b.block_instr2(.or_, check_sign, b.i1_type, is_minus, is_plus)
	original := b.m.add_block(func_id, 'plus_sign_original')
	b.block_instr3(.br, check_sign, b.void_type, has_sign, ValueID(original), ValueID(append))
	b.block_instr1(.ret, original, b.void_type, s)
	size := b.block_instr2(.add, append, b.i64_type, len, one)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out := b.block_instr2(.call, append, ptr_i8, malloc_ref, size)
	b.block_instr2(.store, append, b.void_type, plus, out)
	dest := b.block_instr2(.add, append, ptr_i8, out, one)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, append, ptr_i8, memcpy_ref, dest, data, len)
	nul := b.m.get_or_add_const(b.i8_type, '0')
	term := b.block_instr2(.add, append, ptr_i8, out, size)
	b.block_instr2(.store, append, b.void_type, nul, term)
	result := b.emit_make_string(append, out, size, 0)
	b.block_instr1(.ret, append, b.void_type, result)
}

// register_v3_string_format_stubs registers helper calls emitted by the string-interpolation
// transformer. The C backend provides these as inline C helpers; the SSA/native path needs
// synthetic functions so self-hosted ARM64 builds can lower the transformed calls.
fn (mut b Builder) register_v3_string_format_stubs() {
	mut p_pad := []TypeID{}
	p_pad << b.str_type
	p_pad << b.i32_type
	p_pad << b.i32_type
	if b.m.target.ptr_size == 4 {
		if pad_id := b.register_runtime_function('v3_string_pad', b.str_type, p_pad) {
			b.generate_string_pad_body(pad_id)
		}

		mut p_char := []TypeID{}
		p_char << b.i32_type
		if char_id := b.register_runtime_function('v3_char_string', b.str_type, p_char) {
			b.generate_char_string_body(char_id)
		}

		if plus_id := b.register_runtime_function('v3_string_plus_sign', b.str_type, [
			b.str_type,
		]) {
			b.generate_plus_sign_body(plus_id)
		}

		mut p_f64 := []TypeID{}
		p_f64 << b.f64_type
		p_f64 << b.i32_type
		if fixed_id := b.register_runtime_function('v3_f64_fixed', b.str_type, p_f64) {
			// Keep the unsupported helper bodyless so Wasm rejects it instead of
			// formatting every input as a constant. Source functions with the same
			// name retain their independent bodies.
			b.m.funcs[fixed_id].is_prototype = true
		}

		mut p_int := []TypeID{}
		p_int << b.i32_type
		p_int << b.i32_type
		if int_zpad_id := b.register_runtime_function('v3_int_zpad', b.str_type, p_int) {
			b.generate_int_zpad_body(int_zpad_id, b.i32_type, false)
		}

		mut p_i64 := []TypeID{}
		p_i64 << b.i64_type
		p_i64 << b.i32_type
		if i64_zpad_id := b.register_runtime_function('v3_i64_zpad', b.str_type, p_i64) {
			b.generate_int_zpad_body(i64_zpad_id, b.i64_type, false)
		}

		mut p_u64 := []TypeID{}
		p_u64 << b.u64_type
		p_u64 << b.i32_type
		if u64_zpad_id := b.register_runtime_function('v3_u64_zpad', b.str_type, p_u64) {
			b.generate_int_zpad_body(u64_zpad_id, b.u64_type, true)
		}
	} else {
		pad_id := b.register_synthetic_function('v3_string_pad', b.str_type, p_pad)
		b.generate_string_pad_body(pad_id)
	}
}

// register_pointer_string_stubs updates register pointer string stubs state for ssa.
fn (mut b Builder) register_pointer_string_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_i8
	for name in ['u8.vstring', 'char.vstring', 'byteptr.vstring', 'charptr.vstring',
		'u8.vstring_literal', 'char.vstring_literal', 'byteptr.vstring_literal',
		'charptr.vstring_literal'] {
		if func_id := b.register_runtime_function(name, b.str_type, p1_ptr) {
			// The `_literal` wrappers borrow static C data (is_lit=1, never freed); the plain
			// ones return a non-copying view of a runtime buffer (is_lit=0), matching V.
			b.generate_tos2_body(func_id, if name.contains('_literal') { 1 } else { 0 })
		}
	}

	mut p2_ptr_len := []TypeID{}
	p2_ptr_len << ptr_i8
	p2_ptr_len << b.i64_type
	for name in ['u8.vstring_with_len', 'char.vstring_with_len', 'byteptr.vstring_with_len',
		'charptr.vstring_with_len', 'u8.vstring_literal_with_len', 'char.vstring_literal_with_len',
		'byteptr.vstring_literal_with_len', 'charptr.vstring_literal_with_len'] {
		if func_id := b.register_runtime_function(name, b.str_type, p2_ptr_len) {
			b.generate_vstring_with_len_body(func_id, if name.contains('_literal') { 1 } else { 0 })
		}
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
	mut radix := b.m.get_or_add_const(b.i64_type, '10')
	if has_radix {
		radix = b.func_add_argument(func_id, b.i64_type, 'radix')
	}

	alloca_n := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_len := b.block_instr0(.alloca, entry, ptr_i64)
	alloca_pos := b.block_instr0(.alloca, entry, ptr_ptr_i8)
	alloca_neg := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.i1_type))

	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	// 64 binary digits, an optional sign and the terminator.
	buf_size := b.m.get_or_add_const(b.i64_type, '72')
	last_off := b.m.get_or_add_const(b.i64_type, '71')
	minus_one := b.m.get_or_add_const(b.i64_type, '-1')
	ascii_zero := b.m.get_or_add_const(b.i64_type, '48')
	ascii_alpha_lower := b.m.get_or_add_const(b.i64_type, '87')
	ascii_minus := b.m.get_or_add_const(b.i64_type, '45')
	ten := b.m.get_or_add_const(b.i64_type, '10')
	false_val := b.m.get_or_add_const(b.i1_type, '0')
	true_val := b.m.get_or_add_const(b.i1_type, '1')

	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
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
	// The magnitude of the minimum signed integer occupies the unsigned half of i64.
	rem := b.block_instr2(.urem, loop_block, b.i64_type, n_cur, radix)
	numeric_digit := b.block_instr2(.add, loop_block, b.i64_type, rem, ascii_zero)
	alpha_digit := b.block_instr2(.add, loop_block, b.i64_type, rem, ascii_alpha_lower)
	is_numeric_digit := b.block_instr2(.lt, loop_block, b.i1_type, rem, ten)
	digit := b.block_instr3(.select, loop_block, b.i64_type, is_numeric_digit, numeric_digit,
		alpha_digit)
	pos := b.block_instr1(.load, loop_block, ptr_i8, alloca_pos)
	pos_next := b.block_instr2(.add, loop_block, ptr_i8, pos, minus_one)
	b.block_instr2(.store, loop_block, b.void_type, digit, pos_next)
	b.block_instr2(.store, loop_block, b.void_type, pos_next, alloca_pos)
	old_len := b.block_instr1(.load, loop_block, b.i64_type, alloca_len)
	new_len := b.block_instr2(.add, loop_block, b.i64_type, old_len, one)
	b.block_instr2(.store, loop_block, b.void_type, new_len, alloca_len)
	quot := b.block_instr2(.udiv, loop_block, b.i64_type, n_cur, radix)
	b.block_instr2(.store, loop_block, b.void_type, quot, alloca_n)
	has_more := b.block_instr2(.ne, loop_block, b.i1_type, quot, zero)
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
	memmove_ref := b.m.add_value(.func_ref, b.void_type, 'memmove', b.runtime_fn_id('memmove'))
	b.block_instr4(.call, done_block, ptr_i8, memmove_ref, buf, data, len)
	buf_term := b.block_instr2(.add, done_block, ptr_i8, buf, len)
	b.block_instr2(.store, done_block, b.void_type, nul, buf_term)
	result := b.emit_make_string(done_block, buf, len, 0)
	b.block_instr1(.ret, done_block, b.void_type, result)
}

// generate_string_int_body supports generate string int body handling for Builder.
//
// It mirrors strconv.common_parse_int as driven by builtin.string_int_base, so a
// wasm build parses what a native `s.int()` does: an optional sign, a base chosen
// from a 0x/0b/0o prefix, digits validated against that base, and the 2^31-1 clamp.
// A byte the base cannot use stops the parse and keeps the value accumulated so far,
// which is the partial result common_parse_uint2 returns on a bad digit. An illicit
// underscore instead yields 0, matching the same rule in the reference.
fn (mut b Builder) generate_string_int_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, b.str_type, 's')
	s_slot := b.block_instr0(.alloca, entry, ptr_string)
	result_slot := b.block_instr0(.alloca, entry, ptr_i64)
	i_slot := b.block_instr0(.alloca, entry, ptr_i64)
	base_slot := b.block_instr0(.alloca, entry, ptr_i64)
	sign_slot := b.block_instr0(.alloca, entry, ptr_i64)
	digits_slot := b.block_instr0(.alloca, entry, ptr_i64)
	b.block_instr2(.store, entry, b.void_type, s, s_slot)

	data_ptr := b.block_struct_field_ptr(entry, s_slot, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, s_slot, b.str_type, 1)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)

	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	two64 := b.m.get_or_add_const(b.i64_type, '2')
	three64 := b.m.get_or_add_const(b.i64_type, '3')
	eight64 := b.m.get_or_add_const(b.i64_type, '8')
	nine64 := b.m.get_or_add_const(b.i64_type, '9')
	ten64 := b.m.get_or_add_const(b.i64_type, '10')
	sixteen64 := b.m.get_or_add_const(b.i64_type, '16')
	minus_one := b.m.get_or_add_const(b.i64_type, '-1')
	upper_adjust := b.m.get_or_add_const(b.i64_type, '55')
	lower_adjust := b.m.get_or_add_const(b.i64_type, '87')
	ascii_zero_i64 := b.m.get_or_add_const(b.i64_type, '48')
	max_i32 := b.m.get_or_add_const(b.i64_type, '2147483647')
	min_i32 := b.m.get_or_add_const(b.i64_type, '-2147483648')
	max_u32 := b.m.get_or_add_const(b.i64_type, '4294967295')
	ascii_minus := b.m.get_or_add_const(b.i8_type, '45')
	ascii_plus := b.m.get_or_add_const(b.i8_type, '43')
	ascii_underscore := b.m.get_or_add_const(b.i8_type, '95')
	// The folded prefix letters are upper case, so they compare against the uppercase
	// codes; clearing bit 5 with the mask above is what gets the input there.
	ascii_zero8 := b.m.get_or_add_const(b.i8_type, '48')
	ascii_x8 := b.m.get_or_add_const(b.i8_type, '88')
	ascii_b8 := b.m.get_or_add_const(b.i8_type, '66')
	ascii_o8 := b.m.get_or_add_const(b.i8_type, '79')
	// Clearing bit 5 folds lower case onto upper, so one comparison covers `x`, `b`
	// and `o` however the input spells them.
	case_mask := b.m.get_or_add_const(b.i8_type, '223')

	b.block_instr2(.store, entry, b.void_type, zero64, result_slot)
	b.block_instr2(.store, entry, b.void_type, zero64, i_slot)
	b.block_instr2(.store, entry, b.void_type, zero64, digits_slot)
	b.block_instr2(.store, entry, b.void_type, ten64, base_slot)
	b.block_instr2(.store, entry, b.void_type, one64, sign_slot)

	check_sign := b.m.add_block(func_id, 'string_int_check_sign')
	prefix := b.m.add_block(func_id, 'string_int_prefix')
	prefix_read := b.m.add_block(func_id, 'string_int_prefix_read')
	prefix_take := b.m.add_block(func_id, 'string_int_prefix_take')
	prefix_base := b.m.add_block(func_id, 'string_int_prefix_base')
	prefix_octal := b.m.add_block(func_id, 'string_int_prefix_octal')
	loop := b.m.add_block(func_id, 'string_int_loop')
	body := b.m.add_block(func_id, 'string_int_body')
	underscore := b.m.add_block(func_id, 'string_int_underscore')
	underscore_pair := b.m.add_block(func_id, 'string_int_underscore_pair')
	digit := b.m.add_block(func_id, 'string_int_digit')
	accumulate := b.m.add_block(func_id, 'string_int_accumulate')
	step := b.m.add_block(func_id, 'string_int_step')
	stop_zero := b.m.add_block(func_id, 'string_int_stop_zero')
	stop := b.m.add_block(func_id, 'string_int_stop')

	has_chars := b.block_instr2(.gt, entry, b.i1_type, len, zero64)
	b.block_instr3(.br, entry, b.void_type, has_chars, ValueID(check_sign), ValueID(stop))

	// A sign is skipped for the prefix scan as well as the digits, so the parse never
	// has to remember which of the two it started at.
	first_ch := b.block_instr1(.load, check_sign, b.i8_type, data)
	is_minus := b.block_instr2(.eq, check_sign, b.i1_type, first_ch, ascii_minus)
	is_plus := b.block_instr2(.eq, check_sign, b.i1_type, first_ch, ascii_plus)
	signed := b.block_instr2(.or_, check_sign, b.i1_type, is_minus, is_plus)
	negative := b.block_instr3(.select, check_sign, b.i64_type, is_minus, minus_one,
		one64)
	start_one := b.block_instr3(.select, check_sign, b.i64_type, signed, one64,
		zero64)
	b.block_instr2(.store, check_sign, b.void_type, start_one, i_slot)
	b.block_instr2(.store, check_sign, b.void_type, negative, sign_slot)
	b.block_instr1(.jmp, check_sign, b.void_type, ValueID(prefix))

	// string_int_base only reports a prefix when two bytes follow the sign, so a
	// shorter tail is a plain decimal number.
	start := b.block_instr1(.load, prefix, b.i64_type, i_slot)
	rest := b.block_instr2(.sub, prefix, b.i64_type, len, start)
	has_rest2 := b.block_instr2(.ge, prefix, b.i1_type, rest, two64)
	b.block_instr3(.br, prefix, b.void_type, has_rest2, ValueID(prefix_read), ValueID(loop))

	start_ptr := b.block_instr2(.add, prefix_read, ptr_i8, data, start)
	zero_ch := b.block_instr1(.load, prefix_read, b.i8_type, start_ptr)
	is_zero := b.block_instr2(.eq, prefix_read, b.i1_type, zero_ch, ascii_zero8)
	one_later := b.block_instr2(.add, prefix_read, b.i64_type, start, one64)
	second_ptr := b.block_instr2(.add, prefix_read, ptr_i8, data, one_later)
	second_ch := b.block_instr1(.load, prefix_read, b.i8_type, second_ptr)
	folded_ch := b.block_instr2(.and_, prefix_read, b.i8_type, second_ch, case_mask)
	is_hex := b.block_instr2(.eq, prefix_read, b.i1_type, folded_ch, ascii_x8)
	is_bin := b.block_instr2(.eq, prefix_read, b.i1_type, folded_ch, ascii_b8)
	is_oct := b.block_instr2(.eq, prefix_read, b.i1_type, folded_ch, ascii_o8)
	hex_or_bin := b.block_instr2(.or_, prefix_read, b.i1_type, is_hex, is_bin)
	hex_or_oct := b.block_instr2(.or_, prefix_read, b.i1_type, hex_or_bin, is_oct)
	has_prefix := b.block_instr2(.and_, prefix_read, b.i1_type, is_zero, hex_or_oct)
	b.block_instr3(.br, prefix_read, b.void_type, has_prefix, ValueID(prefix_take),
		ValueID(loop))

	// common_parse_uint2 needs three bytes to read a prefix, so "0x" on its own falls
	// back to base 8 and stops on the `x` as a bad digit.
	has_rest3 := b.block_instr2(.ge, prefix_take, b.i1_type, rest, three64)
	b.block_instr3(.br, prefix_take, b.void_type, has_rest3, ValueID(prefix_base),
		ValueID(prefix_octal))

	b.block_instr2(.store, prefix_octal, b.void_type, eight64, base_slot)
	b.block_instr2(.store, prefix_octal, b.void_type, start, digits_slot)
	b.block_instr1(.jmp, prefix_octal, b.void_type, ValueID(loop))

	// Two nested selects cover the three prefixes: the inner one splits 0b from 0o,
	// and the outer one lifts 0x out of both.
	other_base := b.block_instr3(.select, prefix_base, b.i64_type, is_bin, two64, eight64)
	chosen_base := b.block_instr3(.select, prefix_base, b.i64_type, is_hex, sixteen64,
		other_base)
	b.block_instr2(.store, prefix_base, b.void_type, chosen_base, base_slot)
	prefix_end := b.block_instr2(.add, prefix_base, b.i64_type, start, two64)
	end_ptr := b.block_instr2(.add, prefix_base, ptr_i8, data, prefix_end)
	end_ch := b.block_instr1(.load, prefix_base, b.i8_type, end_ptr)
	after_us := b.block_instr2(.eq, prefix_base, b.i1_type, end_ch, ascii_underscore)
	skipped := b.block_instr2(.add, prefix_base, b.i64_type, prefix_end, one64)
	digits := b.block_instr3(.select, prefix_base, b.i64_type, after_us, skipped,
		prefix_end)
	b.block_instr2(.store, prefix_base, b.void_type, digits, i_slot)
	b.block_instr2(.store, prefix_base, b.void_type, digits, digits_slot)
	b.block_instr1(.jmp, prefix_base, b.void_type, ValueID(loop))

	i := b.block_instr1(.load, loop, b.i64_type, i_slot)
	more := b.block_instr2(.lt, loop, b.i1_type, i, len)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(stop))

	body_i := b.block_instr1(.load, body, b.i64_type, i_slot)
	ch_ptr := b.block_instr2(.add, body, ptr_i8, data, body_i)
	ch := b.block_instr1(.load, body, b.i8_type, ch_ptr)
	is_us := b.block_instr2(.eq, body, b.i1_type, ch, ascii_underscore)
	b.block_instr3(.br, body, b.void_type, is_us, ValueID(underscore), ValueID(digit))

	// A `_` separates digits but may not lead or trail the number, sit directly
	// after the prefix, or double up; each of those is a syntax error, so the whole
	// value collapses to 0 rather than to a partial parse.
	digits_start := b.block_instr1(.load, underscore, b.i64_type, digits_slot)
	at_start := b.block_instr2(.eq, underscore, b.i1_type, body_i, digits_start)
	last := b.block_instr2(.sub, underscore, b.i64_type, len, one64)
	at_end := b.block_instr2(.ge, underscore, b.i1_type, body_i, last)
	at_edge := b.block_instr2(.or_, underscore, b.i1_type, at_start, at_end)
	b.block_instr3(.br, underscore, b.void_type, at_edge, ValueID(stop_zero), ValueID(underscore_pair))

	prev_i := b.block_instr2(.sub, underscore_pair, b.i64_type, body_i, one64)
	prev_ptr := b.block_instr2(.add, underscore_pair, ptr_i8, data, prev_i)
	prev_ch := b.block_instr1(.load, underscore_pair, b.i8_type, prev_ptr)
	prev_us := b.block_instr2(.eq, underscore_pair, b.i1_type, prev_ch, ascii_underscore)
	next_i := b.block_instr2(.add, underscore_pair, b.i64_type, body_i, one64)
	next_ptr := b.block_instr2(.add, underscore_pair, ptr_i8, data, next_i)
	next_ch := b.block_instr1(.load, underscore_pair, b.i8_type, next_ptr)
	next_us := b.block_instr2(.eq, underscore_pair, b.i1_type, next_ch, ascii_underscore)
	doubled := b.block_instr2(.or_, underscore_pair, b.i1_type, prev_us, next_us)
	b.block_instr3(.br, underscore_pair, b.void_type, doubled, ValueID(stop_zero),
		ValueID(step))

	// Fold the byte onto its digit value before comparing it with the base, because
	// `A`-`F` sit 7 above the decimal digits and `a`-`f` 32 above those.
	ch64 := b.block_instr1(.zext, digit, b.i64_type, ch)
	is_upper_af := b.u8_in_range(digit, ch, `A`, `F`)
	is_lower_af := b.u8_in_range(digit, ch, `a`, `f`)
	is_letter := b.block_instr2(.or_, digit, b.i1_type, is_upper_af, is_lower_af)
	letter_adjust := b.block_instr3(.select, digit, b.i64_type, is_upper_af, upper_adjust,
		lower_adjust)
	digit_adjust := b.block_instr3(.select, digit, b.i64_type, is_letter, letter_adjust,
		ascii_zero_i64)
	folded := b.block_instr2(.sub, digit, b.i64_type, ch64, digit_adjust)
	base := b.block_instr1(.load, digit, b.i64_type, base_slot)
	below_base := b.block_instr2(.lt, digit, b.i1_type, folded, base)
	not_negative := b.block_instr2(.ge, digit, b.i1_type, folded, zero64)
	in_base := b.block_instr2(.and_, digit, b.i1_type, below_base, not_negative)
	// common_parse_uint2 rejects a bare 10-15, which is what `:` through `?` fold to,
	// unless it came from a letter.
	small := b.block_instr2(.le, digit, b.i1_type, folded, nine64)
	letter_or_small := b.block_instr2(.or_, digit, b.i1_type, is_letter, small)
	valid := b.block_instr2(.and_, digit, b.i1_type, in_base, letter_or_small)
	b.block_instr3(.br, digit, b.void_type, valid, ValueID(accumulate), ValueID(stop))

	cur := b.block_instr1(.load, accumulate, b.i64_type, result_slot)
	scale_base := b.block_instr1(.load, accumulate, b.i64_type, base_slot)
	scaled := b.block_instr2(.mul, accumulate, b.i64_type, cur, scale_base)
	accumulated := b.block_instr2(.add, accumulate, b.i64_type, scaled, folded)
	// The reference stops immediately on u32 overflow, before inspecting any suffix.
	// Keeping this bound also prevents the next multiply/add from overflowing i64.
	overflow := b.block_instr2(.gt, accumulate, b.i1_type, accumulated, max_u32)
	bounded := b.block_instr3(.select, accumulate, b.i64_type, overflow, max_u32, accumulated)
	b.block_instr2(.store, accumulate, b.void_type, bounded, result_slot)
	b.block_instr3(.br, accumulate, b.void_type, overflow, ValueID(stop), ValueID(step))

	step_next_i := b.block_instr2(.add, step, b.i64_type, body_i, one64)
	b.block_instr2(.store, step, b.void_type, step_next_i, i_slot)
	b.block_instr1(.jmp, step, b.void_type, ValueID(loop))

	b.block_instr2(.store, stop_zero, b.void_type, zero64, result_slot)
	b.block_instr1(.jmp, stop_zero, b.void_type, ValueID(stop))

	// common_parse_int clamps at 2^31 rather than wrapping or erroring, so the range
	// check happens on the signed value.
	parsed := b.block_instr1(.load, stop, b.i64_type, result_slot)
	sign := b.block_instr1(.load, stop, b.i64_type, sign_slot)
	signed_val := b.block_instr2(.mul, stop, b.i64_type, parsed, sign)
	too_big := b.block_instr2(.gt, stop, b.i1_type, signed_val, max_i32)
	too_small := b.block_instr2(.lt, stop, b.i1_type, signed_val, min_i32)
	high := b.block_instr3(.select, stop, b.i64_type, too_big, max_i32, signed_val)
	clamped := b.block_instr3(.select, stop, b.i64_type, too_small, min_i32, high)
	b.block_instr1(.ret, stop, b.void_type, clamped)
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
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out_data := b.block_instr2(.call, pad_block, ptr_i8, malloc_ref, alloc_len)
	left := b.block_instr1(.load, pad_block, b.i32_type, left_slot)
	left_aligned := b.block_instr2(.ne, pad_block, b.i1_type, left, zero32)
	copy_left := b.m.add_block(func_id, 'string_pad_copy_left')
	copy_right := b.m.add_block(func_id, 'string_pad_copy_right')
	done := b.m.add_block(func_id, 'string_pad_done')
	b.block_instr3(.br, pad_block, b.void_type, left_aligned, ValueID(copy_left),
		ValueID(copy_right))

	memcpy_ref_left := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, copy_left, ptr_i8, memcpy_ref_left, out_data, data, len64)
	left_pad_dest := b.block_instr2(.add, copy_left, ptr_i8, out_data, len64)
	memset_ref_left := b.m.add_value(.func_ref, b.void_type, 'memset', b.runtime_fn_id('memset'))
	space64 := b.m.get_or_add_const(b.i64_type, '32')
	b.block_instr4(.call, copy_left, ptr_i8, memset_ref_left, left_pad_dest, space64, pad_len64)
	b.block_instr1(.jmp, copy_left, b.void_type, ValueID(done))

	memset_ref_right := b.m.add_value(.func_ref, b.void_type, 'memset', b.runtime_fn_id('memset'))
	b.block_instr4(.call, copy_right, ptr_i8, memset_ref_right, out_data, space64, pad_len64)
	right_text_dest := b.block_instr2(.add, copy_right, ptr_i8, out_data, pad_len64)
	memcpy_ref_right := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, copy_right, ptr_i8, memcpy_ref_right, right_text_dest, data, len64)
	b.block_instr1(.jmp, copy_right, b.void_type, ValueID(done))

	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, done, ptr_i8, out_data, width64)
	b.block_instr2(.store, done, b.void_type, zero8, term_ptr)
	result := b.emit_make_string(done, out_data, width64, 0)
	b.block_instr1(.ret, done, b.void_type, result)
}

// generate_int_zpad_body zero-pads an integer's decimal text to `width`, keeping a
// leading sign outside the zeros the way `${x:05}` does natively: `-7` at width 6
// prints `-00007`. The width was previously discarded, so the helper returned the
// unpadded text, and the unsigned spelling returned the constant `0`.
fn (mut b Builder) generate_int_zpad_body(func_id int, value_type TypeID, is_unsigned bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_string := b.m.type_store.get_ptr(b.str_type)

	entry := b.m.add_block(func_id, 'int_zpad_entry')
	value := b.func_add_argument(func_id, value_type, 'value')
	width_arg := b.func_add_argument(func_id, b.i32_type, 'width')
	text_slot := b.block_instr0(.alloca, entry, ptr_string)
	width_slot := b.block_instr0(.alloca, entry, ptr_i32)
	sign_slot := b.block_instr0(.alloca, entry, ptr_i64)

	zero32 := b.m.get_or_add_const(b.i32_type, '0')
	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	radix := b.m.get_or_add_const(b.i64_type, '10')
	b.block_instr2(.store, entry, b.void_type, width_arg, width_slot)
	b.block_instr2(.store, entry, b.void_type, zero64, sign_slot)

	widened := if value_type == b.i32_type {
		b.block_instr1(.sext, entry, b.i64_type, value)
	} else {
		value
	}
	// The unsigned spelling must read its bits as bits: `strconv__format_uint`
	// formats without a sign, so `u64::MAX` prints as twenty digits, not a negative.
	text := if is_unsigned {
		uint_ref := b.m.add_value(.func_ref, b.str_type, 'strconv__format_uint',
			b.runtime_fn_id('strconv__format_uint'))
		b.block_instr3(.call, entry, b.str_type, uint_ref, widened, radix)
	} else {
		int_str_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.runtime_fn_id('int_str'))
		b.block_instr2(.call, entry, b.str_type, int_str_ref, widened)
	}
	b.block_instr2(.store, entry, b.void_type, text, text_slot)

	prepare := b.m.add_block(func_id, 'int_zpad_prepare')
	return_original := b.m.add_block(func_id, 'int_zpad_original')
	sign_check := b.m.add_block(func_id, 'int_zpad_check_sign')
	read_sign := b.m.add_block(func_id, 'int_zpad_read_sign')
	pad := b.m.add_block(func_id, 'int_zpad_allocate')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(prepare))

	data_ptr := b.block_struct_field_ptr(prepare, text_slot, b.str_type, 0)
	len_ptr := b.block_struct_field_ptr(prepare, text_slot, b.str_type, 1)
	data := b.block_instr1(.load, prepare, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, prepare, b.i32_type, len_ptr)
	width32 := b.block_instr1(.load, prepare, b.i32_type, width_slot)
	already_wide := b.block_instr2(.ge, prepare, b.i1_type, len32, width32)
	b.block_instr3(.br, prepare, b.void_type, already_wide, ValueID(return_original),
		ValueID(sign_check))
	b.block_instr1(.ret, return_original, b.void_type, text)

	nonempty := b.block_instr2(.gt, sign_check, b.i1_type, len32, zero32)
	b.block_instr3(.br, sign_check, b.void_type, nonempty, ValueID(read_sign), ValueID(pad))
	minus := b.m.get_or_add_const(b.i8_type, '45')
	plus := b.m.get_or_add_const(b.i8_type, '43')
	first := b.block_instr1(.load, read_sign, b.i8_type, data)
	is_minus := b.block_instr2(.eq, read_sign, b.i1_type, first, minus)
	is_plus := b.block_instr2(.eq, read_sign, b.i1_type, first, plus)
	has_sign := b.block_instr2(.or_, read_sign, b.i1_type, is_minus, is_plus)
	sign_len := b.block_instr1(.zext, read_sign, b.i64_type, has_sign)
	b.block_instr2(.store, read_sign, b.void_type, sign_len, sign_slot)
	b.block_instr1(.jmp, read_sign, b.void_type, ValueID(pad))

	sign64 := b.block_instr1(.load, pad, b.i64_type, sign_slot)
	len64 := b.block_instr1(.zext, pad, b.i64_type, len32)
	width64 := b.block_instr1(.zext, pad, b.i64_type, width32)
	pad_len64 := b.block_instr1(.zext, pad, b.i64_type,
		b.block_instr2(.sub, pad, b.i32_type, width32, len32))
	alloc_len := b.block_instr2(.add, pad, b.i64_type, width64, one64)
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out := b.block_instr2(.call, pad, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, pad, ptr_i8, memcpy_ref, out, data, sign64)

	zero_char := b.m.get_or_add_const(b.i64_type, '48')
	pad_dest := b.block_instr2(.add, pad, ptr_i8, out, sign64)
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset', b.runtime_fn_id('memset'))
	b.block_instr4(.call, pad, ptr_i8, memset_ref, pad_dest, zero_char, pad_len64)
	text_dest := b.block_instr2(.add, pad, ptr_i8, pad_dest, pad_len64)
	text_src := b.block_instr2(.add, pad, ptr_i8, data, sign64)
	text_len := b.block_instr2(.sub, pad, b.i64_type, len64, sign64)
	b.block_instr4(.call, pad, ptr_i8, memcpy_ref, text_dest, text_src, text_len)

	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term := b.block_instr2(.add, pad, ptr_i8, out, width64)
	b.block_instr2(.store, pad, b.void_type, zero8, term)
	result := b.emit_make_string(pad, out, width64, 0)
	b.block_instr1(.ret, pad, b.void_type, result)
}

// register_bench_runtime_stubs updates register bench runtime stubs state for ssa.
fn (mut b Builder) register_bench_runtime_stubs() {
	names := if b.m.target.ptr_size == 4 {
		wasm_bench_runtime_stub_names
	} else {
		bench_runtime_stub_names
	}
	for name in names {
		if id := b.register_runtime_function(name, b.i64_type, []TypeID{}) {
			b.generate_const_i64_body(id, '0')
		}
	}
}

// generate_tos2_body supports generate tos2 body handling for Builder. It backs both the
// non-copying `tos2`/`vstring` wrappers (is_lit=0) and the `vstring_literal` wrappers
// (is_lit=1, the borrowed pointer is static C data that must never be freed).
fn (mut b Builder) generate_tos2_body(func_id int, is_lit int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, ptr_i8, 's')
	strlen_ref := b.m.add_value(.func_ref, b.i64_type, 'strlen', b.runtime_fn_id('strlen'))
	len := b.block_instr2(.call, entry, b.i64_type, strlen_ref, s)
	result := b.emit_make_string(entry, s, len, is_lit)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_tos_clone_body supports generate tos clone body handling for Builder.
fn (mut b Builder) generate_tos_clone_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	s := b.func_add_argument(func_id, ptr_i8, 's')
	strlen_ref := b.m.add_value(.func_ref, b.i64_type, 'strlen', b.runtime_fn_id('strlen'))
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
	if plus_id := b.register_runtime_function('string__plus', b.str_type, p2) {
		b.generate_string_plus_body(plus_id)
	}

	mut p_many := []TypeID{}
	p_many << b.i64_type
	p_many << b.m.type_store.get_ptr(b.str_type)
	if many_id := b.register_runtime_function('string_plus_many', b.str_type, p_many) {
		b.generate_string_plus_many_body(many_id)
	}
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
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out_data := b.block_instr2(.call, block_id, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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

	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out_data := b.block_instr2(.call, entry, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref_left := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref_left, out_data, left_data, left_len)
	right_dest := b.block_instr2(.add, entry, ptr_i8, out_data, left_len)
	memcpy_ref_right := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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
	stride := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
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
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
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
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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
	b.register_native_array_growth()

	mut p3 := []TypeID{}
	p3 << b.i64_type
	p3 << b.i64_type
	p3 << b.i64_type
	if array_new_id := b.register_runtime_function('array_new', b.array_type, p3) {
		b.generate_array_new_body(array_new_id)
	}

	// `__new_array_noscan(len, cap, size)` is what the transformer emits for
	// `[]T{}` with scalar elements. It takes len/cap/size where array_new
	// takes size/len/cap, so delegate with reordered arguments.
	mut p_noscan := []TypeID{}
	p_noscan << b.i64_type
	p_noscan << b.i64_type
	p_noscan << b.i64_type
	if array_noscan_id := b.register_runtime_function('__new_array_noscan', b.array_type,
		p_noscan) {
		b.generate_array_noscan_body(array_noscan_id)
	}

	mut p2 := []TypeID{}
	p2 << b.array_type
	p2 << b.i64_type
	if array_get_id := b.register_runtime_function('array_get', ptr_i8, p2) {
		b.generate_array_get_body(array_get_id)
	}

	p3 = []TypeID{}
	p3 << b.array_type
	p3 << b.i64_type
	p3 << b.i64_type
	if array_slice_id := b.register_runtime_function('array_slice', b.array_type, p3) {
		b.generate_array_slice_body(array_slice_id)
	}

	mut p2_array := []TypeID{}
	p2_array << b.array_type
	p2_array << b.array_type
	if array_eq_string_id := b.register_runtime_function('__v3_array_eq_string', b.i1_type, p2_array) {
		b.generate_array_eq_string_body(array_eq_string_id)
	}

	mut p3_array := []TypeID{}
	p3_array << b.array_type
	p3_array << b.array_type
	p3_array << b.i64_type
	if array_eq_raw_id := b.register_runtime_function('__v3_array_eq_raw', b.i1_type, p3_array) {
		b.generate_array_eq_raw_body(array_eq_raw_id)
	}

	mut p3_array_depth := []TypeID{}
	p3_array_depth << b.array_type
	p3_array_depth << b.array_type
	p3_array_depth << b.i32_type
	if array_eq_array_id := b.register_runtime_function('__v3_array_eq_array', b.i1_type, p3_array_depth) {
		b.generate_array_eq_array_body(array_eq_array_id)
	}

	p2 = []TypeID{}
	p2 << ptr_array
	p2 << ptr_i8
	if array_push_id := b.register_runtime_function('array_push', b.void_type, p2) {
		b.generate_array_push_body(array_push_id)
	}

	mut p3_push_many := []TypeID{}
	p3_push_many << ptr_array
	p3_push_many << ptr_i8
	p3_push_many << b.i64_type
	if array_push_many_id := b.register_runtime_function('array.push_many', b.void_type,
		p3_push_many) {
		b.generate_array_push_many_body(array_push_many_id)
	}
	// `array_push_many_ptr` is the C-macro name the transformer emits for
	// `arr << ptr_value` / fixed-array appends; it has the same
	// (ptr_array, ptr_value, count) signature as `array.push_many`. It needs its
	// own definition so the native linker resolves the `_array_push_many_ptr` symbol.
	if array_push_many_ptr_id := b.register_runtime_function('array_push_many_ptr', b.void_type,
		p3_push_many) {
		b.generate_array_push_many_body(array_push_many_ptr_id)
	}

	mut p2_push_many := []TypeID{}
	p2_push_many << ptr_array
	p2_push_many << b.array_type
	if array_push_many_wrapper_id := b.register_runtime_function('array_push_many', b.void_type,
		p2_push_many) {
		b.generate_array_push_many_array_body(array_push_many_wrapper_id)
	}

	mut p1 := []TypeID{}
	p1 << b.array_type
	if array_clone_id := b.register_runtime_function('array_clone', b.array_type, p1) {
		b.generate_array_clone_body(array_clone_id)
	}

	// The transformer lowers `arr.clone()` to `array__clone(&arr)`. Provide the
	// pointer-taking ABI alongside the value-taking synthetic clone.
	mut p1_ptr_arr := []TypeID{}
	p1_ptr_arr << b.m.type_store.get_ptr(b.array_type)
	if array_clone_ptr_id := b.register_runtime_function('array__clone', b.array_type, p1_ptr_arr) {
		b.generate_array_clone_ptr_body(array_clone_ptr_id)
	}
	if reverse_id := b.register_runtime_function('array__reverse_in_place', b.void_type, p1_ptr_arr) {
		b.generate_array_reverse_body(reverse_id)
	}
	mut p1_array := []TypeID{}
	p1_array << b.array_type
	reverse_copy_id := b.register_synthetic_function('array.reverse', b.array_type, p1_array)
	b.generate_array_reverse_copy_body(reverse_copy_id)
	reverse_alias_id := b.register_synthetic_function('array__reverse', b.array_type, p1_array)
	b.generate_array_reverse_copy_body(reverse_alias_id)

	mut p3_repeat := []TypeID{}
	p3_repeat << b.array_type
	p3_repeat << b.i64_type
	p3_repeat << b.i64_type
	if array_repeat_id := b.register_runtime_function('array.repeat_to_depth', b.array_type,
		p3_repeat) {
		b.generate_array_repeat_to_depth_body(array_repeat_id)
	}

	// Integer types only, like the C backend: the transformer lowers a float sort
	// to its stable merge sort.
	for sort_type in ['int', 'i8', 'i16', 'i64', 'u8', 'u16', 'u32', 'u64', 'isize', 'uint', 'rune',
		'char'] {
		elem_type := match sort_type {
			'int', 'rune' { b.i32_type }
			'i8' { b.i8_type }
			'i16' { b.m.type_store.get_int(16) }
			'i64', 'isize' { b.i64_type }
			'u8', 'char' { b.u8_type }
			'u16' { b.u16_type }
			'u32' { b.u32_type }
			else { b.u64_type }
		}
		mut sort_params := []TypeID{}
		sort_params << ptr_array
		if sort_id := b.register_runtime_function('v3_array_sort_${sort_type}', b.void_type,
			sort_params) {
			b.generate_scalar_array_sort_body(sort_id, elem_type)
		}
	}
}

// generate_array_reverse_copy_body clones the source array and reverses the
// clone, preserving value semantics for reverse().
fn (mut b Builder) generate_array_reverse_copy_body(func_id int) {
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	source := b.func_add_argument(func_id, b.array_type, 'source')
	clone_ref := b.m.add_value(.func_ref, b.array_type, 'array_clone',
		b.runtime_fn_id('array_clone'))
	cloned := b.block_instr2(.call, entry, b.array_type, clone_ref, source)
	slot := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, cloned, slot)
	reverse_ref := b.m.add_value(.func_ref, b.void_type, 'array__reverse_in_place',
		b.runtime_fn_id('array__reverse_in_place'))
	b.block_instr2(.call, entry, b.void_type, reverse_ref, slot)
	result := b.block_instr1(.load, entry, b.array_type, slot)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_array_reverse_body reverses an array in place using its runtime
// element size, so the same helper covers scalars, strings and aggregates.
fn (mut b Builder) generate_array_reverse_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, ptr_array, 'arr')
	left_slot := b.block_instr0(.alloca, entry, ptr_i64)
	right_slot := b.block_instr0(.alloca, entry, ptr_i64)
	byte_slot := b.block_instr0(.alloca, entry, ptr_i64)
	data_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 0)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len := b.block_load_array_int_field(entry, arr, 2)
	elem_size := b.block_load_array_int_field(entry, arr, 5)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	has_multiple := b.block_instr2(.gt, entry, b.i1_type, len, one)
	init := b.m.add_block(func_id, 'reverse_init')
	check := b.m.add_block(func_id, 'reverse_check')
	byte_init := b.m.add_block(func_id, 'reverse_byte_init')
	byte_check := b.m.add_block(func_id, 'reverse_byte_check')
	swap := b.m.add_block(func_id, 'reverse_swap')
	byte_next := b.m.add_block(func_id, 'reverse_byte_next')
	outer_next := b.m.add_block(func_id, 'reverse_outer_next')
	done := b.m.add_block(func_id, 'reverse_done')
	b.block_instr3(.br, entry, b.void_type, has_multiple, ValueID(init), ValueID(done))

	b.block_instr2(.store, init, b.void_type, zero, left_slot)
	right := b.block_instr2(.sub, init, b.i64_type, len, one)
	b.block_instr2(.store, init, b.void_type, right, right_slot)
	b.block_instr1(.jmp, init, b.void_type, ValueID(check))

	left := b.block_instr1(.load, check, b.i64_type, left_slot)
	right_value := b.block_instr1(.load, check, b.i64_type, right_slot)
	active := b.block_instr2(.lt, check, b.i1_type, left, right_value)
	b.block_instr3(.br, check, b.void_type, active, ValueID(byte_init), ValueID(done))

	b.block_instr2(.store, byte_init, b.void_type, zero, byte_slot)
	b.block_instr1(.jmp, byte_init, b.void_type, ValueID(byte_check))

	byte_index := b.block_instr1(.load, byte_check, b.i64_type, byte_slot)
	has_byte := b.block_instr2(.lt, byte_check, b.i1_type, byte_index, elem_size)
	b.block_instr3(.br, byte_check, b.void_type, has_byte, ValueID(swap),
		ValueID(outer_next))

	mut left_offset := b.block_instr2(.mul, swap, b.i64_type, left, elem_size)
	mut right_offset := b.block_instr2(.mul, swap, b.i64_type, right_value, elem_size)
	left_offset = b.block_instr2(.add, swap, b.i64_type, left_offset, byte_index)
	right_offset = b.block_instr2(.add, swap, b.i64_type, right_offset, byte_index)
	left_ptr := b.block_instr2(.get_element_ptr, swap, ptr_i8, data, left_offset)
	right_ptr := b.block_instr2(.get_element_ptr, swap, ptr_i8, data, right_offset)
	left_byte := b.block_instr1(.load, swap, b.i8_type, left_ptr)
	right_byte := b.block_instr1(.load, swap, b.i8_type, right_ptr)
	b.block_instr2(.store, swap, b.void_type, right_byte, left_ptr)
	b.block_instr2(.store, swap, b.void_type, left_byte, right_ptr)
	b.block_instr1(.jmp, swap, b.void_type, ValueID(byte_next))

	next_byte := b.block_instr2(.add, byte_next, b.i64_type, byte_index, one)
	b.block_instr2(.store, byte_next, b.void_type, next_byte, byte_slot)
	b.block_instr1(.jmp, byte_next, b.void_type, ValueID(byte_check))

	next_left := b.block_instr2(.add, outer_next, b.i64_type, left, one)
	next_right := b.block_instr2(.sub, outer_next, b.i64_type, right_value, one)
	b.block_instr2(.store, outer_next, b.void_type, next_left, left_slot)
	b.block_instr2(.store, outer_next, b.void_type, next_right, right_slot)
	b.block_instr1(.jmp, outer_next, b.void_type, ValueID(check))

	b.block_instr0(.ret, done, b.void_type)
}

// generate_scalar_array_sort_body emits the native equivalent of the scalar qsort helpers
// generated by the C backend.
fn (mut b Builder) generate_scalar_array_sort_body(func_id int, elem_type TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	ptr_elem := b.m.type_store.get_ptr(elem_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, ptr_array, 'arr')
	i_slot := b.block_instr0(.alloca, entry, ptr_i64)
	j_slot := b.block_instr0(.alloca, entry, ptr_i64)
	data_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 0)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len := b.block_load_array_int_field(entry, arr, 2)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	elem_size_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 5)
	elem_size32 := b.block_instr1(.load, entry, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, entry, b.i64_type, elem_size32)
	has_multiple := b.block_instr2(.gt, entry, b.i1_type, len, one)
	outer_init := b.m.add_block(func_id, 'sort_outer_init')
	outer_check := b.m.add_block(func_id, 'sort_outer_check')
	inner_init := b.m.add_block(func_id, 'sort_inner_init')
	inner_check := b.m.add_block(func_id, 'sort_inner_check')
	compare := b.m.add_block(func_id, 'sort_compare')
	swap := b.m.add_block(func_id, 'sort_swap')
	inner_next := b.m.add_block(func_id, 'sort_inner_next')
	outer_next := b.m.add_block(func_id, 'sort_outer_next')
	done := b.m.add_block(func_id, 'sort_done')
	b.block_instr3(.br, entry, b.void_type, has_multiple, ValueID(outer_init), ValueID(done))

	b.block_instr2(.store, outer_init, b.void_type, zero, i_slot)
	b.block_instr1(.jmp, outer_init, b.void_type, ValueID(outer_check))

	i := b.block_instr1(.load, outer_check, b.i64_type, i_slot)
	last := b.block_instr2(.sub, outer_check, b.i64_type, len, one)
	has_outer := b.block_instr2(.lt, outer_check, b.i1_type, i, last)
	b.block_instr3(.br, outer_check, b.void_type, has_outer, ValueID(inner_init), ValueID(done))

	b.block_instr2(.store, inner_init, b.void_type, one, j_slot)
	b.block_instr1(.jmp, inner_init, b.void_type, ValueID(inner_check))

	j := b.block_instr1(.load, inner_check, b.i64_type, j_slot)
	remaining := b.block_instr2(.sub, inner_check, b.i64_type, len, i)
	has_inner := b.block_instr2(.lt, inner_check, b.i1_type, j, remaining)
	b.block_instr3(.br, inner_check, b.void_type, has_inner, ValueID(compare), ValueID(outer_next))

	prev_idx := b.block_instr2(.sub, compare, b.i64_type, j, one)
	cur_offset := b.block_instr2(.mul, compare, b.i64_type, j, elem_size)
	prev_offset := b.block_instr2(.mul, compare, b.i64_type, prev_idx, elem_size)
	cur_ptr := b.block_instr2(.get_element_ptr, compare, ptr_elem, data, cur_offset)
	prev_ptr := b.block_instr2(.get_element_ptr, compare, ptr_elem, data, prev_offset)
	cur := b.block_instr1(.load, compare, elem_type, cur_ptr)
	prev := b.block_instr1(.load, compare, elem_type, prev_ptr)
	less_op := if b.is_unsigned_type(elem_type) { OpCode.ult } else { OpCode.lt }
	less := b.block_instr2(less_op, compare, b.i1_type, cur, prev)
	b.block_instr3(.br, compare, b.void_type, less, ValueID(swap), ValueID(inner_next))

	b.block_instr2(.store, swap, b.void_type, cur, prev_ptr)
	b.block_instr2(.store, swap, b.void_type, prev, cur_ptr)
	b.block_instr1(.jmp, swap, b.void_type, ValueID(inner_next))

	next_j := b.block_instr2(.add, inner_next, b.i64_type, j, one)
	b.block_instr2(.store, inner_next, b.void_type, next_j, j_slot)
	b.block_instr1(.jmp, inner_next, b.void_type, ValueID(inner_check))

	next_i := b.block_instr2(.add, outer_next, b.i64_type, i, one)
	b.block_instr2(.store, outer_next, b.void_type, next_i, i_slot)
	b.block_instr1(.jmp, outer_next, b.void_type, ValueID(outer_check))

	b.block_instr0(.ret, done, b.void_type)
}

// register_panic_stub updates register panic stub state for ssa.
fn (mut b Builder) register_panic_stub() {
	mut p1 := []TypeID{}
	p1 << b.str_type
	if panic_id := b.register_runtime_function('panic', b.void_type, p1) {
		b.generate_panic_body(panic_id)
	}
}

// register_printing_stubs updates register printing stubs state for ssa.
fn (mut b Builder) register_printing_stubs() {
	mut p1 := []TypeID{}
	p1 << b.str_type
	if print_id := b.register_runtime_function('print', b.void_type, p1) {
		b.generate_print_body(print_id, '1', false)
	}
	if println_id := b.register_runtime_function('println', b.void_type, p1) {
		b.generate_print_body(println_id, '1', true)
	}
	if eprint_id := b.register_runtime_function('eprint', b.void_type, p1) {
		b.generate_print_body(eprint_id, '2', false)
	}
	if eprintln_id := b.register_runtime_function('eprintln', b.void_type, p1) {
		b.generate_print_body(eprintln_id, '2', true)
	}
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
	write_ref := b.m.add_value(.func_ref, b.void_type, 'write', b.runtime_fn_id('write'))
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
	if eprintln_id := b.runtime_fn_lookup('eprintln') {
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'eprintln', eprintln_id)
		b.block_instr2(.call, entry, b.void_type, fn_ref, message)
	}
	if exit_id := b.runtime_fn_lookup('exit') {
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
	if new_id := b.register_runtime_function('strings.new_builder', b.array_type, p1) {
		b.generate_builder_new_body(new_id)
	}

	mut p2 := []TypeID{}
	p2 << ptr_builder
	p2 << b.str_type
	if write_string_id := b.register_runtime_function('strings.Builder.write_string', b.void_type, p2) {
		b.generate_builder_write_string_body(write_string_id, false)
	}
	if writeln_id := b.register_runtime_function('strings.Builder.writeln', b.void_type, p2) {
		b.generate_builder_write_string_body(writeln_id, true)
	}

	p1 = []TypeID{}
	p1 << ptr_builder
	if str_id := b.register_runtime_function('strings.Builder.str', b.str_type, p1) {
		b.generate_builder_str_body(str_id)
	}
	if free_id := b.register_runtime_function('strings.Builder.free', b.void_type, p1) {
		b.generate_builder_free_body(free_id)
	}

	p2 = []TypeID{}
	p2 << ptr_builder
	p2 << b.i8_type
	if b.m.target.ptr_size == 4 {
		if write_u8_id := b.register_runtime_function('strings.Builder.write_u8', b.void_type, p2) {
			b.generate_builder_write_u8_body(write_u8_id)
		}
	} else {
		write_u8_id := b.register_synthetic_function('strings.Builder.write_u8', b.void_type, p2)
		b.generate_builder_write_u8_body(write_u8_id)
		write_byte_id := b.register_synthetic_function('strings.Builder.write_byte', b.void_type, p2)
		b.generate_builder_write_u8_body(write_byte_id)
	}
	p2 = []TypeID{}
	p2 << ptr_builder
	p2 << b.i32_type
	write_rune_id := b.register_synthetic_function('strings.Builder.write_rune', b.void_type, p2)
	b.generate_builder_write_rune_body(write_rune_id)

	mut p3 := []TypeID{}
	p3 << ptr_builder
	p3 << ptr_i8
	p3 << b.i64_type
	if write_ptr_id := b.register_runtime_function('strings.Builder.write_ptr', b.void_type, p3) {
		b.generate_builder_write_ptr_body(write_ptr_id)
	}
	if push_many_id := b.register_runtime_function('strings.Builder.push_many', b.void_type, p3) {
		b.generate_builder_write_ptr_body(push_many_id)
	}

	p2 = []TypeID{}
	p2 << ptr_builder
	p2 << b.i64_type
	if last_n_id := b.register_runtime_function('strings.Builder.last_n', b.str_type, p2) {
		b.generate_builder_last_n_body(last_n_id)
	}
}

// generate_builder_new_body supports generate builder new body handling for Builder.
fn (mut b Builder) generate_builder_new_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	initial_size := b.func_add_argument(func_id, b.i64_type, 'initial_size')
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
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
	double_needed := b.block_instr2(.mul, blk_grow, b.i64_type, needed, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, double_needed, two)
	if b.m.target.ptr_size == 4 {
		old_data := b.block_instr1(.load, blk_grow, ptr_i8, data_ptr)
		realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
		new_data := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref, old_data, new_cap)
		b.block_instr2(.store, blk_grow, b.void_type, new_data, data_ptr)
		b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	} else {
		grow_ref := b.m.add_value(.func_ref, b.void_type, '__ssa_array_grow', b.fn_ids['__ssa_array_grow'])
		b.block_instr3(.call, blk_grow, b.void_type, grow_ref, builder_ptr, new_cap)
	}
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_copy))

	data := b.block_instr1(.load, blk_copy, ptr_i8, data_ptr)
	dest := b.block_instr2(.add, blk_copy, ptr_i8, data, old_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	s := b.func_add_argument(func_id, b.str_type, 's')

	alloca_s := b.block_instr0(.alloca, entry, ptr_string)
	b.block_instr2(.store, entry, b.void_type, s, alloca_s)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	str_ptr_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_s, zero)
	// The length's offset belongs to the target layout: 8 is where a 64-bit
	// pointer puts it, and on a 4-byte pointer target that word is `is_lit`.
	len_ptr := b.block_struct_field_ptr(entry, alloca_s, b.str_type, 1)
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

// generate_builder_write_rune_body encodes one Unicode scalar into UTF-8 and
// appends the exact byte range to the builder.
fn (mut b Builder) generate_builder_write_rune_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_builder := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	builder_ptr := b.func_add_argument(func_id, ptr_builder, 'builder')
	r := b.func_add_argument(func_id, b.i32_type, 'r')
	code_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.i32_type))
	b.block_instr2(.store, entry, b.void_type, r, code_slot)
	four := b.m.get_or_add_const(b.i64_type, '4')
	buffer := b.block_instr1(.alloca, entry, ptr_i8, four)

	zero := b.m.get_or_add_const(b.i32_type, '0')
	max_code := b.m.get_or_add_const(b.i32_type, '1114111')
	surrogate_min := b.m.get_or_add_const(b.i32_type, '55296')
	surrogate_max := b.m.get_or_add_const(b.i32_type, '57343')
	replacement := b.m.get_or_add_const(b.i32_type, '65533')
	nonnegative := b.block_instr2(.ge, entry, b.i1_type, r, zero)
	below_max := b.block_instr2(.le, entry, b.i1_type, r, max_code)
	in_range := b.block_instr2(.and_, entry, b.i1_type, nonnegative, below_max)
	above_low := b.block_instr2(.ge, entry, b.i1_type, r, surrogate_min)
	below_high := b.block_instr2(.le, entry, b.i1_type, r, surrogate_max)
	surrogate := b.block_instr2(.and_, entry, b.i1_type, above_low, below_high)
	not_surrogate := b.block_instr2(.xor, entry, b.i1_type, surrogate,
		b.m.get_or_add_const(b.i1_type, '1'))
	valid := b.block_instr2(.and_, entry, b.i1_type, in_range, not_surrogate)
	invalid_block := b.m.add_block(func_id, 'write_rune_invalid')
	select_block := b.m.add_block(func_id, 'write_rune_select')
	b.block_instr3(.br, entry, b.void_type, valid, ValueID(select_block),
		ValueID(invalid_block))
	b.block_instr2(.store, invalid_block, b.void_type, replacement, code_slot)
	b.block_instr1(.jmp, invalid_block, b.void_type, ValueID(select_block))

	code := b.block_instr1(.load, select_block, b.i32_type, code_slot)
	one_limit := b.m.get_or_add_const(b.i32_type, '127')
	two_limit := b.m.get_or_add_const(b.i32_type, '2047')
	three_limit := b.m.get_or_add_const(b.i32_type, '65535')
	one_block := b.m.add_block(func_id, 'write_rune_one')
	two_check := b.m.add_block(func_id, 'write_rune_two_check')
	three_check := b.m.add_block(func_id, 'write_rune_three_check')
	two_block := b.m.add_block(func_id, 'write_rune_two')
	three_block := b.m.add_block(func_id, 'write_rune_three')
	four_block := b.m.add_block(func_id, 'write_rune_four')
	one_fits := b.block_instr2(.le, select_block, b.i1_type, code, one_limit)
	b.block_instr3(.br, select_block, b.void_type, one_fits, ValueID(one_block),
		ValueID(two_check))
	two_fits := b.block_instr2(.le, two_check, b.i1_type, code, two_limit)
	b.block_instr3(.br, two_check, b.void_type, two_fits, ValueID(two_block),
		ValueID(three_check))
	three_fits := b.block_instr2(.le, three_check, b.i1_type, code, three_limit)
	b.block_instr3(.br, three_check, b.void_type, three_fits, ValueID(three_block),
		ValueID(four_block))
	b.emit_builder_write_utf8(func_id, one_block, builder_ptr, buffer, code, 1)
	b.emit_builder_write_utf8(func_id, two_block, builder_ptr, buffer, code, 2)
	b.emit_builder_write_utf8(func_id, three_block, builder_ptr, buffer, code, 3)
	b.emit_builder_write_utf8(func_id, four_block, builder_ptr, buffer, code, 4)
}

fn (mut b Builder) emit_builder_write_utf8(func_id int, block_id BlockID, builder_ptr ValueID, buffer ValueID, code ValueID, nbytes int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	for i in 0 .. nbytes {
		mut byte := code
		if nbytes == 1 {
			byte = b.block_instr2(.and_, block_id, b.i32_type, code,
				b.m.get_or_add_const(b.i32_type, '127'))
		} else {
			shift := b.m.get_or_add_const(b.i32_type, '${(nbytes - i - 1) * 6}')
			byte = b.block_instr2(.lshr, block_id, b.i32_type, code, shift)
			mask := b.m.get_or_add_const(b.i32_type, if i == 0 {
				match nbytes {
					2 { '31' }
					3 { '15' }
					else { '7' }
				}
			} else {
				'63'
			})
			byte = b.block_instr2(.and_, block_id, b.i32_type, byte, mask)
			prefix := b.m.get_or_add_const(b.i32_type, if i == 0 {
				match nbytes {
					2 { '192' }
					3 { '224' }
					else { '240' }
				}
			} else {
				'128'
			})
			byte = b.block_instr2(.or_, block_id, b.i32_type, byte, prefix)
		}
		byte8 := b.block_instr1(.trunc, block_id, b.i8_type, byte)
		offset := b.m.get_or_add_const(b.i64_type, '${i}')
		byte_ptr := b.block_instr2(.add, block_id, ptr_i8, buffer, offset)
		b.block_instr2(.store, block_id, b.void_type, byte8, byte_ptr)
	}
	len := b.m.get_or_add_const(b.i64_type, '${nbytes}')
	end_block := b.emit_builder_append(func_id, block_id, builder_ptr, buffer, len)
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
	zero := b.m.get_or_add_const(b.i32_type, '0')
	b.block_instr2(.store, entry, b.void_type, zero, len_ptr)
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
	zero := b.m.get_or_add_const(b.i64_type, '0')
	non_positive := b.block_instr2(.le, entry, b.i1_type, n, zero)
	too_long := b.block_instr2(.gt, entry, b.i1_type, n, len)
	invalid := b.block_instr2(.or_, entry, b.i1_type, non_positive, too_long)
	empty_block := b.m.add_block(func_id, 'builder_last_n_empty')
	copy_block := b.m.add_block(func_id, 'builder_last_n_copy')
	b.block_instr3(.br, entry, b.void_type, invalid, ValueID(empty_block), ValueID(copy_block))

	empty := b.m.add_value(.string_literal, b.str_type, '', 0)
	b.block_instr1(.ret, empty_block, b.void_type, empty)

	start := b.block_instr2(.sub, copy_block, b.i64_type, len, n)
	src := b.block_instr2(.add, copy_block, ptr_i8, data, start)
	// Copy out an owned substring (matches strings.Builder.last_n -> spart): the result
	// must not alias the interior of the builder buffer, since string.free would later
	// pass that non-allocation-start pointer to free().
	result := b.emit_make_owned_string(copy_block, src, n)
	b.block_instr1(.ret, copy_block, b.void_type, result)
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
		if normalize_id := b.register_runtime_function(name, b.void_type, p1) {
			b.generate_builder_free_body(normalize_id)
		}
	}

	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	for name in ['join_path_single', 'os.join_path_single'] {
		if join_id := b.register_runtime_function(name, b.str_type, p2) {
			b.generate_join_path_single_body(join_id)
		}
	}
}

// generate_join_path_single_body supports generate join path single body handling for Builder.
fn (mut b Builder) generate_join_path_single_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	base := b.func_add_argument(func_id, b.str_type, 'base')
	elem := b.func_add_argument(func_id, b.str_type, 'elem')
	slash := b.m.add_value(.string_literal, b.str_type, '/', 0)
	plus_ref := b.m.add_value(.func_ref, b.str_type, 'string__plus', b.runtime_fn_id('string__plus'))
	with_sep := b.block_instr3(.call, entry, b.str_type, plus_ref, base, slash)
	result := b.block_instr3(.call, entry, b.str_type, plus_ref, with_sep, elem)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_map_runtime_stubs updates register map runtime stubs state for ssa.
fn (mut b Builder) register_map_runtime_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	if b.m.target.ptr_size == 8 {
		b.register_native_map_index_helpers()
	}
	alignment_exponent_id := b.register_synthetic_function('__ssa_data_alignment_exponent',
		b.u8_type, [b.u64_type])
	b.generate_data_alignment_exponent_body(alignment_exponent_id)

	mut p2 := []TypeID{}
	p2 << ptr_map
	p2 << ptr_i8
	if find_id := b.register_runtime_function('v3_map_find', b.i64_type, p2) {
		b.generate_map_find_body(find_id)
	}

	mut p6 := []TypeID{}
	for _ in 0 .. 6 {
		p6 << b.i64_type
	}
	if new_id := b.register_runtime_function('new_map', b.map_type, p6) {
		b.generate_new_map_body(new_id)
		b.runtime_fn_ids['new_map'] = new_id
		if fn_type := b.fn_types['new_map'] {
			b.runtime_fn_types['new_map'] = fn_type
		}
	}
	mut p8 := []TypeID{}
	for _ in 0 .. 8 {
		p8 << b.i64_type
	}
	if aligned_id := b.register_runtime_function('__ssa_new_map_aligned', b.map_type, p8) {
		b.generate_new_map_aligned_body(aligned_id)
		if fn_type := b.fn_types['__ssa_new_map_aligned'] {
			b.fn_ids['new_map_aligned'] = aligned_id
			b.fn_types['new_map_aligned'] = fn_type
		}
	}

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
	if sized_set_id := b.register_runtime_function('v3_map_set_sized', b.void_type, p5) {
		b.generate_map_set_sized_body(sized_set_id)
	}
	if set_id := b.register_runtime_function('map__set', b.void_type, p3) {
		b.generate_map_set_default_body(set_id)
	}

	p3 = []TypeID{}
	p3 << ptr_map
	p3 << ptr_i8
	p3 << ptr_i8
	if get_id := b.register_runtime_function('map__get', ptr_i8, p3) {
		b.generate_map_get_body(get_id)
	}
	mut p2_map_key := []TypeID{}
	p2_map_key << ptr_map
	p2_map_key << ptr_i8
	if b.m.target.ptr_size == 4 {
		if get_check_id := b.register_runtime_function('map__get_check', ptr_i8, p2_map_key) {
			b.generate_map_get_check_body(get_check_id)
		}
		if get_key_check_id := b.register_runtime_function('map__get_key_check', ptr_i8, p2_map_key) {
			b.generate_map_get_key_check_body(get_key_check_id)
		}
		if get_or_set_id := b.register_runtime_function('map__get_or_set', ptr_i8, p3) {
			b.generate_map_get_body(get_or_set_id)
		}
	} else {
		get_check_id := b.register_synthetic_function('map__get_check', ptr_i8, p2_map_key)
		b.generate_map_get_check_body(get_check_id)
		get_key_check_id := b.register_synthetic_function('map__get_key_check', ptr_i8, p2_map_key)
		b.generate_map_get_key_check_body(get_key_check_id)
		get_or_set_id := b.register_synthetic_function('map__get_or_set', ptr_i8, p3)
		b.generate_native_map_get_and_set_body(get_or_set_id)
		get_and_set_id := b.register_synthetic_function('map__get_and_set', ptr_i8, p3)
		b.generate_native_map_get_and_set_body(get_and_set_id)
	}

	p2 = []TypeID{}
	p2 << ptr_map
	p2 << ptr_i8
	if exists_id := b.register_runtime_function('map__exists', b.i1_type, p2) {
		b.generate_map_exists_body(exists_id)
	}

	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_map
	if clear_id := b.register_runtime_function('map__clear', b.void_type, p1_ptr) {
		b.generate_map_clear_body(clear_id)
	}
	if free_id := b.register_runtime_function('map__free', b.void_type, p1_ptr) {
		b.generate_map_free_body(free_id)
	}
	mut p2_arr := []TypeID{}
	p2_arr << ptr_map
	p2_arr << b.i64_type
	if keys_id := b.register_runtime_function('map__keys', b.array_type, p2_arr) {
		b.generate_map_keys_values_body(keys_id, 0, 4)
	}
	if values_id := b.register_runtime_function('map__values', b.array_type, p2_arr) {
		b.generate_map_keys_values_body(values_id, 1, 5)
	}

	mut p2_reserve := []TypeID{}
	p2_reserve << ptr_map
	p2_reserve << b.u32_type
	if reserve_id := b.register_runtime_function('map__reserve', b.void_type, p2_reserve) {
		b.generate_map_reserve_body(reserve_id)
	}

	p2 = []TypeID{}
	p2 << ptr_map
	p2 << ptr_i8
	if delete_id := b.register_runtime_function('map__delete', b.void_type, p2) {
		b.generate_map_delete_body(delete_id)
	}

	mut p1_map := []TypeID{}
	p1_map << ptr_map
	if clone_id := b.register_runtime_function('map__clone', b.map_type, p1_map) {
		b.generate_map_clone_body(clone_id)
	}

	mut p1_ptr_map := []TypeID{}
	p1_ptr_map << ptr_map
	if move_id := b.register_runtime_function('map__move', b.map_type, p1_ptr_map) {
		b.generate_map_move_body(move_id)
	}
}

fn (mut b Builder) generate_map_reserve_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	n := b.func_add_argument(func_id, b.u32_type, 'n')
	state := b.map_state_ptr(entry, m)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, state, zero_state)
	check_cap := b.m.add_block(func_id, 'map_reserve_check')
	done := b.m.add_block(func_id, 'map_reserve_done')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(check_cap), ValueID(done))
	requested := b.block_instr1(.zext, check_cap, b.i64_type, n)
	current_cap := b.map_state_load_i64(check_cap, state, 2)
	needs_grow := b.block_instr2(.gt, check_cap, b.i1_type, requested, current_cap)
	grow := b.m.add_block(func_id, 'map_reserve_grow')
	b.block_instr3(.br, check_cap, b.void_type, needs_grow, ValueID(grow), ValueID(done))

	keys_ptr := b.map_state_field_ptr(grow, state, 0)
	vals_ptr := b.map_state_field_ptr(grow, state, 1)
	old_keys := b.block_instr1(.load, grow, ptr_i8, keys_ptr)
	old_vals := b.block_instr1(.load, grow, ptr_i8, vals_ptr)
	len := b.map_state_load_i64(grow, state, 3)
	key_size := b.map_state_load_i64(grow, state, 4)
	val_size := b.map_state_load_i64(grow, state, 5)
	key_alignment_ptr := b.map_state_field_ptr(grow, state, 13)
	val_alignment_ptr := b.map_state_field_ptr(grow, state, 14)
	key_exponent8 := b.block_instr1(.load, grow, b.u8_type, key_alignment_ptr)
	val_exponent8 := b.block_instr1(.load, grow, b.u8_type, val_alignment_ptr)
	key_exponent := b.block_instr1(.zext, grow, b.i64_type, key_exponent8)
	val_exponent := b.block_instr1(.zext, grow, b.i64_type, val_exponent8)
	one := b.m.get_or_add_const(b.i64_type, '1')
	key_alignment := b.block_instr2(.shl, grow, b.i64_type, one, key_exponent)
	val_alignment := b.block_instr2(.shl, grow, b.i64_type, one, val_exponent)
	new_keys := b.map_alloc_aligned_buffer(grow, requested, key_size, key_alignment)
	new_vals := b.map_alloc_aligned_buffer(grow, requested, val_size, val_alignment)
	key_bytes := b.block_instr2(.mul, grow, b.i64_type, len, key_size)
	val_bytes := b.block_instr2(.mul, grow, b.i64_type, len, val_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, grow, ptr_i8, memcpy_ref, new_keys, old_keys, key_bytes)
	b.block_instr4(.call, grow, ptr_i8, memcpy_ref, new_vals, old_vals, val_bytes)
	free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
	b.block_instr2(.call, grow, b.void_type, free_ref, old_keys)
	b.block_instr2(.call, grow, b.void_type, free_ref, old_vals)
	b.block_instr2(.store, grow, b.void_type, new_keys, keys_ptr)
	b.block_instr2(.store, grow, b.void_type, new_vals, vals_ptr)
	b.map_state_store_i64(grow, state, 2, requested)
	if b.m.target.ptr_size == 8 {
		even_index_ptr := b.block_struct_field_ptr(grow, state, b.map_state_type, 2)
		two := b.m.get_or_add_const(b.u32_type, '2')
		even_index := b.block_instr2(.mul, grow, b.u32_type, n, two)
		b.block_instr2(.store, grow, b.void_type, even_index, even_index_ptr)
		b.invalidate_native_map_index(grow, state)
		deletes_ptr := b.map_dense_field_ptr(grow, state, 4)
		deletes := b.block_instr1(.load, grow, b.u32_type, deletes_ptr)
		has_deletes := b.block_instr2(.ne, grow, b.i1_type, deletes,
			b.m.get_or_add_const(b.u32_type, '0'))
		blk_bitmap := b.m.add_block(func_id, 'map_reserve_bitmap')
		b.block_instr3(.br, grow, b.void_type, has_deletes, ValueID(blk_bitmap),
			ValueID(done))
		bitmap_one := b.m.get_or_add_const(b.i64_type, '1')
		calloc_ref := b.m.add_value(.func_ref, ptr_i8, 'calloc', b.fn_ids['calloc'])
		bitmap := b.block_instr3(.call, blk_bitmap, ptr_i8, calloc_ref, requested,
			bitmap_one)
		old_bitmap_ptr := b.map_dense_field_ptr(blk_bitmap, state, 7)
		old_bitmap := b.block_instr1(.load, blk_bitmap, ptr_i8, old_bitmap_ptr)
		b.block_instr4(.call, blk_bitmap, ptr_i8, memcpy_ref, bitmap, old_bitmap, len)
		free_bitmap_ref := b.m.add_value(.func_ref, b.void_type, 'free',
			b.fn_ids['free'])
		b.block_instr2(.call, blk_bitmap, b.void_type, free_bitmap_ref, old_bitmap)
		b.block_instr2(.store, blk_bitmap, b.void_type, bitmap, old_bitmap_ptr)
		b.block_instr1(.jmp, blk_bitmap, b.void_type, ValueID(done))
	} else {
		b.block_instr1(.jmp, grow, b.void_type, ValueID(done))
	}
	b.block_instr0(.ret, done, b.void_type)
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

	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.map_state_store_i64(blk_clear, state, 3, zero)
	if b.m.target.ptr_size == 8 {
		count_ptr := b.block_struct_field_ptr(blk_clear, state, b.map_state_type, 13)
		count_zero := b.m.get_or_add_const(b.i32_type, '0')
		b.block_instr2(.store, blk_clear, b.void_type, count_zero, count_ptr)
	}
	if b.m.target.ptr_size == 8 { b.invalidate_native_map_index(blk_clear, state) }
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
	if b.m.target.ptr_size == 4 {
		b.generate_compact_map_delete_body(func_id)
		return
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'm')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	blk_found := b.m.add_block(func_id, 'map_delete_found')
	blk_done := b.m.add_block(func_id, 'map_delete_done')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find',
		b.runtime_fn_id('v3_map_find'))
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_done))

	state := b.map_state_ptr(blk_found, map_ptr)
	len := b.map_state_load_i64(blk_found, state, 3)
	keys_ptr := b.map_state_field_ptr(blk_found, state, 0)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	keys := b.block_instr1(.load, blk_found, ptr_i8, keys_ptr)
	vals := b.block_instr1(.load, blk_found, ptr_i8, vals_ptr)
	key_size := b.map_state_load_i64(blk_found, state, 4)
	val_size := b.map_state_load_i64(blk_found, state, 5)
	key_offset := b.block_instr2(.mul, blk_found, b.i64_type, idx, key_size)
	val_offset := b.block_instr2(.mul, blk_found, b.i64_type, idx, val_size)
	key_target := b.block_instr2(.add, blk_found, ptr_i8, keys, key_offset)
	val_target := b.block_instr2(.add, blk_found, ptr_i8, vals, val_offset)
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset', b.runtime_fn_id('memset'))
	zero_byte := b.m.get_or_add_const(b.i32_type, '0')
	b.block_instr4(.call, blk_found, ptr_i8, memset_ref, key_target, zero_byte, key_size)
	b.block_instr4(.call, blk_found, ptr_i8, memset_ref, val_target, zero_byte, val_size)

	deletes_ptr := b.map_dense_field_ptr(blk_found, state, 4)
	deletes := b.block_instr1(.load, blk_found, b.u32_type, deletes_ptr)
	first_tombstone := b.block_instr2(.eq, blk_found, b.i1_type, deletes,
		b.m.get_or_add_const(b.u32_type, '0'))
	blk_alloc_bitmap := b.m.add_block(func_id, 'map_delete_alloc_bitmap')
	blk_mark := b.m.add_block(func_id, 'map_delete_mark')
	b.block_instr3(.br, blk_found, b.void_type, first_tombstone, ValueID(blk_alloc_bitmap),
		ValueID(blk_mark))
	cap := b.map_state_load_i64(blk_alloc_bitmap, state, 2)
	one_byte := b.m.get_or_add_const(b.i64_type, '1')
	calloc_ref := b.m.add_value(.func_ref, ptr_i8, 'calloc', b.fn_ids['calloc'])
	bitmap := b.block_instr3(.call, blk_alloc_bitmap, ptr_i8, calloc_ref, cap, one_byte)
	bitmap_ptr := b.map_dense_field_ptr(blk_alloc_bitmap, state, 7)
	b.block_instr2(.store, blk_alloc_bitmap, b.void_type, bitmap, bitmap_ptr)
	b.block_instr1(.jmp, blk_alloc_bitmap, b.void_type, ValueID(blk_mark))

	current_bitmap_ptr := b.map_dense_field_ptr(blk_mark, state, 7)
	current_bitmap := b.block_instr1(.load, blk_mark, ptr_i8, current_bitmap_ptr)
	index_flag_ptr := b.block_instr2(.add, blk_mark, ptr_i8, current_bitmap, idx)
	one8 := b.m.get_or_add_const(b.i8_type, '1')
	b.block_instr2(.store, blk_mark, b.void_type, one8, index_flag_ptr)
	one32u := b.m.get_or_add_const(b.u32_type, '1')
	new_deletes := b.block_instr2(.add, blk_mark, b.u32_type, deletes, one32u)
	b.block_instr2(.store, blk_mark, b.void_type, new_deletes, deletes_ptr)

	if b.m.target.ptr_size == 8 {
		count_ptr := b.block_struct_field_ptr(blk_mark, state, b.map_state_type, 13)
		count := b.block_instr1(.load, blk_mark, b.i32_type, count_ptr)
		one32 := b.m.get_or_add_const(b.i32_type, '1')
		new_count := b.block_instr2(.sub, blk_mark, b.i32_type, count, one32)
		b.block_instr2(.store, blk_mark, b.void_type, new_count, count_ptr)
	}

	len_slot := b.block_instr0(.alloca, blk_mark, b.m.type_store.get_ptr(b.i64_type))
	b.block_instr2(.store, blk_mark, b.void_type, len, len_slot)
	blk_trim := b.m.add_block(func_id, 'map_delete_trim')
	blk_trim_entry := b.m.add_block(func_id, 'map_delete_trim_entry')
	blk_clear_tail := b.m.add_block(func_id, 'map_delete_clear_tail')
	blk_cleanup := b.m.add_block(func_id, 'map_delete_cleanup')
	blk_free_bitmap := b.m.add_block(func_id, 'map_delete_free_bitmap')
	blk_store := b.m.add_block(func_id, 'map_delete_store')
	b.block_instr1(.jmp, blk_mark, b.void_type, ValueID(blk_trim_entry))

	trim_index := b.block_instr1(.load, blk_trim_entry, b.i64_type, len_slot)
	has_entries := b.block_instr2(.gt, blk_trim_entry, b.i1_type, trim_index,
		b.m.get_or_add_const(b.i64_type, '0'))
	b.block_instr3(.br, blk_trim_entry, b.void_type, has_entries, ValueID(blk_trim),
		ValueID(blk_cleanup))
	one64 := b.m.get_or_add_const(b.i64_type, '1')
	last_index := b.block_instr2(.sub, blk_trim, b.i64_type, trim_index, one64)
	trim_bitmap_ptr := b.map_dense_field_ptr(blk_trim, state, 7)
	trim_bitmap := b.block_instr1(.load, blk_trim, ptr_i8, trim_bitmap_ptr)
	last_flag_ptr := b.block_instr2(.add, blk_trim, ptr_i8, trim_bitmap, last_index)
	last_flag := b.block_instr1(.load, blk_trim, b.i8_type, last_flag_ptr)
	last_deleted := b.block_instr2(.ne, blk_trim, b.i1_type, last_flag,
		b.m.get_or_add_const(b.i8_type, '0'))
	b.block_instr3(.br, blk_trim, b.void_type, last_deleted, ValueID(blk_clear_tail),
		ValueID(blk_cleanup))
	b.block_instr2(.store, blk_clear_tail, b.void_type,
		b.m.get_or_add_const(b.i8_type, '0'), last_flag_ptr)
	trimmed_len := last_index
	b.block_instr2(.store, blk_clear_tail, b.void_type, trimmed_len, len_slot)
	trimmed_deletes := b.block_instr2(.sub, blk_clear_tail, b.u32_type,
		b.block_instr1(.load, blk_clear_tail, b.u32_type, deletes_ptr), one8)
	b.block_instr2(.store, blk_clear_tail, b.void_type, trimmed_deletes, deletes_ptr)
	b.block_instr1(.jmp, blk_clear_tail, b.void_type, ValueID(blk_trim_entry))

	remaining_deletes := b.block_instr1(.load, blk_cleanup, b.u32_type, deletes_ptr)
	no_deletes := b.block_instr2(.eq, blk_cleanup, b.i1_type, remaining_deletes,
		b.m.get_or_add_const(b.u32_type, '0'))
	b.block_instr3(.br, blk_cleanup, b.void_type, no_deletes, ValueID(blk_free_bitmap),
		ValueID(blk_store))
	final_bitmap_ptr := b.map_dense_field_ptr(blk_free_bitmap, state, 7)
	final_bitmap := b.block_instr1(.load, blk_free_bitmap, ptr_i8, final_bitmap_ptr)
	free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
	b.block_instr2(.call, blk_free_bitmap, b.void_type, free_ref, final_bitmap)
	b.block_instr2(.store, blk_free_bitmap, b.void_type,
		b.m.get_or_add_const(ptr_i8, '0'), final_bitmap_ptr)
	b.block_instr1(.jmp, blk_free_bitmap, b.void_type, ValueID(blk_store))

	final_len := b.block_instr1(.load, blk_store, b.i64_type, len_slot)
	b.map_state_store_i64(blk_store, state, 3, final_len)
	trimmed_tail := b.block_instr2(.lt, blk_store, b.i1_type, final_len, len)
	blk_invalidate := b.m.add_block(func_id, 'map_delete_invalidate')
	b.block_instr3(.br, blk_store, b.void_type, trimmed_tail, ValueID(blk_invalidate),
		ValueID(blk_done))
	b.invalidate_native_map_index(blk_invalidate, state)
	b.block_instr1(.jmp, blk_invalidate, b.void_type, ValueID(blk_done))

	b.block_instr0(.ret, blk_done, b.void_type)
}

// generate_compact_map_delete_body preserves the compact 32-bit map ABI.
fn (mut b Builder) generate_compact_map_delete_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'm')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.runtime_fn_id('v3_map_find'))
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)
	blk_found := b.m.add_block(func_id, 'map_delete_found')
	blk_done := b.m.add_block(func_id, 'map_delete_done')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_done))

	state := b.map_state_ptr(blk_found, map_ptr)
	keys_ptr := b.map_state_field_ptr(blk_found, state, 0)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	len := b.map_state_load_i64(blk_found, state, 3)
	one := b.m.get_or_add_const(b.i64_type, '1')
	new_len := b.block_instr2(.sub, blk_found, b.i64_type, len, one)
	deleting_last := b.block_instr2(.eq, blk_found, b.i1_type, idx, new_len)
	blk_compact := b.m.add_block(func_id, 'map_delete_compact')
	blk_zero_tail := b.m.add_block(func_id, 'map_delete_zero_tail')
	blk_store_len := b.m.add_block(func_id, 'map_delete_store_len')
	b.block_instr3(.br, blk_found, b.void_type, deleting_last, ValueID(blk_zero_tail),
		ValueID(blk_compact))

	keys := b.block_instr1(.load, blk_compact, ptr_i8, keys_ptr)
	vals := b.block_instr1(.load, blk_compact, ptr_i8, vals_ptr)
	key_size := b.map_state_load_i64(blk_compact, state, 4)
	val_size := b.map_state_load_i64(blk_compact, state, 5)
	key_dst_off := b.block_instr2(.mul, blk_compact, b.i64_type, idx, key_size)
	val_dst_off := b.block_instr2(.mul, blk_compact, b.i64_type, idx, val_size)
	remaining := b.block_instr2(.sub, blk_compact, b.i64_type, new_len, idx)
	key_bytes := b.block_instr2(.mul, blk_compact, b.i64_type, remaining, key_size)
	val_bytes := b.block_instr2(.mul, blk_compact, b.i64_type, remaining, val_size)
	key_src_index := b.block_instr2(.add, blk_compact, b.i64_type, idx, one)
	key_src_off := b.block_instr2(.mul, blk_compact, b.i64_type, key_src_index, key_size)
	val_src_off := b.block_instr2(.mul, blk_compact, b.i64_type, key_src_index, val_size)
	key_dst := b.block_instr2(.add, blk_compact, ptr_i8, keys, key_dst_off)
	val_dst := b.block_instr2(.add, blk_compact, ptr_i8, vals, val_dst_off)
	key_src := b.block_instr2(.add, blk_compact, ptr_i8, keys, key_src_off)
	val_src := b.block_instr2(.add, blk_compact, ptr_i8, vals, val_src_off)
	memmove_ref_key := b.m.add_value(.func_ref, b.void_type, 'memmove',
		b.runtime_fn_id('memmove'))
	memmove_ref_val := b.m.add_value(.func_ref, b.void_type, 'memmove',
		b.runtime_fn_id('memmove'))
	b.block_instr4(.call, blk_compact, ptr_i8, memmove_ref_key, key_dst, key_src, key_bytes)
	b.block_instr4(.call, blk_compact, ptr_i8, memmove_ref_val, val_dst, val_src, val_bytes)
	b.block_instr1(.jmp, blk_compact, b.void_type, ValueID(blk_zero_tail))

	keys_tail := b.block_instr1(.load, blk_zero_tail, ptr_i8, keys_ptr)
	vals_tail := b.block_instr1(.load, blk_zero_tail, ptr_i8, vals_ptr)
	key_size_tail := b.map_state_load_i64(blk_zero_tail, state, 4)
	val_size_tail := b.map_state_load_i64(blk_zero_tail, state, 5)
	key_tail_off := b.block_instr2(.mul, blk_zero_tail, b.i64_type, new_len, key_size_tail)
	val_tail_off := b.block_instr2(.mul, blk_zero_tail, b.i64_type, new_len, val_size_tail)
	key_tail := b.block_instr2(.add, blk_zero_tail, ptr_i8, keys_tail, key_tail_off)
	val_tail := b.block_instr2(.add, blk_zero_tail, ptr_i8, vals_tail, val_tail_off)
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset',
		b.runtime_fn_id('memset'))
	zero_byte := b.m.get_or_add_const(b.i32_type, '0')
	b.block_instr4(.call, blk_zero_tail, ptr_i8, memset_ref, key_tail, zero_byte,
		key_size_tail)
	b.block_instr4(.call, blk_zero_tail, ptr_i8, memset_ref, val_tail, zero_byte,
		val_size_tail)
	b.block_instr1(.jmp, blk_zero_tail, b.void_type, ValueID(blk_store_len))
	b.map_state_store_i64(blk_store_len, state, 3, new_len)
	b.block_instr1(.jmp, blk_store_len, b.void_type, ValueID(blk_done))
	b.block_instr0(.ret, blk_done, b.void_type)
}

// generate_map_clone_body supports generate map clone body handling for Builder.
fn (mut b Builder) generate_map_clone_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	old_state := b.map_state_ptr(entry, m)
	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, old_state, zero_state)
	blk_clone := b.m.add_block(func_id, 'map_clone_copy')
	blk_empty := b.m.add_block(func_id, 'map_clone_empty')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(blk_clone), ValueID(blk_empty))

	one := b.m.get_or_add_const(b.i64_type, '1')
	state_size := b.m.get_or_add_const(b.i64_type, b.m.type_size(b.map_state_type).str())
	calloc_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.runtime_fn_id('calloc'))
	new_state_raw := b.block_instr3(.call, blk_clone, ptr_i8, calloc_ref, one, state_size)
	new_state := b.block_instr1(.bitcast, blk_clone, ptr_state, new_state_raw)

	old_keys_ptr := b.map_state_field_ptr(blk_clone, old_state, 0)
	old_vals_ptr := b.map_state_field_ptr(blk_clone, old_state, 1)
	old_keys := b.block_instr1(.load, blk_clone, ptr_i8, old_keys_ptr)
	old_vals := b.block_instr1(.load, blk_clone, ptr_i8, old_vals_ptr)
	cap := b.map_state_load_i64(blk_clone, old_state, 2)
	len := b.map_state_load_i64(blk_clone, old_state, 3)
	key_size := b.map_state_load_i64(blk_clone, old_state, 4)
	val_size := b.map_state_load_i64(blk_clone, old_state, 5)
	key_alignment_ptr := b.map_state_field_ptr(blk_clone, old_state, 13)
	val_alignment_ptr := b.map_state_field_ptr(blk_clone, old_state, 14)
	key_exponent8 := b.block_instr1(.load, blk_clone, b.u8_type, key_alignment_ptr)
	val_exponent8 := b.block_instr1(.load, blk_clone, b.u8_type, val_alignment_ptr)
	key_exponent := b.block_instr1(.zext, blk_clone, b.i64_type, key_exponent8)
	val_exponent := b.block_instr1(.zext, blk_clone, b.i64_type, val_exponent8)
	one_alignment := b.m.get_or_add_const(b.i64_type, '1')
	key_alignment := b.block_instr2(.shl, blk_clone, b.i64_type, one_alignment, key_exponent)
	val_alignment := b.block_instr2(.shl, blk_clone, b.i64_type, one_alignment, val_exponent)
	new_keys := b.map_alloc_aligned_buffer(blk_clone, cap, key_size, key_alignment)
	new_vals := b.map_alloc_aligned_buffer(blk_clone, cap, val_size, val_alignment)
	key_bytes := b.block_instr2(.mul, blk_clone, b.i64_type, len, key_size)
	val_bytes := b.block_instr2(.mul, blk_clone, b.i64_type, len, val_size)
	memcpy_ref_keys := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	memcpy_ref_vals := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, blk_clone, ptr_i8, memcpy_ref_keys, new_keys, old_keys, key_bytes)
	b.block_instr4(.call, blk_clone, ptr_i8, memcpy_ref_vals, new_vals, old_vals, val_bytes)

	new_keys_ptr := b.map_state_field_ptr(blk_clone, new_state, 0)
	new_vals_ptr := b.map_state_field_ptr(blk_clone, new_state, 1)
	b.block_instr2(.store, blk_clone, b.void_type, new_keys, new_keys_ptr)
	b.block_instr2(.store, blk_clone, b.void_type, new_vals, new_vals_ptr)
	b.map_state_store_i64(blk_clone, new_state, 2, cap)
	b.map_state_store_i64(blk_clone, new_state, 3, len)
	b.map_state_store_i64(blk_clone, new_state, 4, key_size)
	b.map_state_store_i64(blk_clone, new_state, 5, val_size)
	mut result_block := blk_clone
	if b.m.target.ptr_size == 8 {
		for field_idx in 9 .. 13 {
			old_field_ptr := b.map_state_field_ptr(blk_clone, old_state, field_idx)
			field_type := b.m.type_store.types[b.map_state_type].fields[field_idx]
			field_value := b.block_instr1(.load, blk_clone, field_type, old_field_ptr)
			new_field_ptr := b.map_state_field_ptr(blk_clone, new_state, field_idx)
			b.block_instr2(.store, blk_clone, b.void_type, field_value, new_field_ptr)
		}
		for field_idx in [13, 14] {
			old_field_ptr := b.map_state_field_ptr(blk_clone, old_state, field_idx)
			field_value := b.block_instr1(.load, blk_clone, b.u8_type, old_field_ptr)
			new_field_ptr := b.map_state_field_ptr(blk_clone, new_state, field_idx)
			b.block_instr2(.store, blk_clone, b.void_type, field_value, new_field_ptr)
		}
		old_count_ptr := b.block_struct_field_ptr(blk_clone, old_state, b.map_state_type, 13)
		old_count := b.block_instr1(.load, blk_clone, b.i32_type, old_count_ptr)
		new_count_ptr := b.block_struct_field_ptr(blk_clone, new_state, b.map_state_type, 13)
		b.block_instr2(.store, blk_clone, b.void_type, old_count, new_count_ptr)
		old_deletes_ptr := b.map_dense_field_ptr(blk_clone, old_state, 4)
		old_deletes := b.block_instr1(.load, blk_clone, b.u32_type, old_deletes_ptr)
		new_deletes_ptr := b.map_dense_field_ptr(blk_clone, new_state, 4)
		b.block_instr2(.store, blk_clone, b.void_type, old_deletes, new_deletes_ptr)
		has_deletes := b.block_instr2(.ne, blk_clone, b.i1_type, old_deletes,
			b.m.get_or_add_const(b.u32_type, '0'))
		blk_bitmap := b.m.add_block(func_id, 'map_clone_bitmap')
		blk_ready := b.m.add_block(func_id, 'map_clone_ready')
		result_block = blk_ready
		old_bitmap_ptr := b.map_dense_field_ptr(blk_clone, old_state, 7)
		new_bitmap_ptr := b.map_dense_field_ptr(blk_clone, new_state, 7)
		b.block_instr3(.br, blk_clone, b.void_type, has_deletes, ValueID(blk_bitmap),
			ValueID(blk_ready))
		one_byte := b.m.get_or_add_const(b.i64_type, '1')
		bitmap := b.block_instr3(.call, blk_bitmap, ptr_i8, calloc_ref, cap, one_byte)
		old_bitmap := b.block_instr1(.load, blk_bitmap, ptr_i8, old_bitmap_ptr)
		b.block_instr4(.call, blk_bitmap, ptr_i8, memcpy_ref_keys, bitmap, old_bitmap, cap)
		b.block_instr2(.store, blk_bitmap, b.void_type, bitmap, new_bitmap_ptr)
		b.block_instr1(.jmp, blk_bitmap, b.void_type, ValueID(blk_ready))
	}

	result_slot := b.block_instr0(.alloca, result_block, ptr_map)
	result_state_field := b.block_struct_field_ptr(result_block, result_slot, b.map_type, 0)
	b.block_instr2(.store, result_block, b.void_type, new_state, result_state_field)
	result := b.block_instr1(.load, result_block, b.map_type, result_slot)
	b.block_instr1(.ret, result_block, b.void_type, result)

	empty_result := b.block_instr1(.load, blk_empty, b.map_type, m)
	b.block_instr1(.ret, blk_empty, b.void_type, empty_result)
}

// generate_map_move_body moves the simplified SSA map header out of `m` and zeroes `m`.
fn (mut b Builder) generate_map_move_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	ptr_state := b.m.type_store.get_ptr(b.map_state_type)
	entry := b.m.add_block(func_id, 'entry')
	m := b.func_add_argument(func_id, ptr_map, 'm')
	state_field := b.block_struct_field_ptr(entry, m, b.map_type, 0)
	state := b.block_instr1(.load, entry, ptr_state, state_field)

	zero_state := b.m.get_or_add_const(ptr_state, '0')
	has_state := b.block_instr2(.ne, entry, b.i1_type, state, zero_state)
	blk_reset := b.m.add_block(func_id, 'map_move_reset_source')
	blk_done := b.m.add_block(func_id, 'map_move_done')
	b.block_instr3(.br, entry, b.void_type, has_state, ValueID(blk_reset), ValueID(blk_done))

	key_size := b.map_state_load_i64(blk_reset, state, 4)
	val_size := b.map_state_load_i64(blk_reset, state, 5)
	fresh_state := b.emit_map_state_alloc(blk_reset, key_size, val_size)
	if b.m.target.ptr_size == 8 {
		// Keep the source map usable after the move with the same hashing,
		// cloning, freeing and element alignment guarantees as a fresh map.
		key_alignment_ptr := b.map_state_field_ptr(blk_reset, state, 13)
		val_alignment_ptr := b.map_state_field_ptr(blk_reset, state, 14)
		key_alignment8 := b.block_instr1(.load, blk_reset, b.u8_type, key_alignment_ptr)
		val_alignment8 := b.block_instr1(.load, blk_reset, b.u8_type, val_alignment_ptr)
		key_alignment_exponent := b.block_instr1(.zext, blk_reset, b.i64_type, key_alignment8)
		val_alignment_exponent := b.block_instr1(.zext, blk_reset, b.i64_type, val_alignment8)
		alignment_one := b.m.get_or_add_const(b.i64_type, '1')
		key_alignment := b.block_instr2(.shl, blk_reset, b.i64_type, alignment_one,
			key_alignment_exponent)
		val_alignment := b.block_instr2(.shl, blk_reset, b.i64_type, alignment_one,
			val_alignment_exponent)
		fresh_keys_ptr := b.map_state_field_ptr(blk_reset, fresh_state, 0)
		fresh_vals_ptr := b.map_state_field_ptr(blk_reset, fresh_state, 1)
		default_keys := b.block_instr1(.load, blk_reset, ptr_i8, fresh_keys_ptr)
		default_vals := b.block_instr1(.load, blk_reset, ptr_i8, fresh_vals_ptr)
		free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
		b.block_instr2(.call, blk_reset, b.void_type, free_ref, default_keys)
		b.block_instr2(.call, blk_reset, b.void_type, free_ref, default_vals)
		default_cap := b.m.get_or_add_const(b.i64_type, '8')
		aligned_keys := b.map_alloc_aligned_buffer(blk_reset, default_cap, key_size,
			key_alignment)
		aligned_vals := b.map_alloc_aligned_buffer(blk_reset, default_cap, val_size,
			val_alignment)
		b.block_instr2(.store, blk_reset, b.void_type, aligned_keys, fresh_keys_ptr)
		b.block_instr2(.store, blk_reset, b.void_type, aligned_vals, fresh_vals_ptr)
		has_string_keys_ptr := b.map_state_field_ptr(blk_reset, state, 8)
		has_string_keys := b.block_instr1(.load, blk_reset,
			b.m.type_store.types[b.map_state_type].fields[16], has_string_keys_ptr)
		new_has_string_keys_ptr := b.map_state_field_ptr(blk_reset, fresh_state, 8)
		b.block_instr2(.store, blk_reset, b.void_type, has_string_keys, new_has_string_keys_ptr)
		for field_idx in 9 .. 13 {
			old_field_ptr := b.map_state_field_ptr(blk_reset, state, field_idx)
			field_type := b.m.type_store.types[b.map_state_type].fields[field_idx]
			field_value := b.block_instr1(.load, blk_reset, field_type, old_field_ptr)
			new_field_ptr := b.map_state_field_ptr(blk_reset, fresh_state, field_idx)
			b.block_instr2(.store, blk_reset, b.void_type, field_value, new_field_ptr)
		}
		for field_idx in [13, 14] {
			old_field_ptr := b.map_state_field_ptr(blk_reset, state, field_idx)
			field_value := b.block_instr1(.load, blk_reset, b.u8_type, old_field_ptr)
			new_field_ptr := b.map_state_field_ptr(blk_reset, fresh_state, field_idx)
			b.block_instr2(.store, blk_reset, b.void_type, field_value, new_field_ptr)
		}
	}
	b.block_instr2(.store, blk_reset, b.void_type, fresh_state, state_field)
	b.block_instr1(.jmp, blk_reset, b.void_type, ValueID(blk_done))

	result_slot := b.block_instr0(.alloca, blk_done, ptr_map)
	result_state_field := b.block_struct_field_ptr(blk_done, result_slot, b.map_type, 0)
	b.block_instr2(.store, blk_done, b.void_type, state, result_state_field)
	result := b.block_instr1(.load, blk_done, b.map_type, result_slot)
	b.block_instr1(.ret, blk_done, b.void_type, result)
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
	src := b.block_instr1(.load, blk_copy, ptr_i8, src_ptr)
	len := b.map_state_load_i64(blk_copy, state, 3)
	count := if b.m.target.ptr_size == 8 {
		count_ptr := b.block_struct_field_ptr(blk_copy, state, b.map_state_type, 13)
		count32 := b.block_instr1(.load, blk_copy, b.i32_type, count_ptr)
		b.block_instr1(.zext, blk_copy, b.i64_type, count32)
	} else {
		len
	}
	elem_size := b.map_state_load_i64(blk_copy, state, size_field)
	alignment_field := if data_field == 0 { 13 } else { 14 }
	alignment_ptr := b.map_state_field_ptr(blk_copy, state, alignment_field)
	alignment8 := b.block_instr1(.load, blk_copy, b.u8_type, alignment_ptr)
	alignment_exponent := b.block_instr1(.zext, blk_copy, b.i64_type, alignment8)
	alignment_one := b.m.get_or_add_const(b.i64_type, '1')
	alignment := b.block_instr2(.shl, blk_copy, b.i64_type, alignment_one, alignment_exponent)
	arr := if aligned_id := b.fn_ids['__new_array_aligned'] {
		// Map exports retain the key/value alignment, including the aligned
		// ArrayDataHeader ABI used by `array.free`.
		aligned_ref := b.m.add_value(.func_ref, b.array_type, '__new_array_aligned', aligned_id)
		b.m.add_instr(.call, blk_copy, b.array_type,
			[aligned_ref, count, count, elem_size, alignment])
	} else {
		array_new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
		b.block_instr4(.call, blk_copy, b.array_type, array_new_ref, elem_size, count, count)
	}
	arr_slot := b.block_instr0(.alloca, blk_copy, ptr_array)
	b.block_instr2(.store, blk_copy, b.void_type, arr, arr_slot)
	data_ptr := b.block_struct_field_ptr(blk_copy, arr_slot, b.array_type, 0)
	data := b.block_instr1(.load, blk_copy, ptr_i8, data_ptr)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	if b.m.target.ptr_size == 8 {
		i_slot := b.block_instr0(.alloca, blk_copy, b.m.type_store.get_ptr(b.i64_type))
		out_slot := b.block_instr0(.alloca, blk_copy, b.m.type_store.get_ptr(b.i64_type))
		zero64 := b.m.get_or_add_const(b.i64_type, '0')
		b.block_instr2(.store, blk_copy, b.void_type, zero64, i_slot)
		b.block_instr2(.store, blk_copy, b.void_type, zero64, out_slot)
		loop := b.m.add_block(func_id, 'map_array_loop')
		check_deletes := b.m.add_block(func_id, 'map_array_check_deletes')
		check_live := b.m.add_block(func_id, 'map_array_check_live')
		body := b.m.add_block(func_id, 'map_array_body')
		next := b.m.add_block(func_id, 'map_array_next')
		done := b.m.add_block(func_id, 'map_array_done')
		b.block_instr1(.jmp, blk_copy, b.void_type, ValueID(loop))
		i := b.block_instr1(.load, loop, b.i64_type, i_slot)
		more := b.block_instr2(.lt, loop, b.i1_type, i, len)
		b.block_instr3(.br, loop, b.void_type, more, ValueID(check_deletes),
			ValueID(done))
		deletes_ptr := b.map_dense_field_ptr(check_deletes, state, 4)
		deletes := b.block_instr1(.load, check_deletes, b.u32_type, deletes_ptr)
		has_deletes := b.block_instr2(.ne, check_deletes, b.i1_type, deletes,
			b.m.get_or_add_const(b.u32_type, '0'))
		b.block_instr3(.br, check_deletes, b.void_type, has_deletes,
			ValueID(check_live), ValueID(body))
		bitmap_ptr := b.map_dense_field_ptr(check_live, state, 7)
		bitmap := b.block_instr1(.load, check_live, ptr_i8, bitmap_ptr)
		flag_ptr := b.block_instr2(.add, check_live, ptr_i8, bitmap, i)
		flag := b.block_instr1(.load, check_live, b.i8_type, flag_ptr)
		live := b.block_instr2(.eq, check_live, b.i1_type, flag,
			b.m.get_or_add_const(b.i8_type, '0'))
		b.block_instr3(.br, check_live, b.void_type, live, ValueID(body),
			ValueID(next))
		out := b.block_instr1(.load, body, b.i64_type, out_slot)
		src_offset := b.block_instr2(.mul, body, b.i64_type, i, elem_size)
		dst_offset := b.block_instr2(.mul, body, b.i64_type, out, elem_size)
		src_elem := b.block_instr2(.add, body, ptr_i8, src, src_offset)
		dst_elem := b.block_instr2(.add, body, ptr_i8, data, dst_offset)
		b.block_instr4(.call, body, ptr_i8, memcpy_ref, dst_elem, src_elem, elem_size)
		next_out := b.block_instr2(.add, body, b.i64_type, out,
			b.m.get_or_add_const(b.i64_type, '1'))
		b.block_instr2(.store, body, b.void_type, next_out, out_slot)
		b.block_instr1(.jmp, body, b.void_type, ValueID(next))
		next_i := b.block_instr2(.add, next, b.i64_type, i,
			b.m.get_or_add_const(b.i64_type, '1'))
		b.block_instr2(.store, next, b.void_type, next_i, i_slot)
		b.block_instr1(.jmp, next, b.void_type, ValueID(loop))
		result := b.block_instr1(.load, done, b.array_type, arr_slot)
		b.block_instr1(.ret, done, b.void_type, result)
	} else {
		byte_len := b.block_instr2(.mul, blk_copy, b.i64_type, len, elem_size)
		b.block_instr4(.call, blk_copy, ptr_i8, memcpy_ref, data, src, byte_len)
		result := b.block_instr1(.load, blk_copy, b.array_type, arr_slot)
		b.block_instr1(.ret, blk_copy, b.void_type, result)
	}

	zero := b.m.get_or_add_const(b.i64_type, '0')
	empty_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
	empty := b.block_instr4(.call, blk_empty, b.array_type, empty_ref, elem_size_arg, zero, zero)
	b.block_instr1(.ret, blk_empty, b.void_type, empty)
}

// register_u8_runtime_stubs updates register u8 runtime stubs state for ssa.
fn (mut b Builder) register_u8_runtime_stubs() {
	mut p1 := []TypeID{}
	p1 << b.i8_type
	for name in ['u8.is_digit', 'u8.is_letter', 'u8.is_alnum', 'u8.is_capital'] {
		if func_id := b.register_runtime_function(name, b.i1_type, p1) {
			b.generate_u8_predicate_body(func_id, name)
		}
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
	if alloc_id := b.register_runtime_function('_ht_alloc', b.void_type, p2) {
		b.generate_void_noop_body(alloc_id)
	}
	mut p1 := []TypeID{}
	p1 << ptr_i8
	if free_id := b.register_runtime_function('_ht_free', b.void_type, p1) {
		b.generate_void_noop_body(free_id)
	}
}

// generate_void_noop_body supports generate void noop body handling for Builder.
fn (mut b Builder) generate_void_noop_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	b.block_instr0(.ret, entry, b.void_type)
}

// register_process_capture_helpers supplies the C header's process capture helpers for native output.
fn (mut b Builder) register_process_capture_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	if b.m.target.ptr_size == 4 {
		mut p3 := []TypeID{}
		p3 << ptr_i8
		p3 << ptr_i8
		p3 << ptr_i8
		for name in ['v_os_execute_capture_start', 'v_os_exec_capture_start'] {
			if func_id := b.register_runtime_function(name, b.i64_type, p3) {
				b.generate_const_i64_body(func_id, '1')
			}
		}
	} else if !b.use_native_c_sources {
		ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
		ptr_argv := b.m.type_store.get_ptr(ptr_i8)
		b.register_native_process_externs()
		execute_id := b.register_synthetic_c_function('v_os_execute_capture_start', b.i64_type,
			[ptr_i8, ptr_i64, ptr_i64])
		b.generate_native_process_capture_body(execute_id, true)
		exec_id := b.register_synthetic_c_function('v_os_exec_capture_start', b.i64_type,
			[ptr_argv, ptr_i64, ptr_i64])
		b.generate_native_process_capture_body(exec_id, false)
	} else {
		ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
		ptr_argv := b.m.type_store.get_ptr(ptr_i8)
		b.register_extern('v_os_execute_capture_start', b.i64_type, [ptr_i8, ptr_i64, ptr_i64])
		b.register_extern('v_os_exec_capture_start', b.i64_type, [ptr_argv, ptr_i64, ptr_i64])
	}
}

// register_file_check_stubs updates register file check stubs state for ssa.
fn (mut b Builder) register_file_check_stubs() {
	mut p1 := []TypeID{}
	p1 << b.i64_type
	for name in ['check_fwrite', 'os.check_fwrite', 'check_fread', 'os.check_fread'] {
		if func_id := b.register_runtime_function(name, b.i64_type, p1) {
			b.generate_identity_i64_body(func_id)
		}
	}
}

// register_modulecache_stubs makes the C-only metadata helper fall back to content hashing
// when V3 itself is compiled by the native backend.
fn (mut b Builder) register_modulecache_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)
	mut params := []TypeID{cap: 8}
	params << ptr_i8
	for _ in 0 .. 7 {
		params << ptr_u64
	}
	if func_id := b.register_runtime_c_function('v3_modulecache_file_metadata', b.i64_type, params) {
		b.generate_const_i64_with_params_body(func_id, params, '0')
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
	if zero_id := b.register_runtime_function('FD_ZERO', b.void_type, fd_set_params) {
		b.generate_void_noop_with_params_body(zero_id, fd_set_params)
	}

	mut fd_check_params := []TypeID{}
	fd_check_params << b.i64_type
	fd_check_params << ptr_i8
	if set_id := b.register_runtime_function('FD_SET', b.void_type, fd_check_params) {
		b.generate_void_noop_with_params_body(set_id, fd_check_params)
	}
	if isset_id := b.register_runtime_function('FD_ISSET', b.i64_type, fd_check_params) {
		b.generate_const_i64_with_params_body(isset_id, fd_check_params, '0')
	}
}

// register_signal_macro_stubs updates register signal macro stubs state for ssa.
fn (mut b Builder) register_signal_macro_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut params := []TypeID{}
	params << b.i64_type
	params << ptr_i8
	name := 'v_signal_with_handler_cast'
	func_id := b.register_runtime_c_function(name, ptr_i8, params) or { return }
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
		if is_dir_id := b.register_runtime_function(name, b.i1_type, p1) {
			b.generate_os_stat_kind_body(is_dir_id, 'stat', '16384')
		}
	}
	for name in ['is_link', 'os.is_link'] {
		if is_link_id := b.register_runtime_function(name, b.i1_type, p1) {
			b.generate_os_stat_kind_body(is_link_id, 'lstat', '40960')
		}
	}
	ls_result_type := b.option_type_id(flat.source_type_id('[]string'), true)
	for name in ['ls', 'os.ls'] {
		if ls_id := b.register_runtime_function(name, ls_result_type, p1) {
			b.generate_os_ls_body(ls_id, ls_result_type)
		}
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
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
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

	mut mode_offset := '4'
	$if linux {
		// Linux x86_64 struct stat stores st_mode after dev, ino and nlink.
		mode_offset = '24'
	}
	mode_off := b.m.get_or_add_const(b.i64_type, mode_offset)
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

	str_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	zero64 := b.m.get_or_add_const(b.i64_type, '0')
	cap := b.m.get_or_add_const(b.i64_type, '50')
	array_new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
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

	mut name_offset := '21'
	$if linux {
		// Linux x86_64 dirent: ino(8), off(8), reclen(2), type(1), d_name.
		name_offset = '19'
	}
	name_off := b.m.get_or_add_const(b.i64_type, name_offset)
	name_ptr := b.block_instr2(.get_element_ptr, blk_check, ptr_i8, ent, name_off)
	first := b.block_instr1(.load, blk_check, b.i8_type, name_ptr)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	has_name := b.block_instr2(.ne, blk_check, b.i1_type, first, zero8)
	blk_push := b.m.add_block(func_id, 'ls_push')
	blk_next := b.m.add_block(func_id, 'ls_next')
	blk_dot := b.m.add_block(func_id, 'ls_dot')
	blk_second := b.m.add_block(func_id, 'ls_second')
	blk_double_dot := b.m.add_block(func_id, 'ls_double_dot')
	blk_third := b.m.add_block(func_id, 'ls_third')
	b.block_instr3(.br, blk_check, b.void_type, has_name, ValueID(blk_dot), ValueID(blk_next))

	// readdir includes both self and parent entries; callers such as rmdir_all
	// recurse over this result, so exclude only the exact names "." and "..".
	dot := b.m.get_or_add_const(b.i8_type, '46')
	starts_with_dot := b.block_instr2(.eq, blk_dot, b.i1_type, first, dot)
	b.block_instr3(.br, blk_dot, b.void_type, starts_with_dot, ValueID(blk_second),
		ValueID(blk_push))
	one := b.m.get_or_add_const(b.i64_type, '1')
	second_ptr := b.block_instr2(.get_element_ptr, blk_second, ptr_i8, name_ptr, one)
	second := b.block_instr1(.load, blk_second, b.i8_type, second_ptr)
	is_self := b.block_instr2(.eq, blk_second, b.i1_type, second, zero8)
	b.block_instr3(.br, blk_second, b.void_type, is_self, ValueID(blk_next),
		ValueID(blk_double_dot))
	second_is_dot := b.block_instr2(.eq, blk_double_dot, b.i1_type, second, dot)
	b.block_instr3(.br, blk_double_dot, b.void_type, second_is_dot, ValueID(blk_third),
		ValueID(blk_push))
	two := b.m.get_or_add_const(b.i64_type, '2')
	third_ptr := b.block_instr2(.get_element_ptr, blk_third, ptr_i8, name_ptr, two)
	third := b.block_instr1(.load, blk_third, b.i8_type, third_ptr)
	is_parent := b.block_instr2(.eq, blk_third, b.i1_type, third, zero8)
	b.block_instr3(.br, blk_third, b.void_type, is_parent, ValueID(blk_next),
		ValueID(blk_push))

	tos_clone_ref := b.m.add_value(.func_ref, b.str_type, 'tos_clone', b.runtime_fn_id('tos_clone'))
	name_str := b.block_instr2(.call, blk_push, b.str_type, tos_clone_ref, name_ptr)
	name_alloca := b.block_instr0(.alloca, blk_push, ptr_string)
	b.block_instr2(.store, blk_push, b.void_type, name_str, name_alloca)
	name_elem := b.block_instr1(.bitcast, blk_push, ptr_i8, name_alloca)
	array_push_ref := b.m.add_value(.func_ref, b.void_type, 'array_push', b.runtime_fn_id('array_push'))
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
	err_index := b.m.type_store.types[opt_typ].field_names.index('err')
	if err_index >= 0 {
		err_type := b.m.type_store.types[opt_typ].fields[err_index]
		err_ptr := b.block_struct_field_ptr(block_id, alloca, opt_typ, err_index)
		err_value := b.m.get_or_add_const(err_type, '0')
		b.block_instr2(.store, block_id, b.void_type, err_value, err_ptr)
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
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out_data := b.block_instr2(.call, block_id, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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

// register_prealloc_allocator_stubs uses libc allocation for the native backend.
// Scoped preallocation is a C runtime optimization; returning a nil scope keeps its
// callers on their existing no-scope paths without requiring the arena ABI in SSA.
fn (mut b Builder) register_prealloc_allocator_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1 := []TypeID{}
	p1 << b.i64_type
	if malloc_id := b.register_runtime_function('prealloc_malloc', ptr_i8, p1) {
		b.generate_prealloc_malloc_body(malloc_id)
	}
	if calloc_id := b.register_runtime_function('prealloc_calloc', ptr_i8, p1) {
		b.generate_prealloc_calloc_body(calloc_id)
	}

	mut p2 := []TypeID{}
	p2 << b.i64_type
	p2 << b.i64_type
	if malloc_align_id := b.register_runtime_function('prealloc_malloc_align', ptr_i8, p2) {
		b.generate_prealloc_malloc_align_body(malloc_align_id)
	}

	mut p3 := []TypeID{}
	p3 << ptr_i8
	p3 << b.i64_type
	p3 << b.i64_type
	if realloc_id := b.register_runtime_function('prealloc_realloc', ptr_i8, p3) {
		b.generate_prealloc_realloc_body(realloc_id)
	}
	mut realloc_params := []TypeID{}
	realloc_params << ptr_i8
	realloc_params << b.i64_type
	if v_realloc_id := b.register_runtime_function('v_realloc', ptr_i8, realloc_params) {
		b.generate_libc_realloc_body(v_realloc_id)
	}

	if scope_begin_id := b.register_runtime_function('prealloc_scope_begin', ptr_i8, []TypeID{}) {
		b.generate_const_ptr_with_params_body(scope_begin_id, []TypeID{}, ptr_i8, '0')
	}
}

fn (mut b Builder) generate_prealloc_malloc_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	n := b.func_add_argument(func_id, b.i64_type, 'n')
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	result := b.block_instr2(.call, entry, ptr_i8, malloc_ref, n)
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) generate_prealloc_calloc_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	n := b.func_add_argument(func_id, b.i64_type, 'n')
	one := b.m.get_or_add_const(b.i64_type, '1')
	calloc_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.runtime_fn_id('calloc'))
	result := b.block_instr3(.call, entry, ptr_i8, calloc_ref, one, n)
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) generate_prealloc_malloc_align_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	n := b.func_add_argument(func_id, b.i64_type, 'n')
	_ := b.func_add_argument(func_id, b.i64_type, 'align')
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	result := b.block_instr2(.call, entry, ptr_i8, malloc_ref, n)
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) generate_prealloc_realloc_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	old_data := b.func_add_argument(func_id, ptr_i8, 'old_data')
	_ := b.func_add_argument(func_id, b.i64_type, 'old_size')
	new_size := b.func_add_argument(func_id, b.i64_type, 'new_size')
	realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
	result := b.block_instr3(.call, entry, ptr_i8, realloc_ref, old_data, new_size)
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) generate_libc_realloc_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	old_data := b.func_add_argument(func_id, ptr_i8, 'old_data')
	new_size := b.func_add_argument(func_id, b.i64_type, 'new_size')
	realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
	result := b.block_instr3(.call, entry, ptr_i8, realloc_ref, old_data, new_size)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_prealloc_atomic_stubs updates register prealloc atomic stubs state for ssa.
fn (mut b Builder) register_prealloc_atomic_stubs() {
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	mut p1_ptr := []TypeID{}
	p1_ptr << ptr_i64
	if load_id := b.register_runtime_function('v_prealloc_atomic_load_i32', b.i64_type, p1_ptr) {
		b.generate_atomic_load_i64_body(load_id)
	}

	mut p2 := []TypeID{}
	p2 << ptr_i64
	p2 << b.i64_type
	if add_id := b.register_runtime_function('v_prealloc_atomic_add_i32', b.i64_type, p2) {
		b.generate_atomic_add_i64_body(add_id)
	}
	if store_id := b.register_runtime_function('v_prealloc_atomic_store_i32', b.i64_type, p2) {
		b.generate_atomic_store_i64_body(store_id)
	}

	mut p3 := []TypeID{}
	p3 << ptr_i64
	p3 << b.i64_type
	p3 << b.i64_type
	if cas_id := b.register_runtime_function('v_prealloc_atomic_cas_i32', b.i64_type, p3) {
		b.generate_atomic_cas_i64_body(cas_id)
	}
}

fn (mut b Builder) register_atomic_builtin_stubs() {
	b.register_atomic_scalar_stubs('byte', b.u8_type)
	b.register_atomic_scalar_stubs('u16', b.u16_type)
	b.register_atomic_scalar_stubs('u32', b.u32_type)
	b.register_atomic_scalar_stubs('u64', b.u64_type)
	b.register_atomic_ptr_stubs()

	mut fence_params := []TypeID{}
	fence_params << b.i64_type
	if fence_id := b.register_runtime_c_function('atomic_thread_fence', b.void_type, fence_params) {
		b.generate_void_return_body(fence_id)
	}
	if cpu_relax_id := b.register_runtime_c_function('cpu_relax', b.void_type, []TypeID{}) {
		b.generate_void_return_body(cpu_relax_id)
	}
}

fn (mut b Builder) register_atomic_scalar_stubs(suffix string, typ TypeID) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1 := []TypeID{}
	p1 << ptr_i8
	if load_id := b.register_runtime_c_function('atomic_load_${suffix}', typ, p1) {
		b.generate_atomic_load_body(load_id, typ)
	}

	mut p2 := []TypeID{}
	p2 << ptr_i8
	p2 << typ
	if store_id := b.register_runtime_c_function('atomic_store_${suffix}', b.void_type, p2) {
		b.generate_atomic_store_body(store_id, typ)
	}
	if exchange_id := b.register_runtime_c_function('atomic_exchange_${suffix}', typ, p2) {
		b.generate_atomic_exchange_body(exchange_id, typ)
	}
	if fetch_add_id := b.register_runtime_c_function('atomic_fetch_add_${suffix}', typ, p2) {
		b.generate_atomic_fetch_add_body(fetch_add_id, typ)
	}
	if fetch_sub_id := b.register_runtime_c_function('atomic_fetch_sub_${suffix}', typ, p2) {
		b.generate_atomic_fetch_sub_body(fetch_sub_id, typ)
	}

	mut p3 := []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << typ
	if strong_id := b.register_runtime_c_function('atomic_compare_exchange_strong_${suffix}',
		b.i1_type, p3) {
		b.generate_atomic_compare_exchange_body(strong_id, typ)
	}
	if weak_id := b.register_runtime_c_function('atomic_compare_exchange_weak_${suffix}', b.i1_type, p3) {
		b.generate_atomic_compare_exchange_body(weak_id, typ)
	}
}

fn (mut b Builder) register_atomic_ptr_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p1 := []TypeID{}
	p1 << ptr_i8
	if load_id := b.register_runtime_c_function('atomic_load_ptr', ptr_i8, p1) {
		b.generate_atomic_load_ptr_body(load_id)
	}

	mut p2 := []TypeID{}
	p2 << ptr_i8
	p2 << ptr_i8
	if store_id := b.register_runtime_c_function('atomic_store_ptr', b.void_type, p2) {
		b.generate_atomic_store_ptr_body(store_id)
	}
	if exchange_id := b.register_runtime_c_function('atomic_exchange_ptr', ptr_i8, p2) {
		b.generate_atomic_exchange_ptr_body(exchange_id)
	}
	if fetch_add_id := b.register_runtime_c_function('atomic_fetch_add_ptr', ptr_i8, p2) {
		b.generate_atomic_fetch_add_ptr_body(fetch_add_id)
	}
	if fetch_sub_id := b.register_runtime_c_function('atomic_fetch_sub_ptr', ptr_i8, p2) {
		b.generate_atomic_fetch_sub_ptr_body(fetch_sub_id)
	}

	mut p3 := []TypeID{}
	p3 << ptr_i8
	p3 << ptr_i8
	p3 << b.i64_type
	if strong_id := b.register_runtime_c_function('atomic_compare_exchange_strong_ptr', b.i1_type, p3) {
		b.generate_atomic_compare_exchange_ptr_body(strong_id)
	}
	if weak_id := b.register_runtime_c_function('atomic_compare_exchange_weak_ptr', b.i1_type, p3) {
		b.generate_atomic_compare_exchange_ptr_body(weak_id)
	}
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
	if array_str_id := b.register_runtime_function('Array_str', b.str_type, p1) {
		b.generate_const_string_body(array_str_id, '[]')
	}
	for name in ['bytestr', 'Array_u8__bytestr', '[]u8.bytestr'] {
		if bytestr_id := b.register_runtime_function(name, b.str_type, p1) {
			b.generate_array_bytestr_body(bytestr_id)
		}
	}
	for name in ['Array_u8__hex', '[]u8.hex'] {
		if b.m.target.ptr_size == 4 {
			if array_hex_id := b.register_runtime_function(name, b.str_type, p1) {
				b.generate_const_string_body(array_hex_id, '')
			}
		} else {
			array_hex_id := b.register_synthetic_function(name, b.str_type, p1)
			b.generate_array_byte_hex_body(array_hex_id)
		}
	}
	for name in ['Array_rune__string', '[]rune.string'] {
		if rune_string_id := b.register_runtime_function(name, b.str_type, p1) {
			b.generate_array_rune_string_body(rune_string_id)
		}
	}

	mut p2 := []TypeID{}
	p2 << b.array_type
	p2 << b.str_type
	for name in ['array_string_join', 'Array_string__join'] {
		if func_id := b.register_runtime_function(name, b.str_type, p2) {
			b.generate_array_string_join_body(func_id)
		}
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
		b.runtime_fn_id('strings.new_builder'))
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
		b.runtime_fn_id('strings.Builder.write_string'))
	b.block_instr3(.call, write_sep, b.void_type, write_ref, builder_alloca, sep)
	b.block_instr1(.jmp, write_sep, b.void_type, ValueID(write_elem))

	elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	offset := b.block_instr2(.mul, write_elem, b.i64_type, i_val, elem_size)
	elem_ptr_raw := b.block_instr2(.add, write_elem, ptr_i8, data, offset)
	elem_ptr := b.block_instr1(.bitcast, write_elem, b.m.type_store.get_ptr(b.str_type),
		elem_ptr_raw)
	elem := b.block_instr1(.load, write_elem, b.str_type, elem_ptr)
	write_ref2 := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_string',
		b.runtime_fn_id('strings.Builder.write_string'))
	b.block_instr3(.call, write_elem, b.void_type, write_ref2, builder_alloca, elem)
	next_i := b.block_instr2(.add, write_elem, b.i64_type, i_val, one)
	b.block_instr2(.store, write_elem, b.void_type, next_i, i_alloca)
	b.block_instr1(.jmp, write_elem, b.void_type, ValueID(loop))

	str_ref := b.m.add_value(.func_ref, b.str_type, 'strings.Builder.str',
		b.runtime_fn_id('strings.Builder.str'))
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
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.runtime_fn_id('malloc'))
	out_data := b.block_instr2(.call, entry, ptr_i8, malloc_ref, alloc_len)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, entry, ptr_i8, memcpy_ref, out_data, data, len)
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	term_ptr := b.block_instr2(.add, entry, ptr_i8, out_data, len)
	b.block_instr2(.store, entry, b.void_type, zero8, term_ptr)

	result := b.emit_make_string(entry, out_data, len, 0)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_array_rune_string_body encodes rune elements into a builder and
// returns their owned UTF-8 string.
fn (mut b Builder) generate_array_rune_string_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_i32 := b.m.type_store.get_ptr(b.i32_type)
	ptr_i64 := b.m.type_store.get_ptr(b.i64_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')

	arr_alloca := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, arr, arr_alloca)
	data_ptr := b.block_struct_field_ptr(entry, arr_alloca, b.array_type, 0)
	len_ptr := b.block_struct_field_ptr(entry, arr_alloca, b.array_type, 2)
	data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)

	new_ref := b.m.add_value(.func_ref, b.array_type, 'strings.new_builder',
		b.runtime_fn_id('strings.new_builder'))
	builder := b.block_instr2(.call, entry, b.array_type, new_ref, len)
	builder_alloca := b.block_instr0(.alloca, entry, ptr_array)
	b.block_instr2(.store, entry, b.void_type, builder, builder_alloca)

	i_alloca := b.block_instr0(.alloca, entry, ptr_i64)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	rune_size := b.m.get_or_add_const(b.i64_type, '4')
	b.block_instr2(.store, entry, b.void_type, zero, i_alloca)
	loop := b.m.add_block(func_id, 'array_rune_string_loop')
	body := b.m.add_block(func_id, 'array_rune_string_body')
	done := b.m.add_block(func_id, 'array_rune_string_done')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(loop))

	i_val := b.block_instr1(.load, loop, b.i64_type, i_alloca)
	more := b.block_instr2(.lt, loop, b.i1_type, i_val, len)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(body), ValueID(done))

	offset := b.block_instr2(.mul, body, b.i64_type, i_val, rune_size)
	rune_ptr_raw := b.block_instr2(.add, body, ptr_i8, data, offset)
	rune_ptr := b.block_instr1(.bitcast, body, ptr_i32, rune_ptr_raw)
	rune_val := b.block_instr1(.load, body, b.i32_type, rune_ptr)
	write_ref := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_rune',
		b.runtime_fn_id('strings.Builder.write_rune'))
	b.block_instr3(.call, body, b.void_type, write_ref, builder_alloca, rune_val)
	next_i := b.block_instr2(.add, body, b.i64_type, i_val, one)
	b.block_instr2(.store, body, b.void_type, next_i, i_alloca)
	b.block_instr1(.jmp, body, b.void_type, ValueID(loop))

	str_ref := b.m.add_value(.func_ref, b.str_type, 'strings.Builder.str',
		b.runtime_fn_id('strings.Builder.str'))
	result := b.block_instr2(.call, done, b.str_type, str_ref, builder_alloca)
	free_ref := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.free',
		b.runtime_fn_id('strings.Builder.free'))
	b.block_instr2(.call, done, b.void_type, free_ref, builder_alloca)
	b.block_instr1(.ret, done, b.void_type, result)
}

fn (mut b Builder) register_at_exit_stub() {
	mut p1 := []TypeID{}
	p1 << b.resolve_type(flat.source_type_id('FnExitCb'))
	result_type := b.option_type_id(flat.source_type_void, true)
	if func_id := b.register_runtime_function('at_exit', result_type, p1) {
		b.generate_at_exit_stub_body(func_id, result_type, p1)
	}
}

fn (mut b Builder) generate_at_exit_stub_body(func_id int, result_type TypeID, params []TypeID) {
	entry := b.m.add_block(func_id, 'at_exit_entry')
	for i, param_type in params {
		_ := b.func_add_argument(func_id, param_type, 'arg${i}')
	}
	result := b.block_option_value(entry, result_type, true, ValueID(0))
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) register_pthread_compat_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	if zero_id := b.register_runtime_c_function('v3_pthread_zero', b.u64_type, []TypeID{}) {
		b.generate_const_body_with_params(zero_id, b.u64_type, '0', []TypeID{})
	}
	mut p_create := []TypeID{}
	p_create << ptr_i8
	p_create << b.i64_type
	p_create << ptr_i8
	p_create << ptr_i8
	if create_id := b.register_runtime_c_function('v3_pthread_create', b.i32_type, p_create) {
		b.generate_const_body_with_params(create_id, b.i32_type, '11', p_create)
	}
	mut p_setkind := []TypeID{}
	p_setkind << ptr_i8
	p_setkind << b.i32_type
	if setkind_id := b.register_runtime_c_function('pthread_rwlockattr_setkind_np', b.i32_type,
		p_setkind) {
		b.generate_const_body_with_params(setkind_id, b.i32_type, '0', p_setkind)
	}
}

fn (mut b Builder) register_rand_prng_interface_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p_recv := []TypeID{}
	p_recv << ptr_i8
	prng_ptr := b.resolve_type(flat.source_type_id('&rand.PRNG'))
	config_type := b.resolve_type(flat.source_type_id('config.PRNGConfigStruct'))
	mut p_new_default := []TypeID{}
	p_new_default << config_type
	for name in ['rand.new_default', 'rand__new_default'] {
		if func_id := b.register_runtime_function(name, prng_ptr, p_new_default) {
			b.generate_const_body_with_params(func_id, prng_ptr, '0', p_new_default)
		}
	}
	mut p_seed := []TypeID{}
	p_seed << ptr_i8
	p_seed << b.array_type
	for name in ['rand.PRNG.seed', 'rand__PRNG__seed'] {
		if func_id := b.register_runtime_function(name, b.void_type, p_seed) {
			b.generate_noop_body(func_id, p_seed)
		}
	}
	for name in ['rand.PRNG.free', 'rand__PRNG__free'] {
		if func_id := b.register_runtime_function(name, b.void_type, p_recv) {
			b.generate_noop_body(func_id, p_recv)
		}
	}
	for name in ['rand.PRNG.u8', 'rand__PRNG__u8'] {
		if func_id := b.register_runtime_function(name, b.u8_type, p_recv) {
			b.generate_const_body_with_params(func_id, b.u8_type, '0', p_recv)
		}
	}
	for name in ['rand.PRNG.u16', 'rand__PRNG__u16'] {
		if func_id := b.register_runtime_function(name, b.u16_type, p_recv) {
			b.generate_const_body_with_params(func_id, b.u16_type, '0', p_recv)
		}
	}
	for name in ['rand.PRNG.u32', 'rand__PRNG__u32'] {
		if func_id := b.register_runtime_function(name, b.u32_type, p_recv) {
			b.generate_const_body_with_params(func_id, b.u32_type, '0', p_recv)
		}
	}
	for name in ['rand.PRNG.u64', 'rand__PRNG__u64'] {
		if func_id := b.register_runtime_function(name, b.u64_type, p_recv) {
			b.generate_const_body_with_params(func_id, b.u64_type, '0', p_recv)
		}
	}
	for name in ['rand.PRNG.block_size', 'rand__PRNG__block_size'] {
		if func_id := b.register_runtime_function(name, b.i32_type, p_recv) {
			b.generate_const_body_with_params(func_id, b.i32_type, '8', p_recv)
		}
	}
}

fn (mut b Builder) register_embed_file_interface_stubs() {
	result_type := b.option_type_id(flat.source_type_id('[]u8'), true)
	mut params := []TypeID{}
	params << b.resolve_type(flat.source_type_id('embed_file.Decoder'))
	params << b.array_type
	func_id := b.register_runtime_function('embed_file.Decoder.decompress', result_type, params) or {
		return
	}
	entry := b.m.add_block(func_id, 'decompress_entry')
	for i, param_type in params {
		_ := b.func_add_argument(func_id, param_type, 'arg${i}')
	}
	result := b.block_option_value(entry, result_type, true, ValueID(0))
	b.block_instr1(.ret, entry, b.void_type, result)
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
	interface_type := b.resolve_type(flat.source_type_id('IError'))
	message_error_type := b.resolve_type(flat.source_type_id('MessageError'))
	ptr_ierror := b.m.type_store.get_ptr(interface_type)
	mut p1 := []TypeID{}
	p1 << ptr_ierror
	if msg_id := b.register_runtime_function('IError.msg', b.str_type, p1) {
		b.generate_ierror_msg_body(msg_id, ptr_ierror)
	}
	if code_id := b.register_runtime_function('IError.code', b.i64_type, p1) {
		b.generate_ierror_code_body(code_id, ptr_ierror)
	}
	mut p_error := []TypeID{}
	p_error << b.str_type
	if error_id := b.register_runtime_function('error', interface_type, p_error) {
		b.generate_error_boxing_body(error_id, interface_type, message_error_type, false)
	}
	mut p_error_code := []TypeID{}
	p_error_code << b.str_type
	p_error_code << b.i32_type
	if error_id := b.register_runtime_function('error_with_code', interface_type, p_error_code) {
		b.generate_error_boxing_body(error_id, interface_type, message_error_type, true)
	}
}

fn (mut b Builder) generate_error_boxing_body(func_id int, interface_type TypeID, message_error_type TypeID, has_code bool) {
	entry := b.m.add_block(func_id, 'entry')
	message := b.func_add_argument(func_id, b.str_type, 'message')
	mut code := ValueID(0)
	if has_code {
		code = b.func_add_argument(func_id, b.i32_type, 'code')
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_message_error := b.m.type_store.get_ptr(message_error_type)
	message_alloca := b.block_instr0(.alloca, entry, ptr_message_error)
	message_ptr := b.block_struct_field_ptr(entry, message_alloca, message_error_type, 0)
	b.block_instr2(.store, entry, b.void_type, message, message_ptr)
	code_ptr := b.block_struct_field_ptr(entry, message_alloca, message_error_type, 1)
	code_value := if has_code {
		code
	} else {
		b.m.get_or_add_const(b.i32_type, '0')
	}
	b.block_instr2(.store, entry, b.void_type, code_value, code_ptr)
	size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(message_error_type)}')
	source := b.block_instr1(.bitcast, entry, ptr_i8, message_alloca)
	memdup_ref := b.m.add_value(.func_ref, ptr_i8, '__ssa_memdup',
		b.fn_ids['__ssa_memdup'])
	object := b.block_instr3(.call, entry, ptr_i8, memdup_ref, source, size)
	ptr_interface := b.m.type_store.get_ptr(interface_type)
	interface_alloca := b.block_instr0(.alloca, entry, ptr_interface)
	object_ptr := b.block_struct_field_ptr(entry, interface_alloca, interface_type, 0)
	b.block_instr2(.store, entry, b.void_type, object, object_ptr)
	type_ptr := b.block_struct_field_ptr(entry, interface_alloca, interface_type, 1)
	dispatch_id := b.interface_dispatch_id(interface_type, flat.source_type_id('MessageError'))
	type_id := b.m.get_or_add_const(b.i32_type, '${dispatch_id}')
	b.block_instr2(.store, entry, b.void_type, type_id, type_ptr)
	boxed_ptr := b.block_struct_field_ptr(entry, interface_alloca, interface_type, 2)
	boxed := b.m.get_or_add_const(b.i1_type, '1')
	b.block_instr2(.store, entry, b.void_type, boxed, boxed_ptr)
	result := b.block_instr1(.load, entry, interface_type, interface_alloca)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_ierror_msg_body supports generate ierror msg body handling for Builder.
fn (mut b Builder) generate_ierror_msg_body(func_id int, ptr_ierror TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	err := b.func_add_argument(func_id, ptr_ierror, 'err')
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	object := b.block_instr1(.bitcast, entry, ptr_i8, err)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	msg_ptr := b.block_instr2(.get_element_ptr, entry, b.m.type_store.get_ptr(b.str_type),
		object, zero)
	msg := b.block_instr1(.load, entry, b.str_type, msg_ptr)
	b.block_instr1(.ret, entry, b.void_type, msg)
}

// generate_ierror_code_body supports generate ierror code body handling for Builder.
fn (mut b Builder) generate_ierror_code_body(func_id int, ptr_ierror TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	err := b.func_add_argument(func_id, ptr_ierror, 'err')
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	object := b.block_instr1(.bitcast, entry, ptr_i8, err)
	offset := b.m.get_or_add_const(b.i64_type, '16')
	code_ptr := b.block_instr2(.get_element_ptr, entry, b.m.type_store.get_ptr(b.i32_type),
		object, offset)
	code := b.block_instr1(.load, entry, b.i32_type, code_ptr)
	result := b.block_instr1(.sext, entry, b.i64_type, code)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// register_fixed_array_contains_stubs reports register_fixed_array_contains_stubs logic in ssa.
fn (mut b Builder) register_fixed_array_contains_stubs() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut p3_string := []TypeID{}
	p3_string << ptr_i8
	p3_string << b.i64_type
	p3_string << b.str_type
	if contains_string_id := b.register_runtime_function('__v3_fixed_array_contains_string', b.i1_type,
		p3_string) {
		b.generate_fixed_array_contains_string_body(contains_string_id)
	}

	mut p3_int := []TypeID{}
	p3_int << ptr_i8
	p3_int << b.i64_type
	p3_int << b.i64_type
	if contains_int_id := b.register_runtime_function('__v3_fixed_array_contains_int', b.i1_type, p3_int) {
		b.generate_const_bool_body(contains_int_id, false)
	}
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

	stride := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	offset := b.block_instr2(.mul, blk_body, b.i64_type, i, stride)
	slot := b.block_instr2(.add, blk_body, ptr_i8, arr, offset)
	slot_string_ptr := b.block_instr1(.bitcast, blk_body, ptr_string, slot)
	slot_string := b.block_instr1(.load, blk_body, b.str_type, slot_string_ptr)
	eq_ref := b.m.add_value(.func_ref, b.void_type, 'string__eq', b.runtime_fn_id('string__eq'))
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
	if index_string_id := b.register_runtime_function('__v3_array_index_string', b.i64_type, p2_string) {
		b.generate_array_index_string_body(index_string_id)
	}
	if contains_string_id := b.register_runtime_function('__v3_array_contains_string', b.i1_type,
		p2_string) {
		b.generate_array_contains_from_index_body(contains_string_id, '__v3_array_index_string', b.str_type)
	}

	mut p2_int := []TypeID{}
	p2_int << b.array_type
	p2_int << b.i64_type
	if index_int_id := b.register_runtime_function('__v3_array_index_int', b.i64_type, p2_int) {
		b.generate_array_index_int_body(index_int_id)
	}
	if contains_int_id := b.register_runtime_function('__v3_array_contains_int', b.i1_type, p2_int) {
		b.generate_array_contains_from_index_body(contains_int_id, '__v3_array_index_int', b.i64_type)
	}
}

// generate_array_contains_from_index_body
// supports helper handling in ssa.
fn (mut b Builder) generate_array_contains_from_index_body(func_id int, index_name string, needle_type TypeID) {
	entry := b.m.add_block(func_id, 'entry')
	arr := b.func_add_argument(func_id, b.array_type, 'arr')
	needle := b.func_add_argument(func_id, needle_type, 'needle')
	index_ref := b.m.add_value(.func_ref, b.void_type, index_name, b.runtime_fn_id(index_name))
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
	eq_ref := b.m.add_value(.func_ref, b.void_type, 'string__eq', b.runtime_fn_id('string__eq'))
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
	empty := b.m.get_or_add_const(b.i64_type, '0')
	null_data := b.m.get_or_add_const(ptr_i8, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	state_size := b.m.get_or_add_const(b.i64_type, b.m.type_size(b.map_state_type).str())
	calloc_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.runtime_fn_id('calloc'))
	state_raw := b.block_instr3(.call, block_id, ptr_i8, calloc_ref, one, state_size)
	state := b.block_instr1(.bitcast, block_id, ptr_state, state_raw)

	keys_ptr := b.map_state_field_ptr(block_id, state, 0)
	vals_ptr := b.map_state_field_ptr(block_id, state, 1)
	b.block_instr2(.store, block_id, b.void_type, null_data, keys_ptr)
	b.block_instr2(.store, block_id, b.void_type, null_data, vals_ptr)
	b.map_state_store_i64(block_id, state, 2, empty)
	b.map_state_store_i64(block_id, state, 3, zero)
	b.map_state_store_i64(block_id, state, 4, key_size)
	b.map_state_store_i64(block_id, state, 5, val_size)
	if b.m.target.ptr_size == 8 {
		dense_type := b.m.type_store.types[b.map_state_type].fields[5]
		dense_ptr := b.block_struct_field_ptr(block_id, state, b.map_state_type, 5)
		dense_key_size_ptr := b.block_struct_field_ptr(block_id, dense_ptr, dense_type, 0)
		dense_val_size_ptr := b.block_struct_field_ptr(block_id, dense_ptr, dense_type, 1)
		b.block_instr2(.store, block_id, b.void_type, b.block_instr1(.trunc, block_id,
			b.i32_type, key_size), dense_key_size_ptr)
		b.block_instr2(.store, block_id, b.void_type, b.block_instr1(.trunc, block_id,
			b.i32_type, val_size), dense_val_size_ptr)
	}
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
	if b.m.target.ptr_size == 8 {
		if field_idx in [0, 1, 2, 3, 13, 14] {
			dense_type := b.m.type_store.types[b.map_state_type].fields[5]
			dense_ptr := b.block_struct_field_ptr(block_id, state_ptr, b.map_state_type, 5)
			dense_index := match field_idx {
				0 { 8 }
				1 { 9 }
				2 { 2 }
				3 { 3 }
				13 { 5 }
				else { 6 }
			}
			return b.block_struct_field_ptr(block_id, dense_ptr, dense_type, dense_index)
		}
		flat_index := match field_idx {
			4 { 0 }
			5 { 1 }
			6 { 14 }
			7 { 15 }
			8 { 16 }
			else { field_idx }
		}
		return b.block_struct_field_ptr(block_id, state_ptr, b.map_state_type, flat_index)
	}
	return b.block_struct_field_ptr(block_id, state_ptr, b.map_state_type, field_idx)
}

// map_dense_field_ptr returns a field pointer in the nested DenseArray layout
// used by the x64 map runtime.
fn (mut b Builder) map_dense_field_ptr(block_id BlockID, state_ptr ValueID, field_idx int) ValueID {
	dense_type := b.m.type_store.types[b.map_state_type].fields[5]
	dense_ptr := b.block_struct_field_ptr(block_id, state_ptr, b.map_state_type, 5)
	return b.block_struct_field_ptr(block_id, dense_ptr, dense_type, field_idx)
}

// map_entry_is_live reports whether a dense key/value position has not been
// tombstoned by delete.
fn (mut b Builder) map_entry_is_live(block_id BlockID, state_ptr ValueID, index ValueID) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	if b.m.target.ptr_size != 8 {
		return b.m.get_or_add_const(b.i1_type, '1')
	}
	zero8 := b.m.get_or_add_const(b.i8_type, '0')
	zero_slot := b.block_instr0(.alloca, block_id, b.m.type_store.get_ptr(b.i8_type))
	b.block_instr2(.store, block_id, b.void_type, zero8, zero_slot)
	deletes_ptr := b.map_dense_field_ptr(block_id, state_ptr, 4)
	deletes32 := b.block_instr1(.load, block_id, b.u32_type, deletes_ptr)
	has_tombstones := b.block_instr2(.ne, block_id, b.i1_type, deletes32,
		b.m.get_or_add_const(b.u32_type, '0'))
	bitmap_ptr := b.map_dense_field_ptr(block_id, state_ptr, 7)
	bitmap := b.block_instr1(.load, block_id, ptr_i8, bitmap_ptr)
	effective_bitmap := b.block_instr3(.select, block_id, ptr_i8, has_tombstones, bitmap,
		zero_slot)
	// The fallback contains one byte, independent of the dense entry index.
	effective_index := b.block_instr3(.select, block_id, b.i64_type, has_tombstones, index,
		b.m.get_or_add_const(b.i64_type, '0'))
	flag_ptr := b.block_instr2(.add, block_id, ptr_i8, effective_bitmap, effective_index)
	flag := b.block_instr1(.load, block_id, b.i8_type, flag_ptr)
	return b.block_instr2(.eq, block_id, b.i1_type, flag, zero8)
}

fn (mut b Builder) map_state_load_i64(block_id BlockID, state_ptr ValueID, field_idx int) ValueID {
	field_ptr := b.map_state_field_ptr(block_id, state_ptr, field_idx)
	if b.m.target.ptr_size == 8 && field_idx in [2, 3, 4, 5] {
		value32 := b.block_instr1(.load, block_id, b.i32_type, field_ptr)
		return b.block_instr1(.zext, block_id, b.i64_type, value32)
	}
	return b.block_instr1(.load, block_id, b.i64_type, field_ptr)
}

fn (mut b Builder) map_state_store_i64(block_id BlockID, state_ptr ValueID, field_idx int, value ValueID) {
	field_ptr := b.map_state_field_ptr(block_id, state_ptr, field_idx)
	if b.m.target.ptr_size == 8 && field_idx in [2, 3, 4, 5] {
		value32 := b.block_instr1(.trunc, block_id, b.i32_type, value)
		b.block_instr2(.store, block_id, b.void_type, value32, field_ptr)
		return
	}
	b.block_instr2(.store, block_id, b.void_type, value, field_ptr)
}

fn (mut b Builder) map_alloc_aligned_buffer(block_id BlockID, count ValueID, elem_size ValueID, alignment ValueID) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	if 'aligned_alloc' !in b.fn_ids {
		b.register_extern('aligned_alloc', ptr_i8, [b.i64_type, b.i64_type])
	}
	minimum_alignment := b.m.get_or_add_const(b.i64_type, '16')
	below_minimum := b.block_instr2(.ult, block_id, b.i1_type, alignment, minimum_alignment)
	safe_alignment := b.block_instr3(.select, block_id, b.i64_type, below_minimum,
		minimum_alignment, alignment)
	size := b.block_instr2(.mul, block_id, b.i64_type, count, elem_size)
	one := b.m.get_or_add_const(b.i64_type, '1')
	align_minus_one := b.block_instr2(.sub, block_id, b.i64_type, safe_alignment, one)
	padded := b.block_instr2(.add, block_id, b.i64_type, size, align_minus_one)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	neg_alignment := b.block_instr2(.sub, block_id, b.i64_type, zero, safe_alignment)
	rounded := b.block_instr2(.and_, block_id, b.i64_type, padded, neg_alignment)
	alloc := b.m.add_value(.func_ref, ptr_i8, 'aligned_alloc', b.fn_ids['aligned_alloc'])
	raw := b.block_instr3(.call, block_id, ptr_i8, alloc, safe_alignment, rounded)
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset', b.runtime_fn_id('memset'))
	zero_byte := b.m.get_or_add_const(b.i32_type, '0')
	b.block_instr4(.call, block_id, ptr_i8, memset_ref, raw, zero_byte, size)
	return raw
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

	alloca_m := b.block_instr0(.alloca, entry, ptr_map)
	state := b.emit_map_state_alloc(entry, key_size, val_size)
	if b.m.target.ptr_size == 8 {
		hash_ptr := b.map_state_field_ptr(entry, state, 9)
		eq_ptr := b.map_state_field_ptr(entry, state, 10)
		clone_ptr := b.map_state_field_ptr(entry, state, 11)
		free_ptr := b.map_state_field_ptr(entry, state, 12)
		b.block_instr2(.store, entry, b.void_type, hash_fn, hash_ptr)
		b.block_instr2(.store, entry, b.void_type, eq_fn, eq_ptr)
		b.block_instr2(.store, entry, b.void_type, clone_fn, clone_ptr)
		b.block_instr2(.store, entry, b.void_type, free_fn, free_ptr)
	}
	state_ptr := b.block_struct_field_ptr(entry, alloca_m, b.map_type, 0)
	b.block_instr2(.store, entry, b.void_type, state, state_ptr)
	result := b.block_instr1(.load, entry, b.map_type, alloca_m)
	b.block_instr1(.ret, entry, b.void_type, result)
}

fn (mut b Builder) generate_new_map_aligned_body(func_id int) {
	entry := b.m.add_block(func_id, 'map_aligned_entry')
	key_size := b.func_add_argument(func_id, b.i64_type, 'key_size')
	val_size := b.func_add_argument(func_id, b.i64_type, 'val_size')
	hash_fn := b.func_add_argument(func_id, b.i64_type, 'hash_fn')
	eq_fn := b.func_add_argument(func_id, b.i64_type, 'eq_fn')
	clone_fn := b.func_add_argument(func_id, b.i64_type, 'clone_fn')
	free_fn := b.func_add_argument(func_id, b.i64_type, 'free_fn')
	key_alignment := b.func_add_argument(func_id, b.i64_type, 'key_alignment')
	value_alignment := b.func_add_argument(func_id, b.i64_type, 'value_alignment')
	new_ref := b.m.add_value(.func_ref, b.map_type, 'new_map',
		b.runtime_fn_id('new_map'))
	mut call_args := []ValueID{}
	call_args << new_ref
	call_args << key_size
	call_args << val_size
	call_args << hash_fn
	call_args << eq_fn
	call_args << clone_fn
	call_args << free_fn
	result := b.m.add_instr(.call, entry, b.map_type, call_args)
	result_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.map_type))
	b.block_instr2(.store, entry, b.void_type, result, result_slot)
	state := b.map_state_ptr(entry, result_slot)
	key_alignment_ptr := b.map_state_field_ptr(entry, state, 13)
	value_alignment_ptr := b.map_state_field_ptr(entry, state, 14)
	exponent_ref := b.m.add_value(.func_ref, b.u8_type, '__ssa_data_alignment_exponent',
		b.fn_ids['__ssa_data_alignment_exponent'])
	key_exponent := b.block_instr2(.call, entry, b.u8_type, exponent_ref, key_alignment)
	value_exponent := b.block_instr2(.call, entry, b.u8_type, exponent_ref, value_alignment)
	b.block_instr2(.store, entry, b.void_type, key_exponent, key_alignment_ptr)
	b.block_instr2(.store, entry, b.void_type, value_exponent, value_alignment_ptr)
	keys_ptr := b.map_state_field_ptr(entry, state, 0)
	vals_ptr := b.map_state_field_ptr(entry, state, 1)
	old_keys := b.block_instr1(.load, entry, b.m.type_store.get_ptr(b.i8_type), keys_ptr)
	old_vals := b.block_instr1(.load, entry, b.m.type_store.get_ptr(b.i8_type), vals_ptr)
	cap_eight := b.m.get_or_add_const(b.i64_type, '8')
	new_keys := b.map_alloc_aligned_buffer(entry, cap_eight, key_size, key_alignment)
	new_vals := b.map_alloc_aligned_buffer(entry, cap_eight, val_size, value_alignment)
	free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
	b.block_instr2(.call, entry, b.void_type, free_ref, old_keys)
	b.block_instr2(.call, entry, b.void_type, free_ref, old_vals)
	b.block_instr2(.store, entry, b.void_type, new_keys, keys_ptr)
	b.block_instr2(.store, entry, b.void_type, new_vals, vals_ptr)
	b.block_instr1(.ret, entry, b.void_type, result)
}

// generate_data_alignment_exponent_body provides the source builtin helper to
// synthetic native runtime functions even when its source body is not selected.
fn (mut b Builder) generate_data_alignment_exponent_body(func_id int) {
	entry := b.m.add_block(func_id, 'alignment_exponent_entry')
	alignment := b.func_add_argument(func_id, b.u64_type, 'alignment')
	exponent := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.u8_type))
	remaining := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.u64_type))
	zero8 := b.m.get_or_add_const(b.u8_type, '0')
	one64 := b.m.get_or_add_const(b.u64_type, '1')
	sixteen64 := b.m.get_or_add_const(b.u64_type, '16')
	b.block_instr2(.store, entry, b.void_type, zero8, exponent)
	b.block_instr2(.store, entry, b.void_type, alignment, remaining)
	check := b.m.add_block(func_id, 'alignment_exponent_check')
	body := b.m.add_block(func_id, 'alignment_exponent_body')
	done := b.m.add_block(func_id, 'alignment_exponent_done')
	too_small := b.block_instr2(.ule, entry, b.i1_type, alignment, sixteen64)
	b.block_instr3(.br, entry, b.void_type, too_small, ValueID(done), ValueID(check))
	current_remaining := b.block_instr1(.load, check, b.u64_type, remaining)
	more := b.block_instr2(.ugt, check, b.i1_type, current_remaining, one64)
	b.block_instr3(.br, check, b.void_type, more, ValueID(body), ValueID(done))
	current_exponent := b.block_instr1(.load, body, b.u8_type, exponent)
	next_exponent := b.block_instr2(.add, body, b.u8_type, current_exponent, b.m.get_or_add_const(b.u8_type,
		'1'))
	next_remaining := b.block_instr2(.lshr, body, b.u64_type, current_remaining, one64)
	b.block_instr2(.store, body, b.void_type, next_exponent, exponent)
	b.block_instr2(.store, body, b.void_type, next_remaining, remaining)
	b.block_instr1(.jmp, body, b.void_type, ValueID(check))
	result := b.block_instr1(.load, done, b.u8_type, exponent)
	b.block_instr1(.ret, done, b.void_type, result)
}

// generate_map_find_body supports generate map find body handling for Builder.
fn (mut b Builder) generate_map_find_body(func_id int) {
	if b.m.target.ptr_size == 4 {
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
		b.block_instr3(.br, blk_has_map, b.void_type, has_state, ValueID(blk_loop),
			ValueID(blk_not_found))

		i := b.block_instr1(.load, blk_loop, b.i64_type, alloca_i)
		len := b.map_state_load_i64(blk_loop, state, 3)
		in_range := b.block_instr2(.lt, blk_loop, b.i1_type, i, len)
		b.block_instr3(.br, blk_loop, b.void_type, in_range, ValueID(blk_body), ValueID(blk_not_found))

		keys := b.block_instr1(.load, blk_body, ptr_i8, keys_ptr)
		key_size := b.map_state_load_i64(blk_body, state, 4)
		offset := b.block_instr2(.mul, blk_body, b.i64_type, i, key_size)
		slot_key := b.block_instr2(.add, blk_body, ptr_i8, keys, offset)
		string_key_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
		is_string_key := b.block_instr2(.eq, blk_body, b.i1_type, key_size, string_key_size)
		b.block_instr3(.br, blk_body, b.void_type, is_string_key, ValueID(blk_string_cmp),
			ValueID(blk_mem_cmp))

		slot_string_ptr := b.block_instr1(.bitcast, blk_string_cmp, ptr_string, slot_key)
		key_string_ptr := b.block_instr1(.bitcast, blk_string_cmp, ptr_string, key_ptr)
		slot_string := b.block_instr1(.load, blk_string_cmp, b.str_type, slot_string_ptr)
		key_string := b.block_instr1(.load, blk_string_cmp, b.str_type, key_string_ptr)
		eq_ref := b.m.add_value(.func_ref, b.void_type, 'fast_string_eq', b.runtime_fn_id('fast_string_eq'))
		string_eq := b.block_instr3(.call, blk_string_cmp, b.i1_type, eq_ref, slot_string, key_string)
		b.block_instr3(.br, blk_string_cmp, b.void_type, string_eq, ValueID(blk_found),
			ValueID(blk_next))

		memcmp_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.runtime_fn_id('memcmp'))
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
	} else {
		b.generate_native_map_find_body(func_id)
	}
}

// generate_map_exists_body supports generate map exists body handling for Builder.
fn (mut b Builder) generate_map_exists_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.runtime_fn_id('v3_map_find'))
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
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.runtime_fn_id('v3_map_find'))
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)

	blk_found := b.m.add_block(func_id, 'map_get_found')
	blk_missing := b.m.add_block(func_id, 'map_get_missing')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_missing))

	state := b.map_state_ptr(blk_found, map_ptr)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	vals := b.block_instr1(.load, blk_found, ptr_i8, vals_ptr)
	val_size := b.map_state_load_i64(blk_found, state, 5)
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
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.runtime_fn_id('v3_map_find'))
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)

	blk_found := b.m.add_block(func_id, 'map_get_check_found')
	blk_missing := b.m.add_block(func_id, 'map_get_check_missing')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_missing))

	state := b.map_state_ptr(blk_found, map_ptr)
	vals_ptr := b.map_state_field_ptr(blk_found, state, 1)
	vals := b.block_instr1(.load, blk_found, ptr_i8, vals_ptr)
	val_size := b.map_state_load_i64(blk_found, state, 5)
	offset := b.block_instr2(.mul, blk_found, b.i64_type, idx, val_size)
	result := b.block_instr2(.add, blk_found, ptr_i8, vals, offset)
	b.block_instr1(.ret, blk_found, b.void_type, result)

	zero_ptr := b.m.get_or_add_const(ptr_i8, '0')
	b.block_instr1(.ret, blk_missing, b.void_type, zero_ptr)
}

fn (mut b Builder) generate_map_get_key_check_body(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_map := b.m.type_store.get_ptr(b.map_type)
	entry := b.m.add_block(func_id, 'entry')
	map_ptr := b.func_add_argument(func_id, ptr_map, 'map')
	key_ptr := b.func_add_argument(func_id, ptr_i8, 'key')
	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.runtime_fn_id('v3_map_find'))
	idx := b.block_instr3(.call, entry, b.i64_type, find_ref, map_ptr, key_ptr)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	found := b.block_instr2(.ge, entry, b.i1_type, idx, zero)

	blk_found := b.m.add_block(func_id, 'map_get_key_check_found')
	blk_missing := b.m.add_block(func_id, 'map_get_key_check_missing')
	b.block_instr3(.br, entry, b.void_type, found, ValueID(blk_found), ValueID(blk_missing))

	state := b.map_state_ptr(blk_found, map_ptr)
	keys_ptr := b.map_state_field_ptr(blk_found, state, 0)
	keys := b.block_instr1(.load, blk_found, ptr_i8, keys_ptr)
	key_size := b.map_state_load_i64(blk_found, state, 4)
	offset := b.block_instr2(.mul, blk_found, b.i64_type, idx, key_size)
	result := b.block_instr2(.add, blk_found, ptr_i8, keys, offset)
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
	key_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	val_size := b.m.get_or_add_const(b.i64_type, '8')
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_set_sized',
		b.runtime_fn_id('v3_map_set_sized'))
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

	find_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_find', b.runtime_fn_id('v3_map_find'))
	idx := b.block_instr3(.call, blk_ready, b.i64_type, find_ref, map_ptr, key_ptr)
	found := b.block_instr2(.ge, blk_ready, b.i1_type, idx, zero)
	b.block_instr3(.br, blk_ready, b.void_type, found, ValueID(blk_update), ValueID(blk_insert))

	vals_update := b.block_instr1(.load, blk_update, ptr_i8, vals_ptr)
	val_size_update := b.map_state_load_i64(blk_update, state, 5)
	update_off := b.block_instr2(.mul, blk_update, b.i64_type, idx, val_size_update)
	update_dest := b.block_instr2(.add, blk_update, ptr_i8, vals_update, update_off)
	memcpy_ref_update := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, blk_update, ptr_i8, memcpy_ref_update, update_dest, val_ptr,
		val_size_update)
	b.block_instr0(.ret, blk_update, b.void_type)

	len := b.map_state_load_i64(blk_insert, state, 3)
	cap := b.map_state_load_i64(blk_insert, state, 2)
	needs_grow := b.block_instr2(.ge, blk_insert, b.i1_type, len, cap)
	blk_grow := b.m.add_block(func_id, 'map_set_grow')
	blk_store := b.m.add_block(func_id, 'map_set_store')
	b.block_instr3(.br, blk_insert, b.void_type, needs_grow, ValueID(blk_grow), ValueID(blk_store))

	two := b.m.get_or_add_const(b.i64_type, '2')
	eight := b.m.get_or_add_const(b.i64_type, '8')
	old_cap_empty := b.block_instr2(.eq, blk_grow, b.i1_type, cap, zero)
	blk_alloc_metas := b.m.add_block(func_id, 'map_set_alloc_metas')
	blk_grow_storage := b.m.add_block(func_id, 'map_set_grow_storage')
	b.block_instr3(.br, blk_grow, b.void_type, old_cap_empty, ValueID(blk_alloc_metas),
		ValueID(blk_grow_storage))
	metas_count := b.m.get_or_add_const(b.i64_type, '16')
	four := b.m.get_or_add_const(b.i64_type, '4')
	calloc_ref := b.m.add_value(.func_ref, ptr_i8, 'calloc', b.runtime_fn_id('calloc'))
	metas := b.block_instr3(.call, blk_alloc_metas, ptr_i8, calloc_ref, metas_count, four)
	metas_ptr := b.block_struct_field_ptr(blk_alloc_metas, state, b.map_state_type, 6)
	b.block_instr2(.store, blk_alloc_metas, b.void_type, metas, metas_ptr)
	b.block_instr1(.jmp, blk_alloc_metas, b.void_type, ValueID(blk_grow_storage))

	key_size_grow := b.map_state_load_i64(blk_grow_storage, state, 4)
	val_size_grow := b.map_state_load_i64(blk_grow_storage, state, 5)
	keys_old := b.block_instr1(.load, blk_grow_storage, ptr_i8, keys_ptr)
	vals_old := b.block_instr1(.load, blk_grow_storage, ptr_i8, vals_ptr)
	double_cap := b.block_instr2(.mul, blk_grow_storage, b.i64_type, cap, two)
	new_cap := b.block_instr2(.add, blk_grow_storage, b.i64_type, double_cap, eight)
	key_bytes := b.block_instr2(.mul, blk_grow_storage, b.i64_type, new_cap, key_size_grow)
	val_bytes := b.block_instr2(.mul, blk_grow_storage, b.i64_type, new_cap, val_size_grow)
	realloc_ref_keys := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
	realloc_ref_vals := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
	keys_new := b.block_instr3(.call, blk_grow_storage, ptr_i8, realloc_ref_keys, keys_old,
		key_bytes)
	vals_new := b.block_instr3(.call, blk_grow_storage, ptr_i8, realloc_ref_vals, vals_old,
		val_bytes)
	b.block_instr2(.store, blk_grow_storage, b.void_type, keys_new, keys_ptr)
	b.block_instr2(.store, blk_grow_storage, b.void_type, vals_new, vals_ptr)
	b.map_state_store_i64(blk_grow_storage, state, 2, new_cap)
	if b.m.target.ptr_size == 8 {
		deletes_ptr := b.map_dense_field_ptr(blk_grow_storage, state, 4)
		deletes := b.block_instr1(.load, blk_grow_storage, b.u32_type, deletes_ptr)
		has_deletes := b.block_instr2(.ne, blk_grow_storage, b.i1_type, deletes,
			b.m.get_or_add_const(b.u32_type, '0'))
		blk_bitmap := b.m.add_block(func_id, 'map_set_grow_bitmap')
		b.block_instr3(.br, blk_grow_storage, b.void_type, has_deletes,
			ValueID(blk_bitmap), ValueID(blk_store))
		bitmap_one := b.m.get_or_add_const(b.i64_type, '1')
		bitmap := b.block_instr3(.call, blk_bitmap, ptr_i8, calloc_ref, new_cap,
			bitmap_one)
		old_bitmap_ptr := b.map_dense_field_ptr(blk_bitmap, state, 7)
		old_bitmap := b.block_instr1(.load, blk_bitmap, ptr_i8, old_bitmap_ptr)
		bitmap_memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy',
			b.runtime_fn_id('memcpy'))
		b.block_instr4(.call, blk_bitmap, ptr_i8, bitmap_memcpy_ref, bitmap, old_bitmap,
			len)
		free_bitmap_ref := b.m.add_value(.func_ref, b.void_type, 'free',
			b.fn_ids['free'])
		b.block_instr2(.call, blk_bitmap, b.void_type, free_bitmap_ref, old_bitmap)
		b.block_instr2(.store, blk_bitmap, b.void_type, bitmap, old_bitmap_ptr)
		b.block_instr1(.jmp, blk_bitmap, b.void_type, ValueID(blk_store))
	} else {
		b.block_instr1(.jmp, blk_grow_storage, b.void_type, ValueID(blk_store))
	}

	keys := b.block_instr1(.load, blk_store, ptr_i8, keys_ptr)
	vals := b.block_instr1(.load, blk_store, ptr_i8, vals_ptr)
	key_size := b.map_state_load_i64(blk_store, state, 4)
	val_size := b.map_state_load_i64(blk_store, state, 5)
	key_off := b.block_instr2(.mul, blk_store, b.i64_type, len, key_size)
	val_off := b.block_instr2(.mul, blk_store, b.i64_type, len, val_size)
	key_dest := b.block_instr2(.add, blk_store, ptr_i8, keys, key_off)
	val_dest := b.block_instr2(.add, blk_store, ptr_i8, vals, val_off)
	memcpy_ref_key := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	memcpy_ref_val := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, blk_store, ptr_i8, memcpy_ref_key, key_dest, key_ptr, key_size)
	b.block_instr4(.call, blk_store, ptr_i8, memcpy_ref_val, val_dest, val_ptr, val_size)
	one := b.m.get_or_add_const(b.i64_type, '1')
	new_len := b.block_instr2(.add, blk_store, b.i64_type, len, one)
	b.map_state_store_i64(blk_store, state, 3, new_len)
	if b.m.target.ptr_size == 8 {
		count_ptr := b.block_struct_field_ptr(blk_store, state, b.map_state_type, 13)
		count := b.block_instr1(.load, blk_store, b.i32_type, count_ptr)
		new_count := b.block_instr2(.add, blk_store, b.i32_type, count,
			b.m.get_or_add_const(b.i32_type, '1'))
		b.block_instr2(.store, blk_store, b.void_type, new_count, count_ptr)
		deletes_ptr := b.map_dense_field_ptr(blk_store, state, 4)
		deletes := b.block_instr1(.load, blk_store, b.u32_type, deletes_ptr)
		has_deletes := b.block_instr2(.ne, blk_store, b.i1_type, deletes,
			b.m.get_or_add_const(b.u32_type, '0'))
		blk_invalidate := b.m.add_block(func_id, 'map_set_invalidate')
		blk_finish := b.m.add_block(func_id, 'map_set_finish')
		b.block_instr3(.br, blk_store, b.void_type, has_deletes, ValueID(blk_invalidate),
			ValueID(blk_finish))
		b.invalidate_native_map_index(blk_invalidate, state)
		b.block_instr1(.jmp, blk_invalidate, b.void_type, ValueID(blk_finish))
		b.block_instr0(.ret, blk_finish, b.void_type)
	} else {
		b.block_instr0(.ret, blk_store, b.void_type)
	}
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

	fn_ref := b.m.add_value(.func_ref, b.void_type, 'calloc', b.runtime_fn_id('calloc'))
	one := b.m.get_or_add_const(b.i64_type, '1')
	payload_size := b.block_instr2(.mul, blk_alloc, b.i64_type, final_cap, elem_size)
	allocation_size := b.block_instr2(.add, blk_alloc, b.i64_type, payload_size,
		b.m.get_or_add_const(b.i64_type, '31'))
	raw := b.block_instr3(.call, blk_alloc, ptr_i8, fn_ref, one, allocation_size)
	raw_integer := b.block_instr1(.bitcast, blk_alloc, b.u64_type, raw)
	biased := b.block_instr2(.add, blk_alloc, b.u64_type, raw_integer,
		b.m.get_or_add_const(b.u64_type, '31'))
	aligned := b.block_instr2(.and_, blk_alloc, b.u64_type, biased,
		b.m.get_or_add_const(b.u64_type, '18446744073709551600'))
	allocated_data := b.block_instr1(.bitcast, blk_alloc, ptr_i8, aligned)
	header := b.block_instr2(.sub, blk_alloc, ptr_i8, allocated_data,
		b.m.get_or_add_const(b.i64_type, '16'))
	allocation_ptr := b.block_instr1(.bitcast, blk_alloc, ptr_ptr_i8, header)
	b.block_instr2(.store, blk_alloc, b.void_type, raw, allocation_ptr)
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
	managed_flags := b.m.get_or_add_const(b.u32_type, '16')
	b.block_instr2(.store, blk_fields, b.void_type, managed_flags, flags_ptr)
	b.block_instr2(.store, blk_fields, b.void_type, elem_size, elem_size_ptr)

	arr := b.block_instr1(.load, blk_fields, b.array_type, alloca_arr)
	b.block_instr1(.ret, blk_fields, b.void_type, arr)
}

// generate_array_noscan_body delegates `__new_array_noscan(len, cap, size)`
// to array_new, which takes (size, len, cap).
fn (mut b Builder) generate_array_noscan_body(func_id int) {
	entry := b.m.add_block(func_id, 'entry')
	len := b.func_add_argument(func_id, b.i64_type, 'len')
	cap := b.func_add_argument(func_id, b.i64_type, 'cap')
	size := b.func_add_argument(func_id, b.i64_type, 'size')
	new_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.runtime_fn_id('array_new'))
	res := b.block_instr4(.call, entry, b.array_type, new_ref, size, len, cap)
	b.block_instr1(.ret, entry, b.array_type, res)
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
	zero_i64 := b.m.get_or_add_const(b.i64_type, '0')
	negative_len := b.block_instr2(.lt, entry, b.i1_type, slice_len, zero_i64)
	safe_slice_len := b.block_instr3(.select, entry, b.i64_type, negative_len, zero_i64,
		slice_len)
	old_offset_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 1)
	old_offset32 := b.block_instr1(.load, entry, b.i32_type, old_offset_ptr)
	old_offset := b.block_instr1(.zext, entry, b.i64_type, old_offset32)
	total_offset := b.block_instr2(.add, entry, b.i64_type, old_offset, offset)
	flags_ptr := b.block_struct_field_ptr(entry, alloca_arr, b.array_type, 4)
	flags := b.block_instr1(.load, entry, b.u32_type, flags_ptr)
	storage_flags := b.block_instr2(.and_, entry, b.u32_type, flags,
		b.m.get_or_add_const(b.u32_type, '48'))
	slice_flags := b.block_instr2(.or_, entry, b.u32_type, storage_flags,
		b.m.get_or_add_const(b.u32_type, '64'))
	managed := b.block_instr2(.and_, entry, b.u32_type, flags,
		b.m.get_or_add_const(b.u32_type, '16'))
	is_managed := b.block_instr2(.ne, entry, b.i1_type, managed,
		b.m.get_or_add_const(b.u32_type, '0'))
	has_data := b.block_instr2(.ne, entry, b.i1_type, data,
		b.m.get_or_add_const(ptr_i8, '0'))
	mark_managed := b.block_instr2(.and_, entry, b.i1_type, is_managed, has_data)
	mark_shared := b.m.add_block(func_id, 'array_slice_mark_shared')
	finish := b.m.add_block(func_id, 'array_slice_finish')
	b.block_instr3(.br, entry, b.void_type, mark_managed, ValueID(mark_shared), ValueID(finish))
	base_data := b.block_instr2(.sub, mark_shared, ptr_i8, data, old_offset)
	shared_ptr := b.block_instr2(.sub, mark_shared, ptr_i8, base_data,
		b.m.get_or_add_const(b.i64_type, '8'))
	b.block_instr2(.store, mark_shared, b.void_type, b.m.get_or_add_const(b.i8_type, '1'), shared_ptr)
	b.block_instr1(.jmp, mark_shared, b.void_type, ValueID(finish))

	out_data_ptr := b.block_struct_field_ptr(finish, alloca_out, b.array_type, 0)
	out_offset_ptr := b.block_struct_field_ptr(finish, alloca_out, b.array_type, 1)
	out_len_ptr := b.block_struct_field_ptr(finish, alloca_out, b.array_type, 2)
	out_cap_ptr := b.block_struct_field_ptr(finish, alloca_out, b.array_type, 3)
	out_flags_ptr := b.block_struct_field_ptr(finish, alloca_out, b.array_type, 4)
	out_elem_size_ptr := b.block_struct_field_ptr(finish, alloca_out, b.array_type, 5)
	b.block_instr2(.store, finish, b.void_type, slice_data, out_data_ptr)
	b.block_instr2(.store, finish, b.void_type, total_offset, out_offset_ptr)
	b.block_instr2(.store, finish, b.void_type, safe_slice_len, out_len_ptr)
	b.block_instr2(.store, finish, b.void_type, safe_slice_len, out_cap_ptr)
	b.block_instr2(.store, finish, b.void_type, slice_flags, out_flags_ptr)
	b.block_instr2(.store, finish, b.void_type, elem_size, out_elem_size_ptr)

	result := b.block_instr1(.load, finish, b.array_type, alloca_out)
	b.block_instr1(.ret, finish, b.void_type, result)
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
	string_eq_ref := b.m.add_value(.func_ref, b.i1_type, 'string__eq', b.runtime_fn_id('string__eq'))
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
	memcmp_ref := b.m.add_value(.func_ref, b.i64_type, 'memcmp', b.runtime_fn_id('memcmp'))
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
	if arguments_id := b.register_runtime_function('arguments', b.array_type, []TypeID{}) {
		b.generate_arguments_body(arguments_id)
	}
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

	elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	one := b.m.get_or_add_const(b.i64_type, '1')
	ptr_size := b.m.get_or_add_const(b.i64_type, '${b.m.target.ptr_size}')
	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
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
	tos_clone_ref := b.m.add_value(.func_ref, b.str_type, 'tos_clone', b.runtime_fn_id('tos_clone'))
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

	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
	clone := b.block_instr4(.call, entry, b.array_type, new_ref, elem_size, len, cap)
	b.block_instr2(.store, entry, b.void_type, clone, alloca_clone)

	clone_data_ptr := b.block_struct_field_ptr(entry, alloca_clone, b.array_type, 0)
	clone_data := b.block_instr1(.load, entry, ptr_i8, clone_data_ptr)
	copy_size := b.block_instr2(.mul, entry, b.i64_type, len, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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

	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
	clone := b.block_instr4(.call, entry, b.array_type, new_ref, elem_size, len, cap)
	b.block_instr2(.store, entry, b.void_type, clone, alloca_clone)

	clone_data_ptr := b.block_struct_field_ptr(entry, alloca_clone, b.array_type, 0)
	clone_data := b.block_instr1(.load, entry, ptr_i8, clone_data_ptr)
	copy_size := b.block_instr2(.mul, entry, b.i64_type, len, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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

	new_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', b.runtime_fn_id('array_new'))
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
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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
	flags_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 4)
	elem_size_ptr := b.block_struct_field_ptr(entry, arr_ptr, b.array_type, 5)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	cap32 := b.block_instr1(.load, entry, b.i32_type, cap_ptr)
	cap := b.block_instr1(.zext, entry, b.i64_type, cap32)
	needs_grow := b.block_instr2(.ge, entry, b.i1_type, len, cap)

	blk_grow := b.m.add_block(func_id, 'array_push_grow')
	blk_check_unique := b.m.add_block(func_id, 'array_push_check_unique')
	blk_unique := b.m.add_block(func_id, 'array_push_unique')
	blk_store := b.m.add_block(func_id, 'array_push_store')
	b.block_instr3(.br, entry, b.void_type, needs_grow, ValueID(blk_grow),
		ValueID(blk_check_unique))

	two := b.m.get_or_add_const(b.i64_type, '2')
	one := b.m.get_or_add_const(b.i64_type, '1')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	double_cap := b.block_instr2(.mul, blk_grow, b.i64_type, cap, two)
	empty_cap := b.block_instr2(.eq, blk_grow, b.i1_type, cap, zero)
	empty_addend := b.block_instr1(.zext, blk_grow, b.i64_type, empty_cap)
	empty_addend_scaled := b.block_instr2(.mul, blk_grow, b.i64_type, empty_addend, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, double_cap, empty_addend_scaled)
	if b.m.target.ptr_size == 4 {
		old_data := b.block_instr1(.load, blk_grow, ptr_i8, data_ptr)
		elem_size_grow32 := b.block_instr1(.load, blk_grow, b.i32_type, elem_size_ptr)
		elem_size_grow := b.block_instr1(.zext, blk_grow, b.i64_type, elem_size_grow32)
		new_size := b.block_instr2(.mul, blk_grow, b.i64_type, new_cap, elem_size_grow)
		realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
		new_data := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref, old_data, new_size)
		b.block_instr2(.store, blk_grow, b.void_type, new_data, data_ptr)
		b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	} else {
		grow_ref := b.m.add_value(.func_ref, b.void_type, '__ssa_array_grow', b.fn_ids['__ssa_array_grow'])
		b.block_instr3(.call, blk_grow, b.void_type, grow_ref, arr_ptr, new_cap)
	}
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_store))

	flags := b.block_instr1(.load, blk_check_unique, b.u32_type, flags_ptr)
	is_slice_bits := b.block_instr2(.and_, blk_check_unique, b.u32_type, flags,
		b.m.get_or_add_const(b.u32_type, '64'))
	is_slice := b.block_instr2(.ne, blk_check_unique, b.i1_type, is_slice_bits,
		b.m.get_or_add_const(b.u32_type, '0'))
	b.block_instr3(.br, blk_check_unique, b.void_type, is_slice, ValueID(blk_unique),
		ValueID(blk_store))

	clone_input := b.block_instr1(.load, blk_unique, b.array_type, arr_ptr)
	clone_ref := b.m.add_value(.func_ref, b.array_type, 'array_clone',
		b.runtime_fn_id('array_clone'))
	clone := b.block_instr2(.call, blk_unique, b.array_type, clone_ref, clone_input)
	b.block_instr2(.store, blk_unique, b.void_type, clone, arr_ptr)
	b.block_instr1(.jmp, blk_unique, b.void_type, ValueID(blk_store))

	data := b.block_instr1(.load, blk_store, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, blk_store, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, blk_store, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, blk_store, b.i64_type, len, elem_size)
	dest := b.block_instr2(.add, blk_store, ptr_i8, data, offset)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.block_instr4(.call, blk_store, ptr_i8, memcpy_ref, dest, elem_ptr, elem_size)
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

	two := b.m.get_or_add_const(b.i64_type, '2')
	new_cap_base := b.block_instr2(.mul, blk_grow, b.i64_type, new_len, two)
	new_cap := b.block_instr2(.add, blk_grow, b.i64_type, new_cap_base, two)
	if b.m.target.ptr_size == 4 {
		old_data := b.block_instr1(.load, blk_grow, ptr_i8, data_ptr)
		elem_size_grow32 := b.block_instr1(.load, blk_grow, b.i32_type, elem_size_ptr)
		elem_size_grow := b.block_instr1(.zext, blk_grow, b.i64_type, elem_size_grow32)
		new_size := b.block_instr2(.mul, blk_grow, b.i64_type, new_cap, elem_size_grow)
		realloc_ref := b.m.add_value(.func_ref, b.void_type, 'realloc', b.runtime_fn_id('realloc'))
		new_data := b.block_instr3(.call, blk_grow, ptr_i8, realloc_ref, old_data, new_size)
		b.block_instr2(.store, blk_grow, b.void_type, new_data, data_ptr)
		b.block_instr2(.store, blk_grow, b.void_type, new_cap, cap_ptr)
	} else {
		grow_ref := b.m.add_value(.func_ref, b.void_type, '__ssa_array_grow', b.fn_ids['__ssa_array_grow'])
		b.block_instr3(.call, blk_grow, b.void_type, grow_ref, arr_ptr, new_cap)
	}
	b.block_instr1(.jmp, blk_grow, b.void_type, ValueID(blk_copy))

	data := b.block_instr1(.load, blk_copy, ptr_i8, data_ptr)
	elem_size32 := b.block_instr1(.load, blk_copy, b.i32_type, elem_size_ptr)
	elem_size := b.block_instr1(.zext, blk_copy, b.i64_type, elem_size32)
	offset := b.block_instr2(.mul, blk_copy, b.i64_type, len, elem_size)
	dest := b.block_instr2(.add, blk_copy, ptr_i8, data, offset)
	copy_size := b.block_instr2(.mul, blk_copy, b.i64_type, count, elem_size)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
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
		b.runtime_fn_id('array.push_many'))
	b.block_instr4(.call, entry, b.void_type, push_many_ref, arr_ptr, data, len)
	b.block_instr0(.ret, entry, b.void_type)
}

// register_string_eq_stub updates register string eq stub state for ssa.
fn (mut b Builder) register_string_eq_stub() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	if func_id := b.register_runtime_function('string__eq', b.i1_type, p2) {
		b.generate_string_eq_body(func_id)
	}
}

// register_fast_string_eq_stub updates register fast string eq stub state for ssa.
fn (mut b Builder) register_fast_string_eq_stub() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	if func_id := b.register_runtime_function('fast_string_eq', b.i1_type, p2) {
		b.generate_string_eq_body(func_id)
	}
}

// register_string_lt_stub updates register string lt stub state for ssa.
fn (mut b Builder) register_string_lt_stub() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	if func_id := b.register_runtime_function('string__lt', b.i1_type, p2) {
		b.generate_string_lt_body(func_id)
	}
}

// register_string_trim_stubs updates register string trim stubs state for ssa.
fn (mut b Builder) register_string_trim_stubs() {
	mut p2 := []TypeID{}
	p2 << b.str_type
	p2 << b.str_type
	if trim_right_id := b.register_runtime_function('string.trim_right', b.str_type, p2) {
		b.generate_string_trim_right_body(trim_right_id)
	}
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
		if substr_id := b.runtime_fn_lookup(substr_name) {
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
		if func_id := b.register_runtime_function(name, b.str_type, p2) {
			b.generate_string_all_last_body(func_id, true)
		}
	}
	for name in ['string.all_after_last', 'string__all_after_last', 'all_after_last'] {
		if func_id := b.register_runtime_function(name, b.str_type, p2) {
			b.generate_string_all_last_body(func_id, false)
		}
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
	memcmp_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.runtime_fn_id('memcmp'))
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
	entry := b.m.add_block(func_id, 'entry')
	param_a := b.func_add_argument(func_id, b.str_type, 'a')
	param_b := b.func_add_argument(func_id, b.str_type, 'b')

	alloca_a := b.block_instr0(.alloca, entry, ptr_string)
	alloca_b := b.block_instr0(.alloca, entry, ptr_string)
	b.block_instr2(.store, entry, b.void_type, param_a, alloca_a)
	b.block_instr2(.store, entry, b.void_type, param_b, alloca_b)

	zero_64 := b.m.get_or_add_const(b.i64_type, '0')
	a_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_a, zero_64)
	b_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_b, zero_64)
	// The length's offset belongs to the target layout: 8 is where a 64-bit
	// pointer puts it, and on a 4-byte pointer target that word is `is_lit`.
	a_len_ptr := b.block_struct_field_ptr(entry, alloca_a, b.str_type, 1)
	b_len_ptr := b.block_struct_field_ptr(entry, alloca_b, b.str_type, 1)
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
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.runtime_fn_id('memcmp'))
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
	a_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_a, zero_64)
	b_str_ptr := b.block_instr2(.get_element_ptr, entry, ptr_i8, alloca_b, zero_64)
	// The length's offset belongs to the target layout: 8 is where a 64-bit
	// pointer puts it, and on a 4-byte pointer target that word is `is_lit`.
	a_len_ptr := b.block_struct_field_ptr(entry, alloca_a, b.str_type, 1)
	b_len_ptr := b.block_struct_field_ptr(entry, alloca_b, b.str_type, 1)
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
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'memcmp', b.runtime_fn_id('memcmp'))
	// memcmp returns C int; its upper register bits are outside the ABI result.
	cmp32 := b.block_instr4(.call, blk_cmp, b.i32_type, fn_ref, a_str, b_str, min_len64)
	cmp := b.block_instr1(.sext, blk_cmp, b.i64_type, cmp32)
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
	wymix0 := b.m.get_or_add_const(b.i64_type, '11562461410679940143')
	wymix1 := b.m.get_or_add_const(b.i64_type, '16646288086500911323')

	x := b.block_instr2(.xor, entry, b.i64_type, param_a, wyp0)
	y := b.block_instr2(.xor, entry, b.i64_type, param_b, wyp1)
	mix_a := b.wymix_inline(entry, x, wymix0)
	mix_b := b.wymix_inline(entry, y, wymix1)
	result := b.block_instr2(.xor, entry, b.i64_type, mix_a, mix_b)
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

// c_variant_function_names lists functions with platform C implementations.
fn (b &Builder) c_variant_function_names() map[string]bool {
	mut names := map[string]bool{}
	mut scan_module := ''
	for node in b.a.nodes {
		if node.kind == .file {
			scan_module = ''
			continue
		}
		if node.kind == .module_decl {
			scan_module = b.source_module_name(node)
			continue
		}
		if node.kind == .fn_decl {
			if file := b.a.source_files[node.pos.id] {
				if file.name.ends_with('.c.v') {
					names[ssa_fn_name_in_module(scan_module, node.value)] = true
				}
			}
		}
	}
	return names
}

// build_functions builds functions data for ssa.
fn (mut b Builder) build_functions() {
	c_variant_names := b.c_variant_function_names()
	progress := os.getenv('V_SSA_PROGRESS') != ''
	mut built_functions := 0
	mut cur_module := ''
	for node in b.a.nodes {
		if node.kind == .file {
			cur_module = ''
			continue
		}
		if node.kind == .module_decl {
			cur_module = b.source_module_name(node)
			continue
		}
		if node.kind == .fn_decl {
			if file := b.a.source_files[node.pos.id] {
				fn_name := ssa_fn_name_in_module(cur_module, node.value_text())
				if c_variant_names[fn_name] && !file.name.ends_with('.c.v') {
					continue
				}
			}
			if b.skip_source_fn_in_module(node.value_text(), cur_module) {
				continue
			}
			fn_name := ssa_fn_name_in_module(cur_module, node.value_text())
			if node.value_text() in native_runtime_override_names && node.value_text() in b.fn_ids {
				continue
			}
			// Dead-module elimination: skip every function from a skipped module.
			if b.skip_modules.len > 0 && cur_module in b.skip_modules {
				b.mark_fn_prototype(fn_name)
				continue
			}
			if (b.exact_used_fns || b.used_fns.len > 0) && !b.source_fn_is_used(node.value_text(), cur_module) {
				continue
			}
			// Hot reload: only the named function's body is materialized.
			if b.hot_fn.len > 0 && node.value_text() != b.hot_fn && fn_name != b.hot_fn {
				b.mark_fn_prototype(fn_name)
				continue
			}
			// Signature-only build: register the prototype, skip the body.
			if b.skip_fn_bodies {
				b.mark_fn_prototype(fn_name)
				continue
			}
			b.build_function(node, cur_module)
			built_functions++
			if progress && built_functions % 100 == 0 {
				eprintln('ssa: built ${built_functions} function bodies (nodes=${b.m.values.len})')
			}
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
		if node.kind == .module_decl && node.value_text().len > 0 && node.value_text() != 'builtin' {
			return node.value_text()
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
			cur_module = b.source_module_name(node)
			continue
		}
		// A type_decl with no children and a non-empty `typ` is an alias
		// (`type Foo = Bar`). Compact sum declarations also have no children,
		// but retain their `A | B` spelling in `typ`.
		if node.kind == .type_decl && node.children_count == 0 && node.typ != 0
			&& !node.typ_text().contains('|') {
			base := b.resolve_type_in_module(node.typ, cur_module)
			if base > 0 {
				b.type_aliases[node.value_text()] = base
				qualified := qualify_type_name(node.value_text(), cur_module)
				b.type_aliases[qualified] = base
			}
		}
	}
}

// source_param_type_name returns the ABI storage type for a parsed parameter.
fn (mut b Builder) source_param_type_name(param flat.Node, fn_name string, module_name string, idx int) u32 {
	param_shape := flat.source_type_shape(param.typ)
	mut name := if param_shape.has(.pointer) || param_shape.has(.array_type) {
		param.typ
	} else {
		b.checker_param_type_name(fn_name, module_name, idx) or { param.typ }
	}
	if param.typ != 0 {
		ast_type := b.resolve_type_in_module(param.typ, module_name)
		if b.is_sum_type_id(ast_type) {
			name = param.typ
		}
	}
	// An explicit `mut p &T` keeps &T in the parsed type and marks the extra
	// caller slot with op == .amp. The transformed body dereferences that slot.
	if param.is_mut && param.op == .amp && flat.source_type_shape(name).has(.pointer) {
		name = flat.source_type_id('&' + flat.source_type_name(name))
	}
	return name
}

// checker_param_type_name supports checker param type name handling for Builder.
fn (b &Builder) checker_param_type_name(fn_name string, module_name string, idx int) ?u32 {
	if b.tc == unsafe { nil } {
		return none
	}
	candidates := checker_fn_name_candidates(fn_name, module_name)
	for candidate in candidates {
		if params := b.tc.fn_param_types[candidate] {
			if idx >= 0 && idx < params.len {
				return params[idx].source_type_id()
			}
		}
	}
	return none
}

// checker_param_type returns the checker's concrete parameter type.
fn (b &Builder) checker_param_type(fn_name string, module_name string, idx int) ?types.Type {
	if b.tc == unsafe { nil } {
		return none
	}
	candidates := checker_fn_name_candidates(fn_name, module_name)
	for candidate in candidates {
		if params := b.tc.fn_param_types[candidate] {
			if idx >= 0 && idx < params.len {
				return params[idx]
			}
		}
	}
	return none
}

// checker_return_type_name returns the concrete return spelling for a
// monomorphized function, including generic struct instances like `Box[int]`.
fn (b &Builder) checker_return_type_name(fn_name string, module_name string) ?u32 {
	if b.tc == unsafe { nil } {
		return none
	}
	candidates := checker_fn_name_candidates(fn_name, module_name)
	for candidate in candidates {
		if ret := b.tc.fn_ret_types[candidate] {
			name := ret.source_type_id()
			if name != 0 {
				return name
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
			typ_id := b.ssa_type_from_checker_type(ret)
			if ret is types.Struct && typ_id > 0 && typ_id < b.m.type_store.types.len
				&& b.m.type_store.types[typ_id].fields.len == 0 {
				fields := b.tc.struct_fields_for_type(ret.source_type_id())
				if fields.len > 0 {
					mut field_types := []TypeID{cap: fields.len}
					mut field_names := []string{cap: fields.len}
					for field in fields {
						field_types << b.ssa_type_from_checker_type(field.typ)
						field_names << field.name
					}
					b.m.type_store.types[typ_id] = Type{
						kind:        .struct_t
						fields:      field_types
						field_names: field_names
					}
				}
			}
			return typ_id
		}
	}
	return none
}

fn (mut b Builder) prepare_used_fn_lookups() {
	for used_name, _ in b.used_fns {
		normalized := used_name.replace('__', '.')
		b.used_fn_normalized[normalized] = true
		b.add_used_fn_suffixes(used_name)
		if normalized != used_name {
			b.add_used_fn_suffixes(normalized)
		}
	}
}

fn (mut b Builder) add_used_fn_suffixes(name string) {
	mut search_start := 0
	for {
		dot := name.index_after('.', search_start) or { break }
		search_start = dot + 1
		if search_start < name.len {
			b.used_fn_suffixes[name[search_start..]] = true
		}
	}
}

// fn_is_used supports fn is used handling for Builder.
fn (b &Builder) fn_is_used(name string) bool {
	return b.fn_is_used_for_decl(name, false)
}

fn (b &Builder) fn_is_used_for_decl(name string, qualified_method bool) bool {
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
	if name in ['os.Process.close', 'os__Process__close', 'os.fd_close', 'os__fd_close'] {
		return true
	}
	runtime_name := name.replace('__', '.').all_after_last('.')
	if runtime_name in ['error_file_not_opened', 'error_size_of_type_0', 'fileno',
		'posix_wait4_to_exit_status', 'vpopen', 'vpclose']
		|| runtime_name.starts_with('posix_wait_status_') {
		return true
	}
	if name.contains('__') && name.replace('__', '.') in b.used_fns {
		return true
	}
	if !name.contains('.') && name in b.used_fn_suffixes {
		return true
	}
	normalized_name := name.replace('__', '.')
	used_short_name := normalized_name.all_after_last('.')
	// A retained C prototype is tracked by its bare symbol name. It does not
	// make a same-named V function in another module reachable.
	if !qualified_method && used_short_name in b.used_fns && used_short_name !in b.c_fn_ids {
		return true
	}
	if name in b.used_fn_suffixes {
		return true
	}
	mut suffix_start := 0
	for {
		dot := normalized_name.index_after('.', suffix_start) or { break }
		suffix_start = dot + 1
		if suffix_start < normalized_name.len {
			suffix := normalized_name[suffix_start..]
			if (!qualified_method || suffix.contains('.')) && suffix in b.used_fn_normalized
				&& (suffix.contains('.') || suffix !in b.c_fn_ids) {
				return true
			}
		}
	}
	if name.starts_with('array_') || name.starts_with('string__') || name.starts_with('strings__')
		|| name.starts_with('strconv__') || name.starts_with('IError.') {
		return true
	}
	if name in ['new_map', 'memdup', 'int_str', 'bool_str', 'print', 'println', 'eprint', 'eprintln',
		'exit', 'arguments', 'tos', 'tos2', 'tos3', 'tos_clone', 'cstring_to_vstring', 'malloc_noscan',
		'isnil', 'error', 'error_with_code', 'join_path_single', 'DenseArray.has_index',
		'DenseArray__has_index', 'has_index'] {
		return true
	}
	return false
}

fn (b &Builder) source_fn_is_used(name string, module_name string) bool {
	fn_name := ssa_fn_name_in_module(module_name, name)
	if b.exact_used_fns {
		return b.used_fns[fn_name]
	}
	// Method declarations include their receiver; a bare libc name is unrelated.
	qualified_method := name.replace('__', '.').contains('.')
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return b.fn_is_used_for_decl(fn_name, qualified_method)
	}
	return b.fn_is_used_for_decl(name, qualified_method) || b.fn_is_used_for_decl(fn_name, qualified_method)
}

// ownership_drop_intrinsic_name recognizes generic cleanup calls that backends lower
// directly rather than materializing as source functions.
fn (b &Builder) ownership_drop_intrinsic_name(name string) bool {
	if name in ['builtin.drop_owned', 'builtin__drop_owned']
		|| name.starts_with('builtin.drop_owned_T_') || name.starts_with('builtin__drop_owned_T_') {
		return true
	}
	if name in ['drop_owned_v3_interface', 'builtin.drop_owned_v3_interface',
		'builtin__drop_owned_v3_interface']
		|| name.starts_with('drop_owned_v3_interface_T_')
		|| name.starts_with('builtin.drop_owned_v3_interface_T_')
		|| name.starts_with('builtin__drop_owned_v3_interface_T_') {
		return true
	}
	if name != 'drop_owned' && !name.starts_with('drop_owned_T_') {
		return false
	}
	if b.tc == unsafe { nil } {
		return name == 'drop_owned'
	}
	module_name := b.tc.fn_type_modules['drop_owned'] or { return false }
	return module_name == 'builtin'
}

// build_function builds function data for ssa.
fn (mut b Builder) build_function(node flat.Node, module_name string) {
	fn_name := ssa_fn_name_in_module(module_name, node.value_text())
	func_id := b.fn_ids[fn_name]
	b.cur_module = module_name
	b.cur_func = func_id
	b.cur_func_ret_type = b.checker_return_type_name(node.value, module_name) or {
		qualify_type_ref_name(node.typ, module_name)
	}
	b.reset_function_state()

	entry := b.m.add_block(func_id, 'entry')
	b.cur_block = entry

	mut param_index := 0
	for i in 0 .. node.children_count {
		child := b.a.child_node(&node, i)
		if child.kind == .param {
			typ_name_source_id := b.source_param_type_name(child, node.value_text(), module_name,
				param_index)
			typ_name_source_id_2 := typ_name_source_id

			mut typ := b.resolve_type_in_module(typ_name_source_id, module_name)
			if checked := b.checker_param_type(node.value_text(), module_name, param_index) {
				if checked is types.SumType || (checked is types.Struct
					&& b.source_is_sum_type(flat.source_type_name(checked.source_type_id())))
					|| checker_type_is_function(checked) {
					typ = b.ssa_type_from_checker_type(checked)
				}
			}
			param_val := b.m.add_value(.argument, typ, child.value_text(), b.m.funcs[func_id].params.len)
			mut f := b.m.funcs[func_id]
			f.params << param_val
			b.m.funcs[func_id] = f

			alloca := b.emit0(.alloca, b.m.type_store.get_ptr(typ))
			b.emit2(.store, b.void_type, param_val, alloca)
			b.vars[child.value_text()] = alloca
			b.var_type_names.set_id(child.value_text(), typ_name_source_id_2)
			if child.is_mut && b.is_pointer_type(typ) {
				b.mut_param_names[child.value_text()] = true
			}
			if child.is_mut && (child.op == .amp || child.is_mut_builtin_pointer_param())
				&& b.is_pointer_type(typ) {
				b.mut_pointer_params[child.value_text()] = b.m.type_store.types[typ].elem_type
			}
			param_index++
		}
	}

	body_ids := b.fn_body_ids(node)
	b.initialize_deferred_stmts(body_ids)
	for id in body_ids {
		b.build_stmt(id)
	}

	blk := b.m.blocks[b.cur_block]
	if blk.instrs.len == 0 || !b.is_terminator(blk.instrs.last()) {
		ret_type := b.m.funcs[func_id].typ
		if ret_type == b.void_type
			|| (b.is_option_type(ret_type) && b.option_value_type(ret_type) == b.void_type) {
			b.emit_void_return()
		}
	}
}

// emit_void_return returns the success value for result/option void functions.
fn (mut b Builder) emit_void_return() {
	b.emit_deferred_stmts()
	ret_type := b.m.funcs[b.cur_func].typ
	if b.is_option_type(ret_type) && b.option_value_type(ret_type) == b.void_type {
		value := b.build_option_value(ret_type, true, ValueID(0))
		b.emit1(.ret, b.void_type, value)
	} else {
		b.emit0(.ret, b.void_type)
	}
}

// build_top_level_main builds top level main data for ssa.
fn (mut b Builder) build_top_level_main() {
	func_id := b.fn_ids['main'] or { return }
	b.cur_module = 'main'
	b.cur_func = func_id
	b.cur_func_ret_type = u32(0)
	b.reset_function_state()
	entry := b.m.add_block(func_id, 'entry')
	b.cur_block = entry

	body_ids := b.top_level_stmt_ids()
	b.initialize_deferred_stmts(body_ids)
	for id in body_ids {
		b.build_stmt(id)
	}

	blk := b.m.blocks[b.cur_block]
	if blk.instrs.len == 0 || !b.is_terminator(blk.instrs.last()) {
		b.emit_deferred_stmts()
		b.emit0(.ret, b.void_type)
	}
}

fn (mut b Builder) reset_function_state() {
	b.vars.clear()
	b.var_type_names.clear()
	b.mut_pointer_params.clear()
	b.mut_param_names.clear()
	b.sum_guard_variants.clear()
	b.label_blocks.clear()
	b.break_labels.clear()
	b.continue_labels.clear()
	b.pending_loop_label = ''
	b.defer_body_ids.clear()
	b.defer_active_slots.clear()
	b.pending_loop_label = ''
	b.loop_targets.clear()
	for name, val_id in b.global_vars {
		b.vars[name] = val_id
		if typ_source_id := b.global_type_names.type_id(name) {
			b.var_type_names.set_id(name, typ_source_id)
		}
	}
	for alias, name in b.global_aliases {
		if addr := b.global_vars[name] {
			b.vars[alias] = addr
			if typ_source_id := b.global_type_names.type_id(name) {
				b.var_type_names.set_id(alias, typ_source_id)
			}
		}
	}
	if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
		prefix := b.cur_module + '.'
		for name, val_id in b.global_vars {
			if name.starts_with(prefix) {
				short := name[prefix.len..]
				if !short.contains('.') {
					b.vars[short] = val_id
					if typ_source_id := b.global_type_names.type_id(name) {
						b.var_type_names.set_id(short, typ_source_id)
					}
				}
			}
		}
	}
	if b.source_modules.len > 0 && b.cur_module !in ['', 'main', 'builtin'] {
		for name, val_id in b.global_vars {
			if name.starts_with(b.cur_module + '.') {
				b.vars[name[b.cur_module.len + 1..]] = val_id
			}
		}
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
	return node.value_text()
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
			typ:    b.var_type_names.type_id(name) or { u32(0) }
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
		if binding.typ != 0 {
			b.var_type_names.set_id(name, binding.typ)
		} else {
			b.var_type_names.delete(name)
		}
	} else {
		b.vars.delete(name)
		b.var_type_names.delete(name)
	}
}

// bind_loop_var records a loop binding with separate layout and source spelling IDs.
fn (mut b Builder) bind_loop_var(name string, typ TypeID, typ_name_id u32) ValueID {
	if name.len == 0 {
		return ValueID(0)
	}
	slot := b.emit0(.alloca, b.m.type_store.get_ptr(typ))
	b.vars[name] = slot
	if typ_name_id != 0 {
		b.var_type_names.set_id(name, typ_name_id)
	}
	return slot
}

// ssa_pointer_base_source_type retains unchanged spelling identities.
fn ssa_pointer_base_source_type(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.pointer) { return type_id }
	return flat.source_type_id(flat.source_type_name(type_id).trim_left('&'))
}

// for_in_container_type_name supports for in container type name handling for Builder.
fn (b &Builder) for_in_container_type_name(container_id flat.NodeId) u32 {
	if b.tc != unsafe { nil } && b.valid_node_id(container_id) {
		resolved_type := b.tc.resolve_type(container_id)
		resolved_id := resolved_type.source_type_id()
		resolved_name := flat.source_type_name(resolved_id).trim_left('&')
		if resolved_id != 0 && resolved_id != flat.source_type_unknown && resolved_name.len > 0 {
			return flat.source_type_id(resolved_name)
		}
	}
	if b.valid_node_id(container_id) {
		node := b.a.nodes[int(container_id)]
		if node.kind == .ident {
			if typ_source_id := b.var_type_names.type_id(node.value) {
				typ_name := flat.source_type_name(typ_source_id).trim_left('&')
				if typ_name.len > 0 {
					return flat.source_type_id(typ_name)
				}
			}
		}
	}
	checked_source_id_2 := b.checked_expr_type_name(container_id)
	if checked_source_id_2 != 0 && checked_source_id_2 != flat.source_type_unknown {
		return ssa_pointer_base_source_type(checked_source_id_2)
	}
	if !b.valid_node_id(container_id) {
		return u32(0)
	}
	node := b.a.nodes[int(container_id)]
	if node.typ != 0 {
		return ssa_pointer_base_source_type(node.typ)
	}
	if node.kind == .ident {
		return ssa_pointer_base_source_type(b.var_type_names.type_id(node.value_text()) or { u32(0) })
	}
	if node.kind == .selector && node.children_count > 0 {
		base_type := b.for_in_container_type_name(b.a.child(&node, 0))
		if field_type_source_id := b.registered_field_type(base_type, node.value_text()) {
			return ssa_pointer_base_source_type(field_type_source_id)
		}
	}
	return u32(0)
}

// for_in_array_elem_type_name supports for in array elem type name handling for Builder.
fn (b &Builder) for_in_array_elem_type_name(container_id flat.NodeId, container_type_id u32) u32 {
	if b.valid_node_id(container_id) {
		node := b.a.nodes[int(container_id)]
		if node.kind == .array_literal && node.children_count > 0 {
			first_id := b.a.child(&node, 0)
			first_type_source_id := b.checked_expr_type_name(first_id)
			if first_type_source_id != 0 && first_type_source_id != flat.source_type_unknown {
				return first_type_source_id
			}
		}
	}
	clean_id := ssa_pointer_base_source_type(container_type_id)
	clean := flat.source_type_name(clean_id)
	if clean.starts_with('[]') {
		return flat.source_type_id(clean[2..])
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 && idx + 1 < clean.len {
			return flat.source_type_id(clean[idx + 1..])
		}
	}
	if clean_id == flat.source_type_string {
		return flat.source_type_u8
	}
	return flat.source_type_int
}

fn (b &Builder) is_result_type_id(typ_id TypeID) bool {
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return false
	}
	return 'err' in b.m.type_store.types[typ_id].field_names
}

fn (b &Builder) result_error_type(typ_id TypeID) TypeID {
	typ := b.m.type_store.types[typ_id]
	index := typ.field_names.index('err')
	if index >= 0 && index < typ.fields.len {
		return typ.fields[index]
	}
	return TypeID(0)
}

fn (b &Builder) is_error_result_expr(id flat.NodeId, value ValueID, result_type TypeID) bool {
	err_type := b.result_error_type(result_type)
	if err_type == 0 {
		return false
	}
	if b.value_type(value) == err_type {
		return true
	}
	if b.tc != unsafe { nil } {
		name_id := b.checked_expr_type_name(id)
		name := flat.source_type_name(name_id).trim_left('&?!')
		if name.len > 0 && b.tc.named_type_implements_interface(flat.source_type_id(name),
			flat.source_type_id('IError')) {
			return true
		}
	}
	return b.option_value_type(result_type) == b.void_type
		&& b.a.nodes[int(id)].kind != .none_expr
}

fn (mut b Builder) build_result_error_value(result_type TypeID, error_value ValueID) ValueID {
	result_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(result_type))
	ok_ptr := b.block_struct_field_ptr(b.cur_block, result_alloca, result_type, 0)
	ok := b.m.get_or_add_const(b.i1_type, '0')
	b.emit2(.store, b.void_type, ok, ok_ptr)
	payload_type := b.option_value_type(result_type)
	if payload_type != b.void_type {
		value_ptr := b.block_struct_field_ptr(b.cur_block, result_alloca, result_type, 1)
		b.emit2(.store, b.void_type, b.default_value_for_type(payload_type), value_ptr)
	}
	err_index := b.m.type_store.types[result_type].field_names.index('err')
	if err_index >= 0 {
		err_ptr := b.block_struct_field_ptr(b.cur_block, result_alloca, result_type, err_index)
		b.emit2(.store, b.void_type, error_value, err_ptr)
	}
	return b.emit1(.load, result_type, result_alloca)
}

// build_stmt builds stmt data for ssa.
fn (mut b Builder) build_stmt(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	node := b.a.nodes[int(id)]
	if node.kind !in [.label_stmt, .for_stmt, .for_in_stmt] {
		b.pending_loop_label = ''
	}
	match node.kind {
		.expr_stmt {
			expr_id := b.a.child(&node, 0)
			expr := b.a.nodes[int(expr_id)]
			if expr.kind == .infix && expr.op == .left_shift && expr.value_text() == 'push' {
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
				is_result := b.is_result_type_id(ret_type)
				is_option_like := b.is_option_type(ret_type) || is_result
				mut val := if node.children_count > 1 && b.is_struct_type(ret_type) {
					payload_type := if is_option_like {
						b.option_value_type(ret_type)
					} else {
						ret_type
					}
					b.build_multi_return_value(payload_type, node)
				} else {
					// Resolve `return .member` against the declared return type so a
					// duplicated enum-member name is not built as 0 (see build_assign_rhs).
					b.build_field_value(expr_id, b.cur_func_ret_type)
				}
				if !is_option_like {
					if b.is_interface_type_id(ret_type) && b.value_type(val) != ret_type {
						val = b.box_interface_value(ret_type, b.checked_expr_type_name(expr_id), val)
					} else {
						val = b.coerce_value_for_type(val, ret_type)
					}
				}
				if is_option_like && b.value_type(val) != ret_type {
					expr_node := b.a.nodes[int(expr_id)]
					if is_result && b.is_error_result_expr(expr_id, val, ret_type) {
						err_type := b.result_error_type(ret_type)
						if b.value_type(val) != err_type {
							val = b.box_interface_value(err_type,
								b.checked_expr_type_name(expr_id), val)
						}
						val = b.build_result_error_value(ret_type, val)
					} else {
						val = b.build_option_value(ret_type, expr_node.kind != .none_expr, val)
					}
				}
				b.emit_deferred_stmts()
				b.emit1(.ret, b.void_type, val)
			} else {
				b.emit_void_return()
			}
		}
		.defer_stmt {
			if node.children_count > 0 {
				b.activate_deferred_stmt(b.a.child(&node, 0))
			}
		}
		.for_stmt {
			saved_sum_guards := b.sum_guard_variants.clone()
			b.build_for(node)
			b.intersect_sum_guards(saved_sum_guards)
		}
		.for_in_stmt {
			saved_sum_guards := b.sum_guard_variants.clone()
			b.build_for_in(node)
			b.intersect_sum_guards(saved_sum_guards)
		}
		.break_stmt {
			b.build_loop_control(node.value_text(), false)
		}
		.continue_stmt {
			b.build_loop_control(node.value_text(), true)
		}
		.goto_stmt {
			target := b.label_block(node.value_text())
			b.emit1(.jmp, b.void_type, ValueID(target))
		}
		.label_stmt {
			if b.build_loop_label(node.value_text()) {
				return
			}
			target := b.label_block(node.value_text())
			if !b.current_block_terminated() {
				b.emit1(.jmp, b.void_type, ValueID(target))
			}
			b.cur_block = target
			b.pending_loop_label = node.value_text()
		}
		.if_expr {
			b.build_if(node)
		}
		.match_stmt {
			b.build_match_stmt(node)
		}
		.block {
			b.build_scoped_stmts(node, 0)
		}
		.lock_expr {
			if node.children_count > 0 {
				body_id := b.a.child(&node, node.children_count - 1)
				body := b.a.nodes[int(body_id)]
				if body.kind == .block {
					b.build_scoped_stmts(body, 0)
				} else {
					b.build_stmt(body_id)
				}
			}
		}
		.select_stmt {
			b.build_select_placeholder(node)
		}
		.assert_stmt {
			b.build_assert(node)
		}
		.fn_decl, .c_fn_decl, .struct_decl, .field_decl, .global_decl, .const_decl, .const_field,
		.enum_decl, .enum_field, .type_decl, .interface_decl, .interface_field, .import_decl,
		.module_decl, .directive, .param {
		}
		.empty {}
		else {
			eprintln('build_stmt: unsupported node kind: ${node.kind}')
		}
	}
}

fn (mut b Builder) build_select_placeholder(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	mut selected := b.a.nodes[int(b.a.child(&node, 0))]
	for i in 0 .. node.children_count {
		branch := b.a.nodes[int(b.a.child(&node, i))]
		if branch.kind == .select_branch && branch.value == 'else' {
			selected = branch
			break
		}
	}
	if selected.kind != .select_branch {
		return
	}
	body_start := if selected.value == 'else' {
		0
	} else if selected.children_count > 1
		&& b.a.nodes[int(b.a.child(&selected, 1))].kind == .prefix
		&& b.a.nodes[int(b.a.child(&selected, 1))].op == .arrow {
		2
	} else {
		1
	}
	for i in body_start .. selected.children_count {
		b.build_stmt(b.a.child(&selected, i))
	}
}

fn (b &Builder) save_scope() BuilderScope {
	if b.m.target.ptr_size != 4 {
		return BuilderScope{}
	}
	return BuilderScope{
		active:     true
		vars:       b.vars.clone()
		type_names: b.var_type_names.clone()
	}
}

fn (mut b Builder) restore_scope(scope BuilderScope) {
	if scope.active {
		b.vars = scope.vars.clone()
		b.var_type_names = scope.type_names.clone()
	}
}

fn (mut b Builder) build_scoped_stmts(node flat.Node, start int) {
	scope := b.save_scope()
	for i in start .. node.children_count {
		b.build_stmt(b.a.child(&node, i))
		if scope.active && b.current_block_terminated() {
			break
		}
	}
	b.restore_scope(scope)
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
		mut rhs_val := b.build_expr(rhs_id)
		mut rhs_type := b.value_type(rhs_val)
		declared_type_source_id := b.declared_v_type_name(lhs_id, rhs_id)
		declared_storage_type := if declared_type_source_id != 0 {
			b.resolve_type(declared_type_source_id)
		} else {
			TypeID(0)
		}
		if declared_storage_type > 0 {
			rhs_val = b.coerce_value_for_type(rhs_val, declared_storage_type)
			if b.is_int_type(declared_storage_type) || b.is_float_type(declared_storage_type)
				|| b.is_pointer_type(declared_storage_type) {
				rhs_type = declared_storage_type
			}
		}
		if int(rhs_id) >= 0 && b.a.nodes[int(rhs_id)].kind == .nil_literal && lhs.typ != 0 {
			declared_type := b.resolve_type(lhs.typ)
			if b.is_pointer_type(declared_type) && declared_type != rhs_type {
				// Transformed pointer-valued branches initialize their typed staging
				// local with nil. Preserve its pointee instead of inferring voidptr.
				rhs_val = b.emit1(.bitcast, declared_type, rhs_val)
				rhs_type = declared_type
			}
		}
		alloca := b.emit0(.alloca, b.m.type_store.get_ptr(rhs_type))
		b.emit2(.store, b.void_type, rhs_val, alloca)
		if lhs.kind == .ident {
			b.vars[lhs.value_text()] = alloca
			rhs_type_name_source_id := b.checked_expr_type_name(rhs_id)
			b.var_type_names.set_id(lhs.value_text(), if declared_type_source_id != 0 {
				declared_type_source_id
			} else if rhs_type_name_source_id != 0 && rhs_type_name_source_id != flat.source_type_unknown {
				rhs_type_name_source_id
			} else {
				node.typ
			})
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
	mut type_name := u32(0)
	if lhs.kind == .ident {
		type_name = b.var_type_names.type_id(lhs.value_text()) or { u32(0) }
	} else if lhs.kind == .selector {
		type_name = b.selector_type_name(lhs) or { u32(0) }
	}
	if type_name != 0 {
		return b.build_field_value(rhs_id, type_name)
	}
	return b.build_expr(rhs_id)
}

fn (mut b Builder) build_assign(node flat.Node) {
	mut parallel_values := []ValueID{}
	if b.m.target.ptr_size == 4 && node.op == .assign && node.children_count > 2 {
		for index := 0; index < node.children_count; index += 2 {
			lhs := b.a.child_node(&node, index)
			parallel_values << b.build_assign_rhs(*lhs, b.a.child(&node, index + 1))
		}
	}
	mut i := 0
	for i < node.children_count {
		lhs_id := b.a.child(&node, i)
		rhs_id := b.a.child(&node, i + 1)
		lhs := b.a.nodes[int(lhs_id)]

		if lhs.kind == .ident && lhs.value_text() == '_' {
			if parallel_values.len == 0 {
				b.build_expr(rhs_id)
			}
			i += 2
			continue
		}
		if lhs.kind == .ident {
			if addr := b.vars[lhs.value_text()] {
				if node.op == .assign {
					rhs_val := if parallel_values.len > 0 {
						parallel_values[i / 2]
					} else {
						b.build_assign_rhs(lhs, rhs_id)
					}
					b.emit2(.store, b.void_type, b.coerce_value_for_type(rhs_val, b.deref_type(addr)),
						addr)
				} else {
					flag_pointer := node.op in [.amp_assign, .pipe_assign, .xor_assign]
						&& b.is_pointer_type(b.deref_type(addr)) && b.is_flag_enum_expr(lhs_id)
					target_addr := if lhs.value_text() in b.mut_param_names || flag_pointer {
						b.build_lvalue_addr(lhs_id)
					} else {
						addr
					}
					cur := b.emit1(.load, b.deref_type(target_addr), target_addr)
					mut rhs_val := b.coerce_int_value(b.build_assign_rhs(lhs, rhs_id),
						b.value_type(cur))
					rhs_val = b.coerce_flag_enum_assign_rhs(lhs_id, node.op, rhs_id, rhs_val)
					result := b.build_compound_assign_value(node.op, cur, rhs_val,
						b.checked_expr_type_name(lhs_id))
					b.emit2(.store, b.void_type, result, target_addr)
				}
				b.sum_guard_variants.delete(addr)
			}
		} else if lhs.kind == .selector || lhs.kind == .index
			|| (lhs.kind == .prefix && lhs.op == .mul) {
			addr := if lhs.kind == .selector {
				b.build_selector_addr(lhs)
			} else if lhs.kind == .prefix && lhs.children_count > 0 {
				b.build_expr(b.a.child(&lhs, 0))
			} else {
				b.build_lvalue_addr(lhs_id)
			}
			if node.op == .assign {
				rhs_val := if parallel_values.len > 0 {
					parallel_values[i / 2]
				} else {
					b.build_assign_rhs(lhs, rhs_id)
				}
				b.emit2(.store, b.void_type, b.coerce_value_for_type(rhs_val, b.deref_type(addr)),
					addr)
			} else {
				field_type := b.deref_type(addr)
				cur := b.emit1(.load, field_type, addr)
				mut rhs_val := b.coerce_int_value(b.build_assign_rhs(lhs, rhs_id), field_type)
				rhs_val = b.coerce_flag_enum_assign_rhs(lhs_id, node.op, rhs_id, rhs_val)
				result := b.build_compound_assign_value(node.op, cur, rhs_val,
					b.checked_expr_type_name(lhs_id))
				b.emit2(.store, b.void_type, result, addr)
			}
			if lhs.kind == .prefix && lhs.op == .mul {
				if origin := b.sum_guard_binding_addr(lhs_id) {
					b.sum_guard_variants.delete(origin)
				}
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
				b.emit2(.store, b.void_type, b.coerce_value_for_type(rhs_val, b.deref_type(field_ptr)),
					field_ptr)
			} else {
				field_type := b.deref_type(field_ptr)
				cur := b.emit1(.load, field_type, field_ptr)
				mut rhs_val := b.build_expr(rhs_id)
				rhs_val = b.coerce_flag_enum_assign_rhs(lhs_id, node.op, rhs_id, rhs_val)
				result := b.build_compound_assign_value(node.op, cur, rhs_val,
					b.checked_expr_type_name(lhs_id))
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
			b.emit2(.store, b.void_type, b.coerce_value_for_type(rhs_val, b.deref_type(addr)),
				addr)
		} else {
			field_type := b.deref_type(addr)
			cur := b.emit1(.load, field_type, addr)
			mut rhs_val := b.build_expr(rhs_id)
			rhs_val = b.coerce_flag_enum_assign_rhs(lhs_id, node.op, rhs_id, rhs_val)
			result := b.build_compound_assign_value(node.op, cur, rhs_val,
				b.checked_expr_type_name(lhs_id))
			b.emit2(.store, b.void_type, result, addr)
		}
		i += 2
	}
}

// build_for builds for data for ssa.
fn (mut b Builder) build_for(node flat.Node) {
	scope := b.save_scope()
	label := b.take_pending_loop_label()
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
	b.push_loop_targets(label, node, 3, exit_block, if post_node.kind != .empty {
		post_block
	} else {
		cond_block
	})

	b.build_scoped_stmts(node, 3)
	b.loop_targets.delete_last()

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
	b.restore_scope(scope)
}

// build_for_in builds for in data for ssa.
fn (mut b Builder) build_for_in(node flat.Node) {
	label := b.take_pending_loop_label()
	if node.children_count < 3 {
		return
	}
	key_id := b.a.child(&node, 0)
	val_id := b.a.child(&node, 1)
	container_id := b.a.child(&node, 2)
	body_start := if node.value_text() == '4' { 4 } else { 3 }
	if body_start == 4 {
		end_id := b.a.child(&node, 3)
		b.build_range_for_in(node, key_id, container_id, end_id, body_start, label)
		return
	}
	container := b.a.child_node(&node, 2)
	if container.kind == .range && container.children_count >= 2 {
		b.build_range_for_in(node, key_id, b.a.child(container, 0), b.a.child(container, 1),
			body_start, label)
		return
	}
	container_type_source_id := b.for_in_container_type_name(container_id)
	container_type := flat.source_type_name(container_type_source_id)
	if container_type_source_id == flat.source_type_string {
		b.build_string_for_in(node, key_id, val_id, container_id, body_start, label)
		return
	}
	if container_type.starts_with('map[') || container_type_source_id in [
		ssa_map_source_type,
		ssa_nominal_map_source_type,
	] {
		b.build_map_for_in(node, key_id, val_id, container_id, body_start, container_type_source_id,
			label)
		return
	}
	b.build_array_for_in(node, key_id, val_id, container_id, body_start, container_type_source_id, label)
}

// build_range_for_in builds range for in data for ssa.
fn (mut b Builder) build_range_for_in(node flat.Node, key_id flat.NodeId, start_id flat.NodeId, end_id flat.NodeId, body_start int, label string) {
	start_val := b.build_expr(start_id)
	end_val := b.build_expr(end_id)
	idx_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i64_type))
	b.emit2(.store, b.void_type, start_val, idx_alloca)
	key_name := b.ident_name(key_id)
	old_key := b.save_var_binding(key_name)
	key_alloca := b.bind_loop_var(key_name, b.i64_type, flat.source_type_int)

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
	b.push_loop_targets(label, node, body_start, exit_block, post_block)
	b.build_for_in_body(node, body_start)
	b.loop_targets.delete_last()
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

fn (mut b Builder) build_for_in_container_value(container_id flat.NodeId, container_type TypeID) ValueID {
	value := b.build_expr(container_id)
	// Mutable parameters and pointer containers expose the header through an address.
	return b.coerce_value_for_type(value, container_type)
}

// build_array_for_in builds array for in data for ssa.
fn (mut b Builder) build_array_for_in(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, body_start int, container_type_id u32, label string) {
	arr_val := b.build_for_in_container_value(container_id, b.array_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	arr_alloca := b.emit0(.alloca, ptr_array)
	b.emit2(.store, b.void_type, arr_val, arr_alloca)
	idx_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.i64_type))
	zero := b.m.get_or_add_const(b.i64_type, '0')
	b.emit2(.store, b.void_type, zero, idx_alloca)

	mut elem_type_name_source_id := b.for_in_array_elem_type_name(container_id, container_type_id)
	mut elem_type := b.resolve_type(elem_type_name_source_id)
	if b.tc != unsafe { nil } {
		resolved_container := types.unalias_type(b.tc.resolve_type(container_id))
		if resolved_container is types.Array {
			elem_type_name_source_id = resolved_container.elem_type.source_type_id()
			elem_type = b.ssa_type_from_checker_type(resolved_container.elem_type)
		}
	}
	if b.valid_node_id(container_id) {
		container_node := b.a.nodes[int(container_id)]
		if container_node.kind == .ident {
			if container_var_type := b.var_type_names.type_id(container_node.value) {
				container_var_name := flat.source_type_name(container_var_type)
				if container_var_name.contains('u128') {
					elem_type_name_source_id = flat.source_type_id('u128')
					elem_type = b.wide_integer_type()
				}
			}
		}
	}
	mut fixed_elem_type_name_id := if b.is_fixed_array_type_name(elem_type_name_source_id) {
		elem_type_name_source_id
	} else {
		u32(0)
	}
	if fixed_elem_type_name_id == 0 && b.tc != unsafe { nil } {
		resolved_container_id := b.tc.resolve_type(container_id).source_type_id()
		resolved_elem_id := indexed_elem_type_name(resolved_container_id)
		if b.is_fixed_array_type_name(resolved_elem_id) {
			fixed_elem_type_name_id = resolved_elem_id
		}
	}
	// A single loop variable takes the element, not the index: the parser
	// leaves an `.empty` placeholder where the value variable would be, and
	// valid_node_id accepts it, so check the kind explicitly. Without this,
	// `for x in arr` binds x to the key and sums indices instead of elements.
	has_value_var := b.valid_node_id(val_id) && b.a.nodes[int(val_id)].kind == .ident
	key_name := b.ident_name(key_id)
	val_name := if has_value_var { b.ident_name(val_id) } else { key_name }
	old_key := b.save_var_binding(key_name)
	old_val := if has_value_var { b.save_var_binding(val_name) } else { VarBinding{} }
	key_alloca := if has_value_var {
		b.bind_loop_var(key_name, b.i64_type, flat.source_type_int)
	} else {
		ValueID(0)
	}
	val_alloca := b.bind_loop_var(val_name, elem_type, elem_type_name_source_id)

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
		if elem_type == b.wide_integer_type() {
			elem_ptr := b.build_array_data_index_addr(arr_alloca, body_idx, elem_type)
			elem_raw := b.emit1(.bitcast, b.m.type_store.get_ptr(b.i8_type), elem_ptr)
			val_raw := b.emit1(.bitcast, b.m.type_store.get_ptr(b.i8_type), val_alloca)
			size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(elem_type)}')
			memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy',
				b.runtime_fn_id('memcpy'))
			b.emit4(.call, b.m.type_store.get_ptr(b.i8_type), memcpy_ref, val_raw, elem_raw,
				size)
		} else {
			elem_kind := b.m.type_store.types[elem_type].kind
			elem_val := if fixed_elem_type_name_id != 0 && elem_kind != .struct_t {
				elem_ptr := b.build_array_data_index_addr_sized(arr_alloca, body_idx,
					elem_type, b.sizeof_type_name(fixed_elem_type_name_id))
				b.emit1(.bitcast, elem_type, elem_ptr)
			} else {
				elem_ptr := b.build_array_data_index_addr(arr_alloca, body_idx, elem_type)
				b.emit1(.load, elem_type, elem_ptr)
			}
			b.emit2(.store, b.void_type, elem_val, val_alloca)
		}
	}
	b.push_loop_targets(label, node, body_start, exit_block, post_block)
	b.build_for_in_body(node, body_start)
	b.loop_targets.delete_last()
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
fn (mut b Builder) build_string_for_in(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, body_start int, label string) {
	str_val := b.build_for_in_container_value(container_id, b.str_type)
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
		b.bind_loop_var(key_name, b.i64_type, flat.source_type_int)
	} else {
		ValueID(0)
	}
	val_alloca := b.bind_loop_var(val_name, b.i8_type, flat.source_type_u8)

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
	b.push_loop_targets(label, node, body_start, exit_block, post_block)
	b.build_for_in_body(node, body_start)
	b.loop_targets.delete_last()
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
fn (mut b Builder) build_map_for_in(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, body_start int, container_type_id u32, label string) {
	key_type_name_id, val_type_name_id := map_type_parts(container_type_id)
	key_type := if key_type_name_id != 0 {
		b.resolve_type(key_type_name_id)
	} else {
		b.str_type
	}
	val_type := if val_type_name_id != 0 {
		b.resolve_type(val_type_name_id)
	} else {
		b.i64_type
	}
	map_val := b.build_for_in_container_value(container_id, b.map_type)
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
	key_alloca := b.bind_loop_var(key_name, key_type, key_type_name_id)
	val_alloca := if has_value_var {
		b.bind_loop_var(val_name, val_type, val_type_name_id)
	} else {
		ValueID(0)
	}

	cond_block := b.m.add_block(b.cur_func, 'for_in_map_cond')
	check_block := b.m.add_block(b.cur_func, 'for_in_map_check_live')
	body_block := b.m.add_block(b.cur_func, 'for_in_map_body')
	post_block := b.m.add_block(b.cur_func, 'for_in_map_post')
	exit_block := b.m.add_block(b.cur_func, 'for_in_map_exit')
	b.emit3(.br, b.void_type, has_state, ValueID(cond_block), ValueID(exit_block))

	b.cur_block = cond_block
	idx := b.emit1(.load, b.i64_type, idx_alloca)
	len := b.map_state_load_i64(cond_block, state, 3)
	in_range := b.emit2(.lt, b.i1_type, idx, len)
	b.emit3(.br, b.void_type, in_range, ValueID(check_block), ValueID(exit_block))

	b.cur_block = check_block
	body_idx := b.emit1(.load, b.i64_type, idx_alloca)
	live := b.map_entry_is_live(check_block, state, body_idx)
	b.emit3(.br, b.void_type, live, ValueID(body_block), ValueID(post_block))

	b.cur_block = body_block
	keys_ptr := b.map_state_field_ptr(body_block, state, 0)
	vals_ptr := b.map_state_field_ptr(body_block, state, 1)
	keys := b.emit1(.load, b.m.type_store.get_ptr(b.i8_type), keys_ptr)
	vals := b.emit1(.load, b.m.type_store.get_ptr(b.i8_type), vals_ptr)
	key_size := b.map_state_load_i64(body_block, state, 4)
	val_size := b.map_state_load_i64(body_block, state, 5)
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
	b.push_loop_targets(label, node, body_start, exit_block, post_block)
	b.build_for_in_body(node, body_start)
	b.loop_targets.delete_last()
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
	b.build_scoped_stmts(node, body_start)
}

// build_if builds if data for ssa.
fn (mut b Builder) build_if(node flat.Node) {
	saved_sum_guards := b.sum_guard_variants.clone()
	mut else_sum_guards := saved_sum_guards.clone()
	cond_id := b.a.child(&node, 0)
	cond_node := b.a.child_node(&node, 0)
	then_block := b.m.add_block(b.cur_func, 'if_then')
	merge_block := b.m.add_block(b.cur_func, 'if_merge')
	mut guard := IfGuardState{}
	mut else_terminated := false

	if cond_node.kind == .empty {
		b.emit1(.jmp, b.void_type, ValueID(then_block))
	} else {
		mut cond_val := ValueID(0)
		if cond_node.kind == .decl_assign {
			cond_val, guard = b.build_if_guard_condition(*cond_node)
		} else {
			cond_val = b.build_expr(b.a.child(&node, 0))
		}
		b.sum_guard_variants = saved_sum_guards.clone()
		b.preserve_sum_guard(cond_id, false)
		if node.children_count > 2 {
			else_block := b.m.add_block(b.cur_func, 'if_else')
			b.emit3(.br, b.void_type, cond_val, ValueID(then_block), ValueID(else_block))

			b.cur_block = else_block
			else_node := b.a.child_node(&node, 2)
			if else_node.kind == .if_expr {
				b.build_if(*else_node)
			} else if else_node.kind == .block {
				b.build_scoped_stmts(*else_node, 0)
			}
			eblk := b.m.blocks[b.cur_block]
			else_terminated = b.current_block_terminated()
			if eblk.instrs.len == 0 || !b.is_terminator(eblk.instrs.last()) {
				b.emit1(.jmp, b.void_type, ValueID(merge_block))
			}
		} else {
			b.emit3(.br, b.void_type, cond_val, ValueID(then_block), ValueID(merge_block))
		}
		else_sum_guards = b.sum_guard_variants.clone()
	}

	b.cur_block = then_block
	b.sum_guard_variants = saved_sum_guards.clone()
	b.preserve_sum_guard(cond_id, true)
	if guard.active {
		b.activate_if_guard(guard)
	}
	then_node := b.a.child_node(&node, 1)
	b.build_scoped_stmts(*then_node, 0)
	tblk := b.m.blocks[b.cur_block]
	then_terminated := b.current_block_terminated()
	then_sum_guards := b.sum_guard_variants.clone()
	if tblk.instrs.len == 0 || !b.is_terminator(tblk.instrs.last()) {
		b.emit1(.jmp, b.void_type, ValueID(merge_block))
	}
	if guard.active {
		b.restore_var_binding(guard.name, guard.old)
	}

	b.cur_block = merge_block
	if then_terminated && else_terminated {
		b.sum_guard_variants = saved_sum_guards
	} else if then_terminated {
		b.sum_guard_variants = else_sum_guards
	} else {
		b.sum_guard_variants = then_sum_guards
		if !else_terminated {
			b.intersect_sum_guards(else_sum_guards)
		}
	}
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
	if lhs.kind == .ident && rhs.kind == .index && rhs.children_count >= 2 && rhs.value_text() != 'range' {
		ok, cond, guard := b.build_map_index_if_guard(lhs.value_text(), rhs)
		if ok {
			return cond, guard
		}
	}
	if lhs.kind == .ident {
		ok, cond, guard := b.build_option_if_guard(lhs.value_text(), rhs_id)
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
	if name.len == 0 || key_type_name == 0 || val_type_name == 0 {
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
	get_ref := b.m.add_value(.func_ref, ptr_i8, 'map__get_check', b.runtime_fn_id('map__get_check'))
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
	if guard.typ_name != 0 {
		b.var_type_names.set_id(guard.name, guard.typ_name)
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
	if values.len > 0 {
		first_type := b.value_type(values[0])
		if first_type != b.void_type && values.all(b.value_type(it) == first_type) {
			return first_type
		}
		mut integer_width := 0
		mut saw_unsigned := false
		for value in values {
			typ := b.value_type(value)
			if typ > 0 && typ < b.m.type_store.types.len {
				type_info := b.m.type_store.types[typ]
				if type_info.kind == .int_t {
					integer_width = int_max(integer_width, type_info.width)
					saw_unsigned = saw_unsigned || type_info.is_unsigned
				}
			}
		}
		if integer_width > 0 {
			return if saw_unsigned {
				b.m.type_store.get_uint(integer_width)
			} else {
				b.m.type_store.get_int(integer_width)
			}
		}
	}
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			name_id := typ.source_type_id()
			if name_id != 0 && name_id !in [flat.source_type_unknown, flat.source_type_void] {
				return b.resolve_type(name_id)
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
	mut location := 'assertion failed'
	if file := b.a.source_files[int(node.pos.id)] {
		pos := file.position(node.pos)
		location = 'assertion failed at ${pos}'
	}
	if eprintln_fn := b.runtime_fn_lookup('eprintln') {
		message := b.m.add_value(.string_literal, b.str_type, location, 0)
		fn_ref := b.m.add_value(.func_ref, b.void_type, 'eprintln', eprintln_fn)
		b.emit2(.call, b.void_type, fn_ref, message)
	}
	if exit_fn := b.runtime_fn_lookup('exit') {
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
	if node.kind in [.int_literal, .infix, .prefix, .paren, .cast_expr]
		&& b.is_wide_integer_expr(id) {
		if value := b.build_wide_integer_expr(id, node) {
			return value
		}
	}
	match node.kind {
		.empty {
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.int_literal {
			// Constant folding can replace an unsigned cast with a typed literal.
			// Preserve that type so arithmetic and range checks remain unsigned.
			if typ := b.primitive_type_id(b.checked_expr_type_name(id)) {
				if b.is_unsigned_type(typ) {
					return b.m.get_or_add_const(typ, node.value_text())
				}
			}
			return b.m.get_or_add_const(b.i64_type, node.value_text())
		}
		.float_literal {
			return b.m.get_or_add_const(b.float_literal_type(id, node), node.value_text())
		}
		.bool_literal {
			val := if node.value_text() == 'true' { '1' } else { '0' }
			return b.m.get_or_add_const(b.i1_type, val)
		}
		.string_literal {
			return b.m.add_value(.string_literal, b.str_type, node.value_text(), 0)
		}
		.char_literal {
			if node.value_text().starts_with('c:') {
				return b.m.add_value(.string_literal, b.m.type_store.get_ptr(b.u8_type),
					decode_native_c_string(node.value_text()[2..]), 0)
			}
			return b.m.get_or_add_const(b.i32_type, '${char_literal_value(node.value_text())}')
		}
		.string_interp {
			return b.build_string_interp(node)
		}
		.enum_val {
			return b.build_enum_val(id, node)
		}
		.sizeof_expr {
			mut size := 0
			operand_text := node.value_text()
			if node.children_count > 0 {
				operand := b.a.child(&node, 0)
				// sizeof uses the operand's type without evaluating its value.
				size = b.sizeof_type_name(b.checked_expr_type_name(operand))
			} else if type_name_source_id := b.var_type_names.type_id(operand_text) {
				type_name_source_id_2 := type_name_source_id

				size = b.sizeof_type_name(type_name_source_id_2)
			} else if addr := b.vars[operand_text] {
				size = b.m.type_size(b.deref_type(addr))
			} else {
				size = b.sizeof_type_name(node.source_value_id())
			}
			return b.m.get_or_add_const(b.i64_type, '${size}')
		}
		.offsetof_expr {
			typ_id := b.resolve_type(node.source_value_id())
			field_name := flat.source_type_name(node.typ)
			mut offset := 0
			if typ_id > 0 && typ_id < b.m.type_store.types.len {
				typ := b.m.type_store.types[typ_id]
				field_index := typ.field_names.index(field_name)
				if field_index >= 0 {
					offset = b.m.struct_field_offset(typ_id, field_index)
				}
			}
			return b.m.get_or_add_const(b.i64_type, '${offset}')
		}
		.ident {
			if addr := b.vars[node.value_text()] {
				addr_val := b.m.values[addr]
				if addr_val.kind == .argument {
					return addr
				}
				if smart_val := b.load_smartcast_sum_value(addr, id) {
					return smart_val
				}
				if addr_val.kind == .global {
					return b.load_native_global(addr)
				}
				return b.emit1(.load, b.deref_type(addr), addr)
			}
			if node.value_text() == 'path_separator' {
				return b.m.add_value(.string_literal, b.str_type, '/', 0)
			}
			match node.value_text() {
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
					return b.m.get_or_add_const(b.resolve_type(flat.source_type_id(node.value_text()[4..])), '0')
				}
				'max_u8' {
					return b.m.get_or_add_const(b.u8_type, '255')
				}
				'max_u16' {
					return b.m.get_or_add_const(b.u16_type, '65535')
				}
				'max_u32' {
					return b.m.get_or_add_const(b.u32_type, '4294967295')
				}
				'max_u64' {
					return b.m.get_or_add_const(b.u64_type, '18446744073709551615')
				}
				else {}
			}

			if expr_id := b.lookup_const_expr(node.value_text()) {
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
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.infix {
			return b.build_infix(id, node)
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
		.spawn_expr {
			return b.build_spawn_expr(node)
		}
		.selector {
			if addr := b.native_global_selector_addr(node) {
				return b.load_native_global(addr)
			}
			return b.build_selector(node)
		}
		.index {
			return b.build_index(id, node)
		}
		.struct_init {
			return b.build_struct_init(id, node)
		}
		.array_literal {
			return b.build_array_literal(id, node)
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
		.file {
			// File roots can remain as explicit child placeholders after transform;
			// they have no runtime expression value.
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.block {
			return b.build_block_expr(node)
		}
		.lock_expr {
			if node.children_count > 0 {
				body_id := b.a.child(&node, node.children_count - 1)
				body := b.a.nodes[int(body_id)]
				if body.kind == .block {
					return b.build_block_expr(body)
				}
				b.build_stmt(body_id)
			}
			return b.m.get_or_add_const(b.i64_type, '0')
		}
		.select_stmt {
			b.build_select_placeholder(node)
			return b.m.get_or_add_const(b.i64_type, '0')
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
	mut key_type := if key_type_name != 0 {
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
	exists_ref := b.m.add_value(.func_ref, b.i1_type, 'map__exists', b.runtime_fn_id('map__exists'))
	return b.emit3(.call, b.i1_type, exists_ref, map_ptr, key_ptr)
}

// build_enum_val builds enum val data for ssa.
fn (mut b Builder) build_enum_val(id flat.NodeId, node flat.Node) ValueID {
	if node.typ != 0 {
		if value := b.enum_value_for_type(node.typ, node.value_text()) {
			return b.m.get_or_add_const(b.i64_type, value.str())
		}
	}
	raw_member := node.value_text().trim_left('.')
	member_keys := enum_member_lookup_keys(raw_member)
	for key in member_keys {
		if value := b.enum_values[key] {
			return b.m.get_or_add_const(b.i64_type, value.str())
		}
	}
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			if value := b.enum_value_for_type(typ.source_type_id(), node.value_text()) {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}
	for key in member_keys {
		member_name := key.all_after_last('.')
		if !b.enum_member_dupes[member_name] {
			if value := b.enum_member_values[member_name] {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn enum_member_lookup_keys(member string) []string {
	field := member.all_after_last('.')
	plain := if field.starts_with('@') { field[1..] } else { field }
	if token.Token.from_string_tinyv(plain).is_keyword() {
		alternate := if field.starts_with('@') { plain } else { '@' + field }
		return [member, member[..member.len - field.len] + alternate]
	}
	return [member]
}

// enum_value_for_type supports enum value for type handling for Builder.
fn (b &Builder) enum_value_for_type(type_name_id u32, member string) ?int {
	return b.enum_field_value_for_type(type_name_id, member, true)
}

fn (b &Builder) enum_field_value_for_type(type_name_id u32, member string, as_flag_bit bool) ?int {
	if type_name_id == 0 || type_name_id in [flat.source_type_int, flat.source_type_unknown] {
		return none
	}
	mut resolved_enum_id := ssa_pointer_base_source_type(type_name_id)
	for _ in 0 .. 32 {
		if target_source_id := b.type_alias_target(resolved_enum_id) {
			if target_source_id != resolved_enum_id {
				resolved_enum_id = ssa_pointer_base_source_type(target_source_id)
				continue
			}
		}
		break
	}
	resolved_enum_name := flat.source_type_name(resolved_enum_id)
	raw_member := member.trim_left('.')
	member_keys := enum_member_lookup_keys(raw_member)
	mut names := []string{}
	names << resolved_enum_name
	short_type := resolved_enum_name.all_after('.')
	if short_type != resolved_enum_name {
		names << short_type
	}
	for candidate in member_keys {
		if value := b.enum_values[candidate] {
			enum_name := candidate.all_before_last('.')
			return if as_flag_bit && b.is_flag_enum_type_name(flat.source_type_id(enum_name)) {
				1 << value
			} else {
				value
			}
		}
		clean_member := candidate.all_after_last('.')
		for name in names {
			key := name + '.' + clean_member
			if value := b.enum_values[key] {
				return if as_flag_bit && b.is_flag_enum_type_name(flat.source_type_id(name)) {
					1 << value
				} else {
					value
				}
			}
		}
	}
	return none
}

// is_flag_enum_type_name reports whether is flag enum type name applies in ssa.
fn (b &Builder) is_flag_enum_type_name(type_name_id u32) bool {
	type_name := flat.source_type_name(type_name_id)
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
fn (b &Builder) checker_has_flag_enum(type_name_id u32) bool {
	type_name := flat.source_type_name(type_name_id)
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
fn (mut b Builder) build_enum_val_with_type(node flat.Node, type_name_id u32) ?ValueID {
	if value := b.enum_value_for_type(type_name_id, node.value_text()) {
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
		if value[1] in [`x`, `u`, `U`] {
			return int(global_integer_literal('0x' + value[2..]))
		}
		if value.len == 4 && value[1] >= `0` && value[1] <= `7` {
			return int(global_integer_literal('0o' + value[1..]))
		}
		return match value[1] {
			`a` { 7 }
			`b` { 8 }
			`e` { 27 }
			`f` { 12 }
			`n` { 10 }
			`r` { 13 }
			`t` { 9 }
			`v` { 11 }
			`0` { 0 }
			`\\` { 92 }
			`'` { 39 }
			else { int(value[1]) }
		}
	}
	return int(value.runes()[0])
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

fn (mut b Builder) build_array_literal(id flat.NodeId, node flat.Node) ValueID {
	if b.is_fixed_array_type_name(node.typ) {
		mut fixed_node := node
		fixed_node.set_source_value_id(node.typ)
		return b.build_fixed_array_init(fixed_node)
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	mut values := []ValueID{}
	mut elem_type := b.i64_type
	mut fixed_elem_type_name_id := u32(0)
	for i in 0 .. node.children_count {
		value := b.build_expr(b.a.child(&node, i))
		if i == 0 {
			elem_type = b.value_type(value)
		}
		values << value
	}
	// Integer literals type wider than the array element: an untyped `1` is
	// i64 while `[]int` holds i32. Take the width from the array type when the
	// checker knows it, or the header stride disagrees with every reader,
	// which all use the element type, and elements read back shifted.
	arr_type_id := b.checked_expr_type_name(id)
	arr_type := flat.source_type_name(arr_type_id)
	if flat.source_type_shape(arr_type_id).has(.array_type) && arr_type.len > 2 {
		elem_type_name_id := indexed_elem_type_name(arr_type_id)
		elem_type = b.resolve_type(elem_type_name_id)
		if b.is_fixed_array_type_name(elem_type_name_id) {
			fixed_elem_type_name_id = elem_type_name_id
		}
	} else if node.children_count > 0 {
		mut all_int_literals := true
		for index in 0 .. node.children_count {
			if !b.is_integer_literal_expr(b.a.child(&node, index)) {
				all_int_literals = false
				break
			}
		}
		if all_int_literals {
			elem_type = b.i32_type
		}
	}
	mut coerced_values := []ValueID{cap: values.len}
	for value in values {
		coerced_values << b.coerce_numeric_value(value, elem_type)
	}
	elem_size := if fixed_elem_type_name_id != 0 {
		b.sizeof_type_name(fixed_elem_type_name_id)
	} else {
		b.m.type_size(elem_type)
	}
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	elem_size_const := b.m.get_or_add_const(b.i64_type, '${actual_elem_size}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	cap_const := b.m.get_or_add_const(b.i64_type, '${node.children_count}')
	new_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.runtime_fn_id('array_new'))
	arr := b.emit4(.call, b.array_type, new_ref, elem_size_const, zero, cap_const)
	arr_alloca := b.emit0(.alloca, ptr_array)
	b.emit2(.store, b.void_type, arr, arr_alloca)
	push_ref := b.m.add_value(.func_ref, b.void_type, 'array_push', b.runtime_fn_id('array_push'))
	for value in coerced_values {
		if fixed_elem_type_name_id != 0 {
			value_arg := if b.value_type(value) == ptr_i8 {
				value
			} else {
				b.emit1(.bitcast, ptr_i8, value)
			}
			b.emit3(.call, b.void_type, push_ref, arr_alloca, value_arg)
			continue
		}
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
	if b.is_fixed_array_type_name(node.source_value_id()) {
		return b.build_fixed_array_init(node)
	}
	mut elem_type_id := node.source_value_id()
	mut len_val := b.m.get_or_add_const(b.i64_type, '0')
	mut cap_val := b.m.get_or_add_const(b.i64_type, '0')
	mut init_val := ValueID(0)
	if flat.source_type_shape(elem_type_id).has(.leading_bracket) {
		type_text := node.value_text()
		len_text := type_text.all_after('[').all_before(']')
		len_val = b.fixed_array_len_value(len_text)
		cap_val = len_val
		elem_type_id = flat.source_type_id(type_text.all_after(']'))
	} else {
		mut has_cap := false
		for i in 0 .. node.children_count {
			child_id := b.a.child(&node, i)
			child := b.a.nodes[int(child_id)]
			if child.kind == .field_init && child.children_count > 0 {
				value := b.build_expr(b.a.child(&child, 0))
				if child.value_text() == 'len' {
					len_val = value
					if !has_cap {
						cap_val = value
					}
				} else if child.value_text() == 'cap' {
					cap_val = value
					has_cap = true
				} else if child.value_text() == 'init' {
					init_val = value
				}
			}
		}
	}
	elem_type := b.resolve_type(elem_type_id)
	elem_size := b.m.type_size(elem_type)
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	elem_size_const := b.m.get_or_add_const(b.i64_type, '${actual_elem_size}')
	new_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.runtime_fn_id('array_new'))
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
		return b.default_value_for_type(b.resolve_type(node.source_value_id()))
	}
	child_id := b.a.child(&node, 0)
	child_type_name_source_id := b.checked_expr_type_name(child_id)

	sum_name_source_id_2 := b.canonical_sum_type_name(child_type_name_source_id) or { return b.build_expr(child_id) }

	field_name := sum_field_name(node.value_text())
	sum_val := b.build_expr(child_id)
	sum_type := b.resolve_type(sum_name_source_id_2)
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
	child_type_name_source_id := b.checked_expr_type_name(child_id)

	sum_name_source_id_3 := b.canonical_sum_type_name(child_type_name_source_id) or {
		return b.m.get_or_add_const(b.i1_type, '1')
	}

	tag := b.sum_variant_index(sum_name_source_id_3, node.source_value_id())
	if tag <= 0 {
		return b.m.get_or_add_const(b.i1_type, '0')
	}
	sum_val := b.build_expr(child_id)
	sum_type := b.resolve_type(sum_name_source_id_3)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(sum_type))
	b.emit2(.store, b.void_type, sum_val, alloca)
	tag_ptr := b.get_field_ptr(alloca, 'typ')
	tag_val := b.emit1(.load, b.deref_type(tag_ptr), tag_ptr)
	tag_const := b.m.get_or_add_const(b.value_type(tag_val), '${tag}')
	return b.emit2(.eq, b.i1_type, tag_val, tag_const)
}

fn (mut b Builder) build_map_init(node flat.Node) ValueID {
	key_type_name, val_type_name := map_type_parts(node.source_value_id())
	if key_type_name == 0 || val_type_name == 0 {
		return b.default_value_for_type(b.map_type)
	}
	key_type := b.resolve_type(key_type_name)
	val_type := b.resolve_type(val_type_name)
	key_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(key_type)}')
	val_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(val_type)}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	mut args := []ValueID{}
	if fn_id := b.runtime_fn_lookup('new_map') {
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
		if set_id := b.runtime_fn_lookup('v3_map_set_sized') {
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

fn (b &Builder) is_fixed_array_type_name(type_name_id u32) bool {
	type_name := flat.source_type_name(type_name_id)
	if type_name.starts_with('[') {
		idx := type_name.index_u8(`]`)
		return idx > 1 && idx + 1 < type_name.len
	}
	if type_name.contains('[') && type_name.ends_with(']') {
		idx := type_name.index_u8(`[`)
		if idx <= 0 || idx >= type_name.len - 1 {
			return false
		}
		length_text := type_name[idx + 1..type_name.len - 1].replace('_', '')
		return b.is_decimal_int_text(length_text) || b.lookup_const_expr(length_text) != none
	}
	return false
}

fn (b &Builder) fixed_array_elem_type_name(type_name_id u32) u32 {
	type_name := flat.source_type_name(type_name_id)
	if type_name.starts_with('[') {
		idx := type_name.index_u8(`]`)
		if idx > 0 && idx + 1 < type_name.len {
			return flat.source_type_id(type_name[idx + 1..])
		}
	} else if type_name.contains('[') && type_name.ends_with(']') {
		idx := type_name.index_u8(`[`)
		if idx > 0 {
			return flat.source_type_id(type_name[..idx])
		}
	}
	return flat.source_type_u8
}

fn (b &Builder) fixed_array_len_text(type_name_id u32) string {
	type_name := flat.source_type_name(type_name_id)
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

fn (mut b Builder) sizeof_type_name(type_name_id u32) int {
	if b.is_fixed_array_type_name(type_name_id) {
		// Recurse so nested fixed arrays (`[2][3]u8`) use their element's full size.
		return b.fixed_array_len_text(type_name_id).int() * b.sizeof_type_name(b.fixed_array_elem_type_name(type_name_id))
	}
	return b.m.type_size(b.resolve_type(type_name_id))
}

fn (mut b Builder) build_fixed_array_init(node flat.Node) ValueID {
	elem_type_name_source_id := b.fixed_array_elem_type_name(node.source_value_id())

	elem_type := b.resolve_type(elem_type_name_source_id)
	ptr_elem := b.m.type_store.get_ptr(elem_type)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	len_text := b.fixed_array_len_text(node.source_value_id())
	len_val := b.m.get_or_add_const(b.i64_type, len_text)
	nested_fixed := b.is_fixed_array_type_name(elem_type_name_source_id)
	elem_size := if nested_fixed {
		b.sizeof_type_name(elem_type_name_source_id)
	} else {
		b.m.type_size(elem_type)
	}
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	alloca_count := if nested_fixed {
		b.m.get_or_add_const(b.i64_type, '${len_text.int() * actual_elem_size}')
	} else {
		len_val
	}
	alloca_type := if nested_fixed { ptr_i8 } else { ptr_elem }
	alloca := b.emit1(.alloca, alloca_type, alloca_count)
	data := if ptr_elem == ptr_i8 {
		alloca
	} else {
		b.emit1(.bitcast, ptr_i8, alloca)
	}

	byte_count := b.m.get_or_add_const(b.i64_type, '${len_text.int() * actual_elem_size}')
	zero := b.m.get_or_add_const(b.i64_type, '0')
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset', b.runtime_fn_id('memset'))
	b.emit4(.call, ptr_i8, memset_ref, data, zero, byte_count)

	mut elem_index := 0
	for i in 0 .. node.children_count {
		child_id := b.a.child(&node, i)
		child := b.a.nodes[int(child_id)]
		if child.kind == .field_init {
			continue
		}
		value := b.coerce_numeric_value(b.build_expr(child_id), elem_type)
		offset := b.m.get_or_add_const(b.i64_type, '${elem_index * actual_elem_size}')
		slot_i8 := b.emit2(.get_element_ptr, ptr_i8, data, offset)
		if nested_fixed {
			source := if b.value_type(value) == ptr_i8 {
				value
			} else {
				b.emit1(.bitcast, ptr_i8, value)
			}
			memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy',
				b.runtime_fn_id('memcpy'))
			size := b.m.get_or_add_const(b.i64_type, '${actual_elem_size}')
			b.emit4(.call, ptr_i8, memcpy_ref, slot_i8, source, size)
		} else {
			slot := if ptr_elem == ptr_i8 {
				slot_i8
			} else {
				b.emit1(.bitcast, ptr_elem, slot_i8)
			}
			b.emit2(.store, b.void_type, value, slot)
		}
		elem_index++
	}
	if nested_fixed {
		return b.emit1(.bitcast, b.resolve_type(node.source_value_id()), data)
	}
	return alloca
}

fn (mut b Builder) build_or_expr(id flat.NodeId, node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	expr_id := b.a.child(&node, 0)
	expr := b.a.nodes[int(expr_id)]
	if expr.kind == .index && expr.children_count >= 2 && expr.value_text() != 'range' {
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
	if key_type_name == 0 || val_type_name == 0 {
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
	get_ref := b.m.add_value(.func_ref, ptr_i8, 'map__get_check', b.runtime_fn_id('map__get_check'))
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

fn (mut b Builder) or_result_type(id flat.NodeId, fallback_type_id u32) TypeID {
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			name_id := typ.source_type_id()
			if name_id != 0 && name_id != flat.source_type_unknown {
				return b.resolve_type(name_id)
			}
		}
	}
	return b.resolve_type(fallback_type_id)
}

fn (mut b Builder) default_value_for_type(typ TypeID) ValueID {
	if b.is_option_type(typ) {
		return b.build_option_value(typ, false, ValueID(0))
	}
	if typ == b.str_type {
		return b.m.add_value(.string_literal, b.str_type, '', 0)
	}
	if typ == b.array_type {
		fn_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.runtime_fn_id('array_new'))
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
	err_index := b.m.type_store.types[opt_typ].field_names.index('err')
	if err_index >= 0 {
		err_type := b.m.type_store.types[opt_typ].fields[err_index]
		err_ptr := b.block_struct_field_ptr(b.cur_block, alloca, opt_typ, err_index)
		err_value := b.m.get_or_add_const(err_type, '0')
		b.emit2(.store, b.void_type, err_value, err_ptr)
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

fn (mut b Builder) wide_integer_type() TypeID {
	if b.wide_int_type > 0 {
		return b.wide_int_type
	}
	b.wide_int_type = b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [b.u64_type, b.u64_type]
		field_names: ['low', 'high']
	})
	return b.wide_int_type
}

fn (mut b Builder) is_wide_integer_expr(id flat.NodeId) bool {
	name := flat.source_type_name(b.checked_expr_type_name(id)).trim_left('&')
	if name in ['u128', 'i128'] {
		return true
	}
	node := b.a.nodes[int(id)]
	node_type := flat.source_type_name(node.typ).trim_left('&')
	if node_type in ['u128', 'i128'] {
		return true
	}
	if b.tc != unsafe { nil } {
		return b.ssa_type_from_checker_type(b.tc.resolve_type(id)) == b.wide_integer_type()
	}
	return false
}

fn (mut b Builder) make_wide_integer(block BlockID, low ValueID, high ValueID) ValueID {
	wide_type := b.wide_integer_type()
	slot := b.block_instr0(.alloca, block, b.m.type_store.get_ptr(wide_type))
	low_ptr := b.block_struct_field_ptr(block, slot, wide_type, 0)
	high_ptr := b.block_struct_field_ptr(block, slot, wide_type, 1)
	b.block_instr2(.store, block, b.void_type, low, low_ptr)
	b.block_instr2(.store, block, b.void_type, high, high_ptr)
	return b.block_instr1(.load, block, wide_type, slot)
}

fn (mut b Builder) wide_integer_limbs(block BlockID, value ValueID) (ValueID, ValueID) {
	wide_type := b.wide_integer_type()
	slot := b.block_instr0(.alloca, block, b.m.type_store.get_ptr(wide_type))
	b.block_instr2(.store, block, b.void_type, value, slot)
	low_ptr := b.block_struct_field_ptr(block, slot, wide_type, 0)
	high_ptr := b.block_struct_field_ptr(block, slot, wide_type, 1)
	low := b.block_instr1(.load, block, b.u64_type, low_ptr)
	high := b.block_instr1(.load, block, b.u64_type, high_ptr)
	return low, high
}

fn (mut b Builder) wide_integer_from_scalar(value ValueID) ValueID {
	low := b.coerce_numeric_value(value, b.u64_type)
	return b.make_wide_integer(b.cur_block, low, b.m.get_or_add_const(b.u64_type, '0'))
}

fn (mut b Builder) wide_integer_operand(id flat.NodeId) ValueID {
	value := b.build_expr(id)
	if b.is_wide_integer_expr(id) {
		return value
	}
	return b.wide_integer_from_scalar(value)
}

fn (mut b Builder) build_wide_integer_expr(id flat.NodeId, node flat.Node) ?ValueID {
	match node.kind {
		.int_literal {
			low := b.m.get_or_add_const(b.u64_type, node.value)
			return b.make_wide_integer(b.cur_block, low,
				b.m.get_or_add_const(b.u64_type, '0'))
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			child_id := b.a.child(&node, 0)
			if b.is_wide_integer_expr(child_id) {
				return b.build_expr(child_id)
			}
			return b.wide_integer_from_scalar(b.build_expr(child_id))
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			return b.build_expr(b.a.child(&node, 0))
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			lhs_id := b.a.child(&node, 0)
			rhs_id := b.a.child(&node, 1)
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
				shift_text := b.const_int_literal_value(rhs_id) or { return none }
				shift := shift_text.int()
				if shift < 0 || shift >= 128 {
					zero := b.m.get_or_add_const(b.u64_type, '0')
					return b.make_wide_integer(b.cur_block, zero, zero)
				}
				lhs := b.build_expr(lhs_id)
				low, high := b.wide_integer_limbs(b.cur_block, lhs)
				zero := b.m.get_or_add_const(b.u64_type, '0')
				if node.op == .left_shift {
					if shift >= 64 {
						next_high := b.block_instr2(.shl, b.cur_block, b.u64_type, low,
							b.m.get_or_add_const(b.u64_type, '${shift - 64}'))
						return b.make_wide_integer(b.cur_block, zero, next_high)
					}
					next_low := b.block_instr2(.shl, b.cur_block, b.u64_type, low,
						b.m.get_or_add_const(b.u64_type, '${shift}'))
					next_high := b.block_instr2(.shl, b.cur_block, b.u64_type, high,
						b.m.get_or_add_const(b.u64_type, '${shift}'))
					if shift == 0 {
						return b.make_wide_integer(b.cur_block, next_low, next_high)
					}
					carry := b.block_instr2(.lshr, b.cur_block, b.u64_type, low,
						b.m.get_or_add_const(b.u64_type, '${64 - shift}'))
					combined_high := b.block_instr2(.or_, b.cur_block, b.u64_type,
						next_high, carry)
					return b.make_wide_integer(b.cur_block, next_low, combined_high)
				}
				if shift >= 64 {
					next_low := b.block_instr2(.lshr, b.cur_block, b.u64_type, high,
						b.m.get_or_add_const(b.u64_type, '${shift - 64}'))
					return b.make_wide_integer(b.cur_block, next_low, zero)
				}
				next_low := b.block_instr2(.lshr, b.cur_block, b.u64_type, low,
					b.m.get_or_add_const(b.u64_type, '${shift}'))
				next_high := b.block_instr2(.lshr, b.cur_block, b.u64_type, high,
					b.m.get_or_add_const(b.u64_type, '${shift}'))
				if shift == 0 {
					return b.make_wide_integer(b.cur_block, next_low, next_high)
				}
				carry := b.block_instr2(.shl, b.cur_block, b.u64_type, high,
					b.m.get_or_add_const(b.u64_type, '${64 - shift}'))
				combined_low := b.block_instr2(.or_, b.cur_block, b.u64_type, next_low,
					carry)
				return b.make_wide_integer(b.cur_block, combined_low, next_high)
			}
			lhs := b.wide_integer_operand(lhs_id)
			rhs := b.wide_integer_operand(rhs_id)
			lhs_low, lhs_high := b.wide_integer_limbs(b.cur_block, lhs)
			rhs_low, rhs_high := b.wide_integer_limbs(b.cur_block, rhs)
			match node.op {
				.pipe, .amp, .xor {
					op := match node.op {
						.pipe { OpCode.or_ }
						.amp { OpCode.and_ }
						else { OpCode.xor }
					}
					low := b.block_instr2(op, b.cur_block, b.u64_type, lhs_low, rhs_low)
					high := b.block_instr2(op, b.cur_block, b.u64_type, lhs_high, rhs_high)
					return b.make_wide_integer(b.cur_block, low, high)
				}
				.plus, .minus {
					op := if node.op == .plus { OpCode.add } else { OpCode.sub }
					low := b.block_instr2(op, b.cur_block, b.u64_type, lhs_low, rhs_low)
					carry := if node.op == .plus {
						b.block_instr2(.ult, b.cur_block, b.i1_type, low, lhs_low)
					} else {
						b.block_instr2(.ult, b.cur_block, b.i1_type, lhs_low, rhs_low)
					}
					high_base := b.block_instr2(op, b.cur_block, b.u64_type, lhs_high,
						rhs_high)
					carry64 := b.block_instr1(.zext, b.cur_block, b.u64_type, carry)
					carry_op := if node.op == .plus { OpCode.add } else { OpCode.sub }
					high := b.block_instr2(carry_op, b.cur_block, b.u64_type, high_base,
						carry64)
					return b.make_wide_integer(b.cur_block, low, high)
				}
				else {
					return none
				}
			}
		}
		else {
			return none
		}
	}
}

fn (mut b Builder) build_infix(id flat.NodeId, node flat.Node) ValueID {
	if node.op == .power {
		panic('power expression reached the V3 SSA backend')
	}
	if node.op == .logical_and || node.op == .logical_or {
		return b.build_short_circuit(node)
	}
	if node.op == .left_shift && node.value_text() == 'push' {
		return b.build_array_push_expr(node)
	}
	if result := b.build_qualified_infix_call(node) {
		return result
	}
	lhs_id := b.a.child(&node, 0)
	rhs_id := b.a.child(&node, 1)
	lhs_node := b.a.nodes[int(lhs_id)]
	rhs_node := b.a.nodes[int(rhs_id)]
	lhs_type_name_source_id := b.checked_expr_type_name(lhs_id)

	rhs_type_name_source_id_2 := b.checked_expr_type_name(rhs_id)

	mut lhs := if lhs_node.kind == .enum_val && rhs_type_name_source_id_2 != 0 {
		b.build_enum_val_with_type(lhs_node, rhs_type_name_source_id_2) or { b.build_expr(lhs_id) }
	} else {
		b.build_expr(lhs_id)
	}
	mut rhs := if rhs_node.kind == .enum_val && lhs_type_name_source_id != 0 {
		b.build_enum_val_with_type(rhs_node, lhs_type_name_source_id) or { b.build_expr(rhs_id) }
	} else {
		b.build_expr(rhs_id)
	}
	if node.op in [.amp, .pipe, .xor] {
		if b.is_pointer_type(b.value_type(lhs)) && b.is_flag_enum_expr(lhs_id) {
			lhs = b.emit1(.load, b.deref_type(lhs), lhs)
		}
		if b.is_pointer_type(b.value_type(rhs)) && b.is_flag_enum_expr(rhs_id) {
			rhs = b.emit1(.load, b.deref_type(rhs), rhs)
		}
	}
	if b.value_type(lhs) == b.str_type || b.value_type(rhs) == b.str_type {
		return b.build_string_infix(node.op, lhs, rhs)
	}
	lhs_infix_type := b.value_type(lhs)
	rhs_infix_type := b.value_type(rhs)
	if node.op in [.plus, .minus] {
		if b.is_pointer_type(lhs_infix_type) && b.is_int_type(rhs_infix_type) {
			elem_size := b.m.type_size(b.deref_type(lhs))
			mut offset := b.coerce_int_value(rhs, b.i64_type)
			if elem_size > 1 {
				size := b.m.get_or_add_const(b.i64_type, '${elem_size}')
				offset = b.emit2(.mul, b.i64_type, offset, size)
			}
			op := if node.op == .plus { OpCode.add } else { OpCode.sub }
			return b.emit2(op, lhs_infix_type, lhs, offset)
		}
		if node.op == .plus && b.is_int_type(lhs_infix_type)
			&& b.is_pointer_type(rhs_infix_type) {
			elem_size := b.m.type_size(b.deref_type(rhs))
			mut offset := b.coerce_int_value(lhs, b.i64_type)
			if elem_size > 1 {
				size := b.m.get_or_add_const(b.i64_type, '${elem_size}')
				offset = b.emit2(.mul, b.i64_type, offset, size)
			}
			return b.emit2(.add, rhs_infix_type, rhs, offset)
		}
	}
	if b.tc != unsafe { nil } {
		lhs_literal := b.is_integer_literal_expr(lhs_id)
		rhs_literal := b.is_integer_literal_expr(rhs_id)
		if node.op in [.eq, .ne, .lt, .gt, .le, .ge] {
			if lhs_literal && !rhs_literal {
				lhs = b.coerce_comparison_literal(lhs_id, lhs, b.value_type(rhs))
			} else if rhs_literal && !lhs_literal {
				rhs = b.coerce_comparison_literal(rhs_id, rhs, b.value_type(lhs))
			}
			if b.is_int_type(b.value_type(lhs)) && b.is_int_type(b.value_type(rhs)) {
				if b.is_unsigned_type(b.value_type(lhs)) != b.is_unsigned_type(b.value_type(rhs)) {
					width := int_max(b.scalar_type_width(b.value_type(lhs)), b.scalar_type_width(b.value_type(rhs)))
					if b.m.target.ptr_size == 8 {
						common_unsigned := b.m.type_store.get_uint(width)
						lhs = b.coerce_numeric_value(lhs, common_unsigned)
						rhs = b.coerce_numeric_value(rhs, common_unsigned)
					}
					lhs_value := b.m.values[lhs]
					rhs_value := b.m.values[rhs]
					if lhs_value.kind == .constant && !lhs_value.name.starts_with('-')
						&& !b.is_unsigned_type(b.value_type(lhs)) {
						lhs = b.coerce_numeric_value(lhs, b.m.type_store.get_uint(width))
					} else if rhs_value.kind == .constant && !rhs_value.name.starts_with('-')
						&& !b.is_unsigned_type(b.value_type(rhs)) {
						rhs = b.coerce_numeric_value(rhs, b.m.type_store.get_uint(width))
					}
				}
				if b.is_unsigned_type(b.value_type(lhs)) != b.is_unsigned_type(b.value_type(rhs)) {
					return b.build_mixed_integer_comparison(node.op, lhs, rhs)
				}
				common := if b.is_unsigned_type(b.value_type(lhs)) {
					b.m.type_store.get_uint(int_max(b.scalar_type_width(b.value_type(lhs)),
						b.scalar_type_width(b.value_type(rhs))))
				} else {
					b.m.type_store.get_int(int_max(b.scalar_type_width(b.value_type(lhs)),
						b.scalar_type_width(b.value_type(rhs))))
				}
				lhs = b.coerce_int_value(lhs, common)
				rhs = b.coerce_int_value(rhs, common)
			} else if b.is_float_type(b.value_type(lhs)) || b.is_float_type(b.value_type(rhs)) {
				lhs_float_literal := b.is_float_literal_expr(lhs_id)
				rhs_float_literal := b.is_float_literal_expr(rhs_id)
				if lhs_float_literal && !rhs_float_literal && b.is_float_type(b.value_type(rhs)) {
					lhs = b.coerce_numeric_value(lhs, b.value_type(rhs))
				} else if rhs_float_literal && !lhs_float_literal
					&& b.is_float_type(b.value_type(lhs)) {
					rhs = b.coerce_numeric_value(rhs, b.value_type(lhs))
				}
				common := if b.value_type(lhs) == b.f64_type || b.value_type(rhs) == b.f64_type {
					b.f64_type
				} else {
					b.f32_type
				}
				lhs = b.coerce_numeric_value(lhs, common)
				rhs = b.coerce_numeric_value(rhs, common)
			}
		} else if b.m.target.ptr_size == 4 {
			// Untyped literals must keep their bits until the operation's type is known.
			if !lhs_literal {
				lhs = b.coerce_numeric_value(lhs, b.global_constant_type(lhs_id))
			}
			if !rhs_literal {
				rhs = b.coerce_numeric_value(rhs, b.global_constant_type(rhs_id))
			}
			result := b.global_constant_type(id)
			lhs = b.coerce_numeric_value(lhs, result)
			if node.op !in [.left_shift, .right_shift, .right_shift_unsigned] {
				rhs = b.coerce_numeric_value(rhs, result)
			}
		} else {
			// Native scalar operations use a common expression type. In
			// particular, u8 * i64 must promote the byte before multiplication
			// instead of truncating the signed result back to u8.
			mut result := b.global_constant_type(id)
			if b.is_int_type(result) {
				lhs_value_type := b.value_type(lhs)
				rhs_value_type := b.value_type(rhs)
				if b.is_int_type(lhs_value_type) {
					mut width := b.scalar_type_width(lhs_value_type)
					if node.op !in [.left_shift, .right_shift, .right_shift_unsigned]
						&& b.is_int_type(rhs_value_type) {
						width = int_max(width, b.scalar_type_width(rhs_value_type))
					}
					if width > b.scalar_type_width(result) {
						unsigned := b.is_unsigned_type(lhs_value_type)
							&& (node.op in [.left_shift, .right_shift, .right_shift_unsigned]
								|| b.is_unsigned_type(rhs_value_type))
						result = if unsigned {
							b.m.type_store.get_uint(width)
						} else {
							b.m.type_store.get_int(width)
						}
					}
				}
			}
			if b.is_int_type(result) {
				if b.is_int_type(b.value_type(lhs)) {
					lhs = b.coerce_numeric_value(lhs, result)
				}
				if node.op !in [.left_shift, .right_shift, .right_shift_unsigned]
					&& b.is_int_type(b.value_type(rhs)) {
					rhs = b.coerce_numeric_value(rhs, result)
				}
			} else if b.is_float_type(result) {
				if b.is_float_type(b.value_type(lhs)) || b.is_int_type(b.value_type(lhs)) {
					lhs = b.coerce_numeric_value(lhs, result)
				}
				if b.is_float_type(b.value_type(rhs)) || b.is_int_type(b.value_type(rhs)) {
					rhs = b.coerce_numeric_value(rhs, result)
				}
			}
		}
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
			.right_shift_unsigned {
				op = OpCode.lshr
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

fn (mut b Builder) build_mixed_integer_comparison(op flat.Op, lhs ValueID, rhs ValueID) ValueID {
	lhs_signed := !b.is_unsigned_type(b.value_type(lhs))
	signed_value := if lhs_signed { lhs } else { rhs }
	zero := b.m.get_or_add_const(b.value_type(signed_value), '0')
	negative := b.emit2(.lt, b.i1_type, signed_value, zero)
	width := int_max(b.scalar_type_width(b.value_type(lhs)), b.scalar_type_width(b.value_type(rhs)))
	common := b.m.type_store.get_uint(width)
	left := b.coerce_int_value(lhs, common)
	right := b.coerce_int_value(rhs, common)
	comparison_op := match op {
		.eq { OpCode.eq }
		.ne { OpCode.ne }
		.lt { OpCode.ult }
		.le { OpCode.ule }
		.gt { OpCode.ugt }
		.ge { OpCode.uge }
		else { OpCode.eq }
	}
	nonnegative_result := b.emit2(comparison_op, b.i1_type, left, right)
	negative_result := match op {
		.lt, .le { lhs_signed }
		.gt, .ge { !lhs_signed }
		.ne { true }
		else { false }
	}
	constant := b.m.get_or_add_const(b.i1_type, if negative_result { '1' } else { '0' })
	return b.emit3(.select, b.i1_type, negative, constant, nonnegative_result)
}

fn int_max(left int, right int) int {
	return if left > right { left } else { right }
}

fn (b &Builder) is_integer_literal_expr(id flat.NodeId) bool {
	if !b.valid_node_id(id) {
		return false
	}
	node := b.a.nodes[int(id)]
	if node.kind == .int_literal {
		return true
	}
	if node.children_count > 0 && (node.kind == .paren
		|| (node.kind == .prefix && node.op in [.plus, .minus])) {
		return b.is_integer_literal_expr(b.a.child(&node, 0))
	}
	return false
}

fn (b &Builder) is_float_literal_expr(id flat.NodeId) bool {
	if !b.valid_node_id(id) {
		return false
	}
	node := b.a.nodes[int(id)]
	if node.kind == .float_literal {
		return true
	}
	if node.children_count > 0 && (node.kind == .paren
		|| (node.kind == .prefix && node.op in [.plus, .minus])) {
		return b.is_float_literal_expr(b.a.child(&node, 0))
	}
	return false
}

fn (b &Builder) is_negative_integer_literal_expr(id flat.NodeId) bool {
	node := b.a.nodes[int(id)]
	if node.kind == .int_literal {
		return node.value_text().starts_with('-')
	}
	if node.kind == .prefix && node.op == .minus {
		return !b.is_negative_integer_literal_expr(b.a.child(&node, 0))
	}
	if node.children_count > 0 {
		return b.is_negative_integer_literal_expr(b.a.child(&node, 0))
	}
	return false
}

fn (mut b Builder) coerce_comparison_literal(id flat.NodeId, value ValueID, peer_type TypeID) ValueID {
	mut target := peer_type
	value_type := b.value_type(value)
	if b.is_int_type(value_type) && b.is_int_type(peer_type) {
		width := int_max(b.scalar_type_width(value_type), b.scalar_type_width(peer_type))
		target = if b.is_unsigned_type(peer_type) && !b.is_negative_integer_literal_expr(id) {
			b.m.type_store.get_uint(width)
		} else {
			b.m.type_store.get_int(width)
		}
	}
	return b.coerce_numeric_value(value, target)
}

fn (mut b Builder) coerce_numeric_value(value ValueID, target TypeID) ValueID {
	return b.coerce_numeric_value_in_block(value, target, b.cur_block)
}

fn (mut b Builder) coerce_numeric_value_in_block(value ValueID, target TypeID, block_id BlockID) ValueID {
	from := b.value_type(value)
	if from == target {
		return value
	}
	mut op := OpCode.bitcast
	if b.is_int_type(from) && b.is_int_type(target) {
		from_width := b.scalar_type_width(from)
		to_width := b.scalar_type_width(target)
		if to_width > 0 && from_width > 0 && to_width < from_width {
			op = .trunc
		} else if to_width > from_width {
			op = if b.is_unsigned_type(from) { OpCode.zext } else { OpCode.sext }
		}
	} else if b.is_int_type(from) && b.is_float_type(target) {
		op = if b.is_unsigned_type(from) { OpCode.uitofp } else { OpCode.sitofp }
	} else if b.is_float_type(from) && b.is_int_type(target) {
		op = if b.is_unsigned_type(target) { OpCode.fptoui } else { OpCode.fptosi }
	} else if !b.is_float_type(from) || !b.is_float_type(target) {
		return value
	}
	return b.block_instr1(op, block_id, target, value)
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
	if fn_node.kind != .ident || fn_node.value_text().len == 0 {
		return none
	}
	for candidate in ['${lhs_name}.${fn_node.value_text()}', '${lhs_name}__${fn_node.value_text()}'] {
		if candidate in b.fn_ids {
			return b.build_direct_call(candidate, rhs)
		}
	}
	return none
}

fn (mut b Builder) build_direct_call(resolved_name string, node flat.Node) ValueID {
	mut fn_idx := b.fn_ids[resolved_name] or {
		panic('ssa: unknown function `${resolved_name}`')
	}
	if resolved_name in native_runtime_override_names {
		if runtime_idx := b.runtime_fn_ids[resolved_name] {
			fn_idx = runtime_idx
		}
	}
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
					if addr := b.vars[arg_node.value_text()] {
						if b.should_pass_ident_addr_for_ptr_param(addr, param_type, true) {
							args << addr
							continue
						}
					}
				} else if arg_node.kind == .selector {
					addr := b.build_selector_addr(arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type, true) {
						args << addr
						continue
					}
				} else if arg_node.kind == .index {
					addr := b.build_index_addr(arg_id, arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type, true) {
						args << addr
						continue
					}
				} else if arg_node.kind == .prefix && arg_node.op == .mul {
					addr := b.build_expr(b.a.child(&arg_node, 0))
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type, true) {
						args << addr
						continue
					}
				}
			}
		}
		args << if param_idx < param_types.len {
			b.build_typed_call_arg(arg_id, param_types[param_idx], true)
		} else {
			b.build_expr(arg_id)
		}
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
			return node.value_text()
		}
		.selector {
			if node.children_count == 0 {
				return node.value_text()
			}
			base := b.qualified_expr_name(b.a.child(&node, 0))
			if base.len == 0 {
				return node.value_text()
			}
			return '${base}.${node.value_text()}'
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
	if fn_idx := b.runtime_fn_lookup(name) {
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
	target_name_id := if node.typ != 0 { node.typ } else { node.source_value_id() }
	if target_name_id == 0 {
		return b.build_expr(b.a.child(&node, 0))
	}
	if value := b.build_sum_type_cast(target_name_id, b.a.child(&node, 0)) {
		return value
	}
	child_id := b.a.child(&node, 0)
	mut value := b.build_expr(child_id)
	child := b.a.node(child_id)
	if child.kind == .ident {
		if semantic_type := b.mut_pointer_params[child.value_text()] {
			if b.is_pointer_type(b.value_type(value)) && b.deref_type(value) == semantic_type {
				// An explicit cast reads a mutable pointer's value, including casts
				// whose target remains a pointer. Its ABI slot is not that value.
				value = b.emit1(.load, semantic_type, value)
			}
		}
	}
	checked_name_source_id := b.checked_expr_type_name(child_id)

	if checked_name_source_id != 0 && checked_name_source_id != flat.source_type_unknown {
		checked_type := b.resolve_type(checked_name_source_id)
		if b.is_pointer_type(b.value_type(value)) && b.value_type(value) != checked_type
			&& b.deref_type(value) == checked_type {
			// Mutable values can retain their storage pointer inside a cast. Read
			// the semantic value before performing the explicit conversion.
			value = b.emit1(.load, checked_type, value)
		}
	}
	target_type := b.resolve_type(target_name_id)
	// A positive literal can occupy the unsigned half of i64 before a float cast.
	if b.m.target.ptr_size == 4 && b.is_float_type(target_type) && child.kind == .int_literal {
		value = b.m.get_or_add_const(b.u64_type, child.value_text())
	}
	from_type := b.value_type(value)
	if target_type == from_type {
		return value
	}
	if b.is_interface_type_id(target_type) {
		return b.box_interface_value(target_type, checked_name_source_id, value)
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

fn (mut b Builder) build_sum_type_cast(target_name_id u32, value_expr flat.NodeId) ?ValueID {
	sum_name_source_id_4 := b.canonical_sum_type_name(target_name_id) or { return none }

	sum_type := b.resolve_type(sum_name_source_id_4)
	if sum_type <= 0 {
		return none
	}
	value := b.build_expr(value_expr)
	variant_source_id_2 := b.sum_variant_for_expr(sum_name_source_id_4, value_expr, value) or { return none }

	tag := b.sum_variant_index(sum_name_source_id_4, variant_source_id_2)
	if tag <= 0 {
		return none
	}
	return b.wrap_sum_value(sum_type, variant_source_id_2, tag, value)
}

fn (mut b Builder) wrap_sum_value(sum_type TypeID, variant_id u32, tag int, value ValueID) ValueID {
	variant := flat.source_type_name(variant_id)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(sum_type))
	if '_pointer_variant_is_owned' in b.m.type_store.types[sum_type].field_names {
		flag := b.get_field_ptr(alloca, '_pointer_variant_is_owned')
		zero := b.m.get_or_add_const(b.i32_type, '0')
		b.emit2(.store, b.void_type, zero, flag)
	}
	tag_ptr := b.get_field_ptr(alloca, 'typ')
	tag_val := b.m.get_or_add_const(b.i32_type, tag.str())
	b.emit2(.store, b.void_type, tag_val, tag_ptr)
	field_ptr := b.get_field_ptr(alloca, sum_field_name(variant))
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
	memdup_ref := b.m.add_value(.func_ref, ptr_i8, '__ssa_memdup',
		b.fn_ids['__ssa_memdup'])
	copied := b.emit3(.call, ptr_i8, memdup_ref, src, size_const)
	return b.emit1(.bitcast, ptr_type, copied)
}

fn (mut b Builder) box_interface_value(interface_type TypeID, source_type_id u32, value ValueID) ValueID {
	value_type := b.value_type(value)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(interface_type))
	object := if b.is_pointer_type(value_type) {
		b.emit1(.bitcast, b.m.type_store.get_ptr(b.i8_type), value)
	} else {
		b.heap_copy_value(value, value_type)
	}
	object_ptr := b.get_field_ptr(alloca, '_object')
	b.emit2(.store, b.void_type, object, object_ptr)
	type_ptr := b.get_field_ptr(alloca, '_typ')
	box_type_id := b.interface_dispatch_id(interface_type, source_type_id)
	type_id := b.m.get_or_add_const(b.i32_type, '${box_type_id}')
	b.emit2(.store, b.void_type, type_id, type_ptr)
	boxed_ptr := b.get_field_ptr(alloca, '_object_is_boxed')
	boxed := b.m.get_or_add_const(b.i1_type, if b.is_pointer_type(value_type) {
		'0'
	} else {
		'1'
	})
	b.emit2(.store, b.void_type, boxed, boxed_ptr)
	interface_info := b.m.type_store.types[interface_type]
	if value_type == b.map_type && 'len' in interface_info.field_names {
		map_slot := b.emit0(.alloca, b.m.type_store.get_ptr(b.map_type))
		b.emit2(.store, b.void_type, value, map_slot)
		len_ptr := b.get_field_ptr(alloca, 'len')
		len_value := b.load_map_len(map_slot)
		b.emit2(.store, b.void_type, len_value, len_ptr)
	}
	return b.emit1(.load, interface_type, alloca)
}

fn (mut b Builder) unbox_interface_value(interface_value ValueID, target_type TypeID) ValueID {
	interface_type := b.value_type(interface_value)
	alloca := b.emit0(.alloca, b.m.type_store.get_ptr(interface_type))
	b.emit2(.store, b.void_type, interface_value, alloca)
	object_ptr := b.get_field_ptr(alloca, '_object')
	object := b.emit1(.load, b.m.type_store.get_ptr(b.i8_type), object_ptr)
	if b.is_pointer_type(target_type) {
		return b.emit1(.bitcast, target_type, object)
	}
	target_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(target_type), object)
	return b.emit1(.load, target_type, target_ptr)
}

fn (b &Builder) canonical_sum_type_name(name_id u32) ?u32 {
	name := flat.source_type_name(name_id)
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified := '${b.cur_module}.${name}'
		if canonical_source_id := b.sum_type_canonical.type_id(qualified) {
			return canonical_source_id
		}
	}
	if canonical_source_id := b.sum_type_canonical.type_id(name) {
		return canonical_source_id
	}
	if name.contains('.') {
		return none
	}
	short_name := name.all_after('.')
	if canonical_source_id := b.sum_type_canonical.type_id(short_name) {
		return canonical_source_id
	}
	return none
}

fn (mut b Builder) sum_variant_for_expr(sum_name_id u32, expr_id flat.NodeId, value ValueID) ?u32 {
	mut type_name := u32(0)
	if b.tc != unsafe { nil } && int(expr_id) >= 0 && int(expr_id) < b.a.nodes.len {
		if typ := b.tc.expr_type(expr_id) {
			type_name = typ.source_type_id()
		}
		if type_name == 0 || type_name == flat.source_type_unknown {
			type_name = b.tc.resolve_type(expr_id).source_type_id()
		}
	}
	if type_name == 0 || type_name == flat.source_type_unknown {
		type_name = b.infer_v_type(expr_id)
	}
	if variant := b.find_sum_variant(sum_name_id, type_name) {
		return variant
	}
	value_type := b.value_type(value)
	variants := b.sum_type_variants.type_ids(flat.source_type_name(sum_name_id)) or { []u32{} }
	for variant in variants {
		if b.resolve_type(variant) == value_type {
			return variant
		}
	}
	return none
}

fn (b &Builder) find_sum_variant(sum_name_id u32, type_name_id u32) ?u32 {
	variants := b.sum_type_variants.type_ids(flat.source_type_name(sum_name_id)) or { return none }
	if type_name_id in variants { return type_name_id }
	type_name := flat.source_type_name(type_name_id)
	if type_name_id == 0 || type_name.contains('.') { return none }
	base := type_name.trim_left('&')
	pointers := type_name.len - base.len
	mut found := u32(0)
	for variant_id in variants {
		variant := flat.source_type_name(variant_id)
		variant_base := variant.trim_left('&')
		if variant.len - variant_base.len != pointers { continue }
		if variant_base.all_after_last('.') != base { continue }
		if found != 0 { return none }
		found = variant_id
	}
	if found == 0 { return none }
	return found
}

fn (b &Builder) sum_variant_index(sum_name_id u32, variant_id u32) int {
	resolved := b.find_sum_variant(sum_name_id, variant_id) or { return 0 }
	variants := b.sum_type_variants.type_ids(flat.source_type_name(sum_name_id)) or { return 0 }
	return variants.index(resolved) + 1
}

fn (b &Builder) is_sum_type_id(typ_id TypeID) bool {
	if typ_id <= 0 {
		return false
	}
	for sum_name, _ in b.sum_type_variants.ids {
		if canonical_source_id := b.sum_type_canonical.type_id(sum_name) {
			if sum_typ := b.registered_struct_type(canonical_source_id) {
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
	for sum_name, variants_id in b.sum_type_variants.ids {
		if canonical_source_id := b.sum_type_canonical.type_id(sum_name) {
			if sum_typ := b.registered_struct_type(canonical_source_id) {
				if sum_typ != typ_id {
					continue
				}
				module_name := flat.source_type_name(canonical_source_id).all_before_last('.')
				for variant in variants_id {
					if sum_variant_field_name(flat.source_type_name(variant), module_name) == field_name {
						return true
					}
				}
			}
		}
	}
	return false
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
	mut rhs_val := b.build_expr(rhs_id)
	arr_type_id := b.checked_expr_type_name(lhs_id)
	if flat.source_type_shape(arr_type_id).has(.array_type)
		&& flat.source_type_name(arr_type_id).len > 2 {
		elem_type := b.resolve_type(indexed_elem_type_name(arr_type_id))
		rhs_val = b.coerce_numeric_value(rhs_val, elem_type)
	}
	if node.value_text() == 'push_many' || b.value_type(rhs_val) == b.array_type {
		if fn_id := b.runtime_fn_lookup('array_push_many') {
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
	if fn_id := b.runtime_fn_lookup('array_push') {
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
	saved_sum_guards := b.sum_guard_variants.clone()
	b.preserve_sum_guard(b.a.child(&node, 0), node.op == .logical_and)
	rhs := b.build_expr(b.a.child(&node, 1))
	b.sum_guard_variants = saved_sum_guards
	b.emit2(.store, b.void_type, rhs, result_alloca)
	b.emit1(.jmp, b.void_type, ValueID(merge_block))

	b.cur_block = merge_block
	return b.emit1(.load, b.i1_type, result_alloca)
}

fn (mut b Builder) build_prefix(node flat.Node, _id flat.NodeId) ValueID {
	mut child_id := b.a.child(&node, 0)
	mut child := b.a.nodes[int(child_id)]
	for node.op == .amp && child.kind == .paren && child.children_count > 0 {
		child_id = b.a.child(&child, 0)
		child = b.a.nodes[int(child_id)]
	}
	if node.op == .amp && child.kind == .struct_init {
		return b.build_heap_struct_init(child_id, child)
	}
	if node.op == .amp && child.kind == .ident {
		if addr := b.vars[child.value_text()] {
			// Fixed-array locals hold their storage pointer. Taking their address
			// must expose the array bytes rather than the pointer's local slot.
			storage_type := b.deref_type(addr)
			if b.is_fixed_array_type_name(b.var_type_names.type_id(child.value_text()) or { u32(0) })
				&& storage_type > 0 && storage_type < b.m.type_store.types.len
				&& b.m.type_store.types[storage_type].kind == .ptr_t {
				return b.emit1(.load, storage_type, addr)
			}
			return addr
		}
		if expr_id := b.lookup_const_expr(child.value_text()) {
			if addr := b.const_global_addr(child.value_text(), expr_id) {
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
	if node.op == .amp && child.kind == .prefix && child.op == .mul {
		return b.build_expr(b.a.child(&child, 0))
	}
	val := b.build_expr(child_id)
	match node.op {
		.minus {
			typ := b.value_type(val)
			if b.m.type_store.types[typ].kind == .float_t {
				negative_zero := b.m.get_or_add_const(typ, '-0.0')
				return b.emit2(.fsub, typ, negative_zero, val)
			}
			zero := b.m.get_or_add_const(b.i64_type, '0')
			return b.emit2(.sub, typ, zero, val)
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
			if _ := b.fixed_array_expr_type_name(child_id) {
				// Fixed arrays already evaluate to their inline byte storage.
				return val
			}
			// Constants and expression results need an addressable value slot.
			// A map value is its state pointer, not the address of its header.
			slot := b.emit0(.alloca, b.m.type_store.get_ptr(b.value_type(val)))
			b.emit2(.store, b.void_type, val, slot)
			return slot
		}
		.mul {
			if fixed_type_name := b.fixed_array_expr_type_name(child_id) {
				return b.emit1(.bitcast, b.resolve_type(fixed_type_name), val)
			}
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
	typ_id, _ := b.struct_literal_type(expr.source_value_id())
	if typ_id <= 0 {
		return none
	}
	qualified_name := if name.contains('.') || b.cur_module.len == 0 || b.cur_module == 'main' {
		name
	} else {
		b.cur_module + '.' + name
	}
	global_name := '__const_' + ssa_c_name(qualified_name)
	if val_id := b.global_vars[global_name] {
		return val_id
	}
	val_id := b.m.add_global(global_name, typ_id)
	b.global_vars[global_name] = val_id
	b.vars[global_name] = val_id
	return val_id
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
	op := if b.is_float_type(value_type) {
		if node.op == .inc { OpCode.fadd } else { OpCode.fsub }
	} else {
		if node.op == .inc { OpCode.add } else { OpCode.sub }
	}
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
		.paren {
			return b.build_lvalue_addr(b.a.child(&node, 0))
		}
		.ident {
			if addr := b.vars[node.value_text()] {
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
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				return b.build_expr(b.a.child(&node, 0))
			}
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
	if name := b.source_selector_name(node) {
		if addr := b.vars[name] {
			return addr
		}
	}
	if addr := b.native_global_selector_addr(node) {
		return addr
	}
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	base_id := b.a.child(&node, 0)
	base := b.a.nodes[int(base_id)]
	if base.kind == .ident && base.value_text() == 'C' && node.value_text() == 'errno' {
		return b.native_errno_addr()
	}
	if node.op == .arrow {
		base_ptr := b.build_arrow_selector_base_ptr(base_id, base)
		return b.get_field_ptr(base_ptr, node.value_text())
	}
	if base.kind == .ident {
		if addr := b.vars[base.value_text()] {
			if field_ptr := b.smartcast_sum_selector_addr(addr, base_id, node.value_text()) {
				return field_ptr
			}
			return b.get_field_ptr(addr, node.value_text())
		}
	}
	if base.kind == .selector {
		base_addr := b.build_selector_addr(base)
		return b.get_field_ptr(base_addr, node.value_text())
	}
	if base.kind == .index {
		base_addr := b.build_index_addr(base_id, base)
		return b.get_field_ptr(base_addr, node.value_text())
	}
	base_val := b.build_expr(base_id)
	base_typ := b.value_type(base_val)
	if base_typ > 0 && base_typ < b.m.type_store.types.len {
		base_type_kind := b.m.type_store.types[base_typ].kind
		if base_type_kind == .struct_t {
			alloca := b.emit0(.alloca, b.m.type_store.get_ptr(base_typ))
			b.emit2(.store, b.void_type, base_val, alloca)
			return b.get_field_ptr(alloca, node.value_text())
		}
		if base_type_kind == .ptr_t {
			// The base is already a pointer to the struct, e.g. a `&Struct(ptr)` cast
			// or `(&Struct(ptr)).field` lvalue. Index straight off the pointer; if the
			// pointee struct type is known the cast keeps it, otherwise re-derive it.
			ptr_val := b.coerce_struct_ptr_for_field(base_val, base_id)
			return b.get_field_ptr(ptr_val, node.value_text())
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
	struct_name_id := b.pointer_cast_struct_name(base_id) or { return ptr_val }
	if struct_id := b.registered_struct_type(struct_name_id) {
		want := b.m.type_store.get_ptr(struct_id)
		if want != ptr_typ {
			return b.emit1(.bitcast, want, ptr_val)
		}
	}
	return ptr_val
}

// pointer_cast_struct_name returns the source spelling ID targeted by a `&Struct(...)`
// pointer cast expression, stripping the leading `&`.
fn (b &Builder) pointer_cast_struct_name(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := b.a.nodes[int(id)]
	mut name_id := u32(0)
	if node.kind == .cast_expr {
		name_id = node.source_value_id()
	} else if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child := b.a.nodes[int(b.a.child(&node, 0))]
		if child.kind == .cast_expr {
			name_id = child.source_value_id()
		}
	}
	if name_id == 0 {
		return none
	}
	name_id = ssa_pointer_base_source_type(name_id)
	if _ := b.registered_struct_type(name_id) {
		return name_id
	}
	return none
}

fn (mut b Builder) build_arrow_selector_base_ptr(base_id flat.NodeId, base flat.Node) ValueID {
	if base.kind == .ident {
		if addr := b.vars[base.value_text()] {
			return b.emit1(.load, b.deref_type(addr), addr)
		}
	}
	if base.kind == .selector && b.selector_has_addressable_root(&base) {
		base_addr := b.build_selector_addr(base)
		return b.emit1(.load, b.deref_type(base_addr), base_addr)
	}
	return b.build_expr(base_id)
}

// build_spawn_expr transfers evaluated call arguments to a pthread trampoline.
// Reuse ordinary call lowering so mutable receivers, casts, and module resolution
// have exactly the same ABI as a direct call.
fn (mut b Builder) build_spawn_expr(node flat.Node) ValueID {
	if node.children_count == 0 {
		return b.m.get_or_add_const(b.u64_type, '0')
	}
	call_id := b.a.child(&node, 0)
	call_value := b.build_expr(call_id)
	value := b.m.values[call_value]
	if value.kind != .instruction {
		return b.m.get_or_add_const(b.u64_type, '0')
	}
	call := b.m.instrs[value.index]
	if call.op !in [.call, .call_indirect] {
		panic('ssa: unsupported spawn call')
	}
	// The call itself belongs to the worker; its argument evaluations remain in
	// the spawning function, where their source locals are still available.
	call_index := b.m.blocks[call.block].instrs.index(call_value)
	mut never_returns := false
	if call_index + 1 < b.m.blocks[call.block].instrs.len {
		terminator := b.m.blocks[call.block].instrs[call_index + 1]
		if b.m.instrs[b.m.values[terminator].index].op == .unreachable {
			// Only the transferred call's terminator belongs to the worker.
			// Terminating argument evaluations still stop the spawning thread.
			b.m.detach_instruction_uses(terminator)
			b.m.blocks[call.block].instrs.delete(call_index + 1)
			b.m.values[terminator].kind = .unknown
			b.m.values[terminator].index = -1
			never_returns = true
		}
	}
	b.m.detach_instruction_uses(call_value)
	b.m.blocks[call.block].instrs.delete(call_index)
	b.m.values[call_value].kind = .unknown
	b.m.values[call_value].index = -1
	mut fields := []TypeID{cap: call.operands.len}
	mut field_names := []string{cap: call.operands.len}
	for i, operand in call.operands {
		fields << if i == 0 { b.u64_type } else { b.value_type(operand) }
		field_names << 'p${i}'
	}
	packet_type := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      fields
		field_names: field_names
	})
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	packet_size := b.m.get_or_add_const(b.u64_type, b.m.type_size(packet_type).str())
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	packet_raw := b.emit2(.call, ptr_i8, malloc_ref, packet_size)
	packet := b.emit1(.bitcast, b.m.type_store.get_ptr(packet_type), packet_raw)
	for i, operand in call.operands {
		field := b.block_struct_field_ptr(b.cur_block, packet, packet_type, i)
		mut arg := operand
		if i == 0 {
			callee := b.m.values[operand]
			arg = if callee.kind == .func_ref {
				b.m.add_value(.func_ref, b.u64_type, callee.name, callee.index)
			} else {
				b.emit1(.bitcast, b.u64_type, operand)
			}
		}
		b.emit2(.store, b.void_type, arg, field)
	}
	name := '__ssa_spawn_${b.m.funcs.len}'
	trampoline_id := b.register_synthetic_function(name, ptr_i8, [ptr_i8])
	b.generate_spawn_trampoline(trampoline_id, packet_type, call.typ, node.is_detached_spawn(),
		never_returns)
	trampoline := b.m.add_value(.func_ref, ptr_i8, name, trampoline_id)
	thread_slot := b.emit0(.alloca, b.m.type_store.get_ptr(b.u64_type))
	// Darwin's opaque pthread_attr_t fits in this aligned reservation.
	attr_type := b.m.type_store.get_array(b.u64_type, 16)
	attr_slot := b.emit0(.alloca, b.m.type_store.get_ptr(attr_type))
	attr := b.emit1(.bitcast, ptr_i8, attr_slot)
	init_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_attr_init', b.fn_ids['pthread_attr_init'])
	init_status := b.emit2(.call, b.i32_type, init_ref, attr)
	b.check_thread_status(init_status)
	if b.thread_stack_size > 0 {
		stack_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_attr_setstacksize',
			b.fn_ids['pthread_attr_setstacksize'])
		stack_size := b.m.get_or_add_const(b.u64_type, b.thread_stack_size.str())
		stack_status := b.emit3(.call, b.i32_type, stack_ref, attr, stack_size)
		b.check_thread_status(stack_status)
	}
	create_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_create', b.fn_ids['pthread_create'])
	status := b.m.add_instr(.call, b.cur_block, b.i32_type, [create_ref, thread_slot, attr, trampoline,
		packet_raw])
	destroy_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_attr_destroy',
		b.fn_ids['pthread_attr_destroy'])
	b.emit2(.call, b.i32_type, destroy_ref, attr)
	b.check_thread_status(status)
	thread_handle := b.emit1(.load, b.u64_type, thread_slot)
	if node.is_detached_spawn() {
		detach_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_detach', b.fn_ids['pthread_detach'])
		detach_status := b.emit2(.call, b.i32_type, detach_ref, thread_handle)
		b.check_thread_status(detach_status)
	}
	return thread_handle
}

fn (mut b Builder) generate_spawn_trampoline(func_id int, packet_type TypeID, ret_type TypeID, detached bool, never_returns bool) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'entry')
	packet_raw := b.func_add_argument(func_id, ptr_i8, 'packet')
	packet := b.block_instr1(.bitcast, entry, b.m.type_store.get_ptr(packet_type), packet_raw)
	mut args := []ValueID{cap: b.m.type_store.types[packet_type].fields.len}
	for i, field_type in b.m.type_store.types[packet_type].fields {
		field := b.block_struct_field_ptr(entry, packet, packet_type, i)
		args << b.block_instr1(.load, entry, field_type, field)
	}
	free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
	if never_returns {
		// Arguments have been copied out of the packet before the terminating call.
		b.block_instr2(.call, entry, b.void_type, free_ref, packet_raw)
		b.m.add_instr(.call_indirect, entry, ret_type, args)
		b.block_instr0(.unreachable, entry, b.void_type)
		return
	}
	result := b.m.add_instr(.call_indirect, entry, ret_type, args)
	b.block_instr2(.call, entry, b.void_type, free_ref, packet_raw)
	if ret_type == b.void_type || detached {
		nil_result := b.m.get_or_add_const(ptr_i8, '0')
		b.block_instr1(.ret, entry, b.void_type, nil_result)
		return
	}
	size := b.m.get_or_add_const(b.u64_type, b.m.type_size(ret_type).str())
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	box_raw := b.block_instr2(.call, entry, ptr_i8, malloc_ref, size)
	box := b.block_instr1(.bitcast, entry, b.m.type_store.get_ptr(ret_type), box_raw)
	b.block_instr2(.store, entry, b.void_type, result, box)
	b.block_instr1(.ret, entry, b.void_type, box_raw)
}

fn (mut b Builder) check_thread_status(status ValueID) {
	zero := b.m.get_or_add_const(b.i32_type, '0')
	ok := b.emit2(.eq, b.i1_type, status, zero)
	success := b.m.add_block(b.cur_func, 'thread_ok')
	failure := b.m.add_block(b.cur_func, 'thread_failed')
	b.emit3(.br, b.void_type, ok, ValueID(success), ValueID(failure))
	message := b.m.add_value(.string_literal, b.str_type, 'native thread operation failed', 0)
	panic_ref := b.m.add_value(.func_ref, b.void_type, 'panic', b.fn_ids['panic'])
	b.block_instr2(.call, failure, b.void_type, panic_ref, message)
	b.block_instr0(.unreachable, failure, b.void_type)
	b.cur_block = success
}

fn (mut b Builder) build_thread_wait(id flat.NodeId, node flat.Node, thread_id flat.NodeId) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	thread_handle := b.build_expr(thread_id)
	result_slot := b.emit0(.alloca, b.m.type_store.get_ptr(ptr_i8))
	join_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_join', b.fn_ids['pthread_join'])
	status := b.emit3(.call, b.i32_type, join_ref, thread_handle, result_slot)
	b.check_thread_status(status)
	thread_type_id := ssa_pointer_base_source_type(b.receiver_type_name(thread_id))
	thread_type := flat.source_type_name(thread_type_id)
	ret_type := if thread_type.starts_with('thread ') {
		b.resolve_type(flat.source_type_id(thread_type[7..]))
	} else if thread_type_id == ssa_thread_source_type {
		b.void_type
	} else {
		b.call_expr_result_type(id, node)
	}
	if ret_type == b.void_type {
		return b.m.get_or_add_const(b.void_type, '0')
	}
	box_raw := b.emit1(.load, ptr_i8, result_slot)
	box := b.emit1(.bitcast, b.m.type_store.get_ptr(ret_type), box_raw)
	result := b.emit1(.load, ret_type, box)
	free_ref := b.m.add_value(.func_ref, b.void_type, 'free', b.fn_ids['free'])
	b.emit2(.call, b.void_type, free_ref, box_raw)
	return result
}

fn (mut b Builder) build_call(id flat.NodeId, node flat.Node) ValueID {
	fn_node_id := b.a.child(&node, 0)
	fn_node := b.a.nodes[int(fn_node_id)]
	fn_name := fn_node.value_text()
	if fn_node.kind == .ident && fn_name == '__v3_closure_current_data' {
		for global_name in ['closure.g_closure', 'g_closure'] {
			if closure_addr := b.global_vars[global_name] {
				getter_addr := b.get_field_ptr(closure_addr, 'closure_get_data')
				getter := b.emit1(.load, b.deref_type(getter_addr), getter_addr)
				return b.emit1(.call_indirect, b.m.type_store.get_ptr(b.i8_type), getter)
			}
		}
		panic('SSA: closure context getter requires closure.g_closure')
	}
	if fn_node.kind == .ident && fn_name == '__alignof__' && node.children_count == 2 {
		arg_id := b.a.child(&node, 1)
		arg := b.a.nodes[int(arg_id)]
		mut type_name_id := u32(0)
		if arg.kind == .sizeof_expr && arg.children_count > 0 {
			type_name_id = b.checked_expr_type_name(b.a.child(&arg, 0))
		} else if b.var_type_names.contains(arg.value_text()) {
			type_name_id = b.var_type_names.value_id(arg.value_text())
		} else if arg.source_value_id() == 0 {
			type_name_id = b.checked_expr_type_name(arg_id)
		} else {
			type_name_id = arg.source_value_id()
		}
		for b.is_fixed_array_type_name(type_name_id) {
			type_name_id = b.fixed_array_elem_type_name(type_name_id)
		}
		alignment := b.m.type_align(b.resolve_type(type_name_id))
		return b.m.get_or_add_const(b.u64_type, alignment.str())
	}
	if b.is_function_field_expr(fn_node_id) {
		// A callable struct field takes precedence over method-name fallbacks.
		return b.build_indirect_call(id, node, fn_node_id)
	}
	if fn_node.kind == .index && b.is_function_value_expr(fn_node_id) {
		return b.build_indirect_call(id, node, fn_node_id)
	}
	if fn_node.kind == .selector && fn_node.children_count > 0 {
		base_id := b.a.child(&fn_node, 0)
		receiver_type := b.resolve_type(b.receiver_type_name(base_id))
		if b.is_interface_related_type(receiver_type) {
			if stub_id := b.register_interface_method_stub('', base_id, fn_node.value) {
				return b.build_interface_dispatch_call(stub_id, node, base_id)
			}
		}
	}
	if fn_node.kind == .selector && fn_node.children_count > 0
		&& fn_node.value_text() in ['set', 'clear', 'has', 'all'] {
		if result := b.build_flag_enum_method_call(b.a.child(&fn_node, 0), fn_node.value_text(),
			node)
		{
			return result
		}
	}
	if fn_node.kind == .selector && fn_name == 'wait' && fn_node.children_count > 0 {
		thread_id := b.a.child(&fn_node, 0)
		thread_type := ssa_pointer_base_source_type(b.receiver_type_name(thread_id))
		if thread_type == ssa_thread_source_type || flat.source_type_shape(thread_type).has(.thread_type) {
			return b.build_thread_wait(id, node, thread_id)
		}
	}

	mut is_method := false
	mut is_c_call := false
	mut base_id := flat.NodeId(0)
	mut actual_name := fn_name
	if fn_node.kind == .selector {
		base_id = b.a.child(&fn_node, 0)
		base := b.a.nodes[int(base_id)]
		if b.m.target.ptr_size == 4 && fn_node.value_text() == 'str' {
			receiver_name := if base.kind == .bool_literal {
				'bool'
			} else {
				flat.source_type_name(b.receiver_type_name(base_id))
			}
			if result := b.build_scalar_str_call(node, '${receiver_name}.str', base_id) {
				return result
			}
		}
		if base.kind == .ident && base.value_text() == 'C' {
			actual_name = fn_node.value_text()
			is_c_call = true
		} else {
			mut found_name := fn_node.value_text()
			mut resolved_handled := false
			if name := b.source_selector_name(fn_node) {
				if name in b.fn_ids {
					found_name = name
					resolved_handled = true
				}
			}
			if builder_name := b.builder_method_name_for_base(base, fn_node.value_text()) {
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
				if typed_name := b.typed_receiver_method_name(base_id, fn_node.value_text()) {
					found_name = typed_name
					is_method = true
					resolved_handled = true
				}
			}
			if !resolved_handled && base.kind == .ident {
				mut candidates := []string{}
				full_name := '${base.value_text()}.${fn_node.value_text()}'
				candidates << full_name
				candidates << full_name.replace('.', '__')
				if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
					qualified_name := '${b.cur_module}.${base.value_text()}.${fn_node.value_text()}'
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
					full_name := '${qualified_base}.${fn_node.value_text()}'
					c_name := full_name.replace('.', '__')
					if full_name in b.fn_ids {
						found_name = full_name
					} else if c_name in b.fn_ids {
						found_name = c_name
					}
				}
			}
			if found_name == fn_node.value_text() {
				base_is_array := b.call_base_is_array(base_id)
				for fname, _ in b.fn_ids {
					if fname.ends_with('.${fn_node.value_text()}') || fname.ends_with('__${fn_node.value_text()}') {
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
	} else if node.pos == token.Pos{} && fn_name in b.runtime_fn_ids {
		// Lowered runtime calls have no source span; keep their original helper
		// owner even when the current module declares a function with that name.
		actual_name = fn_name
	} else {
		qualified_name := ssa_fn_name_in_module(b.cur_module, fn_name)
		if b.source_modules.len > 0 && qualified_name in b.fn_ids {
			actual_name = qualified_name
		} else if resolved := b.resolved_call_name(id) {
			actual_name = resolved
		}
	}
	if fn_node.kind == .selector && !is_c_call && actual_name !in b.fn_ids {
		if typed_name := b.typed_receiver_method_name(base_id, fn_node.value_text()) {
			actual_name = typed_name
			is_method = true
		}
	}
	if actual_name.starts_with('C.') {
		actual_name = actual_name[2..]
		is_c_call = true
	}
	if actual_name == 'new_map_aligned' && '__ssa_new_map_aligned' in b.fn_ids {
		actual_name = '__ssa_new_map_aligned'
	}
	if b.m.target.ptr_size == 4 && !is_c_call {
		scalar_base := if fn_node.kind == .selector { base_id } else { flat.empty_node }
		if result := b.build_scalar_str_call(node, actual_name, scalar_base) {
			return result
		}
	}
	if !is_c_call && b.is_disabled_fn_name(actual_name) {
		// The transformer normally elides disabled `@[if ...]` calls. Minimal
		// programs can skip transforming an otherwise reachable builtin body, so
		// enforce the same no-op semantics here without evaluating the arguments.
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	if node.children_count == 2 && b.ownership_drop_intrinsic_name(actual_name) {
		// SSA does not yet have the C backend's recursive ownership-destruction ABI.
		// Preserve evaluation and the native backend's previous leak-tolerant behavior.
		return b.build_expr(b.a.child(&node, 1))
	}

	if fn_node.kind == .selector && fn_node.value_text() == 'delete_last' {
		return b.build_array_len_mutator_call(base_id, fn_node.value_text())
	}
	if fn_node.kind == .selector && b.call_base_is_array(base_id) {
		if fn_node.value_text() == 'pop' {
			return b.build_array_pop_call(id, base_id)
		}
		if fn_node.value_text() == 'pop_left' {
			return b.build_array_pop_left_call(id, base_id)
		}
		if fn_node.value_text() in ['delete_last', 'clear'] {
			return b.build_array_len_mutator_call(base_id, fn_node.value_text())
		}
		if fn_node.value_text() in ['first', 'last'] {
			return b.build_array_first_last_call(id, base_id, fn_node.value_text())
		}
		if fn_node.value_text() == 'index' && node.children_count > 1 {
			return b.build_array_index_call(base_id, b.a.child(&node, 1))
		}
		if fn_node.value_text() == 'join' && node.children_count > 1 {
			return b.build_array_join_call(base_id, b.a.child(&node, 1))
		}
		if fn_node.value_text() == 'repeat' && node.children_count > 1 {
			return b.build_array_repeat_call(base_id, b.a.child(&node, 1), flat.NodeId(-1))
		}
		if fn_node.value_text() == 'repeat_to_depth' && node.children_count > 2 {
			return b.build_array_repeat_call(base_id, b.a.child(&node, 1), b.a.child(&node, 2))
		}
	}
	if fn_node.kind == .selector && fn_node.value_text() == 'delete' && node.children_count > 1
		&& flat.source_type_shape(ssa_pointer_base_source_type(b.expr_type_name_for_map(base_id))).has(.map_type) {
		return b.build_map_delete_call(base_id, b.a.child(&node, 1))
	}
	if fn_node.kind == .selector && fn_node.value_text() == 'clone'
		&& flat.source_type_shape(ssa_pointer_base_source_type(b.expr_type_name_for_map(base_id))).has(.map_type) {
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
		'ast.[]Expr.name_list', '[]Expr.name_list', 'name_list', 'types__Type__str']
		&& !b.source_fn_names[actual_name] {
		return b.m.add_value(.string_literal, b.str_type, '', 0)
	}
	if actual_name == 'os.join_path'
		|| (actual_name == 'join_path' && !b.source_fn_names[actual_name]) {
		return b.build_join_path_call(node)
	}
	if fn_node.kind == .selector && node.children_count == 2 {
		base := b.a.child_node(fn_node, 0)
		if base.kind == .ident && base.value_text() == 'C' && actual_name !in b.fn_ids {
			return b.build_expr(b.a.child(&node, 1))
		}
	}
	if node.children_count == 2 && actual_name !in b.fn_ids {
		mut cast_name := actual_name
		if fn_node.kind == .selector {
			base := b.a.child_node(fn_node, 0)
			if base.kind == .ident {
				full_name := '${base.value_text()}.${fn_node.value_text()}'
				if _ := b.canonical_sum_type_name(flat.source_type_id(full_name)) {
					cast_name = full_name
				}
			}
		}
		if value := b.build_sum_type_cast(flat.source_type_id(cast_name), b.a.child(&node, 1)) {
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
		if method_name in ['vstring', 'vstring_with_len', 'vstring_literal', 'vstring_literal_with_len'] {
			u8_name := 'u8.${method_name}'
			if u8_name in b.fn_ids {
				resolved_name = u8_name
			}
		}
	}
	if fn_node.kind == .selector && fn_node.value_text() == 'str' && !is_c_call
		&& resolved_name !in b.fn_ids {
		base_type_name := ssa_pointer_base_source_type(b.checked_expr_type_name(base_id))
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
	runtime_call := !is_c_call && node.pos == token.Pos{} && resolved_name in b.runtime_fn_ids
	if is_c_call {
		if idx := b.c_fn_ids[resolved_name] {
			fn_idx = idx
		} else if idx := b.fn_ids[resolved_name] {
			fn_idx = idx
		} else {
			panic('ssa: unknown function `${actual_name}`')
		}
	} else if runtime_call {
		fn_idx = b.runtime_fn_ids[resolved_name]
	} else if idx := b.fn_ids[resolved_name] {
		fn_idx = idx
	} else {
		mut callee_var_names := [actual_name, fn_node.value]
		if b.cur_module.len > 0 {
			callee_var_names << '${b.cur_module}.${actual_name}'
		}
		for var_name in callee_var_names {
			if addr := b.vars[var_name] {
				callee := b.emit1(.load, b.deref_type(addr), addr)
				return b.build_indirect_call_value(id, node, fn_node_id, callee)
			}
		}
		if fn_node.kind == .ident {
			if _ := b.vars[fn_node.value_text()] {
				return b.build_indirect_call(id, node, fn_node_id)
			}
		} else if fn_node.kind == .selector && b.is_function_value_expr(fn_node_id) {
			return b.build_indirect_call(id, node, fn_node_id)
		}
		if stub_id := b.register_interface_method_stub(actual_name, base_id, fn_node.value) {
			resolved_name = actual_name
			fn_idx = stub_id
			is_method = true
		} else if stub_id := b.register_builtin_method_stub(actual_name, id, node) {
			resolved_name = actual_name
			fn_idx = stub_id
			is_method = true
		} else if stub_id := b.register_missing_call_stub(actual_name, id, node, base_id,
			is_method) {
			resolved_name = actual_name
			fn_idx = stub_id
		} else {
			// Name the caller as well as the callee: when the missing callee is a
			// skipped runtime function, the interesting half is which body still
			// mentions it.
			caller := if b.cur_func >= 0 && b.cur_func < b.m.funcs.len {
				b.m.funcs[b.cur_func].name
			} else {
				'<none>'
			}
			panic('ssa: unknown function `${actual_name}` (called from `${caller}` in module `${b.cur_module}`)')
		}
	}
	fn_ref := b.m.add_value(.func_ref, b.void_type, resolved_name, fn_idx)
	ret_type := b.m.funcs[fn_idx].typ

	mut param_types := []TypeID{}
	if is_c_call {
		if ft_id := b.c_fn_types[resolved_name] {
			ft := b.m.type_store.types[ft_id]
			param_types = ft.params.clone()
		} else if ft_id := b.fn_types[resolved_name] {
			// C macros such as array_push_many_ptr use native runtime helpers.
			// Their signatures still determine pointer-backed value arguments.
			ft := b.m.type_store.types[ft_id]
			param_types = ft.params.clone()
		}
	} else if runtime_call {
		if ft_id := b.runtime_fn_types[resolved_name] {
			param_types = b.m.type_store.types[ft_id].params.clone()
		}
	} else if ft_id := b.fn_types[resolved_name] {
		ft := b.m.type_store.types[ft_id]
		param_types = ft.params.clone()
	}

	if resolved_name == 'map__set' && (runtime_call || !b.source_fn_names[resolved_name]) {
		return b.build_map_set_call(node)
	}

	// C macros backed by V helpers retain the helper's mutable-pointer ABI.
	allow_pointer_slot := !is_c_call || resolved_name !in b.c_fn_types
	mut args := []ValueID{}
	args << fn_ref
	if b.m.target.ptr_size == 4 && resolved_name in ['print', 'println', 'eprint', 'eprintln']
		&& node.children_count == 1 {
		args << b.m.add_value(.string_literal, b.str_type, '', 0)
	}
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
					if addr := b.vars[base_node.value_text()] {
						if b.should_pass_ident_addr_for_ptr_param(addr, param_types[0], true) {
							args << addr
						} else {
							args << b.build_expr(base_id)
						}
					} else {
						args << b.build_expr(base_id)
					}
				} else if base_node.kind == .selector {
					addr := b.build_selector_addr(base_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_types[0], true) {
						args << addr
					} else {
						args << b.build_expr(base_id)
					}
				} else if base_node.kind == .index {
					addr := b.build_index_addr(base_id, base_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_types[0], true) {
						args << addr
					} else {
						args << b.build_expr(base_id)
					}
				} else {
					args << b.build_expr(base_id)
				}
			} else {
				args << b.coerce_value_for_type(b.build_expr(base_id), param_types[0])
			}
		} else {
			args << b.build_expr(base_id)
		}
	}
	if is_method && args.len > 1 && param_types.len > 0
		&& b.is_interface_type_id(b.value_type(args[1]))
		&& !b.is_interface_related_type(param_types[0]) {
		args[1] = b.unbox_interface_value(args[1], param_types[0])
	}
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		param_idx := if is_method { i } else { i - 1 }
		if resolved_name == 'v3_heap_array' && param_idx == 0 {
			args << b.coerce_value_for_type(b.build_expr(arg_id), b.array_type)
			continue
		}
		if resolved_name == '__v3_fixed_array_contains_string' && param_idx == 0 {
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
					if addr := b.vars[arg_node.value_text()] {
						if b.should_pass_ident_addr_for_ptr_param(addr, param_type, allow_pointer_slot) {
							args << addr
							continue
						}
					}
				} else if arg_node.kind == .selector {
					addr := b.build_selector_addr(arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type, allow_pointer_slot) {
						args << addr
						continue
					}
				} else if arg_node.kind == .index {
					addr := b.build_index_addr(arg_id, arg_node)
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type, allow_pointer_slot) {
						args << addr
						continue
					}
				} else if arg_node.kind == .prefix && arg_node.op == .mul {
					addr := b.build_expr(b.a.child(&arg_node, 0))
					if b.should_pass_ident_addr_for_ptr_param(addr, param_type, allow_pointer_slot) {
						args << addr
						continue
					}
				}
			}
		}
		mut arg := if param_idx < param_types.len {
			b.build_typed_call_arg(arg_id, param_types[param_idx], allow_pointer_slot)
		} else {
			b.build_expr(arg_id)
		}
		if param_idx < param_types.len && b.is_interface_type_id(b.value_type(arg))
			&& !b.is_interface_related_type(param_types[param_idx]) {
			arg = b.unbox_interface_value(arg, param_types[param_idx])
		}
		if b.m.target.ptr_size == 4 && resolved_name in ['print', 'println', 'eprint', 'eprintln'] {
			arg = b.stringify_scalar(arg)
		}
		args << arg
	}
	if resolved_name in ['map__keys', 'map__values'] && args.len == 2 && param_types.len == 2 {
		if elem_size := b.map_array_elem_size_arg(id, node) {
			args << elem_size
		} else {
			args << b.m.get_or_add_const(b.i64_type, '1')
		}
	}
	if resolved_name in ['new_map', 'new_map_aligned'] && args.len >= 7 {
		args = b.fix_native_map_callback_args(mut args)
	}
	result := b.m.add_instr(.call, b.cur_block, ret_type, args)
	if b.call_never_returns(id, resolved_name, is_c_call) {
		b.emit0(.unreachable, b.void_type)
	}
	return result
}

// fix_native_map_callback_args keeps the standard integer callbacks in sync
// with the lowered key size. Some transforms select platform-word callbacks for
// `int`, while V's `int` storage remains 32-bit on every supported target.
fn (mut b Builder) fix_native_map_callback_args(mut args []ValueID) []ValueID {
	key_size_value := b.m.values[args[1]]
	if key_size_value.kind != .constant {
		return args
	}
	key_size := key_size_value.name.int()
	if key_size !in [1, 2, 4, 8, 16] {
		return args
	}
	correct_names := [
		'map_hash_int_${key_size}',
		'map_eq_int_${key_size}',
		'map_clone_int_${key_size}',
	]
	families := ['map_hash_int_', 'map_eq_int_', 'map_clone_int_']
	for offset, correct_name in correct_names {
		arg_index := offset + 3
		current := b.m.values[args[arg_index]]
		if current.kind != .func_ref || !current.name.starts_with(families[offset]) {
			continue
		}
		if current.name == correct_name {
			continue
		}
		if fn_id := b.fn_ids[correct_name] {
			args[arg_index] = b.m.add_value(.func_ref, b.i64_type, correct_name, fn_id)
		}
	}
	return args
}

// register_interface_method_stub gives a known abstract interface method a
// callable SSA symbol. The native backends do not yet emit the C backend's
// type-id dispatch switch, but keeping the method signature lets programs that
// call through an interface compile and keeps later codegen deterministic.
fn (mut b Builder) register_interface_method_stub(name string, base_id flat.NodeId, method string) ?int {
	if b.tc == unsafe { nil } {
		return none
	}
	method_name := method.all_after_last('.')
	receiver_type := flat.source_type_name(b.receiver_type_name(base_id)).trim_space()
	mut receiver_interface := receiver_type.trim_left('&').trim_space()
	if receiver_interface.starts_with('mut ') {
		receiver_interface = receiver_interface[4..].trim_space()
	}
	mut interface_candidates := []string{cap: 3}
	if receiver_interface.len > 0 {
		interface_candidates << receiver_interface
	}
	if name.contains('.') {
		named_interface := name.all_before_last('.')
		interface_candidates << named_interface
		interface_candidates << named_interface.all_after_last('.')
	}
	if receiver_interface.len > 0 {
		interface_candidates << receiver_interface.all_after_last('.')
	}
	mut interface_name := ''
	mut interface_id := u32(0)
	for candidate in interface_candidates {
		if candidate.len == 0 {
			continue
		}
		candidate_id := flat.source_type_id(candidate)
		has_signature := b.tc.interface_method_signature_key(candidate_id, method_name) != none
		if method_name in b.tc.interface_abstract_method_names(candidate_id) || has_signature {
			interface_name = candidate
			interface_id = candidate_id
			break
		}
	}
	if interface_id == 0 {
		return none
	}
	if existing := b.fn_ids[name] {
		return existing
	}
	decl_key := b.tc.interface_method_signature_key(interface_id, method_name) or {
		'${interface_name}.${method_name}'
	}
	params, ret := b.tc.specialized_interface_method_signature(interface_id, decl_key)
	mut ssa_params := []TypeID{cap: params.len}
	for param in params {
		ssa_params << b.ssa_type_from_checker_type(param)
	}
	if ssa_params.len == 0 {
		ssa_params << b.resolve_type(flat.source_type_id(receiver_type))
	}
	ret_type := b.ssa_type_from_checker_type(ret)
	stub_name := if name.len > 0 { name } else { '${interface_name}.${method_name}' }
	stub_id := b.register_synthetic_function(stub_name, ret_type, ssa_params)
	b.generate_interface_method_dispatch(stub_id, interface_id, stub_name, method_name, ssa_params,
		ret_type)
	return stub_id
}

fn (mut b Builder) generate_interface_method_dispatch(func_id int, interface_id u32, func_name string, method string, param_types []TypeID, ret_type TypeID) {
	impl_ids := b.tc.interface_impl_names(interface_id)
	if impl_ids.len == 0 {
		return
	}
	old_func := b.cur_func
	old_block := b.cur_block
	old_ret_type := b.cur_func_ret_type
	b.cur_func = func_id
	b.cur_func_ret_type = ret_type
	entry := b.m.add_block(func_id, 'interface_dispatch')
	mut args := []ValueID{cap: param_types.len}
	for typ in param_types {
		args << b.func_add_argument(func_id, typ, 'arg${args.len}')
	}
	if args.len == 0 {
		b.cur_func = old_func
		b.cur_block = old_block
		b.cur_func_ret_type = old_ret_type
		return
	}
	param_interface_type := param_types[0]
	param_type := b.m.type_store.types[param_interface_type]
	is_interface_pointer := param_type.kind == .ptr_t
		&& b.is_interface_type_id(param_type.elem_type)
	interface_type := if is_interface_pointer { param_type.elem_type } else { param_interface_type }
	interface_ptr := if is_interface_pointer {
		args[0]
	} else {
		interface_alloca := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(interface_type))
		b.block_instr2(.store, entry, b.void_type, args[0], interface_alloca)
		interface_alloca
	}
	object_ptr := b.block_struct_field_ptr(entry, interface_ptr, interface_type, 0)
	object := b.block_instr1(.load, entry, b.m.type_store.get_ptr(b.i8_type), object_ptr)
	type_ptr := b.block_struct_field_ptr(entry, interface_ptr, interface_type, 1)
	concrete_typ := b.block_instr1(.load, entry, b.i32_type, type_ptr)

	mut cases := []BlockID{}
	mut checks := []BlockID{}
	mut methods := []int{}
	mut concrete_types := []TypeID{}
	for impl_id in impl_ids {
		impl_name := flat.source_type_name(impl_id).trim_left('&').trim_space()
		if impl_name == 'array' || impl_name.starts_with('[]') {
			// Builtin array high-order methods (`count`, `filter`, ...) are
			// compiler macros, not callable concrete functions. They are not valid
			// interface dispatch targets and would otherwise create unresolved
			// `C.array.<method>` symbols in native backends.
			continue
		}
		method_id := b.interface_concrete_method_id(impl_id, method) or { continue }
		if method_id == func_id {
			continue
		}
		checks << b.m.add_block(func_id, 'interface_${method}_check_${cases.len}')
		cases << b.m.add_block(func_id, 'interface_${method}_case_${cases.len}')
		methods << method_id
		concrete_types << b.interface_dispatch_id(interface_type, impl_id)
	}
	default_block := b.m.add_block(func_id, 'interface_${method}_default')
	if checks.len == 0 {
		if ret_type == b.void_type {
			b.block_instr0(.ret, entry, b.void_type)
		} else {
			b.block_instr1(.ret, entry, b.void_type, b.default_value_for_type(ret_type))
		}
		b.cur_func = old_func
		b.cur_block = old_block
		b.cur_func_ret_type = old_ret_type
		return
	}
	b.block_instr1(.jmp, entry, b.void_type, ValueID(checks[0]))
	for index, method_id in methods {
		b.cur_block = checks[index]
		concrete_type := concrete_types[index]
		type_const := b.m.get_or_add_const(b.i32_type, '${concrete_type}')
		matches := b.block_instr2(.eq, checks[index], b.i1_type, concrete_typ, type_const)
		next := if index + 1 < checks.len {
			checks[index + 1]
		} else {
			default_block
		}
		b.block_instr3(.br, checks[index], b.void_type, matches, ValueID(cases[index]),
			ValueID(next))

		b.cur_block = cases[index]
		mut method_params := []TypeID{}
		for name, type_id in b.fn_types {
			if name == func_name {
				continue
			}
			if b.fn_ids[name] or { -1 } == method_id {
				method_params = b.m.type_store.types[type_id].params.clone()
				break
			}
		}
		if method_params.len == 0 {
			method_params << b.resolve_type(impl_ids[index])
		}
		mut call_args := []ValueID{}
		call_args << b.m.add_value(.func_ref, b.void_type, b.m.funcs[method_id].name, method_id)
		receiver_type := method_params[0]
		receiver := if b.is_pointer_type(receiver_type) {
			b.emit1(.bitcast, receiver_type, object)
		} else {
			receiver_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(receiver_type), object)
			b.emit1(.load, receiver_type, receiver_ptr)
		}
		call_args << receiver
		for arg_index in 1 .. args.len {
			if arg_index >= method_params.len {
				break
			}
			call_args << b.coerce_value_for_type(args[arg_index], method_params[arg_index])
		}
		result := b.m.add_instr(.call, b.cur_block, ret_type, call_args)
		if ret_type == b.void_type {
			b.emit0(.ret, b.void_type)
		} else {
			b.emit1(.ret, b.void_type, result)
		}
	}
	b.cur_block = default_block
	if ret_type == b.void_type {
		b.emit0(.ret, b.void_type)
	} else {
		b.emit1(.ret, b.void_type, b.default_value_for_type(ret_type))
	}
	b.cur_func = old_func
	b.cur_block = old_block
	b.cur_func_ret_type = old_ret_type
}

fn (b &Builder) interface_concrete_method_id(impl_id u32, method string) ?int {
	impl_name := flat.source_type_name(impl_id).trim_left('&')
	short_name := impl_name.all_after_last('.')
	for candidate in [
		'${impl_name}.${method}',
		'${impl_name.replace('.', '__')}.${method}',
		'${short_name}.${method}',
		'${short_name}__${method}',
	] {
		if method_id := b.fn_ids[candidate] {
			return method_id
		}
	}
	return none
}

// register_builtin_method_stub registers checker-known runtime methods that the
// SSA backend does not lower yet, notably channel try_push/try_pop. The stub
// keeps the call ABI and result type so native code generation can complete.
fn (mut b Builder) register_builtin_method_stub(name string, call_id flat.NodeId, node flat.Node) ?int {
	if !name.contains('.') {
		return none
	}
	receiver_name := name.all_before_last('.')
	if !receiver_name.starts_with('chan ') {
		return none
	}
	if existing := b.fn_ids[name] {
		return existing
	}
	mut params := []TypeID{cap: node.children_count}
	params << b.resolve_type(flat.source_type_id(receiver_name))
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		arg_type_name := b.checked_expr_type_name(arg_id)
		params << if arg_type_name != 0 {
			b.resolve_type(arg_type_name)
		} else {
			b.i64_type
		}
	}
	ret_type := b.call_expr_result_type(call_id, node)
	return b.register_synthetic_function(name, ret_type, params)
}

// register_missing_call_stub materializes a checker-validated call whose native
// lowering is not available yet. This mirrors the ARM64 backend's empty-function
// stubs and is enabled only for native compile-only builds.
fn (mut b Builder) register_missing_call_stub(name string, call_id flat.NodeId, node flat.Node, base_id flat.NodeId, is_method bool) ?int {
	if !b.allow_missing_fn_stubs || name.len == 0 {
		return none
	}
	if existing := b.fn_ids[name] {
		return existing
	}
	mut params := []TypeID{cap: node.children_count + 1}
	if is_method {
		params << b.resolve_type(b.receiver_type_name(base_id))
	}
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		arg_type := b.checked_expr_type_name(arg_id)
		params << if arg_type != 0 && arg_type != flat.source_type_unknown {
			b.resolve_type(arg_type)
		} else {
			b.i64_type
		}
	}
	ret_type := b.call_expr_result_type(call_id, node)
	return b.register_synthetic_function(name, ret_type, params)
}

fn (b &Builder) call_never_returns(id flat.NodeId, name string, is_c_call bool) bool {
	if b.tc != unsafe { nil } && b.tc.resolved_call_never_returns(id) {
		return true
	}
	if name in ['panic', 'panic_debug', 'exit', 'v_exit'] || b.a.noreturn_fns[name] {
		return true
	}
	return is_c_call && b.a.noreturn_fns['C.${name}']
}

fn (mut b Builder) stringify_scalar(value ValueID) ValueID {
	typ := b.value_type(value)
	if typ == b.str_type {
		return value
	}
	if typ == b.i1_type {
		return b.emit_runtime_call('bool_str', b.str_type, [value])
	}
	if b.is_unsigned_type(typ) {
		ten := b.m.get_or_add_const(b.i64_type, '10')
		return b.emit_runtime_call('strconv__format_uint', b.str_type, [value, ten])
	}
	if b.is_int_type(typ) {
		return b.emit_runtime_call('int_str', b.str_type, [value])
	}
	return value
}

fn (mut b Builder) build_scalar_str_call(node flat.Node, resolved_name string, base_id flat.NodeId) ?ValueID {
	name := resolved_name.replace('__', '.').trim_string_left('builtin.')
	primitive_name := name.all_before_last('.')
	if name.all_after_last('.') != 'str'
		|| primitive_name !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'bool',
			'rune', 'isize', 'uint', 'string'] {
		return none
	}
	mut value_id := base_id
	if int(base_id) < 0 {
		if node.children_count != 2 {
			return none
		}
		value_id = b.a.child(&node, 1)
	} else if node.children_count != 1 {
		base := b.a.nodes[int(base_id)]
		if node.children_count != 2 || base.kind != .ident || base.value_text() != primitive_name {
			return none
		}
		value_id = b.a.child(&node, 1)
	}
	// Default integer literals can exceed int's width before their print conversion.
	value := if primitive_name == 'int' {
		b.build_expr(value_id)
	} else {
		b.coerce_numeric_value(b.build_expr(value_id), b.resolve_type(flat.source_type_id(primitive_name)))
	}
	// A rune's ordinary str() prints the character, not its code point, so it cannot
	// share the integer branch: stringify_scalar would hand the codepoint to int_str
	// and `${r}` would print `90` for `Z`.
	if primitive_name == 'rune' {
		return b.emit_runtime_call('v3_char_string', b.str_type, [value])
	}
	return b.stringify_scalar(value)
}

fn (b &Builder) is_disabled_fn_name(name string) bool {
	if name in b.a.disabled_fns || name.replace('__', '.') in b.a.disabled_fns {
		return true
	}
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module !in ['main', 'builtin'] {
		return '${b.cur_module}.${name}' in b.a.disabled_fns
	}
	return false
}

fn (mut b Builder) map_array_elem_size_arg(id flat.NodeId, node flat.Node) ?ValueID {
	mut typ_name := b.checked_expr_type_name(id)
	if typ_name == 0 || typ_name == flat.source_type_unknown {
		typ_name = node.typ
	}
	if !flat.source_type_shape(typ_name).has(.array_type) {
		return none
	}
	elem_type := b.resolve_type(flat.source_type_id(flat.source_type_name(typ_name)[2..]))
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

fn (mut b Builder) coerce_value_for_type(value ValueID, param_type TypeID) ValueID {
	value_type := b.value_type(value)
	if value_type > 0 && value_type < b.m.type_store.types.len {
		typ := b.m.type_store.types[value_type]
		if typ.kind == .ptr_t && typ.elem_type == param_type {
			return b.emit1(.load, param_type, value)
		}
	}
	if b.is_int_type(param_type) && b.is_int_type(value_type) {
		return b.coerce_numeric_value(value, param_type)
	}
	if b.is_float_type(param_type)
		&& (b.is_float_type(value_type) || b.is_int_type(value_type)) {
		return b.coerce_numeric_value(value, param_type)
	}
	return value
}

fn (mut b Builder) build_typed_call_arg(id flat.NodeId, param_type TypeID, allow_pointer_slot bool) ValueID {
	if b.is_sum_type_id(param_type) {
		if target_name := b.ssa_type_names[param_type] {
			if cast := b.build_sum_type_cast(flat.source_type_id(target_name), id) {
				return cast
			}
		}
	}
	value := b.build_expr(id)
	value_type := b.value_type(value)
	if b.is_sum_type_id(param_type) && value_type != param_type {
		mut target_name := b.ssa_type_names[param_type] or { '' }
		if target_name.len == 0 {
			for name, typ_id in b.struct_types {
				if typ_id == param_type
					&& b.canonical_sum_type_name(flat.source_type_id(name)) != none {
					target_name = name
					break
				}
			}
		}
		if target_name.len > 0 {
			variants := b.sum_type_variants.type_ids(target_name) or { []u32{} }
			for index, variant in variants {
				if b.resolve_type(variant) == value_type {
					return b.wrap_sum_value(param_type, variant, index + 1, value)
				}
			}
		}
	}
	mut argument := b.a.node(id)
	for argument.kind == .paren && argument.children_count > 0 {
		argument = b.a.node(b.a.child(argument, 0))
	}
	if argument.kind == .prefix && argument.op == .amp
		&& param_type == b.m.type_store.get_ptr(b.i8_type) {
		// Opaque byte pointers preserve explicit addresses, including the address
		// of a pointer element passed to array_push or a C memory function.
		return value
	}
	if b.is_pointer_type(param_type) && b.fixed_array_expr_type_name(id) != none {
		// A fixed array value is represented by the address of its inline storage.
		// Do not dereference it again when adapting it to a C/pointer parameter.
		if value_type != param_type {
			return b.emit1(.bitcast, param_type, value)
		}
		return value
	}
	if !allow_pointer_slot && b.is_pointer_type(value_type) && b.is_pointer_type(param_type)
		&& value_type != param_type {
		// C converts pointer values; a deeper declared pointer type does not borrow
		// the V local slot holding the argument.
		return b.emit1(.bitcast, param_type, value)
	}
	if allow_pointer_slot && b.is_pointer_type(value_type) && b.is_pointer_type(param_type)
		&& b.m.type_store.types[param_type].elem_type == value_type {
		if _ := b.fixed_array_expr_type_name(id) {
			return value
		}
		// `mut &buffer[i]` provides a temporary pointer value. A mutable pointer
		// parameter still needs a slot holding that value, just as `mut pointer` does.
		slot := b.emit0(.alloca, param_type)
		b.emit2(.store, b.void_type, value, slot)
		return slot
	}
	return b.coerce_value_for_type(value, param_type)
}

fn (mut b Builder) build_flag_enum_method_call(base_id flat.NodeId, method string, node flat.Node) ?ValueID {
	if node.children_count != 2 {
		return none
	}
	flag_type_name_source_id := b.flag_enum_expr_type_name(base_id)

	if flag_type_name_source_id == 0 || !b.is_flag_enum_type_name(flag_type_name_source_id) {
		return none
	}
	base_addr := b.build_lvalue_addr(base_id)
	flag_type := b.deref_type(base_addr)
	mut flag := if node.children_count > 1 {
		arg_id := b.a.child(&node, 1)
		arg_node := b.a.nodes[int(arg_id)]
		mask := b.flag_enum_mask_for_expr(arg_node, flag_type_name_source_id)
		if value := mask {
			b.m.get_or_add_const(flag_type, value.str())
		} else {
			b.build_expr(arg_id)
		}
	} else {
		b.m.get_or_add_const(flag_type, '0')
	}
	flag = b.coerce_int_value(flag, flag_type)
	current := b.emit1(.load, flag_type, base_addr)
	if method in ['has', 'all'] {
		masked := b.emit2(.and_, flag_type, current, flag)
		if method == 'all' {
			return b.emit2(.eq, b.i1_type, masked, flag)
		}
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

fn (b &Builder) flag_enum_mask_for_expr(node flat.Node, flag_type_name_id u32) ?int {
	if node.kind == .enum_val {
		value := b.enum_field_value_for_type(flag_type_name_id, node.value, false) or {
			return none
		}
		return 1 << value
	}
	if node.kind == .infix && node.children_count >= 2 {
		left_node := b.a.nodes[int(b.a.child(&node, 0))]
		right_node := b.a.nodes[int(b.a.child(&node, 1))]
		left := b.flag_enum_mask_for_expr(left_node, flag_type_name_id) or { return none }
		right := b.flag_enum_mask_for_expr(right_node, flag_type_name_id) or { return none }
		return match node.op {
			.pipe { left | right }
			.amp { left & right }
			.xor { left ^ right }
			else { none }
		}
	}
	if node.kind == .prefix && node.op == .bit_not && node.children_count > 0 {
		value := b.flag_enum_mask_for_expr(b.a.nodes[int(b.a.child(&node, 0))],
			flag_type_name_id) or { return none }
		return ~value
	}
	return none
}

fn (mut b Builder) coerce_flag_enum_assign_rhs(lhs_id flat.NodeId, op flat.Op, rhs_id flat.NodeId, value ValueID) ValueID {
	if op !in [.amp_assign, .pipe_assign, .xor_assign] {
		return value
	}
	flag_type_name_id := b.flag_enum_expr_type_name(lhs_id)
	if flag_type_name_id == 0 || !b.is_flag_enum_type_name(flag_type_name_id) {
		return value
	}
	rhs_node := b.a.nodes[int(rhs_id)]
	mask := b.flag_enum_mask_for_expr(rhs_node, flag_type_name_id) or { return value }
	return b.m.get_or_add_const(b.value_type(value), mask.str())
}

fn (b &Builder) is_flag_enum_expr(id flat.NodeId) bool {
	return b.is_flag_enum_type_name(b.flag_enum_expr_type_name(id))
}

fn (b &Builder) flag_enum_expr_type_name(id flat.NodeId) u32 {
	mut name := ssa_pointer_base_source_type(b.receiver_type_name(id))
	for _ in 0 .. 32 {
		if b.is_flag_enum_type_name(name) {
			return name
		}
		if target_source_id := b.type_alias_target(name) {
			if target_source_id != name {
				name = ssa_pointer_base_source_type(target_source_id)
				continue
			}
		}
		break
	}
	if int(id) < 0 {
		return u32(0)
	}
	node := b.a.nodes[int(id)]
	if node.typ != 0 && b.is_flag_enum_type_name(node.typ) {
		return node.typ
	}
	if node.kind == .selector && node.children_count > 0 {
		mut base_type := ssa_pointer_base_source_type(b.receiver_type_name(b.a.child(&node, 0)))
		field_type_source_id_2 := b.field_type_name(base_type, node.value_text())
		if field_type_source_id_2 != 0 {
			if b.is_flag_enum_type_name(field_type_source_id_2) {
				return field_type_source_id_2
			}
		}
		if node.value_text() == 'flags' {
			for _ in 0 .. 32 {
				if base_type == ssa_array_source_type || base_type == ssa_builtin_array_source_type
					|| flat.source_type_shape(base_type).has(.array_type) || b.type_aliases[flat.source_type_name(base_type)] == b.array_type {
					return ssa_array_flags_source_type
				}
				if target_source_id := b.type_alias_target(base_type) {
					if target_source_id != base_type {
						base_type = ssa_pointer_base_source_type(target_source_id)
						continue
					}
				}
				break
			}
		}
	}
	return u32(0)
}

fn (mut b Builder) coerce_int_value(value ValueID, to_type TypeID) ValueID {
	from_type := b.value_type(value)
	if !b.is_int_type(from_type) || !b.is_int_type(to_type) {
		return value
	}
	return b.coerce_numeric_value(value, to_type)
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
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.runtime_fn_id('array_get'))
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
	value := b.emit1(.load, elem_type, typed_ptr)
	new_len32 := b.emit1(.trunc, b.i32_type, index)
	b.emit2(.store, b.void_type, new_len32, len_ptr)
	return value
}

fn (mut b Builder) build_array_pop_left_call(id flat.NodeId, base_id flat.NodeId) ValueID {
	arr_addr := b.build_lvalue_addr(base_id)
	elem_type := b.array_method_elem_type(id, base_id)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	data_ptr := b.get_field_ptr(arr_addr, 'data')
	elem_size_ptr := b.get_field_ptr(arr_addr, 'element_size')
	data := b.emit1(.load, ptr_i8, data_ptr)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), data)
	value := b.emit1(.load, elem_type, typed_ptr)
	elem_size32 := b.emit1(.load, b.i32_type, elem_size_ptr)
	elem_size := b.emit1(.zext, b.i64_type, elem_size32)
	next_data := b.emit2(.add, ptr_i8, data, elem_size)
	b.emit2(.store, b.void_type, next_data, data_ptr)
	offset_ptr := b.get_field_ptr(arr_addr, 'offset')
	offset32 := b.emit1(.load, b.i32_type, offset_ptr)
	offset64 := b.emit1(.sext, b.i64_type, offset32)
	next_offset := b.emit2(.add, b.i64_type, offset64, elem_size)
	b.emit2(.store, b.void_type, b.emit1(.trunc, b.i32_type, next_offset), offset_ptr)
	for field in ['len', 'cap'] {
		field_ptr := b.get_field_ptr(arr_addr, field)
		field32 := b.emit1(.load, b.i32_type, field_ptr)
		field64 := b.emit1(.sext, b.i64_type, field32)
		updated64 := b.emit2(.sub, b.i64_type, field64,
			b.m.get_or_add_const(b.i64_type, '1'))
		b.emit2(.store, b.void_type, b.emit1(.trunc, b.i32_type, updated64), field_ptr)
	}
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
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.runtime_fn_id('array_get'))
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
	return b.emit1(.load, elem_type, typed_ptr)
}

fn (mut b Builder) build_array_index_call(base_id flat.NodeId, needle_id flat.NodeId) ValueID {
	base := b.build_expr(base_id)
	elem_type := b.array_receiver_elem_type(base_id)
	fn_name := if elem_type == b.str_type {
		'__v3_array_index_string'
	} else {
		'__v3_array_index_int'
	}
	needle_type := if elem_type == b.str_type { b.str_type } else { b.i64_type }
	mut needle := b.build_expr(needle_id)
	needle = b.coerce_int_value(needle, needle_type)
	fn_ref := b.m.add_value(.func_ref, b.i64_type, fn_name, b.runtime_fn_id(fn_name))
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
		b.runtime_fn_id('strings.new_builder'))
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
		b.runtime_fn_id('strings.Builder.write_string'))
	b.emit3(.call, b.void_type, write_ref, builder_alloca, sep)
	b.emit1(.jmp, b.void_type, ValueID(write_elem))

	b.cur_block = write_elem
	elem_size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(b.str_type)}')
	offset := b.emit2(.mul, b.i64_type, i_val, elem_size)
	elem_ptr_raw := b.emit2(.add, ptr_i8, data, offset)
	elem_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(b.str_type), elem_ptr_raw)
	elem := b.emit1(.load, b.str_type, elem_ptr)
	write_ref2 := b.m.add_value(.func_ref, b.void_type, 'strings.Builder.write_string',
		b.runtime_fn_id('strings.Builder.write_string'))
	b.emit3(.call, b.void_type, write_ref2, builder_alloca, elem)
	next_i := b.emit2(.add, b.i64_type, i_val, one)
	b.emit2(.store, b.void_type, next_i, i_alloca)
	b.emit1(.jmp, b.void_type, ValueID(loop))

	b.cur_block = done
	str_ref := b.m.add_value(.func_ref, b.str_type, 'strings.Builder.str',
		b.runtime_fn_id('strings.Builder.str'))
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
		b.runtime_fn_id('array.repeat_to_depth'))
	return b.emit4(.call, b.array_type, fn_ref, base, count, depth)
}

fn (mut b Builder) array_receiver_elem_type(base_id flat.NodeId) TypeID {
	base_name := flat.source_type_name(b.receiver_type_name(base_id))
	if base_name.starts_with('[]') {
		return b.resolve_type(flat.source_type_id(base_name[2..]))
	}
	return b.i64_type
}

fn (mut b Builder) array_method_elem_type(id flat.NodeId, base_id flat.NodeId) TypeID {
	ret_name_source_id := b.checked_expr_type_name(id)
	ret_name_shape := flat.source_type_shape(ret_name_source_id)
	if ret_name_source_id != 0 && ret_name_source_id != flat.source_type_unknown && !ret_name_shape.has(.array_type) {
		return b.resolve_type(ret_name_source_id)
	}
	base_name := flat.source_type_name(b.receiver_type_name(base_id))
	if base_name.starts_with('[]') {
		return b.resolve_type(flat.source_type_id(base_name[2..]))
	}
	return b.i64_type
}

fn (b &Builder) call_base_is_array(base_id flat.NodeId) bool {
	if int(base_id) < 0 {
		return false
	}
	base_name_shape := flat.source_type_shape(b.receiver_type_name(base_id))
	if base_name_shape.has(.array_type) {
		return true
	}
	base_node := b.a.nodes[int(base_id)]
	if base_node.kind == .ident {
		if addr := b.vars[base_node.value_text()] {
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

fn (b &Builder) receiver_type_name(base_id flat.NodeId) u32 {
	if int(base_id) < 0 {
		return u32(0)
	}
	base_node := b.a.nodes[int(base_id)]
	if base_node.kind == .ident {
		if var_name_source_id := b.var_type_names.type_id(base_node.value_text()) {
			return var_name_source_id
		}
	} else if base_node.kind == .selector && base_node.children_count > 0 {
		base_type := ssa_pointer_base_source_type(b.receiver_type_name(b.a.child(&base_node, 0)))
		field_type := b.field_type_name(base_type, base_node.value_text())
		if field_type != 0 {
			return field_type
		}
	}
	return b.checked_expr_type_name(base_id)
}

fn (b &Builder) typed_receiver_method_name(base_id flat.NodeId, method string) ?string {
	receiver_name := flat.source_type_name(b.receiver_type_name(base_id)).trim_left('&')
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
	return b.build_indirect_call_value(id, node, callee_id, callee)
}

fn (mut b Builder) build_indirect_call_value(id flat.NodeId, node flat.Node, callee_id flat.NodeId, callee ValueID) ValueID {
	mut param_types := []TypeID{}
	if b.m.target.ptr_size == 4 && b.tc != unsafe { nil } {
		mut callee_type := types.unalias_type(b.tc.expr_type(callee_id) or {
			b.tc.resolve_type(callee_id)
		})
		if callee_type !is types.FnType {
			// Transformed local callees can retain their type only on the binding.
			callee_type = types.unalias_type(b.tc.parse_type(b.checked_expr_type_name(callee_id)))
		}
		if callee_type is types.FnType {
			for param_type in callee_type.params {
				param_types << b.ssa_type_from_checker_type(param_type.typ)
			}
		}
	}
	mut args := []ValueID{}
	args << callee
	for i in 1 .. node.children_count {
		mut arg := b.build_expr(b.a.child(&node, i))
		if i - 1 < param_types.len {
			arg = b.coerce_numeric_value(arg, param_types[i - 1])
		}
		args << arg
	}
	return b.m.add_instr(.call_indirect, b.cur_block, b.call_expr_result_type(id, node), args)
}

fn (mut b Builder) build_interface_dispatch_call(stub_id int, node flat.Node, base_id flat.NodeId) ValueID {
	fn_name := b.m.funcs[stub_id].name
	ret_type := b.m.funcs[stub_id].typ
	ft_id := b.fn_types[fn_name] or { return b.m.get_or_add_const(ret_type, '0') }
	param_types := b.m.type_store.types[ft_id].params
	fn_ref := b.m.add_value(.func_ref, b.void_type, fn_name, stub_id)
	mut args := []ValueID{}
	args << fn_ref
	args << b.build_typed_call_arg(base_id, param_types[0], false)
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		if i < param_types.len {
			args << b.build_typed_call_arg(arg_id, param_types[i], false)
		} else {
			args << b.build_expr(arg_id)
		}
	}
	return b.m.add_instr(.call, b.cur_block, ret_type, args)
}

fn (b &Builder) is_function_value_expr(id flat.NodeId) bool {
	mut typ := b.checked_expr_type_name(id)
	if flat.source_type_shape(typ).has(.padded) {
		typ = flat.source_type_id(flat.source_type_name(typ).trim_space())
	}
	return b.is_function_type_name(typ)
}

fn (b &Builder) is_function_field_expr(id flat.NodeId) bool {
	node := b.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_type := ssa_pointer_base_source_type(b.receiver_type_name(b.a.child(&node, 0)))
	return b.is_function_type_name(b.field_type_name(base_type, node.value_text()))
}

fn (b &Builder) is_function_type_name(typ_id u32) bool {
	mut name := typ_id
	for _ in 0 .. 32 {
		shape := flat.source_type_shape(name)
		if shape.has(.function_compact) || shape.has(.function_spaced) {
			return true
		}
		if target_source_id := b.type_alias_target(name) {
			if target_source_id != name {
				name = target_source_id
				continue
			}
		}
		break
	}
	return false
}

fn (mut b Builder) call_expr_result_type(id flat.NodeId, node flat.Node) TypeID {
	checked_source_id_3 := b.checked_expr_type_name(id)

	if checked_source_id_3 != 0 && checked_source_id_3 != flat.source_type_unknown {
		return b.resolve_type(checked_source_id_3)
	}
	if node.typ != 0 && node.typ != flat.source_type_unknown {
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
	stride := b.m.type_size(b.str_type)
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
		return b.lookup_const_expr(node.value_text())
	}
	if node.kind == .selector {
		if node.value_text() == 'data' && node.children_count > 0 {
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
	if _ := b.vars[node.value_text()] {
		return none
	}
	expr_id := b.lookup_const_expr(node.value_text()) or { return none }
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
			return node.value_text()
		}
		.ident {
			if expr_id := b.lookup_const_expr(node.value_text()) {
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
				callee := b.qualified_expr_name(b.a.child(&node, 0))
				if callee in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64',
					'uint', 'rune'] {
					return b.const_int_literal_value(b.a.child(&node, 1))
				}
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
	checked_source_id_4 := b.checked_expr_type_name(id)

	if checked_source_id_4 == flat.source_type_f32 || node.typ == flat.source_type_f32 {
		return b.f32_type
	}
	return b.f64_type
}

fn (mut b Builder) build_map_set_call(node flat.Node) ValueID {
	fn_idx := b.runtime_fn_id('v3_map_set_sized')
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'v3_map_set_sized', fn_idx)
	mut args := []ValueID{}
	args << fn_ref
	key_size, val_size := b.map_set_call_sizes(node)
	for i in 1 .. node.children_count {
		arg_id := b.a.child(&node, i)
		if i == 2 && key_size == 16 {
			mut key_node := b.a.nodes[int(arg_id)]
			mut key_id := arg_id
			for key_node.kind == .paren && key_node.children_count > 0 {
				key_id = b.a.child(&key_node, 0)
				key_node = b.a.nodes[int(key_id)]
			}
			if key_node.kind == .ident {
				if addr := b.vars[key_node.value] {
					if b.deref_type(addr) == b.wide_integer_type() {
						args << addr
						continue
					}
				}
			}
		}
		args << b.build_expr(arg_id)
	}
	args << b.m.get_or_add_const(b.i64_type, '${key_size}')
	args << b.m.get_or_add_const(b.i64_type, '${val_size}')
	return b.m.add_instr(.call, b.cur_block, b.void_type, args)
}

fn (mut b Builder) build_map_delete_call(base_id flat.NodeId, key_id flat.NodeId) ValueID {
	map_type := b.expr_type_name_for_map(base_id)
	key_type_name, _ := map_type_parts(map_type)
	key_type := if key_type_name != 0 {
		b.resolve_type(key_type_name)
	} else {
		b.str_type
	}
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
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'map__delete', b.runtime_fn_id('map__delete'))
	return b.emit3(.call, b.void_type, fn_ref, map_ptr, key_ptr)
}

fn (mut b Builder) build_map_clone_call(base_id flat.NodeId) ValueID {
	fn_ref := b.m.add_value(.func_ref, b.void_type, 'map__clone', b.runtime_fn_id('map__clone'))
	base := b.map_expr_ptr(base_id)
	return b.emit2(.call, b.map_type, fn_ref, base)
}

fn (mut b Builder) map_set_call_sizes(node flat.Node) (int, int) {
	if node.children_count <= 1 {
		return 16, 8
	}
	map_type := b.map_arg_type_name(b.a.child(&node, 1))
	key_type, val_type := map_type_parts(map_type)
	if key_type == 0 || val_type == 0 {
		return 16, 8
	}
	key_size := b.m.type_size(b.resolve_type(key_type))
	val_size := b.m.type_size(b.resolve_type(val_type))
	actual_key_size := if key_size > 0 { key_size } else { 8 }
	actual_val_size := if val_size > 0 { val_size } else { 8 }
	return actual_key_size, actual_val_size
}

fn (b &Builder) map_arg_type_name(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := b.a.nodes[int(id)]
	if node.kind == .prefix && node.children_count > 0 && node.op == .amp {
		return ssa_pointer_base_source_type(b.expr_type_name_for_map(b.a.child(&node, 0)))
	}
	return ssa_pointer_base_source_type(b.expr_type_name_for_map(id))
}

fn (b &Builder) expr_type_name_for_map(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := b.a.nodes[int(id)]
	if node.kind == .ident {
		if var_name_source_id := b.var_type_names.type_id(node.value_text()) {
			var_name_source_id_3 := var_name_source_id

			if var_name_source_id_3 != 0 {
				return var_name_source_id_3
			}
		}
	}
	checked_source_id_5 := b.checked_expr_type_name(id)

	if checked_source_id_5 != 0 && checked_source_id_5 != flat.source_type_unknown {
		return checked_source_id_5
	}
	if node.typ != 0 {
		return node.typ
	}
	if node.kind == .prefix && node.children_count > 0 && node.op == .amp {
		inner_source_id := b.expr_type_name_for_map(b.a.child(&node, 0))
		inner := flat.source_type_name(inner_source_id)
		if inner_source_id != 0 {
			return flat.source_type_id('&' + inner)
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_type := b.expr_type_name_for_map(b.a.child(&node, 0))
		if field_type_source_id := b.registered_field_type(base_type, node.value_text()) {
			return field_type_source_id
		}
	}
	return u32(0)
}

fn map_type_parts(map_type_id u32) (u32, u32) {
	if map_type_id == 0 { return 0, 0 }
	key, value := map_type_parts_bytes(flat.source_type_name(map_type_id))
	return flat.source_type_id(key), flat.source_type_id(value)
}

fn map_type_parts_bytes(map_type string) (string, string) {
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

fn indexed_elem_type_name(container_type_id u32) u32 {
	if container_type_id == 0 { return 0 }
	return flat.source_type_id(indexed_elem_type_name_bytes(flat.source_type_name(container_type_id)))
}

fn indexed_elem_type_name_bytes(container_type string) string {
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
		_, val_type := map_type_parts_bytes(clean)
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
	if idx := b.runtime_fn_lookup('join_path_single') {
		fn_idx = idx
		found_fn = true
	}
	if !found_fn {
		if idx := b.runtime_fn_lookup('os.join_path_single') {
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

fn (b &Builder) declared_v_type_name(lhs_id flat.NodeId, rhs_id flat.NodeId) u32 {
	if int(rhs_id) >= 0 {
		rhs := b.a.nodes[int(rhs_id)]
		if rhs.kind == .array_init && b.is_fixed_array_type_name(rhs.source_value_id()) {
			return rhs.source_value_id()
		}
		if rhs.kind == .array_literal && b.is_fixed_array_type_name(rhs.typ) {
			return rhs.typ
		}
		if rhs.kind == .call && rhs.children_count > 0 {
			fn_node := b.a.child_node(&rhs, 0)
			if fn_node.kind == .selector && fn_node.children_count > 0
				&& fn_node.value_text() == 'new_builder' {
				base := b.a.child_node(fn_node, 0)
				if base.kind == .ident && base.value_text() == 'strings' {
					return flat.source_type_id('strings.Builder')
				}
			}
		}
	}
	if int(lhs_id) >= 0 {
		lhs := b.a.nodes[int(lhs_id)]
		if lhs.typ != 0 {
			return lhs.typ
		}
	}
	lhs_type_source_id := b.checked_expr_type_name(lhs_id)

	if lhs_type_source_id != 0 && lhs_type_source_id != flat.source_type_unknown {
		return lhs_type_source_id
	}
	if int(rhs_id) >= 0 {
		rhs := b.a.nodes[int(rhs_id)]
		if rhs.typ != 0 && rhs.typ != flat.source_type_unknown {
			return rhs.typ
		}
	}
	rhs_type_source_id := b.checked_expr_type_name(rhs_id)

	if rhs_type_source_id != 0 && rhs_type_source_id != flat.source_type_unknown {
		return rhs_type_source_id
	}
	return u32(0)
}

fn (b &Builder) checked_expr_type_name(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := b.a.nodes[int(id)]
	if node.kind == .selector {
		if typ_source_id := b.selector_type_name(node) {
			return typ_source_id
		}
	}
	if node.kind == .index && node.children_count > 0 {
		base_type := b.checked_expr_type_name(b.a.child(&node, 0))
		elem_type := indexed_elem_type_name(base_type)
		if elem_type != 0 {
			return elem_type
		}
	}
	if b.tc != unsafe { nil } {
		if typ := b.tc.expr_type(id) {
			if typ is types.MultiReturn {
				return flat.source_type_id(b.multi_return_c_type(typ))
			}
			name := typ.source_type_id()
			if name != 0 && name != flat.source_type_unknown {
				return name
			}
		}
	}
	if node.typ != 0 && node.typ != flat.source_type_unknown {
		return node.typ
	}
	if node.kind == .ident {
		if typ_source_id := b.var_type_names.type_id(node.value_text()) {
			return typ_source_id
		}
	}
	return u32(0)
}

fn (b &Builder) selector_type_name(node flat.Node) ?u32 {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := b.a.child(&node, 0)
	base_type_source_id_2 := b.checked_expr_type_name(base_id)
	base_type_source_id := base_type_source_id_2

	if base_type_source_id_2 == 0 {
		return none
	}
	if sum_name_source_id := b.canonical_sum_type_name(base_type_source_id_2) {
		sum_name := flat.source_type_name(sum_name_source_id)

		if variants_source_ids := b.sum_type_variants.type_ids(sum_name) {
			for variant_source_id in variants_source_ids {
				variant := flat.source_type_name(variant_source_id)

				if sum_field_name(variant) == node.value_text() {
					return variant_source_id
				}
			}
		}
	}
	field_type_source_id_4 := b.field_type_name(base_type_source_id, node.value_text())

	if field_type_source_id_4 != 0 {
		return field_type_source_id_4
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
	receiver_type := b.var_type_names.type_id(base.value_text()) or { u32(0) }
	if receiver_type !in [ssa_strings_builder_source_type, ssa_builder_source_type,
		ssa_strings_builder_ref_source_type, ssa_builder_ref_source_type] {
		return none
	}
	name := 'strings.Builder.${method}'
	if name in b.fn_ids {
		return name
	}
	return none
}

fn is_builder_method(method string) bool {
	return method in ['write_string', 'writeln', 'str', 'free', 'write_u8', 'write_byte', 'write_ptr',
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

fn (b &Builder) should_pass_ident_addr_for_ptr_param(addr ValueID, param_type TypeID, allow_pointer_slot bool) bool {
	if param_type <= 0 || param_type >= b.m.type_store.types.len {
		return false
	}
	param := b.m.type_store.types[param_type]
	if param.kind != .ptr_t {
		return false
	}
	// V mutable pointer parameters receive the caller's pointer slot. C pointer
	// parameters receive the slot's value, including opaque pointer conversions.
	if allow_pointer_slot && b.value_type(addr) == param_type {
		return true
	}
	arg_type := b.deref_type(addr)
	if arg_type <= 0 || arg_type >= b.m.type_store.types.len {
		return true
	}
	arg := b.m.type_store.types[arg_type]
	return arg.kind != .ptr_t
}

fn (mut b Builder) build_selector(node flat.Node) ValueID {
	if name := b.source_selector_name(node) {
		if addr := b.vars[name] {
			return b.emit1(.load, b.deref_type(addr), addr)
		}
		if expr_id := b.lookup_const_expr(name) {
			return b.build_expr(expr_id)
		}
		if fn_idx := b.fn_ids[name] {
			return b.m.add_value(.func_ref, b.i64_type, name, fn_idx)
		}
	}
	qualified_name := b.selector_qualified_name(node)
	if qualified_name.len > 0 {
		if fn_idx := b.fn_ids[qualified_name] {
			return b.m.add_value(.func_ref, b.i64_type, qualified_name, fn_idx)
		}
	}
	base_id := b.a.child(&node, 0)
	base := b.a.nodes[int(base_id)]
	field_name := node.value_text()
	if field_name == 'len' {
		if array_type_source_id := b.fixed_array_expr_type_name(base_id) {
			return b.fixed_array_len_value(b.fixed_array_len_text(array_type_source_id))
		}
	}

	if base.kind == .ident {
		if value := b.enum_value_for_type(base.source_value_id(), field_name) {
			return b.m.get_or_add_const(b.i64_type, value.str())
		}
		if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
			if value := b.enum_value_for_type(flat.source_type_id(b.cur_module + '.' + base.value_text()), field_name) {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}
	if base.kind == .selector && base.children_count > 0 {
		enum_base := b.selector_qualified_name(base)
		if enum_base.len > 0 {
			if value := b.enum_value_for_type(flat.source_type_id(enum_base), field_name) {
				return b.m.get_or_add_const(b.i64_type, value.str())
			}
		}
	}

	if base.kind == .ident && base.value_text() == 'os' {
		if field_name == 'args' {
			if fn_id := b.runtime_fn_lookup('arguments') {
				fn_ref := b.m.add_value(.func_ref, b.array_type, 'arguments', fn_id)
				return b.emit1(.call, b.array_type, fn_ref)
			}
		}
		if field_name == 'path_separator' {
			return b.m.add_value(.string_literal, b.str_type, '/', 0)
		}
	}

	if base.kind == .ident && base.value_text() == 'C' {
		match field_name {
			'environ' {
				ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
				environ_type := b.m.type_store.get_ptr(ptr_i8)
				environ_addr := b.m.add_external_global('environ', environ_type)
				return b.emit1(.load, environ_type, environ_addr)
			}
			'errno' {
				return b.emit1(.load, b.i32_type, b.native_errno_addr())
			}
			'FLT_EPSILON' {
				return b.m.get_or_add_const(b.f32_type, '1.1920928955078125e-7')
			}
			'DBL_EPSILON' {
				return b.m.get_or_add_const(b.f64_type, '2.220446049250313e-16')
			}
			else {}
		}
		if value := native_platform_constant(field_name) {
			typ := if field_name == 'MAP_FAILED' {
				b.m.type_store.get_ptr(b.i8_type)
			} else {
				b.i64_type
			}
			return b.m.get_or_add_const(typ, value.str())
		}
	}

	if base.kind == .ident {
		const_name := base.value_text() + '.' + field_name
		if expr_id := b.lookup_const_expr(const_name) {
			return b.build_expr(expr_id)
		}
		if addr := b.vars[base.value_text()] {
			if b.is_option_like_var(base.value_text()) {
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
		if b.m.target.ptr_size == 8 {
			base_type := b.resolve_type(b.checked_expr_type_name(base_id))
			if field := b.native_utsname_field_value(base_type, field_ptr) {
				return field
			}
		}
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
	checked_name_source_id_2 := b.checked_expr_type_name(expr_id)

	if checked_name_source_id_2 == 0 || b.resolve_type(checked_name_source_id_2) == sum_type {
		return none
	}
	sum_name_source_id_6 := b.sum_name_for_type_id(sum_type) or { return none }

	variant_source_id_4 := b.find_sum_variant(sum_name_source_id_6, checked_name_source_id_2) or { return none }
	variant := flat.source_type_name(variant_source_id_4)
	variant_type := b.resolve_type(variant_source_id_4)
	if variant_type <= 0 || variant_type >= b.m.type_store.types.len {
		return none
	}
	payload_ptr := b.get_field_ptr(sum_addr, sum_field_name(variant))

	return b.emit1(.load, variant_type, payload_ptr)
}

fn (mut b Builder) load_smartcast_sum_selector(sum_addr ValueID, base_id flat.NodeId, field_name string) ?ValueID {
	field_ptr := b.smartcast_sum_selector_addr(sum_addr, base_id, field_name) or { return none }
	return b.emit1(.load, b.deref_type(field_ptr), field_ptr)
}

fn (mut b Builder) smartcast_sum_selector_addr(sum_addr ValueID, base_id flat.NodeId, field_name string) ?ValueID {
	sum_type := b.sum_storage_type(sum_addr) or { return none }
	base_type_name_source_id := b.sum_guard_variants[sum_addr] or { b.checked_expr_type_name(base_id) }

	if base_type_name_source_id == 0 {
		return none
	}
	if b.resolve_type(base_type_name_source_id) == sum_type {
		return none
	}
	sum_name_source_id_7 := b.sum_name_for_type_id(sum_type) or { return none }
	variant_source_id_5 := b.find_sum_variant(sum_name_source_id_7, base_type_name_source_id) or { return none }
	variant := flat.source_type_name(variant_source_id_5)
	mut variant_type := b.resolve_type(variant_source_id_5)
	for b.is_pointer_type(variant_type) {
		variant_type = b.m.type_store.types[variant_type].elem_type
	}
	if variant_type <= 0 || variant_type >= b.m.type_store.types.len {
		return none
	}
	variant_layout := b.m.type_store.types[variant_type]
	if variant_layout.kind != .struct_t {
		return none
	}
	if field_name !in variant_layout.field_names {
		_ := b.embedded_field_path(variant_type, field_name, []TypeID{}) or { return none }
	}
	payload_ptr := b.get_field_ptr(sum_addr, sum_field_name(variant))
	return b.get_field_ptr(payload_ptr, field_name)
}

// sum_storage_type follows reference types without changing the binding used by guard facts.
fn (b &Builder) sum_storage_type(addr ValueID) ?TypeID {
	mut typ := b.deref_type(addr)
	for b.is_pointer_type(typ) {
		typ = b.m.type_store.types[typ].elem_type
	}
	if b.is_sum_type_id(typ) {
		return typ
	}
	return none
}

fn (b &Builder) sum_name_for_type_id(typ_id TypeID) ?u32 {
	if typ_id <= 0 {
		return none
	}
	for sum_name, _ in b.sum_type_variants.ids {
		if canonical_source_id := b.sum_type_canonical.type_id(sum_name) {
			if sum_typ := b.registered_struct_type(canonical_source_id) {
				if sum_typ == typ_id {
					return flat.source_type_id(sum_name)
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
			if cur.value_text().len > 0 {
				parts << cur.value_text()
			}
			break
		}
		if cur.kind != .selector {
			break
		}
		if cur.value_text().len > 0 {
			parts << cur.value_text()
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
	len := if b.m.target.ptr_size == 8 {
		count_ptr := b.block_struct_field_ptr(blk_load, state, b.map_state_type, 13)
		count := b.block_instr1(.load, blk_load, b.i32_type, count_ptr)
		b.block_instr1(.zext, blk_load, b.i64_type, count)
	} else {
		b.map_state_load_i64(blk_load, state, 3)
	}
	b.emit2(.store, b.void_type, len, result_slot)
	b.emit1(.jmp, b.void_type, ValueID(blk_done))

	b.cur_block = blk_empty
	zero_len := b.m.get_or_add_const(b.i64_type, '0')
	b.emit2(.store, b.void_type, zero_len, result_slot)
	b.emit1(.jmp, b.void_type, ValueID(blk_done))

	b.cur_block = blk_done
	return b.emit1(.load, b.i64_type, result_slot)
}

fn (mut b Builder) native_utsname_field_value(struct_typ_id TypeID, field_ptr ValueID) ?ValueID {
	if b.m.target.ptr_size == 8 {
		mut base_type := struct_typ_id
		if base_type > 0 && base_type < b.m.type_store.types.len
			&& b.m.type_store.types[base_type].kind == .ptr_t {
			base_type = b.m.type_store.types[base_type].elem_type
		}
		if uname_type := b.struct_types['C.utsname'] {
			field_type := b.deref_type(field_ptr)
			if base_type == uname_type && field_type > 0
				&& field_type < b.m.type_store.types.len
				&& b.m.type_store.types[field_type].kind == .array_t {
				// Preserve the logical &char field value: it is the inline array's
				// address, rather than a pointer loaded from its first eight bytes.
				return b.emit1(.bitcast, b.m.type_store.get_ptr(b.i8_type), field_ptr)
			}
		}
	}
	return none
}

fn (mut b Builder) load_selector_field(node flat.Node, struct_typ_id TypeID, field_ptr ValueID) ValueID {
	if field := b.native_utsname_field_value(struct_typ_id, field_ptr) {
		return field
	}
	field := b.emit1(.load, b.deref_type(field_ptr), field_ptr)
	if node.typ == 0 || !b.sum_type_has_field(struct_typ_id, node.value_text()) {
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
	typ_source_id_2 := b.var_type_names.type_id(name) or { return false }
	return typ_source_id_2 != 0
		&& (flat.source_type_shape(typ_source_id_2).has(.optional)
			|| typ_source_id_2 == ssa_option_source_type)
}

fn (b &Builder) selector_has_addressable_root(node &flat.Node) bool {
	match node.kind {
		.ident {
			if _ := b.vars[node.value_text()] {
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
		if fixed_type_source_id := b.fixed_array_expr_type_name(base_id) {
			if node.value == 'range' {
				return b.build_fixed_array_slice(base_id, node, fixed_type_source_id)
			}
			index := b.build_expr(b.a.child(&node, 1))
			elem_ptr, elem_type := b.build_fixed_array_index_addr(base_id, index, fixed_type_source_id)
			return b.emit1(.load, elem_type, elem_ptr)
		}
	}
	mut base := b.build_expr(base_id)
	mut base_typ := b.value_type(base)
	if base_typ > 0 && base_typ < b.m.type_store.types.len {
		base_type := b.m.type_store.types[base_typ]
		if base_type.kind == .ptr_t && base_type.elem_type == b.array_type {
			base = b.emit1(.load, b.array_type, base)
			base_typ = b.array_type
		}
	}
	if node.value_text() == 'range' {
		mut container_len := b.m.get_or_add_const(b.i64_type, '0')
		if base_typ == b.str_type {
			len32 := b.load_struct_field_from_value(base, b.str_type, 'len')
			container_len = b.emit1(.zext, b.i64_type, len32)
		} else {
			container_len = b.load_struct_field_from_value(base, b.array_type, 'len')
		}
		mut start := b.m.get_or_add_const(b.i64_type, '0')
		if node.children_count > 1 {
			start_node := b.a.child_node(&node, 1)
			if start_node.kind == .empty {
				start = b.m.get_or_add_const(b.i64_type, '0')
			} else {
				start = b.build_expr(b.a.child(&node, 1))
				start = b.normalize_range_bound(start, container_len)
			}
		}
		mut end := b.m.get_or_add_const(b.i64_type, '0')
		if node.children_count > 2 {
			end_node := b.a.child_node(&node, 2)
			if end_node.kind == .empty {
				end = container_len
			} else {
				end = b.build_expr(b.a.child(&node, 2))
				end = b.normalize_range_bound(end, container_len)
			}
		} else {
			end = container_len
		}
		if node.op == .gated_index {
			start = b.clamp_range_bound(start, container_len)
			end = b.clamp_range_bound(end, container_len)
			narrower_than_start := b.emit2(.lt, b.i1_type, end, start)
			end = b.emit3(.select, b.i64_type, narrower_than_start, start, end)
		}
		if base_typ == b.str_type {
			for substr_name in ['string.substr', 'string__substr', 'substr'] {
				if substr_id := b.runtime_fn_lookup(substr_name) {
					substr_ref := b.m.add_value(.func_ref, b.str_type, substr_name, substr_id)
					return b.emit4(.call, b.str_type, substr_ref, base, start, end)
				}
			}
			data := b.load_struct_field_from_value(base, b.str_type, 'str')
			slice_data := b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), data, start)
			slice_len := b.emit2(.sub, b.i64_type, end, start)
			return b.emit_make_owned_string(b.cur_block, slice_data, slice_len)
		}
		fn_ref := b.m.add_value(.func_ref, b.array_type, 'array_slice', b.runtime_fn_id('array_slice'))
		return b.emit4(.call, b.array_type, fn_ref, base, start, end)
	}
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.i64_type, '0')
	}
	index := b.build_expr(b.a.child(&node, 1))
	if base_typ == b.str_type {
		data := b.load_struct_field_from_value(base, b.str_type, 'str')
		elem_ptr := b.emit2(.add, b.m.type_store.get_ptr(b.u8_type), data, index)
		return b.emit1(.load, b.u8_type, elem_ptr)
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	map_type_name := b.expr_type_name_for_map(base_id)
	if flat.source_type_shape(map_type_name).has(.map_type) {
		key_type_name, val_type_name := map_type_parts(map_type_name)
		if key_type_name != 0 && val_type_name != 0 {
			val_type := b.resolve_type(val_type_name)
			typed_value_ptr := b.build_map_index_addr(base_id, index, key_type_name, val_type_name)
			return b.emit1(.load, val_type, typed_value_ptr)
		}
	}
	if fixed_elem_type_name := b.indexed_fixed_array_elem_type_name(base_id) {
		fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.runtime_fn_id('array_get'))
		elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
		return b.emit1(.bitcast, b.resolve_type(fixed_elem_type_name), elem_ptr)
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
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.runtime_fn_id('array_get'))
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	elem_type := b.index_elem_type(id, node)
	typed_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
	return b.emit1(.load, elem_type, typed_ptr)
}

fn (mut b Builder) build_fixed_array_slice(base_id flat.NodeId, node flat.Node, type_name_id u32) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	elem_type_name_id := b.fixed_array_elem_type_name(type_name_id)
	elem_type := b.resolve_type(elem_type_name_id)
	elem_size := if b.is_fixed_array_type_name(elem_type_name_id) {
		b.sizeof_type_name(elem_type_name_id)
	} else {
		b.m.type_size(elem_type)
	}
	actual_elem_size := if elem_size > 0 { elem_size } else { 8 }
	length := b.fixed_array_len_value(b.fixed_array_len_text(type_name_id))
	mut start := b.m.get_or_add_const(b.i64_type, '0')
	if node.children_count > 1 {
		start_node := b.a.child_node(&node, 1)
		if start_node.kind != .empty {
			start = b.build_expr(b.a.child(&node, 1))
			start = b.normalize_range_bound(start, length)
		}
	}
	mut end := length
	if node.children_count > 2 {
		end_node := b.a.child_node(&node, 2)
		if end_node.kind != .empty {
			end = b.build_expr(b.a.child(&node, 2))
			end = b.normalize_range_bound(end, length)
		}
	}
	if node.op == .gated_index {
		start = b.clamp_range_bound(start, length)
		end = b.clamp_range_bound(end, length)
	}
	count := b.emit2(.sub, b.i64_type, end, start)
	base_addr := b.build_lvalue_addr(base_id)
	base_data := b.emit1(.bitcast, ptr_i8, base_addr)
	offset := b.emit2(.mul, b.i64_type, start,
		b.m.get_or_add_const(b.i64_type, '${actual_elem_size}'))
	source := b.emit2(.add, ptr_i8, base_data, offset)
	elem_size_value := b.m.get_or_add_const(b.i64_type, '${actual_elem_size}')
	new_ref := b.m.add_value(.func_ref, b.array_type, 'array_new', b.runtime_fn_id('array_new'))
	result := b.emit4(.call, b.array_type, new_ref, elem_size_value, count, count)
	result_alloca := b.emit0(.alloca, b.m.type_store.get_ptr(b.array_type))
	b.emit2(.store, b.void_type, result, result_alloca)
	dest_data_ptr := b.get_field_ptr(result_alloca, 'data')
	dest := b.emit1(.load, ptr_i8, dest_data_ptr)
	copy_size := b.emit2(.mul, b.i64_type, count, elem_size_value)
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.runtime_fn_id('memcpy'))
	b.emit4(.call, ptr_i8, memcpy_ref, dest, source, copy_size)
	return b.emit1(.load, b.array_type, result_alloca)
}

// normalize_range_bound resolves a negative slice endpoint relative to the
// container length, matching the C backend's `s[-n..]` and `s[..-n]` behavior.
fn (mut b Builder) normalize_range_bound(value ValueID, length ValueID) ValueID {
	bound := b.coerce_int_value(value, b.i64_type)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	negative := b.emit2(.lt, b.i1_type, bound, zero)
	adjusted := b.emit2(.add, b.i64_type, length, bound)
	return b.emit3(.select, b.i64_type, negative, adjusted, bound)
}

fn (mut b Builder) clamp_range_bound(value ValueID, length ValueID) ValueID {
	zero := b.m.get_or_add_const(b.i64_type, '0')
	below_zero := b.emit2(.lt, b.i1_type, value, zero)
	nonnegative := b.emit3(.select, b.i64_type, below_zero, zero, value)
	above_length := b.emit2(.gt, b.i1_type, nonnegative, length)
	return b.emit3(.select, b.i64_type, above_length, length, nonnegative)
}

fn (mut b Builder) build_map_index_addr(base_id flat.NodeId, index ValueID, key_type_name_id u32, val_type_name_id u32) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	key_type := b.resolve_type(key_type_name_id)
	val_type := b.resolve_type(val_type_name_id)
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
	get_ref := b.m.add_value(.func_ref, ptr_i8, 'map__get', b.runtime_fn_id('map__get'))
	value_ptr := b.emit4(.call, ptr_i8, get_ref, map_ptr, key_ptr, default_ptr)
	if b.m.type_store.get_ptr(val_type) == ptr_i8 {
		return value_ptr
	}
	return b.emit1(.bitcast, b.m.type_store.get_ptr(val_type), value_ptr)
}

fn (mut b Builder) build_index_addr(id flat.NodeId, node flat.Node) ValueID {
	if node.children_count < 2 {
		return b.m.get_or_add_const(b.m.type_store.get_ptr(b.i8_type), '0')
	}
	// `a[lo..hi]` (an index node tagged `range`) is a slice expression producing a fresh
	// array value, not an addressable element. Its address must point at the materialized
	// slice value; computing an element pointer would make the slice's fields read as
	// garbage (len 0). Affects `&a[lo..hi]`, `array__clone(&a[lo..hi])`, `&[]T` args, etc.
	if node.value_text() == 'range' {
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
	if fixed_type_source_id := b.fixed_array_expr_type_name(base_id) {
		index := b.build_expr(b.a.child(&node, 1))
		elem_ptr, _ := b.build_fixed_array_index_addr(base_id, index, fixed_type_source_id)
		return elem_ptr
	}
	base := b.build_expr(base_id)
	index := b.build_expr(b.a.child(&node, 1))
	base_typ := b.value_type(base)
	if base_typ == b.str_type {
		data := b.load_struct_field_from_value(base, b.str_type, 'str')
		return b.emit2(.add, b.m.type_store.get_ptr(b.i8_type), data, index)
	}
	map_type_name := b.expr_type_name_for_map(base_id)
	if flat.source_type_shape(map_type_name).has(.map_type) {
		key_type_name, val_type_name := map_type_parts(map_type_name)
		if key_type_name != 0 && val_type_name != 0 {
			return b.build_map_index_addr(base_id, index, key_type_name, val_type_name)
		}
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
	fn_ref := b.m.add_value(.func_ref, ptr_i8, 'array_get', b.runtime_fn_id('array_get'))
	elem_ptr := b.emit3(.call, ptr_i8, fn_ref, base, index)
	elem_type := b.index_elem_type(id, node)
	return b.emit1(.bitcast, b.m.type_store.get_ptr(elem_type), elem_ptr)
}

fn (b &Builder) fixed_array_expr_type_name(id flat.NodeId) ?u32 {
	// Mutable fixed-array parameters have a reference type, while their storage
	// still contains inline elements rather than pointers to those elements.
	mut type_name := b.checked_expr_type_name(id)
	if flat.source_type_shape(type_name).has(.pointer) {
		type_name = flat.source_type_id(flat.source_type_name(type_name)[1..])
	}
	if b.is_fixed_array_type_name(type_name) {
		return type_name
	}
	return none
}

fn (b &Builder) indexed_fixed_array_elem_type_name(id flat.NodeId) ?u32 {
	container_type_id := b.checked_expr_type_name(id)
	if !flat.source_type_shape(container_type_id).has(.array_type) {
		return none
	}
	elem_type_id := indexed_elem_type_name(container_type_id)
	if b.is_fixed_array_type_name(elem_type_id) {
		return elem_type_id
	}
	return none
}

fn (mut b Builder) build_fixed_array_index_addr(base_id flat.NodeId, index ValueID, type_name_id u32) (ValueID, TypeID) {
	elem_type_name_id := b.fixed_array_elem_type_name(type_name_id)
	elem_type := b.resolve_type(elem_type_name_id)
	elem_size := if b.is_fixed_array_type_name(elem_type_name_id) {
		b.sizeof_type_name(elem_type_name_id)
	} else {
		b.m.type_size(elem_type)
	}
	offset := if elem_size > 1 {
		elem_size_const := b.m.get_or_add_const(b.i64_type, '${elem_size}')
		b.emit2(.mul, b.i64_type, index, elem_size_const)
	} else {
		index
	}
	base_addr := b.build_lvalue_addr(base_id)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	raw_base := if b.value_type(base_addr) == ptr_i8 {
		base_addr
	} else {
		b.emit1(.bitcast, ptr_i8, base_addr)
	}
	raw_elem := b.emit2(.add, ptr_i8, raw_base, offset)
	ptr_elem := b.m.type_store.get_ptr(elem_type)
	if ptr_elem == ptr_i8 {
		return raw_elem, elem_type
	}
	return b.emit1(.bitcast, ptr_elem, raw_elem), elem_type
}

fn (mut b Builder) build_array_data_index_addr(array_ptr ValueID, index ValueID, elem_type TypeID) ValueID {
	return b.build_array_data_index_addr_sized(array_ptr, index, elem_type,
		b.m.type_size(elem_type))
}

fn (mut b Builder) build_array_data_index_addr_sized(array_ptr ValueID, index ValueID, elem_type TypeID, elem_size int) ValueID {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	data_ptr := b.get_field_ptr(array_ptr, 'data')
	data := b.emit1(.load, ptr_i8, data_ptr)
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
		if elem_type != 0 {
			return b.resolve_type(elem_type)
		}
	}
	if node.typ != u32(0) && !flat.source_type_shape(node.typ).has(.array_type) {
		return b.resolve_type(node.typ)
	}
	checked_source_id_6 := b.checked_expr_type_name(id)
	checked := flat.source_type_name(checked_source_id_6)
	if checked_source_id_6 != 0 && checked_source_id_6 != flat.source_type_unknown && !checked.starts_with('[]') && checked[0] != `?`
		&& checked[0] != `!` {
		return b.resolve_type(checked_source_id_6)
	}
	return b.i64_type
}

fn (mut b Builder) build_storable_field_value(expr_id flat.NodeId, field_type_name_id u32, field_type TypeID) ValueID {
	value := b.build_field_value(expr_id, field_type_name_id)
	if b.is_interface_type_id(field_type) && b.value_type(value) != field_type {
		source_id := b.checked_expr_type_name(expr_id)
		source_name := flat.source_type_name(source_id).trim_left('&?!')
		if source_id != 0 && source_id != flat.source_type_unknown && source_name.len > 0 {
			source_ssa_type := b.resolve_type(flat.source_type_id(source_name))
			if source_ssa_type != field_type && !b.is_interface_related_type(source_ssa_type) {
				return b.box_interface_value(field_type, flat.source_type_id(source_name), value)
			}
		}
	}
	return b.coerce_store_value(value, field_type)
}

fn (mut b Builder) build_struct_init(id flat.NodeId, node flat.Node) ValueID {
	type_id := node.source_value_id()
	type_text := node.value_text()
	if type_text.len > 1 && (type_text[0] == `?` || type_text[0] == `!`) {
		// Transformed optional literals may be their type's first use. Register
		// the wrapper before short-name lookup can select its payload struct.
		b.resolve_type(type_id)
	}
	initial_typ_id, initial_struct_name_id := b.struct_literal_type(type_id)
	mut typ_id := initial_typ_id
	mut struct_name_id := initial_struct_name_id
	if typ_id == 0 || typ_id >= b.m.type_store.types.len
		|| b.m.type_store.types[typ_id].kind != .struct_t
		|| b.m.type_store.types[typ_id].fields.len == 0 {
		mut checked_source_id := b.checked_expr_type_name(id)
		checked_name := flat.source_type_name(checked_source_id)
		if checked_name.starts_with('&') {
			checked_source_id = flat.source_type_id(checked_name[1..])
		}
		if checked_source_id != 0 && checked_source_id != flat.source_type_unknown {
			checked_typ_id := b.resolve_type(checked_source_id)
			if checked_typ_id > 0 && checked_typ_id < b.m.type_store.types.len
				&& b.m.type_store.types[checked_typ_id].kind == .struct_t
				&& b.m.type_store.types[checked_typ_id].fields.len > 0 {
				typ_id = checked_typ_id
				struct_name_id = checked_source_id
			}
		}
	}
	if b.is_sum_type_id(typ_id) {
		return b.build_sum_init(node, typ_id, struct_name_id)
	}
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
					field_type_name_source_id := b.field_type_name(struct_name_id, fname)

					mut field_val := b.build_storable_field_value(field_expr_id,
						field_type_name_source_id, field_type)
					if b.is_interface_type_id(typ_id) && fname == '_typ' {
						if concrete_type := b.interface_init_concrete_type(node, typ_id) {
							field_val = b.m.get_or_add_const(field_type, '${concrete_type}')
						}
					}
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
			if fname !in initialized && (!typ.is_union || initialized.len == 0) {
				offset := b.m.struct_field_offset(typ_id, fi)
				off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
				field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
				field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type), alloca,
					off_const)
				default_value := b.default_field_value(struct_name_id, fname, zero)
				b.emit2(.store, b.void_type, default_value, field_ptr)
			}
		}
		return b.emit1(.load, typ_id, alloca)
	}
	return b.m.get_or_add_const(b.i64_type, '0')
}

fn (mut b Builder) build_sum_init(node flat.Node, typ TypeID, name u32) ValueID {
	storage := b.emit0(.alloca, b.m.type_store.get_ptr(typ))
	if '_pointer_variant_is_owned' in b.m.type_store.types[typ].field_names {
		flag := b.get_field_ptr(storage, '_pointer_variant_is_owned')
		zero := b.m.get_or_add_const(b.i32_type, '0')
		b.emit2(.store, b.void_type, zero, flag)
	}
	for i in 0 .. node.children_count {
		field := b.a.child_node(&node, i)
		ptr := b.get_field_ptr(storage, field.value)
		value_type := b.field_type_name(name, field.value)
		value := b.build_field_value(b.a.child(field, 0), value_type)
		stored := b.coerce_store_value(value, b.deref_type(ptr))
		b.emit2(.store, b.void_type, stored, ptr)
	}
	return b.emit1(.load, typ, storage)
}

fn (mut b Builder) coerce_store_value(source ValueID, target_type TypeID) ValueID {
	value := b.coerce_value_for_type(source, target_type)
	mut result := b.coerce_int_value(value, target_type)
	value_type := b.value_type(value)
	if b.is_interface_type_id(target_type) && value_type != target_type {
		if source_name := b.source_name_for_ssa_type_id(value_type) {
			source_type := b.resolve_type(flat.source_type_id(source_name))
			if source_type != target_type && !b.is_interface_related_type(source_type) {
				return b.box_interface_value(target_type, flat.source_type_id(source_name), value)
			}
		}
	}
	if b.m.target.ptr_size == 4 {
		result = b.coerce_numeric_value(value, target_type)
	}
	if target_type > 0 && target_type < b.m.type_store.types.len
		&& b.m.type_store.types[target_type].kind == .array_t
		&& value_type > 0 && value_type < b.m.type_store.types.len {
		source_type := b.m.type_store.types[value_type]
		if source_type.kind == .ptr_t
			&& (source_type.elem_type == b.m.type_store.types[target_type].elem_type
				|| source_type.elem_type == target_type) {
			// Fixed-array locals expose their storage; inline fields copy the elements.
			array_ptr := b.emit1(.bitcast, b.m.type_store.get_ptr(target_type), value)
			return b.emit1(.load, target_type, array_ptr)
		}
	}
	if b.is_pointer_type(target_type) && !b.is_pointer_type(value_type) {
		result = b.heap_copy_value(value, value_type)
	} else if target_type != value_type && b.is_pointer_type(target_type)
		&& b.is_pointer_type(value_type) {
		result = b.emit1(.bitcast, target_type, value)
	}
	return result
}

fn (b &Builder) source_name_for_ssa_type_id(typ_id TypeID) ?string {
	if name := b.ssa_type_names[typ_id] {
		if name.len > 0 {
			return name
		}
	}
	mut short_name := ''
	for name, candidate in b.struct_types {
		if candidate != typ_id || name.len == 0 || name.starts_with('[') || name.starts_with('[]')
			|| name.starts_with('map[') || name.starts_with('?') || name.starts_with('!') {
			continue
		}
		if name.contains('.') {
			return name
		}
		short_name = name
	}
	if short_name.len > 0 {
		return short_name
	}
	return none
}

fn (mut b Builder) build_field_value(expr_id flat.NodeId, field_type_name_id u32) ValueID {
	if int(expr_id) >= 0 {
		expr := b.a.nodes[int(expr_id)]
		if expr.kind == .prefix && expr.op == .bit_not && expr.children_count > 0 {
			value := b.build_field_value(b.a.child(&expr, 0), field_type_name_id)
			all_bits := b.m.get_or_add_const(b.value_type(value), '-1')
			return b.emit2(.xor, b.value_type(value), value, all_bits)
		}
		if expr.kind == .enum_val && field_type_name_id != 0 {
			return b.build_enum_val_with_type(expr, field_type_name_id) or { b.build_expr(expr_id) }
		}
		if expr.kind == .infix && field_type_name_id != 0 && b.is_enum_type_name(field_type_name_id) {
			return b.build_enum_expr_with_type(expr, field_type_name_id) or { b.build_expr(expr_id) }
		}
	}
	return b.build_expr(expr_id)
}

fn (mut b Builder) build_enum_expr_with_type(node flat.Node, type_name_id u32) ?ValueID {
	if node.kind == .enum_val {
		return b.build_enum_val_with_type(node, type_name_id)
	}
	if node.kind != .infix || node.children_count < 2 {
		return none
	}
	lhs := b.a.nodes[int(b.a.child(&node, 0))]
	rhs := b.a.nodes[int(b.a.child(&node, 1))]
	lval := b.enum_const_value_with_type(lhs, type_name_id) or { return none }
	rval := b.enum_const_value_with_type(rhs, type_name_id) or { return none }
	mut result := 0
	match node.op {
		.pipe { result = lval | rval }
		.amp { result = lval & rval }
		.xor { result = lval ^ rval }
		else { return none }
	}

	return b.m.get_or_add_const(b.i64_type, result.str())
}

fn (b &Builder) enum_const_value_with_type(node flat.Node, type_name_id u32) ?int {
	if node.kind == .enum_val {
		return b.enum_value_for_type(type_name_id, node.value_text())
	}
	if node.kind == .infix && node.children_count >= 2 {
		lhs := b.a.nodes[int(b.a.child(&node, 0))]
		rhs := b.a.nodes[int(b.a.child(&node, 1))]
		lval := b.enum_const_value_with_type(lhs, type_name_id) or { return none }
		rval := b.enum_const_value_with_type(rhs, type_name_id) or { return none }
		match node.op {
			.pipe { return lval | rval }
			.amp { return lval & rval }
			.xor { return lval ^ rval }
			else { return none }
		}
	}
	return none
}

fn (mut b Builder) default_field_value(struct_name_id u32, field_name string, zero ValueID) ValueID {
	struct_name := flat.source_type_name(struct_name_id)
	if value := b.declared_field_default_value(struct_name_id, field_name) {
		return value
	}
	if struct_name.ends_with('SystemError') {
		if field_name == 'msg' {
			return b.m.add_value(.string_literal, b.str_type, '', 0)
		}
		if field_name == 'code' {
			return b.m.get_or_add_const(b.i32_type, '-1')
		}
	}
	field_type_source_id_5 := b.field_type_name(struct_name_id, field_name)
	field_type := flat.source_type_name(field_type_source_id_5)
	if field_type.starts_with('[]') {
		elem_name := field_type[2..]
		elem_type := b.resolve_type(flat.source_type_id(elem_name))
		elem_size := b.m.type_size(elem_type)
		elem_size_const := b.m.get_or_add_const(b.i64_type, '${elem_size}')
		if fn_id := b.runtime_fn_lookup('array_new') {
			fn_ref := b.m.add_value(.func_ref, b.void_type, 'array_new', fn_id)
			return b.emit4(.call, b.array_type, fn_ref, elem_size_const, zero, zero)
		}
	}
	if field_type.starts_with('map[') {
		key_type, val_type := map_type_parts(field_type_source_id_5)
		key_size := b.m.type_size(b.resolve_type(key_type))
		val_size := b.m.type_size(b.resolve_type(val_type))
		actual_key_size := if key_size > 0 { key_size } else { 8 }
		actual_val_size := if val_size > 0 { val_size } else { 8 }
		key_size_const := b.m.get_or_add_const(b.i64_type, '${actual_key_size}')
		val_size_const := b.m.get_or_add_const(b.i64_type, '${actual_val_size}')
		if fn_id := b.runtime_fn_lookup('new_map') {
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
		storage_type := b.struct_field_storage_type(struct_name_id, field_name) or { b.resolve_type(field_type_source_id_5) }
		if value := b.nested_struct_default_value(field_type_source_id_5, storage_type) {
			return value
		}
		return b.default_value_for_type(storage_type)
	}
	return zero
}

// registered_field_type preserves exact-owner then short-name field lookup,
// including present empty bindings. Tuple source owners stay numeric; nominal and
// emitted C owner names retain their existing symbol index.
fn (b &Builder) registered_field_type(struct_name_id u32, field_name string) ?u32 {
	owner_id := ssa_pointer_base_source_type(struct_name_id)
	if fields := b.multi_return_field_types[owner_id] {
		if typ_source_id := fields[field_name] {
			return typ_source_id
		}
	}
	name := flat.source_type_name(owner_id)
	if typ_source_id := b.struct_field_types.type_id(name + '.' + field_name) {
		return typ_source_id
	}
	short_name := name.all_after('.')
	if typ_source_id := b.struct_field_types.type_id(short_name + '.' + field_name) {
		return typ_source_id
	}
	return none
}

fn (b &Builder) field_type_name(struct_name_id u32, field_name string) u32 {
	return b.registered_field_type(struct_name_id, field_name) or { u32(0) }
}

// struct_literal_type resolves a layout and its source owner in declaration lookup order.
fn (b &Builder) struct_literal_type(name_id u32) (TypeID, u32) {
	name := flat.source_type_name(name_id)
	short_name := name.all_after('.')
	if name.contains('[') && b.cur_func_ret_type != 0 {
		concrete_return := flat.source_type_name(b.cur_func_ret_type)
		if concrete_return.contains('[') {
			if typ := b.register_checker_generic_struct(concrete_return) {
				return typ, b.cur_func_ret_type
			}
		}
	}
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified_name := b.cur_module + '.' + short_name
		if typ := b.struct_types[qualified_name] {
			return typ, flat.source_type_id(qualified_name)
		}
	}
	if typ := b.registered_struct_type(name_id) {
		return typ, name_id
	}
	if typ := b.struct_types[short_name] {
		return typ, flat.source_type_id(short_name)
	}
	if typ := b.register_checker_generic_struct(name) {
		return typ, name_id
	}
	return TypeID(0), name_id
}

// register_checker_generic_struct materializes a concrete generic struct layout
// from the checker, which substitutes fields such as `[]T` with `[]int`.
fn (mut b Builder) register_checker_generic_struct(name string) ?TypeID {
	if b.tc == unsafe { nil } || !name.contains('[') || name.starts_with('[]')
		|| name.starts_with('map[')
		|| (name.starts_with('[') && b.is_fixed_array_type_name(flat.source_type_id(name))) {
		return none
	}
	canonical_name := qualify_type_name(name, b.cur_module)
	mut fields := b.tc.struct_fields_for_type(flat.source_type_id(canonical_name))
	if fields.len == 0 && canonical_name != name {
		fields = b.tc.struct_fields_for_type(flat.source_type_id(name))
	}
	if fields.len == 0 {
		return none
	}
	existing_id := b.struct_types[canonical_name] or { b.struct_types[name] or { TypeID(0) } }
	if existing_id > 0 && existing_id < b.m.type_store.types.len
		&& b.m.type_store.types[existing_id].fields.len > 0 {
		b.struct_types[name] = existing_id
		b.struct_types[canonical_name] = existing_id
		return existing_id
	}
	typ_id := if existing_id > 0 {
		existing_id
	} else {
		b.m.type_store.register(Type{
			kind: .struct_t
		})
	}
	b.struct_types[name] = typ_id
	b.struct_types[canonical_name] = typ_id
	mut field_types := []TypeID{cap: fields.len}
	mut field_names := []string{cap: fields.len}
	for field in fields {
		field_types << b.ssa_type_from_checker_type(field.typ)
		field_names << field.name
	}
	b.m.type_store.types[typ_id] = Type{
		kind:        .struct_t
		fields:      field_types
		field_names: field_names
	}
	return typ_id
}

fn (mut b Builder) build_heap_struct_init(id flat.NodeId, node flat.Node) ValueID {
	initial_typ_id, initial_struct_name_id := b.struct_literal_type(node.source_value_id())
	mut typ_id := initial_typ_id
	mut struct_name_id := initial_struct_name_id
	if typ_id == 0 || typ_id >= b.m.type_store.types.len
		|| b.m.type_store.types[typ_id].kind != .struct_t
		|| b.m.type_store.types[typ_id].fields.len == 0 {
		mut checked_source_id := b.checked_expr_type_name(id)
		checked_name := flat.source_type_name(checked_source_id)
		if checked_name.starts_with('&') {
			checked_source_id = flat.source_type_id(checked_name[1..])
		}
		if checked_source_id != 0 && checked_source_id != flat.source_type_unknown {
			checked_typ_id := b.resolve_type(checked_source_id)
			if checked_typ_id > 0 && checked_typ_id < b.m.type_store.types.len
				&& b.m.type_store.types[checked_typ_id].kind == .struct_t
				&& b.m.type_store.types[checked_typ_id].fields.len > 0 {
				typ_id = checked_typ_id
				struct_name_id = checked_source_id
			}
		}
	}
	if b.is_sum_type_id(typ_id) {
		value := b.build_sum_init(node, typ_id, struct_name_id)
		return b.heap_copy_value(value, typ_id)
	}
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
					field_type_name_source_id := b.field_type_name(struct_name_id, fname)

					mut field_val := b.build_storable_field_value(field_expr_id,
						field_type_name_source_id, field_type)
					if b.is_interface_type_id(typ_id) && fname == '_typ' {
						if concrete_type := b.interface_init_concrete_type(node, typ_id) {
							field_val = b.m.get_or_add_const(field_type, '${concrete_type}')
						}
					}
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
			if fname !in initialized && (!typ.is_union || initialized.len == 0) {
				offset := b.m.struct_field_offset(typ_id, fi)
				off_const := b.m.get_or_add_const(b.i64_type, '${offset}')
				field_type := if fi < typ.fields.len { typ.fields[fi] } else { b.i64_type }
				field_ptr := b.emit2(.get_element_ptr, b.m.type_store.get_ptr(field_type), alloca,
					off_const)
				default_value := b.default_field_value(struct_name_id, fname, zero)
				b.emit2(.store, b.void_type, default_value, field_ptr)
			}
		}
		size := b.m.type_size(typ_id)
		size_const := b.m.get_or_add_const(b.i64_type, '${size}')
		ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
		src_cast := b.emit1(.bitcast, ptr_i8, alloca)
		memdup_ref := b.m.add_value(.func_ref, ptr_i8, '__ssa_memdup',
			b.fn_ids['__ssa_memdup'])
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
	mut result := ValueID(0)
	plus_ref := b.m.add_value(.func_ref, b.str_type, 'string__plus', b.runtime_fn_id('string__plus'))
	for i in 0 .. n {
		child_id := b.a.child(&node, i)
		child := b.a.nodes[int(child_id)]
		mut part := ValueID(0)
		if child.kind == .string_literal {
			part = b.m.add_value(.string_literal, b.str_type, child.value_text(), 0)
		} else {
			val := b.build_expr(child_id)
			value_type := b.value_type(val)
			if value_type == b.str_type {
				part = val
			} else if value_type == b.i1_type {
				bool_ref := b.m.add_value(.func_ref, b.str_type, 'bool_str', b.runtime_fn_id('bool_str'))
				part = b.emit2(.call, b.str_type, bool_ref, val)
			} else if value_type in [b.f32_type, b.f64_type] {
				name := if value_type == b.f32_type {
					'strconv__f32_to_str_l'
				} else {
					'strconv__f64_to_str_l'
				}
				float_ref := b.m.add_value(.func_ref, b.str_type, name, b.runtime_fn_id(name))
				part = b.emit2(.call, b.str_type, float_ref, val)
			} else {
				int_str_ref := b.m.add_value(.func_ref, b.str_type, 'int_str', b.runtime_fn_id('int_str'))
				part = b.emit2(.call, b.str_type, int_str_ref, val)
			}
		}
		result = if i == 0 { part } else { b.emit3(.call, b.str_type, plus_ref, result, part) }
	}
	return result
}

fn (mut b Builder) get_field_ptr(base_addr ValueID, field_name string) ValueID {
	mut address := base_addr
	mut owner := b.deref_type(address)
	for b.is_pointer_type(owner) {
		address = b.emit1(.load, owner, address)
		owner = b.deref_type(address)
	}
	if owner > 0 && owner < b.m.type_store.types.len {
		storage_field := if owner == b.map_type && field_name == 'data' {
			// The SSA map header calls its sole field `state`, while the source
			// map type exposes the same pointer as `data`.
			'state'
		} else if field_name == '__v_sum_type_tag__' && b.is_sum_type_id(owner) {
			'typ'
		} else {
			field_name
		}
		if storage_field !in ['typ', '_pointer_variant_is_owned', '_payload'] && b.is_sum_type_id(owner) {
			payload := b.get_field_ptr(address, '_payload')
			canonical_field := b.canonical_sum_field_name(owner, storage_field)
			return b.get_field_ptr(payload, canonical_field)
		}
		typ := b.m.type_store.types[owner]
		index := typ.field_names.index(storage_field)
		if index >= 0 && index < typ.fields.len {
			offset := b.m.struct_field_offset(owner, index)
			constant := b.m.get_or_add_const(b.i64_type, offset.str())
			pointer := b.m.type_store.get_ptr(typ.fields[index])
			return b.emit2(.get_element_ptr, pointer, address, constant)
		}
		if path := b.embedded_field_path(owner, storage_field, []TypeID{}) {
			mut field := address
			for name in path {
				field = b.get_field_ptr(field, name)
			}
			return field
		}
	}
	zero := b.m.get_or_add_const(b.i64_type, '0')
	pointer := b.m.type_store.get_ptr(b.i64_type)
	return b.emit2(.get_element_ptr, pointer, address, zero)
}

// canonical_sum_field_name maps both qualified and short smartcast spellings to
// the short payload field stored in the sum layout.
fn (b &Builder) canonical_sum_field_name(sum_type TypeID, requested string) string {
	sum_name_source_id := b.sum_name_for_type_id(sum_type) or { return requested }
	sum_name := flat.source_type_name(sum_name_source_id)
	variants := b.sum_type_variants.type_ids(sum_name) or { return requested }
	module_name := sum_name.all_before_last('.')
	for variant in variants {
		variant_name := flat.source_type_name(variant)
		short_field := sum_variant_field_name(variant_name, module_name)
		qualified_field := sum_field_name(variant_name)
		if requested == short_field || requested == qualified_field {
			return short_field
		}
	}
	return requested
}

// embedded_field_path retains each embed so pointer fields are loaded before their members.
fn (b &Builder) embedded_field_path(owner TypeID, name string, seen []TypeID) ?[]string {
	if owner <= 0 || owner >= b.m.type_store.types.len || owner in seen {
		return none
	}
	typ := b.m.type_store.types[owner]
	if name in typ.field_names {
		return [name]
	}
	mut visited := seen.clone()
	visited << owner
	for index in b.struct_embeds[owner] {
		mut child := typ.fields[index]
		for b.is_pointer_type(child) {
			child = b.m.type_store.types[child].elem_type
		}
		if nested := b.embedded_field_path(child, name, visited) {
			mut path := [typ.field_names[index]]
			path << nested
			return path
		}
	}
	return none
}

// resolve_type lowers a source spelling identity to this builder's SSA TypeID namespace.
fn (mut b Builder) resolve_type(name_id u32) TypeID {
	if typ := b.primitive_type_id(name_id) {
		return typ
	}
	name := flat.source_type_name(name_id)
	// wasm has no 128-bit integer, and a u128 that falls through the primitive
	// table below degrades to a narrower type and then answers wrong: `u128 >> 64`
	// gives 0 and a 128-bit product loses its high limb. Refuse the name here
	// rather than lower it, so the caller is told which type is missing instead
	// of being handed a wrong number. The other 32-bit targets keep their own
	// resolution path, and a 64-bit target either supports u128 or refuses it in
	// its own backend.
	if b.m.target.ptr_size == 4 && name_id in [ssa_u128_source_type, ssa_i128_source_type] {
		panic('wasm: `${name}` is not supported on the wasm target; wasm has no 128-bit integer type')
	}
	if name_id == ssa_thread_source_type || name.starts_with('thread ') {
		return b.u64_type
	}
	if name in ['C.wchar_t', 'wchar.Character', 'wchar.C.wchar_t'] {
		return b.u32_type
	}
	if name in ['u128', 'i128'] {
		return b.wide_integer_type()
	}
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return b.option_type_id(flat.source_type_id(name[1..]), name[0] == `!`)
	}
	if name.starts_with('&') {
		inner := b.resolve_type(flat.source_type_id(name[1..]))
		return b.m.type_store.get_ptr(inner)
	}
	clean_name := normalize_primitive_type_name(name)
	if clean_name != name {
		return b.resolve_type(flat.source_type_id(clean_name))
	}
	if interface_typ := b.register_checker_interface(name_id) {
		return interface_typ
	}
	if name.contains('[') {
		if typ := b.register_checker_generic_struct(name) {
			return typ
		}
	}
	if alias_source_id := b.type_alias_target(name_id) {
		if alias_source_id != name_id {
			return b.resolve_type(alias_source_id)
		}
	}
	// SSA-level alias fallback (used when building without a checker).
	if alias_typ := b.type_aliases[name] {
		if alias_typ > 0 {
			return alias_typ
		}
	}
	if b.is_fixed_array_type_name(name_id) {
		elem_type := b.resolve_type(b.fixed_array_elem_type_name(name_id))
		return b.m.type_store.get_ptr(elem_type)
	}
	if typ := b.register_checker_generic_struct(name) {
		return typ
	}
	// Nominal compiler descriptors can use these names without being runtime containers.
	if name_id in [ssa_nominal_array_source_type, ssa_nominal_map_source_type] {
		if b.cur_module.len > 0 && b.cur_module != 'main' && b.cur_module != 'builtin' {
			if typ := b.struct_types[b.cur_module + '.' + name] {
				return typ
			}
		}
		if typ := b.struct_types[name] {
			return typ
		}
	}
	if name.starts_with('[]') || name_id in [ssa_array_source_type, ssa_nominal_array_source_type] {
		return b.array_type
	}
	if name_id == ssa_strings_builder_source_type {
		return b.array_type
	}
	if name.starts_with('map[') || name_id in [ssa_map_source_type, ssa_nominal_map_source_type] {
		return b.map_type
	}
	if b.is_enum_type_name(name_id) {
		return b.i32_type
	}
	if canonical_source_id := b.canonical_sum_type_name(name_id) {
		if typ := b.registered_struct_type(canonical_source_id) {
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
	if typ := b.registered_struct_type(name_id) {
		return typ
	}
	if typ := b.struct_types[short_name] {
		return typ
	}
	if name_id == ssa_builder_source_type {
		return b.array_type
	}
	return b.i64_type
}

fn (mut b Builder) primitive_type_id(name_id u32) ?TypeID {
	return match name_id {
		flat.source_type_int {
			b.i32_type
		}
		flat.source_type_isize {
			if b.m.target.ptr_size == 4 { b.i32_type } else { b.i64_type }
		}
		flat.source_type_uint {
			if b.m.target.ptr_size == 4 { b.u32_type } else { b.u64_type }
		}
		flat.source_type_i8, flat.source_type_char {
			b.i8_type
		}
		flat.source_type_i16 {
			b.m.type_store.get_int(16)
		}
		flat.source_type_i32 {
			b.i32_type
		}
		flat.source_type_i64 {
			b.i64_type
		}
		flat.source_type_u8 {
			b.u8_type
		}
		flat.source_type_u16 {
			b.u16_type
		}
		flat.source_type_u32, flat.source_type_rune {
			b.u32_type
		}
		flat.source_type_u64 {
			b.u64_type
		}
		flat.source_type_f32 {
			b.f32_type
		}
		flat.source_type_f64 {
			b.f64_type
		}
		flat.source_type_bool {
			b.i1_type
		}
		flat.source_type_string {
			b.str_type
		}
		flat.source_type_void, u32(0) {
			b.void_type
		}
		flat.source_type_voidptr {
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
		'int', 'i8', 'char', 'i16', 'i32', 'rune', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
		'f64', 'bool', 'string', 'void', 'voidptr', '' {
			short_name
		}
		else {
			name
		}
	}
}

fn (mut b Builder) option_type_id(base_name_id u32, is_result bool) TypeID {
	base_typ := b.resolve_type(base_name_id)
	return b.option_type_id_with_payload(base_name_id, base_typ, is_result)
}

fn (mut b Builder) option_type_id_with_payload(base_name_id u32, base_typ TypeID, is_result bool) TypeID {
	base_name := flat.source_type_name(base_name_id)
	key_text := (if is_result { '!' } else { '?' }) + base_name
	key := flat.source_type_id(key_text)
	if typ := b.option_types[key] {
		return typ
	}
	mut fields := []TypeID{}
	mut field_names := []string{}
	fields << b.i1_type
	field_names << 'ok'
	if base_typ != b.void_type {
		fields << base_typ
		field_names << 'value'
	}
	if is_result {
		fields << b.resolve_type(flat.source_type_id('IError'))
		field_names << 'err'
	}
	typ_id := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      fields
		field_names: field_names
	})
	b.option_types[key] = typ_id
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

fn (b &Builder) option_payload_type_name(id flat.NodeId) u32 {
	name_source_id := b.checked_expr_type_name(id)
	name := flat.source_type_name(name_source_id)
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return flat.source_type_id(name[1..])
	}
	return name_source_id
}

fn (b &Builder) is_option_type(typ_id TypeID) bool {
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return false
	}
	// Void is TypeID(0), so the payload type cannot identify a void result.
	// Use registered option identities so an ordinary `ok` field stays ordinary.
	typ := b.m.type_store.types[typ_id]
	if typ.kind != .struct_t || typ.field_names.len == 0 || typ.field_names[0] != 'ok' {
		return false
	}
	for _, option_typ in b.option_types {
		if option_typ == typ_id {
			return true
		}
	}
	return false
}

fn (b &Builder) is_enum_type_name(name_id u32) bool {
	name := flat.source_type_name(name_id)
	if name.len == 0 {
		return false
	}
	if name in b.enum_types {
		return true
	}
	short_name := name.all_after('.')
	return short_name in b.enum_types
}

fn (b &Builder) enum_autostr_fn_name(type_name_id u32) ?string {
	type_name := flat.source_type_name(type_name_id)
	if type_name_id == 0 {
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
		if !b.is_enum_type_name(flat.source_type_id(candidate)) {
			continue
		}
		fn_name := '${ssa_c_name(candidate)}__autostr'
		if fn_name in b.fn_ids {
			return fn_name
		}
	}
	return none
}

// resolve_type_in_module lowers a source spelling in its declaration module.
fn (mut b Builder) resolve_type_in_module(name_id u32, module_name string) TypeID {
	name := flat.source_type_name(name_id)
	if name.len > 1 && (name[0] == `?` || name[0] == `!`) {
		return b.option_type_id(flat.source_type_id(name[1..]), name[0] == `!`)
	}
	if name.starts_with('&') {
		inner := b.resolve_type_in_module(flat.source_type_id(name[1..]), module_name)
		return b.m.type_store.get_ptr(inner)
	}
	if !name.contains('.') && module_name.len > 0 && module_name != 'main'
		&& module_name != 'builtin' {
		qualified_name := module_name + '.' + name
		qualified_id := flat.source_type_id(qualified_name)
		if qualified_name in b.enum_types {
			return b.i32_type
		}
		if typ := b.struct_types[qualified_name] {
			return typ
		}
		if alias_source_id := b.type_alias_target(qualified_id) {
			if alias_source_id != qualified_id {
				return b.resolve_type_in_module(alias_source_id, module_name)
			}
		}
	}
	return b.resolve_type(name_id)
}

// resolve_c_decl_type lowers a source spelling using C declaration rules.
fn (mut b Builder) resolve_c_decl_type(name_id u32) TypeID {
	name := flat.source_type_name(name_id)
	if name.starts_with('...') {
		return b.m.type_store.get_ptr(b.i8_type)
	}
	if name.starts_with('C.') {
		return b.resolve_type(flat.source_type_id(name[2..]))
	}
	return b.resolve_type(name_id)
}

fn (b &Builder) type_alias_target(name_id u32) ?u32 {
	name := flat.source_type_name(name_id)
	if b.tc == unsafe { nil } || name.len == 0 {
		return none
	}
	if target_source_id := b.tc.type_aliases.type_id(name) {
		return target_source_id
	}
	if !name.contains('.') && b.cur_module.len > 0 && b.cur_module != 'main'
		&& b.cur_module != 'builtin' {
		qualified_name := b.cur_module + '.' + name
		if target_source_id := b.tc.type_aliases.type_id(qualified_name) {
			return target_source_id
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

fn (b &Builder) infer_v_type(id flat.NodeId) u32 {
	if int(id) < 0 {
		return flat.source_type_int
	}
	node := b.a.nodes[int(id)]
	match node.kind {
		.struct_init {
			return node.source_value_id()
		}
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				inner := b.infer_v_type(b.a.child(&node, 0))
				return flat.source_type_id('&' + flat.source_type_name(inner))
			}
			return flat.source_type_int
		}
		.string_literal, .string_interp {
			return flat.source_type_string
		}
		.int_literal {
			return flat.source_type_int
		}
		.bool_literal {
			return flat.source_type_bool
		}
		.ident {
			if addr := b.vars[node.value_text()] {
				val := b.m.values[addr]
				if val.typ == b.str_type || (val.typ > 0 && val.typ < b.m.type_store.types.len
					&& b.m.type_store.types[val.typ].kind == .ptr_t
					&& b.m.type_store.types[val.typ].elem_type == b.str_type) {
					return flat.source_type_string
				}
			}
			if expr_id := b.lookup_const_expr(node.value_text()) {
				return b.infer_v_type(expr_id)
			}
			return flat.source_type_int
		}
		.call {
			fn_node := b.a.child_node(&node, 0)
			if fn_node.value_text() == 'int_str' {
				return flat.source_type_string
			}
			if fn_idx := b.fn_ids[fn_node.value_text()] {
				ret := b.m.funcs[fn_idx].typ
				if ret == b.str_type {
					return flat.source_type_string
				}
			}
			return flat.source_type_int
		}
		.infix {
			lt_source_id := b.infer_v_type(b.a.child(&node, 0))

			if lt_source_id == flat.source_type_string {
				return flat.source_type_string
			}
			return lt_source_id
		}
		else {
			return flat.source_type_int
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
	ops << if op == .store { b.coerce_scalar_store_value(a, c, b.cur_block) } else { a }
	ops << c
	return b.m.add_instr(op, b.cur_block, typ, ops)
}

fn (mut b Builder) coerce_scalar_store_value(value ValueID, address ValueID, block_id BlockID) ValueID {
	address_type := b.value_type(address)
	if b.m.target.ptr_size != 4 || !b.is_pointer_type(address_type) {
		return value
	}
	// Store conversions must survive scalar alloca promotion in production builds.
	target := b.m.type_store.types[address_type].elem_type
	return b.coerce_numeric_value_in_block(value, target, block_id)
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

fn (mut b Builder) build_compound_assign_value(op flat.Op, current ValueID, rhs ValueID, lhs_type_source_id u32) ValueID {
	typ := b.value_type(current)
	if typ == b.str_type && op == .plus_assign {
		return b.build_string_infix(.plus, current, rhs)
	}
	if value := b.build_compound_operator_call(lhs_type_source_id, op, current, rhs) {
		return value
	}
	return b.emit2(b.compound_to_op(op, typ), typ, current, rhs)
}

// build_compound_operator_call lowers `a += b` on operator-overloaded values
// through the same method used by `a + b`, instead of emitting integer add/sub
// instructions on the aggregate representation.
fn (mut b Builder) build_compound_operator_call(lhs_type_source_id u32, op flat.Op, current ValueID, rhs ValueID) ?ValueID {
	typ_id := b.value_type(current)
	if typ_id <= 0 || typ_id >= b.m.type_store.types.len {
		return none
	}
	typ := b.m.type_store.types[typ_id]
	if typ.kind !in [.struct_t, .array_t] {
		return none
	}
	type_name := flat.source_type_name(lhs_type_source_id).trim_left('&')
	if type_name.len == 0 || type_name in ['int', 'i64', 'u64', 'array', 'map'] {
		return none
	}
	operator := match op {
		.plus_assign { '+' }
		.minus_assign { '-' }
		.mul_assign { '*' }
		.div_assign { '/' }
		.mod_assign { '%' }
		.amp_assign { '&' }
		.pipe_assign { '|' }
		.xor_assign { '^' }
		else { return none }
	}
	short_name := type_name.all_after_last('.')
	mut candidates := []string{}
	candidates << '${type_name}.${operator}'
	candidates << '${short_name}.${operator}'
	if b.cur_module.len > 0 && b.cur_module !in ['main', 'builtin'] {
		candidates << '${b.cur_module}.${type_name}.${operator}'
	}
	for candidate in candidates {
		fn_idx := b.fn_ids[candidate] or { continue }
		ret_type := b.m.funcs[fn_idx].typ
		if ret_type == 0 {
			return none
		}
		fn_ref := b.m.add_value(.func_ref, b.void_type, candidate, fn_idx)
		mut args := []ValueID{}
		args << fn_ref
		args << current
		args << rhs
		return b.m.add_instr(.call, b.cur_block, ret_type, args)
	}
	return none
}

fn (b &Builder) compound_to_op(op flat.Op, typ TypeID) OpCode {
	if b.is_float_type(typ) {
		return match op {
			.plus_assign { .fadd }
			.minus_assign { .fsub }
			.mul_assign { .fmul }
			.div_assign { .fdiv }
			.mod_assign { .frem }
			else { .fadd }
		}
	}
	if op == .right_shift_unsigned_assign {
		return .lshr
	}
	if b.is_unsigned_type(typ) {
		match op {
			.div_assign { return .udiv }
			.mod_assign { return .urem }
			.right_shift_assign { return .lshr }
			else {}
		}
	}
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
		.power_assign { panic('power assignment reached the V3 SSA backend') }
		else { .add }
	}
}

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

// cur_func_ret_type_text resolves the indexed source type spelling.
@[inline]
pub fn (value &Builder) cur_func_ret_type_text() string {
	return flat.source_type_name(value.cur_func_ret_type)
}

// typ_name_text resolves the indexed source type spelling.
@[inline]
fn (value &IfGuardState) typ_name_text() string {
	return flat.source_type_name(value.typ_name)
}

// field_types_text resolves the indexed source type spelling.
@[inline]
fn (value &NativeCStructAbi) field_types_text() []string {
	return value.field_types.names()
}
