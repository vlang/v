module c

struct ProfileCounterMeta {
	fn_name      string
	counter_name string
	calls_name   string
}

// The loader uses the declared short name until a user `profile` module makes
// the canonical `v.profile` name necessary to keep the two modules distinct.
fn (g &FlatGen) profile_runtime_module_name() string {
	if 'v.profile.v__profile_enabled' in g.global_types {
		return 'v.profile'
	}
	return 'profile'
}

fn (g &FlatGen) profile_enabled_c_name() string {
	return g.global_c_name('${g.profile_runtime_module_name()}.v__profile_enabled')
}

fn (g &FlatGen) profile_timer_c_name() string {
	return if g.target.os == 'macos' { 'time__vpc_now_darwin' } else { 'time__vpc_now' }
}

fn profile_v1_fn_name(cfn_name string, module_name string, fn_name string) string {
	if cfn_name == 'main' && fn_name == 'main' && module_name in ['', 'main'] {
		return 'main__main'
	}
	if module_name in ['', 'main'] && !cfn_name.starts_with('main__') {
		return 'main__${cfn_name}'
	}
	if module_name == 'builtin' && !cfn_name.starts_with('builtin__') {
		return 'builtin__${cfn_name}'
	}
	return cfn_name
}

fn (mut g FlatGen) gen_profile_fn_begin(cfn_name string, module_name string, fn_name string, is_inline bool) {
	g.profile_fn_active = false
	g.profile_fn_restore_enabled = false
	if g.profile_file.len == 0 || (g.profile_no_inline && is_inline)
		|| module_name == g.profile_runtime_module_name()
		|| fn_name.starts_with('time.vpc_now')
		|| cfn_name.starts_with('time__vpc_now') {
		return
	}
	// Prefix the counter names with a unique per-function index. Without it the derived
	// names collide: a function `…__lower`'s call counter is `vpc_…__lower_calls` (u64),
	// which is identical to a function `…__lower_calls`'s time accumulator `vpc_…__lower_calls`
	// (double) — a "redefinition with a different type" C error. The same holds for the
	// `_only_current` suffix. The index makes every base name unambiguous.
	counter_name := 'vpc_${g.profile_counters.len}_${cfn_name}'
	calls_name := '${counter_name}_calls'
	profile_fn_name := profile_v1_fn_name(cfn_name, module_name, fn_name)
	g.profile_fn_active = true
	g.profile_fn_restore_enabled = g.profile_fns.len > 0 && profile_fn_name in g.profile_fns
	profile_enabled := g.profile_enabled_c_name()
	if g.profile_fn_restore_enabled {
		g.writeln('bool _prev_v__profile_enabled = ${profile_enabled};')
		g.writeln('${profile_enabled} = true;')
	}
	g.writeln('double _PROF_FN_START = ${g.profile_timer_c_name()}();')
	g.writeln('double _PROF_PREV_MEASURED_TIME = prof_measured_time;')
	g.writeln('if (${profile_enabled}) { ${calls_name}++; } // ${profile_fn_name}')
	g.profile_counters << ProfileCounterMeta{
		fn_name:      profile_fn_name
		counter_name: counter_name
		calls_name:   calls_name
	}
}

fn (mut g FlatGen) gen_profile_fn_exit() {
	if !g.profile_fn_active {
		return
	}
	pc := g.profile_counters.last()
	profile_enabled := g.profile_enabled_c_name()
	g.writeln('if (${profile_enabled}) {')
	g.indent++
	g.writeln('double _PROF_ELAPSED = ${g.profile_timer_c_name()}() - _PROF_FN_START;')
	g.writeln('${pc.counter_name} += _PROF_ELAPSED;')
	g.writeln('${pc.counter_name}_only_current += _PROF_ELAPSED - (prof_measured_time - _PROF_PREV_MEASURED_TIME);')
	g.writeln('prof_measured_time = _PROF_PREV_MEASURED_TIME + _PROF_ELAPSED;')
	g.indent--
	g.writeln('}')
	if g.profile_fn_restore_enabled {
		g.writeln('${profile_enabled} = _prev_v__profile_enabled;')
	}
}

fn (mut g FlatGen) gen_profile_registration() {
	if g.profile_file.len == 0 {
		return
	}
	g.writeln('\tv_signal_with_handler_cast(SIGINT, vprint_profile_stats_on_signal);')
	g.writeln('\tv_signal_with_handler_cast(SIGTERM, vprint_profile_stats_on_signal);')
	g.writeln('\tatexit(vprint_profile_stats);')
	if 'no_profile_startup' in g.compile_values || g.profile_fns.len > 0 {
		g.writeln('\tvreset_profile_stats();')
	}
	if g.profile_fns.len > 0 {
		// v__profile_enabled will be set true *inside* the fns in g.profile_fns:
		g.writeln('\t${g.profile_enabled_c_name()} = false;')
	}
}

fn (mut g FlatGen) gen_profile_startup_enable() {
	if g.profile_file.len > 0 {
		g.writeln('\t${g.profile_enabled_c_name()} = true;')
	}
}

fn (mut g FlatGen) gen_profile_support() {
	if g.profile_file.len == 0 {
		return
	}
	// The profiling timer is forced into the used-function set even when user
	// code does not import `time`. Keep its platform C function declarations
	// available in that minimal program too. The V-generated Mach struct already
	// exists here, so including the system header would redefine it.
	if g.target.os == 'macos' {
		g.writeln('extern u64 mach_absolute_time(void);')
		g.writeln('extern int mach_timebase_info(mach_timebase_info_data_t*);')
	}
	g.writeln('// V profile counters:')
	for pc in g.profile_counters {
		g.writeln('double ${pc.counter_name} = 0.0; double ${pc.counter_name}_only_current = 0.0; u64 ${pc.calls_name} = 0;')
	}
	g.writeln('// V profile thread local:')
	g.writeln('#if defined(__TINYC__)')
	g.writeln('/* TinyCC on Darwin does not implement working TLS; profiling is already documented as not thread safe. */')
	g.writeln('#elif defined(__cplusplus) && __cplusplus >= 201103L')
	g.writeln('#define PROF_THREAD_LOCAL thread_local')
	g.writeln('#elif defined(__GNUC__) && __GNUC__ < 5')
	g.writeln('#define PROF_THREAD_LOCAL __thread')
	g.writeln('#elif defined(_MSC_VER)')
	g.writeln('#define PROF_THREAD_LOCAL __declspec(thread)')
	g.writeln('#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_THREADS__)')
	g.writeln('#define PROF_THREAD_LOCAL _Thread_local')
	g.writeln('#endif')
	g.writeln('#ifndef PROF_THREAD_LOCAL')
	g.writeln('#if defined(__GNUC__) && !defined(__TINYC__)')
	g.writeln('#define PROF_THREAD_LOCAL __thread')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifdef PROF_THREAD_LOCAL')
	g.writeln('static PROF_THREAD_LOCAL double prof_measured_time = 0.0;')
	g.writeln('#else')
	g.writeln('double prof_measured_time = 0.0; // multithreaded: wrong values for func times without its children')
	g.writeln('#endif')
	g.writeln('void vprint_profile_stats(void) {')
	g.indent++
	g.writeln('double f = 1.0;')
	if g.target.os == 'windows' {
		// QueryPerformanceCounter() / QueryPerformanceFrequency()
		// https://learn.microsoft.com/en-us/windows/win32/sysinfo/acquiring-high-resolution-time-stamps
		g.writeln('u64 freq_time = 0;')
		g.writeln('QueryPerformanceFrequency((void*)(&freq_time));')
		g.writeln('f = (double)1000000000.0 / (double)freq_time;')
	}
	fstring := '"%14llu %14.3fms %14.3fms %14.0fns %s \\n"'
	if g.profile_file == '-' {
		for pc in g.profile_counters {
			g.writeln('if (${pc.calls_name}) printf(${fstring}, ${pc.calls_name}, (${pc.counter_name} * f) / 1000000.0, (${pc.counter_name}_only_current * f) / 1000000.0, (${pc.counter_name} * f) / ${pc.calls_name}, "${c_escape(pc.fn_name)}");')
		}
	} else {
		g.writeln('FILE* fp;')
		g.writeln('fp = fopen("${c_escape(g.profile_file)}", "w+");')
		g.writeln('if (fp == NULL) { return; }')
		for pc in g.profile_counters {
			g.writeln('if (${pc.calls_name}) fprintf(fp, ${fstring}, ${pc.calls_name}, (${pc.counter_name} * f) / 1000000.0, (${pc.counter_name}_only_current * f) / 1000000.0, (${pc.counter_name} * f) / ${pc.calls_name}, "${c_escape(pc.fn_name)}");')
		}
		g.writeln('fclose(fp);')
	}
	g.indent--
	g.writeln('}')
	g.writeln('')
	g.writeln('void vreset_profile_stats(void) {')
	g.indent++
	for pc in g.profile_counters {
		g.writeln('${pc.calls_name} = 0;')
		g.writeln('${pc.counter_name} = 0.0;')
		g.writeln('${pc.counter_name}_only_current = 0.0;')
	}
	g.indent--
	g.writeln('}')
	g.writeln('')
	g.writeln('void vprint_profile_stats_on_signal(int sig) {')
	g.indent++
	g.writeln('(void)sig;')
	g.writeln('exit(130);')
	g.indent--
	g.writeln('}')
	g.writeln('')
}
