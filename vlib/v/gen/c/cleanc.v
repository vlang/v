	compile_values                  map[string]string // explicit `-d` values used by `$d(...)` in `#flag`s
	output_path                     string
	output_error                    string
	c99_mode                        bool
	trace_calls                     bool // -d trace: custom call-site hooks
	is_trace_calls                  bool // -trace-calls: function-entry logging
	trace_fns                       []string
	track_heap                      bool
	inside_trace_call               bool
	skip_generics                   bool
	skip_enum_autostr               bool
	// prefix: a parallel C build repeats that prefix per unit, and only the unit
	// holding `_vinit` may define them.
	g.gen_embed_blob_joined()
	g.writeln('void _vinit() {')
	g.gen_trace_call('_vinit', '', '_vinit')
	if 'gcboehm' in g.compile_defines || 'vgc' in g.compile_defines {
		g.writeln('\tgc_runtime_init();')
	}
	// A split `$embed_file` payload is put back together before anything else can
	}
}

fn (mut g FlatGen) gen_vcleanup() {
	if !g.is_shared && g.module_cleanup_fns.len == 0 && !g.is_trace_calls {
		return
	}
	fn_start_pos := g.sb.len
	g.writeln('void _vcleanup(void) {')
	g.writeln('\tstatic bool once = false;')
	g.writeln('\tif (once) { return; }')
	g.writeln('\tonce = true;')
	g.gen_trace_call('_vcleanup', '', '_vcleanup')
	cleanup_fns := g.ordered_module_cleanup_fns()
	for i := cleanup_fns.len - 1; i >= 0; i-- {
		g.writeln('\t${cleanup_fns[i]}();')
	}
