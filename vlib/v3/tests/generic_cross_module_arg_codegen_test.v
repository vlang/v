import os

const generic_cross_vexe = @VEXE
const generic_cross_tests_dir = os.dir(@FILE)
const generic_cross_v3_dir = os.dir(generic_cross_tests_dir)
const generic_cross_vlib_dir = os.dir(generic_cross_v3_dir)
const generic_cross_v3_src = os.join_path(generic_cross_v3_dir, 'v3.v')

fn generic_cross_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_cross_module_arg_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_cross_vexe} -gc none -path "${generic_cross_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_cross_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn generic_cross_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn generic_cross_run_project(v3_bin string, name string, files map[string]string) string {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_${name}_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		generic_cross_write_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_imported_generic_arg_uses_qualified_local_type_in_specialized_signature() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_cross_module_arg', {
		'wrap/wrap.v': 'module wrap\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n'
		'user/user.v': 'module user\n\nimport wrap\n\nstruct Local {\n\tn int\n}\n\nstruct Holder[T] {\n\tbox wrap.Box[T]\n}\n\nfn Holder.make[T](box wrap.Box[T]) Holder[T] {\n\treturn Holder[T]{\n\t\tbox: box\n\t}\n}\n\nfn take(box wrap.Box[Local]) int {\n\treturn box.value.n\n}\n\npub fn run() int {\n\tbox := wrap.Box[Local]{\n\t\tvalue: Local{\n\t\t\tn: 7\n\t\t}\n\t}\n\tholder := Holder.make(box)\n\treturn take(holder.box)\n}\n'
		'main.v':      'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '7'
}

fn test_imported_generic_application_arg_keeps_caller_base_module() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_cross_module_application_arg', {
		'wrap/wrap.v': 'module wrap\n\npub struct Wrapper[T] {\npub:\n\tvalues []T\n}\n\npub fn make[T]() Wrapper[T] {\n\treturn Wrapper[T]{}\n}\n'
		'main.v':      'module main\n\nimport wrap\n\nstruct User {\n\tn int\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nfn main() {\n\twrapped := wrap.make[Box[User]]()\n\tprintln(int_str(wrapped.values.len))\n}\n'
	})
	assert out == '0'
}

fn test_nested_generic_return_infers_outer_static_method_arg() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_nested_return_arg', {
		'wrap/wrap.v': 'module wrap\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n\npub fn Box.new[T](value T) Box[T] {\n\treturn Box[T]{\n\t\tvalue: value\n\t}\n}\n'
		'user/user.v': 'module user\n\nimport wrap\n\nstruct Local {\n\tn int\n}\n\nstruct Holder[T] {\n\tbox wrap.Box[T]\n}\n\nfn Holder.from[T](box wrap.Box[T]) Holder[T] {\n\treturn Holder[T]{\n\t\tbox: box\n\t}\n}\n\npub fn run() int {\n\tholder := Holder.from(wrap.Box.new(Local{\n\t\tn: 9\n\t}))\n\treturn holder.box.value.n\n}\n'
		'main.v':      'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '9'
}

fn test_specialized_receiver_method_body_specializes_nested_generic_method_return() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_nested_receiver_method_return', {
		'printer/printer.v': 'module printer\n\npub struct Standard[W] {\npub:\n\tw W\n}\n\npub struct Sink[W] {\npub:\n\tw W\n}\n\npub fn Standard.new[W](w W) Standard[W] {\n\treturn Standard[W]{\n\t\tw: w\n\t}\n}\n\npub fn (standard Standard[W]) sink() Sink[W] {\n\treturn Sink[W]{\n\t\tw: standard.w\n\t}\n}\n\npub fn (sink Sink[W]) value() W {\n\treturn sink.w\n}\n'
		'user/user.v':       'module user\n\nimport printer\n\nstruct Local {\n\tn int\n}\n\nstruct Worker[W] {\n\tstandard printer.Standard[W]\n}\n\nfn (worker Worker[W]) run() W {\n\tsink := worker.standard.sink()\n\treturn sink.value()\n}\n\npub fn run() int {\n\tworker := Worker[Local]{\n\t\tstandard: printer.Standard.new(Local{\n\t\t\tn: 11\n\t\t})\n\t}\n\treturn worker.run().n\n}\n'
		'main.v':            'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '11'
}

fn test_specialized_receiver_method_body_offsets_explicit_receiver_args() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_receiver_explicit_arg_offset', {
		'printer/printer.v': 'module printer\n\npub struct Matcher {\npub:\n\tn int\n}\n\npub fn (m Matcher) clone() Matcher {\n\treturn m\n}\n\npub struct Standard[W] {\npub:\n\tw W\n}\n\npub struct Sink[W] {\npub:\n\tw W\n\tn int\n}\n\npub fn Standard.new[W](w W) Standard[W] {\n\treturn Standard[W]{\n\t\tw: w\n\t}\n}\n\npub fn (mut standard Standard[W]) sink_with_matcher(matcher Matcher) Sink[W] {\n\treturn Sink[W]{\n\t\tw: standard.w\n\t\tn: matcher.n\n\t}\n}\n\npub fn (sink Sink[W]) value() W {\n\treturn sink.w\n}\n\npub fn (sink Sink[W]) count() int {\n\treturn sink.n\n}\n'
		'user/user.v':       'module user\n\nimport printer\n\nstruct Local {\n\tn int\n}\n\nstruct Worker[W] {\nmut:\n\tstandard printer.Standard[W]\n}\n\nfn (mut worker Worker[W]) run(matcher printer.Matcher) int {\n\tsink := worker.standard.sink_with_matcher(matcher.clone())\n\treturn sink.value().n + sink.count()\n}\n\npub fn run() int {\n\tmut worker := Worker[Local]{\n\t\tstandard: printer.Standard.new(Local{\n\t\t\tn: 13\n\t\t})\n\t}\n\treturn worker.run(printer.Matcher{\n\t\tn: 5\n\t})\n}\n'
		'main.v':            'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '18'
}

fn test_multiline_generic_builder_call_infers_all_arguments() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_multiline_builder_call', {
		'worker/worker.v': 'module worker\n\npub struct Matcher {\npub:\n\tn int\n}\n\npub struct Searcher {\npub:\n\tn int\n}\n\npub fn Searcher.new() Searcher {\n\treturn Searcher{\n\t\tn: 3\n\t}\n}\n\npub struct Printer[W] {\npub:\n\tw W\n}\n\npub fn Printer.new[W](w W) Printer[W] {\n\treturn Printer[W]{\n\t\tw: w\n\t}\n}\n\npub struct Worker[W] {\npub:\n\tmatcher Matcher\n\tsearcher Searcher\n\tprinter Printer[W]\n}\n\npub struct Builder {}\n\npub fn Builder.new() Builder {\n\treturn Builder{}\n}\n\npub fn (builder Builder) build[W](matcher Matcher, searcher Searcher, printer Printer[W]) Worker[W] {\n\t_ = builder\n\treturn Worker[W]{\n\t\tmatcher: matcher\n\t\tsearcher: searcher\n\t\tprinter: printer\n\t}\n}\n'
		'user/user.v':     'module user\n\nimport worker\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tbuilder := worker.Builder.new()\n\tprinter := worker.Printer.new(Local{\n\t\tn: 17\n\t})\n\tmut w := builder.build(worker.Matcher{\n\t\tn: 2\n\t}, worker.Searcher.new(),\n\t\tprinter)\n\treturn w.matcher.n + w.searcher.n + w.printer.w.n\n}\n'
		'main.v':          'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '22'
}

fn test_generic_builder_call_infers_static_wrapped_nested_generic_arg() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_static_wrapped_builder_call', {
		'worker/worker.v': 'module worker\n\npub struct Matcher {\npub:\n\tn int\n}\n\npub struct Searcher {\npub:\n\tn int\n}\n\npub fn Searcher.new() Searcher {\n\treturn Searcher{\n\t\tn: 3\n\t}\n}\n\npub struct Summary[W] {\npub:\n\tw W\n}\n\npub fn Summary.new[W](w W) Summary[W] {\n\treturn Summary[W]{\n\t\tw: w\n\t}\n}\n\npub enum PrinterKind {\n\tsummary\n}\n\npub struct Printer[W] {\npub:\n\tkind PrinterKind\n\tsummary Summary[W]\n}\n\npub fn Printer.summary[W](summary Summary[W]) Printer[W] {\n\treturn Printer[W]{\n\t\tkind: .summary\n\t\tsummary: summary\n\t}\n}\n\npub struct Worker[W] {\npub:\n\tmatcher Matcher\n\tsearcher Searcher\n\tprinter Printer[W]\n}\n\npub struct Builder {}\n\npub fn Builder.new() Builder {\n\treturn Builder{}\n}\n\npub fn (builder Builder) build[W](matcher Matcher, searcher Searcher, printer Printer[W]) Worker[W] {\n\t_ = builder\n\treturn Worker[W]{\n\t\tmatcher: matcher\n\t\tsearcher: searcher\n\t\tprinter: printer\n\t}\n}\n'
		'user/user.v':     'module user\n\nimport worker\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tbuilder := worker.Builder.new()\n\tsummary_printer := worker.Printer.summary(worker.Summary.new(Local{\n\t\tn: 19\n\t}))\n\tmut w := builder.build(worker.Matcher{\n\t\tn: 2\n\t}, worker.Searcher.new(),\n\t\tsummary_printer)\n\treturn w.matcher.n + w.searcher.n + w.printer.summary.w.n\n}\n'
		'main.v':          'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '24'
}

fn test_static_generic_type_function_with_builtin_arg_uses_emitted_name() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_static_builtin_arg_builder_call', {
		'worker/worker.v': 'module worker\n\npub struct Summary[W] {\npub:\n\tw W\n}\n\npub fn Summary.new[W](w W) Summary[W] {\n\treturn Summary[W]{\n\t\tw: w\n\t}\n}\n\npub enum PrinterKind {\n\tsummary\n}\n\npub struct Printer[W] {\npub:\n\tkind PrinterKind\n\tsummary Summary[W]\n}\n\npub fn Printer.summary[W](summary Summary[W]) Printer[W] {\n\treturn Printer[W]{\n\t\tkind: .summary\n\t\tsummary: summary\n\t}\n}\n\npub struct Worker[W] {\npub:\n\tprinter Printer[W]\n}\n\npub struct Builder {}\n\npub fn Builder.new() Builder {\n\treturn Builder{}\n}\n\npub fn (builder Builder) build[W](printer Printer[W]) Worker[W] {\n\t_ = builder\n\treturn Worker[W]{\n\t\tprinter: printer\n\t}\n}\n'
		'user/user.v':     'module user\n\nimport worker\n\npub fn run() int {\n\tbuilder := worker.Builder.new()\n\tprinter := worker.Printer.summary(worker.Summary.new(7))\n\tw := builder.build(printer)\n\treturn w.printer.summary.w\n}\n'
		'main.v':          'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '7'
}

fn test_static_generic_type_function_with_imported_generic_arg_uses_emitted_name() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_static_imported_arg_builder_call', {
		'printer/printer.v': 'module printer\n\npub struct Standard[W] {\npub:\n\tw W\n}\n\npub fn Standard.new[W](w W) Standard[W] {\n\treturn Standard[W]{\n\t\tw: w\n\t}\n}\n'
		'core/core.v':       'module core\n\nimport printer\n\nstruct BufferWriter {\n\tn int\n}\n\nstruct Printer[W] {\n\tstandard printer.Standard[W]\n}\n\nfn Printer.standard[W](standard printer.Standard[W]) Printer[W] {\n\treturn Printer[W]{\n\t\tstandard: standard\n\t}\n}\n\npub fn run() int {\n\tstandard := printer.Standard.new(BufferWriter{\n\t\tn: 61\n\t})\n\tprinter_ := Printer.standard(standard)\n\treturn printer_.standard.w.n\n}\n'
		'main.v':            'module main\n\nimport core\n\nfn main() {\n\tprintln(int_str(core.run()))\n}\n'
	})
	assert out == '61'
}

fn test_generic_struct_interface_dispatch_emits_required_methods() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_interface_dispatch', {
		'sink/sink.v': 'module sink\n\npub interface Sink {\nmut:\n\tbegin() bool\n}\n\npub struct Box[W] {\nmut:\n\tvalue W\n\tcount int\n}\n\npub fn Box.new[W](value W) Box[W] {\n\treturn Box[W]{\n\t\tvalue: value\n\t}\n}\n\npub fn (mut b Box[W]) begin() bool {\n\tb.count++\n\treturn true\n}\n\npub fn use_sink(mut sink Sink) bool {\n\treturn sink.begin()\n}\n'
		'user/user.v': 'module user\n\nimport sink\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tmut boxed := sink.Box.new(Local{\n\t\tn: 29\n\t})\n\tok := sink.use_sink(&boxed)\n\treturn if ok { 29 } else { 0 }\n}\n'
		'main.v':      'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '29'
}

fn test_generic_comptime_if_uses_interface_implementation() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_comptime_interface_impl', {
		'sink/sink.v': 'module sink\n\npub interface Writer {\nmut:\n\twrite(n int) int\n}\n\npub struct Counter[W] {\nmut:\n\twriter W\n}\n\npub fn Counter.new[W](writer W) Counter[W] {\n\treturn Counter[W]{\n\t\twriter: writer\n\t}\n}\n\npub fn (mut counter Counter[W]) write(n int) int {\n\t$if W is Writer {\n\t\treturn counter.writer.write(n)\n\t} $else {\n\t\treturn -1\n\t}\n}\n'
		'user/user.v': 'module user\n\nimport sink\n\nstruct LocalWriter implements sink.Writer {\nmut:\n\ttotal int\n}\n\nfn (mut w LocalWriter) write(n int) int {\n\tw.total += n\n\treturn w.total\n}\n\npub fn run() int {\n\tmut counter := sink.Counter.new(LocalWriter{})\n\treturn counter.write(41)\n}\n'
		'main.v':      'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '41'
}

fn test_interface_generated_generic_method_body_preserves_cross_module_arg_type() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_interface_nested_cross_module_arg', {
		'sink/sink.v': 'module sink\n\npub interface Sink {\nmut:\n\tbegin() int\n}\n\npub struct Values[^a] {\n\tn int\n}\n\npub struct Helper {}\n\npub fn (helper Helper) apply[^a, W](values Values[^a], mut writer W) int {\n\t_ = helper\n\t_ = values\n\t_ = writer\n\treturn 31\n}\n\npub struct Counter[W] {\nmut:\n\twriter W\n}\n\npub struct Summary[W] {\nmut:\n\tcounter Counter[W]\n}\n\npub fn Summary.new[W](writer W) Summary[W] {\n\treturn Summary[W]{\n\t\tcounter: Counter[W]{\n\t\t\twriter: writer\n\t\t}\n\t}\n}\n\npub struct Box[^a, W] {\nmut:\n\tsummary &^a Summary[W]\n\thelper Helper\n}\n\npub fn (mut b Box[^a, W]) begin[^a]() int {\n\tvalues := Values[^a]{\n\t\tn: 1\n\t}\n\treturn b.helper.apply(values, mut b.summary.counter)\n}\n\npub fn use_sink(mut sink Sink) int {\n\treturn sink.begin()\n}\n\npub fn use_summary[^a, W](summary &^a Summary[W]) int {\n\tmut boxed := Box[^a, W]{\n\t\tsummary: summary\n\t\thelper: Helper{}\n\t}\n\treturn use_sink(&boxed)\n}\n'
		'user/user.v': 'module user\n\nimport sink\n\nstruct LocalWriter {\n\tn int\n}\n\npub fn run() int {\n\tmut summary := sink.Summary.new(LocalWriter{\n\t\tn: 37\n\t})\n\treturn sink.use_summary(&summary)\n}\n'
		'main.v':      'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '31'
}

fn test_generated_generic_body_late_transforms_lifetime_helper() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_interface_late_lifetime_helper', {
		'sink/sink.v': "module sink\n\npub interface Sink {\nmut:\n\tbegin() int\n}\n\npub struct Link {\n\tn int\n}\n\npub struct Cache[^a] {\n\ttag &^a string\nmut:\n\tlink ?Link\n}\n\npub fn Cache.new[^a](tag &^a string) Cache[^a] {\n\treturn Cache[^a]{\n\t\ttag: tag\n\t}\n}\n\nfn marker_text() string {\n\treturn 'cache'\n}\n\nfn default_link_text() string {\n\treturn '\${marker_text()}'\n}\n\nfn default_link_n() int {\n\t_ := default_link_text()\n\treturn 43\n}\n\npub fn (mut cache Cache[^a]) fill[^a]() {\n\tcache.link = Link{\n\t\tn: default_link_n()\n\t}\n}\n\npub fn (mut cache Cache[^a]) get[^a]() ?&Link {\n\tif cache.link == none {\n\t\tcache.fill()\n\t}\n\treturn unsafe { &cache.link? }\n}\n\npub struct Box[W] {\n\tvalue W\n}\n\npub fn Box.new[W](value W) Box[W] {\n\treturn Box[W]{\n\t\tvalue: value\n\t}\n}\n\npub fn (mut b Box[W]) begin() int {\n\t_ = b\n\tlabel := 'cache'\n\tmut cache := Cache.new(&label)\n\tlink := cache.get() or { return 0 }\n\treturn link.n\n}\n\npub fn use_sink(mut sink Sink) int {\n\treturn sink.begin()\n}\n"
		'user/user.v': 'module user\n\nimport sink\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tmut boxed := sink.Box.new(Local{\n\t\tn: 5\n\t})\n\treturn sink.use_sink(&boxed)\n}\n'
		'main.v':      'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '43'
}

fn test_specialized_generic_mut_receiver_body_uses_emitted_method_name() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_specialized_mut_receiver_body_name', {
		'printer/printer.v': 'module printer\n\npub struct Impl[W] {\n\tvalue W\nmut:\n\tcount int\n}\n\npub fn Impl.new[W](value W) Impl[W] {\n\treturn Impl[W]{\n\t\tvalue: value\n\t}\n}\n\npub fn (mut imp Impl[W]) run() int {\n\timp.bump()\n\treturn imp.count\n}\n\nfn (mut imp Impl[W]) bump() {\n\timp.count += 47\n}\n'
		'user/user.v':       'module user\n\nimport printer\n\nstruct BufferWriter {}\n\npub fn run() int {\n\tmut imp := printer.Impl.new(BufferWriter{})\n\treturn imp.run()\n}\n'
		'main.v':            'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '47'
}

fn test_lifetime_receiver_method_body_specializes_nested_methods() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_lifetime_receiver_nested_methods', {
		'printer/printer.v': 'module printer\n\npub struct Sink[^a, W] {\n\ttag &^a string\n\tvalue W\nmut:\n\tcount int\n}\n\npub fn Sink.new[^a, W](tag &^a string, value W) Sink[^a, W] {\n\treturn Sink[^a, W]{\n\t\ttag: tag\n\t\tvalue: value\n\t}\n}\n\npub fn (mut sink Sink[^a, W]) run[^a]() !int {\n\tsink.bump()!\n\tif sink.active() {\n\t\tsink.bump_more()!\n\t\treturn sink.count\n\t}\n\treturn 0\n}\n\nfn (mut sink Sink[^a, W]) bump[^a]() ! {\n\tsink.count += 50\n}\n\nfn (mut sink Sink[^a, W]) bump_more[^a]() ! {\n\tsink.count += 3\n}\n\nfn (sink Sink[^a, W]) active[^a]() bool {\n\treturn sink.tag.len > 0\n}\n'
		'user/user.v':       "module user\n\nimport printer\n\nstruct BufferWriter {}\n\npub fn run() int {\n\tlabel := 'x'\n\tmut sink := printer.Sink.new(&label, BufferWriter{})\n\treturn sink.run() or { 0 }\n}\n"
		'main.v':            'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '53'
}

fn test_lifetime_only_method_body_specializes_generic_receiver_methods() {
	v3_bin := generic_cross_build_v3()
	out := generic_cross_run_project(v3_bin, 'generic_lifetime_only_method_body', {
		'ignore/ignore.v': 'module ignore\n\npub struct Ref[^a] {\n\tn int\n}\n\npub struct Match[T] {\n\tvalue T\n\thas_value bool\n}\n\npub fn (m Match[T]) is_none() bool {\n\treturn !m.has_value\n}\n\npub fn (m Match[T]) inner() ?T {\n\tif !m.has_value {\n\t\treturn none\n\t}\n\treturn m.value\n}\n\nstruct Matcher {}\n\nfn (matcher &^a Matcher) matched[^a](n int) Match[Ref[^a]] {\n\t_ = matcher\n\tif n == 0 {\n\t\treturn Match[Ref[^a]]{}\n\t}\n\treturn Match[Ref[^a]]{\n\t\tvalue: Ref[^a]{\n\t\t\tn: n\n\t\t}\n\t\thas_value: true\n\t}\n}\n\nstruct Override {\n\tmatcher Matcher\n}\n\nfn (o &^a Override) matched[^a](n int) int {\n\tmat := o.matcher.matched(n)\n\tif mat.is_none() {\n\t\treturn 1\n\t}\n\tif value := mat.inner() {\n\t\treturn value.n\n\t}\n\treturn 0\n}\n\npub fn run() int {\n\toverride := Override{\n\t\tmatcher: Matcher{}\n\t}\n\treturn override.matched(59)\n}\n'
		'main.v':          'module main\n\nimport ignore\n\nfn main() {\n\tprintln(int_str(ignore.run()))\n}\n'
	})
	assert out == '59'
}
