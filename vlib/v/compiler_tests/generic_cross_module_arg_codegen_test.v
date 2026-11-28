import os
import v.tempname

fn generic_cross_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn generic_cross_project_dir(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	return root
}

fn generic_cross_compile(root string) string {
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	return bin
}

fn generic_cross_run_project(name string, files map[string]string) string {
	root := generic_cross_project_dir(name)
	defer { os.rmdir_all(root) or {} }
	for rel, source in files {
		generic_cross_write_file(root, rel, source)
	}
	bin := generic_cross_compile(root)
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

struct GenericCrossFixture {
	name     string
	files    map[string]string
	expected string
}

fn generic_cross_check_batch(name string, cases []GenericCrossFixture) {
	root := generic_cross_project_dir(name)
	defer { os.rmdir_all(root) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		// Each case has its own module tree, including its dependencies.
		for rel, source in case.files {
			generic_cross_write_file(root, os.join_path(case.name, rel), source)
		}
		imports << 'import ${case.name}'
		calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
	}
	generic_cross_write_file(root, 'main.v', "module main\n\n${imports.join('\n')}\n\nfn main() {\n\tmatch arguments()[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n")
	bin := generic_cross_compile(root)
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn test_generic_builders_and_receivers() {
	generic_cross_check_batch('generic_builders_and_receivers', [
		GenericCrossFixture{
			name:     'signature'
			files:    {
				'wrap/wrap.v': 'module wrap\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n'
				'user/user.v': 'module user\n\nimport signature.wrap\n\nstruct Local {\n\tn int\n}\n\nstruct Holder[T] {\n\tbox wrap.Box[T]\n}\n\nfn Holder.make[T](box wrap.Box[T]) Holder[T] {\n\treturn Holder[T]{\n\t\tbox: box\n\t}\n}\n\nfn take(box wrap.Box[Local]) int {\n\treturn box.value.n\n}\n\npub fn run() int {\n\tbox := wrap.Box[Local]{\n\t\tvalue: Local{\n\t\t\tn: 7\n\t\t}\n\t}\n\tholder := Holder.make(box)\n\treturn take(holder.box)\n}\n'
				'case.v':      'module signature\n\nimport signature.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '7'
		},
		GenericCrossFixture{
			name:     'nested_return'
			files:    {
				'wrap/wrap.v': 'module wrap\n\npub struct Box[T] {\npub:\n\tvalue T\n}\n\npub fn Box.new[T](value T) Box[T] {\n\treturn Box[T]{\n\t\tvalue: value\n\t}\n}\n'
				'user/user.v': 'module user\n\nimport nested_return.wrap\n\nstruct Local {\n\tn int\n}\n\nstruct Holder[T] {\n\tbox wrap.Box[T]\n}\n\nfn Holder.from[T](box wrap.Box[T]) Holder[T] {\n\treturn Holder[T]{\n\t\tbox: box\n\t}\n}\n\npub fn run() int {\n\tholder := Holder.from(wrap.Box.new(Local{\n\t\tn: 9\n\t}))\n\treturn holder.box.value.n\n}\n'
				'case.v':      'module nested_return\n\nimport nested_return.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '9'
		},
		GenericCrossFixture{
			name:     'nested_receiver'
			files:    {
				'printer/printer.v': 'module printer\n\npub struct Standard[W] {\npub:\n\tw W\n}\n\npub struct Sink[W] {\npub:\n\tw W\n}\n\npub fn Standard.new[W](w W) Standard[W] {\n\treturn Standard[W]{\n\t\tw: w\n\t}\n}\n\npub fn (standard Standard[W]) sink() Sink[W] {\n\treturn Sink[W]{\n\t\tw: standard.w\n\t}\n}\n\npub fn (sink Sink[W]) value() W {\n\treturn sink.w\n}\n'
				'user/user.v':       'module user\n\nimport nested_receiver.printer\n\nstruct Local {\n\tn int\n}\n\nstruct Worker[W] {\n\tstandard printer.Standard[W]\n}\n\nfn (worker Worker[W]) run() W {\n\tsink := worker.standard.sink()\n\treturn sink.value()\n}\n\npub fn run() int {\n\tworker := Worker[Local]{\n\t\tstandard: printer.Standard.new(Local{\n\t\t\tn: 11\n\t\t})\n\t}\n\treturn worker.run().n\n}\n'
				'case.v':            'module nested_receiver\n\nimport nested_receiver.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '11'
		},
		GenericCrossFixture{
			name:     'receiver_offset'
			files:    {
				'printer/printer.v': 'module printer\n\npub struct Matcher {\npub:\n\tn int\n}\n\npub fn (m Matcher) clone() Matcher {\n\treturn m\n}\n\npub struct Standard[W] {\npub:\n\tw W\n}\n\npub struct Sink[W] {\npub:\n\tw W\n\tn int\n}\n\npub fn Standard.new[W](w W) Standard[W] {\n\treturn Standard[W]{\n\t\tw: w\n\t}\n}\n\npub fn (mut standard Standard[W]) sink_with_matcher(matcher Matcher) Sink[W] {\n\treturn Sink[W]{\n\t\tw: standard.w\n\t\tn: matcher.n\n\t}\n}\n\npub fn (sink Sink[W]) value() W {\n\treturn sink.w\n}\n\npub fn (sink Sink[W]) count() int {\n\treturn sink.n\n}\n'
				'user/user.v':       'module user\n\nimport receiver_offset.printer\n\nstruct Local {\n\tn int\n}\n\nstruct Worker[W] {\nmut:\n\tstandard printer.Standard[W]\n}\n\nfn (mut worker Worker[W]) run(matcher printer.Matcher) int {\n\tsink := worker.standard.sink_with_matcher(matcher.clone())\n\treturn sink.value().n + sink.count()\n}\n\npub fn run() int {\n\tmut worker := Worker[Local]{\n\t\tstandard: printer.Standard.new(Local{\n\t\t\tn: 13\n\t\t})\n\t}\n\treturn worker.run(printer.Matcher{\n\t\tn: 5\n\t})\n}\n'
				'case.v':            'module receiver_offset\n\nimport receiver_offset.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '18'
		},
		GenericCrossFixture{
			name:     'multiline_builder'
			files:    {
				'worker/worker.v': 'module worker\n\npub struct Matcher {\npub:\n\tn int\n}\n\npub struct Searcher {\npub:\n\tn int\n}\n\npub fn Searcher.new() Searcher {\n\treturn Searcher{\n\t\tn: 3\n\t}\n}\n\npub struct Printer[W] {\npub:\n\tw W\n}\n\npub fn Printer.new[W](w W) Printer[W] {\n\treturn Printer[W]{\n\t\tw: w\n\t}\n}\n\npub struct Worker[W] {\npub:\n\tmatcher Matcher\n\tsearcher Searcher\n\tprinter Printer[W]\n}\n\npub struct Builder {}\n\npub fn Builder.new() Builder {\n\treturn Builder{}\n}\n\npub fn (builder Builder) build[W](matcher Matcher, searcher Searcher, printer Printer[W]) Worker[W] {\n\t_ = builder\n\treturn Worker[W]{\n\t\tmatcher: matcher\n\t\tsearcher: searcher\n\t\tprinter: printer\n\t}\n}\n'
				'user/user.v':     'module user\n\nimport multiline_builder.worker\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tbuilder := worker.Builder.new()\n\tprinter := worker.Printer.new(Local{\n\t\tn: 17\n\t})\n\tmut w := builder.build(worker.Matcher{\n\t\tn: 2\n\t}, worker.Searcher.new(),\n\t\tprinter)\n\treturn w.matcher.n + w.searcher.n + w.printer.w.n\n}\n'
				'case.v':          'module multiline_builder\n\nimport multiline_builder.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '22'
		},
		GenericCrossFixture{
			name:     'wrapped_builder'
			files:    {
				'worker/worker.v': 'module worker\n\npub struct Matcher {\npub:\n\tn int\n}\n\npub struct Searcher {\npub:\n\tn int\n}\n\npub fn Searcher.new() Searcher {\n\treturn Searcher{\n\t\tn: 3\n\t}\n}\n\npub struct Summary[W] {\npub:\n\tw W\n}\n\npub fn Summary.new[W](w W) Summary[W] {\n\treturn Summary[W]{\n\t\tw: w\n\t}\n}\n\npub enum PrinterKind {\n\tsummary\n}\n\npub struct Printer[W] {\npub:\n\tkind PrinterKind\n\tsummary Summary[W]\n}\n\npub fn Printer.summary[W](summary Summary[W]) Printer[W] {\n\treturn Printer[W]{\n\t\tkind: .summary\n\t\tsummary: summary\n\t}\n}\n\npub struct Worker[W] {\npub:\n\tmatcher Matcher\n\tsearcher Searcher\n\tprinter Printer[W]\n}\n\npub struct Builder {}\n\npub fn Builder.new() Builder {\n\treturn Builder{}\n}\n\npub fn (builder Builder) build[W](matcher Matcher, searcher Searcher, printer Printer[W]) Worker[W] {\n\t_ = builder\n\treturn Worker[W]{\n\t\tmatcher: matcher\n\t\tsearcher: searcher\n\t\tprinter: printer\n\t}\n}\n'
				'user/user.v':     'module user\n\nimport wrapped_builder.worker\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tbuilder := worker.Builder.new()\n\tsummary_printer := worker.Printer.summary(worker.Summary.new(Local{\n\t\tn: 19\n\t}))\n\tmut w := builder.build(worker.Matcher{\n\t\tn: 2\n\t}, worker.Searcher.new(),\n\t\tsummary_printer)\n\treturn w.matcher.n + w.searcher.n + w.printer.summary.w.n\n}\n'
				'case.v':          'module wrapped_builder\n\nimport wrapped_builder.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '24'
		},
		GenericCrossFixture{
			name:     'builtin_builder'
			files:    {
				'worker/worker.v': 'module worker\n\npub struct Summary[W] {\npub:\n\tw W\n}\n\npub fn Summary.new[W](w W) Summary[W] {\n\treturn Summary[W]{\n\t\tw: w\n\t}\n}\n\npub enum PrinterKind {\n\tsummary\n}\n\npub struct Printer[W] {\npub:\n\tkind PrinterKind\n\tsummary Summary[W]\n}\n\npub fn Printer.summary[W](summary Summary[W]) Printer[W] {\n\treturn Printer[W]{\n\t\tkind: .summary\n\t\tsummary: summary\n\t}\n}\n\npub struct Worker[W] {\npub:\n\tprinter Printer[W]\n}\n\npub struct Builder {}\n\npub fn Builder.new() Builder {\n\treturn Builder{}\n}\n\npub fn (builder Builder) build[W](printer Printer[W]) Worker[W] {\n\t_ = builder\n\treturn Worker[W]{\n\t\tprinter: printer\n\t}\n}\n'
				'user/user.v':     'module user\n\nimport builtin_builder.worker\n\npub fn run() int {\n\tbuilder := worker.Builder.new()\n\tprinter := worker.Printer.summary(worker.Summary.new(7))\n\tw := builder.build(printer)\n\treturn w.printer.summary.w\n}\n'
				'case.v':          'module builtin_builder\n\nimport builtin_builder.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '7'
		},
		GenericCrossFixture{
			name:     'imported_builder'
			files:    {
				'printer/printer.v': 'module printer\n\npub struct Standard[W] {\npub:\n\tw W\n}\n\npub fn Standard.new[W](w W) Standard[W] {\n\treturn Standard[W]{\n\t\tw: w\n\t}\n}\n'
				'core/core.v':       'module core\n\nimport imported_builder.printer\n\nstruct BufferWriter {\n\tn int\n}\n\nstruct Printer[W] {\n\tstandard printer.Standard[W]\n}\n\nfn Printer.standard[W](standard printer.Standard[W]) Printer[W] {\n\treturn Printer[W]{\n\t\tstandard: standard\n\t}\n}\n\npub fn run() int {\n\tstandard := printer.Standard.new(BufferWriter{\n\t\tn: 61\n\t})\n\tprinter_ := Printer.standard(standard)\n\treturn printer_.standard.w.n\n}\n'
				'case.v':            'module imported_builder\n\nimport imported_builder.core\n\npub fn exercise() {\n\tprintln(int_str(core.run()))\n}\n'
			}
			expected: '61'
		},
		GenericCrossFixture{
			name:     'late_helper'
			files:    {
				'sink/sink.v': "module sink\n\npub interface Sink {\nmut:\n\tbegin() int\n}\n\npub struct Link {\n\tn int\n}\n\npub struct Cache[^a] {\n\ttag &^a string\nmut:\n\tlink ?Link\n}\n\npub fn Cache.new[^a](tag &^a string) Cache[^a] {\n\treturn Cache[^a]{\n\t\ttag: tag\n\t}\n}\n\nfn marker_text() string {\n\treturn 'cache'\n}\n\nfn default_link_text() string {\n\treturn '\${marker_text()}'\n}\n\nfn default_link_n() int {\n\t_ := default_link_text()\n\treturn 43\n}\n\npub fn (mut cache Cache[^a]) fill[^a]() {\n\tcache.link = Link{\n\t\tn: default_link_n()\n\t}\n}\n\npub fn (mut cache Cache[^a]) get[^a]() ?&Link {\n\tif cache.link == none {\n\t\tcache.fill()\n\t}\n\treturn unsafe { &cache.link? }\n}\n\npub struct Box[W] {\n\tvalue W\n}\n\npub fn Box.new[W](value W) Box[W] {\n\treturn Box[W]{\n\t\tvalue: value\n\t}\n}\n\npub fn (mut b Box[W]) begin() int {\n\t_ = b\n\tlabel := 'cache'\n\tmut cache := Cache.new(&label)\n\tlink := cache.get() or { return 0 }\n\treturn link.n\n}\n\npub fn use_sink(mut value Sink) int {\n\treturn value.begin()\n}\n"
				'user/user.v': 'module user\n\nimport late_helper.sink\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tmut boxed := sink.Box.new(Local{\n\t\tn: 5\n\t})\n\treturn sink.use_sink(mut boxed)\n}\n'
				'case.v':      'module late_helper\n\nimport late_helper.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '43'
		},
		GenericCrossFixture{
			name:     'pointer_callback'
			files:    {
				'case.v': "module pointer_callback\n\n@[heap]\nstruct Dog {\n\tname string\n}\n\nstruct Box[T] {\n\titem T\n}\n\nfn apply[T](b Box[T], f fn (mut it T)) T {\n\tmut it := b.item\n\tf(mut it)\n\treturn it\n}\n\npub fn exercise() {\n\tfirst := &Dog{\n\t\tname: 'first'\n\t}\n\tsecond := &Dog{\n\t\tname: 'second'\n\t}\n\tbox := Box[&Dog]{\n\t\titem: first\n\t}\n\tresult := apply[&Dog](box, fn [second] (mut it &Dog) {\n\t\tit = second\n\t})\n\tprintln(result.name)\n}\n"
			}
			expected: 'second'
		},
		GenericCrossFixture{
			name:     'value_callback'
			files:    {
				'case.v': "module value_callback\n\nstruct Dog {\nmut:\n\tname string\n}\n\nstruct Box[T] {\n\titem T\n}\n\nfn apply[T](b Box[T], f fn (mut it T)) T {\n\tmut it := b.item\n\tf(mut it)\n\treturn it\n}\n\npub fn exercise() {\n\tbox := Box[Dog]{\n\t\titem: Dog{\n\t\t\tname: 'first'\n\t\t}\n\t}\n\tresult := apply[Dog](box, fn (mut it Dog) {\n\t\tit.name = 'second'\n\t})\n\tprintln(result.name)\n}\n"
			}
			expected: 'second'
		},
		GenericCrossFixture{
			name:     'mut_receiver'
			files:    {
				'printer/printer.v': 'module printer\n\npub struct Impl[W] {\n\tvalue W\nmut:\n\tcount int\n}\n\npub fn Impl.new[W](value W) Impl[W] {\n\treturn Impl[W]{\n\t\tvalue: value\n\t}\n}\n\npub fn (mut imp Impl[W]) run() int {\n\timp.bump()\n\treturn imp.count\n}\n\nfn (mut imp Impl[W]) bump() {\n\timp.count += 47\n}\n'
				'user/user.v':       'module user\n\nimport mut_receiver.printer\n\nstruct BufferWriter {}\n\npub fn run() int {\n\tmut imp := printer.Impl.new(BufferWriter{})\n\treturn imp.run()\n}\n'
				'case.v':            'module mut_receiver\n\nimport mut_receiver.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '47'
		},
	])
}

fn test_imported_generic_application_arg_keeps_caller_base_module() {
	out := generic_cross_run_project('generic_cross_module_application_arg', {
		'wrap/wrap.v': 'module wrap\n\npub struct Wrapper[T] {\npub:\n\tvalues []T\n}\n\npub fn make[T]() Wrapper[T] {\n\treturn Wrapper[T]{}\n}\n'
		'main.v':      'module main\n\nimport wrap\n\nstruct User {\n\tn int\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nfn main() {\n\twrapped := wrap.make[Box[User]]()\n\tprintln(int_str(wrapped.values.len))\n}\n'
	})
	assert out == '0'
}

fn test_static_generic_call_uses_declared_module_name_for_nested_import_path() {
	out := generic_cross_run_project('generic_static_nested_import_path', {
		'vsl/la/matrix.v': 'module la\n\npub struct Matrix[T] {\npub:\n\tn int\n}\n\npub fn Matrix.new[T](n int) Matrix[T] {\n\treturn Matrix[T]{\n\t\tn: n\n\t}\n}\n'
		'user/user.v':     'module user\n\nimport vsl.la\n\npub fn run() int {\n\tmatrix := la.Matrix.new[f64](37)\n\treturn matrix.n\n}\n'
		'main.v':          'module main\n\nimport user\n\nfn main() {\n\tprintln(int_str(user.run()))\n}\n'
	})
	assert out == '37'
}

fn test_generic_interface_result_call_keeps_specialized_symbol_and_wrapper() {
	out := generic_cross_run_project('generic_interface_result_call', {
		'store/store.v': 'module store\n\npub interface Store[T] {\nmut:\n\tget() !T\n}\n\npub struct Sessions[T] {\npub mut:\n\tstore Store[T]\n}\n\npub fn (mut sessions Sessions[T]) get[X](_ X) !T {\n\treturn sessions.store.get()!\n}\n'
		'main.v':        'module main\n\nimport store\n\nstruct User {\n\tname string\n}\n\nstruct MemoryStore {}\n\nfn (mut memory MemoryStore) get() !User {\n\t_ = memory\n\treturn User{\n\t\tname: "ok"\n\t}\n}\n\nfn main() {\n\tmut sessions := store.Sessions[User]{\n\t\tstore: MemoryStore{}\n\t}\n\tprintln(sessions.get(0)!.name)\n}\n'
	})
	assert out == 'ok'
}

fn test_generic_interfaces_and_lifetimes() {
	generic_cross_check_batch('generic_interfaces_and_lifetimes', [
		GenericCrossFixture{
			name:     'comptime_iface'
			files:    {
				'sink/sink.v': 'module sink\n\npub interface Writer {\nmut:\n\twrite(n int) int\n}\n\npub struct Counter[W] {\nmut:\n\twriter W\n}\n\npub fn Counter.new[W](writer W) Counter[W] {\n\treturn Counter[W]{\n\t\twriter: writer\n\t}\n}\n\npub fn (mut counter Counter[W]) write(n int) int {\n\t\$if W is Writer {\n\t\treturn counter.writer.write(n)\n\t} \$else {\n\t\treturn -1\n\t}\n}\n'
				'user/user.v': 'module user\n\nimport comptime_iface.sink\n\nstruct LocalWriter implements sink.Writer {\nmut:\n\ttotal int\n}\n\nfn (mut w LocalWriter) write(n int) int {\n\tw.total += n\n\treturn w.total\n}\n\npub fn run() int {\n\tmut counter := sink.Counter.new(LocalWriter{})\n\treturn counter.write(41)\n}\n'
				'case.v':      'module comptime_iface\n\nimport comptime_iface.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '41'
		},
		GenericCrossFixture{
			name:     'nested_iface'
			files:    {
				'sink/sink.v': 'module sink\n\npub interface Sink {\nmut:\n\tbegin() int\n}\n\npub struct Values[^a] {\n\tn int\n}\n\npub struct Helper {}\n\npub fn (helper Helper) apply[^a, W](values Values[^a], mut writer W) int {\n\t_ = helper\n\t_ = values\n\t_ = writer\n\treturn 31\n}\n\npub struct Counter[W] {\nmut:\n\twriter W\n}\n\npub struct Summary[W] {\nmut:\n\tcounter Counter[W]\n}\n\npub fn Summary.new[W](writer W) Summary[W] {\n\treturn Summary[W]{\n\t\tcounter: Counter[W]{\n\t\t\twriter: writer\n\t\t}\n\t}\n}\n\npub struct Box[^a, W] {\nmut:\n\tsummary &^a Summary[W]\n\thelper Helper\n}\n\npub fn (mut b Box[^a, W]) begin[^a]() int {\n\tvalues := Values[^a]{\n\t\tn: 1\n\t}\n\treturn b.helper.apply(values, mut b.summary.counter)\n}\n\npub fn use_sink(mut value Sink) int {\n\treturn value.begin()\n}\n\npub fn use_summary[^a, W](summary &^a Summary[W]) int {\n\tmut boxed := Box[^a, W]{\n\t\tsummary: summary\n\t\thelper: Helper{}\n\t}\n\treturn use_sink(mut boxed)\n}\n'
				'user/user.v': 'module user\n\nimport nested_iface.sink\n\nstruct LocalWriter {\n\tn int\n}\n\npub fn run() int {\n\tmut summary := sink.Summary.new(LocalWriter{\n\t\tn: 37\n\t})\n\treturn sink.use_summary(&summary)\n}\n'
				'case.v':      'module nested_iface\n\nimport nested_iface.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '31'
		},
		GenericCrossFixture{
			name:     'lifetime_nested'
			files:    {
				'printer/printer.v': 'module printer\n\npub struct Sink[^a, W] {\n\ttag &^a string\n\tvalue W\nmut:\n\tcount int\n}\n\npub fn Sink.new[^a, W](tag &^a string, value W) Sink[^a, W] {\n\treturn Sink[^a, W]{\n\t\ttag: tag\n\t\tvalue: value\n\t}\n}\n\npub fn (mut sink Sink[^a, W]) run[^a]() !int {\n\tsink.bump()!\n\tif sink.active() {\n\t\tsink.bump_more()!\n\t\treturn sink.count\n\t}\n\treturn 0\n}\n\nfn (mut sink Sink[^a, W]) bump[^a]() ! {\n\tsink.count += 50\n}\n\nfn (mut sink Sink[^a, W]) bump_more[^a]() ! {\n\tsink.count += 3\n}\n\nfn (sink Sink[^a, W]) active[^a]() bool {\n\treturn sink.tag.len > 0\n}\n'
				'user/user.v':       "module user\n\nimport lifetime_nested.printer\n\nstruct BufferWriter {}\n\npub fn run() int {\n\tlabel := 'x'\n\tmut sink := printer.Sink.new(&label, BufferWriter{})\n\treturn sink.run() or { 0 }\n}\n"
				'case.v':            'module lifetime_nested\n\nimport lifetime_nested.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '53'
		},
		GenericCrossFixture{
			name:     'lifetime_only'
			files:    {
				'ignore/ignore.v': 'module ignore\n\npub struct Ref[^a] {\n\tn int\n}\n\npub struct Match[T] {\n\tvalue T\n\thas_value bool\n}\n\npub fn (m Match[T]) is_none() bool {\n\treturn !m.has_value\n}\n\npub fn (m Match[T]) inner() ?T {\n\tif !m.has_value {\n\t\treturn none\n\t}\n\treturn m.value\n}\n\nstruct Matcher {}\n\nfn (matcher &^a Matcher) matched[^a](n int) Match[Ref[^a]] {\n\t_ = matcher\n\tif n == 0 {\n\t\treturn Match[Ref[^a]]{}\n\t}\n\treturn Match[Ref[^a]]{\n\t\tvalue: Ref[^a]{\n\t\t\tn: n\n\t\t}\n\t\thas_value: true\n\t}\n}\n\nstruct Override {\n\tmatcher Matcher\n}\n\nfn (o &^a Override) matched[^a](n int) int {\n\tmat := o.matcher.matched(n)\n\tif mat.is_none() {\n\t\treturn 1\n\t}\n\tif value := mat.inner() {\n\t\treturn value.n\n\t}\n\treturn 0\n}\n\npub fn run() int {\n\toverride := Override{\n\t\tmatcher: Matcher{}\n\t}\n\treturn override.matched(59)\n}\n'
				'case.v':          'module lifetime_only\n\nimport lifetime_only.ignore\n\npub fn exercise() {\n\tprintln(int_str(ignore.run()))\n}\n'
			}
			expected: '59'
		},
		// The `mut` interface parameter of `use_sink` is named `value`: a parameter may not
		// share the name of its module, and the argument for it is passed with `mut`.
		GenericCrossFixture{
			name:     'bool_dispatch'
			files:    {
				'sink/sink.v': 'module sink\n\npub interface Sink {\nmut:\n\tbegin() bool\n}\n\npub struct Box[W] {\nmut:\n\tvalue W\n\tcount int\n}\n\npub fn Box.new[W](value W) Box[W] {\n\treturn Box[W]{\n\t\tvalue: value\n\t}\n}\n\npub fn (mut b Box[W]) begin() bool {\n\tb.count++\n\treturn true\n}\n\npub fn use_sink(mut value Sink) bool {\n\treturn value.begin()\n}\n'
				'user/user.v': 'module user\n\nimport bool_dispatch.sink\n\nstruct Local {\n\tn int\n}\n\npub fn run() int {\n\tmut boxed := sink.Box.new(Local{\n\t\tn: 29\n\t})\n\tok := sink.use_sink(mut boxed)\n\treturn if ok { 29 } else { 0 }\n}\n'
				'case.v':      'module bool_dispatch\n\nimport bool_dispatch.user\n\npub fn exercise() {\n\tprintln(int_str(user.run()))\n}\n'
			}
			expected: '29'
		},
	])
}

fn test_generic_receiver_boxes_lifetime_struct_for_interface_dispatch() {
	out := generic_cross_run_project('generic_receiver_lifetime_interface_dispatch', {
		'sink/sink.v': "module sink\n\npub interface Sink {\nmut:\n\tbegin() !bool\n\tmatched[^b](event &Event[^b]) !bool\n}\n\npub struct Event[^b] {\npub:\n\ttext &^b string\n}\n\npub struct Box[^p, ^s, W] {\n\tpath &^p string\n\ttag &^s int\n\tvalue W\nmut:\n\tcount int\n}\n\npub fn (mut b Box[^p, ^s, W]) begin[^p, ^s]() !bool {\n\tb.count++\n\treturn true\n}\n\npub fn (mut b Box[^p, ^s, W]) matched[^b, ^p, ^s](event &Event[^b]) !bool {\n\tb.count += event.text.len\n\treturn true\n}\n\npub fn use_sink(mut value Sink) !int {\n\tif !value.begin()! {\n\t\treturn 0\n\t}\n\ttext := 'ok'\n\tevent := Event{\n\t\ttext: &text\n\t}\n\tif !value.matched(&event)! {\n\t\treturn 0\n\t}\n\treturn 43\n}\n\npub struct Driver[W] {\npub:\n\tvalue W\n}\n\npub fn (mut driver Driver[W]) run[^p, ^s](path &^p string, tag &^s int) !int {\n\tmut boxed := Box[^p, ^s, W]{\n\t\tpath: path\n\t\ttag: tag\n\t\tvalue: driver.value\n\t}\n\treturn use_sink(&boxed)\n}\n"
		'main.v':      "module main\n\nimport sink\n\nstruct Local {}\n\nfn main() {\n\tpath := 'x'\n\ttag := 1\n\tmut driver := sink.Driver[Local]{\n\t\tvalue: Local{}\n\t}\n\tprintln(int_str(driver.run(&path, &tag) or { 0 }))\n}\n"
	})
	assert out == '43'
}
