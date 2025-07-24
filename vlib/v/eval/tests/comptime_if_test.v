import v.eval

fn test_comptime_if() {
	mut e := eval.create()

	ret := e.run('const a = 
	\$if amd64 { "amd" } 
	\$else \$if i386        { "i386" } 
	\$else \$if aarch64     { "aarch64" } 
	\$else \$if arm64       { "arm64" } 
	\$else \$if arm32       { "arm32" } 
	\$else \$if rv64        { "rv64" } 
	\$else \$if rv32        { "rv32" } 
	\$else \$if s390x       { "s390x" } 
	\$else \$if ppc64le     { "ppc64le" } 
	\$else \$if loongarch64 { "loongarch64" } 
	\$else { "unknown" }

	const b = 1.5

	fn display() (string,f64) { println(a) println(b) return a,b } display()')!

	dump(ret)
	assert ret[0].string().len != 0
	assert ret[0].string() != 'unknown'
	assert ret[1].float_val() == 1.5
}

fn test_comptime_if_without_func() {
	mut e := eval.create()

	ret := e.run('const a =
	\$if amd64 { "amd" }
	\$else \$if i386        { "i386" }
	\$else \$if aarch64     { "aarch64" }
	\$else \$if arm64       { "arm64" }
	\$else \$if arm32       { "arm32" }
	\$else \$if rv64        { "rv64" }
	\$else \$if rv32        { "rv32" }
	\$else \$if s390x       { "s390x" }
	\$else \$if ppc64le     { "ppc64le" }
	\$else \$if loongarch64 { "loongarch64" }
	\$else { "unknown" }

	const b = 1.5')!

	dump(ret)
	assert ret == []
}

fn test_comptime_if_infix() {
	mut e := eval.create()

	ret := e.run('const a =
	\$if amd64 || aarch64 || arm64 || rv64 { "64bit" }
	\$else \$if i386 || arm32 || rv32 { "32bit" }
	\$else \$if s390x       { "s390x" }
	\$else \$if ppc64le     { "ppc64le" }
	\$else \$if loongarch64 { "loongarch64" }
	\$else { "unknown" }

	const b = 1.5

	fn display() (string,f64) { println(a) println(b) return a,b } display()')!

	dump(ret)
	assert ret[0].string().len != 0
	assert ret[0].string() != 'unknown'
	assert ret[1].float_val() == 1.5
}
