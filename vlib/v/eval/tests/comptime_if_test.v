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
	\$else { "unknown" }')!
	dump(ret)
	assert ret[0].string().len != 0
	assert ret[0].string() != 'unknown'
}
