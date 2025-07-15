const arch = $if amd64 {
	'amd64'
} $else $if i386 {
	'i386'
}
//$else $if aarch64 {'aarch64'}
$else $if arm64 {
	'arm64'
} $else $if arm32 {
	'arm32'
} $else $if rv64 {
	'rv64'
} $else $if rv32 {
	'rv32'
} $else $if s390x {
	's390x'
} $else $if ppc64le {
	'ppc64le'
} $else $if loongarch64 {
	'loongarch64'
} $else {
	'unknown'
}

fn test_main() {
	println('arch is ${arch}')
	assert arch != 'unknown'
}
