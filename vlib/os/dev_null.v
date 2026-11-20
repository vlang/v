module os

$if windows {
	pub const dev_null = 'NUL'
} $else {
	pub const dev_null = '/dev/null'
}
