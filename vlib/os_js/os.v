module os_js

pub const (
	// todo(playX): NodeJS does not seem to have any path limit?
	max_path_len = 4096
)

pub struct Result {
pub:
	exit_code int
	output    string
	// stderr string // TODO
}
