module os

// clearenv removes all environment variables. Mirrors Go's os.Clearenv.
pub fn clearenv() {
	for key, _ in environ() {
		unsetenv(key)
	}
}
