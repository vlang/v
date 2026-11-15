module os

// clearenv removes all environment variables from the current process.
// It is safe to call repeatedly, including when the environment is already empty.
pub fn clearenv() {
	for key, _ in environ() {
		unsetenv(key)
	}
}
