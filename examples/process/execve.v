import os

// NOTE: `execve` executes a new child process, in place of the current process.
// Therefore, only the topmost example will be executed when it's not commented out.
fn main() {
	// Passes only an array of args.
	os.execve(os.find_abs_path_of_executable('ls')!, ['-lh', '-s'], [])!

	// Considers args that would need to be passed within quotes. E.g.: `bash -c "ls -lh"`.
	os.execve(os.find_abs_path_of_executable('bash')!, ['-c', 'ls -lah -s'], [])!

	// Passes an environment variable that affects the commands output.
	os.execve(os.find_abs_path_of_executable('man')!, ['true'], ['MANWIDTH=60'])!
}
