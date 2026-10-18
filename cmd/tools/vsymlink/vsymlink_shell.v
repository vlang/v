// shell_shim_content renders the POSIX shell shim installed next to `v.bat`
// when a native symlink cannot be created. A `.bat` file is invisible to POSIX
// shells: Git Bash, MSYS2 and Cygwin resolve `v` through `env`, which cannot
// execute it, so anything with a `#!/usr/bin/env -S v ...` shebang - including
// the pre-commit hook installed by `v git-fmt-hook install` - fails there with
// `env: 'v': No such file or directory`. The shim forwards to the same
// executable, so `v` resolves in those shells too. The path uses forward
// slashes, which Git Bash, MSYS2 and Cygwin all accept, including the `C:/...`
// drive form. Quote for the target POSIX shell, independently of the host's
// command-line quoting rules.
fn shell_shim_content(vexe_path string) string {
	path := vexe_path.replace('\\', '/').replace("'", '\'"\'"\'')
	return '#!/bin/sh\nexec \'${path}\' "$@"\n'
}
