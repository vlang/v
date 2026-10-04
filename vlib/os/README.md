## Description

`os` provides common OS/platform independent functions for accessing
command line arguments, reading/writing files, listing folders,
handling processes etc.

On Windows, `os.data_dir()` uses `%LocalAppData%` for user-specific
application data.

### Console input

On Windows, `os.input()` supports both console and redirected standard input.
It returns an empty string when the standard input handle is invalid.

### Path helpers

`os.dir()` returns everything before the last separator, matching the classic
`dirname` behaviour. It is not a "go up one level" primitive: on Windows it
answers `.` for `C:` and the bare volume `C:` for `C:\dir`, and both of those
name a *current* directory rather than a location in the given path.

`os.parent_dir()` is the walking variant. Every value it returns is safe to
probe directly, and it returns an empty string once there is no parent left:

```v ignore
mut dir := os.dir(os.real_path(some_file))
for dir != '' {
    if os.is_file(os.join_path_single(dir, 'v.mod')) {
        break
    }
    dir = os.parent_dir(dir)
}
```

It differs from `os.dir()` in three ways:

- A filesystem root has no parent, so `/`, `C:\`, `C:`, `\\server\share` and
  `\\?\UNC\server\share` all give `''` and end the loop above.
- The parent of a top level entry is the absolute root, so `os.parent_dir(r'C:\dir')`
  is `C:\`, never the drive relative `C:`.
- A single element with no directory in it has no parent, so `file.v` gives `''`.
- A Windows drive relative path (`C:`, `C:file.v`, `C:dir\file.v`) gives `''` as
  well, however many components it has. It resolves against the current
  directory *of that drive*, which is state the caller cannot see, and so does
  every one of its ancestors, so none of them is safe to hand back.

Each step is guaranteed to make progress: trailing separators name the same
directory, so they are ignored, and `os.parent_dir('/a/b/')` is `/a` rather than
`/a/b`. A separator is any byte the platform accepts as one, so a Windows path
may mix `/` and `\` and the last separator of either kind decides the parent.

`os.path_rel()` goes the other way: given a base and a target, it returns the
path that leads from one to the other. It is the function to reach for instead of
stripping a common prefix by hand, which gets the number of `..` wrong as soon as
the two paths share no prefix at all:

```v ignore
css := os.path_rel('/srv/app', '/srv/app/static/main.css') or { panic(err) } // 'static/main.css'
up := os.path_rel('/srv/app/logs', '/srv/app/static') or { panic(err) }      // '../static'
```

Both paths are normalized first, so `.` comes back for two spellings of the same
path, and `..` is collapsed before anything is compared. The separator in the
result is the platform's, so on Windows the results above are `static\main.css`
and `..\static`. Two cases have no answer and return an error rather than a
misleading path: when one path is absolute and the other is not, and when, after
the leading components it shares with the target, the base still contains `..`
(the base climbs higher above the starting directory than the target does). A
`..` in the target is fine, so `os.path_rel('a', '../b')` is `../../b`, and so is
a `..` the two paths share: `os.path_rel('../a', '../b')` is `../b`.

`os.path_rel()` is purely lexical: it does not access the filesystem, resolve
symlinks or use the working directory, so `a/link/..` collapses to `a` even if
`link` is a symlink. Pass the paths through `os.real_path()` or `os.abs_path()`
first if that matters (this is also why mixing an absolute and a relative path is
an error).

On Windows the two paths must also be on the same volume, since `C:\a` cannot be
reached from `D:\b` by changing directory.

On Windows, `os.uname()` leaves `release` and `version` empty if the `ver` command
fails or does not report a numeric version. Localized version labels are accepted.

### Walking a tree

`os.walk()` reports files only, and `os.walk_with_context()` reports directories
too but cannot skip them, so neither lets you say "do not descend into this one",
and a large tree has to be read in full. `os.walk_dir()` reports every entry,
directories included, and lets the callback prune:

```v ignore
os.walk_dir('/srv/app', fn (path string, entry os.WalkDirEntry) os.WalkDirAction {
	if err := entry.err {
		eprintln('skipping ${path}: ${err}')
		return .proceed
	}
	if entry.is_dir && entry.name in ['.git', 'node_modules', 'target'] {
		return .skip_dir // the directory is reported, its contents are never read
	}
	if !entry.is_dir && entry.name.ends_with('.v') {
		println(path)
	}
	return .proceed
}) or { panic(err) }
```

Returning `.stop` ends the walk where it stands. Entries are visited in lexical
order and the root is reported first. Symlinks are reported but never followed,
a symlinked root included; pass `os.real_path(root)` to walk what it points to.

`entry.err` is set in two cases. An entry that cannot be stat'ed, such as a
missing root, arrives with `is_dir` left false, so a callback cannot accidentally
descend into something unreadable. A directory that cannot be listed is reported
a second time, with `entry.err` set and `is_dir` still true; returning `.stop`
from that report ends the walk, anything else moves on to its next sibling.

To accumulate state across calls, pass a method of a `mut` local, as in
`os.walk_dir(root, c.visit)`, or capture a reference (`mut c := &Counter{}`): a
closure that captures `[mut n]` only updates its own copy, see
[Closures](https://github.com/vlang/v/blob/master/doc/docs.md#closures).

### Running commands

Use `os.exec(['program', 'arg 1', 'arg 2'])` when the command and its arguments
are separate values. It runs the program directly without invoking a shell, so
spaces and shell metacharacters inside arguments are passed literally. Pass raw
arguments without `os.quoted_path()` or shell escaping.

`os.exec_opt(args)` returns an error on failure. `os.exec_or_panic(args)` and
`os.exec_or_exit(args)` report failures by panicking or exiting. Use
`os.system_args(args)` to inherit standard streams and return only the exit code.
`os.util.exec_with_timeout(args, milliseconds)` returns `none` when the timeout
elapses; it does not terminate the child process.

For configuration values such as a tool name followed by options,
`os.split_args(text)!` parses quoting into literal arguments without shell expansion.
Keep data such as paths and URLs as separate array elements rather than interpolating
it into the configuration string.

The string APIs `os.execute`, `os.raw_execute`, `os.system`, `os.execute_opt`,
`os.execute_or_panic`, `os.execute_or_exit`, and `os.util.execute_with_timeout` are
deprecated because command strings can allow shell injection. Streaming shell commands
through `os.start_new_command` or `os.Command.start` is also deprecated; use
`os.start_new_command_args(args)` instead. It returns a `CommandArgs` stream with
`read_line()`, `eof`, `close()`, and `exit_code`. `read_line()` waits for a complete
line or the end of the output pipe, including when the child pauses between writes.
For more control, use `os.new_process(program)` and `process.set_args(args)`.

When shell syntax is required, invoke the shell explicitly with an argument array.
A shell still interprets its script as code: use a fixed script with positional
arguments for data, and never interpolate untrusted values into the script.
On Windows, shell builtins and batch scripts likewise require an explicit shell.

---

### Error codes

The `IError` returned by the failing `os.*` functions carries a message produced by the
platform's `strerror()`/`FormatMessage()`, so the message text is not the same on every
system. Check the error with a predicate instead:

```v
import os

path := 'no_such_file.txt'
if st := os.stat(path) {
	println('${path} has ${st.size} bytes')
} else {
	if os.is_not_exist(err) {
		println('${path} does not exist')
	}
}
```

`os.is_not_exist`, `os.is_exist` and `os.is_permission_denied` accept every code the
platform may use for that condition. Checking the error is also what avoids the extra
`os.exists()` call that would otherwise be a time-of-check/time-of-use race.

On POSIX systems, the `os.*` functions report a C `errno` value. On Windows, the functions
built on the C runtime (for example `os.stat`, `os.rm`, `os.read_file`, `os.open`,
`os.create`, `os.write_file`, `os.chdir`, `os.truncate` and `os.rename`) report a C `errno`
value too, while the ones built on the Win32 API (`os.mkdir`, `os.rmdir`, `os.ls`,
`os.symlink` and `os.link`) report a Win32 error code. The `os.error_code_*` constants
hold one of these codes for a condition, so prefer the predicates over comparing
`err.code()` with them.

---

### Security advice related to TOCTOU attacks

A few `os` module functions can lead to the **TOCTOU** vulnerability if used incorrectly.
**TOCTOU** (Time-of-Check-to-Time-of-Use problem) can occur when a file, folder or similar
is checked for certain specifications (e.g. read, write permissions) and a change is made
afterwards.
In the time between the initial check and the edit, an attacker can then cause damage.
The following example shows an attack strategy on the left and an improved variant on the right
so that **TOCTOU** is no longer possible.

**Example** <br>
*Hint*: `os.create()` opens a file in write-only mode

<table>
<tr>
<td>Possibility for TOCTOU attack</td>
<td>TOCTOU not possible</td>
</tr>
<tr>
<td>

```v ignore
if os.is_writable("file") {
    // time to make a quick attack
    // (e.g. symlink /etc/passwd to `file`)

    mut f := os.create('path/to/file')!
    // do something with file
    f.close()
}
```

</td>
<td>

```v ignore
mut f := os.create('path/to/file') or {
    println("file not writable")
}

// file is locked
// do something with file

f.close()
```

</td>
</tr>
</table>

**Proven affected functions** <br>
The following functions should be used with care and only when used correctly.

- os.is_readable()
- os.is_writable()
- os.is_executable()
- os.is_link()
