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

On Windows, `os.uname()` leaves `release` and `version` empty if the `ver` command
fails or does not report a numeric version. Localized version labels are accepted.

### Walking a tree

`os.walk()` reports files only. It never tells you about a directory, so there is
no way to say "do not descend into this one", and a large tree has to be read in
full. `os.walk_dir()` reports every entry, directories included, and lets the
callback prune:

```v ignore
os.walk_dir('/srv/app', fn (path string, entry os.WalkDirEntry) os.WalkDirAction {
	if entry.err != none {
		eprintln('skipping ${path}: ${entry.err!}')
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
order, the root is reported first, symlinks are reported but never followed, and
an entry that cannot be read arrives with `entry.err` set and `is_dir` left false,
so a callback cannot accidentally descend into something unreadable.

Note that the error travels as a message inside the entry rather than as an
`IError` argument, because the compiler cannot currently pass the builtin `IError`
through a `fn` type declared in a vlib module.

**Accumulating state needs a `mut` parameter.** A closure that inherits a mutable
variable, and a method value taken from a mutable local, both run but discard
their writes:

```v ignore
mut n := 0
os.walk_dir(root, fn [mut n] (path string, entry os.WalkDirEntry) os.WalkDirAction {
	n++ // n is still 0 after the walk
	return .proceed
})
```

Put the state in a struct and pass it as a `mut` parameter instead, which does
accumulate:

```v ignore
struct Counter {
mut:
	files int
}

fn (mut c Counter) visit(_ string, entry os.WalkDirEntry) os.WalkDirAction {
	if entry.err == none && !entry.is_dir {
		c.files++
	}
	return .proceed
}

fn count(root string) int {
	mut c := Counter{}
	os.walk_dir(root, c.visit) or { panic(err) }
	return c.files
}
```

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
