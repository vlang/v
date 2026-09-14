## Description

`os` provides common OS/platform independent functions for accessing
command line arguments, reading/writing files, listing folders,
handling processes etc.

On Windows, `os.data_dir()` uses `%LocalAppData%` for user-specific
application data.

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

### Running commands

Use `os.exec(['program', 'arg 1', 'arg 2'])` when the command and its arguments
are already separate values. It runs the program directly and does not invoke a
shell, so spaces and shell metacharacters inside arguments are passed literally.

Use `os.execute('command string')` only when shell syntax is intended.

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
