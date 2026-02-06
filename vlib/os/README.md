## Description

`os` provides common OS/platform independent functions for accessing
command line arguments, reading/writing files, listing folders,
handling processes etc.

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
