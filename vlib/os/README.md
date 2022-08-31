## Description:

`os` provides common OS/platform independent functions for accessing
command line arguments, reading/writing files, listing folders,
handling processes etc.

* * *


### Security advice related to TOCTOU attacks

A few `os` module functions can lead to the <b>TOCTOU</b> vulnerability if used incorrectly. 
<b>TOCTOU</b> (Time-of-Check-to-Time-of-Use problem) can occur when a file, folder or similar 
is checked for certain specifications (e.g. read, write permissions) and a change is made 
afterwards. 
In the time between the initial check and the edit, an attacker can then cause damage. 
The following example shows an attack strategy on the left and an improved variant on the right 
so that <b>TOCTOU</b> is no longer possible.


<b>Example</b>
<i>Hint</i>: `os.create()` opens a file in write-only mode

<table>
<tr>
<td>
Possibility for TOCTOU attack

```v ignore 
if os.is_writable("file"){

    // >> time to make a quick attack (e.g. symlink /etc/passwd to >file<) <<

    mut f := os.create('path/to/file') ?
        // <do something with file>
    f.close()
}
```
</td>
<td>TOCTOU not possible

```v ignore
mut f := os.create('path/to/file') or {
    println("file not writable")
}

// >> do someting with file; file is locked <<

f.close()
```
</td>
</tr>
</table>

<b> Proven affected functions </b></br>
The following functions should be used with care and only when used correctly.

* os.is_readable()
* os.is_writable()
* os.is_executable()
* os.is_link()
