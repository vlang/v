# Long paths on Windows

Windows limits most paths to 260 characters (`MAX_PATH`). The system setting
`LongPathsEnabled` lifts that limit only for executables that declare themselves
`longPathAware` in their manifest, or for calls that use `\\?\` paths.

## Executables built by V

On a Windows host, V gives a Windows executable a manifest that declares `longPathAware`
right after linking it, the way `-icon` adds an icon. With `LongPathsEnabled` turned on,
such an executable can create, check and open files past `MAX_PATH`.

* An executable that its linker already gave a manifest keeps that manifest unchanged.
  MinGW GCC links one that declares `longPathAware` already.
* If `<output>.manifest` exists beside the final executable, V leaves that external manifest
  in control. Include `longPathAware` there if the program also needs long paths.
* Executables linked by MSVC get no manifest: they load `dbghelp.dll` at startup, and its
  initialization would hang on a long `TMP` (see below) before any V code runs.
* Executables built with the FastC backend (`-b fastc`), shared libraries, and Windows
  executables cross-compiled on another system get no manifest either.
* If the manifest cannot be added, the build still succeeds. V then prints a warning with
  `-v` or `-showcc`, or when the build directory had to move (see below).

`os.exists` uses `GetFileAttributesW`, so it follows the manifest. The compiler starts a tool
whose path is 260 characters or longer through its `\\?\` form, which `CreateProcessW`
accepts; `os.Process` also takes a `\\?\` path as it is.

## Long TMP and TEMP values

In a `longPathAware` process, Windows' own `GetTempPathW` loops forever when `TMP` is 261
characters or longer (or `TEMP`, or `USERPROFILE`, when the variables before it are unset).
`CreateProcessW` calls it for every program it starts, so such a process would hang at
100% CPU. Programs like `cmd.exe` are `longPathAware` as well.

Before a V program starts another program on Windows (`os.exec`, `os.execute`,
`os.Process`, `os.Command`, `os.system`, `os.execvp`, `os.execve`, `os.open_uri`), it replaces
a temporary folder variable that is too long with a short one, best first:

1. the 8.3 short name of the same folder,
2. `%LOCALAPPDATA%\Temp`,
3. `%SystemRoot%\Temp`.

The program's own environment changes, so child programs inherit the short value. An
environment given to a child explicitly (`os.Process.set_environment`, `os.execve`) gets
the same fix.

## Build directories

`tcc.exe` is not long-path aware, and `CreateProcessW` cannot start a program whose working folder
is past `MAX_PATH`. When the C build directory next to the output (also the one of
`-b fastc`), or the TinyCC preflight directory under the temporary folder, would be 240
characters or longer, V uses a short name
for it under `%LOCALAPPDATA%\v3cc`, which only the user can write to. That per-build directory
is removed after the build; the `v3cc` folder itself stays. Shorter directories keep their
usual place, and other operating systems never move them.

The C build directory stays where it is when a C or linker flag holds a relative path that
leaves it, such as `-L../libs`, since such a path names a file beside the output.
