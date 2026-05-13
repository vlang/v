# CodeLite Setup

CodeLite works well with V when you open the code as a
[File System Workspace](https://docs.codelite.org/workspaces/file_system/)
and define custom build targets with
[CodeLite macros](https://docs.codelite.org/settings/macros/).

This guide uses POSIX shell commands, which match Linux and macOS. On Windows,
use `.exe` outputs and replace `rm -f` with `del`.

## Prerequisites

- Put `v` on your `PATH`. If it is not on your `PATH`, replace `v` below
  with the absolute path to your V compiler.
- Create a CodeLite workspace from your existing source tree:
  1. Open `File` -> `New` -> `New workspace`
  2. Choose `File System Workspace`
  3. Select the folder containing your V sources

Open the workspace settings by right-clicking the top-level folder and choosing
`Settings...`.

## General Settings

Set these fields first:

- `Tool chain`: `gcc` or `clang`
- `File extensions`: add `v`, `vsh`, and `vv`

CodeLite uses the selected tool chain mainly to parse build output, so any
GNU-like tool chain is fine here.

For single-file programs, use:

- `Executable`: `$(CurrentFilePath)/$(CurrentFileName)`
- `Working directory`: `$(CurrentFilePath)`

For a project rooted at the workspace folder, use:

- `Executable`: `$(WorkspacePath)/$(WorkspaceName)`
- `Working directory`: `$(WorkspacePath)`

## Build Targets

The default `build` and `clean` targets can not be removed. Add `run` and
`test` targets if you want them in the `Build` menu.

### Build the current file

This is the simplest setup for scripts, examples, and small programs:

- `build`:
  `v -g -o "$(CurrentFilePath)/$(CurrentFileName)" "$(CurrentFileFullPath)"`
- `clean`: `rm -f "$(CurrentFilePath)/$(CurrentFileName)"`
- `run`: `v run "$(CurrentFileFullPath)"`
- `test`: `v test "$(CurrentFileFullPath)"`

### Build the whole workspace

If the workspace root is a V project with `v.mod`, or the workspace folder is
your app entry point, use:

- `build`: `v -g -o "$(WorkspacePath)/$(WorkspaceName)" "$(WorkspacePath)"`
- `clean`: `rm -f "$(WorkspacePath)/$(WorkspaceName)"`

If your app starts from a specific file such as `src/main.v`, replace
`"$(WorkspacePath)"` with that file path in the `build` target.

## Running and Debugging

- Press `F7` to run the `build` target.
- Use `Run` or `Debug` after `Executable` points to the file produced by the
  build target.
- Keep `-g` in the build command so `gdb` or `lldb` can show V source
  locations.

## Better Compiler Errors in the IDE

If CodeLite opens the wrong file from compiler errors, add this workspace
environment variable:

- `VERROR_PATHS=absolute`

That makes `v` print absolute file paths, which is often better for IDE
launchers.

## Formatting and Language Features

For manual formatting, or for a formatter command inside CodeLite, use:

- `v fmt -w "$(CurrentFileFullPath)"`

For completion, go-to-definition, hover information, and related IDE features,
use [v-analyzer](https://github.com/vlang/v-analyzer) alongside CodeLite.
