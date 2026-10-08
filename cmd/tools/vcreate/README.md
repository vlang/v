# Project creation

`v new <project_name>` creates a project in a new directory. Put template flags before the name,
for example `v new --lib my_library` or `v new --web my_app`.

`v init` sets up the current directory. If it already contains a valid `v.mod`, its module name
is used in the completion message and `.gitignore`. Generated library identifiers and filenames
replace hyphens in that name with underscores; the project name and manifest are preserved.
If the manifest cannot be read or its name is empty, the directory name is used,
with hyphens replaced by underscores. This fallback does not rewrite the existing manifest.
Existing V source files are preserved; a template is generated only when none exist.
When the library identifier differs from the project directory name, its source is generated
in a subdirectory with that identifier so the generated tests can import it. Other libraries
keep their source directly in the project directory.

Use `v init --lib` for a library or `v init --web` for a web application. Without a template flag,
the executable template is selected. Setup prompts run only when standard input is a terminal.

Pass `--agents-md` to generate an optional `AGENTS.md` contributor contract, for example
`v new --lib --agents-md my_library` or `v init --agents-md`. Without the flag, no contract is
created. Existing contracts, including symbolic links, are preserved during initialization.
The generated commands match the template: libraries use `v test .` and omit `v run .`.
For an existing project with a custom source layout, the contract describes the source files
without assuming a missing template entry file exists.
