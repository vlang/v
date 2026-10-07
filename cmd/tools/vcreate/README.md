# Project creation

`v new <project_name>` creates a project in a new directory. Put template flags before the name,
for example `v new --lib my_library` or `v new --web my_app`.

`v init` sets up the current directory. If it already contains a valid `v.mod`, its module name
is used in the completion message, generated library files and `.gitignore`. The manifest is
preserved. If the manifest cannot be read or its name is empty, the directory name is used,
with hyphens replaced by underscores. This fallback does not rewrite the existing manifest.
Existing V source files are preserved; a template is generated only when none exist.

Use `v init --lib` for a library or `v init --web` for a web application. Without a template flag,
the executable template is selected. Setup prompts run only when standard input is a terminal.
