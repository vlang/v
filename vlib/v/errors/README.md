## Description

`v.errors` formats compiler diagnostics with source excerpts, source positions, and
template call stacks. `DiagnosticRun` shares path resolution and the most recently read source
file across a report, and finds a line in that file from the line it found before. Its
`formatted_error` and `formatted_parser_diagnostic` methods preserve the standalone formatter
output. Reuse a run when reporting multiple checker or parser diagnostics.
Use a fresh run for each report and keep the working directory, path display settings, and
reported source files unchanged while it is in use.
