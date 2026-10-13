## Description

`v.errors` formats compiler diagnostics with source excerpts, source positions, and
template call stacks. `DiagnosticRun` shares path resolution and the most recently read source
file across a report. Its `formatted_error` method preserves the standalone formatter output.
Use a fresh run for each report and keep the working directory, path display settings, and
reported source files unchanged while it is in use.
