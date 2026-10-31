# Local names in nested modules

A local variable in a nested module may use the module's final directory name.
For example, a file in `service/job_api/task` declares `module task` and may use
`task := 1` inside a function.

The compiler still rejects a local that duplicates a known top-level module name
or an imported module name. When a file's directory matches its module name but
the source root is unknown, the compiler permits the local name rather than
assuming the module is top-level. This includes layouts whose entry directory is
a sibling of the imported modules.
