# Compiler pipeline
A simple high level explanation
how the compiler pipeline (`parser` -> `checker` -> `generator`) works.

## Reading files
### Getting builtin files
To load all builtin files,
a preference `Preferences.lookup_path` for the path where to look for exists.
See `Builder.get_builtin_files` as example.
If the file is a `.vsh` file and the backend is C, `vlib/os` will also be loaded as builtin.

### Getting project files
Either there is a specific file: `my_file.v` or a directory containing V files.
In the last case it scans that directory for all files.
See `Builder.v_files_from_dir` as the helper method.
This list of files needs to be filtered so that only `*.v` files exist.

Skips the following file types:
- `*_test.v`
- either `*.c.v` or `*.c.js` depending on the backend
- all files that doesn't end with `.v`
- Files that are not defined in `Preferences.compile_defines`
or `Preferences.compile_defines_all` **if any file is defined**.

## Parsing files
To parse something a new template is created as the first step:
```v
import v.table

table := table.new_table()
```

a new preference is created:
```v
import v.pref

pref := pref.Preferences{}
```

and a new scope is created:
```v
import v.ast

scope := ast.Scope{
	parent: 0
}
```
after that, you can parse your files.

## Parse text
If you want to parse only text which isn't saved on the disk you can use this function.
```v oksyntax
import v.parser

code := ''
// table, pref and scope needs to be passed as reference
parsed_file := parser.parse_text(code, table, .parse_comments, &pref, &scope)
```

## Parse a single file
For parsing files on disk, a path needs to be provided.
The paths are collected one step earlier.
```v oksyntax
import v.parser

path := ''
// table, pref and scope needs to be passed as reference
parsed_file := parser.parse_file(path, table, .parse_comments, &pref, &scope)
```

## Parse a set of files
If you have a batch of paths available which should be parsed,
there is also a function which does all the work.
```v oksyntax
import v.parser

paths := ['']
// table, pref and scope needs to be passed as reference
parsed_files := parser.parse_files(paths, table, &pref, &scope)
```

## Parse imports
A file often contains imports. These imports might need to be parsed as well.
The builder contains a method which does this: `Builder.parse_imports`.

If the module which is imported isn't parsed already,
you have to collect it relatively from the main file.
For this the `ast.File` contains a list of imports.
Those imports needs to be found on disk.
`.` is just replaced with seperators in the relative location of the main file.
Then all files from that directory are collected and parsed again like the previous steps explained.

## Checking AST
A new checker is created:
```v oksyntax
import v.checker

mut checker := checker.new_checker(table, &pref)
```

After checking your files in `checker.errors` and `checker.warnings` you can see the results.

### Check `ast.File`
```v oksyntax
checker.check(parsed_file)
```

### Check a list of `ast.File`
```v oksyntax
checker.check_files(parsed_files)
```

## Generate target from AST
Generating C code works just as this:
```v oksyntax
import v.gen

res := gen.cgen(parsed_files, table, &pref)
```
