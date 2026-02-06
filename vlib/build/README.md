## Description

`build` provides a small build system leveraging V(SH) for the buildscript.

## Example

> See also: [build_system example](https://github.com/vlang/v/tree/master/examples/build_system)

```v
#!/usr/bin/env -S v run

import build
// .vsh automatically imports `os`, so you don't need this typically
import os { system }

const app_name = 'vlang'
const program_args = 'World'

mut context := build.context(
	// Set the default task to `release` when no arguments are provided
	default: 'release'
)

context.task(name: 'doc', run: |self| system('v doc .'))
context.task(name: 'run', run: |self| system('v run . ${program_args}'))
context.task(name: 'build', run: |self| system('v .'))
context.task(name: 'build.prod', run: |self| system('v -prod .'))

context.task(
	name:    'release'
	depends: ['doc']
	run:     fn (self build.Task) ! {
		system('v -prod -o build/${app_name} .')
		// You could use Git to publish a release here too
	}
)

context.run()
```

## Pre-Compiling

Running VSH scripts requires V to compile the script before executing it, which can cause a delay
between when you run `./build.vsh` and when the script actually starts executing.

If you want to fix this, you can "pre-compile" the buildscript by building the script, i.e, running
`v -skip-running build.vsh`.

> You will need to rebuild every time you change the buildscript, and you should also add `/build`
> to your `.gitignore`

> If you want maximum speed, you can also `v -prod -skip-running build.vsh`