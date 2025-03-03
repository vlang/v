## Description

`build` provides a small build system leveraging V(SH) for the buildscript.

## Example

```v
#!/usr/bin/env v

import build

const app_name = 'vlang'
const program_args = 'World'

mut context := build.context()

context.task(name: 'doc', run: || system('v doc .'))
context.task(name: 'run', run: || system('v run . ${program_args}'))
context.task(name: 'build', run: || system('v .'))
context.task(name: 'build.prod', run: || system('v -prod .'))

context.task(
	name: 'release',
	depends: ['doc'],
	run: fn () {
		system('v -prod -o build/${app_name} .')
		// You could use Git to publish a release here too
	}
)

context.run()
```
