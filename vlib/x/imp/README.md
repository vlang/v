imp is a mischievous implicit build system meant to be used in V scripts

## features
- dependencies derived from what flows take and give
- skips unchanged flows, by mtime or content hash
- pre/post hooks
- script and flow flags
- generated clean and run flows
- checks for binaries, V modules and C libraries

## example usage
```v
#!/usr/bin/env -S v run

import x.imp

const target = $if windows { 'hello.exe' } $else { 'hello' }

mut ctx := imp.new()
ctx.add_target(target)
ctx.add_flow(
	id:    'build'
	help:  'build the binary'
	takes: ['src']
	gives: [target]
	flags: [imp.Flag{ id: 'prod', help: 'optimised build' }]
	flow:  fn (self imp.Flow) ! {
		mut args := ['v', '-o', target]
		if self.flag('prod') {
			args << '-prod'
		}
		args << 'src'
		imp.system_args(args)!
	}
)
ctx.add_auto_flow(.clean)
ctx.add_auto_flow(.run)
ctx.default(['build'])
ctx.flow()
```

## flow fields
| id      | what is it |
|---------|------------|
| `name`  | display name |
| `help`  | help text |
| `takes` | input paths |
| `gives` | output paths |
| `restat` | restore output mtimes when content is unchanged |
| `check` | .mtime (default), .hash or .none |
| `pre`   | []imp.FlowDep run before |
| `post`  | []imp.FlowDep run after |
| `flags` | []imp.Flag read with self.flag |
| `flow`  | fn (self imp.Flow) ! |

Set `restat: true` on a flow whose body may rewrite an output with identical
content. After the body succeeds, imp compares the content of the file outputs
with their previous values and restores the original mtimes for the ones that
did not change, so downstream mtime flows can still skip. Changed content is
left alone. Directories and symlinks are not restatted.

## hooks
| declaration                                | result |
|--------------------------------------------|--------|
| `imp.FlowDep{id: 'clean'}`                 | skipped along with its flow |
| `imp.FlowDep{id: 'clean', required: true}` | always runs, pre before the skip check |

A later post hook waits for the remaining post work of earlier hooks. A hook
that needs an active flow waits for that flow to produce its outputs. Cycles of
post hooks run each flow once; their pending work finishes before continuations
waiting outside the cycle resume.

## flags
| declaration                                | result |
|--------------------------------------------|--------|
| `flags: [imp.Flag{id: 'prod'}]` | `self.flag('prod')`, forces the flow to run |
| `ctx.flag(id: 'android')`       | returns a bool, also readable with `self.flag` |

## checks
`imp.check_binary(['tar'])`
`imp.check_v_modules(['ui2'])`
`imp.check_system_c_links(['mpv/client.h'], flags: ['-lmpv'])`

all return a `bool`

`panic: true` ends the build instead
