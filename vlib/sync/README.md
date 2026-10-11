## Description

`sync` provides cross platform handling of concurrency primitives.

Channels can carry function values, including aliases such as `type Task = fn ()`.
Function channels support explicit capacities and default initialization in structs
and globals. Arrays of channels can use an explicit `init` expression to initialize
each channel. A received function value can be called normally.

Use a timer when a wait needs to participate in a `select`:

```v
import sync
import time

timer := sync.new_timer(500 * time.millisecond)
defer {
	timer.stop()
}
select {
	fired_at := <-timer.c {
		println('timer fired at ${fired_at}')
	}
	// another channel can be handled here
}
```
