## Description

`log` provides your application logging services.
You can log to file or to the console and use different
logging levels, so that you are not overwhelmed by the logs.

## Basic usage

The log module creates a default Log instance by default, and
provides utility functions, that you can use to access it.
Note: the default Log instance is thread safe.

That makes it very convenient to use in subsystems, without having
to thread a log instance everywhere:

```v
import log

fn abc() {
	log.info('some information')
	log.warn('a warning')
}

// this will not be visible, the default log level is .info:
log.debug('a debug message')

log.set_level(.debug)

// this will be now visible, the log level was changed to .debug:
log.debug('a debug message')

abc()
```

## Advanced usage

You can also create your own log instances, with different options
applied to them:

```v
import log

fn main() {
	mut l := log.Log{}
	l.set_level(.info)
	l.set_full_logpath('./info.log')
	l.log_to_console_too()

	l.info('info')
	l.warn('warn')
	l.error('error')
	l.fatal('fatal') // panic, marked as [noreturn]
}
```

## Backwards compatibility

After 2025/01/21, the `log` module outputs to `stderr` by default.
Before that, it used `stdout` by default.

If you want to restore the previous behaviour, you have to explicitly call `log.use_stdout()` :

The replacement logger respects `-d log_default_level`, which defaults to `info`.

```v
import os
import log

fn main() {
	// log.info('this will be printed to stderr after 2025/01/21 by default')
	log.use_stdout()
	log.info('this will be printed to stdout')
}
```

If you want to just silence the note about the stdout -> stderr, during the transition period,
call `l.set_output_stream(os.stderr())` explicitly.

## Structured records

`Log` and `ThreadSafeLog` support `debug_with`, `info_with`, `warn_with`, `error_with`,
and `fatal_with`, each taking a message and a `map[string]string` of fields.
These methods emit one JSON object per line and respect the configured log level and targets.
The built-in `time`, `level`, and `msg` keys precede user fields sorted by key.
Fields with those reserved names are ignored, and all string keys and values are JSON escaped.
The plain logging methods retain their text format. `fatal_with` panics like `fatal`.
Use `ThreadSafeLog` when multiple threads share a logger.

## Structured logging with RecordLogger

`RecordLogger` is a richer, slog-style API. It keeps the typed attributes,
pluggable handlers, derived loggers and context aware logging separate from the
legacy `Log` type, while preserving the exact text format of the plain methods.

```v
import log
import os

fn main() {
	// A handler decides both the format and the destination.
	h := log.new_text_handler(os.stderr(), log.HandlerOptions{})

	// Derived loggers share the handler, so they stay thread safe.
	req_log := log.new_logger(h).with(log.str('service', 'api'))

	req_log.info('request started', log.int('port', 8080))
	req_log.warn('slow response', log.float('seconds', 1.5))

	// log.json returns newline delimited JSON with stable field order.
	jh := log.new_json_handler(os.stderr(), log.HandlerOptions{})
	log.new_logger(jh).info('done', log.bool('ok', true))
}
```

`set_level` controls both the legacy logger and the structured package-level
calls; `set_record_level`/`set_record_logger` are available when the two need
different configuration.

Package level calls accept attributes too, using the global JSON logger:

```v
import log

fn main() {
	log.info('user signed in', log.str('user', 'alice'))
}
```

Available attribute constructors: `str`, `int`, `float`, `bool`, `time_attr`,
`group` and `err`. User fields named `time`, `level` or `msg` are ignored so a
record cannot spoof its metadata. Non-finite floats (NaN, infinity) are written
as JSON `null`. For hot paths there are two slice-free options: reuse an `Attrs`
builder with `log_attrs` (it starts with room for eight fields and grows if
needed), or pass a stack allocated `FixedAttrs` list (`fixed1` .. `fixed4`) to
`log_fixed`. Built-in handlers render into a reused scratch buffer and write it
with a single call.

Built-in handlers: `TextHandler`, `JSONHandler`, `MultiHandler` and
`DiscardHandler`. `HandlerOptions` supports a fixed or runtime adjustable level,
`replace_attr` for redaction, time format, local time and source capture (when
compiled with call site support).

### Wrapping handlers

`SampledHandler` forwards one record in every N (deterministic) or with
probability 1/N (random). `AsyncHandler` moves formatting and I/O to a worker
thread. Because it buffers records, it is lossy on abrupt exit: call `close()`
to drain the queue before shutting down.

```v
import log
import os

fn main() {
	h := log.new_json_handler(os.stderr(), log.HandlerOptions{})
	async_h := log.new_async_handler(log.new_sampled_handler(h, 10), 1024)
	defer { async_h.close() }
	log.new_logger(async_h).info('sampled and asynchronous')
}
```
