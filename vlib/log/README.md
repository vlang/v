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
