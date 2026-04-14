module main

import middleware { logger_middleware }
import foo.middleware as mw { aliased_logger_middleware }

struct UseConfig {
	handler fn () bool = unsafe { nil }
}

struct App {}

fn (app App) use(config UseConfig) bool {
	return config.handler()
}

fn main() {
	app := App{}
	assert app.use(handler: logger_middleware)
	assert app.use(handler: aliased_logger_middleware)
}
