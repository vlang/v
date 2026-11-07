module multi_return_models

pub struct Config {
pub:
	value string
}

pub enum Mode {
	enabled
}

// load_config returns a configuration through a result wrapper.
pub fn load_config(value string) !Config {
	return Config{ value: value }
}

// optional_config returns none for an empty value.
pub fn optional_config(value string) ?Config {
	if value.len == 0 {
		return none
	}
	return Config{ value: value }
}

// load_mode returns an enum through a result wrapper.
pub fn load_mode() !Mode {
	return .enabled
}
