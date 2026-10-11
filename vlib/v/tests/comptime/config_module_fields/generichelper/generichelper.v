module generichelper

import alternate as config
import config as real

// field_names returns the field names of the caller's type.
pub fn field_names[T]() []string {
	mut names := []string{}
	$for field in T.fields {
		names << field.name
	}
	return names
}

// value_field_names returns the fields of an already checked canonical value.
pub fn value_field_names(value real.Cfg) []string {
	mut names := []string{}
	$for field in value.fields {
		names << field.name
	}
	return names
}

// local_field_names returns the fields of the explicitly imported alias.
pub fn local_field_names() []string {
	mut names := []string{}
	$for field in config.Cfg.fields {
		names << field.name
	}
	return names
}

// method_names returns the method names of the caller's type.
pub fn method_names[T]() []string {
	mut names := []string{}
	$for method in T.methods {
		names << method.name
	}
	return names
}

// value_method_names returns the methods of an already checked canonical value.
pub fn value_method_names(value real.Cfg) []string {
	mut names := []string{}
	$for method in value.methods {
		names << method.name
	}
	return names
}

// local_method_names returns the methods of the explicitly imported alias.
pub fn local_method_names() []string {
	mut names := []string{}
	$for method in config.Cfg.methods {
		names << method.name
	}
	return names
}
