module os

$if js_node {
	#global.$ENV = $process.env
} $else {
	#global.$ENV = {}
}

// setenv sets the value of an environment variable with `name` to `value`.
pub fn setenv(key string, val string, overwrite bool) {
	#if ($ENV[key] && !(overwrite.valueOf())) return;
	#$ENV[key] = val + '';
}

// `getenv` returns the value of the environment variable named by the key.
pub fn getenv(key string) string {
	mut res := ''
	#if ($ENV[key]) res = new string($ENV[key])

	return res
}

// `getenv_opt` returns the value of the environment variable named by the key.
// If such an environment variable does not exist, then it returns `none`.
pub fn getenv_opt(key string) ?string {
	#if (!$ENV[key]) return none__;

	mut res := ''
	#if ($ENV[key]) res = new string($ENV[key]);

	return res
}

// unsetenv clears an environment variable with `name`.
pub fn unsetenv(name string) int {
	#$ENV[name] = ""

	return 1
}

pub fn environ() map[string]string {
	mut res := map[string]string{}
	#for (const key in $ENV) {
	#res.map.set(key,$ENV[key])
	#}

	return res
}
