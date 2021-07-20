module os_js

// os.setenv sets the value of an environment variable with `name` to `value`.
pub fn setenv(key string, val string, overwrite bool) {
	#if ($process.env[key] && !(overwrite.valueOf())) return;
	#$process.env[key] = val + '';
}

// `getenv` returns the value of the environment variable named by the key.
pub fn getenv(key string) string {
	mut res := ''
	#if ($process.env[key]) res = new builtin.string($process.env[key])

	return res
}

// os.unsetenv clears an environment variable with `name`.
pub fn unsetenv(name string) int {
	#$process.env[name] = ""

	return 1
}

pub fn environ() map[string]string {
	mut res := map[string]string{}
	#for (const key in $process.env) {
	#res.map.set(key,$process.env[key])
	#}

	return res
}
