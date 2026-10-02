module os

fn windows_version_parts(output string) (string, string) {
	for word in output.split_any(' \t\r\n[]') {
		parts := word.split('.')
		if parts.len < 3 {
			continue
		}
		mut valid := true
		for part in parts {
			if part.len == 0 || part.bytes().any(it < `0` || it > `9`) {
				valid = false
				break
			}
		}
		if valid {
			return parts[..2].join('.'), parts[2]
		}
	}
	return '', ''
}
