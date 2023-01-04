import semver

fn main() {
	semver.from('asd') or { check_error(err) }
	semver.from('') or { check_error(err) }
}

fn check_error(err IError) {
	match err {
		semver.InvalidVersionFormatError {
			println('wrong format')
		}
		semver.EmptyInputError {
			println('empty input')
		}
		else {
			println('unknown error')
		}
	}
}
