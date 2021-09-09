module os

pub fn open_uri(uri string) ? {
	$if macos {
		result := execute('open "$uri"')
		if result.exit_code != 0 {
			return error('unable to open url: $result.output')
		}
	} $else $if freebsd || openbsd {
		result := execute('xdg-open "$uri"')
		if result.exit_code != 0 {
			return error('unable to open url: $result.output')
		}
	} $else $if linux {
		providers := ['xdg-open', 'x-www-browser', 'www-browser', 'wslview']

		// There are multiple possible providers to open a browser on linux
		// One of them is xdg-open, another is x-www-browser, then there's www-browser, etc.
		// Look for one that exists and run it
		for provider in providers {
			if exists_in_system_path(provider) {
				result := execute('$provider "$uri"')
				if result.exit_code != 0 {
					return error('unable to open url: $result.output')
				}
				break
			}
		}
	} $else {
		return error('unsupported platform')
	}
}
