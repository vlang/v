module scanner

import v.token

pub struct Warning {
	message   string
	file_path string
	pos       token.Position
	reporter  Reporter
}
