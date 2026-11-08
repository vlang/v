module mail

import time

fn test_parse_date() {
	t := parse_date('Tue, 01 Jan 2030 00:00:00 +0000')!
	assert t.year == 2030
}
