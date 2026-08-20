module tests

import os

fn automation_root() string {
	return os.real_path(os.join_path(os.getwd(), 'thirdparty', 'tccbin_automation'))
}
