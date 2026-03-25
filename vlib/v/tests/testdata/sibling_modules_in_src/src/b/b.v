module b

import a

pub fn say_b() string {
	return 'b says ${a.say_a()}'
}
