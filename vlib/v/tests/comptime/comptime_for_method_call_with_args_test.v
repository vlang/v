import os

struct Dummy {}

fn (d Dummy) sample(file_name string) {
	println(file_name)
}

fn test_comptime_for_method_call_with_args() {
	$for method in Dummy.methods {
		if os.args.len > 1 {
			d := Dummy{}
			d.$method(...os.args)
		}
	}
	assert true
}

struct SpreadReceiver {
mut:
	total int
}

fn (d SpreadReceiver) add(value int) int {
	return d.total + value
}

fn (mut d SpreadReceiver) update(value int) int {
	d.total += value
	return d.total
}

fn test_comptime_spread_keeps_receiver_declared_inside_runtime_branch() {
	$for method in SpreadReceiver.methods {
		if os.args.len > 0 {
			mut d := SpreadReceiver{ total: 40 }
			args := ['5']
			result := d.$method(...args)
			assert result == 45
			if method.name == 'update' {
				assert d.total == 45
			} else {
				assert d.total == 40
			}
			mut array := SpreadReceiver{ total: 40 }
			shadow_result := array.$method(...args)
			assert shadow_result == 45
			if method.name == 'update' {
				assert array.total == 45
			} else {
				assert array.total == 40
			}
		}
	}
}
