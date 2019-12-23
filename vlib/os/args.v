module os

pub fn args_before_and_after(args []string, what []string) ([]string,[]string) {
	mut found := false
	mut args_before := []string
	mut args_after := []string
	for i, a in args {
		if a in what {
			found = true
			continue
		}
		if !found {
			args_before << a
		}else{
			args_after << a
		}
	}
	return args_before, args_after
}
