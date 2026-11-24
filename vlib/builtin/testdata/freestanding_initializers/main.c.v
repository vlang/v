module main

#flag @DIR/callbacks.c

fn C.v_test_initializers_ready() i32

fn main() {
	if C.v_test_initializers_ready() != 1 { exit(89) }
	args := arguments()
	if args.len != 3 || args[1] != 'alpha beta' || args[2] != 'λ' { exit(88) }
	print('M')
}
