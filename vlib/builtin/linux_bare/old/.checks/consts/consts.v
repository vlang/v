module main
import forkedtest

const (
	integer1 = 111
	integer2 = 222
	integer3 = integer1+integer2
	integer9 = integer3 * 3
	abc = "123"
)

fn check_const_initialization() {
	assert abc == "123"
	assert integer9 == 999
}

fn main(){
	mut fails := 0
	fails += forkedtest.normal_run(check_const_initialization, "check_const_initialization")
	assert fails == 0
	sys_exit(0)
}
