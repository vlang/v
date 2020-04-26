type A = int | string | f64

fn main() {
	x := A('test')
	_ = match x {
		int {
			'int'
		}
		string {
			'string'
		}
	}
	_ := match x {
		int {
			'int'
		}
		string {
			'string'
		}
		f64 {
			'f64'
		}
		else {
			'else'
		}
	}
	_ := match x {
		int {
			'int'
		}
		string {
			'string'
		}
		else {
			'else'
		}
		f64 {
			'f64'
		}
	}
}
