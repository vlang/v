struct Vec2d {
	x int
	y int
}

struct Vec3d {
	x int
	y int
	z int
}

type Vec = Vec2d | Vec3d
type SumType = int | string | Vec2d | []Vec2d

fn match_vec(v Vec) {
	match v {
		Vec2d {
			println('Vec2d(${v.x},${v.y})')
		}
		Vec3d {
			println('Vec2d(${v.x},${v.y},${v.z})')
		}
	}
}

fn match_classic_num() {
	match 42 {
		0 {
			assert false
			(false)
		}
		1 {
			assert false
			(false)
		}
		42 {
			println('life')
		}
		else {
			assert false
			(false)
		}
	}
}

fn match_classic_string() {
	os := 'JS'
	print('V is running on ')
	match os {
		'darwin' { println('macOS.') }
		'linux' { println('Linux.') }
		else { println(os) }
	}
}

fn match_bool_cond() {
	volume := 'c:'
	rooted := false
	path_separator := '/'
	println(match true {
		volume.len != 0 { volume }
		!rooted { '.' }
		else { path_separator }
	})
}

fn match_sum_type(sum SumType) {
	match sum {
		int {
			println('sum is int')
		}
		string {
			println('sum is string')
		}
		Vec2d {
			println('sum is Vec2d')
		}
		[]Vec2d {
			println('sum is []Vec2d')
		}
	}
}

fn main() {
	match_vec(Vec2d{42, 43})
	match_vec(Vec3d{46, 74, 21})
	match_classic_num()
	match_classic_string()
	match_bool_cond()
	match_sum_type(42)
	match_sum_type('everything')
	match_sum_type(Vec2d{7, 11})
	match_sum_type([Vec2d{7, 11}, Vec2d{13, 17}])
}
