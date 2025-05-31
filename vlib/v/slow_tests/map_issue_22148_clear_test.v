import time
import rand

fn check_keys_with_map_clear(iteration int, keys []int) {
	mut clr := map[int]int{}
	for i in 0 .. 256 {
		mut new := map[int]int{}
		clr.clear()
		for e in keys {
			new[e] = e
			clr[e] = e
			if new.len != clr.len {
				println('# new.len == ${new.len:3} != clr.len == ${clr.len:3} \n    i: ${i:4} e: ${e:4} | keys.len: ${keys.len:3}, keys: ${keys}')
				assert new.len == clr.len
				return
			}
		}
		if new != clr {
			println('# new != clr \n    i: ${i:4} | keys.len: ${keys.len:3}, keys: ${keys}')
			assert new == clr
			return
		}
	}
}

// vfmt off
const found_problem_keys = [
	 [4369, 3749, 2511, 2472, 4583, 2643, 2074, 492, 53, 4166, 1355, 2912, 4207, 3454, 3893, 1175, 86, 1520, 4888],
	 [2, 11, 13, 17, 20, 23, 31, 33, 41, 41, 46, 52, 56, 61, 61, 68, 71, 73, 74, 74, 82, 88, 90, 94, 96, 98, 98],
	 [25, 14, 84, 11, 21, 54, 87, 90, 16, 65, 63, 43, 61, 94, 29, 49, 60, 57, 10, 92, 93, 20],
	 [23, 59, 16, 32, 18, 72, 53, 35, 78, 5, 13, 56, 64, 49, 37, 88, 50, 81, 35, 71, 84, 53, 7, 60, 17],
	 [1, 8, 15, 55, 10, 81, 22, 68, 76, 31, 95, 51, 78, 49, 3, 49, 0, 27, 97, 86, 0, 37, 24, 39],
	 [8, 1, 6, 4, 11, 7, 19, 8, 17, 9, 14, 5, 17, 19, 3, 4, 15, 13, 14, 1, 17, 8, 0, 6, 10, 17, 11, 16],
	 [770, 21, 176, 827, 5, 488, 214, 950, 641, 102, 654, 243, 359, 169, 265, 92, 305, 442, 274],
	 [569, 122, 509, 442, 114, 790, 769, 159, 78, 191, 433, 636, 471, 253],
	 [705, 518, 396, 203, 162, 896, 752, 749, 718, 809, 792, 827, 690, 941, 889, 20, 19, 804, 334, 293, 243],
	 [379, 478, 177, 411, 171, 360, 441, 705, 810, 275, 429, 584, 672, 303, 492, 440, 574, 401, 903, 10, 755, 324, 164, 335],
	 [694, 922, 411, 610, 37, 287, 577, 217, 417, 791, 379, 540, 588, 239, 0, 362, 81, 332, 663, 986, 210],
	 [359, 783, 194, 763, 46, 763, 340, 252, 756, 693, 234, 748, 799, 168, 7, 498, 819, 581, 35, 0, 625, 352, 10, 420, 823],
	 [379, 136, 659, 194, 413, 464, 584, 420, 602, 90, 966, 863, 374, 736, 249],
	 [283, 319, 708, 672, 725, 781, 249, 78, 983, 720, 777, 548, 620, 689, 307, 153, 574, 254, 306, 481, 861, 595, 569, 654, 584],
	 [999, 40, 708, 971, 227, 378, 355, 215, 371, 237, 848, 409, 303, 927, 876, 46, 927, 0, 392, 576, 23, 617, 869, 419, 174],
	 [358, 463, 965, 279, 417, 192, 906, 642, 887, 760, 936, 45, 836, 657, 654, 231, 270, 275, 241, 388, 187, 941, 34, 979, 908],
]
// vfmt on

fn test_potential_problem_keys() {
	for idx, keys in found_problem_keys {
		println('##### idx: ${idx:5}, keys: ${keys}')
		check_keys_with_map_clear(idx, keys)
	}
}

fn test_some_new_random_keys_for_a_while() {
	start_ticks := time.ticks()
	mut idx := 0
	for time.ticks() - start_ticks < 2000 {
		keys := get_random_numbers()
		println('##### random idx: ${idx:5}, keys: ${keys}')
		check_keys_with_map_clear(idx, keys)
		idx++
	}
}

fn get_random_numbers() []int {
	mut res := []int{}
	n := rand.u32n(32) or { 5 }
	for _ in 0 .. n {
		x := rand.intn(500) or { 0 }
		// filtering out the 0 key, makes failures much less likely
		// (sorted keys *with 0*, are also much less likely to fail)
		// if x == 0 { continue }
		res << x
	}
	// sorting reduces the chances of a new.len != clr.len *a lot*
	// return res.sorted()
	return res
}
