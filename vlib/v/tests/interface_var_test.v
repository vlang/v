interface Param {}

fn test_main() {
	param := Param(false)

	match param {
		bool {
			println(param) // &false
			println(param == true) // false
			println(param == false) // true
			if param {
				assert false
			}
			if !param {
				assert true
			}
			if param == true {
				assert false
			}
			if param == false {
				assert true
			}
		}
		else {
			return
		}
	}
}
