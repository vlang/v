fn factorial(num int) int {
	if num == 1 { 
		return 1
	} else { 
		return num * factorial(num -1 )
	}
}

fn main() {

	result := factorial(10)
	println(result)

}
