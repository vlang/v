const array1 = [1]
const array2 = [array1]
const array3 = array2[0]

fn test_main() {
	assert array3 == array1
}
