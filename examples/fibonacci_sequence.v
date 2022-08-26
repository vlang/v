/* 
The fibonacci sequence is the series, in which each 
number is the sum of the two previous numbers
*/
fn main() {
	mut prev_num := 1
	mut num := 1 
	mut sum := 0
	term := 10

	print(prev_num)
	print(" ")
	print(num)
	print(" ")

	for i := 0; i < term; i++ {
		sum = prev_num + num
		prev_num = num
		num = sum
		print(sum)
		print(" ")
	}
}
