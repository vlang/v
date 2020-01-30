fn test_while_conditional_loop() {
  mut i := 0
  mut arr := []int
  while i < 5 {
	  arr << i
	  i++
  }
  assert arr.len == 5
}

fn test_while_infinite_loop_with_break() {
  mut count := 0
  while {
	  if count == 5 {
		  break
	  }
	  count++
  }
  assert count == 5
}

fn test_while_with_x_in_arr_format() {
  mut arr := [1,5,2,5,3,6]
  mut i := 0
  while 5 in arr {
	  if arr[i] == 5 {
		  arr[i] = 0
	  }
	  i++
  }
  assert arr[1] == 0
  assert arr[3] == 0
}
