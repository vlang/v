mut i := 4
goto L1
L1: for {
	i++
	for {
		if i < 7 {continue L1}
		else {break L1}
	}
}
assert i == 7
