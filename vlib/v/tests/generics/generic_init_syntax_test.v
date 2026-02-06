fn func[I]() I {
	return I{0}
}

fn test_main() {
	assert func[f64]() == 0.0
	assert func[int]() == 0
	assert func[bool]() == false
}
