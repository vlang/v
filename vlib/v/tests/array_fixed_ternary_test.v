fn test_main() {
	_ := if true { [0]! } else { [1]! }
	_ := if true { [0] } else { [1] }
}
