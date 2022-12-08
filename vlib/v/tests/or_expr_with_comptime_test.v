fn foo() ! {
}

fn test_bar() {
	foo() or {
		$if linux {
		}
	}
}
