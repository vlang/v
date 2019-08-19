fn test_match() {
	// a := 3 
	// mut b := 0 
	// match a {
	//    2 => println('two') 
	//    3 => println('three') 
	//         b = 3 
	//    4 => println('four') 
	// else => println('???') 
	// } 
	// assert b == 3 

	assert match 1 {
        1 => 2
        2 => 3
        3 => {
            3
        }
        4 => {
            4
        }
        else => 5
    } == 2
    assert match 3 {
        1 => 2
        2 => 3
        3 => {
            3
        }
        4 => {
            4
        }
        else => 5
    } == 3
    assert match 0 {
        1 => 2
        2 => 3
        3 => {
            3
        }
        4 => {
            4
        }
        else => 5
    } == 5

    assert match 1 {
        else => 5
    } == 5
    
    mut a := 0
    match 0 {
        0 => a = 1
        1 => {
            a = a + 2
        }
    }
    assert a == 1
    
    a = 0
    match 1 {
        0 => a = 1
        1 => {
            a = a + 2
            a = a + 2
            a = a + 2
        }
    }
    assert a == 6
} 
