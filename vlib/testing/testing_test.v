import testing

fn test_is_same_int(){
        ok_assert := testing.is(int(42), int(42), "should match same int")
        assert ok_assert == true
}

fn test_is_different_int(){
        ok_assert := testing.is(int(42), int(12), "should match same int")
        assert ok_assert == false
}

fn test_is_not_same_int(){
        ok_assert := testing.is_not(42, 42, "should not match same int")
        assert ok_assert == false
}

fn test_is_not_different_int(){
        ok_assert := testing.is_not(42, 43, "should not match same int")
        assert ok_assert == true
}

fn test_is_same_string(){
        ok_assert := testing.is_string("a", "a", "should match same string")
        assert ok_assert == true
}

fn test_is_different_string(){
        ok_assert := testing.is_string("a", "b", "should match same string")
        assert ok_assert == false
}

fn test_is_not_same_string(){
        ok_assert := testing.is_not_string("a", "a", "should not match same string")
        assert ok_assert == false
}

fn test_is_not_different_string(){
        ok_assert := testing.is_not_string("a", "b", "should not match same string")
        assert ok_assert == true
}

fn test_is_match_on_matched_string(){
        ok_assert := testing.is_match("fox", "The quick brown fox jumps over the lazy dog", "should match substring")
        assert ok_assert == true
}

fn test_is_match_on_nonmatch_string(){
        ok_assert := testing.is_match("wolf", "The quick brown fox jumps over the lazy dog", "should match substring")
        assert ok_assert == false
}
