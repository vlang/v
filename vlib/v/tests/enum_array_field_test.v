struct Abc {
mut:
    flags []Flag
}

enum Flag {
    flag_one
    flag_two
}

fn test_enum_array_field() {
    mut a := Abc{}
    a.flags << .flag_one
    assert true
    a.flags << .flag_two
    assert true
    a.flags << .flag_one
    println(a)
    assert true
}
