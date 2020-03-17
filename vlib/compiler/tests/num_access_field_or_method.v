fn int_access_field_or_method() {
    x1 := 1234.str()
    assert x1 == '1234'
    x2 := -0xffff.str()
    assert x2 == '-65535'
    x3 := 0b1001001.str()
    assert x3 == '73'
    x4 := 0o653262.str()
    assert x4 == '218802'
}

fn float_access_field_or_method() {
    x1 := -123.66.str()
    assert x1 == '-1.2366e+02'
    x2 := 12.5e-2.str()
    assert x2 == '1.25e-01'
}
