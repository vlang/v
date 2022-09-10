import strings

fn test_int_formatting() {
    assert "-10,000,000,000" == strings.format_int(-10000000000, ",")
}

fn test_uint_formatting() {
    assert "100,000" == strings.format_uint(100000, ",")
}

fn test_f32_formatting() {
    assert "10,000.45" == strings.format_f32(10000.45, ",", ".")
}

fn test_f64_formatting() {
    assert "10,000.45676576789" == strings.format_f64(10000.45676576789, ",", ".")
}
