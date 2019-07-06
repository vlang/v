import encoding.base64

pub fn test_submodules() {
    assert base64.decode('c3VibW9kdWxlcyBhcmUgd29ya2luZyE=') == 'submodules are working!'
}
