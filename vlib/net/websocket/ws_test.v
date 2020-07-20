module websocket

// TODO This only checks that the client compiles, do real tests
fn test_compile() {
	new('ws://echo.websocket.org')//wss://echo.websocket.org
}
