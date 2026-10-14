module builtin

// TODO: Remove this later, added to make sure v self works
$if ios {
	#flag ios @VEXEROOT/thirdparty/ios/ios.o
	#flag ios -framework Foundation
}
