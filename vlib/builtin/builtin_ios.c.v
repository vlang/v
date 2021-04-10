module builtin

// TODO: Remove this later, added to make sure v self works
$if ios {
	#include "@VROOT/thirdparty/ios/ios.m"
}

fn C.WrappedNSLog(str &byte)