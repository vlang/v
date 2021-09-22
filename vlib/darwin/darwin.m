
///void NSLog(id x);

#include <Cocoa/Cocoa.h>

NSString* nsstring2(string s) {
	return [ [ NSString alloc ] initWithBytesNoCopy:s.str  length:s.len
	  encoding:NSUTF8StringEncoding freeWhenDone: false];
}
