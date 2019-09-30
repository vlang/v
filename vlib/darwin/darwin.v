module darwin

#include <Cocoa/Cocoa.h>
#include <CoreFoundation/CoreFoundation.h>

#flag -framework Cocoa

struct C.NSString { }

// macOS and iOS helpers
pub fn nsstring(s string) *C.NSString {
	// println('ns $s len=$s.len')
	# return [ [ NSString alloc ] initWithBytesNoCopy:s.str  length:s.len
	# encoding:NSUTF8StringEncoding freeWhenDone: false];
	return 0
	
	//ns := C.alloc_NSString()
	//return ns.initWithBytesNoCopy(s.str, length: s.len,
		//encoding: NSUTF8StringEncoding,		freeWhenDone: false)
}

// returns absolute path to folder where your resources should / will reside
// for .app packages: .../my.app/Contents/Resources
// for cli: .../parent_folder/Resources
pub fn resource_path() string {

	main_bundle := C.CFBundleGetMainBundle()
	resource_dir_url := C.CFBundleCopyResourcesDirectoryURL(main_bundle)
	assert !isnil(resource_dir_url)
	buffer_size := 4096
	mut buffer := malloc(buffer_size)
	buffer[0] = 0
	conv_result := C.CFURLGetFileSystemRepresentation(resource_dir_url, true, buffer, buffer_size)
	assert conv_result
	result := string(buffer)
	C.CFRelease(resource_dir_url)
	return result
}

