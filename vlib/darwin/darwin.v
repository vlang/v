module darwin

#include <Cocoa/Cocoa.h>
#include <CoreFoundation/CoreFoundation.h>

#flag -framework Cocoa
#flag -framework Carbon

struct C.NSString {}

#include "@VROOT/vlib/darwin/darwin.m"

fn C.nsstring2(s string) voidptr

// macOS and iOS helpers
// pub fn nsstring(s string) *C.NSString {
pub fn nsstring(s string) voidptr {
	return C.nsstring2(s)
	// println('ns $s len=$s.len')
	//# return [ [ NSString alloc ] initWithBytesNoCopy:s.str  length:s.len
	//# encoding:NSUTF8StringEncoding freeWhenDone: false];
	// return 0

	// ns := C.alloc_NSString()
	// return ns.initWithBytesNoCopy(s.str, length: s.len,
	// encoding: NSUTF8StringEncoding,		freeWhenDone: false)
}

// returns absolute path to folder where your resources should / will reside
// for .app packages: .../my.app/Contents/Resources
// for cli: .../parent_folder/Resources

fn C.CFBundleCopyResourcesDirectoryURL(bundle voidptr) byteptr
fn C.CFBundleGetMainBundle() voidptr
fn C.CFURLGetFileSystemRepresentation(url byteptr, resolve_against_base bool, buffer byteptr, buffer_size int) int
fn C.CFRelease(url byteptr)

pub fn resource_path() string {
	main_bundle := C.CFBundleGetMainBundle()
	resource_dir_url := C.CFBundleCopyResourcesDirectoryURL(main_bundle)
	if isnil(resource_dir_url) {
		panic('CFBundleCopyResourcesDirectoryURL failed')
	}
	buffer_size := 4096
	mut buffer := unsafe{ malloc(buffer_size) }
	unsafe{ buffer[0] = 0 }
	conv_result := C.CFURLGetFileSystemRepresentation(resource_dir_url, true, buffer,
		buffer_size)
	if conv_result == 0 {
		panic('CFURLGetFileSystemRepresentation failed')
	}
	result := unsafe { buffer.vstring() }
	C.CFRelease(resource_dir_url)
	return result
}
