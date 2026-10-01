# Cocoa NSFont bindings

On macOS, an active include of `Cocoa/Cocoa.h`, `AppKit/AppKit.h`, or `AppKit/NSFont.h`
makes an opaque `C.NSFont` binding refer to the Objective-C class.
The compiler evaluates the program's ordered preprocessor directives, including
conditional branches, local macro values, and `-D`/`-U` flags, to ignore inactive includes.
This check uses the program's directives and does not read framework headers.
An include in a branch whose header-provided macros are unknown remains conservatively possible.
