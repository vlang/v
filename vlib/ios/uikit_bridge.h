#ifndef IOS_UIKIT_BRIDGE_H
#define IOS_UIKIT_BRIDGE_H

#import <UIKit/UIKit.h>

static inline int ios_ui_application_main(int argc, char** argv, void* principal, void* delegate) {
	return UIApplicationMain(argc, argv, (__bridge NSString*)principal, (__bridge NSString*)delegate);
}

static inline void* ios_color_from_hex(unsigned int hex) {
	CGFloat r = ((hex >> 16) & 0xFF) / 255.0;
	CGFloat g = ((hex >> 8) & 0xFF) / 255.0;
	CGFloat b = (hex & 0xFF) / 255.0;
	return (void*)[UIColor colorWithRed:r green:g blue:b alpha:1.0];
}

static inline void* ios_color_from_hex_alpha(unsigned int hex, double alpha) {
	CGFloat r = ((hex >> 16) & 0xFF) / 255.0;
	CGFloat g = ((hex >> 8) & 0xFF) / 255.0;
	CGFloat b = (hex & 0xFF) / 255.0;
	return (void*)[UIColor colorWithRed:r green:g blue:b alpha:alpha];
}

static inline long long ios_index_path_row(void* index_path) {
	return [((NSIndexPath*)index_path) row];
}

static inline void* ios_index_path_for_row(long long row, long long section) {
	return (void*)[NSIndexPath indexPathForRow:row inSection:section];
}

#endif
