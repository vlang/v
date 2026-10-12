#include <Cocoa/Cocoa.h>
#include <stdint.h>
#include <stdlib.h>

// See this tutorial to learn about NSApplicationDelegate:
// https://bumbershootsoft.wordpress.com/2018/11/22/unfiltered-cocoa-completing-the-application/

typedef void (*tray_click_callback)(void*, int32_t);
typedef struct tray_info {
  NSApplication *app;
  id app_delegate;
} tray_info;

// Manages the app lifecycle.
@interface AppDelegate : NSObject <NSApplicationDelegate> {
  NSAutoreleasePool *pool;
  NSApplication *app;
  NSStatusItem *statusItem;    // our button
  NSArray *titles;
  tray_click_callback on_click;
  void *user_data;
}
@end

@implementation AppDelegate
- (AppDelegate *)initWithTitles:(const char *const *)labels
                         count:(int32_t)count
                      callback:(tray_click_callback)callback
                      userData:(void *)data {
  if (self = [super init]) {
    NSMutableArray *items = [NSMutableArray arrayWithCapacity:count];
    for (int32_t i = 0; i < count; i++) {
      NSString *title = [NSString stringWithUTF8String:labels[i]];
      [items addObject:title ? title : @""];
    }
    titles = [items copy];
    on_click = callback;
    user_data = data;
  }
  return self;
}

- (void)dealloc {
  [titles release];
  [super dealloc];
}

// Called when NSMenuItem is clicked. V owns the menu items and resolves the index.
- (void)onAction:(id)sender {
  if (on_click) {
    on_click(user_data, (int32_t)[sender tag]);
  }
}

- (NSMenu *)buildMenu {
  NSMenu *menu = [[[NSMenu alloc] init] autorelease];
  [menu setAutoenablesItems:NO];
  for (NSUInteger i = 0; i < [titles count]; i++) {
    NSMenuItem *item = [menu addItemWithTitle:[titles objectAtIndex:i]
                                       action:@selector(onAction:)
                                keyEquivalent:@""];
    [item setTag:(NSInteger)i];
    [item setTarget:self];
    [item setEnabled:YES];
  }
  return menu;
}

- (void)initTrayMenuItem {
  NSStatusBar *statusBar = [NSStatusBar systemStatusBar];
  statusItem = [statusBar statusItemWithLength:NSSquareStatusItemLength];
  [statusItem retain];
  [statusItem setVisible:YES];
  NSStatusBarButton *statusBarButton = [statusItem button];

  // Height must be 22px.
  NSImage *img = [NSImage imageNamed:@"icon.png"];
  [statusBarButton setImage:img];
  NSMenu *menu = [self buildMenu];
  [statusItem setMenu:menu];
}

- (void)applicationWillFinishLaunching:(NSNotification *)notification {
  NSLog(@"applicationWillFinishLaunching called");
}

- (void)applicationWillTerminate:(NSNotification *)notif;
{ NSLog(@"applicationWillTerminate called"); }

- (NSApplicationTerminateReply)applicationShouldTerminate:
    (NSApplication *)sender {
  NSLog(@"applicationShouldTerminate called");
  return NSTerminateNow;
}
@end

// Initializes NSApplication and NSStatusItem, aka system tray menu item.
void *tray_app_init(const char *const *titles, int32_t count, tray_click_callback callback, void *user_data) {
  NSApplication *app = [NSApplication sharedApplication];
  AppDelegate *appDelegate = [[AppDelegate alloc] initWithTitles:titles count:count callback:callback userData:user_data];

  // Hide icon from the doc.
  [app setActivationPolicy:NSApplicationActivationPolicyProhibited];
  [app setDelegate:appDelegate];

  [appDelegate initTrayMenuItem];

  tray_info *info = malloc(sizeof(tray_info));
  info->app = app;
  info->app_delegate = appDelegate;
  return info;
}

// Blocks and runs the application.
void tray_app_run(void *handle) {
  NSApplication *app = ((tray_info *)handle)->app;
  [app run];
}

// Processes a single NSEvent while blocking the thread
// until there is an event.
void tray_app_loop(void *handle) {
  NSDate *until = [NSDate distantFuture];

  NSApplication *app = ((tray_info *)handle)->app;
  NSEvent *event = [app nextEventMatchingMask:ULONG_MAX
                                    untilDate:until
                                       inMode:@"kCFRunLoopDefaultMode"
                                      dequeue:YES];

  if (event) {
    [app sendEvent:event];
  }
}

// Terminates the app.
void tray_app_exit(void *handle) {
  NSApplication *app = ((tray_info *)handle)->app;
  [app terminate:app];
}
