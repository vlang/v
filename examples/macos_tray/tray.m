// #include <Cocoa/Cocoa.h> // Uncomment for C/C++ intellisense

// See this tutorial to learn about NSApplicationDelegate:
// https://bumbershootsoft.wordpress.com/2018/11/22/unfiltered-cocoa-completing-the-application/

static NSString *nsstring(string s) {
  return [[NSString alloc] initWithBytesNoCopy:s.str
                                        length:s.len
                                      encoding:NSUTF8StringEncoding
                                  freeWhenDone:NO];
}

// Manages the app lifecycle.
@interface AppDelegate : NSObject <NSApplicationDelegate> {
  NSAutoreleasePool *pool;
  NSApplication *app;
  NSStatusItem *statusItem;    // our button
  main__TrayParams trayParams; // TrayParams is defined in tray.v
}
@end

@implementation AppDelegate
- (AppDelegate *)initWithParams:(main__TrayParams)params {
  if (self = [super init]) {
    trayParams = params;
  }
  return self;
}

// Called when NSMenuItem is clicked.
- (void)onAction:(id)sender {
  struct main__TrayMenuItem *item =
      (struct main__TrayMenuItem *)[[sender representedObject] pointerValue];
  if (item) {
    trayParams.on_click(*item);
  }
}

- (NSMenu *)buildMenu {
  NSMenu *menu = [NSMenu new];
  [menu autorelease];
  [menu setAutoenablesItems:NO];

  main__TrayMenuItem *params_items = trayParams.items.data;
  for (int i = 0; i < trayParams.items.len; i++) {
    NSString *title = nsstring(params_items[i].text);
    NSMenuItem *item = [menu addItemWithTitle:title
                                       action:@selector(onAction:)
                                keyEquivalent:@""];
    NSValue *representedObject = [NSValue valueWithPointer:(params_items + i)];
    [item setRepresentedObject:representedObject];
    [item setTarget:self];
    [item autorelease];
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
main__TrayInfo *tray_app_init(main__TrayParams params) {
  NSApplication *app = [NSApplication sharedApplication];
  AppDelegate *appDelegate = [[AppDelegate alloc] initWithParams:params];

  // Hide icon from the doc.
  [app setActivationPolicy:NSApplicationActivationPolicyProhibited];
  [app setDelegate:appDelegate];

  [appDelegate initTrayMenuItem];

  main__TrayInfo *tray_info = malloc(sizeof(main__TrayInfo));
  tray_info->app = app;
  tray_info->app_delegate = appDelegate;
  return tray_info;
}

// Blocks and runs the application.
void tray_app_run(main__TrayInfo *tray_info) {
  NSApplication *app = (NSApplication *)(tray_info->app);
  [app run];
}

// Processes a single NSEvent while blocking the thread
// until there is an event.
void tray_app_loop(main__TrayInfo *tray_info) {
  NSDate *until = [NSDate distantFuture];

  NSApplication *app = (NSApplication *)(tray_info->app);
  NSEvent *event = [app nextEventMatchingMask:ULONG_MAX
                                    untilDate:until
                                       inMode:@"kCFRunLoopDefaultMode"
                                      dequeue:YES];

  if (event) {
    [app sendEvent:event];
  }
}

// Terminates the app.
void tray_app_exit(main__TrayInfo *tray_info) {
  NSApplication *app = (NSApplication *)(tray_info->app);
  [app terminate:app];
}
