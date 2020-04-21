module uiold

#flag -framework Carbon
#flag -framework Cocoa

#include <Cocoa/Cocoa.h>
#include <Carbon/Carbon.h>

__global default_font &C.NSFont


fn focus_app(next, event, data voidptr) {
  #NSLog(@"2The hot key was pressed.");
  #NSApplication *myApp = [NSApplication sharedApplication];
  #[myApp activateIgnoringOtherApps:YES];
  //return noErr;
}

pub fn reg_key_vid() {
	println('REGISTERING VID KEY')
  #EventHotKeyRef gMyHotKeyRef;

  #EventHotKeyID gMyHotKeyID;
  #EventTypeSpec eventType;
  #eventType.eventClass = kEventClassKeyboard;
  #eventType.eventKind = kEventHotKeyPressed;
  #InstallApplicationEventHandler(&uiold__focus_app, 1, &eventType, NULL, NULL);
  #gMyHotKeyID.signature = 'rml1';
  #gMyHotKeyID.id = 1;
  #RegisterEventHotKey(kVK_ANSI_1, cmdKey, gMyHotKeyID,
                      #GetApplicationEventTarget(), 0, &gMyHotKeyRef);
}
