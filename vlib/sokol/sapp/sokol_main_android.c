// The Android NativeActivity entry point required by sokol_app.h.
//
// V's compiler generates the program's entry as a plain `main()`, and the
// user's code registers the app description through `sapp.run()`, which
// stores a copy in the `sapp__g_desc` global (while `sapp_run()` is an empty
// stub in sokol_app.h's normal entry mode). sokol_app.h's Android glue
// instead requires the program to provide `sokol_main()`, so this glue runs
// the program's `main()` and hands the registered description back to
// sokol_app.h, which then takes over the event loop.
//
// This file is textual C, included into the program's translation unit by
// sokol_main_android.c.v, and only for Android builds.
#include "sokol_app.h"

extern int main(int argc, char *argv[]);
extern sapp_desc sapp__g_desc;

sapp_desc sokol_main(int argc, char *argv[]) {
	(void)argc;
	(void)argv;
	main(argc, argv);
	return sapp__g_desc;
}
