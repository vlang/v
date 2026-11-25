#ifndef V_LINUX_BARE_INITIALIZERS_H
#define V_LINUX_BARE_INITIALIZERS_H
void v_bare_run_initializers(int argc, char **argv, void (*loader_fini)(void));
void v_bare_run_finalizers(void);
#endif
