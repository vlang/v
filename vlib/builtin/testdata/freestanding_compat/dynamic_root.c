extern int v_dynamic_leaf(int);
static int ready;
__attribute__((constructor)) static void initialize(void) { ready=1; }
int v_dynamic_root(int value) { return ready ? v_dynamic_leaf(value)+1 : -1; }
extern void v_dynamic_leaf_trace(void (*)(void *),void *);
void v_dynamic_root_trace(void (*callback)(void *),void *data) { v_dynamic_leaf_trace(callback,data); }
