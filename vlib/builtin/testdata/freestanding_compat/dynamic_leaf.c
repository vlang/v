int v_dynamic_leaf(int value) { return value * 7; }
void v_dynamic_leaf_trace(void (*callback)(void *),void *data) { callback(data); }

__asm__(".globl v_dynamic_zero\n.set v_dynamic_zero,0");
