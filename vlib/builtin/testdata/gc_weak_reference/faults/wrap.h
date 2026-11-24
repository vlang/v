#ifndef V_GC_WEAK_REFERENCE_WRAP_H
#define V_GC_WEAK_REFERENCE_WRAP_H
void weak_fault_begin(int);
void weak_fault_end(void);
int weak_fault_atomic_calls(void);
int weak_fault_registration_calls(void);
int weak_fault_free_calls(void);
#endif
