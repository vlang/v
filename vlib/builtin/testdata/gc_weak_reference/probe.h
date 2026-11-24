#ifndef V_GC_WEAK_REFERENCE_PROBE_H
#define V_GC_WEAK_REFERENCE_PROBE_H
void weak_test_initialize(void);
int weak_test_track(void *);
void weak_test_keep_strong(void *);
void weak_test_clear_strong(void);
int weak_test_collect(void);
void *weak_test_non_gc(void);
#endif
