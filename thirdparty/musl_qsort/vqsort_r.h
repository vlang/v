
typedef int (*vqsort_r_cmpfun)(const void *, const void *, void *);
void vqsort_r(void *base, size_t nel, size_t width, vqsort_r_cmpfun cmp, void *arg);
