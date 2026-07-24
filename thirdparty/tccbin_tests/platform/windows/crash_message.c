/* Windows-specific: same fault as shared/crash.c, but additionally
   verifies tcc's own SEH-based diagnostic message. The exact wording
   is a tcc-on-Windows implementation detail (tccrun.c's
   cpu_exception_handler), not something other platforms are expected
   to reproduce - that's why this lives here and not in shared/. */
int main(void) {
    int *p = (int *)0;
    return *p;
}
