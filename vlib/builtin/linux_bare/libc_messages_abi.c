#include "libc_compat.h"
#include <libintl.h>
#undef gettext
#undef dgettext
#undef dcgettext
#undef ngettext
#undef dngettext
#undef dcngettext
extern uintptr_t v_message_domain(uintptr_t);
extern uintptr_t v_message_binding(uintptr_t,uintptr_t,int32_t);
extern uintptr_t v_message_translate(uintptr_t,uintptr_t,uintptr_t);
char *textdomain(const char *s) { return (char *)v_message_domain((uintptr_t)s); }
char *bindtextdomain(const char *d,const char *p) { return (char *)v_message_binding((uintptr_t)d,(uintptr_t)p,0); }
char *bind_textdomain_codeset(const char *d,const char *c) { return (char *)v_message_binding((uintptr_t)d,(uintptr_t)c,1); }
char *gettext(const char *s) { return (char *)v_message_translate((uintptr_t)s,0,1); }
char *dgettext(const char *d,const char *s) { (void)d; return (char *)v_message_translate((uintptr_t)s,0,1); }
char *dcgettext(const char *d,const char *s,int c) { (void)d; (void)c; return (char *)v_message_translate((uintptr_t)s,0,1); }
char *ngettext(const char *a,const char *b,unsigned long n) { return (char *)v_message_translate((uintptr_t)a,(uintptr_t)b,n); }
char *dngettext(const char *d,const char *a,const char *b,unsigned long n) { (void)d; return (char *)v_message_translate((uintptr_t)a,(uintptr_t)b,n); }
char *dcngettext(const char *d,const char *a,const char *b,unsigned long n,int c) { (void)d; (void)c; return (char *)v_message_translate((uintptr_t)a,(uintptr_t)b,n); }
