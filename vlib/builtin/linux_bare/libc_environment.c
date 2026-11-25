#include "libc_compat.h"
#include "libc_environment.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Constructors see the kernel vector before V initializers run. Only vectors
 * and strings allocated by this implementation are freed on later mutations;
 * putenv deliberately borrows its caller's storage. */
char **environ;
static int v_environment_lock;
static char **v_owned_vector;
struct v_env_string {
    struct v_env_string *next;
    char text[];
};
static struct v_env_string *v_owned_strings;
/* V environment ownership participates in its heap hooks, while vector capacity
 * remains internal C bookkeeping. Hosted sanitizer probes use their allocator. */
extern void *v_freestanding_environment_allocate(size_t) __attribute__((weak));
extern void v_freestanding_environment_release(void *) __attribute__((weak));
static void *v_env_allocate(size_t size) {
    return v_freestanding_environment_allocate ? v_freestanding_environment_allocate(size)
                                               : malloc(size);
}
static void v_env_release(void *p) {
    if (v_freestanding_environment_release)
        v_freestanding_environment_release(p);
    else
        free(p);
}
void v_freestanding_environment_lock(void) { v_linux_lock(&v_environment_lock); }
void v_freestanding_environment_unlock(void) { v_linux_unlock(&v_environment_lock); }
static int v_env_name(const char *name) { return name && *name && !strchr(name, '='); }
static size_t v_env_find(const char *name, size_t length, size_t *count) {
    size_t i = 0, found = SIZE_MAX;
    if (environ)
        for (; environ[i]; ++i)
            if (found == SIZE_MAX && !strncmp(environ[i], name, length) &&
                environ[i][length] == '=')
                found = i;
    *count = i;
    return found;
}
char *v_freestanding_environment_get_locked(const char *name) {
    if (!v_env_name(name))
        return NULL;
    size_t count, at = v_env_find(name, strlen(name), &count);
    return at == SIZE_MAX ? NULL : environ[at] + strlen(name) + 1;
}
char *getenv(const char *name) {
    v_linux_lock(&v_environment_lock);
    char *value = v_freestanding_environment_get_locked(name);
    v_linux_unlock(&v_environment_lock);
    return value;
}
static void v_env_retire(char *text) {
    struct v_env_string **p = &v_owned_strings;
    while (*p && (*p)->text != text)
        p = &(*p)->next;
    if (*p) {
        struct v_env_string *old = *p;
        *p = old->next;
        v_env_release(old);
    }
}
static int v_env_install(char *text, size_t length, int overwrite, struct v_env_string *owned) {
    size_t count, at = v_env_find(text, length, &count);
    if (at != SIZE_MAX && !overwrite) {
        v_env_release(owned);
        return 0;
    }
    if (at == SIZE_MAX || environ != v_owned_vector) {
        if (count > SIZE_MAX / sizeof(char *) - 2) {
            v_env_release(owned);
            errno = ENOMEM;
            return -1;
        }
        char **vector = malloc((count + 2) * sizeof *vector);
        if (!vector) {
            v_env_release(owned);
            errno = ENOMEM;
            return -1;
        }
        if (count)
            memcpy(vector, environ, count * sizeof *vector);
        vector[count] = vector[count + 1] = NULL;
        free(v_owned_vector);
        environ = v_owned_vector = vector;
    }
    if (at == SIZE_MAX)
        at = count;
    else if (environ[at] != text)
        v_env_retire(environ[at]);
    environ[at] = text;
    if (owned) {
        owned->next = v_owned_strings;
        v_owned_strings = owned;
    }
    return 0;
}
int setenv(const char *name, const char *value, int overwrite) {
    if (!v_env_name(name)) {
        errno = EINVAL;
        return -1;
    }
    size_t n = strlen(name), v = strlen(value), count;
    if (n > SIZE_MAX - v - sizeof(struct v_env_string) - 2) {
        errno = ENOMEM;
        return -1;
    }
    v_linux_lock(&v_environment_lock);
    size_t at = v_env_find(name, n, &count);
    if (at != SIZE_MAX && (!overwrite || !strcmp(environ[at] + n + 1, value))) {
        v_linux_unlock(&v_environment_lock);
        return 0;
    }
    struct v_env_string *s = v_env_allocate(sizeof *s + n + v + 2);
    if (!s) {
        v_linux_unlock(&v_environment_lock);
        errno = ENOMEM;
        return -1;
    }
    memcpy(s->text, name, n);
    s->text[n] = '=';
    memcpy(s->text + n + 1, value, v + 1);
    int result = v_env_install(s->text, n, overwrite, s);
    v_linux_unlock(&v_environment_lock);
    return result;
}
int unsetenv(const char *name) {
    if (!v_env_name(name)) {
        errno = EINVAL;
        return -1;
    }
    size_t n = strlen(name);
    v_linux_lock(&v_environment_lock);
    if (environ)
        for (size_t i = 0; environ[i];) {
            if (!strncmp(environ[i], name, n) && environ[i][n] == '=') {
                if (environ != v_owned_vector) {
                    size_t count = 0;
                    while (environ[count])
                        ++count;
                    char **vector = malloc((count + 1) * sizeof *vector);
                    if (!vector) {
                        v_linux_unlock(&v_environment_lock);
                        errno = ENOMEM;
                        return -1;
                    }
                    memcpy(vector, environ, (count + 1) * sizeof *vector);
                    free(v_owned_vector);
                    environ = v_owned_vector = vector;
                }
                v_env_retire(environ[i]);
                size_t j = i;
                do {
                    environ[j] = environ[j + 1];
                } while (environ[j++]);
            } else
                ++i;
        }
    v_linux_unlock(&v_environment_lock);
    return 0;
}
int putenv(char *text) {
    char *eq = strchr(text, '=');
    if (!eq)
        return unsetenv(text);
    if (eq == text) {
        errno = EINVAL;
        return -1;
    }
    v_linux_lock(&v_environment_lock);
    int result = v_env_install(text, eq - text, 1, NULL);
    v_linux_unlock(&v_environment_lock);
    return result;
}
int clearenv(void) {
    v_linux_lock(&v_environment_lock);
    environ = NULL;
    free(v_owned_vector);
    v_owned_vector = NULL;
    while (v_owned_strings) {
        struct v_env_string *s = v_owned_strings;
        v_owned_strings = s->next;
        v_env_release(s);
    }
    v_linux_unlock(&v_environment_lock);
    return 0;
}
