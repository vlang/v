#include <glob.h>
#include <regex.h>
#include <string.h>

static int v_test_musl_headers(void) {
    regex_t expression;
    regmatch_t match[2];
    if (regcomp(&expression, "(ab)", REG_EXTENDED)) return 1;
    int failed = expression.re_nsub != 1 || regexec(&expression, "cab", 2, match, 0)
                 || match[1].rm_so != 1 || match[1].rm_eo != 3;
    regfree(&expression);
    if (failed) return 2;

    glob_t paths = {.gl_offs = 1};
    if (glob("/dev/nu[l]l", GLOB_DOOFFS, NULL, &paths)) return 3;
    failed = paths.gl_pathc != 1 || paths.gl_offs != 1 || paths.gl_pathv[0]
             || strcmp(paths.gl_pathv[1], "/dev/null") || paths.gl_pathv[2];
    if (!failed) {
        failed = glob("missing-file", GLOB_DOOFFS | GLOB_APPEND | GLOB_NOCHECK, NULL, &paths)
                 || paths.gl_pathc != 2 || strcmp(paths.gl_pathv[2], "missing-file")
                 || paths.gl_pathv[3];
    }
    globfree(&paths);
    return failed ? 4 : 0;
}
