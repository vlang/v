#include <stdio.h>
#include <unistd.h>

/* Supply malformed entries and empty values without a shell normalizing them. */
int main(int argc, char **argv) {
    if (argc != 3) return 2;
    char *arguments[] = {argv[1], argv[2], NULL};
    char *environment[] = {
        "FENV_EMPTY=", "FENV_UNICODE=λ🙂", "FENV_EQUALS=left=right",
        "FENV_PREFIXQ", "=invalid-empty-name", "NO_EQUALS", NULL
    };
    execve(argv[1], arguments, environment);
    perror("environment fixture execve");
    return 3;
}
