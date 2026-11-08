#!/bin/sh
# The C compiler must start after V exits; failed builds must preserve the old binary.
set -eu
repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
work=$(mktemp -d "${TMPDIR:-/tmp}/v-prod-build.XXXXXX")
trap 'rm -rf "$work"' EXIT
cat > "$work/compiler" <<'MOCK'
#!/bin/sh
set -eu
[ "$1" = self ] && [ "$2" = -prod ] && [ "$3" = -cc ] && [ "$4" = 'compiler with spaces' ]
[ "$5" = -parallel-cc ] && [ "$6" = -generate-c-project ]
project=$7
[ "$8" = -o ] && [ "$9" = "$WORK/vprod" ]
echo "$project" > "$WORK/project"
echo $$ > "$WORK/generator.pid"
[ "${FAIL_STAGE:-}" != generate ] || exit 7
cat > "$project/Makefile" <<'MAKE'
all:
	@if kill -0 "$$(cat "$$WORK/generator.pid")" 2>/dev/null; then echo 'V is still running'; exit 90; fi
	@test "$${FAIL_STAGE:-}" != compile || exit 8
	@printf '#!/bin/sh\necho optimized\n' > v
MAKE
sleep 0.2
MOCK
chmod +x "$work/compiler"
for stage in success generate compile; do
    printf original > "$work/vprod"
    status=0
    WORK="$work" FAIL_STAGE="$stage" sh "$repo/cmd/tools/build_vprod.sh" \
        "$work/compiler" "$work/vprod" 'compiler with spaces' > "$work/log" 2>&1 || status=$?
    case "$stage" in
        success) [ "$status" -eq 0 ] && [ "$("$work/vprod")" = optimized ] ;;
        *) [ "$status" -ne 0 ] && [ "$(cat "$work/vprod")" = original ] ;;
    esac
    [ ! -e "$(cat "$work/project")" ]
done
echo 'sequential production build, cleanup, and failure propagation passed'
