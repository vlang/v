#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
umask 077

readonly vc_repository_allowlisted='https://github.com/vlang/vc'
readonly bootstrap_lock_relative='thirdparty/tccbin_automation/bootstrap/vc.lock'

die() {
	printf '%s\n' "tccbin bootstrap: $*" >&2
	exit 1
}

usage() {
	printf '%s\n' 'usage: bootstrap.sh <contract-root> <contract-repository> <contract-sha> <vc-source-root> <work-root>' >&2
	exit 2
}

[[ $# -eq 5 ]] || usage

contract_root_input=$1
contract_repository=$2
contract_sha=$3
vc_root_input=$4
work_root_input=$5

case "$contract_repository" in
	GGRei/v | vlang/v) ;;
	*) die 'contract repository is not allowlisted' ;;
esac
[[ "$contract_sha" =~ ^[0-9a-f]{40}$ ]] || die 'contract SHA must be a full lowercase commit SHA'

while IFS= read -r environment_name; do
	environment_name_upper=$(printf '%s' "$environment_name" \
		| LC_ALL=C tr '[:lower:]' '[:upper:]') \
		|| die 'environment variable name cannot be normalized'
	case "$environment_name_upper" in
		GIT_DIR | GIT_WORK_TREE | GIT_COMMON_DIR | GIT_INDEX_FILE | GIT_OBJECT_DIRECTORY \
			| GIT_ALTERNATE_OBJECT_DIRECTORIES | GIT_REPLACE_REF_BASE | GIT_GRAFT_FILE \
			| GIT_SHALLOW_FILE | GIT_NAMESPACE | GIT_EXEC_PATH | GIT_CONFIG \
			| GIT_CONFIG_PARAMETERS | GIT_CONFIG_COUNT | GIT_CONFIG_SYSTEM | GIT_CONFIG_GLOBAL \
			| GIT_CONFIG_NOSYSTEM | GIT_TEMPLATE_DIR | GIT_NO_LAZY_FETCH | GIT_TERMINAL_PROMPT \
			| VFLAGS | VMODULES | VCACHE | VTMP | VROOT | VEXE | VCHILD | VOSARGS \
			| VBUILD_DEFINES | VBUILD_FACTS | VNORUN | VJOBS | CFLAGS | CPPFLAGS | LDFLAGS)
			die "${environment_name_upper} must be unset"
			;;
		GIT_CONFIG_KEY_* | GIT_CONFIG_VALUE_*)
			die 'Git configuration injection variables must be unset'
			;;
	esac
done < <(compgen -e)
export GIT_NO_LAZY_FETCH=1
export GIT_TERMINAL_PROMPT=0
export GIT_CONFIG_NOSYSTEM=1
export GIT_CONFIG_SYSTEM=/dev/null
export GIT_CONFIG_GLOBAL=/dev/null

physical_directory() {
	local candidate=$1
	[[ -d "$candidate" && ! -L "$candidate" ]] || die 'checkout root must be a physical directory'
	(cd -P -- "$candidate" && pwd -P) || die 'checkout root cannot be resolved'
}

canonical_new_directory() {
	local candidate=$1
	local parent base
	[[ "$candidate" != *$'\n'* && "$candidate" != *$'\r'* && "$candidate" != *$'\t'* ]] \
		|| die 'work root contains a control character'
	parent=$(dirname -- "$candidate")
	base=$(basename -- "$candidate")
	[[ "$base" != '.' && "$base" != '..' && -n "$base" ]] || die 'work root basename is invalid'
	parent=$(physical_directory "$parent")
	printf '%s/%s\n' "$parent" "$base"
}

path_key() {
	case "$(uname -s)" in
		Darwin | MINGW* | MSYS* | CYGWIN*) printf '%s' "$1" | tr '[:upper:]' '[:lower:]' ;;
		*) printf '%s' "$1" ;;
	esac
}

roots_overlap() {
	local left right
	left=$(path_key "$1")
	right=$(path_key "$2")
	[[ "$left" == "$right" || "$left" == "$right/"* || "$right" == "$left/"* ]]
}

git_checked() {
	local root=$1
	shift
	git --no-replace-objects -C "$root" -c core.autocrlf=false -c core.fsmonitor=false \
		-c core.hooksPath=/dev/null "$@"
}

validate_checkout() {
	local root=$1
	local expected_repository_url=$2
	local expected_sha=$3
	local actual remote status replacements symbolic_rc local_config config_name config_name_lower \
		git_dir common_dir

	actual=$(git_checked "$root" rev-parse --is-inside-work-tree) || die 'Git checkout cannot be inspected'
	[[ "$actual" == 'true' ]] || die 'checkout is not a Git worktree'
	actual=$(git_checked "$root" rev-parse --show-toplevel) || die 'checkout top level cannot be resolved'
	[[ "$(path_key "$(physical_directory "$actual")")" == "$(path_key "$root")" ]] \
		|| die 'checkout root differs from its Git top level'
	actual=$(git_checked "$root" rev-parse --show-object-format) || die 'checkout object format cannot be resolved'
	[[ "$actual" == 'sha1' ]] || die 'checkout must use the SHA-1 Git object format'
	actual=$(git_checked "$root" rev-parse --is-shallow-repository) || die 'checkout shallow state cannot be resolved'
	[[ "$actual" == 'false' ]] || die 'checkout must be an exact non-shallow snapshot or capsule'
	actual=$(git_checked "$root" config --local --get core.autocrlf) || die 'checkout must set core.autocrlf=false locally'
	[[ "$actual" == 'false' ]] || die 'checkout must set core.autocrlf=false locally'
	remote=$(git_checked "$root" remote get-url origin) || die 'checkout origin cannot be resolved'
	[[ "$remote" == "$expected_repository_url" || "$remote" == "${expected_repository_url}.git" ]] \
		|| die 'checkout origin is not the allowlisted repository'
	actual=$(git_checked "$root" rev-parse --verify "${expected_sha}^{commit}") \
		|| die 'checkout commit cannot be resolved'
	[[ "$actual" == "$expected_sha" ]] || die 'checkout commit resolution differs from the lock'
	actual=$(git_checked "$root" rev-parse HEAD) || die 'checkout HEAD cannot be resolved'
	[[ "$actual" == "$expected_sha" ]] || die 'checkout HEAD differs from the lock'
	set +e
	git_checked "$root" symbolic-ref -q HEAD >/dev/null 2>&1
	symbolic_rc=$?
	set -e
	[[ $symbolic_rc -eq 1 ]] || die 'checkout HEAD must be detached'
	status=$(git_checked "$root" status --porcelain=v1 --untracked-files=all --ignored=matching) \
		|| die 'checkout status cannot be inspected'
	[[ -z "$status" ]] || die 'checkout must be clean, including ignored files'
	replacements=$(git_checked "$root" for-each-ref --format='%(refname)' refs/replace/) \
		|| die 'replacement refs cannot be inspected'
	[[ -z "$replacements" ]] || die 'replacement refs are forbidden'
	git_dir=$(git_checked "$root" rev-parse --absolute-git-dir) \
		|| die 'checkout Git directory cannot be resolved'
	common_dir=$(git_checked "$root" rev-parse --path-format=absolute --git-common-dir) \
		|| die 'checkout Git common directory cannot be resolved'
	git_dir=$(physical_directory "$git_dir")
	common_dir=$(physical_directory "$common_dir")
	for redirect_path in "$git_dir/info/grafts" "$common_dir/info/grafts" \
		"$git_dir/objects/info/alternates" "$common_dir/objects/info/alternates"; do
		[[ ! -e "$redirect_path" && ! -L "$redirect_path" ]] \
			|| die 'checkout contains a graft or object alternate'
	done
	set +e
	local_config=$(git_checked "$root" config --local --name-only --get-regexp '.*')
	symbolic_rc=$?
	set -e
	[[ $symbolic_rc -eq 0 || $symbolic_rc -eq 1 ]] \
		|| die 'checkout local configuration cannot be inspected'
	while IFS= read -r config_name; do
		[[ -n "$config_name" ]] || continue
		config_name_lower=$(printf '%s' "$config_name" \
			| LC_ALL=C tr '[:upper:]' '[:lower:]') \
			|| die 'checkout local configuration name cannot be normalized'
		case "$config_name_lower" in
			filter.* | include.* | includeif.* | core.attributesfile | core.hookspath \
				| core.fsmonitor | init.templatedir)
				die 'checkout local configuration contains a filter, include, hook, or template override'
				;;
		esac
	done <<< "$local_config"
}

read_lock() {
	local lock_path=$1
	local line actual_size canonical_size
	lock_lines=()
	while IFS= read -r line || [[ -n "$line" ]]; do
		lock_lines+=("$line")
	done < "$lock_path"
	[[ ${#lock_lines[@]} -eq 6 ]] || die 'VC lock must contain exactly six records'
	[[ "${lock_lines[0]}" == 'format=vc-lock-v1' ]] || die 'VC lock format record is invalid'
	[[ "${lock_lines[1]}" == repository=* ]] || die 'VC lock repository record is invalid'
	[[ "${lock_lines[2]}" == commit=* ]] || die 'VC lock commit record is invalid'
	[[ "${lock_lines[3]}" == tree=* ]] || die 'VC lock tree record is invalid'
	[[ "${lock_lines[4]}" == v.c=* ]] || die 'VC lock v.c record is invalid'
	[[ "${lock_lines[5]}" == v_win.c=* ]] || die 'VC lock v_win.c record is invalid'
	lock_repository=${lock_lines[1]#repository=}
	lock_commit=${lock_lines[2]#commit=}
	lock_tree=${lock_lines[3]#tree=}
	lock_v_c=${lock_lines[4]#v.c=}
	lock_v_win_c=${lock_lines[5]#v_win.c=}
	[[ "$lock_repository" == "$vc_repository_allowlisted" ]] || die 'VC lock repository is not allowlisted'
	[[ "$lock_commit" =~ ^[0-9a-f]{40}$ ]] || die 'VC lock commit is not a full lowercase SHA'
	[[ "$lock_tree" =~ ^[0-9a-f]{40}$ ]] || die 'VC lock tree is not a full lowercase SHA'
	actual_size=$(wc -c < "$lock_path") || die 'VC lock byte size cannot be read'
	actual_size=${actual_size//[[:space:]]/}
	canonical_size=0
	for line in "${lock_lines[@]}"; do
		canonical_size=$((canonical_size + ${#line} + 1))
	done
	[[ "$actual_size" == "$canonical_size" ]] || die 'VC lock bytes are not canonical'
}

parse_artifact_record() {
	local record=$1
	local label=$2
	local -a fields
	local old_ifs=$IFS
	IFS=' '
	read -r -a fields <<< "$record"
	IFS=$old_ifs
	[[ ${#fields[@]} -eq 4 && "$record" == "${fields[0]} ${fields[1]} ${fields[2]} ${fields[3]}" ]] \
		|| die "VC lock ${label} tuple is not canonical"
	[[ "${fields[0]}" == '100644' ]] || die "VC lock ${label} mode is invalid"
	[[ "${fields[1]}" =~ ^[0-9a-f]{40}$ ]] || die "VC lock ${label} blob is invalid"
	[[ "${fields[2]}" =~ ^[1-9][0-9]*$ ]] || die "VC lock ${label} size is invalid"
	[[ "${fields[3]}" =~ ^[0-9a-f]{64}$ ]] || die "VC lock ${label} SHA-256 is invalid"
	artifact_mode=${fields[0]}
	artifact_blob=${fields[1]}
	artifact_size=${fields[2]}
	artifact_sha256=${fields[3]}
}

sha256_file() {
	local path=$1
	local value
	if command -v sha256sum >/dev/null 2>&1; then
		value=$(sha256sum -- "$path") || die 'sha256sum failed'
		printf '%s\n' "${value%% *}"
	elif command -v shasum >/dev/null 2>&1; then
		value=$(shasum -a 256 -- "$path") || die 'shasum failed'
		printf '%s\n' "${value%% *}"
	elif command -v sha256 >/dev/null 2>&1; then
		sha256 -q -- "$path" || die 'sha256 failed'
	else
		die 'no allowlisted SHA-256 implementation is available'
	fi
}

validate_vc_artifact() {
	local vc_root=$1
	local path=$2
	local record=$3
	local tree_record actual_mode actual_type actual_blob actual_path actual_size actual_sha actual_git_blob object_type
	parse_artifact_record "$record" "$path"
	object_type=$(git_checked "$vc_root" cat-file -t "$artifact_blob") \
		|| die "VC ${path} Git object is not a local blob"
	[[ "$object_type" == 'blob' ]] || die "VC ${path} Git object is not a local blob"
	tree_record=$(git_checked "$vc_root" ls-tree "$lock_commit" -- "$path") \
		|| die "VC ${path} Git tuple cannot be inspected"
	IFS=$' \t' read -r actual_mode actual_type actual_blob actual_path <<< "$tree_record"
	[[ "$actual_mode" == "$artifact_mode" && "$actual_type" == 'blob' \
		&& "$actual_blob" == "$artifact_blob" && "$actual_path" == "$path" \
		&& "$tree_record" == "${artifact_mode} blob ${artifact_blob}"$'\t'"${path}" ]] \
		|| die "VC ${path} Git tuple differs from the lock"
	[[ -f "$vc_root/$path" && ! -L "$vc_root/$path" ]] || die "VC ${path} is not a regular file"
	actual_size=$(wc -c < "$vc_root/$path") || die "VC ${path} size cannot be read"
	actual_size=${actual_size//[[:space:]]/}
	[[ "$actual_size" == "$artifact_size" ]] || die "VC ${path} size differs from the lock"
	actual_sha=$(sha256_file "$vc_root/$path")
	[[ "$actual_sha" == "$artifact_sha256" ]] || die "VC ${path} SHA-256 differs from the lock"
	actual_git_blob=$(git_checked "$vc_root" hash-object --no-filters -- "$path") \
		|| die "VC ${path} blob cannot be recomputed"
	[[ "$actual_git_blob" == "$artifact_blob" ]] || die "VC ${path} bytes differ from the locked Git blob"
}

materialize_vc_artifact() {
	local vc_root=$1
	local record=$2
	local destination=$3
	parse_artifact_record "$record" 'selected bootstrap source'
	git_checked "$vc_root" cat-file blob "$artifact_blob" > "$destination" \
		|| die 'locked VC blob cannot be materialized without lazy fetching'
	chmod 600 "$destination" 2>/dev/null || true
	validate_materialized_vc_artifact "$vc_root" "$destination"
}

validate_materialized_vc_artifact() {
	local vc_root=$1
	local path=$2
	local actual_size actual_sha actual_git_blob
	[[ -f "$path" && ! -L "$path" ]] || die 'materialized VC source is not a regular file'
	actual_size=$(wc -c < "$path") || die 'materialized VC source size cannot be read'
	actual_size=${actual_size//[[:space:]]/}
	[[ "$actual_size" == "$artifact_size" ]] || die 'materialized VC source size differs from the lock'
	actual_sha=$(sha256_file "$path")
	[[ "$actual_sha" == "$artifact_sha256" ]] || die 'materialized VC source SHA-256 differs from the lock'
	actual_git_blob=$(git_checked "$vc_root" hash-object --no-filters -- "$path") \
		|| die 'materialized VC source blob cannot be recomputed'
	[[ "$actual_git_blob" == "$artifact_blob" ]] \
		|| die 'materialized VC source differs from the locked Git blob'
}

clone_contract_checkout() {
	local source_root=$1
	local destination_root=$2
	local expected_url=$3
	local expected_sha=$4
	local template_root=$5
	git --no-replace-objects -c core.autocrlf=false -c core.fsmonitor=false \
		-c core.hooksPath=/dev/null -c protocol.file.allow=always clone --quiet --no-checkout \
		--no-local --no-hardlinks --template="$template_root" "$source_root" "$destination_root" \
		|| die 'private contract clone failed without lazy fetching'
	git_checked "$destination_root" config --local core.autocrlf false \
		|| die 'private contract clone cannot disable autocrlf'
	git_checked "$destination_root" remote set-url origin "$expected_url" \
		|| die 'private contract clone origin cannot be normalized'
	git_checked "$destination_root" checkout --quiet --detach --force "$expected_sha" \
		|| die 'private contract clone cannot checkout the exact contract SHA'
	validate_checkout "$destination_root" "$expected_url" "$expected_sha"
}

resolve_cc() {
	local requested=${CC:-cc}
	local kind resolved base
	[[ "$requested" =~ ^[A-Za-z0-9_./:+-]+$ ]] \
		|| die 'CC must name one executable without shell syntax'
	kind=$(type -t -- "$requested" || true)
	[[ "$kind" == 'file' ]] || die 'CC must resolve to an external executable'
	resolved=$(command -v -- "$requested") || die 'CC cannot be resolved'
	[[ "$resolved" == /* ]] || die 'CC must resolve to an absolute path'
	base=$(basename -- "$resolved")
	case "$base" in
		cc | cc.exe | gcc | gcc.exe | gcc-[0-9]* | gcc-[0-9]*.exe | clang | clang.exe \
			| clang-[0-9]* | clang-[0-9]*.exe | *-gcc | *-gcc.exe | *-clang | *-clang.exe) ;;
		*) die 'CC executable name is not allowlisted' ;;
	esac
	[[ -x "$resolved" && ! -d "$resolved" ]] || die 'CC is not an executable file'
	printf '%s\n' "$resolved"
}

contract_root=$(physical_directory "$contract_root_input")
vc_root=$(physical_directory "$vc_root_input")
work_root=$(canonical_new_directory "$work_root_input")
[[ ! -e "$work_root" && ! -L "$work_root" ]] || die 'work root must not already exist'
roots_overlap "$work_root" "$contract_root" && die 'work root must be outside the contract checkout'
roots_overlap "$work_root" "$vc_root" && die 'work root must be outside the VC checkout'
roots_overlap "$contract_root" "$vc_root" && die 'contract and VC checkouts must be physically separate'

[[ -f "$0" && ! -L "$0" && "$(basename -- "$0")" == 'bootstrap.sh' ]] \
	|| die 'bootstrap helper must be executed as a physical contract file'
bootstrap_script_root=$(physical_directory "$(dirname -- "$0")")
bootstrap_script_path="$bootstrap_script_root/bootstrap.sh"
expected_bootstrap_script="$contract_root/thirdparty/tccbin_automation/bootstrap/bootstrap.sh"
[[ "$(path_key "$bootstrap_script_path")" == "$(path_key "$expected_bootstrap_script")" ]] \
	|| die 'bootstrap helper does not belong to the exact contract checkout'

contract_repository_url="https://github.com/${contract_repository}"
validate_checkout "$contract_root" "$contract_repository_url" "$contract_sha"

mkdir -- "$work_root"
chmod 700 "$work_root" 2>/dev/null || true
bootstrap_complete=false
cleanup_incomplete_bootstrap() {
	if [[ "$bootstrap_complete" != true ]]; then
		rm -rf -- "$work_root"
	fi
}
exit_from_signal() {
	local status=$1
	trap - HUP INT TERM
	exit "$status"
}
trap cleanup_incomplete_bootstrap EXIT
trap 'exit_from_signal 129' HUP
trap 'exit_from_signal 130' INT
trap 'exit_from_signal 143' TERM

mkdir -- "$work_root/empty-template" "$work_root/cache" "$work_root/modules" "$work_root/tmp"
chmod 700 "$work_root/empty-template" "$work_root/cache" "$work_root/modules" "$work_root/tmp" \
	2>/dev/null || true
export VCACHE="$work_root/cache"
export VMODULES="$work_root/modules"
export VTMP="$work_root/tmp"
export TMPDIR="$work_root/tmp"
export VFLAGS=
export VOSARGS=
export VBUILD_DEFINES=
export VBUILD_FACTS=
export VNORUN=
export VJOBS=1
export CFLAGS=
export CPPFLAGS=
export LDFLAGS=

private_contract_root="$work_root/contract-source"
clone_contract_checkout "$contract_root" "$private_contract_root" "$contract_repository_url" \
	"$contract_sha" "$work_root/empty-template"
lock_path="$private_contract_root/$bootstrap_lock_relative"
[[ -f "$lock_path" && ! -L "$lock_path" ]] || die 'VC lock is absent from the contract checkout'
read_lock "$lock_path"
vc_commit_type=$(git_checked "$vc_root" cat-file -t "$lock_commit") \
	|| die 'VC locked commit is not a local commit'
[[ "$vc_commit_type" == 'commit' ]] || die 'VC locked commit is not a local commit'
vc_tree_type=$(git_checked "$vc_root" cat-file -t "$lock_tree") \
	|| die 'VC locked tree is not a local tree'
[[ "$vc_tree_type" == 'tree' ]] || die 'VC locked tree is not a local tree'
validate_checkout "$vc_root" "$lock_repository" "$lock_commit"
actual_vc_tree=$(git_checked "$vc_root" rev-parse "${lock_commit}^{tree}") \
	|| die 'VC tree cannot be resolved'
[[ "$actual_vc_tree" == "$lock_tree" ]] || die 'VC tree differs from the lock'
validate_vc_artifact "$vc_root" 'v.c' "$lock_v_c"
validate_vc_artifact "$vc_root" 'v_win.c' "$lock_v_win_c"
vc_source_v="$work_root/v.c"
vc_source_win="$work_root/v_win.c"
materialize_vc_artifact "$vc_root" "$lock_v_c" "$vc_source_v"
materialize_vc_artifact "$vc_root" "$lock_v_win_c" "$vc_source_win"

cc_command=$(resolve_cc)
case "$(uname -s)" in
	MINGW* | MSYS* | CYGWIN*)
		vc_source=$vc_source_win
		vc_record=$lock_v_win_c
		exe_suffix='.exe'
		"$cc_command" -std=c99 -municode -w -o "$private_contract_root/v1.exe" "$vc_source" \
			-ladvapi32 -lws2_32 -Wl,-stack=33554432
		;;
	*)
		vc_source=$vc_source_v
		vc_record=$lock_v_c
		exe_suffix=''
		"$cc_command" -std=c99 -w -o "$private_contract_root/v1" "$vc_source" -lm -lpthread
		;;
esac
cli_path="$work_root/tccbin-automation${exe_suffix}"
parse_artifact_record "$vc_record" 'selected bootstrap source'
validate_materialized_vc_artifact "$vc_root" "$vc_source"

(
	cd -P -- "$private_contract_root"
	"$private_contract_root/v1${exe_suffix}" -no-parallel -nocache -cc "$cc_command" \
		-o "$private_contract_root/v2${exe_suffix}" -gc none cmd/v
	"$private_contract_root/v2${exe_suffix}" -no-parallel -nocache -cc "$cc_command" \
		-o "$private_contract_root/v${exe_suffix}" -gc none cmd/v
		"$private_contract_root/v${exe_suffix}" -no-parallel -nocache -cc "$cc_command" \
			-gc none \
			-d "tccbin_contract_repository=${contract_repository}" \
			-d "tccbin_contract_sha=${contract_sha}" \
			-o "$cli_path" thirdparty/tccbin_automation/bin/cmd
)

rm -f -- "$private_contract_root/v1${exe_suffix}" "$private_contract_root/v2${exe_suffix}" \
	"$private_contract_root/v${exe_suffix}" \
	|| die 'private bootstrap compilers cannot be removed before final attestation'

parse_artifact_record "$vc_record" 'selected bootstrap source'
validate_materialized_vc_artifact "$vc_root" "$vc_source"
validate_checkout "$contract_root" "$contract_repository_url" "$contract_sha"
validate_checkout "$private_contract_root" "$contract_repository_url" "$contract_sha"
validate_checkout "$vc_root" "$lock_repository" "$lock_commit"

binding_output=$("$cli_path" contract-binding) \
	|| die 'compiled validator rejected its runtime contract binding'
[[ "$binding_output" == "repository=${contract_repository} sha=${contract_sha}" ]] \
	|| die 'compiled validator reported a different runtime contract binding'

bootstrap_complete=true
trap - EXIT HUP INT TERM
