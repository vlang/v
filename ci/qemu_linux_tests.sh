#!/usr/bin/env bash

set -Eeuo pipefail

usage() {
	cat <<'EOF'
Usage: ci/qemu_linux_tests.sh [options] [-- V arguments]

Sync the current checkout to an existing Debian ARM64 QEMU guest, rebuild V,
and run the Linux test suite. With no V arguments, the script runs `test-all`.

Options:
  --no-sync       Use the checkout already present in the guest.
  --provision     Install the packages used by the Linux compiler tests.
  -h, --help      Show this help.

Environment overrides:
  V_QEMU_VM_DIR        VM state directory (default: ~/.local/share/v3-qemu-debian13)
  V_QEMU_SSH_PORT      Forwarded SSH port (default: 2222)
  V_QEMU_GUEST         SSH target (default: v@127.0.0.1)
  V_QEMU_GUEST_REPO    Guest checkout (default: /home/v/v3); absolute paths using
                       only letters, digits, '.', '_', '-', and '/' are accepted
  V_QEMU_CPUS          Virtual CPUs (default: 4)
  V_QEMU_MEMORY_MB     Guest memory in MiB (default: 16384)
  V_QEMU_JOBS          V test jobs (default: 1)
  V_QEMU_CC            Guest compiler exposed as `cc` to nested V3 processes
                       (default: clang)
  V_QEMU_VFLAGS        Flags inherited by V subprocesses
                       (default: -cc clang -no-memory-limit)
  V_QEMU_NO_FALLBACK   Set V_MACOS_V3_NO_FALLBACK (default: 1)
  V_QEMU_TMPDIR        Guest disk-backed V temporary root (default: /var/tmp/v-qemu-tests)
  V_QEMU_STOP_AFTER    Power off the guest after the run when set to 1
  V_QEMU_FIRMWARE      AArch64 UEFI firmware path
  V_QEMU_RSYNC         Host rsync executable (only needed when syncing)

Examples:
  ci/qemu_linux_tests.sh
  ci/qemu_linux_tests.sh -- -cc clang test vlib/v3/
  V_QEMU_CC=gcc V_QEMU_VFLAGS='-cc gcc -no-memory-limit' \
    ci/qemu_linux_tests.sh -- -cc gcc test vlib/v3/
  V_QEMU_NO_FALLBACK=0 ci/qemu_linux_tests.sh -- -old-compiler test-all
EOF
}

provision=0
sync_checkout=1
while (($# > 0)); do
	case "$1" in
		--no-sync)
			sync_checkout=0
			shift
			;;
		--provision)
			provision=1
			shift
			;;
		-h | --help)
			usage
			exit 0
			;;
		--)
			shift
			break
			;;
		*)
			break
			;;
	esac
done

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
vm_dir=${V_QEMU_VM_DIR:-"${HOME}/.local/share/v3-qemu-debian13"}
ssh_port=${V_QEMU_SSH_PORT:-2222}
guest=${V_QEMU_GUEST:-v@127.0.0.1}
guest_repo=${V_QEMU_GUEST_REPO:-/home/v/v3}
cpus=${V_QEMU_CPUS:-4}
memory_mb=${V_QEMU_MEMORY_MB:-16384}
# A V3 compiler-tree test can exceed 6 GiB RSS on ARM64. One unbounded worker is
# reliable in the default 16 GiB guest; callers with larger guests can opt into
# more parallelism explicitly.
jobs=${V_QEMU_JOBS:-1}
nested_cc=${V_QEMU_CC:-clang}
vflags=${V_QEMU_VFLAGS:--cc clang -no-memory-limit}
no_fallback=${V_QEMU_NO_FALLBACK:-1}
guest_tmp_root=${V_QEMU_TMPDIR:-/var/tmp/v-qemu-tests}
stop_after=${V_QEMU_STOP_AFTER:-0}

if [[ ! "$guest_repo" =~ ^/[A-Za-z0-9._/-]+$ ]]; then
	echo "V_QEMU_GUEST_REPO must be an absolute path containing only letters, digits, '.', '_', '-', and '/': ${guest_repo}" >&2
	exit 1
fi
case "/${guest_repo#/}/" in
	*"/../"*)
		echo "V_QEMU_GUEST_REPO cannot contain '..' path segments: ${guest_repo}" >&2
		exit 1
		;;
esac

if [[ "$guest_tmp_root" != /* ]]; then
	echo "V_QEMU_TMPDIR must be an absolute guest path: ${guest_tmp_root}" >&2
	exit 1
fi

qemu_bin=${V_QEMU_BIN:-$(command -v qemu-system-aarch64 || true)}
if [[ -z "$qemu_bin" ]]; then
	echo 'qemu-system-aarch64 is required (on macOS: brew install qemu).' >&2
	exit 1
fi
rsync_bin=
if ((sync_checkout)); then
	rsync_candidate=${V_QEMU_RSYNC:-rsync}
	rsync_bin=$(command -v "$rsync_candidate" || true)
	if [[ -z "$rsync_bin" ]]; then
		echo 'rsync is required unless --no-sync is used.' >&2
		exit 1
	fi
fi

key_file="${vm_dir}/id_ed25519"
known_hosts_file="${vm_dir}/known_hosts"
pid_file="${vm_dir}/qemu.pid"
disk_file="${vm_dir}/disk.qcow2"
vars_file="${vm_dir}/edk2-vars.fd"
seed_file="${vm_dir}/seed.iso"
serial_log="${vm_dir}/serial.log"
qemu_log="${vm_dir}/qemu.log"

for required_file in "$key_file" "$known_hosts_file" "$disk_file" "$vars_file" "$seed_file"; do
	if [[ ! -f "$required_file" ]]; then
		echo "Missing VM asset: ${required_file}" >&2
		echo 'Create the Debian cloud guest first or set V_QEMU_VM_DIR.' >&2
		exit 1
	fi
done

firmware=${V_QEMU_FIRMWARE:-}
if [[ -z "$firmware" ]]; then
	qemu_prefix=$(CDPATH= cd -- "$(dirname -- "$qemu_bin")/.." && pwd)
	firmware="${qemu_prefix}/share/qemu/edk2-aarch64-code.fd"
fi
if [[ ! -f "$firmware" ]]; then
	echo "Missing AArch64 UEFI firmware: ${firmware}" >&2
	echo 'Set V_QEMU_FIRMWARE to the edk2 AArch64 code image.' >&2
	exit 1
fi

ssh_options=(
	-i "$key_file"
	-p "$ssh_port"
	-o "UserKnownHostsFile=\"${known_hosts_file}\""
	-o StrictHostKeyChecking=yes
	-o ConnectTimeout=5
)
if [[ ! "$ssh_port" =~ ^[0-9]+$ ]]; then
	echo "Invalid SSH port: ${ssh_port}" >&2
	exit 1
fi
if [[ "$key_file" == *"'"* || "$known_hosts_file" == *"'"* || "$known_hosts_file" == *'"'* ]]; then
	echo 'The QEMU SSH key and known-hosts paths cannot contain quote characters.' >&2
	exit 1
fi
# openrsync understands quoted arguments but treats Bash `%q` backslashes
# literally. Preserve spaces with single-quoted arguments, and retain inner
# quotes around the known-hosts value for OpenSSH's own option parser.
printf -v rsync_ssh_command \
	"ssh -i '%s' -p '%s' '-oUserKnownHostsFile=\"%s\"' -o StrictHostKeyChecking=yes" \
	"$key_file" "$ssh_port" "$known_hosts_file"

vm_is_running() {
	[[ -s "$pid_file" ]] && kill -0 "$(<"$pid_file")" 2>/dev/null
}

start_vm() {
	if vm_is_running; then
		return
	fi
	if [[ -f "$pid_file" ]]; then
		rm -f -- "$pid_file"
	fi
	local accelerator=tcg
	local cpu=max
	if [[ $(uname -s) == Darwin ]]; then
		accelerator=hvf
		cpu=host
	fi
	"$qemu_bin" \
		-machine "virt,accel=${accelerator},highmem=on" \
		-cpu "$cpu" \
		-smp "$cpus" \
		-m "$memory_mb" \
		-drive "if=pflash,format=raw,readonly=on,file=${firmware}" \
		-drive "if=pflash,format=raw,file=${vars_file}" \
		-drive "if=virtio,file=${disk_file},format=qcow2" \
		-drive "if=virtio,file=${seed_file},format=raw,readonly=on" \
		-device virtio-net-pci,netdev=net0 \
		-netdev "user,id=net0,hostfwd=tcp:127.0.0.1:${ssh_port}-:22" \
		-display none \
		-serial "file:${serial_log}" \
		-monitor none \
		-daemonize \
		-pidfile "$pid_file" \
		-D "$qemu_log"
}

wait_for_ssh() {
	for _ in {1..120}; do
		if ssh "${ssh_options[@]}" "$guest" true 2>/dev/null; then
			return
		fi
		sleep 1
	done
	echo "Guest SSH did not become ready on port ${ssh_port}." >&2
	exit 1
}

power_off() {
	if [[ "$stop_after" == 1 ]] && vm_is_running; then
		ssh "${ssh_options[@]}" "$guest" 'sudo poweroff' >/dev/null 2>&1 || true
	fi
}
trap power_off EXIT

start_vm
wait_for_ssh

if ((provision)); then
	# A fresh Debian guest does not necessarily have the remote half of rsync yet.
	# Install it before attempting to transfer the checkout.
	ssh "${ssh_options[@]}" "$guest" "sudo apt-get update && sudo DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential clang git lld rsync pkg-config libssl-dev sqlite3 libsqlite3-dev default-libmysqlclient-dev libpq-dev postgresql valgrind libfreetype6-dev libxi-dev libxcursor-dev libgl-dev libxrandr-dev libasound2-dev libegl-dev libwayland-dev libxkbcommon-dev libwayland-egl1 libxkbcommon-x11-dev wayland-protocols libx11-dev libgl1-mesa-dri xauth xvfb"
fi

host_head=$(git -C "$repo_root" rev-parse HEAD)
printf -v guest_repo_q '%q' "$guest_repo"
guest_head=$(ssh "${ssh_options[@]}" "$guest" "git -C ${guest_repo_q} rev-parse HEAD")
if [[ "$host_head" != "$guest_head" ]]; then
	echo "Host and guest baselines differ (${host_head} != ${guest_head})." >&2
	echo "Update ${guest_repo} in the guest before syncing local changes." >&2
	exit 1
fi

if ((sync_checkout)); then
	# Remove every untracked path copied by the previous sync before restoring the
	# current set. This keeps a locally deleted untracked source from lingering in
	# guest module discovery without cleaning unrelated guest build artifacts.
	ssh "${ssh_options[@]}" "$guest" "bash -s -- ${guest_repo_q}" <<'EOF'
set -Eeuo pipefail
guest_repo=$1
git_dir=$(git -C "$guest_repo" rev-parse --absolute-git-dir)
manifest="${git_dir}/qemu-linux-tests-untracked"
if [[ -f "$manifest" ]]; then
	cd "$guest_repo"
	while IFS= read -r -d '' path; do
		case "$path" in
			'' | /* | .. | ../* | */.. | */../*)
				echo "Invalid path in QEMU sync manifest: ${path}" >&2
				exit 1
				;;
		esac
		rm -f -- "$path"
	done < "$manifest"
fi
EOF
	# Remove tracked paths deleted or renamed in the local worktree. The NUL-delimited
	# protocol keeps arbitrary Git filenames safe, and every removal is scoped beneath
	# the already-validated guest checkout.
	git -C "$repo_root" diff --no-renames --name-only --diff-filter=D -z HEAD -- \
		| ssh "${ssh_options[@]}" "$guest" "cd ${guest_repo_q} && xargs -0 -r rm -f --"
	# Staged deletions are absent from `git ls-files --cached`, while unstaged
	# deletions are still listed. Send only paths that currently exist locally.
	git -C "$repo_root" ls-files -z --cached --others --exclude-standard \
		| while IFS= read -r -d '' path; do
			if [[ -e "${repo_root}/${path}" || -L "${repo_root}/${path}" ]]; then
				printf '%s\0' "$path"
			fi
		done \
		| "$rsync_bin" -az \
		--from0 \
		--files-from=- \
		--exclude '.detect_tcc*' \
		-e "$rsync_ssh_command" \
		"${repo_root}/" "${guest}:${guest_repo}/"
	# Record only untracked paths that the rsync exclusion above allows through.
	git -C "$repo_root" ls-files -z --others --exclude-standard \
		| while IFS= read -r -d '' path; do
			case "$path" in
				.detect_tcc* | */.detect_tcc*) continue ;;
			esac
			printf '%s\0' "$path"
		done \
		| ssh "${ssh_options[@]}" "$guest" \
			"git_dir=\$(git -C ${guest_repo_q} rev-parse --absolute-git-dir) && cat > \"\${git_dir}/qemu-linux-tests-untracked\""
fi

if (($# == 0)); then
	set -- test-all
fi

printf -v jobs_q '%q' "$jobs"
printf -v nested_cc_q '%q' "$nested_cc"
printf -v vflags_q '%q' "$vflags"
printf -v no_fallback_q '%q' "$no_fallback"
printf -v guest_tmp_root_q '%q' "$guest_tmp_root"
printf -v guest_tmp_template_q '%q' "${guest_tmp_root%/}/run.XXXXXXXX"
printf -v test_command '%q ' ./vnew "$@"

remote_command="cd ${guest_repo_q}"
remote_command+=" && mkdir -p ${guest_tmp_root_q}"
remote_command+=" && qemu_tmp=\$(mktemp -d ${guest_tmp_template_q})"
remote_command+=" && trap 'rm -rf -- \"\$qemu_tmp\"' EXIT"
remote_command+=" && export PATH=${guest_repo_q}:\$PATH"
remote_command+=" TMPDIR=/tmp VTMP=\$qemu_tmp"
remote_command+=" V_C_ERROR_BUG_REPORT_DISABLED=1"
remote_command+=" && if [ ! -x thirdparty/tcc/tcc.exe ] || [ ! -f thirdparty/tcc/lib/libgc.a ]; then make; fi"
remote_command+=" && ./v -old-compiler -o ./vnew cmd/v"
if ((provision)); then
	remote_command+=" && ./vnew retry -- ./vnew install markdown"
	remote_command+=" && if [ ! -f thirdparty/sqlite/sqlite3.c ]; then ./vnew -old-compiler run vlib/db/sqlite/install_thirdparty_sqlite.vsh; fi"
fi
remote_command+=" && ./vnew wipe-cache"
remote_command+=" && mkdir -p \"\$qemu_tmp/bin\""
remote_command+=" && ln -sf ${guest_repo_q}/vnew \"\$qemu_tmp/bin/v\""
remote_command+=" && nested_cc_path=\$(command -v ${nested_cc_q})"
remote_command+=" && ln -sf \"\$nested_cc_path\" \"\$qemu_tmp/bin/cc\""
remote_command+=" && export PATH=\"\$qemu_tmp/bin:\$PATH\""
remote_command+=" && VFLAGS=${vflags_q} VJOBS=${jobs_q}"
remote_command+=" V_MACOS_V3_NO_FALLBACK=${no_fallback_q} ${test_command}"
ssh "${ssh_options[@]}" "$guest" "$remote_command"
