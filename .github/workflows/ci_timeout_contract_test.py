#!/usr/bin/env python3
"""Check that CI scheduling changes retain every suite and all shard indices."""

import itertools
from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[2]


def require(condition, message):
    if not condition:
        raise SystemExit(message)


def job(workflow, name):
    match = re.search(
        rf"^  {re.escape(name)}:\n(.*?)(?=^  [\w-]+:\n|\Z)",
        workflow,
        re.MULTILINE | re.DOTALL,
    )
    require(match is not None, f"missing job: {name}")
    return match.group(1)


def field(text, name, indent):
    values = re.findall(rf"^{' ' * indent}{re.escape(name)}: (.+)$", text, re.MULTILINE)
    require(len(values) == 1, f"expected exactly one {name} field")
    return values[0]


def axis(text, name):
    value = field(text, name, 8)
    require(value.startswith("[") and value.endswith("]"), f"invalid {name} axis")
    return [part.strip() for part in value[1:-1].split(",")]


def command_step(text, command):
    steps = re.split(r"^      - ", text, flags=re.MULTILINE)[1:]
    matches = [step for step in steps if f"        run: {command}\n" in step]
    require(len(matches) == 1, f"expected exactly one step for: {command}")
    return matches[0]


def routed_command(text, command, condition):
    step = command_step(text, command)
    require(field(step, "if", 8) == condition, f"wrong suite routing: {command}")


def check_linux(workflow):
    for compiler, prod_task in [("gcc", "prod_gcc"), ("clang", "vprod_clang")]:
        name = f"{compiler}-linux-self-tests"
        lane = job(workflow, name)
        require(field(lane, "if", 4) == "github.ref == 'refs/heads/master'", name)
        require(field(lane, "fail-fast", 6) == "false", f"{name} must run every shard")
        require(field(lane, "timeout-minutes", 4) == "121", f"{name} timeout changed")
        require("include:" not in lane, f"{name} has unexpected matrix additions")
        cases = list(itertools.product(axis(lane, "suite"), axis(lane, "shard")))
        excluded = re.findall(
            r"^          - suite: (\w+)\n            shard: (\d+)$",
            lane,
            re.MULTILINE,
        )
        require(len(excluded) == 3 and len(set(excluded)) == 3, f"{name} exclusions")
        require(set(excluded) <= set(cases), f"{name} excludes nonexistent cases")
        cases = [case for case in cases if case not in excluded]
        expected = [(suite, str(shard)) for suite in ["default", "prod"] for shard in range(4)]
        expected.append(("strict", "0"))
        require(sorted(cases) == sorted(expected), f"{name} loses or duplicates suite coverage")
        require(field(lane, "VFLAGS", 6) == f"-cc {compiler}", f"{name} compiler")
        require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", name)
        require(
            field(lane, "VTEST_SELF_SHARD_COUNT", 6)
            == "${{ matrix.suite == 'strict' && '1' || '4' }}",
            f"{name} shard count must match the scheduled indices",
        )
        require("VTEST_ONLY" not in lane, f"{name} must retain the full suite selection")
        require(field(lane, "V_MACOS_V3_NO_FALLBACK", 6) == "1", name)
        setup = "uses: ./.github/actions/setup-v"
        dependencies = f"v run ci/linux_ci.vsh install_dependencies_for_examples_and_tools_{compiler}"
        command_step(lane, dependencies)
        require(setup in lane and lane.index(setup) < lane.index(dependencies), f"{name} setup order")
        for suite, task in [
            ("default", compiler),
            ("prod", prod_task),
            ("strict", f"cstrict_{compiler}"),
        ]:
            command = f"v run ci/linux_ci.vsh self_tests_{task}"
            routed_command(lane, command, f"matrix.suite == '{suite}'")
            require(lane.index(dependencies) < lane.index(command), f"{name} dependency order")
        if compiler == "clang":
            build = "v -cc clang -o vprod -prod cmd/v"
            routed_command(lane, build, "matrix.suite == 'strict'")
            require(
                lane.index(dependencies) < lane.index(build) < lane.index("run: v run ci/linux_ci.vsh self_tests_cstrict_clang"),
                "clang strict suite must build its own vprod after dependency setup",
            )
        serial = job(workflow, f"{compiler}-linux")
        require("steps.self_tests" not in serial, f"{compiler} checks depend on a moved step")
        examples = command_step(serial, f"v run ci/linux_ci.vsh build_examples_{compiler}")
        require(
            "steps.test_pure_v_math_module.outcome == 'success'" in field(examples, "if", 8),
            f"{compiler} examples must still run after the serial checks",
        )


def check_macos(workflow):
    lane = job(workflow, "tools-macos")
    cases = list(itertools.product(axis(lane, "cc"), axis(lane, "suite"), axis(lane, "shard")))
    expected = list(itertools.product(["clang"], ["default", "strict"], ["0", "1"]))
    require(sorted(cases) == sorted(expected), "macOS tools loses or duplicates suite coverage")
    require("exclude:" not in lane and "include:" not in lane, "unexpected macOS matrix overrides")
    require(field(lane, "fail-fast", 6) == "false", "macOS must run every shard")
    require(field(lane, "VTEST_SELF_SHARD_INDEX", 6) == "${{ matrix.shard }}", "macOS shard index")
    require(field(lane, "VTEST_SELF_SHARD_COUNT", 6) == "'2'", "macOS shard count")
    require("VTEST_ONLY" not in lane, "macOS must retain the full tool suite selection")
    routed_command(lane, "./v -silent -N -W -check build-tools", "matrix.suite == 'default' && matrix.shard == 0")
    routed_command(lane, "./v -silent test-self cmd", "matrix.suite == 'default'")
    routed_command(lane, "./v -silent -W -cstrict test-self cmd", "matrix.suite == 'strict'")


def check_riscv(workflow):
    lane = job(workflow, "riscv64_linux")
    require(field(lane, "timeout-minutes", 4) == "180", "RISC-V needs time for tests after bootstrap")
    commands = [
        "            make\n",
        "            V_CI_SOURCE_BUILD=1 sh .github/workflows/prepare_v.sh\n",
        "            ./v test vlib/builtin vlib/os vlib/encoding/binary\n",
        "            ./v test vlib/v/slow_tests/assembly\n",
        "            VTEST_ONLY=closure ./v test vlib/v/tests\n",
    ]
    require(all(lane.count(command) == 1 for command in commands), "RISC-V coverage changed")
    positions = [lane.index(command) for command in commands]
    require(positions == sorted(positions), "RISC-V native bootstrap and test order changed")


def check_msvc(workflow):
    lane = job(workflow, "sanitize-address-msvc")
    require(sorted(axis(lane, "stage")) == ["fmt", "vet"], "MSVC must retain both cleancode stages")
    require("exclude:" not in lane and "include:" not in lane, "unexpected MSVC matrix overrides")
    require(field(lane, "fail-fast", 6) == "false", "MSVC must run both cleancode stages")
    require(field(lane, "timeout-minutes", 4) == "30", "MSVC deadline changed")
    require(field(lane, "VTEST_CLEANCODE_STAGE", 6) == "${{ matrix.stage }}", "MSVC stage selection")
    require(field(lane, "VJOBS", 6) == "4", "MSVC cleancode must not serialize all files")
    require(field(lane, "VTEST_SHOW_START", 6) == "'1'", "MSVC must report the active file")
    command_step(lane, r".\v.exe -silent test-cleancode")


if __name__ == "__main__":
    check_linux((ROOT / ".github/workflows/linux_ci.yml").read_text())
    check_macos((ROOT / ".github/workflows/tools_ci.yml").read_text())
    check_riscv((ROOT / ".github/workflows/riscv64_linux_ci.yml").read_text())
    check_msvc((ROOT / ".github/workflows/sanitized_ci.yml").read_text())
    print("CI suite routing, complete shard coverage, and bootstrap prerequisites passed")
