# V3 AMD64 backend

This directory contains the AMD64-specific V3 lowering, encoding, object-format, and
publication components. Its active, bounded path turns an immutable lowering plan
and explicit ABI facts into deterministic relocatable object bytes. It also contains
validated memory/frame planners. Their only active production edge is the explicit,
opt-in Windows COFF M7 contract below; broader memory/frame activation and
executable-image writers remain separate from the active object path.

The package is AMD64 only. Generic V3 parsing, typing, transformation, SSA
discovery, producer-side facts, target routing, startup and entry policy, runtime
metadata, SDK selection, and end-to-end activation remain outside this backend
package.

## Architecture

The component ownership and intended data flow are:

```text
SSA module and explicit backend facts
  -> ABI classification, call-frame plans, and explicit ABI consumption
  -> memory aggregation and frame/save/CFI composition
  -> AMD64 instruction lowering and exact byte encoding
  -> canonical Object
  -> ELF64 / Mach-O 64 / AMD64 COFF / PE32+
  -> transactional publication
```

These arrows describe interfaces, not a claim that every edge is active. The
current active object route enters instruction lowering through an immutable
`LoweringPlan` and explicit ABI sidecars, then emits a canonical `Object`.

The only memory/frame production edge is the opt-in
`Gen.new_with_scalar_abi_memory_frames` route for Microsoft x64 COFF. Each activated
function must be a final, one-block, direct no-argument void `CALL` wrapper with an
empty M1 memory/action plan, an empty callee-save set, and the exact M6
`C=32`/`D=40` composition. M7 snapshots its prologue, epilogue, and xdata into
SymbolID-owned `Object` frame records. COFF explicit-frame mode consumes those
records for `.pdata` and `.xdata` without falling back to `CALL` inference. Legacy
routes remain byte-identical; default `Object` validation and the ELF, Mach-O, and
PE writers reject frame-bearing objects.

All other memory/frame compositions remain `INERT`. This opt-in API supplies no
generic producer, target routing, source-name inference, or full memory activation.
ELF64, Mach-O 64, and AMD64 COFF relocatable writers are active. Tiny executable and
PE32+ writers require explicit definitions and remain `UPSTREAM_STANDBY`; this
package does not claim generic routing, startup/runtime construction, SDK
integration, or executable end-to-end activation.

Backend code consumes explicit, validated facts and does not infer absent generic
names, signatures, sidecars, entry points, imports, or runtime policy. Changes to
the generic V3 pipeline or activation boundary are outside this package.

## Supported targets

| OS | ABI | Active object format | Additional bounded component |
| --- | --- | --- | --- |
| Linux x86_64 | System V AMD64 | ELF64 `ET_REL` | Direct ELF64 `ET_EXEC` writer: `UPSTREAM_STANDBY` |
| macOS x86_64 | Apple x86-64/System V | Mach-O 64 `MH_OBJECT` | Tiny link artifact writer: `UPSTREAM_STANDBY` |
| Windows x86_64 | Microsoft x64 | AMD64 COFF relocatable | Opt-in M7 CALL32 frame/unwind activation; PE32+ writer: `UPSTREAM_STANDBY` |

## Authority

Current V3 source, official specifications, and real bounded tests are
authoritative. V2 is inventory evidence only and does not define V3 behavior.
Normative references are the Intel and AMD x86-64 architecture manuals, the
System V AMD64 psABI and ELF specification, Apple's x86-64 ABI and Mach-O
definitions, Microsoft's x64 ABI, unwind, and PE/COFF documentation, and DWARF
Version 5. Tests pin exact bytes and use available GNU, LLVM, Apple-target, and
Windows-target tools as independent evidence; tests do not create missing product
routing.

## Bounded validation

Use a compiler rebuilt from the checkout. Keep jobs, time, and address space
bounded, and run the test file relevant to the change:

```sh
VJOBS=1 VFLAGS='-gc none -cc /usr/bin/cc' \
  timeout 120s prlimit --as=1073741824 -- ./v \
  -gc none -cc /usr/bin/cc -stats test -fail-fast \
  vlib/v3/gen/amd64/gen_test.v

VJOBS=1 VFLAGS='-gc none -cc /usr/bin/cc' \
  timeout 120s prlimit --as=1073741824 -- ./v \
  -gc none -cc /usr/bin/cc -stats test -fail-fast \
  vlib/v3/gen/amd64/memory_frame_compose_test.v

timeout 120s prlimit --as=1073741824 -- ./v fmt -verify \
  vlib/v3/gen/amd64/memory_frame_compose.v
```

Check inventory identity from the repository root:

```sh
comm -3 \
  <(LC_ALL=C rg --files vlib/v3/gen/amd64 | sed 's#.*/##' | sort) \
  <(LC_ALL=C sed -n '/FILE_INVENTORY_BEGIN/,/FILE_INVENTORY_END/p' \
      vlib/v3/gen/amd64/README.md |
    awk -F'`' '/^\| `/{print $2}' | sort)
```

No output means the inventory and current tree are identical.

## File inventory

Status meanings:

- `ACTIVE`: used by the bounded relocatable-object path or maintained test/evidence.
- `INERT`: validated planner/helper with no production edge into generation or objects.
- `ACTIVE` for an M7 subset, otherwise `INERT`: only the exact opt-in Windows COFF
  contract enters generation and objects.
- `UPSTREAM_STANDBY`: implementation exists, but an authoritative generic producer,
  route, or activation contract is absent.

Paths below are relative to this directory. The snapshot is generated from
`LC_ALL=C rg --files vlib/v3/gen/amd64 | sort`.

<!-- FILE_INVENTORY_BEGIN -->
| File | Role | Principal input -> output | Companion test or evidence | Status |
| --- | --- | --- | --- | --- |
| `README.md` | Package boundary, architecture, validation, and inventory authority. | Current tree and reviewed contracts -> maintained package map. | Exact inventory comparison and audit/ledger golden requirement. | `ACTIVE` |
| `abi.v` | Classifies scalar and aggregate function ABI behavior for each target profile. | Type store, layouts, function type, profile -> `AbiFunctionDecision`. | `abi_test.v`; official System V, Apple, and Microsoft ABI rules. | `ACTIVE` (explicit API) |
| `abi_consume.v` | Consumes explicit signatures, constants, and ABI decisions for scalar lowering. | SSA plus explicit ABI sidecars -> immutable scalar `LoweringPlan`. | `abi_consume_test.v`, `gen_test.v`. | `ACTIVE` (explicit API; generic producer remains upstream-owned) |
| `abi_consume_test.v` | Pins accepted ABI sidecars, refusal order, and lowering-plan snapshots. | ABI/SSA fixtures -> exact plans and errors. | `abi_consume.v`; downstream `gen_test.v`. | `ACTIVE` |
| `abi_frame.v` | Derives call locations, stack homes, and call-frame requirements. | `AbiFunctionDecision` and call facts -> `AbiCallFramePlan`/`AbiPlannedCall`. | `abi_frame_test.v`. | `ACTIVE` (explicit API) |
| `abi_frame_test.v` | Covers register/stack placement, home intervals, limits, and determinism. | ABI fixtures -> exact frame plans and errors. | `abi_frame.v`; official ABI rules. | `ACTIVE` |
| `abi_test.v` | Covers profile classification, layouts, UDT rules, and refusal boundaries. | Type/profile fixtures -> exact ABI decisions and errors. | `abi.v`; official ABI rules. | `ACTIVE` |
| `asm.v` | Encodes the approved AMD64 instruction subset and relocation sites. | Validated operands and ABI locations -> exact machine bytes/fixups. | `asm_test.v`, object-writer tests, external disassemblers. | `ACTIVE` |
| `asm_test.v` | Pins literal instruction bytes, displacement limits, calls, and stack forms. | Encoding fixtures -> exact byte sequences and errors. | `asm.v`; GNU/LLVM tooling. | `ACTIVE` |
| `coff.v` | Serializes canonical objects as AMD64 COFF relocatables; explicit M7 mode consumes owned frame records for `.pdata`/`.xdata` without `CALL` inference fallback. | Validated `Object`, optionally with exact M7 frames -> deterministic COFF bytes. | `coff_test.v`, `gen_test.v`; LLVM/GNU COFF readers. | `ACTIVE` |
| `coff_test.v` | Pins COFF sections, symbols, relocations, addends, unwind data, explicit M7 frame consumption/refusal, and limits. | Object/frame fixtures -> exact bytes, errors, and parser acceptance. | `coff.v`; LLVM/GNU tools. | `ACTIVE` |
| `diagnostics.v` | Normalizes AMD64 lowering diagnostics and context. | Lowering context and detail -> stable errors. | `ssa_lower_test.v`, `gen_test.v`. | `ACTIVE` |
| `elf.v` | Serializes canonical objects as x86-64 ELF relocatables and rejects COFF function-frame metadata. | Validated frame-free `Object` -> deterministic ELF64 `ET_REL` bytes. | `elf_test.v`, `object_test.v`; GNU/LLVM ELF tools. | `ACTIVE` |
| `elf_test.v` | Pins ELF headers, sections, symbols, relocations, data, and rejection limits. | Object fixtures -> exact bytes plus parser acceptance. | `elf.v`; GNU/LLVM tools. | `ACTIVE` |
| `elf_tiny.v` | Builds a bounded direct Linux executable image from explicit definitions. | `Object` plus explicit entry/runtime definition -> ELF64 `ET_EXEC` bytes. | `elf_tiny_test.v`. | `UPSTREAM_STANDBY` |
| `elf_tiny_test.v` | Pins the tiny ELF image contract without asserting generic activation. | Explicit image fixtures -> exact executable bytes and parser evidence. | `elf_tiny.v`; GNU/LLVM ELF tools. | `ACTIVE` (standby-component evidence) |
| `gen.v` | Orchestrates immutable lowering plans, instruction encoding, objects, active writers, and the opt-in exact-M6 Windows frame route. | `LoweringPlan`, explicit sidecars, and optional exact M6 compositions -> `Object` or relocatable bytes. | `gen_test.v`; lowerer, assembler, memory-frame, and writer tests. | `ACTIVE` |
| `gen_test.v` | Pins bounded generation cross-products, snapshots, objects, writer output, M7 activation/refusal, and legacy byte identity. | Lowering/M6 fixtures -> exact objects/bytes and errors. | `gen.v`; memory-frame and object-format evidence. | `ACTIVE` |
| `macho.v` | Serializes canonical objects as x86-64 Mach-O relocatables and rejects COFF function-frame metadata. | Validated frame-free `Object` -> deterministic Mach-O 64 `MH_OBJECT` bytes. | `macho_test.v`, `object_test.v`; LLVM Mach-O tools. | `ACTIVE` |
| `macho_test.v` | Pins Mach-O commands, sections, symbols, relocations, data, and limits. | Object fixtures -> exact bytes plus parser acceptance. | `macho.v`; LLVM tools. | `ACTIVE` |
| `macho_tiny.v` | Builds a bounded macOS link artifact from explicit entry/runtime facts. | `Object` plus explicit definition -> `MachoTinyArtifact`. | `macho_tiny_test.v`. | `UPSTREAM_STANDBY` |
| `macho_tiny_test.v` | Pins tiny Mach-O object bytes and entry-link-symbol metadata. | Explicit fixtures -> exact artifact and parser evidence. | `macho_tiny.v`; LLVM Mach-O tools. | `ACTIVE` (standby-component evidence) |
| `memory_agg.v` | Plans bounded scalar/aggregate memory operations from final static SSA; M7 accepts only its empty-plan attestation. | SSA and `MemoryAggFunctionFacts` -> immutable `MemoryAggPlan`. | `memory_agg_test.v`, `gen_test.v`. | `ACTIVE` for the empty M7 plan; otherwise `INERT` |
| `memory_agg_test.v` | Pins aggregate discovery, aliases, copies, caps, errors, and snapshots. | SSA/fact fixtures -> exact memory plans and refusals. | `memory_agg.v`; M7 accepts only empty output. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame.v` | Computes target frame geometry, red-zone policy, stack extent, and slot placement. | Memory slot/call facts and policy -> `MemoryFrameLayout`. | `memory_frame_test.v`, `memory_frame_policy_test.v`, `gen_test.v`. | `ACTIVE` for M7 `C=32`/`D=40`; otherwise `INERT` |
| `memory_frame_cfi.v` | Derives semantic DWARF CFI rows or Windows-none disposition from saved frames. | Save-aware frame -> semantic CFI plan. | `memory_frame_cfi_test.v`, `gen_test.v`; DWARF/Windows specifications. | `ACTIVE` for M7 Windows-none; otherwise `INERT` |
| `memory_frame_cfi_encode.v` | Encodes semantic CFI into format-neutral DWARF instruction fragments. | Semantic CFI plan -> CIE-initial and FDE instruction bytes. | `memory_frame_cfi_encode_test.v`, `gen_test.v`; GNU/LLVM/Apple probes. | `ACTIVE` for M7 absent-DWARF attestation; otherwise `INERT` |
| `memory_frame_cfi_encode_test.v` | Pins CIE/FDE fragments, ULEB boundaries, caps, and toolchain identity. | Semantic CFI fixtures -> exact fragments and refusals. | `memory_frame_cfi_encode.v`; GNU/LLVM/Apple tools. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame_cfi_test.v` | Pins semantic rows, save coordinates, phase ordering, and final baseline. | Save-frame fixtures -> exact CFI plans and errors. | `memory_frame_cfi.v`. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame_compose.v` | Composes memory, call-extent, frame, save, and CFI plans; M7 accepts only the exact Windows empty-M1 CALL32 result. | M1 memory facts plus explicit call/save sidecars -> composition plan. | `memory_frame_compose_test.v`, `gen_test.v`; M0-M5 tests. | `ACTIVE` for the exact M7 composition; otherwise `INERT` |
| `memory_frame_compose_test.v` | Pins M6 composition, real-call attestations, bijections, caps, and deep snapshots. | SSA/backend fixtures -> exact composed plans and errors. | `memory_frame_compose.v`; closed M0-M5 matrix and M7 gate. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame_encode.v` | Encodes frame allocation, restoration, probing, addressing, and unwind facts. | `MemoryFrameLayout` -> prologue/epilogue bytes, fixups, and unwind data. | `memory_frame_encode_test.v`, `memory_frame_policy_test.v`, `gen_test.v`. | `ACTIVE` for exact M7 frame bytes; otherwise `INERT` |
| `memory_frame_encode_test.v` | Pins exact frame bytes, RSP translations, probe fixups, unwind, and limits. | Frame fixtures -> encoded frames and errors. | `memory_frame_encode.v`; LLVM Windows tools. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame_policy_test.v` | Cross-checks red-zone policy and frame/encoding invariants. | Policy/frame fixtures -> exact geometry, bytes, and refusal evidence. | `memory_frame.v`, `memory_frame_encode.v`. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame_save.v` | Adds deterministic callee-save layout and push/pop encoding to frames; M7 accepts only the empty-save result. | Frame layout plus save facts -> save-aware frame plan. | `memory_frame_save_test.v`, `gen_test.v`. | `ACTIVE` for the empty-save M7 frame; otherwise `INERT` |
| `memory_frame_save_test.v` | Pins save sets, ordering, offsets, probe interaction, unwind, and caps. | Frame/save fixtures -> exact save plans and errors. | `memory_frame_save.v`. | `ACTIVE` (planner/M7 evidence) |
| `memory_frame_test.v` | Pins frame geometry, red-zone endpoints, alignment, calls, probing, and errors. | Slot/call fixtures -> exact layouts and refusals. | `memory_frame.v`; ABI stack rules. | `ACTIVE` (planner/M7 evidence) |
| `object.v` | Defines and validates the canonical object model, including deep-cloned SymbolID-owned M7 function-frame records and writer capability gates. | Encoded functions, data, symbols, relocations, optional M7 frames -> validated `Object`. | `object_test.v`; all active writer tests. | `ACTIVE` |
| `object_test.v` | Pins object invariants, frame ownership/deep cloning, writer rejection, symbols, relocations, data, and snapshots. | Object/frame fixtures -> accepted objects and exact errors. | `object.v`; format-writer tests. | `ACTIVE` |
| `pe.v` | Serializes a bounded PE32+ image from explicit image/import/runtime metadata, including explicitly bound `malloc`, `free`, and `calloc` helpers, and rejects Object function-frame records. | Frame-free `Object` plus explicit PE definition -> deterministic PE32+ bytes. | `pe_test.v`, `object_test.v`; LLVM PE/COFF tools. | `UPSTREAM_STANDBY` |
| `pe_test.v` | Pins PE headers, sections, imports, relocations, explicit `malloc`/`free`/`calloc` bytes, import ownership, unwind, transactionality, predecessor identity, limits, and bounded LLVM and guarded Windows DUMPBIN oracles. | Explicit image fixtures -> exact PE bytes and parser evidence. | `pe.v`; LLVM PE/COFF tools. | `ACTIVE` (standby-component evidence) |
| `publish.v` | Publishes completed bytes transactionally without defining target routing. | Artifact bytes and output path -> atomically published file. | `publish_test.v`. | `ACTIVE` for objects; `UPSTREAM_STANDBY` for executable routing |
| `publish_test.v` | Pins replacement, hard-link, cleanup, and failure behavior in bounded temp trees. | Bytes/path/filesystem fixtures -> publication results and errors. | `publish.v`. | `ACTIVE` |
| `ssa_lower.v` | Lowers the approved final-static SSA subset into an immutable plan. | SSA plus explicit scalar constants -> `LoweringPlan`. | `ssa_lower_test.v`, `gen_test.v`. | `ACTIVE` |
| `ssa_lower_test.v` | Pins supported SSA forms, ordering, diagnostics, constants, and refusals. | SSA fixtures -> exact lowering plans and errors. | `ssa_lower.v`, `diagnostics.v`. | `ACTIVE` |
<!-- FILE_INVENTORY_END -->

Any add, rename, or delete under `vlib/v3/gen/amd64` must update this inventory
in the same change.
