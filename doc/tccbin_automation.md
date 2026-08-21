# TinyCC bundle automation

This document describes the version 1 contract for validating and eventually publishing the
TinyCC bundles used by V. The implementation is deliberately fail-closed. Contract code can be
merged and tested before any repository credential, state ref, branch rule, or publish unlock is
configured.

## Scope

`thirdparty/tccbin_automation/targets.json` is the only registry of bundle targets. It records 16
known bundle branches and exactly six managed targets with native tccbin CI:

- `linux-amd64`;
- `macos-amd64`;
- `macos-arm64`;
- `freebsd-amd64`;
- `openbsd-amd64`;
- `windows-amd64`.

Branch discovery never expands this allowlist. A legacy branch must first gain an explicitly
reviewed native recipe, probes, target entry, and tccbin CI.

## Contract artifacts

The authoritative JSON schemas are under `thirdparty/tccbin_automation/schemas/`. Inputs use
UTF-8 without a byte-order mark. Duplicate keys, invalid Unicode, floating-point values, lexical
`-0`, and integers outside the interoperable safe range are rejected before map decoding.

Canonical hashes use the RFC 8785 representation implemented by the checked-in V code. Runtime
dependency downloads are not permitted. Three values remain distinct:

- the SHA-256 of the manifest bytes;
- the SHA-256 of the semantic input projection;
- the SHA-256 of the produced artifact projection.

The semantic projection excludes run IDs, attempts, timestamps, ledger generations, transaction
HEADs, candidate refs, and publication results. Consequently, moving a canonical branch from `A`
to `B` without changing semantic inputs preserves the input fingerprint, while the separate HEAD
comparison still prevents a stale publication.

## Provenance

The validator derives `complete`, `opaque-accepted`, or `incomplete` before native lanes run.
`incomplete` always blocks bootstrap, eligibility, no-op, promotion, and publication.

The version 1 registry contains one reviewed opaque acceptance. It binds
`windows-amd64:lib/openlibm.o` to its exact Git mode, SHA-256, ELF64 little-endian relocatable
x86-64 System V header, role, required probe, and x64-only lane. A manifest can reference this
acceptance but cannot create or widen it.

The static classification is not a gate result. A real Windows candidate must later prove that
the exact object is linked without fallback by the math, JSON, stbi, vorbis, and fontstash
consumer groups, and that no i386 consumer needs it. Phase A tests only the pure aggregator with
synthetic results; they are not native evidence.

## Durable state

Production state will live only on `refs/heads/tccbin-automation-state`. Its schemas are versioned
on the default branch. Each target has a monotonic generation, a target state, a publication
state, incidents, an optional active intention, and separate last-known-good and provisional
tuples.

An uninitialized target starts with `bootstrap_required=true` and no last-known-good tuple. The
only initial path is `initial_adopt_current`, which validates the existing canonical HEAD without
moving it. A complete green verdict seeds the tuple atomically. Publication and rollback remain
impossible until that seed exists.

An intention is reserved before a candidate SHA, tree, or output exists. A candidate binding is
created only after the remote create-only ref has been fetched and verified. The pure state
machine keeps the prior good tuple unchanged until post-publication validation succeeds.

All state changes use a generation compare-and-swap. Production writes must use GitHub's
`createCommitOnBranch(expectedHeadOid)` and verify the parent, tree, actor, and verified signature
after the mutation. A conflict is retried at most three times with full revalidation. No path uses
a forced update.

## Native gate correlation

Each native execution binds an immutable subject and `subject_generation`. A separate
`expected_ledger_generation` advances atomically with later target state writes. These two values
must never be conflated.

Gate epoch zero selects the run produced by the original immutable ref. Before any retrigger, the
state writer closes the current epoch and opens a new one with exactly one expected ref. The first
matching run selected by the state compare-and-swap wins. Late runs and all other runs exit before
native lanes and cannot emit the protected candidate context.

The accepted candidate requires both of these check names on the exact candidate SHA and from the
configured GitHub App sources:

- `tccbin-candidate-gate`;
- `v-candidate-smoke`.

An ordinary branch run uses `tccbin-branch-gate`. A stale or non-selected run uses
`tccbin-ineligible-run`.

## Source recovery

TinyCC, bdwgc, and libatomic_ops resolution uses three attempts: immediately, after 15 seconds,
and after 45 seconds. Each attempt has a 10-second connection timeout and a 60-second total
timeout. DNS, transient connectivity or TLS failures, timeouts, HTTP 429, and HTTP 5xx do not open
or update an issue.

After a transient outage, the resolver enters `upstream-recovery-daily`. It performs at most one
lightweight resolution per 24-hour period for as many days as necessary. It resumes only consumers
that were already persisted. A successful recovery plus a terminal green, no-op, or routed
functional verdict returns the source to the normal monthly cadence. Merely resolving a source or
dispatching a receiver does not.

The recovery workflow is separated into resolver, state-pre, dispatcher, and state-ack jobs. A
native-gate recovery for post-validation or remediation closes handoff H1 and creates pending
handoff H2 in one compare-and-swap. H2 runs the final V revalidation. H1 cannot be reused and no
second successor is allowed.

## Issue projection

The reporter maintains at most one open issue per owner repository and OS. Its stable identity is
the hash of `owner_repository + os`. The exact secondary key is ABI, target, architecture,
component, and failure class. Distinct tests and lanes remain nested evidence in that row; they do
not create another issue identity.

Bundle, patch, payload, recipe, native build, native probe, and artifact failures belong to
`vlang/tccbin`. Registry, resolver, state machine, selection, fallback, publisher, and independent
V integration failures belong to `vlang/v`. A V smoke failure belongs to tccbin only when a green
baseline and the exact candidate isolate the bundle as causal. Without that isolation it is
persisted as `ownership-ambiguous` under V, never attributed to both repositories.

Issue state is a projection, never an authorization. Closing or editing an issue does not remove
quarantine. A fresh validation of the integrated owner HEAD must pass before the bot resolves an
entry. Source outages do not create issue entries.

## Credentials and repository configuration

Production requires five separate GitHub Apps:

- state writer: V state-ref contents write only;
- validator dispatcher: V Actions write and read-only tccbin plus checks/statuses write;
- tccbin gate dispatcher: tccbin Actions write and contents read only;
- issue reporter: issues write on the one owner repository;
- publisher: tccbin contents write, constrained by protected environments and rulesets.

App IDs are repository or environment variables. Private keys are protected environment secrets.
No personal access token is part of the target configuration. The publisher token is minted only
inside the candidate-ref-create, gate-trigger-ref-create, or canonical-promote job and is destroyed
after that single operation.

The exact credential names are:

- `TCCBIN_STATE_APP_ID` and `TCCBIN_STATE_APP_PRIVATE_KEY`;
- `TCCBIN_VALIDATOR_APP_ID` and `TCCBIN_VALIDATOR_APP_PRIVATE_KEY`;
- `TCCBIN_GATE_APP_ID` and `TCCBIN_GATE_APP_PRIVATE_KEY`;
- `TCCBIN_REPORTER_APP_ID` and `TCCBIN_REPORTER_APP_PRIVATE_KEY`;
- `TCCBIN_PUBLISH_APP_ID` and `TCCBIN_PUBLISH_APP_PRIVATE_KEY`.

The required-check configuration is
`TCCBIN_ACTIONS_INTEGRATION_ID`, `TCCBIN_VALIDATOR_INTEGRATION_ID`, and
`TCCBIN_GATE_WORKFLOW_ID`. Missing, non-numeric, or aliased source IDs block every promotion. The
workflow ID must resolve to `.github/workflows/build-and-test.yml`.

Every unlock is absent or false by default. The complete variable set is:

- `TCCBIN_SCHEDULE_PUBLISH_UNLOCKED`;
- `TCCBIN_LINUX_AMD64_PUBLISH_UNLOCKED`;
- `TCCBIN_MACOS_AMD64_PUBLISH_UNLOCKED`;
- `TCCBIN_MACOS_ARM64_PUBLISH_UNLOCKED`;
- `TCCBIN_FREEBSD_AMD64_PUBLISH_UNLOCKED`;
- `TCCBIN_OPENBSD_AMD64_PUBLISH_UNLOCKED`;
- `TCCBIN_WINDOWS_AMD64_PUBLISH_UNLOCKED`;
- `MACOS_AMD64_LIBGC_PUBLISH_UNLOCKED`.

A scheduled publication requires the global unlock and its target unlock. A macOS-amd64 libgc
publication additionally requires the libgc unlock. A targeted manual publication does not depend
on the monthly target unlock; otherwise the two manual proofs required before enabling that unlock
could never be produced. The macOS-amd64 libgc safety unlock still applies to a targeted manual
publication that includes libgc.

The state writer is globally serialized as `tccbin-automation-state-writer`; publication and
promotion use `tccbin-automation-publication`. State compare-and-swap attempts occur at 0, 1, and
3 seconds. Candidate ref creation attempts occur at 0, 5, and 15 seconds. A native gate permits
one initial attempt and one infrastructure retry, each bounded to 90 minutes.

Server rulesets expand target IDs explicitly. Candidate patterns are
`tccbin-candidate/<target-id>/*`, with exactly one trailing segment. Gate-trigger patterns are
`tccbin-gate-trigger/<target-id>/*/*`, with exactly two. `File::FNM_PATHNAME` semantics apply:
`*` cannot cross a slash, `**` is never used, and unknown targets or extra segments are rejected.
Candidate and gate-trigger refs are create-only; publisher bypass never permits update, delete,
force, or bypass of either expected required check.

Before production activation, a repository owner must create the state ref, install the Apps,
configure their IDs and private keys, create the documented rulesets, bind required checks to the
expected Integration IDs, and leave all publish unlock variables false. These actions cannot be
simulated by repository code.

Credential rotation is performed one App at a time while all unlocks remain false. Replace its
protected key, run the corresponding no-write verification, revoke the old key, and then verify
that none of the other four roles gained permission. Never substitute another App or a personal
token during rotation.

## Local and pull-request validation

### Immutable validator bootstrap

`thirdparty/tccbin_automation/bootstrap/vc.lock` is the sole bootstrap snapshot. Its six ordered
records bind the canonical `vlang/vc` repository, one full commit and tree, and the Git mode,
blob, byte size, and SHA-256 of both `v.c` and `v_win.c`. It is parsed as data and is never sourced
as shell code.

`bootstrap/bootstrap.sh` accepts an already-present contract checkout and an already-present VC
checkout. Both must be exact, detached, clean (including ignored files), free of replacement refs,
and configured locally with `core.autocrlf=false`. The helper never fetches, pulls, invokes make,
or selects a moving revision. It clones the validated contract checkout privately, materializes the
locked VC blob into a separate work root, and builds `v1`, `v2`, and `v` at the private clone root
so that every stage resolves that clone's `vlib`. Those temporary compilers are removed before the
clone is re-attested clean. The final validator is emitted outside both input repositories, embeds
`contract_repository` and `contract_sha`, and must report the same pair through its
`contract-binding` oracle.

Git repository, object, index, template, replacement, lazy-fetch, prompt, and configuration
environment overrides are rejected before checkout access. V option, root, cache, module,
temporary, child, build-fact, no-run, job, and compiler/linker flag overrides are also rejected;
the helper creates private V directories and passes one resolved `CC` explicitly to every V
compilation.

The bootstrap interface is:

```sh
thirdparty/tccbin_automation/bootstrap/bootstrap.sh \
  <contract-root> <contract-repository> <contract-sha> <vc-source-root> <work-root>
```

Success is defined only by a zero exit status. The validator path is deterministic:
`<work-root>/tccbin-automation` on POSIX hosts and `<work-root>/tccbin-automation.exe` on Windows.
Standard output and standard error contain diagnostics only; callers must not parse either stream
to discover the validator path, a result file, or a last-line protocol.

Materializing the two exact checkouts is the caller's responsibility. A workflow must use reviewed,
immutable checkout actions and must not ask the bootstrap helper to contact a remote repository.

### Candidate staging preflight

The `candidate-preflight` command accepts an explicit managed target and transition kind, one local
tccbin candidate repository, its exact base and candidate SHAs, a fresh work root, and a
publication request. The candidate must be a clean direct child of the base. In `monthly` mode,
both revisions carry valid manifests. Their target, branch, recipe path, and policy projection are
stable. The recipe declaration (path, version, and SHA), its Git mode and blob, and every reviewed
patch and transform tuple/blob are immutable in this composer; recipe or control migration uses a
separately reviewed transition mode. The overlays, inventory, and outputs remain in the same
collections with the same static descriptors, and overlay bytes remain exact. The diff is closed
to the manifest and that fixed recipe/payload set; workflow changes, collection moves, path
replacements, and other automation files are rejected.

The command makes an independent local clone with lazy fetching disabled, checks out the candidate
detached, and exports only the exact union of inventory, overlays, and outputs. It does not follow
symlinks or create hardlinks, rejects special objects, and delegates the final byte, mode, Git
blob, provenance, contract-binding, and publication decision to the existing staged authenticator.

```sh
tccbin-automation candidate-preflight \
  <target-id> <monthly|legacy-onboard|baseline-activate> <candidate-repo-root> \
  <base-sha> <candidate-sha> <work-root> <publish-requested>
```

The successful work root contains detached `base-source/` and `candidate-source/` clones and the
payload-only `payload/`. A caller may continue only when the command reports both `eligible=true`
and, for publication, `publish_allowed=true`.

`baseline-activate` is validation-only: `publish-requested=true` is rejected immediately after its
reviewed policy is loaded and before candidate input or a work root is touched.

### Candidate composition and dormant legacy onboarding

`candidate-compose` constructs a candidate without mutating its reviewed base or the RAW build
root. Its request fixes the target, transition kind, base repository and SHA, RAW root, external
manifest, and a result root that must not exist. Inventory and outputs are copied only when named
by the manifest; RAW extras are ignored. Overlays and every control input come from the immutable
base. Payload names such as nested `build.sh` or `.patch` files are data when their exact paths are
declared in a reviewed collection; names and suffixes are never control-plane authority. Before
parsing or replacement, the manifest is attested as an exact private physical `100644` Git blob,
or as exactly absent for a legacy base. Replacement uses a private regular temporary file and a
no-follow rename. The exact desired Git tree is written through an explicit no-filter index,
committed once as a direct child with deterministic bot identity and time, and passed to the real
publication-disabled candidate preflight.

```sh
tccbin-automation candidate-compose \
  <target-id> <monthly|legacy-onboard|baseline-activate> <base-repo-root> <base-sha> \
  <raw-root> <manifest> <result-root>
```

Success writes nothing to standard output. A sibling private transaction is renamed atomically
only after preflight; the final root contains only `candidate-repository/`, with a clean detached
HEAD at the new direct-child commit. Failure leaves the requested result absent. An incomplete
legacy candidate may be exposed only for the exact `staged_provenance_incomplete` decision and can
never authorize publication; every other ineligible decision is terminal.

Each managed target carries a legacy base SHA and a nullable reviewed-policy path/hash pair in
`targets.json`. The six production pairs are intentionally null, so `legacy-onboard` currently
fails with `target has no reviewed legacy onboarding policy` before reading any candidate input.
Future policy files live at `onboarding/<target>.policy.json`. Their canonical JSON v1 projection
pins target and branch, source identities without resolved SHAs, the reviewed toolchain profile
ID and hash, recipe
identity, patch and transform semantics without byte hashes, probes and effects, and the static
partition of overlays, inventory, and outputs. Runtime contract identity, resolved source and
toolchain observations, byte digests, and derived provenance status remain outside that policy.

### Dormant managed-baseline activation

`baseline-activate` is the one-time migration boundary for the six manifest-bearing Phase A
baselines. It is distinct from `legacy-onboard`: the reviewed base already contains an automation
manifest, so it cannot satisfy the legacy rule that the base manifest be absent. It is also
distinct from `monthly`: every Phase A base has an all-null toolchain binding and incomplete
provenance, while a monthly transition requires authenticated producer observations under the
same non-null reviewed profile in both manifests.

Each target's `managed_baseline_activation` registry object seals the exact integrated Phase A
commit, tree, sole parent, manifest byte hash, and the immutable contract repository/SHA recorded
by that manifest. The six reviewed anchors are:

- `freebsd-amd64`
  - Baseline commit: `e71cda6242e88e47312ca9bfc4548b0579636e0c`
  - Baseline tree: `9438879ad9906e970d45bafacf6ce2cc63ae4c53`
  - Sole parent: `fdf5cdfea6ea84612e068bc3bea433dbba263404`
  - Manifest SHA-256: `a9c2f15451a7e94261c6dd4d9e47cc3965414a2179d04af5902dffc9471a4db3`
- `linux-amd64`
  - Baseline commit: `d6e7ac1b1bcc98aed734a6ecbfa8509f24606c74`
  - Baseline tree: `22851c0f356fefcb63718ce63d50a870150a491c`
  - Sole parent: `ece46f06fbe6eb701d52442f11dd59c48d166cae`
  - Manifest SHA-256: `bcdce1bea1facb24175229a16dc6e8a2c4210aafc05f0526b2728dc060223ed4`
- `macos-amd64`
  - Baseline commit: `199fa78395ca413aac23d02ec69cc5e7b1d805a2`
  - Baseline tree: `db67711bfeb33be63dbc8eb03ecdddc2e127cc8c`
  - Sole parent: `da8ac5a4369accc67c485191d02535d77718a1c8`
  - Manifest SHA-256: `09dc54928f4690cfee7fd113de93f36479a40cabd10eeb3ee416a3175b42270a`
- `macos-arm64`
  - Baseline commit: `1d0ad0ecf70a91a1df64cebf215e683b1d5aedb5`
  - Baseline tree: `96af5121f065310ba9168e3e2dc61adf340e2738`
  - Sole parent: `274abd2466a14861b75e5b91fd946ad27d114499`
  - Manifest SHA-256: `be45aaee1e65cc1ed2ee6bd2f121d72cbc248887ffa3d57f4b6d59cb6ea73525`
- `openbsd-amd64`
  - Baseline commit: `8c7d96c75ea8548f007432d70f1ae33cccd81838`
  - Baseline tree: `f75c4862711184c4c191a73e6f996eb421bd37b4`
  - Sole parent: `45230fde96c17fff4baf37deb55e90803c043063`
  - Manifest SHA-256: `873b62af697ba25f0abe5887ba05972a93bd48990233853b362bed6cb9137699`
- `windows-amd64`
  - Baseline commit: `86ae5844b8b56071b21ae3aa138b247d5eb9ddd9`
  - Baseline tree: `818d7794ebdf41de60e5679d485e3a5d49272171`
  - Sole parent: `f7c7199bb87fda8b80b31fefa470b2efc952326b`
  - Manifest SHA-256: `b5728124ecf8dc01e4f16cf4188411d6f633bb57997ef36df2dc5fd182e8535a`

All six anchors bind `base_contract_repository=vlang/v` and
`base_contract_sha=7545e515b434cd399333d43659238427d72e22e7`. Their policy path/hash pairs
are deliberately null. Consequently, `baseline-activate` stops with
`target has no reviewed managed baseline activation policy` before it reads candidate input. A
future reviewed policy will be canonical JSON at
`baseline-activation/<target>.policy.json`, using onboarding-policy projection v2 and binding the
resolved target toolchain profile. Projection v1 remains exclusive to legacy onboarding. The v2
policy has one closed `source_commit_evidence` entry for every source and no extra entry. External
sources seal ID, repository, ref, commit SHA, tree SHA, and canonical base64 of the bounded raw Git
commit object. The validator reconstructs `SHA1("commit <length>\0" + raw)`, requires the first
and only `tree` header to match, and compares the candidate's complete five-field source tuple to
that reviewed evidence. Candidate source or provenance fields are never their own authority.

The Windows `v-libgc` entry instead has only `authority=runtime-contract`, its fixed source ID,
repository, and ref; it cannot auto-pin a SHA or tree into the policy stored by the same V commit.
At activation time its candidate SHA must equal the embedded runtime contract SHA, and its tree
must equal the tree resolved for that exact commit in a separate hardened V Git checkout. That
checkout must be a complete SHA-1 repository with `core.autocrlf=false`, no redirects, alternates,
grafts, or replacement refs, the exact `vlang/v` HTTPS origin, and a clean tracked, detached HEAD at
the runtime SHA. The updater must supply this private checkout; a mutable Actions branch checkout
is not activation authority. The base anchors remain audit inputs only: they are not source
provenance, native evidence, last-known-good tuples, or permission to seed durable state.

For an activated candidate, every payload entry with complete provenance binds its provenance
repository to the SHA from reviewed commit evidence, or to the runtime SHA for `vlang/v`.
`vlang/tccbin` provenance instead binds to the sealed baseline commit. Unknown repositories,
sources absent from the target's closed source matrix, and SHAs borrowed from a different source
are rejected.

This foundation introduces no production profile, onboarding or baseline-activation policy,
producer observation, source SHA/tree resolution, complete payload provenance, state writer,
candidate ref, check producer, or publication path. It therefore cannot make a target eligible
and cannot publish even when a caller requests publication. In particular, the current Phase A
manifests keep every source SHA/tree and toolchain member null and classify every payload entry as
incomplete; Windows additionally retains its separately reviewed opaque `openlibm.o` acceptance.

The updater is not yet a producer for this transition. Its Unix jobs still invoke the legacy
in-place recipes instead of setting `TCCBIN_DEFER_COMMIT=1`, producing a private RAW root, and
calling `candidate-compose`; Windows is not present in its selectable target matrices. The current
Linux and BSD jobs also request moving `ubuntu-latest` hosts while the reviewed profile model
requires `ubuntu-24.04`, the macOS ARM64 updater job requests `macos-latest` while the model
requires `macos-15`, the Windows branch requests `windows-latest` while the model requires
`windows-2022`, and BSD cross-platform-actions runtime assets remain unauthenticated. These gaps,
plus native producer/validator observations and external evidence transport, must be closed before
any policy pair is populated.

Patch and transform retirement is likewise not activated here. The current manifest vocabulary
records Windows patch state and effect/probe bindings, but no workflow yet builds an otherwise
identical unpatched counterfactual, derives a closed
`required`/`redundant`/`broken`/`unknown` verdict, or authorizes a removal or issue write. Until a
later reviewed lifecycle binds those facts durably, an apply failure, unexpected pass, or changed
effect remains fail-closed and requires human review; it must never be interpreted as proof that a
patch or transform can be silently dropped.

### Dormant toolchain identity profiles

Each managed target also carries an all-null or all-resolved `toolchain_profile` triple in
`targets.json`. The six production triples are intentionally null. A resolved profile must be
canonical JSON at `toolchain-profiles/<target>.profile.json`; its target, profile ID, and SHA-256
are bound by the registry. No production profile file or toolchain observation is introduced by
this dormant contract. Profiles and observations are read only from non-symlink regular files,
under a 512 KiB bound. Before reading, the no-follow path snapshot must identify the opened native
handle. POSIX opens with read-only, nonblocking, no-follow, close-on-exec, and
no-controlling-terminal flags where available, then immediately applies `fstat`; Windows opens the
reparse point itself and requires a regular disk handle before reading. After reading, the handle
snapshot must be unchanged and the final no-follow path snapshot must still identify that handle.
Regular-file type, mode, size, native file identity, POSIX modification/change seconds and
nanoseconds, or opaque Windows timestamp ticks are compared independently at both boundaries;
timestamps are never collapsed through potentially overflowing arithmetic.

A profile has exact, lexically ordered producer and validator roles. Each role selects one closed
identity strategy: GitHub-hosted, cross-platform-actions host, cross-platform-actions guest, or
GitHub-hosted MSYS2. The strategy fixes the complete fact-name and match-policy set. Reviewed
facts are either exact, required non-empty, a lowercase SHA-256, or a compatible guest release.
The MSYS2 strategy fixes `UCRT64`; action identities are full lowercase commit SHAs. An observation
must contain exactly the selected phase's roles and exactly the strategy's fact names in lexical
order.

The target also fixes the topology and reviewed static values. Linux and both macOS targets use
one GitHub-hosted role per phase; each BSD target uses one CPA host and one CPA guest role per
phase; Windows uses one GitHub-hosted MSYS2 role per phase. Runner label, OS, architecture,
compiler command/family, BSD guest OS/release, and the Windows package are target-bound. Exact
compiler-target spellings are not yet supported by reviewed native evidence, so
`compiler_target` is deliberately a required non-empty observation rather than a profile value.

Every role resolution is hashed with the target, profile ID and hash, phase, role, strategy, and
resolved facts. The top observation digest covers the complete canonical observation without its
own digest, including every evidence SHA-256 and role-resolution digest. Timestamps, run IDs,
attempts, and volatile filesystem paths are not schema members. Profile matching does not replace
future authentication of the validator observation, native lane evidence, hosted image provenance,
compiler loader/PATH/SDK policy, or native Windows and BSD execution; those remain a later T2/B2
workflow gate.

The bundle manifest carries exactly `profile_id`, `profile_sha256`, and an embedded
`producer_observation`. All-null and reviewed-profile-with-null-observation forms are structurally
valid but derive `incomplete` provenance. Only a producer-phase observation authenticated against
the registry profile can make the toolchain complete. A resolved-inputs object therefore always
retains the profile ID/hash plus the canonical observation SHA-256 and derived observation digest;
if producer identity is unresolved, the entire resolved-inputs object and any active intent remain
absent. Target-root and active-intent resolved inputs are identical, and verdict consumers rebuild
their canonical manifest projection and recheck the manifest input fingerprint before accepting
evidence.

Monthly composition requires both base and candidate manifests to carry authenticated producer
observations under the same non-null reviewed profile ID/hash. The observation may refresh as a
dynamic input and changes the input fingerprint, but monthly composition cannot activate a null
observation or migrate the reviewed profile. Legacy onboarding policy likewise freezes only the
profile ID/hash. The two production manifest fixtures intentionally keep the all-null form until
the six real profiles and producer observations are reviewed.

At this T2a boundary, `evidence_sha256` is producer-declared and digest-bound but is not yet
authenticated against a workflow artifact. Independent evidence production and validator
observation verification belong to the next T2 gate; a producer observation alone cannot authorize
a build or publication.

### Dormant native lane matrix and verdict bridge

T2b1 defines a canonical native-lane matrix without connecting it to a CLI, workflow, or
publication. The matrix file contains schema version 1, the complete native-gate subject and its
derived subject hash, the four-field producer identity derived from the authenticated manifest, a
complete validator-phase observation under the same sealed reviewed profile, the selected
run/attempt/check-suite triple, and every lane result in exact manifest order. The file has no
self-declared matrix digest. Its private authenticated digest is the SHA-256 of the complete exact
JCS bytes.

Each result records its probe and lane, the manifest-derived expected lane count, one closed
`passed`, `failed`, or `blocked` status, a SHA-256 for its external evidence, fallback and linked
object facts, and the exact openlibm consumer group where applicable. Each expected-lane array and
the fully materialized result matrix are bounded to 1,024 entries. A manifest lane is always a
normal lane ID, and every non-patch probe declares at least one. When and only when the patch list
is empty, `patch-probes.expected_lanes` is empty and the matrix contains the single exact
`patch-probes` result `expected=0`, count zero, passed, without fallback, linked object, or consumer
group. Windows declares exactly five x64 openlibm consumers: `fontstash`, `json`, `math`, `stbi`,
and `vorbis`. A passed openlibm result without fallback must bind its exact consumer and set
`object_linked=true`. A passed result with fallback, or a failed or blocked result, remains a
representable red fact even when `object_linked=false`; it never authorizes a green verdict.

The sole public loader accepts a path, reads at most 512 KiB through the stable no-follow regular
file reader, requires exact canonical bytes, and returns a sealed envelope. The producer tuple is
rederived from the manifest; the validator is checked against the complete profile snapshot
retained when that manifest was authenticated, so a second registry or profile read cannot change
the authority. Validator-role evidence hashes are sealed by the observation, and lane evidence
hashes are sealed by the matrix bytes, but T2b1 does not download or authenticate the external
evidence artifacts themselves.

T2b2 removes the synthetic lane-evidence verdict path. Each in-process green or red proof now
carries the authenticated manifest, the sealed matrix, and two complete 32-field `gate_run`
projections. The proof subject is never caller-supplied: it is the target's durable active native
subject. A private bridge reparses and fully reauthenticates the matrix's raw JCS bytes against that
subject and manifest. It then binds the matrix subject hash and selected run/attempt/check-suite
triple to the completed write-once native winner, and requires the native gate output digest to be
the exact matrix byte digest. The V smoke proof is independently bound to its allowlisted workflow,
validator Integration, trusted `master`, and resolved V source SHA.

A green verdict requires a passed matrix without fallback plus successful native and V run/check
conclusions. A failed lane, fallback, or run/check failure is functional. Only when no functional
fact exists, a blocked lane or cancelled, timed-out, neutral, or skipped run/check is
infrastructure; functional evidence therefore dominates mixed infrastructure evidence. A publisher
failure is representable only after the matrix and both gates are already green. Neither the
functional nor infrastructure class remains caller-selectable independently of these sealed facts.

Durable intent collection is non-authorizing. Pre-check stages retain zero gate projections.
`checks_running` and `checks_waiting_source` may progressively retain zero, one, or both complete
gate kinds, at most one of each; two are ordered native then V. `aborted` and `superseded` may
retain the same bounded history without authorizing anything. Green, promotion,
post-check, completed, and blocked stages retain exactly the ordered pair. The V model
revalidates the complete shape and order; the durable schema closes cardinality, kinds, and order
with the locally validated Draft
2020 `prefixItems` contract.

T2c1 adds only a dormant physical capsule authenticator. The capsule root is an exact absolute,
physical, non-symlink directory containing only `native-lane-matrix.json` and an `evidence/`
directory. The latter contains exactly one physical, single-link regular file named by each unique
lowercase evidence SHA-256 declared by the producer roles, validator roles, and lane results. It
contains no caller-supplied index, manifest, gate projection, sidecar digest, or capsule schema.
The authenticated manifest and complete subject remain sealed API inputs.

The matrix keeps its 512 KiB bound. Each nonempty evidence file is at most 256 KiB; the matrix plus
all unique evidence files is at most 16 MiB. The authenticated topology permits at most 1,028
producer, validator, and lane evidence declarations; the physical walker independently stops before
materializing a 1,057th directory entry. The loader anchors both directories with native handles,
enumerates and snapshots them before and after use, opens every child relative to its locked parent,
requires exactly one physical link, and rehashes each file against its name. Its private capsule
digest is derived from canonical JSON binding the manifest hash, subject hash, exact matrix digest,
and the sorted unique evidence hash/size list. No digest supplied in the capsule is authoritative.
On Windows, child opens use `NtOpenFile` with the parent handle as `RootDirectory`, and directory
enumeration uses `NtQueryDirectoryFile(FileNamesInformation)` on that same handle; neither operation
reconstructs a pathname. The bundled TCC headers omit `winternl.h`, so the C-only wrapper carries a
reviewed, prefixed minimal ABI for `UNICODE_STRING`, `OBJECT_ATTRIBUTES`, and
`IO_STATUS_BLOCK`, with compile-time size and offset assertions for x86 and x64. Variable directory
records are parsed only as bounded bytes with `memcpy`; no NT layout crosses into V.

These bounded evidence files are compact attestations. A larger compiler log, binary, archive, or
runner artifact remains external; merely hashing a compact attestation that names such an artifact
does not authenticate the larger artifact. T2c3 must transport and rehash those external bytes and
bind that result through the sole reviewed workflow/API producer before activation.

T2c2 defines a distinct durable `last_native_validation` replay fact without adding a live writer.
Its schema-version-1 object has exactly fourteen members: `schema_version`, `operation_id`,
`transition`, `resulting_generation`, `verdict`, `manifest_source`, `manifest_hash`,
`native_lane_matrix`, `matrix_digest`, `evidence`, `capsule_digest`, `native_gate`, `v_smoke_gate`,
and `validation_digest`. The manifest source retains at most 512 KiB of the exact authenticated raw
bytes and its byte hash. The matrix remains an object and is rebound through SHA-256 of its complete
canonical JSON. The evidence projection is the sorted unique hash/size set from the authenticated
capsule, under the same 1,028-file, 256-KiB-per-file, and 16-MiB-total bounds. The validation digest
commits the other thirteen fields under `vlang/v:tccbin-native-validation-record:v1`.

Every non-publisher green, functional, or infrastructure verdict derives a fresh record from the
sealed capsule and both complete gate runs in the same target CAS. Replay rejoins the record to one
retained operation, the exact target and manifest, the producer tuple, full subject hash, selected
run/attempt/check-suite triple, native output digest, distinct native and V gate authorities, and
the derived verdict. Replay also enforces the transition-specific consumer kind and rechecks every
URL, ref, SHA, actor, Integration, deterministic external ID, timestamp and conclusion in both full
gate runs. Durable JSON replay additionally validates the retained raw manifest against the current
bundle schema and reviewed registry/profile, recomputes its manifest/input/artifact projections and
complete resolved inputs, then reauthenticates the canonical matrix source and validator observation
against that same profile. The deliberately non-staged replay rejects production or opaque records
until T2c3 can carry their observed staging proof. Candidate promotion revalidates the candidate
record before changing HEAD; normal publication additionally requires a green record for the exact
last-known-good tuple. The durable `resolved_inputs` root is closed independently: `source_checks`
must be a one-to-one resolved evidence set for `sources`, while only the seven manifest-backed
fields are compared to the authenticated manifest projection. A blocked rollback-candidate,
publish-post, or rollback-post red record must match its branch-specific kind, owner ID and exact
artifact/validation tuple, then equal the active subject/hash, ordered intent gates, selected native
winner, and matrix output. A
publisher failure is not a new validation result and therefore preserves the prior green record.
The same rule applies to a publisher failure during rollback, while a functional or infrastructure
rollback failure replaces the record with its newly authenticated red facts.

Target-state schema migration adds the nullable field to all nineteen historical fixtures. The V
state model nevertheless refuses a newly checked or eligible target without its derived record.
Terminal state projections are version 3 and carry the field; terminal revalidation is version 5
and commits it. The version-1 terminal owner payload deliberately excludes mutable execution and
validation evidence, so an owner identity does not change merely because a later verdict is
recorded. Recovery H2 has no capsule authority or publisher fact in T2c2: every legacy,
source-waiting, or terminal projection must preserve explicit null-to-null. Any non-null H2 record,
including a preserved green publisher record, fails closed until the T2c3 writer supplies the exact
projectable authority.

**Activation is still refused at this boundary.** T2c2 adds no live-state serializer, CLI, workflow,
or capsule producer. T2c3 must make the single reviewed writer materialize the record within the
existing 2-MiB target-state bound, transport and rehash the external artifacts, bind six reviewed
target identities and pinned actions, and close its compare-and-swap persistence. Until that gate
is reviewed, neither a capsule nor an in-memory record can authorize workflow publication.

T2c3a introduces only a deterministic preparation boundary for one target-state transition. It
authenticates the supplied predecessor bytes independently with SHA-256 and their Git blob OID,
loads the complete strict and schema-valid JSON root, applies the typed state machine, replaces only
the 27 members owned by `TargetModel`, and emits JCS bytes without a trailing newline. The three
immutable members, the complete incident objects, and the eight receiver/recovery/reporter-owned
members are preserved from the authenticated root. A transition that changes incidents or requires
one of those companion owners fails closed. The prepared bytes are reparsed, revalidated, projected
back to the exact resulting model, and kept inside the existing 2-MiB target bound.
Every JSON integer decoded into a V `int`, including nested native gates and validation records, is
range-checked by an exact `i64 -> int -> i64` round trip before shared parsing. Durable native
subjects preserve their authenticated digest-array order at both root and execution locations;
only the separate subject-hash projection sorts digests for its canonical hash.

The prepared artifact is explicitly not a writer or a capability. Its `expected_state_head_oid` is
only a validated lowercase correlation token copied from the caller; it neither authenticates the
state ref nor authorizes a compare-and-swap. T2c3a adds no CLI, workflow, network request, GraphQL
mutation, ref lookup, commit, or publication path. A later single-writer layer must independently
authenticate the current state HEAD, call `createCommitOnBranch(expectedHeadOid)`, and
reauthenticate the resulting parent, tree, App actor, and verified signature on every bounded CAS
attempt.

T2c3b adds only the dormant local authority adapter around that prepared artifact. The adapter
accepts a target identity and typed transition facts, never caller-supplied state bytes, generation,
path, HEAD, blob OID, SHA-256, or preparation preconditions. It reads the closed public-proof
bundle and complete bare-repository inventory twice from scratch. Each pass authenticates the
protected ref, commit, tree, sole parent, verified signature, State Writer App and actor, then
derives the target path and joins one exact mode-100644 tree entry to its bounded blob bytes,
SHA-256, schema, semantic model, and ledger generation. Only two completely equal passes may bind
the private T2c3a preconditions and return the deterministic prepared bytes.
The `ls-tree -z` framing is closed: one nonempty record and one terminal NUL are required, with no
leading NUL, doubled terminator, empty record, or second record accepted.

`ReauthenticatedPreparedTargetStateWrite` intentionally exposes only copy getters. The returned
proof and prepared write are observations, not sealed values: a caller can forge or alter its own
copies, and no production consumer accepts them. The proof is local and non-fresh with respect to
the remote service; the adapter performs no network lookup, Git or filesystem write, ref update,
GraphQL request, `createCommitOnBranch`, retry, or compare-and-swap. It computes no authority for
a later moment. A future single-writer layer must fetch a fresh remote proof, submit the exact
authenticated HEAD as `expectedHeadOid`, and independently authenticate the resulting commit,
parent, tree, App actor, and verified signature before any outcome is durable.

The update workflow encodes full commit pins for checkout, upload-artifact, and
cross-platform-actions, checks out the locked VC commit, and bootstraps from a physical private
contract clone bound to `GITHUB_REPOSITORY/GITHUB_SHA`. Those source pins do not authenticate the
VM image, hypervisor, or resource archives that cross-platform-actions downloads dynamically, and
they do not make native guest results authoritative. No byte digest currently binds those assets;
the action still starts its hypervisor through `sudo`, and `cpa.sh --sync-files both` can rewrite
the broad hosted-runner workspace synchronization root, including material later consumed from
`RUNNER_TEMP`, before preflight or publication. BSD Phase A and publication therefore remain
non-authoritative and activation remains blocked. GGRei-only tests must place CPA last, expose no
secret, produced artifact, or validator to it, and disable return synchronization. Native execution
of the bootstrap and file-identity helper on Windows, the guest BSD runners, and the macOS Bash 3.2
host remains a required B2 gate, together with review of those CPA runtime assets and the hosted
toolchain, loader, PATH, and SDK policy. Until those gates and the six target manifests exist, this
local bootstrap and candidate preflight do not authorize any workflow publication.

From the V repository root:

```sh
./v -g -keepc -o ./vnew cmd/v
./vnew -silent test thirdparty/tccbin_automation/tests/
./vnew run thirdparty/tccbin_automation/bin/cmd contract
./vnew run thirdparty/tccbin_automation/bin/cmd dry-run
```

The pull-request workflow always emits `tccbin-automation-contract` and
`tccbin-automation-dry-run`. It has `contents:read`, receives no secret, and cannot create refs,
state, issues, dispatches, or publications.

The scheduled recovery, revalidation, and issue workflows are also no-write in Phase A. Their
jobs exercise schemas and pure decisions only. The receiver simulations do not trust the opaque
input: they resolve it in a closed ledger fixture. The target's consumer pointer equals the
selected entry's immutable intent identity (and the consumer entry's identity), while its recovery
pointer selects the exact handoff. When consumer and handoff projections coexist, both preserve the
same exact target, intent, and complete subject. Every request,
`workflow_run` completion, and active-recovery lookup joins the selected entry back to one target's
generation, canonical HEAD, input/artifact/manifest fingerprints, active consumer, active subject,
subject ref, and subject hash. Canonical subjects also bind their expected HEAD to the subject SHA;
the workflow repository/ID/path/ref/event and ACK-selected run/attempt remain exact. The derived
result always has
`publish_allowed=false`; an unselected run stops before tests, secrets, state writes, ref creation,
or publication. The issue reconciliation job likewise re-reads a closed, byte-bounded snapshot of
the protected state ref, validates its state commit, target path/generation, owner, OS,
fingerprints, and incident diagnostics, then derives the single owner+OS projection before its
reporter tests. It has neither an issue credential nor issue write permission. Later phases must
not treat these dry runs or their synthetic lane results as native bundle proof.

### Dormant durable target commit planning

The first offline commit planner is deliberately limited to the two ledger-repair transitions.
It accepts a seeded `unknown_blocked` target only when the publication lane is idle and every
active intent, native consumer, V smoke, remediation, recovery, and native-validation companion
is absent. A repair with incidents remains quarantined; a repair without incidents returns to
validation. All other transition names, including `start_build`, stale, source, publication,
rollback, and remediation transitions, remain outside this adapter.

Before opening a schema-dependent authority, each of two independent passes authenticates the
local bare state HEAD and its public proof, reads the complete `ls-tree -rlz --full-tree`
inventory, joins every blob to its mode, size, Git OID, and SHA-256, and reconstructs the old root
tree in memory. Exactly six targets and three source states are required. All six target blobs must
carry `last_native_validation: null`, the proof bundle's historical directory must be empty, and
no terminal source handoff or business evidence may be present. The only automation-root inputs
are then the recursively discovered, physically re-read eleven-file target/evidence/source schema
closure. Only after the raw null-six scan, a pure validator receives the already acquired complete
blob set; it has no repository loader. The planner constructs its private target observation and
prepared transition from that same snapshot and does not call the public T2c3b reauthentication or
history path. Any missing, extra, linked, escaped, or changing dependency fails closed.

This planner is currently a POSIX-only MVP. On Windows its first effective instruction returns
`durable target commit planning is unavailable on Windows without a raw-byte Git runner`; it does
not open a proof, configuration, repository, or child process. The older T2c3b authority remains a
read-only Windows-capable observer, but its inherited process adapter is deliberately not reused by
the commit planner. Linux uses mandatory `close_range`; macOS, FreeBSD, and OpenBSD use mandatory
`closefrom`. Other POSIX targets fail before `fork` until an exhaustive descriptor-close primitive
is specified for them.

On supported hosts one private, exclusive child-reaping lease requires unchanged default
`SIGCHLD` ownership, finite descriptor limits, and a dedicated quiescent process with no competing
`wait*`, `pclose`, process wrapper, or signal mutator. Each Git invocation is a direct bounded
`fork`/`execve` with stdin on the null device, independently drained raw stdout and stderr pipes,
a reserved process group, monotonic deadline, and a guarded one-byte release gate. The child closes
the inherited gate writer, consumes exactly `G` then EOF, closes every descriptor from three up,
and only then executes an absolute Git binary. The parent preserves its gate reader through the
single write to avoid `SIGPIPE`, closes reader then writer, observes both output EOFs before any
wait, and kills the group and PID before reaping on any pre-EOF failure. PID, process-group, signal,
pipe, byte, and wait results are all fail-closed. Abort request, proved termination, and terminal
reap are distinct states: a hard signal failure is never represented as termination. Polling and
reaping have monotonic bounds and no blocking wait is used. The unchanged `SIGCHLD` snapshot is
checked immediately before every signal attempt, signal retry, and `waitpid`; drift poisons the
session and permits no later signal or wait. In particular, post-fork drift before release leaves
GO, kill, and wait counts at zero and retains the lease. `ECHILD` and foreign wait identities share
one closed child-reaping-exclusivity failure. A recorded abort failure remains fatal after pipe EOF
and cannot be masked by a later zero child exit, signal-check failure, or cleanup error; the public
planner stores and preserves the session's first causal runner failure before any legacy
`os.Result` projection, then considers the core and cleanup fallbacks in that order. The
post-fork route, GO/kill/wait authorization, action accounting, and atomic lease acquire/release
are single private transitions shared by production and closed test seams. If a
child cannot be terminated or reaped inside those bounds, the session is poisoned and retains its
exclusive lease so a second reaper cannot start in the same process.

The child receives only `PATH`, fixed C locale variables, no-lazy-fetch/no-prompt/no-lock Git
settings, and null system/global configuration. Every argv includes `--no-pager`,
`--no-replace-objects`, and `--no-lazy-fetch`; history reads additionally disable signatures,
external diffs, text conversion, rename detection, color, decoration, notes, mailmaps, and custom
ordering. The physical local config is a stable regular single-link file bounded at 64 KiB. Its
bytes are lexed as one closed core/remote document before Git runs; its independently emitted
NUL-framed key sequence must be an exact order-preserving join. Both admit only the three named
core keys and safe remote URL/fetch/mirror keys.
Includes, worktree config, promisor/partial-clone/filter settings, alternates, grafts, shallow or
common-dir topology, trace variables, pagers, signing helpers, diff helpers, and all other config
are rejected before object reads and rechecked afterward.

The planner commits the complete proof, physical inventory, schema closure, selected target and
unique resolved source/check row, invocation facts, transition template, global null-six fact,
bounds, and exact-two postimage policy into a domain-separated JCS plan subject. It alone derives
the operation ID at CAS attempt one; callers cannot supply a `TransitionContext` or reserve an
operation ID. The ID is rejected if it or the planned evidence path already occurs in any target,
incident, source operation window, handoff, gate, or evidence identity. The resulting artifacts
are one canonical target replacement and one create-only exact-20 evidence blob. Their Git tree is
also reconstructed entirely in memory, with all other entries preserved byte-for-byte. Collision
scans include active intent and both predecessor/successor handoff identities. A single canonical
path map, component trie, bottom-up tree pass, and two-cursor exact-two comparison replace repeated
whole-inventory scans. The predecessor must leave the one evidence slot needed for the postimage;
99,999 entries may become 100,000, while a 100,000-entry predecessor is rejected before
postimage construction. File, aggregate byte, path, stdout, stderr, history, and deadline additions
are checked before allocation or append.

The public result has private storage and returns only detached, forgeable observations. It is
stale as soon as returned and is neither a proof, write request, retry token, commit, ref update,
remote observation, nor CAS capability. No CLI, workflow, transport, network call, filesystem
write, or Git mutation consumes it. A future writer must reauthenticate remote and local state and
replan before every attempt. The private reducer only describes bounded outcomes: ambiguity needs
reconciliation on the same attempt; confirmed conflicts require a fresh plan for attempts two and
three after one and three seconds; a third conflict becomes `unknown_blocked`.

The planner trusts `automation_root`, the physical state Git directory, and the dedicated host to
remain administratively quiescent during one call. Repeated handle/path/config/proof/inventory
checks detect the modeled mutations, but they are not a universal filesystem ABA proof and do not
authenticate the installed V executable. Activation therefore still requires an immutable V
commit, a separately reviewed all-platform raw runner, fresh remote authority, and a real CAS
writer; none is implied by this dormant observation API.

## Rollback procedure

Rollback is a normal revert commit, never a forced branch move.

1. Persist quarantine and the bad provisional tuple while preserving the prior good tuple.
2. Confirm that the canonical HEAD is still the exact bad provisional SHA.
3. Reserve a rollback intention without candidate outputs.
4. Build a complete revert of the transaction with the bad SHA as its sole parent.
5. Create and fetch an immutable candidate ref, then bind its exact tree and digests.
6. Require both native and V checks from their expected Apps.
7. Promote the same SHA by a bounded compare-and-swap.
8. Post-validate the revert before replacing the last-known-good tuple.
9. Keep the original incident active until a separate remediation validation is green.

If the canonical branch has a human descendant, the controller validates that descendant through
`adopt-current`; it never reverts it blindly. An unavailable source resumes the same rollback
subject. Before binding, a rebuild is allowed only when the candidate ref is confirmed absent and
all inputs remain unchanged. After binding, only the already-bound SHA may resume.

## Ledger recovery

An unreadable, unsigned, ambiguously written, or schema-unknown state ref yields
`unknown_blocked` for every affected target. Keep all unlocks false and do not infer state from an
issue, artifact, cache, or tccbin branch alone. Re-fetch the last verified state commit and the six
canonical refs, reconstruct each target from immutable evidence, and prepare a schema-valid repair
on the state ref. The state writer applies it with `createCommitOnBranch(expectedHeadOid)` and then
verifies parent, tree, App actor, and signature. A repaired target returns to `quarantined` when a
blocker exists, otherwise to `validating`; it never returns directly to `eligible`.

If issue reconciliation failed after quarantine was persisted, rerun only the reporter projection.
If state persistence failed, do not open or close issues as a substitute. Issue closure follows a
fresh exact validation and the resolving commit recorded in the ledger.

## Activation order

The contract and validators merge before any tccbin manifest. Fork manifests use
`contract_mode=fork-dry-run` and cannot publish. After the V contract is merged, every managed
tccbin manifest must point to the immutable merged V SHA in production mode and rerun all gates.
Only after those branch changes merge can a V follow-up seal their exact upstream HEADs as dormant
managed-baseline anchors. Sealing is not activation: the baseline policy pairs and all six
toolchain-profile triples remain null, provenance remains incomplete, and no durable state is
seeded.

Before a real `baseline-activate` candidate can be composed, a later reviewed phase must make the
authentication and evidence-transport infrastructure authoritative, add the six resolved profiles
and baseline policies, authenticate the producer environments, resolve every source SHA/tree, and
generate complete per-file provenance. Validator observations and candidate-bound native proofs
are produced and transported only after `candidate-compose` creates the immutable candidate ref;
they are required before that candidate may enter durable eligibility or be published.
The updater must first use deferred RAW builds plus `candidate-compose` for all six targets,
including a native Windows producer. Patch/transform counterfactual classification and the BSD
runtime-asset boundary must also fail closed until their own evidence paths are authoritative.

Build-only validation precedes targeted manual publication. Each target needs two consecutive,
fully green targeted publications before a repository owner may set its monthly target unlock to
true. Those two manual publications are performed while that monthly target unlock is still false.
After qualification, a scheduled publication requires both the global schedule unlock and the
target unlock. The existing macOS libgc unlock remains an additional condition for that target.
