# Driving the handshake core over QUIC

This document explains **how the TLS 1.3 handshake in this repository is
structured**, **how support for QUIC (RFC 9001) was adapted into it**, and
**how to write a QUIC adapter** that drives the core — without importing the
`tls` record layer, without touching a socket, and without reimplementing the
TLS state machine.

The module it describes is `tls.handshake` (`src/tls/handshake/`). Everything
`pub` in that module is the adapter contract; everything else is private. This
file is the long-form guide to the QUIC surface of the unified core — the shared
accessors in `key_schedule.v` and the QUIC-only guards in `core_guarded.v`, on top
of the shared handshake engine in `state_machine.v`.

---

## 1. What "transport-neutral" means here

TLS 1.3 is defined as a sequence of **handshake messages** bound to a running
transcript, plus a **key schedule** that turns a shared secret into traffic
secrets. Everything else — records, AEAD framing, sequence numbers, sockets —
is how those messages and secrets are *carried*. RFC 9001 (TLS over QUIC)
changes the carrier completely: handshake messages ride QUIC **CRYPTO frames**
on three packet levels, and packet protection is a QUIC concern, not a TLS
record concern.

The core was therefore split so that the carrier is always someone else's job:

```
   ┌──────────────────────────── tls.handshake (this module) ───────────────────────────┐
   │  Shared core: state_machine.v + state_machine_extra.v + key_exchange.v         │
   │  Key schedule: key_schedule.v (holders + §7.5 lifecycle)                       │
   │  Codecs: messages.v / client_hello.v / server_hello.v / server_flight.v        │
   │          post_handshake.v / transcript.v                                       │
   │  Key seam: key_schedule.v accessors answer both modes (leg shape by mode)      │
   │  One-sided: core_guarded.v ratchet/reassembly_len + params/HRR (is_quic-guarded)      │
   │  aead.v / certificate_verify.v / alert.v / passthrough.v                       │
   └────────────────────────────────────────────────────────────────────────────────────┘
        ▲                                     ▲                                     ▲
        │ receive_initial_bytes                │ handshake keys / secrets             │ create_client_hello /
        │ receive_handshake_bytes              │ application key/secrets              │ phase-named receives
        │                                      │                                      │
   ┌────┴──────────────┐              ┌───────┴───────────┐               ┌─────────┴──────────┐
   │ tls TCP adapter   │              │ a QUIC engine     │               │ tests / other      │
   │ conn.v            │              │ (RFC 9001)        │               │ transports         │
   │ records + sockets │              │ CRYPTO frames +   │               │                    │
   │                   │              │ packet protection │               │                    │
   └───────────────────┘              └───────────────────┘               └────────────────────┘
```

Two properties make this work:

1. **The core owns no record, cipher, or socket concept.** A grep gate over
   `src/tls/handshake/` (`Record`, `TLSProtection`, `ContentType`,
   `change_cipher_spec`, `record_header_len`) keeps it that way. The core never
   encrypts, never frames a record, and never calls `read(2)`/`write(2)`.

2. **The public boundary is a small, phase-named seam.** The unit the adapter
   exchanges with the core is a `HandshakeMessage` — a complete handshake
   message (4-byte header included). Its TLS protection phase is carried by the
   entry point that produced it, not stored as a tag on the message. Framing
   (records vs. CRYPTO frames) and protection (AEAD records vs. QUIC packet
   protection) are entirely adapter-side.

The phase vocabulary is transport-neutral and lives in the method names, not in a
level value: `receive_initial_bytes()` / `receive_handshake_bytes()`,
`traffic_keys()` (which dispatches on `HandshakeState`), and
`zeroize_traffic_secrets()`. A `HandshakeMessage` does not store its phase; the
QUIC engine knows it from the packet type whose CRYPTO frame completed the message
(Initial → initial, Handshake → handshake).

Here the initial phase names the pre-ServerHello TLS handshake stage, not QUIC
Initial packet protection. The QUIC engine routes Initial packet CRYPTO to
`receive_initial_bytes()` and Handshake packet CRYPTO to
`receive_handshake_bytes()`; it discards 1-RTT CRYPTO because a handshake-only
engine does not consume post-handshake messages such as NewSessionTicket. The TCP adapter routes plaintext
handshake records to `receive_initial_bytes()` and decrypted handshake records to
`receive_handshake_bytes()`. QUIC packet levels and packet-number spaces remain
adapter-side.

QUIC handshakes include the TLS `quic_transport_parameters` extension
(RFC 9001 §8.2). The core treats its content as opaque bytes: it rules only on
placement — required when the connection is QUIC, rejected on a record transport
— and carries the value out and back in untouched. Encoding this endpoint's own
parameters and decoding a peer's id/length/value triplets lives above the
handshake, in `tls.quic` (`transport_params.v`): a client advertises a complete
RFC 9000 §18.2 parameter set — `max_idle_timeout` (0x01), `max_udp_payload_size`
(0x03), `initial_max_data` (0x04), the three stream-data limits (0x05/0x06/0x07),
`initial_max_streams_bidi`/`_uni` (0x08/0x09), `ack_delay_exponent` (0x0a),
`max_ack_delay` (0x0b), and `initial_source_connection_id` (0x0f) — in ascending
id order, so a strict server does not reject an undersized set as
`illegal_parameter`. The flow-control numbers are handshake-only placeholders:
this stack implements no stream data plane, so they are legal defaults that are
never acted on. On the receive side the engine requires a server to declare both
`initial_source_connection_id` (0x0f) and `original_destination_connection_id`
(0x00), checking each against the connection IDs it actually saw on the wire (RFC
9000 §7.3). The server-only ids a client must never send —
`original_destination_connection_id` (0x00), `stateless_reset_token` (0x02), and
`retry_source_connection_id` (0x10) — are read but never emitted (§7.3 makes a
client sending any of them a TRANSPORT_PARAMETER_ERROR). Parameters outside the
known set reach the adapter intact and are ignored there.

---

## 2. The handshake the core performs

The core is a **client** state machine. Its public `HandshakeState` is a single
enum mirroring the client state machine of RFC 8446 Appendix A.1; it is itself the
ordering cursor that enforces the fixed post-ServerHello order of §4.1:

```
start
  └─ create_client_hello() ─▶ wait_sh
        │  ServerHello (or HelloRetryRequest → re-send ClientHello, back to wait_sh)
        ▼
  wait_ee                      (handshake keys installed)
        │  EncryptedExtensions   (ALPN/SNI checks happen here)
        ▼
  wait_cert_cr                 (CertificateRequest or Certificate)
        │  CertificateRequest?   (optional, §4.3.2; client answers with an empty Certificate, §4.4.2)
        ▼
  wait_cert                    (Certificate)
        │  Certificate           (chain parsed + validated against the injected pki.Validator)
        ▼
  wait_cv                      (CertificateVerify)
        │  CertificateVerify     (server signature over the §4.4.3 content)
        ▼
  wait_finished                (Finished)
        │  Finished              (transcript-bound verify_data)
        ▼
  connected                    (client Finished emitted; application secrets live)
```

`state()` reports the exact cursor position and rejects an out-of-order
encrypted-flight message, advancing only as each message is handled; adapters may
poll any of these states — `conn.v` keys its record routing on `wait_sh`,
`connected`, and `failed` — while key-material readiness follows the same cursor:
handshake keys once the core leaves `wait_sh`, application keys at `connected`.
`failed` is terminal: once reached,
the phase-named receives refuse further input. A protocol violation moves the
machine to `failed` and wipes all derived material (see
`fail_and_zeroize_secrets()` in [CONTRACT.md](./CONTRACT.md)). The public
accessors `state()`, `negotiated_alpn()`, and `negotiated_cipher_suite()` let an
adapter poll progress and outcome.

### The ClientHello and HelloRetryRequest

`create_client_hello()` builds the first ClientHello with a key share for the
configured initial group (X25519 by default), the configured cipher suites (or
the built-in TLS 1.3 defaults), `supported_groups`, `signature_algorithms`, and
`supported_versions`. `SNI` is included for a DNS server name, and `ALPN` when
protocols are configured.
A HelloRetryRequest is fed as plaintext handshake input; the core switches to
the requested group, regenerates the share, echoes any cookie, and the receive
**returns the second ClientHello** (an initial-phase message, so it goes back on
the Initial/plaintext path) for the adapter to send.
The transcript substitutes the first ClientHello with its
`message_hash` (§4.4.1) so the Finished binds the whole two-round exchange.
The retry suite is remembered (`hrr_cipher_suite`) and the final ServerHello
must reuse it (§4.1.4).

No dummy ChangeCipherSpec is ever produced: that is a TCP middlebox
compatibility hack (RFC 8446 App. D.4), and QUIC packets are already opaque
(RFC 9001 §4.1.3).

### Client authentication

If the server requests the client's certificate, the core accepts the
`CertificateRequest` and emits an **empty `Certificate`** (§4.4.2) — this
library has no client credential to offer, so it declines rather than aborting.
That empty Certificate is folded into the transcript before the server flight
is verified.

---

## 3. How QUIC support was adapted in

The integration is **not a separate handshake implementation**. A QUIC-mode
`HandshakeCore` (built by `new_quic_handshake`, `is_quic true`) drives the same
TLS handshake state machine as the record mode (`is_quic false`) used by
`tls.Conn`. One type, one flag: the transport-specific methods stay explicit
because each guards on `is_quic` — a wrong-transport call returns an error
without touching state or keys — while TLS message processing, transcript and
key-schedule state are shared outright.

### 3.1 Initial secrets are not the core's business

QUIC derives its **Initial** packet keys from the destination connection ID
(RFC 9001 §5.2), not from the TLS key schedule. Those packet keys are outside
the core's phase vocabulary; the engine derives them itself, below this
layer. The shared handshake API starts with plaintext ServerHello processing.

### 3.2 Packet keys, not raw secrets, cross the boundary

RFC 9001 §5.1 expands each traffic secret into per-direction packet keys:

| Label      | Purpose                       |
|------------|-------------------------------|
| `quic key` | AEAD key for packet protection |
| `quic iv`  | packet-number nonce base       |
| `quic hp`  | header-protection key          |

The core runs those expansions through `traffic_keys()` — the shared,
transport-neutral accessor on `HandshakeCore` declared in key_schedule.v — which
returns a fresh caller-owned `TrafficKeys` generation whose legs on a QUIC-mode core
are fully filled `KeyMaterial` — key, iv *and* hp; the same call on a TLS-mode core
returns key-and-IV record legs with the hp slot left empty. The phase is implied by
`HandshakeState`, never an argument, and it is the core's mode — not the accessor
name — that picks the leg shape.
Raw §7.1 traffic secrets never leave the core in either mode, so an adapter has no use
for `hkdf_expand_label()` of its own.

### 3.3 Handshake keys survive until no longer needed

A QUIC server may keep sending Handshake packets until the client
confirms the handshake with `HANDSHAKE_DONE` (RFC 9001 §4.1.2), not merely
until the client sends its Finished. So the core deliberately retains the
handshake traffic secrets past `.connected`; the engine owns its derived packet
keys and, when it processes `HANDSHAKE_DONE` (§4.9.2), calls `release_handshake_keys()`
— a single step that zeroizes the core's `.handshake` traffic secrets *and* zeroes
the Handshake packet-protection states together, so neither half can be forgotten.
The TCP adapter zeroizes all traffic secrets together at close time through
`zeroize_traffic_secrets()`; **a QUIC engine must not** release handshake keys
before it has finished using the Handshake packet keys. Terminal cleanup is
centralized in `close()`, which seals the final `CONNECTION_CLOSE` first and only
then zeroizes the core secrets and every remaining packet key (`destroy_on_close()`).

### 3.4 KeyUpdate is replaced by Key Phase

Over QUIC, TLS KeyUpdate does not exist: an endpoint MUST NOT send one and MUST
treat a received one as a connection error (RFC 9001 §6). The next generation
of 1-RTT secrets is derived engine-side with the `quic ku` label
(§6.1) — **not** via `next_application_key_material(is_server_leg)`,
whose `traffic upd` label is the TLS record path's. The header-protection key
never updates.

### 3.5 Alerts never ride CRYPTO

An alert over QUIC is not a handshake message; it becomes a **connection
error** whose code is `0x100 + AlertDescription` (CRYPTO_ERROR, RFC 9001 §4.8).
`quic_connection_error()` does that mapping. The core's shared
`AlertDescription` vocabulary (including the classified `failure_alert()` a
failed handshake exposes) is therefore reusable by a QUIC adapter with no
CRYPTO alert path at all.

### 3.6 CRYPTO reassembly

Within a phase the CRYPTO stream is self-framed handshake messages
(type ∥ u24 length). `next_recordless_message()` — the recordless twin of the
record layer's parser — extracts one complete message from the front of a byte
buffer, capping the declared length at `max_handshake_message_len` (1 MiB) from
the header alone so a hostile peer cannot make the adapter buffer indefinitely.
It is public precisely so an engine can reuse it engine-side. The shared core
owns one reassembly buffer for each handshake instance.

---

## 4. Writing a QUIC adapter: the engine loop

An engine imports **`tls.handshake`** and never imports `tls`. Certificate
validation defaults to the system root bundle; provide a `pki.Validator` in
`HandshakeOptions` when using private roots or custom validation policy. The
whole integration is this loop:

```v
import tls.handshake

// The omitted validator uses the system root CA bundle.
mut hs := handshake.new_quic_handshake('example.com', handshake.HandshakeOptions{
	alpn: ['h3']
})!

// 1. First ClientHello. `ch1` is a complete handshake message; place it into a
//    CRYPTO frame in an Initial packet (which YOU protect with Initial keys
//    derived from the destination connection ID, RFC 9001 §5.2).
ch1 := hs.create_client_hello()!

// 2. Send ch1 in Initial CRYPTO; read Initial packets; strip Initial protection;
//    concatenate CRYPTO frame payloads into `initial_stream`.

// 3. Feed the ServerHello (ServerHello rides Initial; HRR also rides Initial).
msgs := hs.receive_initial_bytes(initial_stream)!
for m in msgs {
    // The only initial-phase output here is the retry ClientHello after a
    // HelloRetryRequest — send it in an Initial CRYPTO frame and loop.
    _ = m
}

// 4. Now derive Handshake packet keys from the core and protect Handshake
//    packets with them; the server flight (EE, [CertReq], Cert, CV, Finished)
//    rides Handshake CRYPTO.
mut gen := hs.traffic_keys()!   // TrafficKeys: both legs, key+iv+hp filled (phase from HandshakeState)
mut client_leg := gen.take_client_material()
qkey := client_leg.take_key()
// ... take_iv() (12 bytes) and take_hp() likewise; wipe() handles when done (§7.5) ...

// 5. Feed the reassembled Handshake CRYPTO stream. The return carries the
//    client Finished (a handshake-phase message) once the server flight verifies.
flight := hs.receive_handshake_bytes(handshake_stream)!
for m in flight {
    // Every message here is handshake-key protected: send m.msg in a Handshake
    // CRYPTO frame. (The TCP adapter sends a dummy CCS here; QUIC does not.)
    _ = m
}

// 6. Application keys are live at .connected. Derive 1-RTT packet keys with
//    traffic_keys() — the same filled KeyMaterial legs (phase from HandshakeState).
mut app_gen := hs.traffic_keys()!

// 7. For a custom client adapter, release all traffic secrets after
//    receiving HANDSHAKE_DONE. Conn does this automatically at close().
hs.zeroize_traffic_secrets()!

// 8. Post-handshake: NewSessionTicket rides the 1-RTT CRYPTO stream. Reassemble
//    with next_recordless_message and decode with parse_new_session_ticket.
//    A KeyUpdate here is a connection error (RFC 9001 §6) — do NOT feed keys.
```

### Notes on each step

- The shared API sequence, phase guards, and TLS-only operations are specified
  in the [handshake adapter contract](./CONTRACT.md). For QUIC, queue every
  returned message by the entry point that produced it: output of
  `receive_initial_bytes()` and `create_client_hello()` is Initial CRYPTO, output
  of `receive_handshake_bytes()` is Handshake CRYPTO. `HandshakeMessage` carries
  no level tag.
- The QUIC engine discards CRYPTO data carried in 1-RTT packets: it is
  post-handshake traffic, and an engine that does not implement resumption must
  not let a NewSessionTicket flight block the HANDSHAKE_DONE that confirms the
  connection. Handle post-handshake CRYPTO separately with
  `next_recordless_message()` and `parse_new_session_ticket()` if needed.
- `negotiated_alpn()` returns the server's selection (`''` if none); the shared
  core verifies that any selection was offered (RFC 7301 §3.1).

---

## 5. API reference

The canonical list of shared and transport-specific methods, lifecycle rules,
and public helper APIs is in the [handshake adapter contract](./CONTRACT.md).
This guide focuses on QUIC behavior and adapter sequencing rather than
repeating that API inventory.

The validator in `HandshakeOptions` is optional. If omitted, construction uses
`pki.default_validator()` and propagates an error if the system root bundle
cannot be loaded. Supply a custom `pki.Validator` to use private trust roots or
custom validation policy. The QUIC client constructor requires `server_name`;
its optional TLS settings are supplied through `quic.Config.hsk_options`.
For example:

```v
mut conn := quic.new_client_engine('example.com', quic.Config{
	hsk_options: handshake.HandshakeOptions{ alpn: ['h3'] }
})!
```

`new_client_engine` returns a transport-agnostic `quic.Engine` — it builds and
protects packets in memory but owns no socket, so the caller pumps datagrams
through `start()` / `receive()` / `retransmit_expired()` by hand. For a
ready-to-use client, `quic.dial(address, quic.Config)` wraps that engine in a
socket-owning `quic.Conn` facade: it opens the UDP socket, derives `server_name`
from the address host when unset, runs the handshake loop to confirmation under
`Config.timeout`, retransmits the flight on a probe timeout, and returns a
confirmed connection exposing `is_confirmed()`, `negotiated_alpn()`,
`negotiated_cipher_suite()`, `serve(idle)` and `close()`:

```v
mut conn := quic.dial('example.com:443', quic.Config{
	hsk_options: handshake.HandshakeOptions{ alpn: ['h3'] }
})!
defer {
	conn.close() or {}
}
println(conn.negotiated_alpn())
```

---

## 6. What this module does *not* give you

The handshake core is complete, and `tls.quic` now carries a handshake robust
enough to finish against a real server. The following are still outside
`tls.handshake`, and `tls.quic` does not provide them either:

- **Key Phase** — the 1-RTT key update mechanism that replaces TLS KeyUpdate
  (RFC 9001 §6); the engine does not implement key updates.
- **Full connection machinery** — streams, flow control, loss recovery for
  application data, congestion control, connection migration, and Retry/0-RTT.
  The engine retransmits only the handshake flight, not a general data plane.

What `tls.quic` *does* give you at the packet layer, on top of the core:

- **General handshake packet handling** — it builds and parses Initial and
  Handshake packets with real RFC 9001 protection, and fragments outbound CRYPTO
  so no sealed packet exceeds `Config.max_packet_size` (RFC 9000 §14, §18.2).
- **ACK emission** — the engine acknowledges the peer's handshake packets, both
  piggybacked on CRYPTO it sends and as bare ACK datagrams when it has nothing
  new to emit (RFC 9000 §13.2, §19.3).
- **Handshake retransmission** — sent-but-unacknowledged handshake CRYPTO is
  retained and re-sent under fresh packet numbers on a probe-timeout clock the
  caller drives, so lossy paths still complete (RFC 9000 §6.2.1, §A.4).
- A socket-owning `quic.dial()` facade (`quic.Conn`) that runs the whole loop to
  confirmation.
- **A complete client transport-parameter advertisement** — the ClientHello carries
  the full RFC 9000 §18.2 numeric set (§1 above), though the flow-control values are
  handshake-only placeholders: there is still no stream/flow-control data plane behind
  them.

What the module *does* give you: the entire TLS 1.3 state machine, transcript,
key schedule, certificate validation hook, alert vocabulary, and the
phase-named seam to plug them into your packet layer — so your adapter only
has to be a transport.

---

## 7. Debugging the bundled example

Run the example with `QUIC_DEBUG=1` to trace the handshake on stderr:

```bash
QUIC_DEBUG=1 v run src/example/quic_dial_example.v
```

The trace prints the plaintext ClientHello (length and hex), the size of the
sealed first flight, and its leading bytes. The negotiated ALPN and cipher suite
still go to stdout, so the trace can be redirected independently. This matters
when a peer rejects a ClientHello — the wire bytes are sealed by Initial
protection, and the alert alone rarely shows which parameter was at fault.

---

## 8. See also

- `src/tls/handshake/CONTRACT.md` — the unified-core contract, including the
  remaining opportunity to report handshake milestones
  through structured progress results.
- `src/tls/handshake/state_machine.v` — the unified TLS handshake core.
- `src/tls/handshake/core_guarded.v` — the core's one-sided methods: the TLS-record
  ratchet/reassembly_len and the QUIC params/HRR surface (is_quic-guarded).
- `src/tls/handshake/passthrough.v` — CRYPTO-stream reassembly (RFC 9001 §4).
- `src/tls/handshake/alert.v` — `AlertDescription` + `quic_connection_error()`.
- `src/tls/handshake/key_schedule.v` — `hkdf_expand_label`, the §7.1 ladder.
- `src/tls/conn.v` — the reference TCP adapter: the
  same seam driven over records and sockets, useful as a worked example of every
  adapter responsibility.
- RFC 9001 §4 (carrying TLS), §4.1 (interface), §4.8 (alerts), §5 (packet
  protection), §6 (Key Phase).
