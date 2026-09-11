# osaho

**筬 (osa) is the reed of a loom.** Every warp thread passes through one dent of
it, the reed holds them all in position, and the weft is beaten home against it.
`osaho` makes nothing and decides nothing; it is what everything passing through
must fit. That is this repository: **Kotoba KIR — the checked intermediate
representation every backend shares, and the canonical definition identity
(DefCID) they are all keyed on.**

The name is coined, not a traditional weaving term (`osaba` 筬羽, `osauchi`
筬打ち and `osatōshi` 筬通し are; `osaho` is not), so it is stated here rather
than left to be guessed — the workspace rule for a repository whose name does
not announce its function. It sits in the loom the rest of the family is
already named from:

| | | |
|---|---|---|
| `kotoba` | 言葉 | the language |
| `amu` | 編む | the compiler — weaves one closed cloth |
| `abi` | 経 | the warp: WIT / admission contract |
| **`osaho`** | **筬** | **the checked IR every thread passes through, and its identity** |
| 綾 | aya | the backends: `kotoba-wasm`, `kotoba-native`, `kotoba-script`, `kotoba-component` |
| `kototama` | 言霊 | the VM contract — reduction, state, authority, receipt |

**Renamed from `kotoba-kir` on 2026-09-07** (owner decision). GitHub redirects
the old name, so `io.github.kotoba-lang/kotoba-kir` git coordinates keep
resolving; the `kotoba.kir.*` namespaces are **unchanged** and remain the API,
the same way `amu` kept `kotoba.compiler.*` after its own rename. Names are not
in a DefCID on either side of a call, so **no definition identity moved.**

**Tier**: `T0`  **Role**: `contract`

Split out of the overloaded core repos by ADR-2607266000 so that each
responsibility has exactly one owner and the dependency direction is
checkable from outside.

## Owns

- `kotoba.kir (KIR v3/v4 shape + lowering budget)`
- `kotoba.kir.value (portable value model)`
- `kotoba.kir.target (target profile registry)`
- `kotoba.kir.definition-identity (canonical DAG-CBOR DefCID)`
- native typed-feature admission, including the closed scalar-variant export
  boundary (`qualified name`, `1..32` unique cases, `:i64`/`:bool` payloads)

## Does not own

- parse .kotoba source
- define or validate the checked HIR envelope
- emit machine code or wasm
- decide policy

## Depends on

- `kotoba-lang/kotoba-hir`
- `kotoba-lang/security`

## Test

```bash
kbb -M:test
```
