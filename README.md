# kotoba-kir

Kotoba KIR — the checked intermediate representation shared by every backend.

**Tier**: `T0`  **Role**: `contract`

Split out of the overloaded core repos by ADR-2607266000 so that each
responsibility has exactly one owner and the dependency direction is
checkable from outside.

## Owns

- `kotoba.kir (KIR v3/v4 shape + lowering budget)`
- `kotoba.kir.value (portable value model)`
- `kotoba.kir.target (target profile registry)`

## Does not own

- parse .kotoba source
- emit machine code or wasm
- decide policy

## Depends on

- `kotoba-lang/security`

## Test

```bash
clojure -M:test
```
