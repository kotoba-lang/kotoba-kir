# ADR 0225: Own DefCID with a golden-preserving extraction

- Status: accepted
- Date: 2026-08-10

## Decision

`kotoba-kir` owns definition content identity in
`kotoba.kir.definition-identity`. A DefCID is CIDv1 with the DAG-CBOR codec over
the closed, normalized definition payload. It is distinct from SourceCID,
BuildCID, and ArtifactCID.

The implementation is extracted from `kotoba-lang` without changing payload
version 2, normalized tags, deterministic ordering, canonical bytes, or CID.
The pre-extraction canonical hex and CID are frozen in this repository's test
suite. A later representation change requires an explicit payload-version bump;
it must never reinterpret an existing DefCID.

## Boundary

KIR owns the semantic identity algorithm and lock admission checks. The
language repository may retain a compatibility namespace, but it delegates to
this implementation and is no longer an independent identity authority.
Canonical EDN remains the human/reference projection. JSON is an interop
projection. Neither printed representation participates in DefCID.
