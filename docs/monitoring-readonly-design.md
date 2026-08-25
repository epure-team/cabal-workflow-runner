# Read-only CWR monitoring proposal

This is a design artifact only. It does not add an HTTP server, credentials, or an
engine control path.

## Contract sketch

The proposed OpenAPI surface ([`monitoring-readonly.openapi.yaml`](monitoring-readonly.openapi.yaml)) exposes only `GET /v1/ledgers` and
`GET /v1/ledgers/{id}/entries?offset=&limit=` over an operator-selected artifact
root. Responses label each entry `raw_untrusted`; they do not claim replay or
attestation verification. There are no routes for run, replay, dispatch, approval,
write, deletion, or backend configuration.

## Required controls

* Confine selected paths beneath one configured root after realpath/symlink checks.
* Authorize authenticated operators per ledger and deny cross-tenant identifiers.
* Bound pagination, response sizes, parsing time, and incomplete ledger reads.
* Redact configured sensitive fields before returning content.
* Escape all strings in the mini web consumer; never insert ledger text as HTML.
* Use an explicit CORS allowlist; do not expose backend credentials to the service.

The future mini site is a GET-only consumer of this API. It has no filesystem-write
authority and never invokes the CWR engine.
