# Nous Portal provider support — file design

Date: 2026-09-17

Status: design only, nothing implemented yet. Several facts below are
unconfirmed and marked as such — verifying them against a real account is
the first implementation step, not something to guess at here.

## Problem

Tonight's mimo-v2.5 investigation found real, repeated failures — both fast
429 rejections and genuine multi-minute timeouts — on OpenRouter's routing
to that model. The mitigations shipped tonight (throughput-sort routing, a
longer timeout, richer failure reporting) reduce the pain but don't remove
the underlying single point of failure: every mimo call goes through one
provider, one routing policy, one account's rate limit.

The operator has a funded Nous Portal account with access to the same
model family. Adding it as a genuine second provider, not just a second
OpenRouter profile, is the actual ask — and the substrate currently has no
concept of "more than one remote provider" at all.

## What's actually confirmed vs. not

Confirmed (from Nous Research's own documentation, found via search):

- Base URL: `https://inference-api.nousresearch.com/v1`, OpenAI-compatible
  (`GET /v1/models`, `POST /v1/chat/completions`).
- Auth: Bearer token (a Nous Portal API key). Also supports the x402 HTTP
  payment protocol for pay-per-request access, not needed for a
  provisioned account and not in scope here.
- The API "follows the standard OpenAI API structure."

**Not confirmed, and needed before implementation starts:**

- The exact mimo model slug/id as it appears on Nous Portal (OpenRouter
  calls it `xiaomi/mimo-v2.5`; Nous Portal's naming may differ).
- The exact error response shape on a rejection (standard OpenAI errors
  are `{"error": {"message", "type", "param", "code"}}`, structurally
  different from OpenRouter's `{"error": {"code": number, "message",
  "metadata": {"error_type", "provider_code"}}}` that tonight's mid-stream
  fix was built against).
- Whether streaming (SSE) is supported, and whether a mid-stream failure
  behaves like OpenRouter's (HTTP 200 already sent, error embedded in an
  SSE event) or fails before headers go out.
- Whether responses report a cost/usage field the way OpenRouter's
  `usage.cost` does, or only token counts (very likely only token counts,
  since that's an OpenRouter-specific extension, not part of the OpenAI
  spec Nous Portal says it follows).
- Whether native tool/function calling and a reasoning-effort-style
  parameter are supported, both hard requirements of every profile this
  substrate currently runs.
- Published rate limits, if any.

A web attempt to fetch Nous Portal's live model catalog produced an
unreliable result (a list containing model names lifted from this
session's own context, not the real page) and was discarded rather than
used here. The next real step is a manual `curl` against the live API with
a real key, not another automated fetch.

## Why this is a second-provider integration, not a config profile

`%conversation-openrouter-endpoint-p` in `conversation-runtime.lisp` is a
hard triple gate: provider must literally be `"openrouter"`, and the
endpoint must equal the exact string
`https://openrouter.ai/api/v1/chat/completions`. This function *is* the
authorization boundary — `%conversation-authorized-endpoint-p` is
"loopback or this" — so a request to any other remote host is refused
before a connection is even attempted. That is deliberate, not an
oversight: it is the thing standing between "validate egress before
creating even an inbound event" and a mistaken remote call. It cannot be
loosened casually.

Three further systems are built specifically around OpenRouter's own
extensions to the OpenAI shape, not the underlying spec:

- **Cost accounting.** `%conversation-openrouter-charge` reads
  `response.usage.cost` directly — a real dollar figure OpenRouter reports
  per call. A standards-only provider has no equivalent field; cost has to
  be computed locally from token counts and a price table (the profiles
  already carry `max_price_usd_per_million` and a `pricing_snapshot` for
  exactly this kind of estimate, so the data shape to compute from already
  exists — the charge *function* does not).
- **Privacy routing.** `zdr`, `data_collection`, and `provider_routing`
  (`sort`, `require_parameters`, `only`, `allow_fallbacks`) are OpenRouter
  concepts describing how OpenRouter itself selects and constrains an
  upstream sub-provider. They have no defined meaning for a provider that
  *is* the endpoint, not a router in front of one.
- **Error classification.** Tonight's two reporting fixes
  (`%conversation-provider-http-message`, and the mid-stream
  `error.code`/`error.metadata` extraction) are both shaped around
  OpenRouter's specific error body. A different provider needs its own
  classification, not a silent fallback to a generic message.

## Selected design

Generalize the provider concept rather than special-case a second one.

- Add a `provider` dispatch, not another endpoint-string comparison.
  `%conversation-openrouter-endpoint-p` becomes one case of a small
  `%conversation-remote-provider-kind` that reads `(gethash "provider"
  profile)` and returns `:openrouter`, `:nous-portal`, or `nil` (refused) —
  matched against both the declared provider value *and* the profile's own
  endpoint, same defense-in-depth the current function already has, just
  no longer assuming there is only one legitimate answer.
- `%conversation-authorized-endpoint-p` becomes "loopback, or a profile
  whose declared provider/endpoint pair matches a known remote provider" —
  still a positive allowlist, never a denylist, just no longer hardcoded
  to one entry.
- Charge extraction branches on provider kind: OpenRouter keeps reading
  `usage.cost` exactly as today; a Nous Portal (or any future
  standards-only) provider computes an estimate from `usage.prompt_tokens`
  / `usage.completion_tokens` against the profile's own
  `max_price_usd_per_million`, using the same
  "admitted-bound-is-the-worst-case-fallback" pattern the OpenRouter path
  already uses when its own reported cost is missing or untrustworthy —
  reused, not reinvented.
- Error classification branches on provider kind too:
  `%conversation-provider-failure-details` and the streaming
  `consume-json-event` handler each need a second, standards-shaped
  extraction path (`error.message`/`error.type`/`error.code`, no
  `metadata`) alongside the OpenRouter-shaped one already there.
- ZDR/data_collection stay OpenRouter-only fields; a Nous Portal profile
  simply omits them. Whatever privacy posture Nous Portal actually offers
  (once confirmed) becomes its own, separately-named fields if it needs
  representing at all — not force-fit onto OpenRouter's vocabulary.
- A new profile entry in `config/conscious-provider-profiles.json`
  (`"provider": "nous-portal"`, `"endpoint":
  "https://inference-api.nousresearch.com/v1/chat/completions"`) becomes
  selectable via the existing `--provider-profile` flag and
  `experiment_routing`, no new CLI surface needed.

## Rejected alternatives

- **Treating Nous Portal as "just another OpenRouter-shaped endpoint"**
  (same code path, different URL) was rejected: it would silently read a
  nonexistent `usage.cost` field, misclassify every real error through the
  wrong parser, and worse, `%conversation-openrouter-endpoint-p`'s literal
  string match means it would simply be refused outright today, not
  quietly misbehave — the safer failure mode, but not a fix.
- **A generic "OpenAI-compatible" abstraction that OpenRouter itself also
  routes through** was rejected for this slice: OpenRouter's real
  extensions (cost reporting, ZDR routing, its specific error shape) are
  load-bearing for the existing accounting and reliability work, most of
  it built or hardened this very night. Collapsing the two into one
  generic path now would risk exactly the kind of silent regression this
  whole session has been chasing out of the substrate. Provider-kind
  dispatch keeps both fully intact and adds a second path beside them.
- **Automatic failover between providers on a mimo failure** was
  considered and deferred, not rejected outright: worth real design once
  both providers individually work, since it raises its own questions
  (does a failed-over request re-admit under the second provider's own
  budget, does a partial mid-stream response from provider A ever get
  discarded in favor of retrying provider B — OpenRouter's own docs
  explicitly warn against exactly that once output has started reaching
  the caller). Out of scope for a first slice that just needs Nous Portal
  to work as a manually-selected profile.

## Handoff

First real step is not code: confirm the unverified facts above against
the operator's actual Nous Portal account (a manual `curl` to
`/v1/chat/completions` with the real mimo model slug, checking the error
shape on a deliberately bad request, and confirming tool-calling/reasoning
parameter support) before writing the provider-kind dispatch. Implementing
against guessed shapes would repeat tonight's exact lesson from the
timeout config: assumed behavior that was never actually verified against
what's really running.
