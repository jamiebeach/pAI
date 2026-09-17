# Nous Portal provider support — file design

Date: 2026-09-17

Status: design only, nothing implemented yet. Updated same day with a real
`curl` result against the operator's own account — the scope narrowed
significantly once actual data replaced the guesses below. Remaining
unconfirmed facts are marked as such.

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

An early web attempt to fetch Nous Portal's live model catalog produced an
unreliable result (a list containing model names lifted from this
session's own context, not the real page) and was discarded. What follows
instead is a real, operator-run `curl` against `/v1/chat/completions` with
model `xiaomi/mimo-v2.5`, a system+user message, non-streaming.

**Confirmed, from that real response:**

- The model slug is exactly `xiaomi/mimo-v2.5` — identical to OpenRouter's.
- The response is not merely OpenAI-shaped, it is **OpenRouter-shaped**:
  `"id": "gen-<...>"`, `"object": "chat.completion"`,
  `"native_finish_reason"`, `"reasoning"` / `"reasoning_details"` on the
  message, and — the load-bearing part — a `usage` object carrying
  `"cost"`, `"is_byok"`, and `"cost_details": {"upstream_inference_cost",
  "upstream_inference_prompt_cost", "upstream_inference_completions_cost"}`.
  None of that is part of the OpenAI spec; all of it is OpenRouter's own
  set of response extensions. Nous Portal's mimo route reproduces it
  exactly, field-for-field.
- `"provider": "Xiaomi"` in the response — the model's own creator,
  directly. Compare this session's own earlier OpenRouter probe of the
  same model, which landed on `"provider": "Novita"`, a third-party host.
  Nous Portal's route to mimo is not the same upstream path OpenRouter's
  own `sort: throughput`/`sort: price` routing picks — plausibly a more
  direct, and possibly more reliable, one. This is the actual point of
  adding it, not just redundancy for its own sake.
- Reasoning is on by default and returned unprompted (no
  `reasoning.effort` parameter was sent in the test call) — worth
  confirming whether Nous Portal accepts the same `reasoning: {enabled,
  effort}` request parameter OpenRouter does, or defaults it differently.
- `usage.is_byok: true` on this response is notable: it suggests Nous
  Portal's own account may itself be routing this particular model through
  an OpenRouter-compatible layer using its own upstream key, which would
  explain the exact shape match. That does not make it not-a-real-second-
  path — the actual upstream (`Xiaomi` vs `Novita`) differs — but it means
  "Nous Portal" may not be a uniformly independent stack across every
  model it lists, only that this one reproduces the interop shape closely
  enough to reuse existing parsing as-is.

**Still not confirmed, and worth checking before implementation, though no
longer blocking the core design:**

- The exact error response shape on a rejection (a deliberately bad
  request — bad model name, over quota, malformed body — was not tried).
  Given the success shape matches OpenRouter's exactly, the error shape
  reusing `{"error": {"code", "message", "metadata": {"error_type",
  "provider_code"}}}` is a reasonable working assumption now, but still an
  assumption.
- Whether streaming (SSE) is supported and shaped the same way (the test
  request did not set `"stream": true`).
- Whether native tool/function calling is supported and behaves like
  OpenRouter's (`tool_calls` on the message, `require_parameters`-style
  guarantees) — untested, and a hard requirement of every profile this
  substrate runs.
- Published rate limits, if any.

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

Two systems were assumed OpenRouter-only before the real `curl` result;
one of them turns out not to need new code at all:

- **Cost accounting — reuse, not reimplement.** `%conversation-openrouter-
  charge` reads `response.usage.cost` directly. The real Nous Portal
  response carries that exact field (plus `cost_details`), so this
  function can be called unchanged against a Nous Portal response, once
  it's reachable at all. The earlier assumption that cost would need
  local estimation from token counts was wrong for this route; keep the
  price-table fields (`max_price_usd_per_million`) as the admission
  reservation bound only, the same role they already play for OpenRouter.
- **Error classification — probably reuse, not yet confirmed.** The two
  reporting fixes built tonight (`%conversation-provider-http-message`,
  and the mid-stream `error.code`/`error.metadata` extraction) are shaped
  around OpenRouter's error body. Given the success shape matches exactly,
  reuse is the working assumption, but only a real failed request would
  confirm it — a standards-only fallback path is still worth keeping as a
  defensive default in case a specific error condition ever diverges.

One system remains genuinely OpenRouter-specific with no equivalent:

- **Privacy routing.** `zdr`, `data_collection`, and `provider_routing`
  (`sort`, `require_parameters`, `only`, `allow_fallbacks`) describe how
  OpenRouter itself selects and constrains an upstream sub-provider. They
  have no defined meaning for a provider that *is* the endpoint, not a
  router in front of one -- Nous Portal profiles simply omit them.

## Selected design

Generalize the *authorization and charge* concept to recognize a second
provider; reuse the OpenRouter-shaped parsing wherever the real response
confirmed it matches, rather than building a parallel standards-only path
that the evidence doesn't actually call for yet.

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
- Charge extraction (`%conversation-openrouter-charge`) and the
  OpenRouter-shaped error extraction stay as-is and get called for either
  provider kind: the confirmed response shape means no fork is needed
  there. Keep the function names as they are for this slice rather than
  renaming for symmetry — renaming a charge-accounting function touched
  this heavily tonight is its own risk, better done deliberately later
  with the accounting test suite in hand, not as a drive-by rename here.
- Add a defensive standards-shaped fallback in the error classifier only
  (not the charge path, which has no reasonable standards-only fallback
  that wouldn't risk misreporting spend): if a Nous Portal response's
  error object doesn't have the OpenRouter shape, fall back to a plain
  `error.message` read rather than crashing opaquely.
- ZDR/data_collection stay OpenRouter-only fields; a Nous Portal profile
  simply omits them.
- A new profile entry in `config/conscious-provider-profiles.json`
  (`"provider": "nous-portal"`, `"endpoint":
  "https://inference-api.nousresearch.com/v1/chat/completions"`,
  `"model": "xiaomi/mimo-v2.5"`) becomes selectable via the existing
  `--provider-profile` flag and `experiment_routing`, no new CLI surface
  needed.
- Before wiring it into a live instance: confirm streaming and a real
  error response against the actual account (see above), since the
  charge/error reuse decision rests on the success-path evidence only.

## Rejected alternatives

- **Leaving `%conversation-openrouter-endpoint-p` as a literal string
  match and just adding a second, parallel literal-string function for
  Nous Portal** was rejected in favor of the provider-kind dispatch:
  functionally similar today, but a third provider later would mean a
  third near-duplicate function rather than a third `cond` clause.
- **Building a separate standards-only cost-estimation path up front**
  (computing charge from token counts against the price table) was
  rejected for this slice, now that the real response confirms
  `usage.cost` is present and OpenRouter-shaped. Revisit only if a real
  error response or a different Nous Portal model is confirmed to lack
  it — don't build for a case that hasn't been observed.
- **A generic "OpenAI-compatible" abstraction that OpenRouter itself also
  routes through** stays rejected: the two providers' request/response
  shapes matching closely today is convenient, not a guarantee they stay
  that way, and OpenRouter's real extensions are load-bearing for the
  accounting and reliability work hardened this very night. Provider-kind
  dispatch keeps both paths independently correct even if one shape
  drifts from the other later.
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

The core cost/error assumption is now backed by a real success-path
response, which is enough to start the provider-kind dispatch and
authorization-gate changes. Two things are still worth confirming against
the real account before calling this done, not before starting:

- A deliberately bad request (wrong model slug, or a request over
  whatever quota exists), to see the real error shape and confirm the
  reuse decision above rather than assume it.
- A `"stream": true` request, since every live conversation call in this
  substrate streams and none of tonight's evidence covers it.

Implementing the dispatch itself does not need to wait on those two --
they inform whether the defensive standards-only fallback in the error
path ever actually gets exercised, not whether the design is sound.
