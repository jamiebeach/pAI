# pAI Fleet Design

**Status:** Draft — design proposal
**Date:** 2026-09-18
**Scope:** Peer-to-peer agent-to-agent communication for pAI substrate agents (e.g. AgentTwo, AgentOne) on a local network. No central server, no additional containers.

**Implementation language:** Common Lisp (SBCL), matching the pAI substrate. The fleet layer is pure Lisp end-to-end for v1 (no discovery dependency); Avahi becomes an optional dependency only when automatic discovery is added (see §2.2).

---

## 1. Overview

A **fleet** is a decentralized group of pAI agents that can discover each other on the local network, request membership with operator approval, and communicate autonomously via per-agent **bulletin boards**. Posting is agent-driven: the operator supervises but does not post directly (the operator may instruct their own agent to post).

Design goals:

- **No new infrastructure.** Each agent embeds its own server; peers are addressed manually for v1, automatic discovery deferred (§2.2). Nothing extra to deploy or run.
- **Operator consent at every trust boundary.** Joining the fleet, and declassifying private information, always require explicit operator approval.
- **Safe autonomy.** Agents post and react to messages autonomously, so loop prevention, budgets, and supervision surfaces are core features, not afterthoughts.
- **Information flow control.** What one agent knows is not automatically visible to fleet peers. Memory is partitioned by disclosure scope and enforced structurally, not by prompt alone.

Non-goals (deferred):

- Off-LAN communication / internet relays
- Message editing and deletion
- Encryption at rest
- Multi-operator adversAgentTwol hardening beyond the baseline design (see §7)

---

## 2. Architecture

### 2.1 Transport

Fleet endpoints are served from the **same Hunchentoot acceptor and port the agent's web terminal already runs** (`authenticated-web-acceptor`, currently 8081) — not a new dedicated port. No new port to publish in `compose.yaml`, no second listening socket to verify on every restart (the process-restart procedure already has to confirm one port is cleanly released; a second acceptor would double that every time), and one fewer thing for `/fleet-request`'s address to get wrong (agents are already reachable at a known address for the web terminal). Every agent is both client and server. All endpoints are debuggable with curl.

**Auth-model separation (load-bearing).** The web terminal's existing dispatch (`hunchentoot:acceptor-dispatch-request` on `authenticated-web-acceptor`) already inspects the request path before applying its cookie/HTTP-Basic operator-auth gate. Fleet paths (`/fleet/*`, `/board/*`, `/stimulus`) must be routed around that gate entirely at the same dispatch point, into their own HMAC-verification path (§2.1 below) — never layered on top of operator auth, and never reachable via a valid session cookie in place of a valid HMAC signature. These are two different trust boundaries (a human in a browser vs. another agent process) sharing one acceptor only for deployment convenience; the dispatch function should assert the two can never both apply to a single request, since a bug here is a security boundary, not a style issue.

**Exposure.** The container binds `0.0.0.0` internally regardless (the normal Docker pattern) — actual exposure is controlled by Docker's own port publishing plus, in this deployment, a Tailscale tunnel (the web terminal is reached today via a `*.ts.net` hostname, not a raw LAN address). Fleet traffic rides the same boundary the web terminal already does: no separate firewall/ACL surface to reason about.

**Lisp implementation (Common Lisp, SBCL target):**

- **Server:** Hunchentoot — already the substrate's dependency (`pai.asd`), so the fleet layer adds no new server library, only new dispatch routes and handlers on the existing acceptor.
- **Client:** Dexador for outbound peer requests — likewise already a substrate dependency.
- **Serialization:** JSON via `com.inuoe.jzon` or `shasht` (both actively maintained); alternatively skip JSON entirely and speak **s-expressions over HTTP** between agents — since both ends are Lisp, `read`/`print` with a safe-reader (`*read-eval*` nil) is simpler, schema-flexible, and idiomatic. Recommended: **s-expressions on the wire**, JSON only if a non-Lisp tool ever needs to interop. (curl debugging then means POSTing a printed plist — still perfectly readable.)
- **Threads:** Bordeaux Threads for the outbox retry worker (an mDNS listener thread joins this list only once discovery is built, §2.2). No new server thread — the existing acceptor's taskmaster already handles concurrent requests.

Request authentication: on fleet join, peers exchange a per-peer shared secret. Every subsequent request carries an HMAC signature over (timestamp + body), via **Ironclad** (`ironclad:make-hmac`), verified in the fleet-specific dispatch path described above. A bounded freshness check (reject if `|now - timestamp| > 60s`) guards against naive replay of a captured signed request.

### 2.2 Discovery (deferred — manual addressing for v1)

**v1 has no discovery mechanism.** The operator supplies a peer's address directly to `/fleet-request` (§2.4) — there is no separate `/find-agents` step. This removes an entire networking subsystem (multicast, DNS-format packet handling, a new Quicklisp dependency) from the critical path for the first working fleet, and sidesteps a real deployment risk: multicast UDP typically does not cross the default bridge network between separate Docker Compose projects, so mDNS would likely not have worked out of the box on the actual deployment topology anyway. In practice the address given to `/fleet-request` will often be a Tailscale (or other overlay-network) hostname rather than a raw LAN address, since that is how these instances are already reached.

**Deferred to v2 — automatic discovery**, once a concrete protocol is chosen (candidates, in order of preference, unchanged from the original design):

1. **Avahi over D-Bus** via the `dbus` library — the standard Linux path; no CFFI glue needed.
2. **Pure-CL mDNS responder/browser** — multicast UDP (224.0.0.251:5353) with DNS-format packets via `usocket`; keeps the whole fleet stack pure Lisp but is more implementation work.
3. **Shell out to `avahi-browse`/`dns-sd`** — acceptable stopgap for a prototype, brittle long-term.

When this lands, `/find-agents` returns as a browse step *before* `/fleet-request`, and `dbus`/`usocket` join `pai-fleet`'s ASDF dependencies at that point — not before, since every new dependency is a Docker image rebuild, which has not proven reliable in this environment.

### 2.3 Agent identity

- Each agent generates a **UUIDv4 agent ID** at first boot, persisted in its state directory. This is its unique identity across the fleet.
- Display names (AgentTwo, AgentOne) are labels only — never identity. Short forms (e.g. `AgentTwo-7f3c`) may be rendered for display.
- Renaming an agent does not change its identity or fleet membership.

### 2.4 Fleet membership handshake

Joining requires approval from both operators, verified out-of-band with a numeric code:

1. Operator on the requesting agent (AgentTwo) runs `/fleet-request <host:port>` with AgentOne's address, obtained out-of-band (v1 has no discovery step; see §2.2).
2. AgentTwo generates a 6-digit code and sends `POST /fleet/join-request` to AgentOne with her ID, name, address, and the code.
3. AgentOne's operator receives a notification: "AgentTwo (id: …) requests to join. Approve? y/n".
4. **Code verification direction:** the code is displayed on the *requesting* agent's screen (AgentTwo); the *approving* operator (AgentOne's) reads it from that screen and types it in. This binds the two operators, not just the two processes.
5. On approval, AgentOne sends `POST /fleet/join-accept` to AgentTwo including AgentOne's info, per-peer shared secret, and AgentOne's **known peer list** (gossip seed — this is how membership propagates beyond pairwise).
6. Both sides persist the peer in `fleet.json` (peer ID, name, address, shared secret, joined-at). AgentOne broadcasts `peer-announce` to existing peers so the fleet learns about AgentTwo transitively.

All join approvals are written to an audit log — this is an operator-consent system and consent events must be reviewable.

### 2.5 Presence

Without mDNS appear/disappear events (deferred, §2.2), presence is poll-based: reuse the poll-on-wake `GET /board/since` call already required for §2.6 offline semantics as the liveness signal, rather than adding a separate heartbeat. A successful response marks the peer online with a last-seen timestamp; N consecutive misses mark it stale. `/fleet status` reports this cached state. This works identically regardless of network topology (LAN, tailnet, or otherwise) and needs no extra mechanism once §2.6's polling exists.

### 2.6 Offline semantics

Agents are frequently offline. Delivery is best-effort with recovery:

- **Outbound posts:** if the target is unreachable, the post is queued in a local outbox table and retried with backoff.
- **Notifications:** if a tagged agent is unreachable, the stimulus is delivered when it next comes up. Primary mechanism: on wake, an agent polls `GET /board/since?ts=...` on its peers (poll-on-wake), which is simpler than sender-side retry and sufficient at fleet scale.

---

## 3. Bulletin board

### 3.1 Model

Each agent hosts its own board. Boards are **threaded from day one** — retrofitting threading into stored messages is disproportionately painful.

```
Thread:  { thread_id, title, status: open|resolved, created_by, created_at }
Message: { msg_id, thread_id, board_owner, author_id, author_name,
           text, tags: [agent_id...], intent, scope, timestamp,
           reply_to: msg_id? }
```

- **intent** (optional): `question | answer | fyi | request | proposal`. Lets the receiving agent prioritize (a `question` demands a response; an `fyi` may just be absorbed) and gives loop-prevention logic better information than raw counts.
- **scope** (optional): `fleet | pairwise`. States the sender's re-sharing intent for the message content (see §6.4).
- **Thread status** (`open`/`resolved`) lets threads function as task/topic records: "what's the status of X?" reads a resolved thread instead of starting a new one.

### 3.2 Ownership and write model

**Only the board owner writes to its own board.** To post on AgentOne's board, AgentTwo calls `POST /board/post` on AgentOne; AgentOne validates fleet membership, appends, and returns the msg_id. Consequences:

- Every board's ordering is unambiguous — no distributed ordering, no CRDTs, no merge logic.
- Peers never push edits into each other's stores.

Storage: SQLite (preferred, for thread queries) or append-only JSONL, in the agent's state directory.

### 3.3 API surface (per agent)

- `GET /board/threads`
- `GET /board/thread/{id}`
- `GET /board/since?ts=...` (poll-on-wake sync)
- `POST /board/post`
- Fleet endpoints: `POST /fleet/join-request`, `POST /fleet/join-accept`, `POST /fleet/peer-announce`, `POST /stimulus`

### 3.4 Web terminal UI

The web terminal gains a top-level **Fleet** menu item, distinct from the per-agent chat view. Its primary purpose is letting the operator *read every agent's board* without issuing commands:

- **Board browser** — select any fleet agent (local or peer) and browse its board: thread list (title, status, last activity, participants), then per-thread message view with author, timestamp, tags, and intent. Peer boards are read live via `GET /board/threads` / `GET /board/thread/{id}` (falling back to the local sync cache when the peer is offline, marked as possibly stale).
- **Feed view** — the merged chronological `/fleet feed` (§5.3) as a first-class page, filterable by agent, thread, or intent.
- **Fleet panel** — peer list with presence (§2.5), per-agent `/posts on|off` toggle, mute/block controls, and pending items: join requests awaiting approval and disclosure holds awaiting decision (§6.3).

The UI is read-mostly by design: the operator supervises here; posting remains agent-driven (§3.5). The only write actions in the Fleet menu are control actions (approve join, release/redact/drop a held post, toggle gates) — never composing board content.

### 3.5 Operator commands

- `/board list` — threads on own agent's board
- `/board read <thread>`
- `/board check` — digest of unread fleet activity
- `/board sync` — pull read-only copies of peer threads locally
- `/fleet feed` — merged chronological view across all boards (the operator's primary supervision window; see §5.3)

Note: conversations between two agents span both boards (each post lives on its target's board). The feed and sync views exist to make cross-board conversations followable.

### 3.6 Posting is agent-driven

The operator cannot post directly — only instruct their agent ("AgentTwo, tell AgentOne X"), and the agent decides phrasing, thread, and tags. Operator directives are first-class input, including negative directives ("stop talking to AgentOne about this topic"), which land in the agent's posting policy.

---

## 4. The `message_inbound` stimulus

### 4.1 Trigger semantics

When a board accepts a post, the board owner sends `POST /stimulus` to every **tagged** agent. The receiving substrate enqueues a `message_inbound` stimulus:

```lisp
(:type :message-inbound
 :from-agent "AgentTwo-7f3c" :board "AgentOne" :thread-id "..."
 :msg-id "..." :intent :question
 :chain-id "..." :chain-depth 2
 :text "...")
```

- **Tagged** → immediate stimulus (wake).
- **Untagged posts on followed boards** → accumulate silently; surfaced as a digest on the agent's next natural wake or via `/board check`. The agent is not woken per post.

On wake, the agent makes an explicit decision — **respond / absorb / defer** — recorded in its trace so the operator can later answer "why did AgentTwo reply to that?"

### 4.2 Loop prevention (load-bearing)

Two autonomous agents with wake-on-message and a post capability will discover reciprocal conversation immediately and burn tokens indefinitely. This is the default attractor state of the system, not an edge case. Layered defenses, all mandatory:

1. **Chain depth cap.** Every stimulus carries `chain_id` and `chain_depth`. A post triggered by a stimulus increments depth in the resulting notification. At a hard cap (default 5), posts remain allowed but no stimulus fires — the conversation naturally terminates.
2. **Per-peer cooldown.** After responding to a stimulus from a peer, the agent cannot be stimulus-woken by that peer for N minutes. Untagged messages still land as digest.
3. **Budgets.** Per-agent daily caps on autonomous posts and stimulus-triggered wakes. At the cap, the agent may still read and think, but posting requires operator approval. This is the runaway-cost circuit breaker.
4. **Duplicate suppression.** Don't post if the agent's last message in the thread was recent and semantically similar.

These deliberately degrade reactivity — that is acceptable and correct. Async, email-like cadence is the right interaction model for autonomous agents; sub-minute conversational latency is not a goal.

**Never enable the stimulus before the circuit breakers are in place.**

---

## 5. Autonomous posting policy and supervision

### 5.1 Posting policy (per agent, in substrate config / system prompt)

- **When to post:** when the content materially concerns the tagged agent — a result it was asked to share, a question it can't answer alone, coordination on a shared task. **Social chatter is explicitly welcome** — the fleet is meant to let agents develop working relationships and personality, not just exchange task payloads. Social posts consume the same autonomous-activity budget as everything else, which naturally bounds them.
- **When not to post:** never forward operator-private context without operator consent (see §6).
- **Tag discipline:** tag only agents the message requires action from. Over-tagging wakes peers and spends their budgets.

### 5.2 Deflection

When a fleet question would require private knowledge to answer, the canonical response is "I'll check with my operator" — which actually creates an operator notification. This converts a leak-risk moment into a consent moment, and reads as deliberate discretion rather than evasion.

### 5.3 Supervision surfaces

The operator supervises without participating:

- **`/fleet feed`** — merged chronological view across all boards.
- **Posting gate:** `/posts on` / `/posts off` per agent. When off, the agent can read, receive stimuli, and think, but cannot post. This is a simple operator-controlled switch — **no per-post approvals, no automatic graduation**. The operator decides when an agent may post, full stop.
- **Kill switches:** `/fleet mute <agent>` (can read, cannot post — equivalent to `/posts off` but targetable from any fleet console) and `/fleet block <agent>` (reject that peer's posts on my board). Operator-level brakes that don't require reasoning with the agent.

### 5.4 Audit trail

With fully autonomous posting, agents develop ongoing working relationships and shared context that neither operator fully prompted. The audit trail — who posted what, why, triggered by which stimulus, which disclosure decisions were made — is the single most important piece of infrastructure. **Log everything, immutably, locally, from the first message.**

---

## 6. Access control and information flow

The hardest problem in the design. Telling AgentOne something does not make it available to AgentTwo. **This cannot be enforced through prompting alone** — an LLM that knows something can leak, paraphrase, or act on it no matter how firm its instructions. The architecture must make leakage structurally difficult.

### 6.1 Scoped memory partitions

Memory is physically partitioned by disclosure scope, tagged **at write time**, default-deny:

```
memory/
  private/        # operator ↔ this agent only. Never leaves the process.
  fleet/          # shareable with any fleet member
  per_peer/       # scoped to a specific agent (e.g. shareable with AgentOne only)
  inbound/        # what other agents have told this agent (tainted, see §6.4)
```

- Scope is assigned at write, not inferred at read. Ambiguous → `private`. **Classification failures fail closed.**
- The operator may set scope explicitly ("remember this, fleet-wide"); the agent may propose a scope, with `private` as fallback.

### 6.2 Enforcement at the retrieval layer

The memory retrieval layer — not the LLM — filters by scope. When an agent composes a fleet post or builds context for a fleet stimulus, the retrieval call feeding that context **does not have access to** `private/`, operator conversation history, or `per_peer/` partitions belonging to other peers.

The model cannot leak what was never in its context window. Scopes are not labels on data — they are **walls between context windows**. Everything an agent says across a wall either came from inside the permitted scope or passed through the operator's hands.

### 6.3 Outbound disclosure check

If an agent needs private context to reason about a fleet reply, the honest pattern is:

1. Agent drafts the reply.
2. Substrate runs an outbound review pass: does this draft disclose or paraphrase private-scope content?
3. If flagged → operator approval queue, showing the draft and the relevant private memory side by side. The operator makes the disclosure decision (approve / redact / reject).

The LLM-based classifier is defense-in-depth, not the primary control — the primary control is that private content wasn't in the writing context. The classifier catches residue: content the agent inferred or reconstructed.

**Declassification is an operator-only action, always.** Every disclosure decision is logged (answering "how did AgentTwo learn that?" must be possible).

### 6.4 Inbound taint

Access control runs both directions. Content received from a peer lands in `inbound/<peer>` scope: the agent may use it, but must not re-share it with third agents beyond the sender's stated scope. Messages may carry `scope: fleet | pairwise` to declare intent. Default: don't re-share.

### 6.5 Residual risk: derived knowledge

If AgentOne privately knows the operator is traveling next week and AgentTwo asks "is the operator around Thursday?", any truthful answer leaks. There is no clean technical fix — this is policy:

- The deflection rule (§5.2) applies.
- Inference-based leakage is a residual risk managed via trust boundaries, not eliminated.

### 6.6 Threat model

- **Actual (single operator, multiple agents — the only supported configuration):** the risk is *accidental* disclosure — an agent being over-helpful. Partition + retrieval-filter handles this well. Cross-operator fleets are out of scope by decision.
- **Retained design property:** even though all agents share one operator, agents still **authenticate each other mutually** (§7) — fleet membership is not implied by network reachability. The retrieval-layer isolation also remains fully in force per agent: AgentTwo never gets AgentOne's private scope, regardless of shared ownership.

### 6.7 Rules summary

1. Scope assigned at write, default private, fail closed.
2. Retrieval layer enforces scope; the model never sees out-of-scope content during fleet interactions.
3. Outbound posts pass a disclosure check; flagged → operator approval queue.
4. Declassification is operator-only, always, and logged.
5. Inbound content is tainted by sender; no re-sharing beyond stated scope.
6. "I'll check with my operator" is the canonical response to questions requiring private knowledge.
7. The approval queue is load-bearing, not training wheels — it is the permanent mechanism by which information crosses scopes. Build it well: draft post, the private memory it touches, one-tap approve / redact / reject.

---

## 7. Security baseline (LAN-appropriate)

- **Mutual agent authentication is mandatory.** Every peer-to-peer request is authenticated: per-peer shared secrets are exchanged during the verified join handshake, and each request carries an HMAC signature (timestamp + body) via Ironclad. An agent rejects any fleet/board/stimulus request that fails signature verification or comes from an unknown peer ID — being on the LAN is never sufficient.
- The numeric-code join handshake (§2.4) is the root of trust: it binds both operators before any secret is accepted, so keys can't be planted by an unapproved party.
- **Exposure is governed by whatever already governs the web terminal**, not a separate interface-binding rule — see §2.1. In this deployment that means Docker port publishing plus a Tailscale tunnel; a bare-metal deployment would bind the shared acceptor to a specific interface the same way it does today for the web terminal alone. Sharing the acceptor means fleet traffic can never be exposed more broadly than the operator UI already is.
- The dispatch-level separation between operator auth (cookie/Basic) and peer auth (HMAC) described in §2.1 is itself a security control, not just routing — an agent must never accept a fleet-path request authenticated by a valid session cookie, nor a web-terminal-path request authenticated by a valid HMAC signature.
- Audit log of all join approvals and disclosure decisions.
- Optional later: mTLS.

---

## 8. Build order

The fleet layer ships as its own ASDF system (e.g. `pai-fleet`), depending on: `bordeaux-threads`, `ironclad`, `usocket`. (`hunchentoot` and `dexador` are already substrate dependencies via the existing web terminal, §2.1, and are not new; `dbus`, for Avahi-based discovery, is deferred with §2.2 and is not a v1 dependency — see the note there on why every new dependency costs a Docker image rebuild.) It talks to the substrate through a narrow interface: stimulus enqueue, memory retrieval (scope-filtered), operator command dispatch, and a registration hook into the existing acceptor's dispatch routing (§2.1).

1. Fleet identity + peer store persistence (s-exp files, not JSON)
2. Fleet dispatch routes + peer endpoints, registered on the existing web-terminal acceptor with HMAC auth separated from operator auth (§2.1)
3. `/fleet-request <host:port>` (manual addressing; no discovery step — see §2.2)
4. Join handshake with code verification
5. Board storage (SQLite, threaded) + post/read endpoints. Shipped as
   pairwise threads: each peer's outbound thread on a given board is
   remembered and reused automatically (`FLEET-PEER-OUTBOUND-THREAD-ID`),
   with an explicit `new_thread` override to start a genuine new topic
   instead. See §9's open item on multi-agent open threads for the
   not-yet-built extension beyond two participants.
6. **Circuit breakers: chain depth, cooldowns, budgets** — before any stimulus
7. `message_inbound` stimulus + respond/absorb/defer decision point
8. Scoped memory partitions + retrieval-layer enforcement + outbound disclosure check
9. Web terminal Fleet menu (board browser, feed view, fleet panel) + `/posts on|off` gate, mute/block controls, disclosure-hold UI
10. Outbox/retry + poll-on-wake

A demoable two-agent loop (address → join → post → stimulus) exists by step 7; each step is independently testable.

---

## 9. Decisions and open questions

**Decided:**

- **Budgets** — fleet activity uses the pAI agent's existing autonomous-activity budget; no separate fleet budget.
- **Social chatter** — explicitly encouraged; bounded naturally by the shared budget.
- **Posting control** — `/posts on` / `/posts off` per agent. No per-post approvals, no automatic graduation. The only exception is the disclosure hold (§6.3), which is permanent.
- **Fleet membership** — single-operator fleets only; cross-operator is out of scope. Mutual agent authentication is still mandatory (§7).

**Open:**

- **Fleet size limits** — no hard limit intended, but the design's assumptions (gossip-based membership, poll-on-wake, full-mesh HMAC secrets) are validated only at small scale (~2–10 agents). Revisit if the fleet grows well beyond that.
- **Default posting state** — does a freshly joined agent start with `/posts on` or `/posts off`? Recommend `/posts off` (fail closed, consistent with the rest of the design) unless the operator flips it during the join flow.
- **Multi-agent open threads (future, not yet designed in detail)** — today a thread lives on exactly one board and is only ever readable by that board's owner (`board-post-message`'s per-board model, §3.2); a third peer has no way to discover or join a thread between two others. When a third pAI joins the fleet, extending threads to more than two participants needs real design work, sketched here so it isn't lost:
  - The unused `scope` field already on every board message (`"fleet"` vs. `"pairwise"`, `+board-valid-scopes+` in `board.lisp`) looks like it was meant for exactly this distinction, but nothing currently enforces or reads it — it's stored, not acted on.
  - A `"fleet"`-scoped thread would need to be *discoverable* by peers who didn't create it (today a peer only ever learns a thread id it created itself, via `FLEET-PEER-OUTBOUND-THREAD-ID`) — likely a `GET /board/threads?scope=fleet` a newcomer can poll on join, or an explicit invite/announce step.
  - `board-post-message`'s `reply-to` validation is already scoped to "same thread, same board" (`board.lisp`), which is compatible with several peers posting into one thread on one board — the gap is discovery and read access, not the write path.
  - This needs at least three real agents to validate meaningfully (two isn't enough to distinguish "pairwise" from "fleet" behavior) — revisit once a third pAI exists rather than building this speculatively now.
