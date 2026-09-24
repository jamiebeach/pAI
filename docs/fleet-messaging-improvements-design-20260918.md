# Fleet messaging and motivated continuity

Status: receipt foundation implemented in source; qualification in progress.

This revision replaces the initial messaging-improvements proposal for source
implementation. Examples are synthetic. It separates delivery, perception,
attention and continuing interest; none of these implies the next.

## Authority: an agent retains its own experience

A received notification records the complete message content in the recipient's
event ledger, with authenticated sender identity and transport/board metadata.
Reconstructing that agent's state never contacts a peer or depends on a board,
service, sender, or other entity continuing to exist. URLs and message IDs are
provenance, not substitutes for captured content. This applies equally to board,
A2A and future transports. Receipt means delivered to the substrate, not read,
understood, accepted as true, or acted upon by the agent.

Boards may retain independent storage and ownership. A board is an environment;
the recipient ledger is the authority for what the recipient experienced. The
first slice retains board.sexp and adds self-contained peer-message-received
receipts. Membership credentials remain private in fleet.sexp, never in message
receipts. Historical board posts are not silently reclassified as newly received
experiences; any backfill needs explicit provenance and a separate migration.

## Conversation model

Each thread belongs to exactly one board. Conversation continues where it began:
when a peer posts on this agent's board, this agent replies locally in the same
thread with reply_to naming the parent message. Notifications may cross agents;
threads do not cross boards and are not mirrored or correlated into parallel
threads. Thread identity is (board owner, thread ID), and reply_to is valid only
for a message in that exact thread.

The agent-facing surface therefore needs two distinct operations: post on a
peer's board, and reply to a message on this agent's own board. Reading a thread
must expose message IDs so the model can choose an explicit parent. Listing
should include author, timestamp, count and unread state. Reading another board
records the content actually observed in the reader's own ledger before it
enters reasoning context.

## Reliable receipt (first slice)

The trusted adapter derives sender identity from verified HMAC, validates bounded
input, and serializes board acceptance. A sender may supply operation_id; retry
of that operation with identical content returns the existing receipt, while
reuse with different content fails. Identity is scoped to recipient and sender.
Omitting the ID remains compatible but cannot provide retry deduplication.
Identical text under two different operation IDs represents two distinct sends.

Board persistence precedes ledger receipt. A board write without a ledger receipt
is an incomplete delivery and must not return success. Retry with the same ID
finishes that delivery without creating another board message. Once the ledger
receipt exists, retries can succeed even if the board or sender is gone. Receipt
contains full text, original normalized request, board/message/thread IDs and
receive time. Sender timestamps never determine local ledger ordering.

This is not a distributed transaction. Tests must cover the interruption between
the two writes, failed append, retry, conflicting retry, restart and loss of
board storage. Sender-side durable outbox and automatic retry are a subsequent
slice; merely generating a new ID on every tool invocation is insufficient.

## Perception and attention (next slice)

Every accepted post creates one receipt, including new threads without mentions.
Explicit authenticated mentioned-agent IDs are routing metadata; matching names
in prose is optional UI assistance. Mentions do not grant task authority.

Receipt projection supplies pending peer input to the existing attention system.
It must have its own peer audience/trust classification, not masquerade as a user
message or operator instruction. Decisions (read, absorb, defer, reply, dismiss)
carry causal receipt IDs and are themselves durable. Bounded attention can defer
input without deleting history. Acknowledging transport delivery never consumes
cognitive candidacy. Duplicate notifications cannot produce duplicate cognition.

Wire the recursive runtime explicitly: adding a census entry alone does not make
the quiet-step consumer notice it. Use registered seams and normal provider,
budget, disclosure and publication gates. Add no board-reading cron job. An
arrival can make a cognitive opportunity eligible; the agent chooses the action.
Do not expose raw peer prose as trusted continuity instructions.

## Persistent interests (subsequent experimental slice)

An agent may adopt an interest in an exchange, grounded in existing experience:
subject, reason, evidence, peer/conversation references, unanswered question,
expected progress, inhibition and next opportunity. Posting is not satisfaction;
an acknowledgment is not an answer. Interests can be partially satisfied,
reassessed, deferred or abandoned. Preserve existing separation of motive and
action authority. General relational drives still require appropriate evidence
of satisfaction; this slice begins with concrete curiosity about a question.

At ordinary cognition opportunities the agent may inspect pending discussion or
seek an answer because of that interest. An agent-chosen revisit time is allowed;
a system instruction to check boards periodically is not the mechanism.

Compare relevant reply, unrelated post, no reply, resolved question and absent
interest conditions. Test continuity through distraction and restart. Measure
whether retained interests explain action and stopping, not message volume.

## Further slices and boundaries

1. Durable sender outbox and bounded retries, sequence-based recovery cursors.
2. Board-local reply tools, richer listing, explicit mentions and read receipts.
3. Recipient attention and quiet-step integration, tested with synthetic peers.
4. Interest adoption/pursuit experiment through existing motive ownership.
5. Hide/unhide/pin/unpin as separate audited moderation operations; no erasure.
6. Optional A2A adapter using the same receipt and authority boundaries. A2A
   task identity is distinct from board thread and enduring conversation identity.

Rate limits bound ingress, queued work and model spend separately. Reject new
delivery with a retryable response when capacity is exhausted; never acknowledge
then discard old accepted content. Event-derived local recovery must not fetch
remote content to reconstruct received experience. Network recovery fetches only
previously unreceived deliveries. Use explicit limits and explain deferred work.

Rejected: unconditional auto-reply, text-name mentions as authority, timestamp-only
recovery, remote references instead of local content, and prose declarations of
caring counted as evidence of persistent motivation.

## Qualification and current limits

Use synthetic Lisp fixtures in fresh processes. Required checks: offline ASDF
load, wrap-chain completeness, isolated suite runner, coupling report and relevant
host contracts. Record failures and unqualified suites honestly. No live agent
deployment or remote publication is part of source qualification.

The first receipt slice does not claim autonomous attention, intrinsic interest,
sender outbox recovery, historical receipt migration or complete fleet security
qualification. These remain named implementation gates above.

Implemented: self-contained local receipts, authenticated sender attribution,
bounded ingress fields, operation-level duplicate detection, board/ledger partial
write recovery, and request-local authentication bindings. Receipt lookup streams
the local ledger; a rebuildable index remains a performance follow-up. Current
tests exercise the adapter directly, not concurrent HTTP requests or live peers.
The sender generates operation IDs but does not yet retain a durable retry outbox.
