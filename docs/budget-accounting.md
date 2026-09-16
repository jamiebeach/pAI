# Provider budget accounting

The runtime admits provider work against cost, not request count. The global
cost ceiling and the private cost share are independent limits. Request totals
in `/budget` are telemetry; they do not authorize another call.

Provider calls normally charge the provider's reported cost. Before an
OpenRouter request starts, the runtime also computes a conservative capacity
bound from the selected model's context capacity, maximum configured token
prices, and the request's completion bound. That bound is accounting evidence
for cases in which the transport outcome is ambiguous.

An interrupted response can expose a valid generation ID before exact provider
cost becomes available. The runtime records that generation and its sealed
capacity bound, marks accounting uncertain, and pauses further provider work.
At the next admission attempt it makes one later exact-cost lookup:

1. If exact cost is available, it charges that amount.
2. If exact cost is still unavailable and the sealed bound is finite, it charges
   the bound and records `generation-capacity-fallback`.
3. If no finite bound exists, accounting remains uncertain and admission stays
   paused.

The settlement is charged to the global counter and, when the interrupted call
was private, to the private counter as well. Clearing uncertainty restores
admission only if the resulting counters still fit their respective ceilings.

`/budget` reports raw spent and remaining dollars, `accounting_uncertain`, the
pending generation count, and the sum of their conservative fallbacks. Remaining
dollars and admission state can therefore differ while a generation awaits
settlement.

Rejected alternatives were clearing uncertainty, charging zero, resetting
session spend, or retrying exact metadata forever. The first three can discard a
real provider charge; the last turns a metadata gap into a permanent cognition
outage.
