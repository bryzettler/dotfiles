# implement-tickets: re-hardening

Step 4, for a group that has a `Hardened:` line and whose tip moved since.

**Round count.** Count rounds per group over its whole life, not per run: the group's past rounds are the sum of the round counts on its tickets' `Hardened:` lines, and a new round's number is that sum plus one. A group that has used four rounds gets no more.

**The round.** At most one: a delta round on the diff since the last `Hardened:` sha, with that line's report path, and as follow-ups the `Unverified:` lines of the tickets landed since. Run it only when a ticket landed since that sha has Value as its manifest tier reason (funds, authority, keys, on-chain, migration, concurrency). Else run no round: a comment, docs, test, or ordinary code change does not buy a review of an already-hardened branch.

**The `Hardened:` line.** `cap` when the lifetime cap stopped a round. `carried` with 0 rounds when re-hardening ran none, followed by the ticket numbers no round read.
