---
name: cross-repo-tracer
description: Trace data flow and dependencies across multiple repos in ~/Documents/Work when debugging cross-service issues
---

# Cross-Repo Tracer

Trace data flow and logic across repos in `~/Documents/Work` when debugging issues that span multiple services.

## When to Use

- Debugging issues that cross repo boundaries
- Tracing how data flows between services
- Understanding cross-service dependencies
- Finding where shared types, events, or API contracts are defined and consumed

## Process

### 1. Discover Repos

Scan `~/Documents/Work` for git repos dynamically — never hardcode repo names.

```bash
find ~/Documents/Work -maxdepth 2 -name .git -type d | sed 's|/.git||'
```

### 2. Identify Connection Points

Search across repos in parallel for these cross-boundary connectors:

- **Protobuf definitions** (`*.proto` files) — shared message types and service definitions
- **API routes/endpoints** — REST paths, gRPC service methods
- **Shared type names** — structs, interfaces, type aliases that appear in multiple repos
- **Event names** — publish/subscribe topics, webhook event types
- **Database schemas** — table names, migration files
- **Import paths** — cross-repo package references
- **Environment variables** — shared config keys

### 3. Parallel Search Strategy

Spawn parallel Explore agents, one per relevant repo (or group of small repos). Each agent searches for:

- The specific identifier being traced (function name, type, event, route)
- Related identifiers (callers, consumers, producers)
- Configuration that connects services (URLs, topic names, queue names)

### 4. Build the Trace

Present results as a flow showing ownership and transformations:

```
repo-a:src/handler.rs::publish_event()
  → proto/events.proto::RewardEvent
  → repo-b:src/listener.ts::onRewardEvent()
  → repo-c:src/api/rewards.rs::process_reward()
```

Include:

- Which repo **owns** the definition
- Which repos **consume** it
- Where **transformations** happen (serialization, mapping, validation)
- Where the **bug likely lives** based on the trace

### 5. Output Format

```markdown
## Trace: [identifier being traced]

### Flow

[arrow diagram as above]

### Repos Involved

- **repo-name**: role (owner/consumer/transformer)

### Key Files

- `repo/path/to/file.rs:42` — description of what happens here

### Analysis

[Where the issue likely is and why]
```

## Key Behaviors

- Always discover repos dynamically from `~/Documents/Work`
- Use parallel agents — one per repo or logical group
- Follow the data, not the code structure
- Prioritize protobuf, API contracts, and event names as primary connectors
- When tracing Helium-specific flows, pay attention to Solana program instructions, gRPC services, and Oracle reward calculations
