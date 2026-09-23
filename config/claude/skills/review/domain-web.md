# Domain: Web

Loaded for every file with no chain or CI signal: backend, frontend (Next.js, React Router, React Native), jobs, infra, config. Also loaded beside a chain domain for the client that talks to the chain, and beside `database` for query and migration code.

## Tooling

`tsc --noEmit --noUnusedLocals --noUnusedParameters` (the flags catch what a fix commit leaves behind) and the repo's lint script, `npm audit` (or `pnpm audit`), and the **trust-boundary grep** over the changed files, saved by the scout and passed to the Value agent as a path:

```
rg -n 'req\.(body|query|params|headers|cookies)|searchParams|process\.env|exec\(|spawn\(|eval\(|dangerouslySetInnerHTML|innerHTML|redirect\(|fetch\(|axios|cors|Access-Control|webhook|stripe|verify(Signature|Webhook)|jwt|session|cookie|admin|role|isAuthorized|middleware|use server|use client|revalidate(Path|Tag)|AsyncStorage|SecureStore|Linking\.'
```

The scout also records whether the lint config carries `react-hooks` and `jsx-a11y`. A repo on biome alone enforces neither, and the Effect race and States and access lenses then do that work by hand.

## Entry map

The **trust-boundary map**: every place an input crosses from outside (a request, a webhook, a queue message, an env var, a third-party response, a URL parameter, a server action argument, a deep link or push payload) into code that reads it; every place the code decides who the caller is; every outbound request the input can steer; every path that moves money, credits, or privilege.

## Defects lenses

- **Effect race** — every fetch or subscription inside an effect. → When the inputs change or the component unmounts, the effect cancels the request or ignores its result (an abort controller, an `ignore` flag, or a query library keyed on the inputs). Two overlapping requests with the slower one landing last is the failure. Where the lint has no `react-hooks` rules, also compare each effect's dependency list against what its body reads.
- **Derived state** — every `useState` seeded from a prop or a query result, and every effect that copies one state into another. → The copy goes stale on the next prop change. The value is computed during render, or the component is keyed so it remounts.
- **Server boundary** (Next.js) — every `"use client"` file, every server action, and every write in a route handler. → A client file imports nothing that reads a secret env var or a database client; a shared module that does either is `server-only`. A write calls `revalidatePath` or `revalidateTag` for every cached page that shows the row, or the page is dynamic.
- **Cache** — every react-query or swr key and every mutation. → The key includes every input the fetcher reads. The mutation invalidates or updates every key it made stale. A `staleTime` or `cache: 'force-cache'` on data the user just changed is the failure.
- **List identity** — every `.map` that renders a list and every `FlatList`. → The key is a stable id when the list can reorder, filter, or hold inputs. An index key on such a list attaches state to the wrong row after a change. A `keyExtractor` whose output can collide is the same defect.
- **Pending** — every form submit and every control that fires a mutation. → The control is disabled or the request is deduplicated while pending, and the error path re-enables it. A double submit that reaches the server twice is a Replay hit for the Value agent.
- **Hydration** — every render that reads `Date.now`, `Math.random`, `window`, locale, or timezone. → It runs client-only (an effect, a `useSyncExternalStore` fallback, `dynamic` with `ssr: false`), or its output is identical on server and client.
- **States and access** — every component that shows async data and every interactive element. → Loading, empty, and error states each render something. Every clickable element is a `button` or a link, or carries a role and a label, and is reachable by keyboard. An icon-only control has an accessible name; a custom pressable in React Native has `accessibilityRole` and `accessibilityLabel`. Severity is low, except a keyboard-unreachable primary action or a missing error state on a money or auth flow, which is medium.

## Value lenses

- **Authorisation** — every handler and server action that reads or writes a resource by id. → The check is per object, not per route: the caller owns or is granted this record, and the id in the request is bound to the authenticated principal, never trusted from the body. A server action re-reads the session; its arguments are a request body. A new endpoint inherits the middleware chain of its neighbours or states why not. An admin flag, role, or plan tier read from a client-controlled source (a JWT claim the server never verifies, a cookie, a query string) is a hit.
- **Input trust** — every input on the trust-boundary map. → It is validated by shape and bound before use, and it never reaches a shell, a query, a file path, a template, or `innerHTML` unescaped. A URL the caller supplies and the server fetches (image proxy, webhook target, import) is bound to an allowlist or a public-host check, or the caller can reach internal services. A deep link's parameters are inputs of the same rank.
- **Session** — every login, logout, token issue, and cookie. → Cookies carry `HttpOnly`, `Secure`, and a `SameSite` value; a state-changing request from a browser needs a CSRF defence the diff can name; tokens expire and are revoked on logout; a password or token comparison is constant-time and a reset token is single-use. On React Native a token lives in `SecureStore` or the keychain, and a token in `AsyncStorage` is a hit.
- **Money path** — every payment, credit, refund, payout, and webhook. → The webhook signature is verified with the provider's secret before any state change, the event id is stored so a redelivery is a no-op, amounts and currencies come from the provider's object rather than the client's request, and a failed downstream step after a charge is visible (recorded and retried), not swallowed.
- **Exposure** — every response, log line, and error. → Internal ids, secrets, stack traces, and other users' data stay out of responses and logs. A CORS origin of `*` with credentials, a wildcard reflected from the request, or a debug endpoint left mounted is a hit.

## Pinned additions

Every authorisation check the diff adds has a test with another user's id, and every server action with a session check has a test with no session. Every input validator has a test with a malformed and an oversized input. Every webhook handler has a test with a bad signature and a redelivered event. Every component with an error state has a test that renders it.
