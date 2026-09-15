# Domain: Web

Loaded for every diff that touches backend, frontend, jobs, infra, or config with no chain signal, and alongside a chain domain when the diff touches the client that talks to the chain.

## Tooling

`tsc --noEmit` and the repo's lint script, `npm audit` (or `pnpm audit`), and the **trust-boundary grep** over the changed files, attached to the Value brief:

```
rg -n 'req\.(body|query|params|headers|cookies)|searchParams|process\.env|exec\(|spawn\(|eval\(|dangerouslySetInnerHTML|innerHTML|redirect\(|fetch\(|axios|cors|Access-Control|webhook|stripe|verify(Signature|Webhook)|jwt|session|cookie|admin|role|isAuthorized|middleware'
```

## Entry map

The **trust-boundary map**: every place an input crosses from outside (a request, a webhook, a queue message, an env var, a third-party response, a URL parameter) into code that reads it; every place the code decides who the caller is; every outbound request the input can steer; every path that moves money, credits, or privilege.

## Value lenses

- **Authorisation** — every handler that reads or writes a resource by id. → The check is per object, not per route: the caller owns or is granted this record, and the id in the request is bound to the authenticated principal, never trusted from the body. A new endpoint inherits the middleware chain of its neighbours or states why not. An admin flag, role, or plan tier read from a client-controlled source (a JWT claim the server never verifies, a cookie, a query string) is a hit.
- **Input trust** — every input on the trust-boundary map. → It is validated by shape and bound before use, and it never reaches a shell, a query, a file path, a template, or `innerHTML` unescaped. A URL the caller supplies and the server fetches (image proxy, webhook target, import) is bound to an allowlist or a public-host check, or the caller can reach internal services.
- **Session** — every login, logout, token issue, and cookie. → Cookies carry `HttpOnly`, `Secure`, and a `SameSite` value; a state-changing request from a browser needs a CSRF defence the diff can name; tokens expire and are revoked on logout; a password or token comparison is constant-time and a reset token is single-use.
- **Money path** — every payment, credit, refund, payout, and webhook. → The webhook signature is verified with the provider's secret before any state change, the event id is stored so a redelivery is a no-op, amounts and currencies come from the provider's object rather than the client's request, and a failed downstream step after a charge is visible (recorded and retried), not swallowed.
- **Exposure** — every response, log line, and error. → Internal ids, secrets, stack traces, and other users' data stay out of responses and logs. A CORS origin of `*` with credentials, a wildcard reflected from the request, or a debug endpoint left mounted is a hit.

## Pinned additions

Every authorisation check the diff adds has a test with another user's id. Every input validator has a test with a malformed and an oversized input. Every webhook handler has a test with a bad signature and a redelivered event.
