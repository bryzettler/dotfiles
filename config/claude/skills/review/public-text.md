# Public text

Every body that leaves the machine meets this standard: a PR review, an inline comment, a reply, a summary, a PR body, a commit message. Treat the repo as open source: a body is public the moment it posts.

## Write from the diff

Name the code, the line, and the property of the code that makes the change worth taking. Leave everything else in the terminal report.

A body is public-safe when it contains none of these:

- `.scratch/` paths, ticket numbers, `GLOSSARY.md` terms, memory files, or session history
- incident or outage details, log excerpts, replica counts, tick rates
- hostnames, RPC endpoints, dashboards, Sentry or ticket links
- wallet or program addresses, key names, multisig details, deploy schedules
- the names of people or teams
- anything learned about how the code runs in production

When a reason needs one of these to make sense, rewrite it to lean on the code. When the code alone cannot carry it, post the edit with a shorter reason.

## Disclosure

A Value finding whose exploit path also exists in deployed code (a pre-existing missing check the change now makes reachable, or a helper the change reuses) is a live vulnerability. Post only the fix, with a neutral one-line reason ("bind the destination to the signer"). Put the attack, the affected deployed paths, and the urgency in the terminal report for the user, who decides how to notify the team.
