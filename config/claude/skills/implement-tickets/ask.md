# implement-tickets: ask

Run when a question arises: a deep `mismatch` in step 3, or the follow-ups and spec questions of a group's last round in step 4. Ask only the new questions; the rest of the run keeps going.

## Sort the follow-ups

Each follow-up in a step 4 return (a finding left as is with a wrong outcome, an accepted gap, a pre-existing bug out of scope) arrives tagged with a severity and a kind, and goes to one place. Make no follow-up a `needs-triage` ticket.

- Kind `live-check` (the claim was never run live, in staging, or in production), any severity → one `- [ ]` line in the group's pre-deploy checklist, `NN-predeploy-<group-slug>.md`: `**Type:** chore`, `**Status:** ready-for-human`, `**PR:** <group>`. Create it at the next free number on the group's first such follow-up, and append after that.
- Kind `pr-note` (a line the PR body or a README owes a reviewer), any severity → one line in `<issues-folder>/../pr-notes-<group>.md`.
- Kind `code`, severity low → one line in `<issues-folder>/../followups-<group>.md`: severity, the scenario in one sentence, `file:line`, the report path.
- Kind `code`, severity medium or higher → a question for the steps below, `Kind: followup`, with the return's two answers, `fix: <outcome>` and `wontfix: <reason>`, and its recommendation.

Each line names the ticket and round it came from. File a follow-up once: skip it when its `file:line` and scenario already appear in a ticket of any status, the checklist, either notes file, or `spec-questions.md`. Done when every follow-up is in one place.

## Ask the questions

The questions that just arose and are not yet in `spec-questions.md`. None: return to the step that called this file.

`AskUserQuestion` unavailable (a headless run): do step 1 only, then return to the step that called this file.

1. **File** — append each question to `<issues-folder>/../spec-questions.md` before you ask, so an interrupted session keeps it. An entry has `Ticket:`, `PR group:`, `Kind:`, the question verbatim, its spec line, the answers the return proposed with the recommended one marked, and an empty `Answer:`. The top of the file says how to answer by hand: `keep` or `change: <behaviour>` for a spec question, `fix: <outcome>` or `wontfix: <reason>` for a follow-up, a free-text decision for a mismatch. The next run applies the filled answers first; no flag is needed. Add a question already in the file once only.
2. **Ask** — put the questions to the user with `AskUserQuestion`, at most four per call, until each has an answer. Per question: the header is `#NN`; the question text gives the finding or mismatch in one sentence, its blast radius, then the spec line; the options are the return's recommended answer first, with `(Recommended)` after its label and the return's reason as its description, then the other proposed answer, then `Decide later`. Keep the return's recommendation: the agent that read the spec and the code made it. Change it only when something the user said earlier in this session settles the question, and say so in the description.
3. **Record** — write each answer to its entry's `Answer:` line. A chosen answer is recorded as its text: `keep` or `change: <behaviour>` for a spec question, `fix: <outcome>` or `wontfix: <reason>` for a follow-up, the answer itself for a mismatch. An `Other` reply that names a behaviour is `change: <that behaviour>`, for a follow-up `fix: <that outcome>`, or for a mismatch the reply itself. `Decide later`, and any other reply, leaves `Answer:` empty. Then apply the answered entries per `answers.md`.
4. **Return** — to the step that called this file. A ticket that `answers.md` created or set to `ready-for-agent` is picked up by step 5's loop; one whose blockers are all `done` may also dispatch now, as a frontier ticket.
