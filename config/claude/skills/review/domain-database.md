# Domain: Database

Loaded when the diff touches a migration, a schema definition, or code that builds queries. Every target is Postgres. The named forms are raw SQL, drizzle, prisma, kysely, knex, sequelize, sqlx, and Ecto; a lens states the rule once and the forms are how to check it.

## Tooling

`squawk` over every changed `.sql` file, one output file, listed as unavailable when it is not on `PATH`. Migrations written in JS, Elixir, or the prisma DSL get no tool, and the Lock lens does that work by hand. Then the **schema-change grep** over the changed files with `drizzle/meta/` snapshots dropped from the list (generated JSON that matches every term), saved by the scout and passed to the Value agent as a path:

```
rg -n -i 'alter table|create (unique )?index|drop (table|column|index)|truncate (table )?"|delete from|update \w+ set|not null|set default|rename (to|column)|alter column|lock table|for update|concurrently|disable_ddl_transaction|\.transaction\(|sql`|\.raw\(|\.execute\(|\.query\(|query!\(|Repo\.(query|delete_all|update_all)'
```

## Entry map

The **schema-change map**: every DDL statement in the diff, every raw SQL string, every query that writes, and every `WHERE` clause that takes an input. Each raw SQL string that interpolates a value is also a stop for the web Input trust lens.

## Defects lenses

- **Lock** — every `ALTER TABLE`, `CREATE INDEX`, and change to `NOT NULL` or a default. → Name the lock the statement takes and how long it holds on a table of production size; reads and writes queue behind an `ACCESS EXCLUSIVE` lock for the whole rewrite. The safe forms: `CREATE INDEX CONCURRENTLY` outside a transaction (Ecto: `concurrently: true` with `@disable_ddl_transaction true`; drizzle and sqlx: a migration file marked to run without a transaction), `ADD COLUMN` with no volatile default, `NOT NULL` added as a `CHECK ... NOT VALID` then validated. An ORM `migrate` step that emits any of the unsafe forms is the same hit.
- **Expand-contract** — every rename, drop, or type change of a column or table. → The previous release still runs while the migration lands. The diff adds the new shape, dual-writes, and drops in a later change, or states why the old code cannot be live. A migration that runs in the build step (`db:migrate && next build`) always overlaps the previous release. Whether the dropped data survives is the Irreversible lens's question; this lens owns the outage.
- **Backfill** — every `UPDATE` or `INSERT ... SELECT` inside a migration. → It runs in bounded batches, and it does not share a transaction with the DDL that locks the table. An unbatched backfill of a large table holds row locks for its whole run. When the repo does not state the table's size, the hit stays and names the size at which it bites.
- **Index** — every new query predicate, join, order, and foreign key. → An index covers it, or the table is provably small. A query built inside a loop over another query's rows is an N+1; the fix is a join or an `IN` list.
- **Transaction** — every multi-statement write and every read-then-write. → One transaction wraps the write set, so a failure midway leaves nothing half-applied. The read-then-write holds `FOR UPDATE`, or a unique constraint turns the race into a clean error, or the write is idempotent. Two requests interleaving between the read and the write is the failure.
- **Types** — every column type and every value crossing the driver. → Money is `numeric` or an integer count of minor units, never `float`. Timestamps carry a timezone (`timestamptz`). A `bigint` or `numeric` reaching JS arrives as a string or `BigInt`, never `Number`. An enum change is additive, or the code handles the removed value.

## Value lenses

- **Irreversible** — every `DROP`, `TRUNCATE`, `DELETE`, type narrowing, and migration with no down. → The data is backed up, copied to the new shape, or provably unused. A `DELETE` or `UPDATE` with no `WHERE`, or whose `WHERE` takes a client input, is a hit. A drop that also breaks the live release is reported once, under Expand-contract, with the data question answered here.
- **Row scope** — every query that reads or writes by id. → The predicate binds the row to the principal (`AND user_id = $1`, `AND org_id = ...`). Web Authorisation checks the handler; this checks the query, since a handler that checks and a query that does not is one refactor from a leak.

## Pinned additions

Every migration has been run up and then down in the test run, or the report lists that under tools unavailable. Every row-scoped query has a test with another principal's id. Every transaction has a test that fails a middle statement and asserts nothing landed.
