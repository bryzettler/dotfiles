# RTK - Rust Token Killer

**Usage**: Token-optimized CLI proxy (60-90% savings on dev operations)

## Meta Commands (always use rtk directly)

```bash
rtk gain              # Show token savings analytics
rtk gain --history    # Show command usage history with savings
rtk discover          # Analyze Claude Code history for missed opportunities
rtk bypass <cmd>      # Execute raw command without filtering (for debugging)
```

## Troubleshooting

If `rtk gain` fails, check `which rtk`: reachingforthejack/rtk (Rust Type Kit) uses the same name.

## Hook-Based Usage

All other commands are automatically rewritten by the Claude Code hook.
Example: `git status` → `rtk git status` (transparent, 0 tokens overhead)

Run `rtk --help` for the full command list.

## Exclusions

`git log` and `git show` are never rewritten (`[hooks] exclude_commands` in `~/Library/Application Support/rtk/config.toml`). For one raw run of any other command, prefix it with `RTK_DISABLED=1`.
