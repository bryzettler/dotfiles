# Domain: EVM

Loaded when the diff touches Solidity, Vyper, or a client that sends transactions to an EVM chain.

## Tooling

`forge build` and `forge test` (or the Hardhat equivalents), `slither .` when installed, and the **external-call grep** over the changed contracts, attached to the Value brief:

```
rg -n '\.call\{?|\.delegatecall|\.staticcall|selfdestruct|ecrecover|tx\.origin|block\.timestamp|block\.number|msg\.value|transfer\(|transferFrom\(|safeTransfer|approve\(|_mint\(|_burn\(|initializer|_disableInitializers|onlyOwner|onlyRole|unchecked|assembly|receive\(\)|fallback\(\)|balanceOf\(address\(this\)\)|latestRoundData|getReserves|slot0'
```

## Entry map

The **external-call map**: every call that leaves the contract (transfer, `call`, `delegatecall`, an interface call to a token, an oracle, a router, a callback), in program order relative to the state writes around it; every function with a modifier or with none; every `payable` entry; every `initialize` and every storage-layout change on an upgradeable contract.

## Value lenses

- **Access** — every external and public function. → Name who may call it and the line that enforces it (`onlyOwner`, `onlyRole`, an `msg.sender ==` check). A function with no modifier is public to the world; a function that trusts `tx.origin` is public to any contract the owner ever calls. Every privileged setter (fee, oracle, treasury, implementation, pauser) is behind the role the spec names, and the role can be transferred in two steps or is deliberately one-way.
- **Reentrancy** — every external call, in program order. → Checks, effects, interactions: every state write the call could care about lands before the call, or the function is behind a reentrancy guard that also covers its siblings (cross-function and cross-contract reentrancy share the guard only when the guard is shared). ETH transfers to an address the caller chose, ERC777 and ERC1155 hooks, and `onERC721Received` are all callbacks. A read-only function whose result another protocol consumes can be reentered too.
- **Call result** — every low-level `call`, every token call. → The success flag is checked, or the call is wrapped in `SafeERC20`. A token that returns `false` instead of reverting, a token that returns nothing, and a call to an address with no code (which succeeds) each pass an unchecked call. `delegatecall` goes only to an address the contract controls; `selfdestruct` in a delegatecall target kills the caller.
- **Token quirks** — every token the contract accepts. → Fee-on-transfer and rebasing tokens make "amount sent" differ from "amount received"; the code measures balance before and after or the spec excludes such tokens. Decimals are read, not assumed 18. `approve` from non-zero to non-zero races; `permit` is front-runnable, so the caller's flow survives a permit that already landed. A pausable or blocklisting token can freeze a vault's accounting.
- **Upgradeable** — every contract behind a proxy. → The implementation's `initialize` runs once (`initializer` modifier, `_disableInitializers` in the constructor), the storage layout of the new version appends and never reorders or retypes (check against the previous version's layout, not the diff), no immutable or constructor state is relied on from the proxy, and the upgrade function is behind the role the spec names.
- **Signature** — every `ecrecover`, `ECDSA.recover`, and EIP-712 digest. → The digest binds the domain (name, version, chain id, verifying contract), the action, every parameter that matters, a nonce that the contract consumes, and a deadline. `ecrecover` of a bad signature returns address zero, so the result is compared to a non-zero expected signer. Signature malleability is handled by the library or by an `s` bound. Then replay: the same signature on another chain, another contract, another function, or after the parameters it does not cover have changed.
- **Value** — every `payable` function and every `msg.value` read. → `msg.value` inside a loop or a `multicall` is counted once per call but spent once per iteration. ETH sent to a contract with no `receive` reverts; a contract that can receive ETH it cannot withdraw locks it. A pull-payment pattern is preferred over pushing ETH to an address that can revert.
- **Time and order** — every `block.timestamp`, `block.number`, and every action whose value depends on what lands before it. → A miner or sequencer can shift the timestamp within a small window; a deadline or auction end tolerates that. A transaction that reveals a price or a winner can be sandwiched or front-run; the code carries a commit-reveal, a slippage bound, or a deadline, and the spec accepts the residual.

## Pinned additions

Every `require` or custom error the diff adds has a test that reaches it and asserts that specific error. Every external call that moves value has a test with a reentrant receiver contract. Every `initialize` has a second-call test. Every signed action has a replay test (same signature twice, and on a second contract instance). Every arithmetic path with a user-controlled input has a fuzz test with the bound stated.
