# RemoteSlurm findings from surface preparation

2026-09-26. Read-only investigation of the installed editable RemoteSlurm
checkout at `/Users/bbuchsbaum/code/pycode/remoteslurm`, HEAD
`7d3f775d4846440889ba9162f6a4eb6ac0fadae7`. No RemoteSlurm files changed.

## Confirmed: upload source loses its trailing slash

`src/remoteslurm/cli.py:557`, `cmd_put()`, converts `args.src` to a `Path`,
then forwards `str(src)` to `_rsync()`. The conversion drops the trailing slash.
For rsync this changes directory-content upload into directory upload.

A mocked call against the installed CLI confirmed that `bundle/` becomes
`bundle`. The earlier Trillium transfer demonstrated the actual consequence:
the destination contained an extra `bundle-01/` directory. No files were lost,
and all files were subsequently hash-verified at their actual location.

Fix scope: preserve the user's trailing-slash intent in the source string
passed to rsync after path expansion/validation. Add directory transfer tests
with and without a slash, including an already-existing destination.

## Confirmed: rsync omits configured SSH transport options

`src/remoteslurm/cli.py:649`, `_rsync()`, uses `sync.DEFAULT_SSH`, while the
SSH transport builds its commands using `control_path` and `extra_ssh_opts`.
A mocked transfer with an explicit socket and port showed both missing from
the resulting rsync SSH command. This affects rsync-backed `put` and `get`.

For Nibi, doctor reports a configured ControlPath distinct from the default
SSH-config path. Therefore a working remoteslurm connection alone does not
establish that this rsync path will reuse the same connection. This run used
non-deleting rsync with the explicit known Nibi socket and preserved trailing
slash; upload succeeded in 6.9 seconds.

Fix scope: derive the rsync SSH argv from the active SSH transport and quote
that argv for rsync's `-e` argument. Test ControlPath, port/ProxyJump options,
and preservation of the existing authenticated transport. Do not initiate a
new MFA login as a transfer fallback.

Reproduction evidence: `work/remoteslurm-bug-reproduction.json`, generated
against the installed CLI with mocked external side effects. Transfer receipt:
`work/nibi-upload.log.meta.json`.

## Unresolved: original Trillium scheduler timeouts

Neither confirmed transfer defect explains the original `squeue`, `sshare`,
or `sbatch --test-only` timeouts. Those commands use the stub transport, not the
rsync wrapper. The earlier Trillium preflight returned all 47 successful hash
checks before timing out at the scheduler command.

The later direct-SSH comparison could not reach the scheduler: it returned
`Permission denied (keyboard-interactive,hostbased)`. This is a later
authentication failure, not proof of the cause of the earlier timeouts.
Nibi's doctor, partition query, and Workbench probe now succeed through
remoteslurm. A same-session direct-versus-stub comparison on Trillium remains
needed before attributing its original stalls to either the cluster or client.
