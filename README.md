# incio.el

An Emacs interface to [incident.io](https://incident.io/) using the `inc` CLI.

## Usage

Install `inc` and authenticate with `inc auth login`, then run:

- `M-x incio-incident-list` for active incidents
- `M-x incio-alert-list` for alerts
- `M-x incio-escalation-list` for live escalations
- `M-x incio-schedule-list` for on-call schedules

Incident list keys:

| Key | Action |
| --- | --- |
| `RET` | Visit incident |
| `s` | Change status |
| `a` | Assign role |
| `A` | Acknowledge |
| `R` | Reject |
| `m` | Merge into another incident |
| `c` | Close incident |
| `u` | Post update |
| `F` | Add follow-up |
| `w` | Open permalink |
| `r` / `g` | Refresh |
| `q` | Quit |

Press `RET` on an incident to open its detail view. It shows the summary,
severity, timestamps, type, visibility, mode, roles, custom fields, Slack
channel, and permalink. Detail views support `s`, `a`, `A`, `R`, `m`, `c`,
`u`, `F`, `w`, `g`, and `q` as well.

All mutating actions ask for confirmation where appropriate. The package uses
`inc api` for incident.io operations not exposed as typed CLI commands.

## Customization

`incio-inc-executable` controls the backend executable path.

## License

GPL-3.0-or-later.
