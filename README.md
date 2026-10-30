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
| `t` | Set title |
| `S` | Set summary |
| `v` | Set severity |
| `T` | Set timestamp |
| `f` | Set custom field |
| `w` | Open permalink |
| `/` | Filter incidents with API v2 query params (`C-u M-x incio-incident-list` to start filtered) |
| `\|` | Clear filters |
| `O` | Open on-call schedules |
| `r` / `g` | Refresh |
| `q` | Quit |

Press `RET` on an incident to open its detail view. It shows the summary,
severity, timestamps, type, visibility, mode, roles, custom fields, Slack
channel, and permalink. Detail views support the same keys, except `RET`.

Press `O` in the incident list to open schedules. Schedule list keys: `RET`
opens the schedule's on-call entries, `TAB` folds/unfolds a preview of the
next shifts, `r` / `g` refresh, `q` quits. In a schedule view:

| Key | Action |
| --- | --- |
| `]` | Next window |
| `[` | Previous window |
| `.` | Back to now |
| `c` | Toggle calendar view |
| `t` | Toggle timeline view |
| `o` | Override the shift (or day) at point |
| `r` / `g` | Refresh |

The window spans `incio-schedule-window-days` (default 7); the current shift
is shown in bold and overridden shifts use `incio-schedule-override`. The
calendar view shows one row per day and one column per rotation. Overrides
prompt for user, start and end, defaulting to the shift at point. The
timeline view shows one row per rotation, one character per
`incio-schedule-timeline-slot-hours` (default 3).

All mutating actions ask for confirmation where appropriate. The package uses
`inc api` for incident.io operations not exposed as typed CLI commands.

## Customization

`incio-inc-executable` controls the backend executable path.

## License

GPL-3.0-or-later.
