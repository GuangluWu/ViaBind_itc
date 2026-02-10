# ITCSuiteWeb Deployment Guide (Small Scale, <10 concurrent users)

## 1. Target Architecture

- Single Linux host
- `shiny-server` (or Posit Connect)
- `nginx` reverse proxy
- `systemd` service guard
- Single app instance (`ITCSuiteWeb`)

## 2. Run Command (systemd target)

```bash
cd /Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb
Rscript -e 'shiny::runApp(".", host="0.0.0.0", port=3838)'
```

## 3. Example systemd unit

See:

- `/Users/guanglu/Documents/myScript/ITCSuite/scripts/deploy/itcsuiteweb.service`

## 4. Example nginx site config

See:

- `/Users/guanglu/Documents/myScript/ITCSuite/scripts/deploy/nginx.itcsuiteweb.conf`

## 5. Health and Logs

- App logs: `/Users/guanglu/Documents/myScript/ITCSuite/ITCSuiteWeb/logs/itcsuiteweb.jsonl`
- Service logs (systemd): `journalctl -u itcsuiteweb -f`

## 6. Grey Release Checklist (5-10 internal users)

- Validate `itc_bundle_v1` export/import roundtrip
- Validate legacy sheet import (`integration_rev`, `meta_rev`)
- Validate cancellation during long fitting jobs
- Track errors and average durations on Metrics tab
