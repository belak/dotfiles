# Zidane HTTPS
Or: One proxy to serve them all

Caddy on `zidane` terminates HTTPS and forwards requests to the service
hosts. Virtual hosts use the primary `elwert.cloud` and `*.elwert.cloud`
ACME certificate unless they override `useACMEHost`.

## Sandbox services

Zidane uses a separate certificate for `*.sandbox.elwert.cloud`, issued
and renewed through the existing Cloudflare DNS validation. Sandbox
virtual hosts must set `useACMEHost = "sandbox"`; the primary wildcard
certificate does not cover these nested names. ACME renewals reload
Caddy automatically.
