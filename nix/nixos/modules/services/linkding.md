# Linkding
Or: One bookmark manager to find them

Linkding runs alongside Readeck on `vivi` at
<https://linkding.elwert.cloud>, using the native NixOS service with SQLite
and Pocket ID sign-in. Caddy forwards to nginx on `vivi`, which proxies
to Linkding on loopback port 9090. HTTPS uses Zidane's primary
`*.elwert.cloud` certificate.

## Authentication

Create or update the Pocket ID client with callback URL
`https://linkding.elwert.cloud/oidc/callback/` (including the trailing
slash). Restrict the client's access to the `linkding:users` group.
From the repository's `secrets/` directory, run `agenix -e linkding-env.age`
and set:

```text
OIDC_RP_CLIENT_ID=your-client-id
OIDC_RP_CLIENT_SECRET=your-client-secret
```

Deploy both `vivi` and `zidane` after configuring the client. When changing
the domain, update the existing client's callback URL; its credentials
can stay the same.

On `vivi`, run `sudo linkding-manage createsuperuser` before the first
Pocket ID login. Set its email to the address Pocket ID returns so OIDC
sign-in matches the administrator account. If the account already exists,
promote it instead of creating another account.

Other OIDC users are created automatically. Pocket ID groups do not assign
Linkding administrator access. Local login remains available for recovery,
so the Pocket ID group restriction does not apply to local-password login.

## Backups

Back up `/var/lib/linkding`, including the SQLite databases, generated
secret key, and saved assets. Readeck and Linkding keep separate data;
enabling Linkding does not import bookmarks from Readeck.
