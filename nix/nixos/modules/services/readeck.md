# Readeck
Or: Read now, remember later

Readeck runs on `vivi` at <https://readeck.elwert.cloud>, using SQLite and
Pocket ID for sign-in. Caddy on `zidane` terminates HTTPS and forwards to
nginx on `vivi`. The service uses the pinned unstable package because
stable nixpkgs does not yet include native OIDC support.

## Authentication

Create a Pocket ID client with callback URL
`https://readeck.elwert.cloud/login/oidc` and allow the `readeck:users` and
`readeck:admins` groups. From the repository's `secrets/` directory, run
`agenix -e readeck-env.age` and add:

```text
READECK_AUTH_OIDC_PROVIDERS_0_CLIENT_ID=your-client-id
READECK_AUTH_OIDC_PROVIDERS_0_CLIENT_SECRET=your-client-secret
READECK_SECRET_KEY=a-long-random-value
```

Generate the secret key locally with `openssl rand -hex 32` and keep it
stable across restarts. Do not put plaintext credentials in Nix settings.
OIDC users are provisioned automatically. The `readeck:admins` group maps
to administrator access, and `readeck:users` maps to regular-user access.
The administrator mapping takes precedence for members of both groups.
Restrict client access in Pocket ID; role mapping is not an access allowlist.
Keep a separate local administrator for recovery if Pocket ID is unavailable.

## Backups and imports

Back up `/var/lib/private/readeck`, which systemd exposes as
`/var/lib/readeck`, including the SQLite database and saved content. Browser
tab exports and Watch Later JSON need an import adapter; enabling this
service does not import them.
