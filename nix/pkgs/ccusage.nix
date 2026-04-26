{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  nodejs_24,
  pnpm_10,
  bun,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ccusage";
  version = "18.0.11";

  src = fetchFromGitHub {
    owner = "ryoppippi";
    repo = "ccusage";
    tag = "v${finalAttrs.version}";
    hash = "sha256-EzHFKZVq0okgRumxn6+4rfxDtz0jY6FBoO9eyrGX4ys=";
  };

  pnpmDeps = pnpm_10.fetchDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 2;
    hash = "sha256-5y9S0tGV6ANVFDP8ATv3tw/uImYhbOYNMwlyLF60O9w=";
  };

  nativeBuildInputs = [
    nodejs_24
    pnpm_10.configHook
    makeWrapper
    bun
  ];

  buildPhase = ''
    runHook preBuild

    # The upstream monorepo declares engines.runtime with node@^24 / onFail:
    # download, causing pnpm to vendor a Node binary into node_modules that
    # doesn't work inside the Nix sandbox. Replace the broken shim with the
    # system Node so pnpm exec and .bin scripts resolve correctly.
    ln -sf ${lib.getExe nodejs_24} node_modules/.bin/node

    # Skip generate:schema (needs `git rev-parse`); the committed
    # config-schema.json is the real input.
    pnpm --filter=ccusage exec tsdown

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/ccusage $out/bin
    cp -r apps/ccusage/dist $out/lib/ccusage/

    makeWrapper ${lib.getExe nodejs_24} $out/bin/ccusage \
      --add-flags $out/lib/ccusage/dist/index.js

    runHook postInstall
  '';

  meta = {
    description = "Usage analysis tool for Claude Code";
    homepage = "https://github.com/ryoppippi/ccusage";
    license = lib.licenses.mit;
    mainProgram = "ccusage";
    platforms = lib.platforms.all;
  };
})
