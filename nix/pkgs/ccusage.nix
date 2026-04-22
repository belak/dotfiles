{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
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
    hash = "sha256-zHGQlTWCsXJJrkRRh3gevpkL1R61Rmdtrt+LCGeazzk=";
  };

  nativeBuildInputs = [
    nodejs
    pnpm_10.configHook
    makeWrapper
    bun
  ];

  buildPhase = ''
    runHook preBuild
    # Run tsdown directly instead of `pnpm run build`. The build script also
    # runs generate:schema, which shells out to `git rev-parse` to copy the
    # schema into docs/public/ — a docs-site artifact that ccusage itself
    # doesn't need. The committed config-schema.json is the real input.
    pnpm --filter=ccusage exec tsdown
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/ccusage $out/bin
    cp -r apps/ccusage/dist $out/lib/ccusage/

    makeWrapper ${lib.getExe nodejs} $out/bin/ccusage \
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
