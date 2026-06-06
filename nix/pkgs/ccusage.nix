{
  lib,
  stdenv,
  rustPlatform,
  fetchFromGitHub,
  fetchurl,
  pkg-config,
  apple-sdk_15,
  libiconv,
}:
let
  # ccusage embeds the LiteLLM model-pricing table at build time. Its build
  # script downloads this file from the network, which fails in the Nix sandbox.
  # Upstream pins the exact revision via a flake input; mirror that here.
  litellmPricing = fetchurl {
    url = "https://raw.githubusercontent.com/BerriAI/litellm/f27df8d516802ce4c1b32973992154fe83b851cf/model_prices_and_context_window.json";
    hash = "sha256-zJa6H2EwP9s+hMVs78Y+hwo4UX1dHRtvX5J3MdGh5aI=";
  };
in
rustPlatform.buildRustPackage (finalAttrs: {
  pname = "ccusage";
  version = "20.0.6";

  src = fetchFromGitHub {
    owner = "ryoppippi";
    repo = "ccusage";
    tag = "v${finalAttrs.version}";
    hash = "sha256-uf/FlPprxx4jh74YwjmYMtoIHpTkKrWTLetbNoYiFv4=";
  };

  cargoRoot = "rust";
  buildAndTestSubdir = "rust";

  cargoHash = "sha256-izA2Gs5nPmt0zn6/e1xM80vyyQHYKGEUDpUFRpyFiB8=";

  strictDeps = true;

  nativeBuildInputs = [ pkg-config ];

  buildInputs = lib.optionals stdenv.hostPlatform.isDarwin [
    apple-sdk_15
    libiconv
  ];

  env.CCUSAGE_PRICING_JSON_PATH = "${litellmPricing}";

  cargoBuildFlags = [
    "-p"
    "ccusage"
    "--bin"
    "ccusage"
  ];

  doCheck = false;

  meta = {
    description = "Analyze coding agent CLI token usage and costs from local data";
    homepage = "https://github.com/ryoppippi/ccusage";
    license = lib.licenses.mit;
    mainProgram = "ccusage";
    platforms = lib.platforms.all;
  };
})
