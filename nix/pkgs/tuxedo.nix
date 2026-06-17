{
  lib,
  rustPlatform,
  fetchFromGitHub,
  writableTmpDirAsHomeHook,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "tuxedo";
  version = "2026.6.3";

  src = fetchFromGitHub {
    owner = "webstonehq";
    repo = "tuxedo";
    tag = "v${finalAttrs.version}";
    hash = "sha256-1uTa+S1bUyBsWy5FpmXFbggFc7lMbnDKul0h1O4NvMI=";
  };

  cargoHash = "sha256-PIhtD0/0hxFOn51PwOWCtz82a2dvhS+2jbd8Wvr/JUM=";

  nativeBuildInputs = [ writableTmpDirAsHomeHook ];

  meta = {
    description = "Fast, keyboard-driven terminal UI for todo.txt";
    homepage = "https://github.com/webstonehq/tuxedo";
    license = lib.licenses.mit;
    mainProgram = "tuxedo";
    platforms = lib.platforms.all;
  };
})
