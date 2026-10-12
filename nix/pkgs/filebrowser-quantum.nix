{
  lib,
  fetchFromGitHub,
  buildGo127Module,
  versionCheckHook,
  buildNpmPackage,
  fetchNpmDeps,
}:

let
  version = "2.0.0-stable";

  src = fetchFromGitHub {
    owner = "gtsteffaniak";
    repo = "filebrowser";
    tag = "v${version}";
    hash = "sha256-Bfy0ntpPB6U7NVNKkPc4S6sGRrYk+Dvm1MOnLxq8Seo=";
  };

  frontend = buildNpmPackage {
    pname = "filebrowser-quantum-frontend";
    inherit version src;

    # Build from the repo root so the tree is writable; Vite writes to
    # backend/internal/web/dist.
    npmRoot = "frontend";
    npmDeps = fetchNpmDeps {
      inherit src;
      name = "filebrowser-quantum-frontend-${version}-npm-deps";
      sourceRoot = "${src.name}/frontend";
      hash = "sha256-ZJZtHaeNpHV7n3cT0m7YmBuIGM8CObp3IdQWdSiqbKc=";
    };

    buildPhase = ''
      runHook preBuild

      pushd frontend
      npm run build:docker
      popd

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out
      cp -r backend/internal/web/dist/* $out

      runHook postInstall
    '';
  };

in
buildGo127Module {
  pname = "filebrowser-quantum";
  inherit version src;

  modRoot = "backend";

  vendorHash = "sha256-P3H5kESEd1X5tl0EQyIm680fKz/18KegoPBp1jb69vo=";

  preBuild = ''
    mkdir -p internal/web/embed
    cp -r ${frontend}/* internal/web/embed/
  '';

  postInstall = ''
    mv $out/bin/backend $out/bin/filebrowser-quantum
    rm -f $out/bin/setup-gofitz-cgo
  '';

  doInstallCheck = true;
  nativeInstallCheckInputs = [ versionCheckHook ];
  versionCheckProgramArg = "version";

  ldflags = [
    "-w"
    "-s"
    "-X github.com/gtsteffaniak/filebrowser/backend/internal/version.Version=v${version}"
  ];

  meta = {
    description = "Access and manage your files from the web";
    homepage = "https://github.com/gtsteffaniak/filebrowser";
    changelog = "https://github.com/gtsteffaniak/filebrowser/blob/v${version}/CHANGELOG.md";
    license = lib.licenses.asl20;
    mainProgram = "filebrowser-quantum";
  };
}
