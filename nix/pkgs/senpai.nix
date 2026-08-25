{
  lib,
  buildGoModule,
  fetchFromSourcehut,
  installShellFiles,
  scdoc,
  nix-update-script,
}:

buildGoModule rec {
  pname = "senpai";
  version = "0.5.0";

  src = fetchFromSourcehut {
    owner = "~delthas";
    repo = "senpai";
    rev = "v${version}";
    hash = "sha256-VjXgKdy4IpBhAP6uw/NtlexPki7nJzQi/HuY/+5lE/o=";
  };

  vendorHash = "sha256-4Ax9YVa9z1Unk3Z2iy9ZEqKjNmdgK0aF4GrD9ucXtjk=";

  subPackages = [
    "cmd/senpai"
  ];

  ldflags = [
    "-s"
    "-w"
    "-X=git.sr.ht/~delthas/senpai.version=${version}-nix"
  ];

  nativeBuildInputs = [
    scdoc
    installShellFiles
  ];

  postInstall = ''
    scdoc < doc/senpai.1.scd > doc/senpai.1
    scdoc < doc/senpai.5.scd > doc/senpai.5
    installManPage doc/senpai.*
    install -D -m 444 -t $out/share/applications contrib/senpai.desktop
    install -D -m 444 res/icon.48.png $out/share/icons/hicolor/48x48/apps/senpai.png
    install -D -m 444 res/icon.128.png $out/share/icons/hicolor/128x128/apps/senpai.png
    install -D -m 444 res/icon.svg $out/share/icons/hicolor/scalable/apps/senpai.svg
  '';
}
