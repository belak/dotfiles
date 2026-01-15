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
  version = "d89304820f80822c378b025afcf20b18a4fff8a3";

  src = fetchFromSourcehut {
    owner = "~delthas";
    repo = "senpai";
    rev = version;
    sha256 = "sha256-9Y1TqE9eLpNIcbSvgIN2THwj6BYvXZdBBB45G+E060E=";
  };

  vendorHash = "sha256-puZvUF4y9bYVBy8EEqDIkdJbJps3CsoasGsMqTrhaFQ=";

  subPackages = [
    "cmd/senpai"
  ];

  ldflags = [
    "-s"
    "-w"
    #"-X=main.Version=${version}-nix"
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
