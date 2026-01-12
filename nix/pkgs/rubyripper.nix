{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  cdparanoia,
  cddiscid,
  cdrdao,
  ruby,
}:
let
  myRuby = ruby.withPackages (
    ps: with ps; [
      gettext
      gtk3
    ]
  );
in
stdenv.mkDerivation rec {
  version = "0.8.0rc4";
  pname = "rubyripper";

  src = fetchFromGitHub {
    owner = "bleskodev";
    repo = "rubyripper";
    rev = "v${version}";
    sha256 = "sha256-WpYd61+jxVd4/odi1rSiy8m5oPrdAOI67PJXR1kPQ+4=";
  };

  preConfigure = "patchShebangs .";

  configureFlags = [
    "--enable-cli"
    "--enable-gtk3"
  ];

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [
    cddiscid
    cdparanoia
    cdrdao
    myRuby
  ];

  postInstall = ''
    cp -r share $out/
  '';

  postFixup = ''
    wrapProgram $out/bin/rrip_cli --prefix PATH : ${
      lib.makeBinPath [
        cddiscid
        cdparanoia
        cdrdao
        myRuby
      ]
    }

    wrapProgram $out/bin/rrip_gui \
      --prefix PATH : ${
        lib.makeBinPath [
          cddiscid
          cdparanoia
          cdrdao
          myRuby
        ]
      }
  '';
}
