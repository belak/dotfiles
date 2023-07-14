{ lib
, stdenv
, fetchFromGitHub
, installShellFiles
}:

stdenv.mkDerivation rec {
  pname = "pyenv-virtualenv";
  version = "1.2.1";

  src = fetchFromGitHub {
    owner = "pyenv";
    repo = "pyenv-virtualenv";
    rev = "refs/tags/v${version}";
    hash = "sha256-kIjhxr39r8PT3pMvUQohkS2QHwX3QwtZn9n1Z7/nOxc=";
  };

  postPatch = ''
    patchShebangs --build install.sh
  '';

  nativeBuildInputs = [
    installShellFiles
  ];

  configureScript = "src/configure";

  makeFlags = ["-C" "src"];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    PREFIX="$out" ./install.sh
  '';

  meta = with lib; {
    description = "Simple Python version management";
    homepage = "https://github.com/pyenv/pyenv-virtualenv";
    changelog = "https://github.com/pyenv/pyenv-virtualenv/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ tjni ];
    platforms = platforms.all;
  };
}
