{
  lib,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  pname = "pyenv-virtualenv";
  version = "1.2.4";

  src = fetchFromGitHub {
    owner = "pyenv";
    repo = "pyenv-virtualenv";
    rev = "refs/tags/v${version}";
    hash = "sha256-NgtowwE1T5NoiYiL18vdpYumVuPSWoDCOyP2//d+uHk=";
  };

  installPhase = ''
    mkdir -p "$out"
    cp -R bin "$out/bin"
    cp -R libexec "$out/libexec"
    cp -R shims "$out/shims"
    cp -R etc "$out/etc"
  '';

  meta = with lib; {
    description = "Simple Python version management";
    homepage = "https://github.com/pyenv/pyenv-virtualenv";
    changelog = "https://github.com/pyenv/pyenv-virtualenv/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.all;
  };
}
