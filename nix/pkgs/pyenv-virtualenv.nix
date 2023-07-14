{ lib, stdenv, fetchFromGitHub, bash }:

stdenv.mkDerivation rec {
  pname = "pyenv-virtualenv";
  version = "1.2.1";

  src = fetchFromGitHub {
    owner = "pyenv";
    repo = "pyenv-virtualenv";
    rev = "refs/tags/v${version}";
    hash = "sha256-G79U7/jd1tYP5xp+1UkK91mnRpM1o1h8ypQ3PmfMUDM=";
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
