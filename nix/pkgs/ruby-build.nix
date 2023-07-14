{ lib, stdenv, fetchFromGitHub, bash }:

stdenv.mkDerivation rec {
  pname = "ruby-build";
  version = "20230710";

  src = fetchFromGitHub {
    owner = "rbenv";
    repo = "ruby-build";
    rev = "v${version}";
    hash = "sha256-/IoRgODmXotvOeCNiJN9SePvywNGgccq2nGtfMG/wtA=";
  };

  installPhase = ''
    mkdir -p "$out"
    cp -R bin "$out/bin"
    cp -R share "$out/share"
  '';

  meta = with lib; {
    description = "Groom your app’s Ruby environment";
    longDescription = ''
      Use rbenv to pick a Ruby version for your application and guarantee that your development environment matches production.
      Put rbenv to work with Bundler for painless Ruby upgrades and bulletproof deployments.
    '';
    homepage = "https://github.com/rbenv/ruby-build";
    changelog = "https://github.com/rbenv/ruby-build/releases/tag/v${src.rev}";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.all;
  };
}
