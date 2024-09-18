{
  lib,
  stdenv,
  fetchFromGitHub,
  installShellFiles,
}:
stdenv.mkDerivation rec {
  pname = "rbenv";
  version = "1.3.0";

  nativeBuildInputs = [ installShellFiles ];

  src = fetchFromGitHub {
    owner = "rbenv";
    repo = "rbenv";
    rev = "v${version}";
    hash = "sha256-AO0z9QtCGHwUr2ji28sbvQmCBDIfjAqbiac+HTH3N7Q=";
  };

  postPatch = ''
    patchShebangs src/configure
    pushd src
  '';

  installPhase = ''
    popd
    mkdir -p $out/bin
    mv libexec $out
    ln -s $out/libexec/rbenv $out/bin/rbenv

    installShellCompletion completions/{rbenv.bash,_rbenv}
  '';

  meta = with lib; {
    description = "Groom your app’s Ruby environment";
    longDescription = ''
      Use rbenv to pick a Ruby version for your application and guarantee that your development environment matches production.
      Put rbenv to work with Bundler for painless Ruby upgrades and bulletproof deployments.
    '';
    homepage = "https://github.com/rbenv/rbenv";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.all;
  };
}
