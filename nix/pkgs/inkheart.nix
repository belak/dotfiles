{
  lib,
  stdenv,
  fetchFromGitLab,
  fetchurl,
  rustPlatform,
  buildNpmPackage,
  makeWrapper,
  pkg-config,
  openssl,
}:

let
  version = "1.5.1";

  src = fetchFromGitLab {
    owner = "Nystik";
    repo = "inkheart";
    tag = version;
    hash = "sha256-2QuMLy91+mOsoRlgMxX425wqQktMqB0M7Mq/rGQEQJU=";
  };

  # Must match the PDFium pin in backend/build.rs; postPatch checks.
  pdfiumVersion = "8138";
  pdfiumBuilds = {
    x86_64-linux = {
      target = "linux-x64";
      sha256 = "d6b1e28950a0392590d79bbdbb7eb19686872b688f8a36f170acb19e41473113";
    };
    aarch64-linux = {
      target = "linux-arm64";
      sha256 = "c70b1ce4d13257f741af19e0aeee575890875995eb81370a65be942e601a1862";
    };
    aarch64-darwin = {
      target = "mac-arm64";
      sha256 = "21a1567b397b571f4f77f21ca91eac7b892774219c3c715a5127a20950ccb18b";
    };
  };
  pdfiumBuild =
    pdfiumBuilds.${stdenv.hostPlatform.system}
      or (throw "inkheart: unsupported system ${stdenv.hostPlatform.system}");
  pdfiumLib = "libpdfium${stdenv.hostPlatform.extensions.sharedLibrary}";
  pdfium = fetchurl {
    url = "https://github.com/bblanchon/pdfium-binaries/releases/download/chromium%2F${pdfiumVersion}/pdfium-${pdfiumBuild.target}.tgz";
    inherit (pdfiumBuild) sha256;
  };

  frontend = buildNpmPackage {
    pname = "inkheart-frontend";
    inherit version src;
    sourceRoot = "${src.name}/frontend";
    npmDepsHash = "sha256-QGwDHPhInknN3n46Y7Yt062eYWod/1r5iWxckc3t5Uk=";

    # Needed for the svelte-navigator git dependency.
    makeCacheWritable = true;
    forceGitDeps = true;

    installPhase = ''
      runHook preInstall
      cp -r dist $out
      runHook postInstall
    '';
  };
in
rustPlatform.buildRustPackage {
  pname = "inkheart";
  inherit version src;
  sourceRoot = "${src.name}/backend";
  cargoHash = "sha256-mheEypZXxxAYxeu34GntAVdU5sGfcVyaF1FAPbw18nU=";

  nativeBuildInputs = [
    makeWrapper
    pkg-config
  ];
  buildInputs = [ openssl ];

  postPatch = ''
    grep -qF '"chromium%2F${pdfiumVersion}"' build.rs
    grep -qF '("${pdfiumBuild.target}", "${pdfiumBuild.sha256}")' build.rs
  '';

  # build.rs only downloads PDFium when lib/ lacks it.
  preBuild = ''
    mkdir -p lib
    tar -xzf ${pdfium} -C lib --strip-components=1 lib/${pdfiumLib}
  '';

  # The binary finds lib/ and public/ relative to its working directory.
  postInstall = ''
    mkdir -p $out/libexec $out/share/inkheart
    mv $out/bin/inkheart $out/libexec/inkheart
    cp -r lib $out/share/inkheart/lib
    cp -r ${frontend} $out/share/inkheart/public
    makeWrapper $out/libexec/inkheart $out/bin/inkheart \
      --chdir $out/share/inkheart
  '';

  meta = {
    description = "Self-hosted PDF library and reader";
    homepage = "https://gitlab.com/Nystik/inkheart";
    changelog = "https://gitlab.com/Nystik/inkheart/-/blob/${version}/CHANGELOG.md";
    license = lib.licenses.asl20;
    mainProgram = "inkheart";
    platforms = lib.attrNames pdfiumBuilds;
  };
}
