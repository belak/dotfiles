{
  stdenv,
  lib,
  fetchurl,
  fontconfig,
  openssl,
  libGL,
  libX11,
  libxcb,
  libxkbcommon,
  wayland,
  xcbutil,
  xcbutilimage,
  zlib,
  libiconv,
}:
let
  rpath = lib.makeLibraryPath (
    [
      fontconfig
      zlib
    ]
    ++ lib.optionals stdenv.isLinux [
      libGL
      libX11
      libxcb
      libxkbcommon
      openssl
      wayland
      xcbutil
      xcbutilimage
    ]
    ++ lib.optionals stdenv.isDarwin [ libiconv ]
  );
in
stdenv.mkDerivation rec {
  pname = "wezterm-bin";
  version = "20240203-110809-5046fc22";

  src = fetchurl {
    url = "https://github.com/wez/wezterm/releases/download/${version}/wezterm-${version}.Ubuntu22.04.tar.xz";
    hash = "sha256-FCy0beZIIAuFLMSx2TLmuPKp08VfbcWvRLbU7rk4X1w=";
  };

  #nativeBuildInputs = [ pkg-config python3 perl ];

  # prevent further changes to the RPATH
  dontPatchELF = true;

  installPhase = ''
    mkdir -p $out
    cp -r ./usr/* $out/
  '';

  postFixup = ''
    for artifact in wezterm wezterm-gui wezterm-mux-server strip-ansi-escapes; do
     patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$out/bin/$artifact" || true
     patchelf --set-rpath "${rpath}" $out/bin/$artifact
    done
  '';

  meta = with lib; {
    description = "GPU-accelerated cross-platform terminal emulator and multiplexer written by @wez and implemented in Rust (Pre-compiled version)";
    homepage = "https://github.com/wez/wezterm";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
