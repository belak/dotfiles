{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  pname = "trekscii";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "k-vernooy";
    repo = "trekscii";
    rev = "8b51971c4c62f49f886d59f2c8445ce8734b00e8";
    hash = "sha256-Mn3wasplwXsDCBEpHLqdh0G+SqYIirj7lKvM3VehPH0=";
  };

  installFlags = [ "DESTDIR=$(out)" ];

  installPhase = ''
    mkdir -p $out/bin
    install -m 0755 bin/trekscii $out/bin/
  '';

  #enableParallelBuilding = true;

  # NOTE: 2018-05-31: CMake is working but it is not officially supported
  #nativeBuildInputs = [ pkg-config ];

  #buildInputs = [ boost libtorrent-rasterbar qtbase qttools qtsvg ];
}
