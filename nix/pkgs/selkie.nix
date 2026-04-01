{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "selkie";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "btucker";
    repo = "selkie";
    rev = "905b2f9ddb1ed6fd2698c6cebab51eaacd20e710";
    hash = "sha256-bfSnJFdb49koCfSCx5U5caJDMoqTnsOSOuR+h9DPVYY=";
  };

  cargoHash = "sha256-W05kgCPRp5ThxhGZ2Qc5k8vMfQ23dGJoLjtdmjPRh+E=";
}
