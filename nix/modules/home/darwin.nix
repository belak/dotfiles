{ pkgs, lib, ... }:

let
  inherit (pkgs) stdenv;
in
{
  config = lib.mkIf stdenv.isDarwin {
    home.packages = with pkgs; [ ];
  };
}
