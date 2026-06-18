{ pkgs ? import <nixpkgs> { } }:

# Scala Native dev shell.
#
# http4s Ember (via fs2-io's TLS layer) links against s2n-tls on Scala Native,
# so `libs2n` must be on the linker path. Inside this shell the Nix clang
# wrapper automatically adds `-L`/`-I` flags for every buildInput, which is
# what lets `-ls2n` resolve at link time. Just run `nix-shell` then `sbt run`.
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.sbt
    pkgs.clang
    pkgs.llvm
    pkgs.boehmgc # Scala Native's default (immix/commix) doesn't need this,
                 # but it's handy if you switch GC.
  ];

  buildInputs = [
    pkgs.s2n-tls # TLS backend used by fs2-io / http4s-ember on Native
    pkgs.openssl # libcrypto, transitive dependency of s2n-tls
    pkgs.zlib
  ];

  # Scala Native compiles its C runtime without -O in debug mode, which trips
  # NixOS's default _FORTIFY_SOURCE hardening into emitting noisy warnings.
  hardeningDisable = [ "fortify" ];
}
