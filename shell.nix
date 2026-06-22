{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.clang
    pkgs.llvm
  ];

  buildInputs = [
    pkgs.s2n-tls # TLS backend used by fs2-io / http4s-ember on Native
    pkgs.openssl # libcrypto, transitive dependency of s2n-tls
    pkgs.zlib
  ];

  # Scala Native compiles its C runtime without -O in debug mode, which trips
  # NixOS's default _FORTIFY_SOURCE hardening into emitting noisy warnings.
  # hardeningDisable = [ "fortify" ];

  # `nix-shell --run` creates a fresh per-invocation TMPDIR under /tmp and
  # removes it on exit. Mill's long-lived daemon captures TMPDIR at startup, so
  # on a later invocation the Nix clang wrapper tries to write its cc-params
  # response file into the now-deleted dir and `mktemp` fails. Pin TMPDIR to a
  # stable location so the daemon and clang always have a directory that exists.
  #shellHook = ''
  #  export TMPDIR="''${TMPDIR_STABLE:-/tmp}"
  #'';
}
