with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    gmp
  ];
}
