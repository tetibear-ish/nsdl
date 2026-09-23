{
  description = "NSDL toolchain (OCaml + dune + menhir)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems f;
    in
    {
      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            packages = [
              pkgs.ocaml
              pkgs.dune_3
              pkgs.ocamlPackages.menhir
              pkgs.ocamlPackages.findlib
              pkgs.ocamlPackages.notty-community
              pkgs.ocamlPackages.nottui
              pkgs.ocamlPackages.nottui-unix
              pkgs.ocamlPackages.lwd

              # WebAssembly build (web/): js_of_ocaml is the runtime
              # library nsdl_web.ml is written against; wasm_of_ocaml
              # -compiler (dune's `(modes wasm)`) is what actually
              # produces the .wasm, but its dune integration shares
              # tooling with js_of_ocaml-compiler, so both are here.
              pkgs.ocamlPackages.js_of_ocaml
              pkgs.ocamlPackages.js_of_ocaml-ppx
              pkgs.ocamlPackages.js_of_ocaml-compiler
              pkgs.ocamlPackages.wasm_of_ocaml-compiler
              pkgs.binaryen # provides wasm-opt, which wasm_of_ocaml shells out to
            ];

            # `nix develop` has no real derivation output, so it fabricates
            # $out as $PWD/outputs/out for scripts that expect it to exist.
            # nixpkgs' stdenv setup hook then adds "-rpath $out/lib" to
            # NIX_LDFLAGS, and since this project's path ("IT Wizards")
            # contains a space, the C linker word-splits that flag and
            # fails. Strip just that bogus self-rpath entry.
            shellHook = ''
              export NIX_LDFLAGS="$(printf '%s' "$NIX_LDFLAGS" | sed "s#-rpath $PWD/outputs/out/lib##")"
            '';
          };
        });
    };
}
