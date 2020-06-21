all:
	nix-shell -E 'let rp = import ./dep/reflex-platform {}; in rp.workOn rp.ghc (rp.ghc.callCabal2nix "reflex-dom-tree" ./. {})' --run "cabal new-build"
watch:
	nix-shell -E 'let rp = import ./dep/reflex-platform {}; in rp.workOn rp.ghc (rp.ghc.callCabal2nix "reflex-dom-tree" ./. {})' --run "ghcid -c 'cabal new-repl'"
