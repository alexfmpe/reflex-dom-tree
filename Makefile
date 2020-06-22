LIB=reflex-dom-tree

all:
	nix-shell --run "cd ${LIB} && cabal new-build"

watch:
	nix-shell --run "cd ${LIB} && ghcid -c 'cabal new-repl'"

test:
	nix-shell --run "cd ${LIB} && ghcid -c 'cabal new-repl test:trees'"
