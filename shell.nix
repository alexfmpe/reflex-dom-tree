let lib = "reflex-dom-tree";
    src = ./. + ("/" + lib);
    rp = import ./dep/reflex-platform {};
in rp.workOn rp.ghc (rp.ghc.callCabal2nix lib src {})
