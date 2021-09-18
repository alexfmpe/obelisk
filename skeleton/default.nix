{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    reflex = hackGet ./dep/reflex;
    reflex-dom = (hackGet ./dep/reflex-dom) + /reflex-dom;
    reflex-dom-core = (hackGet ./dep/reflex-dom) + /reflex-dom-core;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    reflex-dom-core = doJailbreak (dontCheck super.reflex-dom-core);
    reflex-dom = doJailbreak (dontCheck super.reflex-dom);
  };
})
