{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl {
  inherit system iosSdkVersion;
  # You must accept the Android Software Development Kit License Agreement at
  # https://developer.android.com/studio/terms in order to build Android apps.
  # Uncomment and set this to `true` to indicate your acceptance:
  # config.android_sdk.accept_license = false;
};
project ./. ({ pkgs, hackGet, ... }: with pkgs.haskell.lib;
let
  repos = {
    reflex = hackGet dep/reflex;
  };

  overrideSrcs = {
    inherit (repos) reflex;
  };
in {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = pkgs.lib.foldr pkgs.lib.composeExtensions  (_: _: {}) [
    (self: super: pkgs.lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
  ];
})
