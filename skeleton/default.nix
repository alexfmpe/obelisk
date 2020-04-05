{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super: {
    inline-c = self.callHackageDirect {
      pkg = "inline-c";
      ver = "0.9.1.0";
      sha256 = "0phr840cjkb2hj4hvgrrfxqj1zfgy204qa1h3mnhnag8fi2d10ly";
    } {};
  };
})
