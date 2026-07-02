(ns game.platform
  "Platform detection for KAMI Engine SDK.

  Restored from kami-game (kotoba-lang/kami-engine, deleted PR #82),
  per ADR-2607010930. Ported 1:1 from `kami-game/src/platform.rs`.
  Detects iOS, Android, and Web (desktop browser) at runtime; used by
  input systems to decide touch vs keyboard controls.")

(def platforms
  "Rust `Platform` enum values."
  #{:ios :android :web})

(defn mobile?
  "True if running on a mobile device (iOS or Android)."
  [platform]
  (contains? #{:ios :android} platform))

(defn touch?
  "True if touch input should be the primary input method."
  [platform]
  (mobile? platform))

(defn detect-from-user-agent
  "Detect the current platform from a user-agent string."
  [ua]
  (let [ua-lower (clojure.string/lower-case ua)]
    (cond
      (or (clojure.string/includes? ua-lower "iphone")
          (clojure.string/includes? ua-lower "ipad")
          (clojure.string/includes? ua-lower "ipod")
          (and (clojure.string/includes? ua-lower "macintosh")
               (clojure.string/includes? ua-lower "mobile")))
      :ios

      (clojure.string/includes? ua-lower "android")
      :android

      :else :web)))
