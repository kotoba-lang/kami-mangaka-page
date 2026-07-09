# kami-mangaka-page-clj

Work-agnostic **graphic-novel PAGE composition** (komawari コマ割り + DTP) — the
Tier-1 `mangaka` platform page layer (ADR-2606282100).

Ported from `mangaka.gftd.ai`'s `manga-layouts.ts` `GRAPHIC_NOVEL_TEMPLATES`
(left-to-right reading). It places rendered panel images into a B5 page, draws
panel frames + gutters, and overlays dialogue as speech bubbles and narration as
caption boxes — turning isolated panels into a readable page.

Generic: a `page` is just `{:layout str :panels [{:id :size :narration :dialogue}…]}`
and `img-of` maps a panel-id → image `File` (or nil → placeholder). No story,
character, or world. JVM/Java2D **headless** — no Canvas-2D, no GPU (this is page
DTP, not the engine's wgpu render path).

Sibling crates: `kami-mangaka-render-clj` (2D panel prompts) and
`kami-mangaka-scene` / `kami-mangaka-scene-clj` (3D).

## Origin

Split out of `kotoba-lang/kami-engine`'s
[`kami-mangaka-page-clj/`](https://github.com/kotoba-lang/kami-engine/tree/main/kami-mangaka-page-clj)
subtree into its own standalone repo, following the same split pattern already
used for the sibling `kami-mangaka-genko-clj` → `kotoba-lang/kami-genko`. This is
a live, current Clojure project — it was never part of `kami-engine`'s deleted
Rust workspace (PR #82).

## API

```clojure
(require '[kami.mangaka.page :as page])

(page/template-for 4)          ; → 4-panel %-rect layout (falls back to a grid for n>9)
(page/layout-page page)        ; → {:bleed bool :pairs [[panel [x y w h]] …]} (ネーム-driven)
(page/compose-page! page img-of "out.png")   ; → writes a B5 PNG, returns the path
```

A work supplies `page` maps from its own storyboard and a panel-id→File resolver;
`kami-app-sip-clj`'s `sip.page` is the thin facade that wires `sip.storyboard`.

## effectLines (効果線) & gaze (視線誘導)

Panels may carry the `ai.gftd.mangaka` page-lexicon fields `:effectLines` and
`:gaze`. Both use the **panel-local 0-1000 coordinate space** (both axes,
independent of the panel's pixel size — `{:centerX 480 :centerY 450}` = 48%
across / 45% down the panel rect).

```clojure
{:id "p1" :size "wide"
 ;; 効果線 — baked into the print image, clipped to the panel.
 ;; Z-order per the lexicon: panel art → tones → effectLines → SFX → bubbles.
 :effectLines [{:kind "focus" :centerX 480 :centerY 450 :density 44 :coverage 85}]
 ;; 視線誘導 — review-only metadata, never part of print output.
 :gaze {:entryX 850 :entryY 150 :focusX 480 :focusY 450
        :exitX 200 :exitY 700 :impression "dread"}}
```

- `:kind` — `focus`/`explosion` (radial black lines from the border toward the
  centre, leaving an inner clear radius derived from `:coverage`; `explosion`
  adds deterministic jitter + an irregular inner radius), `flash`
  (lighter-stroke radial burst), `speed` (parallel horizontal lines; optional
  `:direction` degrees). `:density` ≈ line count; unknown kinds are ignored.
- `:gaze` renders as a dashed red entry→focus→exit curve with an arrowhead,
  focus ring, and the `:impression` label — **only** when composing with the
  opt-in flag:

```clojure
(page/compose-page! page img-of "review.png" :gaze-overlay? true)
```

Without `:gaze-overlay? true` the output is byte-identical whether or not
`:gaze` is present.

## Known issue: `kami-mangaka-text-clj` dependency not yet split out

`deps.edn` depends on the shared multilingual lettering layer via
`{:local/root "../kami-mangaka-text-clj"}`. In the `kami-engine` monorepo that
sibling directory sat next to `kami-mangaka-page-clj/`; in this standalone repo
there is no sibling checkout, so `clojure -M:test` currently fails at classpath
resolution:

```
Error building classpath. Local lib gftd/kami-mangaka-text-clj not found: /private/tmp/kami-mangaka-text-clj
```

`kami-mangaka-text-clj` is still living inside `kotoba-lang/kami-engine` (not yet
split into its own repo) as of this migration. To run tests locally today,
clone `kotoba-lang/kami-engine` alongside this repo and symlink/copy its
`kami-mangaka-text-clj/` subtree to `../kami-mangaka-text-clj` relative to this
repo's root, or check out `kami-engine` at a path such that
`../kami-mangaka-text-clj` resolves correctly. Once `kami-mangaka-text-clj` gets
its own standalone repo (mirroring this split), `deps.edn` should be updated to
point at that repo via a `:git/url`+`:sha` (or `:local/root` pointing at a
sibling clone) instead of the current monorepo-relative path.

## Test

```bash
bb test     # templates / layout (splash·grid·ネーム rows) / headless compose-page!
```

(See the "Known issue" section above — this currently fails to build its
classpath until the `kami-mangaka-text-clj` dependency is resolved.)
