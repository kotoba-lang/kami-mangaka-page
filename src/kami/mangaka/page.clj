(ns kami.mangaka.page
  "Work-agnostic graphic-novel PAGE composition (ADR-2606282100, Tier-1 mangaka).

  Ported from mangaka.gftd.ai's `manga-layouts.ts` GRAPHIC_NOVEL_TEMPLATES
  (left-to-right reading): place rendered panel images into a B5 page, draw panel
  frames + gutters, overlay dialogue as speech bubbles and narration as caption
  boxes — turning isolated panels into a readable graphic-novel page.

  Generic: a `page` is just {:layout str :panels [{:id :size :narration :dialogue}…]}
  and `img-of` maps a panel-id → image File (or nil → placeholder). The text layer
  is the shared locale-keyed MangaText (kami.mangaka.text): `compose-page!` takes a
  `:locale`, so the baked page (EPUB/KDP/print) is multilingual from the same data
  the web reader uses — image stays language-neutral. No story, character, or world.
  JVM/Java2D headless — no Canvas-2D, no GPU (page DTP, not the wgpu render path)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [kami.mangaka.text :as t])
  (:import [java.awt Color Font BasicStroke RenderingHints GraphicsEnvironment
                     RadialGradientPaint GradientPaint]
           [java.awt.geom RoundRectangle2D$Double Point2D$Float Path2D$Double]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

;; --- graphic-novel layout templates (x y w h in %, LTR reading order) --------

(def gn-templates
  {1 [[0 0 100 100]]
   2 [[0 0 50 100] [50 0 50 100]]
   3 [[0 0 100 55] [0 55 50 45] [50 55 50 45]]
   4 [[0 0 50 50] [50 0 50 50] [0 50 50 50] [50 50 50 50]]
   5 [[0 0 100 40] [0 40 50 30] [50 40 50 30] [0 70 50 30] [50 70 50 30]]
   6 [[0 0 33.34 50] [33.34 0 33.33 50] [66.67 0 33.33 50]
      [0 50 33.34 50] [33.34 50 33.33 50] [66.67 50 33.33 50]]
   7 [[0 0 100 34] [0 34 33.34 33] [33.34 34 33.33 33] [66.67 34 33.33 33]
      [0 67 33.34 33] [33.34 67 33.33 33] [66.67 67 33.33 33]]
   8 [[0 0 25 50] [25 0 25 50] [50 0 25 50] [75 0 25 50]
      [0 50 25 50] [25 50 25 50] [50 50 25 50] [75 50 25 50]]
   9 [[0 0 33.34 33.34] [33.34 0 33.33 33.34] [66.67 0 33.33 33.34]
      [0 33.34 33.34 33.33] [33.34 33.34 33.33 33.33] [66.67 33.34 33.33 33.33]
      [0 66.67 33.34 33.33] [33.34 66.67 33.33 33.33] [66.67 66.67 33.33 33.33]]})

(defn- grid-template [n]
  (let [cols (long (Math/ceil (Math/sqrt n)))
        rows (long (Math/ceil (/ (double n) cols)))
        cw (/ 100.0 cols) ch (/ 100.0 rows)]
    (vec (for [i (range n) :let [r (quot i cols) c (mod i cols)]]
           [(* c cw) (* r ch) cw ch]))))

(defn template-for [n]
  (cond (<= n 0) []
        (get gn-templates n) (get gn-templates n)
        :else (grid-template n)))

;; --- komawari force-line tilt (ADR-2607051500, ported from ai-gftd-mangaka's
;; mangaka.layout.komawari — same design, this ecosystem's Java2D pixel space
;; instead of that one's normalized-EDN datom space) ---------------------------
;; A panel may opt into a dynamic diagonal border by carrying :tilt/:intensity/
;; :vector (mirroring komawari's :beat/* keys). Purely additive: a panel with
;; none of these renders exactly as before (axis-aligned rect, unchanged pixel
;; output) — see page_test.clj's pre-existing layout/compose assertions.

(def phi 1.618033988749895)

(def ^:private impact-tilt-max
  "The 1:φ rectangle's own diagonal angle (~31.7°) — same bound as
  mangaka.layout.komawari/impact-tilt-max, so both ecosystems agree on how
  steep a panel border is allowed to read."
  (Math/toDegrees (Math/atan (/ 1.0 phi))))
(def ^:private tension-tilt-max 10.0)

(defn komawari-tilt
  "An action-line angle (degrees, any range) + :calm/:tension/:impact →
  a signed shear angle within a legible bound, or nil for :calm. Identical
  folding/clamping to mangaka.layout.komawari/tilt-for."
  [vector-deg intensity]
  (when (and vector-deg (not= intensity :calm))
    (let [max-t (if (= intensity :impact) impact-tilt-max tension-tilt-max)
          folded (- (mod (+ vector-deg 90) 180) 90)]
      (double (max (- max-t) (min max-t folded))))))

(defn- row-tilt
  "The shared tilt for a row of panels: the first :impact panel's tilt, else
  the first :tension panel's, else nil — a row reads as ONE force-line."
  [row]
  (let [pick (or (first (filter #(= :impact (:intensity %)) row))
                 (first (filter #(= :tension (:intensity %)) row)))]
    (when pick (komawari-tilt (:vector pick) (:intensity pick)))))

;; --- ネーム-driven layout: vary panel size by the storyboard's intent --------
;; A uniform grid reads flat. Real pages breathe: hero bands, split rows, and
;; full-bleed splashes. We derive that from each panel's :size + the page layout,
;; so the composition follows the artist's ネーム, not a generic grid.

(defn- size-weight [s]
  (let [s (str/lower-case (str s))]
    (cond (re-find #"full" s)            2.6
          (re-find #"two-thirds|wide" s) 2.0
          (re-find #"half" s)            1.5
          (re-find #"one-third|narrow" s) 1.05
          :else 1.5)))

(defn- small? [s]
  (boolean (re-find #"one-third|narrow|half" (str/lower-case (str s)))))

(defn- rows-of
  "Pack panels into rows (reading order): two consecutive small panels share a
  row (side-by-side); everything else gets its own full-width band."
  [panels]
  (loop [ps panels rows []]
    (if (empty? ps)
      rows
      (let [a (first ps) b (second ps)]
        (if (and (small? (:size a)) b (small? (:size b)))
          (recur (drop 2 ps) (conj rows [a b]))
          (recur (rest ps) (conj rows [a])))))))

(defn layout-page
  "→ {:bleed bool :pairs [[panel [x y w h] tilt] ...]} in page-percent units,
  derived from the page :layout + each panel's :size. `tilt` (nil unless a
  panel opts into :intensity/:vector) is the row's shared komawari force-line
  angle — see `komawari-tilt` — nil for every panel unless a caller supplies
  those keys, so pre-existing callers see byte-identical `pairs` rects."
  [page]
  (let [ps (:panels page) n (count ps)
        lay (str/lower-case (str (:layout page)))]
    (cond
      ;; Authored Genko/storyboard geometry is authoritative. Values use the
      ;; portable normalized [x y w h] contract; convert to this compositor's
      ;; percent coordinate space without reflowing the page.
      (and (seq ps) (every? #(let [r (:rect %)]
                              (and (= 4 (count r)) (every? number? r))) ps))
      {:bleed false
       :pairs (mapv (fn [p]
                      (let [[x y w h] (:rect p)]
                        [p [(* 100.0 x) (* 100.0 y) (* 100.0 w) (* 100.0 h)] nil]))
                    ps)}

      (or (<= n 1) (re-find #"splash|full|spread" lay))
      {:bleed true :pairs (mapv #(conj % nil) (mapv vector ps [[0 0 100 100]]))}

      (re-find #"grid" lay)
      {:bleed false :pairs (mapv #(conj % nil) (mapv vector ps (template-for n)))}

      :else
      (let [rows    (rows-of ps)
            weights (mapv (fn [row] (reduce max (map #(size-weight (:size %)) row))) rows)
            total   (reduce + weights)
            pairs   (volatile! []) y (volatile! 0.0)]
        (doseq [[row w] (map vector rows weights)]
          (let [h (* 100.0 (/ w total)) k (count row) cw (/ 100.0 k)
                tilt (row-tilt row)]
            (doseq [[i p] (map-indexed vector row)]
              (vswap! pairs conj [p [(* i cw) @y cw h] tilt]))
            (vswap! y + h)))
        {:bleed false :pairs @pairs}))))

;; --- fonts (pick an installed JP-capable family) -----------------------------

(def ^:private jp-font-family
  (let [avail (set (.getAvailableFontFamilyNames
                    (GraphicsEnvironment/getLocalGraphicsEnvironment)))]
    (some avail ["Hiragino Maru Gothic ProN" "Hiragino Sans" "YuGothic"
                 "Yu Gothic" "Noto Sans CJK JP" "Noto Sans JP"])))

(defn- font [style size]
  (Font. (or jp-font-family Font/SANS_SERIF) style (int size)))

;; --- manga-expression → Java2D mapping (kami.mangaka.expression tags) ---------
;; 文字の薄さ (:weight) → 色/太さ, 文字の大きさ (:scale) → フォントサイズ,
;; 背景トーン (:tone) → コマ背景の効果, :nameplate/:chatter register → 専用描画。

(defn- weight->color [w]
  (case w
    :faint (Color. 140 140 140)   ; 薄いグレー (モブのざわめき)
    :light (Color. 85 85 85)
    Color/BLACK))

(defn- weight->style [w] (case w (:bold :heavy) Font/BOLD Font/PLAIN))

(defn- scaled [base scale] (int (Math/round (* (double base) (double (or scale 1.0))))))

(defn- tone-bg!
  "Draw a panel background TONE over the image (under the text): screentone /
  focus-lines / flash / vignette / crowd-silhouette — the 背景トーン device that
  keys mood to emotion (kami.mangaka.expression :tone)."
  [g tone x y w h]
  (when (and tone (not (#{:none :flat-white} tone)))
    (let [g2 (.create g)
          x (double x) y (double y) w (double w) h (double h)
          cx (+ x (/ w 2.0)) cy (+ y (/ h 2.0))]
      (try
        (.setClip g2 (int x) (int y) (int w) (int h))   ; g2 inherits g's AA (RenderingHints copied by .create)
        (case tone
          (:focus-lines :radial-burst)
          (do (.setColor g2 (Color. 0 0 0 150)) (.setStroke g2 (BasicStroke. 2.0))
              (doseq [a (range 0 360 6)]
                (let [r (Math/toRadians a)]
                  (.drawLine g2 (int cx) (int cy)
                             (int (+ cx (* (Math/cos r) w 1.3)))
                             (int (+ cy (* (Math/sin r) h 1.3)))))))
          :flash
          (do (.setColor g2 (Color. 0 0 0 205)) (.setStroke g2 (BasicStroke. 3.5))
              (doseq [a (range 0 360 13)]
                (let [r (Math/toRadians a)]
                  (.drawLine g2 (int cx) (int cy)
                             (int (+ cx (* (Math/cos r) w 1.3)))
                             (int (+ cy (* (Math/sin r) h 1.3)))))))
          :vignette-dark
          (do (.setPaint g2 (RadialGradientPaint.
                             (Point2D$Float. (float cx) (float cy)) (float (max w h))
                             (float-array [0.5 1.0])
                             (into-array Color [(Color. 0 0 0 0) (Color. 0 0 0 190)])))
              (.fillRect g2 (int x) (int y) (int w) (int h)))
          :gradient
          (do (.setPaint g2 (GradientPaint. (float x) (float y) (Color. 0 0 0 0)
                                            (float x) (float (+ y h)) (Color. 0 0 0 130)))
              (.fillRect g2 (int x) (int y) (int w) (int h)))
          :dot
          (do (.setColor g2 (Color. 0 0 0 55))
              (doseq [yy (range (int y) (int (+ y h)) 11) xx (range (int x) (int (+ x w)) 11)]
                (.fillOval g2 xx yy 3 3)))
          :hatching
          (do (.setColor g2 (Color. 0 0 0 75)) (.setStroke g2 (BasicStroke. 1.0))
              (doseq [d (range (int (- x h)) (int (+ x w)) 8)]
                (.drawLine g2 d (int y) (int (+ d h)) (int (+ y h)))))
          :crowd-silhouette
          (do (.setColor g2 (Color. 0 0 0 95))
              (doseq [i (range 0 (inc (int (/ w 40.0))))]
                (let [hx (+ x 8 (* i 40.0)) hy (+ y (* h 0.5))]
                  (.fillOval g2 (int hx) (int hy) 28 28)
                  (.fillRect g2 (int (- hx 5)) (int (+ hy 22)) 38 (int (max 0 (- (+ y h) (+ hy 22))))))))
          nil)
        (finally (.dispose g2))))))

(defn- nameplate!
  "キャラ名札 → 黒箱に白抜きゴシック, コマ左下 (HxH の従事兵ラベル)."
  [g text x y w h]
  (when (and text (seq (str text)))
    (.setFont g (font Font/BOLD 20))
    (let [fm (.getFontMetrics g) pad 8
          tw (.stringWidth fm (str text))
          bw (+ (* 2 pad) tw) bh (+ (* 2 pad) (.getHeight fm))
          bx (int (+ x 12)) by (int (- (+ y h) bh 12))]
      (doto g (.setColor Color/BLACK) (.fillRect bx by bw bh)
              (.setColor Color/WHITE))
      (.drawString g ^String (str text) (int (+ bx pad)) (int (+ by pad (.getAscent fm)))))))

(defn- chatter!
  "モブのざわめき → 薄いグレーの小さな独白, コマ上部に散らす."
  [g texts x y w]
  (doseq [[i s] (map-indexed vector (filter #(seq (str %)) texts))]
    (.setFont g (font Font/PLAIN 16))
    (.setColor g (Color. 140 140 140))
    (.drawString g ^String (str s)
                 (int (+ x 14 (* i (* w 0.24))))
                 (int (+ y 26 (* (mod i 2) 22))))))

;; --- text wrapping (CJK: break by char to fit width) -------------------------

(defn- wrap [g text max-w]
  (let [fm (.getFontMetrics g)]
    (loop [chars (seq (str text)) cur "" lines []]
      (if (empty? chars)
        (cond-> lines (seq cur) (conj cur))
        (let [c (first chars) cand (str cur c)]
          (if (and (seq cur) (> (.stringWidth fm cand) max-w))
            (recur chars "" (conj lines cur))
            (recur (rest chars) cand lines)))))))

;; --- drawing helpers ---------------------------------------------------------

(defn- aa! [g]
  (doto g
    (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)))

(defn- shear-polygon
  "[x y w h] + tilt degrees → a Path2D parallelogram shifting the TOP edge by
  dx = h·tan(tilt) — the same shear model as
  mangaka.layout.komawari/shear-polygon (ai-gftd-mangaka), here in Java2D
  pixel space instead of normalized page-space."
  ^Path2D$Double [x y w h tilt-deg]
  (let [dx (* h (Math/tan (Math/toRadians tilt-deg)))]
    (doto (Path2D$Double.)
      (.moveTo (+ x dx) y)
      (.lineTo (+ x w dx) y)
      (.lineTo (+ x w) (+ y h))
      (.lineTo x (+ y h))
      (.closePath))))

(defn- draw-cover
  "Draws `img` covering [x y w h]. An optional komawari `tilt` (degrees) clips
  to the sheared parallelogram instead of the axis-aligned rect — nil/absent
  renders identically to before."
  [g ^BufferedImage img x y w h & [tilt]]
  (let [g2 (.create g)]
    (try
      (if tilt
        (.setClip g2 (shear-polygon x y w h tilt))
        (.setClip g2 (int x) (int y) (int w) (int h)))
      (let [iw (.getWidth img) ih (.getHeight img)
            s (max (/ (double w) iw) (/ (double h) ih))
            sw (* iw s) sh (* ih s)
            dx (+ x (/ (- w sw) 2.0)) dy (+ y (/ (- h sh) 2.0))]
        (.drawImage g2 img (int dx) (int dy) (int sw) (int sh) nil))
      (finally (.dispose g2)))))

(defn- placeholder [g x y w h id & [tilt]]
  (.setColor g (Color. 233 227 207))
  (if tilt (.fill g (shear-polygon x y w h tilt)) (.fillRect g (int x) (int y) (int w) (int h)))
  (doto g (.setColor (Color. 150 150 140)) (.setFont (font Font/PLAIN 22)))
  (.drawString g (str id) (int (+ x 14)) (int (+ y 30))))

(defn- caption-box
  "Narration → a small caption box at the panel's top-left (serif, white). The
  optional style map carries :weight (薄さ→色/太さ) + :scale (大きさ→フォント)."
  [g text x y w & [{:keys [weight scale]}]]
  (when (and text (seq (str text)))
    (.setFont g (font (weight->style weight) (scaled 22 scale)))
    (let [pad 10 maxw (int (- (* w 0.62) (* 2 pad)))
          lines (wrap g text maxw)
          fm (.getFontMetrics g) lh (+ 4 (.getHeight fm))
          bw (+ (* 2 pad) (reduce max 1 (map #(.stringWidth fm %) lines)))
          bh (+ (* 2 pad) (* lh (count lines)))
          bx (+ x 10) by (+ y 10)]
      (doto g (.setColor (Color. 255 253 247 235))
              (.fillRect bx by bw bh)
              (.setColor (Color. 60 60 56)) (.setStroke (BasicStroke. 1.5))
              (.drawRect bx by bw bh))
      (.setColor g (if (#{:faint :light} weight) (weight->color weight) (Color. 40 40 38)))
      (doseq [[i ln] (map-indexed vector lines)]
        (.drawString g ^String ln (int (+ bx pad)) (int (+ by pad (* (inc i) lh) -6)))))))

(defn- bubble
  "Dialogue → a white rounded speech bubble with a tail + black outline + JP text.
  `cx` is the desired bubble centre; `side` (:l/:r) aims the tail. The optional
  style map carries :weight (薄さ→色/太さ), :scale (大きさ→フォント), and :shape
  (:spike/:jagged/:burst の叫び系は太い輪郭で強調 — full clip-path は web reader 側).
  Returns bottom-y."
  [g text cx top w side & [{:keys [weight scale shape writing-mode columns
                                   width-ratio height-ratio panel-height tail-target]}]]
  (when (and text (seq (str text)))
    (.setFont g (font (weight->style weight) (scaled 25 scale)))
    (let [vertical? (= writing-mode "vertical-rtl")
          shout? (boolean (#{:spike :jagged :burst} shape))
          pad 17 maxw (int (* w (double (or width-ratio 0.46))))
          lines (if vertical?
                  (or (seq columns) (mapv str (seq (str text))))
                  (wrap g text maxw))
          fm (.getFontMetrics g) lh (+ 6 (.getHeight fm))
          tw (reduce max 1 (map #(.stringWidth fm %) lines))
          bw (if width-ratio (max (+ (* 2 pad) tw) (* w width-ratio)) (+ (* 2 pad) tw))
          bh (if height-ratio (max (+ (* 2 pad) lh) (* (or panel-height w) height-ratio))
                 (+ (* 2 pad) (* lh (count lines))))
          bx (- cx (/ bw 2.0)) by top
          rad (if shout? 8 38)
          rr (RoundRectangle2D$Double. bx by bw bh rad rad)
          ;; tail: a small triangle dropping from the bubble toward the speaker
          tailx (if (= side :r) (- (+ bx bw) (* bw 0.28)) (+ bx (* bw 0.16)))
          [target-x target-y] (or tail-target
                                  [(+ tailx (if (= side :r) -2 28)) (+ by bh 24)])
          txs (int-array [(int tailx) (int (+ tailx 26)) (int target-x)])
          tys (int-array [(int (+ by bh -4)) (int (+ by bh -4)) (int target-y)])]
      (doto g (.setColor Color/WHITE) (.fillPolygon txs tys 3) (.fill rr)
              (.setColor Color/BLACK) (.setStroke (BasicStroke. (float (if shout? 5.0 3.0))))
              (.drawPolygon txs tys 3) (.draw rr)
              ;; paint over the seam where the tail meets the bubble
              (.setColor Color/WHITE) (.fillRect (int (+ tailx 2)) (int (+ by bh -7)) 22 6))
      (.setColor g (weight->color weight))
      (if vertical?
        (let [chars (seq (str text)) rows (max 1 (int (/ (- bh (* 2 pad)) lh)))]
          (doseq [[i ch] (map-indexed vector chars)]
            (let [col (quot i rows) row (mod i rows)]
              (.drawString g (str ch)
                           (int (- (+ bx bw (- pad)) (* col lh)))
                           (int (+ by pad (* (inc row) lh) -8))))))
        (doseq [[i ln] (map-indexed vector lines)]
          (.drawString g ^String ln
                       (int (- cx (/ (.stringWidth fm ln) 2.0)))
                       (int (+ by pad (* (inc i) lh) -8)))))
      (+ by bh 30))))

(defn- draw-sfx
  "SFX (擬音) → bold white text with a black stroke, tilted, upper-right of the
  panel. The locale-resolved string (e.g. ちゃぷ / lap…)."
  [g text x y w & [scale]]
  (when (and text (seq (str text)))
    (let [g2 (.create g)]
      (try
        (let [fsz (int (max 30 (* w 0.16 (double (or scale 1.0)))))
              sx  (int (+ x (* w 0.5))) sy (int (+ y (* w 0.3)))]
          (.setFont g2 (font Font/BOLD fsz))
          (.rotate g2 (Math/toRadians -8) sx sy)
          (.setColor g2 Color/BLACK)
          (doseq [dx [-3 -2 2 3] dy [-3 -2 2 3]]
            (.drawString g2 ^String (str text) (+ sx (int dx)) (+ sy (int dy))))
          (.setColor g2 Color/WHITE)
          (.drawString g2 ^String (str text) sx sy))
        (finally (.dispose g2))))))

;; --- pre-flight sizing (public): compute a dialogue/SFX box BEFORE calling
;; compose-page!, so callers (genko authoring, generation pipelines) don't
;; have to guess :width/:height fractions and re-render to find out they were
;; wrong. `bubble`'s vertical-text loop places character i at
;; col=(quot i rows), row=(mod i rows) and does NOT clip or grow the bubble
;; to fit — a box sized too small silently draws the tail characters past the
;; bubble's own left edge (they land on the panel art, not "dropped" but
;; effectively illegible). These functions reuse the *exact* same formulas
;; `bubble` draws with (not an approximation of them), so a box this reports
;; as fitting is guaranteed to fit when compose-page! actually draws it.

(def ^:private metrics-scratch
  (delay (.createGraphics (BufferedImage. 8 8 BufferedImage/TYPE_INT_RGB))))

(defn line-height-for
  "Pixel line-height (character cell height, vertical writing) for a given
  :scale/:weight — mirrors `bubble`'s `lh` exactly (font size = (scaled 25
  scale), lh = 6 + FontMetrics.getHeight)."
  [{:keys [scale weight]}]
  (let [f (font (weight->style weight) (scaled 25 scale))
        fm (.getFontMetrics ^java.awt.Graphics2D @metrics-scratch f)]
    (+ 6 (.getHeight fm))))

(defn- vertical-rows-for
  "Same `rows` formula as `bubble`'s vertical-text branch: how many
  characters fit down one column at height `bh`."
  [bh lh]
  (max 1 (int (/ (- bh (* 2 17)) lh))))

(defn- vertical-cols-needed
  "The column index of the LAST character (0-based) + 1, replaying `bubble`'s
  own `col = (quot i rows)` assignment for i in [0, n) -- i.e. exactly how
  many columns `bubble` will actually use for `n` characters at `rows` rows
  per column (not a separate ceil-based estimate of it)."
  [n rows]
  (inc (quot (dec (max 1 n)) rows)))

(defn fit-vertical-dialogue
  "Pre-flight box size for one `writing-mode \"vertical-rtl\"` dialogue
  string, given the panel's actual pixel `:panel-w`/`:panel-h` (from
  `layout-page`, NOT guessed) and the same :scale/:weight the caller will
  pass to `bubble` via the panel's :dialogue map. Returns
  {:width :height :cols :rows :fits?} — guaranteed (by replaying `bubble`'s
  own placement formula, not a separate approximation of it) to hold every
  character inside the drawn bubble.

  `:target-height` (default 0.5) is the caller's DESIRED height (an
  aesthetic choice — short for a one-word interjection near a face, tall for
  a paragraph of narration): it is used as-is whenever the resulting box
  already fits `:max-width` (default 0.94) at that height. Only when the
  text is too wide at :target-height does this grow the height (taller
  column = fewer columns = narrower) up to `:max-height` (default 0.94)
  looking for a box that fits; if even :max-height doesn't fit :max-width,
  returns :fits? false at the caps — the caller should shrink :scale and
  retry rather than ship a box that silently draws characters past its own
  left edge (see the `bubble` docstring: it does not clip or grow itself)."
  [text {:keys [scale weight panel-w panel-h max-width max-height target-height]
         :or {max-width 0.94 max-height 0.94 target-height 0.5}}]
  (let [n (count (str text))
        lh (line-height-for {:scale scale :weight weight})
        pad 17
        max-rows (vertical-rows-for (* panel-h max-height) lh)
        start-rows (max 1 (min max-rows (vertical-rows-for (* panel-h target-height) lh)))
        box-at (fn [rows]
                 (let [cols (vertical-cols-needed n rows)
                       bw (+ (* 2 pad) (* cols lh))
                       bh (+ (* 2 pad) (* rows lh))]
                   {:width (/ bw panel-w) :height (max target-height (/ bh panel-h))
                    :cols cols :rows rows}))]
    (loop [rows start-rows]
      (let [{:keys [width] :as box} (box-at rows)]
        (cond
          (<= width max-width) (assoc box :fits? true)
          (>= rows max-rows) (assoc (box-at max-rows) :width max-width :fits? false)
          :else (recur (inc rows)))))))

(defn fit-sfx
  "Pre-flight check for `draw-sfx` (single-line, not column-wrapped): the
  font size `draw-sfx` will actually use, and whether the rendered string
  width fits within `:max-width` (default 0.9) of the panel. `:fits? false`
  means the caller should lower :scale — draw-sfx does not wrap or shrink
  SFX text itself."
  [text {:keys [scale panel-w] :or {scale 1.0}}]
  (let [fsz (int (max 30 (* panel-w 0.16 (double scale))))
        f (font Font/BOLD fsz)
        fm (.getFontMetrics ^java.awt.Graphics2D @metrics-scratch f)
        tw (.stringWidth fm (str text))]
    {:font-size fsz :text-width tw :fits? (<= tw (* panel-w 0.9))}))

;; --- 効果線 (effectLines) + 視線誘導 (gaze) — ai.gftd.mangaka page lexicon ----
;; Both fields live on the panel in the lexicon's panel-local 0-1000 coordinate
;; space (both axes, independent of the panel's pixel size): {:centerX 480
;; :centerY 450} means 48% across / 45% down the panel rect. Z-order per the
;; lexicon: panel art → tones → effectLines → SFX → bubbles.

(defn- fx->px
  "Panel-local 0-1000 coordinate `v` → a pixel position along [origin origin+extent]
  (nil → 500 = panel centre)."
  ^double [v origin extent]
  (+ (double origin) (* (double extent) (/ (double (or v 500)) 1000.0))))

(defn- draw-effect-lines!
  "Draw 効果線 (effect lines) over the panel rect [x y w h] — the lexicon panel
  field `effectLines`: [{:kind :centerX :centerY :density :coverage}…] in
  panel-local 0-1000 coords. Kinds:
    :focus / :explosion — radial black lines from the panel border toward the
      centre, leaving an inner clear radius derived from :coverage (85 → lines
      cover the outer 85% of the radius); :explosion adds per-line angle jitter
      + an irregular inner radius (seeded, so bakes stay deterministic).
    :flash — lighter-stroke radial burst (alternating inner reach = フラッシュ).
    :speed — parallel horizontal lines (optional :direction degrees rotates them).
  :density ≈ line count. Unknown kinds are ignored. Clipped to the panel rect;
  drawn after the tone layer and before SFX/bubbles (lexicon z-order)."
  [g lines x y w h]
  (doseq [{:keys [kind centerX centerY density coverage direction]} lines]
    (let [g2 (.create g)]
      (try
        (.setClip g2 (int x) (int y) (int w) (int h))
        (let [kind (keyword (str/lower-case (name (or kind :focus))))
              n    (int (min 240 (max 4 (long (if (number? density) density 32)))))
              cov  (double (if (number? coverage) coverage 70))
              cx   (fx->px centerX x w)
              cy   (fx->px centerY y h)
              ;; far enough to reach the panel's farthest corner from the centre
              rmax (Math/hypot (max (- cx x) (- (+ x w) cx))
                               (max (- cy y) (- (+ y h) cy)))
              r0   (* rmax (max 0.02 (- 1.0 (/ cov 100.0))))
              rnd  (java.util.Random. (long (+ n (* 31 (long cx)) (* 131 (long cy)))))]
          (case kind
            (:focus :explosion)
            (let [expl? (= kind :explosion)]
              (.setColor g2 (Color. 0 0 0 (if expl? 225 195)))
              (doseq [i (range n)]
                (let [base (* 2.0 Math/PI (/ (double i) n))
                      a    (if expl? (+ base (* 0.12 (.nextGaussian rnd))) base)
                      ri   (if expl? (* r0 (+ 0.55 (* 0.9 (.nextDouble rnd)))) r0)]
                  (.setStroke g2 (BasicStroke. (float (if expl?
                                                        (+ 1.5 (* 2.5 (.nextDouble rnd)))
                                                        2.0))))
                  (.drawLine g2 (int (+ cx (* ri (Math/cos a)))) (int (+ cy (* ri (Math/sin a))))
                             (int (+ cx (* rmax (Math/cos a)))) (int (+ cy (* rmax (Math/sin a))))))))
            :flash
            (do (.setColor g2 (Color. 0 0 0 130))
                (.setStroke g2 (BasicStroke. 1.2))
                (doseq [i (range n)]
                  (let [a  (* 2.0 Math/PI (/ (double i) n))
                        ri (* r0 (if (even? i) 1.0 1.4))]
                    (.drawLine g2 (int (+ cx (* ri (Math/cos a)))) (int (+ cy (* ri (Math/sin a))))
                               (int (+ cx (* rmax (Math/cos a)))) (int (+ cy (* rmax (Math/sin a))))))))
            :speed
            (let [ang  (Math/toRadians (double (if (number? direction) direction 0)))
                  mx   (+ x (/ w 2.0)) my (+ y (/ h 2.0))
                  span (Math/hypot w h)
                  x0   (- mx (/ span 2.0))]
              (.rotate g2 ang mx my)
              (.setColor g2 (Color. 0 0 0 175))
              (doseq [i (range n)]
                (let [yy (+ y (* h (/ (+ i 0.5) (double n))) (* 2.0 (.nextGaussian rnd)))
                      st (+ x0 (* span 0.35 (.nextDouble rnd)))
                      en (+ st (* span (+ 0.35 (* 0.45 (.nextDouble rnd)))))]
                  (.setStroke g2 (BasicStroke. (float (+ 0.8 (* 1.6 (.nextDouble rnd))))))
                  (.drawLine g2 (int st) (int yy) (int en) (int yy)))))
            nil))
        (finally (.dispose g2))))))

(defn- draw-gaze-overlay!
  "視線誘導 (gaze guidance) REVIEW overlay — NOT print content: only drawn when
  `compose-page!` is called with :gaze-overlay? true. `gaze` is the lexicon
  panel field {:entryX :entryY :focusX :focusY :exitX :exitY :impression} in
  panel-local 0-1000 coords: a dashed red entry→focus→exit curve (a quadratic
  passing through the focus point) with an entry dot, a focus ring, an
  arrowhead at the exit, and the :impression label near the entry."
  [g gaze x y w h]
  (when (map? gaze)
    (let [g2 (.create g)]
      (try
        (.setClip g2 (int x) (int y) (int w) (int h))
        (let [ex (fx->px (:entryX gaze) x w) ey (fx->px (:entryY gaze) y h)
              fx (fx->px (:focusX gaze) x w) fy (fx->px (:focusY gaze) y h)
              gx (fx->px (:exitX gaze) x w)  gy (fx->px (:exitY gaze) y h)
              ;; control point so the quadratic passes through focus at t=0.5
              qx (- (* 2.0 fx) (/ (+ ex gx) 2.0))
              qy (- (* 2.0 fy) (/ (+ ey gy) 2.0))
              red (Color. 220 30 30 235)]
          (.setColor g2 red)
          (.setStroke g2 (BasicStroke. 3.0 BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND
                                       10.0 (float-array [12.0 9.0]) 0.0))
          (.draw g2 (doto (Path2D$Double.) (.moveTo ex ey) (.quadTo qx qy gx gy)))
          (.fillOval g2 (int (- ex 5)) (int (- ey 5)) 10 10)          ; entry dot
          (.setStroke g2 (BasicStroke. 2.5))
          (.drawOval g2 (int (- fx 11)) (int (- fy 11)) 22 22)        ; focus ring
          ;; arrowhead at the exit along the curve's end tangent (exit − control)
          (let [vx (- gx qx) vy (- gy qy)
                m  (max 1.0E-6 (Math/hypot vx vy))
                ux (/ vx m) uy (/ vy m) len 18.0
                rot (fn [th] [(- (* (- ux) (Math/cos th)) (* (- uy) (Math/sin th)))
                              (+ (* (- ux) (Math/sin th)) (* (- uy) (Math/cos th)))])
                [lx ly] (rot 0.45) [rx ry] (rot -0.45)]
            (.setStroke g2 (BasicStroke. 3.0 BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
            (.drawLine g2 (int gx) (int gy) (int (+ gx (* len lx))) (int (+ gy (* len ly))))
            (.drawLine g2 (int gx) (int gy) (int (+ gx (* len rx))) (int (+ gy (* len ry)))))
          (when-let [imp (:impression gaze)]
            (.setFont g2 (font Font/PLAIN 16))
            (.drawString g2 ^String (str imp) (int (+ ex 8)) (int (- ey 8)))))
        (finally (.dispose g2))))))

;; --- page composition --------------------------------------------------------

(def PAGE-W 1075) (def PAGE-H 1518)   ; B5 @ ~150dpi
(def MARGIN 38) (def GUTTER 16)

(defn panel-pixel-sizes
  "The actual pixel {:id :w :h} compose-page! will draw each panel at, for
  this `page` ({:layout :panels […]}) — i.e. `layout-page` run through the
  same page-minus-margins math `compose-page!` itself uses. Callers use this
  BEFORE compose-page! to size dialogue/SFX (see `fit-vertical-dialogue`/
  `fit-sfx`) against real panel dimensions instead of guessing them."
  [page]
  (let [{:keys [bleed pairs]} (layout-page page)
        m (if bleed 0 MARGIN) gut (if bleed 0 GUTTER)
        cw (- PAGE-W (* 2 m)) ch (- PAGE-H (* 2 m))]
    (mapv (fn [[panel [_px _py pw ph] _tilt]]
            {:id (:id panel)
             :w (- (* cw (/ pw 100.0)) gut)
             :h (- (* ch (/ ph 100.0)) gut)})
          pairs)))

(defn compose-page!
  "Compose one storyboard `page` ({:layout :panels […]}) into a B5 PNG at `out`.
  `img-of` maps a panel-id → image File (or nil → placeholder). The text layer
  (narration caption + dialogue bubbles + SFX) is derived from each panel via
  kami.mangaka.text and rendered in `:locale` (default :ja) — the image is the
  same regardless of language.

  A panel may carry the lexicon fields `:effectLines` (効果線, baked into the
  print image between the tone layer and the lettering) and `:gaze` (視線誘導,
  review-only: drawn as a dashed red overlay ONLY when called with
  `:gaze-overlay? true` — never part of print output). Both use the panel-local
  0-1000 coordinate space (see `draw-effect-lines!` / `draw-gaze-overlay!`)."
  [page img-of out & {:keys [locale gaze-overlay?] :or {locale :ja}}]
  (let [{:keys [bleed pairs]} (layout-page page)
        canvas (BufferedImage. PAGE-W PAGE-H BufferedImage/TYPE_INT_RGB)
        g (.createGraphics canvas)
        ;; a full-bleed splash uses the whole sheet; otherwise inset by MARGIN
        m  (if bleed 0 MARGIN)
        gut (if bleed 0 GUTTER)
        cx0 m cy0 m cw (- PAGE-W (* 2 m)) ch (- PAGE-H (* 2 m))]
    (aa! g)
    (.setColor g (Color. 22 20 18)) (.fillRect g 0 0 PAGE-W PAGE-H) ; ink page base = crisp gutters
    (doseq [[idx [panel [px py pw ph] tilt]] (map-indexed vector pairs)]
      (let [x (+ cx0 (* cw (/ px 100.0)) (/ gut 2.0))
            y (+ cy0 (* ch (/ py 100.0)) (/ gut 2.0))
            w (- (* cw (/ pw 100.0)) gut)
            h (- (* ch (/ ph 100.0)) gut)
            f (img-of (:id panel))]
        (if (and f (.exists ^File f))
          (draw-cover g (ImageIO/read ^File f) x y w h tilt)
          (placeholder g x y w h (:id panel) tilt))
        ;; 背景トーン (kami.mangaka.expression :tone) — 画像の上, フレーム/文字の下
        ;; (rect-clipped even under a komawari tilt — a sheared tone overlay is
        ;; a future refinement, not needed to demonstrate the force-line frame)
        (tone-bg! g (:tone panel) x y w h)
        ;; 効果線 (lexicon z-order: panel art → tones → effectLines → SFX → bubbles)
        (when-let [fx (seq (or (:effectLines panel) (:effect-lines panel)))]
          (draw-effect-lines! g fx x y w h))
        ;; confident black frame (heavier on a bleed splash) — a komawari force-line
        ;; row draws the sheared parallelogram, so the frame itself carries the angle
        (.setColor g Color/BLACK) (.setStroke g (BasicStroke. (float (if bleed 10 5))))
        (if tilt
          (.draw g (shear-polygon x y w h tilt))
          (.drawRect g (int x) (int y) (int w) (int h)))
        ;; text layer (shared, locale-keyed): nameplate + chatter + caption + bubbles + SFX
        (let [els     (t/panel->elements panel)
              narr-el (some #(when (= :narration (:kind %)) %) els)
              dlgs    (if (seq (:dialogue panel))
                        (map #(assoc % :kind :dialogue) (:dialogue panel))
                        (filter #(= :dialogue (:kind %)) els))
              sfxs    (filter #(= :sfx (:kind %)) els)
              nps     (filter #(= :nameplate (:kind %)) els)
              chat    (filter #(= :chatter (:kind %)) els)]
          (when (seq chat)
            (chatter! g (map #(t/localize (:text %) locale) chat) x y w))
          (when narr-el
            (caption-box g (t/localize (:text narr-el) locale) x y w
                         {:weight (:weight narr-el) :scale (:scale narr-el)}))
          ;; alternate bubble side per panel for rhythm; keep inside the panel
          (let [side (if (even? idx) :l :r)
                cx (+ x (* w (if (= side :l) 0.42 0.58)))]
            (loop [ds dlgs top (+ y 22)]
              (when (seq ds)
                (let [d (first ds)
                      [rx ry] (:pos d)
                      dcx (if (number? rx) (+ x (* w rx)) cx)
                      dtop (if (number? ry) (+ y (* h ry)) top)
                      [tx ty] (:tail-target d)
                      target (when (and (number? tx) (number? ty))
                               [(+ x (* w tx)) (+ y (* h ty))])
                      nt (bubble g (t/localize (:text d) locale) dcx dtop w side
                                 {:weight (:weight d) :scale (:scale d) :shape (:bubble d)
                                  :writing-mode (:writing-mode d) :columns (:columns d)
                                  :width-ratio (:width d) :height-ratio (:height d)
                                  :panel-height h
                                  :tail-target target})]
                  (recur (rest ds) (or nt (+ top 96)))))))
          (doseq [s sfxs] (draw-sfx g (t/localize (:text s) locale) x y w (:scale s)))
          (doseq [np nps] (nameplate! g (t/localize (:text np) locale) x y w h)))
        ;; 視線誘導 review overlay — opt-in only, on top of everything in the panel
        (when gaze-overlay?
          (draw-gaze-overlay! g (:gaze panel) x y w h))))
    (.dispose g)
    (io/make-parents out)
    (ImageIO/write canvas "png" (File. ^String out))
    out))
