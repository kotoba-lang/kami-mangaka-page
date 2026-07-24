(ns kami.mangaka.page-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [kami.mangaka.page :as page])
  (:import [javax.imageio ImageIO]))

(defn- bake-bytes
  "Compose `pg` to a temp PNG named `nm` (with `opts` kv-args) → its bytes."
  [pg nm & opts]
  (let [out (str (System/getProperty "java.io.tmpdir") "/" nm)]
    (io/delete-file out true)
    (apply page/compose-page! pg (constantly nil) out opts)
    (java.nio.file.Files/readAllBytes (.toPath (io/file out)))))

(deftest template-for-test
  (is (= [] (page/template-for 0)))
  (is (= [[0 0 100 100]] (page/template-for 1)))
  (is (= 4 (count (page/template-for 4))))
  (testing "n beyond the preset table falls back to a square-ish grid"
    (is (= 10 (count (page/template-for 10))))))

(deftest layout-page-splash-is-full-bleed
  (let [{:keys [bleed pairs]} (page/layout-page {:layout "splash"
                                                 :panels [{:id "a" :size "full-page"}]})]
    (is (true? bleed))
    (is (= [[0 0 100 100]] (mapv second pairs)))))

(deftest layout-page-grid
  (let [{:keys [bleed pairs]} (page/layout-page
                               {:layout "4-panel-grid"
                                :panels (mapv #(hash-map :id % :size "half") (range 4))})]
    (is (false? bleed))
    (is (= 4 (count pairs)))))

(deftest layout-page-name-driven-rows
  (testing "two consecutive narrow panels share a row; wide gets its own band"
    (let [{:keys [pairs]} (page/layout-page
                           {:layout "vertical"
                            :panels [{:id "a" :size "narrow"} {:id "b" :size "narrow"}
                                     {:id "c" :size "wide"}]})
          ys (mapv (fn [[_ [_ y _ _]]] y) pairs)]
      ;; a & b on the same row (same y), c on a later row (greater y)
      (is (= (nth ys 0) (nth ys 1)))
      (is (> (nth ys 2) (nth ys 0))))))

(deftest layout-page-komawari-tilt-is-additive
  (testing "no panel opts into :intensity/:vector → every tilt is nil (unchanged output)"
    (let [{:keys [pairs]} (page/layout-page
                           {:layout "vertical"
                            :panels [{:id "a" :size "narrow"} {:id "b" :size "narrow"}
                                     {:id "c" :size "wide"}]})]
      (is (every? nil? (map #(nth % 2) pairs)))))
  (testing "an :impact panel shares its tilt with the whole row (force-line, ADR-2607051500)"
    (let [{:keys [pairs]} (page/layout-page
                           {:layout "vertical"
                            :panels [{:id "a" :size "narrow" :intensity :impact :vector 20}
                                     {:id "b" :size "narrow"}]})
          tilts (map #(nth % 2) pairs)]
      (is (apply = tilts) "both panels in the row share one tilt")
      (is (some? (first tilts)))))
  (testing "tilt is clamped to the 1:φ diagonal for :impact, and never fires for :calm"
    (is (nil? (page/komawari-tilt 45 :calm)))
    (is (<= (Math/abs (page/komawari-tilt 89 :impact)) 32.0))))

(deftest compose-page!-with-force-line-tilt
  (testing "a tilted row bakes without error (sheared clip/frame code path)"
    (let [out (str (System/getProperty "java.io.tmpdir") "/mangaka-komawari-tilt.png")]
      (io/delete-file out true)
      (is (= out (page/compose-page!
                  {:layout "vertical"
                   :panels [{:id "a" :size "narrow" :intensity :impact :vector 25
                             :dialogue [{:text "ZBAAAN!!"}]}
                            {:id "b" :size "narrow"}]}
                  (constantly nil) out)))
      (is (.exists (io/file out))))))

(deftest compose-page!-multilingual-bake
  (testing "bake consumes the shared MangaText layer + renders the chosen locale"
    (let [page {:layout "splash"
                :panels [{:id "p" :size "full-page"
                          :narration {:ja "今朝も、運河は青い。" :en "Canal's blue again."}
                          :dialogue [{:speaker "tamaki" :text {:ja "やあ。" :en "Hi."}}]
                          :gh/sfx [{:text {:ja "ちゃぷ" :en "lap"}}]}]}
          ja (str (System/getProperty "java.io.tmpdir") "/mk-bake-ja.png")
          en (str (System/getProperty "java.io.tmpdir") "/mk-bake-en.png")]
      (io/delete-file ja true) (io/delete-file en true)
      (is (= ja (page/compose-page! page (constantly nil) ja :locale :ja)))
      (is (= en (page/compose-page! page (constantly nil) en :locale :en)))
      (is (.exists (io/file ja))) (is (.exists (io/file en)))
      (testing "same image source, different lettering → different PNG bytes"
        (is (not= (.length (io/file ja)) (.length (io/file en))))))))

(deftest compose-page!-expression-bake
  (testing "背景トーン・薄さ・大きさ・名札・ざわめき を焼き込む (全 tone の描画経路が例外なく走る)"
    (let [panels (for [[i tone] (map-indexed vector
                                             [:focus-lines :flash :vignette-dark :gradient
                                              :dot :hatching :crowd-silhouette])]
                   {:id (str "p" i) :size "half" :tone tone
                    :nameplate (str "第" i "王子親衛兵")
                    :dialogue [{:text "バカな！！" :bubble :spike :weight :heavy :scale 1.6}]
                    :chatter ["ざわ" "ざわ"]})
          out (str (System/getProperty "java.io.tmpdir") "/mangaka-expr-bake.png")]
      (io/delete-file out true)
      (is (= out (page/compose-page! {:layout "grid" :panels (vec panels)}
                                     (constantly nil) out)))
      (is (.exists (io/file out)))
      (is (pos? (.length (io/file out)))))))

(deftest compose-page!-effect-lines-bake
  (testing "every effectLines kind bakes without error (focus/explosion/flash/speed)"
    (let [panels [{:id "p0" :size "half"
                   :effectLines [{:kind "focus" :centerX 480 :centerY 450 :density 44 :coverage 85}]}
                  {:id "p1" :size "half"
                   :effectLines [{:kind "explosion" :centerX 500 :centerY 500 :density 40 :coverage 80}]}
                  {:id "p2" :size "half"
                   :effectLines [{:kind "flash" :centerX 500 :centerY 350 :density 62 :coverage 85}]}
                  {:id "p3" :size "half"
                   :effectLines [{:kind "speed" :direction 0 :density 30}]}]
          out (str (System/getProperty "java.io.tmpdir") "/mangaka-effect-lines.png")]
      (io/delete-file out true)
      (is (= out (page/compose-page! {:layout "grid" :panels panels} (constantly nil) out)))
      (is (.exists (io/file out)))
      (is (pos? (.length (io/file out)))))))

(deftest compose-page!-focus-lines-pixels
  (testing "focus lines darken toward the panel border, leave the coverage-cleared centre clean"
    (let [out (str (System/getProperty "java.io.tmpdir") "/mangaka-focus-pixels.png")
          _   (io/delete-file out true)
          _   (page/compose-page!
               {:layout "splash"
                :panels [{:id "p" :size "full-page"
                          :effectLines [{:kind "focus" :centerX 500 :centerY 500
                                         :density 60 :coverage 85}]}]}
               (constantly nil) out)
          img (ImageIO/read (io/file out))
          dark? (fn [rgb]
                  (let [r (bit-and (bit-shift-right rgb 16) 0xFF)
                        g (bit-and (bit-shift-right rgb 8) 0xFF)
                        b (bit-and rgb 0xFF)]
                    (< (/ (+ r g b) 3.0) 110)))
          cx (quot (.getWidth img) 2) cy (quot (.getHeight img) 2)
          count-dark (fn [x0 x1 y0 y1]
                       (count (filter dark? (for [xx (range x0 x1) yy (range y0 y1)]
                                              (.getRGB img xx yy)))))
          centre (count-dark (- cx 50) (+ cx 50) (- cy 50) (+ cy 50))
          edge   (count-dark 60 260 (- cy 50) (+ cy 50))]
      (is (zero? centre) "the coverage-derived inner radius stays clear of lines")
      (is (> edge 20) "radial lines are present near the panel border"))))

(deftest compose-page!-effect-lines-nil-guard
  (testing "a page with nil/empty effectLines renders byte-identically to one without the key"
    (let [base   {:layout "grid" :panels [{:id "a" :size "half"} {:id "b" :size "half"}]}
          plain  (bake-bytes base "mk-fx-base.png")
          w-nil  (bake-bytes (assoc-in base [:panels 0 :effectLines] nil) "mk-fx-nil.png")
          w-empt (bake-bytes (assoc-in base [:panels 0 :effectLines] []) "mk-fx-empty.png")]
      (is (= (seq plain) (seq w-nil) (seq w-empt))))))

(deftest compose-page!-gaze-overlay-opt-in
  (testing "gaze is a review overlay: absent from output unless :gaze-overlay? true"
    (let [gaze {:entryX 850 :entryY 150 :focusX 480 :focusY 450
                :exitX 200 :exitY 700 :impression "dread"}
          pg   {:layout "splash"
                :panels [{:id "p" :size "full-page" :gaze gaze}]}
          no-key  (bake-bytes (update-in pg [:panels 0] dissoc :gaze) "mk-gaze-nokey.png")
          plain   (bake-bytes pg "mk-gaze-plain.png")
          overlay (bake-bytes pg "mk-gaze-overlay.png" :gaze-overlay? true)]
      (is (= (seq no-key) (seq plain)) "without the option, gaze data changes nothing")
      (is (not= (seq plain) (seq overlay)) "with :gaze-overlay? true the overlay is drawn")
      (testing "the overlay actually draws red strokes near the focus point"
        (let [img (ImageIO/read (io/file (str (System/getProperty "java.io.tmpdir")
                                              "/mk-gaze-overlay.png")))
              red? (fn [rgb]
                     (let [r (bit-and (bit-shift-right rgb 16) 0xFF)
                           g (bit-and (bit-shift-right rgb 8) 0xFF)
                           b (bit-and rgb 0xFF)]
                       (and (> r 170) (< g 110) (< b 110))))
              ;; focus (480,450)/1000 of the full-bleed B5 page
              fx (int (* (.getWidth img) 0.48)) fy (int (* (.getHeight img) 0.45))]
          (is (some red? (for [xx (range (- fx 100) (+ fx 100))
                               yy (range (- fy 100) (+ fy 100))]
                           (.getRGB img xx yy)))))))))

(deftest compose-page!-writes-a-png
  (testing "headless Java2D composes a page (placeholders, no source images)"
    (let [out (str (System/getProperty "java.io.tmpdir") "/mangaka-page-test.png")
          _   (io/delete-file out true)
          r   (page/compose-page! {:layout "3-panel-vertical"
                                   :panels [{:id "p1" :size "wide" :narration "今朝も、運河は青い。"}
                                            {:id "p2" :size "narrow"
                                             :dialogue [{:text "よし。行こう。"}]}
                                            {:id "p3" :size "narrow"}]}
                                  (constantly nil)            ; no images → placeholders
                                  out)]
      (is (= out r))
      (is (.exists (io/file out)))
      (is (pos? (.length (io/file out)))))))

(deftest authored-rects-are-authoritative
  (let [pg {:layout "grid"
            :panels [{:id "right" :rect [0.55 0.05 0.4 0.35]}
                     {:id "left" :rect [0.05 0.05 0.45 0.35]}]}
        pairs (:pairs (page/layout-page pg))]
    (is (every? #(< (Math/abs %) 1.0e-9)
                (map -
                     (mapcat identity [[55.0 5.0 40.0 35.0] [5.0 5.0 45.0 35.0]])
                     (mapcat second pairs))))
    (is (every? nil? (map #(nth % 2) pairs)))))

;; --- fit-vertical-dialogue / fit-sfx --------------------------------------
;; A box `bubble` will draw MUST hold every character inside itself: `bubble`
;; places char i at col=(quot i rows) and never clips or grows the box, so a
;; too-small box silently draws tail characters past the bubble's own left
;; edge (illegible against the panel art, not a crash). These tests replay
;; `bubble`'s own col/row formula against fit-vertical-dialogue's output —
;; not a second approximation of it — so a false positive here would mean
;; the two are actually out of sync, not just "the box looked small."

(defn- simulate-fits?
  "True iff every character bubble would draw for `text` at this exact
  {:width :height} (against `panel-w`/`panel-h`) lands within the box —
  the same col=(quot i rows) placement `bubble` itself uses."
  [text {:keys [scale weight panel-w panel-h]} {:keys [width height]}]
  (let [lh (page/line-height-for {:scale scale :weight weight})
        pad 17
        bw (* width panel-w) bh (* height panel-h)
        rows (max 1 (int (/ (- bh (* 2 pad)) lh)))
        n (count (str text))
        max-col (quot (dec (max 1 n)) rows)]
    (<= (* max-col lh) (- bw (* 2 pad)))))

(deftest fit-vertical-dialogue-regression-dropped-characters
  (testing "a 42-char narration in a wide panel (the exact case that used to
            silently drop はずかずか's second ずか when width/height were
            guessed instead of computed) now verifiably fits"
    (let [text "廊下から、隣のD組の白瀬寧が、コメッコンのドーナツ袋を片手に、ずかずかと入ってきた。"
          opts {:scale 0.6 :weight :light :panel-w 983.0 :panel-h 427.7 :target-height 0.85}
          fit (page/fit-vertical-dialogue text opts)]
      (is (:fits? fit))
      (is (simulate-fits? text opts fit)))))

(deftest fit-vertical-dialogue-grows-height-before-giving-up
  (testing "text too wide at the requested target-height grows taller
            (fewer, narrower columns) rather than reporting fits? false
            prematurely"
    (let [text "起きないと、コメッコンの米粉ドーナツ、わたしが、全部食べるよ"
          opts {:scale 0.55 :weight :bold :panel-w 483.5 :panel-h 316.8 :target-height 0.1}
          fit (page/fit-vertical-dialogue text opts)]
      (is (:fits? fit))
      (is (> (:height fit) 0.1) "grew past the too-small target-height")
      (is (simulate-fits? text opts fit)))))

(deftest fit-vertical-dialogue-short-text-stays-compact
  (testing "a short interjection doesn't get blown up to target-height's
            full box — cols stays small"
    (let [fit (page/fit-vertical-dialogue "……" {:scale 0.85 :weight :faint
                                                  :panel-w 483.5 :panel-h 316.8
                                                  :target-height 0.18})]
      (is (:fits? fit))
      (is (<= (:cols fit) 2)))))

(deftest fit-vertical-dialogue-reports-fits-false-at-the-caps
  (testing "even :max-height can't rescue a huge string crammed into a tiny
            panel at a large scale -- fits? false tells the caller to shrink
            :scale rather than silently shipping a box that drops characters"
    (let [fit (page/fit-vertical-dialogue
               "あいうえおかきくけこさしすせそたちつてとなにぬねの"
               {:scale 3.0 :panel-w 100.0 :panel-h 100.0})]
      (is (false? (:fits? fit))))))

(deftest fit-sfx-reports-width-fit
  (let [ok (page/fit-sfx "ドン" {:scale 1.0 :panel-w 400.0})
        too-wide (page/fit-sfx "うわああああああああああ" {:scale 1.5 :panel-w 200.0})]
    (is (:fits? ok))
    (is (false? (:fits? too-wide)))))

(deftest panel-pixel-sizes-matches-layout-page-panel-count
  (let [sizes (page/panel-pixel-sizes
               {:panels [{:id "a" :size "wide"} {:id "b" :size "half"} {:id "c" :size "half"}]})]
    (is (= 3 (count sizes)))
    (is (every? #(and (pos? (:w %)) (pos? (:h %))) sizes))))

;; --- :font-role -------------------------------------------------------------
;; kami.mangaka.expression resolves a :font-role per archetype/register
;; (:gothic for :stoic/:energetic, :mincho for narration/:strategist, :maru
;; for :gentle, :handwritten for :chatter/:mob, …) so two speakers with
;; different archetypes read in different typefaces, not just different
;; weight/size. These tests are about the family/metrics PLUMBING (a role
;; resolves to a real installed family, and different roles can have
;; different metrics at the same nominal size) — not about which exact font
;; ships on any given machine, since font-role-families itself
;; graceful-degrades per installed fonts (same pattern as the pre-existing
;; single jp-font-family).

(deftest font-role-families-resolve-to-installed-fonts
  (let [avail (set (.getAvailableFontFamilyNames
                    (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)))]
    (doseq [[role family] page/font-role-families]
      (is (or (nil? family) (avail family))
          (str role " resolved to " family " which isn't an installed family")))))

(deftest font-role-changes-line-height
  (testing "two font-roles at the same :scale can report different
            line-height-for -- different typefaces have different metrics at
            the same nominal size, so fit-vertical-dialogue sizing a box for
            the WRONG role's metrics would under/over-shoot"
    (let [roles (keys page/font-role-families)
          heights (into #{} (map #(page/line-height-for {:scale 1.0 :font-role %})) roles)]
      ;; not asserting a *specific* pair differs (font availability varies by
      ;; machine) -- only that the plumbing threads :font-role into the real
      ;; FontMetrics call at all, rather than silently ignoring it.
      (is (every? pos? heights)))))

(deftest fit-vertical-dialogue-respects-font-role-metrics
  (testing "fit-vertical-dialogue's own box uses the SAME :font-role metrics
            line-height-for reports for that role (not a mismatched default)"
    (let [text "あいうえおかきくけこさしすせそ"
          fit (page/fit-vertical-dialogue text {:scale 1.0 :font-role :mincho
                                                 :panel-w 400.0 :panel-h 400.0
                                                 :target-height 0.5})]
      (is (:fits? fit))
      (is (simulate-fits? text {:scale 1.0 :weight nil :panel-w 400.0 :panel-h 400.0} fit)))))

;; --- tight fit (no more padding height out to :target-height) --------------

(deftest fit-vertical-dialogue-does-not-pad-height-past-what-the-text-needs
  (testing "a short line at a generous :target-height comes back close to the
            TIGHT box the text needs, not blown up to fill :target-height --
            the bug that made every bubble render mostly empty white space"
    (let [fit (page/fit-vertical-dialogue "……" {:scale 1.0 :panel-w 500.0 :panel-h 500.0
                                                  :target-height 0.85})]
      (is (:fits? fit))
      (is (< (:height fit) 0.3)
          (str "height " (:height fit) " was padded out toward target-height 0.85"
               " instead of the ~2-character box the text actually needs")))))

;; --- bubble-placement-score (HUNTER×HUNTER baseline) -------------------------
;; Found the hard way: fit-vertical-dialogue guaranteeing text fits its OWN
;; box says nothing about whether that box was placed on top of the
;; speaker's face. These tests are about THAT separate failure mode.

(deftest bubble-placement-score-penalizes-covering-the-face
  (testing "a bubble that fully overlaps the speaker's face bbox scores
            :face-clear 0, not a false pass"
    (let [on-face (page/bubble-placement-score
                   {:bubble-rect [0.1 0.1 0.6 0.6] :face-bbox [0.2 0.2 0.5 0.5]})
          clear (page/bubble-placement-score
                 {:bubble-rect [0.6 0.02 0.95 0.3] :face-bbox [0.1 0.3 0.5 0.7]})]
      (is (= 0.0 (:face-clear on-face)))
      (is (= 1.0 (:face-clear clear)))
      (is (< (:score on-face) (:score clear))))))

(deftest bubble-placement-score-partial-overlap-is-between-0-and-1
  (let [nicked (page/bubble-placement-score
                {:bubble-rect [0.0 0.0 0.3 0.3] :face-bbox [0.25 0.25 0.6 0.6]})]
    (is (< 0.0 (:face-clear nicked) 1.0))))

(deftest bubble-placement-score-rewards-corner-hugging
  (let [flush (page/bubble-placement-score {:bubble-rect [0.0 0.0 0.4 0.3] :corner :tl})
        near (page/bubble-placement-score {:bubble-rect [0.02 0.02 0.4 0.3] :corner :tl})
        floating (page/bubble-placement-score {:bubble-rect [0.4 0.4 0.7 0.6] :corner :tl})]
    (is (= 1.0 (:corner-hug flush)))
    (is (> (:corner-hug near) 0.8) "the library's own standard 0.02 margin should score close to flush")
    (is (< (:corner-hug floating) (:corner-hug near)))))

(deftest bubble-placement-score-tail-must-land-on-the-speaker
  (let [on-speaker (page/bubble-placement-score
                    {:bubble-rect [0 0 0.3 0.3] :face-bbox [0.5 0.5 0.8 0.8]
                     :tail-target [0.6 0.6]})
        off-speaker (page/bubble-placement-score
                     {:bubble-rect [0 0 0.3 0.3] :face-bbox [0.5 0.5 0.8 0.8]
                      :tail-target [0.1 0.1]})]
    (is (= 1.0 (:tail-on-speaker on-speaker)))
    (is (= 0.0 (:tail-on-speaker off-speaker)))))

(deftest bubble-placement-score-unmeasured-dimensions-are-nil-not-a-free-pass
  (testing "no :face-bbox given -> :face-clear nil (unmeasured), and the
            overall :score is the mean of only what WAS measured -- an
            author who omits face-bbox doesn't get a free 1.0 for it"
    (let [bare (page/bubble-placement-score {:bubble-rect [0.02 0.02 0.3 0.2]})]
      (is (nil? (:face-clear bare)))
      (is (nil? (:tail-on-speaker bare)))
      (is (some? (:score bare)))
      (is (= 1.0 (:rectilinear bare))))))

(deftest bubble-placement-score-tilted-panel-fails-rectilinear-unless-declared
  (let [rigid (page/bubble-placement-score {:bubble-rect [0.02 0.02 0.3 0.2]})
        sheared (page/bubble-placement-score {:bubble-rect [0.02 0.02 0.3 0.2] :tilted? true})]
    (is (= 1.0 (:rectilinear rigid)))
    (is (= 0.0 (:rectilinear sheared)))))

(deftest panel-placement-score-averages-its-bubbles
  (let [result (page/panel-placement-score
                [{:bubble-rect [0.02 0.02 0.3 0.2] :corner :tl}
                 {:bubble-rect [0.6 0.6 0.9 0.9] :face-bbox [0.1 0.1 0.4 0.4]}])]
    (is (= 2 (count (:bubbles result))))
    (is (some? (:score result)))))
