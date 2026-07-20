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
