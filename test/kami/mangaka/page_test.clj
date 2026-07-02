(ns kami.mangaka.page-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [kami.mangaka.page :as page]))

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
