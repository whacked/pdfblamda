(ns pdfblambda.utility
  )

(defn exec-pdftotext [pdf-file pageno x y W H]
  (let [txt-file (str "pdftotext-" x "-" y "-" W "-" H ".txt")
        vec-arg (map str ["pdftotext" "-f" pageno "-l" pageno "-x" (int x) "-y" (int y) "-W" (int W) "-H" (int H) pdf-file "-"])
        process (.. Runtime getRuntime (exec (into-array vec-arg)))]
    (println (clojure.string/join " " vec-arg))
    (. process waitFor)
    (clojure.string/trim (clojure.string/join " " (line-seq (java.io.BufferedReader.
                                                             (java.io.InputStreamReader. (. process getInputStream))))))
    ))

(defn PDGamma2RGB
  "pdg is a PDColor object"
  [pdg & alpha]
  (str "#"
       (clojure.string/join
        ""
        (map
         (fn [f]
           (let [hex (str (Integer/toHexString (Math/round (* f 255))))]
             (if (> 2 (count hex))
               (str "0" hex) hex)))
         [(get (.getComponents pdg) 0)
          (get (.getComponents pdg) 1)
          (get (.getComponents pdg) 2)
          (if alpha (first alpha) 1.0) ;; alpha
          ]))))

(defn flip-y-in-coord-list [vec-xy-coord page-h]
  (map (fn [func & rest-arg] (func rest-arg))
       (cycle [first (fn [ls-arg] (apply (fn [y] (- page-h y)) ls-arg))])
       vec-xy-coord))

