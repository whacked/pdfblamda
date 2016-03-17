(ns pdfblambda.xournal
  (:import
   (java.util List)
   (java.io IOException
            FileInputStream InputStreamReader BufferedReader
            FileOutputStream OutputStreamWriter BufferedWriter)
   (java.awt.geom Rectangle2D$Float)
   (org.apache.pdfbox.pdmodel PDDocument PDPage)
   (org.apache.pdfbox.pdmodel.common PDRectangle)
   (org.apache.pdfbox.pdmodel PDPageContentStream)
   (org.apache.pdfbox.pdmodel.font PDFont PDType1Font)
   (org.apache.pdfbox.pdmodel.graphics.color PDGamma)
   (org.apache.pdfbox.pdmodel.interactive.action PDActionURI)
   (org.apache.pdfbox.pdmodel.interactive.annotation PDAnnotationLine
                                                     PDAnnotationSquareCircle
                                                     PDAnnotationMarkup
                                                     PDAnnotationPopup
                                                     PDAnnotationTextMarkup
                                                     PDAnnotationLink
                                                     PDBorderStyleDictionary)
   (org.apache.pdfbox.text PDFTextStripperByArea)
   (java.util.zip GZIPInputStream GZIPOutputStream)
   )
  (:require [clojure.string :as s]
            [pdfblambda.utility :as util]))

(defn make-header []
  (str
   "<?xml version=\"1.0\" standalone=\"no\"?>\n<xournal version=\"0.4.5\">
<title>Xournal document - see http://math.mit.edu/~auroux/software/xournal/</title>"))

(defn make-page-header [w h filename pageno]
  (str "<page width=\"" w "\" height=\"" h "\">"
       "<background type=\"pdf\" "
       (if (= pageno 1)
         (str "domain=\"absolute\" filename=\""
              (s/replace filename #"&" "&amp;")
              "\" ")
         "")
       "pageno=\"" pageno "\" />"
       "<layer>"))

(defn make-page-footer []
  "</layer>\n</page>")

(defn make-footer [] "</xournal>")

(defn make-stroke [ls-float color]
  (str
   "<stroke tool=\"pen\" color=\"" color "\" width=\"1.41\">\n"
   (clojure.string/join " " ls-float)
   ;; if just a single dot, there will be just 1 set of [x y] coordinates
   ;; and xournal will reject the file. it looks like what xournal does for
   ;; single dots is to create a stroke with [x0 y0 x0+0.1 y0]
   (if (= 2 (count ls-float))
     (str " " (+ 0.1 (first ls-float)) " " (second ls-float)))
   "\n</stroke>"))

(defn make-text [coord text color]
  (str
   "<text font=\"Sans\" size=\"12\" color=\"" color "\" x=\"" (first coord) "\" y=\"" (second coord) "\">" text "\n</text>"))

(defn make-highlight [ls-float color width]
  (str
   "<stroke tool=\"highlighter\" color=\"" color "\" width=\"" width "\">\n"
   (clojure.string/join " " ls-float)
   ;; if just a single dot, there will be just 1 set of [x y] coordinates
   ;; and xournal will reject the file. it looks like what xournal does for
   ;; single dots is to create a stroke with [x0 y0 x0+0.1 y0]
   (if (= 2 (count ls-float))
     (str " " (+ 0.1 (first ls-float)) " " (second ls-float)))
   "\n</stroke>"))

;; output xoj annotation stream after reading annotated pdf
(defn parse-pdf-make-xoj [pdf-filepath & yes-extract-text]
  (println "---------------------------------------------------")
  (let [pdf-filename (. (java.io.File. pdf-filepath) getName)
        pdf (PDDocument/load (java.io.File. pdf-filepath))
        page-total (.. pdf getDocumentCatalog getPages getCount)
        xoj-filepath (s/replace pdf-filepath #"pdf$" "pdf.xoj")
        xoj-out (FileOutputStream. xoj-filepath)
        xoj-buf (-> xoj-out GZIPOutputStream. OutputStreamWriter. BufferedWriter.)

        VERBOSITY 0
        xoj-out-writeline (fn [line]
                            (when (> VERBOSITY 0)
                              (println line))
                            (. xoj-buf write (str line "\n")))
        
        ]
    
    (xoj-out-writeline (make-header))
    (doseq [i (range page-total)]
      (let [pageno (inc i)
            page (.getPage pdf i)
            page-w (.. page getMediaBox getWidth)
            page-h (.. page getMediaBox getHeight)
            ls-annot (. page getAnnotations)
            ]
        (xoj-out-writeline (make-page-header page-w page-h pdf-filename pageno))
        ;; (println "mediabox " (. page findMediaBox))

        (doseq [annot ls-annot]
          (let [annot-class (class annot)]
            (cond ;; (or
                  ;;  (isa? annot-class PDAnnotationTextMarkup)
                  ;;  (isa? annot-class PDAnnotationSquareCircle))
                  ;; (when false
                  ;;   (let [annot-rect (. annot getRectangle)
                  ;;         region-name "myregion"
                  ;;         x1 (. annot-rect getLowerLeftX)
                  ;;         ;; this works on the foxit test
                  ;;         ;; but for the TESTFILE generated from the add annotation code above
                  ;;         ;; y1 and y2 need to be swapped! i.e.
                  ;;         ;; y2 (. annot-rect getUpperRightY),
                  ;;         ;; (- y2 y1) etc
                  ;;         y1 (. annot-rect getUpperRightY)
                  ;;         x2 (. annot-rect getUpperRightX)
                  ;;         y2 (. annot-rect getLowerLeftY)
                  ;;         region-rect (Rectangle2D$Float.
                  ;;                      x1
                  ;;                      (- page-h y1)
                  ;;                      (- x2 x1)
                  ;;                      (- y1 y2))
                  ;;         psba (PDFTextStripperByArea.)]
                  ;;     (doto psba
                  ;;       (.setSortByPosition true)
                  ;;       (.setStartPage 0)
                  ;;       (.setEndPage 0)
                  ;;       (.addRegion region-name region-rect)
                  ;;       )
                  ;;     (. psba extractRegions page)
                  ;;     (println
                  ;;      ;; "\nCONTENTS:" (. annot getContents)
                  ;;      ;; "\nDICT:" (. annot getDictionary)
                  ;;      "\nregion rect:" region-rect
                  ;;      "\nRECT:" annot-rect
                  ;;      "\nEXTRACT:" (. psba getTextForRegion region-name)
                  ;;      "\nSUBJECT:" (. annot getSubject)
                  ;;      "\nDATE:" (. annot getCreationDate)
                  ;;      ;; WARNING XXX
                  ;;      ;; THIS IS OUT OF DATE using 2.0.0!
                  ;;      "\nCOLOR:" (let [mycol (. annot getColor)]
                  ;;                   (str (. mycol getR) " "
                  ;;                        (. mycol getG) " "
                  ;;                        (. mycol getB)))
                  ;;      "\nNAME:" (. annot getAnnotationName))
                  ;;     ))

                  

             (or (isa? annot-class PDAnnotationPopup)
                 (isa? annot-class PDAnnotationMarkup)
                 )                  
                  (let [annot-subtype (. annot getSubtype)]
                    ;; (println
                    ;;  (str "MARKUP TYPE: " annot-class
                    ;;       "\nSUBJECT:" (. annot getSubject)
                    ;;       "\nCONTENTS:" (. annot getContents)
                    ;;       "\n"
                    ;;       ))
                    ;; for the pencil object, this seems to give a list of floats
                    
                    (cond
                     (= annot-subtype org.apache.pdfbox.pdmodel.fdf.FDFAnnotationInk/SUBTYPE)
                     (let [InkListObj (.getInkList (org.apache.pdfbox.pdmodel.fdf.FDFAnnotationInk. (.getCOSObject annot)))
                           ;; InkListObj (. (. annot get) getDictionaryObject "InkList")
                           ]
                       (when (not (nil? InkListObj))
                         (doseq [ls-COSVal InkListObj]
                           ;;(println (map #(identity %) (. InkListObj toList)))
                           (xoj-out-writeline
                            (make-stroke
                             (util/flip-y-in-coord-list ls-COSVal page-h) 
                             (util/PDGamma2RGB (. annot getColor))))
                           ;;(doseq [COSVal (partition 2 ls-COSVal)]
                           ;;  (println "x " (. (first COSVal) floatValue) " y " (. (second COSVal) floatValue)))
                           )

                         ))
                     (or (= annot-subtype "Highlight")
                         (= annot-subtype "Underline")
                         (= annot-subtype "Square")
                         (= annot-subtype "Text")
                         )
                     (let [annot-rect (. annot getRectangle)
                           region-name "myregion"
                           x1 (. annot-rect getLowerLeftX)
                           ;; this works on the foxit test
                           ;; but for the TESTFILE generated from the add annotation code above
                           ;; y1 and y2 need to be swapped! i.e.
                           ;; y2 (. annot-rect getUpperRightY),
                           ;; (- y2 y1) etc
                           y1 (. annot-rect getUpperRightY)
                           x2 (. annot-rect getUpperRightX)
                           y2 (. annot-rect getLowerLeftY)
                           W (- x2 x1)
                           H (- y1 y2)
                           mean-y (- page-h (/ (+ y1 y2) 2.0))
                           region-rect (Rectangle2D$Float.
                                        x1
                                        (- page-h y1)
                                        W
                                        H)
                           psba (PDFTextStripperByArea.)]
                       (doto psba
                         (.setSortByPosition true)
                         (.setStartPage pageno)
                         (.setEndPage pageno)
                         (.addRegion region-name region-rect)
                         )
                       
                       ;; this one writes the big blob (entire annotation rect into a single fat highlight stroke)
                       ;; (xoj-out-writeline
                       ;;  (make-highlight [x1 mean-y x2 mean-y] (util/PDGamma2RGB (. annot getColor) 0.5) (- y1 y2)))
                       
                       (cond (= annot-subtype "Square")
                             (xoj-out-writeline (make-stroke
                                                 (let [[x0 y0 x1 y1] (.. annot getRectangle getCOSArray toFloatArray)]
                                                   [x0 (- page-h y0) x1 (- page-h y0) ;; right
                                                    x1 (- page-h y0) x1 (- page-h y1) ;; down
                                                    x1 (- page-h y1) x0 (- page-h y1) ;; left
                                                    x0 (- page-h y1) x0 (- page-h y0) ;; up
                                                    ]
                                                   )
                                                 (util/PDGamma2RGB (. annot getColor) 0.5)))

                             (= annot-subtype "Text")
                             (xoj-out-writeline (make-text [x1 (- page-h y1)] (. annot getContents) "#0000ffff"))

                             true
                             (doseq [quadpoint
                                     (partition 8 (util/flip-y-in-coord-list (. annot getQuadPoints) page-h))]
                               ;; quadpoint defines a 4 set of (x, y) coords
                               ;; defining the quadrilateral of the annotation we
                               ;; want to convert this to a single highlight stroke
                               (let [dualpoint (take 4 quadpoint)
                                     qp-x1 (nth dualpoint 0)        ; should be same as x3
                                     qp-y1 (nth dualpoint 1)
                                     qp-x2 (nth dualpoint 2)        ; should be same as x4
                                     ;; the final y in the 8-point QuadPoint list should be the largest y
                                     qp-y2 (last quadpoint)
                                     qp-ym (+ (/ (+ qp-y1 qp-y2) 2.0)
                                              (cond (= annot-subtype "Underline")
                                                    (* (- qp-y2 qp-y1) 0.250)

                                                    true 0)
                                              )
                                     
                                     qp-H (cond (= annot-subtype "Highlight")
                                                (- qp-y2 qp-y1)

                                                (= annot-subtype "Underline")
                                                (* (- qp-y2 qp-y1) 0.125)
                                                )
                                     ]
                                 (xoj-out-writeline (make-highlight [qp-x1 qp-ym qp-x2 qp-ym] (util/PDGamma2RGB (. annot getColor) 0.5) qp-H)))
                               ))

                       (when yes-extract-text
                         (try
                           (do
                             (. psba extractRegions page)
                             (println
                              "\nEXTRACT:" (. psba getTextForRegion region-name))
                             )
                           (catch IndexOutOfBoundsException e
                             ;; (println "GOT EXCEPTION... trying alternative method")
                             (println "\nPDFTOTEXT:" (util/exec-pdftotext pdf-filepath pageno x1 (- page-h y1) W H)))))
                       )

                     ;;(= annot-subtype "Popup")
                     ;;(let []
                     ;;  (def *popup* annot)
                     ;;  )
                     ;;
                     ;;(= annot-subtype "Circle")
                     ;;(let []
                     ;;  (def *circle* annot)
                     ;;  )
                     
                     true
                     (do (println (str "unhandled subtype: " annot-subtype)))
                     

                     )
                    )
                  
                  true
                  (do (println (str "unhandled class: " annot-class)))
                  
                  ))
          )
        
        (xoj-out-writeline (make-page-footer))


        ))
    (xoj-out-writeline (make-footer))

    (. pdf close)

    (. xoj-buf close)
    (. xoj-out close)
    (println "DONE")
    ))

(defn process-directory
  "example: (process-directory \"/tmp/dir-with-pdfs\")"
  [d]
  (doseq [f (filter
             (fn [f] (re-find #"(?i)pdf$" (. f getName)))
             (.listFiles (java.io.File. d)))]
    (parse-pdf-make-xoj (. f getAbsolutePath))))
