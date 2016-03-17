(ns pdfblambda.core
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
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [pdfblambda.xournal :as xoj]
            [pdfblambda.utility :as util]))






                     )
                    )
                  
                  true
                  (do (println (str "unhandled class: " annot-class)))
                  
                  ))
          )
        
        (xoj-out-writeline (make-xoj-page-footer))


        ))
    (xoj-out-writeline (make-xoj-footer))

    (. pdf close)

    (. xoj-buf close)
    (. xoj-out close)
    (println "DONE")
    ))


(defn process-directory [d]
  (doseq [f (filter
             (fn [f] (re-find #"(?i)pdf$" (. f getName)))
             (.listFiles (java.io.File. d)))]
    (parse-pdf-make-xoj (. f getAbsolutePath))))

;; example
;; (process-directory "/home/example/pdf")
