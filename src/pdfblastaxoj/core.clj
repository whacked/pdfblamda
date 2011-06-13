
;; (use '[clojure.contrib.repl-utils :only (show)])

(ns pdfblastaxoj.core
  (:import
   (java.util List)
   (java.io IOException
            FileInputStream InputStreamReader BufferedReader
            FileOutputStream OutputStreamWriter BufferedWriter)
   (java.awt.geom Rectangle2D$Float)
   (org.apache.pdfbox.pdmodel PDDocument PDPage)
   (org.apache.pdfbox.pdmodel.common PDRectangle)
   (org.apache.pdfbox.pdmodel.edit PDPageContentStream)
   (org.apache.pdfbox.pdmodel.font PDFont PDType1Font)
   (org.apache.pdfbox.pdmodel.graphics.color PDGamma)
   (org.apache.pdfbox.pdmodel.interactive.action.type PDActionURI)
   (org.apache.pdfbox.pdmodel.interactive.annotation PDAnnotationLine
                                                     PDAnnotationSquareCircle
                                                     PDAnnotationMarkup
                                                     PDAnnotationTextMarkup
                                                     PDAnnotationLink
                                                     PDBorderStyleDictionary)
   (org.apache.pdfbox.util PDFTextStripperByArea)
   (java.util.zip GZIPInputStream GZIPOutputStream)
   )
  (:use (clojure.string)
        (clojure.contrib.string)))

(use 'clojure.string)
(use 'clojure.contrib.string)

(defn make-xoj-header []
  (str
   "<?xml version=\"1.0\" standalone=\"no\"?>\n<xournal version=\"0.4.5\">
<title>Xournal document - see http://math.mit.edu/~auroux/software/xournal/</title>"))
(defn make-xoj-page-header [w h filename pageno]
  (str "<page width=\"" w "\" height=\"" h "\">"
       "<background type=\"pdf\" "
       (if (= pageno 1)
         (str "domain=\"absolute\" filename=\""
              (clojure.contrib.string/replace-re #"&" "&amp;" filename)
              "\" ")
         "")
       "pageno=\"" pageno "\" />"
       "<layer>"))
(defn make-xoj-page-footer []
  "</layer>\n</page>")
(defn make-xoj-footer [] "</xournal>")
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

(defn PDGamma2RGB [pdg & alpha]
  (str "#"
       (clojure.string/join
        ""
        (map
         (fn [f]
           (let [hex (str (Integer/toHexString (Math/round (* f 255))))]
             (if (> 2 (count hex))
               (str "0" hex) hex)))
         [(. pdg getR)
          (. pdg getG)
          (. pdg getB)
          (if alpha (first alpha) 1.0) ;; alpha
          ]))))

(defn exec-pdftotext [pdf-file pageno x y W H]
  (let [txt-file (str "pdftotext-" x "-" y "-" W "-" H ".txt")
        vec-arg (map str ["pdftotext" "-f" pageno "-l" pageno "-x" (int x) "-y" (int y) "-W" (int W) "-H" (int H) pdf-file "-"])
        process (.. Runtime getRuntime (exec (into-array vec-arg)))]
    (println (clojure.string/join " " vec-arg))
    (. process waitFor)
    (clojure.string/trim (clojure.string/join " " (line-seq (BufferedReader. (InputStreamReader. (. process getInputStream))))))
    ))

;; (def p (.. Runtime getRuntime (exec   )))

;; write (int ) annotations
(when false
  (let [document (PDDocument.)
        page (PDPage.)]

    (. document addPage page)


    ;; Setup some basic reusable objects/constants
    ;; Annotations themselves can only be used once!

    (let [annotations (. page getAnnotations)
          INCH 72.0
          
          colRED (let [mycol (PDGamma.)] (. mycol setR 1) mycol)
          colBLUE (let [mycol (PDGamma.)] (. mycol setB 1) mycol)
          colBLACK (PDGamma.)

          bdrTHICK (let [mybdr (PDBorderStyleDictionary.)] (. mybdr setWidth (/ INCH 12)) mybdr)
          bdrTHIN (let [mybdr (PDBorderStyleDictionary.)] (. mybdr setWidth (/ INCH 72)) mybdr)
          bdrULINE (let [mybdr (PDBorderStyleDictionary.)]
                     (. mybdr setStyle PDBorderStyleDictionary/STYLE_UNDERLINE)
                     (. mybdr setWidth (/ INCH 72))
                     mybdr)

          pw (.. page getMediaBox getUpperRightX)
          ph (.. page getMediaBox getUpperRightY)
          
          ;; First add some text, two lines we'll add some annotations to this later
          font PDType1Font/HELVETICA_BOLD

          contentStream (PDPageContentStream. document page)

          ]

      (doto contentStream
        (.beginText)
        (.setFont font 18)
        (.moveTextPositionByAmount INCH (- ph INCH 18))
        (.drawString "PDFBox")
        (.moveTextPositionByAmount 0 (/ (- INCH) 2))
        (.drawString "Click Here")
        (.endText)
        (.close))

      ;; Now add the markup annotation, a highlight to PDFBox text
      (let [txtMark (let [mymrk (PDAnnotationTextMarkup. PDAnnotationTextMarkup/SUB_TYPE_HIGHLIGHT)]
                      (. mymrk setColour colBLUE)
                      (. mymrk setConstantOpacity 0.2)
                      mymrk)
            ;; Set the rectangle containing the markup

            txtWidth (* (/ (. font getStringWidth "PDFBox") 1000) 18)
            position (let [mypos (PDRectangle.)]
                       (doto mypos
                         (.setLowerLeftX INCH)
                         (.setLowerLeftY (- ph INCH 18))
                         (.setUpperRightX (+ 72 txtWidth))
                         (.setUpperRightY (- ph INCH)))
                       mypos)
            ]
        (. txtMark setRectangle position)
        
        ;; work out the points forming the four corners of the annotations
        ;; set out in anti clockwise form (Completely wraps the text)
        ;; OK, the below doesn't match that description.
        ;; It's what acrobat 7 does and displays properly!

        (let [x1 (. position getLowerLeftX)
              y1 (- (. position getUpperRightY) 2)
              x2 (. position getUpperRightX)
              y2 y1
              x3 x1
              y3 (- (. position getLowerLeftY) 2)
              x4 x2
              y4 y3]
          (. txtMark setQuadPoints (float-array [x1 y1 x2 y2
                                                 x3 y3 x4 y4]))
          (. txtMark setContents "Highlighted since it's important")
          (. annotations add txtMark)
          ))

      ;; Now add the link annotation, so the clickme works
      
      (let [txtLink (PDAnnotationLink.)
            ;; Set the rectangle containing the link
            txtWidth (* (/ (. font getStringWidth "Click Here") 1000) 18)
            position (let [mypos (PDRectangle.)]
                       (doto mypos
                         (.setLowerLeftY INCH)
                         (.setLowerLeftY (- ph (* INCH 1.5) 20))
                         (.setUpperRightX (+ txtWidth 72))
                         (.setUpperRightY (- ph (* INCH 1.5))))
                       mypos)
            ;; add an action
            action (let [myact (PDActionURI.)]
                     (. myact setURI "http://www.pdfbox.org"))
            ]

        (. txtLink setBorderStyle bdrULINE)
        (. txtLink setRectangle position)
        (. txtLink setAction action)
        (. annotations add txtLink)
        )

      ;; Now draw a few more annotations
      (let [aCircle (let [mycir (PDAnnotationSquareCircle. PDAnnotationSquareCircle/SUB_TYPE_CIRCLE)]
                      (doto mycir
                        (.setContents "Circle Annotation")
                        (.setInteriorColour colRED) ;; Fill in circle in red
                        (.setColour colBLUE) ;; The border itself will be blue
                        (.setBorderStyle bdrTHIN)
                        )
                      mycir)
            ;; Place the annotation on the page, we'll make this 1" round
            ;; 3" down, 1" in on the page
            position (let [mypos (PDRectangle.)]
                       (doto mypos
                         (.setLowerLeftX INCH)
                         (.setLowerLeftY (- ph (* INCH 3) INCH)) ;; 1" height, 3" down
                         (.setUpperRightX (* 2 INCH)) ;; 1" in, 1" width
                         (.setUpperRightY(- ph (* INCH 3))) ;; 3" down
                         )
                       mypos)
            ]
        (. aCircle setRectangle position)
        ;;  add to the annotations on the page
        (. annotations add aCircle)
        )

      ;; Now a square annotation
      (let [aSquare (let [mysqr (PDAnnotationSquareCircle. PDAnnotationSquareCircle/SUB_TYPE_SQUARE)]
                      (doto mysqr
                        (.setContents "Square Annotation")
                        (.setColour colRED) ;; Outline in red, not setting a fill
                        (.setBorderStyle bdrTHICK))
                      mysqr)
            ;; Place the annotation on the page, we'll make this 1" (72points) square
            ;; 3.5" down, 1" in from the right on the page
            position (let [mypos (PDRectangle.)]
                       (doto mypos
                         (.setLowerLeftX (- pw (* INCH 2))) ;; 1" in from right, 1" wide
                         (.setLowerLeftY (- ph (* INCH 3.5) INCH)) ;; 1" height, 3.5" down
                         (.setUpperRightX (- pw INCH)) ;; 1" in, 1" width
                         (.setUpperRightY(- ph (* INCH 3.5))) ;; 3.5" down
                         )
                       mypos)
            ]
        (. aSquare setRectangle position)
        ;;  add to the annotations on the page
        (. annotations add aSquare)
        )

      ;; Now we want to draw a line between the two, one end with an open arrow
      (let [aLine (let [mylin (PDAnnotationLine.)]
                    (doto mylin
                      (.setEndPointEndingStyle PDAnnotationLine/LE_OPEN_ARROW )
                      (.setContents "Circle->Square")
                      (.setCaption true) ;; Make the contents a caption on the line
                      )
                    mylin)
            ;; Set the rectangle containing the line
            position (let [mypos (PDRectangle.)]
                       (doto mypos
                         (.setLowerLeftX (* INCH 2)) ;; 1" in + width of circle
                         (.setLowerLeftY (- ph (* INCH 3.5) INCH)) ;; 1" height, 3.5" down
                         (.setUpperRightX (- pw INCH INCH)) ;; 1" in from right, and width of square
                         (.setUpperRightY(- ph (* INCH 3))) ;; 3" down (top of circle)
                         )
                       mypos)
            ;; Now set the line position itself
            x1 (* INCH 2)                    ;; x1 = rhs of circle
            y1 (- ph (* INCH 3.5))           ;; y1 halfway down circle
            x2 (- pw (* INCH 2))             ;; x2 = lhs of square
            y2 (- ph (* INCH 4))             ;; y2 halfway down square
            ]
        (doto aLine
          (.setLine (float-array [x1 y1 x2 y2]))
          (.setRectangle position)
          (.setBorderStyle bdrTHICK)
          (.setColour colBLACK))
        ;; add to the annotations on the page
        (. annotations add aLine)
        )
      
      )
    
    ;; Finally all done
    (. document save "TESTFILE.pdf")
    (. document close)
    (println "SUCCESS!")
    ))



;; test xoj file I/O
(when false
  (let [fis (FileInputStream. "raw.xoj")
        br (-> fis GZIPInputStream. InputStreamReader. BufferedReader.)

        fos (FileOutputStream. "out.xoj")
        bw (-> fos GZIPOutputStream. OutputStreamWriter. BufferedWriter.)
        ]
    (doseq [line (line-seq br)]
      (. bw write (str line "\n")))
    (. bw close)
    (. fis close)
    (. fos close)))




(defn flip-y-in-coord-list [vec-xy-coord page-h]
  (map (fn [func & rest-arg] (func rest-arg))
       (cycle [first (fn [ls-arg] (apply (fn [y] (- page-h y)) ls-arg))])
       vec-xy-coord))


;; output xoj annotation stream after reading annotated pdf
(when true
  (println "---------------------------------------------------")
  (let [pdf-filepath "test.pdf"
        pdf-filename (. (java.io.File. pdf-filepath) getName)
        pdf (. PDDocument (load pdf-filepath))
        ls-page (.. pdf getDocumentCatalog getAllPages)
        xoj-filepath (clojure.contrib.string/replace-re #"pdf$" "pdf.xoj" pdf-filepath)
        xoj-out (FileOutputStream. xoj-filepath)
        xoj-buf (-> xoj-out GZIPOutputStream. OutputStreamWriter. BufferedWriter.)

        VERBOSITY 0
        xoj-out-writeline (fn [line]
                            (when (> VERBOSITY 0)
                              (println line))
                            (. xoj-buf write (str line "\n")))
        
        ]
    
    (xoj-out-writeline (make-xoj-header))
    (doseq [[page pageno] (map vector
                               ls-page
                               (range 1 (inc (count ls-page))))]
      (let [page-w (.. page findMediaBox getWidth)
            page-h (.. page findMediaBox getHeight)
            ls-annot (. page getAnnotations)
            ]
        (xoj-out-writeline (make-xoj-page-header page-w page-h pdf-filename pageno))
        (println "mediabox " (. page findMediaBox))

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
                  ;;      "\nCOLOR:" (let [mycol (. annot getColour)]
                  ;;                   (str (. mycol getR) " "
                  ;;                        (. mycol getG) " "
                  ;;                        (. mycol getB)))
                  ;;      "\nNAME:" (. annot getAnnotationName))
                  ;;     ))

                  
                  (isa? annot-class PDAnnotationMarkup)
                  (let [annot-subtype (. annot getSubtype)]
                    ;; (println
                    ;;  (str "MARKUP TYPE: " annot-class
                    ;;       "\nSUBJECT:" (. annot getSubject)
                    ;;       "\nCONTENTS:" (. annot getContents)
                    ;;       "\n"
                    ;;       ))
                    ;; for the pencil object, this seems to give a list of floats
                    
                    (cond
                     (= annot-subtype "Ink")
                     (let [InkListObj (. (. annot getDictionary) getDictionaryObject "InkList")]
                       (when (not (nil? InkListObj))
                         (doseq [ls-COSVal InkListObj]
                           ;;(println (map #(identity %) (. InkListObj toList)))
                           (xoj-out-writeline
                            (make-stroke
                             (flip-y-in-coord-list (. ls-COSVal toFloatArray) page-h) 
                             (PDGamma2RGB (. annot getColour))))
                           ;;(doseq [COSVal (partition 2 ls-COSVal)]
                           ;;  (println "x " (. (first COSVal) floatValue) " y " (. (second COSVal) floatValue)))
                           )

                         ))
                     (= annot-subtype "Highlight")
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
                       ;;  (make-highlight [x1 mean-y x2 mean-y] (PDGamma2RGB (. annot getColour) 0.5) (- y1 y2)))
                       
                       ;; (def qp (. annot getQuadPoints))
                       (doseq [quadpoint
                               (partition 8 (flip-y-in-coord-list (. annot getQuadPoints) page-h))]
                         ;; quadpoint defines a 4 set of (x, y) coords
                         ;; defining the quadrilateral of the annotation we
                         ;; want to convert this to a single highlight stroke
                         (let [dualpoint (take 4 quadpoint)
                               qp-x1 (nth dualpoint 0) ; should be same as x3
                               qp-y1 (nth dualpoint 1)
                               qp-x2 (nth dualpoint 2) ; should be same as x4
                               ;; the final y in the 8-point QuadPoint list should be the largest y
                               qp-y2 (last quadpoint)
                               qp-ym (/ (+ qp-y1 qp-y2) 2.0)
                               qp-H (- qp-y2 qp-y1)
                               ]
                           (xoj-out-writeline (make-highlight [qp-x1 qp-ym qp-x2 qp-ym] (PDGamma2RGB (. annot getColour) 0.5) qp-H)))
                           )

                       (try
                         (do
                           (. psba extractRegions page)
                           (println
                            "\nEXTRACT:" (. psba getTextForRegion region-name))
                           )
                         (catch IndexOutOfBoundsException e
                           ;; (println "GOT EXCEPTION... trying alternative method")
                           (println "\nPDFTOTEXT:" (exec-pdftotext pdf-filepath pageno x1 (- page-h y1) W H))))
                       )
                  
                     )
                    )
                  
                  true
                  (str "unhandled class: " annot-class)
                  ))
          )
        
        (xoj-out-writeline (make-xoj-page-footer))


        ))
    (xoj-out-writeline (make-xoj-footer))

    (. pdf save "TESTFILE-foo.pdf")
    (. pdf close)

    (. xoj-buf close)
    (. xoj-out close)
    (println "DONE")

    ))
