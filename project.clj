(defproject pdfblastaxoj "1.0.0-SNAPSHOT"
  :description "process an annotated pdf and output a xoj annotation file"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 ;; if install this from the jar download
                 ;; make sure you install the "app" version
                 ;; and not the "compiled binary"
                 ;; use this command:
                 ;; mvn install:install-file -DgroupId=org.apache -DartifactId=pdfbox -Dversion=1.5.0 -Dpackaging=jar -Dfile=/path/to/pdfbox-app-1.5.0.jar
                 [org.apache/pdfbox "1.5.0"]])
