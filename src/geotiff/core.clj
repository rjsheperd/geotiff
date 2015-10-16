(ns geotiff.core
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :refer [<! <!! >! >!! go go-loop chan close!]])
  (:import  [javax.imageio ImageIO ImageReader]
            [javax.imageio ImageTypeSpecifier]
            [java.awt Rectangle]
            [java.awt Image]
            [java.awt.image BufferedImage Raster]
            [java.io FileInputStream OutputStream]
            [org.apache.commons.imaging Imaging]))

(defn get-sizes
  [img] 
   (let [file (io/file img)
         fin  ^FileInputStream (FileInputStream. file)
         iis  (ImageIO/createImageInputStream fin)
         rdr  ^ImageReader (.next (ImageIO/getImageReaders iis))
         _    (.setInput rdr iis)

         width  (.getWidth rdr 0)
         height (.getHeight rdr 0)]
       (.dispose rdr)
       (.close iis)
       (.close fin)
      [height width]))

(defn get-metadata-raw
  [img]
  (apply hash-map
    (flatten
      (map 
        #(list (keyword (first %)) (second %))
      (map
        (fn [item] (.split (str item) ":"))
        (.getItems
          (Imaging/getMetadata 
            (io/file img))))))))

(comment "Lat, Lng")

(defn get-metadata
  [img] 
   (let [metadata (get-metadata-raw img)
         scale (map #(Double/parseDouble (.trim %)) (.split (:ModelPixelScaleTag metadata) ","))
         tie (partition 3 (map #(Double/parseDouble (.trim %)) (.split (:ModelTiepointTag metadata) ",") ))
         translate (map (partial apply +) (partition 2 (apply interleave tie)))
         [height width] (get-sizes img)]
     {:raw metadata
      :height height
      :width width
      :scale (vec scale )
      :translate (vec translate)
      :bbox [[(first translate) (second translate)]
             [(first translate)
              (- (second translate) (* width (second scale)))] 
             [(+ (first translate) (* height (first scale)))
              (- (second translate) (* width (second scale)))] 
             [(+ (first translate) (* height (first scale)))
              (second translate)]]}))

(defn tile-processor
  [{[tlat tlng _] :translate 
    [slat slng _] :scale}
   width height out]
   (let [in  (chan 1024)
         ex  (java.util.concurrent.Executors/newFixedThreadPool 1)]
     (go-loop [[[src-x src-y] img] (<! in)]
       (if (nil? img)
         (.submit ex ^Runnable
           (fn [] (close! out)))
         (do 
           (println "got tile!")
           (time
           (let [tile ^Raster (.getData ^BufferedImage img) 
                 pvals (vec (.getDataElements tile 0 0 width height nil))]
             (dotimes [y height]
              (let [lng (+ (* (+ (max 0 (dec y)) src-y) slng) tlng)] 
               (comment "Beware, while y=0 it will always read byte 0!")
               (dotimes [x width]
                 (let [pval (get pvals (* x y))]
                   (if (> pval 0)
                     (let [lat (+ tlat (* (+ src-x x) slat))]
                       ;; testar sem enviar!
                       (.submit ex 
                          ^Runnable 
                          (fn [] (>!! out [[lat lng] pval])))
                       ))))))))
           (recur (<! in)))))
     in))

(defn read-async
  ([img] 
   (let [out (chan 1024)]
     (read-async img out)
     out))
  ([img out] 
   (let [file (io/file img)
         fin  ^FileInputStream (FileInputStream. file)
         iis  (ImageIO/createImageInputStream fin)
         rdr  ^ImageReader (.next (ImageIO/getImageReaders iis))
         _    (.setInput rdr iis)

         width  (.getWidth rdr 0)
         height (.getHeight rdr 0)

         height 100
         hsize  10
         wsize  width

         buff   (.createBufferedImage ^ImageTypeSpecifier (.next (.getImageTypes rdr 0)) wsize hsize)
         params (doto (.getDefaultReadParam rdr) (.setDestination buff))

         metadata  (get-metadata file)

         tile-processor (tile-processor metadata wsize hsize out)]
     (go
     (time
       (do
       (doseq [x (range 0 (/ width wsize))
               y (range 0 (/ height hsize))]
         (let [source (Rectangle. (* x wsize) (* y hsize) wsize hsize)]
           (.setSourceRegion params source)
           (println "Read a tile")
           (>! tile-processor 
             [[x y] (time (.read ^ImageReader rdr 0 params)) ])))
       (close! tile-processor)
       (.dispose rdr)
       (.close iis)
       (.close fin)))
       out))))

(defn read-sync
  [img fun]
   (let [out (chan 1024)]
     (read-async img out)
     (loop [pval (<!! out)]
       (if (not (nil? pval))
         (do 
           (fun pval)
           (recur (<!! out)))))))
  
