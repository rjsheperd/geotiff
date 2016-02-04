(ns geotiff.core
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :refer [<! <!! >! >!! go go-loop chan close!]])
  (:import  [javax.imageio ImageIO ImageReader ImageTypeSpecifier ImageReadParam]
            [java.awt Rectangle]
            [java.awt Image]
            [java.awt.image BufferedImage Raster]
            [java.io FileInputStream OutputStream]
            [org.apache.commons.imaging Imaging]
            [java.util.concurrent ExecutorService Executors]))

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

(defn get-metadata
  [img] 
   (let [metadata (get-metadata-raw img)
         scale (map (fn [^String p] (Double/parseDouble (.trim p))) (.split ^String (:ModelPixelScaleTag metadata) ","))
         tie (partition 3 (map (fn [^String p] (Double/parseDouble (.trim p))) (.split ^String (:ModelTiepointTag metadata) ",")))
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

(defn calc-lng
  [^Integer y ^Integer src-y ^Double slng ^Double tlng]
   (let [^Integer dec-y (dec y)]
     (+ (* (+ (max 0 dec-y) src-y) slng) tlng)))

(defn calc-lat
  [^Integer x ^Integer src-x ^Integer slat ^Integer tlat]
   (+ tlat (* (+ src-x x) slat)))

(defn ^Raster get-data
  [^BufferedImage img]
   (.getData img))

(defn submit
  [^ExecutorService ex ^Runnable fun]
  (.submit ex fun))

(defn tile-processor
  [{[tlat tlng _] :translate [slat slng _] :scale} ^Integer width ^Integer height out]
   (let [in  (chan 1024)
         ex  ^ExecutorService (Executors/newFixedThreadPool 1)]
     (go-loop [[[src-x src-y] img] (<! in)]
       (if (nil? img)
         (submit ex (fn [] (close! out)))
         (let [^Raster tile (get-data img) 
               pvals (vec (.getDataElements tile 0 0 width height nil))]
           (dotimes [y height]
            (let [lng (calc-lng y src-y slng tlng)]
             (dotimes [x width]
               (let [pval (get pvals (* x y))]
                 (if (> pval 0)
                   (let [lat (calc-lat x src-x slat tlat)]
                     (submit ex (fn [] (>!! out [[lat lng] pval])))))))))
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

         hsize  10
         wsize  width

         buff   (.createBufferedImage ^ImageTypeSpecifier (.next (.getImageTypes rdr 0)) wsize hsize)
         params ^ImageReadParam (doto (.getDefaultReadParam rdr) (.setDestination buff))

         metadata  (get-metadata file)

         tile-processor (tile-processor metadata wsize hsize out)]
     (go
       (dotimes [x (/ width wsize)]
         (dotimes [y (/ height hsize)]
           (time
           (let [source (Rectangle. (* x wsize) (* y hsize) wsize hsize)]
             (.setSourceRegion params source)
             (>! tile-processor 
               [[x y] (.read ^ImageReader rdr 0 params)])))))
       (close! tile-processor)
       (.dispose rdr)
       (.close iis)
       (.close fin))
       out)))

(defn read-sync
  [img fun]
   (let [out (chan 1024)]
     (read-async img out)
     (loop [pval (<!! out)]
       (if (not (nil? pval))
         (do 
           (fun pval)
           (recur (<!! out)))))))
  
