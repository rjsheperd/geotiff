(ns geotiff.core
  (:require [clojure.java.io :as io])
  (:require [clojure.core.async :refer [<! <!! >! >!! go go-loop chan close!]])
  (:import  [javax.imageio ImageIO ImageReader ImageTypeSpecifier ImageReadParam]
            [com.twelvemonkeys.imageio.plugins.tiff TIFFImageReader]
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
      :scale (vec scale)
      :translate (vec translate)
      :bbox [[(first translate) (second translate)]
             [(first translate)
              (- (second translate) (* height (second scale)))]
             [(+ (first translate) (* width (first scale)))
              (- (second translate) (* height (second scale)))]
             [(+ (first translate) (* width (first scale)))
              (second translate)]]}))

(defn ^Double calc-lng
  [^Integer x ^Integer src-x ^Double slng ^Double tlng]
   (+ (* (+ x src-x) slng) tlng))

(defn ^Double calc-lat
  [^Integer y ^Integer src-y ^Integer slat ^Integer tlat]
  (- tlat (* (+ y src-y) slat)))

(defn ^Raster get-data
  [^BufferedImage img]
   (.getData img))

(defn submit
  [^ExecutorService ex ^Runnable fun]
  (.submit ex fun))

(defn fix-val
  [^Integer x]
   (if (> x -1) x
     (+ 256 x)))

(defn tile-processor
  [{[tlng tlat _] :translate [slng slat _] :scale} ^Integer width ^Integer height out]
   (let [in  (chan 1024)
         ex  ^ExecutorService (Executors/newFixedThreadPool 1)]
     (go-loop [[[src-x src-y] img] (<! in)]
       (if (nil? img)
         (submit ex (fn [] (close! out)))
         (let [^Raster tile (get-data img)
               nbands (.getNumDataElements tile)
               pvals  (vec (partition nbands (.getDataElements tile 0 0 width height nil)))]
           (dotimes [i (count pvals)]
             (let [y (Math/floor (/ i width))
                   x (- i (* y width))
                   lat (calc-lat y src-y slat tlat)
                   lng (calc-lng x src-x slng tlng)
                   bvals (mapv fix-val (get pvals i))]
               (submit ex (fn [] (>!! out [[lng lat] bvals])))))
           (recur (<! in)))))
     in))

(defn set-cut
  [^ImageReadParam params ^Rectangle source]
  (.setSourceRegion params source))

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

         hsize  (/ 270 2)
         wsize  width

         buff   (.createBufferedImage ^ImageTypeSpecifier (.next (.getImageTypes rdr 0)) wsize hsize)
         params ^ImageReadParam (doto (.getDefaultReadParam rdr) (.setDestination buff))

         metadata  (get-metadata file)

         tile-processor (tile-processor metadata wsize hsize out)]
     (go
       (dotimes [y (Math/ceil (/ height hsize))]
         (dotimes [x (Math/ceil (/ width wsize))]
           (let [source (Rectangle. (* x wsize) (* y hsize) wsize hsize)]
             (set-cut params source)
             (>! tile-processor
               [[(* x wsize) (* y hsize)] (.read ^ImageReader rdr 0 params)]))))
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


(comment
  (def file (io/file "fire-area-cog.tif"))
  (def file (io/file "az-clip-header"))
  (def ^BufferedImage bi (ImageIO/read file))
  (.getWidth bi)
  (.getHeight bi)
  (.getNumXTiles bi)
  (.getNumYTiles bi)
  (.getType bi)
  (.getRGB bi 2000 3000)

  (def ^BufferedImage sbi (.getSubimage bi
                                        (/ (.getWidth bi) 4)
                                        (/ (.getHeight bi) 4)
                                        (/ (.getWidth bi) 2)
                                        (/ (.getHeight bi) 2)))

  (.getWidth bi)
  (.getWidth sbi)
  (.getHeight sbi)

  (io/file "fire-area-new-cog.tif")

  (def fin (FileInputStream. file))
  (def iss (ImageIO/createImageInputStream fin))
  (def ^TIFFImageReader rdr (.next (ImageIO/getImageReadersByFormatName "tiff")))
  (println rdr)
  (.setInput rdr iss false)

  (.getNumImages rdr true)
  (.getWidth rdr 0)
  (.getHeight rdr 0)
  (.getRawImageType rdr 0)

  (.isImageTiled rdr 0)
  (.getTileWidth rdr 0)
  (.getTileHeight rdr 0)
  (.readTileRaster rdr 0 0 1)
  (.getItems (.getImageMetadata rdr 0))

  (def new-size (Rectangle. 1000 1000 1 1))
  (def ^ImageReadParam param (.getDefaultReadParam rdr))
  (.setSourceRegion param new-size)

  (.read rdr 0 param)


  (.getReaderFormatNames ImageIO)

  )
