(ns geotiff.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.shell :as sh]
            [geotiff.core :as gt]))

(deftest get-metadata
  (let [metadata1 (gt/get-metadata "test/demo.tiff")
        metadata2 (gt/get-metadata "test/demo_mini.tiff")]
    (is (= (dissoc metadata1 :raw)
           {:bbox [[-80.0 0.0] [-80.0 -10.0] [-70.0 -10.0] [-70.0 0.0]]
            :height 40000
            :width 40000
            :translate [-80.0 0.0 0.0]
            :scale [2.5E-4 2.5E-4 0.0]}))
    (is (= (dissoc metadata2 :raw)
           {:bbox [[-180.0 90.0] [-180.0 -90.0] [180.0 -90.0] [180.0 90.0]]
            :height 270
            :width 540
            :translate [-180.0 90.0 0.0]
            :scale [0.6666666666666666 0.6666666666666666 0.0]}))))

(deftest read-sync
  (let [data (transient [])]
    (time
      (gt/read-sync "test/demo_mini.tiff"
        (fn [pval] (conj! data pval))))
    (let [data (persistent! data)
          s    0.6666666666666666]
      (is (= (count data) (* 540 270)))
      (is (= (first data) [[-180.0 90.0] [2 5 20]]))
      (is (= (second data) [[(+ -180.0 s) 90.0] [2 5 20]]))
      (is (= (last data) [[(- 180.0 s) (+ -90.0 s)] [241 242 244]]))
      (is (= (second (reverse data)) [[(- 180 (* s 2)) (+ -90.0 s)] [255 255 255]])))))


(comment
  (def geotiff "fire-area-cog.tif")
  (def metadata (gt/get-metadata geotiff))
  (def size (gt/get-sizes geotiff))
  size

  (:raw metadata)
  (def gdalinfo (sh/sh "gdalinfo" "fire-area-cog.tif"))
  gdalinfo
  )


