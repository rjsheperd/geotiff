(ns geotiff.core-test
  (:require [midje.sweet :refer :all]
            [geotiff.core :refer :all]))

(fact "Metadata"
  (let [metadata1 (get-metadata "test/demo.tiff")
        metadata2 (get-metadata "test/demo_mini.tiff")]
    (dissoc metadata1 :raw)
      => {:bbox [[-80.0 0.0] [-80.0 -10.0] [-70.0 -10.0] [-70.0 0.0]]
          :height 40000 
          :width 40000
          :translate [-80.0 0.0 0.0]
          :scale [2.5E-4 2.5E-4 0.0]}
    (dissoc metadata2 :raw)
      => {:bbox [[-180.0 90.0] [-180.0 -90.0] [180.0 -90.0] [180.0 90.0]]
          :height 270
          :width 540
          :translate [-180.0 90.0 0.0]
          :scale [0.6666666666666666 0.6666666666666666 0.0]}))

(fact "Lets try this"
  (let [data (transient [])]
    (time
      (read-sync "test/demo_mini.tiff"
        (fn [pval] (conj! data pval))))
    (let [data (persistent! data)
          s 0.6666666666666666]
      (count data) => (* 540 270)
      (first data) => [[-180.0 90.0] [2 5 20]]
      (second data) => [[(+ -180.0 s) 90.0] [2 5 20]]
      (last data) => [[(- 180.0 s) (+ -90.0 s)] [241 242 244]]
      (second (reverse data)) => [[(- 180 (* s 2)) (+ -90.0 s)] [255 255 255]]
      )))

#_(fact "Time only"
  (time (read-sync "test/demo.tiff" (fn [pval] nil))) )

