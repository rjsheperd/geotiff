(ns geotiff.core-test
  (:require [midje.sweet :refer :all]
            [geotiff.core :refer :all]))

(fact "Metadata"
  (let [metadata1 (get-metadata "test/demo.tif")
        metadata2 (get-metadata "test/demo2.tif")]
    (dissoc metadata1 :raw)
      => {:bbox [[-80.0 0.0] [-80.0 -10.0] [-70.0 -10.0] [-70.0 0.0]]
          :height 40000 
          :width 40000
          :translate [-80.0 0.0 0.0]
          :scale [2.5E-4 2.5E-4 0.0]}
    (dissoc metadata2 :raw)
      => {:bbox [[-70.0 0.0] [-70.0 -10.0] [-60.0 -10.0] [-60.0 0.0]]
          :height 40000 
          :width 40000
          :translate [-70.0 0.0 0.0]
          :scale [2.5E-4 2.5E-4 0.0]}))

(fact "Lets try this"
  (let [data (transient [])]
    (time
      (read-sync "test/demo.tif"
        (fn [pval] (conj! data pval))))
    (let [data (persistent! data)]
      (println (take 10 data))
      (map second (take 10 data))
       => (list 9 9 8 13 13 13 7 7 7 13) 
      (take 10 data)
       => (list [[-79.9945 0.0] 9]
                [[-79.99425 0.0] 9]
                [[-79.98725 0.0] 8]
                [[-79.9615 0.0] 13]
                [[-79.9595 0.0] 13]
                [[-79.95775 0.0] 13]
                [[-79.9165 0.0] 7]
                [[-79.91625 0.0] 7]
                [[-79.91575 0.0] 7]
                [[-79.9155 0.0] 13])
      )))

