(defproject org.clojars.diogok/geotiff "0.0.1-SNAPSHOT"
  :description "GeoTIFF reader"
  :url "http://github.com/diogok/geotiff"
  :license {:name "MIT"}
  :main geotiff.core
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [com.twelvemonkeys.imageio/imageio-tiff "3.1.1"]
                 [org.apache.commons/commons-imaging "1.0-SNAPSHOT"]]
  :repositories [["apache.snapshots" "http://repository.apache.org/snapshots"]]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.7.0"]]
                   :plugins [[lein-midje "3.1.3"]]}})
