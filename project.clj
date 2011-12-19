(defproject data.tree "0.0.1-SNAPSHOT"
  :description "Implementations of persistent tree structures"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [slingshot "0.10.0"]
                 [org.clojure/core.match "0.2.0-alpha8"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [clojure-source "1.3.0"]
                     [radagast "1.1.1"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  ) 