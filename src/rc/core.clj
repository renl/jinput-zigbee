(ns rc.core
  (:require [popen :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [serial.core :refer :all]
            [serial.util :refer :all])
  (:import [net.java.games.input ControllerEnvironment])
  (:gen-class))

(def str-to-send (atom ""))

(defn gamepad-xboxdrv []
  (let [p (popen/popen ["xboxdrv"])
        out (popen/stdout p)]
    (doseq [line (line-seq (io/reader out))]
      (let [throttle (second (re-matches #".*Y1:\s*([-\d]+)\s.*" line))
            steer (second (re-matches #".*X2:\s*([-\d]+)\s.*" line))]
        (when throttle
          (let [throttle (read-string throttle)
                steer (read-string steer)
                throttle (cond
                          (< (Math/abs throttle) 5000) 0.0
                          (> (Math/abs throttle) 30000) (Math/signum (double throttle))
                          :else (/ throttle 30000.0))
                steer (cond
                       (< (Math/abs steer) 5000) 0.0
                       (> (Math/abs steer) 30000) (Math/signum (double steer))
                       :else (/ steer 30000.0))]
            (reset! str-to-send (format "#%.2f|%.2f\n" throttle steer)))))))
  (println "end"))

(defn gamepad-jstest []
  (let [p (popen/popen ["jstest" "/dev/input/js0"])
        out (popen/stdout p)]
    (doseq [line (line-seq (io/reader out))]
      ;; (println line)
      (let [throttle (second (re-matches #"Axes:.*1:\s*([-\d]+)\s.*" line))
            steer (second (re-matches #"Axes.*2:\s*([-\d]+)\s.*" line))]
        (when throttle
          (let [throttle (read-string throttle)
                steer (read-string steer)
                throttle (- throttle)
                throttle (cond
                          (< (Math/abs throttle) 10000) 0.0
                          (> (Math/abs throttle) 32767) (Math/signum (double throttle))
                          :else (/ throttle 32767.0))
                steer (cond
                       (< (Math/abs steer) 10000) 0.0
                       (> (Math/abs steer) 32767) (Math/signum (double steer))
                       :else (/ steer 32767.0))]
            ;; (println throttle steer)
            (reset! str-to-send (format "#%.2f|%.2f\n" throttle steer)))))))
  (println "end"))

(defn find-controller []
  (.getControllers (ControllerEnvironment/getDefaultEnvironment)))

(defn gamepad-jinput [controller]
  (println (.getName controller))
  (let [components (.getComponents controller)
        rumbler (first (.getRumblers controller))
        throttle-comp (first (filter #(= "y" (.getName %)) components))
        steer-comp (first (filter #(= "z" (.getName %)) components))]
    (loop []
      (.poll controller)
      (let [throttle (.getPollData throttle-comp)
            throttle (if (zero? throttle)
                       throttle (- throttle))
            steer (.getPollData steer-comp)]
        ;; (println throttle steer)
        (if (> throttle 0.9) (.rumble rumbler 1) (.rumble rumbler 0))
        (reset! str-to-send (format "#%.2f|%.2f\n" throttle steer)))
      (Thread/sleep 100)
      (recur))))

(defn write-to-port []
  (let [port (open "/dev/ttyUSB0" :baud-rate 19200)]
    (loop []
      (when (not-empty @str-to-send)
        (write port (map byte @str-to-send)))
      (Thread/sleep 200)
      (recur))))

(defn -main
  [& args]
  (future (gamepad-jinput (first (find-controller))))
  (future (write-to-port))
  )
