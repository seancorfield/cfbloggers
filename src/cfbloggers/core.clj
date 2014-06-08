(ns cfbloggers.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clj-http.client :as client]
            [clj-time.coerce :as tc]
            [clj-time.core :as dt]
            [clj-time.format :as fmt]))

(def opml-url "http://www.coldfusionbloggers.org/opml.cfm")

(def counter (atom 0))

(def io (agent nil))

(defn println-1
  [& args]
  (apply send io (fn [_ & args] (apply println args)) args)
  ;; must return nil - we're a substitute for println and some code assumes this!
  nil)

(defn get-opml
  []
  (xml/parse opml-url))

(defn string-as-input-stream
  [^String s]
  (-> s
      (str/replace #"&" "&amp;")
      (.getBytes java.nio.charset.StandardCharsets/UTF_8)
      (java.io.ByteArrayInputStream.)))

(defn parse-rss
  [url]
  (println-1 (str "fetching: " url " (" @counter ")"))
  (try
    (let [response (client/get url {:socket-timeout 60000
                                    :conn-timeout 60000
                                    :throw-exceptions false})]
      (if (= 200 (:status response))
        (try
          (xml/parse (string-as-input-stream (:body response)))
          (catch Exception _
            (println-1 (str "unparsable " url " " _))
            "unparsable"))
        (do
          (println-1 (str "returned status "
                          (:status response) " from " url))
          "not found")))
    (catch Exception _
      (println-1 (str "failed to get " url " " _)))))

(defn maybe-first
  [s]
  (if (string? s) s
    (if (seq s) (first s)
      s)))

(defn parse-blog-raw
  [[title desc url link]]
  (let [header (if (= title desc) title (str title " - " desc))
        slug (str "<a href=\"" link "\">" header "</a>")]
    (if-let [feed (parse-rss url)]
      (if (string? feed)
        [:bad (str slug " - " feed) url]
        (let [stream (drop-while (fn [e] (and (not= :item (:tag e))
                                              (not= :entry (:tag e))))
                                 (xml-seq feed))
              post (-> (for [e stream :when (= :title (:tag e))]
                         (:content e))
                       maybe-first maybe-first)
              raw-date (-> (for [e stream :when (or (= :pubDate (:tag e))
                                                    (= :updated (:tag e))
                                                    (= :dc:date (:tag e))
                                                    (= :published (:tag e)))]
                             (:content e))
                           maybe-first maybe-first)
              date (when raw-date
                     (-> raw-date
                         (str/replace #"--" "-")
                         (str/replace #"([0-9])([-+])([0-9]{4})" "$1 $2$3")
                         (str/replace #"\+([1-9]00)" "+0$1")
                         (str/replace #"([0-9])(P[SD]T|E[SD]T)" "$1 $2")
                         (str/replace #" P[SD]T" " -0800")
                         (str/replace #" E[SD]T" " -0500")
                         (str/replace #" GMT" " +0000")))
              ;; failure to parse -> now which will highlight problems
              date (when (seq stream)
                     (or (fmt/parse date)
                         (do
                           (println-1 (str "bad date? " url " " raw-date))
                           (dt/date-time 1962 7 7))))]
          (if (seq stream)
            [:ok date (str slug " - " date ": " post) url]
            (do
              (println-1 (str "not an rss feed? " url))
              [:bad (str slug " - not an rss feed?") url]))))
      [:gone slug url])))

(defn parse-blog
  [data]
  (swap! counter inc)
  (let [result (parse-blog-raw data)]
    (swap! counter dec)
    result))

(defn parse-opml
  []
  (for [e (xml-seq (get-opml)) :when (= :outline (:tag e))]
    (map (fn [k] (get-in e [:attrs k]))
         [:title :description :xmlUrl :htmlUrl])))

(defn parse-and-sift
  [n]
  (->> (vec (take n (parse-opml)))
       (mapv (fn [blog] (future (parse-blog blog))))
       (mapv deref)
       (group-by first)))

(defn process-blogs
  [n]
  (let [blogs (parse-and-sift n)
        gone  (->> blogs :gone (sort-by second) (map rest))
        bad   (->> blogs :bad (sort-by second) (map rest))
        here  (->> blogs :ok (sort-by second) reverse (map rest))]
    [(count gone) (count bad) (count here)
     gone bad here]))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println-1 "Starting to fetch blog data...")
  (let [n-str (or (first args) "30")
        n     (Long/parseLong n-str)
        [n-gone n-bad n-here gone bad here] (process-blogs n)]
    (let [file  "/Users/sean/Box Sync/corfield.org/articles/cfbloggers.html"
          output (with-out-str
                   (println (str "<p>Of the " (+ n-gone n-bad n-here)
                                 " blogs on "
                                 "<a href=\"http://www.coldfusionbloggers.org/\">ColdFusion Bloggers</a>"
                                 ", " n-here " are still around (but not all of them are covering CFML).</p>"))
                   (println "<p>Here's a list showing the most recent post on each one, ordered by most recent first:</p>")
                   (println "<ul>")
                   (doseq [[_ blog _] here]
                     (println (str "<li>" blog "</li>")))
                   (println "</ul>")
                   (println (str "<p>Then there are " n-bad " that are currently unparsable using my simple code:</p>"))
                   (println "<ul>")
                   (doseq [[blog _] bad]
                     (println (str "<li>" blog "</li>")))
                   (println "</ul>")
                   (println (str "<p>Then there are " n-gone " that have ceased to exist:</p>"))
                   (println "<ul>")
                   (doseq [[blog _] gone]
                     (println (str "<li>" blog "</li>")))
                   (println "</ul>"))]
      (spit file output)
      (println "Output written: FTP to server or open" file))
    (let [file "/Users/sean/Box Sync/corfield.org/articles/cfbloggers.json"
          data (concat (for [[date blog url] here]
                         {:url url :lastpost (-> date tc/to-date .getTime java.sql.Timestamp. str) :hint blog})
                       (for [[blog url] bad]
                         {:url url :lastpost 0 :hint blog})
                       (for [[blog url] gone]
                         {:url url :lastpost 0 :hint blog}))]
      (spit file (json/write-str data))
      (println "JSON written: FTP to server of open " file)))
  (shutdown-agents))
