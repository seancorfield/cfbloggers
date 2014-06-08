(ns cfbloggers.core
  "Scratch code to fetch the OPML feeds list from coldfusionbloggers.org
  and attempt to parse every feed listed (in parallel!) to find the most
  recent blog post (title and date).
  Produces an HTML page listing all of those blog posts, along with a
  list of blogs that could not be parsed, and a list of blogs that could
  not be reached at all (i.e., the RSS feed could not be retrieved)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clj-http.client :as client]
            [clj-time.coerce :as tc]
            [clj-time.core :as dt]
            [clj-time.format :as fmt]))

(def opml-url "http://www.coldfusionbloggers.org/opml.cfm")

(def counter
  "Just to show how many concurrent requests are active while we run."
  (atom 0))

(def io
  "Used to single-thread the console output to make it more readable,
  so that output produced by multiple threads doesn't get interleaved."
  (agent nil))

(defn println-1
  "A single-threaded println. Uses the io agent."
  [& args]
  (apply send io (fn [_ & args] (apply println @counter ")" args)) args)
  ;; must return nil - we're a substitute for println and some code assumes this!
  nil)

(defn get-opml
  "Return data structure representing parsed XML version of OPML feeds list."
  []
  (xml/parse opml-url))

(defn string-as-input-stream
  "Given a string, return an input stream of its bytes.
  Note that we encode & to &amp; to avoid badly formatted blogs."
  [^String s]
  (-> s
      (str/replace #"&" "&amp;")
      (.getBytes java.nio.charset.StandardCharsets/UTF_8)
      (java.io.ByteArrayInputStream.)))

(defn parse-rss
  "Attempt to parse an RSS feeed. Returns nil if the URL doesn't
  resolve or times out. Returns a string if we cannot parse the
  RSS XML (because the request got a 404 Not Found, or the XML
  is invalid for whatever reason. Otherwise returns a data structure
  that represents the parsed XML."
  [url]
  (println-1 (str "fetching: " url))
  (try
    (let [response (client/get url {:socket-timeout 9000
                                    :conn-timeout 9000
                                    :throw-exceptions false
                                    :retry-handler (fn [ex n _]
                                                     (and (not (instance? java.net.UnknownHostException ex))
                                                          (< n 9)))})]
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
  "If the argument is a non-empty sequence (and not a string),
  return the first item of that sequence."
  [s]
  (if (string? s) s
    (if (seq s) (first s)
      s)))

(defn parse-blog-raw
  "Given a tuple of a blog's title, description, RSS feed, and site link,
  return a tuple that indicates the parsed version of the RSS feed:
  :ok date-of-last-post title-etc-and-blog-post url-of-blog
  :gone title-etc url-of-blog
  :bad title-etc-and-failure-reason url-of-blog
  We look for the first <item> or <entry>, then look for the first
  <title> after that to find the first post, and also look for the
  first likely date (based on heuristic gleaned from looking at
  lots of RSS and Atom XML data). Date formates seem to be all over
  the map so we do a fair bit of regex patching to clean things up
  so that RFC822 and a handful of other 'standard' date parsers
  actually stand a chance of making sense of the result."
  [[title desc url link]]
  (let [header (if (= title desc) title (str title " - " desc))
        slug (str "<a href=\"" link "\">" header "</a>")]
    (if-let [feed (parse-rss url)]
      (if (string? feed)
        [:bad (str slug " - " feed) url]
        (let [stream (drop-while (fn [e] (and (not= :item (:tag e))
                                              (not= :entry (:tag e))))
                                 (take 500 (xml-seq feed)))
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
            (do
              (println-1 (str "done! " url))
              [:ok date (str slug " - " date ": " post) url])
            (do
              (println-1 (str "not an rss feed? " url))
              [:bad (str slug " - not an rss feed?") url]))))
      [:gone slug url])))

(defn parse-blog
  "Parse a blog, counting how many active requests we have in parallel."
  [data]
  (swap! counter inc)
  (let [result (parse-blog-raw data)]
    (swap! counter dec)
    result))

(defn parse-opml
  "Parse the OPML feed list and return a sequence of tuples describing
  each blog in the list (title, description, RSS feed, blog link)."
  []
  (for [e (xml-seq (get-opml)) :when (= :outline (:tag e))]
    (map (fn [k] (get-in e [:attrs k]))
         [:title :description :xmlUrl :htmlUrl])))

(defn parse-and-sift
  "For the first N blogs in the OPML feed list, try to parse them
  all in parallel using futures, then 'join' those threads by
  forcing a dereference of all the futures, then group them according
  to success or failure (:ok, :bad, or :gone)."
  [n]
  (->> (take n (parse-opml))
       (mapv (fn [blog] (future (parse-blog blog))))
       (mapv deref)
       (group-by first)))

(defn process-blogs
  "Process the first N blogs on coldfusionbloggers.org and return
  a summary data structure with counts of how many could not be
  reached, how many could not be parsed, and how many were parsed
  successfully, as well as the raw data.
  Frankly, a map would make more sense here and the calling code
  could deal with counts, but this started off as just getting
  the counts, in the REPL and grew into the HTML / JSON producing
  code that follows..."
  [n]
  (let [blogs (parse-and-sift n)
        _     (println-1 "Got all the data, sorting...")
        gone  (->> blogs :gone (sort-by second) (map rest))
        bad   (->> blogs :bad (sort-by second) (map rest))
        here  (->> blogs :ok (sort-by second) reverse (map rest))]
    [(count gone) (count bad) (count here)
     gone bad here]))

(defn -main
  "Given a number of blog feeds (which defaults to 30 for testing),
  analyze that many from coldfusionbloggers.org and produce an HTML
  file summarizing the results, and a JSON file for Ray Camden."
  [& args]
  (println-1 "Starting to fetch blog data...")
  (let [n-str (or (first args) "30")
        n     (Long/parseLong n-str)
        [n-gone n-bad n-here gone bad here] (process-blogs n)]
    (println-1 "Writing files...")
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
