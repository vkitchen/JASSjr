#!/usr/bin/env -S clojure -M

(ns jassjr
  (:require [clojure.string :as str])
  (:import
   [java.io BufferedReader]
   [java.nio ByteBuffer ByteOrder]
   [java.nio.file Files Paths]
   [java.nio.charset StandardCharsets]
   [java.util Comparator Locale PriorityQueue]))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:const k1 0.9)
(def ^:const b 0.4)
(def ^:const max-top-k 1000)

;; ---------------------------------------------------------------------------
;; Binary file utilities
;; ---------------------------------------------------------------------------

(defn read-buffer
  "Read a binary file into a little-endian ByteBuffer."
  [filename]
  (-> (Files/readAllBytes (Paths/get filename (make-array String 0)))
      ByteBuffer/wrap
      (.order ByteOrder/LITTLE_ENDIAN)))

(defn unsigned-int
  "Interpret a signed Java int as an unsigned 32-bit integer."
  [n]
  (Integer/toUnsignedLong n))

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(defn load-vocabulary
  "Load the vocabulary.

   Record layout:
     uint8   token-length
     bytes   token
     uint8   NUL terminator
     uint32  postings offset
     uint32  postings size (number of pairs)"
  [filename]
  (let [^ByteBuffer buffer (read-buffer filename)
        vocab (transient {})]
    (while (.hasRemaining buffer)
      (let [token-length (bit-and 0xff (.get buffer))
            token-bytes (byte-array token-length)]
        (.get buffer token-bytes)

        ;; Consume NUL terminator.
        (.get buffer)

        (let [offset (unsigned-int (.getInt buffer))
              size   (unsigned-int (.getInt buffer))
              token  (String. token-bytes StandardCharsets/UTF_8)]
          (assoc! vocab token
                  {:offset offset
                   :size size}))))
    (persistent! vocab)))

;; ---------------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------------

(defn load-docnos
  "Read external document identifiers, one per line."
  [filename]
  (with-open [reader (Files/newBufferedReader
                      (Paths/get filename (make-array String 0))
                      StandardCharsets/UTF_8)]
    (persistent!
     (loop [result (transient [])
            line (.readLine reader)]
       (if (nil? line)
         result
         (recur (conj! result line)
                (.readLine reader)))))))

(defn load-doclengths
  "Read raw little-endian uint32 document lengths."
  [filename]
  (let [^ByteBuffer buffer (read-buffer filename)
        n (quot (.remaining buffer) Integer/BYTES)
        lengths (long-array n)]
    (dotimes [i n]
      (aset-long lengths i (unsigned-int (.getInt buffer))))
    lengths))

;; ---------------------------------------------------------------------------
;; Tokenization
;; ---------------------------------------------------------------------------

(defn tokenize
  "Extract lowercase alphanumeric terms from a query."
  [query]
  (->> (re-seq #"[\p{L}\p{N}]+" query)
       (map #(str/lower-case %))))

;; ---------------------------------------------------------------------------
;; BM25
;; ---------------------------------------------------------------------------

(defn bm25-idf
  [documents document-frequency]
  (Math/log (/ (double documents)
               (double document-frequency))))

(defn bm25-term-score
  [tf document-length average-length idf]
  (let [tf (double tf)
        normalization
        (+ (- 1.0 b)
           (* b (/ (double document-length)
                   average-length)))]
    (* idf
       (/ (* tf (+ k1 1.0))
          (+ tf (* k1 normalization))))))

;; ---------------------------------------------------------------------------
;; Top-K accumulator
;; ---------------------------------------------------------------------------

(defn top-k-comparator
  "Lowest score first; for equal scores, highest docid first.
   Thus the queue head is always the worst retained result."
  []
  (reify Comparator
    (compare [_ a b]
      (let [score-order
            (Double/compare (double (first a))
                            (double (first b)))]
        (if (zero? score-order)
          (Long/compare (long (second b))
                        (long (second a)))
          score-order)))))

(defn top-k
  "Return at most k [score docid] pairs, ranked by descending score."
  [scores k]
  (let [queue (PriorityQueue. (int (max 1 k))
                              (top-k-comparator))]
    (doseq [[docid score] scores
            :when (pos? (double score))]
      (let [candidate [score docid]]
        (cond
          (< (.size queue) k)
          (.offer queue candidate)

          (pos? (.compare (.comparator queue)
                          candidate
                          (.peek queue)))
          (do
            (.poll queue)
            (.offer queue candidate)))))
    (->> (iterator-seq (.iterator queue))
         (sort-by (fn [[score docid]]
                    [(- (double score)) docid]))
         vec)))

;; ---------------------------------------------------------------------------
;; Search
;; ---------------------------------------------------------------------------

(defn search
  "Search the index for query terms.

   Returns a vector of [score docid] pairs."
  [{:keys [vocabulary postings doclengths average-length documents]}
   query
   & {:keys [limit] :or {limit max-top-k}}]

  (let [terms (distinct (tokenize query))
        scores
        (reduce
         (fn [scores term]
           (if-let [{:keys [offset size]} (get vocabulary term)]
             (let [df size]
               (if (zero? df)
                 scores
                 (let [idf (bm25-idf documents df)]
                   (loop [i 0
                          scores scores]
                     (if (>= i df)
                       scores
                       (let [position (+ offset (* i 8))
                             tf (unsigned-int
                                 (.getInt ^ByteBuffer postings
                                          (int position)))
                             docid (unsigned-int
                                    (.getInt ^ByteBuffer postings
                                             (int (+ position 4))))
                             document-length
                             (aget ^longs doclengths (int docid))
                             score
                             (bm25-term-score
                              tf document-length average-length idf)]
                         (recur (inc i)
                                (assoc! scores docid
                                        (+ (double (get scores docid 0.0))
                                           score)))))))))
             scores))
         (transient {})
         terms)]
    (top-k (persistent! scores) limit)))

;; ---------------------------------------------------------------------------
;; Index initialization
;; ---------------------------------------------------------------------------

(defn open-index
  "Load an index into memory."
  [directory]
  (let [path #(str directory "/" %)
        vocabulary (load-vocabulary (path "vocab.bin"))
        postings (read-buffer (path "postings.bin"))
        docnos (load-docnos (path "docids.bin"))
        doclengths (load-doclengths (path "lengths.bin"))
        documents (count docnos)
        total-length (reduce + 0 doclengths)
        average-length (/ (double total-length)
                          (double documents))]

    (when-not (= documents (alength doclengths))
      (throw (ex-info "Document count does not match document lengths."
                      {:docnos documents
                       :doclengths (alength doclengths)})))

    {:vocabulary vocabulary
     :postings postings
     :docnos docnos
     :doclengths doclengths
     :documents documents
     :average-length average-length}))

;; ---------------------------------------------------------------------------
;; TREC output
;; ---------------------------------------------------------------------------

(defn format-result
  [query-id docnos rank [score docid]]
  (format Locale/ROOT
          "%s Q0 %s %d %.2f JASSjr"
          query-id
          (nth docnos (int docid))
          rank
          (double score)))

(defn print-results
  [query-id docnos results]
  (doseq [[rank result] (map-indexed vector results)]
    (println (format-result query-id docnos (inc rank) result))))

;; ---------------------------------------------------------------------------
;; Interactive query processing
;; ---------------------------------------------------------------------------

(defn parse-query-line
  "Parse a line containing a query ID followed by query text."
  [line]
  (when-let [[_ query-id query]
             (re-matches #"^\s*(\S+)\s+(.+?)\s*$" line)]
    [query-id query]))

(defn run-queries
  [index ^BufferedReader reader]
  (loop []
    (when-let [line (.readLine reader)]
      (when-let [[query-id query] (parse-query-line line)]
        (let [results (search index query)]
          (print-results query-id (:docnos index) results)))
      (recur))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main
  [& args]
  (let [[directory] args]
    (when-not directory
      (throw (ex-info "Usage: clojure -M -m jassjr <index-directory>"
                      {})))
    (let [index (open-index directory)]
      (binding [*out* *out*]
        (run-queries index
                     (BufferedReader.
                      (java.io.InputStreamReader.
                       System/in
                       StandardCharsets/UTF_8)))))))

(apply -main *command-line-args*)
