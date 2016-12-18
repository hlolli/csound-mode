;; Clojure script to generate csound-opcodes.el
;; Some manual changes may be needed,
;; Zerodbfs needs to be changed to 0dbfs
;; `tab` and `tb` opcodes need to be split up

(ns xml-to-emacsdb
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.string :as string]))


(def OPCODE-XML-DIR "/home/hlolli/csound/manual/opcodes/")

(def quoted-ampersands
  (loop [docs (rest (file-seq (io/file OPCODE-XML-DIR)))
         xml-string ""]
    (if (empty? docs)
      xml-string
      (recur (rest docs)
             (str xml-string
                  (-> (string/replace
                       (slurp (first docs))
                       "&" "")
                      (string/replace "<command>" "")
                      (string/replace "</command>" "")))))))

(defn parse [s]
  (xml/parse
   (java.io.ByteArrayInputStream. (.getBytes s))))

(def parsed-docs (parse (str "<manual>" quoted-ampersands "</manual>")))

(defn find-synopsis [vek]
  (loop [v vek]
    (if (empty? v)
      "ERROR"
      (if (= (:tag (second (:content (first v)))) :synopsis)
        (apply str (seq (interpose "\n"
                                   (filter #(not (nil? %))
                                           (apply vector
                                                  (for [x (:content (first v))]
                                                    (when (= :synopsis (:tag x))
                                                      (-> (first (:content x))
                                                          (string/replace "\n" "")
                                                          (string/replace "\t" "")
                                                          (string/replace "\\" "")
                                                          (string/replace (re-pattern "\\s+") " ")
                                                          (string/trim)))))))))
        (recur (rest v))))))

(defn find-docstring [vek]
  (loop [v vek]
    (if (empty? v)
      ""
      (if (= (:tag (second (:content (first v)))) :refpurpose)
        (str (nth (:content (first v)) 3))
        (recur (rest v))))))

(defn fix-csound-names [id-str]
  (case id-str
    "Zerodbfs" "0dbfs"
    "notequal" "!="
    "equals" "=="
    id-str))

(defn spit-emacs-file []
  (spit "/home/hlolli/.emacs.d/hlolli/csound-mode/csound-opcodes.el" ;;"csound-opcodes.el"
        (loop [docs (:content parsed-docs)
               out ""]
          (if (empty? docs)
            (str
             "(setq csdoc-opcode-database (makehash 'equal))\n"
             out
             "\n (provide 'csound-opcodes)")
            (let [doc (first docs)
                  id (-> doc :attrs :id)
                  id (fix-csound-names id)
                  template (find-synopsis (-> doc :content))
                  docstring (if-not (< 3 (count (-> doc :content)))
                              ""
                              (let [untrimmed (-> doc
                                                  :content (nth 3)
                                                  :content second
                                                  :content first)]
                                (if (string? untrimmed)
                                  (string/trim untrimmed)
                                  (if (map? untrimmed)
                                    (-> untrimmed
                                        :content
                                        first
                                        string/trim)
                                    (str "WTF: " untrimmed)))))]
              (recur (rest docs)
                     (let [strcand (format "(puthash \"%s\" '(:template \"%s\" :doc \"%s\") csdoc-opcode-database)"
                                           id (str template) docstring)]
                       (if (or (re-find #"null" strcand)
                               (re-find #"ERROR" strcand)
                               (re-find #";" strcand))
                         out (str out strcand "\n")))))))))

(spit-emacs-file)


;; (:content ((:content (first (:content parsed-docs))) 3))
;; ((:content ((:content ((:content parsed-docs) 0)) 5)) 1)


