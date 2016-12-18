;; Clojure script to generate csound-opcodes.el
;; Some manual changes may be needed,
;; Zerodbfs needs to be changed to 0dbfs

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
                  bottleneck (-> doc :content)
                  template (find-synopsis bottleneck)]
              (recur (rest docs)
                     (let [strcand (format "(puthash \"%s\" '(:template \"%s\" :doc \"%s\" :html \"%s\") csdoc-opcode-database)"
                                           id (str template) "doc" "seinna")]
                       (if (or (re-find #"null" strcand)
                               (re-find #"ERROR" strcand))
                         out (str out strcand "\n")))))))))

(spit-emacs-file)


;; (count (:content (first (:content parsed-docs))))
;; ((:content ((:content ((:content parsed-docs) 0)) 5)) 1)


