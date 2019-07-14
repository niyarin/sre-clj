(ns sre-clj.core)

(defmacro error [msg & [data]]
  `(throw (ex-info (str "Error:" ~msg) {:causes ~data})))

(declare valid-sre?)

(defn valid-sre-seq? [sre-list]
  "Aux function for valid-sre?"
  (if (and 
        (seq? sre-list)
        (not (empty? sre-list)))

        (reduce 
          (fn [a b] ;TODO: Stop recursion , if invalid pattern is passed.
            (and
              a
              (valid-sre? b)))
          true
          sre-list)
        false))

(defn valid-sre-core? [sre-code]
  "Aux function for valid-sre?"
  (cond 
      (seq? sre-code)
        (case (first sre-code)
          (* zero-or-more + one-or-more ? optional)
            (valid-sre-seq? sre-code)

          (exactory = >= at-least)
            (and
              (not (empty? (next sre-code)))
              (number? (second sre-code))
              (valid-sre-seq? (nnext sre-code)))
          (repeated **)
            (and
              (not (empty? (next sre-code)))
              (number? (second sre-code))
              (not (empty? (nnext sre-code)))
              (number? (nth sre-code 3))
              (valid-sre-seq? (next (nnext sre-code))))
          (\: seq or)
            (valid-sre-seq? (next sre-code))
            )

      (string? sre-code)
        true
      (symbol? sre-code)
        '()
      :else 
        false))

(defn valid-sre? [sre-code]
  (or (valid-sre-core? sre-code)
      ;TODO:CSET
      ))

(defn aux-cont [cont sre-code]
  (cond
    (not cont)
      (case (first sre-code)
        (\: seq * zero-or-more or)
          (next sre-code)
          )
    :default
        (if (and (seq? cont) (first cont)) cont '())
    ))

(defn reg-tail [operator]
  (case operator
    (\: seq)
      ""
    (* zero-or-more)
      "*"
    (or)
      ""
      ))

(defn reg-escape [s]
  (clojure.string/replace 
    s
    #"[()!\\\[\]]"
    (fn [rvs]
      (str \\ (first rvs)))))

(defn aux-sre->regexp-string
  "Convert SRE to Java Regexp string."
  [sre-code env]
  (loop [sre-code sre-code
         continuation false
         res ""
         env env
         stk '()]

    (let* [cont 
             (if (seq? sre-code) 
               (aux-cont continuation sre-code) 
               '(() ()))
           sre-start (not continuation)
           first-cont (first cont)
           rest-cont (rest cont)
           ]

      (cond
        (and (seq? sre-code) (not (empty? first-cont)))
          (case (first sre-code)
            (\: seq * zero-or-more + one-or-more)
              (recur 
                first-cont
                false
                (str 
                  res
                  (if sre-start "(?:"  "")
                  )
                env
                (cons 
                  (cons sre-code rest-cont)
                  stk))
            (or)
              (recur
                first-cont
                false
                (str 
                  res
                  (if sre-start "(?:" "|"))
                env
                (cons 
                  (cons sre-code rest-cont)
                  stk))
              )
        (seq? sre-code)
          (case (first sre-code)
            (\: seq * zero-or-more or)
              (recur
                (ffirst stk)
                (rest (first stk))
                (str res ")" (reg-tail (first sre-code)) )
                env
                (rest stk)))

        (string? sre-code)
          (recur
            (ffirst stk)
            (rest (first stk))
            (str res (reg-escape sre-code))
            env
            (rest stk))

        (empty? sre-code)
          res
          ))))

(defmacro sre-literal [sre-code]
  `(let [sre-code# ~sre-code]
      (if (valid-sre? sre-code#)
         (aux-sre->regexp-string sre-code# (list {} 0))
         (error "Invalid sre." sre-code#))))

(defn sre [sre-code]
  "Converting  sre to Java Regexp string"
  (if (valid-sre? sre-code)
     (aux-sre->regexp-string sre-code (list {} 0))
     (error "Invalid sre." sre-code)))
