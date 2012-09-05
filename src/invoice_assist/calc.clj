(ns 'invoice-assist.calc)

(defn- safe-sub [a b]
  (max (- a b) 0.0))

(defn- between? [x a b]
  (cond (< a b) (and (<= a x) (<= x b))
        (< b a) (and (<= b x) (<= x a))
        :else (and (not (< x a)) (not (< a x)))))

(defn- select-opt [total opt-a opt-b]
  (let [sum-a (apply + opt-a)
        sum-b (apply + opt-b)]
    (cond (and (< sum-a total) (< sum-b total))
          (if (> sum-a sum-b) opt-a opt-b)
          (between? total sum-a sum-b)
          (let [diff-a (Math/abs (- total sum-a))
                diff-b (Math/abs (- total sum-b))]
            (cond (< diff-a diff-b) opt-a
                  (< diff-b diff-a) opt-b
                  :else (if (> sum-a sum-b) opt-a opt-b)))
          :else
          (if (< sum-a sum-b) opt-a opt-b))))
          
(defn calc-optimal [total invoice-list]
  (cond (empty? invoice-list) nil
        (= 0 total) (list (min invoice-list))
        :else (let [cur (first invoice-list)]
                (select-opt total (cons cur (calc-optimal (safe-sub total cur)
                                                          (next invoice-list)))
                            (calc-optimal total (next invoice-list))))))


(defn- test [total invoice-list]
  (let [opt (calc-optimal total invoice-list)]
    (cons total (cons (apply + opt) opt))))
