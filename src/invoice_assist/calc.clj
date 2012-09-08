(ns invoice-assist.calc)

(defn- safe-sub [a b]
  (max (- a b) 0.0))

(defn- between? [x a b]
  (cond (< a b) (and (<= a x) (<= x b))
        (< b a) (and (<= b x) (<= x a))
        :else (and (not (< x a)) (not (< a x)))))

(defn- indexfy [l]
  (map vector (range) l))

(defn- unindexfy [l]
  (map #(second %) l))

(defn- select-opt [total a b]
  (let [sum-a (apply + (unindexfy a))
        sum-b (apply + (unindexfy b))]
    (cond (and (< sum-a total) (< sum-b total))
          (if (> sum-a sum-b) a b)
          (between? total sum-a sum-b)
          (let [diff-a (Math/abs (- total sum-a))
                diff-b (Math/abs (- total sum-b))]
            (cond (< diff-a diff-b) a
                  (< diff-b diff-a) b
                  :else (if (> sum-a sum-b) a b)))
          :else
          (if (< sum-a sum-b) a b))))

(defn- calc-optimal [total invoice-list]
  (cond (empty? invoice-list) nil
        (= 0.0 (double total)) nil
        :else (let [[idx cur] (first invoice-list)]
                (select-opt total
                            (cons [idx cur] (calc-optimal (safe-sub total cur)
                                                          (next invoice-list)))
                            (calc-optimal total (next invoice-list))))))

(defn- sub-invoices [a b]
  (let [index-set (set (map first b))]
    (filter #(-> % first index-set not) a)))

(defn calc-inovice-combination [total invoices]
  (let [indexed-invoices (indexfy invoices)
        opt (calc-optimal total indexed-invoices)
        opt-result (unindexfy opt)]
    (if (< (apply + opt-result) total)
      (let [remains (sub-invoices indexed-invoices opt)]
        (if (not (empty? remains)) 
          [opt-result (apply min (unindexfy remains))]
          [opt-result nil]))
      [opt-result nil])))

(defn- test [total invoice-list]
  (let [[opt sub-opt] (calc-inovice-combination total invoice-list)]
    (if sub-opt
      (list total (apply + opt) opt (+ (apply + opt) sub-opt) (cons sub-opt opt))
      (list total (apply + opt) opt))))

;(calc-optimal 330 '(300 400))

;; (test 0 '(1 2 3 4 5))
;; (test 1 '(3 9))
;; (test 1 '(3 4))
;;; (test 100 '(550 230))
;; (test 10.0 '(1 1 1 1 1 1 1 1 1.1 50 40 30 20 15))
;; (test 330.0 '(97 88 64 17.5 14.5 18.5 23.7 21.5 1.2 9.0))
;; (test 330.0 '(24.5 18.5 97 88 64.1 17.5  23.7 21.5 1.2 10.0))
;; (test 330.0 '(159 200.3 188.6 129.7))
