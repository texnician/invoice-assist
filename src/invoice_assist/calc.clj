(ns invoice-assist.calc)

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
        (= 0.0 (double total)) nil
        :else (let [cur (first invoice-list)]
                (select-opt total
                              (cons cur (calc-optimal (safe-sub total cur)
                                                      (next invoice-list)))
                            (calc-optimal total (next invoice-list))))))

(defn calc-inovice-combination [total invoices]
  (let [opt (calc-optimal total invoices)]
    (if (and (empty? opt) (not (empty? invoices)))
      (list (apply min invoices))
      opt)))

(defn- test [total invoice-list]
  (let [opt (calc-inovice-combination total invoice-list)]
    (cons total (cons (apply + opt) opt))))

;(calc-optimal 330 '(300 400))

;; (test 0 '(1 2 3 4 5))
;; (test 1 '(3 9))
;;; (test 100 '(550 230))
;; (test 10.0 '(1 1 1 1 1 1 1 1 1.2 1.5))
;; (test 330.0 '(97 88 64 17.5 14.5 18.5 23.7 21.5 1.2 9.0))
;; (test 330.0 '(24.5 18.5 97 88 64.1 17.5  23.7 21.5 1.2 10.0))
;; (test 330.0 '(159 200.3 188.6 129.7))
