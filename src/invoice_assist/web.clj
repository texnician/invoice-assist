(ns invoice-assist.web
  (:use (invoice-assist calc)))
(use '[ring.adapter.jetty :only (run-jetty)]
     '[ring.middleware.params :only (wrap-params)])

(def ^:dynamic *main-page*
  (str "<form name=\"input\" action=\"\" method=\"get\">"
       "总金额: <input type=\"text\" name=\"total\"/></br>"
       ;; "发票金额列表(空格分隔): <input type=\"text\" name=\"invoices\"/>"
       "发票金额列表(空格或换行分隔):</br> <textarea name=\"invoices\" cols=40 rows=6></textarea></br>"
       "<input type=\"submit\" value=\"运气不错\" />" 
       "</form>"))

(defn- diff-value-str [a b]
  (let [diff (- a b)]
    (cond (< diff 0.0) (format "(<span style=\"color:red\">-%.2f</span>)" (Math/abs diff))
          (< 0.0 diff) (format "(<span style=\"color:green\">+%.2f</span>)" diff)
          :else "")))

(defn- gen-result-page [total-str invoices-str]
  (try (cond (empty? total-str) (str "<span style=\"color:red\">金额不能为空</span></br>"
                                     "<a href=\".\">重置</a>")
             (empty? invoices-str) (str "<span style=\"color:red\">发票列表不能为空</span></br>"
                                        "<a href=\".\">重置</a>")
             :else (let [total (Double/parseDouble total-str)
                         invoices (map #(Double/parseDouble %) (clojure.string/split (clojure.string/trim invoices-str) #"\s+"))]
                     (let [[opt sub-opt] (calc-inovice-combination total invoices)
                           opt-sum (float (apply + opt))]
                       (str "<pre>"
                            (format "期望金额: %.2f</br>" total)
                            (if sub-opt
                              (format "最优组合: %.2f%s [%s]</br>次优组合: %.2f%s [%s]</br>"
                                      opt-sum (diff-value-str opt-sum total) (clojure.string/join ", " opt)
                                      (+ sub-opt opt-sum) (diff-value-str (+ sub-opt opt-sum) total) (clojure.string/join ", " (cons sub-opt opt)))
                              (format "最优组合: %.2f%s [%s]</br>" opt-sum (diff-value-str opt-sum total) (clojure.string/join ", " opt)))
                            "</pre>"
                            "<pre><a href=\".\">重置</a></pre>"))))
       (catch Exception e
         (str "<pre>"
              (format "<strong style=\"color:red\">%s</strong>" e)
              "</pre>"
              "<pre><a href=\".\">重置</a></pre>"))
       (finally "<pre><a href=\".\">重置</a></pre>")))

(defn *app* [{:keys [uri params]}]
  (if (empty? params)
    {:headers {"Content-Type" "text/html; charset=utf-8"} :body *main-page*}
    {:headers {"Content-Type" "text/html; charset=utf-8"} :body (gen-result-page (params "total")
                                                                                 (params "invoices"))}))

(def app (wrap-params *app*))

; (def server (run-jetty #'app {:port 8888 :join? false}))
; (.stop server)