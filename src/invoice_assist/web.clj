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

(defn- gen-result-page [total-str invoices-str]
  (cond (empty? total-str) (str "<span style=\"color:red\">金额不能为空</span></br>"
                                "<a href=\".\">重置</a>")
        (empty? invoices-str) (str "<span style=\"color:red\">发票列表不能为空</span></br>"
                                   "<a href=\".\">重置</a>")
        :else (let [total (Double/parseDouble total-str)
                    invoices (map #(Double/parseDouble %) (clojure.string/split invoices-str #"\s+"))]
                (let [opt (calc-inovice-combination total invoices)]
                  (str (format "期望金额:  %s </br> 实际金额:   %s </br> 最佳组合:   %s </br>" total (apply + opt) (clojure.string/join "    " opt))
                       "<a href=\".\">重置</a>")))))

(defn *app* [{:keys [uri params]}]
  (if (empty? params)
    {:headers {"Content-Type" "text/html; charset=utf-8"} :body *main-page*}
    {:headers {"Content-Type" "text/html; charset=utf-8"} :body (gen-result-page (params "total")
                                                                                 (params "invoices"))}))

(def app (wrap-params *app*))

; (def server (run-jetty #'app {:port 8888 :join? false}))
; (.stop server)