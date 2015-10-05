(def sql (slurp "sql.txt" :encoding "GBK"))

(defn split-sql [sql] (let [i1 (.indexOf sql "\u8868\u540d\u79f0")
                            i2 (.indexOf sql "," i1) 
                            i3 (.indexOf sql "\u5217\u540d" i2)
                            i4 (.indexOf sql "\n" i3) 
                            i5 (.indexOf sql ",,,,,," i4)]
      (when (< i1 i2 i3 i4) [[(.substring sql (+ i1 4) i2) (.trim (if (> i5 0) (.substring sql i4 (- i5 1)) (.substring sql i4)))] (if (> i5 0) (.substring sql i5) "")] )
      ))

(defn parse-sql [sql] (let [[x s] (split-sql sql)] (when x (cons x (lazy-seq (parse-sql s))))))

;;; [table table ...]
(def table-list (parse-sql sql))

;;;;;;;;;;

;; f:field, f1_f2 -> f1F2
(defn convert [f]
    (let [i (.indexOf f "_")]
        (if (> i 0)
            (let [l (.substring f 0 i)
                  m (.charAt f (inc i))
                  r (.substring f (+ 2 i))]
               (convert (str l (Character/toUpperCase m) r))
           )
        f)))

(defn capitalize [form] (apply str (cons (Character/toUpperCase (first form)) (rest form))))

(defn fill-form [xml reg value] (.replaceAll xml reg value))

;;;;;;;;;;

(def cfg (slurp "cfg.txt"))
(def cfg-m (read-string cfg))

(def table (:table cfg-m))
(def form (convert table))
(def Form (capitalize form)) 


;;;;;;;;;;
;;; #(for [[k v] %] [k (%2 v)]) ; s:table-list, f:str-to-struct
(defn convert-v-of-kv [s f] (map (juxt first (comp f second)) s))

(defn convert-v-of-kv-map [s f] (convert-v-of-kv s (partial map f)))

(defn comments [s] (if (> (count s) 9) (apply str (interpose "," (drop 7 s))) (last s)))

;;; "" -> (("" "" "") ...)
(defn str-to-struct [s] (map #((juxt second first comments) (.split % ",")) (.split s "\n")))

;;; "bigint(20) unsigned" -> "Long"
(defn map-to-type [x] 
  (if x
  (condp #(< -1 (.indexOf %2 %)) x
    "bigint" "Long"
    "int" "Integer"
    "varchar" "String"
    "text" "String"
    "date" "Date"
    "decimal" "BigDecimal"
    "double" "Double"
    "Object"
    ) ""))

(defn trim [x] (if x (.trim x) ""))

;;; ["" ""] -> ["" "" "" ""] ; s:("bigint(20) unsigned" "id" "ID CODE") f:map-to-type 
(defn to-field-line [f f2 [t x e]] 
  ["\tprivate" (f t) (f2 x) ";" "//" (trim e) "\n\n"])

(def table-field-line (-> table-list (convert-v-of-kv str-to-struct) (convert-v-of-kv (partial map (partial to-field-line map-to-type convert)))))

(def table-entity (-> table-field-line (convert-v-of-kv (partial map (partial interpose " "))) (convert-v-of-kv (partial map #(concat (take 5 %) (drop 6 %))))))

(def template-GetSet (slurp "template/GetSet.template"))
(def table-entity-getset (-> table-field-line 
                             (convert-v-of-kv-map 
                               (fn [[_ t x]] 
                                 (-> template-GetSet 
                                     (fill-form "[$]\\[type\\]" t)
                                     (fill-form "[$]\\[field\\]" x)
                                     (fill-form "[$]\\[Field\\]" (capitalize x)))))))

(def getset-text (apply str ((into {} table-entity-getset) table)))
(def table-tuple-map (into {} table-entity))
(def fields-list (table-tuple-map table))
(def fields-text (apply str (flatten fields-list)))

;;;;;;;;;

;;; (table | "id,int\ndesc,varchar\n") -> [id,desc]
;;; Table String To Stuct;
(defn str-to-seq [s] (->> (.split s "\n") (map #(first (.split % ","))) ))

(def map-list (map #(let [[t s] %] {t (str-to-seq s)}) table-list)) 

(def table-map (reduce into map-list))

;;;;;;;;;

(def field-seq (table-map table))

(def template-sqlmap (slurp "template/sqlmap_form.template"))
(def template-DO_doQueryParams (slurp "template/DO_doQueryParams.template"))
(def template-update_condition (slurp "template/update_condition.template"))
(def template-DOMap (slurp "template/DOMap.template"))
(def template-Form-DAO (slurp "template/FormDAO.template"))
(def template-FormService (slurp "template/FormService.template"))
(def template-FormServiceImpl (slurp "template/FormServiceImpl.template"))
(def template-xml-cfg (slurp "template/xml-cfg.template"))
(def template-FormQuery (slurp "template/FormQuery.template"))
(def template-FormDO (slurp "template/FormDO.template"))

(defn concat-form [src-seq separator transform-fn]
    (->> (map #(transform-fn %) src-seq) (reduce #(str % separator %2))))

;; xml: xml form field, f: original field
(defn fill-condition-field [xml f]
  (let [f2 (convert f)]
    (-> (.replaceAll xml "[$]1[$]" f) (.replaceAll "[$]2[$]" f2))))

(defn date-to-now? [x] (or (= "gmt_create" x) (= "gmt_modified" x)))

(def DOMap (concat-form field-seq "" #(fill-condition-field template-DOMap %)))
(def fields (concat-form field-seq "," identity))
(def query-params (concat-form field-seq "" #(fill-condition-field template-DO_doQueryParams %)))
(def insert_values (concat-form field-seq "," (fn [x] (if (date-to-now? x) "now()" (str "#" (convert x) "#") ))))
(def batch_insert_values (concat-form field-seq "," (fn [x] (if (date-to-now? x) "now()" (str "#[]." (convert x) "#")))))
(def update_condition (concat-form (rest field-seq) "\n" #(fill-condition-field template-update_condition %)))

(def reg-value-map [["[$][{]form[}]" form] 
                    ["[$][{]Form[}]" Form] 
                    ["[$][{]table[}]" table]
                    ["[$]\\[DOMap\\]" DOMap] 
                    ["[$]\\[DO_AllFields\\]" fields] 
                    ["[$]\\[DO_doQueryParams\\]" query-params] 
                    ["[$]\\[insert_values\\]" insert_values] 
                    ["[$]\\[batch_insert_values\\]" batch_insert_values] 
                    ["[$]\\[update_condition\\]" update_condition] 
                    ["[$]\\[fields\\]" fields-text] 
                    ["[$]\\[GetSet\\]" getset-text] 
                    ])

(defn sqlmap [template] (reduce #(apply fill-form % %2) template reg-value-map))

(def sqlmap-xml (sqlmap template-sqlmap))
(def dao (sqlmap template-Form-DAO))
(def Service (sqlmap template-FormService))
(def ServiceImpl (sqlmap template-FormServiceImpl))
(def xml-cfg (sqlmap template-xml-cfg))
(def FormQuery (sqlmap template-FormQuery))
(def FormDO (sqlmap template-FormDO))


(println "\n***OK***\n")

(spit (str "code/sqlmap_" table ".xml") sqlmap-xml)
(spit (str "code/" Form "DAO.java") dao)
(spit (str "code/" Form "Service.java") Service)
(spit (str "code/" Form "ServiceImpl.java") ServiceImpl)
(spit (str "code/" table "-xml-cfg.txt") xml-cfg)
(spit (str "code/" Form "Query.java") FormQuery)
(spit (str "code/" Form "DO.java") FormDO)

