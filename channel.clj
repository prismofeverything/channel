(defstruct plasm :concentrations)
(defstruct membrane :inside :outside :channels)
(defstruct channel :permeability :activity :membrane)
(defstruct ion :type :valence)
(defstruct concentration :type :level)

(def ions 
     (map #(struct :ion (% 0) (% 1)))
     [[:A -1]
      [:Na 1]
      [:Cl -1]
      [:K 1]
      [:Ca 2]])

(defn valence-of 
  [type]
  (let [ion (some #(= type (:type %)) ions)]
    (:valence ion)))

(defn make-plasm
  [concentrations]
  (struct plasm 
	  (let [contents (hash-map)]
	    (reduce (fn [contents concentration] 
			(assoc contents 
			  (:type concentration) 
			  (ref concentration)))
		    content 
		    concentrations))))

(defn level-of
  [type plasm]
  (let [concentration (get (:concentrations plasm) type)]
    (:level concentration)))

(defn nernst
  [membrane type]
  (let [in (type (:inside membrane))
	out (type (:outside membrane))
	ratio (. Math (log (/ out in)))
	valence (:valence (type ions))]
    (* valence ratio)))
    


  



