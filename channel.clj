(defstruct plasm :contents :membranes)
(defstruct membrane :inside :outside :channels)
(defstruct channel :permeability :activity :membrane)
(defstruct ion :type :valence)
(defstruct concentration :ion :level)

(def ions 
     (map (fn [spec] (struct :ion (spec 0) (spec 1))))
     [[:A -1]
      [:Na 1]
      [:Cl -1]
      [:K 1]
      [:Ca 2]])

(defn nernst
  [membrane type]
  (let [in (type (:inside membrane))
	out (type (:outside membrane))
	ratio (. Math (log (/ out in)))
	valence (:valence (type ions))]
    (* valence ratio)))
    

(defn make-plasm
  [concentrations]
  (struct plasm 
	  (let [content (hash-map)]
	    (reduce (fn [c spec]
			(assoc content c spec))
		    concentrations))
	  []))

(defn make-membrane
  [inner-concentrations outer-concentrations channels]
  (let [brane (struct membrane 
		      (make-plasm inner-concentrations)
		      (make-plasm outer-concentrations)
		      channels)]
    (assoc membrane :inside (



