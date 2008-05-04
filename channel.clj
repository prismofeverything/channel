(defstruct plasm :contents :membranes :charge)
(defstruct membrane :inside :outside :channels)
(defstruct channel :permeability :activity :membrane)
(defstruct ion :type :valence)
(defstruct concentration :ion :level)

(def ions 
     (map (fn [spec] (struct :ion (spec 0) (spec 1))))
     [[:a -1]
      [:na 1]
      [:cl -1]
      [:k 1]
      [:ca 2]])

(defn nernst
  [membrane type]
  (let [in (type (:inside membrane))
	out (type (:outside membrane))
	ratio (. Math (log (/ out in)))
	valence (:valence (type ions))]
    (* valence ratio)))
    

