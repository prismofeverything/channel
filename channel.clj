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
  "a plasm maintains its concentrations as refs, as they will change often"
  [concentrations]
  (struct plasm
	  (let [contents (hash-map)]
	    (reduce (fn [contents concentration] 
			(assoc contents 
			  (:type concentration) 
			  (ref concentration)))
		    contents
		    concentrations))))

(defn concentration-of
  [plasm type]
  (get (:concentrations plasm) type))

(defn level-of
  [plasm type]
  (let [concentration (concentration-of plasm type)]
    (:level @concentration)))

(defn alter-level
  [plasm type factor]
  (sync nil
    (let [concentration (concentration-of plasm type)]
      (alter concentration assoc :level (+ factor (:level @concentration))))))

(defn make-channel
  [

(defn make-membrane
  [inside outside]
  (struct membrane
	  inside 
	  outside 
	  (ref [])))

(defn add-channel
  [membrane channel]
  (sync nil
    (let [embedded (assoc channel :membrane membrane)
	  channels (:channels membrane)]
      (alter channels cons embedded @channels))))

(defn exchange
  [membrane type factor]
  (sync nil
    (alter-level (:inside membrane) type factor)
    (alter-level (:outside membrane) type (- factor))))

(defn nernst
  [membrane type]
  (let [in (level-of (:inside membrane) type)
	out (level-of (:outside membrane) type)
	ratio (. Math (log (/ out in)))
	valence (:valence (type ions))]
    (* valence ratio)))








