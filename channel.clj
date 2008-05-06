(defstruct plasm :concentrations)
(defstruct membrane :inside :outside :channels)
(defstruct channel :permeability :activity :membrane)
(defstruct ion :type :valence)
(defstruct concentration :type :level)

(def ions 
     (map #(struct ion (% 0) (% 1))
	  [[:A -1]
	   [:Na 1]
	   [:Cl -1]
	   [:K 1]
	   [:Ca 2]]))

(defn valence-of 
  [type]
  (let [ion (first (filter #(= type (:type %)) ions))]
    (:valence ion)))

(defn concentrations-from
  [pairs]
  (map #(struct concentration (% 0) (% 1))
       pairs))

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

(defn alter-level!
  [plasm type factor]
  (sync nil
    (let [concentration (concentration-of plasm type)]
      (alter concentration assoc :level (+ factor (:level @concentration))))))

(defn make-channel
  [] nil)

(defn make-membrane
  [inside outside]
  (struct membrane
	  inside 
	  outside 
	  (ref [])))

(defn exchange!
  [membrane type factor]
  (sync nil
    (alter-level! (:inside membrane) type factor)
    (alter-level! (:outside membrane) type (- factor)))
  membrane)

(defn nernst
  [membrane type]
  (let [in (level-of (:inside membrane) type)
	out (level-of (:outside membrane) type)
	ratio (. Math (log (/ out in)))
	valence (valence-of type)]
    (* valence ratio)))

(defn add-channel!
  [membrane channel]
  (sync nil
    (let [embedded (assoc channel :membrane membrane)
	  channels (:channels membrane)]
      (alter channels cons embedded @channels))))


(defn test-channel
  []
  (sync nil
    (let [inner (concentrations-from [[:A 147.0]
				      [:K 140.0]
				      [:Na 15.0]
				      [:Cl 8.0]
				      [:Ca 0.1]])

	  outer (concentrations-from [[:A 25.0]
				      [:K 3.0]
				      [:Na 150.0]
				      [:Cl 130.0]
				      [:Ca 1.2]])

	  inside (make-plasm inner)
	  outside (make-plasm outer)
	  membrane (make-membrane inside outside)]

      (if (-> true
	      (and (= -1 (valence-of :A)))
	      (and (= 15.0 (level-of inside :Na)))
	      (and (= 140.0 (level-of (:inside membrane) :K))))
	membrane
	"YOU DO NOT EXIST"))))
	



