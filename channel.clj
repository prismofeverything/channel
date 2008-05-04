(defstruct plasm :contents :membranes :charge)
(defstruct membrane :inside :outside :channels)
(defstruct channel :permeability :activity :membrane)
(defstruct ion :type :charge)
(defstruct concentration :ion :level)

