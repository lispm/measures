(in-package :ms)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (setq *no-warnings-default-p* t)
  (reset-prime-numbers))  ;; Allow reloading of this file.


;;
;; Sources:
;;  [NIST]
;;  Guide for the Use of the International System of Units (SI)
;;  U.S. Department of Commerce
;;  National Institute of Standards and Technology (NIST)
;;  NIST Special Publication 811, 1995 Edition
;;  http://physics.nist.gov/Document/sp811.pdf
;;
;;  [MfM]
;;  Richard A. Young and Thomas J. Glover
;;  Measure for Measure
;;  1996.  ISBN 1-889796-00-X
;;  (Blue Willow, Inc.; Littleton, Colorado, USA)
;;  http://www.bluewillow.com
;;


(defmeasure distance "m"
  :units (("nm" 1/1000000000 "m")
	  ("um" 1/1000000 "m")
	  ("mm" 1/1000 "m")
	  ("cm" 1/100 "m")
	  ("dm" 1/10 "m")
	  ("km" 1000 "m")
	  ("angstrom" 1/10000000000 "m")
	  ("ang" "angstrom")
	  ;; International foot, inch, yard, mile.  Differs slightly
	  ;;  from survey foot, inch, yard, mile
	  ("in" 254/10000 "m") 
	  ("ft" 12 "in")
	  ("yd" 3 "ft")
	  ("mile" 5280 "ft")
	  ("miles" "mile")
	  ("mi" "mile")
	  ;; ("nm" 1852 "m")		; Reserve for nanometer
	  ("NM" 1852 "m")		; Nautical mile
	  ("nautmi" 1852 "m")
	  ;; For convenience, fathom, furlong (and later acre - see area)
	  ;; is defined in terms of the international foot.
	  ;; It really should be the survey foot.
	  ;; The difference from the true value is 2 per million (1.000002)
	  ("fathom" 6 "ft")
	  ("furlong" 220 "yd")
	  ;; ("fathom" 7200/3937 "m")  ;Exact
	  ;; ("furlong" 792000/3937 "m")   ;Exact
	  ("pica" 1/6 "in")		; computer, not printer's
	  ("point" 1/72 "in")		; computer, not printer's
	  ("pt" "point")
	  ("AU" 149597870660 "m")	; MfM
          ; ("lightyear" 9460730000000000 "m") ; NIST - mean Julian year
	  ; ("lightyear" 9460730621329408 "m") ; derived from mean Julian year
	  ; ("lightyear" 9460536274059264 "m") ; derived from mean Gregorian year
	  ;; Lightyear is chosen based on mean tropical year to be consistent
	  ;; with the time definitions.  That way  1year * 1c = 1LY
	  ("lightyear" 1182566022178352616/125 "m") ; Derived from "year" and "c"
	  ("LY" "lightyear")
	  ("parsec" 30856780000000000  "m")) )

(defmeasure area "m2"
  :units ("mm2" "cm2" "km2" "in2" "yd2" "ft2" "mile2" "NM2"
	  ("sqyd" "yd2")
          ("sqft" "ft2")
	  ("sqin" "in2")
          ("sqkm" "km2")
          ("sqmile" "mile2")

          ("hectare" 10000 "m2")
          ("ha" "hectare")
          ; ("hectare" 100 "are")
          ("are" 100 "m2")
	  ("a" "are")
	  ;; For convenience, acre is based on the international foot.
	  ;; The exact value should be based on the U.S. survey ft,
	  ;; giving an exact conversion factor of
	  ;; ("acre" 62726400000/15499969 "m2")
	  ;; The difference from the true value is 4 per million (1.000004)
          ("acre" 4840 "yd2")) )

(defmeasure volume "m3"
  :units (("liter" 1/1000 "m3")
	  ("l" "liter")
          ("ml" 1/1000 "liter")
          ("cc" "ml")
          ("cl" 1/100  "liter")
          ("dl" 1/10   "liter")
          ("Hl" 100    "liter")
          ("cuft" "ft3")
	  ("cuyd" "yd3")
	  
	  "mm3" "cm3" "km3" 
	  "in3" "ft3" "yd3" "mile3" "NM3"

          ("acrefeet" "acre.ft")
	  ("gallon" 3785/1000 "liter")
          ("pint" 1/8 "gallon")
          ("quart" 1/4 "gallon")
	  ("gal" "gallon")
	  ;; ("pt" "pint")			; pt is taken for point
	  ("qt" "quart")
	  ("floz" 1/16 "pint")		; Fluid Ounce
          ("barrel" 42 "gal")
	  ("cup" 1/2 "pint")
	  ("tbl" 1/16 "cup")		; tablespoon
	  ("tsp" 1/3 "tbl")		; teaspoon
          ("bbl" 1 "barrel")
          ("Mbbl" 1000000 "barrel")
          ("Bbbl" 1000000000 "barrel")
          ("BRT" 100 "ft3"))            ; Register ton
  ; :format (:best-fit "ml" "l")
  )

(defmeasure time "s" 
  :units (("ns" 1/1000000000 "s")
	  ("us" 1/1000000 "s")
	  ("ms" 1/1000 "s")
	  ("min" 60 "s") ("h" 60 "min") ("day" 24 "h") ("hr" "h")
	  ("week" 7 "day")
	  ("fortnight" 14 "day")
          ;("year" 36525/100 "day") ; mean Julian year. ; MfM
          ;("year" 3652425/10000 "day") ; mean Gregorian year. ; MfM
	  ("year" 36524219/100000 "day") ; Tropical year. ; MfM
	  ("decade" 10 "year")
	  ("century" 100 "year")
	  ("years" "year") ("days" "day") ("weeks" "week")
	  ))

(defmeasure speed "m/s"
  :units ("km/h" 
          ("mph" "mi/h")
          ("kts" "NM/h")
	  "ft/s" 
          ("knots" "kts")
	  ("c" 299792458 "m/s")	; Speed of Light  ; NIST, MfM
	  ))

(defmeasure acceleration "m/s2"
  :units (("G" 980665/100000 "m/s2")
	  "ft/s2"))

;;  Notes:  The Angle-Measure uses degrees as the fundamental unit rather
;;          than the SI standard of radians.  This was done to make
;;          conversions between degrees and minutes and seconds exact,
;;          since the conversion between radians and degrees involves the
;;          use of the irrational number PI.
(defmeasure angle "deg"
  :units (;("rad" #.(rationalize (/ 180 pi)) "deg")
	  ("rad" 857374503/14964008 "deg")
          ("minute" 1/60 "deg")
          ("sec" 1/60 "minute")
          ("mil" 360/6400 "deg")  ;; Def'n from Webster, agrees with NIST.
	  ;; ("mil" #.(rationalize (/ 17.45)) "deg") ;; Subtend 1' at 1000'
	  ;; ("mil" 1/1000 "rad")  ;; Milliradian
          )
  ; :format (:best-fit "sec" "min" "deg")
  )

(defmeasure mass "kg"
  :units (("g" 1/1000 "kg")
	  ("mg" 1/1000000 "kg")
	  ("ug" 1/1000000000 "kg")
          ("lbs" 45359237/100000000 "kg")
          ("oz" 1/16 "lbs")
	  ("gr" 1/7000 "lbs")  ; grain
	  ("lb" "lbs")
          ("mton" 1000000 "g")  ; metric ton, not milliton
          ("tonne" 1000000 "g")
          ("ton" 2000 "lbs")		; short ton
	  ("longton" 2240 "lbs")
          ("stone" 14 "lbs")		; MfM
          ("hwt" 100 "lbs")
          ("slug" 37069137/2540 "g")	; 14590
          ("carat" 200/1000 "g"))
  ; :format (:best-fit "g" "kg" "mton")
  )

(defmeasure force ("N" "kg.m/s2")
  :units ("g.m/s2"
	  ("kN" 1000 "N")
	  ("kgf" "kg.G")
	  ("lbf" "lb.G")
         ; ("kgf" 9807/1000 "N")
         ; ("lbf" 4448/1000 "N")
          ("dyne" 1/100000 "N")
	  )
  :format (:unit "N"))

(defmeasure pressure ("Pa" "kg/s2m")
  :units ("g/s2m"
	  ("N/m2" 1 "Pa")
	  ("hPa" 100 "Pa")
          ("kPa" 1000 "Pa")
          ;("atm" 10332 "kgf/m2")
          ;("atm" 101325924 "g/s2m")
	  ("atm" 101325 "Pa")
          ("inHg" 3342/100000 "atm")
	  ("torr" 101325/760 "Pa")
          ; ("mmHg" 3937/100000 "inHg")
          ("mmHg" "torr")
          ;("psi"  7031/10 "kgf/m2")
          ("psi"  68953017/10 "g/s2m")
	  ;("bar" 145/10 "psi")
	  ("bar" 100000 "Pa")
          ("mbar" 1/1000 "bar")
          )
 ;  :format (:unit "mbar")
  )

(defmeasure density "kg/m3"
  :units ("g/m3" "g/cm3" "lbs/in3")
  :format (:best-fit "g/m3" "kg/m3" "g/cm3"))

(defmeasure power ("W" "m2kg/s3")
  :units ("N.m/s"
          ;; ("hp" 746 "W")     ; (electric)  [NIST], [MfM]
	  ("hp" 550 "ft.lbf/s")  ; Classic definition [NIST]
	  ("shp" "hp")
	  ("mW" 1/1000 "W")
          ("kW" 1000 "W")
          ("MW" 1000000 "W")
          ("GW" 1000000000 "W"))
  )

;;
;; TORQUE has the same units as WORK, so we cannot
;;  reliably distinguish them.

;; BTU and calorie have several different values.
;; BTU: (IT 1055.056)  (thermochemical 1054.35) (mean 1055.87)
;; calorie: (IT 4.1868 #)  (thermochemical 4.184 #) (mean 4.19002)
;;          (15C 4.18580)  (20C 4.18190)

(defmeasure work-or-torque ("J" "m2kg/s2")
  :units ("N.m"
	  "ft.lbf"
	  "in.lbf"
	  ("kJ" 1000 "J")
	  ("MJ" 1000000 "J")
	  ("GJ" 1000000000 "J")
          ("BTU" 105505585262/100000000 "J")   ; IT
          ("cal" 41868/10000 "J")  ; IT
          ("kcal" 1000 "cal")
	 ; ("Cal" 1000 "cal")  ;; Corresponds to food calorie!
	 ; ("therm" 100000 "BTU")	; EC, not US.  Corresponds to BTU
	  ("therm" 105506000 "J"); EC, not US.  [NIST]
         ; ("erg" 1/10000000 "J")       ;; No ergs because "e" is reserved for expoential number notations.
	  ("kWh" "kW.h")
          )
  )


(defmeasure revolution "deg/s"
  :units (("rps" 360 "deg/s")
          ("rpm" 1/60 "rps"))
  :format (:best-fit))

;;
;; Should this be merged with REVOLUTION?
;;

(defmeasure frequency ("Hz" "s-1")
  :units (("kHz" 1000 "Hz")
	  ("MHz" 1000000 "Hz")
	  ("GHz" 1000000000 "Hz"))
  :format (:best-fit))




;;
;; Electricity & Luminance
;; 

(defmeasure electric-current "A"
  :units (("mA" 1/1000 "A"))
  :format (:best-fit))

(defmeasure electric-charge ("C" "s.A")
  :units (("pC" 1/1000000000000 "C"))
  :format (:best-fit))

(defmeasure electric-potential ("V" "m2kg/s3A")  ; =  "W/A"
  :units (("mV" 1/1000 "V")
	  ("kV" 1000 "V")
	  ("MV" 1000 "kV"))
  :format (:best-fit))

(defmeasure electric-resistance ("ohm" "m2kg/s3A2") ; = "V/A"
  :units (("kohm" 1000 "ohm")
	  ("Mohm" 1000000 "ohm"))
  :format (:best-fit))

(defmeasure capacitance ("F" "A2s4/m2kg")   ; = "C/V"
  :units (("mF" 1/1000 "F")
	  ("uF" 1/1000000 "F")
	  ("pF" 1/1000000 "uF"))
  :format (:best-fit))

(defmeasure inductance ("H" "m2kg/A2s2")    ; = "Wb/A"
  )

(defmeasure magnetic-flux ("Wb" "m2kg/s2A")	; = "V.s"
  )

(defmeasure magnetic-flux-density ("T" "kg/s2A")     ; "Wb/m2"
  )

(defmeasure luminous-intensity "Cd"
  ;; Also includes luminous flux, which has the same reduced units.
  :units (("lumen" 1 "Cd")
	  ("lm" 1 "Cd")))

(defmeasure illuminance ("lux" "Cd/m2")
  :units (("lx" 1 "lux")))

;;
;; DATA
;;


(defmeasure data "bit"
  :units (("byte" 8 "bit")
          ("Kibit" 1024 "bit")		;; Base 2 multiples
	  ("kibit" "Kibit")
	  ("Mibit" 1024 "kibit")
	  ("KiB" 1024 "byte")
	  ("kiB" "KiB")
	  ("MiB" 1024 "KiB")
	  ("GiB" 1024 "MiB")
	  ("TiB" 1024 "GiB")
	  ("kbit" 1000 "bit")		;; Base 10 multiples
	  ("Mbit" 1000 "kbit")
	  ("kB" 1000 "byte")
	  ("MB" 1000 "kB")
	  ("GB" 1000 "MB")
	  ("TB" 1000 "GB"))
  :format (:best-fit "byte" "KiB" "MiB" "GiB" "TiB"))

;;
;; Temperature
;;

(defun f-to-c (x) (* 5/9 (- x 32)))
(defun c-to-f (x) (+ 32 (* 9/5 x)))

(defun f-to-k (x) (* 5/9 (+ x 57461/125)))
(defun k-to-f (x) (- (* 9/5 x) 57461/125))
(defun k-to-c (x) (- x 27316/100))
(defun c-to-k (x) (+ x 27316/100))
(defun k-to-r (x) (* x 9/5))
(defun r-to-k (x) (* x 5/9))

(defspecial-measure temperature "K"
  :units (("F" f-to-k k-to-f)
	  ("C" c-to-k k-to-c)
	  ("R" r-to-k k-to-r)))



;; Relative temperature differences:
;
;
;(defmeasure delta-temperature "oC"
;  :units (("oK" "oC")
;	   ("oF" 9/5 "oC")
;          ("oR" "oF")))
;


;;
;; Currency
;;

#+ms-unicode
(declare-unit-char #.(code-char 8364))   ; Euro sign

#+ms-unicode
(defmeasure currency #.(string (code-char 8364))
  :units (("cent" 1/100  #.(string (code-char 8364)))
          ("usd"  94/100 #.(string (code-char 8364)))
          ("$" "usd")
          ("usc"  1/100  "usd")))

;;
;; Scalars
;;
;; TO DO:  DO WE NEED TO MAKE "NUMBER" BE A SUBCLASS OF SCALAR-MEASURE?
;;

(declare-unit-char #\%)
(defscalars :units (("%" 1/100) ("%%" 1/1000)))

(export '(distance area volume time speed accleration angle mass force pressure density
	  torque power work work-or-torque revolution data temperature scalar frequency
	  electric-current electric-charge electric-potential electric-resistance
	  capacitance inductance magnetic-flux magnetic-flux-density
	  luminous-intensity illuminance currency))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (setq *no-warnings-default-p* nil))
