(in-package "MEASURES")

(defun f-to-c (x) (* 5/9 (- x 32)))
(defun c-to-f (x) (+ 32 (* 9/5 x)))
(defun k-to-c (x) (- x 27316/100))
(defun c-to-k (x) (+ x 27316/100))


(defspecial-measure temperature "C"
  :units (("F" f-to-c c-to-f)
	  ("K" k-to-c c-to-k)
	  ("oC" identity identity)
	  ("oF" f-to-c c-to-f)
	  ("oK" k-to-c c-to-k)))

