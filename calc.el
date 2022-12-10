(setq math-additional-units '((SM "1.989e30 * kg" "Solar masses")))

(defmath eigval (A)
  (interactive 1 "eigval")
  (and (square-matrixp A)
       (solve-for
        (det (- A (calcFunc-diag '(var λ) (1- (length A))))) 0
        '(var λ) 'all)))

(defun convert-to-vector (v)
  (cons 'vec v))

(defmath eigvec (A)
  (interactive 1 "eigvec")
  (and (square-matrixp A)
       (let* ((eigvals (cdr (eigval A)))
              (v eigvals)
              (i 0))
         (foreach ((λ eigvals))
                  (setq (elt v i) (- A (calcFunc-diag λ (1- (length A)))))
                  (setq i (1+ i)))
         (convert-to-vector v))))

(defmath projection (a b)
  (interactive 2 "projection")
  (and (vectorp a) (vectorp b)
       :"a*(a*b)/(a*a)"))

(evil-define-key 'normal calc-mode-map (kbd "v P") #'calc-projection)
(evil-define-key 'normal calc-mode-map (kbd "v E") #'calc-eigval)
