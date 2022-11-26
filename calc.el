(setq math-additional-units '((SM "1.989e30 * kg" "Solar masses")))

(defmath eigval (A)
  (interactive 1 "eigval")
  (and (square-matrixp A)
       (solve-for
        (det (- A (calcFunc-diag '(var λ) (1- (length A))))) 0
        '(var λ) 'all)))

(defmath eigvec (A)
  (interactive 1 "eigvec")
  (and (square-matrixp A)
       (let ((I (calcFunc-diag 1 (1- (length A))))
             (vecs (calcFunc-vec)))
         (foreach ((λ (cdr (eigval A)))
                   (x '(var x)))
                  (setq vecs
                        (cons (solve-for
                               (* x (- A (* λ I))) 0
                               x 'all)
                              vecs)))
         vecs)))
