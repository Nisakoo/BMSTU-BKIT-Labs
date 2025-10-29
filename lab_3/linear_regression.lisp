(defun scalar-product (x y)
    (reduce #'+ (vec-mul x y) :initial-value 0))

(defun vec-sum (x y)
    (mapcar #'+ x y))

(defun vec-mul (x y)
    (mapcar #'* x y))

(defun vec-scalar-mul (num x)
    (mapcar (lambda (y) (* num y)) x))

(defun dot-matrix-vec (matrix v)
    (mapcar (lambda (r) (scalar-product r v)) matrix))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

(defun gradient (features weights target)
    (let ((predicted (predict features weights)))
    (vec-scalar-mul (/ 2 (length target))
                    (dot-matrix-vec (transpose features) (mapcar #'- predicted target)))))

(defun update-weights (weights gradient alpha)
    (mapcar (lambda (x y) (- x (* alpha y))) weights gradient))

(defun transform-features (features)
    (mapcar (lambda (x) (list 1 x)) features))

(defun predict (features weights)
    (dot-matrix-vec features weights))

(defun fit (features target alpha epoches)
    (loop with weights = '(1 1)
        for epoch from 1 to epoches
        do (setq weights (update-weights weights (gradient features weights target) alpha))
        finally (return weights)))

(defun learn (features target alpha epoches)
    (fit (transform-features features) target alpha epoches))

(defun gradient-with-l2-regular (features weights target l)
    (let ((gradient-value (gradient features weights target)))
    (vec-sum gradient-value (vec-scalar-mul (* 2 l) weights))))

; (defun fit-with-l2-regular (features target weights alpha epoches l)
;     (fit-))


; (defun fit-with-regularization (features target weights alpha epoches l)
;     (let ((gradient-value (gradient-with-l2-regularization (dot features weights) target weights (length target) l)))
;     (cond
;         ((= epoches 0) (update-weights weights gradient-value alpha))
;         (t (fit-with-regularization features target (update-weights weights gradient-value alpha) alpha (- epoches 1) l)))))

; (defun learn-with-regularization (features target alpha epoches l)
;     (let ((transformed-features (transform-features features))
;           (weights '(1 1)))
;     (fit-with-regularization transformed-features target weights alpha epoches l)))

; (defun mean-square-error (predicted target)
;     (/ (mean-square-error-helper predicted target) (length predicted)))

; (defun mean-square-error-helper (predicted target)
;     (cond
;         ((or (null predicted) (null target)) 0)
;         (t (+ (expt (- (car predicted) (car target)) 2)
;               (mean-square-error-helper (cdr predicted) (cdr target))))))


; (setq features '(2 3 4 5 6))
; (setq target '(4 5 6 7 8))

(setq u '(9.0 13.2 14.8 15.7 16.3 16.7 17.0 17.1))
(setq i '(0.17 0.09 0.06 0.04 0.03 0.02 0.01 0.01))

; Добавить обучение без пересечения intercept

; (print (predict features target))
; (print (mean-square-error features target))
; (print (gradient features target 5))
; (print (update-weights '(1 1 1 1 1) (gradient features target (length features)) 1))
; (print (transform-features features))
; (print (dot features '(1 1)))

; (setq weights (learn-with-regularization i u 0.1 500 0.1))
; (setq weights (learn i u 0.001 1000))
; (print (transform-features '(2 3)))
; (print (dot (transform-features '(2 3)) weights))
; (print (predict i weights))
; (print weights)

; (print (transform-features i))
; (print (gradient (transform-features i) '(1 1) u))
; (print (update-weights '(1 1) (gradient (transform-features i) '(1 1) u) 0.1))

(print (learn i u 0.01 9100))

; (print (dot-matrix-vec '((1 2) (3 4)) '(1 1)))
; (print (gradient-2 '((1 2) (1 2)) '(1 1) '(1 1)))
; (print (gradient '(3 3) '(1 1) 2))