(defun solve-quadratic (a b c)
    (let ((disc (- (* b b) (* 4 a c))))
    (cond
        ((< disc 0) 'no-real-roots)
        ((= disc 0) (list (/ (- b) (* 2 a))))
        (t (list (/ (+ (- b) (sqrt disc)) (* 2 a))
                 (/ (- (- b) (sqrt disc)) (* 2 a)))))))

(defun find-roots (roots)
    (cond
        ((null roots) nil)
        ((< (car roots) 0) (find-roots (cdr roots)))
        (t (append
                (list (sqrt (car roots)) (- (sqrt (car roots))))
                (find-roots (cdr roots))))))

(defun solve-biquadratic (a b c)
    (let ((roots (solve-quadratic a b c)))
    (cond
        ((eq roots 'no-real-roots) 'no-real-roots)
        (t (find-roots roots)))))

(defun solve-linear (a b)
    (cond
        ((and (= a 0) (= b 0)) 'infinity)
        ((= a 0) 'no-real-roots)
        (t (find-roots (list (/ (- b) a))))))

(defun solve (a b c)
    (cond
        ((= a 0) (solve-linear b c))
        (t (solve-biquadratic a b c))))

(print (solve 4 6 -4))