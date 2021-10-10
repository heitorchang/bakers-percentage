;; Eval (bakers-rescale 'recipe-name 'ingredient new-value)

(defparameter *bakers-recipes*
  '((banana-bread ((procedure
                    "mix dry ingredients (except nuts) with a fork"
                    "sift dry ingredients (except nuts)"
                    "mix liquids"
                    "add liquids to dry ingredients"
                    "mix halfway (lightly)"
                    "add nuts"
                    "mix until all flour is moistened (do not overmix)"
                    "bake at 190 deg. C for 25-30 minutes")
                   (ingredients
                    (flour . 100)
                    (sugar . 40)
                    (baking-powder . 5)
                    (baking-soda . 0.6)
                    (salt . 1.25)
                    (walnuts-chopped . 25)
                    (eggs . 40)
                    (banana-pulp . 100)
                    (butter-melted . 33))))              
    
    (oatmeal ((procedure
               "boil water"
               "add oats"
               "cook 3 minutes")
              (ingredients
               (oats . 150)
               (water . 300))))

    (double-chocolate-chunk-cookies ((procedure
                                      "melt choc. and butter in double boiler. cool to room temp."
                                      "blend sugar, egg (at room temperature) and salt, do not whip."
                                      "sift flour, cocoa powder and baking powder. fold in"
                                      "fold in white chocolate and nuts")
                                     (ingredients
                                      (semisweet-chocolate . 200)
                                      (butter . 67)
                                      (sugar . 33)
                                      (eggs . 42)
                                      (salt . 1.5)
                                      (flour . 100)
                                      (cocoa-powder . 8)
                                      (baking-powder . 3)
                                      (white-chocolate-bits . 67)
                                      (nuts . 33))))                                      
    
    (make-believe-cookies ((procedure
                            "cream butter and sugar"
                            "add flour"
                            "bake for 10 minutes")
                           (ingredients
                            (flour . 100)
                            (butter . 100)
                            (sugar . 100)
                            (organic-free-range-brown-speckled-eggs . 100))))))

(defun bakers-list-recipes ()
  (loop for i in *bakers-recipes* do (print (car i))))

(defun bakers-ingredients (recipe-name)
  (cdr (assoc 'ingredients (cadr (assoc recipe-name *bakers-recipes*)))))

(defun bakers-procedure (recipe-name)
  (dolist (step (cdr (assoc 'procedure (cadr (assoc recipe-name *bakers-recipes*)))))
    (format t "~a~%" step))
  t)

(defun bakers-match-ingredient (recipe-name ingredient)
  (cdr (assoc ingredient
              (bakers-ingredients recipe-name))))

(defun bakers-scale-ingredient (recipe-name ingredient new-weight)
  (/ new-weight (bakers-match-ingredient recipe-name ingredient) 1.0))

(defun bakers-rescale (recipe-name ingredient new-weight)
  (let ((ingredients (bakers-ingredients recipe-name))
        (scale-factor (bakers-scale-ingredient recipe-name ingredient new-weight))
        (max-length-ingredient (apply #'max (mapcar #'(lambda (ingr) (length (write-to-string (car ingr)))) (bakers-ingredients recipe-name))))
        (total-weight 0))
    (dolist (scaled-ingredient
             (mapcar #'(lambda (ingredient) (cons (car ingredient) (* scale-factor (cdr ingredient))))
                     ingredients))
      (format t (concatenate 'string "~" (write-to-string max-length-ingredient) "a ~8<~,1f~>~%")
              (car scaled-ingredient) (cdr scaled-ingredient))
      (incf total-weight (cdr scaled-ingredient)))
    (format t (concatenate 'string "~%~" (write-to-string max-length-ingredient) "a ~8<~,1f~>~%~%")
            "TOTAL-WEIGHT" total-weight)
    (bakers-procedure recipe-name)))
