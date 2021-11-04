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
                    (flour . 250)
                    (sugar . 100)
                    (baking-powder . 12)
                    (baking-soda . 2)
                    (salt . 3)
                    (walnuts-chopped . 65)
                    (eggs . 100)
                    (banana-pulp . 250)
                    (butter-melted . 80))))
    
    (oatmeal ((procedure
               "boil water"
               "add oats"
               "cook 3 minutes")
              (ingredients
               (oats . 150)
               (water . 300))))

    (oatmeal-raisin-cookies ((procedure
                              "have ingredients at room temperature."
                              "cream butter, sugar and salt at low speed."
                              "add vanilla and eggs, one at a time, and blend."
                              "sift the flour and leavening."
                              "add oats to the flour and mix."
                              "mix into butter and eggs until just combined."
                              "blend raisins last."
                              "drop balls of dough on parchment paper."
                              "bake at 190 deg. C for 10-12 mins.")
                             (ingredients
                              (butter . 100)
                              (brown-sugar . 200)
                              (salt . 2)
                              (eggs . 50)
                              (vanilla-extract . 4)
                              (milk . 12)
                              (flour . 150)
                              (baking-powder . 6)
                              (baking-soda . 3)
                              (cinnamon . 2)
                              (oats . 125)
                              (raisins . 100))))

    (chocolate-chip-cookies ((procedure
                              "creaming method."
                              "sugar halved.")
                             (ingredients
                              (butter . 75)
                              (sugar . 45)
                              (brown-sugar . 45)
                              (salt . 2)
                              (eggs . 50)
                              (vanilla-extract . 2)
                              (flour . 150)
                              (baking-soda . 2)
                              (chocolate-chips . 100))))

    (biscotti ((procedure
                "creaming method.")
               (ingredients
                (butter . 115)
                (sugar . 150)
                (eggs . 100)
                (flour . 270)
                (baking-powder . 6)
                (salt . 2)
                (sliced-almonds . 75))))              
    
    ))

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
