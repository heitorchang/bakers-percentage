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
                    (baking-powder . 5.0)
                    (baking-soda . 0.6)
                    (salt . 1.25)
                    (walnuts-chopped . 25)
                    (eggs . 40)
                    (banana-pulp . 100)
                    (butter-melted . 33))))              

    (devils-food-cake ((procedure
                        "two-stage method"
                        "have ingredients at room temperature."
                        "sift all dry ingredients into the bowl."
                        "add the fat and part of the liquid."
                        "mix on low speed for 8 minutes, scraping the sides several times."
                        "in a separate bowl, combine the remaining liquid and beaten eggs."
                        "add this mixture to the batter in 3 parts. scrape the sides each time."
                        "mix for another 5 minutes."
                        "bake at 180 deg. C for about 20 mins.")
                       (ingredients
                        (flour . 100)
                        (cocoa . 17)
                        (salt . 2)
                        (baking-powder . 3)
                        (baking-soda . 2)
                        (butter . 60)                        
                        (sugar . 100)
                        (milk . 67)
                        (vanilla-extract . 1.5)
                        (milk . 50)
                        (eggs . 67))))

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
                              (butter . 67)
                              (brown-sugar . 133)
                              (salt . 1.5)
                              (eggs . 33)
                              (vanilla-extract . 3)
                              (milk . 8)
                              (flour . 100)
                              (baking-powder . 4)
                              (baking-soda . 2)
                              (cinnamon . 1)
                              (oats . 83)
                              (raisins . 67))))

    (chocolate-chip-cookies ((procedure
                              "creaming method.")
                             (ingredients
                              (butter . 150)
                              (sugar . 90)
                              (brown-sugar . 90)
                              (salt . 4)
                              (eggs . 100)
                              (vanilla-extract . 4)
                              (flour . 300)
                              (baking-soda . 4)
                              (chocolate-chips . 300))))

    (biscotti ((procedure
                "creaming method.")
               (ingredients
                (butter . 1)
                (sugar . 1)
                (flour . 2)
                (baking-powder . 0.1)
                (sliced-almonds . 1)
                (eggs . 1))))
                
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
