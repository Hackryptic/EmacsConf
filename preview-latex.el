
;; specify the justification you want
(plist-put org-format-latex-options :justify 'center)

(defun org-justify-fragment-overlay (beg end image imagetype)
  "Adjust the justification of a LaTeX fragment.
The justification is set by :justify in
`org-format-latex-options'. Only equations at the beginning of a
line are justified."
  (cond
   ;; Centered justification
   ((and (eq 'center (plist-get org-format-latex-options :justify)) 
         (= beg (line-beginning-position)))
    (let* ((img (create-image image nil))
           (width (car (image-size img)))
           (offset (floor (- (/ (window-text-width) 2) (/ width 2)))))
      (overlay-put (ov-at) 'before-string (make-string offset ? ))))
   ;; Right justification
   ((and (eq 'right (plist-get org-format-latex-options :justify)) 
         (= beg (line-beginning-position)))
    (let* ((img (create-image image nil))
           (width (car (image-display-size (overlay-get (ov-at) 'display))))
           (offset (floor (- (window-text-width) width (- (line-end-position) end)))))
      (overlay-put (ov-at) 'before-string (make-string offset ? ))))))

(defun org-latex-fragment-tooltip (beg end image imagetype)
  "Add the fragment tooltip to the overlay and set click function to toggle it."
  (overlay-put (ov-at) 'help-echo
               (concat (buffer-substring beg end)
                       "mouse-1 to toggle."))
  (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1]
                                      `(lambda ()
                                         (interactive)
                                         (org-remove-latex-fragment-image-overlays ,beg ,end)))
                                    map)))

;; advise the function to a
(advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)
(advice-add 'org--make-preview-overlay :after 'org-latex-fragment-tooltip)

(defun org-renumber-environment (orig-func &rest args)
  (let ((results '()) 
        (counter -1)
        (numberp))
    (print "hello")
    (setq results (cl-loop for (begin .  env) in 
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (cl-incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (cl-incf counter)
                              (cons begin counter)                          
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (cl-incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  
  (apply orig-func args))

(advice-add 'org-create-formula-image :around #'org-renumber-environment)
