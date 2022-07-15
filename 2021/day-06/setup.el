(progn
  (setq org-latex-listings-options
    '(
      ("backgroundcolor" "\\color[rgb]{.94,.94,.94}")
      ("rulecolor" "\\color[rgb]{.62,.62,.62}")
      ("basicstyle" "\\ttfamily")
      ("commentstyle" "\\color{DeepPink3}")
      ("stringstyle" "\\color[rgb]{.31,.54,.30}")
      ("keywordstyle" "\\color{Blue2}\\bfseries")
;      ("lineskip" "0.5pt")
      ("belowskip" "\\medskipamount")
      ("resetmargins" "true")
      ("xleftmargin" "1em")
      ("xrightmargin" "1em")
      ("frame" "single")
      ("frameround" "ffff")
      ("framesep" "7pt")
     )
  )

;  (defun my-verbatim-block-filter (text backend info)
;    "Surround examples environments with custom verbatim"
;    (when (org-export-derived-backend-p backend 'latex)
;       (concat text "\n\\vspace{1em}%\n")
;    )
;  )
;  (add-to-list 'org-export-filter-example-block-functions 'my-verbatim-block-filter)
;  (setq org-export-filter-example-block-functions nil)

  (defun my-src-block-filter (text backend info)
    "Keep track of named blocks"
    (when (org-export-derived-backend-p backend 'latex)
      (progn
        (let* ((name (s-match "^ *--  *__.*__ *$" text))
               (caption (car name))
              )
          (when caption
            (progn
              (setq caption (s-replace "-- " "" caption))
              (setq caption (s-replace "<<" "" caption))
              (setq caption (s-replace ">>" "" caption))
              (setq caption (s-replace "__" "" caption))
              (setq caption (s-replace "_" " " caption))
              (setq text (s-replace "caption= ," (format "caption={%s}, " caption) text))
            )
          )
        )
        text
      )
    )
  )
  (add-to-list 'org-export-filter-src-block-functions 'my-src-block-filter)
)
