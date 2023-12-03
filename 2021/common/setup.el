(progn
  (setq
   ob-ada-spark-default-file-header (lambda () (format "-- --------------------------------------------------------------------------
--
--  SPDX-License-Identifier: CC-BY-NC-ND-4.0
--  SPDX-FileCopyrightText: Copyright 2022 Francesc Rocher
--  SPDX-Creator: Francesc Rocher (francesc.rocher@gmail.com)
--  Ref: https://creativecommons.org/licenses/by-nc-nd/4.0/deed.en
--
-- --------------------------------------------------------------------------
--
--  Source code generated automatically by 'org-babel-tangle' from
--  file %s
--  %s
--
--  DO NOT EDIT!!
--
-- --------------------------------------------------------------------------

"
                                                       (buffer-file-name (current-buffer))
                                                       (time-stamp-string "%Y-%02m-%02d %02H:%02M:%02S")))
   org-export-headline-levels 3
   org-export-use-babel nil
   org-latex-listings t
   org-latex-classes '(("article" "\\documentclass[10pt]{article}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection*{%s}" . "\\paragraph*{%s}")
                        ("\\paragraph*{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph*{%s}" . "\\subparagraph*{%s}"))
                       ("report" "\\documentclass[11pt]{report}"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\chapter{%s}" . "\\chapter*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                       ("memoir" "\\documentclass[10pt]{memoir}"
                        ("\\part{%s}" . "\\part*{%s}")
                        ("\\chapter{%s}" . "\\chapter*{%s}")
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
   org-latex-listings-options '(("backgroundcolor" "\\color[rgb]{.97,.97,.97}")
                                ("rulecolor" "\\color[rgb]{.62,.62,.62}")
                                ("basicstyle" "\\ttfamily\\small")
                                ("commentstyle" "\\color{DeepPink3}")
                                ("stringstyle" "\\color[rgb]{.31,.54,.30}")
                                ("keywordstyle" "\\color{Blue2}\\bfseries")
                                ;; ("lineskip" "0.5pt")
                                ("belowskip" "\\medskipamount")
                                ("resetmargins" "true")
                                ("xleftmargin" "1em")
                                ("xrightmargin" "1em")
                                ("frame" "single")
                                ("frameround" "ffff")
                                ("framesep" "7pt")
                                ("mathescape" "true")
                                ("escapeinside" "\\{\\}")
                                ("extendedchars" "true")
                                ("literate" "{Δ}{$\\Delta\\;$}1 {Π}{$\\Pi\\;$}1 {Σ}{$\\Sigma$\\ }1 {α}{$\\alpha$}1 {β}{$\\beta$}1 {γ}{$\\gamma$}1 {δ}{$\\delta$}1 {ε}{$\\varepsilon$}1 {θ}{$\\theta$}1 {λ}{$\\lambda$}1 {μ}{$\\mu$}1 {π}{$\\pi$}1 {ρ}{$\\rho$}1 {σ}{$\\sigma$}1 {🪨}{\\emojirock}1 {🧻}{\\emojirollofpaper}1 {✀}{\\emojiscissors}1 {😖}{\\emojiconfoundedface}1 {🤝}{\\emojihandshake}1 {🏆}{\\emojitrophy}1"))
   org-latex-default-packages-alist nil
   org-latex-prefer-user-labels t
   org-latex-reference-command "\\ref{%s}"
   org-latex-title-command "")

;;  (setq org-latex-listings-options
;;    '(
;;      ("backgroundcolor" "\\color[rgb]{.94,.94,.94}")
;;      ("rulecolor" "\\color[rgb]{.62,.62,.62}")
;;      ("basicstyle" "\\ttfamily")
;;      ("commentstyle" "\\color{DeepPink3}")
;;      ("stringstyle" "\\color[rgb]{.31,.54,.30}")
;;      ("keywordstyle" "\\color{Blue2}\\bfseries")
;;;      ("lineskip" "0.5pt")
;;      ("belowskip" "\\medskipamount")
;;      ("resetmargins" "true")
;;      ("xleftmargin" "1em")
;;      ("xrightmargin" "1em")
;;      ("frame" "single")
;;      ("frameround" "ffff")
;;      ("framesep" "7pt")))

 (defun my-verbatim-block-filter (text backend info)
  "Add references to source code blocks"
  (when (org-export-derived-backend-p backend 'latex)
     (concat text "THIS IS A REFERENCE!!")))
 ; (setq org-export-filter-example-block-functions nil)
 ; (add-to-list 'org-export-filter-example-block-functions 'my-verbatim-block-filter)

 (defun my-src-block-filter (text backend info)
   "Keep track of named blocks"
   (when (org-export-derived-backend-p backend 'latex)
     (progn
       (let* ((name (s-match "^ *\\(--\\|#\\)  *__.*__ *$" text))
              (caption (car name)))
         (when caption
           (progn
             (setq caption (s-replace "--  " "" caption))
             (setq caption (s-replace "-- " "" caption))
             (setq caption (s-replace "<<" "" caption))
             (setq caption (s-replace ">>" "" caption))
             (setq caption (s-replace "__" "" caption))
             (setq caption (s-replace "_" " " caption))
             (setq text (s-replace "caption= ," (format "caption={%s}, " caption) text)))))
       text)))
 ;(setq org-export-filter-src-block-functions nil)
 (add-to-list 'org-export-filter-src-block-functions 'my-src-block-filter))
