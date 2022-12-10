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
   org-export-headline-levels 2
   org-export-use-babel nil))
