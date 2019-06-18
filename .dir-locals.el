; Project-specific Emacs configuration
((ess-r-mode . ((ess-r-package-dirs . (("R" . 1) ("tests" . 1) ("testthat" . 2)
                                       ("analysis" . 1) ("scratch" . 2) ("scripts" . 2)
                                       ("data-raw" . 1)))))
 (nil . ((projectile-project-compilation-cmd . "make"))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; End:
