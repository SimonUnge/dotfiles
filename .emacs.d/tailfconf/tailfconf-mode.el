;;; tailfconf-mode.el

;; Author: Simon Unge
 ;;; Code:

 ;;;###autoload
(define-derived-mode tailfconf-mode nxml-mode
  "tailfconf mode"
  "A major mode for editing confd/ncs.conf files."
  )

(provide 'tailfconf-mode)
 ;;;
