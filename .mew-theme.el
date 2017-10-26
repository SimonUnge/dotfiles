;(mew-face-spec-set
; 'mew-face-mark-unread
; '((((class color) (type tty)) (:foreground "OrangeRed"))
;   (((class color) (background light)) (:foreground "Red"))
;   (((class color) (background dark))  (:foreground "Red"))
;   (t (:bold t))))

(mew-setface mark-unread
  :tty "OrangeRed" :light "Red" :dark "Red")
