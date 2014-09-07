;;; its/thai.el --- Inputting Thai characters in Egg Input Method Architecture

;; Copyright (C) 1999,2000 PFU LIMITED

;; Author: KATAYAMA Yoshio <kate@pfu.co.jp>

;; Maintainer: TOMURA Satoru <tomura@etl.go.jp>

;; Keywords: mule, multilingual, input method

;; This file is part of EGG.

;; EGG is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; EGG is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:


(eval-when-compile
  (require 'its)
  (require 'cl))

(eval-when (compile)
  (defconst its-compaction-enable t))

(eval-when-compile
  (defmacro define-its-thai-keymap (&rest rule)
    (let (input output type list)
      (while rule
	(setq input  (car (car rule))
	      output (nth 1 (car rule))
	      type   (nth 2 (car rule))
	      rule   (cdr rule)
	      list   (cons `(its-defrule ,input ,output) list))
	(if type
	    (setq list (cons `(setq ,type (cons (cons ,input ,output) ,type))
			     list))))
      `(let (consonant vowel tone)
	 ,@list
	 (its-thai-composit consonant vowel tone))))

  (defun its-thai-composit (consonant vowel tone)
    (let (keyseq output)
      (while consonant
	(setq keyseq (car (car consonant))
              output (cdr (car consonant))
	      consonant (cdr consonant))
	(its-thai-add-vowel keyseq output vowel tone)
	(its-thai-add-tone keyseq output tone))))

  (defun its-thai-add-vowel (keyseq output vowel tone)
    (let (next-keyseq next-output)
      (while vowel
	(setq next-keyseq (concat keyseq (car (car vowel)))
	      next-output (concat output (cdr (car vowel)))
	      vowel (cdr vowel))
        (its-defrule next-keyseq (compose-string next-output))
	(its-thai-add-tone next-keyseq next-output tone))))

  (defun its-thai-add-tone (keyseq output tone)
    (let (next-keyseq next-output)
      (while tone
	(setq next-keyseq (concat keyseq (car (car tone)))
	      next-output (concat output (cdr (car tone)))
              tone (cdr tone))
        (its-defrule next-keyseq (compose-string next-output))))))

;; Thai Kesmanee keyboard support.

(define-its-state-machine its-thai-kesmanee-map
  "kesmanee" "กก" Thai
  "Map for Thai Kesmanee input method with TIS620 keyboard. (Thai)"

  (define-its-thai-keymap
    ("1"  "ล" consonant)    ("!"  "#")
    ("2"  "/")              ("@"  "๑")
    ("3"  "_")              ("#"  "๒")
    ("4"  "ภ" consonant)    ("$"  "๓")
    ("5"  "ถ" consonant)    ("%"  "๔")
    ("6"  "ุ" vowel)        ("^"  "ู" vowel)
    ("7"  "ึ" vowel)        ("&"  "ั้" vowel)
    ("8"  "ค" consonant)    ("*"  "๕")
    ("9"  "ต" consonant)    ("("  "๖")
    ("0"  "จ" consonant)    (")"  "๗")
    ("-"  "ข" consonant)    ("_"  "๘")
    ("="  "ช" consonant)    ("+"  "๙")
    ("\\" "฿")              ("|"  "๏")
    ("`"  "ฃ" consonant)    ("~"  "ฅ" consonant)

    ("q"  "ๆ")              ("Q"  "๐")
    ("w"  "ไ")              ("W"  "\"")
    ("e"  "ำ")              ("E"  "ฎ" consonant)
    ("r"  "พ" consonant)    ("R"  "ฑ" consonant)
    ("T"  "ธ" consonant)    ("t"  "ะ")
    ("y"  "ั" vowel)        ("Y"  "ํ" tone)
    ("u"  "ี" vowel)        ("U"  "๊" tone)
    ("i"  "ร" consonant)    ("I"  "ณ" consonant)
    ("o"  "น" consonant)    ("O"  "ฯ")
    ("p"  "ย" consonant)    ("P"  "ญ" consonant)
    ("["  "บ" consonant)    ("{"  "ฐ" consonant)
    ("]"  "ล" consonant)    ("}"  ",")

    ("a"  "ฟ" consonant)    ("A"  "ฤ")
    ("s"  "ห" consonant)    ("S"  "ฆ" consonant)
    ("d"  "ก" consonant)    ("D"  "ฏ" consonant)
    ("f"  "ด" consonant)    ("F"  "โ")
    ("g"  "เ")              ("G"  "ฌ" consonant)
    ("h"  "้" tone)         ("H"  "็" vowel)
    ("j"  "่" tone)         ("J"  "๋" tone)
    ("k"  "า")              ("K"  "ษ" consonant)
    ("l"  "ส" consonant)    ("L"  "ศ" consonant)
    (";"  "ว" consonant)    (":"  "ซ" consonant)
    ("'"  "ง" consonant)    ("\"" "ฦ")

    ("z"  "ผ" consonant)    ("Z"  "(")
    ("x"  "ป" consonant)    ("X"  ")")
    ("c"  "แ")              ("C"  "ฉ" consonant)
    ("v"  "อ" consonant)    ("V"  "ฮ" consonant)
    ("b"  "ิ" vowel)        ("B"  "ฺ" vowel)
    ("n"  "ื" vowel)        ("N"  "์" tone)
    ("m"  "ท" consonant)    ("M"  "๎" vowel)
    (","  "ม" consonant)    ("<"  "ฒ" consonant)
    ("."  "ใ")              (">"  "ฬ" consonant)
    ("/"  "ฝ" consonant)    ("?"  "?")))

(define-its-state-machine-append its-thai-kesmanee-map)

(provide 'its/thai)

;;; its/thai.el ends here
