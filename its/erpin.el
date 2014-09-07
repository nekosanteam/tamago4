;;; -*- coding: utf-8-emacs -*-
;;; its/erpin.el --- Erpin Input in Egg Input Method Architecture

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

(defvar its-erpin-cn-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-erpin-cn-open-braket  "「" "*[") ; "［"
(defvar its-erpin-cn-close-braket "」" "*]") ; "］"

(defvar its-erpin-tw-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-erpin-tw-open-braket  "「" "*[") ; "﹝ "
(defvar its-erpin-tw-close-braket "」" "*]") ; "﹞"

(eval-when-compile
  (defun its-define-erpin-qingsheng (shengmu yunmu &optional y)
    (let ((input (concat (car shengmu) yunmu))
	  (output (concat (cdr shengmu) (if y y yunmu) "�����")))
      (prog1
	  (its-defrule input output)
	(its-defrule (concat input " ") output)
	(its-defrule (concat input "0") output))))

  (defmacro its-do-erpin-table (list)
    `(progn
       ,@(mapcar (lambda (syl)
		   `(its-define-erpin ,(car syl) ,(cdr syl)))
		 list)))

  (defmacro its-define-erpin (shengmu yunmu)
    `(let ((y1 (nth 1 ,yunmu)) (y2 (nth 2 ,yunmu)) (y3 (nth 3 ,yunmu))
	   (y4 (nth 4 ,yunmu)) (y5 (nth 5 ,yunmu)) (y (car ,yunmu))
	   (ss (list ,@shengmu)) sa sd state)
       (while ss
	 (setq sa (caar ss) sd (cdar ss) 
	       state (its-define-erpin-qingsheng (car ss) y y5))
	 (its-make-next-state state ?1 (concat sd y1 "�����"))
	 (its-make-next-state state ?2 (concat sd y2 "�����"))
	 (its-make-next-state state ?3 (concat sd y3 "�����"))
	 (its-make-next-state state ?4 (concat sd y4 "�����"))
	 (setq ss (cdr ss)))))

  (defmacro its-define-erpin-table ()
    '(let ((O '("o" . ""))  (B '("b" . "B")) (C '("c" . "C")) (D '("d" . "D"))
	   (F '("f" . "F")) (G '("g" . "G")) (H '("h" . "H")) (J '("j" . "J"))
	   (K '("k" . "K")) (L '("l" . "L")) (M '("m" . "M")) (N '("n" . "N"))
	   (P '("p" . "P")) (Q '("q" . "Q")) (R '("r" . "R")) (S '("s" . "S"))
	   (T '("t" . "T")) (W '("w" . "W")) (X '("x" . "X")) (Y '("y" . "Y"))
	   (Z '("z" . "Z"))
	   (I '("i" . "Ch")) (U '("u" . "Sh")) (V '("v" . "Zh"))

	   (a    '("a"  "ā"    "á"    "ǎ"    "à"    "a"   ))
	   (ai   '("s"  "āi"   "ái"   "ǎi"   "ài"   "ai"  ))
	   (an   '("f"  "ān"   "án"   "ǎn"   "àn"   "an"  ))
	   (ang  '("g"  "āng"  "áng"  "ǎng"  "àng"  "ang" ))
	   (ao   '("d"  "āo"   "áo"   "ǎo"   "ào"   "ao"  ))
	   (e    '("e"  "ē"    "é"    "ě"    "è"    "e"   ))
	   (ei   '("w"  "ēi"   "éi"   "ěi"   "èi"   "ei"  ))
	   (en   '("r"  "ēn"   "én"   "ěn"   "èn"   "en"  ))
	   (eng  '("t"  "ēng"  "éng"  "ěng"  "èng"  "eng" ))
	   (er   '("y"  "ēr"   "ér"   "ěr"   "èr"   "er"  ))
	   (i    '("i"  "ī"    "í"    "ǐ"    "ì"    "i"   ))
	   (ia   '("p"  "iā"   "iá"   "iǎ"   "ià"   "ia"  ))
	   (ian  '("h"  "iān"  "ián"  "iǎn"  "iàn"  "ian" ))
	   (iang '("j"  "iāng" "iáng" "iǎng" "iàng" "iang"))
	   (iao  '("k"  "iāo"  "iáo"  "iǎo"  "iào"  "iao" ))
	   (ie   '("l"  "iē"   "ié"   "iě"   "iè"   "ie"  ))
	   (in   '("m"  "īn"   "ín"   "ǐn"   "ìn"   "in"  ))
	   (ing  '("n"  "īng"  "íng"  "ǐng"  "ìng"  "ing" ))
	   (iong '("b"  "iōng" "ióng" "iǒng" "iòng" "iong"))
	   (iu   '("y"  "iū"   "iú"   "iǔ"   "iù"   "iu"  ))
	   (o    '("o"  "ō"    "ó"    "ǒ"    "ò"    "o"   ))
	   (ong  '("b"  "ōng"  "óng"  "ǒng"  "òng"  "ong" ))
	   (ou   '("q"  "ōu"   "óu"   "ǒu"   "òu"   "ou"  ))
	   (u    '("u"  "ū"    "ú"    "ǔ"    "ù"    "u"   ))
	   (v    '("v"  "ǖ"    "ǘ"    "ǚ"    "ǜ"    "ü"   ))
	   (ua   '("p"  "uā"   "uá"   "uǎ"   "uà"   "ua"  ))
	   (uai  '("k"  "uāi"  "uái"  "uǎi"  "uài"  "uai" ))
	   (uan  '("x"  "uān"  "uán"  "uǎn"  "uàn"  "uan" ))
	   (uang '("j"  "uāng" "uáng" "uǎng" "uàng" "uang"))
	   (ue   '("c"  "uē"   "ué"   "uě"   "uè"   "ue"  ))
	   (ve   '("c"  "üē"   "üé"   "üě"   "üè"   "üe"  ))
	   (ui   '("c"  "uī"   "uí"   "uǐ"   "uì"   "ui"  ))
	   (un   '("z"  "ūn"   "ún"   "ǔn"   "ùn"   "un"  ))
	   (uo   '("o"  "uō"   "uó"   "uǒ"   "uò"   "uo"  )))

       (its-do-erpin-table
	(((O B C D F G H   K L M N P     S T W   Y Z I U V) . a)
	 ((O B C D   G H   K L M N P     S T W     Z I U V) . ai)
	 ((O B C D F G H   K L M N P   R S T W   Y Z I U V) . an)
	 ((O B C D F G H   K L M N P   R S T W   Y Z I U V) . ang)
	 ((O B C D   G H   K L M N P   R S T     Y Z I U V) . ao)
	 ((O   C D   G H   K L M N     R S T     Y Z I U V) . e)
	 ((O B C D F G H   K L M N P       T W     Z   U V) . ei)
	 ((O B C D F G H   K   M N P   R S   W     Z I U V) . en)
	 ((O B C D F G H   K L M N P   R S T W     Z I U V) . eng)
	 ((O                                              ) . er)
	 ((  B C D       J   L M N P Q R S T   X Y Z I U V) . i)
	 ((      D       J   L       Q         X          ) . ia)
	 ((  B   D       J   L M N P Q     T   X          ) . ian)
	 ((              J   L   N   Q         X          ) . iang)
	 ((  B   D       J   L M N P Q     T   X          ) . iao)
	 ((  B   D       J   L M N P Q     T   X          ) . ie)
	 ((  B           J   L M N P Q         X Y        ) . in)
	 ((  B   D       J   L M N P Q     T   X Y        ) . ing)
	 ((              J           Q         X          ) . iong)
	 ((      D       J   L M N   Q         X          ) . iu)
	 ((O B     F           M   P         W   Y        ) . o)
	 ((    C D   G H   K L   N     R S T     Y Z I   V) . ong)
	 ((O   C D F G H   K L M N P   R S T     Y Z I U V) . ou)
	 ((  B C D F G H J K L M N P Q R S T W X Y Z I U V) . u)
	 ((                  L   N                        ) . v)
	 ((          G H   K           R             I U V) . ua)
	 ((          G H   K                         I U V) . uai)
	 ((    C D   G H J K L   N   Q R S T   X Y Z I U V) . uan)
	 ((          G H   K                         I U V) . uang)
	 ((              J           Q         X Y        ) . ue)
	 ((                  L   N                        ) . ve)
	 ((    C D   G H   K           R S T       Z I U V) . ui)
	 ((    C D   G H J K L       Q R S T   X Y Z I U V) . un)
	 ((    C D   G H   K L   N     R S T       Z I U V) . uo)

	 (('("" . "")) . (cons "er" (cdr er)))

	 ((J Q X) . (cons "a" (cdr ia  )))
	 ((J Q X) . (cons "s" (cdr ia  )))
	 ((J Q X) . (cons "f" (cdr ian )))
	 ((J Q X) . (cons "g" (cdr iang)))
	 ((J Q X) . (cons "d" (cdr iao )))
	 ((J Q X) . (cons "e" (cdr ie  )))
	 ((J Q X) . (cons "w" (cdr ie  )))
	 ((J Q X) . (cons "r" (cdr in  )))
	 ((J Q X) . (cons "t" (cdr ing )))
	 ((J Q X) . (cons "q" (cdr iu  )))))

       (dolist (SHENG (list B C D F G H J K L M N P Q R S T W X Y Z I U V))
	 (its-defoutput (car SHENG) (cdr SHENG)))

       (its-define-erpin-qingsheng	H	 "m")
       (its-define-erpin-qingsheng	H	 "n"	"ng")
       (its-define-erpin-qingsheng	O	 "m")
       (its-define-erpin-qingsheng	O	 "n")

       (its-defrule	"on5"	"ng�����")
       (its-defrule	"on2"	"ń�����")
       (its-defrule	"on3"	"ň�����")
       (its-defrule	"on4"	"ǹ�����"))))

(define-its-state-machine its-erpin-cn-map
  "erpin-cn" "二G" Chinese-GB
  "Map for Erpin input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-erpin-table)
  (its-defrule	"b "	"不")
  (its-defrule	"c "	"才")
  (its-defrule	"ch "	"出")
  (its-defrule	"d "	"的")
  (its-defrule	"f "	"反")
  (its-defrule	"g "	"个")
  (its-defrule	"h "	"和")
  (its-defrule	"i "	"一")
  (its-defrule	"j "	"就")
  (its-defrule	"k "	"可")
  (its-defrule	"l "	"了")
  (its-defrule	"m "	"每")
  (its-defrule	"n "	"年")
  (its-defrule	"p "	"批")
  (its-defrule	"q "	"去")
  (its-defrule	"r "	"日")
  (its-defrule	"s "	"是")
  (its-defrule	"u "	"上")
  (its-defrule	"t "	"他")
  (its-defrule	"w "	"我")
  (its-defrule	"x "	"向")
  (its-defrule	"y "	"又")
  (its-defrule	"z "	"在")
  (its-defrule	"v "	"着")

  (dolist (ascii '(("0" . "０")  ("1" . "１")  ("2" . "２")  ("3" . "３")
		   ("4" . "４")  ("5" . "５")  ("6" . "６")  ("7" . "７")
		   ("8" . "８")  ("9" . "９") 
		   (" " . "　")  ("!" . "！")  ("@" . "＠")  ("#" . "＃")
		   ("$" . "＄")  ("%" . "％")  ("^" . "＾")  ("&" . "＆")
		   ("*" . "＊")  ("(" . "（")  (")" . "）")
		   ("-" . "－")  ("=" . "＝")  ("`" . "｀")  ("\\" . "＼")
		   ("|" . "｜")  ("_" . "＿")  ("+" . "＋")  ("~" . "～")
		   ("[" . "［")  ("]" . "］")  ("{" . "｛")  ("}" . "｝")
		   (":" . "：")  (";" . "；")  ("\"" . "＂") ("'" . "＇")
		   ("<" . "＜")  (">" . "＞")  ("?" . "？")  ("/" . "／")
		   ("," . "，")  ("." . "．")
		   ("a" . "ａ")  ("b" . "ｂ")  ("c" . "ｃ")  ("d" . "ｄ")
		   ("e" . "ｅ")  ("f" . "ｆ")  ("g" . "ｇ")  ("h" . "ｈ")
		   ("i" . "ｉ")  ("j" . "ｊ")  ("k" . "ｋ")  ("l" . "ｌ")
		   ("m" . "ｍ")  ("n" . "ｎ")  ("o" . "ｏ")  ("p" . "ｐ")
		   ("q" . "ｑ")  ("r" . "ｒ")  ("s" . "ｓ")  ("t" . "ｔ")
		   ("u" . "ｕ")  ("v" . "ｖ")  ("w" . "ｗ")  ("x" . "ｘ")
		   ("y" . "ｙ")  ("z" . "ｚ")
		   ("A" . "Ａ")  ("B" . "Ｂ")  ("C" . "Ｃ")  ("D" . "Ｄ")
		   ("E" . "Ｅ")  ("F" . "Ｆ")  ("G" . "Ｇ")  ("H" . "Ｈ")
		   ("I" . "Ｉ")  ("J" . "Ｊ")  ("K" . "Ｋ")  ("L" . "Ｌ")
		   ("M" . "Ｍ")  ("N" . "Ｎ")  ("O" . "Ｏ")  ("P" . "Ｐ")
		   ("Q" . "Ｑ")  ("R" . "Ｒ")  ("S" . "Ｓ")  ("T" . "Ｔ")
		   ("U" . "Ｕ")  ("V" . "Ｖ")  ("W" . "Ｗ")  ("X" . "Ｘ")
		   ("Y" . "Ｙ")  ("Z" . "Ｚ")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in)
      (its-defrule (concat its-quanjiao-escape in) out)))

  (its-defrule	","	"，")
  (its-defrule	"."	"。")
  (its-defrule	"/"	"、")
  (its-defrule	":"	"：")
  (its-defrule	";"	"；")
  (its-defrule	"?"	"？")
  (its-defrule	"!"	"！"))

(define-its-state-machine its-erpin-tw-map
  "erpin-tw" "二C" Chinese-CNS
  "Map for Erpin input."

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-erpin-table)
  (its-defrule	"b "	"不")
  (its-defrule	"c "	"才")
  (its-defrule	"ch "	"出")
  (its-defrule	"d "	"的")
  (its-defrule	"f "	"反")
  (its-defrule	"g "	"個")
  (its-defrule	"h "	"和")
  (its-defrule	"i "	"一")
  (its-defrule	"j "	"就")
  (its-defrule	"k "	"可")
  (its-defrule	"l "	"了")
  (its-defrule	"m "	"每")
  (its-defrule	"n "	"年")
  (its-defrule	"p "	"批")
  (its-defrule	"q "	"去")
  (its-defrule	"r "	"日")
  (its-defrule	"s "	"是")
  (its-defrule	"u "	"上")
  (its-defrule	"t "	"他")
  (its-defrule	"w "	"我")
  (its-defrule	"x "	"向")
  (its-defrule	"y "	"又")
  (its-defrule	"z "	"在")
  (its-defrule	"v "	"著")

  (dolist (ascii '(("0" . "０")  ("1" . "１")  ("2" . "２")  ("3" . "３")
		   ("4" . "４")  ("5" . "５")  ("6" . "６")  ("7" . "７")
		   ("8" . "８")  ("9" . "９") 
		   (" " . "　")  ("!" . "！")  ("@" . "＠")  ("#" . "＃")
		   ("$" . "＄")  ("%" . "％")  ("^" . "︿")  ("&" . "＆")
		   ("*" . "＊")  ("(" . "（")  (")" . "）")
		   ("-" . "－")  ("=" . "＝")  ("`" . "′")  ("\\" . "＼")
		   ("|" . "｜")  ("_" . "＿")  ("+" . "＋")  ("~" . "∼")
		   ("[" . "﹝")  ("]" . "﹞")  ("{" . "｛")  ("}" . "｝")
		   (":" . "：")  (";" . "；")  ("\"" . "〞") ("'" . "‵")
		   ("<" . "＜")  (">" . "＞")  ("?" . "？")  ("/" . "／")
		   ("," . "，")  ("." . "．")
		   ("a" . "ａ")  ("b" . "ｂ")  ("c" . "ｃ")  ("d" . "ｄ")
		   ("e" . "ｅ")  ("f" . "ｆ")  ("g" . "ｇ")  ("h" . "ｈ")
		   ("i" . "ｉ")  ("j" . "ｊ")  ("k" . "ｋ")  ("l" . "ｌ")
		   ("m" . "ｍ")  ("n" . "ｎ")  ("o" . "ｏ")  ("p" . "ｐ")
		   ("q" . "ｑ")  ("r" . "ｒ")  ("s" . "ｓ")  ("t" . "ｔ")
		   ("u" . "ｕ")  ("v" . "ｖ")  ("w" . "ｗ")  ("x" . "ｘ")
		   ("y" . "ｙ")  ("z" . "ｚ")
		   ("A" . "Ａ")  ("B" . "Ｂ")  ("C" . "Ｃ")  ("D" . "Ｄ")
		   ("E" . "Ｅ")  ("F" . "Ｆ")  ("G" . "Ｇ")  ("H" . "Ｈ")
		   ("I" . "Ｉ")  ("J" . "Ｊ")  ("K" . "Ｋ")  ("L" . "Ｌ")
		   ("M" . "Ｍ")  ("N" . "Ｎ")  ("O" . "Ｏ")  ("P" . "Ｐ")
		   ("Q" . "Ｑ")  ("R" . "Ｒ")  ("S" . "Ｓ")  ("T" . "Ｔ")
		   ("U" . "Ｕ")  ("V" . "Ｖ")  ("W" . "Ｗ")  ("X" . "Ｘ")
		   ("Y" . "Ｙ")  ("Z" . "Ｚ")))
    (let ((in (car ascii)) (out (cdr ascii)))
      (its-defrule (concat its-banjiao-escape in) in)
      (its-defrule (concat its-quanjiao-escape in) out)))

  (its-defrule	","	"，")
  (its-defrule	"."	"。")
  (its-defrule	"/"	"、")
  (its-defrule	":"	"：")
  (its-defrule	";"	"；")
  (its-defrule	"?"	"？")
  (its-defrule	"!"	"！"))

(define-its-state-machine-append its-erpin-cn-map
  (its-defrule "[" its-erpin-cn-open-braket)
  (its-defrule "]" its-erpin-cn-close-braket)

  (if its-erpin-cn-enable-quanjioao-alphabet
      (progn
	(its-defrule "1"  "１")  (its-defrule "2"  "２")
	(its-defrule "3"  "３")  (its-defrule "4"  "４")
	(its-defrule "5"  "５")  (its-defrule "6"  "６")
	(its-defrule "7"  "７")  (its-defrule "8"  "８")
	(its-defrule "9"  "９")  (its-defrule "0"  "０")
	(its-defrule "@"  "＠")
	(its-defrule "#"  "＃")  (its-defrule "$"  "＄")
	(its-defrule "%"  "％")  (its-defrule "^"  "＾")
	(its-defrule "&"  "＆")  (its-defrule "*"  "＊")
	(its-defrule "("  "（")  (its-defrule ")"  "）")
	(its-defrule "-"  "－")  (its-defrule "~"  "～")
	(its-defrule "="  "＝")  (its-defrule "`"  "｀")
	(its-defrule "\\" "＼")  (its-defrule "|"  "｜")
	(its-defrule "_"  "＿")  (its-defrule "+"  "＋")
	(its-defrule "{"  "｛")  (its-defrule "}"  "｝")
	(its-defrule "\"" "＂")  (its-defrule "'"  "＇")
	(its-defrule "<"  "＜")  (its-defrule ">"  "＞"))
    (progn
      (its-defrule "1"  "1")  (its-defrule "2"  "2")
      (its-defrule "3"  "3")  (its-defrule "4"  "4")
      (its-defrule "5"  "5")  (its-defrule "6"  "6")
      (its-defrule "7"  "7")  (its-defrule "8"  "8")
      (its-defrule "9"  "9")  (its-defrule "0"  "0")
      (its-defrule "@"  "@")
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")  (its-defrule "^"  "^")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "-"  "-")  (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'")
      (its-defrule "<"  "<")  (its-defrule ">"  ">"))))

(define-its-state-machine-append its-erpin-tw-map
  (its-defrule "[" its-erpin-tw-open-braket)
  (its-defrule "]" its-erpin-tw-close-braket)

  (if its-erpin-tw-enable-quanjioao-alphabet
      (progn
	(its-defrule "1"  "１")  (its-defrule "2"  "２")
	(its-defrule "3"  "３")  (its-defrule "4"  "４")
	(its-defrule "5"  "５")  (its-defrule "6"  "６")
	(its-defrule "7"  "７")  (its-defrule "8"  "８")
	(its-defrule "9"  "９")  (its-defrule "0"  "０")
	(its-defrule "@"  "＠")
	(its-defrule "#"  "＃")  (its-defrule "$"  "＄")
	(its-defrule "%"  "％")  (its-defrule "^"  "︿")
	(its-defrule "&"  "＆")  (its-defrule "*"  "＊")
	(its-defrule "("  "（")  (its-defrule ")"  "）")
	(its-defrule "-"  "－")  (its-defrule "~"  "∼")
	(its-defrule "="  "＝")  (its-defrule "`"  "′")
	(its-defrule "\\" "﹨")  (its-defrule "|"  "｜")
	(its-defrule "_"  "＿")  (its-defrule "+"  "＋")
	(its-defrule "{"  "｛")  (its-defrule "}"  "｝")
	(its-defrule "\"" "〞")  (its-defrule "'"  "‵")
	(its-defrule "<"  "＜")  (its-defrule ">"  "＞"))
    (progn
      (its-defrule "1"  "1")  (its-defrule "2"  "2")
      (its-defrule "3"  "3")  (its-defrule "4"  "4")
      (its-defrule "5"  "5")  (its-defrule "6"  "6")
      (its-defrule "7"  "7")  (its-defrule "8"  "8")
      (its-defrule "9"  "9")  (its-defrule "0"  "0")
      (its-defrule "@"  "@")
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")  (its-defrule "^"  "^")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "-"  "-")  (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'")
      (its-defrule "<"  "<")  (its-defrule ">"  ">"))))

(provide 'its/erpin)
