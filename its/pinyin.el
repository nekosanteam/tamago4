;;; -*- coding: utf-8-emacs -*-
;;; its/pinyin.el --- Pinyin Input in Egg Input Method Architecture

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

(defvar its-pinyin-cn-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-pinyin-cn-open-braket  "「" "*[") ; "［"
(defvar its-pinyin-cn-close-braket "」" "*]") ; "］"

(defvar its-pinyin-tw-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-pinyin-tw-open-braket  "「" "*[") ; "﹝ "
(defvar its-pinyin-tw-close-braket "」" "*]") ; "﹞"

(eval-when-compile
  (defun its-prev-terminal-state (state-list)
    (while (and state-list
		(null (its-get-next-state (car state-list) -1)))
      (setq state-list (cdr state-list)))
    (car state-list))

  (defun its-define-qingsheng (shengmu yunmu &optional s y)
    (let ((input (concat shengmu yunmu))
	  (output (concat (if s s (capitalize shengmu)) (if y y yunmu) "�����"))
	  state term-state)
      (setq state (its-defrule* input output))
      (its-make-next-state state ?  output)
      (its-make-next-state state ?0 output)
      (setq term-state (its-prev-terminal-state its-parent-states))
      (if term-state
	  (let ((back (- (length (its-get-keyseq term-state)) (length input)))
		(output (its-get-output (its-get-next-state term-state -1)))
		(parents its-parent-states))
	    (while (null (eq (car parents) term-state))
	      (its-make-next-state (car parents) -1 output (1+ back))
	      (its-defrule-otherwise (car parents) output nil back)
	      (setq back (1+ back)
		    parents (cdr parents)))))
      state))

  (defmacro its-do-sisheng-table (list)
    `(progn
       ,@(mapcar (lambda (syl)
		   `(its-define-sisheng ,@syl))
		 list)))

  (defmacro its-define-sisheng (shengmu yunmu)
    `(let ((qing (nth 5 ,yunmu)) (y (car ,yunmu))
	   (ss (list ,@shengmu)) s cs state i)
       (while ss
	 (setq s (car ss)
	       ss (cdr ss)
	       cs (capitalize s)
	       state (its-define-qingsheng s y cs qing))
	 (its-make-next-state state ?1 (concat cs (nth 1 ,yunmu) "�����"))
	 (its-make-next-state state ?2 (concat cs (nth 2 ,yunmu) "�����"))
	 (its-make-next-state state ?3 (concat cs (nth 3 ,yunmu) "�����"))
	 (its-make-next-state state ?4 (concat cs (nth 4 ,yunmu) "�����")))))

  (defmacro its-define-pinyin-table ()
    '(let ((- "")  (B "b") (C "c") (D "d") (F "f") (G "g") (H "h")
	   (J "j") (K "k") (L "l") (M "m") (N "n") (P "p") (Q "q")
	   (R "r") (S "s") (T "t") (W "w") (X "x") (Y "y") (Z "z")
	   (CH "ch") (SH "sh") (ZH "zh")

	   (A    '("a"    "ā"    "á"    "ǎ"    "à"    "a"   ))
	   (AI   '("ai"   "āi"   "ái"   "ǎi"   "ài"   "ai"  ))
	   (AN   '("an"   "ān"   "án"   "ǎn"   "àn"   "an"  ))
	   (ANG  '("ang"  "āng"  "áng"  "ǎng"  "àng"  "ang" ))
	   (AO   '("ao"   "āo"   "áo"   "ǎo"   "ào"   "ao"  ))
	   (E    '("e"    "ē"    "é"    "ě"    "è"    "e"   ))
	   (EI   '("ei"   "ēi"   "éi"   "ěi"   "èi"   "ei"  ))
	   (EN   '("en"   "ēn"   "én"   "ěn"   "èn"   "en"  ))
	   (ENG  '("eng"  "ēng"  "éng"  "ěng"  "èng"  "eng" ))
	   (ER   '("er"   "ēr"   "ér"   "ěr"   "èr"   "er"  ))
	   (I    '("i"    "ī"    "í"    "ǐ"    "ì"    "i"   ))
	   (IA   '("ia"   "iā"   "iá"   "iǎ"   "ià"   "ia"  ))
	   (IAN  '("ian"  "iān"  "ián"  "iǎn"  "iàn"  "ian" ))
	   (IANG '("iang" "iāng" "iáng" "iǎng" "iàng" "iang"))
	   (IAO  '("iao"  "iāo"  "iáo"  "iǎo"  "iào"  "iao" ))
	   (IE   '("ie"   "iē"   "ié"   "iě"   "iè"   "ie"  ))
	   (IN   '("in"   "īn"   "ín"   "ǐn"   "ìn"   "in"  ))
	   (ING  '("ing"  "īng"  "íng"  "ǐng"  "ìng"  "ing" ))
	   (IONG '("iong" "iōng" "ióng" "iǒng" "iòng" "iong"))
	   (IU   '("iu"   "iū"   "iú"   "iǔ"   "iù"   "iu"  ))
	   (O    '("o"    "ō"    "ó"    "ǒ"    "ò"    "o"   ))
	   (ONG  '("ong"  "ōng"  "óng"  "ǒng"  "òng"  "ong" ))
	   (OU   '("ou"   "ōu"   "óu"   "ǒu"   "òu"   "ou"  ))
	   (U    '("u"    "ū"    "ú"    "ǔ"    "ù"    "u"   ))
	   (V    '("v"    "ǖ"    "ǘ"    "ǚ"    "ǜ"    "ü"   ))
	   (UA   '("ua"   "uā"   "uá"   "uǎ"   "uà"   "ua"  ))
	   (UAI  '("uai"  "uāi"  "uái"  "uǎi"  "uài"  "uai" ))
	   (UAN  '("uan"  "uān"  "uán"  "uǎn"  "uàn"  "uan" ))
	   (UANG '("uang" "uāng" "uáng" "uǎng" "uàng" "uang"))
	   (UE   '("ue"   "uē"   "ué"   "uě"   "uè"   "ue"  ))
	   (VE   '("ve"   "üē"   "üé"   "üě"   "üè"   "üe"  ))
	   (UI   '("ui"   "uī"   "uí"   "uǐ"   "uì"   "ui"  ))
	   (UN   '("un"   "ūn"   "ún"   "ǔn"   "ùn"   "un"  ))
	   (UO   '("uo"   "uō"   "uó"   "uǒ"   "uò"   "uo"  )))

       (its-define-qingsheng	"hm"	"")
       (its-define-qingsheng	"hng"	"")
       (its-defrule*		"m"	"m�����")
       (its-defrule		"m0"	"m�����")
       (its-defrule*		"n"	"n�����")
       (its-defrule		"n0"	"n�����")
       (its-defrule		"n2"	"ń�����")
       (its-defrule		"n3"	"ň�����")
       (its-defrule		"n4"	"ǹ�����")
       (its-define-qingsheng	""	"ng")

       (its-do-sisheng-table
	(((- B C D F G H   K L M N P     S T W   Y Z CH SH ZH ) A)
	 ((- B C D   G H   K L M N P     S T W     Z CH SH ZH ) AI)
	 ((- B C D F G H   K L M N P   R S T W   Y Z CH SH ZH ) AN)
	 ((- B C D F G H   K L M N P   R S T W   Y Z CH SH ZH ) ANG)
	 ((- B C D   G H   K L M N P   R S T     Y Z CH SH ZH ) AO)
	 ((-   C D   G H   K L M N     R S T     Y Z CH SH ZH ) E)
	 ((- B C D F G H   K L M N P       T W     Z    SH ZH ) EI)
	 ((- B C D F G H   K   M N P   R S   W     Z CH SH ZH ) EN)
	 ((- B C D F G H   K L M N P   R S T W     Z CH SH ZH ) ENG)
	 ((-                                                  ) ER)
	 ((  B C D       J   L M N P Q R S T   X Y Z CH SH ZH ) I)
	 ((      D       J   L       Q         X              ) IA)
	 ((  B   D       J   L M N P Q     T   X              ) IAN)
	 ((              J   L   N   Q         X              ) IANG)
	 ((  B   D       J   L M N P Q     T   X              ) IAO)
	 ((  B   D       J   L M N P Q     T   X              ) IE)
	 ((  B           J   L M N P Q         X Y            ) IN)
	 ((  B   D       J   L M N P Q     T   X Y            ) ING)
	 ((              J           Q         X              ) IONG)
	 ((      D       J   L M N   Q         X              ) IU)
	 ((- B     F         L M   P         W   Y            ) O)
	 ((    C D   G H   K L   N     R S T     Y Z CH    ZH ) ONG)
	 ((-   C D F G H   K L M N P   R S T     Y Z CH SH ZH ) OU)
	 ((  B C D F G H J K L M N P Q R S T W X Y Z CH SH ZH ) U)
	 ((                  L   N                            ) V)
	 ((          G H   K           R             CH SH ZH ) UA)
	 ((          G H   K                         CH SH ZH ) UAI)
	 ((    C D   G H J K L   N   Q R S T   X Y Z CH SH ZH ) UAN)
	 ((          G H   K                         CH SH ZH ) UANG)
	 ((              J           Q         X Y            ) UE)
	 ((                  L   N                            ) VE)
	 ((    C D   G H   K           R S T       Z CH SH ZH ) UI)
	 ((    C D   G H J K L       Q R S T   X Y Z CH SH ZH ) UN)
	 ((    C D   G H   K L   N     R S T       Z CH SH ZH ) UO)

	 ((J Q X) (cons "a"   (cdr IA  )))
	 ((J Q X) (cons "ai"  (cdr IA  )))
	 ((J Q X) (cons "an"  (cdr IAN )))
	 ((J Q X) (cons "ang" (cdr IANG)))
	 ((J Q X) (cons "ao"  (cdr IAO )))
	 ((J Q X) (cons "e"   (cdr IE  )))
	 ((J Q X) (cons "ei"  (cdr IE  )))
	 ((J Q X) (cons "en"  (cdr IN  )))
	 ((J Q X) (cons "eng" (cdr ING )))
	 ((J Q X) (cons "ou"  (cdr IU  ))))))))

(define-its-state-machine its-pinyin-cn-map
  "pinyin-cn" "拼G" Chinese-GB
  "Map for Pinyin input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-pinyin-table)
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
  (its-defrule	"sh "	"上")
  (its-defrule	"t "	"他")
  (its-defrule	"w "	"我")
  (its-defrule	"x "	"向")
  (its-defrule	"y "	"又")
  (its-defrule	"z "	"在")
  (its-defrule	"zh "	"着")

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

(define-its-state-machine its-pinyin-tw-map
  "pinyin-tw" "拼C" Chinese-CNS
  "Map for Pinyin input."

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-tw)

  (its-define-pinyin-table)
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
  (its-defrule	"sh "	"上")
  (its-defrule	"t "	"他")
  (its-defrule	"w "	"我")
  (its-defrule	"x "	"向")
  (its-defrule	"y "	"又")
  (its-defrule	"z "	"在")
  (its-defrule	"zh "	"著")

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

(define-its-state-machine-append its-pinyin-cn-map
  (its-defrule "[" its-pinyin-cn-open-braket)
  (its-defrule "]" its-pinyin-cn-close-braket)

  (if its-pinyin-cn-enable-quanjioao-alphabet
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

(define-its-state-machine-append its-pinyin-tw-map
  (its-defrule "[" its-pinyin-tw-open-braket)
  (its-defrule "]" its-pinyin-tw-close-braket)

  (if its-pinyin-tw-enable-quanjioao-alphabet
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

(provide 'its/pinyin)
