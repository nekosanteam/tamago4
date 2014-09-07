;;; -*- coding: utf-8-emacs -*-
;;; its/zhuyin.el --- Zhuyin Input in Egg Input Method Architecture

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

(defvar its-zhuyin-cn-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-zhuyin-cn-open-braket  "「" "*[") ; "［"
(defvar its-zhuyin-cn-close-braket "」" "*]") ; "］"

(defvar its-zhuyin-tw-enable-quanjioao-alphabet
  (if (boundp 'its-enable-fullwidth-alphabet)
      its-enable-fullwidth-alphabet
    t)
  "*Enable Quanjiao alphabet")

(defvar its-zhuyin-tw-open-braket  "「" "*[") ; "﹝ "
(defvar its-zhuyin-tw-close-braket "」" "*]") ; "﹞"

(eval-when-compile
  (defmacro its-do-zhuyin-table (list)
    `(progn
       ,@(mapcar (lambda (syl) `(its-define-zhuyin ,@syl))
		 list)))

  (defmacro its-define-zhuyin (shengmu yunmu1 &optional yunmu2 qingsheng)
    `(let ((s (list ,@shengmu))
	   (yi (concat (car ,yunmu1) (car ,yunmu2)))
	   (yo (concat (nth 1 ,yunmu1) (nth 1 ,yunmu2)))
	   (tone ,(if qingsheng "ˉ" "�����"))
	   in out out1 state)
       (while s
	 (setq in (concat (car (car s)) yi)
	       out (concat (nth 1 (car s)) yo)
	       out1 (concat out tone)
	       state (its-defrule* in out1 out)
	       s (cdr s))
	 (its-defrule (concat in " ") out1)
	 ,@(if (null qingsheng)
	       '((its-make-next-state state ?1 (concat out "ˉ"))
		 (its-make-next-state state ?2 (concat out "ˊ"))
		 (its-make-next-state state ?3 (concat out "ˇ"))
		 (its-make-next-state state ?4 (concat out "ˋ")))))))

  (defmacro its-define-zhuyin-table ()
    '(let ((-  '(""  ""))
	   (B  '("b" "ㄅ")) (P  '("p" "ㄆ")) (M  '("m" "ㄇ")) (F '("f" "ㄈ"))
	   (D  '("d" "ㄉ")) (T  '("t" "ㄊ")) (N  '("n" "ㄋ")) (L '("l" "ㄌ"))
	   (G  '("v" "ㄍ")) (K  '("k" "ㄎ")) (H  '("h" "ㄏ"))
	   (J  '("g" "ㄐ")) (Q  '("7" "ㄑ")) (X  '("c" "ㄒ"))
	   (ZH '("," "ㄓ")) (CH '("." "ㄔ")) (SH '("/" "ㄕ")) (R '("j"  "ㄖ"))
	   (Z  '(";" "ㄗ")) (C  '(":" "ㄘ")) (S  '("s" "ㄙ"))

	   (A   '("a" "ㄚ")) (O   '("o" "ㄛ")) (e   '("r" "ㄜ")) (E   '("w" "ㄝ"))
	   (AI  '("i" "ㄞ")) (EI  '("q" "ㄟ")) (AO  '("z" "ㄠ")) 
	   (AN  '("8" "ㄢ")) (EN  '("9" "ㄣ")) (ANG '("0" "ㄤ")) (ENG '("-" "ㄥ"))
	   (ER  '("^" "ㄦ")) (OU  '("y" "ㄡ"))
	   (I   '("e" "ㄧ")) (U   '("x" "ㄨ")) (V   '("u" "ㄩ")))

       (its-define-zhuyin (- H) M nil t)
       (its-define-zhuyin (- H) '("@" "@") nil t)
       (its-define-zhuyin (-  ) N nil t)

       (its-do-zhuyin-table
	(((                              ZH CH SH R Z C S ) -)
	 ((- B P M F D T N L G K H       ZH CH SH   Z C S ) A)
	 ((- B P M F       L                              ) O)
	 ((-     M   D T N L G K H       ZH CH SH R Z C S ) e)
	 ((- B P M   D T N L G K H       ZH CH SH   Z C S ) AI)
	 ((- B P M F D T N L G K H       ZH    SH   Z C   ) EI)
	 ((- B P M   D T N L G K H       ZH CH SH R Z C S ) AO)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) AN)
	 ((- B P M F D   N   G K H       ZH CH SH R Z C S ) EN)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) ANG)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) ENG)
	 ((-                                              ) ER)
	 ((-   P M F D T N L G K H       ZH CH SH R Z C S ) OU)
	 ((- B P M   D T N L       J Q X                  ) I)
	 ((-         D     L       J Q X                  ) I A)
	 ((-                                              ) I O)
	 ((- B P M   D T N L       J Q X                  ) I E)
	 ((- B P M   D T N L       J Q X                  ) I AO)
	 ((-     M   D   N L       J Q X                  ) I OU)
	 ((- B P M   D T N L       J Q X                  ) I AN)
	 ((- B P M       N L       J Q X                  ) I EN)
	 ((-             N L       J Q X                  ) I ANG)
	 ((- B P M   D T N L       J Q X                  ) I ENG)
	 ((- B P M F D T N L G K H       ZH CH SH R Z C S ) U)
	 ((-                 G K H       ZH CH SH R       ) U A)
	 ((-         D T N L G K H       ZH CH SH R Z C S ) U O)
	 ((-                 G K H       ZH CH SH         ) U AI)
	 ((-         D T     G K H       ZH CH SH R Z C S ) U EI)
	 ((-         D T N L G K H       ZH CH SH R Z C S ) U AN)
	 ((-         D T   L G K H       ZH CH SH R Z C S ) U EN)
	 ((-                 G K H       ZH CH SH         ) U ANG)
	 ((-         D T N L G K H       ZH CH    R Z C S ) U ENG)
	 ((-             N L       J Q X                  ) V)
	 ((-             N L       J Q X                  ) V E)
	 ((-                       J Q X                  ) V AN)
	 ((-                       J Q X                  ) V EN)
	 ((-                       J Q X                  ) V ENG)))

       (mapcar (lambda (s) (its-defoutput (car s) (nth 1 s)))
	       (list B P M F D T N L G K H J Q X))

       (its-defrule (concat (car N) "2") (concat (nth 1 N) "ˊ"))
       (its-defrule (concat (car N) "3") (concat (nth 1 N) "ˇ"))
       (its-defrule (concat (car N) "4") (concat (nth 1 N) "ˋ")))))

(define-its-state-machine its-zhuyin-cn-map
  "zhuyin-cn" "注G" Chinese-GB
  "Map for Zhuyin input. (Chinese-GB)"

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-cn)

  (its-define-zhuyin-table)
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

    (its-defrule "<" "，")
    (its-defrule ">" "。")
    (its-defrule "?" "、"))

(define-its-state-machine its-zhuyin-tw-map
  "zhuyin-tw" "注C" Chinese-CNS
  "Map for Zhuyin input."

  (defconst its-quanjiao-escape "Z")
  (defconst its-banjiao-escape  "X")

  (its-defrule-select-mode-temporally "B" downcase)
  (its-defrule-select-mode-temporally "Q" quanjiao-downcase-tw)

  (its-define-zhuyin-table)
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

    (its-defrule "<" "，")
    (its-defrule ">" "。")
    (its-defrule "?" "、"))

(define-its-state-machine-append its-zhuyin-cn-map
  (its-defrule "[" its-zhuyin-cn-open-braket)
  (its-defrule "]" its-zhuyin-cn-close-braket)

(if its-zhuyin-cn-enable-quanjioao-alphabet
      (progn
	(its-defrule "#"  "＃")  (its-defrule "$"  "＄")
	(its-defrule "%"  "％")
	(its-defrule "&"  "＆")  (its-defrule "*"  "＊")
	(its-defrule "("  "（")  (its-defrule ")"  "）")
	(its-defrule "~"  "～")
	(its-defrule "="  "＝")  (its-defrule "`"  "｀")
	(its-defrule "\\" "＼")  (its-defrule "|"  "｜")
	(its-defrule "_"  "＿")  (its-defrule "+"  "＋")
	(its-defrule "{"  "｛")  (its-defrule "}"  "｝")
	(its-defrule "\"" "＂")  (its-defrule "'"  "＇"))
    (progn
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'"))))

(define-its-state-machine-append its-zhuyin-tw-map
  (its-defrule "[" its-zhuyin-tw-open-braket)
  (its-defrule "]" its-zhuyin-tw-close-braket)

  (if its-zhuyin-tw-enable-quanjioao-alphabet
      (progn
	(its-defrule "#"  "＃")  (its-defrule "$"  "＄")
	(its-defrule "%"  "％")
	(its-defrule "&"  "＆")  (its-defrule "*"  "＊")
	(its-defrule "("  "（")  (its-defrule ")"  "）")
	(its-defrule "~"  "∼")
	(its-defrule "="  "＝")  (its-defrule "`"  "′")
	(its-defrule "\\" "＼")  (its-defrule "|"  "｜")
	(its-defrule "_"  "＿")  (its-defrule "+"  "＋")
	(its-defrule "{"  "｛")  (its-defrule "}"  "｝")
	(its-defrule "\"" "〞")  (its-defrule "'"  "‵"))
    (progn
      (its-defrule "#"  "#")  (its-defrule "$"  "$")
      (its-defrule "%"  "%")
      (its-defrule "&"  "&")  (its-defrule "*"  "*")
      (its-defrule "("  "(")  (its-defrule ")"  ")")
      (its-defrule "~"  "~")
      (its-defrule "="  "=")  (its-defrule "`"  "`")
      (its-defrule "\\" "\\") (its-defrule "|"  "|")
      (its-defrule "_"  "_")  (its-defrule "+"  "+")
      (its-defrule "{"  "{")  (its-defrule "}"  "}")
      (its-defrule "\"" "\"") (its-defrule "'"  "'"))))

(provide 'its/zhuyin)
