;;; -*- coding: utf-8-emacs -*-
;;; egg-com.el --- Communication Routines in Egg Input Method Architecture

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc

;; Author: Hisashi Miyashita <himi@bird.scphys.kyoto-u.ac.jp>
;;         NIIBE Yutaka <gniibe@chroot.org>
;;	   KATAYAMA Yoshio <kate@pfu.co.jp>  ; Korean, Chinese support.

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


(require 'egg-edep)

(defvar egg-fixed-euc '(fixed-euc-jp))
(make-variable-buffer-local 'egg-fixed-euc)
(put 'egg-fixed-euc 'permanent-local t)

(defvar egg-mb-euc 'euc-japan)
(make-variable-buffer-local 'egg-mb-euc)
(put 'egg-mb-euc 'permanent-local t)

;; Japanese

(defun fixed-euc-jp-pre-write-conversion (from to)
  (let ((work-buf (generate-new-buffer " *temp*"))
	ch)
    (if (stringp from)
	(encode-coding-string from 'euc-japan nil work-buf)
      (encode-coding-region from to 'euc-japan work-buf))
    (set-buffer work-buf)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (while (not (eobp))
      (setq ch (following-char))
      (cond ((= ch #x8E)		; SS2 for JISX0201-kana
	     (delete-char 1)		; SS2 BYTE -> 0 BYTE&0x7F
	     (insert 0)
	     (forward-char 1))
	    ((= ch #x8F)		; SS3 for JISX0212
	     (delete-char 1)		; SS3 BYTE1 BYTE2 -> BYTE1 BYTE2&0x7F
	     (forward-char 1)
	     (setq ch (following-char))
	     (delete-char 1)
	     (insert (logand ch #x7F)))
	    ((>= ch #xA0)		; JISX0208
	     (forward-char 2))
	    (t				; ASCII
	     (insert 0)			; BYTE -> 0 BYTE
	     (forward-char 1))))))

(defun fixed-euc-jp-post-read-conversion (len)
  (let ((str (string-as-unibyte (buffer-substring (point) (+ (point) len))))
	(pos (point))
	i ch1 ch2)
    (delete-region (point) (+ (point) len))
    (setq i 0)
    (while (< i len)
      (setq ch1 (aref str i))
      (setq ch2 (aref str (1+ i)))
      (cond ((>= ch1 #x80)
	     (if (>= ch2 #x80)
		 (setq ch1 		; JISX0208
		       (decode-char 'japanese-jisx0208
				    (logior (lsh (logand ch1 #x7F) 8)
					    (logand ch2 #x7F))))
	       (setq ch1		; JISX0212
		     (decode-char 'japanese-jisx0212
				  (logior (lsh (logand ch1 #x7F) 8) ch2)))))
	    (t
	     (if (>= ch2 #x80)
		 (setq ch1		; JISX0201-kana
		       (decode-char 'katakana-jisx0201 (logand ch2 #x7F)))
	       (setq ch1 ch2))))
      (insert ch1)
      (setq i (+ i 2)))
    (prog1 (- (point) pos)
      (goto-char pos))))

(eval-and-compile
  (if (string< mule-version "6.0") ;; for before Emacs23
      (progn
	(define-ccl-program ccl-decode-fixed-euc-jp
	  `(2
	    ((r2 = ,(charset-id 'japanese-jisx0208))
	     (r3 = ,(charset-id 'japanese-jisx0212))
	     (r4 = ,(charset-id 'katakana-jisx0201))
	     (read r0)
	     (loop
	      (read r1)
	      (if (r0 < ?\x80)
		  ((r0 = r1)
		   (if (r1 < ?\x80)
		       (write-read-repeat r0))
		   (write r4)
		   (write-read-repeat r0))
		((if (r1 > ?\x80)
		     ((write r2 r0)
		      (r0 = r1)
		      (write-read-repeat r0))
		   ((write r3 r0)
		    (r0 = (r1 | ?\x80))
		    (write-read-repeat r0)))))))))

	(define-ccl-program ccl-encode-fixed-euc-jp
	  `(2
	    ((read r0)
	     (loop
	      (if (r0 == ,(charset-id 'latin-jisx0201))                   ; Unify
		  ((read r0)
		   (r0 &= ?\x7f)))
	      (if (r0 < ?\x80)                                            ;G0
		  ((write 0)
		   (write-read-repeat r0)))
	      (r6 = (r0 == ,(charset-id 'japanese-jisx0208)))
	      (r6 |= (r0 == ,(charset-id 'japanese-jisx0208-1978)))
	      (if r6                                                      ;G1
		  ((read r0)
		   (write r0)
		   (read r0)
		   (write-read-repeat r0)))
	      (if (r0 == ,(charset-id 'katakana-jisx0201))                ;G2
		  ((read r0)
		   (write 0)
		   (write-read-repeat r0)))
	      (if (r0 == ,(charset-id 'japanese-jisx0212))                ;G3
		  ((read r0)
		   (write r0)
		   (read r0)
		   (r0 &= ?\x7f)
		   (write-read-repeat r0)))
	      (read r0)
	      (repeat)))))
	(make-coding-system 'fixed-euc-jp 4 ?W "Coding System for fixed EUC Japanese"
			    (cons ccl-decode-fixed-euc-jp ccl-encode-fixed-euc-jp))
	)
    ;; Emacs23 or later
    ;; From Handa-san. [mule-ja : No.09414]
    (define-charset 'fixed-euc-jp
      "Fixed EUC Japanese"
      :dimension 2
      :superset '(ascii
		  (katakana-jisx0201 . #x80)
		  (japanese-jisx0208 . #x8080)
		  (japanese-jisx0212 . #x8000)))
    (define-coding-system 'fixed-euc-jp
      "Coding System for fixed EUC Japanese"
      :mnemonic ?W
      :coding-type 'charset
      :charset-list '(fixed-euc-jp))
    )
  )

;; Korean

(eval-and-compile
(define-ccl-program ccl-decode-fixed-euc-kr
  `(2
    ((r2 = ,(charset-id 'korean-ksc5601))
     (read r0)
     (loop
      (read r1)
      (if (r0 < ?\x80)
	  (r0 = r1 & ?\x7f)
	((write r2 r0)
	 (r0 = r1 | ?\x80)))
      (write-read-repeat r0)))))

(define-ccl-program ccl-encode-fixed-euc-kr
  `(2
    ((read r0)
     (loop
      (if (r0 < ?\x80)
	  ((write 0)
	   (write-read-repeat r0)))
      (if (r0 == ,(charset-id 'korean-ksc5601))
	  ((read r0)
	   (write r0)
	   (read r0)
	   (write-read-repeat r0)))
      (read r0)
      (repeat)))))
)

(make-coding-system 'fixed-euc-kr 4 ?W "Coding System for fixed EUC Korean"
		    (cons ccl-decode-fixed-euc-kr ccl-encode-fixed-euc-kr))

;; Chinese

(defconst egg-pinyin-shengmu
  '((""  . 0)  ("B" . 1)  ("C"  . 2)  ("Ch" . 3)  ("D" . 4)
    ("F" . 5)  ("G" . 6)  ("H"  . 7)  ("J"  . 8)  ("K" . 9)
    ("L" . 10) ("M" . 11) ("N"  . 12) ("P"  . 13) ("Q" . 14)
    ("R" . 15) ("S" . 16) ("Sh" . 17) ("T"  . 18) ("W" . 19)
    ("X" . 20) ("Y" . 21) ("Z"  . 22) ("Zh" . 23)))

(defconst egg-pinyin-yunmu
  '(("�����"      0 0) ("�����"      0 1) ("�����"      0 3) ("�����"      0 5) ("�����"      0 7)
    ("a�����"     1 0) ("ā�����"     1 1) ("á�����"     1 3) ("ǎ�����"     1 5) ("à�����"     1 7)
    ("ai�����"    2 0) ("āi�����"    2 1) ("ái�����"    2 3) ("ǎi�����"    2 5) ("ài�����"    2 7)
    ("an�����"    3 0) ("ān�����"    3 1) ("án�����"    3 3) ("ǎn�����"    3 5) ("àn�����"    3 7)
    ("ang�����"   4 0) ("āng�����"   4 1) ("áng�����"   4 3) ("ǎng�����"   4 5) ("àng�����"   4 7)
    ("ao�����"    5 0) ("āo�����"    5 1) ("áo�����"    5 3) ("ǎo�����"    5 5) ("ào�����"    5 7)
    ("e�����"     6 0) ("ē�����"     6 1) ("é�����"     6 3) ("ě�����"     6 5) ("è�����"     6 7)
    ("ei�����"    7 0) ("ēi�����"    7 1) ("éi�����"    7 3) ("ěi�����"    7 5) ("èi�����"    7 7)
    ("en�����"    8 0) ("ēn�����"    8 1) ("én�����"    8 3) ("ěn�����"    8 5) ("èn�����"    8 7)
    ("eng�����"   9 0) ("ēng�����"   9 1) ("éng�����"   9 3) ("ěng�����"   9 5) ("èng�����"   9 7)
    ("er�����"   10 0) ("ēr�����"   10 1) ("ér�����"   10 3) ("ěr�����"   10 5) ("èr�����"   10 7)
    ("i�����"    11 0) ("ī�����"    11 1) ("í�����"    11 3) ("ǐ�����"    11 5) ("ì�����"    11 7)
    ("ia�����"   12 0) ("iā�����"   12 1) ("iá�����"   12 3) ("iǎ�����"   12 5) ("ià�����"   12 7)
    ("ian�����"  13 0) ("iān�����"  13 1) ("ián�����"  13 3) ("iǎn�����"  13 5) ("iàn�����"  13 7)
    ("iang�����" 14 0) ("iāng�����" 14 1) ("iáng�����" 14 3) ("iǎng�����" 14 5) ("iàng�����" 14 7)
    ("iao�����"  15 0) ("iāo�����"  15 1) ("iáo�����"  15 3) ("iǎo�����"  15 5) ("iào�����"  15 7)
    ("ie�����"   16 0) ("iē�����"   16 1) ("ié�����"   16 3) ("iě�����"   16 5) ("iè�����"   16 7)
    ("in�����"   17 0) ("īn�����"   17 1) ("ín�����"   17 3) ("ǐn�����"   17 5) ("ìn�����"   17 7)
    ("ing�����"  18 0) ("īng�����"  18 1) ("íng�����"  18 3) ("ǐng�����"  18 5) ("ìng�����"  18 7)
    ("iong�����" 19 0) ("iōng�����" 19 1) ("ióng�����" 19 3) ("iǒng�����" 19 5) ("iòng�����" 19 7)
    ("iu�����"   20 0) ("iū�����"   20 1) ("iú�����"   20 3) ("iǔ�����"   20 5) ("iù�����"   20 7)
    ("m�����"    21 0) ("m�����"    21 1) ("m�����"    21 3) ("m�����"    21 5) ("m�����"    21 7)
    ("n�����"    22 0) ("n�����"    22 1) ("ń�����"    22 3) ("ň�����"    22 5) ("ǹ�����"    22 7)
    ("ng�����"   23 0) ("ng�����"   23 1) ("ng�����"   23 3) ("ng�����"   23 5) ("ng�����"   23 7)
    ("o�����"    24 0) ("ō�����"    24 1) ("ó�����"    24 3) ("ǒ�����"    24 5) ("ò�����"    24 7)
    ("ong�����"  25 0) ("ōng�����"  25 1) ("óng�����"  25 3) ("ǒng�����"  25 5) ("òng�����"  25 7)
    ("ou�����"   26 0) ("ōu�����"   26 1) ("óu�����"   26 3) ("ǒu�����"   26 5) ("òu�����"   26 7)
    ("u�����"    27 0) ("ū�����"    27 1) ("ú�����"    27 3) ("ǔ�����"    27 5) ("ù�����"    27 7)
    ("ua�����"   28 0) ("uā�����"   28 1) ("uá�����"   28 3) ("uǎ�����"   28 5) ("uà�����"   28 7)
    ("uai�����"  29 0) ("uāi�����"  29 1) ("uái�����"  29 3) ("uǎi�����"  29 5) ("uài�����"  29 7)
    ("uan�����"  30 0) ("uān�����"  30 1) ("uán�����"  30 3) ("uǎn�����"  30 5) ("uàn�����"  30 7)
    ("uang�����" 31 0) ("uāng�����" 31 1) ("uáng�����" 31 3) ("uǎng�����" 31 5) ("uàng�����" 31 7)
    ("ue�����"   32 0) ("uē�����"   32 1) ("ué�����"   32 3) ("uě�����"   32 5) ("uè�����"   32 7)
    ("ui�����"   33 0) ("uī�����"   33 1) ("uí�����"   33 3) ("uǐ�����"   33 5) ("uì�����"   33 7)
    ("un�����"   34 0) ("ūn�����"   34 1) ("ún�����"   34 3) ("ǔn�����"   34 5) ("ùn�����"   34 7)
    ("uo�����"   35 0) ("uō�����"   35 1) ("uó�����"   35 3) ("uǒ�����"   35 5) ("uò�����"   35 7)
    ("ü�����"    36 0) ("ǖ�����"    36 1) ("ǘ�����"    36 3) ("ǚ�����"    36 5) ("ǜ�����"    36 7)
    ("üe�����"   37 0) ("üē�����"   37 1) ("üé�����"   37 3) ("üě�����"   37 5) ("üè�����"   37 7)
    ("0�����"    38 0) ("1�����"    38 1) ("2�����"    38 3) ("3�����"    38 5) ("4�����"    38 7)))

(defconst egg-pinyin-table
  [
   0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0
   0 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 0 1 0 1 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 0 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 0 1 0 0 0 1 1 1 1 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 0 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 0 0 0 0 1 1 1 0 0 1 0 0 0 0 1 1 1 1
   0 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1
   0 0 0 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 1 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 1
   0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1
   0 1 0 1 1 1 1 0 0 0 0 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1 0 0 1 0 1 0 1 0 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1
   0 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1
   ])

(defconst egg-zhuyin-shengmu
  '((""  .  0) ("ㄅ" .  1) ("ㄘ" .  2) ("ㄔ" .  3) ("ㄉ" .  4)
    ("ㄈ" .  5) ("ㄍ" .  6) ("ㄏ" .  7) ("ㄐ" .  8) ("ㄎ" .  9)
    ("ㄌ" . 10) ("ㄇ" . 11) ("ㄋ" . 12) ("ㄆ" . 13) ("ㄑ" . 14)
    ("ㄖ" . 15) ("ㄙ" . 16) ("ㄕ" . 17) ("ㄊ" . 18) ("ㄨ" . 19)
    ("ㄒ" . 20) ("ㄧ" . 21) ("ㄗ" . 22) ("ㄓ" . 23)))

(defconst egg-zhuyin-yunmu
  '(("�����"    0 0) ("ˉ"    0 1) ("ˊ"    0 3) ("ˇ"    0 5) ("ˋ"    0 7) ; i
    ("ㄚ�����"   1 0) ("ㄚˉ"   1 1) ("ㄚˊ"   1 3) ("ㄚˇ"   1 5) ("ㄚˋ"   1 7) ; a
    ("ㄞ�����"   2 0) ("ㄞˉ"   2 1) ("ㄞˊ"   2 3) ("ㄞˇ"   2 5) ("ㄞˋ"   2 7) ; ai
    ("ㄢ�����"   3 0) ("ㄢˉ"   3 1) ("ㄢˊ"   3 3) ("ㄢˇ"   3 5) ("ㄢˋ"   3 7) ; an
    ("ㄤ�����"   4 0) ("ㄤˉ"   4 1) ("ㄤˊ"   4 3) ("ㄤˇ"   4 5) ("ㄤˋ"   4 7) ; ang
    ("ㄠ�����"   5 0) ("ㄠˉ"   5 1) ("ㄠˊ"   5 3) ("ㄠˇ"   5 5) ("ㄠˋ"   5 7) ; ao
    ("ㄜ�����"   6 0) ("ㄜˉ"   6 1) ("ㄜˊ"   6 3) ("ㄜˇ"   6 5) ("ㄜˋ"   6 7) ; e
    ("ㄟ�����"   7 0) ("ㄟˉ"   7 1) ("ㄟˊ"   7 3) ("ㄟˇ"   7 5) ("ㄟˋ"   7 7) ; ei
    ("ㄣ�����"   8 0) ("ㄣˉ"   8 1) ("ㄣˊ"   8 3) ("ㄣˇ"   8 5) ("ㄣˋ"   8 7) ; en
    ("ㄥ�����"   9 0) ("ㄥˉ"   9 1) ("ㄥˊ"   9 3) ("ㄥˇ"   9 5) ("ㄥˋ"   9 7) ; eng
    ("ㄦ�����"  10 0) ("ㄦˉ"  10 1) ("ㄦˊ"  10 3) ("ㄦˇ"  10 5) ("ㄦˋ"  10 7) ; er
    ("ㄧ�����"  11 0) ("ㄧˉ"  11 1) ("ㄧˊ"  11 3) ("ㄧˇ"  11 5) ("ㄧˋ"  11 7) ; i
    ("ㄧㄚ�����" 12 0) ("ㄧㄚˉ" 12 1) ("ㄧㄚˊ" 12 3) ("ㄧㄚˇ" 12 5) ("ㄧㄚˋ" 12 7) ; ia
    ("ㄧㄢ�����" 13 0) ("ㄧㄢˉ" 13 1) ("ㄧㄢˊ" 13 3) ("ㄧㄢˇ" 13 5) ("ㄧㄢˋ" 13 7) ; ian
    ("ㄧㄤ�����" 14 0) ("ㄧㄤˉ" 14 1) ("ㄧㄤˊ" 14 3) ("ㄧㄤˇ" 14 5) ("ㄧㄤˋ" 14 7) ; iang
    ("ㄧㄠ�����" 15 0) ("ㄧㄠˉ" 15 1) ("ㄧㄠˊ" 15 3) ("ㄧㄠˇ" 15 5) ("ㄧㄠˋ" 15 7) ; iao
    ("ㄧㄝ�����" 16 0) ("ㄧㄝˉ" 16 1) ("ㄧㄝˊ" 16 3) ("ㄧㄝˇ" 16 5) ("ㄧㄝˋ" 16 7) ; ie
    ("ㄧㄣ�����" 17 0) ("ㄧㄣˉ" 17 1) ("ㄧㄣˊ" 17 3) ("ㄧㄣˇ" 17 5) ("ㄧㄣˋ" 17 7) ; in
    ("ㄧㄥ�����" 18 0) ("ㄧㄥˉ" 18 1) ("ㄧㄥˊ" 18 3) ("ㄧㄥˇ" 18 5) ("ㄧㄥˋ" 18 7) ; ing
    ("ㄩㄥ�����" 19 0) ("ㄩㄥˉ" 19 1) ("ㄩㄥˊ" 19 3) ("ㄩㄥˇ" 19 5) ("ㄩㄥˋ" 19 7) ; iong
    ("ㄧㄡ�����" 20 0) ("ㄧㄡˉ" 20 1) ("ㄧㄡˊ" 20 3) ("ㄧㄡˇ" 20 5) ("ㄧㄡˋ" 20 7) ; iu
    ("ㄇ�����"  21 0) ("ㄇˉ"  21 1) ("ㄇˊ"  21 3) ("ㄇˇ"  21 5) ("ㄇˋ"  21 7) ; m
    ("ㄋ�����"  22 0) ("ㄋˉ"  22 1) ("ㄋˊ"  22 3) ("ㄋˇ"  22 5) ("ㄋˋ"  22 7) ; n
    ("@�����"  23 0) ("@ˉ"  23 1) ("@ˊ"  23 3) ("@ˇ"  23 5) ("@ˋ"  23 7) ; ng
    ("ㄛ�����"  24 0) ("ㄛˉ"  24 1) ("ㄛˊ"  24 3) ("ㄛˇ"  24 5) ("ㄛˋ"  24 7) ; o
    ("ㄨㄥ�����" 25 0) ("ㄨㄥˉ" 25 1) ("ㄨㄥˊ" 25 3) ("ㄨㄥˇ" 25 5) ("ㄨㄥˋ" 25 7) ; ong
    ("ㄡ�����"  26 0) ("ㄡˉ"  26 1) ("ㄡˊ"  26 3) ("ㄡˇ"  26 5) ("ㄡˋ"  26 7) ; ou
    ("ㄨ�����"  27 0) ("ㄨˉ"  27 1) ("ㄨˊ"  27 3) ("ㄨˇ"  27 5) ("ㄨˋ"  27 7) ; u
    ("ㄨㄚ�����" 28 0) ("ㄨㄚˉ" 28 1) ("ㄨㄚˊ" 28 3) ("ㄨㄚˇ" 28 5) ("ㄨㄚˋ" 28 7) ; ua
    ("ㄨㄞ�����" 29 0) ("ㄨㄞˉ" 29 1) ("ㄨㄞˊ" 29 3) ("ㄨㄞˇ" 29 5) ("ㄨㄞˋ" 29 7) ; uai
    ("ㄨㄢ�����" 30 0) ("ㄨㄢˉ" 30 1) ("ㄨㄢˊ" 30 3) ("ㄨㄢˇ" 30 5) ("ㄨㄢˋ" 30 7) ; uan
    ("ㄨㄤ�����" 31 0) ("ㄨㄤˉ" 31 1) ("ㄨㄤˊ" 31 3) ("ㄨㄤˇ" 31 5) ("ㄨㄤˋ" 31 7) ; uang
    ("ㄩㄝ�����" 37 0) ("ㄩㄝˉ" 37 1) ("ㄩㄝˊ" 37 3) ("ㄩㄝˇ" 37 5) ("ㄩㄝˋ" 37 7) ; ue
    ("ㄨㄟ�����" 33 0) ("ㄨㄟˉ" 33 1) ("ㄨㄟˊ" 33 3) ("ㄨㄟˇ" 33 5) ("ㄨㄟˋ" 33 7) ; ui
    ("ㄨㄣ�����" 34 0) ("ㄨㄣˉ" 34 1) ("ㄨㄣˊ" 34 3) ("ㄨㄣˇ" 34 5) ("ㄨㄣˋ" 34 7) ; un
    ("ㄨㄛ�����" 35 0) ("ㄨㄛˉ" 35 1) ("ㄨㄛˊ" 35 3) ("ㄨㄛˇ" 35 5) ("ㄨㄛˋ" 35 7) ; uo
    ("ㄩ�����"  36 0) ("ㄩˉ"  36 1) ("ㄩˊ"  36 3) ("ㄩˇ"  36 5) ("ㄩˋ"  36 7) ; ü
    ("ㄩㄝ�����" 37 0) ("ㄩㄝˉ" 37 1) ("ㄩㄝˊ" 37 3) ("ㄩㄝˇ" 37 5) ("ㄩㄝˋ" 37 7) ; üe
    ("0�����"  38 0) ("1ˉ"  38 1) ("2ˊ"  38 3) ("3ˇ"  38 5) ("4ˋ"  38 7) ; undefined
    ("ㄩㄢ�����" 39 0) ("ㄩㄢˉ" 39 1) ("ㄩㄢˊ" 39 3) ("ㄩㄢˇ" 39 5) ("ㄩㄢˋ" 39 7) ; üan
    ("ㄩㄣ�����" 40 0) ("ㄩㄣˉ" 40 1) ("ㄩㄣˊ" 40 3) ("ㄩㄣˇ" 40 5) ("ㄩㄣˋ" 40 7) ; ün
    ))

(defconst egg-zhuyin-table
  [
   ;; empty ShengMu
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x9586 ?\x0000 ?\x9592 ?\x9599
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x959b ?\x95a0 ?\x0000 ?\x959e
   ?\x95a2
   ;; ShengMu B
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu C
   ?\x828b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0280 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Ch
   ?\x838b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000
   ?\x0000 ?\x0380 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu D
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu F
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu G
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu H
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu J
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x08a4 ?\x0000 ?\x0000
   ?\x08a7 ?\x0000 ?\x08a5 ?\x0000 ?\x08a8 ?\x0000 ?\x889b ?\x88a0 ?\x8000 ?\x889e
   ?\x88a2
   ;; ShengMu K
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu L
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu M
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu N
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu P
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000 
   ;; ShengMu Q
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0ea4 ?\x0000 ?\x0000
   ?\x0ea7 ?\x0000 ?\x0ea5 ?\x0000 ?\x0ea8 ?\x0000 ?\x8e9b ?\x8ea0 ?\x8000 ?\x8e9e
   ?\x8ea2
   ;; ShengMu R
   ?\x8f8b ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000
   ?\x0000 ?\x0f80 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu S
   ?\x908b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000
   ?\x0000 ?\x1080 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Sh
   ?\x918b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x1180 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu T
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x8000
   ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu W
   ?\x939b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000 ?\x0000 ?\x1380 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu X
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x14a4 ?\x0000 ?\x0000
   ?\x14a7 ?\x0000 ?\x14a5 ?\x0000 ?\x14a8 ?\x0000 ?\x949b ?\x94a0 ?\x8000 ?\x949e
   ?\x94a2
   ;; ShengMu Y 
   ?\x958b ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0090 ?\x0000 ?\x9591 ?\x9592
   ?\x0000 ?\x1580 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x1588 ?\x1589 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0093 ?\x8000 ?\x00a4 ?\x0000 ?\x0000
   ?\x00a7 ?\x0000 ?\x00a5 ?\x0000 ?\x00a8 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Z
   ?\x968b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x1680 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000
   ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ;; ShengMu Zh 
   ?\x978b ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x0000 ?\x1780 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000
   ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x8000 ?\x8000
   ?\x8000 ?\x8000 ?\x0000 ?\x8000 ?\x8000 ?\x8000 ?\x0000 ?\x0000 ?\x8000 ?\x0000
   ?\x0000
   ])

(defconst egg-chinese-syllable-max-len
  (max (length "Zhuāng�����") (length "ㄓㄨㄤˉ")))

(defun egg-chinese-syllable (str pos)
  (setq str (substring str pos (min (length str)
				    (+ pos egg-chinese-syllable-max-len))))
  (or (car (egg-pinyin-syllable str))
      (car (egg-zhuyin-syllable str))))

(defsubst egg-make-fixed-euc-china-code (s y)
  (cons
   (+ (* 2 (nth 1 y)) (logand (nth 2 y) 1) 32)
   (+ (* 4 (if (= s 0) 20 s)) (lsh (nth 2 y) -1) 156)))

(defun egg-pinyin-syllable (str)
  (if (eq (string-match "^[A-Za-zā-ǹ]+�����" str) 0)
      (let (s y end)
	(setq end (match-end 0))
	(cond
	 ((setq s (cdr (assoc (substring str 0 2) egg-pinyin-shengmu)))
	  (setq y (substring str 2 end)))
	 ((setq s (cdr (assoc (substring str 0 1) egg-pinyin-shengmu)))
	  (setq y (substring str 1 end)))
	 (t
	  (setq s 0 y (substring str 0 end))))
	(if (and (setq y (assoc y egg-pinyin-yunmu))
		 (= (aref egg-pinyin-table (+ (* 39 s) (nth 1 y))) 1))
	    (cons end (egg-make-fixed-euc-china-code s y))))))

(defun egg-zhuyin-syllable (str)
  (if (eq (string-match "^[ㄅ-ㄩ@0-4]+[�����ˉˊˇˋ]" str) 0)
      (let (end s y c z (zhuyin-len (length "ㄅ")))
	(setq end (match-end 0)
	      c (substring str 0 zhuyin-len)
	      s (cdr (assoc c egg-zhuyin-shengmu))
	      y (assoc (substring str zhuyin-len end) egg-zhuyin-yunmu))
	(if (or (null (and s y))
		(and (or (eq s 11) (eq s 12)) (eq (nth 1 y) 0))) ; [ㄇㄋ][�����ˉˊˇˋ]
	    (setq s 0
		  y (assoc (substring str 0 end) egg-zhuyin-yunmu)))
	(if (and y
		 (setq z (aref egg-zhuyin-table (+ (* 41 s) (nth 1 y))))
		 (/= (logand z ?\x8000) 0))
	    (if (/= (logand z ?\x80) 0)
		(cons end (egg-make-fixed-euc-china-code
			   (logand (lsh z -8) ?\x7f)
			   (list nil (logand z ?\x7f) (nth 2 y))))
	      (cons end (egg-make-fixed-euc-china-code s y)))))))

(defun encode-fixed-euc-china-region (beg end type)
  "Encode the text in the region to EUC-CN/TW."
  (let (s syl c cset)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (setq s (buffer-substring
		   (point)
		   (min (point-max) (+ (point) egg-chinese-syllable-max-len))))
	  (cond
	   ((setq syl (egg-pinyin-syllable s))
	    (delete-region (point) (+ (point) (car syl)))
	    (insert (car (cdr syl)) (cdr (cdr syl))))
	   ((setq syl (egg-zhuyin-syllable s))
	    (delete-region (point) (+ (point) (car syl)))
	    (insert (car (cdr syl)) (cdr (cdr syl))))
	   (t
	    (setq c (split-char (following-char))
		  cset (car c))
	    (cond
	     ((or (and (eq cset 'chinese-gb2312) (eq type 'cn))
		  (and (eq cset 'chinese-cns11643-1) (eq type 'tw)))
	      (delete-char 1)
	      (insert (+ (nth 1 c) 128) (+ (nth 2 c) 128)))
	     ((and (eq cset 'chinese-cns11643-2) (eq type 'tw))
	      (delete-char 1)
	      (insert (+ (nth 1 c) 128) (nth 2 c)))
	     ((eq cset 'chinese-sisheng)
	      (delete-char 1)
	      (insert 0 (+ (nth 1 c) 128)))
	     ((eq cset 'ascii)
	      (delete-char 1)
	      (insert 0 (nth 1 c)))
	     (t
	      (delete-char 1))))))
	(- (point-max) (point-min))))))

(defun pre-write-encode-fixed-euc-china (from to type)
  (let ((buf (current-buffer))
	(work (get-buffer-create " *pre-write-encoding-work*")))
    (set-buffer work)
    (erase-buffer)
    (if (null (stringp from))
	(save-excursion
	  (set-buffer buf)
	  (setq from (buffer-substring from to))))
    (insert (string-as-multibyte from))
    (encode-fixed-euc-china-region 1 (point-max) type)
    nil))

(defun pre-write-encode-euc-cn (from to)
  (pre-write-encode-fixed-euc-china from to 'cn))

(defun pre-write-encode-euc-tw (from to)
  (pre-write-encode-fixed-euc-china from to 'tw))

(defun decode-fixed-euc-china-region (beg end type zhuyin)
  "Decode EUC-CN/TW encoded text in the region.
Return the length of resulting text."
  (let ((str (string-as-unibyte (buffer-substring beg end)))
	(i 0)
	(char (make-string 3 0))
	l c0 c1 s y ss)
    (delete-region beg end)
    (setq l (1- (length str)))
    (while (< i l)
      (setq c0 (aref str i)
	    c1 (aref str (1+ i))
	    i  (+ i 2))
      (cond
       ((eq c0 0)
	(if (<= c1 ?\xa0)
	    (insert c1)
	  (aset char 0 leading-code-private-11)
	  (aset char 1 (charset-id 'chinese-sisheng))
	  (aset char 2 c1)
	  (insert (string-as-multibyte char))))
       ((>= c0 ?\x80)
	(cond
	 ((eq type 'cn)
	  (aset char 0 (charset-id 'chinese-gb2312))
	  (aset char 1 c0)
	  (aset char 2 (logior c1 ?\x80)))
	 ((>= c1 ?\x80)
	  (aset char 0 (charset-id 'chinese-cns11643-1))
	  (aset char 1 c0)
	  (aset char 2 c1))
	 (t
	  (aset char 0 (charset-id 'chinese-cns11643-2))
	  (aset char 1 c0)
	  (aset char 2 (+ c1 ?\x80))))
	(insert (string-as-multibyte char)))
       (t
	(setq c1 (logand c1 ?\x7f))
	(setq s (- (lsh c1 -2) 7);;(+ (lsh (- c1 32) -2) 1)
	      y (- (lsh c0 -1) 16);;(lsh (- c0 32) -1)
	      ss (+ (logand c0 1) (logand c1 3)))
	(if (and (eq s 20)
		 (eq (aref egg-pinyin-table (+ (* 39 20) y)) 0))
	    (setq s 0))
	(if (null zhuyin)
	    (setq s (car (nth s egg-pinyin-shengmu))
		  y (car (nth (+ (* 5 y) ss) egg-pinyin-yunmu)))
	  (setq c0 (aref egg-zhuyin-table (+ (* 41 s) y)))
	  (if (eq (logand c0 ?\x8080) ?\x80)
	      (setq s (lsh c0 -8)
		    y (logand c0 ?\x7f)))
	  (setq s (car (nth s egg-zhuyin-shengmu))
		y (car (nth (+ (* 5 y) ss) egg-zhuyin-yunmu))))
	(if enable-multibyte-characters
	    (insert s y)
	  (insert (string-as-unibyte s) (string-as-unibyte y))))))
    (- (point) beg)))

(defun post-read-decode-fixed-euc-china (len type zhuyin)
  (let ((pos (point))
	(buffer-modified-p (buffer-modified-p)))
    (prog1
	(decode-fixed-euc-china-region pos (+ pos len) type zhuyin)
      (set-buffer-modified-p buffer-modified-p))))

(defun post-read-decode-euc-py-cn (len)
  (post-read-decode-fixed-euc-china len 'cn nil))

(defun post-read-decode-euc-zy-cn (len)
  (post-read-decode-fixed-euc-china len 'cn t))

(defun post-read-decode-euc-py-tw (len)
  (post-read-decode-fixed-euc-china len 'tw nil))

(defun post-read-decode-euc-zy-tw (len)
  (post-read-decode-fixed-euc-china len 'tw t))

(make-coding-system 'fixed-euc-py-cn 0 ?W
		    "Coding System for fixed EUC Chinese-gb2312")
(coding-system-put 'fixed-euc-py-cn
		   'pre-write-conversion 'pre-write-encode-euc-cn)
(coding-system-put 'fixed-euc-py-cn
		   'post-read-conversion 'post-read-decode-euc-py-cn)

(make-coding-system 'fixed-euc-zy-cn 0 ?W
		    "Coding System for fixed EUC Chinese-gb2312")
(coding-system-put 'fixed-euc-zy-cn
		   'pre-write-conversion 'pre-write-encode-euc-cn)
(coding-system-put 'fixed-euc-zy-cn
		   'post-read-conversion 'post-read-decode-euc-zy-cn)

(make-coding-system 'fixed-euc-py-tw 0 ?W
		    "Coding System for fixed EUC Chinese-cns11643")
(coding-system-put 'fixed-euc-py-tw
		   'pre-write-conversion 'pre-write-encode-euc-tw)
(coding-system-put 'fixed-euc-py-tw
		   'post-read-conversion 'post-read-decode-euc-py-tw)

(make-coding-system 'fixed-euc-zy-tw 0 ?W
		    "Coding System for fixed EUC Chinese-cns11643")
(coding-system-put 'fixed-euc-zy-tw
		   'pre-write-conversion 'pre-write-encode-euc-tw)
(coding-system-put 'fixed-euc-zy-tw
		   'post-read-conversion 'post-read-decode-euc-zy-tw)

;; Binary data

(eval-and-compile
(define-ccl-program ccl-decode-egg-binary
  `(1
    ((read r0)
     (loop
      (if (r0 == ?\xff)
	  (read r1))			; toss out
      (write-read-repeat r0)))))

(define-ccl-program ccl-encode-egg-binary
  `(2
    ((read r0)
     (loop
      (if (r0 == ?\xff)
	  ((write r0)
	   (r0 = 0)))
      (write-read-repeat r0))))))

(make-coding-system 'egg-binary 4 ?W "Coding System for binary data"
		    (cons ccl-decode-egg-binary ccl-encode-egg-binary))


(defun comm-format-u32c (uint32c)
  (insert-char (logand (lsh (car uint32c) -8) 255) 1)
  (insert-char (logand (car uint32c) 255) 1)
  (insert-char (logand (lsh (nth 1 uint32c) -8) 255) 1)
  (insert-char (logand (nth 1 uint32c) 255) 1))

(defun comm-format-u32 (uint32)
  (insert-char (logand (lsh uint32 -24) 255) 1)
  (insert-char (logand (lsh uint32 -16) 255) 1)
  (insert-char (logand (lsh uint32 -8) 255) 1)
  (insert-char (logand uint32 255) 1))

(defun comm-format-i32 (int32)
  (insert-char (logand (ash int32 -24) 255) 1)
  (insert-char (logand (ash int32 -16) 255) 1)
  (insert-char (logand (ash int32 -8) 255) 1)
  (insert-char (logand int32 255) 1))

(defun comm-format-u16 (uint16)
  (insert-char (logand (lsh uint16 -8) 255) 1)
  (insert-char (logand uint16 255) 1))

(defun comm-format-u8 (uint8)
  (insert-char (logand uint8 255) 1))

(defun comm-format-truncate-after-null (s)
  (if (string-match "\0" s)
      (substring s 0 (match-beginning 0))
    s))

(defun comm-format-u16-string (s)
  (insert (encode-coding-string (comm-format-truncate-after-null s)
				egg-fixed-euc))
  (insert-char 0 2))

(defun comm-format-mb-string (s)
  (insert (encode-coding-string  (comm-format-truncate-after-null s)
				 egg-mb-euc))
  (insert-char 0 1))

(defun comm-format-u8-string (s)
  (insert (comm-format-truncate-after-null s))
  (insert-char 0 1))

(defun comm-format-binary-data (s)
  (insert (encode-coding-string s 'egg-binary))
  (insert-char ?\377 2))

(defun comm-format-fixlen-string (s len)
  (setq s (comm-format-truncate-after-null s))
  (insert (if (< (length s) len) s (substring s 0 (1- len))))
  (insert-char 0 (max (- len (length s)) 1)))

(defun comm-format-vector (s len)
  (setq s (concat s))
  (insert (if (<= (length s) len) s (substring s 0 len)))
  (insert-char 0 (- len (length s))))

(defmacro comm-format (format &rest args)
  "Format a string out of a control-list and arguments into the buffer.
The formated datas are network byte oder (i.e. big endian)..
U: 32-bit integer.  The argument is 2 element 16-bit unsigned integer list.
u: 32-bit integer.  The argument is treat as unsigned integer.
   (Note:  Elisp's integer may be less than 32 bits)
i: 32-bit integer.
   (Note:  Elisp's integer may be greater than 32 bits)
w: 16-bit integer.
b: 8-bit integer.
S: 16-bit wide-character EUC string (0x0000 terminated).
E: Multibyte EUC string (0x00 terminated).
s: 8-bit string (0x00 terminated).
B: Binary data (0xff terminated).
v: 8-bit vector (no terminator).  This takes 2 args (data length).
V: Fixed length string (0x00 terminated).  This takes 2 args (data length)."
  (let ((p args)
	(form format)
	(result (list 'progn))
	f arg)
    (while (and form p)
      (setq f (car form)
	    arg (car p))
      (nconc result
	     (list
	      (cond ((eq f 'U) (list 'comm-format-u32c arg))
		    ((eq f 'u) (list 'comm-format-u32 arg))
		    ((eq f 'i) (list 'comm-format-i32 arg))
		    ((eq f 'w) (list 'comm-format-u16 arg))
		    ((eq f 'b) (list 'comm-format-u8 arg))
		    ((eq f 'S) (list 'comm-format-u16-string arg))
		    ((eq f 'E) (list 'comm-format-mb-string arg))
		    ((eq f 's) (list 'comm-format-u8-string arg))
		    ((eq f 'B) (list 'comm-format-binary-data arg))
		    ((eq f 'V) (setq p (cdr p))
			       (list 'comm-format-fixlen-string arg (car p)))
		    ((eq f 'v) (setq p (cdr p))
			       (list 'comm-format-vector arg (car p))))))
      (setq form (cdr form)
	    p (cdr p)))
    (if (or form p)
	(error "comm-format %s: arguments mismatch" format))
    result))

(defvar comm-accept-timeout nil)

;; Assume PROC is bound to the process of current buffer
;; Do not move the point, leave it where it was.
(defmacro comm-accept-process-output ()
  `(let ((p (point)))
     (if (null (accept-process-output proc comm-accept-timeout))
	 (egg-error "backend timeout (comm-accept-process-output)"))
     (goto-char p)))

(defmacro comm-require-process-output (n)
  `(if (< (point-max) (+ (point) ,n))
       (comm-wait-for-space proc ,n)))

(defun comm-wait-for-space (proc n)
  (let ((p (point))
	(r (+ (point) n)))
    (while (< (point-max) r)
      (if (null (accept-process-output proc comm-accept-timeout))
	  (egg-error "backend timeout (comm-wait-for-space)"))
      (goto-char p))))

(defmacro comm-following+forward-char ()
  `(prog1
       (following-char)
     (forward-char 1)))

(defun comm-unpack-u32c ()
  (progn
    (comm-require-process-output 4)
    (list (+ (lsh (comm-following+forward-char) 8)
	     (comm-following+forward-char))
	  (+ (lsh (comm-following+forward-char) 8)
	     (comm-following+forward-char)))))

(defun comm-unpack-i32 ()
  (progn
    (comm-require-process-output 4)
    (+ (lsh (- (logxor (comm-following+forward-char) 128) 128) 24)
       (lsh (comm-following+forward-char) 16)
       (lsh (comm-following+forward-char) 8)
       (comm-following+forward-char))))

(defun comm-unpack-u32 ()
  (progn
    (comm-require-process-output 4)
    (+ (lsh (comm-following+forward-char) 24)
       (lsh (comm-following+forward-char) 16)
       (lsh (comm-following+forward-char) 8)
       (comm-following+forward-char))))

(defun comm-unpack-u16 ()
  (progn
    (comm-require-process-output 2)
    (+ (lsh (comm-following+forward-char) 8)
       (comm-following+forward-char))))

(defun comm-unpack-u8 ()
  (progn
    (comm-require-process-output 1)
    (comm-following+forward-char)))

(defun comm-unpack-u16-string ()
  (let ((start (point)))
    (while (not (search-forward "\0\0" nil t))
      (comm-accept-process-output))
    (decode-coding-string (buffer-substring start (- (point) 2))
			  egg-fixed-euc)))

(defun comm-unpack-mb-string ()
  (let ((start (point)))
    (while (not (search-forward "\0" nil t))
      (comm-accept-process-output))
    (decode-coding-string (buffer-substring start (1- (point)))
			  egg-mb-euc)))

(defun comm-unpack-u8-string ()
  (let ((start (point)))
    (while (not (search-forward "\0" nil 1))
      (comm-accept-process-output))
    (buffer-substring start (1- (point)))))

(defun comm-unpack-binary-data ()
  (let ((start (point)))
    (while (not (search-forward "\377\377" nil 1))
      (comm-accept-process-output))
    (string-as-unibyte
     (decode-coding-string (buffer-substring start (- (point) 2))
			   'egg-binary))))

(defun comm-unpack-fixlen-string (len)
  (let (s)
    (comm-require-process-output len)
    (goto-char (+ (point) len))
    (setq s (buffer-substring (- (point) len) (point)))
    (if (string-match "\0" s)
	(setq s (substring s 0 (match-beginning 0))))
    s))

(defun comm-unpack-vector (len)
  (progn
    (comm-require-process-output len)
    (goto-char (+ (point) len))
    (buffer-substring (- (point) len) (point))))

(defmacro comm-unpack (format &rest args)
  "Unpack a string out of a control-string and set arguments.
See `comm-format' for FORMAT."
  (let ((p args)
	(form format)
	(result (list 'progn))
	arg f)
    (while (and form p)
      (setq f (car form)
	    arg (car p))
      (nconc result
	     (list
	      (cond ((eq f 'U) `(setq ,arg (comm-unpack-u32c)))
		    ((eq f 'u) `(setq ,arg (comm-unpack-u32)))
		    ((eq f 'i) `(setq ,arg (comm-unpack-i32)))
		    ((eq f 'w) `(setq ,arg (comm-unpack-u16)))
		    ((eq f 'b) `(setq ,arg (comm-unpack-u8)))
		    ((eq f 'S) `(setq ,arg (comm-unpack-u16-string)))
		    ((eq f 'E) `(setq ,arg (comm-unpack-mb-string)))
		    ((eq f 's) `(setq ,arg (comm-unpack-u8-string)))
		    ((eq f 'B) `(setq ,arg (comm-unpack-binary-data)))
		    ((eq f 'V) (setq p (cdr p))
			       `(setq ,arg (comm-unpack-fixlen-string ,(car p))))
		    ((eq f 'v) (setq p (cdr p))
			       `(setq ,arg (comm-unpack-vector ,(car p)))))))
      (setq form (cdr form)
	    p (cdr p)))
    (if (or form p)
	(error "comm-unpack %s: arguments mismatch" format))
    result))

(defmacro comm-call-with-proc (proc vlist send-expr &rest receive-exprs)
  (let ((euc-select
	 (and (eq (car-safe (car vlist)) 'zhuyin)
	      '((egg-fixed-euc (nth (if zhuyin 1 0) egg-fixed-euc))))))
  `(let* ((proc ,proc)
	  (buffer (process-buffer proc))
	  ,@vlist)
     (if (and (memq (process-status proc) '(open run))
	      (buffer-live-p buffer))
	 (save-excursion
	   (set-buffer buffer)
	   (let ,euc-select
	     (erase-buffer)
	     ,send-expr
	     (goto-char (point-max))
	     (process-send-region proc (point-min) (point-max))
	     ,@receive-exprs))
       (egg-error "process %s was killed" proc)))))

(defmacro comm-call-with-proc-1 (proc vlist send-expr &rest receive-exprs)
  `(let ,vlist
     (erase-buffer)
     ,send-expr
     (goto-char (point-max))
     (process-send-region proc (point-min) (point-max))
     ,@receive-exprs))

(provide 'egg-com)
;;; egg-com.el ends here.
