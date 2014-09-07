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
  '(("øˆ€€Ÿ"      0 0) ("øˆ€€Ÿ"      0 1) ("øˆ€€Ÿ"      0 3) ("øˆ€€Ÿ"      0 5) ("øˆ€€Ÿ"      0 7)
    ("aøˆ€€Ÿ"     1 0) ("Äøˆ€€Ÿ"     1 1) ("Ã¡øˆ€€Ÿ"     1 3) ("ÇŽøˆ€€Ÿ"     1 5) ("Ã øˆ€€Ÿ"     1 7)
    ("aiøˆ€€Ÿ"    2 0) ("Äiøˆ€€Ÿ"    2 1) ("Ã¡iøˆ€€Ÿ"    2 3) ("ÇŽiøˆ€€Ÿ"    2 5) ("Ã iøˆ€€Ÿ"    2 7)
    ("anøˆ€€Ÿ"    3 0) ("Änøˆ€€Ÿ"    3 1) ("Ã¡nøˆ€€Ÿ"    3 3) ("ÇŽnøˆ€€Ÿ"    3 5) ("Ã nøˆ€€Ÿ"    3 7)
    ("angøˆ€€Ÿ"   4 0) ("Ängøˆ€€Ÿ"   4 1) ("Ã¡ngøˆ€€Ÿ"   4 3) ("ÇŽngøˆ€€Ÿ"   4 5) ("Ã ngøˆ€€Ÿ"   4 7)
    ("aoøˆ€€Ÿ"    5 0) ("Äoøˆ€€Ÿ"    5 1) ("Ã¡oøˆ€€Ÿ"    5 3) ("ÇŽoøˆ€€Ÿ"    5 5) ("Ã oøˆ€€Ÿ"    5 7)
    ("eøˆ€€Ÿ"     6 0) ("Ä“øˆ€€Ÿ"     6 1) ("Ã©øˆ€€Ÿ"     6 3) ("Ä›øˆ€€Ÿ"     6 5) ("Ã¨øˆ€€Ÿ"     6 7)
    ("eiøˆ€€Ÿ"    7 0) ("Ä“iøˆ€€Ÿ"    7 1) ("Ã©iøˆ€€Ÿ"    7 3) ("Ä›iøˆ€€Ÿ"    7 5) ("Ã¨iøˆ€€Ÿ"    7 7)
    ("enøˆ€€Ÿ"    8 0) ("Ä“nøˆ€€Ÿ"    8 1) ("Ã©nøˆ€€Ÿ"    8 3) ("Ä›nøˆ€€Ÿ"    8 5) ("Ã¨nøˆ€€Ÿ"    8 7)
    ("engøˆ€€Ÿ"   9 0) ("Ä“ngøˆ€€Ÿ"   9 1) ("Ã©ngøˆ€€Ÿ"   9 3) ("Ä›ngøˆ€€Ÿ"   9 5) ("Ã¨ngøˆ€€Ÿ"   9 7)
    ("erøˆ€€Ÿ"   10 0) ("Ä“røˆ€€Ÿ"   10 1) ("Ã©røˆ€€Ÿ"   10 3) ("Ä›røˆ€€Ÿ"   10 5) ("Ã¨røˆ€€Ÿ"   10 7)
    ("iøˆ€€Ÿ"    11 0) ("Ä«øˆ€€Ÿ"    11 1) ("Ã­øˆ€€Ÿ"    11 3) ("Çøˆ€€Ÿ"    11 5) ("Ã¬øˆ€€Ÿ"    11 7)
    ("iaøˆ€€Ÿ"   12 0) ("iÄøˆ€€Ÿ"   12 1) ("iÃ¡øˆ€€Ÿ"   12 3) ("iÇŽøˆ€€Ÿ"   12 5) ("iÃ øˆ€€Ÿ"   12 7)
    ("ianøˆ€€Ÿ"  13 0) ("iÄnøˆ€€Ÿ"  13 1) ("iÃ¡nøˆ€€Ÿ"  13 3) ("iÇŽnøˆ€€Ÿ"  13 5) ("iÃ nøˆ€€Ÿ"  13 7)
    ("iangøˆ€€Ÿ" 14 0) ("iÄngøˆ€€Ÿ" 14 1) ("iÃ¡ngøˆ€€Ÿ" 14 3) ("iÇŽngøˆ€€Ÿ" 14 5) ("iÃ ngøˆ€€Ÿ" 14 7)
    ("iaoøˆ€€Ÿ"  15 0) ("iÄoøˆ€€Ÿ"  15 1) ("iÃ¡oøˆ€€Ÿ"  15 3) ("iÇŽoøˆ€€Ÿ"  15 5) ("iÃ oøˆ€€Ÿ"  15 7)
    ("ieøˆ€€Ÿ"   16 0) ("iÄ“øˆ€€Ÿ"   16 1) ("iÃ©øˆ€€Ÿ"   16 3) ("iÄ›øˆ€€Ÿ"   16 5) ("iÃ¨øˆ€€Ÿ"   16 7)
    ("inøˆ€€Ÿ"   17 0) ("Ä«nøˆ€€Ÿ"   17 1) ("Ã­nøˆ€€Ÿ"   17 3) ("Çnøˆ€€Ÿ"   17 5) ("Ã¬nøˆ€€Ÿ"   17 7)
    ("ingøˆ€€Ÿ"  18 0) ("Ä«ngøˆ€€Ÿ"  18 1) ("Ã­ngøˆ€€Ÿ"  18 3) ("Çngøˆ€€Ÿ"  18 5) ("Ã¬ngøˆ€€Ÿ"  18 7)
    ("iongøˆ€€Ÿ" 19 0) ("iÅngøˆ€€Ÿ" 19 1) ("iÃ³ngøˆ€€Ÿ" 19 3) ("iÇ’ngøˆ€€Ÿ" 19 5) ("iÃ²ngøˆ€€Ÿ" 19 7)
    ("iuøˆ€€Ÿ"   20 0) ("iÅ«øˆ€€Ÿ"   20 1) ("iÃºøˆ€€Ÿ"   20 3) ("iÇ”øˆ€€Ÿ"   20 5) ("iÃ¹øˆ€€Ÿ"   20 7)
    ("møˆ€€Ÿ"    21 0) ("møˆ€€Ÿ"    21 1) ("møˆ€€Ÿ"    21 3) ("møˆ€€Ÿ"    21 5) ("møˆ€€Ÿ"    21 7)
    ("nøˆ€€Ÿ"    22 0) ("nøˆ€€Ÿ"    22 1) ("Å„øˆ€€Ÿ"    22 3) ("Åˆøˆ€€Ÿ"    22 5) ("Ç¹øˆ€€Ÿ"    22 7)
    ("ngøˆ€€Ÿ"   23 0) ("ngøˆ€€Ÿ"   23 1) ("ngøˆ€€Ÿ"   23 3) ("ngøˆ€€Ÿ"   23 5) ("ngøˆ€€Ÿ"   23 7)
    ("oøˆ€€Ÿ"    24 0) ("Åøˆ€€Ÿ"    24 1) ("Ã³øˆ€€Ÿ"    24 3) ("Ç’øˆ€€Ÿ"    24 5) ("Ã²øˆ€€Ÿ"    24 7)
    ("ongøˆ€€Ÿ"  25 0) ("Ångøˆ€€Ÿ"  25 1) ("Ã³ngøˆ€€Ÿ"  25 3) ("Ç’ngøˆ€€Ÿ"  25 5) ("Ã²ngøˆ€€Ÿ"  25 7)
    ("ouøˆ€€Ÿ"   26 0) ("Åuøˆ€€Ÿ"   26 1) ("Ã³uøˆ€€Ÿ"   26 3) ("Ç’uøˆ€€Ÿ"   26 5) ("Ã²uøˆ€€Ÿ"   26 7)
    ("uøˆ€€Ÿ"    27 0) ("Å«øˆ€€Ÿ"    27 1) ("Ãºøˆ€€Ÿ"    27 3) ("Ç”øˆ€€Ÿ"    27 5) ("Ã¹øˆ€€Ÿ"    27 7)
    ("uaøˆ€€Ÿ"   28 0) ("uÄøˆ€€Ÿ"   28 1) ("uÃ¡øˆ€€Ÿ"   28 3) ("uÇŽøˆ€€Ÿ"   28 5) ("uÃ øˆ€€Ÿ"   28 7)
    ("uaiøˆ€€Ÿ"  29 0) ("uÄiøˆ€€Ÿ"  29 1) ("uÃ¡iøˆ€€Ÿ"  29 3) ("uÇŽiøˆ€€Ÿ"  29 5) ("uÃ iøˆ€€Ÿ"  29 7)
    ("uanøˆ€€Ÿ"  30 0) ("uÄnøˆ€€Ÿ"  30 1) ("uÃ¡nøˆ€€Ÿ"  30 3) ("uÇŽnøˆ€€Ÿ"  30 5) ("uÃ nøˆ€€Ÿ"  30 7)
    ("uangøˆ€€Ÿ" 31 0) ("uÄngøˆ€€Ÿ" 31 1) ("uÃ¡ngøˆ€€Ÿ" 31 3) ("uÇŽngøˆ€€Ÿ" 31 5) ("uÃ ngøˆ€€Ÿ" 31 7)
    ("ueøˆ€€Ÿ"   32 0) ("uÄ“øˆ€€Ÿ"   32 1) ("uÃ©øˆ€€Ÿ"   32 3) ("uÄ›øˆ€€Ÿ"   32 5) ("uÃ¨øˆ€€Ÿ"   32 7)
    ("uiøˆ€€Ÿ"   33 0) ("uÄ«øˆ€€Ÿ"   33 1) ("uÃ­øˆ€€Ÿ"   33 3) ("uÇøˆ€€Ÿ"   33 5) ("uÃ¬øˆ€€Ÿ"   33 7)
    ("unøˆ€€Ÿ"   34 0) ("Å«nøˆ€€Ÿ"   34 1) ("Ãºnøˆ€€Ÿ"   34 3) ("Ç”nøˆ€€Ÿ"   34 5) ("Ã¹nøˆ€€Ÿ"   34 7)
    ("uoøˆ€€Ÿ"   35 0) ("uÅøˆ€€Ÿ"   35 1) ("uÃ³øˆ€€Ÿ"   35 3) ("uÇ’øˆ€€Ÿ"   35 5) ("uÃ²øˆ€€Ÿ"   35 7)
    ("Ã¼øˆ€€Ÿ"    36 0) ("Ç–øˆ€€Ÿ"    36 1) ("Ç˜øˆ€€Ÿ"    36 3) ("Çšøˆ€€Ÿ"    36 5) ("Çœøˆ€€Ÿ"    36 7)
    ("Ã¼eøˆ€€Ÿ"   37 0) ("Ã¼Ä“øˆ€€Ÿ"   37 1) ("Ã¼Ã©øˆ€€Ÿ"   37 3) ("Ã¼Ä›øˆ€€Ÿ"   37 5) ("Ã¼Ã¨øˆ€€Ÿ"   37 7)
    ("0øˆ€€Ÿ"    38 0) ("1øˆ€€Ÿ"    38 1) ("2øˆ€€Ÿ"    38 3) ("3øˆ€€Ÿ"    38 5) ("4øˆ€€Ÿ"    38 7)))

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
  '((""  .  0) ("ã„…" .  1) ("ã„˜" .  2) ("ã„”" .  3) ("ã„‰" .  4)
    ("ã„ˆ" .  5) ("ã„" .  6) ("ã„" .  7) ("ã„" .  8) ("ã„Ž" .  9)
    ("ã„Œ" . 10) ("ã„‡" . 11) ("ã„‹" . 12) ("ã„†" . 13) ("ã„‘" . 14)
    ("ã„–" . 15) ("ã„™" . 16) ("ã„•" . 17) ("ã„Š" . 18) ("ã„¨" . 19)
    ("ã„’" . 20) ("ã„§" . 21) ("ã„—" . 22) ("ã„“" . 23)))

(defconst egg-zhuyin-yunmu
  '(("øˆ€€Ÿ"    0 0) ("Ë‰"    0 1) ("ËŠ"    0 3) ("Ë‡"    0 5) ("Ë‹"    0 7) ; i
    ("ã„šøˆ€€Ÿ"   1 0) ("ã„šË‰"   1 1) ("ã„šËŠ"   1 3) ("ã„šË‡"   1 5) ("ã„šË‹"   1 7) ; a
    ("ã„žøˆ€€Ÿ"   2 0) ("ã„žË‰"   2 1) ("ã„žËŠ"   2 3) ("ã„žË‡"   2 5) ("ã„žË‹"   2 7) ; ai
    ("ã„¢øˆ€€Ÿ"   3 0) ("ã„¢Ë‰"   3 1) ("ã„¢ËŠ"   3 3) ("ã„¢Ë‡"   3 5) ("ã„¢Ë‹"   3 7) ; an
    ("ã„¤øˆ€€Ÿ"   4 0) ("ã„¤Ë‰"   4 1) ("ã„¤ËŠ"   4 3) ("ã„¤Ë‡"   4 5) ("ã„¤Ë‹"   4 7) ; ang
    ("ã„ øˆ€€Ÿ"   5 0) ("ã„ Ë‰"   5 1) ("ã„ ËŠ"   5 3) ("ã„ Ë‡"   5 5) ("ã„ Ë‹"   5 7) ; ao
    ("ã„œøˆ€€Ÿ"   6 0) ("ã„œË‰"   6 1) ("ã„œËŠ"   6 3) ("ã„œË‡"   6 5) ("ã„œË‹"   6 7) ; e
    ("ã„Ÿøˆ€€Ÿ"   7 0) ("ã„ŸË‰"   7 1) ("ã„ŸËŠ"   7 3) ("ã„ŸË‡"   7 5) ("ã„ŸË‹"   7 7) ; ei
    ("ã„£øˆ€€Ÿ"   8 0) ("ã„£Ë‰"   8 1) ("ã„£ËŠ"   8 3) ("ã„£Ë‡"   8 5) ("ã„£Ë‹"   8 7) ; en
    ("ã„¥øˆ€€Ÿ"   9 0) ("ã„¥Ë‰"   9 1) ("ã„¥ËŠ"   9 3) ("ã„¥Ë‡"   9 5) ("ã„¥Ë‹"   9 7) ; eng
    ("ã„¦øˆ€€Ÿ"  10 0) ("ã„¦Ë‰"  10 1) ("ã„¦ËŠ"  10 3) ("ã„¦Ë‡"  10 5) ("ã„¦Ë‹"  10 7) ; er
    ("ã„§øˆ€€Ÿ"  11 0) ("ã„§Ë‰"  11 1) ("ã„§ËŠ"  11 3) ("ã„§Ë‡"  11 5) ("ã„§Ë‹"  11 7) ; i
    ("ã„§ã„šøˆ€€Ÿ" 12 0) ("ã„§ã„šË‰" 12 1) ("ã„§ã„šËŠ" 12 3) ("ã„§ã„šË‡" 12 5) ("ã„§ã„šË‹" 12 7) ; ia
    ("ã„§ã„¢øˆ€€Ÿ" 13 0) ("ã„§ã„¢Ë‰" 13 1) ("ã„§ã„¢ËŠ" 13 3) ("ã„§ã„¢Ë‡" 13 5) ("ã„§ã„¢Ë‹" 13 7) ; ian
    ("ã„§ã„¤øˆ€€Ÿ" 14 0) ("ã„§ã„¤Ë‰" 14 1) ("ã„§ã„¤ËŠ" 14 3) ("ã„§ã„¤Ë‡" 14 5) ("ã„§ã„¤Ë‹" 14 7) ; iang
    ("ã„§ã„ øˆ€€Ÿ" 15 0) ("ã„§ã„ Ë‰" 15 1) ("ã„§ã„ ËŠ" 15 3) ("ã„§ã„ Ë‡" 15 5) ("ã„§ã„ Ë‹" 15 7) ; iao
    ("ã„§ã„øˆ€€Ÿ" 16 0) ("ã„§ã„Ë‰" 16 1) ("ã„§ã„ËŠ" 16 3) ("ã„§ã„Ë‡" 16 5) ("ã„§ã„Ë‹" 16 7) ; ie
    ("ã„§ã„£øˆ€€Ÿ" 17 0) ("ã„§ã„£Ë‰" 17 1) ("ã„§ã„£ËŠ" 17 3) ("ã„§ã„£Ë‡" 17 5) ("ã„§ã„£Ë‹" 17 7) ; in
    ("ã„§ã„¥øˆ€€Ÿ" 18 0) ("ã„§ã„¥Ë‰" 18 1) ("ã„§ã„¥ËŠ" 18 3) ("ã„§ã„¥Ë‡" 18 5) ("ã„§ã„¥Ë‹" 18 7) ; ing
    ("ã„©ã„¥øˆ€€Ÿ" 19 0) ("ã„©ã„¥Ë‰" 19 1) ("ã„©ã„¥ËŠ" 19 3) ("ã„©ã„¥Ë‡" 19 5) ("ã„©ã„¥Ë‹" 19 7) ; iong
    ("ã„§ã„¡øˆ€€Ÿ" 20 0) ("ã„§ã„¡Ë‰" 20 1) ("ã„§ã„¡ËŠ" 20 3) ("ã„§ã„¡Ë‡" 20 5) ("ã„§ã„¡Ë‹" 20 7) ; iu
    ("ã„‡øˆ€€Ÿ"  21 0) ("ã„‡Ë‰"  21 1) ("ã„‡ËŠ"  21 3) ("ã„‡Ë‡"  21 5) ("ã„‡Ë‹"  21 7) ; m
    ("ã„‹øˆ€€Ÿ"  22 0) ("ã„‹Ë‰"  22 1) ("ã„‹ËŠ"  22 3) ("ã„‹Ë‡"  22 5) ("ã„‹Ë‹"  22 7) ; n
    ("@øˆ€€Ÿ"  23 0) ("@Ë‰"  23 1) ("@ËŠ"  23 3) ("@Ë‡"  23 5) ("@Ë‹"  23 7) ; ng
    ("ã„›øˆ€€Ÿ"  24 0) ("ã„›Ë‰"  24 1) ("ã„›ËŠ"  24 3) ("ã„›Ë‡"  24 5) ("ã„›Ë‹"  24 7) ; o
    ("ã„¨ã„¥øˆ€€Ÿ" 25 0) ("ã„¨ã„¥Ë‰" 25 1) ("ã„¨ã„¥ËŠ" 25 3) ("ã„¨ã„¥Ë‡" 25 5) ("ã„¨ã„¥Ë‹" 25 7) ; ong
    ("ã„¡øˆ€€Ÿ"  26 0) ("ã„¡Ë‰"  26 1) ("ã„¡ËŠ"  26 3) ("ã„¡Ë‡"  26 5) ("ã„¡Ë‹"  26 7) ; ou
    ("ã„¨øˆ€€Ÿ"  27 0) ("ã„¨Ë‰"  27 1) ("ã„¨ËŠ"  27 3) ("ã„¨Ë‡"  27 5) ("ã„¨Ë‹"  27 7) ; u
    ("ã„¨ã„šøˆ€€Ÿ" 28 0) ("ã„¨ã„šË‰" 28 1) ("ã„¨ã„šËŠ" 28 3) ("ã„¨ã„šË‡" 28 5) ("ã„¨ã„šË‹" 28 7) ; ua
    ("ã„¨ã„žøˆ€€Ÿ" 29 0) ("ã„¨ã„žË‰" 29 1) ("ã„¨ã„žËŠ" 29 3) ("ã„¨ã„žË‡" 29 5) ("ã„¨ã„žË‹" 29 7) ; uai
    ("ã„¨ã„¢øˆ€€Ÿ" 30 0) ("ã„¨ã„¢Ë‰" 30 1) ("ã„¨ã„¢ËŠ" 30 3) ("ã„¨ã„¢Ë‡" 30 5) ("ã„¨ã„¢Ë‹" 30 7) ; uan
    ("ã„¨ã„¤øˆ€€Ÿ" 31 0) ("ã„¨ã„¤Ë‰" 31 1) ("ã„¨ã„¤ËŠ" 31 3) ("ã„¨ã„¤Ë‡" 31 5) ("ã„¨ã„¤Ë‹" 31 7) ; uang
    ("ã„©ã„øˆ€€Ÿ" 37 0) ("ã„©ã„Ë‰" 37 1) ("ã„©ã„ËŠ" 37 3) ("ã„©ã„Ë‡" 37 5) ("ã„©ã„Ë‹" 37 7) ; ue
    ("ã„¨ã„Ÿøˆ€€Ÿ" 33 0) ("ã„¨ã„ŸË‰" 33 1) ("ã„¨ã„ŸËŠ" 33 3) ("ã„¨ã„ŸË‡" 33 5) ("ã„¨ã„ŸË‹" 33 7) ; ui
    ("ã„¨ã„£øˆ€€Ÿ" 34 0) ("ã„¨ã„£Ë‰" 34 1) ("ã„¨ã„£ËŠ" 34 3) ("ã„¨ã„£Ë‡" 34 5) ("ã„¨ã„£Ë‹" 34 7) ; un
    ("ã„¨ã„›øˆ€€Ÿ" 35 0) ("ã„¨ã„›Ë‰" 35 1) ("ã„¨ã„›ËŠ" 35 3) ("ã„¨ã„›Ë‡" 35 5) ("ã„¨ã„›Ë‹" 35 7) ; uo
    ("ã„©øˆ€€Ÿ"  36 0) ("ã„©Ë‰"  36 1) ("ã„©ËŠ"  36 3) ("ã„©Ë‡"  36 5) ("ã„©Ë‹"  36 7) ; Ã¼
    ("ã„©ã„øˆ€€Ÿ" 37 0) ("ã„©ã„Ë‰" 37 1) ("ã„©ã„ËŠ" 37 3) ("ã„©ã„Ë‡" 37 5) ("ã„©ã„Ë‹" 37 7) ; Ã¼e
    ("0øˆ€€Ÿ"  38 0) ("1Ë‰"  38 1) ("2ËŠ"  38 3) ("3Ë‡"  38 5) ("4Ë‹"  38 7) ; undefined
    ("ã„©ã„¢øˆ€€Ÿ" 39 0) ("ã„©ã„¢Ë‰" 39 1) ("ã„©ã„¢ËŠ" 39 3) ("ã„©ã„¢Ë‡" 39 5) ("ã„©ã„¢Ë‹" 39 7) ; Ã¼an
    ("ã„©ã„£øˆ€€Ÿ" 40 0) ("ã„©ã„£Ë‰" 40 1) ("ã„©ã„£ËŠ" 40 3) ("ã„©ã„£Ë‡" 40 5) ("ã„©ã„£Ë‹" 40 7) ; Ã¼n
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
  (max (length "ZhuÄngøˆ€€Ÿ") (length "ã„“ã„¨ã„¤Ë‰")))

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
  (if (eq (string-match "^[A-Za-zÄ-Ç¹]+øˆ€€Ÿ" str) 0)
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
  (if (eq (string-match "^[ã„…-ã„©@0-4]+[øˆ€€ŸË‰ËŠË‡Ë‹]" str) 0)
      (let (end s y c z (zhuyin-len (length "ã„…")))
	(setq end (match-end 0)
	      c (substring str 0 zhuyin-len)
	      s (cdr (assoc c egg-zhuyin-shengmu))
	      y (assoc (substring str zhuyin-len end) egg-zhuyin-yunmu))
	(if (or (null (and s y))
		(and (or (eq s 11) (eq s 12)) (eq (nth 1 y) 0))) ; [ã„‡ã„‹][øˆ€€ŸË‰ËŠË‡Ë‹]
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
