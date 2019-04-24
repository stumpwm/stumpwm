;; Copyright (C) 2006-2008 Matthew Kennedy
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Mapping a keysym to a name is a client side activity in X11.  Some
;; of the code here was taken from the CMUCL Hemlocks code base.  The
;; actual mappings were taken from Xorg's keysymdefs.h.
;;
;; Code:

(in-package #:stumpwm)

(defvar *keysym-name-translations* (make-hash-table))
(defvar *name-keysym-translations* (make-hash-table :test #'equal))

(defun define-keysym (keysym name)
  "Define a mapping from a keysym name to a keysym."
  (setf (gethash keysym *keysym-name-translations*) name
        (gethash name *name-keysym-translations*) keysym))

(defun keysym-name->keysym (name)
  "Return the keysym corresponding to NAME."
  (multiple-value-bind (value present-p)
      (gethash name *name-keysym-translations*)
    (declare (ignore present-p))
    value))

(defun keysym->keysym-name (keysym)
  "Return the name corresponding to KEYSYM."
  (multiple-value-bind (value present-p)
      (gethash keysym *keysym-name-translations*)
    (declare (ignore present-p))
    value))

(define-keysym #xffffff "VoidSymbol")   ;Void symbol
(define-keysym #xff08 "BackSpace")      ;Back space, back char
(define-keysym #xff09 "Tab")
(define-keysym #xff0a "Linefeed")       ;Linefeed, LF
(define-keysym #xff0b "Clear")
(define-keysym #xff0d "Return")         ;Return, enter
(define-keysym #xff13 "Pause")          ;Pause, hold
(define-keysym #xff14 "Scroll_Lock")
(define-keysym #xff15 "Sys_Req")
(define-keysym #xff1b "Escape")
(define-keysym #xffff "Delete")         ;Delete, rubout
(define-keysym #xff20 "Multi_key")      ;Multi-key character compose
(define-keysym #xff37 "Codeinput")
(define-keysym #xff3c "SingleCandidate")
(define-keysym #xff3d "MultipleCandidate")
(define-keysym #xff3e "PreviousCandidate")
(define-keysym #xff21 "Kanji")          ;Kanji, Kanji convert
(define-keysym #xff22 "Muhenkan")       ;Cancel Conversion
(define-keysym #xff23 "Henkan_Mode")    ;Start/Stop Conversion
(define-keysym #xff23 "Henkan")         ;Alias for Henkan_Mode
(define-keysym #xff24 "Romaji")         ;to Romaji
(define-keysym #xff25 "Hiragana")       ;to Hiragana
(define-keysym #xff26 "Katakana")       ;to Katakana
(define-keysym #xff27 "Hiragana_Katakana") ;Hiragana/Katakana toggle
(define-keysym #xff28 "Zenkaku")        ;to Zenkaku
(define-keysym #xff29 "Hankaku")        ;to Hankaku
(define-keysym #xff2a "Zenkaku_Hankaku") ;Zenkaku/Hankaku toggle
(define-keysym #xff2b "Touroku")        ;Add to Dictionary
(define-keysym #xff2c "Massyo")         ;Delete from Dictionary
(define-keysym #xff2d "Kana_Lock")      ;Kana Lock
(define-keysym #xff2e "Kana_Shift")     ;Kana Shift
(define-keysym #xff2f "Eisu_Shift")     ;Alphanumeric Shift
(define-keysym #xff30 "Eisu_toggle")    ;Alphanumeric toggle
(define-keysym #xff37 "Kanji_Bangou")   ;Codeinput
(define-keysym #xff3d "Zen_Koho")       ;Multiple/All Candidate(s)
(define-keysym #xff3e "Mae_Koho")       ;Previous Candidate
(define-keysym #xff50 "Home")
(define-keysym #xff51 "Left")           ;Move left, left arrow
(define-keysym #xff52 "Up")             ;Move up, up arrow
(define-keysym #xff53 "Right")          ;Move right, right arrow
(define-keysym #xff54 "Down")           ;Move down, down arrow
(define-keysym #xff55 "Prior")          ;Prior, previous
(define-keysym #xff55 "Page_Up")
(define-keysym #xff56 "Next")           ;Next
(define-keysym #xff56 "Page_Down")
(define-keysym #xff57 "End")            ;EOL
(define-keysym #xff58 "Begin")          ;BOL
(define-keysym #xff60 "Select")         ;Select, mark
(define-keysym #xff61 "Print")
(define-keysym #xff62 "Execute")        ;Execute, run, do
(define-keysym #xff63 "Insert")         ;Insert, insert here
(define-keysym #xff65 "Undo")
(define-keysym #xff66 "Redo")           ;Redo, again
(define-keysym #xff67 "Menu")
(define-keysym #xff68 "Find")           ;Find, search
(define-keysym #xff69 "Cancel")         ;Cancel, stop, abort, exit
(define-keysym #xff6a "Help")           ;Help
(define-keysym #xff6b "Break")
(define-keysym #xff7e "Mode_switch")    ;Character set switch
(define-keysym #xff7e "script_switch")  ;Alias for mode_switch
(define-keysym #xff7f "Num_Lock")
(define-keysym #xff80 "KP_Space")       ;Space
(define-keysym #xff89 "KP_Tab")
(define-keysym #xff8d "KP_Enter")       ;Enter
(define-keysym #xff91 "KP_F1")          ;PF1, KP_A, ...
(define-keysym #xff92 "KP_F2")
(define-keysym #xff93 "KP_F3")
(define-keysym #xff94 "KP_F4")
(define-keysym #xff95 "KP_Home")
(define-keysym #xff96 "KP_Left")
(define-keysym #xff97 "KP_Up")
(define-keysym #xff98 "KP_Right")
(define-keysym #xff99 "KP_Down")
(define-keysym #xff9a "KP_Prior")
(define-keysym #xff9a "KP_Page_Up")
(define-keysym #xff9b "KP_Next")
(define-keysym #xff9b "KP_Page_Down")
(define-keysym #xff9c "KP_End")
(define-keysym #xff9d "KP_Begin")
(define-keysym #xff9e "KP_Insert")
(define-keysym #xff9f "KP_Delete")
(define-keysym #xffbd "KP_Equal")       ;Equals
(define-keysym #xffaa "KP_Multiply")
(define-keysym #xffab "KP_Add")
(define-keysym #xffac "KP_Separator")   ;Separator, often comma
(define-keysym #xffad "KP_Subtract")
(define-keysym #xffae "KP_Decimal")
(define-keysym #xffaf "KP_Divide")
(define-keysym #xffb0 "KP_0")
(define-keysym #xffb1 "KP_1")
(define-keysym #xffb2 "KP_2")
(define-keysym #xffb3 "KP_3")
(define-keysym #xffb4 "KP_4")
(define-keysym #xffb5 "KP_5")
(define-keysym #xffb6 "KP_6")
(define-keysym #xffb7 "KP_7")
(define-keysym #xffb8 "KP_8")
(define-keysym #xffb9 "KP_9")
(define-keysym #xffbe "F1")
(define-keysym #xffbf "F2")
(define-keysym #xffc0 "F3")
(define-keysym #xffc1 "F4")
(define-keysym #xffc2 "F5")
(define-keysym #xffc3 "F6")
(define-keysym #xffc4 "F7")
(define-keysym #xffc5 "F8")
(define-keysym #xffc6 "F9")
(define-keysym #xffc7 "F10")
(define-keysym #xffc8 "F11")
(define-keysym #xffc9 "F12")
(define-keysym #xffca "F13")
(define-keysym #xffcb "F14")
(define-keysym #xffcc "F15")
(define-keysym #xffcd "F16")
(define-keysym #xffce "F17")
(define-keysym #xffcf "F18")
(define-keysym #xffd0 "F19")
(define-keysym #xffd1 "F20")
(define-keysym #xffd2 "F21")
(define-keysym #xffd3 "F22")
(define-keysym #xffd4 "F23")
(define-keysym #xffd5 "F24")
(define-keysym #xffd6 "F25")
(define-keysym #xffd7 "F26")
(define-keysym #xffd8 "F27")
(define-keysym #xffd9 "F28")
(define-keysym #xffda "F29")
(define-keysym #xffdb "F30")
(define-keysym #xffdc "F31")
(define-keysym #xffdd "F32")
(define-keysym #xffde "F33")
(define-keysym #xffdf "F34")
(define-keysym #xffe0 "F35")
(define-keysym #xffe1 "Shift_L")        ;Left shift
(define-keysym #xffe2 "Shift_R")        ;Right shift
(define-keysym #xffe3 "Control_L")      ;Left control
(define-keysym #xffe4 "Control_R")      ;Right control
(define-keysym #xffe5 "Caps_Lock")      ;Caps lock
(define-keysym #xffe6 "Shift_Lock")     ;Shift lock
(define-keysym #xffe7 "Meta_L")         ;Left meta
(define-keysym #xffe8 "Meta_R")         ;Right meta
(define-keysym #xffe9 "Alt_L")          ;Left alt
(define-keysym #xffea "Alt_R")          ;Right alt
(define-keysym #xffeb "Super_L")        ;Left super
(define-keysym #xffec "Super_R")        ;Right super
(define-keysym #xffed "Hyper_L")        ;Left hyper
(define-keysym #xffee "Hyper_R")        ;Right hyper
(define-keysym #xfe01 "ISO_Lock")
(define-keysym #xfe02 "ISO_Level2_Latch")
(define-keysym #xfe03 "ISO_Level3_Shift")
(define-keysym #xfe04 "ISO_Level3_Latch")
(define-keysym #xfe05 "ISO_Level3_Lock")
(define-keysym #xff7e "ISO_Group_Shift") ;Alias for mode_switch
(define-keysym #xfe06 "ISO_Group_Latch")
(define-keysym #xfe07 "ISO_Group_Lock")
(define-keysym #xfe08 "ISO_Next_Group")
(define-keysym #xfe09 "ISO_Next_Group_Lock")
(define-keysym #xfe0a "ISO_Prev_Group")
(define-keysym #xfe0b "ISO_Prev_Group_Lock")
(define-keysym #xfe0c "ISO_First_Group")
(define-keysym #xfe0d "ISO_First_Group_Lock")
(define-keysym #xfe0e "ISO_Last_Group")
(define-keysym #xfe0f "ISO_Last_Group_Lock")
(define-keysym #xfe20 "ISO_Left_Tab")
(define-keysym #xfe21 "ISO_Move_Line_Up")
(define-keysym #xfe22 "ISO_Move_Line_Down")
(define-keysym #xfe23 "ISO_Partial_Line_Up")
(define-keysym #xfe24 "ISO_Partial_Line_Down")
(define-keysym #xfe25 "ISO_Partial_Space_Left")
(define-keysym #xfe26 "ISO_Partial_Space_Right")
(define-keysym #xfe27 "ISO_Set_Margin_Left")
(define-keysym #xfe28 "ISO_Set_Margin_Right")
(define-keysym #xfe29 "ISO_Release_Margin_Left")
(define-keysym #xfe2a "ISO_Release_Margin_Right")
(define-keysym #xfe2b "ISO_Release_Both_Margins")
(define-keysym #xfe2c "ISO_Fast_Cursor_Left")
(define-keysym #xfe2d "ISO_Fast_Cursor_Right")
(define-keysym #xfe2e "ISO_Fast_Cursor_Up")
(define-keysym #xfe2f "ISO_Fast_Cursor_Down")
(define-keysym #xfe30 "ISO_Continuous_Underline")
(define-keysym #xfe31 "ISO_Discontinuous_Underline")
(define-keysym #xfe32 "ISO_Emphasize")
(define-keysym #xfe33 "ISO_Center_Object")
(define-keysym #xfe34 "ISO_Enter")
(define-keysym #xfe50 "dead_grave")
(define-keysym #xfe51 "dead_acute")
(define-keysym #xfe52 "dead_circumflex")
(define-keysym #xfe53 "dead_tilde")
(define-keysym #xfe54 "dead_macron")
(define-keysym #xfe55 "dead_breve")
(define-keysym #xfe56 "dead_abovedot")
(define-keysym #xfe57 "dead_diaeresis")
(define-keysym #xfe58 "dead_abovering")
(define-keysym #xfe59 "dead_doubleacute")
(define-keysym #xfe5a "dead_caron")
(define-keysym #xfe5b "dead_cedilla")
(define-keysym #xfe5c "dead_ogonek")
(define-keysym #xfe5d "dead_iota")
(define-keysym #xfe5e "dead_voiced_sound")
(define-keysym #xfe5f "dead_semivoiced_sound")
(define-keysym #xfe60 "dead_belowdot")
(define-keysym #xfe61 "dead_hook")
(define-keysym #xfe62 "dead_horn")
(define-keysym #xfed0 "First_Virtual_Screen")
(define-keysym #xfed1 "Prev_Virtual_Screen")
(define-keysym #xfed2 "Next_Virtual_Screen")
(define-keysym #xfed4 "Last_Virtual_Screen")
(define-keysym #xfed5 "Terminate_Server")
(define-keysym #xfe70 "AccessX_Enable")
(define-keysym #xfe71 "AccessX_Feedback_Enable")
(define-keysym #xfe72 "RepeatKeys_Enable")
(define-keysym #xfe73 "SlowKeys_Enable")
(define-keysym #xfe74 "BounceKeys_Enable")
(define-keysym #xfe75 "StickyKeys_Enable")
(define-keysym #xfe76 "MouseKeys_Enable")
(define-keysym #xfe77 "MouseKeys_Accel_Enable")
(define-keysym #xfe78 "Overlay1_Enable")
(define-keysym #xfe79 "Overlay2_Enable")
(define-keysym #xfe7a "AudibleBell_Enable")
(define-keysym #xfee0 "Pointer_Left")
(define-keysym #xfee1 "Pointer_Right")
(define-keysym #xfee2 "Pointer_Up")
(define-keysym #xfee3 "Pointer_Down")
(define-keysym #xfee4 "Pointer_UpLeft")
(define-keysym #xfee5 "Pointer_UpRight")
(define-keysym #xfee6 "Pointer_DownLeft")
(define-keysym #xfee7 "Pointer_DownRight")
(define-keysym #xfee8 "Pointer_Button_Dflt")
(define-keysym #xfee9 "Pointer_Button1")
(define-keysym #xfeea "Pointer_Button2")
(define-keysym #xfeeb "Pointer_Button3")
(define-keysym #xfeec "Pointer_Button4")
(define-keysym #xfeed "Pointer_Button5")
(define-keysym #xfeee "Pointer_DblClick_Dflt")
(define-keysym #xfeef "Pointer_DblClick1")
(define-keysym #xfef0 "Pointer_DblClick2")
(define-keysym #xfef1 "Pointer_DblClick3")
(define-keysym #xfef2 "Pointer_DblClick4")
(define-keysym #xfef3 "Pointer_DblClick5")
(define-keysym #xfef4 "Pointer_Drag_Dflt")
(define-keysym #xfef5 "Pointer_Drag1")
(define-keysym #xfef6 "Pointer_Drag2")
(define-keysym #xfef7 "Pointer_Drag3")
(define-keysym #xfef8 "Pointer_Drag4")
(define-keysym #xfefd "Pointer_Drag5")
(define-keysym #xfef9 "Pointer_EnableKeys")
(define-keysym #xfefa "Pointer_Accelerate")
(define-keysym #xfefb "Pointer_DfltBtnNext")
(define-keysym #xfefc "Pointer_DfltBtnPrev")
(define-keysym #xfd01 "3270_Duplicate")
(define-keysym #xfd02 "3270_FieldMark")
(define-keysym #xfd03 "3270_Right2")
(define-keysym #xfd04 "3270_Left2")
(define-keysym #xfd05 "3270_BackTab")
(define-keysym #xfd06 "3270_EraseEOF")
(define-keysym #xfd07 "3270_EraseInput")
(define-keysym #xfd08 "3270_Reset")
(define-keysym #xfd09 "3270_Quit")
(define-keysym #xfd0a "3270_PA1")
(define-keysym #xfd0b "3270_PA2")
(define-keysym #xfd0c "3270_PA3")
(define-keysym #xfd0d "3270_Test")
(define-keysym #xfd0e "3270_Attn")
(define-keysym #xfd0f "3270_CursorBlink")
(define-keysym #xfd10 "3270_AltCursor")
(define-keysym #xfd11 "3270_KeyClick")
(define-keysym #xfd12 "3270_Jump")
(define-keysym #xfd13 "3270_Ident")
(define-keysym #xfd14 "3270_Rule")
(define-keysym #xfd15 "3270_Copy")
(define-keysym #xfd16 "3270_Play")
(define-keysym #xfd17 "3270_Setup")
(define-keysym #xfd18 "3270_Record")
(define-keysym #xfd19 "3270_ChangeScreen")
(define-keysym #xfd1a "3270_DeleteWord")
(define-keysym #xfd1b "3270_ExSelect")
(define-keysym #xfd1c "3270_CursorSelect")
(define-keysym #xfd1d "3270_PrintScreen")
(define-keysym #xfd1e "3270_Enter")
(define-keysym #x0020 "space")          ;U+0020 SPACE
(define-keysym #x0021 "exclam")         ;U+0021 EXCLAMATION MARK
(define-keysym #x0022 "quotedbl")       ;U+0022 QUOTATION MARK
(define-keysym #x0023 "numbersign")     ;U+0023 NUMBER SIGN
(define-keysym #x0024 "dollar")         ;U+0024 DOLLAR SIGN
(define-keysym #x0025 "percent")        ;U+0025 PERCENT SIGN
(define-keysym #x0026 "ampersand")      ;U+0026 AMPERSAND
(define-keysym #x0027 "apostrophe")     ;U+0027 APOSTROPHE
(define-keysym #x0027 "quoteright")     ;deprecated
(define-keysym #x0028 "parenleft")      ;U+0028 LEFT PARENTHESIS
(define-keysym #x0029 "parenright")     ;U+0029 RIGHT PARENTHESIS
(define-keysym #x002a "asterisk")       ;U+002A ASTERISK
(define-keysym #x002b "plus")           ;U+002B PLUS SIGN
(define-keysym #x002c "comma")          ;U+002C COMMA
(define-keysym #x002d "minus")          ;U+002D HYPHEN-MINUS
(define-keysym #x002e "period")         ;U+002E FULL STOP
(define-keysym #x002f "slash")          ;U+002F SOLIDUS
(define-keysym #x0030 "0")              ;U+0030 DIGIT ZERO
(define-keysym #x0031 "1")              ;U+0031 DIGIT ONE
(define-keysym #x0032 "2")              ;U+0032 DIGIT TWO
(define-keysym #x0033 "3")              ;U+0033 DIGIT THREE
(define-keysym #x0034 "4")              ;U+0034 DIGIT FOUR
(define-keysym #x0035 "5")              ;U+0035 DIGIT FIVE
(define-keysym #x0036 "6")              ;U+0036 DIGIT SIX
(define-keysym #x0037 "7")              ;U+0037 DIGIT SEVEN
(define-keysym #x0038 "8")              ;U+0038 DIGIT EIGHT
(define-keysym #x0039 "9")              ;U+0039 DIGIT NINE
(define-keysym #x003a "colon")          ;U+003A COLON
(define-keysym #x003b "semicolon")      ;U+003B SEMICOLON
(define-keysym #x003c "less")           ;U+003C LESS-THAN SIGN
(define-keysym #x003d "equal")          ;U+003D EQUALS SIGN
(define-keysym #x003e "greater")        ;U+003E GREATER-THAN SIGN
(define-keysym #x003f "question")       ;U+003F QUESTION MARK
(define-keysym #x0040 "at")             ;U+0040 COMMERCIAL AT
(define-keysym #x0041 "A")              ;U+0041 LATIN CAPITAL LETTER A
(define-keysym #x0042 "B")              ;U+0042 LATIN CAPITAL LETTER B
(define-keysym #x0043 "C")              ;U+0043 LATIN CAPITAL LETTER C
(define-keysym #x0044 "D")              ;U+0044 LATIN CAPITAL LETTER D
(define-keysym #x0045 "E")              ;U+0045 LATIN CAPITAL LETTER E
(define-keysym #x0046 "F")              ;U+0046 LATIN CAPITAL LETTER F
(define-keysym #x0047 "G")              ;U+0047 LATIN CAPITAL LETTER G
(define-keysym #x0048 "H")              ;U+0048 LATIN CAPITAL LETTER H
(define-keysym #x0049 "I")              ;U+0049 LATIN CAPITAL LETTER I
(define-keysym #x004a "J")              ;U+004A LATIN CAPITAL LETTER J
(define-keysym #x004b "K")              ;U+004B LATIN CAPITAL LETTER K
(define-keysym #x004c "L")              ;U+004C LATIN CAPITAL LETTER L
(define-keysym #x004d "M")              ;U+004D LATIN CAPITAL LETTER M
(define-keysym #x004e "N")              ;U+004E LATIN CAPITAL LETTER N
(define-keysym #x004f "O")              ;U+004F LATIN CAPITAL LETTER O
(define-keysym #x0050 "P")              ;U+0050 LATIN CAPITAL LETTER P
(define-keysym #x0051 "Q")              ;U+0051 LATIN CAPITAL LETTER Q
(define-keysym #x0052 "R")              ;U+0052 LATIN CAPITAL LETTER R
(define-keysym #x0053 "S")              ;U+0053 LATIN CAPITAL LETTER S
(define-keysym #x0054 "T")              ;U+0054 LATIN CAPITAL LETTER T
(define-keysym #x0055 "U")              ;U+0055 LATIN CAPITAL LETTER U
(define-keysym #x0056 "V")              ;U+0056 LATIN CAPITAL LETTER V
(define-keysym #x0057 "W")              ;U+0057 LATIN CAPITAL LETTER W
(define-keysym #x0058 "X")              ;U+0058 LATIN CAPITAL LETTER X
(define-keysym #x0059 "Y")              ;U+0059 LATIN CAPITAL LETTER Y
(define-keysym #x005a "Z")              ;U+005A LATIN CAPITAL LETTER Z
(define-keysym #x005b "bracketleft")    ;U+005B LEFT SQUARE BRACKET
(define-keysym #x005c "backslash")      ;U+005C REVERSE SOLIDUS
(define-keysym #x005d "bracketright")   ;U+005D RIGHT SQUARE BRACKET
(define-keysym #x005e "asciicircum")    ;U+005E CIRCUMFLEX ACCENT
(define-keysym #x005f "underscore")     ;U+005F LOW LINE
(define-keysym #x0060 "grave")          ;U+0060 GRAVE ACCENT
(define-keysym #x0060 "quoteleft")      ;deprecated
(define-keysym #x0061 "a")              ;U+0061 LATIN SMALL LETTER A
(define-keysym #x0062 "b")              ;U+0062 LATIN SMALL LETTER B
(define-keysym #x0063 "c")              ;U+0063 LATIN SMALL LETTER C
(define-keysym #x0064 "d")              ;U+0064 LATIN SMALL LETTER D
(define-keysym #x0065 "e")              ;U+0065 LATIN SMALL LETTER E
(define-keysym #x0066 "f")              ;U+0066 LATIN SMALL LETTER F
(define-keysym #x0067 "g")              ;U+0067 LATIN SMALL LETTER G
(define-keysym #x0068 "h")              ;U+0068 LATIN SMALL LETTER H
(define-keysym #x0069 "i")              ;U+0069 LATIN SMALL LETTER I
(define-keysym #x006a "j")              ;U+006A LATIN SMALL LETTER J
(define-keysym #x006b "k")              ;U+006B LATIN SMALL LETTER K
(define-keysym #x006c "l")              ;U+006C LATIN SMALL LETTER L
(define-keysym #x006d "m")              ;U+006D LATIN SMALL LETTER M
(define-keysym #x006e "n")              ;U+006E LATIN SMALL LETTER N
(define-keysym #x006f "o")              ;U+006F LATIN SMALL LETTER O
(define-keysym #x0070 "p")              ;U+0070 LATIN SMALL LETTER P
(define-keysym #x0071 "q")              ;U+0071 LATIN SMALL LETTER Q
(define-keysym #x0072 "r")              ;U+0072 LATIN SMALL LETTER R
(define-keysym #x0073 "s")              ;U+0073 LATIN SMALL LETTER S
(define-keysym #x0074 "t")              ;U+0074 LATIN SMALL LETTER T
(define-keysym #x0075 "u")              ;U+0075 LATIN SMALL LETTER U
(define-keysym #x0076 "v")              ;U+0076 LATIN SMALL LETTER V
(define-keysym #x0077 "w")              ;U+0077 LATIN SMALL LETTER W
(define-keysym #x0078 "x")              ;U+0078 LATIN SMALL LETTER X
(define-keysym #x0079 "y")              ;U+0079 LATIN SMALL LETTER Y
(define-keysym #x007a "z")              ;U+007A LATIN SMALL LETTER Z
(define-keysym #x007b "braceleft")      ;U+007B LEFT CURLY BRACKET
(define-keysym #x007c "bar")            ;U+007C VERTICAL LINE
(define-keysym #x007d "braceright")     ;U+007D RIGHT CURLY BRACKET
(define-keysym #x007e "asciitilde")     ;U+007E TILDE
(define-keysym #x00a0 "nobreakspace")   ;U+00A0 NO-BREAK SPACE
(define-keysym #x00a1 "exclamdown")  ;U+00A1 INVERTED EXCLAMATION MARK
(define-keysym #x00a2 "cent")           ;U+00A2 CENT SIGN
(define-keysym #x00a3 "sterling")       ;U+00A3 POUND SIGN
(define-keysym #x00a4 "currency")       ;U+00A4 CURRENCY SIGN
(define-keysym #x00a5 "yen")            ;U+00A5 YEN SIGN
(define-keysym #x00a6 "brokenbar")      ;U+00A6 BROKEN BAR
(define-keysym #x00a7 "section")        ;U+00A7 SECTION SIGN
(define-keysym #x00a8 "diaeresis")      ;U+00A8 DIAERESIS
(define-keysym #x00a9 "copyright")      ;U+00A9 COPYRIGHT SIGN
(define-keysym #x00aa "ordfeminine") ;U+00AA FEMININE ORDINAL INDICATOR
(define-keysym #x00ab "guillemotleft") ;U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-keysym #x00ac "notsign")        ;U+00AC NOT SIGN
(define-keysym #x00ad "hyphen")         ;U+00AD SOFT HYPHEN
(define-keysym #x00ae "registered")     ;U+00AE REGISTERED SIGN
(define-keysym #x00af "macron")         ;U+00AF MACRON
(define-keysym #x00b0 "degree")         ;U+00B0 DEGREE SIGN
(define-keysym #x00b1 "plusminus")      ;U+00B1 PLUS-MINUS SIGN
(define-keysym #x00b2 "twosuperior")    ;U+00B2 SUPERSCRIPT TWO
(define-keysym #x00b3 "threesuperior")  ;U+00B3 SUPERSCRIPT THREE
(define-keysym #x00b4 "acute")          ;U+00B4 ACUTE ACCENT
(define-keysym #x00b5 "mu")             ;U+00B5 MICRO SIGN
(define-keysym #x00b6 "paragraph")      ;U+00B6 PILCROW SIGN
(define-keysym #x00b7 "periodcentered") ;U+00B7 MIDDLE DOT
(define-keysym #x00b8 "cedilla")        ;U+00B8 CEDILLA
(define-keysym #x00b9 "onesuperior")    ;U+00B9 SUPERSCRIPT ONE
(define-keysym #x00ba "masculine") ;U+00BA MASCULINE ORDINAL INDICATOR
(define-keysym #x00bb "guillemotright") ;U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-keysym #x00bc "onequarter") ;U+00BC VULGAR FRACTION ONE QUARTER
(define-keysym #x00bd "onehalf")      ;U+00BD VULGAR FRACTION ONE HALF
(define-keysym #x00be "threequarters") ;U+00BE VULGAR FRACTION THREE QUARTERS
(define-keysym #x00bf "questiondown")   ;U+00BF INVERTED QUESTION MARK
(define-keysym #x00c0 "Agrave") ;U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
(define-keysym #x00c1 "Aacute") ;U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
(define-keysym #x00c2 "Acircumflex") ;U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
(define-keysym #x00c3 "Atilde") ;U+00C3 LATIN CAPITAL LETTER A WITH TILDE
(define-keysym #x00c4 "Adiaeresis") ;U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
(define-keysym #x00c5 "Aring") ;U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
(define-keysym #x00c6 "AE")            ;U+00C6 LATIN CAPITAL LETTER AE
(define-keysym #x00c7 "Ccedilla") ;U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
(define-keysym #x00c8 "Egrave") ;U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
(define-keysym #x00c9 "Eacute") ;U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
(define-keysym #x00ca "Ecircumflex") ;U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
(define-keysym #x00cb "Ediaeresis") ;U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
(define-keysym #x00cc "Igrave") ;U+00CC LATIN CAPITAL LETTER I WITH GRAVE
(define-keysym #x00cd "Iacute") ;U+00CD LATIN CAPITAL LETTER I WITH ACUTE
(define-keysym #x00ce "Icircumflex") ;U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
(define-keysym #x00cf "Idiaeresis") ;U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
(define-keysym #x00d0 "ETH")          ;U+00D0 LATIN CAPITAL LETTER ETH
(define-keysym #x00d0 "Eth")            ;deprecated
(define-keysym #x00d1 "Ntilde") ;U+00D1 LATIN CAPITAL LETTER N WITH TILDE
(define-keysym #x00d2 "Ograve") ;U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
(define-keysym #x00d3 "Oacute") ;U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
(define-keysym #x00d4 "Ocircumflex") ;U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
(define-keysym #x00d5 "Otilde") ;U+00D5 LATIN CAPITAL LETTER O WITH TILDE
(define-keysym #x00d6 "Odiaeresis") ;U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
(define-keysym #x00d7 "multiply")       ;U+00D7 MULTIPLICATION SIGN
(define-keysym #x00d8 "Oslash") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-keysym #x00d8 "Ooblique") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-keysym #x00d9 "Ugrave") ;U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
(define-keysym #x00da "Uacute") ;U+00DA LATIN CAPITAL LETTER U WITH ACUTE
(define-keysym #x00db "Ucircumflex") ;U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
(define-keysym #x00dc "Udiaeresis") ;U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
(define-keysym #x00dd "Yacute") ;U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
(define-keysym #x00de "THORN")      ;U+00DE LATIN CAPITAL LETTER THORN
(define-keysym #x00de "Thorn")          ;deprecated
(define-keysym #x00df "ssharp")     ;U+00DF LATIN SMALL LETTER SHARP S
(define-keysym #x00e0 "agrave") ;U+00E0 LATIN SMALL LETTER A WITH GRAVE
(define-keysym #x00e1 "aacute") ;U+00E1 LATIN SMALL LETTER A WITH ACUTE
(define-keysym #x00e2 "acircumflex") ;U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX
(define-keysym #x00e3 "atilde") ;U+00E3 LATIN SMALL LETTER A WITH TILDE
(define-keysym #x00e4 "adiaeresis") ;U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
(define-keysym #x00e5 "aring") ;U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
(define-keysym #x00e6 "ae")             ;U+00E6 LATIN SMALL LETTER AE
(define-keysym #x00e7 "ccedilla") ;U+00E7 LATIN SMALL LETTER C WITH CEDILLA
(define-keysym #x00e8 "egrave") ;U+00E8 LATIN SMALL LETTER E WITH GRAVE
(define-keysym #x00e9 "eacute") ;U+00E9 LATIN SMALL LETTER E WITH ACUTE
(define-keysym #x00ea "ecircumflex") ;U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
(define-keysym #x00eb "ediaeresis") ;U+00EB LATIN SMALL LETTER E WITH DIAERESIS
(define-keysym #x00ec "igrave") ;U+00EC LATIN SMALL LETTER I WITH GRAVE
(define-keysym #x00ed "iacute") ;U+00ED LATIN SMALL LETTER I WITH ACUTE
(define-keysym #x00ee "icircumflex") ;U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
(define-keysym #x00ef "idiaeresis") ;U+00EF LATIN SMALL LETTER I WITH DIAERESIS
(define-keysym #x00f0 "eth")            ;U+00F0 LATIN SMALL LETTER ETH
(define-keysym #x00f1 "ntilde") ;U+00F1 LATIN SMALL LETTER N WITH TILDE
(define-keysym #x00f2 "ograve") ;U+00F2 LATIN SMALL LETTER O WITH GRAVE
(define-keysym #x00f3 "oacute") ;U+00F3 LATIN SMALL LETTER O WITH ACUTE
(define-keysym #x00f4 "ocircumflex") ;U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
(define-keysym #x00f5 "otilde") ;U+00F5 LATIN SMALL LETTER O WITH TILDE
(define-keysym #x00f6 "odiaeresis") ;U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
(define-keysym #x00f7 "division")       ;U+00F7 DIVISION SIGN
(define-keysym #x00f8 "oslash") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-keysym #x00f8 "ooblique") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-keysym #x00f9 "ugrave") ;U+00F9 LATIN SMALL LETTER U WITH GRAVE
(define-keysym #x00fa "uacute") ;U+00FA LATIN SMALL LETTER U WITH ACUTE
(define-keysym #x00fb "ucircumflex") ;U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
(define-keysym #x00fc "udiaeresis") ;U+00FC LATIN SMALL LETTER U WITH DIAERESIS
(define-keysym #x00fd "yacute") ;U+00FD LATIN SMALL LETTER Y WITH ACUTE
(define-keysym #x00fe "thorn")        ;U+00FE LATIN SMALL LETTER THORN
(define-keysym #x00ff "ydiaeresis") ;U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
(define-keysym #x01a1 "Aogonek") ;U+0104 LATIN CAPITAL LETTER A WITH OGONEK
(define-keysym #x01a2 "breve")          ;U+02D8 BREVE
(define-keysym #x01a3 "Lstroke") ;U+0141 LATIN CAPITAL LETTER L WITH STROKE
(define-keysym #x01a5 "Lcaron") ;U+013D LATIN CAPITAL LETTER L WITH CARON
(define-keysym #x01a6 "Sacute") ;U+015A LATIN CAPITAL LETTER S WITH ACUTE
(define-keysym #x01a9 "Scaron") ;U+0160 LATIN CAPITAL LETTER S WITH CARON
(define-keysym #x01aa "Scedilla") ;U+015E LATIN CAPITAL LETTER S WITH CEDILLA
(define-keysym #x01ab "Tcaron") ;U+0164 LATIN CAPITAL LETTER T WITH CARON
(define-keysym #x01ac "Zacute") ;U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
(define-keysym #x01ae "Zcaron") ;U+017D LATIN CAPITAL LETTER Z WITH CARON
(define-keysym #x01af "Zabovedot") ;U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
(define-keysym #x01b1 "aogonek") ;U+0105 LATIN SMALL LETTER A WITH OGONEK
(define-keysym #x01b2 "ogonek")         ;U+02DB OGONEK
(define-keysym #x01b3 "lstroke") ;U+0142 LATIN SMALL LETTER L WITH STROKE
(define-keysym #x01b5 "lcaron") ;U+013E LATIN SMALL LETTER L WITH CARON
(define-keysym #x01b6 "sacute") ;U+015B LATIN SMALL LETTER S WITH ACUTE
(define-keysym #x01b7 "caron")          ;U+02C7 CARON
(define-keysym #x01b9 "scaron") ;U+0161 LATIN SMALL LETTER S WITH CARON
(define-keysym #x01ba "scedilla") ;U+015F LATIN SMALL LETTER S WITH CEDILLA
(define-keysym #x01bb "tcaron") ;U+0165 LATIN SMALL LETTER T WITH CARON
(define-keysym #x01bc "zacute") ;U+017A LATIN SMALL LETTER Z WITH ACUTE
(define-keysym #x01bd "doubleacute")    ;U+02DD DOUBLE ACUTE ACCENT
(define-keysym #x01be "zcaron") ;U+017E LATIN SMALL LETTER Z WITH CARON
(define-keysym #x01bf "zabovedot") ;U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
(define-keysym #x01c0 "Racute") ;U+0154 LATIN CAPITAL LETTER R WITH ACUTE
(define-keysym #x01c3 "Abreve") ;U+0102 LATIN CAPITAL LETTER A WITH BREVE
(define-keysym #x01c5 "Lacute") ;U+0139 LATIN CAPITAL LETTER L WITH ACUTE
(define-keysym #x01c6 "Cacute") ;U+0106 LATIN CAPITAL LETTER C WITH ACUTE
(define-keysym #x01c8 "Ccaron") ;U+010C LATIN CAPITAL LETTER C WITH CARON
(define-keysym #x01ca "Eogonek") ;U+0118 LATIN CAPITAL LETTER E WITH OGONEK
(define-keysym #x01cc "Ecaron") ;U+011A LATIN CAPITAL LETTER E WITH CARON
(define-keysym #x01cf "Dcaron") ;U+010E LATIN CAPITAL LETTER D WITH CARON
(define-keysym #x01d0 "Dstroke") ;U+0110 LATIN CAPITAL LETTER D WITH STROKE
(define-keysym #x01d1 "Nacute") ;U+0143 LATIN CAPITAL LETTER N WITH ACUTE
(define-keysym #x01d2 "Ncaron") ;U+0147 LATIN CAPITAL LETTER N WITH CARON
(define-keysym #x01d5 "Odoubleacute") ;U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
(define-keysym #x01d8 "Rcaron") ;U+0158 LATIN CAPITAL LETTER R WITH CARON
(define-keysym #x01d9 "Uring") ;U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
(define-keysym #x01db "Udoubleacute") ;U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
(define-keysym #x01de "Tcedilla") ;U+0162 LATIN CAPITAL LETTER T WITH CEDILLA
(define-keysym #x01e0 "racute") ;U+0155 LATIN SMALL LETTER R WITH ACUTE
(define-keysym #x01e3 "abreve") ;U+0103 LATIN SMALL LETTER A WITH BREVE
(define-keysym #x01e5 "lacute") ;U+013A LATIN SMALL LETTER L WITH ACUTE
(define-keysym #x01e6 "cacute") ;U+0107 LATIN SMALL LETTER C WITH ACUTE
(define-keysym #x01e8 "ccaron") ;U+010D LATIN SMALL LETTER C WITH CARON
(define-keysym #x01ea "eogonek") ;U+0119 LATIN SMALL LETTER E WITH OGONEK
(define-keysym #x01ec "ecaron") ;U+011B LATIN SMALL LETTER E WITH CARON
(define-keysym #x01ef "dcaron") ;U+010F LATIN SMALL LETTER D WITH CARON
(define-keysym #x01f0 "dstroke") ;U+0111 LATIN SMALL LETTER D WITH STROKE
(define-keysym #x01f1 "nacute") ;U+0144 LATIN SMALL LETTER N WITH ACUTE
(define-keysym #x01f2 "ncaron") ;U+0148 LATIN SMALL LETTER N WITH CARON
(define-keysym #x01f5 "odoubleacute") ;U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
(define-keysym #x01fb "udoubleacute") ;U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
(define-keysym #x01f8 "rcaron") ;U+0159 LATIN SMALL LETTER R WITH CARON
(define-keysym #x01f9 "uring") ;U+016F LATIN SMALL LETTER U WITH RING ABOVE
(define-keysym #x01fe "tcedilla") ;U+0163 LATIN SMALL LETTER T WITH CEDILLA
(define-keysym #x01ff "abovedot")       ;U+02D9 DOT ABOVE
(define-keysym #x02a1 "Hstroke") ;U+0126 LATIN CAPITAL LETTER H WITH STROKE
(define-keysym #x02a6 "Hcircumflex") ;U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX
(define-keysym #x02a9 "Iabovedot") ;U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
(define-keysym #x02ab "Gbreve") ;U+011E LATIN CAPITAL LETTER G WITH BREVE
(define-keysym #x02ac "Jcircumflex") ;U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX
(define-keysym #x02b1 "hstroke") ;U+0127 LATIN SMALL LETTER H WITH STROKE
(define-keysym #x02b6 "hcircumflex") ;U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX
(define-keysym #x02b9 "idotless") ;U+0131 LATIN SMALL LETTER DOTLESS I
(define-keysym #x02bb "gbreve") ;U+011F LATIN SMALL LETTER G WITH BREVE
(define-keysym #x02bc "jcircumflex") ;U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX
(define-keysym #x02c5 "Cabovedot") ;U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE
(define-keysym #x02c6 "Ccircumflex") ;U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX
(define-keysym #x02d5 "Gabovedot") ;U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE
(define-keysym #x02d8 "Gcircumflex") ;U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX
(define-keysym #x02dd "Ubreve") ;U+016C LATIN CAPITAL LETTER U WITH BREVE
(define-keysym #x02de "Scircumflex") ;U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX
(define-keysym #x02e5 "cabovedot") ;U+010B LATIN SMALL LETTER C WITH DOT ABOVE
(define-keysym #x02e6 "ccircumflex") ;U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX
(define-keysym #x02f5 "gabovedot") ;U+0121 LATIN SMALL LETTER G WITH DOT ABOVE
(define-keysym #x02f8 "gcircumflex") ;U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX
(define-keysym #x02fd "ubreve") ;U+016D LATIN SMALL LETTER U WITH BREVE
(define-keysym #x02fe "scircumflex") ;U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX
(define-keysym #x03a2 "kra")            ;U+0138 LATIN SMALL LETTER KRA
(define-keysym #x03a2 "kappa")          ;deprecated
(define-keysym #x03a3 "Rcedilla") ;U+0156 LATIN CAPITAL LETTER R WITH CEDILLA
(define-keysym #x03a5 "Itilde") ;U+0128 LATIN CAPITAL LETTER I WITH TILDE
(define-keysym #x03a6 "Lcedilla") ;U+013B LATIN CAPITAL LETTER L WITH CEDILLA
(define-keysym #x03aa "Emacron") ;U+0112 LATIN CAPITAL LETTER E WITH MACRON
(define-keysym #x03ab "Gcedilla") ;U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
(define-keysym #x03ac "Tslash") ;U+0166 LATIN CAPITAL LETTER T WITH STROKE
(define-keysym #x03b3 "rcedilla") ;U+0157 LATIN SMALL LETTER R WITH CEDILLA
(define-keysym #x03b5 "itilde") ;U+0129 LATIN SMALL LETTER I WITH TILDE
(define-keysym #x03b6 "lcedilla") ;U+013C LATIN SMALL LETTER L WITH CEDILLA
(define-keysym #x03ba "emacron") ;U+0113 LATIN SMALL LETTER E WITH MACRON
(define-keysym #x03bb "gcedilla") ;U+0123 LATIN SMALL LETTER G WITH CEDILLA
(define-keysym #x03bc "tslash") ;U+0167 LATIN SMALL LETTER T WITH STROKE
(define-keysym #x03bd "ENG")          ;U+014A LATIN CAPITAL LETTER ENG
(define-keysym #x03bf "eng")            ;U+014B LATIN SMALL LETTER ENG
(define-keysym #x03c0 "Amacron") ;U+0100 LATIN CAPITAL LETTER A WITH MACRON
(define-keysym #x03c7 "Iogonek") ;U+012E LATIN CAPITAL LETTER I WITH OGONEK
(define-keysym #x03cc "Eabovedot") ;U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE
(define-keysym #x03cf "Imacron") ;U+012A LATIN CAPITAL LETTER I WITH MACRON
(define-keysym #x03d1 "Ncedilla") ;U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
(define-keysym #x03d2 "Omacron") ;U+014C LATIN CAPITAL LETTER O WITH MACRON
(define-keysym #x03d3 "Kcedilla") ;U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
(define-keysym #x03d9 "Uogonek") ;U+0172 LATIN CAPITAL LETTER U WITH OGONEK
(define-keysym #x03dd "Utilde") ;U+0168 LATIN CAPITAL LETTER U WITH TILDE
(define-keysym #x03de "Umacron") ;U+016A LATIN CAPITAL LETTER U WITH MACRON
(define-keysym #x03e0 "amacron") ;U+0101 LATIN SMALL LETTER A WITH MACRON
(define-keysym #x03e7 "iogonek") ;U+012F LATIN SMALL LETTER I WITH OGONEK
(define-keysym #x03ec "eabovedot") ;U+0117 LATIN SMALL LETTER E WITH DOT ABOVE
(define-keysym #x03ef "imacron") ;U+012B LATIN SMALL LETTER I WITH MACRON
(define-keysym #x03f1 "ncedilla") ;U+0146 LATIN SMALL LETTER N WITH CEDILLA
(define-keysym #x03f2 "omacron") ;U+014D LATIN SMALL LETTER O WITH MACRON
(define-keysym #x03f3 "kcedilla") ;U+0137 LATIN SMALL LETTER K WITH CEDILLA
(define-keysym #x03f9 "uogonek") ;U+0173 LATIN SMALL LETTER U WITH OGONEK
(define-keysym #x03fd "utilde") ;U+0169 LATIN SMALL LETTER U WITH TILDE
(define-keysym #x03fe "umacron") ;U+016B LATIN SMALL LETTER U WITH MACRON
(define-keysym #x1001e02 "Babovedot") ;U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE
(define-keysym #x1001e03 "babovedot") ;U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE
(define-keysym #x1001e0a "Dabovedot") ;U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE
(define-keysym #x1001e80 "Wgrave") ;U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
(define-keysym #x1001e82 "Wacute") ;U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
(define-keysym #x1001e0b "dabovedot") ;U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE
(define-keysym #x1001ef2 "Ygrave") ;U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE
(define-keysym #x1001e1e "Fabovedot") ;U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE
(define-keysym #x1001e1f "fabovedot") ;U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE
(define-keysym #x1001e40 "Mabovedot") ;U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE
(define-keysym #x1001e41 "mabovedot") ;U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE
(define-keysym #x1001e56 "Pabovedot") ;U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE
(define-keysym #x1001e81 "wgrave") ;U+1E81 LATIN SMALL LETTER W WITH GRAVE
(define-keysym #x1001e57 "pabovedot") ;U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE
(define-keysym #x1001e83 "wacute") ;U+1E83 LATIN SMALL LETTER W WITH ACUTE
(define-keysym #x1001e60 "Sabovedot") ;U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE
(define-keysym #x1001ef3 "ygrave") ;U+1EF3 LATIN SMALL LETTER Y WITH GRAVE
(define-keysym #x1001e84 "Wdiaeresis") ;U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS
(define-keysym #x1001e85 "wdiaeresis") ;U+1E85 LATIN SMALL LETTER W WITH DIAERESIS
(define-keysym #x1001e61 "sabovedot") ;U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE
(define-keysym #x1000174 "Wcircumflex") ;U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX
(define-keysym #x1001e6a "Tabovedot") ;U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE
(define-keysym #x1000176 "Ycircumflex") ;U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
(define-keysym #x1000175 "wcircumflex") ;U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX
(define-keysym #x1001e6b "tabovedot") ;U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE
(define-keysym #x1000177 "ycircumflex") ;U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX
(define-keysym #x13bc "OE")          ;U+0152 LATIN CAPITAL LIGATURE OE
(define-keysym #x13bd "oe")            ;U+0153 LATIN SMALL LIGATURE OE
(define-keysym #x13be "Ydiaeresis") ;U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
(define-keysym #x047e "overline")       ;U+203E OVERLINE
(define-keysym #x04a1 "kana_fullstop")  ;U+3002 IDEOGRAPHIC FULL STOP
(define-keysym #x04a2 "kana_openingbracket") ;U+300C LEFT CORNER BRACKET
(define-keysym #x04a3 "kana_closingbracket") ;U+300D RIGHT CORNER BRACKET
(define-keysym #x04a4 "kana_comma")     ;U+3001 IDEOGRAPHIC COMMA
(define-keysym #x04a5 "kana_conjunctive") ;U+30FB KATAKANA MIDDLE DOT
(define-keysym #x04a5 "kana_middledot") ;deprecated
(define-keysym #x04a6 "kana_WO")        ;U+30F2 KATAKANA LETTER WO
(define-keysym #x04a7 "kana_a")        ;U+30A1 KATAKANA LETTER SMALL A
(define-keysym #x04a8 "kana_i")        ;U+30A3 KATAKANA LETTER SMALL I
(define-keysym #x04a9 "kana_u")        ;U+30A5 KATAKANA LETTER SMALL U
(define-keysym #x04aa "kana_e")        ;U+30A7 KATAKANA LETTER SMALL E
(define-keysym #x04ab "kana_o")        ;U+30A9 KATAKANA LETTER SMALL O
(define-keysym #x04ac "kana_ya")      ;U+30E3 KATAKANA LETTER SMALL YA
(define-keysym #x04ad "kana_yu")      ;U+30E5 KATAKANA LETTER SMALL YU
(define-keysym #x04ae "kana_yo")      ;U+30E7 KATAKANA LETTER SMALL YO
(define-keysym #x04af "kana_tsu")     ;U+30C3 KATAKANA LETTER SMALL TU
(define-keysym #x04af "kana_tu")        ;deprecated
(define-keysym #x04b0 "prolongedsound") ;U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK
(define-keysym #x04b1 "kana_A")         ;U+30A2 KATAKANA LETTER A
(define-keysym #x04b2 "kana_I")         ;U+30A4 KATAKANA LETTER I
(define-keysym #x04b3 "kana_U")         ;U+30A6 KATAKANA LETTER U
(define-keysym #x04b4 "kana_E")         ;U+30A8 KATAKANA LETTER E
(define-keysym #x04b5 "kana_O")         ;U+30AA KATAKANA LETTER O
(define-keysym #x04b6 "kana_KA")        ;U+30AB KATAKANA LETTER KA
(define-keysym #x04b7 "kana_KI")        ;U+30AD KATAKANA LETTER KI
(define-keysym #x04b8 "kana_KU")        ;U+30AF KATAKANA LETTER KU
(define-keysym #x04b9 "kana_KE")        ;U+30B1 KATAKANA LETTER KE
(define-keysym #x04ba "kana_KO")        ;U+30B3 KATAKANA LETTER KO
(define-keysym #x04bb "kana_SA")        ;U+30B5 KATAKANA LETTER SA
(define-keysym #x04bc "kana_SHI")       ;U+30B7 KATAKANA LETTER SI
(define-keysym #x04bd "kana_SU")        ;U+30B9 KATAKANA LETTER SU
(define-keysym #x04be "kana_SE")        ;U+30BB KATAKANA LETTER SE
(define-keysym #x04bf "kana_SO")        ;U+30BD KATAKANA LETTER SO
(define-keysym #x04c0 "kana_TA")        ;U+30BF KATAKANA LETTER TA
(define-keysym #x04c1 "kana_CHI")       ;U+30C1 KATAKANA LETTER TI
(define-keysym #x04c1 "kana_TI")        ;deprecated
(define-keysym #x04c2 "kana_TSU")       ;U+30C4 KATAKANA LETTER TU
(define-keysym #x04c2 "kana_TU")        ;deprecated
(define-keysym #x04c3 "kana_TE")        ;U+30C6 KATAKANA LETTER TE
(define-keysym #x04c4 "kana_TO")        ;U+30C8 KATAKANA LETTER TO
(define-keysym #x04c5 "kana_NA")        ;U+30CA KATAKANA LETTER NA
(define-keysym #x04c6 "kana_NI")        ;U+30CB KATAKANA LETTER NI
(define-keysym #x04c7 "kana_NU")        ;U+30CC KATAKANA LETTER NU
(define-keysym #x04c8 "kana_NE")        ;U+30CD KATAKANA LETTER NE
(define-keysym #x04c9 "kana_NO")        ;U+30CE KATAKANA LETTER NO
(define-keysym #x04ca "kana_HA")        ;U+30CF KATAKANA LETTER HA
(define-keysym #x04cb "kana_HI")        ;U+30D2 KATAKANA LETTER HI
(define-keysym #x04cc "kana_FU")        ;U+30D5 KATAKANA LETTER HU
(define-keysym #x04cc "kana_HU")        ;deprecated
(define-keysym #x04cd "kana_HE")        ;U+30D8 KATAKANA LETTER HE
(define-keysym #x04ce "kana_HO")        ;U+30DB KATAKANA LETTER HO
(define-keysym #x04cf "kana_MA")        ;U+30DE KATAKANA LETTER MA
(define-keysym #x04d0 "kana_MI")        ;U+30DF KATAKANA LETTER MI
(define-keysym #x04d1 "kana_MU")        ;U+30E0 KATAKANA LETTER MU
(define-keysym #x04d2 "kana_ME")        ;U+30E1 KATAKANA LETTER ME
(define-keysym #x04d3 "kana_MO")        ;U+30E2 KATAKANA LETTER MO
(define-keysym #x04d4 "kana_YA")        ;U+30E4 KATAKANA LETTER YA
(define-keysym #x04d5 "kana_YU")        ;U+30E6 KATAKANA LETTER YU
(define-keysym #x04d6 "kana_YO")        ;U+30E8 KATAKANA LETTER YO
(define-keysym #x04d7 "kana_RA")        ;U+30E9 KATAKANA LETTER RA
(define-keysym #x04d8 "kana_RI")        ;U+30EA KATAKANA LETTER RI
(define-keysym #x04d9 "kana_RU")        ;U+30EB KATAKANA LETTER RU
(define-keysym #x04da "kana_RE")        ;U+30EC KATAKANA LETTER RE
(define-keysym #x04db "kana_RO")        ;U+30ED KATAKANA LETTER RO
(define-keysym #x04dc "kana_WA")        ;U+30EF KATAKANA LETTER WA
(define-keysym #x04dd "kana_N")         ;U+30F3 KATAKANA LETTER N
(define-keysym #x04de "voicedsound") ;U+309B KATAKANA-HIRAGANA VOICED SOUND MARK
(define-keysym #x04df "semivoicedsound") ;U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
(define-keysym #xff7e "kana_switch")    ;Alias for mode_switch
(define-keysym #x10006f0 "Farsi_0") ;U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO
(define-keysym #x10006f1 "Farsi_1") ;U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE
(define-keysym #x10006f2 "Farsi_2") ;U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO
(define-keysym #x10006f3 "Farsi_3") ;U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE
(define-keysym #x10006f4 "Farsi_4") ;U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR
(define-keysym #x10006f5 "Farsi_5") ;U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE
(define-keysym #x10006f6 "Farsi_6") ;U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX
(define-keysym #x10006f7 "Farsi_7") ;U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN
(define-keysym #x10006f8 "Farsi_8") ;U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT
(define-keysym #x10006f9 "Farsi_9") ;U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE
(define-keysym #x100066a "Arabic_percent") ;U+066A ARABIC PERCENT SIGN
(define-keysym #x1000670 "Arabic_superscript_alef") ;U+0670 ARABIC LETTER SUPERSCRIPT ALEF
(define-keysym #x1000679 "Arabic_tteh") ;U+0679 ARABIC LETTER TTEH
(define-keysym #x100067e "Arabic_peh")  ;U+067E ARABIC LETTER PEH
(define-keysym #x1000686 "Arabic_tcheh") ;U+0686 ARABIC LETTER TCHEH
(define-keysym #x1000688 "Arabic_ddal") ;U+0688 ARABIC LETTER DDAL
(define-keysym #x1000691 "Arabic_rreh") ;U+0691 ARABIC LETTER RREH
(define-keysym #x05ac "Arabic_comma")   ;U+060C ARABIC COMMA
(define-keysym #x10006d4 "Arabic_fullstop") ;U+06D4 ARABIC FULL STOP
(define-keysym #x1000660 "Arabic_0")   ;U+0660 ARABIC-INDIC DIGIT ZERO
(define-keysym #x1000661 "Arabic_1")    ;U+0661 ARABIC-INDIC DIGIT ONE
(define-keysym #x1000662 "Arabic_2")    ;U+0662 ARABIC-INDIC DIGIT TWO
(define-keysym #x1000663 "Arabic_3")  ;U+0663 ARABIC-INDIC DIGIT THREE
(define-keysym #x1000664 "Arabic_4")   ;U+0664 ARABIC-INDIC DIGIT FOUR
(define-keysym #x1000665 "Arabic_5")   ;U+0665 ARABIC-INDIC DIGIT FIVE
(define-keysym #x1000666 "Arabic_6")    ;U+0666 ARABIC-INDIC DIGIT SIX
(define-keysym #x1000667 "Arabic_7")  ;U+0667 ARABIC-INDIC DIGIT SEVEN
(define-keysym #x1000668 "Arabic_8")  ;U+0668 ARABIC-INDIC DIGIT EIGHT
(define-keysym #x1000669 "Arabic_9")   ;U+0669 ARABIC-INDIC DIGIT NINE
(define-keysym #x05bb "Arabic_semicolon") ;U+061B ARABIC SEMICOLON
(define-keysym #x05bf "Arabic_question_mark") ;U+061F ARABIC QUESTION MARK
(define-keysym #x05c1 "Arabic_hamza")   ;U+0621 ARABIC LETTER HAMZA
(define-keysym #x05c2 "Arabic_maddaonalef") ;U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE
(define-keysym #x05c3 "Arabic_hamzaonalef") ;U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE
(define-keysym #x05c4 "Arabic_hamzaonwaw") ;U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE
(define-keysym #x05c5 "Arabic_hamzaunderalef") ;U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW
(define-keysym #x05c6 "Arabic_hamzaonyeh") ;U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE
(define-keysym #x05c7 "Arabic_alef")    ;U+0627 ARABIC LETTER ALEF
(define-keysym #x05c8 "Arabic_beh")     ;U+0628 ARABIC LETTER BEH
(define-keysym #x05c9 "Arabic_tehmarbuta") ;U+0629 ARABIC LETTER TEH MARBUTA
(define-keysym #x05ca "Arabic_teh")     ;U+062A ARABIC LETTER TEH
(define-keysym #x05cb "Arabic_theh")    ;U+062B ARABIC LETTER THEH
(define-keysym #x05cc "Arabic_jeem")    ;U+062C ARABIC LETTER JEEM
(define-keysym #x05cd "Arabic_hah")     ;U+062D ARABIC LETTER HAH
(define-keysym #x05ce "Arabic_khah")    ;U+062E ARABIC LETTER KHAH
(define-keysym #x05cf "Arabic_dal")     ;U+062F ARABIC LETTER DAL
(define-keysym #x05d0 "Arabic_thal")    ;U+0630 ARABIC LETTER THAL
(define-keysym #x05d1 "Arabic_ra")      ;U+0631 ARABIC LETTER REH
(define-keysym #x05d2 "Arabic_zain")    ;U+0632 ARABIC LETTER ZAIN
(define-keysym #x05d3 "Arabic_seen")    ;U+0633 ARABIC LETTER SEEN
(define-keysym #x05d4 "Arabic_sheen")   ;U+0634 ARABIC LETTER SHEEN
(define-keysym #x05d5 "Arabic_sad")     ;U+0635 ARABIC LETTER SAD
(define-keysym #x05d6 "Arabic_dad")     ;U+0636 ARABIC LETTER DAD
(define-keysym #x05d7 "Arabic_tah")     ;U+0637 ARABIC LETTER TAH
(define-keysym #x05d8 "Arabic_zah")     ;U+0638 ARABIC LETTER ZAH
(define-keysym #x05d9 "Arabic_ain")     ;U+0639 ARABIC LETTER AIN
(define-keysym #x05da "Arabic_ghain")   ;U+063A ARABIC LETTER GHAIN
(define-keysym #x05e0 "Arabic_tatweel") ;U+0640 ARABIC TATWEEL
(define-keysym #x05e1 "Arabic_feh")     ;U+0641 ARABIC LETTER FEH
(define-keysym #x05e2 "Arabic_qaf")     ;U+0642 ARABIC LETTER QAF
(define-keysym #x05e3 "Arabic_kaf")     ;U+0643 ARABIC LETTER KAF
(define-keysym #x05e4 "Arabic_lam")     ;U+0644 ARABIC LETTER LAM
(define-keysym #x05e5 "Arabic_meem")    ;U+0645 ARABIC LETTER MEEM
(define-keysym #x05e6 "Arabic_noon")    ;U+0646 ARABIC LETTER NOON
(define-keysym #x05e7 "Arabic_ha")      ;U+0647 ARABIC LETTER HEH
(define-keysym #x05e7 "Arabic_heh")     ;deprecated
(define-keysym #x05e8 "Arabic_waw")     ;U+0648 ARABIC LETTER WAW
(define-keysym #x05e9 "Arabic_alefmaksura") ;U+0649 ARABIC LETTER ALEF MAKSURA
(define-keysym #x05ea "Arabic_yeh")     ;U+064A ARABIC LETTER YEH
(define-keysym #x05eb "Arabic_fathatan") ;U+064B ARABIC FATHATAN
(define-keysym #x05ec "Arabic_dammatan") ;U+064C ARABIC DAMMATAN
(define-keysym #x05ed "Arabic_kasratan") ;U+064D ARABIC KASRATAN
(define-keysym #x05ee "Arabic_fatha")   ;U+064E ARABIC FATHA
(define-keysym #x05ef "Arabic_damma")   ;U+064F ARABIC DAMMA
(define-keysym #x05f0 "Arabic_kasra")   ;U+0650 ARABIC KASRA
(define-keysym #x05f1 "Arabic_shadda")  ;U+0651 ARABIC SHADDA
(define-keysym #x05f2 "Arabic_sukun")   ;U+0652 ARABIC SUKUN
(define-keysym #x1000653 "Arabic_madda_above") ;U+0653 ARABIC MADDAH ABOVE
(define-keysym #x1000654 "Arabic_hamza_above") ;U+0654 ARABIC HAMZA ABOVE
(define-keysym #x1000655 "Arabic_hamza_below") ;U+0655 ARABIC HAMZA BELOW
(define-keysym #x1000698 "Arabic_jeh")  ;U+0698 ARABIC LETTER JEH
(define-keysym #x10006a4 "Arabic_veh")  ;U+06A4 ARABIC LETTER VEH
(define-keysym #x10006a9 "Arabic_keheh") ;U+06A9 ARABIC LETTER KEHEH
(define-keysym #x10006af "Arabic_gaf")  ;U+06AF ARABIC LETTER GAF
(define-keysym #x10006ba "Arabic_noon_ghunna") ;U+06BA ARABIC LETTER NOON GHUNNA
(define-keysym #x10006be "Arabic_heh_doachashmee") ;U+06BE ARABIC LETTER HEH DOACHASHMEE
(define-keysym #x10006cc "Farsi_yeh")  ;U+06CC ARABIC LETTER FARSI YEH
(define-keysym #x10006cc "Arabic_farsi_yeh") ;U+06CC ARABIC LETTER FARSI YEH
(define-keysym #x10006d2 "Arabic_yeh_baree") ;U+06D2 ARABIC LETTER YEH BARREE
(define-keysym #x10006c1 "Arabic_heh_goal") ;U+06C1 ARABIC LETTER HEH GOAL
(define-keysym #xff7e "Arabic_switch")  ;Alias for mode_switch
(define-keysym #x1000492 "Cyrillic_GHE_bar") ;U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE
(define-keysym #x1000493 "Cyrillic_ghe_bar") ;U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE
(define-keysym #x1000496 "Cyrillic_ZHE_descender") ;U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
(define-keysym #x1000497 "Cyrillic_zhe_descender") ;U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER
(define-keysym #x100049a "Cyrillic_KA_descender") ;U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER
(define-keysym #x100049b "Cyrillic_ka_descender") ;U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER
(define-keysym #x100049c "Cyrillic_KA_vertstroke") ;U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
(define-keysym #x100049d "Cyrillic_ka_vertstroke") ;U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
(define-keysym #x10004a2 "Cyrillic_EN_descender") ;U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER
(define-keysym #x10004a3 "Cyrillic_en_descender") ;U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER
(define-keysym #x10004ae "Cyrillic_U_straight") ;U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U
(define-keysym #x10004af "Cyrillic_u_straight") ;U+04AF CYRILLIC SMALL LETTER STRAIGHT U
(define-keysym #x10004b0 "Cyrillic_U_straight_bar") ;U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
(define-keysym #x10004b1 "Cyrillic_u_straight_bar") ;U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
(define-keysym #x10004b2 "Cyrillic_HA_descender") ;U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER
(define-keysym #x10004b3 "Cyrillic_ha_descender") ;U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER
(define-keysym #x10004b6 "Cyrillic_CHE_descender") ;U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
(define-keysym #x10004b7 "Cyrillic_che_descender") ;U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER
(define-keysym #x10004b8 "Cyrillic_CHE_vertstroke") ;U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
(define-keysym #x10004b9 "Cyrillic_che_vertstroke") ;U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
(define-keysym #x10004ba "Cyrillic_SHHA") ;U+04BA CYRILLIC CAPITAL LETTER SHHA
(define-keysym #x10004bb "Cyrillic_shha") ;U+04BB CYRILLIC SMALL LETTER SHHA
(define-keysym #x10004d8 "Cyrillic_SCHWA") ;U+04D8 CYRILLIC CAPITAL LETTER SCHWA
(define-keysym #x10004d9 "Cyrillic_schwa") ;U+04D9 CYRILLIC SMALL LETTER SCHWA
(define-keysym #x10004e2 "Cyrillic_I_macron") ;U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON
(define-keysym #x10004e3 "Cyrillic_i_macron") ;U+04E3 CYRILLIC SMALL LETTER I WITH MACRON
(define-keysym #x10004e8 "Cyrillic_O_bar") ;U+04E8 CYRILLIC CAPITAL LETTER BARRED O
(define-keysym #x10004e9 "Cyrillic_o_bar") ;U+04E9 CYRILLIC SMALL LETTER BARRED O
(define-keysym #x10004ee "Cyrillic_U_macron") ;U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON
(define-keysym #x10004ef "Cyrillic_u_macron") ;U+04EF CYRILLIC SMALL LETTER U WITH MACRON
(define-keysym #x06a1 "Serbian_dje") ;U+0452 CYRILLIC SMALL LETTER DJE
(define-keysym #x06a2 "Macedonia_gje") ;U+0453 CYRILLIC SMALL LETTER GJE
(define-keysym #x06a3 "Cyrillic_io")  ;U+0451 CYRILLIC SMALL LETTER IO
(define-keysym #x06a4 "Ukrainian_ie") ;U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE
(define-keysym #x06a4 "Ukranian_je")    ;deprecated
(define-keysym #x06a5 "Macedonia_dse") ;U+0455 CYRILLIC SMALL LETTER DZE
(define-keysym #x06a6 "Ukrainian_i") ;U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
(define-keysym #x06a6 "Ukranian_i")     ;deprecated
(define-keysym #x06a7 "Ukrainian_yi") ;U+0457 CYRILLIC SMALL LETTER YI
(define-keysym #x06a7 "Ukranian_yi")    ;deprecated
(define-keysym #x06a8 "Cyrillic_je")  ;U+0458 CYRILLIC SMALL LETTER JE
(define-keysym #x06a8 "Serbian_je")     ;deprecated
(define-keysym #x06a9 "Cyrillic_lje") ;U+0459 CYRILLIC SMALL LETTER LJE
(define-keysym #x06a9 "Serbian_lje")    ;deprecated
(define-keysym #x06aa "Cyrillic_nje") ;U+045A CYRILLIC SMALL LETTER NJE
(define-keysym #x06aa "Serbian_nje")    ;deprecated
(define-keysym #x06ab "Serbian_tshe") ;U+045B CYRILLIC SMALL LETTER TSHE
(define-keysym #x06ac "Macedonia_kje") ;U+045C CYRILLIC SMALL LETTER KJE
(define-keysym #x06ad "Ukrainian_ghe_with_upturn") ;U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN
(define-keysym #x06ae "Byelorussian_shortu") ;U+045E CYRILLIC SMALL LETTER SHORT U
(define-keysym #x06af "Cyrillic_dzhe") ;U+045F CYRILLIC SMALL LETTER DZHE
(define-keysym #x06af "Serbian_dze")    ;deprecated
(define-keysym #x06b0 "numerosign")     ;U+2116 NUMERO SIGN
(define-keysym #x06b1 "Serbian_DJE") ;U+0402 CYRILLIC CAPITAL LETTER DJE
(define-keysym #x06b2 "Macedonia_GJE") ;U+0403 CYRILLIC CAPITAL LETTER GJE
(define-keysym #x06b3 "Cyrillic_IO") ;U+0401 CYRILLIC CAPITAL LETTER IO
(define-keysym #x06b4 "Ukrainian_IE") ;U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE
(define-keysym #x06b4 "Ukranian_JE")    ;deprecated
(define-keysym #x06b5 "Macedonia_DSE") ;U+0405 CYRILLIC CAPITAL LETTER DZE
(define-keysym #x06b6 "Ukrainian_I") ;U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
(define-keysym #x06b6 "Ukranian_I")     ;deprecated
(define-keysym #x06b7 "Ukrainian_YI") ;U+0407 CYRILLIC CAPITAL LETTER YI
(define-keysym #x06b7 "Ukranian_YI")    ;deprecated
(define-keysym #x06b8 "Cyrillic_JE") ;U+0408 CYRILLIC CAPITAL LETTER JE
(define-keysym #x06b8 "Serbian_JE")     ;deprecated
(define-keysym #x06b9 "Cyrillic_LJE") ;U+0409 CYRILLIC CAPITAL LETTER LJE
(define-keysym #x06b9 "Serbian_LJE")    ;deprecated
(define-keysym #x06ba "Cyrillic_NJE") ;U+040A CYRILLIC CAPITAL LETTER NJE
(define-keysym #x06ba "Serbian_NJE")    ;deprecated
(define-keysym #x06bb "Serbian_TSHE") ;U+040B CYRILLIC CAPITAL LETTER TSHE
(define-keysym #x06bc "Macedonia_KJE") ;U+040C CYRILLIC CAPITAL LETTER KJE
(define-keysym #x06bd "Ukrainian_GHE_WITH_UPTURN") ;U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN
(define-keysym #x06be "Byelorussian_SHORTU") ;U+040E CYRILLIC CAPITAL LETTER SHORT U
(define-keysym #x06bf "Cyrillic_DZHE") ;U+040F CYRILLIC CAPITAL LETTER DZHE
(define-keysym #x06bf "Serbian_DZE")    ;deprecated
(define-keysym #x06c0 "Cyrillic_yu")  ;U+044E CYRILLIC SMALL LETTER YU
(define-keysym #x06c1 "Cyrillic_a")    ;U+0430 CYRILLIC SMALL LETTER A
(define-keysym #x06c2 "Cyrillic_be")  ;U+0431 CYRILLIC SMALL LETTER BE
(define-keysym #x06c3 "Cyrillic_tse") ;U+0446 CYRILLIC SMALL LETTER TSE
(define-keysym #x06c4 "Cyrillic_de")  ;U+0434 CYRILLIC SMALL LETTER DE
(define-keysym #x06c5 "Cyrillic_ie")  ;U+0435 CYRILLIC SMALL LETTER IE
(define-keysym #x06c6 "Cyrillic_ef")  ;U+0444 CYRILLIC SMALL LETTER EF
(define-keysym #x06c7 "Cyrillic_ghe") ;U+0433 CYRILLIC SMALL LETTER GHE
(define-keysym #x06c8 "Cyrillic_ha")  ;U+0445 CYRILLIC SMALL LETTER HA
(define-keysym #x06c9 "Cyrillic_i")    ;U+0438 CYRILLIC SMALL LETTER I
(define-keysym #x06ca "Cyrillic_shorti") ;U+0439 CYRILLIC SMALL LETTER SHORT I
(define-keysym #x06cb "Cyrillic_ka")  ;U+043A CYRILLIC SMALL LETTER KA
(define-keysym #x06cc "Cyrillic_el")  ;U+043B CYRILLIC SMALL LETTER EL
(define-keysym #x06cd "Cyrillic_em")  ;U+043C CYRILLIC SMALL LETTER EM
(define-keysym #x06ce "Cyrillic_en")  ;U+043D CYRILLIC SMALL LETTER EN
(define-keysym #x06cf "Cyrillic_o")    ;U+043E CYRILLIC SMALL LETTER O
(define-keysym #x06d0 "Cyrillic_pe")  ;U+043F CYRILLIC SMALL LETTER PE
(define-keysym #x06d1 "Cyrillic_ya")  ;U+044F CYRILLIC SMALL LETTER YA
(define-keysym #x06d2 "Cyrillic_er")  ;U+0440 CYRILLIC SMALL LETTER ER
(define-keysym #x06d3 "Cyrillic_es")  ;U+0441 CYRILLIC SMALL LETTER ES
(define-keysym #x06d4 "Cyrillic_te")  ;U+0442 CYRILLIC SMALL LETTER TE
(define-keysym #x06d5 "Cyrillic_u")    ;U+0443 CYRILLIC SMALL LETTER U
(define-keysym #x06d6 "Cyrillic_zhe") ;U+0436 CYRILLIC SMALL LETTER ZHE
(define-keysym #x06d7 "Cyrillic_ve")  ;U+0432 CYRILLIC SMALL LETTER VE
(define-keysym #x06d8 "Cyrillic_softsign") ;U+044C CYRILLIC SMALL LETTER SOFT SIGN
(define-keysym #x06d9 "Cyrillic_yeru") ;U+044B CYRILLIC SMALL LETTER YERU
(define-keysym #x06da "Cyrillic_ze")  ;U+0437 CYRILLIC SMALL LETTER ZE
(define-keysym #x06db "Cyrillic_sha") ;U+0448 CYRILLIC SMALL LETTER SHA
(define-keysym #x06dc "Cyrillic_e")    ;U+044D CYRILLIC SMALL LETTER E
(define-keysym #x06dd "Cyrillic_shcha") ;U+0449 CYRILLIC SMALL LETTER SHCHA
(define-keysym #x06de "Cyrillic_che") ;U+0447 CYRILLIC SMALL LETTER CHE
(define-keysym #x06df "Cyrillic_hardsign") ;U+044A CYRILLIC SMALL LETTER HARD SIGN
(define-keysym #x06e0 "Cyrillic_YU") ;U+042E CYRILLIC CAPITAL LETTER YU
(define-keysym #x06e1 "Cyrillic_A")  ;U+0410 CYRILLIC CAPITAL LETTER A
(define-keysym #x06e2 "Cyrillic_BE") ;U+0411 CYRILLIC CAPITAL LETTER BE
(define-keysym #x06e3 "Cyrillic_TSE") ;U+0426 CYRILLIC CAPITAL LETTER TSE
(define-keysym #x06e4 "Cyrillic_DE") ;U+0414 CYRILLIC CAPITAL LETTER DE
(define-keysym #x06e5 "Cyrillic_IE") ;U+0415 CYRILLIC CAPITAL LETTER IE
(define-keysym #x06e6 "Cyrillic_EF") ;U+0424 CYRILLIC CAPITAL LETTER EF
(define-keysym #x06e7 "Cyrillic_GHE") ;U+0413 CYRILLIC CAPITAL LETTER GHE
(define-keysym #x06e8 "Cyrillic_HA") ;U+0425 CYRILLIC CAPITAL LETTER HA
(define-keysym #x06e9 "Cyrillic_I")  ;U+0418 CYRILLIC CAPITAL LETTER I
(define-keysym #x06ea "Cyrillic_SHORTI") ;U+0419 CYRILLIC CAPITAL LETTER SHORT I
(define-keysym #x06eb "Cyrillic_KA") ;U+041A CYRILLIC CAPITAL LETTER KA
(define-keysym #x06ec "Cyrillic_EL") ;U+041B CYRILLIC CAPITAL LETTER EL
(define-keysym #x06ed "Cyrillic_EM") ;U+041C CYRILLIC CAPITAL LETTER EM
(define-keysym #x06ee "Cyrillic_EN") ;U+041D CYRILLIC CAPITAL LETTER EN
(define-keysym #x06ef "Cyrillic_O")  ;U+041E CYRILLIC CAPITAL LETTER O
(define-keysym #x06f0 "Cyrillic_PE") ;U+041F CYRILLIC CAPITAL LETTER PE
(define-keysym #x06f1 "Cyrillic_YA") ;U+042F CYRILLIC CAPITAL LETTER YA
(define-keysym #x06f2 "Cyrillic_ER") ;U+0420 CYRILLIC CAPITAL LETTER ER
(define-keysym #x06f3 "Cyrillic_ES") ;U+0421 CYRILLIC CAPITAL LETTER ES
(define-keysym #x06f4 "Cyrillic_TE") ;U+0422 CYRILLIC CAPITAL LETTER TE
(define-keysym #x06f5 "Cyrillic_U")  ;U+0423 CYRILLIC CAPITAL LETTER U
(define-keysym #x06f6 "Cyrillic_ZHE") ;U+0416 CYRILLIC CAPITAL LETTER ZHE
(define-keysym #x06f7 "Cyrillic_VE") ;U+0412 CYRILLIC CAPITAL LETTER VE
(define-keysym #x06f8 "Cyrillic_SOFTSIGN") ;U+042C CYRILLIC CAPITAL LETTER SOFT SIGN
(define-keysym #x06f9 "Cyrillic_YERU") ;U+042B CYRILLIC CAPITAL LETTER YERU
(define-keysym #x06fa "Cyrillic_ZE") ;U+0417 CYRILLIC CAPITAL LETTER ZE
(define-keysym #x06fb "Cyrillic_SHA") ;U+0428 CYRILLIC CAPITAL LETTER SHA
(define-keysym #x06fc "Cyrillic_E")  ;U+042D CYRILLIC CAPITAL LETTER E
(define-keysym #x06fd "Cyrillic_SHCHA") ;U+0429 CYRILLIC CAPITAL LETTER SHCHA
(define-keysym #x06fe "Cyrillic_CHE") ;U+0427 CYRILLIC CAPITAL LETTER CHE
(define-keysym #x06ff "Cyrillic_HARDSIGN") ;U+042A CYRILLIC CAPITAL LETTER HARD SIGN
(define-keysym #x07a1 "Greek_ALPHAaccent") ;U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS
(define-keysym #x07a2 "Greek_EPSILONaccent") ;U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS
(define-keysym #x07a3 "Greek_ETAaccent") ;U+0389 GREEK CAPITAL LETTER ETA WITH TONOS
(define-keysym #x07a4 "Greek_IOTAaccent") ;U+038A GREEK CAPITAL LETTER IOTA WITH TONOS
(define-keysym #x07a5 "Greek_IOTAdieresis") ;U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
(define-keysym #x07a5 "Greek_IOTAdiaeresis") ;old typo
(define-keysym #x07a7 "Greek_OMICRONaccent") ;U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS
(define-keysym #x07a8 "Greek_UPSILONaccent") ;U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS
(define-keysym #x07a9 "Greek_UPSILONdieresis") ;U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
(define-keysym #x07ab "Greek_OMEGAaccent") ;U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS
(define-keysym #x07ae "Greek_accentdieresis") ;U+0385 GREEK DIALYTIKA TONOS
(define-keysym #x07af "Greek_horizbar") ;U+2015 HORIZONTAL BAR
(define-keysym #x07b1 "Greek_alphaaccent") ;U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
(define-keysym #x07b2 "Greek_epsilonaccent") ;U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
(define-keysym #x07b3 "Greek_etaaccent") ;U+03AE GREEK SMALL LETTER ETA WITH TONOS
(define-keysym #x07b4 "Greek_iotaaccent") ;U+03AF GREEK SMALL LETTER IOTA WITH TONOS
(define-keysym #x07b5 "Greek_iotadieresis") ;U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
(define-keysym #x07b6 "Greek_iotaaccentdieresis") ;U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
(define-keysym #x07b7 "Greek_omicronaccent") ;U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
(define-keysym #x07b8 "Greek_upsilonaccent") ;U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
(define-keysym #x07b9 "Greek_upsilondieresis") ;U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
(define-keysym #x07ba "Greek_upsilonaccentdieresis") ;U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
(define-keysym #x07bb "Greek_omegaaccent") ;U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
(define-keysym #x07c1 "Greek_ALPHA") ;U+0391 GREEK CAPITAL LETTER ALPHA
(define-keysym #x07c2 "Greek_BETA")  ;U+0392 GREEK CAPITAL LETTER BETA
(define-keysym #x07c3 "Greek_GAMMA") ;U+0393 GREEK CAPITAL LETTER GAMMA
(define-keysym #x07c4 "Greek_DELTA") ;U+0394 GREEK CAPITAL LETTER DELTA
(define-keysym #x07c5 "Greek_EPSILON") ;U+0395 GREEK CAPITAL LETTER EPSILON
(define-keysym #x07c6 "Greek_ZETA")  ;U+0396 GREEK CAPITAL LETTER ZETA
(define-keysym #x07c7 "Greek_ETA")    ;U+0397 GREEK CAPITAL LETTER ETA
(define-keysym #x07c8 "Greek_THETA") ;U+0398 GREEK CAPITAL LETTER THETA
(define-keysym #x07c9 "Greek_IOTA")  ;U+0399 GREEK CAPITAL LETTER IOTA
(define-keysym #x07ca "Greek_KAPPA") ;U+039A GREEK CAPITAL LETTER KAPPA
(define-keysym #x07cb "Greek_LAMDA") ;U+039B GREEK CAPITAL LETTER LAMDA
(define-keysym #x07cb "Greek_LAMBDA") ;U+039B GREEK CAPITAL LETTER LAMDA
(define-keysym #x07cc "Greek_MU")      ;U+039C GREEK CAPITAL LETTER MU
(define-keysym #x07cd "Greek_NU")      ;U+039D GREEK CAPITAL LETTER NU
(define-keysym #x07ce "Greek_XI")      ;U+039E GREEK CAPITAL LETTER XI
(define-keysym #x07cf "Greek_OMICRON") ;U+039F GREEK CAPITAL LETTER OMICRON
(define-keysym #x07d0 "Greek_PI")      ;U+03A0 GREEK CAPITAL LETTER PI
(define-keysym #x07d1 "Greek_RHO")    ;U+03A1 GREEK CAPITAL LETTER RHO
(define-keysym #x07d2 "Greek_SIGMA") ;U+03A3 GREEK CAPITAL LETTER SIGMA
(define-keysym #x07d4 "Greek_TAU")    ;U+03A4 GREEK CAPITAL LETTER TAU
(define-keysym #x07d5 "Greek_UPSILON") ;U+03A5 GREEK CAPITAL LETTER UPSILON
(define-keysym #x07d6 "Greek_PHI")    ;U+03A6 GREEK CAPITAL LETTER PHI
(define-keysym #x07d7 "Greek_CHI")    ;U+03A7 GREEK CAPITAL LETTER CHI
(define-keysym #x07d8 "Greek_PSI")    ;U+03A8 GREEK CAPITAL LETTER PSI
(define-keysym #x07d9 "Greek_OMEGA") ;U+03A9 GREEK CAPITAL LETTER OMEGA
(define-keysym #x07e1 "Greek_alpha")  ;U+03B1 GREEK SMALL LETTER ALPHA
(define-keysym #x07e2 "Greek_beta")    ;U+03B2 GREEK SMALL LETTER BETA
(define-keysym #x07e3 "Greek_gamma")  ;U+03B3 GREEK SMALL LETTER GAMMA
(define-keysym #x07e4 "Greek_delta")  ;U+03B4 GREEK SMALL LETTER DELTA
(define-keysym #x07e5 "Greek_epsilon") ;U+03B5 GREEK SMALL LETTER EPSILON
(define-keysym #x07e6 "Greek_zeta")    ;U+03B6 GREEK SMALL LETTER ZETA
(define-keysym #x07e7 "Greek_eta")      ;U+03B7 GREEK SMALL LETTER ETA
(define-keysym #x07e8 "Greek_theta")  ;U+03B8 GREEK SMALL LETTER THETA
(define-keysym #x07e9 "Greek_iota")    ;U+03B9 GREEK SMALL LETTER IOTA
(define-keysym #x07ea "Greek_kappa")  ;U+03BA GREEK SMALL LETTER KAPPA
(define-keysym #x07eb "Greek_lamda")  ;U+03BB GREEK SMALL LETTER LAMDA
(define-keysym #x07eb "Greek_lambda") ;U+03BB GREEK SMALL LETTER LAMDA
(define-keysym #x07ec "Greek_mu")       ;U+03BC GREEK SMALL LETTER MU
(define-keysym #x07ed "Greek_nu")       ;U+03BD GREEK SMALL LETTER NU
(define-keysym #x07ee "Greek_xi")       ;U+03BE GREEK SMALL LETTER XI
(define-keysym #x07ef "Greek_omicron") ;U+03BF GREEK SMALL LETTER OMICRON
(define-keysym #x07f0 "Greek_pi")       ;U+03C0 GREEK SMALL LETTER PI
(define-keysym #x07f1 "Greek_rho")      ;U+03C1 GREEK SMALL LETTER RHO
(define-keysym #x07f2 "Greek_sigma")  ;U+03C3 GREEK SMALL LETTER SIGMA
(define-keysym #x07f3 "Greek_finalsmallsigma") ;U+03C2 GREEK SMALL LETTER FINAL SIGMA
(define-keysym #x07f4 "Greek_tau")      ;U+03C4 GREEK SMALL LETTER TAU
(define-keysym #x07f5 "Greek_upsilon") ;U+03C5 GREEK SMALL LETTER UPSILON
(define-keysym #x07f6 "Greek_phi")      ;U+03C6 GREEK SMALL LETTER PHI
(define-keysym #x07f7 "Greek_chi")      ;U+03C7 GREEK SMALL LETTER CHI
(define-keysym #x07f8 "Greek_psi")      ;U+03C8 GREEK SMALL LETTER PSI
(define-keysym #x07f9 "Greek_omega")  ;U+03C9 GREEK SMALL LETTER OMEGA
(define-keysym #xff7e "Greek_switch")   ;Alias for mode_switch
(define-keysym #x08a1 "leftradical")    ;U+23B7 RADICAL SYMBOL BOTTOM
(define-keysym #x08a2 "topleftradical") ;(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
(define-keysym #x08a3 "horizconnector") ;(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
(define-keysym #x08a4 "topintegral")    ;U+2320 TOP HALF INTEGRAL
(define-keysym #x08a5 "botintegral")    ;U+2321 BOTTOM HALF INTEGRAL
(define-keysym #x08a6 "vertconnector") ;(U+2502 BOX DRAWINGS LIGHT VERTICAL)
(define-keysym #x08a7 "topleftsqbracket") ;U+23A1 LEFT SQUARE BRACKET UPPER CORNER
(define-keysym #x08a8 "botleftsqbracket") ;U+23A3 LEFT SQUARE BRACKET LOWER CORNER
(define-keysym #x08a9 "toprightsqbracket") ;U+23A4 RIGHT SQUARE BRACKET UPPER CORNER
(define-keysym #x08aa "botrightsqbracket") ;U+23A6 RIGHT SQUARE BRACKET LOWER CORNER
(define-keysym #x08ab "topleftparens") ;U+239B LEFT PARENTHESIS UPPER HOOK
(define-keysym #x08ac "botleftparens") ;U+239D LEFT PARENTHESIS LOWER HOOK
(define-keysym #x08ad "toprightparens") ;U+239E RIGHT PARENTHESIS UPPER HOOK
(define-keysym #x08ae "botrightparens") ;U+23A0 RIGHT PARENTHESIS LOWER HOOK
(define-keysym #x08af "leftmiddlecurlybrace") ;U+23A8 LEFT CURLY BRACKET MIDDLE PIECE
(define-keysym #x08b0 "rightmiddlecurlybrace") ;U+23AC RIGHT CURLY BRACKET MIDDLE PIECE
(define-keysym #x08b1 "topleftsummation")
(define-keysym #x08b2 "botleftsummation")
(define-keysym #x08b3 "topvertsummationconnector")
(define-keysym #x08b4 "botvertsummationconnector")
(define-keysym #x08b5 "toprightsummation")
(define-keysym #x08b6 "botrightsummation")
(define-keysym #x08b7 "rightmiddlesummation")
(define-keysym #x08bc "lessthanequal")  ;U+2264 LESS-THAN OR EQUAL TO
(define-keysym #x08bd "notequal")       ;U+2260 NOT EQUAL TO
(define-keysym #x08be "greaterthanequal") ;U+2265 GREATER-THAN OR EQUAL TO
(define-keysym #x08bf "integral")       ;U+222B INTEGRAL
(define-keysym #x08c0 "therefore")      ;U+2234 THEREFORE
(define-keysym #x08c1 "variation")      ;U+221D PROPORTIONAL TO
(define-keysym #x08c2 "infinity")       ;U+221E INFINITY
(define-keysym #x08c5 "nabla")          ;U+2207 NABLA
(define-keysym #x08c8 "approximate")    ;U+223C TILDE OPERATOR
(define-keysym #x08c9 "similarequal")  ;U+2243 ASYMPTOTICALLY EQUAL TO
(define-keysym #x08cd "ifonlyif")      ;U+21D4 LEFT RIGHT DOUBLE ARROW
(define-keysym #x08ce "implies")       ;U+21D2 RIGHTWARDS DOUBLE ARROW
(define-keysym #x08cf "identical")      ;U+2261 IDENTICAL TO
(define-keysym #x08d6 "radical")        ;U+221A SQUARE ROOT
(define-keysym #x08da "includedin")     ;U+2282 SUBSET OF
(define-keysym #x08db "includes")       ;U+2283 SUPERSET OF
(define-keysym #x08dc "intersection")   ;U+2229 INTERSECTION
(define-keysym #x08dd "union")          ;U+222A UNION
(define-keysym #x08de "logicaland")     ;U+2227 LOGICAL AND
(define-keysym #x08df "logicalor")      ;U+2228 LOGICAL OR
(define-keysym #x08ef "partialderivative") ;U+2202 PARTIAL DIFFERENTIAL
(define-keysym #x08f6 "function") ;U+0192 LATIN SMALL LETTER F WITH HOOK
(define-keysym #x08fb "leftarrow")      ;U+2190 LEFTWARDS ARROW
(define-keysym #x08fc "uparrow")        ;U+2191 UPWARDS ARROW
(define-keysym #x08fd "rightarrow")     ;U+2192 RIGHTWARDS ARROW
(define-keysym #x08fe "downarrow")      ;U+2193 DOWNWARDS ARROW
(define-keysym #x09df "blank")
(define-keysym #x09e0 "soliddiamond")   ;U+25C6 BLACK DIAMOND
(define-keysym #x09e1 "checkerboard")   ;U+2592 MEDIUM SHADE
(define-keysym #x09e2 "ht")   ;U+2409 SYMBOL FOR HORIZONTAL TABULATION
(define-keysym #x09e3 "ff")             ;U+240C SYMBOL FOR FORM FEED
(define-keysym #x09e4 "cr")         ;U+240D SYMBOL FOR CARRIAGE RETURN
(define-keysym #x09e5 "lf")             ;U+240A SYMBOL FOR LINE FEED
(define-keysym #x09e8 "nl")             ;U+2424 SYMBOL FOR NEWLINE
(define-keysym #x09e9 "vt")     ;U+240B SYMBOL FOR VERTICAL TABULATION
(define-keysym #x09ea "lowrightcorner") ;U+2518 BOX DRAWINGS LIGHT UP AND LEFT
(define-keysym #x09eb "uprightcorner") ;U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
(define-keysym #x09ec "upleftcorner") ;U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
(define-keysym #x09ed "lowleftcorner") ;U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
(define-keysym #x09ee "crossinglines") ;U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
(define-keysym #x09ef "horizlinescan1") ;U+23BA HORIZONTAL SCAN LINE-1
(define-keysym #x09f0 "horizlinescan3") ;U+23BB HORIZONTAL SCAN LINE-3
(define-keysym #x09f1 "horizlinescan5") ;U+2500 BOX DRAWINGS LIGHT HORIZONTAL
(define-keysym #x09f2 "horizlinescan7") ;U+23BC HORIZONTAL SCAN LINE-7
(define-keysym #x09f3 "horizlinescan9") ;U+23BD HORIZONTAL SCAN LINE-9
(define-keysym #x09f4 "leftt") ;U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
(define-keysym #x09f5 "rightt") ;U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
(define-keysym #x09f6 "bott") ;U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
(define-keysym #x09f7 "topt") ;U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
(define-keysym #x09f8 "vertbar")   ;U+2502 BOX DRAWINGS LIGHT VERTICAL
(define-keysym #x0aa1 "emspace")        ;U+2003 EM SPACE
(define-keysym #x0aa2 "enspace")        ;U+2002 EN SPACE
(define-keysym #x0aa3 "em3space")       ;U+2004 THREE-PER-EM SPACE
(define-keysym #x0aa4 "em4space")       ;U+2005 FOUR-PER-EM SPACE
(define-keysym #x0aa5 "digitspace")     ;U+2007 FIGURE SPACE
(define-keysym #x0aa6 "punctspace")     ;U+2008 PUNCTUATION SPACE
(define-keysym #x0aa7 "thinspace")      ;U+2009 THIN SPACE
(define-keysym #x0aa8 "hairspace")      ;U+200A HAIR SPACE
(define-keysym #x0aa9 "emdash")         ;U+2014 EM DASH
(define-keysym #x0aaa "endash")         ;U+2013 EN DASH
(define-keysym #x0aac "signifblank")    ;(U+2423 OPEN BOX)
(define-keysym #x0aae "ellipsis")       ;U+2026 HORIZONTAL ELLIPSIS
(define-keysym #x0aaf "doubbaselinedot") ;U+2025 TWO DOT LEADER
(define-keysym #x0ab0 "onethird")    ;U+2153 VULGAR FRACTION ONE THIRD
(define-keysym #x0ab1 "twothirds")  ;U+2154 VULGAR FRACTION TWO THIRDS
(define-keysym #x0ab2 "onefifth")    ;U+2155 VULGAR FRACTION ONE FIFTH
(define-keysym #x0ab3 "twofifths")  ;U+2156 VULGAR FRACTION TWO FIFTHS
(define-keysym #x0ab4 "threefifths") ;U+2157 VULGAR FRACTION THREE FIFTHS
(define-keysym #x0ab5 "fourfifths") ;U+2158 VULGAR FRACTION FOUR FIFTHS
(define-keysym #x0ab6 "onesixth")    ;U+2159 VULGAR FRACTION ONE SIXTH
(define-keysym #x0ab7 "fivesixths") ;U+215A VULGAR FRACTION FIVE SIXTHS
(define-keysym #x0ab8 "careof")         ;U+2105 CARE OF
(define-keysym #x0abb "figdash")        ;U+2012 FIGURE DASH
(define-keysym #x0abc "leftanglebracket") ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(define-keysym #x0abd "decimalpoint")   ;(U+002E FULL STOP)
(define-keysym #x0abe "rightanglebracket") ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(define-keysym #x0abf "marker")
(define-keysym #x0ac3 "oneeighth")  ;U+215B VULGAR FRACTION ONE EIGHTH
(define-keysym #x0ac4 "threeeighths") ;U+215C VULGAR FRACTION THREE EIGHTHS
(define-keysym #x0ac5 "fiveeighths") ;U+215D VULGAR FRACTION FIVE EIGHTHS
(define-keysym #x0ac6 "seveneighths") ;U+215E VULGAR FRACTION SEVEN EIGHTHS
(define-keysym #x0ac9 "trademark")      ;U+2122 TRADE MARK SIGN
(define-keysym #x0aca "signaturemark")  ;(U+2613 SALTIRE)
(define-keysym #x0acb "trademarkincircle")
(define-keysym #x0acc "leftopentriangle") ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
(define-keysym #x0acd "rightopentriangle") ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(define-keysym #x0ace "emopencircle")   ;(U+25CB WHITE CIRCLE)
(define-keysym #x0acf "emopenrectangle") ;(U+25AF WHITE VERTICAL RECTANGLE)
(define-keysym #x0ad0 "leftsinglequotemark") ;U+2018 LEFT SINGLE QUOTATION MARK
(define-keysym #x0ad1 "rightsinglequotemark") ;U+2019 RIGHT SINGLE QUOTATION MARK
(define-keysym #x0ad2 "leftdoublequotemark") ;U+201C LEFT DOUBLE QUOTATION MARK
(define-keysym #x0ad3 "rightdoublequotemark") ;U+201D RIGHT DOUBLE QUOTATION MARK
(define-keysym #x0ad4 "prescription")   ;U+211E PRESCRIPTION TAKE
(define-keysym #x0ad6 "minutes")        ;U+2032 PRIME
(define-keysym #x0ad7 "seconds")        ;U+2033 DOUBLE PRIME
(define-keysym #x0ad9 "latincross")     ;U+271D LATIN CROSS
(define-keysym #x0ada "hexagram")
(define-keysym #x0adb "filledrectbullet") ;(U+25AC BLACK RECTANGLE)
(define-keysym #x0adc "filledlefttribullet") ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
(define-keysym #x0add "filledrighttribullet") ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(define-keysym #x0ade "emfilledcircle") ;(U+25CF BLACK CIRCLE)
(define-keysym #x0adf "emfilledrect") ;(U+25AE BLACK VERTICAL RECTANGLE)
(define-keysym #x0ae0 "enopencircbullet") ;(U+25E6 WHITE BULLET)
(define-keysym #x0ae1 "enopensquarebullet") ;(U+25AB WHITE SMALL SQUARE)
(define-keysym #x0ae2 "openrectbullet") ;(U+25AD WHITE RECTANGLE)
(define-keysym #x0ae3 "opentribulletup") ;(U+25B3 WHITE UP-POINTING TRIANGLE)
(define-keysym #x0ae4 "opentribulletdown") ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
(define-keysym #x0ae5 "openstar")       ;(U+2606 WHITE STAR)
(define-keysym #x0ae6 "enfilledcircbullet") ;(U+2022 BULLET)
(define-keysym #x0ae7 "enfilledsqbullet") ;(U+25AA BLACK SMALL SQUARE)
(define-keysym #x0ae8 "filledtribulletup") ;(U+25B2 BLACK UP-POINTING TRIANGLE)
(define-keysym #x0ae9 "filledtribulletdown") ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
(define-keysym #x0aea "leftpointer") ;(U+261C WHITE LEFT POINTING INDEX)
(define-keysym #x0aeb "rightpointer") ;(U+261E WHITE RIGHT POINTING INDEX)
(define-keysym #x0aec "club")           ;U+2663 BLACK CLUB SUIT
(define-keysym #x0aed "diamond")        ;U+2666 BLACK DIAMOND SUIT
(define-keysym #x0aee "heart")          ;U+2665 BLACK HEART SUIT
(define-keysym #x0af0 "maltesecross")   ;U+2720 MALTESE CROSS
(define-keysym #x0af1 "dagger")         ;U+2020 DAGGER
(define-keysym #x0af2 "doubledagger")   ;U+2021 DOUBLE DAGGER
(define-keysym #x0af3 "checkmark")      ;U+2713 CHECK MARK
(define-keysym #x0af4 "ballotcross")    ;U+2717 BALLOT X
(define-keysym #x0af5 "musicalsharp")   ;U+266F MUSIC SHARP SIGN
(define-keysym #x0af6 "musicalflat")    ;U+266D MUSIC FLAT SIGN
(define-keysym #x0af7 "malesymbol")     ;U+2642 MALE SIGN
(define-keysym #x0af8 "femalesymbol")   ;U+2640 FEMALE SIGN
(define-keysym #x0af9 "telephone")      ;U+260E BLACK TELEPHONE
(define-keysym #x0afa "telephonerecorder") ;U+2315 TELEPHONE RECORDER
(define-keysym #x0afb "phonographcopyright") ;U+2117 SOUND RECORDING COPYRIGHT
(define-keysym #x0afc "caret")          ;U+2038 CARET
(define-keysym #x0afd "singlelowquotemark") ;U+201A SINGLE LOW-9 QUOTATION MARK
(define-keysym #x0afe "doublelowquotemark") ;U+201E DOUBLE LOW-9 QUOTATION MARK
(define-keysym #x0aff "cursor")
(define-keysym #x0ba3 "leftcaret")      ;(U+003C LESS-THAN SIGN)
(define-keysym #x0ba6 "rightcaret")     ;(U+003E GREATER-THAN SIGN)
(define-keysym #x0ba8 "downcaret")      ;(U+2228 LOGICAL OR)
(define-keysym #x0ba9 "upcaret")        ;(U+2227 LOGICAL AND)
(define-keysym #x0bc0 "overbar")        ;(U+00AF MACRON)
(define-keysym #x0bc2 "downtack")       ;U+22A5 UP TACK
(define-keysym #x0bc3 "upshoe")         ;(U+2229 INTERSECTION)
(define-keysym #x0bc4 "downstile")      ;U+230A LEFT FLOOR
(define-keysym #x0bc6 "underbar")       ;(U+005F LOW LINE)
(define-keysym #x0bca "jot")            ;U+2218 RING OPERATOR
(define-keysym #x0bcc "quad")       ;U+2395 APL FUNCTIONAL SYMBOL QUAD
(define-keysym #x0bce "uptack")         ;U+22A4 DOWN TACK
(define-keysym #x0bcf "circle")         ;U+25CB WHITE CIRCLE
(define-keysym #x0bd3 "upstile")        ;U+2308 LEFT CEILING
(define-keysym #x0bd6 "downshoe")       ;(U+222A UNION)
(define-keysym #x0bd8 "rightshoe")      ;(U+2283 SUPERSET OF)
(define-keysym #x0bda "leftshoe")       ;(U+2282 SUBSET OF)
(define-keysym #x0bdc "lefttack")       ;U+22A2 RIGHT TACK
(define-keysym #x0bfc "righttack")      ;U+22A3 LEFT TACK
(define-keysym #x0cdf "hebrew_doublelowline") ;U+2017 DOUBLE LOW LINE
(define-keysym #x0ce0 "hebrew_aleph")   ;U+05D0 HEBREW LETTER ALEF
(define-keysym #x0ce1 "hebrew_bet")     ;U+05D1 HEBREW LETTER BET
(define-keysym #x0ce1 "hebrew_beth")    ;deprecated
(define-keysym #x0ce2 "hebrew_gimel")   ;U+05D2 HEBREW LETTER GIMEL
(define-keysym #x0ce2 "hebrew_gimmel")  ;deprecated
(define-keysym #x0ce3 "hebrew_dalet")   ;U+05D3 HEBREW LETTER DALET
(define-keysym #x0ce3 "hebrew_daleth")  ;deprecated
(define-keysym #x0ce4 "hebrew_he")      ;U+05D4 HEBREW LETTER HE
(define-keysym #x0ce5 "hebrew_waw")     ;U+05D5 HEBREW LETTER VAV
(define-keysym #x0ce6 "hebrew_zain")    ;U+05D6 HEBREW LETTER ZAYIN
(define-keysym #x0ce6 "hebrew_zayin")   ;deprecated
(define-keysym #x0ce7 "hebrew_chet")    ;U+05D7 HEBREW LETTER HET
(define-keysym #x0ce7 "hebrew_het")     ;deprecated
(define-keysym #x0ce8 "hebrew_tet")     ;U+05D8 HEBREW LETTER TET
(define-keysym #x0ce8 "hebrew_teth")    ;deprecated
(define-keysym #x0ce9 "hebrew_yod")     ;U+05D9 HEBREW LETTER YOD
(define-keysym #x0cea "hebrew_finalkaph") ;U+05DA HEBREW LETTER FINAL KAF
(define-keysym #x0ceb "hebrew_kaph")    ;U+05DB HEBREW LETTER KAF
(define-keysym #x0cec "hebrew_lamed")   ;U+05DC HEBREW LETTER LAMED
(define-keysym #x0ced "hebrew_finalmem") ;U+05DD HEBREW LETTER FINAL MEM
(define-keysym #x0cee "hebrew_mem")     ;U+05DE HEBREW LETTER MEM
(define-keysym #x0cef "hebrew_finalnun") ;U+05DF HEBREW LETTER FINAL NUN
(define-keysym #x0cf0 "hebrew_nun")     ;U+05E0 HEBREW LETTER NUN
(define-keysym #x0cf1 "hebrew_samech")  ;U+05E1 HEBREW LETTER SAMEKH
(define-keysym #x0cf1 "hebrew_samekh")  ;deprecated
(define-keysym #x0cf2 "hebrew_ayin")    ;U+05E2 HEBREW LETTER AYIN
(define-keysym #x0cf3 "hebrew_finalpe") ;U+05E3 HEBREW LETTER FINAL PE
(define-keysym #x0cf4 "hebrew_pe")      ;U+05E4 HEBREW LETTER PE
(define-keysym #x0cf5 "hebrew_finalzade") ;U+05E5 HEBREW LETTER FINAL TSADI
(define-keysym #x0cf5 "hebrew_finalzadi") ;deprecated
(define-keysym #x0cf6 "hebrew_zade")    ;U+05E6 HEBREW LETTER TSADI
(define-keysym #x0cf6 "hebrew_zadi")    ;deprecated
(define-keysym #x0cf7 "hebrew_qoph")    ;U+05E7 HEBREW LETTER QOF
(define-keysym #x0cf7 "hebrew_kuf")     ;deprecated
(define-keysym #x0cf8 "hebrew_resh")    ;U+05E8 HEBREW LETTER RESH
(define-keysym #x0cf9 "hebrew_shin")    ;U+05E9 HEBREW LETTER SHIN
(define-keysym #x0cfa "hebrew_taw")     ;U+05EA HEBREW LETTER TAV
(define-keysym #x0cfa "hebrew_taf")     ;deprecated
(define-keysym #xff7e "Hebrew_switch")  ;Alias for mode_switch
(define-keysym #x0da1 "Thai_kokai")     ;U+0E01 THAI CHARACTER KO KAI
(define-keysym #x0da2 "Thai_khokhai")  ;U+0E02 THAI CHARACTER KHO KHAI
(define-keysym #x0da3 "Thai_khokhuat") ;U+0E03 THAI CHARACTER KHO KHUAT
(define-keysym #x0da4 "Thai_khokhwai") ;U+0E04 THAI CHARACTER KHO KHWAI
(define-keysym #x0da5 "Thai_khokhon")  ;U+0E05 THAI CHARACTER KHO KHON
(define-keysym #x0da6 "Thai_khorakhang") ;U+0E06 THAI CHARACTER KHO RAKHANG
(define-keysym #x0da7 "Thai_ngongu")    ;U+0E07 THAI CHARACTER NGO NGU
(define-keysym #x0da8 "Thai_chochan")  ;U+0E08 THAI CHARACTER CHO CHAN
(define-keysym #x0da9 "Thai_choching") ;U+0E09 THAI CHARACTER CHO CHING
(define-keysym #x0daa "Thai_chochang") ;U+0E0A THAI CHARACTER CHO CHANG
(define-keysym #x0dab "Thai_soso")      ;U+0E0B THAI CHARACTER SO SO
(define-keysym #x0dac "Thai_chochoe")  ;U+0E0C THAI CHARACTER CHO CHOE
(define-keysym #x0dad "Thai_yoying")    ;U+0E0D THAI CHARACTER YO YING
(define-keysym #x0dae "Thai_dochada")  ;U+0E0E THAI CHARACTER DO CHADA
(define-keysym #x0daf "Thai_topatak")  ;U+0E0F THAI CHARACTER TO PATAK
(define-keysym #x0db0 "Thai_thothan")  ;U+0E10 THAI CHARACTER THO THAN
(define-keysym #x0db1 "Thai_thonangmontho") ;U+0E11 THAI CHARACTER THO NANGMONTHO
(define-keysym #x0db2 "Thai_thophuthao") ;U+0E12 THAI CHARACTER THO PHUTHAO
(define-keysym #x0db3 "Thai_nonen")     ;U+0E13 THAI CHARACTER NO NEN
(define-keysym #x0db4 "Thai_dodek")     ;U+0E14 THAI CHARACTER DO DEK
(define-keysym #x0db5 "Thai_totao")     ;U+0E15 THAI CHARACTER TO TAO
(define-keysym #x0db6 "Thai_thothung") ;U+0E16 THAI CHARACTER THO THUNG
(define-keysym #x0db7 "Thai_thothahan") ;U+0E17 THAI CHARACTER THO THAHAN
(define-keysym #x0db8 "Thai_thothong") ;U+0E18 THAI CHARACTER THO THONG
(define-keysym #x0db9 "Thai_nonu")      ;U+0E19 THAI CHARACTER NO NU
(define-keysym #x0dba "Thai_bobaimai") ;U+0E1A THAI CHARACTER BO BAIMAI
(define-keysym #x0dbb "Thai_popla")     ;U+0E1B THAI CHARACTER PO PLA
(define-keysym #x0dbc "Thai_phophung") ;U+0E1C THAI CHARACTER PHO PHUNG
(define-keysym #x0dbd "Thai_fofa")      ;U+0E1D THAI CHARACTER FO FA
(define-keysym #x0dbe "Thai_phophan")  ;U+0E1E THAI CHARACTER PHO PHAN
(define-keysym #x0dbf "Thai_fofan")     ;U+0E1F THAI CHARACTER FO FAN
(define-keysym #x0dc0 "Thai_phosamphao") ;U+0E20 THAI CHARACTER PHO SAMPHAO
(define-keysym #x0dc1 "Thai_moma")      ;U+0E21 THAI CHARACTER MO MA
(define-keysym #x0dc2 "Thai_yoyak")     ;U+0E22 THAI CHARACTER YO YAK
(define-keysym #x0dc3 "Thai_rorua")     ;U+0E23 THAI CHARACTER RO RUA
(define-keysym #x0dc4 "Thai_ru")        ;U+0E24 THAI CHARACTER RU
(define-keysym #x0dc5 "Thai_loling")    ;U+0E25 THAI CHARACTER LO LING
(define-keysym #x0dc6 "Thai_lu")        ;U+0E26 THAI CHARACTER LU
(define-keysym #x0dc7 "Thai_wowaen")    ;U+0E27 THAI CHARACTER WO WAEN
(define-keysym #x0dc8 "Thai_sosala")    ;U+0E28 THAI CHARACTER SO SALA
(define-keysym #x0dc9 "Thai_sorusi")    ;U+0E29 THAI CHARACTER SO RUSI
(define-keysym #x0dca "Thai_sosua")     ;U+0E2A THAI CHARACTER SO SUA
(define-keysym #x0dcb "Thai_hohip")     ;U+0E2B THAI CHARACTER HO HIP
(define-keysym #x0dcc "Thai_lochula")  ;U+0E2C THAI CHARACTER LO CHULA
(define-keysym #x0dcd "Thai_oang")      ;U+0E2D THAI CHARACTER O ANG
(define-keysym #x0dce "Thai_honokhuk") ;U+0E2E THAI CHARACTER HO NOKHUK
(define-keysym #x0dcf "Thai_paiyannoi") ;U+0E2F THAI CHARACTER PAIYANNOI
(define-keysym #x0dd0 "Thai_saraa")     ;U+0E30 THAI CHARACTER SARA A
(define-keysym #x0dd1 "Thai_maihanakat") ;U+0E31 THAI CHARACTER MAI HAN-AKAT
(define-keysym #x0dd2 "Thai_saraaa")    ;U+0E32 THAI CHARACTER SARA AA
(define-keysym #x0dd3 "Thai_saraam")    ;U+0E33 THAI CHARACTER SARA AM
(define-keysym #x0dd4 "Thai_sarai")     ;U+0E34 THAI CHARACTER SARA I
(define-keysym #x0dd5 "Thai_saraii")    ;U+0E35 THAI CHARACTER SARA II
(define-keysym #x0dd6 "Thai_saraue")    ;U+0E36 THAI CHARACTER SARA UE
(define-keysym #x0dd7 "Thai_sarauee")  ;U+0E37 THAI CHARACTER SARA UEE
(define-keysym #x0dd8 "Thai_sarau")     ;U+0E38 THAI CHARACTER SARA U
(define-keysym #x0dd9 "Thai_sarauu")    ;U+0E39 THAI CHARACTER SARA UU
(define-keysym #x0dda "Thai_phinthu")   ;U+0E3A THAI CHARACTER PHINTHU
(define-keysym #x0dde "Thai_maihanakat_maitho")
(define-keysym #x0ddf "Thai_baht")   ;U+0E3F THAI CURRENCY SYMBOL BAHT
(define-keysym #x0de0 "Thai_sarae")     ;U+0E40 THAI CHARACTER SARA E
(define-keysym #x0de1 "Thai_saraae")    ;U+0E41 THAI CHARACTER SARA AE
(define-keysym #x0de2 "Thai_sarao")     ;U+0E42 THAI CHARACTER SARA O
(define-keysym #x0de3 "Thai_saraaimaimuan") ;U+0E43 THAI CHARACTER SARA AI MAIMUAN
(define-keysym #x0de4 "Thai_saraaimaimalai") ;U+0E44 THAI CHARACTER SARA AI MAIMALAI
(define-keysym #x0de5 "Thai_lakkhangyao") ;U+0E45 THAI CHARACTER LAKKHANGYAO
(define-keysym #x0de6 "Thai_maiyamok") ;U+0E46 THAI CHARACTER MAIYAMOK
(define-keysym #x0de7 "Thai_maitaikhu") ;U+0E47 THAI CHARACTER MAITAIKHU
(define-keysym #x0de8 "Thai_maiek")     ;U+0E48 THAI CHARACTER MAI EK
(define-keysym #x0de9 "Thai_maitho")    ;U+0E49 THAI CHARACTER MAI THO
(define-keysym #x0dea "Thai_maitri")    ;U+0E4A THAI CHARACTER MAI TRI
(define-keysym #x0deb "Thai_maichattawa") ;U+0E4B THAI CHARACTER MAI CHATTAWA
(define-keysym #x0dec "Thai_thanthakhat") ;U+0E4C THAI CHARACTER THANTHAKHAT
(define-keysym #x0ded "Thai_nikhahit") ;U+0E4D THAI CHARACTER NIKHAHIT
(define-keysym #x0df0 "Thai_leksun")    ;U+0E50 THAI DIGIT ZERO
(define-keysym #x0df1 "Thai_leknung")   ;U+0E51 THAI DIGIT ONE
(define-keysym #x0df2 "Thai_leksong")   ;U+0E52 THAI DIGIT TWO
(define-keysym #x0df3 "Thai_leksam")    ;U+0E53 THAI DIGIT THREE
(define-keysym #x0df4 "Thai_leksi")     ;U+0E54 THAI DIGIT FOUR
(define-keysym #x0df5 "Thai_lekha")     ;U+0E55 THAI DIGIT FIVE
(define-keysym #x0df6 "Thai_lekhok")    ;U+0E56 THAI DIGIT SIX
(define-keysym #x0df7 "Thai_lekchet")   ;U+0E57 THAI DIGIT SEVEN
(define-keysym #x0df8 "Thai_lekpaet")   ;U+0E58 THAI DIGIT EIGHT
(define-keysym #x0df9 "Thai_lekkao")    ;U+0E59 THAI DIGIT NINE
(define-keysym #xff31 "Hangul")         ;Hangul start/stop(toggle)
(define-keysym #xff32 "Hangul_Start")   ;Hangul start
(define-keysym #xff33 "Hangul_End")     ;Hangul end, English start
(define-keysym #xff34 "Hangul_Hanja")  ;Start Hangul->Hanja Conversion
(define-keysym #xff35 "Hangul_Jamo")    ;Hangul Jamo mode
(define-keysym #xff36 "Hangul_Romaja")  ;Hangul Romaja mode
(define-keysym #xff37 "Hangul_Codeinput") ;Hangul code input mode
(define-keysym #xff38 "Hangul_Jeonja")  ;Jeonja mode
(define-keysym #xff39 "Hangul_Banja")   ;Banja mode
(define-keysym #xff3a "Hangul_PreHanja") ;Pre Hanja conversion
(define-keysym #xff3b "Hangul_PostHanja") ;Post Hanja conversion
(define-keysym #xff3c "Hangul_SingleCandidate") ;Single candidate
(define-keysym #xff3d "Hangul_MultipleCandidate") ;Multiple candidate
(define-keysym #xff3e "Hangul_PreviousCandidate") ;Previous candidate
(define-keysym #xff3f "Hangul_Special") ;Special symbols
(define-keysym #xff7e "Hangul_switch")  ;Alias for mode_switch
(define-keysym #x0ea1 "Hangul_Kiyeog")
(define-keysym #x0ea2 "Hangul_SsangKiyeog")
(define-keysym #x0ea3 "Hangul_KiyeogSios")
(define-keysym #x0ea4 "Hangul_Nieun")
(define-keysym #x0ea5 "Hangul_NieunJieuj")
(define-keysym #x0ea6 "Hangul_NieunHieuh")
(define-keysym #x0ea7 "Hangul_Dikeud")
(define-keysym #x0ea8 "Hangul_SsangDikeud")
(define-keysym #x0ea9 "Hangul_Rieul")
(define-keysym #x0eaa "Hangul_RieulKiyeog")
(define-keysym #x0eab "Hangul_RieulMieum")
(define-keysym #x0eac "Hangul_RieulPieub")
(define-keysym #x0ead "Hangul_RieulSios")
(define-keysym #x0eae "Hangul_RieulTieut")
(define-keysym #x0eaf "Hangul_RieulPhieuf")
(define-keysym #x0eb0 "Hangul_RieulHieuh")
(define-keysym #x0eb1 "Hangul_Mieum")
(define-keysym #x0eb2 "Hangul_Pieub")
(define-keysym #x0eb3 "Hangul_SsangPieub")
(define-keysym #x0eb4 "Hangul_PieubSios")
(define-keysym #x0eb5 "Hangul_Sios")
(define-keysym #x0eb6 "Hangul_SsangSios")
(define-keysym #x0eb7 "Hangul_Ieung")
(define-keysym #x0eb8 "Hangul_Jieuj")
(define-keysym #x0eb9 "Hangul_SsangJieuj")
(define-keysym #x0eba "Hangul_Cieuc")
(define-keysym #x0ebb "Hangul_Khieuq")
(define-keysym #x0ebc "Hangul_Tieut")
(define-keysym #x0ebd "Hangul_Phieuf")
(define-keysym #x0ebe "Hangul_Hieuh")
(define-keysym #x0ebf "Hangul_A")
(define-keysym #x0ec0 "Hangul_AE")
(define-keysym #x0ec1 "Hangul_YA")
(define-keysym #x0ec2 "Hangul_YAE")
(define-keysym #x0ec3 "Hangul_EO")
(define-keysym #x0ec4 "Hangul_E")
(define-keysym #x0ec5 "Hangul_YEO")
(define-keysym #x0ec6 "Hangul_YE")
(define-keysym #x0ec7 "Hangul_O")
(define-keysym #x0ec8 "Hangul_WA")
(define-keysym #x0ec9 "Hangul_WAE")
(define-keysym #x0eca "Hangul_OE")
(define-keysym #x0ecb "Hangul_YO")
(define-keysym #x0ecc "Hangul_U")
(define-keysym #x0ecd "Hangul_WEO")
(define-keysym #x0ece "Hangul_WE")
(define-keysym #x0ecf "Hangul_WI")
(define-keysym #x0ed0 "Hangul_YU")
(define-keysym #x0ed1 "Hangul_EU")
(define-keysym #x0ed2 "Hangul_YI")
(define-keysym #x0ed3 "Hangul_I")
(define-keysym #x0ed4 "Hangul_J_Kiyeog")
(define-keysym #x0ed5 "Hangul_J_SsangKiyeog")
(define-keysym #x0ed6 "Hangul_J_KiyeogSios")
(define-keysym #x0ed7 "Hangul_J_Nieun")
(define-keysym #x0ed8 "Hangul_J_NieunJieuj")
(define-keysym #x0ed9 "Hangul_J_NieunHieuh")
(define-keysym #x0eda "Hangul_J_Dikeud")
(define-keysym #x0edb "Hangul_J_Rieul")
(define-keysym #x0edc "Hangul_J_RieulKiyeog")
(define-keysym #x0edd "Hangul_J_RieulMieum")
(define-keysym #x0ede "Hangul_J_RieulPieub")
(define-keysym #x0edf "Hangul_J_RieulSios")
(define-keysym #x0ee0 "Hangul_J_RieulTieut")
(define-keysym #x0ee1 "Hangul_J_RieulPhieuf")
(define-keysym #x0ee2 "Hangul_J_RieulHieuh")
(define-keysym #x0ee3 "Hangul_J_Mieum")
(define-keysym #x0ee4 "Hangul_J_Pieub")
(define-keysym #x0ee5 "Hangul_J_PieubSios")
(define-keysym #x0ee6 "Hangul_J_Sios")
(define-keysym #x0ee7 "Hangul_J_SsangSios")
(define-keysym #x0ee8 "Hangul_J_Ieung")
(define-keysym #x0ee9 "Hangul_J_Jieuj")
(define-keysym #x0eea "Hangul_J_Cieuc")
(define-keysym #x0eeb "Hangul_J_Khieuq")
(define-keysym #x0eec "Hangul_J_Tieut")
(define-keysym #x0eed "Hangul_J_Phieuf")
(define-keysym #x0eee "Hangul_J_Hieuh")
(define-keysym #x0eef "Hangul_RieulYeorinHieuh")
(define-keysym #x0ef0 "Hangul_SunkyeongeumMieum")
(define-keysym #x0ef1 "Hangul_SunkyeongeumPieub")
(define-keysym #x0ef2 "Hangul_PanSios")
(define-keysym #x0ef3 "Hangul_KkogjiDalrinIeung")
(define-keysym #x0ef4 "Hangul_SunkyeongeumPhieuf")
(define-keysym #x0ef5 "Hangul_YeorinHieuh")
(define-keysym #x0ef6 "Hangul_AraeA")
(define-keysym #x0ef7 "Hangul_AraeAE")
(define-keysym #x0ef8 "Hangul_J_PanSios")
(define-keysym #x0ef9 "Hangul_J_KkogjiDalrinIeung")
(define-keysym #x0efa "Hangul_J_YeorinHieuh")
(define-keysym #x0eff "Korean_Won")     ;(U+20A9 WON SIGN)
(define-keysym #x1000587 "Armenian_ligature_ew") ;U+0587 ARMENIAN SMALL LIGATURE ECH YIWN
(define-keysym #x1000589 "Armenian_full_stop") ;U+0589 ARMENIAN FULL STOP
(define-keysym #x1000589 "Armenian_verjaket") ;U+0589 ARMENIAN FULL STOP
(define-keysym #x100055d "Armenian_separation_mark") ;U+055D ARMENIAN COMMA
(define-keysym #x100055d "Armenian_but") ;U+055D ARMENIAN COMMA
(define-keysym #x100058a "Armenian_hyphen") ;U+058A ARMENIAN HYPHEN
(define-keysym #x100058a "Armenian_yentamna") ;U+058A ARMENIAN HYPHEN
(define-keysym #x100055c "Armenian_exclam") ;U+055C ARMENIAN EXCLAMATION MARK
(define-keysym #x100055c "Armenian_amanak") ;U+055C ARMENIAN EXCLAMATION MARK
(define-keysym #x100055b "Armenian_accent") ;U+055B ARMENIAN EMPHASIS MARK
(define-keysym #x100055b "Armenian_shesht") ;U+055B ARMENIAN EMPHASIS MARK
(define-keysym #x100055e "Armenian_question") ;U+055E ARMENIAN QUESTION MARK
(define-keysym #x100055e "Armenian_paruyk") ;U+055E ARMENIAN QUESTION MARK
(define-keysym #x1000531 "Armenian_AYB") ;U+0531 ARMENIAN CAPITAL LETTER AYB
(define-keysym #x1000561 "Armenian_ayb") ;U+0561 ARMENIAN SMALL LETTER AYB
(define-keysym #x1000532 "Armenian_BEN") ;U+0532 ARMENIAN CAPITAL LETTER BEN
(define-keysym #x1000562 "Armenian_ben") ;U+0562 ARMENIAN SMALL LETTER BEN
(define-keysym #x1000533 "Armenian_GIM") ;U+0533 ARMENIAN CAPITAL LETTER GIM
(define-keysym #x1000563 "Armenian_gim") ;U+0563 ARMENIAN SMALL LETTER GIM
(define-keysym #x1000534 "Armenian_DA") ;U+0534 ARMENIAN CAPITAL LETTER DA
(define-keysym #x1000564 "Armenian_da") ;U+0564 ARMENIAN SMALL LETTER DA
(define-keysym #x1000535 "Armenian_YECH") ;U+0535 ARMENIAN CAPITAL LETTER ECH
(define-keysym #x1000565 "Armenian_yech") ;U+0565 ARMENIAN SMALL LETTER ECH
(define-keysym #x1000536 "Armenian_ZA") ;U+0536 ARMENIAN CAPITAL LETTER ZA
(define-keysym #x1000566 "Armenian_za") ;U+0566 ARMENIAN SMALL LETTER ZA
(define-keysym #x1000537 "Armenian_E") ;U+0537 ARMENIAN CAPITAL LETTER EH
(define-keysym #x1000567 "Armenian_e") ;U+0567 ARMENIAN SMALL LETTER EH
(define-keysym #x1000538 "Armenian_AT") ;U+0538 ARMENIAN CAPITAL LETTER ET
(define-keysym #x1000568 "Armenian_at") ;U+0568 ARMENIAN SMALL LETTER ET
(define-keysym #x1000539 "Armenian_TO") ;U+0539 ARMENIAN CAPITAL LETTER TO
(define-keysym #x1000569 "Armenian_to") ;U+0569 ARMENIAN SMALL LETTER TO
(define-keysym #x100053a "Armenian_ZHE") ;U+053A ARMENIAN CAPITAL LETTER ZHE
(define-keysym #x100056a "Armenian_zhe") ;U+056A ARMENIAN SMALL LETTER ZHE
(define-keysym #x100053b "Armenian_INI") ;U+053B ARMENIAN CAPITAL LETTER INI
(define-keysym #x100056b "Armenian_ini") ;U+056B ARMENIAN SMALL LETTER INI
(define-keysym #x100053c "Armenian_LYUN") ;U+053C ARMENIAN CAPITAL LETTER LIWN
(define-keysym #x100056c "Armenian_lyun") ;U+056C ARMENIAN SMALL LETTER LIWN
(define-keysym #x100053d "Armenian_KHE") ;U+053D ARMENIAN CAPITAL LETTER XEH
(define-keysym #x100056d "Armenian_khe") ;U+056D ARMENIAN SMALL LETTER XEH
(define-keysym #x100053e "Armenian_TSA") ;U+053E ARMENIAN CAPITAL LETTER CA
(define-keysym #x100056e "Armenian_tsa") ;U+056E ARMENIAN SMALL LETTER CA
(define-keysym #x100053f "Armenian_KEN") ;U+053F ARMENIAN CAPITAL LETTER KEN
(define-keysym #x100056f "Armenian_ken") ;U+056F ARMENIAN SMALL LETTER KEN
(define-keysym #x1000540 "Armenian_HO") ;U+0540 ARMENIAN CAPITAL LETTER HO
(define-keysym #x1000570 "Armenian_ho") ;U+0570 ARMENIAN SMALL LETTER HO
(define-keysym #x1000541 "Armenian_DZA") ;U+0541 ARMENIAN CAPITAL LETTER JA
(define-keysym #x1000571 "Armenian_dza") ;U+0571 ARMENIAN SMALL LETTER JA
(define-keysym #x1000542 "Armenian_GHAT") ;U+0542 ARMENIAN CAPITAL LETTER GHAD
(define-keysym #x1000572 "Armenian_ghat") ;U+0572 ARMENIAN SMALL LETTER GHAD
(define-keysym #x1000543 "Armenian_TCHE") ;U+0543 ARMENIAN CAPITAL LETTER CHEH
(define-keysym #x1000573 "Armenian_tche") ;U+0573 ARMENIAN SMALL LETTER CHEH
(define-keysym #x1000544 "Armenian_MEN") ;U+0544 ARMENIAN CAPITAL LETTER MEN
(define-keysym #x1000574 "Armenian_men") ;U+0574 ARMENIAN SMALL LETTER MEN
(define-keysym #x1000545 "Armenian_HI") ;U+0545 ARMENIAN CAPITAL LETTER YI
(define-keysym #x1000575 "Armenian_hi") ;U+0575 ARMENIAN SMALL LETTER YI
(define-keysym #x1000546 "Armenian_NU") ;U+0546 ARMENIAN CAPITAL LETTER NOW
(define-keysym #x1000576 "Armenian_nu") ;U+0576 ARMENIAN SMALL LETTER NOW
(define-keysym #x1000547 "Armenian_SHA") ;U+0547 ARMENIAN CAPITAL LETTER SHA
(define-keysym #x1000577 "Armenian_sha") ;U+0577 ARMENIAN SMALL LETTER SHA
(define-keysym #x1000548 "Armenian_VO") ;U+0548 ARMENIAN CAPITAL LETTER VO
(define-keysym #x1000578 "Armenian_vo") ;U+0578 ARMENIAN SMALL LETTER VO
(define-keysym #x1000549 "Armenian_CHA") ;U+0549 ARMENIAN CAPITAL LETTER CHA
(define-keysym #x1000579 "Armenian_cha") ;U+0579 ARMENIAN SMALL LETTER CHA
(define-keysym #x100054a "Armenian_PE") ;U+054A ARMENIAN CAPITAL LETTER PEH
(define-keysym #x100057a "Armenian_pe") ;U+057A ARMENIAN SMALL LETTER PEH
(define-keysym #x100054b "Armenian_JE") ;U+054B ARMENIAN CAPITAL LETTER JHEH
(define-keysym #x100057b "Armenian_je") ;U+057B ARMENIAN SMALL LETTER JHEH
(define-keysym #x100054c "Armenian_RA") ;U+054C ARMENIAN CAPITAL LETTER RA
(define-keysym #x100057c "Armenian_ra") ;U+057C ARMENIAN SMALL LETTER RA
(define-keysym #x100054d "Armenian_SE") ;U+054D ARMENIAN CAPITAL LETTER SEH
(define-keysym #x100057d "Armenian_se") ;U+057D ARMENIAN SMALL LETTER SEH
(define-keysym #x100054e "Armenian_VEV") ;U+054E ARMENIAN CAPITAL LETTER VEW
(define-keysym #x100057e "Armenian_vev") ;U+057E ARMENIAN SMALL LETTER VEW
(define-keysym #x100054f "Armenian_TYUN") ;U+054F ARMENIAN CAPITAL LETTER TIWN
(define-keysym #x100057f "Armenian_tyun") ;U+057F ARMENIAN SMALL LETTER TIWN
(define-keysym #x1000550 "Armenian_RE") ;U+0550 ARMENIAN CAPITAL LETTER REH
(define-keysym #x1000580 "Armenian_re") ;U+0580 ARMENIAN SMALL LETTER REH
(define-keysym #x1000551 "Armenian_TSO") ;U+0551 ARMENIAN CAPITAL LETTER CO
(define-keysym #x1000581 "Armenian_tso") ;U+0581 ARMENIAN SMALL LETTER CO
(define-keysym #x1000552 "Armenian_VYUN") ;U+0552 ARMENIAN CAPITAL LETTER YIWN
(define-keysym #x1000582 "Armenian_vyun") ;U+0582 ARMENIAN SMALL LETTER YIWN
(define-keysym #x1000553 "Armenian_PYUR") ;U+0553 ARMENIAN CAPITAL LETTER PIWR
(define-keysym #x1000583 "Armenian_pyur") ;U+0583 ARMENIAN SMALL LETTER PIWR
(define-keysym #x1000554 "Armenian_KE") ;U+0554 ARMENIAN CAPITAL LETTER KEH
(define-keysym #x1000584 "Armenian_ke") ;U+0584 ARMENIAN SMALL LETTER KEH
(define-keysym #x1000555 "Armenian_O") ;U+0555 ARMENIAN CAPITAL LETTER OH
(define-keysym #x1000585 "Armenian_o") ;U+0585 ARMENIAN SMALL LETTER OH
(define-keysym #x1000556 "Armenian_FE") ;U+0556 ARMENIAN CAPITAL LETTER FEH
(define-keysym #x1000586 "Armenian_fe") ;U+0586 ARMENIAN SMALL LETTER FEH
(define-keysym #x100055a "Armenian_apostrophe") ;U+055A ARMENIAN APOSTROPHE
(define-keysym #x10010d0 "Georgian_an") ;U+10D0 GEORGIAN LETTER AN
(define-keysym #x10010d1 "Georgian_ban") ;U+10D1 GEORGIAN LETTER BAN
(define-keysym #x10010d2 "Georgian_gan") ;U+10D2 GEORGIAN LETTER GAN
(define-keysym #x10010d3 "Georgian_don") ;U+10D3 GEORGIAN LETTER DON
(define-keysym #x10010d4 "Georgian_en") ;U+10D4 GEORGIAN LETTER EN
(define-keysym #x10010d5 "Georgian_vin") ;U+10D5 GEORGIAN LETTER VIN
(define-keysym #x10010d6 "Georgian_zen") ;U+10D6 GEORGIAN LETTER ZEN
(define-keysym #x10010d7 "Georgian_tan") ;U+10D7 GEORGIAN LETTER TAN
(define-keysym #x10010d8 "Georgian_in") ;U+10D8 GEORGIAN LETTER IN
(define-keysym #x10010d9 "Georgian_kan") ;U+10D9 GEORGIAN LETTER KAN
(define-keysym #x10010da "Georgian_las") ;U+10DA GEORGIAN LETTER LAS
(define-keysym #x10010db "Georgian_man") ;U+10DB GEORGIAN LETTER MAN
(define-keysym #x10010dc "Georgian_nar") ;U+10DC GEORGIAN LETTER NAR
(define-keysym #x10010dd "Georgian_on") ;U+10DD GEORGIAN LETTER ON
(define-keysym #x10010de "Georgian_par") ;U+10DE GEORGIAN LETTER PAR
(define-keysym #x10010df "Georgian_zhar") ;U+10DF GEORGIAN LETTER ZHAR
(define-keysym #x10010e0 "Georgian_rae") ;U+10E0 GEORGIAN LETTER RAE
(define-keysym #x10010e1 "Georgian_san") ;U+10E1 GEORGIAN LETTER SAN
(define-keysym #x10010e2 "Georgian_tar") ;U+10E2 GEORGIAN LETTER TAR
(define-keysym #x10010e3 "Georgian_un") ;U+10E3 GEORGIAN LETTER UN
(define-keysym #x10010e4 "Georgian_phar") ;U+10E4 GEORGIAN LETTER PHAR
(define-keysym #x10010e5 "Georgian_khar") ;U+10E5 GEORGIAN LETTER KHAR
(define-keysym #x10010e6 "Georgian_ghan") ;U+10E6 GEORGIAN LETTER GHAN
(define-keysym #x10010e7 "Georgian_qar") ;U+10E7 GEORGIAN LETTER QAR
(define-keysym #x10010e8 "Georgian_shin") ;U+10E8 GEORGIAN LETTER SHIN
(define-keysym #x10010e9 "Georgian_chin") ;U+10E9 GEORGIAN LETTER CHIN
(define-keysym #x10010ea "Georgian_can") ;U+10EA GEORGIAN LETTER CAN
(define-keysym #x10010eb "Georgian_jil") ;U+10EB GEORGIAN LETTER JIL
(define-keysym #x10010ec "Georgian_cil") ;U+10EC GEORGIAN LETTER CIL
(define-keysym #x10010ed "Georgian_char") ;U+10ED GEORGIAN LETTER CHAR
(define-keysym #x10010ee "Georgian_xan") ;U+10EE GEORGIAN LETTER XAN
(define-keysym #x10010ef "Georgian_jhan") ;U+10EF GEORGIAN LETTER JHAN
(define-keysym #x10010f0 "Georgian_hae") ;U+10F0 GEORGIAN LETTER HAE
(define-keysym #x10010f1 "Georgian_he") ;U+10F1 GEORGIAN LETTER HE
(define-keysym #x10010f2 "Georgian_hie") ;U+10F2 GEORGIAN LETTER HIE
(define-keysym #x10010f3 "Georgian_we") ;U+10F3 GEORGIAN LETTER WE
(define-keysym #x10010f4 "Georgian_har") ;U+10F4 GEORGIAN LETTER HAR
(define-keysym #x10010f5 "Georgian_hoe") ;U+10F5 GEORGIAN LETTER HOE
(define-keysym #x10010f6 "Georgian_fi") ;U+10F6 GEORGIAN LETTER FI
(define-keysym #x1001e8a "Xabovedot") ;U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE
(define-keysym #x100012c "Ibreve") ;U+012C LATIN CAPITAL LETTER I WITH BREVE
(define-keysym #x10001b5 "Zstroke") ;U+01B5 LATIN CAPITAL LETTER Z WITH STROKE
(define-keysym #x10001e6 "Gcaron") ;U+01E6 LATIN CAPITAL LETTER G WITH CARON
(define-keysym #x10001d1 "Ocaron") ;U+01D2 LATIN CAPITAL LETTER O WITH CARON
(define-keysym #x100019f "Obarred") ;U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE
(define-keysym #x1001e8b "xabovedot") ;U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE
(define-keysym #x100012d "ibreve") ;U+012D LATIN SMALL LETTER I WITH BREVE
(define-keysym #x10001b6 "zstroke") ;U+01B6 LATIN SMALL LETTER Z WITH STROKE
(define-keysym #x10001e7 "gcaron") ;U+01E7 LATIN SMALL LETTER G WITH CARON
(define-keysym #x10001d2 "ocaron") ;U+01D2 LATIN SMALL LETTER O WITH CARON
(define-keysym #x1000275 "obarred") ;U+0275 LATIN SMALL LETTER BARRED O
(define-keysym #x100018f "SCHWA")   ;U+018F LATIN CAPITAL LETTER SCHWA
(define-keysym #x1000259 "schwa")     ;U+0259 LATIN SMALL LETTER SCHWA
(define-keysym #x1001e36 "Lbelowdot") ;U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW
(define-keysym #x1001e37 "lbelowdot") ;U+1E37 LATIN SMALL LETTER L WITH DOT BELOW
(define-keysym #x1001ea0 "Abelowdot") ;U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW
(define-keysym #x1001ea1 "abelowdot") ;U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW
(define-keysym #x1001ea2 "Ahook") ;U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE
(define-keysym #x1001ea3 "ahook") ;U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE
(define-keysym #x1001ea4 "Acircumflexacute") ;U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
(define-keysym #x1001ea5 "acircumflexacute") ;U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
(define-keysym #x1001ea6 "Acircumflexgrave") ;U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
(define-keysym #x1001ea7 "acircumflexgrave") ;U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
(define-keysym #x1001ea8 "Acircumflexhook") ;U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
(define-keysym #x1001ea9 "acircumflexhook") ;U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
(define-keysym #x1001eaa "Acircumflextilde") ;U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
(define-keysym #x1001eab "acircumflextilde") ;U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
(define-keysym #x1001eac "Acircumflexbelowdot") ;U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
(define-keysym #x1001ead "acircumflexbelowdot") ;U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
(define-keysym #x1001eae "Abreveacute") ;U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
(define-keysym #x1001eaf "abreveacute") ;U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE
(define-keysym #x1001eb0 "Abrevegrave") ;U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
(define-keysym #x1001eb1 "abrevegrave") ;U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE
(define-keysym #x1001eb2 "Abrevehook") ;U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
(define-keysym #x1001eb3 "abrevehook") ;U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
(define-keysym #x1001eb4 "Abrevetilde") ;U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE
(define-keysym #x1001eb5 "abrevetilde") ;U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE
(define-keysym #x1001eb6 "Abrevebelowdot") ;U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
(define-keysym #x1001eb7 "abrevebelowdot") ;U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
(define-keysym #x1001eb8 "Ebelowdot") ;U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW
(define-keysym #x1001eb9 "ebelowdot") ;U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW
(define-keysym #x1001eba "Ehook") ;U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE
(define-keysym #x1001ebb "ehook") ;U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE
(define-keysym #x1001ebc "Etilde") ;U+1EBC LATIN CAPITAL LETTER E WITH TILDE
(define-keysym #x1001ebd "etilde") ;U+1EBD LATIN SMALL LETTER E WITH TILDE
(define-keysym #x1001ebe "Ecircumflexacute") ;U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
(define-keysym #x1001ebf "ecircumflexacute") ;U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
(define-keysym #x1001ec0 "Ecircumflexgrave") ;U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
(define-keysym #x1001ec1 "ecircumflexgrave") ;U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
(define-keysym #x1001ec2 "Ecircumflexhook") ;U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
(define-keysym #x1001ec3 "ecircumflexhook") ;U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
(define-keysym #x1001ec4 "Ecircumflextilde") ;U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
(define-keysym #x1001ec5 "ecircumflextilde") ;U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
(define-keysym #x1001ec6 "Ecircumflexbelowdot") ;U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
(define-keysym #x1001ec7 "ecircumflexbelowdot") ;U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
(define-keysym #x1001ec8 "Ihook") ;U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE
(define-keysym #x1001ec9 "ihook") ;U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE
(define-keysym #x1001eca "Ibelowdot") ;U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW
(define-keysym #x1001ecb "ibelowdot") ;U+1ECB LATIN SMALL LETTER I WITH DOT BELOW
(define-keysym #x1001ecc "Obelowdot") ;U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW
(define-keysym #x1001ecd "obelowdot") ;U+1ECD LATIN SMALL LETTER O WITH DOT BELOW
(define-keysym #x1001ece "Ohook") ;U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE
(define-keysym #x1001ecf "ohook") ;U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE
(define-keysym #x1001ed0 "Ocircumflexacute") ;U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
(define-keysym #x1001ed1 "ocircumflexacute") ;U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
(define-keysym #x1001ed2 "Ocircumflexgrave") ;U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
(define-keysym #x1001ed3 "ocircumflexgrave") ;U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
(define-keysym #x1001ed4 "Ocircumflexhook") ;U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
(define-keysym #x1001ed5 "ocircumflexhook") ;U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
(define-keysym #x1001ed6 "Ocircumflextilde") ;U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
(define-keysym #x1001ed7 "ocircumflextilde") ;U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
(define-keysym #x1001ed8 "Ocircumflexbelowdot") ;U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
(define-keysym #x1001ed9 "ocircumflexbelowdot") ;U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
(define-keysym #x1001eda "Ohornacute") ;U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE
(define-keysym #x1001edb "ohornacute") ;U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE
(define-keysym #x1001edc "Ohorngrave") ;U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE
(define-keysym #x1001edd "ohorngrave") ;U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE
(define-keysym #x1001ede "Ohornhook") ;U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
(define-keysym #x1001edf "ohornhook") ;U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
(define-keysym #x1001ee0 "Ohorntilde") ;U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE
(define-keysym #x1001ee1 "ohorntilde") ;U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE
(define-keysym #x1001ee2 "Ohornbelowdot") ;U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
(define-keysym #x1001ee3 "ohornbelowdot") ;U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW
(define-keysym #x1001ee4 "Ubelowdot") ;U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW
(define-keysym #x1001ee5 "ubelowdot") ;U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW
(define-keysym #x1001ee6 "Uhook") ;U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE
(define-keysym #x1001ee7 "uhook") ;U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE
(define-keysym #x1001ee8 "Uhornacute") ;U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE
(define-keysym #x1001ee9 "uhornacute") ;U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE
(define-keysym #x1001eea "Uhorngrave") ;U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE
(define-keysym #x1001eeb "uhorngrave") ;U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE
(define-keysym #x1001eec "Uhornhook") ;U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
(define-keysym #x1001eed "uhornhook") ;U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
(define-keysym #x1001eee "Uhorntilde") ;U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE
(define-keysym #x1001eef "uhorntilde") ;U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE
(define-keysym #x1001ef0 "Uhornbelowdot") ;U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
(define-keysym #x1001ef1 "uhornbelowdot") ;U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW
(define-keysym #x1001ef4 "Ybelowdot") ;U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW
(define-keysym #x1001ef5 "ybelowdot") ;U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW
(define-keysym #x1001ef6 "Yhook") ;U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE
(define-keysym #x1001ef7 "yhook") ;U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE
(define-keysym #x1001ef8 "Ytilde") ;U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE
(define-keysym #x1001ef9 "ytilde") ;U+1EF9 LATIN SMALL LETTER Y WITH TILDE
(define-keysym #x10001a0 "Ohorn") ;U+01A0 LATIN CAPITAL LETTER O WITH HORN
(define-keysym #x10001a1 "ohorn") ;U+01A1 LATIN SMALL LETTER O WITH HORN
(define-keysym #x10001af "Uhorn") ;U+01AF LATIN CAPITAL LETTER U WITH HORN
(define-keysym #x10001b0 "uhorn") ;U+01B0 LATIN SMALL LETTER U WITH HORN
(define-keysym #x10020a0 "EcuSign")     ;U+20A0 EURO-CURRENCY SIGN
(define-keysym #x10020a1 "ColonSign")   ;U+20A1 COLON SIGN
(define-keysym #x10020a2 "CruzeiroSign") ;U+20A2 CRUZEIRO SIGN
(define-keysym #x10020a3 "FFrancSign")  ;U+20A3 FRENCH FRANC SIGN
(define-keysym #x10020a4 "LiraSign")    ;U+20A4 LIRA SIGN
(define-keysym #x10020a5 "MillSign")    ;U+20A5 MILL SIGN
(define-keysym #x10020a6 "NairaSign")   ;U+20A6 NAIRA SIGN
(define-keysym #x10020a7 "PesetaSign")  ;U+20A7 PESETA SIGN
(define-keysym #x10020a8 "RupeeSign")   ;U+20A8 RUPEE SIGN
(define-keysym #x10020a9 "WonSign")     ;U+20A9 WON SIGN
(define-keysym #x10020aa "NewSheqelSign") ;U+20AA NEW SHEQEL SIGN
(define-keysym #x10020ab "DongSign")    ;U+20AB DONG SIGN
(define-keysym #x20ac "EuroSign")       ;U+20AC EURO SIGN
(define-keysym #x1002070 "zerosuperior") ;U+2070 SUPERSCRIPT ZERO
(define-keysym #x1002074 "foursuperior") ;U+2074 SUPERSCRIPT FOUR
(define-keysym #x1002075 "fivesuperior") ;U+2075 SUPERSCRIPT FIVE
(define-keysym #x1002076 "sixsuperior") ;U+2076 SUPERSCRIPT SIX
(define-keysym #x1002077 "sevensuperior") ;U+2077 SUPERSCRIPT SEVEN
(define-keysym #x1002078 "eightsuperior") ;U+2078 SUPERSCRIPT EIGHT
(define-keysym #x1002079 "ninesuperior") ;U+2079 SUPERSCRIPT NINE
(define-keysym #x1002080 "zerosubscript") ;U+2080 SUBSCRIPT ZERO
(define-keysym #x1002081 "onesubscript") ;U+2081 SUBSCRIPT ONE
(define-keysym #x1002082 "twosubscript") ;U+2082 SUBSCRIPT TWO
(define-keysym #x1002083 "threesubscript") ;U+2083 SUBSCRIPT THREE
(define-keysym #x1002084 "foursubscript") ;U+2084 SUBSCRIPT FOUR
(define-keysym #x1002085 "fivesubscript") ;U+2085 SUBSCRIPT FIVE
(define-keysym #x1002086 "sixsubscript") ;U+2086 SUBSCRIPT SIX
(define-keysym #x1002087 "sevensubscript") ;U+2087 SUBSCRIPT SEVEN
(define-keysym #x1002088 "eightsubscript") ;U+2088 SUBSCRIPT EIGHT
(define-keysym #x1002089 "ninesubscript") ;U+2089 SUBSCRIPT NINE
(define-keysym #x1002202 "partdifferential") ;U+2202 PARTIAL DIFFERENTIAL
(define-keysym #x1002205 "emptyset")    ;U+2205 NULL SET
(define-keysym #x1002208 "elementof")   ;U+2208 ELEMENT OF
(define-keysym #x1002209 "notelementof") ;U+2209 NOT AN ELEMENT OF
(define-keysym #x100220B "containsas")  ;U+220B CONTAINS AS MEMBER
(define-keysym #x100221A "squareroot")  ;U+221A SQUARE ROOT
(define-keysym #x100221B "cuberoot")    ;U+221B CUBE ROOT
(define-keysym #x100221C "fourthroot")  ;U+221C FOURTH ROOT
(define-keysym #x100222C "dintegral")   ;U+222C DOUBLE INTEGRAL
(define-keysym #x100222D "tintegral")   ;U+222D TRIPLE INTEGRAL
(define-keysym #x1002235 "because")     ;U+2235 BECAUSE
(define-keysym #x1002248 "approxeq")    ;U+2245 ALMOST EQUAL TO
(define-keysym #x1002247 "notapproxeq") ;U+2247 NOT ALMOST EQUAL TO
(define-keysym #x1002262 "notidentical") ;U+2262 NOT IDENTICAL TO
(define-keysym #x1002263 "stricteq")    ;U+2263 STRICTLY EQUIVALENT TO

;; A bunch of extended keysyms

(define-keysym #x100000A8 "hpmute_acute")
(define-keysym #x100000A9 "hpmute_grave")
(define-keysym #x100000AA "hpmute_asciicircum")
(define-keysym #x100000AB "hpmute_diaeresis")
(define-keysym #x100000AC "hpmute_asciitilde")
(define-keysym #x100000AF "hplira")
(define-keysym #x100000BE "hpguilder")
(define-keysym #x100000EE "hpYdiaeresis")
(define-keysym #x100000EE "hpIO")
(define-keysym #x100000F6 "hplongminus")
(define-keysym #x100000FC "hpblock")
(define-keysym #x1000FF00 "apLineDel")
(define-keysym #x1000FF01 "apCharDel")
(define-keysym #x1000FF02 "apCopy")
(define-keysym #x1000FF03 "apCut")
(define-keysym #x1000FF04 "apPaste")
(define-keysym #x1000FF05 "apMove")
(define-keysym #x1000FF06 "apGrow")
(define-keysym #x1000FF07 "apCmd")
(define-keysym #x1000FF08 "apShell")
(define-keysym #x1000FF09 "apLeftBar")
(define-keysym #x1000FF0A "apRightBar")
(define-keysym #x1000FF0B "apLeftBox")
(define-keysym #x1000FF0C "apRightBox")
(define-keysym #x1000FF0D "apUpBox")
(define-keysym #x1000FF0E "apDownBox")
(define-keysym #x1000FF0F "apPop")
(define-keysym #x1000FF10 "apRead")
(define-keysym #x1000FF11 "apEdit")
(define-keysym #x1000FF12 "apSave")
(define-keysym #x1000FF13 "apExit")
(define-keysym #x1000FF14 "apRepeat")
(define-keysym #x1000FF48 "hpModelock1")
(define-keysym #x1000FF49 "hpModelock2")
(define-keysym #x1000FF6C "hpReset")
(define-keysym #x1000FF6D "hpSystem")
(define-keysym #x1000FF6E "hpUser")
(define-keysym #x1000FF6F "hpClearLine")
(define-keysym #x1000FF70 "hpInsertLine")
(define-keysym #x1000FF71 "hpDeleteLine")
(define-keysym #x1000FF72 "hpInsertChar")
(define-keysym #x1000FF73 "hpDeleteChar")
(define-keysym #x1000FF74 "hpBackTab")
(define-keysym #x1000FF75 "hpKP_BackTab")
(define-keysym #x1000FFA8 "apKP_parenleft")
(define-keysym #x1000FFA9 "apKP_parenright")
(define-keysym #x10004001 "I2ND_FUNC_L")
(define-keysym #x10004002 "I2ND_FUNC_R")
(define-keysym #x10004003 "IREMOVE")
(define-keysym #x10004004 "IREPEAT")
(define-keysym #x10004101 "IA1")
(define-keysym #x10004102 "IA2")
(define-keysym #x10004103 "IA3")
(define-keysym #x10004104 "IA4")
(define-keysym #x10004105 "IA5")
(define-keysym #x10004106 "IA6")
(define-keysym #x10004107 "IA7")
(define-keysym #x10004108 "IA8")
(define-keysym #x10004109 "IA9")
(define-keysym #x1000410A "IA10")
(define-keysym #x1000410B "IA11")
(define-keysym #x1000410C "IA12")
(define-keysym #x1000410D "IA13")
(define-keysym #x1000410E "IA14")
(define-keysym #x1000410F "IA15")
(define-keysym #x10004201 "IB1")
(define-keysym #x10004202 "IB2")
(define-keysym #x10004203 "IB3")
(define-keysym #x10004204 "IB4")
(define-keysym #x10004205 "IB5")
(define-keysym #x10004206 "IB6")
(define-keysym #x10004207 "IB7")
(define-keysym #x10004208 "IB8")
(define-keysym #x10004209 "IB9")
(define-keysym #x1000420A "IB10")
(define-keysym #x1000420B "IB11")
(define-keysym #x1000420C "IB12")
(define-keysym #x1000420D "IB13")
(define-keysym #x1000420E "IB14")
(define-keysym #x1000420F "IB15")
(define-keysym #x10004210 "IB16")
(define-keysym #x1000FF00 "DRemove")
(define-keysym #x1000FEB0 "Dring_accent")
(define-keysym #x1000FE5E "Dcircumflex_accent")
(define-keysym #x1000FE2C "Dcedilla_accent")
(define-keysym #x1000FE27 "Dacute_accent")
(define-keysym #x1000FE60 "Dgrave_accent")
(define-keysym #x1000FE7E "Dtilde")
(define-keysym #x1000FE22 "Ddiaeresis")
(define-keysym #x1004FF02 "osfCopy")
(define-keysym #x1004FF03 "osfCut")
(define-keysym #x1004FF04 "osfPaste")
(define-keysym #x1004FF07 "osfBackTab")
(define-keysym #x1004FF08 "osfBackSpace")
(define-keysym #x1004FF0B "osfClear")
(define-keysym #x1004FF1B "osfEscape")
(define-keysym #x1004FF31 "osfAddMode")
(define-keysym #x1004FF32 "osfPrimaryPaste")
(define-keysym #x1004FF33 "osfQuickPaste")
(define-keysym #x1004FF40 "osfPageLeft")
(define-keysym #x1004FF41 "osfPageUp")
(define-keysym #x1004FF42 "osfPageDown")
(define-keysym #x1004FF43 "osfPageRight")
(define-keysym #x1004FF44 "osfActivate")
(define-keysym #x1004FF45 "osfMenuBar")
(define-keysym #x1004FF51 "osfLeft")
(define-keysym #x1004FF52 "osfUp")
(define-keysym #x1004FF53 "osfRight")
(define-keysym #x1004FF54 "osfDown")
(define-keysym #x1004FF55 "osfPrior")
(define-keysym #x1004FF56 "osfNext")
(define-keysym #x1004FF57 "osfEndLine")
(define-keysym #x1004FF58 "osfBeginLine")
(define-keysym #x1004FF59 "osfEndData")
(define-keysym #x1004FF5A "osfBeginData")
(define-keysym #x1004FF5B "osfPrevMenu")
(define-keysym #x1004FF5C "osfNextMenu")
(define-keysym #x1004FF5D "osfPrevField")
(define-keysym #x1004FF5E "osfNextField")
(define-keysym #x1004FF60 "osfSelect")
(define-keysym #x1004FF63 "osfInsert")
(define-keysym #x1004FF65 "osfUndo")
(define-keysym #x1004FF67 "osfMenu")
(define-keysym #x1004FF69 "osfCancel")
(define-keysym #x1004FF6A "osfHelp")
(define-keysym #x1004FF71 "osfSelectAll")
(define-keysym #x1004FF72 "osfDeselectAll")
(define-keysym #x1004FF73 "osfReselect")
(define-keysym #x1004FF74 "osfExtend")
(define-keysym #x1004FF78 "osfRestore")
(define-keysym #x1004FF7E "osfSwitchDirection")
(define-keysym #x1004FFF5 "osfPriorMinor")
(define-keysym #x1004FFF6 "osfNextMinor")
(define-keysym #x1004FFF7 "osfRightLine")
(define-keysym #x1004FFF8 "osfLeftLine")
(define-keysym #x1004FFFF "osfDelete")
(define-keysym #x1005FF00 "SunFA_Grave")
(define-keysym #x1005FF01 "SunFA_Circum")
(define-keysym #x1005FF02 "SunFA_Tilde")
(define-keysym #x1005FF03 "SunFA_Acute")
(define-keysym #x1005FF04 "SunFA_Diaeresis")
(define-keysym #x1005FF05 "SunFA_Cedilla")
(define-keysym #x1005FF10 "SunF36")
(define-keysym #x1005FF11 "SunF37")
(define-keysym #x1005FF60 "SunSys_Req")
(define-keysym #x1005FF70 "SunProps")
(define-keysym #x1005FF71 "SunFront")
(define-keysym #x1005FF72 "SunCopy")
(define-keysym #x1005FF73 "SunOpen")
(define-keysym #x1005FF74 "SunPaste")
(define-keysym #x1005FF75 "SunCut")
(define-keysym #x1005FF76 "SunPowerSwitch")
(define-keysym #x1005FF77 "SunAudioLowerVolume")
(define-keysym #x1005FF78 "SunAudioMute")
(define-keysym #x1005FF79 "SunAudioRaiseVolume")
(define-keysym #x1005FF7A "SunVideoDegauss")
(define-keysym #x1005FF7B "SunVideoLowerBrightness")
(define-keysym #x1005FF7C "SunVideoRaiseBrightness")
(define-keysym #x1005FF7D "SunPowerSwitchShift")
(define-keysym #xFF20 "SunCompose")
(define-keysym #xFF55 "SunPageUp")
(define-keysym #xFF56 "SunPageDown")
(define-keysym #xFF61 "SunPrint_Screen")
(define-keysym #xFF65 "SunUndo")
(define-keysym #xFF66 "SunAgain")
(define-keysym #xFF68 "SunFind")
(define-keysym #xFF69 "SunStop")
(define-keysym #xFF7E "SunAltGraph")
(define-keysym #x1006FF00 "WYSetup")
(define-keysym #x1006FF00 "ncdSetup")
(define-keysym #x10070001 "XeroxPointerButton1")
(define-keysym #x10070002 "XeroxPointerButton2")
(define-keysym #x10070003 "XeroxPointerButton3")
(define-keysym #x10070004 "XeroxPointerButton4")
(define-keysym #x10070005 "XeroxPointerButton5")
(define-keysym #x1008FF01 "XF86ModeLock")
(define-keysym #x1008FF02 "XF86MonBrightnessUp")
(define-keysym #x1008FF03 "XF86MonBrightnessDown")
(define-keysym #x1008FF04 "XF86KbdLightOnOff")
(define-keysym #x1008FF05 "XF86KbdBrightnessUp")
(define-keysym #x1008FF06 "XF86KbdBrightnessDown")
(define-keysym #x1008FF10 "XF86Standby")
(define-keysym #x1008FF11 "XF86AudioLowerVolume")
(define-keysym #x1008FF12 "XF86AudioMute")
(define-keysym #x1008FF13 "XF86AudioRaiseVolume")
(define-keysym #x1008FF14 "XF86AudioPlay")
(define-keysym #x1008FF15 "XF86AudioStop")
(define-keysym #x1008FF16 "XF86AudioPrev")
(define-keysym #x1008FF17 "XF86AudioNext")
(define-keysym #x1008FF18 "XF86HomePage")
(define-keysym #x1008FF19 "XF86Mail")
(define-keysym #x1008FF1A "XF86Start")
(define-keysym #x1008FF1B "XF86Search")
(define-keysym #x1008FF1C "XF86AudioRecord")
(define-keysym #x1008FF1D "XF86Calculator")
(define-keysym #x1008FF1E "XF86Memo")
(define-keysym #x1008FF1F "XF86ToDoList")
(define-keysym #x1008FF20 "XF86Calendar")
(define-keysym #x1008FF21 "XF86PowerDown")
(define-keysym #x1008FF22 "XF86ContrastAdjust")
(define-keysym #x1008FF23 "XF86RockerUp")
(define-keysym #x1008FF24 "XF86RockerDown")
(define-keysym #x1008FF25 "XF86RockerEnter")
(define-keysym #x1008FF26 "XF86Back")
(define-keysym #x1008FF27 "XF86Forward")
(define-keysym #x1008FF28 "XF86Stop")
(define-keysym #x1008FF29 "XF86Refresh")
(define-keysym #x1008FF2A "XF86PowerOff")
(define-keysym #x1008FF2B "XF86WakeUp")
(define-keysym #x1008FF2C "XF86Eject")
(define-keysym #x1008FF2D "XF86ScreenSaver")
(define-keysym #x1008FF2E "XF86WWW")
(define-keysym #x1008FF2F "XF86Sleep")
(define-keysym #x1008FF30 "XF86Favorites")
(define-keysym #x1008FF31 "XF86AudioPause")
(define-keysym #x1008FF32 "XF86AudioMedia")
(define-keysym #x1008FF33 "XF86MyComputer")
(define-keysym #x1008FF34 "XF86VendorHome")
(define-keysym #x1008FF35 "XF86LightBulb")
(define-keysym #x1008FF36 "XF86Shop")
(define-keysym #x1008FF37 "XF86History")
(define-keysym #x1008FF38 "XF86OpenURL")
(define-keysym #x1008FF39 "XF86AddFavorite")
(define-keysym #x1008FF3A "XF86HotLinks")
(define-keysym #x1008FF3B "XF86BrightnessAdjust")
(define-keysym #x1008FF3C "XF86Finance")
(define-keysym #x1008FF3D "XF86Community")
(define-keysym #x1008FF3E "XF86AudioRewind")
(define-keysym #x1008FF3F "XF86BackForward")
(define-keysym #x1008FF40 "XF86Launch0")
(define-keysym #x1008FF41 "XF86Launch1")
(define-keysym #x1008FF42 "XF86Launch2")
(define-keysym #x1008FF43 "XF86Launch3")
(define-keysym #x1008FF44 "XF86Launch4")
(define-keysym #x1008FF45 "XF86Launch5")
(define-keysym #x1008FF46 "XF86Launch6")
(define-keysym #x1008FF47 "XF86Launch7")
(define-keysym #x1008FF48 "XF86Launch8")
(define-keysym #x1008FF49 "XF86Launch9")
(define-keysym #x1008FF4A "XF86LaunchA")
(define-keysym #x1008FF4B "XF86LaunchB")
(define-keysym #x1008FF4C "XF86LaunchC")
(define-keysym #x1008FF4D "XF86LaunchD")
(define-keysym #x1008FF4E "XF86LaunchE")
(define-keysym #x1008FF4F "XF86LaunchF")
(define-keysym #x1008FF50 "XF86ApplicationLeft")
(define-keysym #x1008FF51 "XF86ApplicationRight")
(define-keysym #x1008FF52 "XF86Book")
(define-keysym #x1008FF53 "XF86CD")
(define-keysym #x1008FF54 "XF86Calculater")
(define-keysym #x1008FF55 "XF86Clear")
(define-keysym #x1008FF56 "XF86Close")
(define-keysym #x1008FF57 "XF86Copy")
(define-keysym #x1008FF58 "XF86Cut")
(define-keysym #x1008FF59 "XF86Display")
(define-keysym #x1008FF5A "XF86DOS")
(define-keysym #x1008FF5B "XF86Documents")
(define-keysym #x1008FF5C "XF86Excel")
(define-keysym #x1008FF5D "XF86Explorer")
(define-keysym #x1008FF5E "XF86Game")
(define-keysym #x1008FF5F "XF86Go")
(define-keysym #x1008FF60 "XF86iTouch")
(define-keysym #x1008FF61 "XF86LogOff")
(define-keysym #x1008FF62 "XF86Market")
(define-keysym #x1008FF63 "XF86Meeting")
(define-keysym #x1008FF65 "XF86MenuKB")
(define-keysym #x1008FF66 "XF86MenuPB")
(define-keysym #x1008FF67 "XF86MySites")
(define-keysym #x1008FF68 "XF86New")
(define-keysym #x1008FF69 "XF86News")
(define-keysym #x1008FF6A "XF86OfficeHome")
(define-keysym #x1008FF6B "XF86Open")
(define-keysym #x1008FF6C "XF86Option")
(define-keysym #x1008FF6D "XF86Paste")
(define-keysym #x1008FF6E "XF86Phone")
(define-keysym #x1008FF70 "XF86Q")
(define-keysym #x1008FF72 "XF86Reply")
(define-keysym #x1008FF73 "XF86Reload")
(define-keysym #x1008FF74 "XF86RotateWindows")
(define-keysym #x1008FF75 "XF86RotationPB")
(define-keysym #x1008FF76 "XF86RotationKB")
(define-keysym #x1008FF77 "XF86Save")
(define-keysym #x1008FF78 "XF86ScrollUp")
(define-keysym #x1008FF79 "XF86ScrollDown")
(define-keysym #x1008FF7A "XF86ScrollClick")
(define-keysym #x1008FF7B "XF86Send")
(define-keysym #x1008FF7C "XF86Spell")
(define-keysym #x1008FF7D "XF86SplitScreen")
(define-keysym #x1008FF7E "XF86Support")
(define-keysym #x1008FF7F "XF86TaskPane")
(define-keysym #x1008FF80 "XF86Terminal")
(define-keysym #x1008FF81 "XF86Tools")
(define-keysym #x1008FF82 "XF86Travel")
(define-keysym #x1008FF84 "XF86UserPB")
(define-keysym #x1008FF85 "XF86User1KB")
(define-keysym #x1008FF86 "XF86User2KB")
(define-keysym #x1008FF87 "XF86Video")
(define-keysym #x1008FF88 "XF86WheelButton")
(define-keysym #x1008FF89 "XF86Word")
(define-keysym #x1008FF8A "XF86Xfer")
(define-keysym #x1008FF8B "XF86ZoomIn")
(define-keysym #x1008FF8C "XF86ZoomOut")
(define-keysym #x1008FF8D "XF86Away")
(define-keysym #x1008FF8E "XF86Messenger")
(define-keysym #x1008FF8F "XF86WebCam")
(define-keysym #x1008FF90 "XF86MailForward")
(define-keysym #x1008FF91 "XF86Pictures")
(define-keysym #x1008FF92 "XF86Music")
(define-keysym #x1008FF93 "XF86Battery")
(define-keysym #x1008FF94 "XF86Bluetooth")
(define-keysym #x1008FF95 "XF86WLAN")
(define-keysym #x1008FF96 "XF86UWB")
(define-keysym #x1008FF97 "XF86AudioForward")
(define-keysym #x1008FF98 "XF86AudioRepeat")
(define-keysym #x1008FF99 "XF86AudioRandomPlay")
(define-keysym #x1008FF9A "XF86Subtitle")
(define-keysym #x1008FF9B "XF86AudioCycleTrack")
(define-keysym #x1008FF9C "XF86CycleAngle")
(define-keysym #x1008FF9D "XF86FrameBack")
(define-keysym #x1008FF9E "XF86FrameForward")
(define-keysym #x1008FF9F "XF86Time")
(define-keysym #x1008FFA0 "XF86Select")
(define-keysym #x1008FFA1 "XF86View")
(define-keysym #x1008FFA2 "XF86TopMenu")
(define-keysym #x1008FFA3 "XF86Red")
(define-keysym #x1008FFA4 "XF86Green")
(define-keysym #x1008FFA5 "XF86Yellow")
(define-keysym #x1008FFA6 "XF86Blue")
(define-keysym #x1008FFA7 "XF86Suspend")
(define-keysym #x1008FFA8 "XF86Hibernate")
(define-keysym #x1008FFA9 "XF86TouchpadToggle")
(define-keysym #x1008FFB0 "XF86TouchpadOn")
(define-keysym #x1008FFB1 "XF86TouchpadOff")
(define-keysym #x1008FFB2 "XF86AudioMicMute")
(define-keysym #x1008FFB5 "XF86RFKill")
(define-keysym #x1008FE01 "XF86_Switch_VT_1")
(define-keysym #x1008FE02 "XF86_Switch_VT_2")
(define-keysym #x1008FE03 "XF86_Switch_VT_3")
(define-keysym #x1008FE04 "XF86_Switch_VT_4")
(define-keysym #x1008FE05 "XF86_Switch_VT_5")
(define-keysym #x1008FE06 "XF86_Switch_VT_6")
(define-keysym #x1008FE07 "XF86_Switch_VT_7")
(define-keysym #x1008FE08 "XF86_Switch_VT_8")
(define-keysym #x1008FE09 "XF86_Switch_VT_9")
(define-keysym #x1008FE0A "XF86_Switch_VT_10")
(define-keysym #x1008FE0B "XF86_Switch_VT_11")
(define-keysym #x1008FE0C "XF86_Switch_VT_12")
(define-keysym #x1008FE20 "XF86_Ungrab")
(define-keysym #x1008FE21 "XF86_ClearGrab")
(define-keysym #x1008FE22 "XF86_Next_VMode")
(define-keysym #x1008FE23 "XF86_Prev_VMode")
(define-keysym #x100000A8 "usldead_acute")
(define-keysym #x100000A9 "usldead_grave")
(define-keysym #x100000AB "usldead_diaeresis")
(define-keysym #x100000AA "usldead_asciicircum")
(define-keysym #x100000AC "usldead_asciitilde")
(define-keysym #x1000FE2C "usldead_cedilla")
(define-keysym #x1000FEB0 "usldead_ring")
