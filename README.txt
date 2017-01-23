                                        ; SLIME 2016-04-19
CL-USER> (loop for add = (random 10)
            until (= add 5) collect add)
(6 9)
While evaluating the form starting at line 3, column 0
of #P"/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp":
While evaluating the form starting at line 3, column 0
of #P"/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp":
CL-USER> (require :elf-parser)

WARNING:
   compiling #<STATIC-FILE "trivial-shell" "website" "source" "index.md"> completed without its input file #P"/Users/zhangdongfeng/quicklisp/dists/quicklisp/software/trivial-shell-20160318-git/website/source/index.md"
WARNING:
   loading #<STATIC-FILE "trivial-shell" "website" "source" "index.md"> completed without its input file #P"/Users/zhangdongfeng/quicklisp/dists/quicklisp/software/trivial-shell-20160318-git/website/source/index.md"
; compiling file "/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp" (written 30 NOV 2016 04:34:34 PM):
; compiling (IN-PACKAGE :ELF-PARSER)
; compiling (DEFUN PACKAGE-SYMBOL-LIST ...)
; compiling (DEFUN SHOW-STATIC-SYMBOLS ...)
; compiling (DEFUN DEBUG-STRINGS ...)
; compiling (DEFUN SHOW-FILE-LAYOUT ...)
; compiling (DEFUN SHOW-MEMORY-LAYOUT ...)
; compiling (DEFUN SYMBOLS-IN-FILE ...)
; compiling (DEFUN DUMP-SYMBOL-LIST ...)
; compiling (DEFUN SHOW-FILE-SYMBOLS ...)
; compiling (DEFPARAMETER *ENDIAN* ...)
; compiling (DEFUN BYTES-TO-INT ...)
; compiling (DEFUN INT-TO-BYTES ...)
; compiling (DEFUN BYTES-FROM ...)
; compiling (DEFUN BYTES-TO ...)
; compiling (DEFINE-BINARY-TYPE UNSIGNED-INTEGER ...)
; compiling (DEFINE-BINARY-TYPE SIGNED-INTEGER ...)
; compiling (DEFMACRO DEFINE-DWARF-DICTIONARY ...)
; compiling (DEFINE-BINARY-TYPE CHAR ...)
; compiling (DEFINE-BINARY-TYPE HALF ...)
; compiling (DEFINE-BINARY-TYPE WORD ...)
; compiling (DEFINE-BINARY-TYPE SWORD ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-TAG-NAME ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-ATTRIBUTE-NAME ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-FORM-VALUE ...)
; compiling (DEFINE-BINARY-TYPE STRING ...)
; compiling (DEFINE-BINARY-TYPE TERMINATED-STRING ...)
; compiling (DEFINE-BINARY-TYPE RAW-BYTES ...)
; compiling (DEFINE-BINARY-CLASS SEGMENT-TUPLES ...)

; file: /Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp
; in: DEFINE-BINARY-CLASS SEGMENT-TUPLES
;     (COM.GIGAMONKEYS.BINARY-DATA:DEFINE-BINARY-CLASS ELF-PARSER::SEGMENT-TUPLES
;                                                      NIL
;                                                      (:READER (ELF-PARSER::IN)
;                                                       (LOOP ELF-PARSER::FOR ELF-PARSER::ADDR = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
;                                                                                                 'ELF-PARSER::WORD
;                                                                                                 ELF-PARSER::IN)
;                                                             ELF-PARSER::FOR ELF-PARSER::SIZE = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
;                                                                                                 'ELF-PARSER::WORD
;                                                                                                 ELF-PARSER::IN)
;                                                             ELF-PARSER::UNTIL (ZEROP
;                                                                                ELF-PARSER::SIZE)
;                                                             ELF-PARSER::COLLECT ...))
;                                                      :WRITER
;                                                      (ELF-PARSER::OUT
;                                                       ELF-PARSER::VAL)
;                                                      (PRINT ELF-PARSER::VAL))
; 
; caught ERROR:
;   (during macroexpansion of (DEFINE-BINARY-CLASS SEGMENT-TUPLES ...))
;   error while parsing arguments to DEFMACRO DEFINE-BINARY-CLASS:
;     too many elements in
;       (SEGMENT-TUPLES NIL
;        (:READER (IN)
;         (LOOP FOR ADDR = (READ-VALUE 'WORD IN)
;               FOR SIZE = (READ-VALUE 'WORD IN)
;               UNTIL (ZEROP SIZE)
;               COLLECT (LIST ADDR SIZE)))
;        :WRITER (OUT VAL) (PRINT VAL))
;     to satisfy lambda list
;       (COM.GIGAMONKEYS.BINARY-DATA::NAME
;        (&REST COM.GIGAMONKEYS.BINARY-DATA::SUPERCLASSES)
;        COM.GIGAMONKEYS.BINARY-DATA::SLOTS):
;     exactly 3 expected, but got 6

; compiling (DEFINE-BINARY-CLASS ADDRESS-RANGE ...)
; compiling (DEFINE-BINARY-CLASS ADDRESS-RANGE-TABLE ...)

                                        ; /Users/zhangdongfeng/.cache/common-lisp/sbcl-1.3.9.122-615bc08-dirty-macosx-x64/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser-TMP.fasl written
                                        ; compilation finished in 0:00:00.746
ASDF could not load elf-parser because
COMPILE-FILE-ERROR while compiling #<CL-SOURCE-FILE "elf-parser" "elf-parser">.
                                        ; 
                                        ; compilation unit aborted
                                        ;   caught 2 fatal ERROR conditions
                                        ;   caught 1 ERROR condition
                                        ; Evaluation aborted on #<UIOP/LISP-BUILD:COMPILE-FILE-ERROR {1005DBCD13}>.
CL-USER> (require :elf-parser)
; compiling file "/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp" (written 30 NOV 2016 04:36:55 PM):
; compiling (IN-PACKAGE :ELF-PARSER)
; compiling (DEFUN PACKAGE-SYMBOL-LIST ...)
; compiling (DEFUN SHOW-STATIC-SYMBOLS ...)
; compiling (DEFUN DEBUG-STRINGS ...)
; compiling (DEFUN SHOW-FILE-LAYOUT ...)
; compiling (DEFUN SHOW-MEMORY-LAYOUT ...)
; compiling (DEFUN SYMBOLS-IN-FILE ...)
; compiling (DEFUN DUMP-SYMBOL-LIST ...)
; compiling (DEFUN SHOW-FILE-SYMBOLS ...)
; compiling (DEFPARAMETER *ENDIAN* ...)
; compiling (DEFUN BYTES-TO-INT ...)
; compiling (DEFUN INT-TO-BYTES ...)
; compiling (DEFUN BYTES-FROM ...)
; compiling (DEFUN BYTES-TO ...)
; compiling (DEFINE-BINARY-TYPE UNSIGNED-INTEGER ...)
; compiling (DEFINE-BINARY-TYPE SIGNED-INTEGER ...)
; compiling (DEFMACRO DEFINE-DWARF-DICTIONARY ...)
; compiling (DEFINE-BINARY-TYPE CHAR ...)
; compiling (DEFINE-BINARY-TYPE HALF ...)
; compiling (DEFINE-BINARY-TYPE WORD ...)
; compiling (DEFINE-BINARY-TYPE SWORD ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-TAG-NAME ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-ATTRIBUTE-NAME ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-FORM-VALUE ...)
; compiling (DEFINE-BINARY-TYPE STRING ...)
; compiling (DEFINE-BINARY-TYPE TERMINATED-STRING ...)
; compiling (DEFINE-BINARY-TYPE RAW-BYTES ...)
; compiling (DEFINE-BINARY-CLASS SEGMENT-TUPLES ...)

; file: /Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp
; in: DEFINE-BINARY-CLASS SEGMENT-TUPLES
;     (COM.GIGAMONKEYS.BINARY-DATA:DEFINE-BINARY-CLASS ELF-PARSER::SEGMENT-TUPLES
;                                                      NIL
;                                                      (:READER (ELF-PARSER::IN)
;                                                       (LOOP ELF-PARSER::FOR ELF-PARSER::ADDR = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
;                                                                                                 'ELF-PARSER::WORD
;                                                                                                 ELF-PARSER::IN)
;                                                             ELF-PARSER::FOR ELF-PARSER::SIZE = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
;                                                                                                 'ELF-PARSER::WORD
;                                                                                                 ELF-PARSER::IN)
;                                                             ELF-PARSER::UNTIL (ZEROP
;                                                                                ELF-PARSER::SIZE)
;                                                             ELF-PARSER::COLLECT ...))
;                                                      (:WRITER
;                                                       (ELF-PARSER::OUT
;                                                        ELF-PARSER::VAL)
;                                                       (PRINT ELF-PARSER::VAL)))
; 
; caught ERROR:
;   (during macroexpansion of (DEFINE-BINARY-CLASS SEGMENT-TUPLES ...))
;   error while parsing arguments to DEFMACRO DEFINE-BINARY-CLASS:
;     too many elements in
;       (SEGMENT-TUPLES NIL
;        (:READER (IN)
;         (LOOP FOR ADDR = (READ-VALUE 'WORD IN)
;               FOR SIZE = (READ-VALUE 'WORD IN)
;               UNTIL (ZEROP SIZE)
;               COLLECT (LIST ADDR SIZE)))
;        (:WRITER (OUT VAL) (PRINT VAL)))
;     to satisfy lambda list
;       (COM.GIGAMONKEYS.BINARY-DATA::NAME
;        (&REST COM.GIGAMONKEYS.BINARY-DATA::SUPERCLASSES)
;        COM.GIGAMONKEYS.BINARY-DATA::SLOTS):
;     exactly 3 expected, but got 4

; compiling (DEFINE-BINARY-CLASS ADDRESS-RANGE ...)
; compiling (DEFINE-BINARY-CLASS ADDRESS-RANGE-TABLE ...)

                                        ; /Users/zhangdongfeng/.cache/common-liqsp/sbcl-1.3.9.122-615bc08-dirty-macosx-x64/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser-TMP.fasl written
                                        ; compilation finished in 0:00:00.670
ASDF could not load elf-parser because
COMPILE-FILE-ERROR while compiling #<CL-SOURCE-FILE "elf-parser" "elf-parser">.
                                        ; 
                                        ; compilation unit aborted
                                        ;   caught 2 fatal ERROR conditions
                                        ;   caught 1 ERROR condition
While evaluating the form starting at line 460, column 0
of #P"/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp":; Evaluation aborted on #<UIOP/LISP-BUILD:COMPILE-FILE-ERROR {10050574A3}>.
CL-USER> (require :elf-parser)

; compiling file "/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp" (written 30 NOV 2016 04:39:50 PM):
; compiling (IN-PACKAGE :ELF-PARSER)
; compiling (DEFUN PACKAGE-SYMBOL-LIST ...)
; compiling (DEFUN SHOW-STATIC-SYMBOLS ...)
; compiling (DEFUN DEBUG-STRINGS ...)
; compiling (DEFUN SHOW-FILE-LAYOUT ...)
; compiling (DEFUN SHOW-MEMORY-LAYOUT ...)
; compiling (DEFUN SYMBOLS-IN-FILE ...)
; compiling (DEFUN DUMP-SYMBOL-LIST ...)
; compiling (DEFUN SHOW-FILE-SYMBOLS ...)
; compiling (DEFPARAMETER *ENDIAN* ...)
; compiling (DEFUN BYTES-TO-INT ...)
; compiling (DEFUN INT-TO-BYTES ...)
; compiling (DEFUN BYTES-FROM ...)
; compiling (DEFUN BYTES-TO ...)
; compiling (DEFINE-BINARY-TYPE UNSIGNED-INTEGER ...)
; compiling (DEFINE-BINARY-TYPE SIGNED-INTEGER ...)
; compiling (DEFMACRO DEFINE-DWARF-DICTIONARY ...)
; compiling (DEFINE-BINARY-TYPE CHAR ...)
; compiling (DEFINE-BINARY-TYPE HALF ...)
; compiling (DEFINE-BINARY-TYPE WORD ...)
; compiling (DEFINE-BINARY-TYPE SWORD ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-TAG-NAME ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-ATTRIBUTE-NAME ...)
; compiling (DEFINE-DWARF-DICTIONARY DW-FORM-VALUE ...)
; compiling (DEFINE-BINARY-TYPE STRING ...)
; compiling (DEFINE-BINARY-TYPE TERMINATED-STRING ...)
; compiling (DEFINE-BINARY-TYPE RAW-BYTES ...)
; compiling (DEFINE-BINARY-TYPE SEGMENT-TUPLES ...)
; compiling (DEFINE-BINARY-CLASS ADDRESS-RANGE ...)
                                        ; compiling (DEFINE-BINARY-CLASS ADDRESS-RANGE-TABLE ...)

                                        ; /Users/zhangdongfeng/.cache/common-lisp/sbcl-1.3.9.122-615bc08-dirty-macosx-x64/Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser-TMP.fasl written
                                        ; compilation finished in 0:00:00.450
NIL
CL-USER> (elf:read-elf #p "/Users/zhangdongfeng/Desktop/tmp/zephyr-project/zephyr/samples/hello_world/microkernel/outdir/zephyr.elf")
#<ELF:ELF {10045BC4B3}>
CL-USER> (defparameter *elf* *)
*ELF*
CL-USER> (in-package :elf-parser)
#<PACKAGE "ELF-PARSER">
ELF-PARSER> (elf:read-elf #p "/Users/zhangdongfeng/Desktop/tmp/zephyr-project/zephyr/samples/hello_world/microkernel/outdir/zephyr.elf")
#<ELF:ELF {100712CB63}>
ELF-PARSER> (defparameter *elf* *)
*ELF*
ELF-PARSER> (show-file-layout *elf*)
OFFSET   CONTENTS           END
0   HEADER             34
34   PROGRAM-TABLE      94
94   text               2640
2640   devconfig          26A0
26A0   rodata             2B64
2B64   FILLER             2B68
2B68   datas              31F4
31F4   initlevel          3254
3254   _k_task_list       32AC
32AC   _k_task_ptr        32B4
32B4   _k_event_list      32C4
32C4   bss                32C4
32C4   noinit             32C4
32C4   .scs               32C4
32C4   .comment           32FF
32FF   .ARM.attributes    3332
3338   .debug_aranges     4538
4538   .debug_info        6BC31
6BC31   .debug_abbrev      7796A
7796A   .debug_line        8605C
8605C   .debug_frame       894EC
894EC   .debug_str         8EE99
8EE99   FILLER             8EEA0
8EEA0   .debug_ranges      904B0
904B0   .debug_loc         9B832
9B832   FILLER             9B834
9B834   .symtab            9F0A4
9F0A4   .strtab            A120D
A120D   .shstrtab          A1304
A1304   SECTION-TABLE      A16EC
A16EC   END                A16EC
NIL
ELF-PARSER> (with-open-file (in file :element-type '(unsigned-byte 8))
              )
                                        ; in: WITH-OPEN-FILE (IN FILE :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                                        ;     (OPEN ELF-PARSER::FILE :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                                        ; 
                                        ; caught WARNING:
                                        ;   undefined variable: FILE
                                        ; 
                                        ; compilation unit finished
                                        ;   Undefined variable:
                                        ;     FILE
                                        ;   caught 1 WARNING condition
                                        ; Evaluation aborted on #<UNBOUND-VARIABLE FILE {1002C804D3}>.
ELF-PARSER> (elf:read-elf )
                                        ; Evaluation aborted on #<SB-INT:SIMPLE-PROGRAM-ERROR "invalid number of arguments: ~S" {1002E2D193}>.
ELF-PARSER> (defparameter *file* #p "/Users/zhangdongfeng/Desktop/tmp/zephyr-project/zephyr/samples/hello_world/microkernel/outdir/zephyr.elf")
*FILE*
ELF-PARSER> ))
                                        ; Evaluation aborted on #<SB-INT:SIMPLE-READER-ERROR "unmatched close parenthesis" {10038F9523}>.
ELF-PARSER> (with-open-file (in *file* :element-type '(unsigned-byte 8))
              (file-position in  #x3338)
              (read-value 'address-range in))
#<ADDRESS-RANGE {1003E44F13}>
ELF-PARSER> (inspect *)

The object is a STANDARD-OBJECT of type ADDRESS-RANGE.
0. UNIT-LENGTH: 28
1. VERSION: 2
2. DEBUGINFO-OFFSET: 0
3. ADDRESS-SIZE: 4
4. SEGMENT-SIZE: 0
5. DUMMY: 0
6. SEGMENTS: ((480 56))
> 6

The object is a CONS.
0. CAR: (480 56)
1. CDR: NIL
> q

                                        ; No value
ELF-PARSER> #x1e0
480

                                        ; file: /Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp
                                        ; in: DEFINE-BINARY-TYPE SEGMENT-TUPLES
                                        ;     (COM.GIGAMONKEYS.BINARY-DATA:DEFINE-BINARY-TYPE ELF-PARSER::SEGMENT-TUPLES
                                        ;         NIL
                                        ;       (:READER (ELF-PARSER::IN)
                                        ;        (LOOP ELF-PARSER::FOR ELF-PARSER::ADDR = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
                                        ;                                                  'ELF-PARSER::WORD
                                        ;                                                  ELF-PARSER::IN)
                                        ;              ELF-PARSER::FOR ELF-PARSER::SIZE = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
                                        ;                                                  'ELF-PARSER::WORD
                                        ;                                                  ELF-PARSER::IN)
                                        ;              ELF-PARSER::UNTIL (AND (ZEROP ELF-PARSER::SIZE)
                                        ;                                     (ZEROP ELF-PARSER::ADDR))
                                        ;              ELF-PARSER::COLLECT ...))
                                        ;       (:WRITER (ELF-PARSER::OUT ELF-PARSER::VAL) (PRINT ELF-PARSER::VAL)))
                                        ; --> PROGN LET* SB-INT:NAMED-LAMBDA FUNCTION SYMBOL-MACROLET 
                                        ; --> SB-PCL::FAST-LEXICAL-METHOD-FUNCTIONS 
                                        ; --> SB-PCL::BIND-FAST-LEXICAL-METHOD-FUNCTIONS FLET LET 
                                        ; --> SB-PCL::BIND-ARGS LET* LOCALLY SYMBOL-MACROLET BLOCK 
                                        ; ==>
                                        ;   (LET ((ELF-PARSER::OUT #:STREAM879) (ELF-PARSER::VAL #:VALUE880))
                                        ;     (PRINT ELF-PARSER::VAL))
                                        ; 
                                        ; caught STYLE-WARNING:
                                        ;   The variable OUT is defined but never used.
                                        ; 
                                        ; compilation unit finished
                                        ;   caught 1 STYLE-WARNING condition

                                        ; file: /Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp
                                        ; in: DEFINE-BINARY-TYPE SEGMENT-TUPLES
                                        ;     (COM.GIGAMONKEYS.BINARY-DATA:DEFINE-BINARY-TYPE ELF-PARSER::SEGMENT-TUPLES
                                        ;         NIL
                                        ;       (:READER (ELF-PARSER::IN)
                                        ;        (LOOP ELF-PARSER::FOR ELF-PARSER::ADDR = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
                                        ;                                                  'ELF-PARSER::WORD
                                        ;                                                  ELF-PARSER::IN)
                                        ;              ELF-PARSER::FOR ELF-PARSER::SIZE = (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE
                                        ;                                                  'ELF-PARSER::WORD
                                        ;                                                  ELF-PARSER::IN)
                                        ;              ELF-PARSER::UNTIL (AND (ZEROP ELF-PARSER::SIZE)
                                        ;                                     (ZEROP ELF-PARSER::ADDR))
                                        ;              ELF-PARSER::COLLECT ...))
                                        ;       (:WRITER (ELF-PARSER::OUT ELF-PARSER::VAL)
                                        ;        (ELF-PARSER::OUT ELF-PARSER::VAL)))
                                        ; --> PROGN LET* SB-INT:NAMED-LAMBDA FUNCTION SYMBOL-MACROLET 
                                        ; --> SB-PCL::FAST-LEXICAL-METHOD-FUNCTIONS 
                                        ; --> SB-PCL::BIND-FAST-LEXICAL-METHOD-FUNCTIONS FLET LET 
                                        ; --> SB-PCL::BIND-ARGS LET* LOCALLY SYMBOL-MACROLET BLOCK 
                                        ; ==>
                                        ;   (LET ((ELF-PARSER::OUT #:STREAM959) (ELF-PARSER::VAL #:VALUE960))
                                        ;     (ELF-PARSER::OUT ELF-PARSER::VAL))
                                        ; 
                                        ; caught STYLE-WARNING:
                                        ;   The variable OUT is defined but never used.


                                        ; file: /Users/zhangdongfeng/quicklisp/local-projects/elf-parser/elf-parser.lisp
                                        ; in: DEFINE-BINARY-TYPE SEGMENT-TUPLES
                                        ;     (ELF-PARSER::OUT ELF-PARSER::VAL)
                                        ; 
                                        ; caught STYLE-WARNING:
                                        ;   undefined function: OUT
                                        ; 
                                        ; compilation unit finished
                                        ;   Undefined function:
                                        ;     OUT
                                        ;   caught 2 STYLE-WARNING conditions
ELF-PARSER> 
                                        ; No value
ELF-PARSER> 
                                        ; No value
ELF-PARSER> 
                                        ; No value
ELF-PARSER> 
                                        ; No value
ELF-PARSER> 
                                        ; No value
ELF-PARSER> (elf:named-section *elf*)
                                        ; Evaluation aborted on #<SB-INT:SIMPLE-PROGRAM-ERROR "invalid number of arguments: ~S" {10031A1673}>.
ELF-PARSER> (elf:named-section *elf* ".debug_aranges") 
#<ELF::SECTION {1007671AD3}>
ELF-PARSER> (inspect *)

The object is a STANDARD-OBJECT of type ELF::SECTION.
0. ELF: #<ELF:ELF {100712CB63}>
1. SH: #<ELF::SECTION-HEADER {1007671793}>
2. PH: NIL
3. NAME: ".debug_aranges"
4. DATA: #(28 0 0 0 2 0 0 0 0 0 4 0 0 0 0 0 224 1 0 0 56 0 0 0 0 0 0 0 0 0 0 0
           44 0 0 0 2 0 13 19 0 0 4 0 0 0 0 0 24 2 0 0 40 0 0 0 64 2 0 0 28 0 0
           0 92 2 0 0 32 0 0 0 0 0 0 0 0 0 0 0 44 0 0 0 2 0 172 40 0 0 4 0 0 0
           0 0 124 2 0 0 28 0 0 0 152 2 0 0 90 0 0 0 242 2 0 0 22 0 0 0 0 0 0 0
           0 0 0 0 44 0 0 0 2 0 24 66 0 0 4 0 0 0 0 0 8 3 0 0 52 0 0 0 60 3 0 0
           52 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 0 0 0 0 20 0 0 0 2 0 166 86 0 0 4
           0 0 0 0 0 0 0 0 0 0 0 0 0 52 0 0 0 2 0 233 105 0 0 4 0 0 0 0 0 0 0 0
           0 44 0 0 0 0 0 0 0 24 0 0 0 112 3 0 0 44 0 0 0 0 0 0 0 24 0 0 0 0 0
           0 0 0 0 0 0 84 0 0 0 2 0 14 108 0 0 4 0 0 0 0 0 156 3 0 0 96 0 0 0
           252 3 0 0 68 0 0 0 64 4 0 0 28 0 0 0 92 4 0 0 52 0 0 0 144 4 0 0 50
           0 0 0 194 4 0 0 68 0 0 0 6 5 0 0 228 0 0 0 236 5 0 0 46 7 0 0 0 0 0
           0 0 0 0 0 60 0 0 0 2 0 248 120 0 0 4 0 0 0 0 0 0 0 0 0 30 0 0 0 0 0
           0 0 64 0 0 0 0 0 0 0 60 0 0 0 0 0 0 0 48 0 0 0 0 0 0 0 44 0 0 0 0 0
           0 0 0 0 0 0 68 0 0 0 2 0 30 124 0 0 4 0 0 0 0 0 28 13 0 0 10 0 0 0
           40 13 0 0 16 0 0 0 56 13 0 0 32 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0 80 0
           0 0 0 0 0 0 40 0 0 0 0 0 0 0 0 0 0 0 124 0 0 0 2 0 101 126 0 0 4 0 0
           0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 36 0 0 0 88 13 0 0 34 0 0 0 0 0 0 0
           20 0 0 0 122 13 0 0 30 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0 24 0 0 0 0 0 0
           0 44 0 0 0 0 0 0 0 46 0 0 0 152 13 0 0 52 0 0 0 204 13 0 0 90 0 0 0
           38 14 0 0 66 0 0 0 0 0 0 0 38 0 0 0 0 0 0 0 0 0 0 0 52 0 0 0 2 0 93
           131 0 0 4 0 0 0 0 0 104 14 0 0 32 0 0 0 136 14 0 0 84 0 0 0 220 14 0
           0 56 0 0 0 20 15 0 0 18 0 0 0 0 0 0 0 0 0 0 0 84 0 0 0 2 0 12 154 0
           0 4 0 0 0 0 0 38 15 0 0 56 0 0 0 0 0 0 0 66 0 0 0 0 0 0 0 112 0 0 0
           96 15 0 0 64 0 0 0 0 0 0 0 22 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 36 0 0
           0 160 15 0 0 28 0 0 0 0 0 0 0 0 0 0 0 124 0 0 0 2 0 108 182 0 0 4 0
           0 0 0 0 0 0 0 0 58 0 0 0 0 0 0 0 70 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0
           92 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 68 0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
           76 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0
           36 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 2 0 11 217 0 0 4
           0 0 0 0 0 188 15 0 0 50 0 0 0 238 15 0 0 60 0 0 0 44 16 0 0 72 0 0 0
           0 0 0 0 62 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 40 0 0 0
           0 0 0 0 0 0 0 0 68 0 0 0 2 0 95 243 0 0 4 0 0 0 0 0 0 0 0 0 16 0 0 0
           0 0 0 0 32 0 0 0 0 0 0 0 48 0 0 0 0 0 0 0 40 0 0 0 116 16 0 0 40 0 0
           0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 2 0 55 10 1 0 4 0 0 0 0
           0 156 16 0 0 16 0 0 0 172 16 0 0 60 0 0 0 232 16 0 0 24 0 0 0 0 0 0
           0 24 0 0 0 0 17 0 0 20 0 0 0 0 0 0 0 48 0 0 0 20 17 0 0 56 0 0 0 0 0
           0 0 0 0 0 0 44 0 0 0 2 0 249 33 1 0 4 0 0 0 0 0 76 17 0 0 32 0 0 0
           108 17 0 0 40 0 0 0 148 17 0 0 184 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 2
           0 235 57 1 0 4 0 0 0 0 0 0 0 0 0 48 0 0 0 0 0 0 0 60 0 0 0 0 0 0 0
           118 0 0 0 0 0 0 0 72 0 0 0 0 0 0 0 22 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0
           36 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 223 83 1 0 4 0 0 0 0 0 0 0 0 0
           10 0 0 0 0 0 0 0 0 0 0 0 44 0 0 0 2 0 109 84 1 0 4 0 0 0 0 0 76 18 0
           0 44 0 0 0 120 18 0 0 52 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 28 0
           0 0 2 0 243 104 1 0 4 0 0 0 0 0 172 18 0 0 42 0 0 0 0 0 0 0 0 0 0 0
           28 0 0 0 2 0 175 125 1 0 4 0 0 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 0 0 0
           0 172 0 0 0 2 0 143 145 1 0 4 0 0 0 0 0 216 18 0 0 52 0 0 0 0 0 0 0
           16 0 0 0 12 19 0 0 84 0 0 0 96 19 0 0 52 0 0 0 148 19 0 0 108 0 0 0
           0 20 0 0 26 0 0 0 26 20 0 0 76 0 0 0 104 20 0 0 100 0 0 0 0 0 0 0 52
           0 0 0 0 0 0 0 16 0 0 0 204 20 0 0 32 0 0 0 236 20 0 0 32 0 0 0 0 0 0
           0 16 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 16 0 0 0 0 0 0
           0 32 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 52 0 0 0
           2 0 214 184 1 0 4 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 28 0 0 0 0 0 0
           0 6 0 0 0 12 21 0 0 32 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 181 217 1
           0 4 0 0 0 0 0 44 21 0 0 84 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 65 239
           1 0 4 0 0 0 0 0 128 21 0 0 36 0 0 0 0 0 0 0 0 0 0 0 36 0 0 0 2 0 164
           15 2 0 4 0 0 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 50 0 0 0 0 0 0 0 0 0 0 0
           52 0 0 0 2 0 116 48 2 0 4 0 0 0 0 0 164 21 0 0 36 0 0 0 200 21 0 0
           14 0 0 0 216 21 0 0 80 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 0 0 0 0 76 0 0
           0 2 0 174 82 2 0 4 0 0 0 0 0 0 0 0 0 62 0 0 0 0 0 0 0 92 0 0 0 0 0 0
           0 124 0 0 0 40 22 0 0 72 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 32 0 0 0 0 0
           0 0 8 0 0 0 0 0 0 0 0 0 0 0 148 0 0 0 2 0 183 117 2 0 4 0 0 0 0 0 0
           0 0 0 82 0 0 0 0 0 0 0 62 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 156 0 0 0 0
           0 0 0 112 0 0 0 0 0 0 0 152 0 0 0 0 0 0 0 84 0 0 0 0 0 0 0 120 0 0 0
           0 0 0 0 108 0 0 0 0 0 0 0 108 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 40 0 0
           0 0 0 0 0 40 0 0 0 0 0 0 0 56 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 48 0 0
           0 0 0 0 0 0 0 0 0 44 0 0 0 2 0 114 161 2 0 4 0 0 0 0 0 112 22 0 0 32
           0 0 0 0 0 0 0 28 0 0 0 144 22 0 0 44 0 0 0 0 0 0 0 0 0 0 0 36 0 0 0
           2 0 151 194 2 0 4 0 0 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 32 0 0 0 0 0 0
           0 0 0 0 0 92 0 0 0 2 0 191 226 2 0 4 0 0 0 0 0 0 0 0 0 34 0 0 0 0 0
           0 0 44 0 0 0 0 0 0 0 84 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0 32 0 0 0 188
           22 0 0 68 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 32 0 0 0 0 23 0 0 24 0 0 0
           0 0 0 0 0 0 0 0 140 0 0 0 2 0 80 6 3 0 4 0 0 0 0 0 0 0 0 0 98 0 0 0
           0 0 0 0 80 0 0 0 0 0 0 0 80 0 0 0 0 0 0 0 72 0 0 0 0 0 0 0 28 0 0 0
           0 0 0 0 212 1 0 0 0 0 0 0 112 0 0 0 0 0 0 0 152 0 0 0 0 0 0 0 152 1
           0 0 0 0 0 0 108 0 0 0 0 0 0 0 96 0 0 0 0 0 0 0 84 0 0 0 0 0 0 0 52 0
           0 0 0 0 0 0 132 0 0 0 0 0 0 0 92 0 0 0 0 0 0 0 0 0 0 0 68 0 0 0 2 0
           231 50 3 0 4 0 0 0 0 0 0 0 0 0 156 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0
           192 0 0 0 0 0 0 0 148 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 40 0 0 0 0 0 0
           0 0 0 0 0 100 0 0 0 2 0 39 87 3 0 4 0 0 0 0 0 0 0 0 0 84 0 0 0 0 0 0
           0 12 0 0 0 0 0 0 0 84 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 92 0 0 0 0 0 0
           0 208 0 0 0 0 0 0 0 236 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 36 0 0 0 0 0
           0 0 32 0 0 0 0 0 0 0 0 0 0 0 212 0 0 0 2 0 46 125 3 0 4 0 0 0 0 0 0
           0 0 0 12 0 0 0 0 0 0 0 26 0 0 0 0 0 0 0 12 0 0 0 24 23 0 0 100 0 0 0
           124 23 0 0 60 0 0 0 184 23 0 0 84 0 0 0 12 24 0 0 12 0 0 0 0 0 0 0
           148 0 0 0 24 24 0 0 112 0 0 0 136 24 0 0 184 0 0 0 0 0 0 0 16 0 0 0
           0 0 0 0 28 0 0 0 64 25 0 0 44 0 0 0 108 25 0 0 136 0 0 0 244 25 0 0
           92 0 0 0 80 26 0 0 112 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0 44 0 0 0 0 0 0
           0 28 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 28 0 0 0 0 0 0
           0 28 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0 0 0 0 0 156 0 0 0 2 0 54 167 3 0
           4 0 0 0 0 0 0 0 0 0 40 0 0 0 192 26 0 0 28 0 0 0 220 26 0 0 36 0 0 0
           0 27 0 0 68 0 0 0 68 27 0 0 76 0 0 0 144 27 0 0 48 0 0 0 0 0 0 0 52
           0 0 0 0 0 0 0 76 0 0 0 0 0 0 0 26 0 0 0 192 27 0 0 36 0 0 0 228 27 0
           0 40 0 0 0 12 28 0 0 36 0 0 0 48 28 0 0 92 0 0 0 0 0 0 0 28 0 0 0 0
           0 0 0 28 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 0 0 0 0 180
           0 0 0 2 0 178 205 3 0 4 0 0 0 0 0 0 0 0 0 196 0 0 0 0 0 0 0 60 0 0 0
           0 0 0 0 66 0 0 0 140 28 0 0 62 0 0 0 0 0 0 0 106 0 0 0 202 28 0 0 80
           0 0 0 0 0 0 0 70 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 30
           0 0 0 0 0 0 0 72 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 24 0 0 0 0 0 0 0 30
           0 0 0 0 0 0 0 194 0 0 0 0 0 0 0 34 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0
           196 0 0 0 0 0 0 0 34 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 0 0 0 0 52 0 0 0
           2 0 97 240 3 0 4 0 0 0 0 0 28 29 0 0 40 0 0 0 0 0 0 0 104 0 0 0 0 0
           0 0 104 0 0 0 0 0 0 0 124 0 0 0 0 0 0 0 0 0 0 0 52 0 0 0 2 0 17 19 4
           0 4 0 0 0 0 0 0 0 0 0 112 0 0 0 0 0 0 0 46 0 0 0 0 0 0 0 68 0 0 0 0
           0 0 0 68 1 0 0 0 0 0 0 0 0 0 0 52 0 0 0 2 0 86 55 4 0 4 0 0 0 0 0 0
           0 0 0 128 0 0 0 0 0 0 0 46 0 0 0 0 0 0 0 136 0 0 0 0 0 0 0 108 1 0 0
           0 0 0 0 0 0 0 0 108 0 0 0 2 0 142 92 4 0 4 0 0 0 0 0 0 0 0 0 38 0 0
           0 0 0 0 0 32 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 28 0 0 0 0 0 0 0 8 0 0 0
           0 0 0 0 8 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 12 0 0 0 0
           0 0 0 12 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 2 0 128 127
           4 0 4 0 0 0 0 0 0 0 0 0 208 0 0 0 0 0 0 0 184 0 0 0 0 0 0 0 200 0 0
           0 0 0 0 0 70 0 0 0 0 0 0 0 228 0 0 0 0 0 0 0 140 3 0 0 0 0 0 0 140 0
           0 0 0 0 0 0 0 0 0 0 44 0 0 0 2 0 58 174 4 0 4 0 0 0 0 0 0 0 0 0 44 0
           0 0 0 0 0 0 16 0 0 0 68 29 0 0 48 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0
           193 207 4 0 4 0 0 0 0 0 116 29 0 0 148 0 0 0 0 0 0 0 0 0 0 0 52 0 0
           0 2 0 34 242 4 0 4 0 0 0 0 0 8 30 0 0 8 0 0 0 16 30 0 0 88 0 0 0 104
           30 0 0 16 0 0 0 120 30 0 0 240 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 90
           245 4 0 4 0 0 0 0 0 104 31 0 0 6 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0
           145 245 4 0 4 0 0 0 0 0 110 31 0 0 18 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0
           2 0 137 23 5 0 4 0 0 0 0 0 128 31 0 0 40 0 0 0 0 0 0 0 0 0 0 0 28 0
           0 0 2 0 253 42 5 0 4 0 0 0 0 0 168 31 0 0 8 0 0 0 0 0 0 0 0 0 0 0 28
           0 0 0 2 0 113 62 5 0 4 0 0 0 0 0 0 0 0 0 32 0 0 0 0 0 0 0 0 0 0 0 28
           0 0 0 2 0 56 67 5 0 4 0 0 0 0 0 176 31 0 0 40 0 0 0 0 0 0 0 0 0 0 0
           28 0 0 0 2 0 12 68 5 0 4 0 0 0 0 0 216 31 0 0 28 0 0 0 0 0 0 0 0 0 0
           0 28 0 0 0 2 0 222 86 5 0 4 0 0 0 0 0 244 31 0 0 48 0 0 0 0 0 0 0 0
           0 0 0 44 0 0 0 2 0 45 107 5 0 4 0 0 0 0 0 36 32 0 0 96 0 0 0 132 32
           0 0 20 0 0 0 152 32 0 0 16 0 0 0 0 0 0 0 0 0 0 0 68 0 0 0 2 0 249
           107 5 0 4 0 0 0 0 0 168 32 0 0 32 0 0 0 200 32 0 0 148 0 0 0 92 33 0
           0 148 0 0 0 240 33 0 0 176 0 0 0 160 34 0 0 204 0 0 0 108 35 0 0 24
           0 0 0 0 0 0 0 0 0 0 0 20 0 0 0 2 0 233 136 5 0 4 0 0 0 0 0 0 0 0 0 0
           0 0 0 52 0 0 0 2 0 73 155 5 0 4 0 0 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0
           32 0 0 0 0 0 0 0 24 0 0 0 132 35 0 0 12 0 0 0 0 0 0 0 0 0 0 0 28 0 0
           0 2 0 83 175 5 0 4 0 0 0 0 0 144 35 0 0 88 0 0 0 0 0 0 0 0 0 0 0 44
           0 0 0 2 0 43 197 5 0 4 0 0 0 0 0 232 35 0 0 12 0 0 0 244 35 0 0 10 0
           0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 251 197 5 0 4 0 0
           0 0 0 0 36 0 0 40 0 0 0 0 0 0 0 0 0 0 0 44 0 0 0 2 0 206 198 5 0 4 0
           0 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0 36 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0
           0 0 0 28 0 0 0 2 0 158 199 5 0 4 0 0 0 0 0 40 36 0 0 36 0 0 0 0 0 0
           0 0 0 0 0 28 0 0 0 2 0 117 200 5 0 4 0 0 0 0 0 76 36 0 0 76 0 0 0 0
           0 0 0 0 0 0 0 28 0 0 0 2 0 35 222 5 0 4 0 0 0 0 0 152 36 0 0 104 0 0
           0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 188 242 5 0 4 0 0 0 0 0 0 37 0 0 96 0
           0 0 0 0 0 0 0 0 0 0 44 0 0 0 2 0 128 19 6 0 4 0 0 0 0 0 96 37 0 0 28
           0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 80 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0
           86 20 6 0 4 0 0 0 0 0 124 37 0 0 6 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2
           0 55 21 6 0 4 0 0 0 0 0 132 37 0 0 16 0 0 0 0 0 0 0 0 0 0 0 20 0 0 0
           2 0 1 40 6 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 36 0 0 0 2 0 102 58 6 0 4 0
           0 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0 52 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2
           0 222 77 6 0 4 0 0 0 0 0 148 37 0 0 18 0 0 0 0 0 0 0 0 0 0 0 28 0 0
           0 2 0 132 96 6 0 4 0 0 0 0 0 0 0 0 0 56 0 0 0 0 0 0 0 0 0 0 0 20 0 0
           0 2 0 100 97 6 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 28 0 0 0 2 0 184 97 6 0
           4 0 0 0 0 0 166 37 0 0 6 0 0 0 0 0 0 0 0 0 0 0)
> 1

The object is a STANDARD-OBJECT of type ELF::SECTION-HEADER.
0. NAME: 145
1. TYPE: :PROGBITS
2. FLAGS: 0
3. ADDRESS: 0
4. OFFSET: 13112
5. SIZE: 4608
6. LINK: 0
7. INFO: 0
8. ADDRALIGN: 8
9. ENTSIZE: 0
> q

                                        ; No value
ELF-PARSER> (elf:sh (elf:named-section *elf* ".debug_aranges") ) 
#<ELF::SECTION-HEADER {1007671793}>
ELF-PARSER> (inspect *)

The object is a STANDARD-OBJECT of type ELF::SECTION-HEADER.
0. NAME: 145
1. TYPE: :PROGBITS
2. FLAGS: 0
3. ADDRESS: 0
4. OFFSET: 13112
5. SIZE: 4608
6. LINK: 0
7. INFO: 0
8. ADDRALIGN: 8
9. ENTSIZE: 0
> q

                                        ; No value
ELF-PARSER> (offset (elf:sh (elf:named-section *elf* ".debug_aranges") ) )

                                        ; in: OFFSET (ELF:SH (ELF:NAMED-SECTION *ELF* ".debug_aranges"))
                                        ;     (ELF-PARSER::OFFSET
                                        ;      (ELF:SH (ELF:NAMED-SECTION ELF-PARSER::*ELF* ".debug_aranges")))
                                        ; 
                                        ; caught STYLE-WARNING:
                                        ;   undefined function: OFFSET
                                        ; 
                                        ; compilation unit finished
                                        ;   Undefined function:
                                        ;     OFFSET
                                        ;   caught 1 STYLE-WARNING condition
                                        ; Evaluation aborted on #<UNDEFINED-FUNCTION OFFSET {100376BDA3}>.
ELF-PARSER> (elf:offset (elf:sh (elf:named-section *elf* ".debug_aranges") ) )
13112
ELF-PARSER> (inpect *elf*)

                                        ; in: INPECT *ELF*
                                        ;     (ELF-PARSER::INPECT ELF-PARSER::*ELF*)
                                        ; 
                                        ; caught STYLE-WARNING:
                                        ;   undefined function: INPECT
                                        ; 
                                        ; compilation unit finished
                                        ;   Undefined function:
                                        ;     INPECT
                                        ;   caught 1 STYLE-WARNING condition
                                        ; Evaluation aborted on #<UNDEFINED-FUNCTION INPECT {1003D42403}>.
ELF-PARSER> (inspect *elf*)

The object is a STANDARD-OBJECT of type ELF:ELF.
0. HEADER: #<ELF:ELF-HEADER {1007661C03}>
1. SECTION-TABLE: (#<ELF::SECTION-HEADER {10076715D3}>
                                         #<ELF::SECTION-HEADER {10076715F3}>
                                         #<ELF::SECTION-HEADER {1007671613}>
                                         #<ELF::SECTION-HEADER {1007671633}>
                                         #<ELF::SECTION-HEADER {1007671653}>
                                         #<ELF::SECTION-HEADER {1007671673}>
                                         #<ELF::SECTION-HEADER {1007671693}>
                                         #<ELF::SECTION-HEADER {10076716B3}>
                                         #<ELF::SECTION-HEADER {10076716D3}>
                                         #<ELF::SECTION-HEADER {10076716F3}>
                                         #<ELF::SECTION-HEADER {1007671713}>
                                         #<ELF::SECTION-HEADER {1007671733}>
                                         #<ELF::SECTION-HEADER {1007671753}>
                                         #<ELF::SECTION-HEADER {1007671773}>
                                         #<ELF::SECTION-HEADER {1007671793}>
                                         #<ELF::SECTION-HEADER {10076717B3}>
                                         #<ELF::SECTION-HEADER {10076717D3}>
                                         #<ELF::SECTION-HEADER {10076717F3}>
                                         #<ELF::SECTION-HEADER {1007671813}>
                                         #<ELF::SECTION-HEADER {1007671833}>
                                         #<ELF::SECTION-HEADER {1007671853}>
                                         #<ELF::SECTION-HEADER {1007671873}>
                                         #<ELF::SECTION-HEADER {100765F9A3}>
                                         #<ELF::SECTION-HEADER {100765F9E3}>
                                         #<ELF::SECTION-HEADER {1007671893}>)
2. PROGRAM-TABLE: (#<ELF::PROGRAM-HEADER-32 {10076718B3}>
                                            #<ELF::PROGRAM-HEADER-32 {10076718D3}>
                                            #<ELF::PROGRAM-HEADER-32 {10076718F3}>)
3. SECTIONS: (#<ELF::SECTION {1007671913}> #<ELF::SECTION {1007671933}>
                             #<ELF::SECTION {1007671953}> #<ELF::SECTION {1007671973}>
                             #<ELF::SECTION {1007671993}> #<ELF::SECTION {10076719B3}>
                             #<ELF::SECTION {10076719D3}> #<ELF::SECTION {10076719F3}>
                             #<ELF::SECTION {1007671A13}> #<ELF::SECTION {1007671A33}>
                             #<ELF::SECTION {1007671A53}> #<ELF::SECTION {1007671A73}>
                             #<ELF::SECTION {1007671A93}> #<ELF::SECTION {1007671AB3}>
                             #<ELF::SECTION {1007671AD3}> #<ELF::SECTION {1007671AF3}>
                             #<ELF::SECTION {1007671B13}> #<ELF::SECTION {1007671B33}>
                             #<ELF::SECTION {1007671B53}> #<ELF::SECTION {1007671B73}>
                             #<ELF::SECTION {1007671B93}> #<ELF::SECTION {100765F983}>
                             #<ELF::SECTION {10071FB3B3}> #<ELF::SECTION {10071FB793}>
                             #<ELF::SECTION {1007665DF3}>)
4. ORDERING: ((0 52 :HEADER) (52 96 :PROGRAM-TABLE) (148 9644 1) (9792 96 2)
              (9888 1220 3) (11108 4 #(0 0 0 0)) (11112 1676 4) (12788 96 5)
              (12884 88 6) (12972 8 7) (12980 16 8) (12996 132 9)
              (12996 6400 10) (12996 4096 11) (12996 59 12) (13055 51 13)
              (13112 4608 14) (17720 423673 15) (441393 48441 16)
              (489834 59122 17) (548956 13456 18) (562412 22957 19)
              (585369 7 #(0 0 0 0 0 0 0)) (585376 5648 20) (591024 45954 21)
              (636978 2 #(0 0)) (636980 14448 23) (651428 8553 24)
              (659981 247 22) (660228 1000 :SECTION-TABLE) (661228 0 :END))
> q

                                        ; No value
ELF-PARSER> (file-position *file*)
                                        ; Evaluation aborted on #<TYPE-ERROR expected-type: "STREAM"
datum:
#P"/Users/zhangdongfeng/Desktop/tmp/zephyr-project/zephyr/samples/hello_world/microkernel/outdir/zephyr.elf">.
ELF-PARSER> (file-position (open *file*))
0
ELF-PARSER> (> 2 1)
T
ELF-PARSER> (with-open-file 
                (file-position in  #x3338)
              (read-value 'address-range in)) T
                                        ; in: WITH-OPEN-FILE (FILE-POSITION IN 13112)
                                        ;     (OPEN ELF-PARSER::IN 13112)
                                        ; 
                                        ; caught WARNING:
                                        ;   The function has an odd number of arguments in the keyword portion.

                                        ; in: WITH-OPEN-FILE (FILE-POSITION IN 13112)
                                        ;     (COM.GIGAMONKEYS.BINARY-DATA:READ-VALUE 'ELF-PARSER::ADDRESS-RANGE
                                        ;                                             ELF-PARSER::IN)
                                        ; 
                                        ; caught WARNING:
                                        ;   undefined variable: IN
                                        ; 
                                        ; compilation unit finished
                                        ;   Undefined variable:
                                        ;     IN
                                        ;   caught 2 WARNING conditions
                                        ; Evaluation aborted on #<UNBOUND-VARIABLE IN {10059409F3}>.
ELF-PARSER> (defun get-address-range-table (elf  file)
              (with-open-file (in file :element-type '(unsigned-byte 8))
                (let* ((sh (elf:sh (elf:named-section elf ".debug_aranges")))
                       (ofs (elf:offset sh))
                       (end (+ ofs (elf:size sh))))
                  (file-position in ofs)
                  (loop for addr-range = (read-value 'address-range in )
                     unless (> (file-position in) end ) collect addr-range))))
GET-ADDRESS-RANGE-TABLE
ELF-PARSER> (get-address-range-table *elf* *file*)
                                        ; Evaluation aborted on NIL.
ELF-PARSER> (defun get-address-range-table (elf  file)
              (with-open-file (in file :element-type '(unsigned-byte 8))
                (let* ((sh (elf:sh (elf:named-section elf ".debug_aranges")))
                       (ofs (elf:offset sh))
                       (end (+ ofs (elf:size sh))))
                  (format t "~d~d~%" ofs end)
                  (file-position in ofs)
                  (loop for addr-range = (read-value 'address-range in )
                     unless (> (file-position in) end ) collect addr-range))))
