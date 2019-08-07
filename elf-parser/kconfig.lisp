(in-package :elf-parser)

(cl-lex:define-string-lexer kconfig-lexer
  ("\\s*menu\\s+\"([^\"]+)\"\\s*\\n"
   (format t "~&menu ~a" $1)
   (return (values 'menu (cons 'menu $1) )))
  ("\\s*menu\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&menu ~a" $1)
   (return (values 'menu (cons 'menu $1) )))
  ("\\s*endmenu\\s*\\n|\\s*endmenu\\s*#[!&| /A-Za-z0-9_]+\\s*\\n"
   (format t "~&endmenu")
   (return (values 'endmenu (cons 'endmenu nil) )))
  ("\\s*if\\s+([&!| /A-Z0-9_]+)\\s*\\n"
   (format t "~&if ~a" $1)
   (return (values 'if (cons 'if $1) )))
  ("\\s*endif\\s*\\n|endif\\s+#[!&| /A-Za-z0-9_]+\\s*\\n"
   (format t "~&endif ~a" $@)
   (return (values 'endif (cons 'endif nil))))

  ("\\s*choice\\s*\\n"
   (format t "~&choice: " )
   (return (values 'choice (cons 'choice nil))))
  ("\\s*choice\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&choice: ~a " $1 )
   (return (values 'choice (cons 'choice $1))))

  ("\\s*endchoice\\s*\\n"
   (format t "~&endchoice ")
   (return (values 'endchoice (cons 'endchoice nil))))
  ("\\t*source\\s+\"([^\"]+)\"\\s*\\n"
   (format t "~&source ~a" $1)
                                        ;(return (values 'source (cons 'source $1)))
   )
  ("\\s*config\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&config ~a" $1)
   (return (values 'config (cons 'config $1))))
  ("\\s*menuconfig\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&menuconfig ~a" $1)
   (return (values 'menuconfig (cons 'menuconfig $1))))
  ("\\s*default\\s+([-\"/A-Za-z0-9_]+)\\s*\\n"
   (format t "~&default ~a" $1)
   (return (values 'default (cons 'default $1))))

  ("\\s*def_bool\\s+([yn])\\s*\\n"
   (format t "~&def_bool ~a" $1)
   (return (values 'def_bool (cons 'def_bool $1))))
  ("\\s*def_bool\\s+([yn])\\s+if\\s+([ !&|A-Za-z0-9_]+)\\s*\\n"
   (format t "~&def_bool ~a" $1)
   (return (values 'def_bool (cons 'def_bool (list $1 $2)))))

  ("\\s*default\\s+([-\"/A-Za-z0-9_]+)\\s+if\\s+([ &|!A-Za-z0-9_]+)\\s*\\n"
   (format t "~&default ~a if ~a" $1 $2)
   (return (values 'default (cons 'default (list $1 $2)))))
  ("\\s*select\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&select ~a" $1)
   (return (values 'select (cons 'select $1))))
  ("\\s*depends on\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&depends ~a" $1)
   (return (values 'depends (cons 'depends $1))))
  ("\\s*depends on\\s+([&!\| /A-Za-z0-9_]+)\\s*\\n"
   (format t "~&depends ~a" $1)
   (return (values 'depends (cons 'depends $1))))
  ("\\s*(bool|string|hex|int)\\s*\\n"
   (format t "~&type ~a" $1 )
   (return (values 'type (cons $1 nil) )))
  ("\\s*(bool|string|hex|int)\\s+\"([^\"]+)\"\\s*\\n"
   (format t "~&type ~a" $1)
   (return (values 'type (cons $1 $2) )))
  ("\\s*(bool|string|hex|int)\\s+\"([^\"]+)\"\\s+if\\s+([A-Za-z0-9_]+)\\s*\\n"
   (format t "~&type ~a" $1)
   (return (values 'type (cons $1 (list $2 $3)))))
  ("\\s*(bool|string|int)\\s*\\n"
   (format t "~&type ~a" $1)
   (return (values 'type (cons $1 nil))))
  ("\\s*option\\s+(\\S+)\\s*\\n"
   (format t "~&option ~a" $1)
   (return (values 'option (cons 'option $1))))

  ("\\s*range\\s+([^#\\n]+)\\s*\\n"
   (format t "~&range ~a" $1))
  ("\\s*prompt\\s+\"([^\"]+)\"\\s*\\n"
   (format t "~&prompt ~a" $1)
   ;;(return (values 'prompt (cons 'prompt $1) ))
   )
  ("\\s+help\\s*\\n"
   (format t "~&help " )
   ;;(return (values 'help (cons 'help nil)))
   )

  ("#[^\\n]*\\n" ;; (format t "~&comment line ~a" $@)
   )
  ("\\t  [^\\n]+\\n" ;;(format t "~&help line ~a" $@)
   )
  ("\\s*\\n" ;;(format t "~&blank line:~a" $@)
   )
  ("\\s+\\s*[^#\\n]+\\n"
   (format t "~&help-txt: ~a" $@)
   ;;(return (values 'help-txt (cons 'help-txt nil)))
   )
  ("[^\\n]+\\n"
   (format t "~&------------------------------------
unknown  line:~a
---------------------------------" $@)))


(yacc:define-parser *kconfig-parser*
  (:muffle-conflicts t)
  (:start-symbol kconfig)
  (:terminals (menuconfig def_bool if endif menu endmenu
                          choice endchoice source config default
                          select depends help help-txt type prompt option))

  (kconfig
   (menu-prefix-expr kconfig endmenu kconfig)
   (expr kconfig)
   (choice  endchoice kconfig)
   (choice  choice-expr endchoice kconfig)
   (if expr endif kconfig)
   (if kconfig  endif kconfig)
   nil)

  (choice-expr
   expr
   type-sub
   (type-sub   expr))

  (menu-prefix-expr
   menu
   (menu select-sub)
   (menu depends-sub))

  (expr
   config-expr
   menuconfig-expr
   (config-expr expr)
   (menuconfig-expr expr))

  (config-expr
   (config type-sub)
   (config type-sub type)
   (config type-sub type type-sub)
   (config type)
   (config type type-sub))

  (menuconfig-expr
   (menuconfig type-sub)
   (menuconfig type-sub type)
   (menuconfig type-sub type type-sub)
   (menuconfig type)
   (menuconfig type type-sub))

  (type-sub
   option
   depends-sub
   select-sub
   default-sub
   (depends-sub  type-sub)
   (select-sub  type-sub)
   (default-sub type-sub))

  (default-sub
      default
      def_bool
    (default default-sub))

  (depends-sub
   depends
   (depends depends-sub))

  (select-sub
   select
   (select select-sub)))


(defun show-kconfig ()
  (let ((f (read-file-into-string "/Users/zhangdongfeng/Downloads/ATS350B_1/kconfig.txt" )))
    (dolist (l (uiop:split-string f :separator '(#\newline)))
      (format t  "~&~a" l)
      (if (search "config" (file-namestring l))
          (yacc:parse-with-lexer
           (kconfig-lexer (uiop:read-file-string l)) *kconfig-parser* )
          (format t  "notkconfig file:~&~a" l))
      )))
