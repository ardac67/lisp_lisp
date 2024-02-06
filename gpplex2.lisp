;; Defining keyword hashtable
(defvar keywords1
    '(
        ;; keyword
        ("and" "KW_AND")
        ("or" "KW_OR")
        ("not" "KW_NOT")
        ("equal" "KW_EQUAL")
        ("less" "KW_LESS")
        ("nil" "KW_NIL")
        ("list" "KW_LIST")
        ("append" "KW_APPEND")
        ("concat" "KW_CONCAT")
        ("set" "KW_SET")
        ("for" "KW_FOR")
        ("if" "KW_IF")
        ("exit" "KW_EXIT")
        ("load" "KW_LOAD")
        ("disp" "KW_DISPLAY")
        ("true" "KW_TRUE")
        ("false" "KW_FALSE")
        ("def" "KW_DEF")
    )
)
(defvar *value-table* (make-hash-table :test #'equal))

;; initialize the hashtable
(defvar keyword-hash (make-hash-table :test 'equal))
(defvar value_table)
(defvar value-hash (make-hash-table :test 'equal))
(dolist (pair keywords1)
(setf (gethash (car pair) keyword-hash) (cadr pair)))

;;constats table
(defconstant keywords
    '(
        ;; keyword
        "and"
        "or"
        "not"
        "equal"
        "less"
        "nil"
        "list"
        "append"
        "concat"
        "set"
        "for"
        "if"
        "exit"
        "load"
        "disp"
        "true"
        "false"
        "def"
    )
)
;;constats hash table
(defvar operatorhash
    '(
        (#\+ "OP_PLUS")
        (#\- "OP_MINUS")
        (#\/ "OP_DIV")
        (#\* "OP_MULT")
        (#\(  "OP_OP")
        (#\)  "OP_CP")
        (#\,  "OP_COMMA")
    )
)
;;operator hash table
(defconstant operator
    '(
        "+"
        "-"
        "/"
        "*"
        "("
        ")"
        ","
    )
)
;;initiailizing hash-maps
(defvar operator-hash (make-hash-table :test 'equal))
(dolist (pair operatorhash)
(setf (gethash (car pair) operator-hash) (cadr pair)))


;;global variables
(defvar is_comment 0)
(defvar token_list nil)
(defvar str "")
(defvar inputFile nil)
(defvar value_list nil)
(defvar is_ok 0)
(defvar key_list nil)
;;main function to run the interpreter that takes in the input file or nothing if input file is nill then takes in the input from the user
(defun g++interpreter ()

 (format t "Enter the file name: ")
  (let ((user-input (read-line)))
    (if (string= user-input "")
      (setq inputFile nil)
      (setq inputFile user-input)))

  (if (eq inputFile nil)
     (loop
        (format t ">: ~%")
        (let ((input (read-line)))
          
          (if (string= input "quit")
              (return)
             (progn (doWork input)
                (
                  (lambda ()
                  ;(dolist (key key_list)
                    ;(format t "~%~a" key))
                    (if (member "KW_SET" key_list :test 'string-equal)
                    ;; Code to execute if KW_SET is found in key_list
                    (progn
                      (process-set-sequence key_list)
                    )
                    ;; Code to execute if KW_SET is not found in key_list
                    (progn
                      (evaluate-expression key_list)
                    ))

                   
                  ))
                  (setf key_list nil)
             ) ;; Print the content after the loop is finished
          )
        )
      )
      (progn 
       (with-open-file (stream inputFile)
          (loop :for line = (read-line stream nil nil) :while line :do
            (dowork line)
                  ;(dolist (key key_list)
                    ;(format t "~%~a" key))
                    (if (member "KW_SET" key_list :test 'string-equal)
                    ;; Code to execute if KW_SET is found in key_list
                    (progn
                      (process-set-sequence key_list)
                    )
                    ;; Code to execute if KW_SET is not found in key_list
                    (progn
                      (evaluate-expression key_list)
                    ))
                    (setf key_list nil)
          )
        ) ; Replace 'process-line' with your line processing function
           

      )
  )
)
(defvar result)
(defvar next-item)
(defvar next-next-item)

(defun make_unified (currChar)
  (if (or (char= currChar #\Tab) (char= currChar #\newline))
      (progn
        (doWork str) ;;unifiy the chars
        (setq str ""))
      (setq str (concatenate 'string str (string currChar)))
  )
)
(defun doWork (input)
    (setq input (concatenate 'string input (string #\Space)))
    (let ((*word* nil))
      (loop for i from 0 below (length input)
          do
            (if (string-equal (char input i) ";")
                (progn 
                  (if (< (+ 1 i) (length input))
                   (progn
                    (if (string-equal (char input (+ i 1) ) ";") ;; check the content is comment or not
                      (progn
                        (setf *word* nil)
                        (setf is_comment 1)
                        (loop for j from i below (length input)
                          do (setq *word* (append *word* (list (char input j))))
                        )
                        (check_token *word*)
                        (return)
                      )
                    )
                   )
                  )  
                )
            )
            (if (and (eq is_comment 0) (not (char= (char input i) #\Space)));;seperating with space
                (progn
                    (setf is_comment 0)
                    (setq *word* (append *word* (list (char input i))))
                )
                (progn
                  (if  (or (char= (char input i) #\Space)  (char= (char input i) #\newline)) ;;check if the char is empty or newLine
                    (when *word* ; Check if *word* is not empty
                        (check_token *word*) ;; check the token
                        (validation token_list);; check the validation
                        (setq token_list nil);; clean content
                        (setq *word* nil) ;; clean content of word
                    )
                  )
                )
            )
       )
    )

)

(defvar counter 0)
(defun check_token (list)
  (cond
    ((null list) 0)
    (t
      (let ((word (coerce list 'string)))
        (if (eq is_comment 1) ;;check if it is comment or not
            (progn 
              (format t "COMMENT: ~a~%" word)
              (setf is_comment 0)
            )
            (progn
                (iterate-list list (length list))
                (setf counter 0)
            )
        )
      )
    )
     
  )
)


(defvar is_first 0)
(defvar is_last 0)

(defun iterate-list (lst x) ;;iterate token list and fill it with find_token function
  (if (null lst)
      nil
      (progn
        (find_token (car lst) )
        (iterate-list (cdr lst) x)
      ) 
  )
) 


(defun find_token (chars) ;; filling with token list with the supplied chars
  (cond
    ((null chars) 0)
    (t
      (let ((word chars))
        (fill_token word)
      )
    )
  )
)

(defvar word1 nil)
(defvar word2 nil)
(defvar upper_valid 0)
(defvar downto_valid 0)

(defun fill_token (chars)
  (cond
    ((null chars) 0)
    (t
      (let ((word chars))
        (setf token_list (append token_list (list word)))
      )
    )
  )
)

(defvar is_special 0)
(defvar special_word nil)
(defun validation (list) ;; check the token is valid if it is valid which type of token is it
  (cond
    ((null list) 0)
    (t
      (let ((word (coerce list 'string)))
        (if (member word keywords :test #'string=) ;;if it is member of keyword list than it is keyword
            (progn 
              ;;(format t "Keyword: ~a~%" (gethash word keyword-hash))
              (setf key_list (append key_list (list (gethash word keyword-hash))))  
            )
            (progn 
              (if (not (eq (check_literal word) 0));; if it fits in literal then it is literal
                (format t "VALUEI: ~a~%" word)
                (progn
                  (if(not (eq (check_built_in word) 0));; if it is special type then it is built in
                    (progn 
                      ;(format t "VALUE_EF: ~a~%" word)
                      (setf (gethash word *value-table*) word)
                      (setq key_list (append key_list (list word)))

                      ;;set necessary data to clean for further use
                      (setf word1 nil)
                      (setf word2 nil)
                      (setf upper_valid 0)
                      (setf downto_valid 0)
                    )
                    (progn 
                      (if (not (eq (check_identifier word) 0)) ;;check the char list is obey the identifier rules
                        ;(progn (format t "Identifier: ~a~%" word)
                        (setf key_list (append key_list (list word))
                        )
                        (progn
                          (if (not (eq (iterate_speciality list) 0)) ;; if it is combined chars check with the char by char
                            (progn 
                              (parse_speciality list)
                            )
                            (progn
                              (if (has-special-characters-p word) ;;checking the unnecessary chars in the word for the not valid error results
                                  ()
                                  (progn 
                                      (format t "ERROR: ~a~%" word)
                                      (return-from validation 0)
                                  )
                              )
                            )
                          )
                        )
                        )
                    )
                    )
                  )
                )
              )
            )
        )
      )
    )
  (setf word1 nil)
  (setf word2 nil)
  (setf upper_valid 0)
  (setf downto_valid 0)
  )
(defun check_operators (str) ;; checking operator existence
  (cond
    ((null str) 0)
    (t
      (let ((word str))
       (if (member word operator :test #'string=) ;; if it is member of operator list then it is operator
           (return-from check_operators 1)
           (return-from check_operators 0)
       )
     )
    )
  )
)

(defun check_identifier (str) ;; checking identifier exintance
  (cond
    ((null str) 0)
      (t
        (let ((word str))
                (if (valid-identifier-p word);; if it is obey the identifier rules then it is identifier
                    1
                    0
                )
        )
      )
  )
)

(defun check_built_in (str);; checking built in existence
   (cond
    ((null str) 0)
      (t
        (let ((word str))
                (if (valid-built_in-p word);; if it is obey the built in rules then it is built in
                    1
                    0
                )
        )
      )
  )
)

(defun check_literal (str);; checking literal existence
  (cond
    ((null str) 0)
      (t
        (let ((word str))
          (if (valid-literal-p word);; if it is obey the literal rules then it is literal
                1
                0
          )
        )
      )
  )
)

(defun valid-identifier-p (str);; checking identifier rules
  (and (> (length str) 0)
       (alpha-char-p (char str 0)) ; Check if the first character is alphabetical
       (every #'alphanumericp str))) ; Check if all characters are alphanumeric

(defvar b-index -1)
(defvar hasB 0)
(defun valid-built_in-p (str);; checking built in rules for VALUE_EF
  (setf word1 nil)
  (setf word2 nil)
  (setf upper_valid 0)
  (setf downto_valid 0)
  (setf b-index -1)
  (loop for j from 0 below (length str);;parsing it with b char if it is exist then lookup and lookdown
      do (if (char= (char str j) #\b)
             (setf b-index j)
             (if (< b-index 0)
                 (setf word1 (append word1 (list (char str j))))
                 (setf word2 (append word2 (list (char str j))))
             )
         )
         
         )
      (if word1
      (if (valid-literal-p word1);; checking the rules for the first part of the word
          (setf upper_valid 1)))
      (if word2
      (if (valid-literal-p word2);; checking the rules for the second part of the word
          (setf downto_valid 1)))
  (return-from valid-built_in-p (and (eq upper_valid 1) (eq downto_valid 1)));; if both of them are valid then it is built in
)

(defun valid-literal-p (str);; checking literal rules
  (cond
    ((null str) 0)
      (t
        (every #'digit-char-p str);; checking if it is digit or not
      )
  )
  
)



(defvar is_op 0)
(defvar is_word 0)

(defun check_speciality (chars);;checks the mixing of operator inside of it
 (cond
    ((null chars) 0)
    (t
      (let ((word chars))
        (if (eq (check_operators word) 1);;are there any operator if yes then it is special
          (progn 
            (return-from check_speciality 1)
            (return-from check_speciality 0)
          )
        )
      )
    )
  )
)

(defun iterate_speciality (list) ;; iterating special list
  (cond
    ((null list) 0)
    (t
        (if (eq (check_speciality (car list)) 1)
          (progn 
            (setf is_special 1)
            (return-from iterate_speciality 1)
          )
          (progn
            (setf is_special 0)
            (iterate_speciality (cdr list))
          )
        )
    )
  )
)
;;(defvar is_ok 0)
;,(defvar key_list)
(defun parse_speciality (list)
  (cond
    ((null list) 0)
    (t
      (loop for item in list
        do(if (eq (check_speciality item) 1) 
              (progn 

                 (cond
                  ((null special_word) 0)
                  (t  
                    (validation special_word);;if not have operator than check it's type
                    (setf special_word nil)
                  )  
                ) 
                ;;(format t "Operator: ~a~%" (gethash item operator-hash));;if it is an operator then print it
                (setf is_ok 1)
                (setf key_list (append key_list (list (gethash item operator-hash))))
              )
              (progn
                (setf special_word (append special_word (list item)));;put the chars into list to check it's type
              )
          )
      )
      (cond
        ((null special_word) 0)
        (t  
          (validation special_word);;if it is not operator then check it's type
          (setf special_word nil)
        )  
      ) 
    )
  )
)


(defun has-special-characters-p (str);;for removing unnecessary chars
  (loop for char across str
        when (member char '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed))
        do (return t)
        finally (return nil)))
        

(defun sum-special-strings (str1 str2)
  (let ((num1 (parse-integer str1 :junk-allowed t))
        (num2 (parse-integer str2 :junk-allowed t))
        (suffix1 (subseq str1 (position #\b str1)))
        (suffix2 (subseq str2 (position #\b str2))))
    (unless (string= suffix1 suffix2)
    (error "Suffixes do not match."))
    (format nil "~a~a" (+ num1 num2) suffix1)))

(defun sub-special-strings (str1 str2)
  (let ((num1 (parse-integer str1 :junk-allowed t))
        (num2 (parse-integer str2 :junk-allowed t))
        (suffix1 (subseq str1 (position #\b str1)))
        (suffix2 (subseq str2 (position #\b str2))))
    (unless (string= suffix1 suffix2)
    (error "Suffixes do not match."))
    (format nil "~a~a" (- num1 num2) suffix1)))
(defun multip-special-strings (str1 str2)
  (let ((num1 (parse-integer str1 :junk-allowed t))
        (num2 (parse-integer str2 :junk-allowed t))
        (suffix1 (subseq str1 (position #\b str1)))
        (suffix2 (subseq str2 (position #\b str2))))
    (unless (string= suffix1 suffix2)
    (error "Suffixes do not match."))
    (format nil "~a~a" (* num1 num2) suffix1)))
(defun divide-special-strings (str1 str2)
  (let ((num1 (parse-integer str1 :junk-allowed t))
        (num2 (parse-integer str2 :junk-allowed t))
        (suffix1 (subseq str1 (position #\b str1)))
        (suffix2 (subseq str2 (position #\b str2))))
    (unless (string= suffix1 suffix2)
    (error "Suffixes do not match."))
    (format nil "~a~a" (/ num1 num2) suffix1)))


(defun evaluate-expression (expr)
  (cond
    ((stringp expr) expr)  ;; If the expression is a string, return it as is.
    ((listp expr)          ;; If the expression is a list, process it.
     (evaluate-list expr))
    (t (error "Invalid expression"))))  ;; Error for invalid expressions

(defun evaluate-expression (expr)
  (handler-case
      (cond
        ((stringp expr) 
         ;; Check if the string is an identifier in the hash table
         (let ((value (gethash expr value-hash)))
           (if value
               value  ;; If it's in the hash table, use the value
               expr)))  ;; Otherwise, treat it as a normal string
        ((listp expr)
         (let ((stack (list nil)))  ; Stack to handle sub-expressions
           (dolist (item expr (evaluate-sub-expression (first stack)))
             (cond
               ((string= item "OP_OP")  ; Start of a new sub-expression
                (push nil stack))
               ((string= item "OP_CP")  ; End of a sub-expression
                (let ((sub-expr (pop stack)))
                  (when stack
                    (setf (first stack) (append (first stack) (list (evaluate-sub-expression sub-expr)))))))
               (t  ; Add item to the current sub-expression
                (setf (first stack) (append (first stack) (list item))))))))
        (t (error "Syntax error")))
    (error () -1)))  ; Return 0 in case of any error


(defun evaluate-sub-expression (expr)
  (when expr
    (let ((op (first expr))
          (args (rest expr)))
      (cond
        ((string= op "OP_PLUS")
         (reduce #'sum-special-strings (mapcar #'evaluate-expression args)))
        ((string= op "OP_MINUS")
         (reduce #'sub-special-strings (mapcar #'evaluate-expression args)))
        ((string= op "OP_MULT")
         (reduce #'multip-special-strings (mapcar #'evaluate-expression args)))
        ((string= op "OP_DIV")
         (reduce #'divide-special-strings (mapcar #'evaluate-expression args)))
        (t (print  op))))
  )
)

(defun process-set-sequence (sequence) 
  (when (and (>= (length sequence) 4) 
             (string= (second sequence) "KW_SET"))
    (let ((identifier (third sequence))
          (value-ef (nth 3 sequence))
          (value-ef-expr (and (>= (length sequence) 5) 
                             (subseq sequence 3 (- (length sequence) 1)))))
      (if (and value-ef-expr
               (string= (first value-ef-expr) "OP_OP")
               (string= (car (last value-ef-expr)) "OP_CP"))
          (let ((value (evaluate-expression value-ef-expr)))
            (setf (gethash identifier value-hash) value))
          (setf (gethash identifier value-hash) value-ef)))))  


(g++interpreter)
