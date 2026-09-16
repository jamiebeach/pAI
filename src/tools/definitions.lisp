

;; === skill: BRAVE-SEARCH ===

;; ASDF compiles each component with CL-USER as the initial reader package.
;; This file historically relied on its caller already being in AGENT, which
;; made BRAVE-SEARCH and its credential lookup land in CL-USER in production
;; even though source-loading fixtures happened to place them in AGENT.
(in-package :agent)

(defun BRAVE-SEARCH (QUERY &KEY (COUNT 10))
  "Query the Brave Web Search API for QUERY and return a formatted
list of the top COUNT results (title, url, description)."
  (BLOCK BRAVE-SEARCH
    (UNLESS (STRINGP QUERY) (SETF QUERY (FORMAT NIL "~a" (OR QUERY ""))))
    (UNLESS (AND (INTEGERP COUNT) (PLUSP COUNT)) (SETF COUNT 10))
    (FLET ((URL-ENCODE (S)
             (WITH-OUTPUT-TO-STRING (OUT)
               (LOOP FOR C ACROSS S
                     DO (COND ((ALPHANUMERICP C) (WRITE-CHAR C OUT))
                              ((FIND C ".-_~") (WRITE-CHAR C OUT))
                              (T (FORMAT OUT "%~2,'0X" (CHAR-CODE C))))))))
      (HANDLER-CASE
       (MULTIPLE-VALUE-BIND (BODY STATUS)
           (DEXADOR:GET
            (FORMAT NIL "https://api.search.brave.com/res/v1/web/search?q=~a"
                    (URL-ENCODE QUERY))
            :HEADERS
            `(("Accept" . "application/json")
              ("X-Subscription-Token"
                . ,(OR (BRAVE-API-KEY)
                       (ERROR "Brave API credential is not configured")))))
         (IF (= STATUS 200)
             (LET* ((DATA (SHASHT:READ-JSON BODY))
                    (RESULTS (GETHASH "results" (GETHASH "web" DATA))))
               (IF (OR (NULL RESULTS)
                       (NOT
                        (AND (TYPEP RESULTS 'SEQUENCE)
                             (PLUSP (LENGTH RESULTS)))))
                   "No results found."
                   (WITH-OUTPUT-TO-STRING (S)
                     (LOOP FOR I FROM 0 BELOW (MIN COUNT (LENGTH RESULTS))
                           FOR R = (AREF RESULTS I)
                           DO (FORMAT S "~a. ~a~%~a~%~a~%~%" (1+ I)
                                      (GETHASH "title" R "")
                                      (GETHASH "url" R "")
                                      (GETHASH "description" R ""))))))
             (FORMAT NIL "ERROR: Brave Search returned HTTP ~a." STATUS)))
       (ERROR (E) (FORMAT NIL "ERROR: Brave Search request failed: ~a" E)))))
)
