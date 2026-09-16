(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)

(defvar *gspf-pass* 0)
(defvar *gspf-fail* 0)
(defun gspf-check (name condition)
  (if condition
      (progn (incf *gspf-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *gspf-fail*) (format t "  FAIL ~a~%" name))))

(defparameter *gspf-fixture-path*
  (namestring (merge-pathnames "evals/fixtures/v1/grounded-story-project.json" *pai-root*)))
(defparameter *gspf-result-path*
  (namestring (merge-pathnames "evals/results/grounded-story-project-v1.sanitized.json" *pai-root*)))

(defun gspf-json (path)
  (shasht:read-json (uiop:read-file-string path)))

(defun gspf-sha256 (path)
  (with-open-file (stream path :direction :input
                               :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (string-downcase
       (ironclad:byte-array-to-hex-string
        (ironclad:digest-sequence :sha256 bytes))))))

(let* ((fixture (gspf-json *gspf-fixture-path*))
       (result (gspf-json *gspf-result-path*))
       (cases (coerce (gethash "cases" fixture) 'list))
       (rows (coerce (gethash "case_results" result) 'list))
       (case-ids (mapcar (lambda (case) (gethash "id" case)) cases))
       (row-ids (mapcar (lambda (row) (gethash "id" row)) rows))
       (categories (remove-duplicates
                    (mapcar (lambda (case) (gethash "category" case)) cases)
                    :test #'string=))
       (metrics (gethash "metrics" result)))
  (format t "~%== sanitized grounded story fixture ==~%")
  (gspf-check "fixture and result versions are pinned"
              (and (string= "grounded-story-project-1.0.0"
                            (gethash "fixture_version" fixture))
                   (string= (gethash "fixture_version" fixture)
                            (gethash "fixture_version" result))))
  (gspf-check "result binds the exact immutable fixture hash"
              (string= (gspf-sha256 *gspf-fixture-path*)
                       (gethash "fixture_sha256" result)))
  (gspf-check "fixture is explicitly synthetic and non-private"
              (and (string= "synthetic-non-private" (gethash "privacy" fixture))
                   (gethash "sanitized" result)))
  (gspf-check "all required failure and fallback categories are present"
              (every (lambda (name) (member name categories :test #'string=))
                     '("operation" "generation-failure" "validation-failure"
                       "commit-failure" "novelty" "appraisal" "composition"
                       "authority" "fallback" "delivery")))
  (gspf-check "every fixture case has exactly one correlated result"
              (and (= (length case-ids) (length row-ids))
                   (= (length row-ids)
                      (length (remove-duplicates row-ids :test #'string=)))
                   (every (lambda (id) (member id row-ids :test #'string=))
                          case-ids)))
  (gspf-check "every declared case result matches its expected outcome/code"
              (every
               (lambda (case)
                 (let ((row (find (gethash "id" case) rows
                                  :key (lambda (item) (gethash "id" item))
                                  :test #'string=)))
                   (and row (gethash "correct" row)
                        (string= (gethash "expected" case)
                                 (gethash "actual" row))
                        (equal (gethash "expected_code" case)
                               (gethash "code" row)))))
               cases))
  (gspf-check "all required result metrics are materialized"
              (every (lambda (name) (nth-value 1 (gethash name metrics)))
                     (coerce (gethash "required_metrics" fixture) 'list)))
  (gspf-check "operation metrics cover the exact five-stage sequence"
              (equal '("outline" "draft" "revise" "validate" "complete")
                     (mapcar (lambda (row) (gethash "operation" row))
                             (coerce (gethash "operation_metrics" result) 'list))))
  (gspf-check "synthetic evaluation records zero cost/network/delivery"
              (let ((execution (gethash "execution" result)))
                (and (zerop (gethash "cost" metrics))
                     (zerop (gethash "delivery_attempts" metrics))
                     (zerop (gethash "external_network_calls" execution))
                     (zerop (gethash "openrouter_calls" execution)))))
  (gspf-check "classifier metrics remain outside the first fixture"
              (and (not (gethash "classifier_metrics_in_scope" fixture))
                   (eq :null (gethash "classifier_metrics" result)))))

(format t "~%~a passed, ~a failed~%" *gspf-pass* *gspf-fail*)
(when (plusp *gspf-fail*) (sb-ext:exit :code 1))
