;;; -*- lexical-binding: t -*-

(require 'ert-async)
(require 'promise)
(require 'f)

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(require 'js99-mode (f-expand "js99-mode.el" root-code-path))
