
(asdf/parse-defsystem:defsystem #:arele
  :description
  "Ebay listing control system"
  :author
  "fskolodny@gmail.com"
  :license
  "BSD"
  :version
  "0.0.0"
  :serial
  t
  :entry-point
  "arele:start-app"
  :depends-on
  (#:clog #:mito)
  :components
  ((:file "arele")
   ))
(asdf/parse-defsystem:defsystem #:arele/tools
  :defsystem-depends-on
  (:clog)
  :depends-on
  (#:arele #:clog/tools)
  :components
  (
   ))
