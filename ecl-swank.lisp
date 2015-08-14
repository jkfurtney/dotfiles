(load (sys:getenv "BLOUP_SWANK"))
(swank-loader:init)

;; this part is inside Blo-Up now see
;; c:/src/svn_bu/bloupgui/src/modelmanager.cpp
;; and c:/src/svn_bu/interpreter/src/wrapper.h
;; (let*
;; ((*standard-output* (make-broadcast-stream)) (*trace-output*
;; *standard-output*) (*error-output* *standard-output*) (*debug-io*
;; *standard-output*) (*standard-input* (make-string-input-stream
;; ""))) (swank:create-server :port 4005))
