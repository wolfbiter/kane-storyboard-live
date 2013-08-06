(ns beatfn-live.actionDB
  (:use
    [monger.core :only [connect! set-db! get-db]]
    [monger.collection
      :only [update insert-batch find-maps remove insert]]))

;
; mongodb stuff
;
; TODO: make indexes for scheduled actions?
; TODO: update to latest version of mongoDB
; TODO: look for a database more suited for this use-case?

(def DB_NAME "storyboard-actions") ; document name to be used with monger

(connect!)
(set-db! (get-db DB_NAME))

(defn find-actions
  [& args]
  (apply find-maps (cons DB_NAME args)))

(defn update-action
  [& args]
  (apply update (cons DB_NAME args)))

(defn remove-actions
  [& args]
  (apply remove (cons DB_NAME args)))
  
; TODO: turn this into a safe insert, so i quit getting this serial error?
(defn insert-action
  [& args]
  (do (println "hi! inserting action: " args)
  (apply insert (cons DB_NAME args))))

(defn insert-actions
  [& args]
  (apply insert-batch (cons DB_NAME args)))