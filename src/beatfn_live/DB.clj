(ns beatfn-live.DB
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









(comment


 (ns beatfn-live.DB
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

(def DB_NAME "storyboard") ; document name to be used with monger

(connect!)
(set-db! (get-db DB_NAME))

(defn find-maps
  [& args]
  (apply _find-maps (cons DB_NAME args)))

(defn update
  [& args]
  (apply _update (cons DB_NAME args)))

(defn remove
  [& args]
  (apply _remove (cons DB_NAME args)))
  
; TODO: turn this into a safe insert, so i quit getting this serial error?
(defn insert
  [& args]
  (apply _insert (cons DB_NAME args)))

(defn insert-batch
  [& args]
  (apply _insert-batch (cons DB_NAME args)))

;
; actions
;

(defn find-actions [args]
  (find-maps (assoc args :datatype :action)))

(defn update-action [args]
  (update (assoc args :datatype :action)))

(defn remove-actions [args]
  (remove (assoc args :datatype :action)))
  
(defn insert-action [args]
  (do (println "hi! inserting action: " args)
  (insert (assoc args :datatype :action))))

(defn insert-actions [args]
  (insert-batch (assoc args :datatype :action)))

;
; scenes
;

; 1. write make-scene
; 2. new scenes are made when an action is placed in that scene and that scene 
;    doesn't already exist. [scene-state step-size]
; 3. step size up and down buttons need to change the current scene's step size.
; 4. the tracker must send events for each scene's current step size
; 5. action handles must somehow be linked to scenes so that 4 is possible.

;(defn make-scene
;  [])

(defn find-scenes [args]
  (find-maps (assoc args :datatype :scene)))

(defn update-scene [args]
  (update (assoc args :datatype :scene)))

(defn remove-scenes [args]
  (remove (assoc args :datatype :scene)))
  
(defn insert-scene [args]
  (do (println "hi! inserting scene: " args)
  (insert (assoc args :datatype :scene))))

(defn insert-scenes [args]
  (insert-batch (assoc args :datatype :scene)))

)