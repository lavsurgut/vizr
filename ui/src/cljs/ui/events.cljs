(ns ui.events
  (:require
   [re-frame.core :as re-frame]
   [ui.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced defn-traced]]
   [ajax.core :refer [GET]]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-active-panel
 (fn-traced [db [_ active-panel]]
   (assoc db :active-panel active-panel)))


(re-frame/reg-event-db ;; <-- register an event handler
  ::request-data      ;; <-- the event id
  (fn                ;; <-- the handler function
    [db _]

    ;; kick off the GET, making sure to supply a callback for success and failure
    (GET
      "http://localhost:3000/queries"
      {:handler       #(re-frame/dispatch [::process-response %1])   ;; <2> further dispatch !!
       ;:error-handler #(re-frame/dispatch [::bad-response %1])
       })     ;; <2> further dispatch !!

    ;; update a flag in `app-db` ... presumably to cause a "Loading..." UI
    (assoc db :loading? true)))    ;; <3> return an updated db


(re-frame/reg-event-db
  ::process-response
  (fn
    [db [_ response]]           ;; destructure the response from the event vector
    (-> db
        (assoc :loading? false) ;; take away that "Loading ..." UI
        (assoc :specs (js->clj response)))))
