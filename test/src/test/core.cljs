(ns test.core
  (:require [reagent.core :as r]
            [taoensso.timbre :as timbre
             :refer-macros [log  trace  debug  info  warn  error  fatal  report
                            logf tracef debugf infof warnf errorf fatalf reportf
                            spy get-env]]
            [datomish.api :as d]
            [honeysql.format :as sql]
            [datomish.db :as db]
            [datomish.db-factory :as db-factory]
            ;[datomish.js-sqlite :as js-sqlite]
            [datomish.sqlite :as sqlite]
            [datomish.transact :as transact]
            [clojure.test :as t :refer [is are deftest testing]]
            [datomish.pair-chan :refer [go-pair <?]]
            [datomish.db.debug :refer [<datoms-after <datoms>= <transactions-after <shallow-entity <fulltext-values]]
            [cljs.core.async :as a :refer [<! >!]]))

(enable-console-print!)

(def test-schema
  [{:db/id        (d/id-literal :db.part/user)
    :db/ident     :name
    :db/unique    :db.unique/identity
    :db/valueType :db.type/string
    :db.install/_attribute :db.part/db}])

(def dbfile "./tmp.test.db")

(def conn (datomish.pair-chan/<? (d/<connect dbfile)))

(defn add-to-db [text]
  (let [{tx0 :tx} (<? (d/<transact! conn test-schema))]
    (let [{:keys [tx txInstant]} (<? (d/<transact! conn [[:db/add 0 :name text]]))]
      (is (= (<? (<datoms-after (d/db conn) tx0))
             #{[0 :name text]}))
      (is (= (<? (<transactions-after (d/db conn) tx0))
             [[0 :name text tx 1] ;; TODO: true, not 1.
              [tx :db/txInstant txInstant tx 1]])))))

(defn testdb []
  ;(add-to-db ["123"])
  ;(let [c (<? (d/<transact! conn test-schema))])
  (timbre/info "check timbre:" conn)
  (println "Open db: " conn))

(defonce todos (r/atom (sorted-map)))

(defonce counter (r/atom 0))

(defn add-todo [text]
  (let [id (swap! counter inc)]
    (swap! todos assoc id {:id id :title text})))

(defn save [id title]
  (swap! todos assoc-in [id :title] title)
  (timbre/info "Title: " title))

(defonce init (do
                (timbre/debug "Starting site....")
                (add-todo "Get datomish - Project mentat")
                (add-todo "Publish localrepo")
                (add-todo "Get simple test site run")
                (add-todo "Hook datomish to test site")))

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [val (r/atom title)
        stop #(do (reset! val "")
                  (if on-stop (on-stop)))
        save #(let [v (-> @val str clojure.string/trim)]
                (if-not (empty? v) (on-save v))
                (stop))]
    (fn [{:keys [id class placeholder]}]
      [:input {:type "text" :value @val
               :id id :class class :placeholder placeholder
               :on-blur save
               :on-change #(reset! val (-> % .-target .-value))
               :on-key-down #(case (.-which %)
                               13 (save)
                               27 (stop)
                               nil)}])))

(defn todo-item []
    (fn [{:keys [id title]}]
      [:li
       ;[:div.view
        [:label title]]))

(defn todo-app [props]
  (testdb)
  (let [filt (r/atom :all)]
    (fn []
      (let [items (vals @todos)]
            ;done (->> items (filter :done) count)
            ;active (- (count items) done)]
        [:div
         [:section#todoapp
          [:header#header
           [:h1 "To do Tasks"]
           [todo-input {:id "new-todo"
                        :placeholder "What needs to be done?"
                        :on-save add-todo}]]
          (when (-> items count pos?)
            [:div
             [:section#main
              [:ul#todo-list
               (for [todo (filter (case @filt
                                    ;:active (complement :done)
                                    ;:done :done
                                    :all identity) items)]
                 ^{:key (:id todo)} [todo-item todo])]]])]]))))


(defn ^:export main []
  (r/render [todo-app]
            (js/document.getElementById "app")))





