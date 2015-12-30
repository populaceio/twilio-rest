(ns twilio.rest
  "A partial wrapper for the Twilio REST library.

   Syncing with the master account requires GET'ing an
   account resource using the minimal authentication
   parameters for the master account:

   {:sid <sid> :auth-token <token>}

   All resources keep an additional reference to their parent
   account in the key :acct for easy access for subsequent
   operations on that resource.

   Supported Nouns:
   - Account
   - SubAccount
   - AvailablePhoneNumber
   - IncomingPhoneNumber
   - Message
   - Notification

   Additional additional nouns to this library should be straightforward.
   "
  (:refer-clojure :exclude [get])
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as string]
            [hara.event :refer :all]))


;;
;; Twilio API Support (Partial)
;; ---------------------------------------


;;
;; Utilities
;;

(defn- ^{:testable true} camelcase->dashed
  "getThisValue -> get-this-value"
  ([string]
     (camelcase->dashed string "-"))
  ([string sep]
     (->> (string/split string #"(?=[A-Z])")
          (filter #(> (count %) 0))
          (map string/lower-case)
          (string/join sep))))

(defn- ^{:testable true} dashed->camelcase [#^String string]
  "get-this-value -> getThisValue"
  (let [[head & rest] (string/split string #"[-_]")]
    (string/join (cons head (map string/capitalize rest)))))

(defn- ^{:testable true} dashed->pascalcase
  "get-this-value -> GetThisValue"
  [string]
  (->> (string/split string #"[-_]")
       (map string/capitalize)
       string/join))

(defn pascalcase-map
  "Convert from clojure labels to pascalcase"
  [themap]
  (zipmap (map (comp dashed->pascalcase name) (keys themap))
          (vals themap)))

(defn underscorecase-map
  "Convert from clojure labels to pascalcase"
  [themap]
  (zipmap (map (comp keyword #(camelcase->dashed % "_") name) (keys themap))
          (vals themap)))

(defn as-twilio-post-params
  "Convert a standard dashed map to pascalcase in a form params block"
  [params]
  {:form-params
   (pascalcase-map params)})

(defn as-twilio-query-params
  "Convert a standard dashed map to pascalcase in a query params block"
  [params]
  {:query-params
   (pascalcase-map params)})

(defn- map-importer
  "Add the parent account to imported objects via the provided
   map->Account constructor"
  [acct mapper]
  (fn [themap]
    (mapper (assoc themap :acct acct))))

(defn parse-rfc2822 [string]
  {:pre [(string? string)]}
  (-> (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z")
      (.parse)))

(defn E164-formatted? [string]
  (re-find #"\+\d*" string))

(defn- phone-number? [string]
  (and (string? string)
       (>= (count string) 10)
       (re-find #"[\(\-\.\s]*\d\d\d[\)\-\.\s]*\d\d\d[\-\.\s]*\d\d\d\d"
                string)))

(defn as-E164
  "Ensure a number or Number is interpreted as an E164 formatted number"
  [reference]
  (cond (and (map? reference) (:phone_number reference))
        (:phone_number reference)
        (E164-formatted? reference) reference
        (phone-number? reference)
        (if (and (= (subs reference 0 1) "1")
                 (> (count subs) 10))
          (clojure.string/replace (subs reference 1) #"[\s\-\.]" "")
          (clojure.string/replace reference #"[\s\-\.]" ""))
        :default (throw (ex-info "Cannot convert to E.164 phone number string"
                                 {:reference reference}))))


;;
;; Twilio REST API IO
;;

(def ^{:private true} twilio-base  "https://api.twilio.com")
(def ^{:private true} api-version  "2010-04-01")
(def ^{:private true} api-base  (str twilio-base "/" api-version))
(def debug? (atom false))

(defn account? [acct]
  (and (:sid acct) (:auth_token acct)))

(defn- as-auth [acct]
  [(:sid acct) (:auth_token acct)])

(defn- with-auth [acct request]
  (assoc request :basic-auth (as-auth acct)))

(defn- with-debug [request]
  (assoc request :throw-exceptions @debug?))

(defn- as-json [request]
  (assoc request :content-type "text/json"))

(defn- postfix-json [url]
  (str url ".json"))

(defn- handle-reply
  [{:keys [status body] :as resp}]
  (let [body (json/parse-string body true)]
    (if (and (>= status 200)
             (< status 300))
      body
      (raise body))))

(defn get-command
  ([acct url request postfix?]
   {:pre [(account? acct)]}
   (->> request
        (merge-with merge (as-twilio-query-params {:page-size 1000}))
        with-debug
        (with-auth acct)
        as-json
        (http/get (cond-> url
                    postfix? postfix-json))
        handle-reply))
  ([acct url request]
   (get-command acct url request true))
  ([acct url]
   (get-command acct url {})))

(defn mapcat-pages
  ([acct k body] (mapcat-pages acct k {} body))
  ([acct k
    {:keys [page-size limit]
     :or {page-size 1000
          limit 1000}
     :as options}
    {:keys [next_page_uri] :as body}]
   (if (and next_page_uri
            (or (nil? limit)
                (> limit (count (k body)))))
     (let [body (->> (get-command acct (str twilio-base next_page_uri)
                                  (as-twilio-query-params {:page-size page-size}) false)
                     (merge-with concat (select-keys body [k])))]
       (mapcat-pages acct k options body))
     (if limit
       (update body k (partial take limit))
       body))))

(defn- post-command
  ([acct url request]
     {:pre [(account? acct)]}
     (->> request
          with-debug
          (with-auth acct)
          (http/post (postfix-json url))
          handle-reply))
  ([acct url]
     (post-command acct url {})))

(defn- put-command
  ([acct url request]
     {:pre [(account? acct)]}
     (->> request
          with-debug
          (with-auth acct)
          (http/put (postfix-json url))
          handle-reply))
  ([acct url]
     (put-command acct url {})))

(defn- delete-command
  ([acct url request]
     {:pre [(account? acct)]}
     (->> request
          with-debug
          (with-auth acct)
          (http/delete (postfix-json url))
          handle-reply))
  ([acct url]
   (delete-command acct url {})))

;;
;; ## Resource protocol
;;

(defprotocol RESTResource
  "A protocol for getting and updating URL resource properties.
   Commands return a fresh object of the type including side-effects."
  (url [res] "The URL of the resource")
  (get [res] "Merge server properties into object")
  (post! [res] "Create/update resource on the server")
  (put! [res] [res props] "Update resource properties")
  (delete! [res] "Remove resource from server"))

;; Convenience method to get a resource from only an account and sid
(defn get-resource [acct type sid]
  (let [map-constructor (resolve (symbol (str "map->" (name type))))]
    (-> {:sid sid :account_sid (:sid acct) :acct acct}
        map-constructor
        get
        map-constructor)))

;; Some Twilio URLs expose generic find or create methods
(defn- resources-dispatch [& args]
  (:type (second args)))
(defmulti create-resource resources-dispatch)
(defmulti list-resources resources-dispatch)

(defn- not-supported
  "Function to call for RESTResource methods that
   are not supported by the endpoint"
  ([]
     (throw (java.lang.Error. "Not supported")))
  ([msg]
     (throw (java.lang.Error. msg))))


;;
;; # Account operations
;;

(defn- account-params [acct]
  (select-keys acct [:friendly_name :status]))

(declare map->Account)

(defrecord Account [sid auth_token]
  RESTResource
  (url [acct] (format "%s/Accounts/%s" api-base sid))
  (get [acct] (merge acct (get-command acct (url acct))))
  (put! [acct params]
    (map->Account
     (post-command acct (url acct) (as-twilio-post-params params))))
  (put! [acct] (put! acct (account-params acct)))
  (post! [acct] (put! acct))
  (delete! [acct] (not-supported "Cannot delete accounts")))

(defmethod print-method Account [acct writer]
  (.write writer (format "#Account[\"%s\"]" (:friendly_name acct))))

(defn set-account-status
  "Change an Account's status"
  [account status]
  (put! account {:status status}))

(defn set-account-name
  "Change an Account's friendly name"
  [account name]
  (put! account {:friendly-name name}))

(defn- account-proxy
  "Creates a proxy object using only the sid"
  [sid]
  {:sid sid})

;;
;; ## Subaccounts
;;

(def Accounts {:type :subaccounts :url (str api-base "/Accounts")})

(defmethod create-resource :subaccounts [acct factory name]
  ((map-importer acct map->Account)
   (post-command acct (:url factory)
                 {:form-params {:FriendlyName name}})))

(defn master-account? [account]
  (= (:owner_account_sid account) (:sid account)))

(defn subaccount? [account]
  (not (master-account? account)))

(defmethod list-resources :subaccounts [acct factory & {:keys [friendly-name status]
                                                        :as filters}]
  (->> (get-command acct (:url factory) (as-twilio-query-params filters))
       (mapcat-pages acct :accounts {:limit nil})
       :accounts
       (map #(map->Account (assoc % :acct acct)))
       (filter subaccount?)))

(defn subaccounts
  ([acct] (subaccounts acct {}))
  ([acct filters]
   (apply list-resources acct Accounts (mapcat identity filters))))

(defn get-subaccount [acct name]
  (let [accounts (subaccounts acct {:friendly-name name
                                    :status "active"})]
    (assert (= 1 (count accounts)) (str "more than one active account exists with name: " name))
    (first accounts)))

(defn close-account [account]
  (set-account-status account "closed"))

;;
;; Applications
;;

(declare map->Application)

(defrecord Application [acct sid account_sid]
  RESTResource
  (url [app] (format "%s/Accounts/%s/Applications/%s"
                     api-base (:account_sid app) (:sid app)))
  (get [app] (get-command acct (url app)))
  (put! [app params]
    ((map-importer acct map->Application)
     (post-command acct (url app)
                   (as-twilio-post-params params))))
  (put! [app] (put! app (dissoc app :sid :account_sid)))
  (post! [app] (put! app))
  (delete! [app] (delete-command acct (url app))))

(defmethod print-method Application [app writer]
  (.write writer (format "#Application[\"%s\"]" (:friendly_name app))))

(def ApplicationList
  {:type :applications
   :urlfn (fn [acct]
            (format "%s/%s/Applications" (:url Accounts) (:sid acct)))})

(defmethod list-resources :applications [acct factory & {:keys [friendly-name]
                                                        :as filters}]
  (->> (get-command acct ((:urlfn factory) acct) (as-twilio-query-params filters))
       (mapcat-pages acct :applications {:limit nil})
       :applications
       (map (map-importer acct map->Application))))

(defmethod create-resource :applications [acct factory params]
  ((map-importer acct map->Application)
   (post-command acct
                 ((:urlfn factory) acct)
                 {:form-params
                  (pascalcase-map params)})))

(defn applications
  ([acct] (applications acct {}))
  ([acct filters]
   (apply list-resources acct ApplicationList (mapcat identity filters))))

(defn get-application [acct name]
  (let [applications (applications acct {:friendly-name name})]
    (assert (= 1 (count applications)) (format "more than one application exists in account: %s with name: %s"
                                               (:friendly_name acct) name))
    (first applications)))

(comment
  ;; Master
  (def master
    {:sid "********************************"
     :auth_token "********************************"})

  ;; Sub Account
  (def subacct
    (create-resource master Accounts "Test Account"))

  ;; Create a test application
  (create-resource subacct ApplicationList
                   {:friendly-name "Testing"
                    :sms-url "http://www.personalexperiments.org/sms/test/receive"
                    :sms-fallback-url "http://www.personalexperiments.org/sms/test/error"}))


;;
;; Manage phone numbers
;;

(defrecord AvailableNumber [acct phone_number iso_country friendly_name])

(defmethod print-method AvailableNumber [anum writer]
  (.write writer (format "#AvailableNumber[%s]" (:friendly_name anum))))

(def AvailableNumbers {:type :available
                       :urlfn (fn [acct country]
                                (format "%s/%s/AvailablePhoneNumbers/%s/Local"
                                        (:url Accounts)
                                        (:sid acct) country))})

;; Takes arguments: area-code, contains, in-region, in-postal-code
(defmethod list-resources :available [acct factory country &
                                      {:keys [area-code contains in-region in-postal-code] :as options}]
  (->> (get-command acct ((:urlfn factory) acct country)
                    (as-twilio-query-params options))
       :available_phone_numbers
       (map (map-importer acct map->AvailableNumber))))

(defn available-numbers
  ([acct country code]
     (list-resources acct AvailableNumbers country :area-code code))
  ([acct country]
     (list-resources acct AvailableNumbers country))
  ([acct]
     (available-numbers acct "US")))

(def PhoneNumbers
  {:type :incoming
   :urlfn (fn [acct]
            (str (url acct) "/IncomingPhoneNumbers"))})

(defrecord PhoneNumber [acct sid account_sid friendly_name phone_number sms_application_sid]
  RESTResource
  (url [number]
    (format "%s/Accounts/%s/IncomingPhoneNumbers/%s"
            api-base
            (:account_sid number)
            (:sid number)))
  (get [number] (merge number (get-command acct (url number))))
  (put! [number map]
    (post! (merge number map)))
  (put! [number]
    (post! number))
  (post! [number]
    (post-command acct (url number)
                  (as-twilio-post-params
                   (dissoc number :sid :account_sid))))
  (delete! [number]
    (delete-command acct (url number))))

(defmethod print-method PhoneNumber [num writer]
  (.write writer (format "#PhoneNumber[%s]" (:phone_number num))))

;; Return a list of the accounts phone numbers
(defmethod list-resources :incoming [acct factory]
  (->> acct
       ((:urlfn factory))
       (get-command acct)
       (mapcat-pages acct :incoming_phone_numbers {:limit nil})
       :incoming_phone_numbers
       (mapv (map-importer acct map->PhoneNumber))))

(defmethod create-resource :incoming
  [acct factory params]
  {:pre [(account? acct)]}
  ((map-importer acct map->PhoneNumber)
   (post-command acct
                 ((:urlfn PhoneNumbers) acct)
                 (as-twilio-post-params params))))

(defn numbers
  "Convenience form for listing current phone numbers"
  [acct]
  (list-resources acct PhoneNumbers))

(defn add-number
  "Add a number returned by Available Numbers"
  ([acct available-number]
     (add-number acct available-number {}))
  ([acct available-number params]
     (create-resource acct PhoneNumbers
                      (merge params
                             (select-keys available-number [:phone_number])))))

(defn add-number-by-code
  "Add a number by allocating by US/Canadian area code"
  ([acct code params]
     {:pre [(number? code)]}
     ((map-importer acct map->PhoneNumber)
      (create-resource acct PhoneNumbers
                       (assoc params :area_code code))))
  ([acct code]
     (add-number-by-code acct code {})))

(defn set-application! [number application]
  (assert (and (instance? PhoneNumber number)))
  ((map-importer (:acct number) map->PhoneNumber)
   (post! (assoc number
            :sms_application_sid (:sid application)))))

(comment
  ;; Get the first number in any subaccount
  (first (numbers (first (subaccounts))))
  ;; Get the first number in a specific subaccount
  (first (numbers (get-subaccount "PersonalExperiments")))
  ;; Define a specific test account, number and application
  (def test-account (get-subaccount "PersonalExperiments"))
  (def test-number (first (numbers test-account)))
  (def test-application (get-application test-account "PersonalExperiments SMS Application"))
  ;; Ensure application is set
  (set-application! test-number test-application))


;;
;; Messages
;;

(def Messages
  {:type :messages
   :urlfn (fn [acct]
            (format "%s/%s/Messages" (:url Accounts) (:sid acct)))})

(declare map->Message)

(defrecord Message [acct sid account_sid date_created date_updated date_sent
                    from to body status direction uri api_version price price_unit num_media
                    from_city from_state from_zip from_country
                    to_city to_state to_zip to_country]
  RESTResource
  (url [msg]
    (assert (:sid msg))
    (format "%s/%s/Messages/%s"
            (:url Accounts)
            (:sid acct)
            (:sid msg)))
  (get [msg]
    ((map-importer acct map->Message)
     (get-command acct (url msg))))
  (post! [msg] (not-supported))
  (put! [msg] (not-supported))
  (put! [msg params] (not-supported))
  (delete! [msg] (not-supported)))

(defmethod print-method Message [msg writer]
  (.write writer (format "#Message[from %s on %s]" (:from msg) (:date_created msg))))


(defmethod list-resources :messages [acct factory &
                                     {:keys [to from date-sent limit page-size] :as options}]
  (->> (get-command acct ((:urlfn factory) acct)
                    (as-twilio-query-params options))
       (mapcat-pages acct :messages {:limit limit
                                     :page-size page-size})
       :messages
       (map (map-importer acct map->Message))))

(defn messages
  [acct options]
  (list-resources acct Accounts options))

(defmethod create-resource :messages [acct factory message]
  {:pre [(every? (comp boolean message) [:from :to :body])]}
  (post-command acct
                ((:urlfn factory) acct)
                (as-twilio-post-params message)))

;;
;; Debugging Support
;; --------------------------

(def Notifications
  {:type :notifications
   :urlfn (fn [account]
            (format "%s/%s/Notifications" (:url Accounts) (:sid account)))})

(defmethod list-resources :notifications [factory account &
                                          {:keys [log message-date]
                                           :as options}]
  (get-command ((:urlfn factory) account)
               (pascalcase-map options)))


;;
;; Lookups
;;

(def Lookups
  {:type :lookup
   :urlfn (fn [number]
            (format "https://lookups.twilio.com/v1/PhoneNumbers/%s" number))})

(defn lookup
  ([acct number] (lookup acct number "US"))
  ([acct number country-code]
   (get-command acct ((:urlfn Lookups) number)
                (as-twilio-query-params {:type "carrier"
                                         :country-code country-code})
                false)))
