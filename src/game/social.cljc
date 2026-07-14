(ns game.social
  "Portable social-game platform contracts.

  This namespace is deliberately pure CLJC: applications provide persistence,
  clocks, signatures and payment verification, while this layer owns the data
  invariants shared by browser games, workers and native hosts. IDs and times are
  explicit inputs so replaying the same command is deterministic and idempotent.")

(def currencies #{:free-gem :paid-gem})

(defn player-profile
  [did display-name created-at]
  {:player/did did
   :player/display-name display-name
   :player/created-at created-at
   :player/status :active
   :player/blocked #{}
   :player/muted #{}})

(defn platform-state []
  {:platform/version 1
   :saves {}
   :wallets {}
   :inventories {}
   :receipts {}
   :leaderboards {}
   :chat []})

;; Save data uses optimistic concurrency. A device must write the revision it
;; read, preventing a stale tab from silently overwriting newer progression.
(defn put-save
  [state {:keys [player game expected-rev data updated-at]}]
  (let [path [player game]
        current (get-in state [:saves path])
        rev (or (:save/rev current) 0)]
    (cond
      (not= expected-rev rev) [:error :revision-conflict state]
      (not (map? data)) [:error :invalid-save state]
      :else
      (let [save {:save/player player :save/game game :save/rev (inc rev)
                  :save/data data :save/updated-at updated-at}]
        [:ok save (assoc-in state [:saves path] save)]))))

(defn wallet
  ([] (wallet {}))
  ([balances]
   {:wallet/balances (merge (zipmap currencies (repeat 0)) balances)
    :wallet/transactions []
    :wallet/applied #{}}))

(defn balance [w currency] (get-in w [:wallet/balances currency] 0))

(defn apply-transaction
  "Apply a server-authorized credit/debit once. Returns
   [:ok wallet'] | [:duplicate wallet] | [:error reason wallet]."
  [w {:tx/keys [id currency amount] :as tx}]
  (cond
    (contains? (:wallet/applied w) id) [:duplicate w]
    (not (contains? currencies currency)) [:error :unknown-currency w]
    (not (integer? amount)) [:error :invalid-amount w]
    (zero? amount) [:error :zero-amount w]
    (neg? (+ (balance w currency) amount)) [:error :insufficient-funds w]
    :else [:ok (-> w
                   (update-in [:wallet/balances currency] + amount)
                   (update :wallet/transactions conj tx)
                   (update :wallet/applied conj id))]))

(defn inventory [] {:inventory/items {} :inventory/entitlements #{}})

(defn grant-item
  [inv {:item/keys [id quantity]}]
  (if (and id (integer? quantity) (pos? quantity))
    [:ok (update-in inv [:inventory/items id] (fnil + 0) quantity)]
    [:error :invalid-item-grant inv]))

(defn grant-entitlement [inv entitlement]
  (update inv :inventory/entitlements conj entitlement))

(defn owned? [inv item-id]
  (or (pos? (get-in inv [:inventory/items item-id] 0))
      (contains? (:inventory/entitlements inv) item-id)))

(defn product
  [{:product/keys [id currency price grants] :as p}]
  (when (and id (contains? currencies currency) (integer? price) (pos? price)
             (seq grants))
    p))

(defn purchase
  "Atomically debit a wallet and grant a catalog product. receipt-id is the
   idempotency key; payment providers remain outside this pure contract."
  [state {:keys [player receipt-id product purchased-at]}]
  (cond
    (contains? (:receipts state) receipt-id) [:duplicate (get-in state [:receipts receipt-id]) state]
    (nil? (game.social/product product)) [:error :invalid-product state]
    :else
    (let [w (get-in state [:wallets player] (wallet))
          inv (get-in state [:inventories player] (inventory))
          debit #:tx{:id receipt-id :currency (:product/currency product)
                     :amount (- (:product/price product)) :kind :purchase
                     :at purchased-at :product (:product/id product)}
          [status x & _] (apply-transaction w debit)]
      (if (not= status :ok)
        [:error x state]
        (let [inv' (reduce (fn [acc grant]
                             (if-let [e (:entitlement/id grant)]
                               (grant-entitlement acc e)
                               (second (grant-item acc grant))))
                           inv (:product/grants product))
              receipt {:receipt/id receipt-id :receipt/player player
                       :receipt/product (:product/id product)
                       :receipt/at purchased-at}
              state' (-> state
                         (assoc-in [:wallets player] x)
                         (assoc-in [:inventories player] inv')
                         (assoc-in [:receipts receipt-id] receipt))]
          [:ok receipt state'])))))

(defn royalties
  "Credits by lineage distance, dropping rounded-to-zero awards."
  ([amount ancestors] (royalties amount ancestors {:rate 0.1 :decay 0.5}))
  ([amount ancestors {:keys [rate decay] :or {rate 0.1 decay 0.5}}]
   (->> ancestors
        (map-indexed (fn [i ancestor]
                       [ancestor (long (#?(:clj Math/floor :cljs js/Math.floor)
                                         (* amount rate
                                            (#?(:clj Math/pow :cljs js/Math.pow) decay i))))]))
        (filterv (fn [[_ credit]] (pos? credit))))))

(defn leaderboard
  [{:board/keys [id game season order] :or {order :desc}}]
  {:board/id id :board/game game :board/season season :board/order order
   :board/entries {} :board/submissions #{}})

(defn submit-score
  "Idempotent best-score submission. Verification is an explicit prerequisite
   supplied by the authoritative game-specific verifier."
  [board {:score/keys [submission player value verified] :as score}]
  (cond
    (contains? (:board/submissions board) submission) [:duplicate board]
    (not verified) [:error :unverified board]
    (not (number? value)) [:error :invalid-score board]
    :else
    (let [old (get-in board [:board/entries player])
          better? (or (nil? old)
                      ((if (= :asc (:board/order board)) < >) value (:score/value old)))
          board' (cond-> (update board :board/submissions conj submission)
                   better? (assoc-in [:board/entries player] score))]
      [:ok board'])))

(defn standings [board]
  (let [cmp (if (= :asc (:board/order board)) identity -)]
    (->> (:board/entries board) vals (sort-by (comp cmp :score/value)) vec)))

(defn admit-chat
  "Validate one player message against room policy and recipient controls.
   Content classification/moderation supplies :message/moderation externally."
  [state {:message/keys [id from to text moderation] :as message}
          {:room/keys [max-length] :or {max-length 280}}]
  (let [recipient (get-in state [:profiles to])]
    (cond
      (or (nil? id) (nil? from) (not (string? text)) (empty? text)) [:error :invalid-message state]
      (> (count text) max-length) [:error :message-too-long state]
      (contains? (:player/blocked recipient) from) [:error :blocked state]
      (= moderation :rejected) [:error :moderation-rejected state]
      :else [:ok message (update state :chat conj message)])))
