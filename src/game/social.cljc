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
   :payment-intents {}
   :refunds {}
   :daily-rewards {}
   :inventories {}
   :receipts {}
   :leaderboards {}
   :chat []
   :friendships #{}
   :friend-requests {}
   :blocks #{}
   :groups {}
   :rooms {}})

(defn pair
  "Canonical undirected player pair."
  [a b]
  (when (and a b (not= a b)) (vec (sort [a b]))))

(defn blocked?
  "True when either player has blocked the other."
  [state a b]
  (or (contains? (:blocks state) [a b])
      (contains? (:blocks state) [b a])))

(defn request-friend
  [state {:keys [request-id from to at]}]
  (let [p (pair from to)]
    (cond
      (or (nil? request-id) (nil? p)) [:error :invalid-friend-request state]
      (contains? (:friend-requests state) request-id) [:duplicate state]
      (blocked? state from to) [:error :blocked state]
      (contains? (:friendships state) p) [:error :already-friends state]
      :else [:ok (assoc-in state [:friend-requests request-id]
                           {:request/id request-id :request/from from :request/to to
                            :request/status :pending :request/at at})])))

(defn answer-friend
  [state {:keys [request-id player accept? at]}]
  (let [request (get-in state [:friend-requests request-id])]
    (cond
      (nil? request) [:error :request-not-found state]
      (not= :pending (:request/status request)) [:duplicate state]
      (not= player (:request/to request)) [:error :not-request-recipient state]
      (blocked? state (:request/from request) player) [:error :blocked state]
      :else
      (let [status (if accept? :accepted :declined)
            state' (assoc-in state [:friend-requests request-id]
                             (assoc request :request/status status :request/answered-at at))]
        [:ok (cond-> state' accept?
               (update :friendships conj (pair (:request/from request) player)))]))))

(defn block-player
  "Directional block. Blocking also removes friendship and pending requests
   between the pair, so a stale invitation cannot recreate the relationship."
  [state blocker blocked at]
  (if-let [p (pair blocker blocked)]
    [:ok (-> state
             (update :blocks conj [blocker blocked])
             (update :friendships disj p)
             (update :friend-requests
                     (fn [requests]
                       (into {} (remove (fn [[_ r]]
                                          (and (= :pending (:request/status r))
                                               (= p (pair (:request/from r) (:request/to r))))))
                             requests)))
             (assoc-in [:block-events [blocker blocked]] at))]
    [:error :invalid-block state]))

(defn remove-friend
  "Remove an existing friendship. Pending requests are also removed so both
   players return to a clean unrelated state."
  [state player other]
  (if-let [p (pair player other)]
    (if (contains? (:friendships state) p)
      [:ok (-> state
               (update :friendships disj p)
               (update :friend-requests
                       (fn [requests]
                         (into {} (remove (fn [[_ request]]
                                            (= p (pair (:request/from request)
                                                       (:request/to request)))))
                               requests))))]
      [:duplicate state])
    [:error :invalid-friend state]))

(def group-kinds #{:party :guild})

(defn create-group
  [state {:group/keys [id kind owner capacity] :as group}]
  (cond
    (or (nil? id) (nil? owner) (not (contains? group-kinds kind))
        (not (integer? capacity)) (< capacity 2)) [:error :invalid-group state]
    (contains? (:groups state) id) [:error :group-exists state]
    :else [:ok (assoc-in state [:groups id]
                         (merge group {:group/members {owner :owner}}))]))

(defn join-group
  [state group-id player role]
  (let [group (get-in state [:groups group-id])]
    (cond
      (nil? group) [:error :group-not-found state]
      (contains? (:group/members group) player) [:duplicate state]
      (>= (count (:group/members group)) (:group/capacity group)) [:error :group-full state]
      (some #(blocked? state player %) (keys (:group/members group))) [:error :blocked state]
      :else [:ok (assoc-in state [:groups group-id :group/members player] (or role :member))])))

(def group-roles #{:owner :moderator :member})

(defn leave-group
  "A non-owner may leave freely. A sole owner deletes the empty group; an owner
   with remaining members must transfer ownership first."
  [state group-id player]
  (let [group (get-in state [:groups group-id])
        role (get-in group [:group/members player])]
    (cond
      (nil? group) [:error :group-not-found state]
      (nil? role) [:duplicate state]
      (and (= :owner role) (> (count (:group/members group)) 1))
      [:error :owner-transfer-required state]
      (= :owner role) [:ok (update state :groups dissoc group-id)]
      :else [:ok (update-in state [:groups group-id :group/members] dissoc player)])))

(defn change-group-role
  "Only the owner may promote/demote an existing non-owner member. Ownership
   uses transfer-group-owner so a group can never have zero or two owners."
  [state group-id actor target role]
  (let [group (get-in state [:groups group-id])]
    (cond
      (nil? group) [:error :group-not-found state]
      (not= :owner (get-in group [:group/members actor])) [:error :not-group-owner state]
      (nil? (get-in group [:group/members target])) [:error :member-not-found state]
      (= :owner (get-in group [:group/members target])) [:error :cannot-change-owner-role state]
      (not (contains? #{:moderator :member} role)) [:error :invalid-group-role state]
      (= role (get-in group [:group/members target])) [:duplicate state]
      :else [:ok (assoc-in state [:groups group-id :group/members target] role)])))

(defn transfer-group-owner
  [state group-id actor target]
  (let [group (get-in state [:groups group-id])]
    (cond
      (nil? group) [:error :group-not-found state]
      (not= :owner (get-in group [:group/members actor])) [:error :not-group-owner state]
      (= actor target) [:duplicate state]
      (nil? (get-in group [:group/members target])) [:error :member-not-found state]
      :else [:ok (-> state
                     (assoc-in [:groups group-id :group/owner] target)
                     (assoc-in [:groups group-id :group/members actor] :moderator)
                     (assoc-in [:groups group-id :group/members target] :owner))])))

(defn remove-group-member
  "Owner may remove moderators/members; moderator may remove members only."
  [state group-id actor target]
  (let [group (get-in state [:groups group-id])
        actor-role (get-in group [:group/members actor])
        target-role (get-in group [:group/members target])
        allowed? (or (= :owner actor-role)
                     (and (= :moderator actor-role) (= :member target-role)))]
    (cond
      (nil? group) [:error :group-not-found state]
      (= actor target) [:error :use-leave-group state]
      (nil? target-role) [:duplicate state]
      (= :owner target-role) [:error :cannot-remove-owner state]
      (not allowed?) [:error :insufficient-group-role state]
      :else [:ok (update-in state [:groups group-id :group/members] dissoc target)])))

(defn create-room
  [state {:room/keys [id kind members max-length history-limit] :as room}]
  (cond
    (or (nil? id) (not (contains? #{:direct :party :guild :game} kind))
        (empty? members)) [:error :invalid-room state]
    (contains? (:rooms state) id) [:error :room-exists state]
    :else [:ok (assoc-in state [:rooms id]
                         (merge {:room/max-length 280 :room/history-limit 100
                                 :room/messages []}
                                room {:room/members (set members)}))]))

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

(def default-daily-reward-schedule [10 10 15 15 20 25 50])

(defn daily-reward-schedule
  [schedule]
  (when (and (vector? schedule) (seq schedule)
             (every? #(and (integer? %) (pos? %)) schedule))
    schedule))

(defn claim-daily-reward
  "Claim one host-verified UTC day. Consecutive days advance the streak and
   cycle through the schedule; a missed day restarts at day one."
  [state {:keys [player day schedule verified? claimed-at]
          :or {schedule default-daily-reward-schedule}}]
  (let [reward-state (get-in state [:daily-rewards player]
                             {:reward/last-day nil :reward/streak 0 :reward/claims {}})
        last-day (:reward/last-day reward-state)]
    (cond
      (or (nil? player) (not (integer? day)) (nil? (daily-reward-schedule schedule)))
      [:error :invalid-daily-reward state]
      (not verified?) [:error :daily-reward-unverified state]
      (contains? (:reward/claims reward-state) day)
      [:duplicate (get-in reward-state [:reward/claims day]) state]
      (and last-day (< day last-day)) [:error :stale-reward-day state]
      :else
      (let [streak (if (= last-day (dec day)) (inc (:reward/streak reward-state)) 1)
            amount (nth schedule (mod (dec streak) (count schedule)))
            claim #:reward{:day day :streak streak :currency :free-gem
                           :amount amount :claimed-at claimed-at}
            tx #:tx{:id (str "daily/" player "/" day) :currency :free-gem
                    :amount amount :kind :daily-reward :at claimed-at :ref (str day)}
            [status wallet] (apply-transaction (get-in state [:wallets player] (wallet)) tx)]
        (if (not= :ok status)
          [:error :daily-reward-credit-failed state]
          [:ok claim (-> state
                         (assoc-in [:wallets player] wallet)
                         (assoc-in [:daily-rewards player :reward/last-day] day)
                         (assoc-in [:daily-rewards player :reward/streak] streak)
                         (assoc-in [:daily-rewards player :reward/claims day] claim))])))))

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

(def fiat-currencies #{:jpy :usd :eur})

(defn gem-pack
  "Validate a provider-neutral paid-gem pack. Fiat amounts use minor units;
   provider product/price IDs stay in the host adapter."
  [{:pack/keys [id gems fiat-currency fiat-amount] :as pack}]
  (when (and id (integer? gems) (pos? gems)
             (contains? fiat-currencies fiat-currency)
             (integer? fiat-amount) (pos? fiat-amount))
    pack))

(defn create-payment-intent
  "Reserve one immutable player/pack purchase. Creating the same intent ID is
   idempotent; payment-provider checkout creation remains a host concern."
  [state {:keys [intent-id player pack created-at]}]
  (cond
    (contains? (:payment-intents state) intent-id)
    [:duplicate (get-in state [:payment-intents intent-id]) state]
    (or (nil? intent-id) (nil? player) (nil? (gem-pack pack)))
    [:error :invalid-payment-intent state]
    :else
    (let [intent #:payment{:id intent-id :player player :pack pack
                           :status :pending :created-at created-at}]
      [:ok intent (assoc-in state [:payment-intents intent-id] intent)])))

(defn settle-payment
  "Credit paid gems exactly once after the host verifies provider authenticity,
   amount, currency, pack mapping, and successful state."
  [state {:keys [intent-id provider-ref verified? settled-at]}]
  (let [intent (get-in state [:payment-intents intent-id])]
    (cond
      (nil? intent) [:error :payment-intent-not-found state]
      (= :settled (:payment/status intent)) [:duplicate intent state]
      (not= :pending (:payment/status intent)) [:error :payment-not-pending state]
      (or (not verified?) (nil? provider-ref)) [:error :payment-unverified state]
      :else
      (let [player (:payment/player intent)
            gems (get-in intent [:payment/pack :pack/gems])
            tx #:tx{:id (str "payment/" intent-id) :currency :paid-gem
                    :amount gems :kind :payment :at settled-at :ref provider-ref}
            [status wallet] (apply-transaction (get-in state [:wallets player] (wallet)) tx)]
        (if (not= :ok status)
          [:error :payment-credit-failed state]
          (let [settled (assoc intent :payment/status :settled
                               :payment/provider-ref provider-ref
                               :payment/settled-at settled-at)]
            [:ok settled (-> state
                             (assoc-in [:wallets player] wallet)
                             (assoc-in [:payment-intents intent-id] settled))]))))))

(defn refund-payment
  "Reverse a settled paid-gem pack once after a verified provider refund. If
   gems were already spent, automatic refund is refused for host review."
  [state {:keys [refund-id intent-id provider-ref verified? refunded-at]}]
  (let [intent (get-in state [:payment-intents intent-id])]
    (cond
      (contains? (:refunds state) refund-id)
      [:duplicate (get-in state [:refunds refund-id]) state]
      (or (nil? refund-id) (nil? intent)) [:error :payment-intent-not-found state]
      (not= :settled (:payment/status intent)) [:error :payment-not-settled state]
      (or (not verified?) (nil? provider-ref)) [:error :refund-unverified state]
      :else
      (let [player (:payment/player intent)
            gems (get-in intent [:payment/pack :pack/gems])
            tx #:tx{:id (str "refund/" refund-id) :currency :paid-gem
                    :amount (- gems) :kind :payment-refund :at refunded-at :ref provider-ref}
            [status x] (apply-transaction (get-in state [:wallets player] (wallet)) tx)]
        (if (= status :error)
          [:error :refund-balance-spent state]
          (let [refund #:refund{:id refund-id :payment intent-id :player player
                                :gems gems :provider-ref provider-ref :at refunded-at}
                refunded (assoc intent :payment/status :refunded
                                 :payment/refunded-at refunded-at)]
            [:ok refund (-> state
                            (assoc-in [:wallets player] x)
                            (assoc-in [:payment-intents intent-id] refunded)
                            (assoc-in [:refunds refund-id] refund))]))))))

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

(defn season
  "Create a portable season definition. Reward tiers are ordered inclusive
   rank ceilings, e.g. [{:through 1 :reward {:free-gem 100}} ...]."
  [{:season/keys [id starts-at ends-at reward-tiers] :as config}]
  (when (and id (integer? starts-at) (integer? ends-at) (< starts-at ends-at)
             (vector? reward-tiers)
             (every? (fn [{:keys [through reward]}]
                       (and (integer? through) (pos? through) (map? reward)))
                     reward-tiers)
             (apply < 0 (map :through reward-tiers)))
    config))

(defn season-status [season now]
  (cond (< now (:season/starts-at season)) :scheduled
        (< now (:season/ends-at season)) :active
        :else :closed))

(defn submit-season-score
  "Season-aware form of submit-score. The authoritative host supplies `now`;
   scheduled/closed seasons reject writes before the normal verified-score
   and best-score invariants run."
  [board season now score]
  (if (= :active (season-status season now))
    (submit-score board score)
    [:error :season-not-active board]))

(defn ranked-standings
  "Deterministic competition ranking. Equal values share rank; the next rank
   skips accordingly. at then player provide stable display order without
   changing tie rank."
  [board]
  (let [direction (if (= :asc (:board/order board)) identity -)
        sorted (sort-by (juxt (comp direction :score/value)
                              #(or (:score/at %) 0) :score/player)
                        (vals (:board/entries board)))]
    (loop [remaining sorted, index 1, previous ::none, previous-rank 0, out []]
      (if-let [entry (first remaining)]
        (let [value (:score/value entry)
              rank (if (= value previous) previous-rank index)]
          (recur (next remaining) (inc index) value rank
                 (conj out (assoc entry :score/rank rank))))
        out))))

(defn reward-for-rank [season rank]
  (some (fn [{:keys [through reward]}] (when (<= rank through) reward))
        (:season/reward-tiers season)))

(defn close-season
  "Freeze deterministic standings and reward assignments after the end time.
   The returned snapshot is pure data; hosts own durable payout idempotency."
  [board season now]
  (if (not= :closed (season-status season now))
    [:error :season-not-closed]
    (let [ranked (ranked-standings board)]
      [:ok {:season/id (:season/id season)
            :season/closed-at now
            :season/standings ranked
            :season/rewards (into {}
                                  (keep (fn [entry]
                                          (when-let [reward (reward-for-rank season (:score/rank entry))]
                                            [(:score/player entry) reward])))
                                  ranked)}])))

;; ── portable platform HUD projection ────────────────────────────────────────

(defn- currency-key [currency]
  (cond (keyword? currency) currency
        (string? currency) (keyword currency)
        :else currency))

(defn platform-hud-model
  "Project a host snapshot into one stable, portable HUD model. Hosts may use
   D1/HTTP, native storage, or an in-memory state; renderers consume this shape
   instead of learning a provider's wire rows. Unknown fields are ignored so
   old clients can read newer snapshots."
  [{:keys [did balances transactions inventory receipts products
           gem-packs payments daily-reward server-day
           friend-requests friendships blocks groups group-members]
    :as _snapshot}]
  (let [wallet-balances (reduce (fn [m {:keys [currency balance]}]
                                  (assoc m (currency-key currency) (or balance 0)))
                                (zipmap currencies (repeat 0)) balances)
        owned-items (->> inventory
                         (map #(update % :entitlement
                                       (fn [value] (or (true? value) (= 1 value)))))
                         (filter #(or (pos? (or (:quantity %) 0))
                                      (true? (:entitlement %))))
                         (sort-by :item) vec)
        friend-ids (->> friendships
                        (keep (fn [{:keys [a b]}]
                                (cond (= did a) b (= did b) a)))
                        distinct sort vec)
        pending-in (->> friend-requests
                        (filter #(and (= did (:recipient %)) (= "pending" (:status %))))
                        (sort-by :created_at) vec)
        pending-out (->> friend-requests
                         (filter #(and (= did (:sender %)) (= "pending" (:status %))))
                         (sort-by :created_at) vec)
        product-models (->> products
                            (map (fn [{:keys [currency price] :as p}]
                                   (let [currency (currency-key currency)]
                                     (assoc p :currency currency
                                            :affordable? (>= (get wallet-balances currency 0)
                                                             (or price 0))))))
                            (sort-by :id) vec)]
    {:hud/version 1
     :player/did did
     :wallet/balances wallet-balances
     :wallet/transactions (vec transactions)
     :wallet/daily-reward daily-reward
     :wallet/server-day server-day
     :wallet/daily-claimable? (not= server-day (:day daily-reward))
     :inventory/items owned-items
     :store/products product-models
     :store/receipts (vec receipts)
     :store/gem-packs (vec gem-packs)
     :store/payments (vec payments)
     :social/friends friend-ids
     :social/pending-in pending-in
     :social/pending-out pending-out
     :social/blocks (vec blocks)
     :social/groups (vec groups)
     :social/group-members (->> group-members
                                (group-by :group_id)
                                (reduce-kv (fn [m group-id members]
                                             (assoc m group-id
                                                    (vec (sort-by :joined_at members))))
                                           {}))
     :hud/tabs [{:tab/id :wallet :tab/count (count transactions)}
                {:tab/id :inventory :tab/count (count owned-items)}
                {:tab/id :store :tab/count (+ (count product-models) (count gem-packs))}
                {:tab/id :friends :tab/count (+ (count friend-ids) (count pending-in))}
                {:tab/id :groups :tab/count (count groups)}]}))

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

(defn admit-room-message
  "Room-aware chat admission. The host supplies moderation classification,
   persistence and rate-limit decisions; this transition owns membership,
   block, length, duplicate and bounded-history invariants."
  [state {:message/keys [id room from text moderation] :as message}]
  (let [r (get-in state [:rooms room])
        members (:room/members r)]
    (cond
      (or (nil? id) (nil? r) (nil? from) (not (string? text)) (empty? text))
      [:error :invalid-message state]
      (not (contains? members from)) [:error :not-room-member state]
      (some #(blocked? state from %) (disj members from)) [:error :blocked state]
      (> (count text) (:room/max-length r)) [:error :message-too-long state]
      (= moderation :rejected) [:error :moderation-rejected state]
      (some #(= id (:message/id %)) (:room/messages r)) [:duplicate state]
      :else
      (let [messages (->> (conj (:room/messages r) message)
                          (take-last (:room/history-limit r)) vec)]
        [:ok message (assoc-in state [:rooms room :room/messages] messages)]))))
