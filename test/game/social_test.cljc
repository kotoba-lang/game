(ns game.social-test
  (:require [clojure.test :refer [deftest is]]
            [game.social :as social]))

(deftest saves-are-revisioned
  (let [[ok save s] (social/put-save (social/platform-state)
                                     {:player "did:p1" :game "g/a" :expected-rev 0
                                      :data {:level 2} :updated-at 10})]
    (is (= :ok ok))
    (is (= 1 (:save/rev save)))
    (is (= :revision-conflict
           (second (social/put-save s {:player "did:p1" :game "g/a" :expected-rev 0
                                       :data {} :updated-at 11}))))))

(deftest wallet-is-idempotent-and-non-negative
  (let [[_ credited] (social/apply-transaction (social/wallet)
                                               #:tx{:id "award-1" :currency :free-gem
                                                    :amount 20 :kind :award})]
    (is (= 20 (social/balance credited :free-gem)))
    (is (= :duplicate (first (social/apply-transaction credited
                                                       #:tx{:id "award-1" :currency :free-gem
                                                            :amount 20}))))
    (is (= :insufficient-funds
           (second (social/apply-transaction credited
                                             #:tx{:id "buy" :currency :free-gem :amount -21}))))))

(deftest purchase-is-atomic
  (let [p #:product{:id :hat :currency :free-gem :price 30
                    :grants [#:item{:id :hat :quantity 1}]}
        [_ w] (social/apply-transaction (social/wallet)
                                        #:tx{:id "seed" :currency :free-gem :amount 50})
        state (assoc-in (social/platform-state) [:wallets "did:p1"] w)
        [ok receipt bought] (social/purchase state {:player "did:p1" :receipt-id "r1"
                                                    :product p :purchased-at 12})]
    (is (= :ok ok))
    (is (= :hat (:receipt/product receipt)))
    (is (= 20 (social/balance (get-in bought [:wallets "did:p1"]) :free-gem)))
    (is (social/owned? (get-in bought [:inventories "did:p1"]) :hat))
    (is (= :duplicate (first (social/purchase bought {:player "did:p1" :receipt-id "r1"
                                                       :product p :purchased-at 13}))))))

(deftest paid-gem-payment-and-refund-lifecycle
  (let [pack #:pack{:id :gem-100 :gems 100 :fiat-currency :jpy :fiat-amount 160}
        [_ intent s] (social/create-payment-intent
                      (social/platform-state)
                      {:intent-id "pi-1" :player "did:p1" :pack pack :created-at 10})]
    (is (= :pending (:payment/status intent)))
    (is (= :duplicate (first (social/create-payment-intent
                              s {:intent-id "pi-1" :player "did:p1"
                                 :pack pack :created-at 11}))))
    (is (= :payment-unverified
           (second (social/settle-payment s {:intent-id "pi-1" :provider-ref "provider-1"
                                             :verified? false :settled-at 12}))))
    (let [[_ settled paid] (social/settle-payment
                            s {:intent-id "pi-1" :provider-ref "provider-1"
                               :verified? true :settled-at 12})]
      (is (= :settled (:payment/status settled)))
      (is (= 100 (social/balance (get-in paid [:wallets "did:p1"]) :paid-gem)))
      (is (= :duplicate (first (social/settle-payment
                                paid {:intent-id "pi-1" :provider-ref "provider-1"
                                      :verified? true :settled-at 13}))))
      (let [[_ refund refunded] (social/refund-payment
                                 paid {:refund-id "re-1" :intent-id "pi-1"
                                       :provider-ref "provider-refund-1"
                                       :verified? true :refunded-at 14})]
        (is (= 100 (:refund/gems refund)))
        (is (zero? (social/balance (get-in refunded [:wallets "did:p1"]) :paid-gem)))
        (is (= :refunded (get-in refunded [:payment-intents "pi-1" :payment/status])))
        (is (= :duplicate (first (social/refund-payment
                                  refunded {:refund-id "re-1" :intent-id "pi-1"
                                            :provider-ref "provider-refund-1"
                                            :verified? true :refunded-at 15})))))))
  (let [pack #:pack{:id :gem-10 :gems 10 :fiat-currency :usd :fiat-amount 99}
        [_ _ s] (social/create-payment-intent (social/platform-state)
                                              {:intent-id "pi-spent" :player "did:p2"
                                               :pack pack :created-at 1})
        [_ _ s] (social/settle-payment s {:intent-id "pi-spent" :provider-ref "pay"
                                          :verified? true :settled-at 2})
        [_ wallet] (social/apply-transaction (get-in s [:wallets "did:p2"])
                                             #:tx{:id "spend" :currency :paid-gem
                                                  :amount -1 :kind :purchase})
        s (assoc-in s [:wallets "did:p2"] wallet)]
    (is (= :refund-balance-spent
           (second (social/refund-payment s {:refund-id "re-spent" :intent-id "pi-spent"
                                             :provider-ref "refund" :verified? true
                                             :refunded-at 3}))))))

(deftest daily-free-gem-reward-is-verified-idempotent-and-streaked
  (let [claim (fn [state day at]
                (social/claim-daily-reward state {:player "did:p1" :day day
                                                  :schedule [10 20 30]
                                                  :verified? true :claimed-at at}))
        [_ first-day s] (claim (social/platform-state) 100 1)]
    (is (= {:reward/day 100 :reward/streak 1 :reward/currency :free-gem
            :reward/amount 10 :reward/claimed-at 1} first-day))
    (is (= :duplicate (first (claim s 100 2))))
    (let [[_ second-day s] (claim s 101 3)]
      (is (= [2 20] [(:reward/streak second-day) (:reward/amount second-day)]))
      (let [[_ reset-day s] (claim s 103 4)]
        (is (= [1 10] [(:reward/streak reset-day) (:reward/amount reset-day)]))
        (is (= 40 (social/balance (get-in s [:wallets "did:p1"]) :free-gem)))
        (is (= :stale-reward-day (second (claim s 102 5)))))))
  (is (= :daily-reward-unverified
         (second (social/claim-daily-reward
                  (social/platform-state)
                  {:player "did:p1" :day 100 :verified? false :claimed-at 1})))))

(deftest ranking-and-chat-contracts
  (let [b (social/leaderboard #:board{:id :weekly :game :drive :season "2026-W29"})
        [_ b] (social/submit-score b #:score{:submission "s1" :player "did:p1"
                                             :value 10 :verified true})
        [_ b] (social/submit-score b #:score{:submission "s2" :player "did:p1"
                                             :value 8 :verified true})]
    (is (= 10 (:score/value (first (social/standings b)))))
    (is (= :unverified (second (social/submit-score b #:score{:submission "bad"
                                                               :player "did:p2" :value 99
                                                               :verified false})))))
  (let [p (assoc (social/player-profile "did:to" "To" 1) :player/blocked #{"did:from"})
        state (assoc-in (social/platform-state) [:profiles "did:to"] p)]
    (is (= :blocked
           (second (social/admit-chat state #:message{:id "m1" :from "did:from"
                                                       :to "did:to" :text "hi"
                                                       :moderation :approved}
                                      #:room{:max-length 20}))))))

(deftest royalty-contract
  (is (= [["parent" 10] ["root" 5]]
         (social/royalties 100 ["parent" "root"]))))

(deftest friendship-block-and-group-contract
  (let [s (social/platform-state)
        [_ s] (social/request-friend s {:request-id "r1" :from "a" :to "b" :at 1})
        [_ s] (social/answer-friend s {:request-id "r1" :player "b" :accept? true :at 2})]
    (is (contains? (:friendships s) ["a" "b"]))
    (let [[_ blocked] (social/block-player s "a" "b" 3)]
      (is (social/blocked? blocked "a" "b"))
      (is (empty? (:friendships blocked)))
      (is (= :blocked (second (social/request-friend blocked
                                                     {:request-id "r2" :from "b" :to "a" :at 4}))))))
  (let [[_ s] (social/create-group (social/platform-state)
                                   #:group{:id "p1" :kind :party :owner "a" :capacity 2})
        [_ s] (social/join-group s "p1" "b" :member)]
    (is (= {"a" :owner "b" :member} (get-in s [:groups "p1" :group/members])))
    (is (= :group-full (second (social/join-group s "p1" "c" :member))))))

(deftest room-chat-is-member-only-moderated-and-bounded
  (let [[_ s] (social/create-room (social/platform-state)
                                  #:room{:id "room" :kind :party :members #{"a" "b"}
                                         :max-length 10 :history-limit 2})
        msg (fn [id from text] #:message{:id id :room "room" :from from :text text
                                         :moderation :approved})
        [_ _ s] (social/admit-room-message s (msg "m1" "a" "one"))
        [_ _ s] (social/admit-room-message s (msg "m2" "b" "two"))
        [_ _ s] (social/admit-room-message s (msg "m3" "a" "three"))]
    (is (= ["m2" "m3"] (mapv :message/id (get-in s [:rooms "room" :room/messages]))))
    (is (= :not-room-member (second (social/admit-room-message s (msg "m4" "c" "hi")))))
    (is (= :message-too-long (second (social/admit-room-message s (msg "m5" "a" "way too long")))))
    (is (= :moderation-rejected
           (second (social/admit-room-message s (assoc (msg "m6" "a" "no")
                                                       :message/moderation :rejected)))))))

(deftest season-ranking-lifecycle-ties-and-rewards
  (let [season (social/season #:season{:id "s1" :starts-at 10 :ends-at 20
                                       :reward-tiers [{:through 1 :reward {:free-gem 100}}
                                                      {:through 3 :reward {:free-gem 25}}]})
        board (social/leaderboard #:board{:id :weekly :game :drive :season "s1"})
        score (fn [id player value at]
                #:score{:submission id :player player :value value :at at :verified true})]
    (is (= :scheduled (social/season-status season 9)))
    (is (= :season-not-active
           (second (social/submit-season-score board season 9 (score "x" "a" 9 9)))))
    (let [[_ b] (social/submit-season-score board season 12 (score "a" "a" 10 12))
          [_ b] (social/submit-season-score b season 13 (score "b" "b" 10 13))
          [_ b] (social/submit-season-score b season 14 (score "c" "c" 8 14))
          ranked (social/ranked-standings b)]
      (is (= [["a" 1] ["b" 1] ["c" 3]]
             (mapv (juxt :score/player :score/rank) ranked)))
      (is (= :season-not-closed (second (social/close-season b season 19))))
      (let [[ok snapshot] (social/close-season b season 20)]
        (is (= :ok ok))
        (is (= {"a" {:free-gem 100} "b" {:free-gem 100} "c" {:free-gem 25}}
               (:season/rewards snapshot)))))))

(deftest platform-hud-projects-provider-snapshot
  (let [hud (social/platform-hud-model
             {:did "did:me"
              :balances [{:currency "free-gem" :balance 80}
                         {:currency :paid-gem :balance 5}]
              :transactions [{:id "tx1"}]
              :inventory [{:item "hat" :quantity 1 :entitlement 0}
                          {:item "skin" :quantity 0 :entitlement 1}
                          {:item "spent" :quantity 0 :entitlement 0}]
              :products [{:id "cheap" :currency "free-gem" :price 30}
                         {:id "premium" :currency "paid-gem" :price 10}]
              :gem-packs [{:id "gem-100" :gems 100}]
              :payments [{:id "pi-1" :status "pending"}]
              :server-day 101
              :daily-reward {:day 100 :streak 2 :amount 10}
              :friendships [{:a "did:me" :b "did:friend"}]
              :friend-requests [{:id "in" :sender "did:new" :recipient "did:me"
                                 :status "pending" :created_at 1}
                                {:id "out" :sender "did:me" :recipient "did:other"
                                 :status "pending" :created_at 2}]
              :groups [{:id "guild"}]
              :group-members [{:group_id "guild" :player "did:friend"
                               :role "member" :joined_at 2}
                              {:group_id "guild" :player "did:me"
                               :role "owner" :joined_at 1}]})]
    (is (= {:free-gem 80 :paid-gem 5} (:wallet/balances hud)))
    (is (:wallet/daily-claimable? hud))
    (is (= 2 (get-in hud [:wallet/daily-reward :streak])))
    (is (= ["hat" "skin"] (mapv :item (:inventory/items hud))))
    (is (= [false true] (mapv :entitlement (:inventory/items hud))))
    (is (= [true false] (mapv :affordable? (:store/products hud))))
    (is (= ["gem-100"] (mapv :id (:store/gem-packs hud))))
    (is (= ["pi-1"] (mapv :id (:store/payments hud))))
    (is (= ["did:friend"] (:social/friends hud)))
    (is (= ["in"] (mapv :id (:social/pending-in hud))))
    (is (= ["did:me" "did:friend"]
           (mapv :player (get-in hud [:social/group-members "guild"]))))
    (is (= [1 2 3 2 1] (mapv :tab/count (:hud/tabs hud))))))

(deftest friendship-and-group-lifecycle
  (let [base (social/platform-state)
        [_ s] (social/request-friend base {:request-id "r" :from "a" :to "b" :at 1})
        [_ s] (social/answer-friend s {:request-id "r" :player "b" :accept? true :at 2})
        [_ s] (social/remove-friend s "a" "b")]
    (is (empty? (:friendships s)))
    (is (empty? (:friend-requests s)))
    (is (= :duplicate (first (social/remove-friend s "a" "b")))))
  (let [[_ s] (social/create-group (social/platform-state)
                                   #:group{:id "g" :kind :guild :owner "a" :capacity 4})
        [_ s] (social/join-group s "g" "b" :member)
        [_ s] (social/join-group s "g" "c" :member)]
    (is (= :owner-transfer-required (second (social/leave-group s "g" "a"))))
    (let [[_ s] (social/change-group-role s "g" "a" "b" :moderator)]
      (is (= :insufficient-group-role
             (second (social/remove-group-member s "g" "c" "b"))))
      (let [[_ s] (social/remove-group-member s "g" "b" "c")
            [_ s] (social/transfer-group-owner s "g" "a" "b")]
        (is (= "b" (get-in s [:groups "g" :group/owner])))
        (is (= {:moderator "a" :owner "b"}
               (into {} (map (fn [[player role]] [role player])
                             (get-in s [:groups "g" :group/members])))))
        (is (= :not-group-owner
               (second (social/change-group-role s "g" "a" "b" :member))))
        (let [[_ s] (social/leave-group s "g" "a")]
          (is (= {"b" :owner} (get-in s [:groups "g" :group/members]))))))))
