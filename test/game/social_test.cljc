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
