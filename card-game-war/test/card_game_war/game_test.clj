(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(defmacro def-suit-fn [suit]
  `(defn ~(symbol (name suit)) [rank#] {:suit ~suit, :rank rank#}))

(def-suit-fn :spade)
(def-suit-fn :club)
(def-suit-fn :diamond)
(def-suit-fn :heart)

(defn round [player1-card player2-card]
  {:player1 {:played-card player1-card}
   :player2 {:played-card player2-card}})

(defn wins-round? [player round-state]
  (= player (:round-winner round-state)))

(defn player1-wins-round [player1-card player2-card]
  (is (wins-round? :player1 (play-round (round player1-card player2-card)))))

(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (wins-round? :player1 (play-round (round (spade 3) (spade 2)))))
    (is (wins-round? :player2  (play-round (round (spade 2) (spade 3))))))
  (testing "queens are higher rank than jacks"
    (player1-wins-round (spade :queen) (spade :jack)))
  (testing "kings are higher rank than queens"
    (player1-wins-round (spade :king) (spade :queen)))
  (testing "aces are higher rank than kings"
    (player1-wins-round (spade :ace) (spade :king)))
  (testing "if the ranks are equal, clubs beat spades"
    (player1-wins-round (club 2) (spade 2)))
  (testing "if the ranks are equal, diamonds beat clubs"
    (player1-wins-round (diamond 2) (club 2)))
  (testing "if the ranks are equal, hearts beat diamonds"
    (player1-wins-round (heart 2) (diamond 2))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= :player1 (:winner (last (play-game {:player1 {:deck [(spade 2) (spade 3)]}
                                               :player2  {:deck []}})))))))
