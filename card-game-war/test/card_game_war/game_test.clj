(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

;; fill in tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= 0 (play-round {:suit :spade, :rank 3} {:suit :spade, :rank 2})))
    (is (= 1 (play-round {:suit :spade, :rank 2} {:suit :spade, :rank 3}))))
  (testing "queens are higher rank than jacks"
    (is (= 0 (play-round {:suit :spade, :rank :queen}
                         {:suit :spade, :rank :jack}))))
  (testing "kings are higher rank than queens"
    (is (= 0 (play-round {:suit :spade, :rank :king}
                         {:suit :spade, :rank :queen}))))
  (testing "aces are higher rank than kings"
    (is (= 0 (play-round {:suit :spade, :rank :ace}
                         {:suit :spade, :rank :king}))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= 0 (play-round {:suit :club, :rank 2}
                         {:suit :spade, :rank 2}))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= 0 (play-round {:suit :diamond, :rank 2}
                         {:suit :club, :rank 2}))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= 0 (play-round {:suit :heart, :rank 2}
                         {:suit :diamond, :rank 2})))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"))
