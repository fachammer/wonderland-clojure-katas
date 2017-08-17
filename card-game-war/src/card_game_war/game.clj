(ns card-game-war.game
  (:require [clojure.set :refer [difference]]))

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (set (for [suit suits
             rank ranks]
         {:suit suit :rank rank}) ))

(defn rank-value [rank]
  (.indexOf ranks rank))

(defn suit-value [suit]
  (.indexOf suits suit))

(defn play-round
  "returns 0 when the first player's card wins, 1 when the second player's card
wins, :same-card-not-allowed if both cards are the same"
  [{player1-rank :rank player1-suit :suit :as player1-card}
   {player2-rank :rank player2-suit :suit :as player2-card}]
  (let [player1-rank-value (rank-value player1-rank)
        player2-rank-value (rank-value player2-rank)
        player1-suit-value (suit-value player1-suit)
        player2-suit-value (suit-value player2-suit)]
    (cond
      (> player1-rank-value player2-rank-value) {:round-winner :player1
                                                 :player1-card player1-card
                                                 :player2-card player2-card
                                                 :reason :higher-rank}
      (< player1-rank-value player2-rank-value) {:round-winner :player2
                                                 :player1-card player1-card
                                                 :player2-card player2-card
                                                 :reason :higher-rank}
      (> player1-suit-value player2-suit-value) {:round-winner :player1
                                                 :player1-card player1-card
                                                 :player2-card player2-card
                                                 :reason :higher-suit}
      (< player1-suit-value player2-suit-value) {:round-winner :player2
                                                 :player1-card player1-card
                                                 :player2-card player2-card
                                                 :reason :higher-suit}
      :else                                     :same-card-not-allowed)))

(defn random-subset [coll n]
  (take n (shuffle coll)))

(defn play-game
  ([[player1-round-card :as player1-cards]
    [player2-round-card :as player2-cards]
    history]
   (cond
     (empty? player2-cards) (conj history {:winner :player1})
     (empty? player1-cards) (conj history {:winner :player2})
     :else                  (let [{round-winner :round-winner
                                   :as          round} (play-round player1-round-card
                                                                   player2-round-card)
                                  remaining-player1-cards (subvec player1-cards 1)
                                  remaining-player2-cards (subvec player2-cards 1)]
                              (cond
                                (= :player1 round-winner)
                                (let [player1-deck (conj remaining-player1-cards
                                                         player1-round-card
                                                         player2-round-card)
                                      entry        (merge round {:player1-deck player1-deck
                                                                 :player2-deck remaining-player2-cards})]
                                  (recur player1-deck
                                         remaining-player2-cards
                                         (conj history entry)))
                                (= :player2 round-winner)
                                (let [player2-deck (conj remaining-player2-cards
                                                         player1-round-card
                                                         player2-round-card)
                                      entry        (merge round {:player1-deck remaining-player1-cards
                                                                 :player2-deck player2-deck})]
                                  (recur remaining-player1-cards
                                         player2-deck
                                         (conj history entry)))
                                :else {:error   :invalid-deck
                                       :history history}))))
  ([player1-cards player2-cards]
   (play-game player1-cards player2-cards [{:player1-deck player1-cards
                                            :player2-deck player2-cards}]))
  ([deck]
   (let [player1-cards (vec (random-subset deck (/ (count deck) 2)))
         player2-cards (vec (difference deck player1-cards))]
     (play-game player1-cards player2-cards))))

(def suit->pretty-str {:spade   "\u2660"
                      :club    "\u2663"
                      :diamond "\u2666"
                      :heart   "\u2665"})

(defn rank->pretty-str [rank]
  (or ({:jack  "J"
        :queen "Q"
        :king  "K"
        :ace   "A"} rank) rank))

(def player->pretty-str {:player1 "Player 1" :player2 "Player 2"})

(def reason->pretty-str {:higher-rank "higher rank" :higher-suit "highter suit"})

(defn card->pretty-str [{suit :suit rank :rank}]
  (str (suit->pretty-str suit) (rank->pretty-str rank)))

(defn deck->pretty-str [deck]
  (apply str (interpose " " (map card->pretty-str deck))))

(defn history-entry->pretty-str [{player1-card :player1-card
                                 player2-card :player2-card
                                 winner       :round-winner
                                 reason       :reason
                                 total-winner :winner
                                 player1-deck :player1-deck
                                 player2-deck :player2-deck}]
  (cond
    total-winner (str (player->pretty-str total-winner) " wins the game")
    (not winner) (str  "P1 deck: "  (deck->pretty-str player1-deck) "\n"
                       "P2 deck: " (deck->pretty-str player2-deck))
    :else        (str "P1: " (card->pretty-str player1-card)
                      " vs "
                      (card->pretty-str player2-card) " :P2"
                      " => " (player->pretty-str winner) " wins round"
                      " because of " (reason->pretty-str reason) ".\n"
                      "P1 deck: " (deck->pretty-str player1-deck) "\n"
                      "P2 deck: " (deck->pretty-str player2-deck))))

(defn game->pretty-str [history]
  (apply str (interpose "\n" (map history-entry->pretty-str history))))

(defn pretty-print-game [history]
  (println (game->pretty-str history)))

(defn run-game [deck]
  ())
