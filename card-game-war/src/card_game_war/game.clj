(ns card-game-war.game
  (:require [clojure.set :refer [difference]]))

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
  [{{{player1-rank :rank player1-suit :suit :as player1-card} :played-card} :player1
    {{player2-rank :rank player2-suit :suit :as player2-card} :played-card} :player2
    :as round-state}]
  (let [player1-rank-value (rank-value player1-rank)
        player2-rank-value (rank-value player2-rank)
        player1-suit-value (suit-value player1-suit)
        player2-suit-value (suit-value player2-suit)]
    (merge round-state
           (cond
             (> player1-rank-value player2-rank-value) {:round-winner :player1
                                                        :win-reason :higher-rank}
             (< player1-rank-value player2-rank-value) {:round-winner :player2
                                                        :win-reason :higher-rank}
             (> player1-suit-value player2-suit-value) {:round-winner :player1
                                                        :win-reason :higher-suit}
             (< player1-suit-value player2-suit-value) {:round-winner :player2
                                                        :win-reason :higher-suit}
             :else                                     {:error :same-card-not-allowed}))))

(defn random-subset [coll n]
  (set (take n (shuffle coll))))

(def players #{:player1 :player2})

(defn other-player [player]
  (first (difference players #{player})))

(defn next-game-state
  [{{player1-deck :deck} :player1
    {player2-deck :deck} :player2
    round                :round
    :as                  game-state}]
  (cond
    (empty? player2-deck) (merge game-state {:winner :player1})
    (empty? player1-deck) (merge game-state {:winner :player2})
    :else                 (let [player1-card (first player1-deck)
                                player2-card (first player2-deck)
                                {:keys [round-winner error] :as round-state}
                                (play-round {:player1 {:played-card player1-card}
                                             :player2 {:played-card player2-card}})]
                            (if error round-state
                                (let [round-loser  (other-player round-winner)
                                      winning-deck (:deck (round-winner game-state))
                                      losing-deck  (:deck (round-loser game-state))]
                                  (merge-with merge round-state
                                              {round-winner {:deck (conj (subvec winning-deck 1)
                                                                         player1-card
                                                                         player2-card)}
                                               round-loser  {:deck (subvec losing-deck 1)}
                                               :round       (inc (or round 0))}
                                              (when (>= 1 (count losing-deck))
                                                {:winner round-winner})))))))

(defn take-until-inclusive [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [x (first s)]
       (cons x (if-not (pred x) (take-until-inclusive pred (rest s))))))))

(defn play-game [game-state]
  (take-until-inclusive :winner (iterate next-game-state game-state)))

(defn play-random-game [deck]
  (let [player1-cards (vec (random-subset (set deck) (/ (count deck) 2)))
        player2-cards (vec (difference (set deck) player1-cards))]
    (play-game {:player1 {:deck player1-cards}
                :player2 {:deck player2-cards}})))

(def suit->pretty-str {:spade   "\u2660"
                      :club    "\u2663"
                      :diamond "\u2666"
                      :heart   "\u2665"})

(defn rank->pretty-str [rank]
  (or ({:jack  "J"
        :queen "Q"
        :king  "K"
        :ace   "A"
        10     "T"} rank) rank))

(def player->pretty-str {:player1 "Player 1" :player2 "Player 2"})

(def reason->pretty-str {:higher-rank "higher rank" :higher-suit "highter suit"})

(defn card->pretty-str [{suit :suit rank :rank}]
  (str (suit->pretty-str suit) (rank->pretty-str rank)))

(defn deck->pretty-str [deck]
  (apply str (interpose " " (map card->pretty-str deck))))

(defn player-decks->pretty-str [player1-deck player2-deck]
  (str "P1 deck: " (deck->pretty-str player1-deck) "\n"
       "P2 deck: " (deck->pretty-str player2-deck) "\n"))

(defn game-state->pretty-str [{{player1-deck :deck
                               player1-card :played-card} :player1
                              {player2-deck :deck
                               player2-card :played-card} :player2
                              round-winner                :round-winner
                              round                       :round
                              win-reason                  :win-reason
                              total-winner                :winner}]
  (if (not round-winner)
    (str "Game starts!" "\n" (player-decks->pretty-str player1-deck player2-deck))
    (str "Round " round ":" "\n"
         "P1 " (card->pretty-str player1-card)
         " vs "
         (card->pretty-str player2-card) " P2"
         " => " (player->pretty-str round-winner) " wins round"
         " because of " (reason->pretty-str win-reason)"\n"
        (player-decks->pretty-str player1-deck player2-deck)
        (when total-winner (str (player->pretty-str total-winner) " wins the game!")))))

(defn pretty-print-game-state! [game-state]
  (print (game-state->pretty-str game-state)))

(defn game-of-war-repl
  ([] (game-of-war-repl cards))
  ([deck]
   (loop [[current-state & rest] (play-random-game deck)
          prev                   nil]
     (pretty-print-game-state! current-state)
     (when rest
       (println "\nPress enter to go to next round or enter 'quit' to return the current game state...\n"))
     (if (or (not rest) (= "quit" (read-line)))
       current-state
       (recur rest current-state)))))
