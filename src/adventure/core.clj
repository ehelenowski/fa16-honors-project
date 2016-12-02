(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {

    :foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created
for a game or something."
           :title "in the foyer"
           :dir {:south :grue-pen, :north :Dough-House}
           :contents #{:raw-egg}}

  :grue-pen {:desc "It is very dark.  You are about to be eaten by a grue."
              :title "in the grue pen"
              :dir {:north :foyer}
              :contents #{:very-sharp-pencil}}

  :Dough-House {:desc "It smells nice in here, almost like a pillsbury dough boy's armpit."
              :title "In the dough House"
              :dir {:down :Basement, :up :Attic, :east :Lounge , :north :Attic }
              :contents #{:cinnabon, :knife }}


  :Basement {:desc "It is dark and cold in here. You desperately need to pee, and want to leave."
              :title "In the dungeon"
              :dir {:east :Billiard-Room, :down :Dough-House, :up :Attic}

              :contents #{:cobwebs, :a-rusty-screw}}



 :Attic {:desc "It's very small and cramped up here... What's that smell?"

             :title "In the Attic"
             :dir {:east :Dining-Room, :west :Hall, :down :Dough-House, :up :Attic }
             :contents #{:lamp, :key}}


 :Hall {:desc "A well lit hallway, there are many doors that lead to unknown locations, which route will you take?"

             :title "In the Attic"
             :dir {:east :Dining-Room, :up :Attic, :west :Storage-Room, :down :Basement, :north :Office-Room, :south :Guest-Room   }
}

 :Storage-Room {:desc "Lots of miscellaneous items in this room, it only has one other door besides the entrance though."
            :title "In the Storage-Room"
            :dir {:east :Hall , :up :Fun-House }
            :contents #{:cork, :tweezers, :baseball, :phallic-object}}


 :Dining-Room {:desc "The livestock seems to be running free in the Dining Room. A chicken decides to pounce on your face and begins scratching your eye balls. What would you like to do?"
             :title "In the Dining-Room"
             :dir {:east :Kitchen, :down :Dough-House, :up :Attic, :west :Ballroom}
             :contents #{:plate}}

 :Ballroom {:desc "Soft music plays in the background. You sit down on the plush couch to rest... You are rudely awakened by a piano that seems to be barelling towards your face, what woudl you like to do?"

             :title "In the Ballroom"
             :dir {:east :Hall, :down :Storage-Room, :up :grue-pen}
             :contents #{:vinyl-record}}

 :Kitchen {:desc "More livestock running around here... the chef is wrestling with a rat in his hair and pots and pans seem to be flying about the room"

             :title "In the Kitchen"
             :dir {:west :Dining-Room, :north :Hall, :down :foyer}
             :contents #{:turkey}}

  :Billiard-Room {:desc "An empty room with a pool table in the center...Balls and sticks are strewn all over, it eerily reminds you of home"
            :title "Does anyone really play this game seriously?"
            :dir {:north :Fun-House, :south :Dining-Room, :east :Office-Room }
             :contents #{:balls, :sticks}}

 :Lounge {:desc "A place to chill out and relax....finally....."
            :title "You fell asleep for 5 seconds. Unfortunately you are now 5 weeks behind in CS. rip"
            :dir {:east :Dining-Room,  :up :Attic, :west :Storage-Room, :down :Fun-House, :north :Office-Room, :south :Guest-Room }}

 :Office-Room {:desc "Don't pretend like you're about to study, we all know netflix is your true calling. Proceed forth to enter the promised land"
            :dir {:north :Fun-House}}

 :Guest-Room {:desc "A nice stranger invites you into the guest room. He lets you sleep there for the night"
            :title "Lesson Number 1, trust no one."
            :dir {:east :foyer , :south :foyer , :down :foyer , :up :foyer ,  :west :foyer , :north :foyer }
            :contents #{:cyanide, :happiness}}


 :Fun-House {:desc "At long last you have reached the end of the line. Two strange men wearing rainbow turbans wait with their arms crossed at the opposite end of the room. They look suspicious...but you approach them eager to exit the nightmare"

            :title "Lesson Number 2, what the hell is a rainbow turban?"}
})

(def adventurer
  {:location :foyer
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
          [:south] (go :south player)
          [:east] (go :east player)
          [:west] (go :west player)
         [:up] (go :up player)
          [:down] (go :down player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
