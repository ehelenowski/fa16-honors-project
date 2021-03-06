(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {
    :foyer {:desc "The walls are freshly painted but do not have any pictures. You get the feeling it was just created for a game or something. "
           :title "in the foyer"
           :dir {:south :grue-pen, :north :Dough-House}
           :contents #{"raw-egg"}}

  :grue-pen {:desc "It is very dark. You are about to be eaten by a grue. "
              :title "in the grue pen"
              :dir {:north :foyer}
              :contents #{"phallic-object"}}

  :Dough-House {:desc "It smells nice in here, almost like a pillsbury dough boy's armpit. "
              :title "in the dough House"
              :dir {:down :Basement, :up :Attic, :east :Lounge , :north :Attic }
              :contents #{"cinnabon", "knife" }}

  :Basement {:desc "It is dark and cold in here. You desperately need to pee, and want to leave. "
              :title "in the dungeon"
              :dir {:east :Billiard-Room, :down :Dough-House, :up :Attic}
              :contents #{"cobwebs", "a-rusty-screw"}}

 :Attic {:desc "It's very small and cramped up here... What's that smell? "
             :title "in the Attic"
             :dir {:east :Dining-Room, :west :Hall, :down :Dough-House, :up :Attic }
             :contents #{"lamp", "waginer"}}

 :Hall {:desc "A well lit hallway, there are many doors that lead to unknown locations, which route will you take? "
             :title "in the Hall"
             :dir {:east :Dining-Room, :up :Attic, :west :Storage-Room, :down :Basement, :north :Office-Room, :south :Guest-Room }}

 :Storage-Room {:desc "Lots of miscellaneous items in this room, it only has one other door besides the entrance though. "
            :title "in the Storage-Room"
            :dir {:east :Hall , :up :Fun-House }
            :contents #{"cork", "tweezers", "baseball", "very-sharp-pencil"}}


 :Dining-Room {:desc "The livestock seems to be running free in the Dining Room. A chicken decides to pounce on your face and begins scratching your eye balls. What would you like to do? "
             :title "in the Dining-Room"
             :dir {:east :Kitchen, :down :Dough-House, :up :Attic, :west :Ballroom}
             :contents #{"plate"}}

 :Ballroom {:desc "Soft music plays in the background. You sit down on the plush couch to rest... You are rudely awakened by a piano that seems to be barelling towards your face, what woudl you like to do? "
             :title "in the Ballroom"
             :dir {:east :Hall, :down :Storage-Room, :up :grue-pen}
             :contents #{"vinyl-record"}}

 :Kitchen {:desc "More livestock running around here... the chef is wrestling with a rat in his hair and pots and pans seem to be flying about the room. "
             :title "in the Kitchen"
             :dir {:west :Dining-Room, :north :Hall, :down :foyer}
             :contents #{"turkey"}}

  :Billiard-Room {:desc "Does anyone really play this game seriously? An empty room with a pool table in the center...Balls and sticks are strewn all over, it eerily reminds you of home. "
            :title "in the Billard Room"
            :dir {:north :Fun-House, :south :Dining-Room, :east :Office-Room }
             :contents #{:balls, :sticks}}
 :Lounge {:desc "You fell asleep for 5 seconds. Unfortunately you are now 5 weeks behind in CS. rip. A place to chill out and relax....finally..... "
            :title "in the Lounge. "
            :dir {:east :Dining-Room,  :up :Attic, :west :Storage-Room, :down :Fun-House, :north :Office-Room, :south :Guest-Room }}

 :Office-Room {:desc "Don't pretend like you're about to study, we all know netflix is your true calling. Proceed forth to enter the promised land. "
            :title "in the Office Room"
            :dir {:north :Fun-House}}

 :Guest-Room {:desc "Lesson Number 1, trust no one. A nice stranger invites you into the guest room. He lets you sleep there for the night. "
            :title "in the Guest Room, sucker"
            :dir {:east :foyer , :south :foyer , :down :foyer , :up :foyer ,  :west :foyer , :north :foyer }
            :contents #{"cyanide", "happiness"}}

 :Fun-House {:desc "At long last you have reached the end of the line. Two strange men wearing rainbow turbans wait with their arms crossed at the opposite end of the room. They look suspicious...but you approach them eager to exit the nightmare. They tell you to drop all your items from your inventory. Now type in puzzle."
            :title "... in the fun house! Ba dum dum!!! "
            :dir {:skin :Escape :north :foyer}
            :puzzle [
                    {:question "The jailer seats three of the men into a line. The fourth man is put behind a screen (or in a separate room). He gives all four men party hats. The jailer explains that there are two black hats and two white hats, that each prisoner is wearing one of the hats, and that each of the prisoners see only the hats in front of him but neither on himself nor behind him. The fourth man behind the screen can't see or be seen by any other prisoner. No communication among the prisoners is allowed.

If any prisoner can figure out what color hat he has on his own head with 100% certainty (without guessing) and tell the jailer, all four prisoners go free. If any prisoner suggests an incorrect answer, all four prisoners are executed. Which prisoner can say with absolute certainty what the color of his hat is? (Back) (Middle) (Front) (Wall-Side)"
                    :answer "middle"}

              ]
          }

 :Escape {:desc " You made it!! Congratulations. Now go out there and make something out of your life. (Type Leave)"
            :title "The light at the end of the tunnel"
            :contents #{"A-CS-Degree"}
            :dir {:leave :Dead}}

 :Dead {}
})

(defn give_puzzle [n puz]
  (let [num (rand-int n)
       ques (get-in puz [num :question])
       ans (get-in puz [num :answer])
       _ (println (str ques))
       answer (read-line)]
  (if (= answer ans)true false)))

(defn puzzle [player]
    (let [curr-room (get-in player [:location])
          puz (get-in the-map [curr-room :puzzle])
          inv (get-in player [:inventory])]
    (if (nil? puz)
        (if (inv "skin")(do (println "Just leave already.")player)player)
        (if (give_puzzle (count puz) puz)
          (let [cont (get-in the-map [curr-room :content])]
              (if (inv "skin") (do (println "You already have the skin") player)
                                    (do (println "You did it! Check your inventory, and type in the recieved item to proceed.")(update-in player [:inventory] #(conj % "skin")))))
              (if (inv "skin") (do (println "leaaaveeeeee.")player)
                              (do (println "WRONG.")player))
         ))))




(defn help [player]
  (println "\nCommands:")
  (println "look- repeats description")
  (println "north / n- go north")
  (println "south / s- go south")
  (println "east / e- go east")
  (println "west / w- go west")
  (println "up / u- go up")
  (println "down / d- go down")
  (println "run - picks random direction")
  (println "grab [item]- pick up an item")
  (println "drop [item]- drop an item")
  (println "tock- increases the numbers of the player's ticks")
  (println "show ticks- shows the number of the player's ticks")
  (println "show inventory- shows the items in the player's possesion")
  (println "look around- displays contents of inventory")
  (println "display- same as look around")
  (println "help / h - displays all commands\n")
  player
  )

(defn display [player]
  (let [location (player :location)]
  (do (println (name (str (-> the-map location :contents) "\n"))) player)))

(defn tock [player]
  (update-in player [:tick] inc))

(defn show-ticks [player]
  (println (-> player :tick)) player)

(defn show-inventory [player]
  (println (-> player :inventory)) player)

(defn grab [player object]
  (let [current_room (-> player :location)
        contents ((-> the-map current_room) :contents)
        inventory (-> player :inventory)]
        (if (contents (name object))
          (if (inventory (name object)) (do (println (str "You can't pick up another " (name object) "! Stop being so greedy! ")) player)
          (do (println (str "You have grabbed a(n) " (name object))) (update-in player [:inventory] #(conj % (name object)))))
          (do (println "That object doesn't exist in this room, ya dummy! ") player))))


 (defn toss [player object]
  (let [inventory (-> player :inventory)]
    (if (inventory (name object)) (do (println (str "You have dropped a(n) " (name object) "\n"))(update-in player [:inventory] #(disj %(name object))))
                                  (do (println "You don't any of this item\n") player))))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
  (if (nil? dest)
  (do (println "You can't go that way.") player)
  (assoc-in player [:location] dest))))

(def not-nil? (complement nil?))
(def not-equal? (complement =))

(defn run [player]
  (loop [ location (-> player :location)
            n (rand-int 6)]
   (cond
   (= n 0) (if (not-nil? (->> the-map location :dir :north)) (go :north player) (recur location (rand-int 6)))
   (= n 1) (if (not-nil? (->> the-map location :dir :south)) (go :south player) (recur location (rand-int 6)))
   (= n 2) (if (not-nil? (->> the-map location :dir :east)) (go :east player) (recur location (rand-int 6)))
   (= n 3) (if (not-nil? (->> the-map location :dir :west)) (go :west player) (recur location (rand-int 6)))
   (= n 4) (if (not-nil? (->> the-map location :dir :up)) (go :up player) (recur location (rand-int 6)))
   (= n 5) (if (not-nil? (->> the-map location :dir :down)) (go :down player) (recur location (rand-int 6)))
   )))

(def adventurer
  {:location :foyer
   :inventory #{}
   :tick 0
   :seen #{}})

(defn kill [player]
  (println "You have been killed! That nasty grue must have gotten to you! You must restart from the begining of this game. \n")
  adventurer)

(def grue {:health 100})

(defn use_item [player monster item]
  (let [loc (-> player :location)]
    ((if (and (= loc :grue-pen) (> (-> monster :health) 0))
      (if (= item :very-sharp-pencil)
      (do (println "You did it!!!") player)
      (do (println "Your choice of weapon was poor and ineffective against the grue!") (kill player)))
      (do (println "Pssst. That did absolutely nothing..") player)))))

(defn respond [player monster command]
  (if (contains? command 1)
  (match [(command 0)]
      [:grab] (grab player (command 1))
      [:drop] (toss player (command 1))
      [:show] (if (= (command 1) :ticks) (show-ticks player) (show-inventory player))
      [:look] (display player)

      [_] (do (println "I don't understand you.") player)
  )
  (match command
      [:look] (update-in player [:seen] #(disj % (-> player :location)))
      [(:or :n :north)](go :north player)
      [(:or :s :south)](go :south player)
      [(:or :e :east)] (go :east player)
      [(:or :w :west)] (go :west player)
      [(:or :u :up)]   (go :up player)
      [(:or :d :down)] (go :down player)
      [(:or :h :help)] (help player)
       [:skin] (go :skin player)
       [:leave] (go :leave player)
      [:display] (display player)
      [:tock] (tock player)
      [:show-ticks] (show-ticks player)
      [:show-inventory] (show-inventory player)
      [:run] (run player)
      [:puzzle] (puzzle player)

      _ (do (println "I don't understand you.") player))))

(defn monster_respond [monster player command]
  (if (contains? command 1)
    (if (and (= (command 0) :use)(= (-> player :location) :grue-pen) (= (command 1) :very-sharp-pencil))
      (do (println "\nAtlas, you've killed the mighty grue! Grab what you need and leave this place!")(assoc monster :health 0))monster)monster))

(defn status [player monster]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (if (and (= location :grue-pen) (> (-> monster :health) 0))
        (println "\nTHE GRUE IS COMING TO KILL YOU!!!! RUN!!!\n"))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn -main
  [& args]
  (loop [local-map the-map
         local-player adventurer
         local-monster grue]
    (let [pl (status local-player local-monster)
          _ (println "\n\nWhat do you want to do?")

          command (read-line)]
      (println "")
      (recur local-map (respond pl local-monster (to-keywords command)) (monster_respond local-monster pl (to-keywords command))))))
