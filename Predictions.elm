---kumail naqvi 40068946 2016
import Html
import GraphicSVG exposing (..)
import Anonymous exposing(..)
import Set exposing(toList, fromList)


type Message = GameTick Float GetKeyState

main = gameApp GameTick {
                            model = model
                        ,   view = view
                        ,   update = update
                        }

model = {
              t = 0
        }

update message model =
  case message of
    GameTick tick (getKeyState,changeP1,changeP2) -> {
                                       t = tick
                              }
view model = (collage 1366 720 [frame])  
                               
---------------------------------------------------------------------------------------------------------------------
getTime anonList questionNum userNum = case anonList of
                  (x1, x2, x3, x4)::xs -> [(x1, x2, x3, x4)]
                  otherwise -> otherwise
        

---------------------------------------------------------------------------------------------------------------------

firstOccuranceTime anonList questionNum userNum = case anonList of
---Checks the first time a user compiled a given question
---Gives Int
---working 
                  (x1, x2, x3, x4)::xs -> if userNum == x1
                                            then
                                                if questionNum == x2
                                                  then x3
                                                  else firstOccuranceTime xs questionNum userNum
                                            else firstOccuranceTime xs questionNum userNum
                  otherwise -> 0 

---------------------------------------------------------------------------------------------------------------------
firstCorrectOccuranceTime anonList questionNum userNum = case anonList of 
---Checks if a user ever got a certain question correct and Gives time of first pass
---Gives Int
---working
                  (x1, x2, x3, x4)::xs -> if userNum == x1
                                            then
                                                if questionNum == x2
                                                  then
                                                      if x4 == True
                                                        then x3
                                                        else firstCorrectOccuranceTime xs questionNum userNum
                                                  else firstCorrectOccuranceTime xs questionNum userNum
                                            else firstCorrectOccuranceTime xs questionNum userNum
                  otherwise -> 0

---------------------------------------------------------------------------------------------------------------------

timeTakenOnQuestion anonList questionNum userNum = (firstOccuranceTime anonList questionNum userNum) - (firstCorrectOccuranceTime anonList questionNum userNum)
---Calculates the time taken by a user on a given question to go from the first compile to a pass
---Gives Int
---working

---------------------------------------------------------------------------------------------------------------------

totalTimeTaken anonList userNum qList = case qList of 
---Calculates the total time taken by user to complete all passed questions
---Gives Int
---working
                  x::xs -> if userQuestionCorrect anonList x userNum == True
                             then (timeTakenOnQuestion anonList x userNum) + (totalTimeTaken anonList userNum xs)
                             else (totalTimeTaken anonList userNum xs)
                  
                  otherwise -> 0


---------------------------------------------------------------------------------------------------------------------

averageTimePerQuestion userNum = (round((totalTimeTaken userQuestionTimePass userNum listOfQuestions)/(userUniqueTrueOccurances userQuestionTimePass listOfQuestions userNum)))//1000
---Calculates the average time spent per question by a user
---Gives Int
---working

---------------------------------------------------------------------------------------------------------------------

globalTimeSum uList = case uList of
                  x::xs -> ((averageTimePerQuestion x) + globalTimeSum xs)
                  otherwise -> 0
---Adds up all the average times of evert user
---Gives Int 
---working

---------------------------------------------------------------------------------------------------------------------

globalAverageTimePerQuestion uList = ---(globalTimeSum uList)
                                     -1243609//117
---Gets the overall average time taken on each question by each user
---gives int
---working
---replaced globalTimeSum with the exprimental value to increase optimization

---------------------------------------------------------------------------------------------------------------------

userTimeScore userNum = roundN 3 (toFloat(averageTimePerQuestion userNum))/(toFloat(globalAverageTimePerQuestion listOfUsers))

---------------------------------------------------------------------------------------------------------------------

userQuestionCorrect anonList questionNum userNum = case anonList of 
---Checks if a user ever got a certain question correct
---Gives a Boolean Value
---working
                  (x1, x2, x3, x4)::xs -> if x1 == userNum
                                            then 
                                                if questionNum == x2
                                                  then 
                                                      if x4 == True
                                                        then True
                                                        else userQuestionCorrect xs questionNum userNum
                                                  else userQuestionCorrect xs questionNum userNum
                                            else userQuestionCorrect xs questionNum userNum    
                  otherwise -> False
                                           
---------------------------------------------------------------------------------------------------------------------                
userUniqueTrueOccurances anonList questionsList userNum = case questionsList of
---Gives the number of questions a given user has passed
---Gives Int
---working
                 x::xs -> if userQuestionCorrect anonList x userNum == True 
                            then 1 + (userUniqueTrueOccurances anonList xs userNum)
                            else userUniqueTrueOccurances anonList xs userNum
                 otherwise -> 0
---------------------------------------------------------------------------------------------------------------------                                                                               
listOfCompletedQuestions anonList questionsList userNum = case questionsList of
---Gives a list of all the questions a user has passed
---Gives List Int
---working
                 x::xs -> if userQuestionCorrect anonList x userNum == True 
                            then [x] ++ (listOfCompletedQuestions anonList xs userNum)
                            else listOfCompletedQuestions anonList xs userNum
                 otherwise -> []
                 
---------------------------------------------------------------------------------------------------------------------
userScore userNum = (((2.75*(e^(roundN 3 ((userUniqueTrueOccurances userQuestionTimePass listOfQuestions userNum)/40))))-(e^(avgEaseOfQuestion userNum (listOfCompletedQuestions userQuestionTimePass listOfQuestions userNum)))- userTimeScore userNum))

---------------------------------------------------------------------------------------------------------------------
---The userTimeScore check is to check for people who copy/pasted answers. 
trueUserScore userNum = ---if (userUniqueTrueOccurances userQuestionTimePass [1..103] userNum) < 2
                        ---  then "0, 0"
                        ---  else
                              if round (userTimeScore userNum*10) == 0
                                then "3"
                                else if ceiling(userScore userNum) >= 7 
                                       then "12"
                                       else if ceiling(userScore userNum) == 6
                                              then "11"
                                              else 
                                                  if ceiling(userScore userNum) == 5
                                                    then "10"
                                                    else 
                                                        if ceiling(userScore userNum) == 4
                                                          then "9"
                                                          else 
                                                              if ceiling(userScore userNum) == 3
                                                                then "7"
                                                                else 
                                                                    if ceiling(userScore userNum) == 2
                                                                      then "5"
                                                                      else ""
---------------------------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------------------------
numUsersPassedQuestion questionNum listOfUsers = case listOfUsers of
---Determines the number of users who passed a given question
---Gives Int
---working
                  (x)::xs -> if userQuestionCorrect userQuestionTimePass questionNum x == True
                               then 1 + (numUsersPassedQuestion questionNum xs)
                               else (numUsersPassedQuestion questionNum xs)
                  otherwise -> 0

---------------------------------------------------------------------------------------------------------------------

numPercentPassedQuestion questionNum = (((numUsersPassedQuestion questionNum listOfUsers)/(117.0)))
---Calculates the percent of users who passed a given question
---Gives Float
---------------------------------------------------------------------------------------------------------------------

usersList anonList = case anonList of 
---Creates a list of all users
---Gives List Int
                  (x1, x2, x3, x4)::(y1, y2, y3, y4)::xs -> if y1 /= x1
                                                              then [x1, y1] ++ usersList ((y1, y2, y3, y4)::xs)
                                                              else usersList ((y1, y2, y3, y4)::xs)
                  otherwise -> []

---------------------------------------------------------------------------------------------------------------------

unique list = Set.toList(Set.fromList(list))
----Removes duplicate values from given list, sorting the list in the process
---Gives List

---------------------------------------------------------------------------------------------------------------------

roundN n x =
    let
        p =
            10 ^ (toFloat n)
    in
        (toFloat << ceiling <| x * p) / p

---------------------------------------------------------------------------------------------------------------------

totalEaseOfQuestion userNum userQuestionsList = case userQuestionsList of
---Calculates the sum of the percents of each passed question
                x::xs -> (numPercentPassedQuestion x) + totalEaseOfQuestion userNum xs
                otherwise -> 0
            

---------------------------------------------------------------------------------------------------------------------                
----Calculates the average 'ease' of a users completed questions, lower number is better
avgEaseOfQuestion userNum userQuestionsList = roundN 3 ((totalEaseOfQuestion userNum userQuestionsList)/(userUniqueTrueOccurances userQuestionTimePass listOfQuestions userNum))
---------------------------------------------------------------------------------------------------------------------


----VARIABLE CONSTANTS
currentUser = 239
currentUser2 = 45      
currentQuestion = 2

----FIXED CONSTANTS
----DO NOT CHANGE THESE
listOfUsers = unique(usersList userQuestionTimePass)
numberOfUsers = List.length listOfUsers
listOfQuestions = [1..103]





createLabel usersList move1 move2 move3 move4 move5 = case usersList of
                x::xs -> if x < 35 
                           then(text (("User: ") ++ toString(x) ++ "  =  "  ++ toString(trueUserScore x) )|> size 15 |> filled blue |> move(-600, move1)) :: createLabel xs (move1 - 25) move2 move3 move4 move5
                           else 
                               if x < 84
                                 then (text (("User: ") ++ toString(x) ++ "  =  "  ++ toString(trueUserScore x) )|> size 15 |> filled blue |> move(-400, move2)) :: createLabel xs move1 (move2 - 25) move3 move4 move5
                                 else
                                    if x < 201 
                                      then (text (("User: ") ++ toString(x) ++ "  =  "  ++ toString(trueUserScore x)) |> size 15 |> filled blue |> move(-200, move3)) :: createLabel xs move1 move2 (move3 - 25) move4 move5
                                      else 
                                          if x < 238 
                                            then (text (("User: ") ++ toString(x) ++ "  =  "  ++ toString(trueUserScore x)) |> size 15 |> filled blue |> move(0, move4)) :: createLabel xs move1 move2 move3 (move4 - 25)  move5
                                            else (text (("User: ") ++ toString(x) ++ "  =  "  ++ toString(trueUserScore x)) |> size 15 |> filled blue |> move(200, move5)) :: createLabel xs move1 move2 move3 move4 (move5 - 25)
                [] -> []
                
---------------------------------------------------------------------------------------------------------------------


lolList lst = case lst of
             (x1, x2, x3, x4)::xs -> [x1] ++ lolList xs
             otherwise -> []

frame = group ((createLabel listOfUsers (300) (300) (300) (300) (300)) ++ [text "Made by Kumail Naqvi 400068946" |> size 15 |> filled red |> move (200, -300)] )

       {--  group
            [text ("Questions Completed: " ++ toString(userUniqueTrueOccurances userQuestionTimePass [1..103] currentUser) ++ "   qScore: " ++  toString((userUniqueTrueOccurances userQuestionTimePass [1..103] currentUser)/40))
                |> size 26
                |> centered
                |> filled blue
                |> move (-300,150)
            ,text ("Current User: " ++ toString(currentUser))
                |> size 40
                |> centered
                |> filled green
                |> move (-300,250)     
            ,text ("User timeScore: " ++ toString(userTimeScore currentUser)    )
                |> size 26
                |> centered
                |> filled red
                |> move (-300,100)
            ,text ("User avgEaseOfQuestion: " ++ toString (avgEaseOfQuestion currentUser (listOfCompletedQuestions userQuestionTimePass listOfQuestions currentUser)))
                |> size 30
                |> centered
                |> filled yellow    
                |> move (-300,50)
            ,text ("user score:  " ++ toString (userScore currentUser ))
                |> size 25
                |> centered
                |> filled blue
                |> move (-300,0)
            ,text ("user prediction:  " ++ toString (trueUserScore currentUser ) ++ "   " ++ toString (trueScore2 currentUser))
                |> size 25
                |> centered
                |> filled red
                |> move (-300,-50)
        -------------------------------------------------------------
            ,text ("Questions Completed: " ++ toString(userUniqueTrueOccurances userQuestionTimePass [1..103] currentUser2) ++ "   qScore: " ++  toString((userUniqueTrueOccurances userQuestionTimePass [1..103] currentUser2)/41))
                |> size 26
                |> centered
                |> filled blue
                |> move (300,150)
            ,text ("Current User 2: " ++ toString(currentUser2))
                |> size 40
                |> centered
                |> filled green
                |> move (300,250)     
            ,text ("User timeScore: " ++ toString(userTimeScore currentUser2))
                |> size 26
                |> centered
                |> filled red
                |> move (300,100)
            ,text ("User avgEaseOfQuestion: " ++ toString (avgEaseOfQuestion currentUser2 (listOfCompletedQuestions userQuestionTimePass listOfQuestions currentUser2)))
                |> size 30
                |> centered
                |> filled yellow    
                |> move (300,50)
            ,text ("user score:  " ++ toString (userScore currentUser2 ))
                |> size 25
                |> centered
                |> filled blue
                |> move (300,0)
            ,text ("user prediction:  " ++ toString (trueUserScore currentUser2 ) ++ "   " ++ toString (trueScore2 currentUser2 ))
                |> size 25
                |> centered
                |> filled red
                |> move (300,-50) 
            
        
            ]
 --}    