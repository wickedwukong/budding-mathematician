module BuddingMathematician exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Random exposing (..)
import Debug exposing (log)

-- MODEL
type alias Question =
  { x : Int
  , y : Int
  , operator : String
  , answer : Int
  , solution : Int
  , isSolutionCorrect : Bool
  }

type alias BasicQuestion =
  { x : Int
  , y : Int
  , operator : String
  , seed : Seed
  }

type alias Model =
  { stars : Int
  , currentQuestion : Question
  , history : List Question
  , currentInput : String
  }

initialModel : Model
initialModel =
  { stars = 0
  , currentQuestion = Question 0 0 "+" -99999999 0 False
  , history = []
  , currentInput = ""
  }

-- Update
seedGenerator : Generator Seed
seedGenerator =
   Random.int Random.minInt Random.maxInt
       |> Random.map (Random.initialSeed)

toOperator : Int -> String
toOperator i =
  case i of
    1 -> "x"
    2 -> "+"
    3 -> "-"
    otherWise -> Debug.crash "Out of range"

initSeed : Cmd Msg
initSeed =
  Random.generate Init seedGenerator

under10Generator : Seed -> String -> BasicQuestion
under10Generator seed operator =
   let
      (x, seed0) = Random.step (int 0 10) seed
      (y, seed1) = Random.step (int 1 10) seed0
   in BasicQuestion x y operator seed1

under100Generator : Seed -> String -> BasicQuestion
under100Generator seed operator =
   let
      (x, seed0) = Random.step (int 0 100) seed
      (y, seed1) = Random.step (int 0 100) seed0
   in BasicQuestion x y operator seed1

generateBasicQuestion : Seed -> String -> BasicQuestion
generateBasicQuestion seed operator =
  if (operator == "+") then
       under100Generator seed "+"
  else if (operator == "x") then
       under10Generator seed "x"
  else
       under100Generator seed "-"

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

type Msg =
  Solution | Input String | Init Seed

makeQuestion : BasicQuestion -> Question
makeQuestion bq =
  if (bq.operator == "+") then
    Question bq.x bq.y bq.operator -99999999 (bq.x + bq.y) False
  else if (bq.operator == "x") then
    Question bq.x bq.y bq.operator -99999999 (bq.x * bq.y) False
  else if (bq.x > bq.y) then
    Question bq.x bq.y bq.operator -99999999 (bq.x - bq.y) False
  else
    Question bq.y bq.x bq.operator -99999999 (bq.y - bq.x) False

toNewQuestion : Seed -> (Question, Seed)
toNewQuestion seed =
  let
    (operator, newSeed) =
        seed
         |> Random.step (Random.int 1 3)
         |> \(number, seed0) -> ((toOperator number), seed0)

    basicQuestion = generateBasicQuestion newSeed operator
    question = makeQuestion basicQuestion
  in (question, basicQuestion.seed)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Init seed ->
      let
          (newQuestion, seed0) =
            seed
            |> toNewQuestion

      in  ({model | currentQuestion = newQuestion}, Cmd.none)
    Solution ->
      case String.toInt(model.currentInput) of
        (Ok answer) ->
          let
            oldCurrentQuestion = model.currentQuestion
            solution = oldCurrentQuestion.solution
            isSolutionCorrect = answer == solution
            newCurrentQuestion = {oldCurrentQuestion | answer = answer, isSolutionCorrect = isSolutionCorrect}
          in
            if (isSolutionCorrect) then
              ({model | stars = model.stars + 1, history = newCurrentQuestion :: model.history, currentInput = ""}, initSeed)
            else
              ({model | history = newCurrentQuestion :: model.history, currentInput = ""}, initSeed)
        _ -> (model, Cmd.none)
    Input input ->
        ({model | currentInput = input}, Cmd.none)

-- VIEW
showSolutionIfAnswerIsWrong : Question -> Html Msg
showSolutionIfAnswerIsWrong q =
  if (q.isSolutionCorrect) then
    text ""
  else
    text ("( ==> " ++ toString q.x ++ q.operator ++ toString q.y ++ "=" ++ toString q.solution ++ ")")

showFeedback : Question -> Html Msg
showFeedback q =
  if (q.isSolutionCorrect) then
    img [src "./happy.png", height 15, width 15] []
  else
    img [src "./sad.png", height 15, width 15] []

viewQuestionItem: Question -> Html Msg
viewQuestionItem q =
  div [ class "siimple-table-row"]
       [ div [class "siimple-table-cell"] [text (toString q.x ++ q.operator ++ toString q.y ++ "=" ++ toString q.answer)]
       , div [class "siimple-table-cell"] [showFeedback q, showSolutionIfAnswerIsWrong q]
       ]

viewHistory : List Question -> Html Msg
viewHistory questions =
  let
     listOfQuestions =
       List.map viewQuestionItem questions
  in
  div [class "siimple-table"]
       [div [class "siimple-table-body siimple-table--border"] listOfQuestions]


viewHeader : String -> Html msg
viewHeader title =
  header []
      [div [ class "siimple-box siimple-box--teal"]
           [ div [class "siimple-box-title"] [text title]
           , div [class "siimple-box-subtitle"] [text "Let's beat the math challenge!"]]
      ]

viewQuestion x operator y currentInput =
  div [class "siimple-h1"]
   [ text (toString x ++ operator ++ toString y ++ "=")
   , input
       [ type_ "number"
       , placeholder "What is your answer?"
       , class "large-input"
       , value currentInput
       , autofocus True
       , onEnter Solution
       , onInput Input]
       []
    ]


viewFooter =
  footer [class "siimple-footer"]
      [a [href "https://github.com/wickedwukong/bingo"]
         [text "View source on Github"]
      ]

showStars : Int -> Html msg
showStars stars =
  div [class "siimple-h6"]
        [ span [] [text ("Stars: " ++ toString stars)]
        , div []
               (List.map (\x -> img [src "./star.jpeg", height 23, width 23] []) (List.range 1 stars))
         ]

showStats : List Question -> Html msg
showStats history =
  let
    totalQuestionsAnswered = List.length history
    totalAdditionQuestions = history |> List.filter (\q -> q.operator == "+") |> List.length
    totalSubtractionQuestions = history |> List.filter (\q -> q.operator == "-") |> List.length
    totalMultiplicationQuestions = history |> List.filter (\q -> q.operator == "x") |> List.length

    assessAccuracy =
      if (totalQuestionsAnswered == 0) then
        0
      else
        let totalCorrectlyAnsweredQuestions = history |> List.filter (\q -> q.isSolutionCorrect) |> List.length
        in (((toFloat totalCorrectlyAnsweredQuestions) / (toFloat totalQuestionsAnswered) * 10000.0) |> round |> toFloat) / 100.0
  in
    div []
          [ div [] [text ("Total questions answered: " ++ toString totalQuestionsAnswered)]
          , div [] [text ("Addition: " ++ toString totalAdditionQuestions)]
          , div [] [text ("Subtraction: " ++ toString totalSubtractionQuestions)]
          , div [] [text ("Multiplication: " ++ toString totalMultiplicationQuestions)]
          , div [] [text ("Accuracy: " ++ toString assessAccuracy ++ "%")]
        ]


view model =
  div [class "siimple-content--fluid", align "center"]
      [ viewHeader "Hello Budding Mathematician, welcome!"
      , showStats model.history
      , showStars model.stars
      , viewQuestion model.currentQuestion.x model.currentQuestion.operator model.currentQuestion.y model.currentInput
      -- , div [class "debug"] [text (toString model)]
      , hr [] []
      , viewHistory model.history
      , viewFooter
      ]

main : Program Never Model Msg
main =
    Html.program
        { init = (initialModel, initSeed)
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none )
        }
