import Time exposing (..)
import List exposing (..)
import Element exposing (..)
import Color exposing (..)
import Html
import AnimationFrame
import Keyboard.Extra exposing (Key(..))
import Window
import Collage exposing (..)
import List exposing (..)
import Text
import String exposing (padLeft)
import Audio exposing (PlaybackOptions, defaultPlaybackOptions, Sound)
import Task exposing (Task, andThen)

{-| ********************************
-- MODEL: states of the game that can be changed
************************************-}

--for play/pause behavior triggered by space key
type State = Loading | NewGame | Starting | Play | Pausing | Pause | Resume | GameOver

type alias Player =
 { 
  angle: Float 
 }

type alias Enemy =
 { radius : Float --distance of enemies from center
 , parts : List(Bool) --list of true/false indicating where "hole" is
 }

type Direction = Left | Right | NotMoving

type alias Game =
 {
    player : Player
  , direction : Direction
  , enemies: List(Enemy)
  , enemySpeed: Float --increases over time
  , pressedKeys : List Key
  , state : State
  , timeStart : Time --timer that counts in millisecs
  , timeTick : Time
  , msRunning : Float
  , autoRotateAngle : Float --for rotating board
  , autoRotateSpeed : Float
  , hasBass : Bool
  , music: Maybe Sound
 }

--colors for the game defined in HSL color model
type alias Colors =
 { dark : Color
 , medium: Color
 , bright : Color
 }

--Step: a frame of the game-loop
--KeyboardExtraMsg: a keyboard input
type Msg = 
      Step Time 
    | KeyboardMsg Keyboard.Extra.Msg
    | MusicLoaded Sound
    | Error String
    | Noop

--load music
loadSound : Task String Sound
loadSound = Audio.loadSound "music/shinytech.mp3"

soundLoaded : Result String Sound -> Msg
soundLoaded result =
    case result of
        Ok music ->
            MusicLoaded music
        Err msg ->
            Error msg

playbackOptions = {
 defaultPlaybackOptions | loop = True, startAt = Nothing }

--start the music, returns command of type Msg
playSound : Sound -> PlaybackOptions -> Cmd Msg
playSound sound options =
 Task.attempt (always Noop) (Audio.playSound options sound)

--stop the music, returns command of type Msg
stopSound : Sound -> Cmd Msg
stopSound sound =
 Task.perform (always Noop) (Audio.stopSound sound)

--constant tuples: screen size, radius of player's periphery
(gameWidth, gameHeight) = (1024, 576) -- 16:9
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
(iHalfWidth, iHalfHeight) = (gameWidth//2, gameHeight//2)

playerRadius : Float
playerRadius = gameWidth / 10.0

playerSize: Float
playerSize = 10.0

playerSpeed : Float
playerSpeed = 0.12

bgBlack : Color
bgBlack = rgb 20 20 20

enemyThickness = 30
enemyDistance = 350
enemyInitialSpeed = 0.25
enemyAcceleration = 0.000002

enemies =
    [ [ False, True, False, True, False, True ]
    , [ False, True, True, True, True, True ]
    , [ True, False, True, True, True, True ]
    , [ True, True, True, True, False, True ]
    , [ True, True, True, False, True, True ]
    , [ False, True, False, True, False, True ]
    , [ True, False, True, False, True, False ]
    , [ True, True, True, True, True, False ]
    ]

{-| ********************************
UPDATE: old Model + Msg -> new Model
************************************-}

--Game loop: Transition from one frame/state to the next
--when no keyboard input: just update frame/state to the next frame/state
--when keyboard input: update frame with user input as well
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
 case msg of
  Step time -> onFrame time game 
  KeyboardMsg keyMsg -> onUserInput keyMsg game
  MusicLoaded music ->
   ( { game |
       state = NewGame,
       music = Just music
     }, Cmd.none)
  Error message -> Debug.crash message
  _ -> (game, Cmd.none)

--when keyboard input: define Left or Right or NotMoving direction based on arrow key pressed
onUserInput : Keyboard.Extra.Msg -> Game -> ( Game, Cmd Msg )
onUserInput keyMsg game =
 let
  pressedKeys =
    Keyboard.Extra.update keyMsg game.pressedKeys
  spacebar =
    List.member Keyboard.Extra.Space pressedKeys
    && not (List.member Keyboard.Extra.Space game.pressedKeys)
  direction =
    if (Keyboard.Extra.arrows pressedKeys).x < 0 then Left
    else if (Keyboard.Extra.arrows pressedKeys).x > 0 then Right
    else NotMoving
  nextState =
   case game.state of
    NewGame -> if spacebar then Play else NewGame
    Play -> if spacebar then Pause else Play
    GameOver -> if spacebar then NewGame else GameOver
    Pause -> if spacebar then Play else Pause
    _ -> game.state
 in
 ( 
   { game
     | pressedKeys = pressedKeys
     , direction = direction
     , state = nextState
   }
 , Cmd.none
 )

--updates game state on the next frame
onFrame : Time -> Game -> (Game, Cmd Msg)
onFrame time game =
 let
  (nextState, nextCmd) =
   case game.music of
    Nothing -> (Loading, Cmd.none)
    Just music ->
     case game.state of
      Starting -> (Play, playSound music { playbackOptions | startAt = Just 0 })
      Resume -> (Play, playSound music playbackOptions)
      Pausing -> (Pause, stopSound music)
      Play -> if isGameOver game
       then (GameOver, stopSound music)
       else (Play, Cmd.none)
      _ -> (game.state, Cmd.none)
 in
 ( {   game 
     | player = updatePlayer game.direction game 
     , enemies = updateEnemies game
     , enemySpeed = updateEnemySpeed game
     , state = Debug.log "state" nextState
     , timeStart = if game.state == NewGame then time else game.timeStart
     , timeTick = time
     , msRunning = Debug.log "msRunning" (updateMsRunning time game)
     , autoRotateAngle = updateAutoRotateAngle game
     , autoRotateSpeed = updateAutoRotateSpeed game
     , hasBass = hasBass game.msRunning
   }
   , nextCmd )

--slowly increases enemySpeed over time starting from an initial value
updateEnemySpeed: Game -> Float
updateEnemySpeed game =
 Debug.log "enemy speed" (enemyInitialSpeed + game.msRunning * enemyAcceleration)

updateEnemies: Game -> List(Enemy)
updateEnemies game =
 let
  enemyProgress = game.msRunning * game.enemySpeed --enemies approach player over time
  numEnemies = List.length enemies
  maxDistance = numEnemies * enemyDistance --total radius of all enemies

  --offsetForEnemy represents distance for a single enemy
  --subtract enemyProgress so value will be negative, but modulo maxDistance, so always between 0 and maxDistance
  offsetForEnemy index =
   round <| enemyDistance * (toFloat index) - enemyProgress
  
  --if finished all enemies (enemyProgress exceeds maxDistance), repeat enemies
  radiusFor index =
   (offsetForEnemy index) % maxDistance --calculate radius for every enemy % maxDistance
   |> toFloat
 in
 List.indexedMap (\index parts -> {
  parts = parts,
  radius = radiusFor index
 }) enemies

--detects if there is a collision
colidesWith: Player -> Enemy -> Bool
colidesWith player enemy =
 let
  collidesAtIndex: Int -> Bool
  collidesAtIndex index =
  let
   fromAngle = (toFloat index) * 60
   toAngle = ((toFloat index) + 1) * 60
   playerDegrees = player.angle * 360 / (2 * pi)
  in
  playerDegrees >= fromAngle && playerDegrees < toAngle
 in
 if enemy.radius > playerRadius || enemy.radius + enemyThickness < playerRadius-playerSize*3/2 then False
 else
  -- check if open
  indexedMap (,) enemy.parts |> filter Tuple.second |> map Tuple.first |> any collidesAtIndex

--isGameOver when player collides with an enemy
isGameOver: Game -> Bool
isGameOver {player, enemies} =
 any (colidesWith player) enemies

--add current minus last stored timestamp to game.msRunning, time passed since last call to onFrame
updateMsRunning: Time -> Game -> Time
updateMsRunning timestamp game =
 case game.state of
  Play -> game.msRunning + timestamp - game.timeTick
  NewGame -> 0.0
  _ -> game.msRunning

--update player's angle depending on keyboard input direction
updatePlayer: Direction -> Game -> Player
updatePlayer dir {player, enemies, state} =
 if state == Play then --player can only move when Play state
  let
   newAngle = 
    if state == NewGame then degrees 30 --if new game, restart state
    else updatePlayerAngle player.angle dir
   newPlayer = { player | angle = newAngle }
  in
   -- stop rotating if there is an enemy passing the ship (so won't game over if hit sideways)
   if any (colidesWith newPlayer) enemies then player
   else newPlayer
 else player

updatePlayerAngle : Float -> Direction -> Float
updatePlayerAngle angle dir =
 let
  sign =
   if dir == Left then 1
   else if dir == Right then -1
   else 0
  newAngle =
   angle + toFloat sign * playerSpeed
 in
 if newAngle < 0 then newAngle + 2 * pi
 else if newAngle > 2 * pi then newAngle - 2 * pi
 else newAngle

--these 2 functions modify the angle passed to the rotate function
--angle by which the field is rotated on every frame
updateAutoRotateAngle: Game -> Float
updateAutoRotateAngle {autoRotateAngle, autoRotateSpeed} =
 autoRotateAngle + autoRotateSpeed

--value by which autoRotateAngle is changed
updateAutoRotateSpeed: Game -> Float
updateAutoRotateSpeed {msRunning, autoRotateSpeed} =
 0.02 * sin (msRunning * 0.0003 |> Debug.log "φ")
 |> Debug.log "autoRotateSpeed"

{-| ********************************
VIEW: old Model + Msg -> new Model
************************************-}

startMessage = "@tet54: press SPACE to start/pause, use LEFT and RIGHT arrows keys to move"

--helper function to transform string into Element type, and format the text
--first argument: font size, second argument: text string
makeTextBox : Float -> String -> Element
makeTextBox size string =
 Text.fromString string
 |> Text.color (rgb 255 255 255)
 |> Text.monospace
 |> Text.height size
 |> leftAligned

--helper function to display the clock
--input: time value stored in msRunning field of Game object
formatTime : Time -> String
formatTime running =
 --clock displays time in steps of 1/100th of a second
 let
  centiseconds = floor (Time.inMilliseconds running / 10) --holds the value
  seconds = centiseconds // 100 --uses integer division to divide the centiseconds and cut off sub-seconds
  centis = centiseconds % 100 --uses modulo to keep only the sub-seconds
 in
 --padLeft pads the number with zeroes
 padLeft 3 '0' (toString seconds) ++ "." ++ padLeft 2 '0' (toString centis)
 
moveRadial : Float -> Float -> Form -> Form
moveRadial angle radius =
 move (radius * cos angle, radius * sin angle)

--define player: triangle
makePlayer : Player -> Form
makePlayer player =
 let
  angle = player.angle - degrees 30
 in
 ngon 3 playerSize --player is a circle
  |> filled (hsl angle 1 0.5) --player color changes depending on angle
  |> moveRadial angle (playerRadius - playerSize)
  |> rotate angle --rotate player to correct angle

--calculates color values
makeColors : Float -> Colors
makeColors msRunning =
 let
  hue = 0.00005 * msRunning
 in
 { dark = (hsl hue 0.6 0.2)
 , medium = (hsl hue 0.6 0.3)
 , bright = (hsla hue 0.6 0.6 0.8)
 }

octagonElement: Int -> List((Float, Float))
octagonElement i =
 let
  radius = halfWidth * sqrt 2
  angle0 = 60 * i |> toFloat |> degrees
  angle1 = 60 * (i+1) |> toFloat |> degrees
 in
  [(0.0, 0.0)
  , (sin angle0 * radius, cos angle0 * radius)
  , (sin angle1 * radius, cos angle1 * radius)
  ]

--helper function: returns alternating dark or medium
makeField: Colors -> Form
makeField colors =
 let
  color i =
   if i % 2 == 0 then
    colors.dark
   else
    colors.medium
  poly i = --creates a filled polygon from triangle points and color
   polygon (octagonElement i)
   |> filled (color i)
 in
  group (map poly (List.range 0 7)) --use map to create polygons using helper functions

--center element is an octagon that has a dark fill color and a light outline
makeCenterHole : Colors -> Game -> List Form
makeCenterHole colors game =
 let
  bassAdd =
   if game.hasBass then 0
   else 100.0 * (pump game.msRunning)
  shape = ngon 6 (60 + bassAdd)
  line = solid colors.bright
 in
 [ shape
   |> filled colors.dark
   |> rotate (degrees 90)
 , shape
   |> (outlined {line | width = 4.0})
   |> rotate (degrees 90)
 ]

--function to create a trapezoid
trapezoid: Float -> Float -> Color -> Form
trapezoid base height color =
 let
  s = height/(tan <| degrees 60)
 in
 filled color <| polygon [
  (-base/2, 0), (base/2, 0), (base/2-s, height), (-base/2+s, height)
 ]

--to display enemies, muses trapezoid function to create a trapezoid for every occupied part of each enemy
--returns a Form that contains a group of these trapezoids
makeEnemy : Color -> Enemy -> Form
makeEnemy color enemy =
 let
  base = 2.0 * (enemy.radius +enemyThickness) / (sqrt 3)
  makeEnemyPart : Int -> Form
  makeEnemyPart index =
   trapezoid base enemyThickness color
    |> rotate (degrees <| toFloat (90 + index * 60))
    |> moveRadial (degrees <| toFloat (index * 60)) (enemy.radius + enemyThickness)
 in
 --filter out parts by creating a tuple of (index:Int, part:Bool), remove parts where part is false
 --then send the index to makeEnemyPart
 group
  (indexedMap (,) enemy.parts |> filter Tuple.second |> map Tuple.first |> map makeEnemyPart)

--makes a List(Form) for every enemy
makeEnemies : Color -> List(Enemy) -> List(Form)
makeEnemies color enemies =
 map (makeEnemy color) enemies

--variables for beat, amplitude, phase
beat = 138.0 |> bpm
beatAmplitude = 0.06
beatPhase = 270 |> degrees

--one rotation should equal one beat
bpm : Float -> Float
bpm beat =
 (2.0 * pi * beat / 60 )

--takes a time value and returns True if there is a bass passage, or False otherwise
hasBass : Time -> Bool
hasBass time =
 if time < 20894 then False
 else if time < 41976 then True
 else if time < 55672 then False
 else if time < 67842 then True
 else if time < 187846 then False
 else if time < 215938 then True
 else False

--takes game state and returns a function that goes from Form -> Form
--input is the playing field; output is either same or pulsating playing field
beatPulse : Game -> Form -> Form
beatPulse game =
 if game.hasBass then
  scale (1 + (pump game.msRunning))
 else
  identity

--calculates value that is passed to the scale method using a sin function
--input is game progress (time in ms)
pump : Float -> Float
pump progress =
 beatAmplitude * (beat * progress / 1000 + beatPhase |> sin)

--main view function: receive current game state, returns Html
view : Game -> Html.Html Msg
view game =
 let
  bg = rect gameWidth gameHeight |> filled bgBlack
  
  colors = makeColors game.msRunning
  
  --shows the time that has passed
  score =
    formatTime game.msRunning
    |> makeTextBox 50

  --value of message box depends on state of game: Game Over or Pause or empty
  message = makeTextBox 50 <|
   case game.state of
    GameOver -> "Game Over"
    Pause -> "Pause"
    _ -> ""

  field = append
   [ makeField colors
   , makePlayer game.player
   , group <| makeEnemies colors.bright game.enemies
   ]
   (makeCenterHole colors game)
  |> group
 in
  toHtml 
   <| container gameWidth gameHeight middle 
   <| collage gameWidth gameHeight
      [ bg
      , field |> rotate game.autoRotateAngle
      , toForm message |> move (0, 40)
      , toForm score |> move (100 - halfWidth, halfHeight - 40)
      , toForm ( --toForm method turns an Element into a Form object, so it can be modified later on
        if game.state == Play then spacer 1 1
        else makeTextBox 20 startMessage
        ) |> move (0, 40 - halfHeight)
      ]


{-| ********************************
main: start point in every Elm app
************************************-}
main =
 Html.program
 { init = init
 , update = update
 , view = view
 , subscriptions = subscriptions }

--defines initial state of game
init : (Game, Cmd Msg)
init =
  ( { player = Player (degrees 30)
    , direction = NotMoving
    , pressedKeys = []
    , state = NewGame
    , enemies = []
    , enemySpeed = 0.0
    , timeStart = 0.0
    , timeTick = 0.0
    , msRunning = 0.0
    , autoRotateAngle = 0.0
    , autoRotateSpeed = 0.0
    , hasBass = False
    , music = Nothing
    }
   , Task.attempt soundLoaded loadSound
  )

--subscribe to keyboard-events and AnimationFrame events
subscriptions : Game -> Sub Msg
subscriptions game =
 Sub.batch 
  [ AnimationFrame.times (\time -> Step time)
  , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
  ]