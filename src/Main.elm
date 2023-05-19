module Main exposing (..)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (Attribute, Html, code, div, h1, h2, img, li, main_, p, pre, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (alt, class, css, src, tabindex)
import Html.Styled.Events as Events
import Json.Decode as Json
import Maybe exposing (withDefault)



-- MAIN
-- Type definitions are optional and can be inferred by the compiler.
-- I have included them on most functions except those that return Slides.


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { slideIndex : Int
    , slides : Array Slide
    }



-- () represents the empty tuple, also called "unit". I'm using it here to indicate that no flags are being passed to Elm on initialisation.


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { slideIndex = 0
      , slides = Array.fromList slides
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NextSlide
    | NoOp -- No Operation; used for keypresses the app ignores
    | PreviousSlide


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextSlide ->
            ( { model | slideIndex = min (Array.length model.slides - 1) (model.slideIndex + 1) }
            , Cmd.none
            )

        NoOp ->
            -- Used to ignore key presses that don't relate to slide movement
            ( model, Cmd.none )

        PreviousSlide ->
            ( { model | slideIndex = max 0 (model.slideIndex - 1) }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- I'm using an underscore here in place of an argument name like "model" to indicate the argument isn't used by the function.
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        currentSlide =
            Array.get model.slideIndex model.slides
                |> withDefault missingSlide
    in
    { title = "Intro to Elm for JS developers"
    , body =
        [ main_
            [ css mainCss
            , tabindex 1 -- Needed so the element raises key events
            , onKeyUp keyToMsg
            , Events.onClick NextSlide
            ]
            currentSlide
        ]
            -- As we're using the rtfeldman/elm-css package to create our body (functions from Html.Styled),
            -- we need to convert our view's body back into a type that vanilla Elm
            -- understands; List (Html.Styled.Html Msg) to List (Html.Html Msg).
            -- Whilst you're getting up to speed with Elm, this subtlety can be safely ignored.
            |> List.map toUnstyled
    }


onKeyUp : (Int -> Msg) -> Attribute Msg
onKeyUp tagger =
    Events.on "keyup" (Json.map tagger Events.keyCode)



-- This function translates key presses on the main element into a Msg.
-- It would also have been possible to have a `KeyUp Int` Msg variant and
-- have the logic for handling different keypresses in the update function.


keyToMsg : Int -> Msg
keyToMsg keyCode =
    if List.member keyCode [ 37, 38 ] then
        -- ArrowLeft, ArrowUp
        PreviousSlide

    else if List.member keyCode [ 39, 40 ] then
        -- ArrowRight, ArrowDown
        NextSlide

    else
        -- Ignore other key presses
        NoOp



-- SLIDES: Hardcoded slide data


type alias Slide =
    List (Html Msg)


slides : List Slide
slides =
    [ titleSlide
    , whatIsElmSlide
    , whyElmIsAwesomeSlide
    , activateJargonKlaxonsSlide
    , whatIsStaticTypingJSSlide
    , whatIsStaticTypingElmSlide
    , whatIsAPureFunctionSlide
    , whatIsImmutabilitySlide
    , whatIsAFunctionalLanguageSlide
    , architectureSlide
    , syntaxPreambleSlide
    , syntaxModelSlide
    , syntaxViewSlide
    , syntaxMsgSlide
    , syntaxUpdateSlide
    , errorSlide
    , interactingWithTheOutsideWorldSlide
    , interfacingWithJavaScriptSlide
    , notableToolsSlide
    , thanksSlide
    ]


missingSlide : Slide



-- Type definitions for other functions returning Slide are omitted but inferred by the compiler


missingSlide =
    [ slideTitle "Slide not found"
    , img [ css [ height (pct 50), marginTop (pct 10) ], alt "Elm logo", src "assets/elm_logo.svg" ] []
    ]


titleSlide =
    [ slideTitle "Introduction to Elm"
    , slideSubtitle "for JavaScript developers"
    , img
        [ css
            [ height (vh 30)
            , margin (em 4)
            ]
        , alt "Elm logo"
        , src "assets/elm_logo.svg"
        ]
        []
    , p [ css [ fontSize (em 2) ] ] [ text "Ross McKelvie | ross.mckelvie@norbroch.com" ]
    ]


whatIsElmSlide =
    [ slideTitle "What is Elm?"
    , div [ css [ displayFlex ] ]
        [ div [ css [ margin (em 4) ] ]
            [ img
                [ css
                    [ height (vh 40)
                    , display block
                    ]
                , alt "Elm logo"
                , src "assets/elm_logo.svg"
                ]
                []
            , div [ css [ marginTop (em 2), fontSize (em 2) ] ] [ text "https://elm-lang.org" ]
            ]
        , div [ css [ margin (em 4) ] ]
            [ img
                [ css
                    [ height (vh 40)
                    , display block
                    ]
                , alt "Evan"
                , src "assets/evancz.jpg"
                ]
                []
            , div [ css [ marginTop (em 2), fontSize (em 2) ] ] [ text "Evan Czaplicki" ]
            ]
        ]
    ]


whyElmIsAwesomeSlide =
    imageOnlySlide "Why Elm is awesome" "Heart" "assets/heart.svg"


activateJargonKlaxonsSlide =
    let
        makeItem s =
            li [ css [ marginTop (em 0.5) ] ] [ text s ]
    in
    [ slideTitle "Activate jargon klaxons!"
    , ul [ css [ fontSize (em 4), textAlign left ] ]
        (List.map makeItem
            [ "Purely functional"
            , "Statically typed"
            , "with immutable values"
            ]
        )
    ]


whatIsStaticTypingJSSlide =
    [ slideTitle "What is static typing?"
    , slideSubtitle "Dynamic typing in JS"
    , div [ css [ textAlign left ] ]
        [ linesOfCode
            [ "3 + 7      # 10"
            , "\"foo\" + 3  # \"foo3\""
            , "10 - 2     # 8"
            , "\"10\" - 2   # NaN"
            ]
        ]
    ]


whatIsStaticTypingElmSlide =
    [ slideTitle "What is static typing?"
    , slideSubtitle "Elm"
    , div [ css [ textAlign left ] ]
        [ linesOfCode
            [ "3 + 7      -- 10"
            , "\"foo\" + 3  -- TYPE MISMATCH"
            , "10 - 2     -- 8"
            , "\"10\" - 2   -- TYPE MISMATCH"
            ]
        ]
    ]


whatIsAPureFunctionSlide =
    [ slideTitle "What is a pure function?"
    , div [ css [ textAlign left ] ]
        [ linesOfCode
            [ "# Pure"
            , "function add(a, b) {"
            , "  return a + b;"
            , "}"
            , " "
            , "# Impure"
            , "function getMillisSinceEpoch() {"
            , "  return Date.now();"
            , "}"
            , " "
            , "# Impure"
            , "function getDieRoll() {"
            , "  return Math.floor( Math.random() * 6 ) + 1;"
            , "}"
            ]
        ]
    ]


whatIsImmutabilitySlide =
    [ slideTitle "What is immutability?"
    , div [ css [ textAlign left, marginTop (em 4) ] ]
        [ linesOfCode
            [ "# JavaScript"
            , "let x = 1;  # x == 1"
            , "x += 10;    # x == 11"
            , " "
            , "-- Elm"
            , "x = 1      -- x == 1"
            , "x = x + 1  -- CYCLIC DEFINITION"
            ]
        ]
    ]


whatIsAFunctionalLanguageSlide =
    [ slideTitle "What is a functional language?" ]


architectureSlide =
    imageOnlySlide "Elm architecture" "Elm architecture" "assets/the_elm_architecture.svg"


syntaxPreambleSlide =
    syntaxSlide "Preamble"
        [ "module Main exposing (..)"
        , ""
        , "import Array exposing (Array)"
        , "import Html"
        , "[-- etc --]"
        , " "
        , "-- MAIN"
        , " "
        , "main : Program Flags Model Msg"
        , "main ="
        , "    Browser.document"
        , "        { init = init"
        , "        , view = view"
        , "        , update = update"
        , "        , subscriptions = subscriptions"
        , "        }"
        ]


syntaxModelSlide =
    syntaxSlide "Model"
        [ "type alias Model ="
        , "    { slideIndex : Int"
        , "    , slides : Array Slide"
        , "    }"
        , " "
        , "init : () -> ( Model, Cmd Msg )"
        , "init _ ="
        , "    ( { slideIndex = 0"
        , "      , slides = Array.fromList slides"
        , "      }"
        , "    , Cmd.none"
        , "    )"
        ]


syntaxViewSlide =
    syntaxSlide "View"
        [ "view : Model -> Browser.Document Msg"
        , "view model ="
        , "    let"
        , "        currentSlide ="
        , "            Array.get model.slideIndex model.slides"
        , "                |> withDefault missingSlide"
        , "    in"
        , "    { title = \"Intro to Elm for JS developers\""
        , "    , body ="
        , "        [ main_"
        , "            [ tabindex 1"
        , "            , onKeyUp keyToMsg"
        , "            , Events.onClick NextSlide"
        , "            ]"
        , "            currentSlide"
        , "        ]"
        , "    }"
        ]


syntaxMsgSlide =
    syntaxSlide "Msg"
        [ "type Msg"
        , "    = NextSlide"
        , "    | NoOp"
        , "    | PreviousSlide"
        ]


syntaxUpdateSlide =
    syntaxSlide "Update"
        [ "update : Msg -> Model -> ( Model, Cmd Msg )"
        , "update msg model ="
        , "    case msg of"
        , "        NextSlide ->"
        , "            ( { model"
        , "                  | slideIndex ="
        , "                      min (Array.length model.slides - 1) (model.slideIndex + 1) }"
        , "            , Cmd.none"
        , "            )"
        , " "
        , "        NoOp ->"
        , "            ( model, Cmd.none )"
        , " "
        , "        PreviousSlide ->"
        , "            ( { model"
        , "                  | slideIndex = max 0 (model.slideIndex - 1) }"
        , "            , Cmd.none"
        , "            )"
        ]


syntaxSlide : String -> List String -> Slide
syntaxSlide title lines =
    [ slideTitle ("Syntax: " ++ title)
    , div [ css [ fontSize (em 1), textAlign left, maxWidth (pct 95) ] ] [ linesOfCode lines ]
    ]


errorSlide =
    [ slideTitle "Errors in Elm"
    , div [ css [ textAlign left, marginTop (em 4) ] ]
        [ linesOfCode
            [ "type Maybe a"
            , "  = Just a"
            , "  | Nothing"
            , " "
            , "type Result error value"
            , "  = Ok value"
            , "  | Err error"
            ]
        ]
    ]


interactingWithTheOutsideWorldSlide =
    [ slideTitle "Commands and subscriptions"
    , div [ css [ marginTop (em 3) ] ]
        [ lineOfCode "update : Msg -> Model -> ( Model, Cmd Msg )"
        ]
    ]


interfacingWithJavaScriptSlide =
    [ slideTitle "JavaScript interop"
    , slideSubtitle "Ports to the rescue"
    ]


notableToolsSlide =
    let
        heading s =
            div [ css [ fontSize (em 2), fontWeight bold, textAlign left, marginTop (em 1), width (pct 80) ] ] [ text s ]

        makeList items =
            ul [ css [ fontSize (em 2), textAlign left, width (pct 80) ] ]
                (List.map makeItem items)

        makeItem s =
            li [ css [ marginTop (em 0.5) ] ] [ text s ]
    in
    [ slideTitle "Notable tools and packages"
    , heading "Tools"
    , makeList
        [ "elm-format: Prettier for Elm code"
        , "elm-live: Better dev server"
        ]
    , heading "Packages:"
    , makeList
        [ "elm-explorations/test: For writing unit tests"
        , "rtfeldman/elm-css: Statically typed CSS in Elm"
        , "mdgriffith/elm-ui: Awesome CSS alternative"
        ]
    , heading "For tonight's sponsor:"
    , makeList
        [ "elm-explorations/webgl: WebGL API." ]
    ]


thanksSlide =
    [ slideTitle "Thank you"
    , slideSubtitle "ross.mckelvie@norbroch.com"
    ]


imageOnlySlide : String -> String -> String -> Slide
imageOnlySlide title imgAlt imgSrc =
    [ slideTitle title
    , img
        [ css [ width (vh 70), display block ]
        , alt imgAlt
        , src imgSrc
        ]
        []
    ]



-- STYLING
{- I have used the rtfeldman/elm-css package for styling the presentation so you don't need to look in another file for the CSS. Other legitimate choices would include:
   * Using Strings for inline styles (yuck)
   * Using an an external stylesheet, providing CSS classes or ids in the view function or
   * My favourite styling package for single-page applications written entirely in Elm: mdgriffith/elm-ui.
-}
-- Styled elements


linesOfCode : List String -> Html Msg
linesOfCode lines =
    List.map lineOfCode lines
        |> Html.Styled.pre [ css [ overflowY auto ] ]


lineOfCode : String -> Html Msg
lineOfCode snippet =
    p [ css codeCss ] [ text snippet ]


slideTitle : String -> Html Msg
slideTitle s =
    h1
        [ css
            [ margin (px 0)
            , fontSize (em 4)
            ]
        ]
        [ text s ]


slideSubtitle : String -> Html Msg
slideSubtitle s =
    h2
        [ css
            [ fontSize (em 2.5)
            ]
        ]
        [ text s ]



-- Styles


mainCss : List Style
mainCss =
    [ backgroundColor darkGreen
    , color white
    , displayFlex
    , flexDirection column
    , justifyContent center
    , alignItems center
    , fontFamilies [ "Helvetica", "sans-serif" ]
    , height (vh 100)
    , width (vw 100)
    , textAlign center
    ]


headingCss : List Style
headingCss =
    [ margin (px 0)
    ]


codeCss =
    [ fontSize (em 3), fontFamily monospace, margin (em 0.1) ]



-- Colours


darkGreen : Color
darkGreen =
    hex "#233645"


white : Color
white =
    hex "#FFF"
