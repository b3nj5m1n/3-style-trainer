port module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline
import Random
import Task
import Url



-- PORTS


port newCommMessage : Maybe String -> Cmd msg


port newSettingsMessage : Settings -> Cmd msg


port newCommReceiver : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    newCommReceiver GeneratedNewComm



---- MODEL ----


type alias CommUF =
    { comm : String
    , piece1 : String
    , piece2 : String
    , comm_type : String
    }


toCommTypeUF : CommUF -> CommType
toCommTypeUF comm =
    UF (Just comm)


decoderCommUF : Decoder CommUF
decoderCommUF =
    Json.Decode.map4
        CommUF
        (Json.Decode.field "comm" Json.Decode.string)
        (Json.Decode.field "piece_1" Json.Decode.string)
        (Json.Decode.field "piece_2" Json.Decode.string)
        (Json.Decode.field "type" Json.Decode.string)


type alias CommUFR =
    { comm : String
    , piece1 : String
    , piece2 : String
    , comm_type : String
    }


toCommTypeUFR : CommUFR -> CommType
toCommTypeUFR comm =
    UFR (Just comm)


decoderCommUFR : Decoder CommUFR
decoderCommUFR =
    Json.Decode.map4
        CommUFR
        (Json.Decode.field "comm" Json.Decode.string)
        (Json.Decode.field "piece_1" Json.Decode.string)
        (Json.Decode.field "piece_2" Json.Decode.string)
        (Json.Decode.field "type" Json.Decode.string)


type alias CommParity =
    { piece : String
    , comm : String
    }


toCommTypeParity : CommParity -> CommType
toCommTypeParity comm =
    Parity (Just comm)


decoderCommParity : Decoder CommParity
decoderCommParity =
    Json.Decode.map2
        CommParity
        (Json.Decode.field "piece" Json.Decode.string)
        (Json.Decode.field "comm" Json.Decode.string)


type alias Comm2Flip =
    { comm : String
    , piece1 : String
    , piece2 : String
    }


toCommType2Flip : Comm2Flip -> CommType
toCommType2Flip comm =
    TwoFlips (Just comm)


decoderComm2Flip : Decoder Comm2Flip
decoderComm2Flip =
    Json.Decode.map3
        Comm2Flip
        (Json.Decode.field "comm" Json.Decode.string)
        (Json.Decode.field "piece_1" Json.Decode.string)
        (Json.Decode.field "piece_2" Json.Decode.string)


type alias Comm2Twist =
    { comm : String
    , piece1 : String
    , piece2 : String
    }


toCommType2Twist : Comm2Twist -> CommType
toCommType2Twist comm =
    TwoTwists (Just comm)


decoderComm2Twist : Decoder Comm2Twist
decoderComm2Twist =
    Json.Decode.map3
        Comm2Twist
        (Json.Decode.field "comm" Json.Decode.string)
        (Json.Decode.field "piece_1" Json.Decode.string)
        (Json.Decode.field "piece_2" Json.Decode.string)


type alias Comm3Twist =
    { comm : String
    , piece1 : String
    , piece2 : String
    }


toCommType3Twist : Comm3Twist -> CommType
toCommType3Twist comm =
    ThreeTwists (Just comm)


decoderComm3Twist : Decoder Comm3Twist
decoderComm3Twist =
    Json.Decode.map3
        Comm3Twist
        (Json.Decode.field "comm" Json.Decode.string)
        (Json.Decode.field "piece_1" Json.Decode.string)
        (Json.Decode.field "piece_2" Json.Decode.string)


type alias Data =
    { uf : List CommUF
    , ufr : List CommUFR
    , parity : List CommParity
    , two_flips : List Comm2Flip
    , two_twists : List Comm2Twist
    , three_twists : List Comm3Twist
    }


decoderData : Decoder Data
decoderData =
    Json.Decode.field "data"
        (Json.Decode.map6
            Data
            (Json.Decode.field "UF" (Json.Decode.list decoderCommUF))
            (Json.Decode.field "UFR" (Json.Decode.list decoderCommUFR))
            (Json.Decode.field "PARITY" (Json.Decode.list decoderCommParity))
            (Json.Decode.field "2FLIPS" (Json.Decode.list decoderComm2Flip))
            (Json.Decode.field "2TWISTS" (Json.Decode.list decoderComm2Twist))
            (Json.Decode.field "3TWISTS" (Json.Decode.list decoderComm3Twist))
        )


decoderToggledTypes : Decoder ToggledTypes
decoderToggledTypes =
    Json.Decode.map6
        ToggledTypes
        (Json.Decode.field "uf" Json.Decode.bool)
        (Json.Decode.field "ufr" Json.Decode.bool)
        (Json.Decode.field "parity" Json.Decode.bool)
        (Json.Decode.field "two_flips" Json.Decode.bool)
        (Json.Decode.field "two_twists" Json.Decode.bool)
        (Json.Decode.field "three_twists" Json.Decode.bool)


decoderToggledCasesUF : Decoder ToggledCasesUF
decoderToggledCasesUF =
    Json.Decode.map8
        ToggledCasesUF
        (Json.Decode.field "four_mover" Json.Decode.bool)
        (Json.Decode.field "u_swap" Json.Decode.bool)
        (Json.Decode.field "e_swap" Json.Decode.bool)
        (Json.Decode.field "s_swap" Json.Decode.bool)
        (Json.Decode.field "m_swap" Json.Decode.bool)
        (Json.Decode.field "alg" Json.Decode.bool)
        (Json.Decode.field "f_swap" Json.Decode.bool)
        (Json.Decode.field "s_insert" Json.Decode.bool)


decoderToggledCasesUFR : Decoder ToggledCasesUFR
decoderToggledCasesUFR =
    Json.Decode.succeed
        ToggledCasesUFR
        |> Json.Decode.Pipeline.required "u_any__u_any" Json.Decode.bool
        |> Json.Decode.Pipeline.required "u_top__d_side" Json.Decode.bool
        |> Json.Decode.Pipeline.required "u_top__d_bottom" Json.Decode.bool
        |> Json.Decode.Pipeline.required "special" Json.Decode.bool
        |> Json.Decode.Pipeline.required "u_side__d_any" Json.Decode.bool
        |> Json.Decode.Pipeline.required "luf_d__any" Json.Decode.bool
        |> Json.Decode.Pipeline.required "bur__d_any" Json.Decode.bool
        |> Json.Decode.Pipeline.required "d_side__d_side" Json.Decode.bool
        |> Json.Decode.Pipeline.required "d_side__d_bottom" Json.Decode.bool
        |> Json.Decode.Pipeline.required "d_bottom__d_bottom" Json.Decode.bool


decoderSettings : Decoder Settings
decoderSettings =
    Json.Decode.field "settings"
        (Json.Decode.map6
            Settings
            (Json.Decode.field "toggled_types" decoderToggledTypes)
            (Json.Decode.field "toggled_cases_uf" decoderToggledCasesUF)
            (Json.Decode.field "toggled_cases_ufr" decoderToggledCasesUFR)
            (Json.Decode.field "comm_visible" Json.Decode.bool)
            (Json.Decode.field "comm_type_visible" Json.Decode.bool)
            (Json.Decode.field "letters_visible" Json.Decode.bool)
        )


decoder : Decoder Flags
decoder =
    Json.Decode.map2
        Flags
        decoderData
        decoderSettings


type alias Flags =
    { data : Data, settings : Settings }


type CommType
    = UF (Maybe CommUF)
    | UFR (Maybe CommUFR)
    | Parity (Maybe CommParity)
    | TwoFlips (Maybe Comm2Flip)
    | TwoTwists (Maybe Comm2Twist)
    | ThreeTwists (Maybe Comm3Twist)


type alias ToggledTypes =
    { uf : Bool
    , ufr : Bool
    , parity : Bool
    , two_flips : Bool
    , two_twists : Bool
    , three_twists : Bool
    }


type alias ToggledCasesUF =
    { four_mover : Bool
    , u_swap : Bool
    , e_swap : Bool
    , s_swap : Bool
    , m_swap : Bool
    , alg : Bool
    , f_swap : Bool
    , s_insert : Bool
    }


type CaseUF
    = FourMover
    | USwap
    | ESwap
    | SSwap
    | MSwap
    | Alg
    | FSwap
    | SInsert


toggleCasesUF : CaseUF -> ToggledCasesUF -> ToggledCasesUF
toggleCasesUF case_to_toggle current =
    case case_to_toggle of
        FourMover ->
            { current | four_mover = not current.four_mover }

        USwap ->
            { current | u_swap = not current.u_swap }

        ESwap ->
            { current | e_swap = not current.e_swap }

        SSwap ->
            { current | s_swap = not current.s_swap }

        MSwap ->
            { current | m_swap = not current.m_swap }

        Alg ->
            { current | alg = not current.alg }

        FSwap ->
            { current | f_swap = not current.f_swap }

        SInsert ->
            { current | s_insert = not current.s_insert }


type alias ToggledCasesUFR =
    { u_any__u_any : Bool
    , u_top__d_side : Bool
    , u_top__d_bottom : Bool
    , special : Bool
    , u_side__d_any : Bool
    , luf_d__any : Bool
    , bur__d_any : Bool
    , d_side__d_side : Bool
    , d_side__d_bottom : Bool
    , d_bottom__d_bottom : Bool
    }


type CaseUFR
    = UAnyUAny
    | UTopDSide
    | UTopDBottom
    | Special
    | USideDAny
    | LUFDAny
    | BURDAny
    | DSideDSide
    | DSideDBottom
    | DBottomDBottom


toggleCasesUFR : CaseUFR -> ToggledCasesUFR -> ToggledCasesUFR
toggleCasesUFR case_to_toggle current =
    case case_to_toggle of
        UAnyUAny ->
            { current | u_any__u_any = not current.u_any__u_any }

        UTopDSide ->
            { current | u_top__d_side = not current.u_top__d_side }

        UTopDBottom ->
            { current | u_top__d_bottom = not current.u_top__d_bottom }

        Special ->
            { current | special = not current.special }

        USideDAny ->
            { current | u_side__d_any = not current.u_side__d_any }

        LUFDAny ->
            { current | luf_d__any = not current.luf_d__any }

        BURDAny ->
            { current | bur__d_any = not current.bur__d_any }

        DSideDSide ->
            { current | d_side__d_side = not current.d_side__d_side }

        DSideDBottom ->
            { current | d_side__d_bottom = not current.d_side__d_bottom }

        DBottomDBottom ->
            { current | d_bottom__d_bottom = not current.d_bottom__d_bottom }


getComm : Maybe CommType -> Maybe String
getComm c =
    case c of
        Just (UF (Just comm)) ->
            Just comm.comm

        Just (UFR (Just comm)) ->
            Just comm.comm

        Just (Parity (Just comm)) ->
            Just comm.comm

        Just (TwoFlips (Just comm)) ->
            Just comm.comm

        Just (TwoTwists (Just comm)) ->
            Just comm.comm

        Just (ThreeTwists (Just comm)) ->
            Just comm.comm

        _ ->
            Nothing


toggleType : CommType -> ToggledTypes -> ToggledTypes
toggleType type_to_toggle current =
    case type_to_toggle of
        UF _ ->
            { current | uf = not current.uf }

        UFR _ ->
            { current | ufr = not current.ufr }

        Parity _ ->
            { current | parity = not current.parity }

        TwoFlips _ ->
            { current | two_flips = not current.two_flips }

        TwoTwists _ ->
            { current | two_twists = not current.two_twists }

        ThreeTwists _ ->
            { current | three_twists = not current.three_twists }


type alias Settings =
    { toggled_types : ToggledTypes
    , toggled_cases_uf : ToggledCasesUF
    , toggled_cases_ufr : ToggledCasesUFR
    , comm_visible : Bool
    , comm_type_visible : Bool
    , letters_visible : Bool
    }


setToggledTypes : ToggledTypes -> Settings -> Settings
setToggledTypes new_toggled_types settings =
    { settings | toggled_types = new_toggled_types }


setToggledCasesUF : ToggledCasesUF -> Settings -> Settings
setToggledCasesUF new_toggled_cases settings =
    { settings | toggled_cases_uf = new_toggled_cases }


setToggledCasesUFR : ToggledCasesUFR -> Settings -> Settings
setToggledCasesUFR new_toggled_cases settings =
    { settings | toggled_cases_ufr = new_toggled_cases }


setCommVisible : Bool -> Settings -> Settings
setCommVisible new_val settings =
    { settings | comm_visible = new_val }


setCommTypeVisible : Bool -> Settings -> Settings
setCommTypeVisible new_val settings =
    { settings | comm_type_visible = new_val }


setLettersVisible : Bool -> Settings -> Settings
setLettersVisible new_val settings =
    { settings | letters_visible = new_val }


type Model
    = Error
    | Loaded
        { data : Data
        , settings : Settings
        , current_comms : List CommType
        , current_comm : Maybe CommType
        , current_setup_alg : Maybe String
        }


setCurrentComms : List CommType -> Model -> Model
setCurrentComms new_current_comms model =
    case model of
        Loaded state ->
            Loaded { state | current_comms = new_current_comms }

        Error ->
            model


setSettings : Settings -> Model -> Model
setSettings settings model =
    case model of
        Loaded state ->
            Loaded { state | settings = settings }

        Error ->
            model


init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case Json.Decode.decodeValue decoderData flags of
        Ok decodedData ->
            case Json.Decode.decodeValue decoderSettings flags of
                Ok decodedSettings ->
                    ( Loaded
                        { data = decodedData
                        , settings = decodedSettings
                        , current_comms = []
                        , current_comm = Nothing
                        , current_setup_alg = Nothing
                        }
                    , send GenerateCurrentComms
                    )

                Err err ->
                    ( Loaded
                        { data = decodedData
                        , settings =
                            { toggled_types =
                                { uf = True
                                , ufr = True
                                , parity = False
                                , two_flips = False
                                , two_twists = False
                                , three_twists = False
                                }
                            , toggled_cases_uf =
                                { four_mover = True
                                , u_swap = True
                                , e_swap = True
                                , s_swap = True
                                , m_swap = True
                                , alg = True
                                , f_swap = True
                                , s_insert = True
                                }
                            , toggled_cases_ufr =
                                { u_any__u_any = True
                                , u_top__d_side = True
                                , u_top__d_bottom = True
                                , special = True
                                , u_side__d_any = True
                                , luf_d__any = True
                                , bur__d_any = True
                                , d_side__d_side = True
                                , d_side__d_bottom = True
                                , d_bottom__d_bottom = True
                                }
                            , comm_visible = False
                            , comm_type_visible = False
                            , letters_visible = False
                            }
                        , current_comms = []
                        , current_comm = Nothing
                        , current_setup_alg = Nothing
                        }
                    , Cmd.none
                    )

        Err err ->
            ( Error, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleType CommType
    | GenerateNewComm
    | UpdateComm Int
    | GeneratedNewComm String
    | GenerateCurrentComms
    | ToggleCommVisible
    | ToggleCommTypeVisible
    | ToggleLettersVisible
    | ToggleCaseUF CaseUF
    | ToggleCaseUFR CaseUFR


randomIndexGenerator : List CommType -> Random.Generator Int
randomIndexGenerator list =
    Random.int 0 (List.length list)


newComm : List CommType -> Cmd Msg
newComm list =
    Random.generate UpdateComm (randomIndexGenerator list)


updateSettings : Settings -> Cmd Msg
updateSettings settings =
    newSettingsMessage settings


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


isValidCaseUF : ToggledCasesUF -> CommUF -> Bool
isValidCaseUF toggled_cases comm =
    case comm.comm_type of
        "4-Mover" ->
            toggled_cases.four_mover

        "U-Swap" ->
            toggled_cases.u_swap

        "E-Swap" ->
            toggled_cases.e_swap

        "S-Swap" ->
            toggled_cases.s_swap

        "M-Swap" ->
            toggled_cases.m_swap

        "Alg" ->
            toggled_cases.alg

        "F-Swap" ->
            toggled_cases.f_swap

        "S-Insert" ->
            toggled_cases.s_insert

        _ ->
            let
                _ =
                    Debug.log "Unkown UF case type." comm
            in
            False


isValidCaseUFR : ToggledCasesUFR -> CommUFR -> Bool
isValidCaseUFR toggled_cases comm =
    case comm.comm_type of
        "U-Any / U-Any" ->
            toggled_cases.u_any__u_any

        "U-Top / D-Side" ->
            toggled_cases.u_top__d_side

        "U-Top / D-Bottom" ->
            toggled_cases.u_top__d_bottom

        "Special" ->
            toggled_cases.special

        "U-Side / D-Any" ->
            toggled_cases.u_side__d_any

        "LUF / D-Any" ->
            toggled_cases.luf_d__any

        "BUR / D-Any" ->
            toggled_cases.bur__d_any

        "D-Side / D-Side" ->
            toggled_cases.d_side__d_side

        "D-Side / D-Bottom" ->
            toggled_cases.d_side__d_bottom

        "D-Bottom / D-Bottom" ->
            toggled_cases.d_bottom__d_bottom

        _ ->
            let
                _ =
                    Debug.log "Unkown UF case type." comm
            in
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked _, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged _, _ ) ->
            ( model, Cmd.none )

        ( ToggleType type_to_toggle, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setToggledTypes (state.settings.toggled_types |> toggleType type_to_toggle)
            in
            ( Loaded state
                |> setSettings new_settings
            , send GenerateCurrentComms
            )

        ( ToggleCaseUF case_to_toggle, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setToggledCasesUF (state.settings.toggled_cases_uf |> toggleCasesUF case_to_toggle)
            in
            ( Loaded state
                |> setSettings new_settings
            , send GenerateCurrentComms
            )

        ( ToggleCaseUFR case_to_toggle, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setToggledCasesUFR (state.settings.toggled_cases_ufr |> toggleCasesUFR case_to_toggle)
            in
            ( Loaded state
                |> setSettings new_settings
            , send GenerateCurrentComms
            )

        ( GenerateCurrentComms, Loaded state ) ->
            let
                new_comms =
                    List.concat
                        [ if state.settings.toggled_types.uf then
                            List.map toCommTypeUF (List.filter (isValidCaseUF state.settings.toggled_cases_uf) state.data.uf)

                          else
                            []
                        , if state.settings.toggled_types.ufr then
                            List.map toCommTypeUFR (List.filter (isValidCaseUFR state.settings.toggled_cases_ufr) state.data.ufr)

                          else
                            []
                        , if state.settings.toggled_types.parity then
                            List.map toCommTypeParity state.data.parity

                          else
                            []
                        , if state.settings.toggled_types.two_flips then
                            List.map toCommType2Flip state.data.two_flips

                          else
                            []
                        , if state.settings.toggled_types.two_twists then
                            List.map toCommType2Twist state.data.two_twists

                          else
                            []
                        , if state.settings.toggled_types.three_twists then
                            List.map toCommType3Twist state.data.three_twists

                          else
                            []
                        ]
            in
            ( Loaded state |> setCurrentComms new_comms, newComm state.current_comms )

        ( GenerateNewComm, Loaded state ) ->
            ( model, newComm state.current_comms )

        ( UpdateComm index, Loaded state ) ->
            let
                new_comm =
                    state.current_comms |> List.drop (index - 1) |> List.head
            in
            ( Loaded { state | current_comm = new_comm }, newCommMessage (new_comm |> getComm) )

        ( GeneratedNewComm alg, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setCommVisible False

                new_state =
                    Loaded { state | current_setup_alg = Just alg, settings = new_settings }
            in
            ( new_state, updateSettings new_settings )

        ( ToggleCommVisible, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setCommVisible (not state.settings.comm_visible)

                new_state =
                    Loaded { state | settings = new_settings }
            in
            ( new_state, updateSettings new_settings )

        ( ToggleCommTypeVisible, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setCommTypeVisible (not state.settings.comm_type_visible)

                new_state =
                    Loaded { state | settings = new_settings }
            in
            ( new_state, updateSettings new_settings )

        ( ToggleLettersVisible, Loaded state ) ->
            let
                new_settings =
                    state.settings |> setLettersVisible (not state.settings.letters_visible)

                new_state =
                    Loaded { state | settings = new_settings }
            in
            ( new_state, updateSettings new_settings )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


viewToggleButton : String -> CommType -> Bool -> Html Msg
viewToggleButton content type_to_toggle toggled =
    button
        [ class "mr-auto hover:bg-emerald-700 ease-in-out duration-75 text-white font-bold py-2 px-4 rounded w-1/2 mb-2"
        , class
            (if toggled == False then
                "bg-emerald-400"

             else
                "bg-emerald-600"
            )
        , onClick (ToggleType type_to_toggle)
        ]
        [ text content ]


viewToggleCaseUFButton : String -> CaseUF -> Bool -> Html Msg
viewToggleCaseUFButton content case_to_toggle toggled =
    button
        [ class "ml-auto hover:bg-emerald-700 ease-in-out duration-75 text-white font-bold py-2 px-4 rounded w-1/2 mb-2"
        , class
            (if toggled == False then
                "bg-emerald-400"

             else
                "bg-emerald-600"
            )
        , onClick (ToggleCaseUF case_to_toggle)
        ]
        [ text content ]


viewToggleCaseUFRButton : String -> CaseUFR -> Bool -> Html Msg
viewToggleCaseUFRButton content case_to_toggle toggled =
    button
        [ class "ml-auto hover:bg-emerald-700 ease-in-out duration-75 text-white font-bold py-2 px-4 rounded w-1/2 mb-2"
        , class
            (if toggled == False then
                "bg-emerald-400"

             else
                "bg-emerald-600"
            )
        , onClick (ToggleCaseUFR case_to_toggle)
        ]
        [ text content ]


viewComm : CommType -> Settings -> Html Msg
viewComm c settings =
    div [ class "text-5xl text-white font-bold text-center" ]
        (case c of
            UF (Just comm) ->
                [ span
                    [ class
                        (if settings.comm_type_visible == True then
                            ""

                         else
                            "blur-lg"
                        )
                    , onClick ToggleCommTypeVisible
                    ]
                    [ text (comm.comm_type ++ ": ") ]
                , span
                    [ class
                        (if settings.letters_visible == True then
                            ""

                         else
                            "blur-md"
                        )
                    , onClick ToggleLettersVisible
                    ]
                    [ text (comm.piece1 ++ " -> " ++ comm.piece2) ]
                ]

            UFR (Just comm) ->
                [ span
                    [ class
                        (if settings.comm_type_visible == True then
                            ""

                         else
                            "blur-lg"
                        )
                    , onClick ToggleCommTypeVisible
                    ]
                    [ text (comm.comm_type ++ ": ") ]
                , span
                    [ class
                        (if settings.letters_visible == True then
                            ""

                         else
                            "blur-md"
                        )
                    , onClick ToggleLettersVisible
                    ]
                    [ text (comm.piece1 ++ " -> " ++ comm.piece2) ]
                ]

            Parity (Just comm) ->
                [ span
                    [ class
                        (if settings.letters_visible == True then
                            ""

                         else
                            "blur-md"
                        )
                    , onClick ToggleLettersVisible
                    ]
                    [ text comm.piece ]
                ]

            TwoFlips (Just comm) ->
                [ span
                    [ class
                        (if settings.letters_visible == True then
                            ""

                         else
                            "blur-md"
                        )
                    , onClick ToggleLettersVisible
                    ]
                    [ text (comm.piece1 ++ " -> " ++ comm.piece2) ]
                ]

            TwoTwists (Just comm) ->
                [ span
                    [ class
                        (if settings.letters_visible == True then
                            ""

                         else
                            "blur-md"
                        )
                    , onClick ToggleLettersVisible
                    ]
                    [ text (comm.piece1 ++ " -> " ++ comm.piece2) ]
                ]

            ThreeTwists (Just comm) ->
                [ span
                    [ class
                        (if settings.letters_visible == True then
                            ""

                         else
                            "blur-md"
                        )
                    , onClick ToggleLettersVisible
                    ]
                    [ text (comm.piece1 ++ " -> " ++ comm.piece2) ]
                ]

            _ ->
                [ span [] [ text ": " ] ]
        )


view : Model -> Browser.Document Msg
view model =
    { title = "3-Style"
    , body =
        [ div [ class "bg-zinc-900 h-screen w-screen" ]
            [ case model of
                Loaded state ->
                    div [ class "h-full w-full flex flex-col justify-center items-center" ]
                        [ div [ class "h-1/2 w-3/4 flex flex-row justify-center" ]
                            [ div [ class "h-full w-1/6 rounded-lg drop-shadow-xl m-2 flex flex-col" ]
                                [ button
                                    [ class "ml-auto bg-emerald-400 hover:bg-emerald-700 ease-in-out duration-75 text-white font-bold py-2 px-4 rounded w-1/2 mb-2"
                                    , onClick GenerateNewComm
                                    ]
                                    [ text "Skip" ]
                                , div
                                    [ class
                                        (if state.settings.toggled_types.uf then
                                            "flex flex-col"

                                         else
                                            "hidden sm:hidden md:hidden lg:hidden xl:hidden"
                                        )
                                    , class "mt-4"
                                    ]
                                    [ viewToggleCaseUFButton "4-Mover" FourMover state.settings.toggled_cases_uf.four_mover
                                    , viewToggleCaseUFButton "U-Swap" USwap state.settings.toggled_cases_uf.u_swap
                                    , viewToggleCaseUFButton "E-Swap" ESwap state.settings.toggled_cases_uf.e_swap
                                    , viewToggleCaseUFButton "S-Swap" SSwap state.settings.toggled_cases_uf.s_swap
                                    , viewToggleCaseUFButton "M-Swap" MSwap state.settings.toggled_cases_uf.m_swap
                                    , viewToggleCaseUFButton "F-Swap" FSwap state.settings.toggled_cases_uf.f_swap
                                    , viewToggleCaseUFButton "S-Insert" SInsert state.settings.toggled_cases_uf.s_insert
                                    , viewToggleCaseUFButton "Alg" Alg state.settings.toggled_cases_uf.alg
                                    ]
                                , div
                                    [ class
                                        (if state.settings.toggled_types.ufr then
                                            "flex flex-col"

                                         else
                                            "hidden sm:hidden md:hidden lg:hidden xl:hidden"
                                        )
                                    , class "mt-4"
                                    ]
                                    [ viewToggleCaseUFRButton "U-Any / U-Any" UAnyUAny state.settings.toggled_cases_ufr.u_any__u_any
                                    , viewToggleCaseUFRButton "U-Top / D-Side" UTopDSide state.settings.toggled_cases_ufr.u_top__d_side
                                    , viewToggleCaseUFRButton "U-Top / D-Bottom" UTopDBottom state.settings.toggled_cases_ufr.u_top__d_bottom
                                    , viewToggleCaseUFRButton "U-Side / D-Any" USideDAny state.settings.toggled_cases_ufr.u_side__d_any
                                    , viewToggleCaseUFRButton "D-Side / D-Side" DSideDSide state.settings.toggled_cases_ufr.d_side__d_side
                                    , viewToggleCaseUFRButton "D-Side / D-Bottom" DSideDBottom state.settings.toggled_cases_ufr.d_side__d_bottom
                                    , viewToggleCaseUFRButton "D-Bottom / D-Bottom" DBottomDBottom state.settings.toggled_cases_ufr.d_bottom__d_bottom
                                    , viewToggleCaseUFRButton "LUF / D-Any" LUFDAny state.settings.toggled_cases_ufr.luf_d__any
                                    , viewToggleCaseUFRButton "BUR / D-Any" BURDAny state.settings.toggled_cases_ufr.bur__d_any
                                    , viewToggleCaseUFRButton "Special" Special state.settings.toggled_cases_ufr.special
                                    ]
                                ]
                            , div [ class "h-full w-4/6 bg-cyan-200 rounded-lg drop-shadow-xl m-2" ]
                                [ div [ class "h-full w-full text-center p-6 text-white text-2xl font-bold", id "twisty-player-container" ]
                                    [ case state.current_setup_alg of
                                        Just current_setup_alg ->
                                            text current_setup_alg

                                        _ ->
                                            text "No setup alg."
                                    ]
                                ]
                            , div [ class "h-full w-1/6 rounded-lg drop-shadow-xl m-2 flex flex-col" ]
                                [ viewToggleButton "UF" (UF Nothing) state.settings.toggled_types.uf
                                , viewToggleButton "UFR" (UFR Nothing) state.settings.toggled_types.ufr
                                , viewToggleButton "Parity" (Parity Nothing) state.settings.toggled_types.parity
                                , viewToggleButton "2 Flips" (TwoFlips Nothing) state.settings.toggled_types.two_flips
                                , viewToggleButton "2 Twists" (TwoTwists Nothing) state.settings.toggled_types.two_twists
                                , viewToggleButton "3 Twists" (ThreeTwists Nothing) state.settings.toggled_types.three_twists
                                ]
                            ]
                        , div [ class "border h-1/3 w-1/2 mt-10 p-5" ]
                            [ case ( state.current_comm, state.current_comm |> getComm ) of
                                ( Just current_comm_obj, Just current_comm ) ->
                                    div [ class "h-full w-full flex flex-col justify-center place-content-center p-10" ]
                                        [ viewComm current_comm_obj state.settings
                                        , div
                                            [ class "text-3xl text-white font-bold w-3/4 m-auto text-center"
                                            , class
                                                (if state.settings.comm_visible == True then
                                                    ""

                                                 else
                                                    "blur"
                                                )
                                            , onClick ToggleCommVisible
                                            ]
                                            [ text current_comm ]
                                        ]

                                ( _, _ ) ->
                                    h1 [ class "text-5xl text-white font-bold text-center" ] [ text "No active comm" ]
                            ]
                        ]

                Error ->
                    div [ class "h-full w-full flex flex-col justify-center items-center" ]
                        [ div
                            [ class "h-1/2 w-1/2 bg-red-400 rounded-lg drop-shadow-xl p-10"
                            , class "flex flex-col justify-center items-center"
                            ]
                            [ h1 [ class "text-9xl text-center text-white" ] [ text "ERROR" ]
                            , h2 [ class "text-5xl text-center" ] [ text "An error occured" ]
                            ]
                        ]
            ]
        ]
    }



---- PROGRAM ----


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
