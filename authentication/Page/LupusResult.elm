module Page.LupusResult exposing (Model, Msg, init, update, view)


import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Element
import Element.Font as Font
--import Axis exposing (bottom)

import Element.Input as Input
import Browser.Navigation as Nav

-- resources
    -- https://ellie-app.com/4jQvybPfNTma1


-- MAIN




-- MODEL
type alias Model =
    {userId: String, navKey: Nav.Key}


type Msg
    = GoDonate
    | GoSymptoms


init : String -> Nav.Key -> ( Model, Cmd Msg )
init userId navKey =
    ( initialModel userId navKey,  Cmd.none )

initialModel: String -> Nav.Key -> (Model)
initialModel userId navKey =
    { navKey = navKey,
      userId = userId 
    }

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoDonate ->
            (model, Nav.load "https://support.lupus.org/site/Donation2;jsessionid=00000000.app30040b?idb=1305452635&df_id=3742&mfc_pref=T&3742.donation=form1&NONCE_TOKEN=7D8E9177D1DB4C61CEEA4214E1BFE60F&s_subsrc=button_nav_donate&s_src=lupus.org&3742_donation=form1")
        GoSymptoms ->
            (model, Nav.pushUrl model.navKey <| "/users/symptoms/"++model.userId)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


view : Model -> Html Msg
view model =
    div [class "container"][
        layout [  ] <|
          column [ width fill, Font.family
            [ Font.typeface "Montserrat"
            , Font.sansSerif
            ] ][ header model, summary, content, button, footer]
    ]
    


header : Model -> Element Msg
header model =
    row [ width fill, Background.color (rgb 1 0.29 0.17), spacing 20, padding 20 ]
        [ el [ Font.color(Element.rgb 1 1 1), Font.bold] (text "LupusLight") ]


summary : Element Msg
summary =
    column [ width (fill|> maximum 800|> minimum 250),
            Background.color (rgb 1 1 1), centerX, spacing 10, padding 40,
            Border.color (rgb 1 0.85 0.85),
            Border.rounded 3]

        [ 
            textColumn [] [],
            textColumn [] [],

            textColumn []  [myResult],
            textColumn [] [],
            textColumn [] [],
            textColumn [] [diagnosis],
        
            textColumn []
                [image [ alignBottom, width fill, centerX]
                    { src = "/img/sampleClassification.png", description = "Sample Classification"} ]
            
        ]


content : Element Msg
content =
    column
        [ width(fill |> maximum 800 |> minimum 250), 
        Background.color (rgb 1 1 1), centerX, spacing 10, padding 40]
        
        [   
            textColumn [] [factTitle],
            textColumn [] [],
            textColumn []
                [
                    image [ width(fill |> maximum 800 |> minimum 250), centerX]{ 
                        src = "/img/SLE2.png", description = "SLE" }],

            textColumn [] [],
            textColumn [] [lupus, lupus1],
            textColumn [] [],
            textColumn [] [],
            textColumn [] [],
            
            textColumn [][  
                image [ paddingEach { edges | left = 20 }, alignRight, centerY]{ 
                     src = "/img/Pie1.png", description = "MaleFemale" },
                female, female2],
            textColumn [] [],
            textColumn [] [],
            textColumn [] [],


            textColumn [][ 
                image [ paddingEach { edges | right = 20 }, alignLeft ]{ 
                    src = "/img/spend.png", description = "fee"}, 
                spend, spend1, spend2],
            textColumn [] [],
            textColumn [] [],
            textColumn [] [],


            textColumn [][ 
                image [ paddingEach { edges | left = 20 }, alignRight, centerY ]{ 
                    src = "/img/Pie2.png", description = "MultipleDisease"}, 
                multipleDisease, multipleDisease1, multipleDisease2 ],
            textColumn [] [],
            textColumn [] [],
            textColumn [] [],


            textColumn [][ 
                image [ paddingEach { edges | right = 20 }, alignLeft, centerY ]{ 
                    src = "/img/year.png", description = "yearNeed"}, 
                year, year0, year1, year2],
            textColumn [] []

        ]


button : Element Msg
button =
    row [ 
        width(fill |> maximum 800 |> minimum 250), 
        Background.color (rgb 1 1 1), centerX, spacing 10, padding 40 ]
        
        [ button1,button2 ]



-- define 

button1 :  Element Msg
button1 =
    Input.button
        [ 
        Background.color(Element.rgb 0.55 0.73 0.85), 
        paddingXY 25 15, Border.rounded 3,
        Element.mouseOver[Background.color(Element.rgb 0.1 0.18 0.95)],
        Element.focused[Background.color(Element.rgb 0.1 0.18 0.95)] ]{
            onPress = Just GoDonate,
            --link[]{ label = text "donate", url = "http://fruits.com"}, 
            label = el [Font.size 19, Font.color(Element.rgb 1 1 1), Font.bold, spacing 20](text "DONATE FOR LUPUS FAMILY")}

--goDonate : Msg
--goDonate  =
--    link[]{} "https://support.lupus.org/site/Donation2;jsessionid=00000000.app362b?idb=1574577297&df_id=3742&mfc_pref=T&3742.donation=form1&NONCE_TOKEN=E7C32967D16B3C66702AC0A6CE680520&s_subsrc=button_nav_donate&s_src=lupus.org&3742_donation=form1")


button2 : Element Msg
button2 =
    Input.button
        [ 
        Background.color(Element.rgb 0.9 0.57 0.57), 
        paddingXY 15 15, Border.rounded 3,
        Element.mouseOver[Background.color(Element.rgb 0.94 0.1 0.1)],
        Element.focused[Background.color(Element.rgb 0.94 0.1 0.1)] ]{
            onPress = Just GoSymptoms, 
            label = el [Font.size 19, Font.color(Element.rgb 1 1 1), Font.bold, spacing 20](text "BUILD DAILY LUPUS PROFILE")}






myResult : Element Msg
myResult =
    paragraph []
        [ 
            el [
                Font.color(Element.rgb 0.4 0.4 0.4),
                Font.extraBold, Font.size 28, spacing 20 ](text "My Result\n")
        ]

diagnosis : Element Msg
diagnosis =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text "Based on you RNA-seqquence information, we classified you 🌟 as "),
        el [Font.color(Element.rgb 1 0.25 0.42), Font.bold](text "Lupus Carrier."),
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text " Please contact your doctor for further diagnosis.")
        ]

factTitle : Element Msg
factTitle =
    paragraph []
        [ 
            el [Font.color(Element.rgb 0.4 0.4 0.4),
                Font.extraBold, Font.size 28, spacing 20 ](text "Fact About Lupus")
        ]
    

lupus : Element Msg
lupus =
    paragraph []

        [
        el [Font.color(Element.rgb 1 0.25 0.42), Font.bold, spacing 20](text "Lupus ") ,
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ]( text "is an inflammatory disease caused when the immune system attacks its own tissues.")
        ]


lupus1 : Element Msg
lupus1 =
    paragraph []

        [
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ]( text "The most common type of lupus is called Systemic Lupus Erythematosus (SLE). SLE is an autoimmune disease in which the immune system attacks its own tissues, causing widespread inflammation and tissue damage in the affected organs.")
        ]


female : Element Msg
female =
    paragraph []

        [
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ]( text "Anyone can get Lupus. The Lupus Foundation of America estimates that 1.5 million Americans, and at least five million people worldwide, have a form of lupus. About "),  
        el [Font.color(Element.rgb 0.4 0.4 0.4), Font.bold, spacing 20](text "9 out of 10 "),   
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text "of lupus are in "),
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text "women ages 15 to 44. ")
        ]


female2 : Element Msg
female2 =
    paragraph []

        [
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text "African-American women are  "),
        el [Font.color(Element.rgb 0.4 0.4 0.4), Font.bold, spacing 20](text "three times "),
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text "more likely to get lupus than white women.  Lupus is also more common in Hispanic, Asian, and Native American and Alaskan Native women. ")
        ]

multipleDisease : Element Msg
multipleDisease =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "People with lupus will development many diverse sympotoms and other disease. ")        ]


multipleDisease1 : Element Msg
multipleDisease1 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), Font.bold, spacing 20](text "1 in 3 "),  
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20 ](text "lupus patients suffer from multiple autoimmune diseases, including. Common diseases that overlap with lupsu include Celia disease, Myasthenia gravis, Rheumatoid arthritis etc. ")        ]

    
multipleDisease2 : Element Msg
multipleDisease2 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), Font.bold, spacing 20](text "Inflammation "), 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text " caused by Lupus can affect many areas in body, including kidneys, brain and central nervous, lungs, heart and so on.")
        ]


spend : Element Msg
spend =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "A 2016 study published in Nature Reviews Rheumatology found that the average annual direct health care costs of a person with lupus was $33,223. ")
        ]

spend1 : Element Msg
spend1 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "The study also determined that the average annual productivity cost was between $1,252 and $20,046. ")
        ]

spend2 : Element Msg
spend2 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "The mean annual total costs for people with lupus (combining direct and indirect costs) can be as high as $50,000. These estimates may be higher among people with lupus nephritis and more severe or active lupus.")
        ]


year : Element Msg
year =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "People have only a few mild symptoms at the beginning, such as fatigue, rash on skin, dry mouth or eyes, which makes the diagnosis difficult.")
        ]

year0 : Element Msg
year0 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "There is no single test to diagnose lupus. Typically, the combination of blood and urine tests, signs and symptoms, and physical examination findings are used for diagnosis.")
        ]

year1 : Element Msg
year1 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "On average, it takes nearly "),
        el [Font.color(Element.rgb 0.4 0.4 0.4), Font.bold, spacing 20](text "6 "),
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "years for people with lupus to be diagnosed, from the time they first notice their lupus symptoms.")
        ]

year2 : Element Msg
year2 =
    paragraph []
        [ 
        el [Font.color(Element.rgb 0.4 0.4 0.4), Font.bold, spacing 20](text "63% "),
        el [Font.color(Element.rgb 0.4 0.4 0.4), spacing 20]( text "of people with lupus surveyed report being incorrectly diagnosed.")
        ]


footer : Element Msg
footer =
    row [ height (px 80), width fill, Background.color (rgb 1 1 1) , spacing 20, padding 20] []

