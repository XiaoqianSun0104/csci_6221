module User exposing
    ( User
    , emptyUser
    , newUserEncoder
    , userDecoder
    , userEncoder
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)


type alias User =
    { id : String
    , username : String
    , password : String
    , zip : Int
    , dataGathered: Bool
    , lupusResult: Int
    , jointPain: Bool
    , jointPainNum: Int
    , jointSwelling: Bool
    , jointSwellingNum: Int
    , fatigue: Bool
    , fatigueNum: Int
    , musclePain: Bool
    , musclePainNum: Int
    , fever: Bool
    , feverNum: Int
    , rash: Bool
    , rashNum: Int
    , chestPain: Bool
    , chestPainNum: Int
    , hairLoss: Bool
    , hairLossNum: Int
    , sunlightSensitivity: Bool
    , sunlightSensitivityNum: Int
    , kidney: Bool
    , kidneyNum: Int
    , mouthSores: Bool
    , mouthSoresNum: Int
    , anemia: Bool
    , anemiaNum: Int
    , dryEyes: Bool
    , dryEyesNum: Int
    , eyeInflammation: Bool
    , eyeInflammationNum: Int
    , eyelidRashes: Bool
    , eyelidRashesNum: Int
    , otherSymptom: String
    , otherSymptomNum: Int
    }

userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" string
        |> required "username" string
        |> required "password" string
        |> required "zip" int
        |> required "dataGathered" Decode.bool
        |> required "lupusResult" int
        |> required "jointPain" Decode.bool
        |> required "jointPainNum" int
        |> required "jointSwelling" Decode.bool
        |> required "jointSwellingNum" int
        |> required "fatigue" Decode.bool
        |> required "fatigueNum" int
        |> required "musclePain" Decode.bool
        |> required "musclePainNum" int
        |> required "fever" Decode.bool
        |> required "feverNum" int
        |> required "rash" Decode.bool
        |> required "rashNum" int
        |> required "chestPain" Decode.bool
        |> required "chestPainNum" int
        |> required "hairLoss" Decode.bool
        |> required "hairLossNum" int
        |> required "sunlightSensitivity" Decode.bool
        |> required "sunlightSensitivityNum" int
        |> required "kidney" Decode.bool
        |> required "kidneyNum" int
        |> required "mouthSores" Decode.bool
        |> required "mouthSoresNum" int
        |> required "anemia" Decode.bool
        |> required "anemiaNum" int
        |> required "dryEyes" Decode.bool
        |> required "dryEyesNum" int
        |> required "eyeInflammation" Decode.bool
        |> required "eyeInflammationNum" int
        |> required "eyelidRashes" Decode.bool
        |> required "eyelidRashesNum" int
        |> required "otherSymptom" string
        |> required "otherSymptomNum" int


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "id", Encode.string user.id )
        , ( "username", Encode.string user.username )
        , ( "password", Encode.string user.username )
        , ( "zip", Encode.int user.zip )
        , ( "dataGathered", Encode.bool user.dataGathered )
        , ("lupusResult", Encode.int user.lupusResult )
        , ("jointPain", Encode.bool user.jointPain )
        , ("jointPainNum", Encode.int user.jointPainNum )
        , ("jointSwelling", Encode.bool user.jointSwelling )
        , ("jointSwellingNum", Encode.int user.jointSwellingNum )
        , ("fatigue", Encode.bool user.fatigue )
        , ("fatigueNum", Encode.int user.fatigueNum )
        , ("musclePain", Encode.bool user.musclePain )
        , ("musclePainNum", Encode.int user.musclePainNum )
        , ("fever", Encode.bool user.fever )
        , ("feverNum", Encode.int user.feverNum )
        , ("rash", Encode.bool user.rash )
        , ("rashNum", Encode.int user.rashNum )
        , ("chestPain", Encode.bool user.chestPain )
        , ("chestPainNum", Encode.int user.chestPainNum )
        , ("hairLoss", Encode.bool user.hairLoss )
        , ("hairLossNum", Encode.int user.hairLossNum )
        , ("sunlightSensitivity", Encode.bool user.sunlightSensitivity )
        , ("sunlightSensitivityNum", Encode.int user.sunlightSensitivityNum )
        , ("kidney", Encode.bool user.kidney )
        , ("kidneyNum", Encode.int user.kidneyNum )
        , ("mouthSores", Encode.bool user.mouthSores )
        , ("mouthSoresNum", Encode.int user.mouthSoresNum )
        , ("anemia", Encode.bool user.anemia )
        , ("anemiaNum", Encode.int user.anemiaNum )
        , ("dryEyes", Encode.bool user.dryEyes )
        , ("dryEyesNum", Encode.int user.dryEyesNum )
        , ("eyeInflammation", Encode.bool user.eyeInflammation )
        , ("eyeInflammationNum", Encode.int user.eyeInflammationNum )
        , ("eyelidRashes", Encode.bool user.eyelidRashes )
        , ("eyelidRashesNum", Encode.int user.eyelidRashesNum )
        , ("otherSymptom", Encode.string user.otherSymptom )
        , ("otherSymptomNum", Encode.int user.otherSymptomNum )
        ]


newUserEncoder : User -> Encode.Value
newUserEncoder user =
    Encode.object
        [ ( "id", Encode.string user.id )
        , ( "username", Encode.string user.username )
        , ( "password", Encode.string user.password )
        , ( "zip", Encode.int user.zip )
        , ( "dataGathered", Encode.bool False )
        , ("lupusResult", Encode.int 0 )
        , ("jointPain", Encode.bool False )
        , ("jointPainNum", Encode.int 0 )
        , ("jointSwelling", Encode.bool False )
        , ("jointSwellingNum", Encode.int 0 )
        , ("fatigue", Encode.bool False )
        , ("fatigueNum", Encode.int 0 )
        , ("musclePain", Encode.bool False )
        , ("musclePainNum", Encode.int 0 )
        , ("fever", Encode.bool False )
        , ("feverNum", Encode.int 0 )
        , ("rash", Encode.bool False )
        , ("rashNum", Encode.int 0 )
        , ("chestPain", Encode.bool False )
        , ("chestPainNum", Encode.int 0 )
        , ("hairLoss", Encode.bool False )
        , ("hairLossNum", Encode.int 0 )
        , ("sunlightSensitivity", Encode.bool False )
        , ("sunlightSensitivityNum", Encode.int 0 )
        , ("kidney", Encode.bool False )
        , ("kidneyNum", Encode.int 0 )
        , ("mouthSores", Encode.bool False )
        , ("mouthSoresNum", Encode.int 0 )
        , ("anemia", Encode.bool False )
        , ("anemiaNum", Encode.int 0 )
        , ("dryEyes", Encode.bool False )
        , ("dryEyesNum", Encode.int 0 )
        , ("eyeInflammation", Encode.bool False )
        , ("eyeInflammationNum", Encode.int 0 )
        , ("eyelidRashes", Encode.bool False )
        , ("eyelidRashesNum", Encode.int 0 )
        , ("otherSymptom", Encode.string "")
        , ("otherSymptomNum", Encode.int 0)
        ]




emptyUser : User
emptyUser =
    { id = ""
    , username = ""
    , password = ""
    , zip = 20052
    , dataGathered = False
    , lupusResult= 0
    , jointPain= False
    , jointPainNum=0
    , jointSwelling= False
    , jointSwellingNum=0
    , fatigue= False
    , fatigueNum=0
    , musclePain= False
    , musclePainNum=0
    , fever= False
    , feverNum=0
    , rash= False
    , rashNum=0
    , chestPain= False
    , chestPainNum=0
    , hairLoss= False
    , hairLossNum=0
    , sunlightSensitivity= False
    , sunlightSensitivityNum=0
    , kidney= False
    , kidneyNum=0
    , mouthSores= False
    , mouthSoresNum=0
    , anemia= False
    , anemiaNum=0
    , dryEyes= False
    , dryEyesNum=0
    , eyeInflammation= False
    , eyeInflammationNum=0
    , eyelidRashes= False
    , eyelidRashesNum=0
    , otherSymptom=""
    , otherSymptomNum = 0
    }