module Weather exposing
    ( Weather
    , weatherDecoder
    )

import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Url.Parser exposing (Parser, custom)


type alias Weather =
    { location : WeatherLocation
    , current : WeatherCurrent
    }

type alias WeatherLocation =
    { name : String
    , region : String
    , country : String
    , lat : Float
    , lon : Float
    , tz_id : String
    , localtime_epoch : Int
    , localtime : String
    }

type alias WeatherCurrentCondition =
    { text : String
    , icon : String
    , code : Int
    }

type alias WeatherCurrentAir_quality =
    { co : Float
    , no2 : Float
    , o3 : Float
    , so2 : Float
    , pm2_5 : Float
    , pm10 : Float
    , usepaindex : Int
    , gbdefraindex : Int
    }

type alias WeatherCurrent =
    { last_updated_epoch : Int
    , last_updated : String
    , temp_c : Float
    , temp_f : Float
    , is_day : Int
    , condition : WeatherCurrentCondition
    , wind_mph : Float
    , wind_kph : Float
    , wind_degree : Int
    , wind_dir : String
    , pressure_mb : Float
    , pressure_in : Float
    , precip_mm : Float
    , precip_in : Float
    , humidity : Int
    , cloud : Int
    , feelslike_c : Float
    , feelslike_f : Float
    , vis_km : Float
    , vis_miles : Float
    , uv : Float
    , gust_mph : Float
    , gust_kph : Float
    , air_quality : WeatherCurrentAir_quality
    }

weatherDecoder : Json.Decode.Decoder Weather
weatherDecoder =
    Json.Decode.succeed Weather
        |> required "location" decodeWeatherLocation
        |> required "current" decodeWeatherCurrent

decodeWeatherLocation : Json.Decode.Decoder WeatherLocation
decodeWeatherLocation =
    Json.Decode.succeed WeatherLocation
        |> required "name"  Json.Decode.string
        |> required  "region"  Json.Decode.string
        |> required  "country"  Json.Decode.string
        |> required  "lat"  Json.Decode.float
        |> required  "lon"  Json.Decode.float
        |> required  "tz_id"  Json.Decode.string
        |> required  "localtime_epoch"  Json.Decode.int
        |> required  "localtime"  Json.Decode.string

decodeWeatherCurrentCondition : Json.Decode.Decoder WeatherCurrentCondition
decodeWeatherCurrentCondition =
    Json.Decode.succeed WeatherCurrentCondition
        |> required  "text"  Json.Decode.string
        |> required  "icon"  Json.Decode.string
        |> required  "code"  Json.Decode.int

decodeWeatherCurrentAir_quality : Json.Decode.Decoder WeatherCurrentAir_quality
decodeWeatherCurrentAir_quality =
    Json.Decode.succeed WeatherCurrentAir_quality
        |> required  "co"  Json.Decode.float
        |> required  "no2"  Json.Decode.float
        |> required  "o3"  Json.Decode.float
        |> required  "so2"  Json.Decode.float
        |> required  "pm2_5"  Json.Decode.float
        |> required  "pm10"  Json.Decode.float
        |> required  "us-epa-index"  Json.Decode.int
        |> required  "gb-defra-index"  Json.Decode.int

decodeWeatherCurrent : Json.Decode.Decoder WeatherCurrent
decodeWeatherCurrent =
    Json.Decode.succeed WeatherCurrent
        |> required  "last_updated_epoch"  Json.Decode.int
        |> required  "last_updated"  Json.Decode.string
        |> required  "temp_c"  Json.Decode.float
        |> required  "temp_f"  Json.Decode.float
        |> required  "is_day"  Json.Decode.int
        |> required  "condition"  decodeWeatherCurrentCondition
        |> required  "wind_mph"  Json.Decode.float
        |> required  "wind_kph"  Json.Decode.float
        |> required  "wind_degree"  Json.Decode.int
        |> required  "wind_dir"  Json.Decode.string
        |> required  "pressure_mb"  Json.Decode.float
        |> required  "pressure_in"  Json.Decode.float
        |> required  "precip_mm"  Json.Decode.float
        |> required  "precip_in"  Json.Decode.float
        |> required  "humidity"  Json.Decode.int
        |> required  "cloud"  Json.Decode.int
        |> required  "feelslike_c"  Json.Decode.float
        |> required  "feelslike_f"  Json.Decode.float
        |> required  "vis_km"  Json.Decode.float
        |> required  "vis_miles"  Json.Decode.float
        |> required  "uv"  Json.Decode.float
        |> required  "gust_mph"  Json.Decode.float
        |> required  "gust_kph"  Json.Decode.float
        |> required  "air_quality"  decodeWeatherCurrentAir_quality

encodeWeather : Weather -> Json.Encode.Value
encodeWeather record =
    Json.Encode.object
        [ ("location",  encodeWeatherLocation <| record.location)
        , ("current",  encodeWeatherCurrent <| record.current)
        ]

encodeWeatherLocation : WeatherLocation -> Json.Encode.Value
encodeWeatherLocation record =
    Json.Encode.object
        [ ("name",  Json.Encode.string <| record.name)
        , ("region",  Json.Encode.string <| record.region)
        , ("country",  Json.Encode.string <| record.country)
        , ("lat",  Json.Encode.float <| record.lat)
        , ("lon",  Json.Encode.float <| record.lon)
        , ("tz_id",  Json.Encode.string <| record.tz_id)
        , ("localtime_epoch",  Json.Encode.int <| record.localtime_epoch)
        , ("localtime",  Json.Encode.string <| record.localtime)
        ]

encodeWeatherCurrentCondition : WeatherCurrentCondition -> Json.Encode.Value
encodeWeatherCurrentCondition record =
    Json.Encode.object
        [ ("text",  Json.Encode.string <| record.text)
        , ("icon",  Json.Encode.string <| record.icon)
        , ("code",  Json.Encode.int <| record.code)
        ]

encodeWeatherCurrentAir_quality : WeatherCurrentAir_quality -> Json.Encode.Value
encodeWeatherCurrentAir_quality record =
    Json.Encode.object
        [ ("co",  Json.Encode.float <| record.co)
        , ("no2",  Json.Encode.float <| record.no2)
        , ("o3",  Json.Encode.float <| record.o3)
        , ("so2",  Json.Encode.float <| record.so2)
        , ("pm2_5",  Json.Encode.float <| record.pm2_5)
        , ("pm10",  Json.Encode.float <| record.pm10)
        , ("us-epa-index",  Json.Encode.int <| record.usepaindex)
        , ("gb-defra-index",  Json.Encode.int <| record.gbdefraindex)
        ]

encodeWeatherCurrent : WeatherCurrent -> Json.Encode.Value
encodeWeatherCurrent record =
    Json.Encode.object
        [ ("last_updated_epoch",  Json.Encode.int <| record.last_updated_epoch)
        , ("last_updated",  Json.Encode.string <| record.last_updated)
        , ("temp_c",  Json.Encode.float <| record.temp_c)
        , ("temp_f",  Json.Encode.float <| record.temp_f)
        , ("is_day",  Json.Encode.int <| record.is_day)
        , ("condition",  encodeWeatherCurrentCondition <| record.condition)
        , ("wind_mph",  Json.Encode.float <| record.wind_mph)
        , ("wind_kph",  Json.Encode.float <| record.wind_kph)
        , ("wind_degree",  Json.Encode.int <| record.wind_degree)
        , ("wind_dir",  Json.Encode.string <| record.wind_dir)
        , ("pressure_mb",  Json.Encode.float <| record.pressure_mb)
        , ("pressure_in",  Json.Encode.float <| record.pressure_in)
        , ("precip_mm",  Json.Encode.float <| record.precip_mm)
        , ("precip_in",  Json.Encode.float <| record.precip_in)
        , ("humidity",  Json.Encode.int <| record.humidity)
        , ("cloud",  Json.Encode.int <| record.cloud)
        , ("feelslike_c",  Json.Encode.float <| record.feelslike_c)
        , ("feelslike_f",  Json.Encode.float <| record.feelslike_f)
        , ("vis_km",  Json.Encode.float <| record.vis_km)
        , ("vis_miles",  Json.Encode.float <| record.vis_miles)
        , ("uv",  Json.Encode.float <| record.uv)
        , ("gust_mph",  Json.Encode.float <| record.gust_mph)
        , ("gust_kph",  Json.Encode.float <| record.gust_kph)
        , ("air_quality",  encodeWeatherCurrentAir_quality <| record.air_quality)
        ]