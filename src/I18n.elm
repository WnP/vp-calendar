module I18n exposing (..)

import Time
import Time.Format.I18n.I_de_de as TimeDe
import Time.Format.I18n.I_en_us as TimeEn
import Time.Format.I18n.I_et_ee as TimeEt
import Time.Format.I18n.I_fi_fi as TimeFi
import Time.Format.I18n.I_fr_fr as TimeFr
import Time.Format.I18n.I_ja_jp as TimeJa
import Time.Format.I18n.I_lt_lt as TimeLt
import Time.Format.I18n.I_nl_nl as TimeNl
import Time.Format.I18n.I_pl_pl as TimePl
import Time.Format.I18n.I_pt_br as TimePt
import Time.Format.I18n.I_ro_ro as TimeRo
import Time.Format.I18n.I_ru_ru as TimeRu
import Time.Format.I18n.I_sv_se as TimeSv


type Language
    = De -- German
    | En -- English
    | Et -- Estonian
    | Fi -- Finnish
    | Fr -- French
    | Ja -- Japanese
    | Lt -- Lithuanian
    | Nl -- Dutch
    | Pl -- Polish
    | Pt -- Portuguese
    | Ro -- Romanian
    | Ru -- Russian
    | Sv -- Swedish
    | Es -- Spanish


availableLanguages : List Language
availableLanguages =
    [ De
    , En
    , Et
    , Fi
    , Fr
    , Ja
    , Lt
    , Nl
    , Pl
    , Pt
    , Ro
    , Ru
    , Sv
    , Es
    ]


shortCode : Language -> String
shortCode lang =
    case lang of
        De ->
            "de"

        En ->
            "en"

        Et ->
            "et"

        Fi ->
            "fi"

        Fr ->
            "fr"

        Ja ->
            "ja"

        Lt ->
            "lt"

        Nl ->
            "nl"

        Pl ->
            "pl"

        Pt ->
            "pt"

        Ro ->
            "ro"

        Ru ->
            "ru"

        Sv ->
            "sv"

        Es ->
            "es"


parseLang : String -> Language
parseLang l =
    let
        lang =
            String.toLower l
    in
    if String.contains "de" lang then
        De

    else if String.contains "et" lang then
        Et

    else if String.contains "fi" lang then
        Fi

    else if String.contains "fr" lang then
        Fr

    else if String.contains "ja" lang then
        Ja

    else if String.contains "lt" lang then
        Lt

    else if String.contains "nl" lang then
        Nl

    else if String.contains "pl" lang then
        Pl

    else if String.contains "pt" lang then
        Pt

    else if String.contains "ro" lang then
        Ro

    else if String.contains "ru" lang then
        Ru

    else if String.contains "sv" lang then
        Sv

    else if String.contains "es" lang then
        Es

    else
        En


getMonthFormater : Language -> (Time.Month -> String)
getMonthFormater lang =
    case lang of
        De ->
            TimeDe.monthName

        En ->
            TimeEn.monthName

        Et ->
            TimeEt.monthName

        Fi ->
            TimeFi.monthName

        Fr ->
            TimeFr.monthName

        Ja ->
            TimeJa.monthName

        Lt ->
            TimeLt.monthName

        Nl ->
            TimeNl.monthName

        Pl ->
            TimePl.monthName

        Pt ->
            TimePt.monthName

        Ro ->
            TimeRo.monthName

        Ru ->
            TimeRu.monthName

        Sv ->
            TimeSv.monthName

        Es ->
            esMonthFormater


getDayFormater : Language -> (Time.Weekday -> String)
getDayFormater lang =
    case lang of
        De ->
            TimeDe.dayShort

        En ->
            TimeEn.dayShort

        Et ->
            TimeEt.dayShort

        Fi ->
            TimeFi.dayShort

        Fr ->
            TimeFr.dayShort

        Ja ->
            TimeJa.dayShort

        Lt ->
            TimeLt.dayShort

        Nl ->
            TimeNl.dayShort

        Pl ->
            TimePl.dayShort

        Pt ->
            TimePt.dayShort

        Ro ->
            TimeRo.dayShort

        Ru ->
            TimeRu.dayShort

        Sv ->
            TimeSv.dayShort

        Es ->
            esDayFormater


toString : Language -> String
toString lang =
    case lang of
        De ->
            "Deutsche"

        En ->
            "English"

        Et ->
            "Eestlane"

        Fi ->
            "Suomalainen"

        Fr ->
            "Français"

        Ja ->
            "日本人"

        Lt ->
            "Lietuvis"

        Nl ->
            "Nederlandse"

        Pl ->
            "Polskie"

        Pt ->
            "Português"

        Ro ->
            "Română"

        Ru ->
            "Pусский"

        Sv ->
            "Svenska"

        Es ->
            "Español"


esMonthFormater : Time.Month -> String
esMonthFormater month =
    case month of
        Time.Jan ->
            "Enero"

        Time.Feb ->
            "Febrero"

        Time.Mar ->
            "Marzo"

        Time.Apr ->
            "Abril"

        Time.May ->
            "Mayo"

        Time.Jun ->
            "Junio"

        Time.Jul ->
            "Julio"

        Time.Aug ->
            "Agosto"

        Time.Sep ->
            "Septiembre"

        Time.Oct ->
            "Octubre"

        Time.Nov ->
            "Noviembre"

        Time.Dec ->
            "Diciembre"


esDayFormater : Time.Weekday -> String
esDayFormater day =
    case day of
        Time.Mon ->
            "Lu"

        Time.Tue ->
            "Ma"

        Time.Wed ->
            "Mi"

        Time.Thu ->
            "Ju"

        Time.Fri ->
            "Vi"

        Time.Sat ->
            "Sa"

        Time.Sun ->
            "Do"
