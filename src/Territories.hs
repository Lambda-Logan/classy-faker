{-# LANGUAGE OverloadedStrings #-}

module Territories where

import Data.Text (Text)
import Lens.Micro (Lens', lens)
import Prelude hiding (GT, LT)

data Alpha2Code
  = AD
  | AE
  | AF
  | AG
  | AI
  | AL
  | AM
  | AO
  | AQ
  | AR
  | AS
  | AT
  | AU
  | AW
  | AX
  | AZ
  | BA
  | BB
  | BD
  | BE
  | BF
  | BG
  | BH
  | BI
  | BJ
  | BL
  | BM
  | BO
  | BQ
  | BR
  | BS
  | BT
  | BW
  | BY
  | BZ
  | CA
  | CC
  | CD
  | CF
  | CG
  | CH
  | CI
  | CK
  | CL
  | CM
  | CN
  | CO
  | CR
  | CU
  | CV
  | CW
  | CX
  | CY
  | CZ
  | DE
  | DJ
  | DK
  | DM
  | DO
  | DZ
  | EC
  | EE
  | EG
  | EH
  | ER
  | ES
  | ET
  | FI
  | FJ
  | FK
  | FM
  | FO
  | FR
  | GA
  | GB
  | GD
  | GE
  | GF
  | GG
  | GH
  | GI
  | GL
  | GM
  | GN
  | GP
  | GQ
  | GR
  | GS
  | GT
  | GU
  | GW
  | GY
  | HK
  | HM
  | HN
  | HR
  | HT
  | HU
  | ID
  | IE
  | IL
  | IM
  | IN
  | IO
  | IQ
  | IR
  | IS
  | IT
  | JE
  | JM
  | JO
  | JP
  | KE
  | KG
  | KH
  | KI
  | KM
  | KN
  | KP
  | KR
  | KW
  | KY
  | KZ
  | LA
  | LB
  | LC
  | LI
  | LK
  | LR
  | LS
  | LT
  | LU
  | LV
  | LY
  | MA
  | MC
  | MD
  | ME
  | MF
  | MG
  | MH
  | MK
  | ML
  | MM
  | MN
  | MO
  | MP
  | MQ
  | MR
  | MS
  | MT
  | MU
  | MV
  | MW
  | MX
  | MY
  | MZ
  | NA
  | NC
  | NE
  | NF
  | NG
  | NI
  | NL
  | NO
  | NP
  | NR
  | NU
  | NZ
  | OM
  | PA
  | PE
  | PF
  | PG
  | PH
  | PK
  | PL
  | PM
  | PN
  | PR
  | PS
  | PT
  | PW
  | PY
  | QA
  | RE
  | RO
  | RS
  | RU
  | RW
  | SA
  | SB
  | SC
  | SD
  | SE
  | SG
  | SH
  | SI
  | SJ
  | SK
  | SL
  | SM
  | SN
  | SO
  | SR
  | SS
  | ST
  | SV
  | SX
  | SY
  | SZ
  | TC
  | TD
  | TF
  | TG
  | TH
  | TJ
  | TK
  | TL
  | TM
  | TN
  | TO
  | TR
  | TT
  | TV
  | TW
  | TZ
  | UA
  | UG
  | US
  | UY
  | UZ
  | VA
  | VC
  | VE
  | VG
  | VI
  | VN
  | VU
  | WF
  | WS
  | YE
  | YT
  | ZA
  | ZM
  | ZW
  deriving (Enum, Show, Eq, Ord)

data Territory = Territory
  { alpha2Code :: !Alpha2Code,
    alpha3Code :: !Text,
    ccTLD :: !Text,
    numericCode :: !Text,
    territoryName :: !Text,
    officialStateName :: !Text
  }
  deriving (Show, Eq, Ord)

alpha2CodeToTerr AF = Territory AF "AFG" ".af" "4" "Afghanistan" "The Islamic Republic of Afghanistan"
alpha2CodeToTerr AX = Territory AX "ALA" ".ax" "248" "Åland Islands" "Åland"
alpha2CodeToTerr AL = Territory AL "ALB" ".al" "8" "Albania" "The Republic of Albania"
alpha2CodeToTerr DZ = Territory DZ "DZA" ".dz" "12" "Algeria" "The People's Democratic Republic of Algeria"
alpha2CodeToTerr AS = Territory AS "ASM" ".as" "16" "American Samoa" "The Territory of American Samoa"
alpha2CodeToTerr AD = Territory AD "AND" ".ad" "20" "Andorra" "The Principality of Andorra"
alpha2CodeToTerr AO = Territory AO "AGO" ".ao" "24" "Angola" "The Republic of Angola"
alpha2CodeToTerr AI = Territory AI "AIA" ".ai" "660" "Anguilla" "Anguilla"
alpha2CodeToTerr AQ = Territory AQ "ATA" ".aq" "10" "Antarctica " "All land and ice shelves south of the 60th parallel south"
alpha2CodeToTerr AG = Territory AG "ATG" ".ag" "28" "Antigua and Barbuda" "Antigua and Barbuda"
alpha2CodeToTerr AR = Territory AR "ARG" ".ar" "32" "Argentina" "The Argentine Republic"
alpha2CodeToTerr AM = Territory AM "ARM" ".am" "51" "Armenia" "The Republic of Armenia"
alpha2CodeToTerr AW = Territory AW "ABW" ".aw" "533" "Aruba" "Aruba"
alpha2CodeToTerr AU = Territory AU "AUS" ".au" "36" "Australia " "The Commonwealth of Australia"
alpha2CodeToTerr AT = Territory AT "AUT" ".at" "40" "Austria" "The Republic of Austria"
alpha2CodeToTerr AZ = Territory AZ "AZE" ".az" "31" "Azerbaijan" "The Republic of Azerbaijan"
alpha2CodeToTerr BS = Territory BS "BHS" ".bs" "44" "Bahamas (the)" "The Commonwealth of The Bahamas"
alpha2CodeToTerr BH = Territory BH "BHR" ".bh" "48" "Bahrain" "The Kingdom of Bahrain"
alpha2CodeToTerr BD = Territory BD "BGD" ".bd" "50" "Bangladesh" "The People's Republic of Bangladesh"
alpha2CodeToTerr BB = Territory BB "BRB" ".bb" "52" "Barbados" "Barbados"
alpha2CodeToTerr BY = Territory BY "BLR" ".by" "112" "Belarus" "The Republic of Belarus"
alpha2CodeToTerr BE = Territory BE "BEL" ".be" "56" "Belgium" "The Kingdom of Belgium"
alpha2CodeToTerr BZ = Territory BZ "BLZ" ".bz" "84" "Belize" "Belize"
alpha2CodeToTerr BJ = Territory BJ "BEN" ".bj" "204" "Benin" "The Republic of Benin"
alpha2CodeToTerr BM = Territory BM "BMU" ".bm" "60" "Bermuda" "Bermuda"
alpha2CodeToTerr BT = Territory BT "BTN" ".bt" "64" "Bhutan" "The Kingdom of Bhutan"
alpha2CodeToTerr BO = Territory BO "BOL" ".bo" "68" "Bolivia" "The Plurinational State of Bolivia"
alpha2CodeToTerr BQ = Territory BQ "BES" ".bq" "535" "Bonaire" "Bonaire, Sint Eustatius and Saba"
alpha2CodeToTerr BA = Territory BA "BIH" ".ba" "70" "Bosnia and Herzegovina" "Bosnia and Herzegovina"
alpha2CodeToTerr BW = Territory BW "BWA" ".bw" "72" "Botswana" "The Republic of Botswana"
alpha2CodeToTerr BR = Territory BR "BRA" ".br" "76" "Brazil" "The Federative Republic of Brazil"
alpha2CodeToTerr IO = Territory IO "IOT" ".io" "86" "British Indian Ocean Territory" "The British Indian Ocean Territory"
alpha2CodeToTerr BG = Territory BG "BGR" ".bg" "100" "Bulgaria" "The Republic of Bulgaria"
alpha2CodeToTerr BF = Territory BF "BFA" ".bf" "854" "Burkina Faso" "Burkina Faso"
alpha2CodeToTerr BI = Territory BI "BDI" ".bi" "108" "Burundi" "The Republic of Burundi"
alpha2CodeToTerr CV = Territory CV "CPV" ".cv" "132" "Cabo Verde " "The Republic of Cabo Verde"
alpha2CodeToTerr KH = Territory KH "KHM" ".kh" "116" "Cambodia" "The Kingdom of Cambodia"
alpha2CodeToTerr CM = Territory CM "CMR" ".cm" "120" "Cameroon" "The Republic of Cameroon"
alpha2CodeToTerr CA = Territory CA "CAN" ".ca" "124" "Canada" "Canada"
alpha2CodeToTerr KY = Territory KY "CYM" ".ky" "136" "Cayman Islands" "The Cayman Islands"
alpha2CodeToTerr CF = Territory CF "CAF" ".cf" "140" "Central African Republic" "The Central African Republic"
alpha2CodeToTerr TD = Territory TD "TCD" ".td" "148" "Chad" "The Republic of Chad"
alpha2CodeToTerr CL = Territory CL "CHL" ".cl" "152" "Chile" "The Republic of Chile"
alpha2CodeToTerr CN = Territory CN "CHN" ".cn" "156" "China" "The People's Republic of China"
alpha2CodeToTerr CX = Territory CX "CXR" ".cx" "162" "Christmas Island" "The Territory of Christmas Island"
alpha2CodeToTerr CC = Territory CC "CCK" ".cc" "166" "Cocos Islands" "The Territory of Cocos (Keeling) Islands"
alpha2CodeToTerr CO = Territory CO "COL" ".co" "170" "Colombia" "The Republic of Colombia"
alpha2CodeToTerr KM = Territory KM "COM" ".km" "174" "Comoros" "The Union of the Comoros"
alpha2CodeToTerr CD = Territory CD "COD" ".cd" "180" "Democratic Republic of the Congo" "The Democratic Republic of the Congo"
alpha2CodeToTerr CG = Territory CG "COG" ".cg" "178" "Congo" "The Republic of the Congo"
alpha2CodeToTerr CK = Territory CK "COK" ".ck" "184" "Cook Islands" "The Cook Islands"
alpha2CodeToTerr CR = Territory CR "CRI" ".cr" "188" "Costa Rica" "The Republic of Costa Rica"
alpha2CodeToTerr CI = Territory CI "CIV" ".ci" "384" "Côte d'Ivoire " "The Republic of Côte d'Ivoire"
alpha2CodeToTerr HR = Territory HR "HRV" ".hr" "191" "Croatia" "The Republic of Croatia"
alpha2CodeToTerr CU = Territory CU "CUB" ".cu" "192" "Cuba" "The Republic of Cuba"
alpha2CodeToTerr CW = Territory CW "CUW" ".cw" "531" "Curaçao" "The Country of Curaçao"
alpha2CodeToTerr CY = Territory CY "CYP" ".cy" "196" "Cyprus" "The Republic of Cyprus"
alpha2CodeToTerr CZ = Territory CZ "CZE" ".cz" "203" "Czechia " "The Czech Republic"
alpha2CodeToTerr DK = Territory DK "DNK" ".dk" "208" "Denmark" "The Kingdom of Denmark"
alpha2CodeToTerr DJ = Territory DJ "DJI" ".dj" "262" "Djibouti" "The Republic of Djibouti"
alpha2CodeToTerr DM = Territory DM "DMA" ".dm" "212" "Dominica" "The Commonwealth of Dominica"
alpha2CodeToTerr DO = Territory DO "DOM" ".do" "214" "Dominican Republic" "The Dominican Republic"
alpha2CodeToTerr EC = Territory EC "ECU" ".ec" "218" "Ecuador" "The Republic of Ecuador"
alpha2CodeToTerr EG = Territory EG "EGY" ".eg" "818" "Egypt" "The Arab Republic of Egypt"
alpha2CodeToTerr SV = Territory SV "SLV" ".sv" "222" "El Salvador" "The Republic of El Salvador"
alpha2CodeToTerr GQ = Territory GQ "GNQ" ".gq" "226" "Equatorial Guinea" "The Republic of Equatorial Guinea"
alpha2CodeToTerr ER = Territory ER "ERI" ".er" "232" "Eritrea" "The State of Eritrea"
alpha2CodeToTerr EE = Territory EE "EST" ".ee" "233" "Estonia" "The Republic of Estonia"
alpha2CodeToTerr SZ = Territory SZ "SWZ" ".sz" "748" "Eswatini " "The Kingdom of Eswatini"
alpha2CodeToTerr ET = Territory ET "ETH" ".et" "231" "Ethiopia" "The Federal Democratic Republic of Ethiopia"
alpha2CodeToTerr FK = Territory FK "FLK" ".fk" "238" "Falkland Islands" "The Falkland Islands"
alpha2CodeToTerr FO = Territory FO "FRO" ".fo" "234" "Faroe Islands" "The Faroe Islands"
alpha2CodeToTerr FJ = Territory FJ "FJI" ".fj" "242" "Fiji" "The Republic of Fiji"
alpha2CodeToTerr FI = Territory FI "FIN" ".fi" "246" "Finland" "The Republic of Finland"
alpha2CodeToTerr FR = Territory FR "FRA" ".fr" "250" "France " "The French Republic"
alpha2CodeToTerr GF = Territory GF "GUF" ".gf" "254" "French Guiana" "Guyane"
alpha2CodeToTerr PF = Territory PF "PYF" ".pf" "258" "French Polynesia" "French Polynesia"
alpha2CodeToTerr TF = Territory TF "ATF" ".tf" "260" "French Southern Territories" "The French Southern and Antarctic Lands"
alpha2CodeToTerr GA = Territory GA "GAB" ".ga" "266" "Gabon" "The Gabonese Republic"
alpha2CodeToTerr GM = Territory GM "GMB" ".gm" "270" "Gambia" "The Republic of The Gambia"
alpha2CodeToTerr GE = Territory GE "GEO" ".ge" "268" "Georgia" "Georgia"
alpha2CodeToTerr DE = Territory DE "DEU" ".de" "276" "Germany" "The Federal Republic of Germany"
alpha2CodeToTerr GH = Territory GH "GHA" ".gh" "288" "Ghana" "The Republic of Ghana"
alpha2CodeToTerr GI = Territory GI "GIB" ".gi" "292" "Gibraltar" "Gibraltar"
alpha2CodeToTerr GR = Territory GR "GRC" ".gr" "300" "Greece" "The Hellenic Republic"
alpha2CodeToTerr GL = Territory GL "GRL" ".gl" "304" "Greenland" "Kalaallit Nunaat"
alpha2CodeToTerr GD = Territory GD "GRD" ".gd" "308" "Grenada" "Grenada"
alpha2CodeToTerr GP = Territory GP "GLP" ".gp" "312" "Guadeloupe" "Guadeloupe"
alpha2CodeToTerr GU = Territory GU "GUM" ".gu" "316" "Guam" "The Territory of Guam"
alpha2CodeToTerr GT = Territory GT "GTM" ".gt" "320" "Guatemala" "The Republic of Guatemala"
alpha2CodeToTerr GG = Territory GG "GGY" ".gg" "831" "Guernsey" "The Bailiwick of Guernsey"
alpha2CodeToTerr GN = Territory GN "GIN" ".gn" "324" "Guinea" "The Republic of Guinea"
alpha2CodeToTerr GW = Territory GW "GNB" ".gw" "624" "Guinea-Bissau" "The Republic of Guinea-Bissau"
alpha2CodeToTerr GY = Territory GY "GUY" ".gy" "328" "Guyana" "The Co-operative Republic of Guyana"
alpha2CodeToTerr HT = Territory HT "HTI" ".ht" "332" "Haiti" "The Republic of Haiti"
alpha2CodeToTerr HM = Territory HM "HMD" ".hm" "334" "Heard Island and McDonald Islands" "The Territory of Heard Island and McDonald Islands"
alpha2CodeToTerr VA = Territory VA "VAT" ".va" "336" "Vatican City" "The Holy See"
alpha2CodeToTerr HN = Territory HN "HND" ".hn" "340" "Honduras" "The Republic of Honduras"
alpha2CodeToTerr HK = Territory HK "HKG" ".hk" "344" "Hong Kong" "The Hong Kong Special Administrative Region of China"
alpha2CodeToTerr HU = Territory HU "HUN" ".hu" "348" "Hungary" "Hungary"
alpha2CodeToTerr IS = Territory IS "ISL" ".is" "352" "Iceland" "Iceland"
alpha2CodeToTerr IN = Territory IN "IND" ".in" "356" "India" "The Republic of India"
alpha2CodeToTerr ID = Territory ID "IDN" ".id" "360" "Indonesia" "The Republic of Indonesia"
alpha2CodeToTerr IR = Territory IR "IRN" ".ir" "364" "Iran" "The Islamic Republic of Iran"
alpha2CodeToTerr IQ = Territory IQ "IRQ" ".iq" "368" "Iraq" "The Republic of Iraq"
alpha2CodeToTerr IE = Territory IE "IRL" ".ie" "372" "Ireland" "Ireland"
alpha2CodeToTerr IM = Territory IM "IMN" ".im" "833" "Isle of Man" "The Isle of Man"
alpha2CodeToTerr IL = Territory IL "ISR" ".il" "376" "Israel" "The State of Israel"
alpha2CodeToTerr IT = Territory IT "ITA" ".it" "380" "Italy" "The Italian Republic"
alpha2CodeToTerr JM = Territory JM "JAM" ".jm" "388" "Jamaica" "Jamaica"
alpha2CodeToTerr JP = Territory JP "JPN" ".jp" "392" "Japan" "Japan"
alpha2CodeToTerr JE = Territory JE "JEY" ".je" "832" "Jersey" "The Bailiwick of Jersey"
alpha2CodeToTerr JO = Territory JO "JOR" ".jo" "400" "Jordan" "The Hashemite Kingdom of Jordan"
alpha2CodeToTerr KZ = Territory KZ "KAZ" ".kz" "398" "Kazakhstan" "The Republic of Kazakhstan"
alpha2CodeToTerr KE = Territory KE "KEN" ".ke" "404" "Kenya" "The Republic of Kenya"
alpha2CodeToTerr KI = Territory KI "KIR" ".ki" "296" "Kiribati" "The Republic of Kiribati"
alpha2CodeToTerr KP = Territory KP "PRK" ".kp" "408" "North Korea " "The Democratic People's Republic of Korea"
alpha2CodeToTerr KR = Territory KR "KOR" ".kr" "410" "South Korea " "The Republic of Korea"
alpha2CodeToTerr KW = Territory KW "KWT" ".kw" "414" "Kuwait" "The State of Kuwait"
alpha2CodeToTerr KG = Territory KG "KGZ" ".kg" "417" "Kyrgyzstan" "The Kyrgyz Republic"
alpha2CodeToTerr LA = Territory LA "LAO" ".la" "418" "Laos " "The Lao People's Democratic Republic"
alpha2CodeToTerr LV = Territory LV "LVA" ".lv" "428" "Latvia" "The Republic of Latvia"
alpha2CodeToTerr LB = Territory LB "LBN" ".lb" "422" "Lebanon" "The Lebanese Republic"
alpha2CodeToTerr LS = Territory LS "LSO" ".ls" "426" "Lesotho" "The Kingdom of Lesotho"
alpha2CodeToTerr LR = Territory LR "LBR" ".lr" "430" "Liberia" "The Republic of Liberia"
alpha2CodeToTerr LY = Territory LY "LBY" ".ly" "434" "Libya" "The State of Libya"
alpha2CodeToTerr LI = Territory LI "LIE" ".li" "438" "Liechtenstein" "The Principality of Liechtenstein"
alpha2CodeToTerr LT = Territory LT "LTU" ".lt" "440" "Lithuania" "The Republic of Lithuania"
alpha2CodeToTerr LU = Territory LU "LUX" ".lu" "442" "Luxembourg" "The Grand Duchy of Luxembourg"
alpha2CodeToTerr MO = Territory MO "MAC" ".mo" "446" "Macao " "The Macao Special Administrative Region of China"
alpha2CodeToTerr MK = Territory MK "MKD" ".mk" "807" "North Macedonia " "The Republic of North Macedonia"
alpha2CodeToTerr MG = Territory MG "MDG" ".mg" "450" "Madagascar" "The Republic of Madagascar"
alpha2CodeToTerr MW = Territory MW "MWI" ".mw" "454" "Malawi" "The Republic of Malawi"
alpha2CodeToTerr MY = Territory MY "MYS" ".my" "458" "Malaysia" "Malaysia"
alpha2CodeToTerr MV = Territory MV "MDV" ".mv" "462" "Maldives" "The Republic of Maldives"
alpha2CodeToTerr ML = Territory ML "MLI" ".ml" "466" "Mali" "The Republic of Mali"
alpha2CodeToTerr MT = Territory MT "MLT" ".mt" "470" "Malta" "The Republic of Malta"
alpha2CodeToTerr MH = Territory MH "MHL" ".mh" "584" "Marshall Islands" "The Republic of the Marshall Islands"
alpha2CodeToTerr MQ = Territory MQ "MTQ" ".mq" "474" "Martinique" "Martinique"
alpha2CodeToTerr MR = Territory MR "MRT" ".mr" "478" "Mauritania" "The Islamic Republic of Mauritania"
alpha2CodeToTerr MU = Territory MU "MUS" ".mu" "480" "Mauritius" "The Republic of Mauritius"
alpha2CodeToTerr YT = Territory YT "MYT" ".yt" "175" "Mayotte" "The Department of Mayotte"
alpha2CodeToTerr MX = Territory MX "MEX" ".mx" "484" "Mexico" "The United Mexican States"
alpha2CodeToTerr FM = Territory FM "FSM" ".fm" "583" "Micronesia" "The Federated States of Micronesia"
alpha2CodeToTerr MD = Territory MD "MDA" ".md" "498" "Moldova" "The Republic of Moldova"
alpha2CodeToTerr MC = Territory MC "MCO" ".mc" "492" "Monaco" "The Principality of Monaco"
alpha2CodeToTerr MN = Territory MN "MNG" ".mn" "496" "Mongolia" "Mongolia"
alpha2CodeToTerr ME = Territory ME "MNE" ".me" "499" "Montenegro" "Montenegro"
alpha2CodeToTerr MS = Territory MS "MSR" ".ms" "500" "Montserrat" "Montserrat"
alpha2CodeToTerr MA = Territory MA "MAR" ".ma" "504" "Morocco" "The Kingdom of Morocco"
alpha2CodeToTerr MZ = Territory MZ "MOZ" ".mz" "508" "Mozambique" "The Republic of Mozambique"
alpha2CodeToTerr MM = Territory MM "MMR" ".mm" "104" "Myanmar " "The Republic of the Union of Myanmar"
alpha2CodeToTerr NA = Territory NA "NAM" ".na" "516" "Namibia" "The Republic of Namibia"
alpha2CodeToTerr NR = Territory NR "NRU" ".nr" "520" "Nauru" "The Republic of Nauru"
alpha2CodeToTerr NP = Territory NP "NPL" ".np" "524" "Nepal" "The Federal Democratic Republic of Nepal"
alpha2CodeToTerr NL = Territory NL "NLD" ".nl" "528" "Netherlands (the)" "The Kingdom of the Netherlands"
alpha2CodeToTerr NC = Territory NC "NCL" ".nc" "540" "New Caledonia" "New Caledonia"
alpha2CodeToTerr NZ = Territory NZ "NZL" ".nz" "554" "New Zealand" "New Zealand"
alpha2CodeToTerr NI = Territory NI "NIC" ".ni" "558" "Nicaragua" "The Republic of Nicaragua"
alpha2CodeToTerr NE = Territory NE "NER" ".ne" "562" "Niger (the)" "The Republic of the Niger"
alpha2CodeToTerr NG = Territory NG "NGA" ".ng" "566" "Nigeria" "The Federal Republic of Nigeria"
alpha2CodeToTerr NU = Territory NU "NIU" ".nu" "570" "Niue" "Niue"
alpha2CodeToTerr NF = Territory NF "NFK" ".nf" "574" "Norfolk Island" "The Territory of Norfolk Island"
alpha2CodeToTerr MP = Territory MP "MNP" ".mp" "580" "Northern Mariana Islands" "The Commonwealth of the Northern Mariana Islands"
alpha2CodeToTerr NO = Territory NO "NOR" ".no" "578" "Norway" "The Kingdom of Norway"
alpha2CodeToTerr OM = Territory OM "OMN" ".om" "512" "Oman" "The Sultanate of Oman"
alpha2CodeToTerr PK = Territory PK "PAK" ".pk" "586" "Pakistan" "The Islamic Republic of Pakistan"
alpha2CodeToTerr PW = Territory PW "PLW" ".pw" "585" "Palau" "The Republic of Palau"
alpha2CodeToTerr PS = Territory PS "PSE" ".ps" "275" "Palestine, State of" "The State of Palestine"
alpha2CodeToTerr PA = Territory PA "PAN" ".pa" "591" "Panama" "The Republic of Panamá"
alpha2CodeToTerr PG = Territory PG "PNG" ".pg" "598" "Papua New Guinea" "The Independent State of Papua New Guinea"
alpha2CodeToTerr PY = Territory PY "PRY" ".py" "600" "Paraguay" "The Republic of Paraguay"
alpha2CodeToTerr PE = Territory PE "PER" ".pe" "604" "Peru" "The Republic of Perú"
alpha2CodeToTerr PH = Territory PH "PHL" ".ph" "608" "Philippines" "The Republic of the Philippines"
alpha2CodeToTerr PN = Territory PN "PCN" ".pn" "612" "Pitcairn " "The Pitcairn, Henderson, Ducie and Oeno Islands"
alpha2CodeToTerr PL = Territory PL "POL" ".pl" "616" "Poland" "The Republic of Poland"
alpha2CodeToTerr PT = Territory PT "PRT" ".pt" "620" "Portugal" "The Portuguese Republic"
alpha2CodeToTerr PR = Territory PR "PRI" ".pr" "630" "Puerto Rico" "The Commonwealth of Puerto Rico"
alpha2CodeToTerr QA = Territory QA "QAT" ".qa" "634" "Qatar" "The State of Qatar"
alpha2CodeToTerr RE = Territory RE "REU" ".re" "638" "Réunion" "Réunion"
alpha2CodeToTerr RO = Territory RO "ROU" ".ro" "642" "Romania" "Romania"
alpha2CodeToTerr RU = Territory RU "RUS" ".ru" "643" "Russia" "The Russian Federation"
alpha2CodeToTerr RW = Territory RW "RWA" ".rw" "646" "Rwanda" "The Republic of Rwanda"
alpha2CodeToTerr BL = Territory BL "BLM" ".bl" "652" "Saint Barthélemy" "The Collectivity of Saint-Barthélemy"
alpha2CodeToTerr SH = Territory SH "SHN" ".sh" "654" "Saint Helena" "Saint Helena, Ascension and Tristan da Cunha"
alpha2CodeToTerr KN = Territory KN "KNA" ".kn" "659" "Saint Kitts and Nevis" "Saint Kitts and Nevis"
alpha2CodeToTerr LC = Territory LC "LCA" ".lc" "662" "Saint Lucia" "Saint Lucia"
alpha2CodeToTerr MF = Territory MF "MAF" ".mf" "663" "Saint Martin (French part)" "The Collectivity of Saint-Martin"
alpha2CodeToTerr PM = Territory PM "SPM" ".pm" "666" "Saint Pierre and Miquelon" "The Overseas Collectivity of Saint-Pierre and Miquelon"
alpha2CodeToTerr VC = Territory VC "VCT" ".vc" "670" "Saint Vincent and the Grenadines" "Saint Vincent and the Grenadines"
alpha2CodeToTerr WS = Territory WS "WSM" ".ws" "882" "Samoa" "The Independent State of Samoa"
alpha2CodeToTerr SM = Territory SM "SMR" ".sm" "674" "San Marino" "The Republic of San Marino"
alpha2CodeToTerr ST = Territory ST "STP" ".st" "678" "Sao Tome and Principe" "The Democratic Republic of São Tomé and Príncipe"
alpha2CodeToTerr SA = Territory SA "SAU" ".sa" "682" "Saudi Arabia" "The Kingdom of Saudi Arabia"
alpha2CodeToTerr SN = Territory SN "SEN" ".sn" "686" "Senegal" "The Republic of Senegal"
alpha2CodeToTerr RS = Territory RS "SRB" ".rs" "688" "Serbia" "The Republic of Serbia"
alpha2CodeToTerr SC = Territory SC "SYC" ".sc" "690" "Seychelles" "The Republic of Seychelles"
alpha2CodeToTerr SL = Territory SL "SLE" ".sl" "694" "Sierra Leone" "The Republic of Sierra Leone"
alpha2CodeToTerr SG = Territory SG "SGP" ".sg" "702" "Singapore" "The Republic of Singapore"
alpha2CodeToTerr SX = Territory SX "SXM" ".sx" "534" "Sint Maarten (Dutch part)" "Sint Maarten"
alpha2CodeToTerr SK = Territory SK "SVK" ".sk" "703" "Slovakia" "The Slovak Republic"
alpha2CodeToTerr SI = Territory SI "SVN" ".si" "705" "Slovenia" "The Republic of Slovenia"
alpha2CodeToTerr SB = Territory SB "SLB" ".sb" "90" "Solomon Islands" "The Solomon Islands"
alpha2CodeToTerr SO = Territory SO "SOM" ".so" "706" "Somalia" "The Federal Republic of Somalia"
alpha2CodeToTerr ZA = Territory ZA "ZAF" ".za" "710" "South Africa" "The Republic of South Africa"
alpha2CodeToTerr GS = Territory GS "SGS" ".gs" "239" "South Georgia & Sandwich Islands" "South Georgia and the South Sandwich Islands"
alpha2CodeToTerr SS = Territory SS "SSD" ".ss" "728" "South Sudan" "The Republic of South Sudan"
alpha2CodeToTerr ES = Territory ES "ESP" ".es" "724" "Spain" "The Kingdom of Spain"
alpha2CodeToTerr LK = Territory LK "LKA" ".lk" "144" "Sri Lanka" "The Democratic Socialist Republic of Sri Lanka"
alpha2CodeToTerr SD = Territory SD "SDN" ".sd" "729" "Sudan (the)" "The Republic of the Sudan"
alpha2CodeToTerr SR = Territory SR "SUR" ".sr" "740" "Suriname" "The Republic of Suriname"
alpha2CodeToTerr SJ = Territory SJ "SJM" "" "744" "Svalbard" "Svalbard and Jan Mayen"
alpha2CodeToTerr SE = Territory SE "SWE" ".se" "752" "Sweden" "The Kingdom of Sweden"
alpha2CodeToTerr CH = Territory CH "CHE" ".ch" "756" "Switzerland" "The Swiss Confederation"
alpha2CodeToTerr SY = Territory SY "SYR" ".sy" "760" "Syria" "The Syrian Arab Republic"
alpha2CodeToTerr TW = Territory TW "TWN" ".tw" "158" "Taiwan" "The Republic of China"
alpha2CodeToTerr TJ = Territory TJ "TJK" ".tj" "762" "Tajikistan" "The Republic of Tajikistan"
alpha2CodeToTerr TZ = Territory TZ "TZA" ".tz" "834" "Tanzania" "The United Republic of Tanzania"
alpha2CodeToTerr TH = Territory TH "THA" ".th" "764" "Thailand" "The Kingdom of Thailand"
alpha2CodeToTerr TL = Territory TL "TLS" ".tl" "626" "Timor-Leste" "The Democratic Republic of Timor-Leste"
alpha2CodeToTerr TG = Territory TG "TGO" ".tg" "768" "Togo" "The Togolese Republic"
alpha2CodeToTerr TK = Territory TK "TKL" ".tk" "772" "Tokelau" "Tokelau"
alpha2CodeToTerr TO = Territory TO "TON" ".to" "776" "Tonga" "The Kingdom of Tonga"
alpha2CodeToTerr TT = Territory TT "TTO" ".tt" "780" "Trinidad and Tobago" "The Republic of Trinidad and Tobago"
alpha2CodeToTerr TN = Territory TN "TUN" ".tn" "788" "Tunisia" "The Republic of Tunisia"
alpha2CodeToTerr TR = Territory TR "TUR" ".tr" "792" "Turkey" "The Republic of Turkey"
alpha2CodeToTerr TM = Territory TM "TKM" ".tm" "795" "Turkmenistan" "Turkmenistan"
alpha2CodeToTerr TC = Territory TC "TCA" ".tc" "796" "Turks and Caicos Islands" "The Turks and Caicos Islands"
alpha2CodeToTerr TV = Territory TV "TUV" ".tv" "798" "Tuvalu" "Tuvalu"
alpha2CodeToTerr UG = Territory UG "UGA" ".ug" "800" "Uganda" "The Republic of Uganda"
alpha2CodeToTerr UA = Territory UA "UKR" ".ua" "804" "Ukraine" "Ukraine"
alpha2CodeToTerr AE = Territory AE "ARE" ".ae" "784" "United Arab Emirates" "The United Arab Emirates"
alpha2CodeToTerr GB = Territory GB "GBR" ".uk" "826" "United Kingdom" "The United Kingdom of Great Britain and Northern Ireland"
alpha2CodeToTerr US = Territory US "USA" ".us" "840" "United States of America" "The United States of America"
alpha2CodeToTerr UY = Territory UY "URY" ".uy" "858" "Uruguay" "The Oriental Republic of Uruguay"
alpha2CodeToTerr UZ = Territory UZ "UZB" ".uz" "860" "Uzbekistan" "The Republic of Uzbekistan"
alpha2CodeToTerr VU = Territory VU "VUT" ".vu" "548" "Vanuatu" "The Republic of Vanuatu"
alpha2CodeToTerr VE = Territory VE "VEN" ".ve" "862" "Venezuela" "The Bolivarian Republic of Venezuela"
alpha2CodeToTerr VN = Territory VN "VNM" ".vn" "704" "Vietnam" "The Socialist Republic of Viet Nam"
alpha2CodeToTerr VG = Territory VG "VGB" ".vg" "92" "British Virgin Islands" "The Virgin Islands"
alpha2CodeToTerr VI = Territory VI "VIR" ".vi" "850" "U.S Virgin Islands" "The Virgin Islands of the United States"
alpha2CodeToTerr WF = Territory WF "WLF" ".wf" "876" "Wallis and Futuna" "The Territory of the Wallis and Futuna Islands"
alpha2CodeToTerr EH = Territory EH "ESH" ".aj" "732" "Western Sahara" "The Sahrawi Arab Democratic Republic"
alpha2CodeToTerr YE = Territory YE "YEM" ".ye" "887" "Yemen" "The Republic of Yemen"
alpha2CodeToTerr ZM = Territory ZM "ZMB" ".zm" "894" "Zambia" "The Republic of Zambia"
alpha2CodeToTerr ZW = Territory ZW "ZWE" ".zw" "716" "Zimbabwe" "The Republic of Zimbabwe"
