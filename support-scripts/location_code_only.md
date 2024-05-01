
# Cleaning the “location” table

### Code only

[**Go to Project Summary**](../readme.md)

``` r
library(readr)
library(dplyr)
library(stringr)
library(purrr)

cat("libraries loaded\n")

location <- read_delim("../original-data/DiverReef_location_information.csv",
                        delim = ";")

cat("data loaded")
```

    ## libraries loaded
    ## data loaded

``` r
glimpse(location)
```

    ## Rows: 2,312
    ## Columns: 13
    ## $ source       <chr> "jimenez_gutierrez_et_al_2009", "jimenez_gutierrez_et_al_…
    ## $ diver_id     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ activity     <chr> "scuba", "scuba", "scuba", "scuba", "scuba", "scuba", "sc…
    ## $ month        <chr> "april", "april", "april", "april", "july", "july", "july…
    ## $ year         <dbl> 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 200…
    ## $ country      <chr> "spain", "spain", "spain", "spain", "spain", "spain", "sp…
    ## $ site         <chr> "benidorm", "benidorm", "benidorm", "benidorm", "benidorm…
    ## $ longitude    <chr> "38.535.367", "38.535.367", "38.535.367", "38.535.367", "…
    ## $ latitude     <chr> " -0.126224", " -0.126224", " -0.126224", " -0.126224", "…
    ## $ mpa          <chr> "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "…
    ## $ dive_site    <chr> "la_llosa", "la_llosa", "la_llosa", "la_llosa", "la_llosa…
    ## $ reef_type    <chr> "rocky_reef", "rocky_reef", "rocky_reef", "rocky_reef", "…
    ## $ visibility_m <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

``` r
# define function to replace 2nd instance of ".", if it exists
replace_second_period <- function(string) {
  # store all positions of "." from the input string
  positions <- str_locate_all(string, fixed("."))[[1]][, "start"]
  # check if string has more than one "."
  if (length(positions) >= 2) {
    # concatenate part before the second "." with part after it
    string <- str_sub(string, 1, positions[2] - 1) %>%
      str_c(str_sub(string, positions[2] + 1))
  }
  return(string)
}

# apply function to longitude and latitude columns
location2 <- location %>%
  mutate(
    longitude = map_chr(longitude, replace_second_period),
    latitude = map_chr(latitude, replace_second_period)
  )
```

``` r
# is there only the value "1" in the count vector?
unique(str_count(location2$longitude, fixed("."))) == 1
unique(str_count(location2$latitude, fixed("."))) == 1
```

    ## [1] TRUE
    ## [1] TRUE

``` r
location3 <- location2 %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))
```

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `longitude = as.numeric(longitude)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
sum(is.na(location3$longitude))  # total number of NAs in longitude
sum(is.na(location3$latitude))  # total number of NAs in latitude
```

    ## [1] 25
    ## [1] 25

``` r
# show only records where longitude is NA
location3 %>% filter(is.na(longitude))
```

    ## # A tibble: 25 × 13
    ##    source   diver_id activity month  year country site  longitude latitude mpa  
    ##    <chr>       <dbl> <chr>    <chr> <dbl> <chr>   <chr>     <dbl>    <dbl> <chr>
    ##  1 renfro_…     2274 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  2 renfro_…     2275 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  3 renfro_…     2276 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  4 renfro_…     2277 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  5 renfro_…     2278 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  6 renfro_…     2279 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  7 renfro_…     2280 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  8 renfro_…     2281 scuba    may    2023 usa     flor…        NA       NA yes  
    ##  9 renfro_…     2282 scuba    may    2023 usa     flor…        NA       NA yes  
    ## 10 renfro_…     2283 scuba    may    2023 usa     flor…        NA       NA yes  
    ## # ℹ 15 more rows
    ## # ℹ 3 more variables: dive_site <chr>, reef_type <chr>, visibility_m <chr>

``` r
# show only records for specific diver_id
location2 %>% filter(diver_id %in% 2274:2298)
```

    ## # A tibble: 25 × 13
    ##    source   diver_id activity month  year country site  longitude latitude mpa  
    ##    <chr>       <dbl> <chr>    <chr> <dbl> <chr>   <chr> <chr>     <chr>    <chr>
    ##  1 renfro_…     2274 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  2 renfro_…     2275 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  3 renfro_…     2276 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  4 renfro_…     2277 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  5 renfro_…     2278 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  6 renfro_…     2279 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  7 renfro_…     2280 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  8 renfro_…     2281 scuba    may    2023 usa     flor… 24 57.38N 80 27.3… yes  
    ##  9 renfro_…     2282 scuba    may    2023 usa     flor… 24 59.15N 80 24.5… yes  
    ## 10 renfro_…     2283 scuba    may    2023 usa     flor… 24 59.15N 80 24.5… yes  
    ## # ℹ 15 more rows
    ## # ℹ 3 more variables: dive_site <chr>, reef_type <chr>, visibility_m <chr>

``` r
# filter for desired diver_id, get only columns of interest and show on screen
cat("location\n")
location %>%
  filter(diver_id %in% 2274:2298) %>%
  select(longitude, latitude) %>%
  glimpse()

cat("\nlocation2\n")
location2 %>%
  filter(diver_id %in% 2274:2298) %>%
  select(longitude, latitude) %>%
  glimpse()
```

    ## location
    ## Rows: 25
    ## Columns: 2
    ## $ longitude <chr> "24 57.38N", "24 57.38N", "24 57.38N", "24 57.38N", "24 57.3…
    ## $ latitude  <chr> "80 27.39W", "80 27.39W", "80 27.39W", "80 27.39W", "80 27.3…
    ## 
    ## location2
    ## Rows: 25
    ## Columns: 2
    ## $ longitude <chr> "24 57.38N", "24 57.38N", "24 57.38N", "24 57.38N", "24 57.3…
    ## $ latitude  <chr> "80 27.39W", "80 27.39W", "80 27.39W", "80 27.39W", "80 27.3…

``` r
# define function to convert coordinates
convert_coord <- function(coord) {
  # split the coordinate into parts
  parts <- str_split(coord, " ")[[1]]

  # extract degrees
  degrees <- as.numeric(parts[1])
  # extract minutes and remove letter
  minutes <- as.numeric(str_replace(parts[2], "[NSEW]", ""))

  # determine the sign based on the direction (last character of the string)
  direction <- str_sub(parts[2], -1, -1)
  sign <- if_else(direction %in% c("N", "E"), 1, -1)

  # calculate decimal degrees
  decimal_degrees <- sign * (degrees + minutes / 60)

  return(decimal_degrees)
}
```

``` r
location2 <- location2 %>%
  mutate(
    # apply function when criteria is met
    longitude = map_if(
      longitude,
      ~ str_detect(., "[NSEW]"),  # check if "NSEW" is in the string
      ~ convert_coord(.)  # convert coordinate system if condition above is met
    ),
    # do the same for latitude
    latitude = map_if(
      latitude,
      ~ str_detect(., "[NSEW]"),
      ~ convert_coord(.)
    )
  )

# check that no NAs were introduced
sum(is.na(location2$longitude))
sum(is.na(location2$latitude))
```

    ## [1] 0
    ## [1] 0

``` r
location2 <- location2 %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

# check a slice of results, show a few entries that were in the correct
# coordinate format and a few that were converted
location2 %>%
  filter(diver_id %in% 2270:2278) %>%
  select(longitude, latitude) %>%
  glimpse()
```

    ## Rows: 9
    ## Columns: 2
    ## $ longitude <dbl> 42.35952, 42.35952, 42.35952, 42.35952, 24.95633, 24.95633, …
    ## $ latitude  <dbl> 10.89499, 10.89499, 10.89499, 10.89499, -80.45650, -80.45650…

``` r
# list unique values in source column
unique(location2$source)

# show type of data
cat("\nClass:\n")
class(location2$source)
```

    ##  [1] "jimenez_gutierrez_et_al_2009" "luna_et_al_2009"             
    ##  [3] "camp_and_fraser_2012"         "moity_et_al_2019"            
    ##  [5] "giglio_et_al_2016"            "roche_et_al_2016"            
    ##  [7] "giglio_et_al_2018"            "lucrezi_et_al_2021"          
    ##  [9] "saliba_et_al_2022"            "grillo_ana"                  
    ## [11] "balzaretti_merino_et_al_2021" "mcbride_2021"                
    ## [13] "casoli_edoardo"               "giglio_et_al_2022"           
    ## [15] "toso_et_al_2022"              "renfro_bobbie"               
    ## 
    ## Class:
    ## [1] "character"

``` r
length(unique(location2$diver_id))  # check number of unique diver_id
class(location2$diver_id)

# check the range of values in diver_id
min(location2$diver_id)
max(location2$diver_id)
```

    ## [1] 2312
    ## [1] "numeric"
    ## [1] 1
    ## [1] 2312

``` r
unique(location2$activity)
class(location2$activity)
```

    ## [1] "scuba"      "snorkeling" "Scuba"     
    ## [1] "character"

``` r
location2 <- location2 %>%
  mutate(
    activity = activity %>%  # in the activity column, do the following:
      case_match(
        "Scuba" ~ "scuba",  # rename Scuba as scuba
        .default = activity  # don't change any of the others
      ) %>%
      as.factor()  # format as factor
  )

unique(location2$activity)
```

    ## [1] scuba      snorkeling
    ## Levels: scuba snorkeling

``` r
unique(location2$month)

cat("\nClass:\n")
class(location2$month)
```

    ##  [1] "april"     "july"      "october"   "may"       "june"      "august"   
    ##  [7] "september" "november"  NA          "december"  "january"   "february" 
    ## [13] "na"        "march"    
    ## 
    ## Class:
    ## [1] "character"

``` r
location2 <- location2 %>%
  mutate(
    month = month %>%
      # substitute "na" for true NAs
      na_if("na") %>%
      # set levels and order
      factor(levels = c(
        "january", "february", "march", "april", "may", "june",
        "july", "august", "september", "october", "november",
        "december"
      ))
  )

# checking to see result
unique(location2$month)
```

    ##  [1] april     july      october   may       june      august    september
    ##  [8] november  <NA>      december  january   february  march    
    ## 12 Levels: january february march april may june july august ... december

``` r
unique(location2$year)
class(location2$year)
```

    ##  [1] 2004 2005 2006 2010 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
    ## [16] 2023
    ## [1] "numeric"

``` r
unique(location2$country)

cat("\nClass:\n")
class(location2$country)
```

    ## [1] "spain"       "usa"         "ecuador"     "brazil"      "philippines"
    ## [6] "italy"       "mozambique"  "mexico"      "seychelles" 
    ## 
    ## Class:
    ## [1] "character"

``` r
location2 <- location2 %>%
  mutate(
    country = country %>%
      case_match(
        "usa" ~ "USA",  # rename usa to USA
        .default = str_to_sentence(country)))  # capitalize the others

unique(location2$country)
```

    ## [1] "Spain"       "USA"         "Ecuador"     "Brazil"      "Philippines"
    ## [6] "Italy"       "Mozambique"  "Mexico"      "Seychelles"

``` r
sort(unique(location2$site))  # sort list of unique site values

cat("\nClass:\n")
class(location2$site)
```

    ##  [1] "abrolhos"                            
    ##  [2] "alcatrazes"                          
    ##  [3] "amp_tor_paterno_shoals"              
    ##  [4] "arraial_do_cabo"                     
    ##  [5] "benidorm"                            
    ##  [6] "fernando_de_noronha"                 
    ##  [7] "florida_keys"                        
    ##  [8] "galapagos_islands"                   
    ##  [9] "giglio_island"                       
    ## [10] "los_arcos"                           
    ## [11] "malapascua"                          
    ## [12] "moalboal"                            
    ## [13] "ponta_do_ouro_partial_marine_reserve"
    ## [14] "porto_cesareo"                       
    ## [15] "portofino"                           
    ## [16] "puerto_galera"                       
    ## [17] "punta_campanella"                    
    ## [18] "seychelles"                          
    ## [19] "sierra_helada_marine_park"           
    ## 
    ## Class:
    ## [1] "character"

``` r
unique(location2$mpa)
class(location2$mpa)
```

    ## [1] "yes" "no" 
    ## [1] "character"

``` r
location2 <- location2 %>%
  mutate(mpa = as.factor(mpa))

class(location2$mpa)
levels(location2$mpa)
```

    ## [1] "factor"
    ## [1] "no"  "yes"

``` r
sort(unique(location2$dive_site))

cat("\nClass:\n")
class(location2$dive_site)
```

    ##   [1] "abobrinha"                "acuario_canon"           
    ##   [3] "acuario_hongo"            "airplane"                
    ##   [5] "airplane_site"            "albatros"                
    ##   [7] "alma_jane"                "altare"                  
    ##   [9] "amp_tor_paterno_shoal"    "anequim"                 
    ##  [11] "aninuan_reef"             "aquarium_three_sisters"  
    ##  [13] "atlantis"                 "ave_maria"               
    ##  [15] "baba.boi"                 "badejo.q"                
    ##  [17] "bahia_de_sta_fe"          "baia_dell_olivetta"      
    ##  [19] "bajo_de_cristo"           "banco"                   
    ##  [21] "bantigue"                 "bartolome"               
    ##  [23] "bartolome-cousins"        "beagle"                  
    ##  [25] "beagle-daphne"            "beagle_y_guy_fawkes"     
    ##  [27] "beehive"                  "blacks"                  
    ##  [29] "booby_rock"               "buraco_das_cabras"       
    ##  [31] "buraco_do_inferno"        "cabeco_das_cordas"       
    ##  [33] "cagarras"                 "cagarras_funda"          
    ##  [35] "cala_cupa"                "cala_mezzo"              
    ##  [37] "canon"                    "canon_quijada"           
    ##  [39] "canyons"                  "cardeiros"               
    ##  [41] "carro_armato"             "casa_del_sindaco"        
    ##  [43] "caverna_da_sapata"        "channel_rock"            
    ##  [45] "chapeirao_sueste"         "cherne"                  
    ##  [47] "coffins_patch"            "colombara"               
    ##  [49] "conch_wall"               "copton_point"            
    ##  [51] "coral_cove"               "coral_garden"            
    ##  [53] "cordilheiras"             "cousins_bartolome"       
    ##  [55] "creche"                   "cristo_degli_abissi"     
    ##  [57] "cueva_del_elefante"       "daphne_gordon"           
    ##  [59] "daphne_minor"             "daphne_y_seymour"        
    ##  [61] "deepslope"                "dolphin_house"           
    ##  [63] "doodles"                  "dragone"                 
    ##  [65] "drop_on"                  "drop_zone"               
    ##  [67] "dungong_wall"             "ernies_cave"             
    ##  [69] "ernies_point"             "ernies_steps"            
    ##  [71] "faca_cega"                "faro"                    
    ##  [73] "farol"                    "Farol"                   
    ##  [75] "fish_sanctuary"           "floreana"                
    ##  [77] "fondeadero"               "forno_beach"             
    ##  [79] "funil"                    "gabbianara"              
    ##  [81] "gato"                     "Geladeira"               
    ##  [83] "Geladeira "               "giant_clams"             
    ##  [85] "gordon-plazas"            "gordon_rocks"            
    ##  [87] "grotta_dei_gamberi"       "grotta_dello_zaffiro"    
    ##  [89] "grotta_eremita"           "grottine"                
    ##  [91] "guy_fawkes_y_daphne"      "house_reef"              
    ##  [93] "il_faro"                  "ilha_do_meio"            
    ##  [95] "ilha_do_meio_trad"        "indiano"                 
    ##  [97] "isla_benidorm"            "isla_mitjana"            
    ##  [99] "isuela"                   "jd.corais"               
    ## [101] "key_largo"                "kilima_steps"            
    ## [103] "la_lea"                   "la_llosa"                
    ## [105] "laguna"                   "lajas"                   
    ## [107] "laje_dois_irmaos"         "le_lastre"               
    ## [109] "lighthouse"               "lingua_da_siriba"        
    ## [111] "low_channel_rock"         "manila_channel"          
    ## [113] "marine_sanctuary"         "mascarat"                
    ## [115] "matacoes"                 "mohawk_deer"             
    ## [117] "monad"                    "monkey_beach"            
    ## [119] "montalto"                 "mosquera"                
    ## [121] "mosquera_norte"           "mosquera_y_daphne"       
    ## [123] "mosquera_y_seymour_canal" "mosquera_y_seymour_norte"
    ## [125] "panagsama_house_reef"     "pedras_secas"            
    ## [127] "pescador"                 "pescador_island"         
    ## [129] "pickles"                  "pink_wall"               
    ## [131] "plazas_n"                 "Portinho"                
    ## [133] "portinho_sul_mato_verde"  "porto_pidocchio"         
    ## [135] "punta_campanella"         "punta_cormoran_champion" 
    ## [137] "punta_di_puolo"           "punta_vessinaro"         
    ## [139] "Raias"                    "raviolo"                 
    ## [141] "red_point"                "rihannas_archkevs_ledge" 
    ## [143] "rocas_beagle"             "rosalinda"               
    ## [145] "saavedra_sanctuary"       "sabang_point"            
    ## [147] "sabang_wrecks"            "santa_fe"                
    ## [149] "scoglio_a_penna"          "scole"                   
    ## [151] "secca_gonzatti"           "seymour"                 
    ## [153] "seymour_canal"            "seymour_gordon"          
    ## [155] "seymour_punta"            "seymur-mosquera"         
    ## [157] "seymur_norte"             "sharks_cave"             
    ## [159] "sky_dive"                 "south_marianne"          
    ## [161] "st_pierre"                "steps"                   
    ## [163] "steves_ledge"             "subbielli"               
    ## [165] "talisay_point"            "tamburo"                 
    ## [167] "targhetta"                "tartaruga"               
    ## [169] "tekno_reef"               "testa_del_leone"         
    ## [171] "texas"                    "the_wall"                
    ## [173] "three_sisters"            "tongo"                   
    ## [175] "torretta"                 "vervece"                 
    ## [177] "west_escocea"             "white_bank"              
    ## [179] "wreck_point"             
    ## 
    ## Class:
    ## [1] "character"

``` r
location2 <- location2 %>%
  mutate(
    dive_site = dive_site %>%
      str_trim() %>%                # trim trailing whitespaces
      str_to_lower() %>%            # convert to lowercase
      str_replace_all("[-.]", "_")  # replace "-" and "." with underscore
  )

# check for any remaining space ("\s"), dot, dash or capital letter ("A-Z")
any(str_detect(location2$dive_site, "[\\s.-]|[A-Z]"), na.rm = TRUE)
```

    ## [1] FALSE

``` r
sort(unique(location2$reef_type))

cat("\nClass:\n")
class(location2$reef_type)
```

    ##  [1] "artificial_reef_shipwreck"                   
    ##  [2] "cave"                                        
    ##  [3] "caves_and_canyons"                           
    ##  [4] "caves_and_gravel"                            
    ##  [5] "coral_reef_barrier_reef"                     
    ##  [6] "deep_forereef_slope"                         
    ##  [7] "flat"                                        
    ##  [8] "flat_and _wall"                              
    ##  [9] "flat_and_caves"                              
    ## [10] "flat_and_smal _pinnacles"                    
    ## [11] "flat_sandstone_reef"                         
    ## [12] "fringe"                                      
    ## [13] "gravel_slopy_bottom"                         
    ## [14] "N/A"                                         
    ## [15] "oligocenic_puddingstone_shoal"               
    ## [16] "oligocenic_puddingstone_vertical_wall"       
    ## [17] "oligocenic_puddingstone_vertical_wall/cavern"
    ## [18] "patch"                                       
    ## [19] "pinnacle"                                    
    ## [20] "rocks_and_gravel"                            
    ## [21] "rocky_reef"                                  
    ## [22] "Rocky_reef"                                  
    ## [23] "rocky_reef-vertical_wall"                    
    ## [24] "rocky_reef_sand-gravelly_ledges"             
    ## [25] "rocky_reef_sand-gravelly_vertical_wall"      
    ## [26] "rocky_reef_sand_gravelly"                    
    ## [27] "rocky_reef_vertical_wall"                    
    ## [28] "rocky_reef_with_ledges_and_vertical_wall"    
    ## [29] "sand_and_cymodocea_meadow"                   
    ## [30] "shallow_backreef"                            
    ## [31] "shoal"                                       
    ## [32] "vertical_wall_rocky_reef"                    
    ## [33] "wall"                                        
    ## [34] "wall_and_cave"                               
    ## [35] "wall_and_gravel"                             
    ## [36] "wall_and_posidonia_meadow"                   
    ## 
    ## Class:
    ## [1] "character"

``` r
location2 <- location2 %>%
  mutate(
    reef_type = reef_type %>%
      na_if("N/A") %>%                  # replace "N/A" with NA
      str_to_lower() %>%                # convert to lowercase
      str_replace_all("[-/]", "_") %>%  # replace "-" and "/" with underscore
      str_replace_all(fixed(" "), "")   # remove all spaces
  )

# check for any remaining space "\s", dash or capital letter "A-Z"
any(str_detect(location2$dive_site, "[\\s-/]|[A-Z]"), na.rm = TRUE)
```

    ## [1] FALSE

``` r
class(location2$visibility_m)
```

    ## [1] "character"

``` r
# convert to numeric
location2 <- location2 %>%
  mutate(visibility_m = as.numeric(visibility_m))

# check for unusual values
max(location2$visibility_m, na.rm = TRUE)   # ignore NA values
min(location2$visibility_m, na.rm = TRUE)
```

    ## [1] 30
    ## [1] 1

``` r
location <- location2
glimpse(location)
```

    ## Rows: 2,312
    ## Columns: 13
    ## $ source       <chr> "jimenez_gutierrez_et_al_2009", "jimenez_gutierrez_et_al_…
    ## $ diver_id     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ activity     <fct> scuba, scuba, scuba, scuba, scuba, scuba, scuba, scuba, s…
    ## $ month        <fct> april, april, april, april, july, july, july, july, july,…
    ## $ year         <dbl> 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 200…
    ## $ country      <chr> "Spain", "Spain", "Spain", "Spain", "Spain", "Spain", "Sp…
    ## $ site         <chr> "benidorm", "benidorm", "benidorm", "benidorm", "benidorm…
    ## $ longitude    <dbl> 38.53537, 38.53537, 38.53537, 38.53537, 38.53537, 38.5353…
    ## $ latitude     <dbl> -0.126224, -0.126224, -0.126224, -0.126224, -0.126224, -0…
    ## $ mpa          <fct> yes, yes, yes, yes, yes, yes, yes, yes, yes, yes, yes, ye…
    ## $ dive_site    <chr> "la_llosa", "la_llosa", "la_llosa", "la_llosa", "la_llosa…
    ## $ reef_type    <chr> "rocky_reef", "rocky_reef", "rocky_reef", "rocky_reef", "…
    ## $ visibility_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

[**Go to Project Summary**](../readme.md)
