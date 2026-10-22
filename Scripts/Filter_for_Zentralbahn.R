## Libraries
library(dplyr)

## Import dataset ----
df <- read.csv("data/raw/2024-11-30_istdaten.csv", 
               stringsAsFactors = FALSE, 
               sep = ";", 
               header = TRUE, 
               check.names = FALSE)

## Check if column names are now correctly parsed ----

colnames(df) 

## Filter for Trains ONLY ----

colnames(df)
df_filter <- df %>% 
  filter(PRODUKT_ID == "Zug")

## Filter NAs ----

##Remove Rows without Ankunftszeit, Ankunftsprognose, Haltestellennamen & DB/ÖBB

df_filter <- df_filter %>% 
  filter(ANKUNFTSZEIT != "" & !is.na(ANKUNFTSZEIT)) %>%
  filter(AN_PROGNOSE != "" & !is.na(AN_PROGNOSE)) %>% 
  filter(HALTESTELLEN_NAME != "" & !is.na(HALTESTELLEN_NAME)) %>% 
  filter(!BETREIBER_ABK %in% c("DB", "DB Regio", "NeTS-ÖBB")) %>% 
  filter(BETREIBER_ABK == "ZB")

#creating a subset
library(dplyr)

# Define the specific Haltestellen
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")



## Subset the data for weather region LU
zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)

summary(zb_final_subset$HALTESTELLEN_NAME)

View(zb_final_subset)


## Reducing columns to fit the requirement ----

zb_final <- read.csv("data/zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

str(zb_final)

View(zb_final)

# Create a backup of the original file

write.csv(zb_final, "data/zentrahlbahn_final_backup.csv", row.names = FALSE)

zb_final_backup <- read.csv("data/zentrahlbahn_final_backup.csv", header = TRUE, stringsAsFactors = TRUE)

str(zb_final_backup)

View(zb_final_backup)

# Select only the relevant columns
# These are the columns you specified in your request
zb_final_reduced <- zb_final %>%
  select(
    BETRIEBSTAG,
    LINIEN_TEXT,
    HALTESTELLEN_NAME,
    ANKUNFTSZEIT,
    AN_PROGNOSE,         
    ANKUNFTDELAY_sec,    
    ABFAHRTSZEIT,
    AB_PROGNOSE,
    ABFAHRTDELAY_sec,
    START,
    ZIEL,
    Delay_Category,     
    TAGESZEIT,
    RUSH_HOUR,
    w_precip_mm_Luzern,
    w_temp_min_c_Luzern,
    w_temp_avg_c_Luzern,
    FAELLT_AUS_TF
  )

# Overwrite the original file with the reduced version
write.csv(zb_final_reduced, "data/zentrahlbahn_final.csv", row.names = FALSE)








