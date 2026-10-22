# Generalised Additive Models GAM

#General codes
library(dplyr)
library(ggplot2)
library(mgcv)
library(tidyr)

"In this project, we apply Generalized Additive Models (GAMs) to analyze the effects of various predictors on train delays, specifically focusing on the response variables ANKUNFTDELAY_min (arrival delay in minutes) and ABFAHRTDELAY_min (departure delay in minutes). 
Due to the limited availability of continuous variables in our dataset these two delay metrics will be the response variables we will focus on.
A key subset of our predictors is related to weather conditions. However, because the ZB Bahn railway line spans multiple climatic regions, 
comparing weather-related predictors across all stations introduces variability that may obscure meaningful patterns. To address this, we restrict our analysis to a subset of stations located within the Lucerne region, where the climate is more uniform. 
This allows for a more reliable interpretation of the relationship between weather conditions and train delays.
"

#Creating subset

zb_final <- read.csv("data/zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

## Subset the data

# Define the specific Haltestellen in the region of Lucerne
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")

# Subset the data

zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)


#Removing rows NA in ANKUNFTDELAY_min and ABFAHRTDELAY_min

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_subset$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_subset <- zb_final_subset %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_subset$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0

#Reducing the subbset just with the relevant columns

zb_final_subset <- zb_final_subset %>% select(BETRIEBSTAG, LINIEN_TEXT, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, ABFAHRTSZEIT, AB_PROGNOSE, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, TAGESZEIT, RUSH_HOUR, w_precip_mm_Luzern, w_temp_avg_c_Luzern)

str(zb_final_subset)

View(zb_final_subset)


"R assumes for GAM models usually the gaussian family for the response variables. Therefore, lets have a look if the response variables ANKUNFTDELAY_min and ABFAHRTDELAY_min are gaussian distributed. 
"

par(mfrow = c(1, 2))


hist(zb_final_subset$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightblue", breaks = 100, xlim = c(-2,8))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightblue", breaks = 100, xlim = c(-2,8))

"We see that both response variables are not normally distrubuted. They seem to be right skewed. Lets check with the QQ plots. 
"

qqnorm(zb_final$ANKUNFTDELAY_min, main = "QQ Plot of ANKUNFTDELAY_min")
qqline(zb_final$ANKUNFTDELAY_min, col = "lightblue")

qqnorm(zb_final_subset$ABFAHRTDELAY_min, main = "QQ Plot of ABFAHRTDELAY_min")
qqline(zb_final_subset$ABFAHRTDELAY_min, col = "lightblue")

"The QQ plots reveal that the response variables are right-skewed, which aligns with expectations. 
In real-world train operations, early arrivals or departures are relatively uncommon compared to delays. 
Moreover, the distribution indicates that longer delays occur less frequently than shorter ones, reflecting 
typical delay patterns observed in Swiss public transportation. For this reason, we restrict our dataset for the 
GAM analysis to observations with delays between -2 and +4 minutes, aiming to ensure a distribution that more closely 
approximates the Gaussian assumption required by the model.
"

zb_final_subset <- zb_final_subset %>%
  filter(ABFAHRTDELAY_min >= -2 & ABFAHRTDELAY_min <= 4,
         ANKUNFTDELAY_min >= -2 & ANKUNFTDELAY_min <= 4)


hist(zb_final_subset$ANKUNFTDELAY_min, main = "Histogram of ANKUNFTDELAY_min", xlab = "ANKUNFTDELAY_min", col = "lightgreen", breaks = 50, xlim = c(-2,4))


hist(zb_final_subset$ABFAHRTDELAY_min, main = "Histogram of ABFAHRTDELAY_min", xlab = "ABFAHRTDELAY_min", col = "lightgreen", breaks = 50, xlim = c(-2,4))

"The histogram indicates an approximately normal distribution. Therefore, the dataset is now suitable for fitting with Generalized Additive Models (GAMs).
"


gam_Ankunft_temp <- gam(ANKUNFTDELAY_min ~ s(w_temp_avg_c_Luzern), data = zb_final_subset) 

summary (gam_Ankunft_temp)

"The p-value associated with the smooth term is close to zero, providing strong evidence that the average temperature in Lucerne has a statistically significant effect on arrival delays. The estimated effective degrees of freedom (edf ≈ 9) suggest a moderately complex, non-linear relationship. However, the adjusted R-squared value of 0.036 and the explained deviance of only 3.67% indicate that the model captures only a small portion of the variation in arrival delays. Thus, while the effect is significant, temperature alone does not explain much of the delay variability.
"

gam_temp_precip_tageszeit_linien_text <- gam(ABFAHRTDELAY_min ~ TAGESZEIT + LINIEN_TEXT + s(w_temp_avg_c_Luzern) + s(w_precip_mm_Luzern), data = zb_final_subset) 

summary(gam_temp_precip_tageszeit_linien_text)

plot(gam_temp_precip_tageszeit_linien_text, residuals = TRUE, select = 1,main = "Effect of Temperature on Departure Delay")
plot(gam_temp_precip_tageszeit_linien_text, residuals = TRUE, select = 2, main = "Effect of Precipitation on Departure Delay")

"
Interpretation of Parametric Coefficients
The reference levels for the categorical predictors are evening for TAGESZEIT and EXT for LINIEN_TEXT. Under these baseline conditions (evening, 0°C, and 0 mm precipitation), the EXT line of Zentralbahn (ZB) in the Lucerne region has an average departure delay of approximately 1.25 minutes. 

The effect of time of day is substantial; compared to the evening, trains during luncht time experience on average 0.30 minutes less delay, those in the afternoon 0.16 minutes less, during night operations 0.30 minutes less and in the morning 0.12 minutes less delay. 

Regarding train lines, the IR line shows a reduction of 0.22 minutes compared to EXT, although this difference is not statistically significant (p = 0.5). In contrast, the PE line exhibits the largest reduction, with trains experiencing on average 2.32 minutes less delay than EXT, a difference that is statistically significant. Likewise, the S4, S41, S44, and S55 lines show significantly reduced delays compared to EXT, with reductions of 0.84, 1.35, 1.04, and 1.96 minutes, respectively, while the S5 line’s difference (0.51 minutes less) is not statistically significant.

Interpretation of the Smooth Terms
The smooth term for average temperature has an estimated effective degrees of freedom (edf) of approximately 9, indicating a moderately complex non-linear relationship with departure delay. We have strong evidence that it is not 0 with a p-value < 0.05. The corresponding plot reveals that temperatures between 0 and 6°C have a stronger effect on departure delay, whereas between roughly 6–7°C and 8–10°C the effect is minimal. 

Similarly, the smooth term for average precipitation (w_precip_mm_Luzern) has an edf of about 8.32, suggesting a moderately complex non-linear relationship. We have strong evidence that it is not 0 with a p-value < 0.05. 

The precipitation plot shows a clear non-linear relationship between precipitation and departure delays. There is a strong effect at lower precipitation levels (up to approximately 20 mm), after which the curve flattens between around 22 mm and 50 mm, indicating a reduced impact on delays. One possible explanation for this flattening is that higher precipitation levels may coincide with times of reduced train activity—such as during the night—when fewer departures occur, thereby minimizing the observed effect on overall delays. Additionally, since the dataset only covers November 2024, it is possible that there were relatively few instances of heavy precipitation during this period in the Lucerne region, which may limit the model's ability to capture stronger effects at higher precipitation levels.

Overall Model Performance
The adjusted R² value of 0.119 means that the model explains around 11.9% of the variation in departure delays. This suggests that the predictors included in the model have a measurable effect on delays, but a large part of the variation (around 88%) is still not accounted for. This can be considered as not unusual in real-world transportation data, where many factors that influence delays—such as technical problems, temporary disruptions, staffing issues or operational decisions—are not included in the dataset. While the model successfully identifies several statistically significant predictors, it also shows that more variables or more complex modeling approaches might be needed to better capture all the factors that contribute to departure delays.
"


