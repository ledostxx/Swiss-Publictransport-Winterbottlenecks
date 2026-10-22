#Binary and binominal

#General codes
library(dplyr)
library(ggplot2)
library(mgcv)
library(caret)


zb_final <- read.csv("data/zentrahlbahn_final.csv", header = TRUE, stringsAsFactors = TRUE)

str (zb_final)

#Subset
zb_final_binominal <- zb_final %>%
  mutate(
    Train_Delayed = case_when(
      Delay_Category == "On Time" ~ FALSE,    
      Delay_Category == "Unknown" ~ NA,
      Delay_Category %in% c("Minor Delay", "Moderate Delay", "Significant Delay") ~ TRUE,  
      TRUE ~ NA  # Handle any other unrecognized categories as NA
    ),
    
   
    Train_RUSH_HOUR = case_when(
      RUSH_HOUR == "rush_hour_none" ~ FALSE,
      RUSH_HOUR %in% c("rush_hour_vormittag", "rush_hour_abend") ~ TRUE,
      is.na(RUSH_HOUR) ~ NA,  # Handle NA values properly
      TRUE ~ NA  # Any other unrecognized or unexpected RUSH_HOUR values are NA
    )
  ) %>%
  select(
    1:(match("Delay_Category", names(.))),  # Columns up to Delay_Category
    Train_Delayed,  # Add Train_Delayed after Delay_Category
    (match("Delay_Category", names(.)) + 1):(match("RUSH_HOUR", names(.))),  # Columns between Delay_Category and RUSH_HOUR
    Train_RUSH_HOUR,  # Add Train_RUSH_HOUR after RUSH_HOUR
    (match("RUSH_HOUR", names(.)) + 1):ncol(.)  # Columns after RUSH_HOUR
  )

#Reducing the subbset just with the relevant columns
zb_final_binominal <- zb_final_binominal %>% select(BETRIEBSTAG, LINIEN_TEXT, FAELLT_AUS_TF, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE, AN_PROGNOSE_STATUS, ABFAHRTSZEIT, AB_PROGNOSE, AB_PROGNOSE_STATUS, ABFAHRTDELAY_min, ANKUNFTDELAY_min, Delay_Category, Train_Delayed, TAGESZEIT, RUSH_HOUR, Train_RUSH_HOUR, STUNDE_ANKUNFT)


#Removing rows NA in ANKUNFTDELAY_min

zb_final_binominal <- zb_final_binominal %>%
  filter(!is.na(ANKUNFTDELAY_min))

sum(is.na(zb_final_binominal$ANKUNFTDELAY_min)) #Checking if ANKUNFTDELAY_min NA is 0

zb_final_binominal <- zb_final_binominal %>%
  filter(!is.na(ABFAHRTDELAY_min))

sum(is.na(zb_final_binominal$ABFAHRTDELAY_min)) #Checking if ABFAHRTDELAY_min NA is 0

# Convert the Train_Delayed to numeric (0 = FALSE, 1 = TRUE) otherwise we can not use it with a logistic regression model
zb_final_binominal$Train_Delayed <- as.numeric(zb_final_binominal$Train_Delayed)


str(zb_final_binominal)

View(zb_final_binominal)

#Models Binary delay


#Plotting the data

zb_final_binominal %>%
  group_by(LINIEN_TEXT) %>%
  summarise(ProportionDelayed = mean(Train_Delayed, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LINIEN_TEXT, ProportionDelayed), y = ProportionDelayed)) +
  geom_point(size = 3, color = "steelblue") +
  labs(x = "Train Line", y = "Proportion Delayed") +
  theme_minimal()

"In this plot, we can see that the EXT line has the highest proportion of delayed trains, with approximately 40% 
of its services experiencing delays. The line with the second highest delay rate is the R71, where around 24% of trains are delayed. 
On the other end of the spectrum, the S41 line shows the lowest delay rate, with only about 10% of its trains being delayed.
"

"To predict whether a train is delayed, we use a binomial logistic regression with Train_Delayed as the binary response variable 
and LINIEN_TEXT (train line) as the predictor. This model estimates how the probability of delay varies across different train lines.
"

glm_delay <- glm(Train_Delayed ~ LINIEN_TEXT, family = "binomial", data = zb_final_binominal)

summary(glm_delay)

"The number of Fisher Scoring iterations is 7, which is an acceptable value.
The dispersion parameter in the model was calculated by dividing the residual deviance (21375) by the residual degrees of 
freedom (57209), yielding a value of 0.374. This value, being less than 1, indicates that there is no overdispersion in the 
data. Therefore, the binomial model is an appropriate fit for the dataset, and no adjustments for overdispersion are necessary.
"

"On one hand, the p-values for almost all the train lines are very small, indicating that the train lines have a statistically significant 
effect on whether a train is delayed or not. Since the coefficients for the train lines are negative, it suggests that the different train lines have a 
lower likeliness of being delayed compared to the baseline train line.
On the other hand, the train line R71, which operates between Meiringen and Innertkirchen, has a p-value above 0.05. This suggests that R71 does not have 
a statistically significant impact on whether a train is delayed or not. This is interesting because, when the predictors were plotted, R71 was the line with 
the second-highest probability of delay, with an average delay of around 24%.
The reason for this contradiction could be that while the R71 line might often experience delays, the variance of the delays might be quite narrow. To explore this, 
let's take a look at the boxplot of the train delays by line.
"

boxplot(ANKUNFTDELAY_min ~ LINIEN_TEXT , data = zb_final_binominal)

"In the boxplot, we can see that the R71 has a relatively large variance between the 25th and 75th percentiles of the data. 
Nevertheless, R71 has fewer outliers compared to the other lines. The delays on R71 might be consistent, but not extreme 
enough to be detected as a significant predictor in the logistic regression model.
Just because R71 has a relatively high average of being delay doesn't necessarily mean that the line itself significantly affects the likelihood of delays 
when considering all other factors. The logistic regression model is trying to predict the probability of a delay (yes/no) based on various predictors. 
Other variables—such as weather, time of day, or operational factors—might influence whether a delay occurs for R71 trains.
"

"Lets have now a look at the coefficients of glm_delay"

exp(coef(glm_delay)) %>% round(digits=2)

"To interpret the effect of different train lines on the likelihood of delays, the logistic regression coefficients were exponentiated to 
obtain odds ratios. These odds ratios provide a clearer understanding of how the odds of a delay on each line compare to the baseline category, which in this model is the train line EXT. 
All other train lines show odds ratios significantly below 1, indicating a lower likelihood of delay relative to EXT. For instance, trains on line IR have odds of being delayed that are only 9% of those on line EXT, 
while line S41 shows an even stronger reduction, with odds at just 1%. Other notable examples include lines PE and R70, each with odds around 7–9% of the baseline, and line S5, with only 5% of the odds of a delay compared to EXT. 
These findings suggest, as already seen in the previous plots, that line EXT has a particularly high likelihood of delays, while other lines operate with considerably greater punctuality.
"


"The next step in our analysis involves simulating predictions based on the logistic regression model to better understand the likelihood of train delays. 
This will help us evaluate the model's performance and gain insights into the probability of delays across different train lines.
"

# Set seed for reproducibility
set.seed(123)

# Simulate new data based on existing data's structure (e.g., random values for LINIEN_TEXT)
simulated_data <- data.frame(
  LINIEN_TEXT = sample(levels(zb_final_binominal$LINIEN_TEXT), 10000, replace = TRUE) #increased the n of trials in order to have higher sampling and higher probability range
)

# Predict the probability of delay for these simulated data points
simulated_data$predicted_prob <- predict(glm_delay, newdata = simulated_data, type = "response")

# Show the first few rows of the simulated data
head(simulated_data)


# Plotting the simulated probabilities of train delays by train line

ggplot(simulated_data, aes(x = reorder(LINIEN_TEXT, predicted_prob), y = predicted_prob, color = LINIEN_TEXT)) +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0) +
  labs(x = "Train Line", y = "Simulated Probability of Delay") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    panel.border = element_blank(), # Remove the box around the plot
    legend.position = "none" # Remove the legend
  )


"In this plot, we can see that the simulated data is similar to the original data. The EXT line shows a simulated probability of delay of approximately 40%, 
while the R71 line has a simulated probability delay of around 24%. 
On the other end, the S41 line exhibits the lowest simulated probability of delay, with only about 1% of its services experiencing delays."

"Lets now compare our simulated data with our real dataset"

summary(simulated_data)

"The mean of the simulated data is 0.105. This is the reason why we cannot take a threshold of 0.5. This would result in nearly all cases being classified as non-delayed, since very few predicted probabilities exceed 0.5—ultimately leading to extremely poor sensitivity and almost no true positives.
Several thresholds were tested to determine the optimal cut-off point for classifying delays based on the predicted probabilities. Thresholds such as 0.03, 0.08, and 0.1 were evaluated, but they either led to a very low specificity or poor sensitivity. A threshold of 0.2 appeared to offer the most reasonable trade-off between correctly identifying delayed trains (sensitivity) and avoiding false delay predictions (specificity), making it the most balanced choice for this model.
"
#  Mean of the Binary Simulated Delay Outcome
mean(as.numeric(as.character(simulated_data$simulated_delay)))

# Discretize the simulated data
simulated_data$simulated_delay <- ifelse(simulated_data$predicted_prob > 0.2, 1, 0) # a threshold of 0.2 was chosen based on the mean the binary simulated delay outcome.

# Sample from the simulated data with replacement to match the number of rows in the real dataset
set.seed(123)
simulated_sample <- simulated_data[sample(1:nrow(simulated_data), nrow(zb_final_binominal), replace = TRUE), ]

# Create the confusion matrix
# Making sure the factor levels are the same for both the simulated data and the real data
simulated_sample$simulated_delay <- factor(simulated_sample$simulated_delay, levels = c(0, 1))
zb_final_binominal$Train_Delayed <- factor(zb_final_binominal$Train_Delayed, levels = c(0, 1))


# Comparing the real data delays with the simulated delays
conf_matrix <- confusionMatrix(as.factor(simulated_sample$simulated_delay), as.factor(zb_final_binominal$Train_Delayed))

# Printing the confusion matrix
print(conf_matrix)

"The model's accuracy is 76.96%, which at first glance suggests decent performance, but this metric is heavily influenced by
the significant imbalance between delayed and non-delayed trains in the dataset. Due to the high punctuality of Swiss trains, 
the vast majority of observations are non-delays, making accuracy a somewhat misleading indicator of model effectiveness. 
Sensitivity, which captures how well the model identifies actual delays, is fairly high at 79.8%, indicating that the model 
detects most delay cases. However, specificity is low at 20.0%, meaning it struggles to correctly identify non-delayed trains, 
frequently labeling them as delayed. The model's precision is high at 95.2%, so when it predicts a delay, it is usually right. 
On the other hand, the negative predictive value is low at 4.7%, reflecting poor performance in correctly predicting trains that are on time. 
Balanced accuracy, which considers both sensitivity and specificity, is 49.9% suggesting the model performs no better than random guessing 
when it comes to balancing delay and non-delay predictions.
Despite strong precision for delays, the inability to reliably detect non-delays remains a key weakness. Several threshold values were tested to improve this balance, 
but none led to a meaningful improvement in the confusion matrix. A threshold of 0.2 offered the best compromise between sensitivity and specificity among the tested options. 
For future work, further adjustments addressing the class imbalance and a more refined threshold selection process may be necessary to enhance model reliability across both outcome classes."




