#load dataset
setwd("/Users/qiaoyinie/Desktop/PS532Quant III/PS532-Final-Project")
getwd()

data <- read.csv("Asian Americans’ Experiences of Discrimination and Political Participation Survey_August 15, 2023_23.54.csv")
View(data)

###data cleaning
fdata <- data[-c(1,2,3,4,12,27,28,50,59,178,205,250,267,268,269,270,271,276,278,279,281,286,288,289,293,317,321,323,352,353,354,355), -c(1:18)]
View(fdata)

install.packages('tidyverse')
library(tidyverse)

#mutate new variables
fdata$X1<- as.numeric(fdata$X1)
fdata$X2<- as.numeric(fdata$X2)
fdata$X3<- as.numeric(fdata$X3)
fdata$X4<- as.numeric(fdata$X4)
fdata$X5<- as.numeric(fdata$X5)

fdata1 <- fdata %>% 
  mutate(MPP = rowMeans(select(fdata, c(25:29)), na.rm = TRUE))

fdata1 <- fdata1 %>% 
  mutate_at(vars(c(30:129)), as.numeric)

fdata1 <- fdata1 %>% 
  mutate(APP = rowMeans(select(fdata1, c(30:129)), na.rm = TRUE))

###subset treatment
fdata1 <- fdata1 %>% 
  mutate_at(vars(c(15:24)), as.numeric)

fdata1 <- fdata1 %>% 
  mutate(Treatment = case_when(
    PD1 >= 1 ~ "1",
    SD1 >= 1 ~ "2",
    P1 >= 1 ~ "3",
    PD1.1 >= 1 ~ "4",
    TRUE ~ "F"  # Default case if none of the conditions match
  ))

fulldata <- fdata1 %>% 
  select(c(1:24,130,132,133,134))


###preps for analysis

#assign meaning
install.packages("Hmisc")
library(Hmisc)
label(fulldata$Race.and.Ethnicity)   <- "Ethnicity"
label(fulldata$Q19)   <- "Frequency of Dis"

###descriptive analysis
#ethnicity
unique(fulldata$Asian)

fulldata <- fulldata[order(fulldata$Asian, decreasing = TRUE), ]

fulldata <- fulldata %>% 
  mutate(Ethcinity = case_when(
    Asian== 1 ~ "Asian Indian",
    Asian== 2 ~ "Bangladeshi",
    Asian== 3 ~ "Bhutanese",
    Asian== 4 ~ "Burmese",
    Asian== 5 ~ "Cambodian",
    Asian== 6 ~ "Chinese",
    Asian== 7 ~ "Philippino",
    Asian== 8 ~ "Hmong",
    Asian== 9 ~ "Indonesian",
    Asian== 10 ~ "Japanese",
    Asian== 11 ~ "Korean",
    Asian== 12 ~ "Laotian",
    Asian== 13 ~ "Mongolian",
    Asian== 14 ~ "Malaysian",
    Asian== 15 ~ "Nepalese",
    Asian== 16 ~ "Okinawan",
    Asian== 17 ~ "Pakistani",
    Asian== 18 ~ "Sri Lankan",
    Asian== 19 ~ "Thai",
    Asian== 20 ~ "Vietnamese",
    TRUE ~ "F"  # Default case if none of the conditions match
  ))


library(dplyr)
fulldata %>%
  group_by(Ethcinity) %>%
  summarise(Count = n()) %>%
  arrange(-Count) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(Ethcinity, -Count), y = Count)) +
  geom_bar(stat = "identity", color="steelblue", fill="steelblue", alpha = 0.5) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=75,hjust=1)) +
  labs(title = "Primary Ethnicity or Family Ancestry", x = "Country of Origin", y = "Number of Participants")


##dis experiences
  fulldata <- fulldata %>% 
  mutate(Dis = case_when(
    Discriminatory.Exper == 2 ~ "No", 
    Discriminatory.Exper == 1 ~ "Yes",
    TRUE ~ "F"  # Default case if none of the conditions match
  ))
  
  category_counts <- table(fulldata$Dis)
 
  # Calculate percentages
  category_percentages <- round(100 * category_counts / sum(category_counts), 1)
  
  # Create a pie chart with percentages as labels
  pie(category_counts, labels = paste(names(category_counts), "\n", category_percentages, "%"),
      main = "Experienced discrimination in the past three years?")

  fulldata <- fulldata %>% 
    mutate(Frequency = case_when(
      Q19== 1 ~ "About once a day",
      Q19== 2 ~ "About once a week",
      Q19== 3 ~ "About once a month",
      Q19== 4 ~ "About once a year",
      Q19== 5 ~ "Exactly once",
      Q19== 6 ~ "Never",
      TRUE ~ "F"  # Default case if none of the conditions match
    ))

  # Create a bar plot with missing values omitted
  install.packages("ggplot2")                           # Install ggplot2 package
  library("ggplot2") 
  
  frequency_table <- table(fulldata$Frequency)
  
  # Display the frequency table
  print(frequency_table)
  
  discrimination_data <- data.frame(
    Category = c("About once a day", "About once a month", "About once a week", "About once a year", "Exactly once"),
    Frequency = c(5, 65, 30, 51,13)
  )
  
  # Create the barplot
  barplot <- ggplot(discrimination_data, aes(x = Category, y = Frequency)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Frequency of Discrimination", x = "Category", y = "Frequency") +
    theme_minimal()
  
  # Display the barplot
  print(barplot)
  
  
  ##subset
  
  library(dplyr)
  fulldata <- fulldata %>% 
    mutate(Treat = case_when(
      Treatment== 3 ~ "1",
      Treatment== 1 ~ "2",
      Treatment== 2 ~ "3",
      Treatment== 4 ~ "4",
    ))
  
  PD <- fulldata[fulldata$Treat ==2, ]
  SD <- fulldata[fulldata$Treat ==3, ]
  Placebo <- fulldata[fulldata$Treat ==1, ]
  PS <- fulldata[fulldata$Treat ==4, ]
 
  ## combine the treated and control/placebo group
 Tr1 <- rbind(PD, Placebo)
 Tr2 <- rbind(SD, Placebo)
  
  ### Propensity Score Matching
  install.packages("Matching")
  library(Matching)
  
  library(ggplot2)
  library(Matching)
  
  # dependent variable
  y <- lalonde$re78
  ##re: real earning
  
  # treatment
  tr <- lalonde$treat
  
  # create a propensity score model
  glm1 <- glm(treat~age+educ+black+hisp+married+nodegr+re74+re75,
              family=binomial, data=lalonde)
  
  joinpscore = cbind(lalonde, glm1$fitted)
  
  colnames(joinpscore)[13] = "Propensity_Score"
  
  #interaction plot
  Fulldata <- fulldata %>% 
    filter(Treat %in% c(2, 3, 4)) %>%
    mutate(Treatment = case_when(
      Treat== 2 ~ "Political Dis.",
      Treat== 3 ~ "Societal Dis.",
      Treat== 4 ~ "Political&Societal Dis.",
      TRUE ~ "F"  # Default case if none of the conditions match
    ))
    
  library (ggplot2)
  library(dplyr)
  s <- lm_robust(MPP ~ Treatment*Dis, data=Fulldata)
  summary(s)
  ??lm_robust
  
  plot_model(s, type = "pred", terms = c("Treatment", "Dis"))+
    theme(plot.background = element_rect(fill = "white"))+
    theme_minimal()
  
 
  m <- lm_robust(APP ~ Treatment*Dis, data=Fulldata)
  
  summary(m)
  
  plot_model(m, type = "pred", terms = c("Treatment", "Dis"))+
    theme_minimal()
  
  H1 <- rbind(PD,Placebo)
  H2 <- rbind(SD,Placebo)
  H3 <- rbind(PS,Placebo)
  
  install.packages("descr")
  library(descr)
  
  ??crosstab
  descr::crosstab(as.numeric(fulldata$Race.and.Ethnicity), as.numeric(fulldata$Treat), prop.r = T)
  
  
#regression analysis

  a <- summary(lm(APP ~ as.factor(Treat), data=fulldata))
  coef(a)
  
  summary(lm(MPP ~ as.factor(Treat), data=fulldata))
  
  fulldata <- fulldata %>% 
    mutate(Exper = case_when(
      Discriminatory.Exper== 2 ~ "0",
      Discriminatory.Exper== 1 ~ "1"))      
  summary(lm(MPP ~ as.factor(Treat), data=fulldata))
  
  
  # Rerun the regressions 
  model_2 <- lm_robust(APP ~ I(Treat == 2), alpha = 0.1, data = subset(fulldata, Treat %in% c(1, 2)))
  model_3 <- lm_robust(APP ~ I(Treat == 3), alpha = 0.1, data = subset(fulldata, Treat %in% c(1, 3)))
  model_4 <- lm_robust(APP ~ I(Treat == 4), alpha = 0.1, data = subset(fulldata, Treat %in% c(1, 4)))
  
  # Extract tidy results with 90% confidence interval
  tidy_2 <- tidy(model_2, conf.int = 0.9)
  tidy_3 <- tidy(model_3, conf.int = 0.9)
  tidy_4 <- tidy(model_4, conf.int = 0.9)
  
  # Verify the extracted values
  print(tidy_2)
  print(tidy_3)
  print(tidy_4)
  
  # The rest remains unchanged.
  
  results <- rbind(
    
    data.frame(Treatment = "Political Discrimination",  estimate = tidy_2$estimate[2], conf.low = tidy_2$conf.low[2], conf.high = tidy_2$conf.high[2]),
    data.frame(Treatment = "Societal Discrimination",  estimate = tidy_3$estimate[2], conf.low = tidy_3$conf.low[2], conf.high = tidy_3$conf.high[2]),
    data.frame(Treatment = "Political and Societal Discrimination",  estimate = tidy_4$estimate[2], conf.low = tidy_4$conf.low[2], conf.high = tidy_4$conf.high[2])
  )
  
  
ggplot(results, aes(y = Treatment, x = estimate, xmin = conf.low, xmax = conf.high)) +
    geom_point(aes(color = Treatment), size = 4) + 
    geom_errorbarh(aes(height = 0.25), color = "black", size = 0.5) +
    labs(title = "Treatment Effects on APP using lm_robust",
         x = "Coefficient (Treatment Effect)",
         y = "Treatment Group",
         color = "Treatment") +
    theme_bw() +
    theme(legend.position = "none", 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_color_brewer(palette = "Set1") + 
    scale_x_continuous(breaks = seq(-0.1, 0.5, by = 0.1), limits = c(-0.1, 0.6))
  

# Rerun the regressions 
model_21 <- lm_robust(MPP ~ I(Treat == 2), alpha = 0.1, data = subset(fulldata, Treat %in% c(1, 2)))
model_31 <- lm_robust(MPP ~ I(Treat == 3), alpha = 0.1, data = subset(fulldata, Treat %in% c(1, 3)))
model_41 <- lm_robust(MPP ~ I(Treat == 4), alpha = 0.1, data = subset(fulldata, Treat %in% c(1, 4)))

# Extract tidy results with 90% confidence interval
tidy_21 <- tidy(model_21, conf.int = 0.9)
tidy_31 <- tidy(model_31, conf.int = 0.9)
tidy_41 <- tidy(model_41, conf.int = 0.9)

# Verify the extracted values
print(tidy_21)
print(tidy_31)
print(tidy_41)

# The rest remains unchanged.

results1 <- rbind(
  
  data.frame(Treatment = "Political Discrimination",  estimate = tidy_21$estimate[2], conf.low = tidy_21$conf.low[2], conf.high = tidy_21$conf.high[2]),
  data.frame(Treatment = "Societal Discrimination",  estimate = tidy_31$estimate[2], conf.low = tidy_31$conf.low[2], conf.high = tidy_31$conf.high[2]),
  data.frame(Treatment = "Political and Societal Discrimination",  estimate = tidy_41$estimate[2], conf.low = tidy_41$conf.low[2], conf.high = tidy_41$conf.high[2])
)


ggplot(results1, aes(y = Treatment, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point(aes(color = Treatment), size = 4) + 
  geom_errorbarh(aes(height = 0.25), color = "black", size = 0.5) +
  labs(title = "Treatment Effects on MPP using lm_robust",
       x = "Coefficient (Treatment Effect)",
       y = "Treatment Group",
       color = "Treatment") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank()) +
  scale_color_brewer(palette = "Set1") + 
  scale_x_continuous(breaks = seq(-0.1, 0.5, by = 0.1), limits = c(-0.1, 0.6))


fit <- lm(MPP ~ I(Treat == 2) * Exp, data = fulldata)


# select only levels 30, 50 and 70 from continuous variable Barthel-Index
plot_model(fit, type = "pred", terms = c("c12hour", "barthtot [30,50,70]", "c161sex"))


  
  library(ggplot2)
  
  ggplot(fulldata, aes(x = Treat, y = MPP)) + 
    geom_boxplot(fill = "steelblue")+
    labs(x = "Treatment", y = "Mainstream Political Participation") +
    theme_bw()

    
  ggplot(fulldata, aes(x = Treat, y = APP)) + 
    geom_boxplot(fill = "steelblue")+
    labs(x = "Treatment", y = "Ethnic-specific Political Participation") +
    theme_bw()
  
  
  #analysis
  install.packages("estimatr")
  library(estimatr)
  sim1 <- lm_robust(MPP ~ Treat, data=H1)
  summary(sim1)
  
  sim2 <- lm_robust(APP ~ Treat, data=H1)
  summary(sim2)
  
  sim3 <- lm_robust(MPP ~ Treat, data=H2)
  summary(sim3)
  
  sim4 <- lm_robust(APP ~ Treat, data=H2)
  summary(sim4)
  
  sim5 <- lm_robust(MPP ~ Treat, data=H3)
  summary(sim5)
  
  sim6 <- lm_robust(APP ~ Treat, data=H3)
  summary(sim6)
  

  #create a join measurement of PD+SD
 # H4 <- H4 %>% 
#    mutate(Treat1 = case_when(
 #     Treat== 2 ~ "5",
  #    Treat== 1 ~ "1",
   #   Treat== 3 ~ "5",
    ))
  #sim6 <- lm_robust(MPP ~ Treat1, data=H4)
  #summary(sim6)
  
  #create interaction tables
  install.packages("huxtable")
  library(huxtable)
  huxreg( sim1,sim2,
         coefs = c("Political Discrimination" = "Treat2"), 
         bold_signif = 0.05,
         stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))
  
  install.packages("huxtable")
  library(huxtable)
  huxreg( sim3,sim4,
          coefs = c("Societal Discrimination" = "Treat3"), 
          bold_signif = 0.05,
          stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

  install.packages("huxtable")
  library(huxtable)
  huxreg(sim5, sim6,
          coefs = c("Political Discrimination*Societal Discrimination" = "Treat4"), 
          bold_signif = 0.05,
          stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))
  
  
  # Install packages if not already installed
  install.packages("sjPlot")
  install.packages("ggplot2")  # If not already installed
  
  # Load the packages
  library(sjPlot)
  library(ggplot2)
  library(effects)
  
  #Marginal effects plot:
  plot_model(sim1, type = "eff")
  
  #Interaction plot:
  plot_model(sim1, type = "int")
  
  #Plot residuals:
  plot_model(sim1, type = "pred")
  
  
  library(likert)
  view_df(H1, show.frq = T, show.prc = T, show.na = T)
  
  H1 %>%
    group_by(Treat)%>%
    plot_frq(MPP)%>%
    plot_grid()
  
  #analysis
  install.packages("estimatr")
  library(estimatr)
  sim11 <- lm_robust(MPP ~ Treat*Dis, data=H1)
  summary(sim11)
  
  #plot_model(sim11, type = "pred", terms = c("Treat", "Dis"))
  
  
  sim22 <- lm_robust(APP ~ Treat*Dis, data=H1)
  summary(sim22)
  
  sim33 <- lm_robust(MPP ~ Treat*Dis, data=H2)
  summary(sim33)
  
  sim44 <- lm_robust(APP ~ Treat*Dis, data=H2)
  summary(sim44)
  
  sim55 <- lm_robust(MPP ~ Treat*Dis, data=H3)
  summary(sim55)
  
  sim66 <- lm_robust(APP ~ Treat*Dis, data=H3)
  summary(sim66)
  
  huxreg(sim11, sim22, 
         coefs = c("Political Discrimination*Societal Discrimination" = "Treat4"), 
         bold_signif = 0.05,
         stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))
  
  huxreg(sim33, sim44,
         coefs = c("Political Discrimination*Societal Discrimination" = "Treat4"), 
         bold_signif = 0.05,
         stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))
  
  huxreg( sim11, sim22, sim33, sim44, sim55, sim66,
         coefs = c("Political Discrimination" = "Treat2", "Societal Discrimination" = "Treat3", "Political Discrimination*Societal Discrimination" = "Treat4"), 
         bold_signif = 0.05,
         stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))
 
  #Matching:
  install.packages("Matching")
  library(Matching)
  data(lalonde)
  data(GerberGreenImai)
  
  library(ggplot2)
  library(Matching)
  
  #balance test: 

  install.packages("cobalt")
  library(cobalt)
  install.packages("caret")
  library(dplyr)
  ?bal.tab
  bal.tab(fulldata, treat = Treat)
  