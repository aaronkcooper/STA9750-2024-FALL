library(tidyverse)
library(janitor)
library(FSA)
library(car)
library(lme4)
library(MuMIn)
library(MASS)
economy <- read_csv("Global Economy Indicators.csv")
economy <- economy |> 
  clean_names()
economy <- economy |> 
  mutate(per_capita_gdp = gross_domestic_product_gdp/population)
medals <- read_csv("olympics/Olympic_Medal_Tally_History.csv")
#Filter for relevant economic data
medals <- filter(medals, year >= 1970)
#Filter for only Olympic years
economy <- filter(economy, year >= 1972 & year %% 2 == 0)
#Weighted medal total, Gold = 3, Silver = 2, Bronze = 1
medals <- medals |> 
  mutate(weighted_total = gold*3 + silver *2 + bronze)
#Can try to merge but some countries may have different names in each table.
merged <- merge(economy, medals, by = c("country", "year"))
merged <- merge(renamed, medals, by = c("country", "year"))
unmatched <- anti_join(medals, economy, by = c("country", "year"))
#2022 economy data not here
countries <- economy |> 
  group_by(country) |> 
  summarise(count = n())
#No Taiwan
#Czechoslovakia (Former) = Czechoslovakia
#Democratic People's Republic of Korea = D.P.R. of Korea
#Germany is combined, in Olympics its east and west until unification
#Ethiopia (Former) = Ethiopia
#Great Britain = United Kingdom (i.e. Ireland is represented separately in Olympics)
#Hong Kong, China = China, Hong Kong SAR
#Iran (Islamic Republic of) = Islamic Republic of Iran
#Saudi Arabia = Kingdom of Saudi Arabia
#Former Netherlands Antilles = Netherlands Antilles
#China = People's Republic of China
#ROC = Russian Olympic Comittee 2018-2022
#Serbia and Montenegro are combined in olympics 1996-2004 following the breakup
#of Yugoslavia but separate in eco data
#Soviet Union = USSR (Former)
#The Bahamas = Bahamas
#Unified Team 1992, Former Soviet Union Countries
#U.R. of Tanzania: Mainland = United Republic of Tanzania 1980
#Venezuela (Bolivarian Republic of) = Venezuela
#Viet Nam = Vietnam
ranking <- merged |> 
  group_by(year, edition) |> 
  mutate(gdp_rank = rank(-gross_domestic_product_gdp),
         medal_rank = rank(-total),
         weight_rank = rank(-weighted_total),
         pop_rank = rank(-population))
merged |> 
  arrange(desc(total))
ranking |> 
  ggplot(aes(gdp_rank, medal_rank)) +
  geom_point() +
  theme_minimal()
m <- lm(medal_rank ~ gdp_rank, ranking)
ranking |> 
  ggplot(aes(gdp_rank, medal_rank)) +
  geom_point() +
  stat_smooth(method = "lm") + 
  theme_minimal()
pop_ranking <- merged |> 
  group_by(year) |> 
  mutate(pop_rank = rank(-population),
         medal_rank = rank(-total))
pop_ranking |> 
  ggplot(aes(pop_rank, medal_rank)) +
  geom_point() +
  stat_smooth(method = "lm") + 
  theme_minimal()
percap_ranking <- merged |> 
  group_by(year) |> 
  mutate(percap_rank = rank(-per_capita_gdp),
         medal_rank = rank(-total))
percap_ranking |> 
  ggplot(aes(percap_rank, medal_rank)) +
  geom_point() +
  stat_smooth(method = "lm") + 
  theme_minimal()
renamed <- economy |> 
  mutate(country = ifelse(country == "Czechoslovakia (Former)", "Czechoslovakia",
                   ifelse(country == "D.P.R. of Korea", "Democratic People's Republic of Korea",
                   ifelse(country == "Ethiopia (Former)", "Ethiopia",
                   ifelse(country == "China, Hong Kong SAR", "Hong Kong, China",
                   ifelse(country == "Iran (Islamic Republic of)", "Islamic Republic of Iran",
                   ifelse(country == "Saudi Arabia", "Kingdom of Saudi Arabia",
                   ifelse(country == "Former Netherlands Antilles", "Netherlands Antilles",
                   ifelse(country == "China", "People's Republic of China",
                   ifelse(country == "USSR (Former)", "Soviet Union",
                   ifelse(country == "USSR (Former)", "Soviet Union",
                   ifelse(country == "Bahamas", "The Bahamas",
                          ifelse(country == "United Kingdom", "Great Britain",
                          ifelse(country == "Viet nam", "Vietnam",country)
                   )))))))
                          )           
                          )        
                          )
                          )))
summary(m)
cor(ranking$gdp_rank, ranking$medal_rank, method="spearman")
rank_summary <- ranking |> 
  group_by(gdp_rank, medal_rank) |> 
  summarize(value = mean(total, na.rm = TRUE), .groups = "drop")
ggplot(ranking, aes(x=gdp_rank, y=medal_rank)) +
  geom_point(color="blue") +
  geom_smooth(method="loess", color="red", se=FALSE) +
  labs(title="GDP Rank vs Medal Rank",
       x="GDP Rank (Wealthier → Lower)",
       y="Medal Rank (More Medals → Lower)") +
  theme_minimal()

model2 <- lm(medal_rank ~ gdp_rank + population, data=ranking)
summary(model2)

ggplot(ranking, aes(x=log(gdp_rank), y=medal_rank)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", formula=y ~ poly(x, 2), color="red") +
  labs(title="Log GDP Rank vs Medal Rank") +
  theme_minimal()

cor.test(ranking$gdp_rank, ranking$medal_rank, method="spearman")

ranking <- ranking |> 
  mutate(gdp_group = case_when(
    gdp_rank <= 10 ~ "Top 10",
    gdp_rank <= 50 ~ "Top 50",
    TRUE ~ "Below 50"
  ))

ggplot(ranking, aes(x=gdp_group, y=medal_rank)) +
  geom_boxplot() +
  labs(title="Medal Rank Distribution by GDP Group") +
  theme_minimal()

kruskal.test(medal_rank ~ gdp_group, data = ranking)

model3 <- lm(medal_rank ~ gdp_rank + per_capita_gdp, data = ranking)
summary(model3)

model4 <- lm(weight_rank ~ gdp_rank, data = ranking)
summary(model4)
cor(ranking$weight_rank, ranking$gdp_rank)

model5 <- lm(medal_rank ~ gdp_rank + pop_rank, data = ranking)
summary(model5)

dunnTest(medal_rank ~ gdp_group, data = ranking, method = "bonferroni")

ranking <- ranking |> 
  mutate(winter = ifelse(grepl("Summer", edition), 0, 1))

model6 <- lm(medal_rank ~ gdp_rank + pop_rank + winter, data = ranking)
summary(model6)

model7 <- lm(formula = medal_rank ~ poly(gdp_rank, 2) + poly(pop_rank, 2) + winter, data = ranking)
summary(model7)

model_random <- lmer(medal_rank ~ gdp_rank + pop_rank + winter + (1|country), data = ranking)
summary(model_random)
r.squaredGLMM(model_random)

model_random_slopes <- lmer(
  medal_rank ~ gdp_rank + pop_rank + winter + 
    (gdp_rank + pop_rank | country),
  data = ranking,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(model_random_slopes)
summary(model_random)
r.squaredGLMM(model_random_slopes)
AIC(model_random, model_random_slopes, model7)

# GDP Rank
ggplot(ranking, aes(x = gdp_rank, y = medal_rank)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), col = "blue") +
  labs(title = "Effect of GDP Rank on Medal Rank") +
  theme_minimal()
# Population Rank
ggplot(ranking, aes(x = pop_rank, y = medal_rank)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), col = "green") +
  labs(title = "Effect of Population Rank on Medal Rank") +
  theme_minimal()
model_interaction <- lm(medal_rank ~ poly(gdp_rank, 2) * poly(pop_rank, 2) + winter, 
                        data = ranking)
AIC(model7, model_interaction)
influencePlot(model7)

model_robust <- rlm(medal_rank ~ poly(gdp_rank, 2) + poly(pop_rank, 2) + winter, 
                    data = ranking)
summary(model_robust)
AIC(model_robust, model7)

plot(model_robust)

model_mixed <- lmer(medal_rank ~ poly(gdp_rank, 2) + poly(pop_rank, 2) + winter + (1 | country), 
                    data = ranking)
summary(model_mixed)


ranef_model <- ranef(model_mixed)$country
ranef_df <- as.data.frame(ranef(model_mixed)$country)
ranef_df$country <- rownames(ranef_df)

plot(influence(model_mixed))

ggplot(ranking, aes(x = pop_rank, y = medal_rank, color = factor(winter))) +
  geom_point(alpha = 0.6) +
  labs(title = "Medal Rank vs Population Rank, Colored by Winter Olympics Participation",
       x = "Population Rank", y = "Medal Rank", color = "Winter Olympics") +
  theme_minimal()

ggplot(ranef_df, aes(x = reorder(country, `(Intercept)`), y = `(Intercept)`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Random Intercepts for Each Country",
       x = "Country", y = "Random Intercept") +
  theme_minimal()

top_5_positive <- ranef_df %>%
  arrange(desc(`(Intercept)`)) %>%
  head(5)

top_5_negative <- ranef_df %>%
  arrange(`(Intercept)`) %>%
  head(5)

highlighted_countries <- bind_rows(top_5_positive, top_5_negative)

# Create a plot with only the highlighted countries
ggplot(highlighted_countries, aes(x = reorder(country, `(Intercept)`), y = `(Intercept)`)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 5 Most Positive and Negative Random Intercepts",
       x = "Country", y = "Random Intercept") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) 
