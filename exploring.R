
cmj_mean <- merged_data %>%
  select(id, body_mass, age_at_test, cmj_height, cmj_rsi, ct, con_imp, rel_pp) %>%
  group_by(id) %>%
  summarise(across(c(body_mass:rel_pp), ~ mean(.x, na.rm = TRUE)))

cor.test(cmj_mean$cmj_height, cmj_mean$con_imp/cmj_mean$body_mass)

ggplot(cmj_mean, aes(x = con_imp, y = cmj_height, color = age_at_test)) +
  geom_point() +
  geom_smooth( method = "lm")


ggplot(merged_data, aes(x = cmj_height, y = imtp)) +
  geom_point() +
  geom_smooth( method = "lm")


cor.test(cmj_mean$body_mass, cmj_mean$con_imp)



ggplot(data = merged_data, aes(x = age_at_test, y = cmj_height, colour = id)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic() +
  theme(legend.position = "none")


library(rmcorr)

rm <- rmcorr(participant = id, measure1 = age_at_test, measure2 = cmj_height, dataset = merged_data)
rm


library(lme4)
library(lmerTest)

m <- lmer(cmj_height ~ age_at_test + (age_at_test | id), data = merged_data)
summary(m)



library(purrr)
library(broom)

# Keep players with enough repeated tests
player_models <- merged_data %>%
  filter(!is.na(age_at_test), !is.na(cmj_height)) %>%
  group_by(id) %>%
  filter(n() >= 3) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(cmj_height ~ age_at_test, data = .x)),
    coefs = map(fit, tidy),
    gl = map(fit, glance)
  ) %>%
  unnest(coefs) %>%
  filter(term == "age_at_test") %>%
  mutate(
    slope_cm_per_year = estimate
  ) %>%
  select(id, slope_cm_per_year, std.error, statistic, p.value)


library(rmcorr)

