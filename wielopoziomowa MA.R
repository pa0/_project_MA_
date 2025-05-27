aggregated <- dataset %>%
  group_by(article_id) %>%
  summarise(
    mean_effect = sum((1 / vi) * `effect size`) / sum(1 / vi),
    vi_agg = 1 / sum(1 / vi),  # wariancja średniego efektu
    k = n()                    # liczba efektów w artykule
  )

library(metafor)

# model jednopoziomowy
model <- rma(yi=dataset$`effect size`, vi = dataset$vi)
model |> summary()


# Model wielopoziomowy: efekty zagnieżdżone w artykułach
model <- rma.mv(yi = dataset$`effect size`, V =  dataset$vi, random = ~ 1 | article_id/effect_id, data = dataset)
model |> summary()


# Wydobycie komponentów wariancji
tau2_between <- model$sigma2[1]  # między artykułami
tau2_within  <- model$sigma2[2]  # wewnątrz artykułów
mean_vi <- mean(dataset$vi, rm.na=T)    # średnia wariancja błędu (sampling variance)

# Obliczenie udziałów wariancji
I2_between <- tau2_between / (tau2_between + tau2_within + mean_vi)
I2_within  <- tau2_within  / (tau2_between + tau2_within + mean_vi)

# Wyświetlenie wyników jako procenty
cat("I² between articles: ", round(100 * I2_between, 2), "%\n")
cat("I² within articles:  ", round(100 * I2_within, 2), "%\n")