library(readxl)
library(tidyverse)
library(sysfonts)
library(showtext)

font_add_google("Space Grotesk", "spacegrotesk")
showtext_auto()

df_kinnisvara <- read_excel("data/Kinnisvara hinnastatistika.xlsx", sheet = "Kinnisvara hinnastatistika", skip = 4) %>%
  fill(1:2, .direction = "down") %>%
  filter(.[[3]] == "KOKKU") %>%
  select(-3)

colnames(df_kinnisvara) <- c("aasta", "omavalitsus", "tehingute_arv", "keskmine_pindala_m2", "mediaan_pinnaühiku_hind_eur_m2")

df_kinnisvara <- df_kinnisvara %>%
  mutate(
    aasta = as.integer(aasta),
    tehingute_arv = as.numeric(tehingute_arv),
    keskmine_pindala_m2 = as.numeric(keskmine_pindala_m2),
    mediaan_pinnaühiku_hind_eur_m2 = as.numeric(mediaan_pinnaühiku_hind_eur_m2)
  )

df_sissetulek <- read_csv("data/Sissetuleku statistika.csv", skip = 2, locale = locale(decimal_mark = ".", grouping_mark = ",")) %>%
  select(-1) %>%
  setNames(c("aasta", "netosissetulek")) %>%
  mutate(aasta = as.integer(aasta), netosissetulek = as.numeric(netosissetulek)
  )

df_kombineeritud <- left_join(df_kinnisvara, df_sissetulek, by = "aasta") %>%
  mutate(kuupalk = netosissetulek / 12,
  palk_ruutmeeter_suhe = (kuupalk / mediaan_pinnaühiku_hind_eur_m2) * 100
)

saveRDS(df_kombineeritud, file = "data/df_kombineeritud.rds")
saveRDS(df_kombineeritud, file = "app/df_kombineeritud.rds")