library(tidyverse)
library(ggpubr)
options(scipen=999)

## convert into tidy data
df <- readxl::read_excel("data/한우_비육우_두당_사육비_20230615125418.xlsx")
df <- df %>% mutate(소분류 = stringr::str_trim(소분류))

## 2022년 규모별 두당 한우 생산비
df %>%
  pivot_longer(cols = 3:7, names_to = "규모", values_to = "비용") %>% 
  filter(규모 != "평균") %>% 
  mutate(
    규모 = factor(규모, levels = c("20두미만", "20~49두", "50~99두", "100두이상"))
  ) %>% 
  ggplot(aes(reorder(대분류, 비용), 비용, fill = 규모)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight(base_family = "NanumGothic") +
  labs(
    title = "2022년 규모별 두당 한우 생산비",
    subtitle = "단위 = 만원"
  ) 

df %>%
  pivot_longer(cols = 3:7, names_to = "규모", values_to = "비용") %>% 
  filter(규모 != "평균") %>% 
  mutate(
    규모 = factor(규모, levels = c("20두미만", "20~49두", "50~99두", "100두이상"))
  ) %>% 
  mutate(비용 = round(비용/10000, 0)) %>% 
  group_by(규모, 대분류) %>% summarise(비용 = sum(비용)) %>% 
  ggbarplot(x = "대분류", y = "비용", fill = "규모", position = position_dodge(0.9), palette = "jco",
            label = TRUE, lab.col = "black") +
  # coord_flip() +
  ggthemes::theme_fivethirtyeight(base_family = "NanumGothic") +
  labs(
    title = "2022년 규모별 두당 한우 생산비",
    subtitle = "단위 = 만원"
  )

## geom_col

order <- df %>% group_by(대분류) %>% summarise(sum = sum(평균)) %>% arrange(sum) %>% pull(대분류)
df %>% select(소분류, 대분류, 평균) %>% 
  mutate(평균 = 평균/10000) %>% 
  mutate(대분류 = factor(대분류, levels = order)) %>% 
  ggplot(aes(대분류, 평균)) +
  geom_col(aes(fill = 소분류)) +
  # scale_fill_viridis_d() +
  coord_flip() +
  ggthemes::theme_fivethirtyeight(base_family = "NanumGothic") +
  labs(
    title = "2022년 비목별 한우 두당 평균 생산비 통계",
    subtitle = "단위 = 만원"
  ) +
  theme(
    legend.position = c(1, 3)
  )

df %>% select(소분류, 대분류, 평균) %>% 
  mutate(평균 = 평균/10000) %>% 
  mutate(대분류 = factor(대분류, levels = order)) %>% 
  ggbarplot(x = "대분류", y = "평균", palette = "jco", fill = "소분류") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight(base_family = "NanumGothic") +
  labs(
    title = "2022년 비목별 한우 두당 평균 생산비 통계",
    subtitle = "단위 = 만원"
  ) +
  theme(
    legend.position = c(0.7, 0.1)
  )

## Sankey Chart