
library(osutools)
library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)

osu.10 <- "tests/osu/10th.osu"
osu.11 <- "tests/osu/Luminal.osu"
osu.12 <- "tests/osu/Tachyon.osu"

osu.10 %<>% chartParse() %>% stressSim() %>% mutate(label = "10")
osu.11 %<>% chartParse() %>% stressSim() %>% mutate(label = "11")
osu.12 %<>% chartParse() %>% stressSim() %>% mutate(label = "12")

osu.stress <- rbind(osu.10, osu.11, osu.12)

osu.stress %<>%
  group_by(keys, offsets, label) %>%
  summarise(stress = quantile(stress, probs = c(0.75))) %>%
  mutate(bins = (offsets %/% 5000) * 5000) %>%
  group_by(bins, label) %>%
  summarise(stress = mean(stress))

ggplot(osu.stress) +
  aes(bins, stress,
      group = label,
      color = label) +
  geom_line(alpha = 0.2) +
  geom_smooth(span = 0.3, se = F)

# ---

osu.10 <- "tests/osu/10th.osu"
osu.11 <- "tests/osu/Luminal.osu"
osu.12 <- "tests/osu/Tachyon.osu"

osu.10 %<>% chartParse() %>% diffBroadcast() %>% mutate(label = "10")
osu.11 %<>% chartParse() %>% diffBroadcast() %>% mutate(label = "11")
osu.12 %<>% chartParse() %>% diffBroadcast() %>% mutate(label = "12")

osu.bcst <- rbind(osu.10, osu.11, osu.12)

osu.bcst %<>%
  filter(keys.froms == keys.tos) %>%
  mutate(jack.inverse = 1 / diffs) %>%
  group_by(keys.froms, offsets, label) %>%
  summarise(jack.inverse.med = quantile(jack.inverse, probs = c(0.75))) %>%
  mutate(bins = (offsets %/% 5000) * 5000) %>%
  group_by(bins, label) %>%
  summarise(jack.inverse.med = mean(jack.inverse.med))

ggplot(osu.bcst) +
  aes(bins, jack.inverse.med,
      group = label,
      color = label) +
  geom_line(alpha = 0.2) +
  geom_smooth(span = 0.2, se = F)


osu.bcst.c <- osu.bcst %>%
  dcast(bins ~ label,
        value.var = 'jack.inverse.med', fill = 0)


osu.stress.c <- osu.stress %>%
  dcast(bins ~ label,
        value.var = 'stress', fill = 0)

osu.j <- cbind(osu.stress.c[,1], (osu.stress.c[,2:4] * osu.bcst.c[,2:4]))
osu.j %<>%
  rename(bins = `osu.stress.c[, 1]`) %>%
  melt(id.vars = 1,
       variable.name = 'label')

ggplot(osu.j) +
  aes(value,
      group = label,
      color = label) +
  geom_histogram(bins = 30) +
  facet_wrap(. ~ label, nrow = 1)


osu.j %>%
  group_by(label) %>%
  summarise(calc = quantile(value, probs = 0.75))

