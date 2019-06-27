library(osutools)
library(ggplot2)


j <- calculateDifficulty(
  "../osutools_test/src/r/osu/4/DJ Myosuke & Noizenecio - Architecture (Mat) [Mat's 4k DEATH].osu",
  keyset.select = '4',
  sim.bin.size = 100)

ggplot(d$model) +
  aes(bins, values) +
  geom_line()


chart <- chartParse("../osutools_test/src/r/osu/4/DJ Myosuke & Noizenecio - Architecture (Mat) [Mat's 4k DEATH].osu")


require(dplyr)
require(magrittr)
require(reshape2)
d <- chart %>%
  filter(!types %in% c("lnotel")) %>%
  mutate(types = 1) %>%
  dcast(offsets ~ keys,value.var = "types",fill = 0) %>%
  mutate(bins = (offsets %/% 500) * 500) %>%
  select(-offsets) %>%
  group_by(bins) %>%
  summarize_all(sum) %>%
  melt(id.vars = 1, variable.name = "keys") %>%
  group_by(bins) %>%
  summarize(manips = var(value))

ggplot(d) +
  aes(bins, manips) +
  geom_smooth(span = 0.15, se = F) +
  geom_smooth(span = 0.15, method = 'loess', data = j$model, aes(bins, values/50), se=F,
              color = 'red')

