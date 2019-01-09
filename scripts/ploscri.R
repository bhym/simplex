# For rapid screening of outputs: plot for each species
#  - number of individuals
#  - total biomass of species
#  - average biomass of species

library("tidyverse")
library("gridExtra")

tidy_and_ggplot <- function(INP, tit){
data.frame(INP) %>%
  rename_all(funs(colnames(N))) %>%
  rownames_to_column(var = "time_") %>%
  gather(species, value, -time_)  %>%
  mutate(time_ = as.integer(time_)) %>%
  mutate(species = gsub("X", "", species)) %>%
  ggplot() +
  geom_line(aes(x = time_, y = value, col = species))  +
  facet_wrap(~species, scale = "free_y") +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle(tit)
}

a  <- tidy_and_ggplot(N, "N")
b  <- tidy_and_ggplot(sp.totb, "sp.totb")
c_ <- tidy_and_ggplot(sp.aveb, "sp.aveb")
d  <-  grid.arrange(a, b, c_, ncol = 3)
fine <- runif(1)
fina <- paste("plots/plot", fine, ".pdf", sep = "")
finu <- paste("intmats/intm", fine, ".tab", sep = "")
ggsave(d, file = fina, width = 3.25, height = 1, scale = 2)
write.csv(giug, file = finu, row.names = F)
