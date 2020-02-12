library(tidyverse)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

fish <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_full.csv"), na = c("", "."))
names(fish) <- stringr::str_to_lower(names(fish))

ggplot2::ggplot(fish, aes(x = individuals, fill= data_source)) +
  geom_histogram(binwidth = 5, position="identity", alpha=0.5) +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title="Kaskaskia Basin Fish Matrix",x="Individuals", y = "Count", fill = "Survey Type") +
  scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
  scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))

ggplot2::ggplot(fish, aes(x = richness, fill= data_source)) +
  geom_histogram(binwidth = 1, position="identity", alpha=0.5) +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title="Kaskaskia Basin Fish Matrix",x="Species Richness", y = "Count", fill = "Survey Type") +
  scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
  scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))

fish_df <- fish %>% 
  filter(richness >5, individuals > 20)

ggplot2::ggplot(fish_df, aes(x = richness, fill= data_source)) +
  geom_histogram(binwidth = 1, position="identity", alpha=0.5) +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title="Kaskaskia Basin Fish Matrix",x="Species Richness", y = "Count", fill = "Survey Type") +
  scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
  scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))

summary(fish_df)

# list <-lapply(1:ncol(mtcars),
#               function(col) ggplot2::qplot(mtcars[[col]],
#                                            geom = "histogram",
#                                            binwidth = 1))
# 
# cowplot::plot_grid(plotlist = list)