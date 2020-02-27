library(tidyverse)
library(reshape2)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

fish <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_full.csv"), na = c("", "."))
names(fish) <- stringr::str_to_lower(names(fish))
fish$data_source <- as.factor(fish$data_source)

# summary(fish)

fish <- fish %>% 
  filter(richness >5, individuals > 20)

summary(fish)

ggplot2::ggplot(melt(fish), aes(x = value)) +
  geom_histogram() +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title=paste0("Kaskaskia Basin Fish Metrics"), y = "Count") +
  facet_wrap(~variable, scales = "free")

tester <- melt(fish)

#### Trial by Fire Work around ####
# metric_list <- fish %>%
#   select(-c(site_id, data_source)) %>% 
#   names()
# 
# sum_df[[i]] <- for (i in metric_list) {
#   
#   fish %>%   
#   summarize(mmse_mean = mean(i),   
#             mmse_se = sd(i)/sqrt(n()),
#             n_samples = n())
#     }

by_metrics <- fish %>%
  select(-c(site_id, data_source)) %>% 
  rename(fecunditytl = fecundity_tl)

sum_df <- by_metrics %>%
  summarise_all(list(~mean(.), ~sd(.), ~IQR(.), ~var(.), n(), ~n_distinct(.), ~skewness(.), ~kurtosis(.)))

fish_summary <- melt(sum_df)
fish_summary$variable <-   str_replace(fish_summary$variable, "n_distinct","ndistinct")

fish_summary$metric <-stringr::str_extract(fish_summary$variable, "[:alnum:]*(?=[:punct:])")
fish_summary$summary_metric<- stringr::str_extract(fish_summary$variable, "(?<=[:punct:])[:alnum:]*")

fish_summary <- fish_summary %>% 
  select(metric, summary_metric, value) %>% 
  spread(summary_metric, value) %>% 
  select(metric, n, mean, var, sd, IQR, ndistinct)
  
write_csv(fish_summary, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/metric_summary_stats.csv"))


library(psych)
fish_summary <- describe(fish)
write.csv(fish_summary, paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/metric_summary_stats.csv"))

#### Loop Trial ####
# metric_list <- fish %>%
#   select(-c(site_id, data_source)) %>%
#   names() %>% 
#   list()
# 
# pdf(file="metric_plots.pdf")
# 
# plot_list = list()
# 
# for (i in metric_list) {
#   x = fish[,i]
#   plot_list[[i]] <- ggplot2::ggplot(data.frame(x), aes(x)) +
#     geom_histogram() +
#     theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#     labs(title=paste0(i, " -Kaskaskia Basin Fish Metrics"),x=paste0(i), y = "Count")
#   
#   print(plot_list[[i]])
# }
# 
# dev.off()

pdf(file=paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/metric_histograms.pdf"))

for (col in 3:ncol(fish)) {
  hist(fish[,col])
  main = names(df[col])
}

dev.off()

as.numeric(fish[,5])
hist(fish[,5])
###

metric_list <- fish %>%
  select(-c(site_id, data_source)) 
  # names() %>% 
  # list()

pdf(file="metric_plots.pdf")

plot_list <- list()

for (i in colnames(metric_list)) {
  plot_list[[i]] <- ggplot2::ggplot(fish, aes(x = i)) +
    geom_histogram()

   print(plot_list[[i]])
}

dev.off()
###
metric_list = fish %>%
  select(-c(site_id, data_source)) %>% 
  colnames() %>% 
  list()

plot_list = list()

for (i in seq_along(metric_list)) {
  plot_list[[i]] = ggplot2::ggplot(fish, aes(x = metric_list[[i]])) +
    geom_histogram()
}

pdf("plots.pdf")
for (i in metric_list) {
  print(plot_list[[i]])
}
dev.off()

###
a <- ggplot2::ggplot(fish, aes(x = individuals)) +
  geom_histogram()

print(a)


for (i in seq_along(metric_list)){
  print(metric_list[[i]])
}

#### Example ####
# Plot separate ggplot figures in a loop.
library(ggplot2)

# Make list of variable names to loop over.
var_list = combn(names(iris)[1:3], 2, simplify=FALSE)

# Make plots.
plot_list = list()
for (i in 1:3) {
  p = ggplot(iris, aes_string(x=var_list[[i]][1], y=var_list[[i]][2])) +
    geom_point(size=3, aes(colour=Species))
  plot_list[[i]] = p
}

# Another option: create pdf where each page is a separate plot.
pdf("plots.pdf")
for (i in 1:3) {
  print(plot_list[[i]])
}
dev.off()



#### Dragons ####
# fish2 <- fish %>%
#   select(-c((tidyselect::vars_select(names(fish), ends_with('ntax', ignore.case = TRUE)))))
# 
# 
# ggplot2::ggplot(fish, aes(x = individuals, fill= data_source)) +
#   geom_histogram() +
#   theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#   labs(title=paste0("Kaskaskia Basin Fish- Individuals"), x= "Individuals", y = "Count", fill = "Survey Type") +
#   scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
#   scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))
# 
# ggplot2::ggplot(fish, aes(x = catontax, fill= data_source)) +
#   geom_histogram() +
#   theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#   labs(title=paste0("Kaskaskia Basin Fish- Catontax"), x= "Catontax", y = "Count", fill = "Survey Type") +
#   scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
#   scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))
# 
# ggplot2::ggplot(fish, aes(x = catopind)) +
#   geom_histogram() +
#   theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#   labs(title=paste0("Kaskaskia Basin Fish- Catopind"), x= "Catopind", y = "Count") +
#   scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
#   scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))
# 
# ggplot2::ggplot(melt(fish), aes(x = value)) +
#   geom_histogram() +
#   theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#   labs(title=paste0("Kaskaskia Basin Fish Metrics"), y = "Count") +
#   facet_wrap(~variable, scales = "free")

#### Below there be dragons ####
# 
# fish2 <- fish %>% 
#   select(-c((tidyselect::vars_select(names(fish), ends_with('ntax', ignore.case = TRUE)))))
# 
# summary(fish2)
# 
# metric_list <- fish %>%
#   select(-c(site_id, data_source)) %>% 
#   names()
# 
# pdf(file="metric_plots.pdf")
# 
# plot_list = list()
# 
# for (i in metric_list) {
#    p = ggplot2::ggplot(fish, aes(x = i, fill= data_source)) +
#     geom_histogram(binwidth = 5, position="identity", alpha=0.5) +
#     theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#     labs(title=paste0("Kaskaskia Basin Fish- ", i),x=paste0(i), y = "Count", fill = "Survey Type") +
#     scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
#     scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9")) 
#    plot_list[[i]] = p
#    
#    print(plot_list[[i]])
# }
# 
# dev.off()
# 
# pdf(file="metric_plots_trial.pdf")
# 
# plot_list = list()
# 
# for (i in metric_list) {
#   p = ggplot2::ggplot(fish, aes(x = i, fill= data_source)) +
#     geom_bar(stat ="identity") +
#     # Stat = bin width 
#     
#     labs(title=paste0("Kaskaskia Basin Fish- ", i),x=paste0(i), y = "Count")
# 
#   plot_list[[i]] = p
#   
#   print(plot_list[[i]])
# }
# 
# dev.off()
# 
# 
# # Listen up here's the story of how my life got flipped turned up side down. 
# # If you take a minutes and sit right there please tell me how to get these data in order like Bellaire.
# 
# ggplot2::ggplot(fish, aes(x = individuals, fill= data_source)) +
#   geom_histogram(binwidth = 5, position="identity", alpha=0.5) +
#   theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#   labs(title=paste0("Kaskaskia Basin Fish", metric),x=paste0(metric), y = "Count", fill = "Survey Type") +
#   scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
#   scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))
# 
# ggplot2::ggplot(fish, aes(x = richness, fill= data_source)) +
#   geom_histogram(binwidth = 1, position="identity", alpha=0.5) +
#   theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
#   labs(title="Kaskaskia Basin Fish Matrix",x="Species Richness", y = "Count", fill = "Survey Type") +
#   scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
#   scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))
