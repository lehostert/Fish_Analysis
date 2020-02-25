library(tidyverse)

network_prefix <- "//INHS-Bison"
# network_prefix <- "/Volumes"

fish <- read_csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Output/fish_matrix_full.csv"), na = c("", "."))
names(fish) <- stringr::str_to_lower(names(fish))
fish$data_source <- as.factor(fish$data_source)

# summary(fish)

fish <- fish %>% 
  filter(richness >5, individuals > 20)

summary(fish)

metric_list <- fish %>%
  select(-c(site_id, data_source)) %>% 
  names()
sum_df[[i]] <- for (i in metric_list) {
  
  fish %>%   
  summarize(mmse_mean = mean(i),   
            mmse_se = sd(i)/sqrt(n()),
            n_samples = n())
    }

by_metrics <- fish %>%
  select(-c(site_id, data_source))

sum_df <- by_metrics %>%
  summarise_all(list(~mean(.), ~sd(.), ~IQR(.), ~var(.), ~n_distinct(.)))
  



#### Below there be dragons ####
# 
# fish2 <- fish %>% 
#   select(-c((tidyselect::vars_select(names(fish), ends_with('ntax', ignore.case = TRUE)))))
# 
# summary(fish2)
# 
# metric_list <- fish2 %>%
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
