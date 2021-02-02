stream_attributes_table <- readxl::read_excel("~/CREP/Analysis/Project-Shelley/Data/kasky_stream_lines_attributes_table.xlsx", sheet = 1)

stream_list <- stream_attributes_table %>% 
  filter(PU_CODE == "kasky") %>% 
  select(PU_GAPCODE)

write_csv(stream_list, "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/kasky_pugap_list.csv")


df <- readxl::read_xlsx("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/GL_AQ_GAP_Data_2010/VIEW_CHANNEL_minus_ny.xlsx", sheet = 2)

stream_list2 <- df %>% 
  filter(PU_CODE == "kasky") %>% 
  select(PU_GAP)

stream_list2 <- rename(stream_list2, PU_GAPCODE = PU_GAP)

y <- setdiff(stream_list2, stream_list) 