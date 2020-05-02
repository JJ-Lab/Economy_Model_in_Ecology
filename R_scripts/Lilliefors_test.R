

library(tidyverse)
library(igraph)
#library(fitdistrplus)
library(nortest)
library(gridExtra)
library(ggpubr)

folder_base <- "web-of-life_WDB/"
files_base <- list.files(folder_base)
list_files <- files_base[grepl("M_PL_", files_base)]

for (i in seq(length(list_files))){

  print(list_files[i])
  
  # Extract the incidence matrix
  inc_matrix <- read.csv(paste0(folder_base,list_files[i]), header=TRUE, row.names=1)
  
  # Create a graph for each incidence matrix
  g_i <- graph_from_incidence_matrix(inc_matrix, directed = FALSE, weighted = TRUE)
  
  # Extract plant and pollinator labels, respectively
  poll_names <- colnames(inc_matrix)
  plant_names <- rownames(inc_matrix)
  
  # Calcualte plant and pollinator strengths, respectively
  strength_plants <- strength(g_i,vids = plant_names)
  strength_poll <- strength(g_i,vids = poll_names)
  
  # Calcualte plant and pollinator degrees, respectively
  degree_plants <- degree(g_i,v = plant_names)
  degree_poll <- degree(g_i,v = poll_names)
  
  #results
  
  name_i <- strsplit(list_files[i],".csv")
  
  # data_i <- tibble(data_set=name_i[[1]],type=c(rep("plant",length(plant_names)),
  #                                              rep("pollinator",length(poll_names))),
  #                    ID = c(plant_names,poll_names),
  #                    log_degree = c(log(degree_plants),log(degree_poll)),
  #                    log_strength = c(log(strength_plants),log(strength_poll))
  #                 )
  
  
  if(length(degree_plants)>4){
    l_deg_plants <- lillie.test(log(degree_plants))
    l_str_plants <- lillie.test(log(strength_plants))
    l_deg_plants <- l_deg_plants$p.value
    l_str_plants <- l_str_plants$p.value
  }else{
    l_deg_plants <- NA
    l_str_plants <- NA
  }
  
  if(length(degree_poll)>4){
    
    if(sum(log(degree_poll))>0){
      l_deg_poll <- lillie.test(log(degree_poll))
      l_deg_poll <- l_deg_poll$p.value
      }
    else{
      l_deg_poll <- NA} #To avoid error in "M_PL_061_33"
    
    l_str_poll <- lillie.test(log(strength_poll))
    l_str_poll <- l_str_poll$p.value
  }else{
    l_deg_poll <- NA
    l_str_poll <- NA
  }
  
  
  data_i <- tibble(data_set=name_i[[1]],l_deg_plants=l_deg_plants,
                   l_deg_poll=l_deg_poll,
                   l_str_plants=l_str_plants,
                   l_str_poll=l_str_poll
                   )
  
  if(i == 1){datasets_test <- data_i}else{datasets_test <- bind_rows(datasets_test,data_i)}
  
}

datasets_test %>% filter(l_deg_poll<0.05) %>% count()
datasets_test %>% filter(l_str_poll<0.05) %>% count()
datasets_test %>% filter(l_deg_plants<0.05) %>% count()
datasets_test %>% filter(l_str_plants<0.05) %>% count()

write_csv(datasets_test,"lnormal_fits/results_Lilliefors_test")



pdf(paste0("lnormal_fits/lilliefors_test_results.pdf"),
    width = 11.69, # The width of the plot in inches
    height = 8.27)



p1<- ggplot(datasets_test)+
  geom_point(aes(x=as.factor(data_set),y=l_deg_plants))+theme_minimal()+
  theme(axis.text.x=element_blank())+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red", size=1)+
  labs(title="log(Degrees) for plants",
       x ="Datasets", y = "p-value")

p2 <- ggplot(datasets_test)+
  geom_point(aes(x=as.factor(data_set),y=l_str_plants))+theme_minimal()+
  theme(axis.text.x=element_blank())+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red", size=1)+
  labs(title="log(Stregths) for plants",
       x ="Datasets", y = "p-value")

p3 <- ggplot(datasets_test)+
  geom_point(aes(x=as.factor(data_set),y=l_deg_poll))+theme_minimal()+
  theme(axis.text.x=element_blank())+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red", size=1)+
  labs(title="log(Degrees) for pollinators",
       x ="Datasets", y = "p-value")

p4 <- ggplot(datasets_test)+
  geom_point(aes(x=as.factor(data_set),y=l_str_poll))+theme_minimal()+
  theme(axis.text.x=element_blank())+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red", size=1)+
  labs(title="log(Stregths) for pollinators",
       x ="Datasets", y = "p-value")

figure <- grid.arrange(p1,p2,p3,p4, 
             ncol = 2, nrow = 2)

annotate_figure(figure,
                top = text_grob("Lilliefors test (p-values)", color = "red", face = "bold", size = 14))


dev.off()
