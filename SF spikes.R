library(randomForest)

word_cloud <- read_csv("SF Word Cloud Responses - Word Cloud Final.csv")


attributes <- word_cloud %>% 
  gather(name, attribute) %>% 
  group_by(name, attribute = make.names(tolower(attribute))) %>% 
  tally() %>% 
  group_by(attribute) %>% 
  mutate(n_people = length(unique(name))) %>% 
  filter(n_people > 1) %>% 
  select(-n_people) %>% 
  spread(attribute, n, fill = 0) 

attributes %>% filter(name %in% c("Jim Savage", "Stu Feldman")) %>% 
  gather(attribute, votes, -name) %>% 
  group_by(attribute) %>% 
  filter(sum(votes)>1 & attribute != "NA.") %>% 
  spread(name, votes) %>% 
  View()

model_fit <- randomForest(~. -name, data = attributes, mtry = 25, ntree = 1000, proximity = T)


pr <- model_fit$proximity

colnames(pr) <- attributes$name
rownames(pr) <- attributes$name

pr2 <- as.dist(1 - pr)

tmp <- hclust(d = pr2, method = "complete")

plot(as.dendrogram(tmp), horiz = T, xlim = c(1, -0.5), col = cutree(tree = tmp, h = 0.95))

  
data_frame(name = names(cutree(tree = tmp, h = 0.95 )), cluster = cutree(tree = tmp, h = 0.95)) %>% 
  arrange(cluster) %>% 
  write_csv(path = "SF_clusters.csv")


ordered_prox <- as_data_frame(pr) %>% 
  mutate(reference_name = attributes$name) %>% 
  gather(comparison_name, proximity, -reference_name) %>% 
  filter(reference_name != comparison_name) %>%
  arrange(reference_name, proximity) %>% 
  group_by(reference_name) %>% 
  filter(1:n() %in% c(1,2,n()-1, n()))

