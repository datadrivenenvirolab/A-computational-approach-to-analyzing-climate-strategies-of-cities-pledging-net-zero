#Ian French

#Remaking plots for the NLP paper

#Setwd
setwd("~/plots")

#load libraries
library(tidyverse)
library(dplyr)
library(scales)
library(UESIplots)
library(Cairo)
library(RColorBrewer)
library(wesanderson)
library(extrafont)
library(ggrepel)
library(ggplot2)
library(ggrepel)
library(patchwork)
loadfonts()


#region counts
region.counts <- read.csv("data/region_counts.csv", stringsAsFactors = F)

#reorder greatest to smallest
region.counts <- region.counts[order(region.counts$region),]

cairo_pdf("outputs/region_counts_plot_v2.pdf", width=8, height=3)

#barplot of regions
ggplot(data=region.counts, aes(x=reorder(X,region), y=region)) +
  geom_bar(stat="identity", fill="#4575B4") +
  geom_text(aes(label=region), position=position_dodge(width=1), hjust=-0.075, family = "Myriad Pro Light") +
  labs(x = "", y = "Number of Cities") +
  scale_y_continuous(breaks=c(25,50,75,100,125,150,175,200)) +
  coord_flip() +
  theme_UESI() +
  theme(axis.text=element_text(size=12))

dev.off()


#significant terms
significant.terms <- read.csv("data/econwide_2grams_regression_significnt_terms.csv", stringsAsFactors = F)

#modify column for labels in plot
significant.terms <- significant.terms %>% mutate(sig = case_when(pval < 0.01 ~ "*", TRUE ~ ""))
significant.terms$pval <- interaction( "P-value: ", significant.terms$pval, sep = "")

significant.terms$topic <- NA

significant.terms$topic[which(significant.terms$term == "class")] <- "N/A"
significant.terms$topic[which(significant.terms$term == "lag")] <- "N/A"
significant.terms$topic[which(significant.terms$term == "fee")] <- "Governance"
significant.terms$topic[which(significant.terms$term == "energy plan")] <- "Governance"
significant.terms$topic[which(significant.terms$term == "square")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "boiler")] <- "Emissions Reduction"
significant.terms$topic[which(significant.terms$term == "emission generate")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "path")] <- "N/A"
significant.terms$topic[which(significant.terms$term == "climate protection")] <- ""
significant.terms$topic[which(significant.terms$term == "advocate")] <- "Human-Centered"
significant.terms$topic[which(significant.terms$term == "ghg reduction")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "chp")] <- "Emissions Reduction"
significant.terms$topic[which(significant.terms$term == "passive")] <- "Emissions Reduction"
significant.terms$topic[which(significant.terms$term == "year use")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "theme")] <- "N/A"
significant.terms$topic[which(significant.terms$term == "inclusive")] <- "Human-Centered"
significant.terms$topic[which(significant.terms$term == "wood")] <- "Emissions Reduction"
significant.terms$topic[which(significant.terms$term == "life cycle")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "percent")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "opportunity reduce")] <- "Quantitative Metrics"
significant.terms$topic[which(significant.terms$term == "mayor")] <- "Governance"
significant.terms$topic[which(significant.terms$term == "climate protection")] <- "N/A"

#replace N/A with other
significant.terms <- significant.terms %>% mutate(topic = str_replace(topic, "N/A", "Other"))

cairo_pdf("outputs/econwideterm_v2.pdf", width=8.5, height=3.5)

#set label font size
geom.text.size = 3

#set color palette

#barplot of significant terms
ggplot(data=significant.terms, aes(x=reorder(term,coef), y=coef, fill=topic)) +
  geom_bar(stat="identity", aes(fill=topic)) +
  labs(x = "", y = "log(coefficient)", caption="Significant terms (p < 0.01) are designated with *") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 25)) +
  scale_fill_manual(name = "Topic", values=wes_palette("Darjeeling1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(family = "Myriad Pro Light")) +
  geom_text(aes(label = sig), hjust = -0.2, colour = "black", family = "Myriad Pro Light", size = geom.text.size) +
  theme_UESI() +
  theme(legend.position = "right")

dev.off()


#GT80pctreduction_2grams_regression_significnt_terms.csv plot

significant.terms2 <- read.csv("data/GT80pctreduction_2grams_regression_significnt_terms.csv", stringsAsFactors = F)

#modify column for easier labels
significant.terms2$pval <- interaction( "P-value:", significant.terms2$pval, sep = "")

png("outputs/GT80pctreduction_2grams_regression_significnt_terms.png")

#barplot of significant terms
ggplot(data=significant.terms2, aes(x=reorder(term,coef), y=coef)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "", y = "log(coefficient)", title="Ambitious Net Zero Plan Significant Terms") +
  ylim(0,15) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(label = pval), hjust = -0.5, colour = "black") +
  theme_UESI() +
  theme(text = element_text(family = "Myriad Pro"))

dev.off()


#topic scores
topic_counts <- read.csv("data/topic_counts.csv", stringsAsFactors = F)

cairo_pdf("outputs/topic_counts.pdf", width=8, height=3)

 ggplot(data=topic_counts, aes(x=reorder(X,score), y=score)) +
  geom_bar(stat="identity", fill="#4575B4") +
  labs(x="", y = "Frequency") +
  coord_flip() +
  theme_UESI() +
  theme(panel.grid.major = element_blank(),panel.border = element_blank(),
        panel.background = element_blank())

dev.off()



#topic correlation plot

#install.packages("ggcorrplot")
library(ggcorrplot)

#install.packages("corrr")
library(corrr)


topic.correlations <- read.csv("data/topic_correlations.csv", stringsAsFactors = F)

#round numbers
topic.correlations <- topic.correlations %>% 
  mutate(across(where(is.numeric), round, 2))

#rename rows
row.names(topic.correlations) <- c("land use","offsets","transportation","heating", "energy","pollution/waste","building","climate impacts","industry")

#rename columns
names(topic.correlations)[names(topic.correlations) == "land.use"] <- "land use"
names(topic.correlations)[names(topic.correlations) == "pollution.waste"] <- "pollution/waste"
names(topic.correlations)[names(topic.correlations) == "climate.impacts"] <- "climate impacts"

#get rid of useless column
topic.correlations <- topic.correlations[,-1]

#convert to matrix
topic.correlations <- as.matrix(topic.correlations)

library(reshape2)
melted_cormat <- melt(topic.correlations)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


#Correlation matrix
# Get lower triangle of the correlation matrix
get_upper_tri<-function(topic.correlations){
  topic.correlations[lower.tri(topic.correlations, diag = TRUE)] <- NA
  return(topic.correlations)
}
# Get upper triangle of the correlation matrix
get_lower_tri <- function(topic.correlations){
  topic.correlations[upper.tri(topic.correlations, diag = TRUE)] <- NA
  return(topic.correlations)
}

upper_tri <- get_upper_tri(topic.correlations)


# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#Reorder
reorder_cormat <- function(topic.correlations){
  # Use correlation between variables as distance
  dd <- as.dist((1-topic.correlations)/2)
  hc <- hclust(dd)
  topic.correlations <-topic.correlations[hc$order, hc$order]
}


# Reorder the correlation matrix
topic.correlations <- reorder_cormat(topic.correlations)
upper_tri <- get_upper_tri(topic.correlations)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

#Add correlation coefficients

png("outputs/topic_correlations_plot.png")

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

dev.off()



#Similarity Plot 

#merge in coordinator info

coordinators <- read.csv("data/eu_cities_netzerostatus_coordinators.csv", stringsAsFactors = F)

similarity.data <- read.csv("data/similarity_graph_data.csv", stringsAsFactors = F)

similarity.data <- similarity.data %>%
  rename(name = city)

similarity.data <- similarity.data %>%
  left_join(coordinators, by = c("name"))

#separate out relevant coordinators
list <- c("Region of Sardinia","Province of Bergamo", "Province of Alicante",
          "Climate Alliance, DE","Provincie Vlaams-Brabant", "Network of Sustainable Greek Islands, GR",
          "Regione Emilia-Romagna","Region of Friuli Venezia Giulia")

similarity.data <- similarity.data %>%
  mutate(Coordinator = ifelse((coordinator_name %in% list), coordinator_name, "N/A"))

#generate palette
pal <- wes_palette("Darjeeling1", 9, type = "continuous") 

#scatterplot
similarity.graph <- ggplot(similarity.data, aes(x=x, y=y, color=region, label = name, shape = Coordinator)) +
  geom_point() +
  scale_shape_manual(values=c(15,19,17,18,13,25,7,8,9))+
  scale_color_manual(name="", values=pal) +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text_repel(aes(label = name), size = 1.2, max.overlaps = Inf) +
  theme_UESI() + 
  theme(legend.position="bottom", legend.text=element_text(size=rel(0.5)),
        legend.title = element_text(size=rel(0.5)), legend.box="vertical", legend.margin=margin(0,0,0,0),
        legend.spacing.y = unit(0, 'line'), legend.justification="center",
        legend.box.margin=margin(-10,-10,-10,-10))


cairo_pdf("outputs/similarity_graph_plot_v2.pdf", width=10, height=11)
ggplot(similarity.data, aes(x=x, y=y, color=region, label = name, shape = Coordinator)) +
  geom_point() +
  scale_shape_manual(values=c(15,19,17,18,13,25,7,8,9))+
  scale_color_manual(name="", values=pal) +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text_repel(aes(label = name), size = 2.5, max.overlaps = Inf) +
  guides(shape=guide_legend(nrow = 3))+
  theme_UESI() + 
  theme(legend.position="bottom", legend.text=element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)), legend.box="vertical")
dev.off()



#Factor Loadings plot

library(ggpubr)

factor.loadings <- read.csv("data/factor_loadings2.csv", stringsAsFactors = F)

#old file - wrong!
#factor.loadings <- read.csv("data/factor_loadings.csv", stringsAsFactors = F)

names(factor.loadings)

factor.loadings <- factor.loadings %>%
  rename(Infrastructure= Factor.1..Urban.Infrastructure, Ecology = Factor.2..Ecological.Factors)

#reshape this 
factor.loadings.m <- factor.loadings %>% pivot_longer(Infrastructure:Ecology, names_to="factors", values_to="value")

#specify color
cols <- c("#D73027","#4575B4")

#names(factor.loadings)

cairo_pdf("outputs/factor_loadings_v2.pdf", width=8.5, height=3.5)
ggplot(data=factor.loadings.m, aes(x=reorder_within(X, value, factors), y=value, fill=factors)) +
  geom_bar(stat="identity") +
  scale_x_reordered() +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),lim = c(-1,1)) +
  coord_flip() +
  facet_wrap(~factors, scales="free_y") +
  scale_fill_manual(name="",values=cols) +
  theme_UESI()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
dev.off()

#first plot
plot1 <- ggplot(data=factor.loadings, aes(x=X)) +
  geom_bar(stat="identity",aes(x=reorder(X, factor1),y=factor1,fill = "steelblue"),colour = "steelblue") +
  labs(x = "term", y = "",title = "Factor 1: Urban Infrastructure") +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),lim = c(-1,1)) +
  coord_flip() +
  scale_colour_manual(name="",values=cols) + scale_fill_manual(name="",values=cols, labels = c("score")) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_UESI()

#second plot
plot2 <- ggplot(data=factor.loadings, aes(x=X)) +
  geom_bar(stat="identity",aes(reorder(X, factor2),y=factor2,fill = "steelblue"),colour = "steelblue") +
  labs(x = "term", y = "", title = "Factor 2: Ecological Considerations") +
  scale_y_continuous(breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),lim = c(-1,1)) +
  coord_flip() +
  scale_colour_manual(name="",values=cols) + scale_fill_manual(name="",values=cols, labels = c("score")) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_UESI()

png("outputs/factor_loadings_plot3.png")

plot3<- ggarrange(plot1,plot2,
                  ncol = 1, nrow = 2,
                  common.legend = TRUE,
                  align = c("h"),
                  legend = "none")

plot3

dev.off()




#quadrant plot exploration

factor <- read.csv("data/factor_scores.csv", stringsAsFactors = F)

factor1 <- factor[-2,]

factor2 <- factor[-1,]

factor1 <- factor1 %>%
pivot_longer(!X_key,names_to = "city")

factor2 <- factor2 %>%
  pivot_longer(!X_key,names_to = "city")

factor1 <- factor1 %>%
  rename(environmental_factor = value) %>%
  select(-X_key)

factor2 <- factor2 %>%
  rename(economic_factor = value) %>%
  select(-X_key)

factor <- factor1 %>%
  left_join(factor2, by = c("city"))

x <- factor

x$city <- sub("\\.", " ", x$city)

#add labels for cities 
x$label <- NA

list <- c("Fontanafredda", "London", "Riga", "Philadelphia", "Munster",
          "Braslau", "Izegem", "Lisbon", "Santiago","San Francisco", 
          "Austin", "Chicago", "Houston", "Portland", "Los Angeles",
          "Tainan", "Vancouver", "Durban", "Buenos Aires", "Sydney")

list <- c("San Francisco", "Austin", "Los Angeles", "Chicago", "Singapore",
          "Tokyo", "London","Philadelphia","Munster", "Fontanafredda",
          "Izegem","Santiago","Lisbon","Tainan", "Vancouver", "Durban", 
          "Buenos Aires", "Sydney")

#figure out spacing of this
          
x <- x %>%
  mutate(label = ifelse(city %in% list, "Yes", NA)) 


# Four quadrant plot
# environmental factor - median for cities is -0.28
med <- median(x$environmental_factor, na.rm=T)
sd <- sd(x$environmental_factor, na.rm=T)
ymed <- median(x$economic_factor, na.rm=T)
ysd <- sd(x$environmental_factor, na.rm=T)

cairo_pdf("outputs/quadrant_plot.pdf")

x %>%
  ggplot(aes(x=environmental_factor, y=economic_factor))+
  geom_rect(aes(xmin = -Inf, xmax = med, ymin = -Inf, ymax = ymed), fill = "white", color = "white", alpha=0.05) + #"4393C3" #blue
  geom_rect(aes(xmin = med, xmax = Inf, ymin = -Inf, ymax = ymed), fill = "#D73027", color = "#D73027", alpha=0.05) + #"C93312" #red
  geom_rect(aes(xmin = -Inf, xmax = med, ymin = ymed, ymax = Inf),fill = "#4575B4", color = "#4575B4", alpha=0.05) + #"C93312" #green
  geom_rect(aes(xmin = med, xmax = Inf, ymin = ymed, ymax = Inf),fill = "mediumpurple1", color = "mediumpurple1", alpha=0.05) + #"F2AD00" #orange
  #geom_rect(aes(xmin = med+sd, xmax = med-sd, ymin = -Inf, ymax = Inf),fill = "lightgray", alpha=0.4) + 
  scale_y_continuous(limits=c(-3,3), breaks=seq(-3,3, by = 0.5)) +
  scale_x_continuous(limits=c(-3,3), breaks=seq(-3,3, by = 0.5)) +
  geom_text(x = -1.75, y = 3, aes(label = "Quadrant 1"), #upper left quadrant 1
            color='white', alpha=.1, size=5,family = "Myriad Pro") + 
  geom_text(x = -1.75, y = -3, aes(label = "Quadrant 3"), #bottom  left
            color='black', alpha = .1, size=5,family = "Myriad Pro") +   
  geom_text(x = 1.5, y = 3, aes(label = "Quadrant 2"),  # upper right quadrant
           color='white', alpha = .1, size=5,family = "Myriad Pro") +   
  geom_text(x = 1.75, y = -3, aes(label = "Quadrant 4"), #bottom right
            color='white', alpha = .1,size=5,family = "Myriad Pro") +
  geom_point(alpha=0.5) +
  scale_size_continuous(name="% reduction target", range=c(0,5))+
  geom_vline(xintercept = med, linetype="dashed")+
  geom_hline(yintercept = ymed, linetype="dashed")+theme_bw()+theme(panel.grid.minor = element_blank(), legend.position = "bottom",legend.direction = "horizontal") +
  #annotate(geom="text", label="<- Median", x=0.2, y=-2.75, family = "Myriad Pro") +
  #annotate(geom="text", label="v Median", x=-2.5, y=0.3, family = "Myriad Pro") +
  labs(x = "Ecology", y = "Infrastructure") +
  geom_text_repel(data=subset(x, !is.na(x$label)), aes(label= city), color = "black",
                  min.segment.length = unit(0, 'lines'), family = "Myriad Pro") +
  theme_UESI()

dev.off()


#nudge_y = 0.2, nudge_x = 0.2

#Examples in each
min(x$environmental_factor)
min(x$economic_factor)
max(x$environmental_factor)

#separate quadrants

q2 <- x[(x$environmental_factor < med & x$economic_factor > ymed),] %>% mutate(quadrant = 2)
q1 <- x[(x$environmental_factor > med & x$economic_factor > ymed),] %>% mutate(quadrant = 1)
q3 <- x[(x$environmental_factor < med & x$economic_factor < ymed),] %>% mutate(quadrant = 3)
q4 <- x[(x$environmental_factor > med & x$economic_factor < ymed),] %>% mutate(quadrant = 4)

# combine all 
quadrants <- bind_rows(q1, q2, q3, q4)

write.csv(q1, "outputs/q1.csv", row.names = F)
write.csv(q2, "outputs/q2.csv", row.names = F)
write.csv(q3, "outputs/q3.csv", row.names = F)
write.csv(q4, "outputs/q4.csv", row.names = F)

### 4/26/22 AH addition 
# climate zone information
cities_meta <- read_csv("~/Documents/GitHub/net_zero/data/Sidd/net_zero_NLP_metadata_master_w_climate.csv") %>%
               mutate(climate_zone = case_when(climate_class_present %in% c(1,3) ~ "Tropical",
                                               climate_class_present %in% c(4,5,6,7) ~ "Arid",
                                               climate_class_present %in% c(8,9,11,12,14,15) ~ "Temperate",
                                               climate_class_present %in% c(18, 25,26,27) ~ "Cold",
                                               climate_class_present == 29 ~ "Polar",
                                               TRUE ~ "None"))

# join with quadrants
cities_meta <- cities_meta %>% left_join(quadrants, by=c("name"="city"))

# boxplot
pal <- wes_palette("Darjeeling1", 6, type="continuous")

envt_factor <- cities_meta %>% filter(climate_zone != "None") %>%
  ggplot(aes(x=climate_zone, y= environmental_factor, fill=climate_zone)) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="", y="Environmental Factor Score") +
  theme_UESI()

econ_factor <- cities_meta %>% filter(climate_zone != "None") %>%
  ggplot(aes(x=climate_zone, y= economic_factor, fill=climate_zone)) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="", y="Economic Factor Score") +
  theme_UESI()

cairo_pdf("outputs/boxplots_comparing_factors_climate_zones.pdf", width=11, height=6)
envt_factor + econ_factor
dev.off()

# boxplots according to cluster
emis_pc <- cities_meta %>% filter(!is.na(quadrant)) %>%
  ggplot(aes(x=factor(quadrant), y= as.numeric(emis_per_capita), fill=factor(quadrant))) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="Quadrant", y="Emissions per capita") +
  theme_UESI()

pop_dens <- cities_meta %>% filter(!is.na(quadrant) & as.numeric(pop_density) < 10000) %>%
  mutate(pop_density=case_when(name == "Hebron" ~ 2900, 
                               name == "New Orleans, LA" ~ 2267,
                               TRUE ~ as.numeric(pop_density))) %>% # issue w/ Hebron
  ggplot(aes(x=factor(quadrant), y=as.numeric(pop_density), fill=factor(quadrant))) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="Quadrant", y="Population density") +
  theme_UESI()

cairo_pdf("outputs/boxplots_comparing_factors_pop_emis.pdf", width=11, height=6)
emis_pc + pop_dens
dev.off()

# additional factors to consider - population, area, continent
pop <- cities_meta %>% filter(!is.na(quadrant)) %>%
  ggplot(aes(x=factor(quadrant), y=as.numeric(population/1e6), fill=factor(quadrant))) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="Quadrant", y="Population in Millions") +
  theme_UESI()

area <- cities_meta %>% filter(!is.na(quadrant)) %>%
  ggplot(aes(x=factor(quadrant), y=as.numeric(area), fill=factor(quadrant))) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="Quadrant", y="Area (sq. km)") +
  theme_UESI()

cairo_pdf("outputs/boxplots_comparing_factors_pop_area.pdf", width=11, height=6)
pop + area
dev.off()

# boxplots by region on factors
pal <- wes_palette("Darjeeling1", 7, type="continuous")

envt_factor <- cities_meta %>% 
  ggplot(aes(x=region, y= environmental_factor, fill=region)) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="", y="Environmental Factor Score") +
  theme_UESI() +
  theme(axis.text.x = element_text(angle=90))

econ_factor <- cities_meta %>%
  ggplot(aes(x=region, y= economic_factor, fill=region)) +
  geom_boxplot(show.legend=FALSE) +
  scale_fill_manual(name="", values=pal) +
  labs(x="", y="Economic Factor Score") +
  theme_UESI() +
theme(axis.text.x = element_text(angle=90))

cairo_pdf("outputs/boxplots_comparing_factors_regions.pdf", width=11, height=6)
envt_factor + econ_factor
dev.off()


# examine if any differences across pop_density, emis_per_capita, climate zone, through box plots and kruskal-wallis tests 
kruskal.test(log(as.numeric(pop_density)) ~ quadrant, data = cities_meta) # p < 0.001 so are significantly different 
kruskal.test(log(as.numeric(emis_per_capita)) ~ quadrant, data = cities_meta) # p = 0.00472
kruskal.test(log(as.numeric(emis_per_capita)) ~ quadrant, data = cities_meta) # p = 0.00472



#most extreme examples 
#Top Left
#Fontanafredda, London, Riga, Philadelphia, Munster

#Bottom Left
#Braslau, Izegem, Lisbon, Santiago

#Top Right
#San Francisco, Austin, Chicago, Houston, Portland, Los Angeles

#Bottom Right
#Tainan, Vancouver, Durban, Buenos Aires, Sydney


#topic dictionary

topicdict <- read.csv("data/topicdict.csv", stringsAsFactors = F)

topicdict <- topicdict %>%
  pivot_longer(!X_key,names_to = "city")

#boxplot of city factor scores
city.factor <- read.csv("data/city_factor_scores.csv", stringsAsFactors = F)

#convert to long format
city.factor.m <- city.factor %>% pivot_longer(Ecology:Infrastructure, names_to="factors", values_to="value")

p <- ggplot(city.factor.m, aes(x=factors, y=value,fill=factors)) +
  geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values=c("#D73027", "#4575B4"))+
  labs(x="", y="")+
  theme_UESI()

# get list of outliers 
out <- ggplot_build(p)[["data"]][[1]][["outliers"]]

#label list elements with factor levels
names(out) <- levels(factor(city.factor.m$factors))

#left join to get city names
tidyout <- purrr::map_df(out, tibble::as_tibble, .id = "factors") %>%
           left_join(city.factor.m, by=c("factors", "value")) %>%
           rename("label"="X")

# remove some to make it less crowded
tidyout$label[tidyout$label=="Lipsi (Aagen Islands)"] <- NA

cairo_pdf("outputs/city_factors_boxplot.pdf", width=6)
p + geom_text_repel(data = tidyout, aes(factors, value, label = label), segment.color = 'transparent', 
              size=2.5, family="Myriad Pro Light")
dev.off()





  
