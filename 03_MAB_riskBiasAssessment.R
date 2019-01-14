## Assessments script for 
## "The behavioral phenotype of early life adversity: a 3-level meta-analysis of preclinical studies"

## Author: Valeria Bonapersona
## Supervision: Caspar J van Lissa

## for questions, contact valeria @ v.bonapersona-2@umcutrecht.nl


# Environment Preparation -------------------------------------------------
rm(list = ls()) #clean environment

#libraries
library(dplyr) #general
library(ggplot2) #for graphs
library(scales) #for percent_format function
library(pwr) #for power calculation
library(stats) #for regression line in plot


load("data.RData")


# Risk of Bias assessment Graph -------------------------------------------------
#for study level biases
biasStudy <- data %>% 
  filter(!duplicated(id)) %>%
  select(seqGeneration, baseline, allocation, housing, incData)

#summarize amounts for each bias assessment component
mat01 <- matrix(nrow = 5, ncol = 4)
for (i in 1:5) {
  mat01[i,] <- as.vector(table(factor(biasStudy[,i], levels = c("N", "C", "Y", "NS"))))
}
mat01 <- cbind(mat01, rep("study", 5))

#for outcome level biases
biasExp <- data %>% 
  select(blindExp, control, outAss, outBlind)
mat02 <- matrix(nrow = 4, ncol = 4)

for (i in 1:4) {
  mat02[i,] <- as.vector(table(factor(biasExp[,i], levels = c("N", "C", "Y", "NS"))))
}
mat02 <- cbind(mat02, rep("comp", 4))


mat <- rbind(mat01, mat02)
colnames(mat) <- c("N", "C", "Y", "NS", "level")


rm(biasStudy, biasExp, mat01, mat02)


pltdf <- data.frame(question = c("seqGeneration", "baseline", "allocation",
                                 "housing", "incData", "blindExp", "control",
                                 "outAss", "outBlind"), 
                    
                    mat)

bias <- reshape(pltdf, 
                varying = c("N", "C", "Y", "NS"),
                v.names = "score",
                timevar = "result",
                times = c("N", "C", "Y", "NS"),
                # new.row.names = 1:123,
                direction = "long")

bias$score <- as.numeric(as.character(bias$score))
bias$Per <- ifelse(bias$level == "study", (bias$score / length(unique(data$id)))*100,
                   (bias$score / length(data$id))*100)

bias$result <- factor(bias$result, levels = c("N", "NS", "C", "Y"))
bias$question <- factor(bias$question, levels = c("incData", "housing", "outBlind",
                                                  "outAss", "seqGeneration", "baseline",
                                                  "control", "allocation", "blindExp"))

rm(mat,pltdf)

#general plot with reported potential bias questions
bg <- ggplot(bias,aes(x = question, y = Per, fill = result)) + 
  theme_classic() +
  facet_grid(level ~., 
             scales = "free") +
#  xlab("") + ylab("") +
  geom_bar(position = "fill",stat = "identity", width = .5, colour = "black") +
  scale_fill_manual("", values = c("#000000", "#606060", "#bababa", "#ffffff")) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("allocation" = "Was group allocation random?",
                              "blindExp" = "Were the experimenters blinded?",
                              "control" = "Was the control group a reliable baseline?",
                              "baseline" = "Were to outcomes adjusted for confounders?",
                              "seqGeneration" = "Was animal selection random?",
                              "outAss" = "Was outcome assessment performed randomly?",
                              "outBlind" = "Was outcome assessment performed blindly?",
                              "housing" = "Were the animals randomly housed?",
                              "incData" = "Were incomplete data adequately addressed?")) +
  coord_flip() + 
  ggtitle("Bias assessment") +
  #  theme(plot.title = element_text(lineheight = .8, face = "bold")) +
  theme(plot.title = element_text(size = 30, face = "bold"),
        axis.text.x = element_text(size = 24),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank(), legend.justification = c(1,1),
        legend.text = element_text(size = 20))

print(bg)
#ggsave("MAB_figures/MAB_biasAssessment.tiff")



# Risk of Bias assessment - quantitative ----------------------------------
#reporting on all bias items

data$allItems <- ifelse(data$seqGeneration != "NS" & data$baseline != "NS" & 
                          data$allocation != "NS" & data$housing != "NS" & 
                          data$incData != "NS" & data$blindExp != "NS" & 
                          data$control != "NS" & data$outAss != "NS" & 
                          data$outBlind != "NS", 1, 0)
sum(data$allItems) #amount of papers that report on all items

#most common score
x <- sum(bias$score)
bias %>% group_by(result) %>%
  summarize(sum(score),
            (sum(score)/x)*100)

#blinded and randomized
data %>% group_by(blindRand) %>%
  summarize(length(each),
            length(unique(id)))
