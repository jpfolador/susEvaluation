# -------------------------------------------------------------------
# Script for analysis of the evaluation of SIDABI system on SUS score 
# by 36 judges
# Author: Joao Paulo Folador
# email: jpfolador@gmail.com
# -------------------------------------------------------------------

setwd("D:/arquivos_doutorado/Doutorado/Projeto de Pesquisa/_paper_sidabi_2020/usability")

# load the packages
if (!require("tidyr")) { install.packages('tidyr') }
if (!require("irr")) { install.packages('irr') }
if (!require("ggplot2")) { install.packages("ggplot2") }
if (!require("cowplot")) { install.packages("cowplot") }
if (!require("extrafont")) { install.packages("extrafont") }
if (!require("RColorBrewer")) { install.packages("RColorBrewer") }

loadfonts(device = "win")

# load the data
df <- read.csv(file = 'usability-db-en.csv', header = TRUE, sep = ";")

#--------------------
#  Chart analysis
#--------------------

## Build a graph to evaluators profiles
theme_set(theme_bw())

# Age
temp <- data.frame(table(df[,2]))
temp <- temp[order(-temp$Freq),] # order by frequency
temp$Var1 <- factor(temp$Var1, levels = temp$Var1[order(-temp$Freq)])

evaluatorAge <- temp
evaluatorAge$percentage <- lapply(evaluatorAge$Freq, function(x) { 
  round(x*100/36, 2)
})

questionAplot <- ggplot(evaluatorAge, aes(x = Var1, y = Freq)) +
                  geom_bar(stat = "identity", width = 0.5, fill = "black", alpha = 0.4) + 
                  geom_text(aes(label=Freq), vjust=-0.2, color="#444444", size=6)+
                  labs(x = "Variables", y = "Frequency",
                       title="Age") + #, subtitle="How old are you?", caption="") +
                  ylim(0, 32) +
                  theme(text=element_text(size=20,  family="serif"),
                        axis.text.x = element_text(size=22),
                        axis.text.y = element_text(size=22),
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
                        plot.title = element_text(size = 20, color="#222222"),
                        plot.subtitle = element_text(size = 18, color="#666666"))


# How long have you been using a computer?
temp <- data.frame(table(df[,3]))
temp <- temp[order(-temp$Freq),] # order by frequency
temp$Var1 <- factor(temp$Var1, levels = temp$Var1[order(-temp$Freq)])

compExperience <- temp
compExperience$percentage <- lapply(compExperience$Freq, function(x) { 
  round(x*100/36, 2)
})

questionBplot <- ggplot(compExperience, aes(x = Var1, y = Freq)) +
                  geom_bar(stat = "identity", width = 0.5, fill = "grey", alpha = 0.5) + 
                  geom_text(aes(label=Freq), vjust=-0.2, color="#444444", size=6)+
                  labs(x = "Variables", y = "Frequency",
                       title="Experience") + #, subtitle="How long have you been using a computer?", caption="") +
                  ylim(0, 32) +
                  theme(text=element_text(size=20,  family="serif"),
                        axis.text.x = element_text(size=22),
                        axis.text.y = element_text(size=22),
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                        plot.title = element_text(size = 20, color="#222222"),
                        plot.subtitle = element_text(size = 18, color="#666666"))


# How much time in a week do you use a computer?
temp <- data.frame(table(df[,4]))
temp <- temp[order(-temp$Freq),] # order by frequency
temp$Var1 <- factor(temp$Var1, levels = temp$Var1[order(-temp$Freq)])

compUseWeek <- temp
compUseWeek$percentage <- lapply(compUseWeek$Freq, function(x) { 
  round(x*100/36, 2)
})

questionCplot <- ggplot(compUseWeek, aes(x = Var1, y = Freq)) +
                  geom_bar(stat = "identity", width = 0.5, fill = "black", alpha = 0.7) + 
                  geom_text(aes(label=Freq), vjust=-0.2, color="#444444", size=6)+
                  labs(x = "Variables", y = "Frequency",
                       title="Time spent") + #, subtitle="How much time in a week do you spend using a computer?", caption="") +
                  ylim(0, 32) +
                  theme(text=element_text(size=20,  family="serif"),
                        axis.text.x = element_text(size=22),
                        axis.text.y = element_text(size=22),
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                        plot.title = element_text(size = 20, color="#222222"),
                        plot.subtitle = element_text(size = 18, color="#666666"))

plot_grid(questionAplot, questionBplot, questionCplot, nrow=2, ncol=2)

#---------------------------------------------------
#  Understanding the distribution of the questions
#---------------------------------------------------

# get only the SUS questions
dataRaw <- df[1:36, 5:ncol(df)]
dataSus <- dataRaw[1:10]
colnames(dataSus) <- c(1:10)
gatheredData <- gather(dataSus, factor_key = TRUE)


ggplot(gatheredData, aes(x = key, y = value, fill=key), color=key) + 
  geom_boxplot(outlier.color = "#0099ff", outlier.fill = "#0099ff", outlier.shape = 8, outlier.size = 3, size = 0.9) + 
  stat_summary(#aes(label = ..y..), 
                fun = function(x) round(mean(x), 2), 
                geom = "point", 
                shape = 19,
                size = 3,
                color = "red") + 
  labs(x = "Questions", y = "Values", title="SUS questions") +
  scale_fill_grey(start = 0.5, end = 1) +
  theme(text=element_text(size=20,  family="serif"),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        legend.position="none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.subtitle = element_text(size = 18, color="#000000")) + 
  coord_flip()


#-------------
#  SUS score
#-------------

# transpose rows and columns and get just the SUS questions
df2 <- data.frame(t(df[-1]))
df2 <- data.frame(df2[4:13,])
colnames(df2) <- df[, 1]
df3 <- df2
rownames(df3) <- 1:nrow(df3)

somaSus <- function(coluna) {
  soma <- 0
  calc <- 0
  for (key in 1:length(coluna)) {
    value <- as.numeric(coluna[key])
    if (key %% 2 == 0) {
      calc <- 5 - value
    }else{
      calc <- value - 1
    }
    soma <- soma + calc
  }
  
  return(as.numeric(soma*2.5))
}
scoreSus <- data.frame( list(apply(df3, 2, somaSus)) )
colnames(scoreSus) <- "points"
lapply(scoreSus, mean)

scoreSus$x <- seq(1, 100, 2.77)
scoreSus$raters <- seq(1, 36, 1)

mean(scoreSus$points)
sd(scoreSus$points)

  ggplot(scoreSus, aes(x=raters, y=points)) + 
    geom_bar(stat="identity", width = 0.5, fill = "black", alpha = 0.4) + 
    geom_hline(aes(yintercept = as.numeric(formatC(mean(points), 2, format="f"))),
               colour = "red", linetype ="longdash", size = .8) +
    geom_text(aes(0, mean(points),label = formatC(mean(points), 2, format="f"), vjust = -0.3, hjust = -1.1), size=7,  family="serif", colour = "red") +
    geom_hline(aes(yintercept = 68),
               colour = "blue", linetype ="dotted", size = 1.2) +
    geom_text(aes(0, 68,label = 68, vjust = -0.3, hjust = -2.5), size=7, family="serif", colour = "blue") +
    
    labs(x = "Judges", y = "Points", title="SUS Score") + 
    theme(text=element_text(size=20,  family="serif"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.title = element_text(size = 20, color="#000000"),
          plot.subtitle = element_text(size = 18, color="#000000"))


#------------------------
#  Statistical analysis
#------------------------
data <- data.frame(sapply(df2, function(x) as.numeric(as.character(x))))
sapply(data, class)

# Used
kendall(data, correct = TRUE)
