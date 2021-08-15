library("ggplot2")
library("dplyr")

load("/Users/tzupingliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2/candidate.rda")

cand <- candidate[seq(1, nrow(candidate), 12), ]
cand <- cand %>% mutate(Respondent=as.factor(online))
levels(cand$Respondent) <- plyr::mapvalues(levels(cand$Respondent), levels(cand$Respondent), c("in-lab", "online"))
cand <- cand %>% count(Gender,Respondent) %>% group_by(Gender) %>% mutate(pct=n/sum(n))
g <- ggplot(cand, aes(Gender, n, fill=Respondent))
#png("gender_online.png", width=600, height=600, res=135)
pdf("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/gender_online.pdf")
g + geom_bar(stat="identity") +
## sprintf("%1.1f", X) means print out X rounded at first decimal
theme_bw(base_size=8) +
geom_text(aes(label=paste0(sprintf("%1.1f", cand$pct*100),"%")),
          position=position_stack(vjust=0.5)) +
labs(y = "Frequency")
dev.off()
