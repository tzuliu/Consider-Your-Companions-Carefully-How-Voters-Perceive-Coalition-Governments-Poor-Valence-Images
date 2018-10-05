library("cjoint")

round1 <- read.csv("/Users/tpliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2/temp.csv", header=TRUE)

attribute_list <- list()
attribute_list[["FeatGender"]] <-c("Female", "Male")
attribute_list[["FeatPartyID"]] <- c("Labour Party", "Conservative Party", "Liberal Democrats")
attribute_list[["FeatEUIntegration"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")
attribute_list[["FeatScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")

constraint_list <- list()

baselines <- list()

baselines$FeatScandal <- "No Scandal"

r1test <- makeDesign(type='constraints', attribute.levels=attribute_list, constraints=constraint_list)

r1results <- amce(Chosen.Candidate ~ FeatGender + FeatPartyID + FeatEUIntegration + FeatScandal,
                  data=round1, baselines=baselines, #respondent.varying = "Gender",
                  cluster=TRUE, respondent.id="Case.ID", design=r1test)

summary(r1results)

pdf("r1results.pdf", width=3, height=4)
plot(r1results, xlab="Change in Pr(Candidate Preffered)", text.size=5)
dev.off()

jpeg("r1results.jpg")
plot(r1results, xlab="Change in Pr(Candidate Preffered)", text.size=15)
dev.off()
