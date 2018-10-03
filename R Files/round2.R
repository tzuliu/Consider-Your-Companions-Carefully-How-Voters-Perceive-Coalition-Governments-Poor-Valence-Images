library("cjoint")

round2 <- read.csv("/Users/tpliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2/temp2.csv", header=TRUE)

attribute_list <- list()
attribute_list[["FeatGender"]] <-c("Female", "Male")
attribute_list[["FeatPartyID"]] <- c("Conservative Party", "Labour Party")
attribute_list[["FeatEUIntegration"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")
attribute_list[["FeatScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")
attribute_list[["FeatMPartyID"]] <- c("Labour Party", "Conservative Party", "Liberal Democrats")
attribute_list[["FeatMScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")

constraint_list <- list()

#constraint_list[[1]] <- list()
#constraint_list[[1]][["FeatPartyID"]] <- c("Labour Party")
#constraint_list[[1]][["FeatMPartyID"]] <- c("Conservative Party", "Liberal Democrats")

#constraint_list[[2]] <- list()
#constraint_list[[2]][["FeatPartyID"]] <- c("Conservative Party")
#constraint_list[[2]][["FeatMPartyID"]] <- c("Labour Party")



baselines <- list()

baselines$FeatScandal <- "No Scandal"
baselines$FeatPartyID <- "Labour Party"
baselines$FeatMScandal <- "No Scandal"

r2test <- makeDesign(type='constraints', attribute.levels=attribute_list, constraints=constraint_list)

r2results <- amce(Chosen.Candidate ~ FeatGender + FeatPartyID + FeatEUIntegration + FeatScandal + FeatMScandal,
                  data=round2, baselines=baselines, #respondent.varying = "Gender",
                  cluster=TRUE, respondent.id="Case.ID", design=r2test)

summary(r2results)

pdf("r2results.pdf", width=3, height=4)
plot(r2results, xlab="Change in Pr(PM Preffered)", text.size=5)
dev.off()

jpeg("r2results.jpg")
plot(r2results, xlab="Change in Pr(PM  Preffered)", text.size=15)
dev.off()