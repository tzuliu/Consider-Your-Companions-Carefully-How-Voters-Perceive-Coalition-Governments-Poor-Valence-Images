library("cjoint")

round3 <- read.csv("/Users/tpliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2/temp3.csv", header=TRUE)

attribute_list <- list()
attribute_list[["FeatGender"]] <-c("Female", "Male")
attribute_list[["FeatPartyID"]] <- c("Conservative Party", "Labour Party", "Liberal Democrats")
attribute_list[["FeatEUIntegration"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")
attribute_list[["FeatScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")
attribute_list[["FeatPMPartyID"]] <- c("Labour Party", "Conservative Party")
attribute_list[["FeatPMScandal"]] <- c("No Scandal", "Having an Extramarital Affair", "Plagiarism",
                                     "Sexual Harassment", "Misreporting Funds")
attribute_list[["FeatPMPosition"]] <- c("Oppose EU Integration", "Support EU Integration", "Neutral")

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
baselines$FeatPMScandal <- "No Scandal"

r3test <- makeDesign(type='constraints', attribute.levels=attribute_list, constraints=constraint_list)

r3results <- amce(Chosen.Candidate ~ FeatGender + FeatPartyID + FeatEUIntegration + FeatScandal + FeatPMScandal + 		              FeatPMPosition,
                  data=round3, baselines=baselines, #respondent.varying = "Gender",
                  cluster=TRUE, respondent.id="Case.ID", design=r3test)

summary(r3results)

pdf("r3results.pdf", width=3, height=4)
plot(r3results, xlab="Change in Pr(PM Preffered)", text.size=5)
dev.off()

jpeg("r3results.jpg")
plot(r3results, xlab="Change in Pr(PM  Preffered)", text.size=15)
dev.off()
