rm(list=ls(all=TRUE))
library("cregg")
library("ggplot2")
library("plyr")
library("dplyr")
####for rearranging facet plot in ggplots
library("grid")
library("gtable")
####for showing layout of ggplots
library("lemon")

load("/Users/tzupingliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2/minister.rda")

mcon <- minister %>% filter(FeatPMPartyID=="Conservative Party")
mcon$FeatPartyID <- droplevels(mcon$FeatPartyID)

mcon$FeatPMScandal <- revalue(mcon$FeatPMScandal, c("No Scandal"="No Scandal (P)", 
                                                    "Having an Extramarital Affair"="Having an Extramarital Affair (P)", 
                                                    "Plagiarism"="Plagiarism (P)",
                                                    "Sexual Harassment"="Sexual Harassment (P)", 
                                                    "Misreporting Funds"="Misreporting Funds (P)"))

mcon$FeatPMPosition <- revalue(mcon$FeatPMPosition, c("Neutral"="Neutral (P)", 
                                                      "Oppose EU Integration"="Oppose EU Integration (P)", 
                                                      "Support EU Integration"="Support EU Integration (P)"))

form <- ChosenCandidate ~ FeatGender + FeatEUIntegration + FeatScandal + FeatPMPosition + FeatPMScandal

## Specify the order of dispalyed features on the plot
feat_order <- c("FeatPMScandal", "FeatPMPosition", "FeatScandal", "FeatEUIntegration", "FeatGender")

## Re-specify the order of levels of each feature
## When displaying, the order from top to down is from right to left here
mcon$FeatGender <- factor(mcon$FeatGender, levels = c("Female", "Male"))

mcon$FeatEUIntegration <- factor(mcon$FeatEUIntegration, levels = c("Oppose EU Integration",
                                                                    "Support EU Integration", "Neutral"))
mcon$FeatScandal <- factor(mcon$FeatScandal, levels = c("No Scandal",
                                                        "Having an Extramarital Affair", "Plagiarism",
                                                        "Misreporting Funds", "Sexual Harassment"))
mcon$FeatPMPosition <- factor(mcon$FeatPMPosition, levels = c("Oppose EU Integration (P)",
                                                              "Support EU Integration (P)", "Neutral (P)"))
mcon$FeatPMScandal <- factor(mcon$FeatPMScandal, levels = c("No Scandal (P)",
                                                            "Having an Extramarital Affair (P)", "Plagiarism (P)",
                                                            "Misreporting Funds (P)", "Sexual Harassment (P)"))

## Only applied to a vector, not a data.frame
mcon$FeatGender <- recode(mcon$FeatGender, "Female" = "    Fmale",
                          "Male" = "    Male")
mcon$FeatEUIntegration <- recode(mcon$FeatEUIntegration,
                                 "Oppose EU Integration" = "    Oppose EU Integration",
                                 "Support EU Integration" = "    Support EU Integration",
                                 "Neutral" = "    Neutral")
mcon$FeatScandal <- recode(mcon$FeatScandal, "No Scandal" = "    No Scandal",
                           "Plagiarism" = "    Plagiarism",
                           "Having an Extramarital Affair" = "    Having an Extramarital Affair",
                           "Misreporting Funds" = "    Misreporting Funds",
                           "Sexual Harassment" = "    Sexual Harassment")
mcon$FeatPMPosition <- recode(mcon$FeatPMPosition ,
                              "Oppose EU Integration (P)" = "    Oppose EU Integration (P)",
                              "Support EU Integration (P)" = "    Support EU Integration (P)",
                              "Neutral (P)" = "    Neutral (P)")
mcon$FeatPMScandal <- recode(mcon$FeatPMScandal, "No Scandal (P)" = "    No Scandal (P)",
                             "Plagiarism (P)" = "    Plagiarism (P)",
                             "Having an Extramarital Affair (P)" = "    Having an Extramarital Affair (P)",
                             "Misreporting Funds (P)" = "    Misreporting Funds (P)",
                             "Sexual Harassment (P)" = "    Sexual Harassment (P)")

## Re-name the label of each feature
feat_label_list <- list()
feat_label_list[["FeatGender"]] <- "Minister's Gender:"
feat_label_list[["FeatPartyID"]] <- "Minister's Party ID:"
feat_label_list[["FeatEUIntegration"]] <- "Minister's EU Integration Attitude: "
feat_label_list[["FeatScandal"]] <- "Minister's Scandal:"
feat_label_list[["FeatPMScandal"]] <- "PM's Scandal:"
feat_label_list[["FeatPMPosition"]] <- "PM's EU Integration Attitude:"

result <- cj(mcon, form, id = ~CaseID,
             estimate = "mm", feature_order=feat_order,
             feature_labels = feat_label_list,
             by = ~FeatPartyID)

beta <- rev(result$estimate) 
b_low <- rev(result$lower)
b_high <- rev(result$upper)

name <- c("Male", "Female", "Neutral", "Support EU Integration", "Oppose EU Integration", 
          "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
          "No Scandal", "Neutral (P)", "Support EU Integration (P)", "Oppose EU Integration (P)",
          "Sexual Harassment (P)", "Misreporting Funds (P)", "Plagiarism (P)", 
          "Having an Extramarital Affair (P)", "No Scandal (P)",
          "Male", "Female", "Neutral", "Support EU Integration", "Oppose EU Integration", 
          "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
          "No Scandal", "Neutral (P)", "Support EU Integration (P)", "Oppose EU Integration (P)",
          "Sexual Harassment (P)", "Misreporting Funds (P)", "Plagiarism (P)", 
          "Having an Extramarital Affair (P)", "No Scandal (P)")

name <- paste0("    ", name)

new_data <- cbind.data.frame(name, beta, b_low, b_high, rev(data.frame(result$FeatPartyID)))
new_data <- rbind(new_data[1:5,], new_data[10,], new_data[6:9,], new_data[11:13,], new_data[18,], new_data[14:17,],
                  new_data[19:23,], new_data[28,], new_data[24:27,], new_data[29:31,], new_data[36,], new_data[32:35,])

new_data$result.FeatPartyID <- recode(new_data$result.FeatPartyID, "Conservative Party" = "Unified Government",
                                      "Liberal Democrats" = "Coalition Government")

colnames(new_data) <- c("name", "beta", "b_low", "b_high", "GovernmentType")
new_data$GovernmentType <- as.factor(new_data$GovernmentType)

#### Create another vector of labels/levels for plotting
name2 <- c("Male", "Female", "Neutral", "Support EU Integration", "Oppose EU Integration", 
           "No Scandal", "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
           "Neutral (P)", "Support EU Integration (P)", "Oppose EU Integration (P)",
           "No Scandal (P)", "Sexual Harassment (P)", "Misreporting Funds (P)", 
           "Plagiarism (P)","Having an Extramarital Affair (P)")

#### Add new labels/levels for plotting
name2 <- paste("   ", name2)
name2 <- append(name2, "Minister's Gender:", after=0)
name2 <- append(name2, "Minister's EU Integration Attitude:", after=3)
name2 <- append(name2, "Minister's Scandal:", after=7)
name2 <- append(name2, "PM's EU Integration Attitude:", after=13)
name2 <- append(name2, "PM's Scandal:", after=17)

n_new_data <- rbind.data.frame(new_data[14:18,], new_data[32:36,])

pdf("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/m_gov_type.pdf")
#png("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/m_gov_type.png", width=800, height=600, res=135)
ggplot(new_data, aes(y=name, x=beta, group = GovernmentType, colour = GovernmentType)) +  
  geom_point(position = ggstance::position_dodgev(height = 0.7)) + 
  geom_errorbarh(aes_string(xmin = "b_low", xmax = "b_high"),  size = 0.3, height = 0, na.rm = TRUE, position = ggstance::position_dodgev(height = 0.7)) + 
  labs(x = paste("Marginal Mean Probability (Minister Preferred) by Government's Type",
                 collapse = ","), y = "") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme_bw(base_size=8) +
  scale_y_discrete(limits = rev(name2)) +
  theme(legend.position = "bottom") +
  theme(legend.key=element_rect(fill="transparent", colour="transparent")) +
  theme(axis.text.y=element_text(hjust=0, size = 7, face="bold"), axis.title.x=element_text(size=6.5, face="bold"))
dev.off()
