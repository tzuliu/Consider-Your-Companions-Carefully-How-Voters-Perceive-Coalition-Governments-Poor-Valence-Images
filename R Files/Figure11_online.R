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

minister$FeatPMScandal <- revalue(minister$FeatPMScandal, c("No Scandal"="No Scandal (P)", 
                                                            "Having an Extramarital Affair"="Having an Extramarital Affair (P)", 
                                                            "Plagiarism"="Plagiarism (P)",
                                                            "Sexual Harassment"="Sexual Harassment (P)", 
                                                            "Misreporting Funds"="Misreporting Funds (P)"))

minister$FeatPMPosition <- revalue(minister$FeatPMPosition, c("Neutral"="Neutral (P)", 
                                                              "Oppose EU Integration"="Oppose EU Integration (P)", 
                                                              "Support EU Integration"="Support EU Integration (P)"))

form <- ChosenCandidate ~ FeatGender + FeatPartyID + FeatEUIntegration + FeatScandal + FeatPMPosition + FeatPMScandal

## Specify the order of dispalyed features on the plot
feat_order <- c("FeatPMScandal", "FeatPMPosition", "FeatScandal", "FeatEUIntegration", "FeatPartyID", "FeatGender")

## Re-specify the order of levels of each feature
## When displaying, the order from top to down is from right to left here
minister$FeatGender <- factor(minister$FeatGender, levels = c("Female", "Male"))

minister$FeatEUIntegration <- factor(minister$FeatEUIntegration, levels = c("Oppose EU Integration",
                                                                            "Support EU Integration", "Neutral"))
minister$FeatScandal <- factor(minister$FeatScandal, levels = c("No Scandal",
                                                                "Having an Extramarital Affair", "Plagiarism",
                                                                "Misreporting Funds", "Sexual Harassment"))
minister$FeatPMPosition <- factor(minister$FeatPMPosition, levels = c("Oppose EU Integration (P)",
                                                                      "Support EU Integration (P)", "Neutral (P)"))
minister$FeatPMScandal <- factor(minister$FeatPMScandal, levels = c("No Scandal (P)",
                                                                    "Having an Extramarital Affair (P)", "Plagiarism (P)",
                                                                    "Misreporting Funds (P)", "Sexual Harassment (P)"))

## Only applied to a vector, not a data.frame
minister$FeatGender <- recode(minister$FeatGender, "Female" = "    Fmale",
                              "Male" = "    Male")
minister$FeatEUIntegration <- recode(minister$FeatEUIntegration,
                                     "Oppose EU Integration" = "    Oppose EU Integration",
                                     "Support EU Integration" = "    Support EU Integration",
                                     "Neutral" = "    Neutral")
minister$FeatScandal <- recode(minister$FeatScandal, "No Scandal" = "    No Scandal",
                               "Plagiarism" = "    Plagiarism",
                               "Having an Extramarital Affair" = "    Having an Extramarital Affair",
                               "Misreporting Funds" = "    Misreporting Funds",
                               "Sexual Harassment" = "    Sexual Harassment")
minister$FeatPMPosition <- recode(minister$FeatPMPosition ,
                                  "Oppose EU Integration (P)" = "    Oppose EU Integration (P)",
                                  "Support EU Integration (P)" = "    Support EU Integration (P)",
                                  "Neutral (P)" = "    Neutral (P)")
minister$FeatPMScandal <- recode(minister$FeatPMScandal, "No Scandal (P)" = "    No Scandal (P)",
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

result <- cj(minister, form, id = ~CaseID,
             estimate = "mm", feature_order=feat_order,
             feature_labels = feat_label_list)

beta <- rev(result$estimate) 
b_low <- rev(result$lower)
b_high <- rev(result$upper)

name <- c("Male", "Female", "Liberal Democrats", "Labour Party", "Conservative Party", "Neutral", 
          "Support EU Integration", "Oppose EU Integration", "Sexual Harassment", "Misreporting Funds", 
          "Plagiarism", "Having an Extramarital Affair", "No Scandal", "Neutral (P)", 
          "Support EU Integration (P)", "Oppose EU Integration (P)",
          "Sexual Harassment (P)", "Misreporting Funds (P)", "Plagiarism (P)", 
          "Having an Extramarital Affair (P)", "No Scandal (P)")

name <- paste0("    ", name)

new_data <- cbind.data.frame(name, beta, b_low, b_high)
new_data <- rbind(new_data[1:2,], new_data[4:5,], new_data[3,], new_data[6:8,], new_data[13,], 
                  new_data[9:12,], new_data[14:16,], new_data[20,], new_data[17:19,], new_data[21,])

#### Create another vector of labels/levels for plotting
name2 <- c("Male", "Female", "Labour Party", "Conservative Party", "Liberal Democrats", 
           "Neutral", "Support EU Integration", "Oppose EU Integration", 
           "No Scandal", "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
           "Neutral (P)", "Support EU Integration (P)", "Oppose EU Integration (P)",
           "No Scandal (P)", "Sexual Harassment (P)", "Misreporting Funds (P)", 
           "Having an Extramarital Affair (P)", "Plagiarism (P)")

#### Add new labels/levels for plotting
name2 <- paste("   ", name2)
name2 <- append(name2, "Minister's Gender:", after=0)
name2 <- append(name2, "Minister's Party ID:", after=3)
name2 <- append(name2, "Minister's EU Integration Attitude:", after=7)
name2 <- append(name2, "Minister's Scandal:", after=11)
name2 <- append(name2, "PM's EU Integration Attitude:", after=17)
name2 <- append(name2, "PM's Scandal:", after=21)

pdf("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/minister.pdf")
#png("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/minister.png", width=800, height=600, res=135)
ggplot(new_data, aes(y=name, x=beta)) +  
  geom_point(position = ggstance::position_dodgev(height = 0.7)) + 
  geom_errorbarh(aes_string(xmin = "b_low", xmax = "b_high"),  size = 0.3, height = 0, na.rm = TRUE, position = ggstance::position_dodgev(height = 0.7)) + 
  labs(x = paste("Marginal Mean Probability (Minister Preferred)",
                 collapse = ","), y = "") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme_bw(base_size=8) +
  scale_y_discrete(limits = rev(name2)) +
  theme(legend.position = "bottom") +
  theme(legend.key=element_rect(fill="transparent", colour="transparent")) +
  theme(axis.text.y=element_text(hjust=0, size = 7, face="bold"), axis.title.x=element_text(size=6.5, face="bold"))
dev.off()