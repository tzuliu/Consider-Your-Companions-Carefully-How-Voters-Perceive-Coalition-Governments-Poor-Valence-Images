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

load("/Users/tzupingliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2/primeminister.rda")

pmcon <- primeminister %>% filter(FeatPartyID=="Conservative Party")
pmcon$FeatMPartyID <- droplevels(pmcon$FeatMPartyID)

pmcon$FeatMScandal <- revalue(pmcon$FeatMScandal, c("No Scandal"="No Scandal (M)", 
                                                    "Having an Extramarital Affair"="Having an Extramarital Affair (M)", 
                                                    "Plagiarism"="Plagiarism (M)",
                                                    "Sexual Harassment"="Sexual Harassment (M)", 
                                                    "Misreporting Funds"="Misreporting Funds (M)"))


form <- ChosenCandidate ~ FeatGender + FeatEUIntegration + FeatScandal + FeatMScandal

## Specify the order of dispalyed features on the plot
feat_order <- c("FeatMScandal", "FeatScandal", "FeatEUIntegration", "FeatGender")

## Re-specify the order of levels of each feature
## When displaying, the order from top to down is from right to left here
pmcon$FeatGender <- factor(pmcon$FeatGender, levels = c("Female", "Male"))

pmcon$FeatEUIntegration <- factor(pmcon$FeatEUIntegration, levels = c("Oppose EU Integration",
                                                                      "Support EU Integration", "Neutral"))
pmcon$FeatScandal <- factor(pmcon$FeatScandal, levels = c("No Scandal",
                                                          "Having an Extramarital Affair", "Plagiarism",
                                                          "Misreporting Funds", "Sexual Harassment"))

pmcon$FeatMScandal <- factor(pmcon$FeatMScandal, levels = c("No Scandal (M)",
                                                            "Having an Extramarital Affair (M)", "Plagiarism (M)",
                                                            "Misreporting Funds (M)", "Sexual Harassment (M)"))

## Only applied to a vector, not a data.frame
pmcon$FeatGender <- recode(pmcon$FeatGender, "Female" = "    Fmale",
                           "Male" = "    Male")
pmcon$FeatPartyID <- recode(pmcon$FeatPartyID, "Labour Party" = "    Labour Party",
                            "Conservative Party" = "    Conservative Party",
                            "Liberal Democrats" = "    Liberal Democrats")
pmcon$FeatEUIntegration <- recode(pmcon$FeatEUIntegration,
                                  "Oppose EU Integration" = "    Oppose EU Integration",
                                  "Support EU Integration" = "    Support EU Integration",
                                  "Neutral" = "    Neutral")
pmcon$FeatScandal <- recode(pmcon$FeatScandal, "No Scandal" = "    No Scandal",
                            "Plagiarism" = "    Plagiarism",
                            "Having an Extramarital Affair" = "    Having an Extramarital Affair",
                            "Misreporting Funds" = "    Misreporting Funds",
                            "Sexual Harassment" = "    Sexual Harassment")

pmcon$FeatMScandal <- recode(pmcon$FeatMScandal, "No Scandal (M)" = "    No Scandal (M)",
                             "Plagiarism (M)" = "    Plagiarism (M)",
                             "Having an Extramarital Affair (M)" = "    Having an Extramarital Affair (M)",
                             "Misreporting Funds (M)" = "    Misreporting Funds (M)",
                             "Sexual Harassment (M)" = "    Sexual Harassment (M)")

## Re-name the label of each feature
feat_label_list <- list()
feat_label_list[["FeatGender"]] <- "PM's Gender:"
feat_label_list[["FeatPartyID"]] <- "PM's Party ID:"
feat_label_list[["FeatEUIntegration"]] <- "PM's EU Integration Attitude: "
feat_label_list[["FeatScandal"]] <- "PM's Scandal:"
feat_label_list[["FeatMScandal"]] <- "Minister's Scandal:"

result <- cj(pmcon, form, id = ~CaseID,
             estimate = "mm", feature_order=feat_order,
             feature_labels = feat_label_list,
             by = ~FeatMPartyID)

beta <- rev(result$estimate) 
b_low <- rev(result$lower)
b_high <- rev(result$upper)

name <- c("Male", "Female", "Neutral", "Support EU Integration", "Oppose EU Integration", 
          "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
          "No Scandal", "Sexual Harassment (M)", "Misreporting Funds (M)", "Plagiarism (M)", 
          "Having an Extramarital Affair (M)", "No Scandal (M)",
          "Male", "Female", "Neutral", "Support EU Integration", "Oppose EU Integration", 
          "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
          "No Scandal", "Sexual Harassment (M)", "Misreporting Funds (M)", "Plagiarism (M)", 
          "Having an Extramarital Affair (M)", "No Scandal (M)")
name <- paste0("    ", name)

new_data <- cbind.data.frame(name, beta, b_low, b_high, rev(data.frame(result$FeatMPartyID)))
new_data <- rbind(new_data[1:5,], new_data[10,], new_data[6:9,], new_data[15,], new_data[11:14,],
                  new_data[16:20,], new_data[25,], new_data[21:24,], new_data[30,], new_data[26:29,])

new_data$result.FeatMPartyID <- recode(new_data$result.FeatMPartyID, "Conservative Party" = "Unified Government",
                                       "Liberal Democrats" = "Coalition Government")

colnames(new_data) <- c("name", "beta", "b_low", "b_high", "GovernmentType")
new_data$GovernmentType <- as.factor(new_data$GovernmentType)

#### Create another vector of labels/levels for plotting
name2 <- c("Male", "Female", "Neutral", "Support EU Integration", "Oppose EU Integration", 
           "No Scandal", "Sexual Harassment", "Misreporting Funds", "Plagiarism", "Having an Extramarital Affair",
           "No Scandal (M)", "Sexual Harassment (M)", "Misreporting Funds (M)", 
           "Plagiarism (M)","Having an Extramarital Affair (M)")

#### Add new labels/levels for plotting
name2 <- paste("   ", name2)
name2 <- append(name2, "PM's Gender:", after=0)
name2 <- append(name2, "PM's EU Integration Attitude:", after=3)
name2 <- append(name2, "PM's Scandal:", after=7)
name2 <- append(name2, "Minister's Scandal:", after=13)

n_new_data <- rbind.data.frame(new_data[11:15,],new_data[26:30,])

pdf("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/pm_gov_type_simple.pdf")
#png("/Users/tzupingliu/Desktop/Projects/Conjoin and Cabinet/Figure_New/pm_gov_type.png", width=800, height=600, res=135)
ggplot(n_new_data, aes(y=name, x=beta, group = GovernmentType, colour = GovernmentType)) +  
  geom_point(position = ggstance::position_dodgev(height = 0.7)) + 
  geom_errorbarh(aes_string(xmin = "b_low", xmax = "b_high"),  size = 0.3, height = 0, na.rm = TRUE, position = ggstance::position_dodgev(height = 0.7)) + 
  labs(x = paste("Marginal Mean Probability (Prime Minister Preferred) by Government's Type",
                 collapse = ","), y = "") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme_bw(base_size=8) +
  scale_y_discrete(limits = rev(name2[14:19])) +
  theme(legend.position = "bottom") +
  theme(legend.key=element_rect(fill="transparent", colour="transparent")) +
  theme(axis.text.y=element_text(hjust=0, size = 7, face="bold"), axis.title.x=element_text(size=6.5, face="bold"))
dev.off()

