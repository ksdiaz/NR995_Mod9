#NR 995 Fall2017
#Module 9 HW
#23 October 2017
#Group D
#Krystalle Diaz

#setwd("/Users/krystallediaz/Dropbox/Classes/NR 995/Coding/")

####1)	The mammal sleep dataset is included in the ggplot2 package and provides information about the sleeping 
#habits of 83 species of mammals. Load the dataset (library(ggplot2); data(msleep); ?msleep).
library("ggplot2")
data(msleep)
?msleep
#How many diet type (i.e., vore) categories are there? 
length(unique(msleep$vore)) #There are 5 diet type ("vore) categories- carni, omni,herbi, insecti, 
#and an NA category.

#Visually investigate whether daily sleep totals vary with diet type: Make a boxplot comparing the daily sleep 
#totals across the vore categories, excluding rows that do not have data in the vore column. Remember to include 
#informative axis labels. Briefly describe in 1-2 sentences the major patterns in the plot.
ggplot(data=msleep[!is.na(msleep$vore), ], aes(x = vore, y = sleep_total)) + geom_boxplot(stat="boxplot") +
  labs(x= "Diet Type (-vore)", y = "Sleep Total (hours)") + ggtitle("Daily Sleep by Diet Type")
#Carnivores, omnivores, and insectivores have a wide range of values, with most of these values falling into a
#wide range of statiscal values.  Insectivores are similar, but values falling outside of the scope of the quartile
#statistics still cling tightly to those statistical values, and the mean is rather high.  Omnivores have a tight 
#range of quartile statistics, with what could be 4 outlier values.


####2)	Using the mammal sleep dataset, use plot() to show the relationship between the natural log of body size 
#and the length of the sleep cycle, labelling axes appropriately. 
msleep$ln_bodywt <- log(msleep$bodywt) #from ?log : "log computes logarithms, by default natural logarithms"
plot(msleep$sleep_cycle, msleep$ln_bodywt, type="p", xlab="Sleep Total (hours)", ylab="ln (Body Weight (kg))", 
     main="Sleep by ln(Body Weight)")
#ggplot version: 
ggplot(data=na.omit(msleep), aes(x=sleep_total, y=ln_bodywt)) + geom_point(aes(color=order)) + 
  facet_wrap(~conservation, nrow=2) + stat_smooth(method="lm", se=F) #used facet_wrap for the panels because it's easier
#Only domesticated and LC (least concerned) species have more than one point of data.  For these, the data seem to
#have a rather loose pattern, with LC looking like it could be random, or at least affected by other factors that
#are not body weight.  So there doesn't seem to be enough data to make that sort of conclusion.
#Got these codes from the Wikipedia article on conservation status, which is where ?msleep says that data is from.
#nt = near threatened
#en = endangered
#lc = least concern 
#vu = vulnerable

####3)	How does the ratio of brain weight to body weight (i.e., brainwt/bodywt) vary by diet type? 
msleep.sub <- msleep[!is.na(msleep$brainwt) & !is.na(msleep$bodywt) & !is.na(msleep$vore), ]
#function to return a data frame summarizing brain to body weight ratio by diet type
brain_body_ratio <- function(x.data) {
  x.data <- x.data[!is.na(x.data$brainwt) & !is.na(x.data$bodywt) & !is.na(x.data$vore), ] #remove NAs for needed rows
  brain_body_mean <- as.data.frame(tapply(x.data$brainwt/x.data$bodywt, x.data$vore, mean)) #calculate means according to diet type
  new.x <- as.data.frame(rownames(brain_body_mean)) #initialize data frame 
  new.x$brain_body_mean <- brain_body_mean$`tapply(x.data$brainwt/x.data$bodywt, x.data$vore, mean)` #get corresponding values
  colnames(new.x) <- c("vore", "brain_body_mean") #fix those ugly column names
  new.x$brain_body_se <- new.x$brain_body_mean/sqrt(length(new.x$vore)) #column for standard error
  return(new.x)
}
diet_stats <- brain_body_ratio(msleep) 


#In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload a link 
#to your group’s GitHub repository (i.e., http://github.com/username/reponame) to submit your assignment in myCourses, one per group.

