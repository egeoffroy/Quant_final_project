library(outbreaks)
library(ggplot2)

onset_sars <- sars_canada_2003$date
cases_sars <- c(1,1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,8,10,13,13,15,17,22, 29,38,45,48,52,53,57,62,69,76,83,90,93,96,101,104,109,116,120,124,126,129,130,131,132,132,132, 134, 135, 138, 140, 140, 141, 141, 141, 142, 142, 142, 143, 143, 143, 143, 144, 145, 146, 146, 147, 147, 147, 147, 148, 148, 150, 150, 153, 155, 157, 158, 160, 163, 167, 172, 177, 181, 187, 193, 197, 205, 210 ,216, 223, 228, 237, 238, 240, 243, 244, 245, 247, 247, 247, 247, 249, 249, 249, 249, 250)
plot(c(1:110), cases)

onset <- unique(mers_korea_2015$linelist$dt_diag)
#162 cases, 5/20 - 6/16 (28 days)
cases_mers <- c(2,3,3,3,3,3,5,5,7,13,15,18,25,30,30,36,42,64,87,95,108,122,126,138,145,150,154,162)
plot(c(1:28), cases)

onset <- influenza_england_1978_school$date
cases_flu <- influenza_england_1978_school$in_bed
plot(onset,cases)

flu_h7n9 <- cases <- c(rep(1,8),rep(2,8),3,4,rep(5,4),rep(6,4),7,7,9,11,13,14,14,14,16,16,17,21,27,29,
                        32,38,40,47,50,52,59,60,68,71,77,83,89,94,99,103,106,111,114,116,116,118,118,120,
                        120,121,122,123,123,rep(124,4),rep(125,19),126)
flu_cases <- cbind(c(1:length(flu_h7n9)), flu_h7n9)
flu_cases <- as.data.frame(flu_cases)
plot(flu_cases$Day, flu_cases$New_Cases)
#Look at first 14 days of each of the three diseases
data <- data.frame(day=c(1:14), influenza=cases_flu, SARS=cases_sars[1:14], MERS=cases_mers[1:14], flu_cases[1:14])


library(ggplot2)
library(reshape2)
d <- melt(data, id.vars="day")

# Everything on the same plot
ggplot(d, aes(day,value, col=variable)) + 
  geom_point() + 
  stat_smooth() 

# Separate plots
ggplot(d, aes(day,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)

data1 <- data.frame(day=c(1:14), disease=rep('Influenza H1N1', 14), Cases=cases_flu)
data2 <- data.frame(day=c(1:110), disease=rep('SARS', 110), Cases=cases_sars)
data3 <- data.frame(day=c(1:28), disease=rep('MERS', 28), Cases=cases_mers)
data4 <- data.frame(day=c(1:length(flu_h7n9)), disease=rep('Influenza H7N9'), Cases=flu_h7n9)
data <- rbind(data1, data2, data3, data4)

#For each of the datasets, we reject the null hypothesis that the data is normal as each of the tests has a p-value less than alpha at significance of 0.05.
shapiro.test(data1$Cases)
shapiro.test(data2$Cases)
shapiro.test(data3$Cases)

# Everything on the same plot
ggplot(data = data, aes(day,Cases, col=disease)) + 
  geom_point() + 
  stat_smooth() 

# Separate plots
ggplot(data, aes(day,Cases)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)
