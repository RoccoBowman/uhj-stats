library(tidyverse)
setwd("~/UHJ")
referrals_data <- read_csv("referrals.csv") %>% 
  select(-2)

t <- data.frame(t(referrals_data)) %>% 
  filter(!row_number() %in% 1) %>% 
  replace(is.na(.), 0) %>% 
  mutate_if(is.character,as.numeric) 

names(t) <- referrals_data$Referrer

t <- t %>% 
  mutate(time = row.names(t), .before = 1) %>% 
  arrange(time)

write_csv(t, "data/referrals_transposed.csv")

ts <- ts(t[2:5], start = c(2014,5), frequency = 12)

start(ts)
end(ts)
time(ts)
deltat(ts)
frequency(ts)
cycle(ts)

ts.plot(ts,
        col = 2:4,
        xlab = "Year",
        ylab = "Count",
        main = paste0("UHJ Views by Referrer from ",
                      start(ts)[1],
                      "-",
                      start(ts)[2],
                      " to ",
                      end(ts)[1],
                      "-",
                      end(ts)[2]))
legend("topleft", colnames(ts), lty = 1, col = 2:4, bty = "n")


request <- ts(t$Google, start = c(2014,5), frequency = 12)
# Plot the Nile data with xlab, ylab, main, and type arguments
plot(request,
     xlab = "Year",
     ylab = "Total Request Volume",
     main = "Volume of Total Requests for the UHJ, 2014-Present",
     type = "b")
# Log rapid_growth
linear_growth <- log(request)

# Plot linear_growth using ts.plot()
ts.plot(linear_growth)
