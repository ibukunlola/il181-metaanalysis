install.packages("meta")
library(meta)

col.contour = c("gray75", "gray85", "gray95")

# Load the data from github.
data <- read.csv(url("https://raw.githubusercontent.com/ibukunlola/il181-metaanalysis/main/metaanalysis_data.csv"))
head(data)

# Calculate the standard erorr from the effect size estimate and one of the CIs.
Std.Error <- (data$Upper.Limit - data$Effect.size)/1.96

# Append the standard error column to the dataframe 
data <- cbind(data,Std.Error)
head(data)

# RANDOM EFFECTS MODEL
m.gen.random <- metagen(TE = log(Effect.size),
                 seTE = Std.Error,
                 studlab = Authors,
                 data = data,
                 sm = "OR",
                 backtransf = TRUE,
                 method.tau = "PM",
                 fixed = F,
                 random = TRUE,
                 hakn = TRUE,
                 title = "Random Effects Model - Effect of masks on COVID contraction rate (odds ratio)")
summary(m.gen.random)

# Create the forest plot
forest.random <- forest.meta(m.gen.random, 
            sortvar = Effect.size,
            comb.random = TRUE,
            comb.fixed = F,
            print.tau2 = T,
            print.I2 = FALSE,
            leftcols = c("studlab"),
            rightcols = c("effect.ci"),
            xlim = c(0.06, 3),
            rightlabs = c("OR [95% CI]"),
            leftlabs = c("Authors"))


# Create the funnel plot - scatterplot of the studies with std. err on the y-axis and OR on the x-axis
funnel.random <- funnel.meta(m.gen.random,
                            xlim = c(0.04, 2),
                            studlab = TRUE,
                            contour = c(0.9, 0.95, 0.99),
                            col.contour = col.contour)
legend(x = 1.2, y = 0, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

title("Contour-Enhanced Funnel Plot (Odds Ratios)")


# FIXED EFFECTS MODEL
m.gen.fixed<- metagen(TE = log(Effect.size),
                        seTE = Std.Error,
                        studlab = Authors,
                        data = data,
                        sm = "OR",
                        backtransf = TRUE,
                        #method.tau = "PM",
                        fixed = T,
                        random = F,
                        hakn = TRUE,
                        title = "Effect of masks on COVID contraction rate (odds ratio)")
summary(m.gen.fixed)

# Create the forest plot
forest.fixed <- forest.meta(m.gen.fixed, 
                             sortvar = Effect.size,
                             comb.random = TRUE,
                             comb.fixed = T,
                             print.tau2 = F,
                             print.I2 = FALSE,
                             leftcols = c("studlab"),
                             rightcols = c("effect.ci"),
                             xlim = c(0.06, 3),
                             rightlabs = c("OR [95% CI]"),
                             leftlabs = c("Authors"))

# Create the funnel plot - scatterplot of the studies with std. err on the y-axis and OR on the x-axis
funnel.fixed <- funnel.meta(m.gen.fixed,
                             xlim = c(0.04, 2),
                             studlab = TRUE,
                            contour = c(0.9, 0.95, 0.99),
                            col.contour = col.contour)
legend(x = 1.2, y = 0, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

title("Contour-Enhanced Funnel Plot (Odds Ratios)")
