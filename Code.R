# =========================
# 1. LOAD LIBRARIES
# =========================
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(tseries)
library(car)

# =========================
# 2. LOAD DATA
# =========================
data <- read_excel("econ_data.xlsx")

# =========================
# 3. CREATE SEASONAL DUMMIES
# =========================
data <- data %>%
  mutate(
    Q2 = ifelse(Qtr == 2, 1, 0),
    Q3 = ifelse(Qtr == 3, 1, 0),
    Q4 = ifelse(Qtr == 4, 1, 0)
  )

# =========================
# 4. CREATE LOG VARIABLES
# =========================
data <- data %>%
  mutate(
    ln_primary = log(Primary),
    ln_resale = log(Resale),
    ln_income = log(Income),
    ln_pop = log(Pop),
    ln_withdrawn = log(Withdrawn + 1)
  )

# =========================
# 5. CREATE FIRST DIFFERENCES (Δlog)
# =========================
data <- data %>%
  mutate(
    d_ln_primary = c(NA, diff(ln_primary)),
    d_ln_resale = c(NA, diff(ln_resale)),
    d_ln_income = c(NA, diff(ln_income)),
    d_ln_pop = c(NA, diff(ln_pop)),
    d_ln_withdrawn = c(NA, diff(ln_withdrawn))
  )

# Remove first NA row
data_diff <- na.omit(data)

# =========================
# 6. STATIONARITY CHECK (ADF)
# =========================
adf.test(data_diff$d_ln_primary)
adf.test(data_diff$d_ln_resale)
adf.test(data_diff$d_ln_income)
adf.test(data_diff$d_ln_pop)
adf.test(data_diff$d_ln_withdrawn)

# =========================
# 7. REGRESSION — PRIMARY MARKET (DIFF LOG)
# =========================
model_primary_diff <- lm(
  d_ln_primary ~
    D_covid + T_covid +
    D_end + T_end +
    D_pol + T_pol +
    d_ln_income + d_ln_pop + d_ln_withdrawn +
    Q2 + Q3 + Q4,
  data = data_diff
)

# =========================
# 8. REGRESSION — RESALE MARKET (DIFF LOG)
# =========================
model_resale_diff <- lm(
  d_ln_resale ~
    D_covid + T_covid +
    D_end + T_end +
    D_pol + T_pol +
    d_ln_income + d_ln_pop + d_ln_withdrawn +
    Q2 + Q3 + Q4,
  data = data_diff
)

# =========================
# 9. STANDARD SUMMARY
# =========================
summary(model_primary_diff)
summary(model_resale_diff)

# =========================
# 10. NEWEY-WEST ROBUST SE
# =========================
coeftest(model_primary_diff, vcov = NeweyWest(model_primary_diff, lag = 4, prewhite = FALSE))
coeftest(model_resale_diff, vcov = NeweyWest(model_resale_diff, lag = 4, prewhite = FALSE))

# =========================
# 11. MULTICOLLINEARITY (VIF)
# =========================
vif(model_primary_diff)
vif(model_resale_diff)

# =========================
# 12. AUTOCORRELATION (Durbin-Watson)
# =========================
dwtest(model_primary_diff)
dwtest(model_resale_diff)

# =========================
# 13. HETEROSKEDASTICITY (Breusch-Pagan)
# =========================
bptest(model_primary_diff)
bptest(model_resale_diff)

# =========================
# 14. RESIDUAL DIAGNOSTICS
# =========================
par(mfrow = c(2,2))
plot(model_primary_diff)

par(mfrow = c(2,2))
plot(model_resale_diff)


hist(residuals(model_primary_diff), breaks = 10, main = "Residuals Histogram")
hist(residuals(model_resale_diff), breaks = 10, main = "Residuals Histogram")

qqnorm(residuals(model_primary_diff))
qqline(residuals(model_primary_diff), col = "red")
qqnorm(residuals(model_resale_diff))
qqline(residuals(model_resale_diff), col = "red")

shapiro.test(residuals(model_primary_diff))
shapiro.test(residuals(model_resale_diff))

library(tseries)
jarque.bera.test(residuals(model_primary_diff))
jarque.bera.test(residuals(model_resale_diff))
