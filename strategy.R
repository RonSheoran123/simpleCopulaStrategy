library (quantmod)
library (copula)
library (urca)

df1 <- as.data.frame(getSymbols('AMZN', src='yahoo', auto.assign=FALSE, from='2014-03-27', to='2017-01-01'))['AMZN.Close']
df2 <- as.data.frame(getSymbols('GOOG', src='yahoo', auto.assign=FALSE, from='2014-03-27', to='2017-01-01'))['GOOG.Close']

df <- data.frame(amzn = df1['AMZN.Close'], goog = df2['GOOG.Close'])

jtest <- ca.jo(df, type="trace", K=2, ecdet="none", spec="longrun")
summary(jtest)

#Finding ratios using Johansen test
# AMZN
h1 <- 0.009333204
# GOOG
h2 <- 0.020163311

u_amzn <- ecdf(df$AMZN.Close)(df$AMZN.Close)
u_goog <- ecdf(df$GOOG.Close)(df$GOOG.Close)

mat <- matrix(, nrow = length(df1[['AMZN.Close']]), ncol=2)

for (i in 1:length(df1[['AMZN.Close']])){
  mat[i,1] <- u_amzn[i]
  mat[i,2] <- u_goog[i]
}

cop <-fitCopula(claytonCopula(dim = 2), mat, method="itau")

money <- 0
position <- 0

for (i in 1:length(df1[['AMZN.Close']])){
  if( ((pCopula(c(mat[i,1], mat[i,2]), cop@copula) / pCopula(c(1, mat[i,2]), cop@copula)) <= 0.05) & ((pCopula(c(mat[i,1], mat[i,2]), cop@copula) / pCopula(c(mat[i,1], 1), cop@copula)) >= 0.95) & position == 0){
    money <- money - (1000 * h1 * df1[['AMZN.Close']][i]) + (1000 * h2 * df2[['GOOG.Close']][i])
    position <- 1
  }
  else if ( ((pCopula(c(mat[i,1], mat[i,2]), cop@copula) / pCopula(c(1, mat[i,2]), cop@copula)) >= 0.95) & ((pCopula(c(mat[i,1], mat[i,2]), cop@copula) / pCopula(c(mat[i,1], 1), cop@copula)) <= 0.05) & position == 1){
    money <- money + (1000 * h1 * df1[['AMZN.Close']][i]) - (1000 * h2 * df2[['GOOG.Close']][i])
    position <- 0
  }
}

print(money)
