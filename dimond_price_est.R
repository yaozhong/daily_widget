# tiffany dimond price estimation
# 2017-01-08

# color, D, E, F, G, H, I  (1, 2, 3, 4, 5)
# clarity, IF, VVS1, VVS2, VS1, VS2, SI (1, 2, 3, 4, 5, 6)


samples.small = list(
         c(0.40, 1, 1, 6550),
         c(0.49, 3, 3, 5800), 
         c(0.58, 3, 2, 7350),
         c(0.64, 2, 4, 8500)
         #c(0.73, 3, 3, 9350)
         
)

samples.big = list(
  
        c(1.01, 3, 1, 21100),
        c(1.05, 3, 1, 22000),
        c(1.10, 5, 1, 17500),
        c(1.18, 3, 3, 21000)

)

samples = samples.small
#samples = samples.big

data = matrix(c(unlist(samples)), nrow=length(samples), byrow=T)
colnames(data) <- c("size", "color", "clarity", "price")
data <- as.data.frame(data)

pModel <- glm(data$price ~ data$size+data$color+data$clarity, family = gaussian)
#pModel <- lm(data$price ~ data$size+data$color+data$clarity)

predict <- function(x){
  sum(pModel$coefficients[-1]*x)+pModel$coefficients[1]
}

print("=== Regression Values====")
predict.y <- apply(data[,1:3],1, predict)
print(data[,4])
print(predict.y)
print("====Target Price Prediction=======")
target = c(0.63, 3, 1)  # 8850
print(predict(target))

## upbound 12550
## lowerbound 8380

## lowerbound based on line interplotation
# unitPrice is based on the samll average (F, VVS)
unitPrice = 12700
unitPrice*0.63
# adjustment for big size clarity difference
unitPrice*0.63 + 3155.77 * 0.63 * (8850/21100)


