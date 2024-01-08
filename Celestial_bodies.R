## Importing packages
install.packages(c('tidyverse', 'cowplot', 'MASS','car','e1071','caret','caTools','kernlab','IRdisplay','plotly'))
library(tidyverse) 
library(cowplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(kernlab)
library(IRdisplay)
library(plotly)
options(warn = -1)
#options(scipen= 999)


################ IMPORTNG CSV FILE #######################

sky <- read.csv("Datasets/Skyserver.csv")

#Checking the data and types
glimpse(sky)


#Checking for NA values, and there are none.
colSums(is.na(sky))


#Checking for unique values in each column
sky%>%summarise_all(funs(n_distinct))


options(repr.plot.width=10, repr.plot.height=6)
p1 <- ggplotly(ggplot(sky, aes(class, fill = class)) + geom_bar() + theme_bw())



theme1<- theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="top")

theme2<- theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="none")

plot_grid(ggplot(sky, aes(ra, fill = class)) + geom_density(alpha = 0.7)+theme1, 
          ggplot(sky, aes(dec, fill = class)) + geom_density(alpha = 0.7)+theme1,
          ggplot(sky, aes(u, fill = class)) + geom_density(alpha = 0.7)+theme1,
          ggplot(sky, aes(g, fill = class)) + geom_density(alpha = 0.7)+theme2,
          ggplot(sky, aes(r, fill = class)) + geom_density(alpha = 0.7)+theme2,
          ggplot(sky, aes(i, fill = class)) + geom_density(alpha = 0.7)+theme2,
          align = "h")


plot_grid(ggplot(sky, aes(z, fill = class)) + geom_density(alpha = 0.7)+theme1, 
          ggplot(sky, aes(run, fill = class)) + geom_density(alpha = 0.7)+theme1,
          ggplot(sky, aes(rerun, fill = class)) + geom_density(alpha = 0.7)+theme1,
          ggplot(sky, aes(camcol, fill = class)) + geom_density(alpha = 0.7)+theme2,
          ggplot(sky, aes(field, fill = class)) + geom_density(alpha = 0.7)+theme2,
          ggplot(sky, aes(specobjid, fill = class)) + geom_density(alpha = 0.7)+theme2,
          align = "h")


plot_grid(ggplot(sky, aes(redshift, fill = class)) + geom_density(alpha = 0.6)+theme1, 
          ggplot(sky, aes(plate, fill = class)) + geom_density(alpha = 0.6)+theme1,
          ggplot(sky, aes(mjd, fill = class)) + geom_density(alpha = 0.6)+theme2,
          ggplot(sky, aes(fiberid, fill = class)) + geom_density(alpha = 0.6)+theme2,
          align = "h")


p4 <- plot_ly(sky, x = sky$ra, y = sky$dec, z = sky$redshift, color = ~sky$class)  %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'x-axis'),
                      yaxis = list(title = 'y-axis'),
                      zaxis = list(title = 'z-axis')))
p4



options(repr.plot.width=12, repr.plot.height=4)
ugriz <- sky %>% gather(4:8 ,key = "spectrum", value = "Total")%>%
  group_by(ra,dec,spectrum,class) %>% 
  summarise(spec = sum(Total)) 

ggplot(ugriz, aes(x= spec, color = spectrum)) +
  geom_density()+
  facet_grid(~class)+
  theme_bw()


sky <- sky[,-c(1,10,13,18)]
sky$class <- factor(sky$class)


# splitting the data between train and test
set.seed(23106786)

indices = sample.split(sky$class, SplitRatio = 0.8)

train = sky[indices,]

test = sky[!(indices),]



#Using Linear Kernel
Model_linear <- ksvm(class~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test[,-11])

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$class)


#Using polynomial Kernel
Model_poly <- ksvm(class~ ., data = train, scale = FALSE, kernel = "polydot")
Eval_poly <- predict(Model_poly, test[,-11])

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,test$class)


#Using RBF Kernel
Model_RBF <- ksvm(class~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF <- predict(Model_RBF, test[,-11])

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$class)




trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(23106786)
grid <- expand.grid(.sigma=c(0.01,0.02,0.03,0.04), .C=c(5,6,7,8) )

fit.svm <- train(class~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)


# Plotting "fit.svm" results
plot(fit.svm)



#Evaluating on the test data

evaluate_kernel_test<- predict(fit.svm, test[, -11])
confusionMatrix(evaluate_kernel_test, test$class)