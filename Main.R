#reading from the csv file
install.packages("tree")
install.packages("caTools")
install.packages("ROCR")
install.packages("shiny")
library(tree)
library(caTools)
library(ROCR)
library(shiny)

hr = read.csv('HR.csv')
str(hr)
hr$left = as.factor(hr$left)
#setting the seed and splitting the data set into 2 as the validation set approach says
set.seed(490)
split = sample.split(hr$left, SplitRatio = 0.7)
hrtr = subset(hr, split == TRUE)
hrte = subset(hr, split == FALSE)
#forming the largest tree with the training data
#TREE1
hrtree = tree(left ~ ., data = hrtr, mindev = 0.0, mincut = 1, minsize = 2)
summary(hrtree)
#making predictions on test data
predhr = predict(hrtree, newdata = hrte, type = "class")
table(hrte$left, predhr)
tbl = table(hrte$left, predhr)
100 * (1 - sum(diag(tbl)) / sum(tbl))

#TREE2: smaller tree, training performance is low(residual mean deviance and error rate are higher)
#test performance is worse than first one acc to gini index.  
hrtree2 = tree(left ~ ., data = hrtr, mindev = 0.0, mincut = 2, minsize = 4)
summary(hrtree2)
predhr2 = predict(hrtree2, newdata = hrte, type = "class")

table(hrte$left, predhr2)
tbl2 = table(hrte$left, predhr2)
100 * (1 - sum(diag(tbl2)) / sum(tbl2))

#TREE3: error rates increase, gini index is worse.
hrtree3 = tree(left ~ ., data = hrtr, mindev = 0.0, mincut = 3, minsize = 6)
summary(hrtree3)
predhr3 = predict(hrtree3, newdata = hrte, type = "class")

table(hrte$left, predhr3)
tbl3 = table(hrte$left, predhr3)
100 * (1 - sum(diag(tbl3)) / sum(tbl3))

#TREE4:gini index is higher, training and test performances are worse error rates are increased
hrtree4 = tree(left ~ ., data = hrtr, mindev = 0.0, mincut = 4, minsize = 8)
summary(hrtree4)
predhr4 = predict(hrtree4, newdata = hrte, type = "class")

table(hrte$left, predhr4)
tbl4 = table(hrte$left, predhr4)
100 * (1 - sum(diag(tbl4)) / sum(tbl4))

#TREE5: while mindev is 0, increasing the mincut and the minsize is lowering the performance
hrtree5 = tree(left ~ ., data = hrtr, mindev = 0.0, mincut = 5, minsize = 10)
summary(hrtree5)
predhr5 = predict(hrtree5, newdata = hrte, type = "class")

table(hrte$left, predhr5)
tbl5 = table(hrte$left, predhr5)
100 * (1 - sum(diag(tbl5)) / sum(tbl5))

#TREE 6: Increasing the mindev, increases the performance
gini = 0
as.array(gini)
for (i in 2:50) {
    hrtree6 = tree(left ~ ., data = hrtr, mindev = 0.001, mincut = i)
    summary(hrtree6)
    predhr6 = predict(hrtree6, newdata = hrte, type = "class")

    table(hrte$left, predhr6)
    tbl6 = table(hrte$left, predhr6)
    gini[i - 1] = 100 * (1 - sum(diag(tbl6)) / sum(tbl6))
}
which.min(gini) #i 2-5 gives the same gini index and that is the minimum one.

#TREE 7: While one error rate is decreased in this tree(0s), 1s error rate increased.
hrtree7 = tree(left ~ ., data = hrtr, mindev = 0.002, mincut = 5, minsize = 10)
summary(hrtree7)
predhr7 = predict(hrtree7, newdata = hrte, type = "class")

table(hrte$left, predhr7)
tbl7 = table(hrte$left, predhr7)
100 * (1 - sum(diag(tbl7)) / sum(tbl7))



#after prepruning the tree, I found the TREE6 is the best, 
#but I want to do the postpruning to be sure, and I will select the best number of nodes according to auc

#TREE6 was:

hrtree6 = tree(left ~ ., data = hrtr, mindev = 0.001, mincut = 5)
summary(hrtree6)
predhr6 = predict(hrtree6, newdata = hrte)
predhr6ROC = prediction(predhr6[, 2], hrte$left)
perf6 = performance(predhr6ROC, "tpr", "fpr")
plot(perf6, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
as.numeric(performance(predhr6ROC, "auc")@y.values)
#Performance according to auc 0.9791666
#Can we do it better? Let's try post pruning and increasing the performance acc to ROCR

#largest tree possible
hrtree = tree(left ~ ., data = hrtr, mindev = 0.0, mincut = 2)
predhr2 = predict(hrtree, newdata = hrte)
predhrROC = prediction(predhr2[, 2], hrte$left)
perf = performance(predhrROC, "tpr", "fpr")
perf
plot(perf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
as.numeric(performance(predhrROC, "auc")@y.values)

auc = 0
as.array(auc)
for (i in 2:41) {
    hrcut = prune.tree(hrtree, best = i)
    predhr2 = predict(hrcut, newdata = hrte)
    predhrROC = prediction(predhr2[, 2], hrte$left)
    auc[i] = as.numeric(performance(predhrROC, "auc")@y.values)
}
which.max(auc)
hrcut = prune.tree(hrtree, best = which.max(auc))
hrcut
plot(hrcut)
summary(hrcut)
predhr2 = predict(hrcut, newdata = hrte)
predhrROC = prediction(predhr2[, 2], hrte$left)
perf = performance(predhrROC, "tpr", "fpr")
plot(perf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
as.numeric(performance(predhrROC, "auc")@y.values)

#best tree according to auc performance on test data is the one with 34 terminal nodes.
#since it has the value of auc nearest to 1.
#this is better than tree6
#34 terminal nodes
#summary of it
summary(hrcut)
#in training set error rate is 0.01857, 1.857%. and accuracy is 100-1.857=98.2%
#in test set, auc is 0.9821298, it is very close to 1.
#let's look at confusion matrix
predhr2 = predict(hrcut, newdata = hrte, type = "class")
table(hrte$left, predhr2)
tbl2 = table(hrte$left, predhr2)
100 * (1 - sum(diag(tbl2)) / sum(tbl2))
#gini index is also lower than the 2.65 which is the index of tree6.
#in the confusion matrix 19+96 instance guessed wrongly.96/(975+96) false positive rate.
#19/3409+19 false negative rate

#which input attributes are important? 
#satisfaction level,number_project,last_evaluation,average_monthly_hours,
#time_spend_company, sales
server <- function(input, output, session) {

    output$my_output_text <- renderText({
        init <- "Your value is: "
        return(paste0(init, input$mytext))
    })

    # send plot to the ui as my_output_plot
    output$my_output_plot <- renderPlot({
        plot(perf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
    })


}

ui <- basicPage(

  h3("ROC Curve of the model we have built for employee quit prediction"),
  
# my_output_plot comes from the server
  plotOutput("my_output_plot")

  )

shinyApp(ui = ui, server = server)
