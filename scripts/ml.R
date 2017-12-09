library(mlr)


ml = function(data.path, output.path){
    task = makeClassifTask(data = read.csv2('../datasets/PRAD.mirnaseq.csv'), target = "Class") 

    ## Two learners to be compared
    lrns = list(makeLearner("classif.svm"), 
        makeLearner("classif.randomForest"),
        makeLearner("classif.knn")

    )

    ## Choose the resampling strategy
    rdesc = makeResampleDesc("CV", iters = 10)

    ## Conduct the benchmark experiment
    bmr = benchmark(lrns, task, rdesc, 
        measures=list(acc, bac, f1, gmean, kappa, tpr, tnr), 
        keep.pred = TRUE, models = TRUE)


}


