library(mlr)


ml = function(data.path, output.path){

    data = read.csv2(data.path)
    task = makeClassifTask(data=data, target = "Class") 

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
        keep.pred = TRUE, models = TRUE, show.info=FALSE)
    
    perf = getBMRAggrPerformances(bmr, as.df = TRUE)[,-1]
    for(i in 2:ncol(perf)){
        perf[,i] = round(perf[,i],4)
    } 

    write.csv2(x=perf, output.path, row.names=FALSE)
    save(perf, file=paste(output.path,'.models.RData',sep=''))

}

main = function(){
    line <- readLines(file("stdin"),1)
    print(line)
}


ml('../input/dataset/in.csv', '../output/ml/out.csv')