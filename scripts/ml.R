library(mlr)


ml = function(in.path, out.pca.path, out.filter.path){

    colsImp = read.table(in.path, sep=',', head=FALSE)
    data = read.csv('../datasets/PRAD.mirnaseq.csv')
    data = data[, c(as.vector(unlist(colsImp)), 'Class')]

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

    write.csv(x=perf, out.filter.path, row.names=FALSE)
    save(perf, file=paste(out.filter.path,'.models.RData',sep=''))


    ##################### PCA ##############################
    #data = read.csv('../output/dr/pca-all/PRAD.mirnaseq.csv')
    #comps = read.csv('../output/dr/pca-all/sdper.PRAD.mirnaseq.csv')[,1]
    # 
    #sum = 0
    #for (i in 1:length(comps)){
    #    sum = sum + comps[i]
    #    if(sum >= 0.9) break
    #}
    #
    #data = data[,c(1:i, ncol(data))]
    #
    #task = makeClassifTask(data=data, target = "Class") 
    #
    ## Two learners to be compared
    #lrns = list(makeLearner("classif.svm"), 
    #    makeLearner("classif.randomForest"),
    #    makeLearner("classif.knn")
    #
    #)
    #
    ## Choose the resampling strategy
    #rdesc = makeResampleDesc("CV", iters = 10)
    #
    ## Conduct the benchmark experiment
    #bmr = benchmark(lrns, task, rdesc, 
    #    measures=list(acc, bac, f1, gmean, kappa, tpr, tnr), 
    #    keep.pred = TRUE, models = TRUE, show.info=FALSE)
    #
    #perf = getBMRAggrPerformances(bmr, as.df = TRUE)[,-1]
    #for(i in 2:ncol(perf)){
    #    perf[,i] = round(perf[,i],4)
    #} 
    #
    #write.csv(x=perf, out.pca.path, row.names=FALSE)
    #save(perf, file=paste(out.pca.path,'.models.RData',sep=''))




}

main = function(){
    line <- readLines(file("stdin"),1)
    print(line)
}


ml('../input/dr/pca/in.csv', '../output/ml/filter/out-pca.csv', '../output/ml/filter/out-filter.csv')
