
prob = function(x){
 return (x/sum(x))
}


pca.dr = function(data.in, data.out, sdper.out){

    colsImp = read.table(data.in, sep=',', head=FALSE)

    data = read.csv('../datasets/PRAD.mirnaseq.csv')
    Class = as.character(data$Class)
    data = data[,-ncol(data)]
    data = data[, as.vector(unlist(colsImp))]
    pca <- prcomp(data)

    newdata = data.frame(pca$x)

    aux = c()
    for(i in pca$sdev){
        if(i > 0 ) aux = c(aux, i)
        else aux = c(aux, 0)
    }

    percDisp = data.frame(prob(aux))
    percDisp$comp = colnames(newdata)
    colnames(percDisp) = c('comp','PCA')

    newdata$Class = Class
    write.csv(x=format(newdata, digits=8), file=data.out, row.names=FALSE)
    write.csv(x=format(percDisp, digits=8), file=sdper.out, row.names=FALSE)

}

pca.dr.aux = function(data.in, data.out, sdper.out){

    data = read.csv(data.in)
    Class = data$Class
    data = data[,-ncol(data)]

    pca = prcomp(data)
    newdata = data.frame(pca$x)

    aux = c()
    for(i in pca$sdev){
        if(i > 0 ) aux = c(aux, i)
        else aux = c(aux, 0)
    }

    percDisp = data.frame(prob(aux))
    percDisp$comp = colnames(newdata)
    colnames(percDisp) = c('comp','PCA')


    newdata$Class = Class
    write.csv(x=format(newdata, digits=8), file=data.out, row.names=FALSE)
    write.csv(x=format(percDisp, digits=8), file=sdper.out, row.names=FALSE)

}


pca.dr('../input/dr/pca/in.csv','../output/dr/pca/data.csv', '../output/dr/pca/sdper.csv')
#pca.dr.aux('../datasets/BRCA.mirnaseq.csv','../output/dr/pca-all/BRCA.mirnaseq.csv', '../output/dr/pca-all/sdper.BRCA.mirnaseq.csv')
#pca.dr.aux('../datasets/PRAD.mirnaseq.csv','../output/dr/pca-all/PRAD.mirnaseq.csv', '../output/dr/pca-all/sdper.PRAD.mirnaseq.csv')

