
norm01 = function(x){
 return (x-min(x))/(max(x)-min(x))

}


pca.dr = function(data.in, data.out, sdper.out){

    data = read.csv(data.in)
    Class = as.character(data$Class)
    data = data[,-ncol(data)]
    pca <- prcomp(data)

    newdata = data.frame(pca$x)
    percDisp = data.frame(norm01(pca$sdev))
    percDisp$comp = colnames(newdata)
    colnames(percDisp) = c('comp','PCA')

    newdata$Class = Class
    write.csv(x=format(newdata, digits=8), file=data.out, row.names=FALSE)
    write.csv(x=format(percDisp, digits=8), file=sdper.out, row.names=FALSE)

}

pca.dr('../input/dr/pca/data.csv','../output/dr/pca/data.csv', '../output/dr/pca/sdper.csv')
pca.dr('../datasets/BRCA.mirnaseq.csv','../output/dr/pca-all/BRCA.mirnaseq.csv', '../output/dr/pca-all/sdper.BRCA.mirnaseq.csv')
pca.dr('../datasets/PRAD.mirnaseq.csv','../output/dr/pca-all/PRAD.mirnaseq.csv', '../output/dr/pca-all/sdper.PRAD.mirnaseq.csv')

