library(mlr)

norm01 = function(x){
    return (normalized = (x-min(x))/(max(x)-min(x)))
}

dataclean = function(){
    dir = '../datasets/'
    files = list.files(dir, "*.csv")
    files.path = paste(dir,files,sep='')


    for (f in files.path){

        df = read.csv2(f)
        colsn = paste('rna.',1:ncol(df),sep='')
        colsn[length(colsn)] = 'Class'
        colnames(df) = colsn

        normal = which(df$Class == 'NT')
        cancer = which(df$Class == 'TP')

        df = df[c(sample(normal,50), sample(cancer,50)),]
        df$Class = ifelse(df$Class == 'NT','normal', 'cancer' )

        write.csv(x=df, file=f, row.names=FALSE)

    }


}

filttelSelection = function(){
    dir = '../datasets/'
    files = list.files(dir, "*.csv")
    files.path = paste(dir,files,sep='')

    i = 0
    for (f in files.path){
        i = 1+i

        df = read.csv(f)

        #df = iris
        colnames(df)[ncol(df)] = 'Class'

        ## Define the task
        ## Specify the type of analysis (e.g. classification) and provide data and response variable
        task = makeClassifTask(data = df, target = "Class")

        #filter step
        fv2 = generateFilterValuesData(task, method = c("information.gain", "chi.squared", "relief", "randomForest.importance"))
        name = fv2$data$name
        fv2 = fv2$data[,c(-1,-2)]
        mt = data.frame(apply(fv2, 2, norm01))
        mt$name = name

        filters = colnames(mt)[-ncol(mt)]

        for (fil in filters){
            aux = mt[,c('name',fil)]
            aux = aux[order(aux[,2],decreasing=TRUE),]
            colnames(aux) = c('names','filter')

            write.csv(x=aux, file =paste('../output/filtters', fil, files[i], sep='/'), row.names=FALSE)
        }

    }


}

#gerOrderDatasets = function(){
#    filtters = c('chi.squared', 'information.gain', 'randomForest.importance', 'relief')
    #filtter.path = list.files('../output/filtters/')
#
#
#    i = 0
#    for(fil in filtters){
#        filtter.path = list.files(paste('../output/filtters/', fil, sep=''), '*.csv')
#
#        for (f in fill.path){
#            df.fil = read.csv2(paste('../output/filtters/', f, sep=''))
#            df = read.csv2(paste('../datasets/', f, sep=''))
#
#            df = df[,c(df.fill[,2], 'Class')]
#
#            write.csv2(x=df, file=paste('../output/orderData/',fil, files[i],sep=''), row.names=FALSE)
#        }
#
#    }
#}


filttelSelection()
#gerOrderDatasets()




