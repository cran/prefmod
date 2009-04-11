llbt.worth <- function(obj, obj.names=NULL, outmat="worth"){

   #if(!(class(obj)[1] %in% c("glm","gnm"))) stop
   if(!("llbtMod" %in% class(obj)))
        stop("function only for objects of class llbtMod (see help for llbtPC.fit)")

   nobj <- obj$envList$nobj

   mtype <- ifelse(is.null(obj$ofInterest),"glm","gnm")


   if(mtype == "gnm"){
       lambda <- obj$coefficients[obj$ofInterest]
       if(any(grep("^g[0-9]|g[0-9]$|^u$",names(lambda))))
           lambda <- lambda[-(grep("^g[0-9]|^u$", names(lambda)))]
       lambda <- ifelse(is.na(lambda),0,lambda)

       npar<-length(lambda)
       lambda.mat <- matrix(, nrow=nobj, ncol=npar/nobj)

       # row and colnames for output matrix
       nam.lambda <- names(lambda)
       rownames(lambda.mat) <- nam.lambda[1:nobj]
       nam.new<-nam.lambda
       for (i  in nam.lambda[1:nobj])
#           nam.new<-gsub(paste("^",i,"$|^",i,"[^0-9]:+|[:alnum:]*:?(",i,"[^0-9]{0,2})$",sep=""),"",nam.new)
           nam.new<-gsub(paste("^",i,"$|^",i,":",sep=""),"",nam.new)
       nn<-nam.new
       nl<-nam.lambda
       colnames(lambda.mat) <- unique(nam.new)

       # fill lambda matrix with correct entries
       npar<-length(lambda)

       # case with covs
       if(npar>nobj){
           lambda.mat <- matrix(, nrow=nobj, ncol=npar/nobj)

           # reference group into first column
           lambda.mat[,1] <- lambda[1:nobj]

           # others according to their labels
           lambda.rest<-lambda[-(1:nobj)]
           nn2<-nn[-(1:nobj)]
           lambda.rest<-lambda.rest[order(nn2)]
           lambda.mat.rest<-matrix(lambda.rest,nrow=nobj)
           lambda.mat[,2:ncol(lambda.mat)]<-lambda.mat.rest

       colnames(lambda.mat) <- unique(nam.new)
       rownames(lambda.mat) <- nam.lambda[1:nobj]

       ## calculate sums accordig to covariates
       lambda.vec <- as.vector(lambda.mat)
       covmat <- unique(obj$envList$covdesmat)
       lambda.groups.mat <- matrix((covmat %x% diag(nobj)) %*% lambda.vec, nrow=nobj)


       ## labels for cov groups
       covlevels <- obj$envList$covlevels
       ncovs<-length(covlevels)
       grid.groups<-vector("list",ncovs)
       for (i in 1:ncovs)
         grid.groups[[i]]<-1:covlevels[i]
       gr<- expand.grid(grid.groups)
       gr2<-gr
       if(all(covlevels==1)){
            gr.labels<-""
       } else {
            for (i in 1:ncovs)
                gr2[i]<-paste(names(covlevels)[i],gr[[i]],sep="")
            gr.labels<-apply(as.matrix(gr2),1,function(x) paste(x,collapse=":"))
       }
       colnames(lambda.groups.mat) <- gr.labels
       if (is.null(obj.names))
          rownames(lambda.groups.mat) <- nam.lambda[1:nobj]
       else
          rownames(lambda.groups.mat) <- obj.names

       ## worth matrix
       worth.groups.mat <- apply(lambda.groups.mat, 2, function(x) exp(2*x)/sum(exp(2*x)))

       # case w/o covs
       } else {
       lambda.mat <- matrix(lambda, nrow = nobj, dimnames = list(names(lambda)[1:nobj], "estimate"))
       lambda.groups.mat <- lambda.mat
       worth.groups.mat <- apply(lambda.groups.mat, 2, function(x) exp(2*x)/sum(exp(2*x)))
       }

   } else if(mtype == "glm"){
   } else {
   }

   switch(outmat,
      "lambda" = return(lambda.groups.mat),
      "worth" = return(worth.groups.mat),
      "est" = return(lambda.mat),
      stop("     outmat must be either 'worth' or 'lambda'\n")
   )
   #list(lam=lambda.mat, lamsum=lambda.groups.mat, w=worth.groups.mat)
}
