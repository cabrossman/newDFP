targeting_custom <- function(criterias, boolString){
  
  
  
  #get rid of any null values passed in custom criteria list
  nonnull_criterias <- list()
  for(i in 1:length(criterias)){
    if(!is.null(criterias[[i]])){
      nonnull_criterias[[names(criterias)[i]]] <- criterias[[i]]
    }
  }
  criterias <- nonnull_criterias
  
  if (length(criterias)>0){
    #parse how to handle boolean string
    # None - All "AND" criteria
    # make-OR-class
    # make-OR-type
    # class-OR-type
    # make-OR-class-OR-type
    
    #create DF with lookup V
    #boolString <- 'make-OR-class'
    boolDF <-cbind.data.frame(
                              #k = c('all AND','make-OR-class, rest=AND','make-OR-type, rest=AND','class-OR-type, rest=AND','make-OR-class-OR-type, rest=AND'),
                              k = c('all AND','make-OR-class','make-OR-type','class-OR-type','make-OR-class-OR-type'),
                              v = c('','589479,590679','589479,591879','590679,591879','589479,590679,591879'))
    
    #get list of keys which are dynamic vs static
    boolSelection <- boolDF %>% filter(k == boolString)
    keySplits <- unlist(strsplit(boolSelection$v,','))
    
    #split criterias list into static - which are applied to all children and dynamic which is only applied one per child
    static <- list()
    dynamic <-list()
    staticNames <- c()
    dynamicNames <- c()
    for(q in 1:length(criterias)){
      if(names(criterias)[q] %in% keySplits){
        dynamic[[length(dynamic)+1]] <- criterias[[q]]
        dynamicNames <- c(dynamicNames, names(criterias)[q])
      } else{
        static[[length(static)+1]] <- criterias[[q]]
        staticNames <- c(staticNames, names(criterias)[q])
      }
    }
    names(static) <- staticNames
    names(dynamic) <- dynamicNames
    
    
    #start building list
    if(length(dynamic) < 2){
      #this creates a grid per every combination of criteria passed with names in columns
      # we need to filter the criterias before passing here!
      indiv_possibilities <- expand.grid(criterias, stringsAsFactors = F)
      #this creates an empty list (level-1) with needed nesting
      ct_object <- vector("list", nrow(indiv_possibilities)+1)
      #assign OR to top level
      ct_object[[1]] <- 'OR'
      
      #for every combination of criteria loop
      for(i in 1:nrow(indiv_possibilities)){
        #create level two list which has the number of children plus a logical operator, and .attrs
        #number of children should equal number of keys
        one_ct_object <- vector("list", ncol(indiv_possibilities)+2)
        #assign AND as the criteria
        one_ct_object[[1]] <- 'AND'
        #assign customer criteria
        one_ct_object[[length(one_ct_object)]] <- c('type'="CustomCriteriaSet")
        
        #assign keys names to level 2 list
        names(one_ct_object) <- c('logicalOperator', rep('children', ncol(indiv_possibilities)), '.attrs')
        
        #loop into key values need one for each possibility
        for(j in 1:ncol(indiv_possibilities)){
          one_ct_object[[j+1]] <- list(keyId=names(indiv_possibilities)[j], 
                                       valueIds=indiv_possibilities[i,j],
                                       operator='IS',
                                       `.attrs`=c('type'="CustomCriteria"))
        }
        ct_object[[i+1]] <- one_ct_object
      }
      names(ct_object) <- c('logicalOperator', rep('children', nrow(indiv_possibilities)))
      #final$customTargeting <- ct_object
    } else{
      
      #create outer list. Number of children = number of dynamic
      ct_object <- vector("list", length(dynamic)+1)
      #assign OR to top level
      ct_object[[1]] <- 'OR'
      
      for(d in 1:length(dynamic)){
        #create new list with all static and one of the dynamic
        temp <- static
        temp[[length(temp)+1]] <- dynamic[[d]]
        names(temp)[length(temp)] <- names(dynamic)[d]
        
        #create level two list which are all and
        one_ct_object <- vector("list", length(temp)+2)
        one_ct_object[[1]] <- 'AND'
        one_ct_object[[length(one_ct_object)]] <- c('type'="CustomCriteriaSet")
        names(one_ct_object) <- c('logicalOperator', rep('children', length(temp)), '.attrs')
        
        #loop into level 3
        for(i in 1:length(temp)){
          oneSubObject <- vector("list", length(temp[[i]])+3)
          oneSubObject[[1]] <- names(temp)[i]
          oneSubObject[[length(oneSubObject) - 1]] <- 'IS'
          oneSubObject[[length(oneSubObject)]] <- c('type'="CustomCriteria")
          
          #loop to assign each of the value ids
          for(j in 1:length(temp[[i]])){
            oneSubObject[[j+1]] <-  temp[[i]][[j]]
          }
          #name subobjects
          names(oneSubObject) <- c('keyId',rep('valueIds', length(temp[[i]])), 'operator','.attrs')
          one_ct_object[[i+1]] <- oneSubObject
          
        }
        ct_object[[d+1]] <- one_ct_object
        
      }
      names(ct_object) <- c('logicalOperator', rep('children', length(dynamic)))
      #final$customTargeting <- ct_object
    }
  }
  return(ct_object)
}
  