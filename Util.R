
# Filter based on Median Absolute Deviation
applyMadding <- function(x, constant = 5.6)
{
  medianX = median(x, na.rm=TRUE)
  madX = mad(medianX, constant=constant, na.rm=TRUE)
  filteredX = pmin(x, medianX + madX)
  filteredX = pmax(filteredX, medianX - madX)
  return(filteredX)
}

colVars <- function(x, na.rm = na.rm) {
  unlist(apply(x, MARGIN = 2, FUN = var, na.rm = na.rm))
}

runPCA <- function(retMatrix, cutOff = 0.01) {
  
  returns = t(apply(retMatrix, 1, applyMadding))
  n = dimnames(returns)[[2]]
  nCountNA = apply(is.na(retMatrix), 2, sum)
  nOK = n[nCountNA == 0]
  nNA = n[nCountNA > 0]
  
  modPC = princomp(x = retMatrix[, nOK, drop = F])
  
  mPCVar = modPC$sdev^2
  
  nPC = sum(mPCVar[1:min(length(mPCVar), 100)]/sum(mPCVar, na.rm = T) >= cutOff, na.rm = T)
  
  ldgPC = matrix(NA, length(n), nPC)
  dimnames(ldgPC) = list(n, paste("comp", 1:nPC, sep = "."))
  ldgPC[nOK,  ] = modPC$loadings[, seq(nPC), drop = F]
  
  retPC = retMatrix[, nOK, drop = F] %*% ldgPC[nOK,  ]
  
  if(length(nNA) == 1) {
    nCount = sum(!is.na(retMatrix[, nNA]))
    if(nCount > dim(retPC)[2]) {
      
      ldgPC[nNA,  ] = sapply(nNA, function(n, rpc, rs) {
        
        I = is.finite(rs)
        qr.coef(qr(rpc[I,  ]), rs[I])
      }, rpc = retPC, rs = retMatrix[, nNA])
    }
  }
  
  if(length(nNA) > 1) {
    nCount = apply(!is.na(retMatrix[, nNA]), 2, sum)
    nNA = nNA[nCount > dim(retPC)[2]]
    
    if(length(nNA) > 1) {
      ldgPC[nNA,  ] = t(sapply(nNA, function(n, rpc, rs)
      {
        I = is.finite(rs[, n])
        qr.coef(qr(rpc[I,  ]), rs[I, n])
      }, rpc = retPC, rs = retMatrix[, nNA]))
    }
  }
  
  varPC = var(retPC)
  
  varResid = applyMadding(colVars(retMatrix - retPC %*% t(ldgPC), na.rm = T))
  if(sum(is.na(varResid)) > 0)
    varResid[is.na(varResid)] = median(varResid, na.rm = T)
  
  VCV = ldgPC %*% varPC %*% t(ldgPC) + diag(varResid)
  dimnames(VCV) = list(dimnames(retMatrix)[[2]], dimnames(retMatrix)[[2]])
  
  list(loadings = ldgPC, factor.covariance = varPC, varResid = varResid, VCV = VCV)
}
