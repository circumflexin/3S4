sp.inits <- function(chain) {
  
  
  return(switch(chain,
                
                "1"=list(
                  
                  compr=0.3,
                  
                  CdAM.g=3e-06*(10^6),
                  CdAM=rep(5e-06*(10^6), n.CdAM),
                  
                  body.density.g=rep(999,n.body.density.g),
                  body.density=rep(999, n.body.density),
                  
                  
                  Vair=10,
                  Vair.d=rep(10, n.Vair),
                  Vair.SD=10
                ),
                
                
                "2"=list(
                  
                  compr=0.4,
                  
                  CdAM.g=7e-06*(10^6),
                  CdAM=rep(6e-06*(10^6), n.CdAM),
                  
                  body.density.g=rep(1010, n.body.density.g),
                  body.density=rep(1010, n.body.density),
                  
                  
                  Vair=30,
                  Vair.d=rep(30, n.Vair),
                  Vair.SD=30
                  
                ),
                
                
                "3"=list(
                  
                  compr=0.6,
                  
                  CdAM.g=10e-06*(10^6),
                  CdAM=rep(7e-06*(10^6), n.CdAM),
                  
                  body.density.g=rep(1100, n.body.density.g),
                  body.density=rep(1100, n.body.density),
                  
                  
                  Vair=50,
                  Vair.d=rep(50, n.Vair),
                  Vair.SD=50
                  
                )
                
  ))
  
}


