moveRed.fast = function(grid, r, c){
  #red is 1
  oriClass = class(grid)
  g = t(grid)
  carPos = which(g==1)
  goingPos = carPos+1
  pos = goingPos%%c==1
  goingPos[pos]=goingPos[pos]-c
  ifEmpty = g[goingPos]==0
  g[carPos[ifEmpty]] = 0
  g[goingPos[ifEmpty]] = 1
  grid = t(g)
  class(grid) = oriClass
  list(grid=grid, velocity=length(carPos[ifEmpty]))
}

moveBlue.fast = function(grid, r ,c){
  #blue is 2
  oriClass = class(grid)
  carPos = which(grid==2)
  goingPos = carPos-1
  pos = goingPos%%r==0
  goingPos[pos]=goingPos[pos]+r
  ifEmpty = grid[goingPos]==0
  grid[carPos[ifEmpty]] = 0
  grid[goingPos[ifEmpty]] = 2
  class(grid) = oriClass
  list(grid=grid, velocity=length(carPos[ifEmpty]))
}

moveRed.slow = function(grid, r, c){
  #red is 1
  oriClass = class(grid)
  count = 0
  for(lr in 1:r){
    carPos = which(grid[lr, ]==1)
    goingPos = (carPos+1)
    goingPos[goingPos>c]=1
    ifEmpty = grid[lr, goingPos]==0
    grid[lr, carPos[ifEmpty]] = 0
    grid[lr, goingPos[ifEmpty]] = 1
    count = count + length(carPos[ifEmpty])
  }
  class(grid) = oriClass
  list(grid=grid, velocity=count)
}

moveBlue.slow = function(grid, r, c){
  #blue is 2
  oriClass = class(grid)
  count = 0
  for(lc in 1:c){
    carPos = which(grid[ , lc]==2)
    goingPos = (carPos-1)
    goingPos[goingPos<1]=r
    ifEmpty = grid[goingPos, lc]==0
    grid[carPos[ifEmpty], lc] = 0
    grid[goingPos[ifEmpty], lc] = 2
    count = count + length(carPos[ifEmpty])
  }
  class(grid) = oriClass
  list(grid=grid, velocity=count)
}

moveRed.simul = function(grid, r, c){
  #red is 1
  oriClass = class(grid)
  count = 0
  r = nrow(grid)
  c = ncol(grid)
  for(lr in 1:r){
    s = tail(which(grid[lr, ]==0),1) #most right empty
    if(length(s)){
      if(s==1)
        index=c:1
      else
        index = c((s-1):1, c:s) #backwards ex.5 4 3 2 1 6
      
      if(grid[lr, index[1]]==1){
        grid[lr, s] = 1
        grid[lr, index[1]] = 0
        count = count+1
      }
      for(lc in 2:(length(index)-1)){
        if(grid[lr, index[lc]]==1 & grid[lr, index[lc-1]]==0){
          grid[lr, index[lc-1]] = 1
          grid[lr, index[lc]] = 0
          count = count+1
        }   
      }
    }
  }
  class(grid) = oriClass
  list(grid=grid, velocity=count)
}

moveBlue.simul = function(grid, r, c){
  #blue is 1
  oriClass = class(grid)
  count = 0
  r = nrow(grid)
  c = ncol(grid)
  for(lc in 1:c){
    s = head(which(grid[ ,lc]==0),1) #most top empty
    if(length(s)){
      if(s==r)
        index=c(1:r)
      else
        index = c((s+1):r, 1:s) #forwards ex.3 4 5 1 2
      
      
      if(grid[index[1], lc]==2){
        grid[s, lc] = 2
        grid[index[1], lc] = 0
        count = count+1
      }
      for(lr in 2:(length(index)-1)){
        if(grid[index[lr], lc]==2 & grid[index[lr-1], lc]==0){
          grid[index[lr-1], lc] = 2
          grid[index[lr], lc] = 0
          count = count+1
        }   
      } 
    }   
  }
  class(grid) = oriClass
  list(grid=grid, velocity=count)
}
