void cmoveBlue1(int *mat, int*nRows, int*nCols)
{
  int total = (*nRows)*(*nCols);
  int omat[total];
  for(int i=0; i<total; i++)
  {
    omat[i]=mat[i];
  }

  for(int r=0; r<(*nRows); r++)
  {
    for(int c=0; c<(*nCols); c++)
    {
      if(omat[c*(*nRows)+r]==2)
      {
        if(r==0)
        {
          if(omat[(c+1)*(*nRows)-1]==0)
          {
            mat[(c+1)*(*nRows)-1]=2;
            mat[c*(*nRows)+r]=0;
          }
        }
        else
        {
          if(omat[c*(*nRows)+r-1]==0)
          {
            mat[c*(*nRows)+r-1]=2;
            mat[c*(*nRows)+r]=0;
          }
        }
      }
    }
  }
}

void cmoveRed1(int *mat, int *nRows, int *nCols)
{
  int total = (*nRows) * (*nCols);
  int omat[total];
  for(int i=0; i<total; i++)
  {
    omat[i]=mat[i];
  }  
  for(int r=0; r<(*nRows); r++)
  {
    for(int c=0; c<(*nCols); c++)
    {
      if(omat[c*(*nRows)+r]==1)
      {
        if((c+1)%(*nCols)==0)
        {
          if(omat[r]==0)
          {
            mat[r]=1;
            mat[c*(*nRows)+r]=0;
          }
        }
        else
        {
          if(omat[(c+1)*(*nRows)+r]==0)
          {
            mat[(c+1)*(*nRows)+r]=1;
            mat[c*(*nRows)+r]=0;
          }
        }
      }
    }
  }
}



void crun1(int *mat, int *nRows, int *nCols, int *T)
{
  if((*T)%2==0)
  {
    for(int i=0; i<((*T)/2); i++)
    {
      cmoveBlue1(mat, nRows, nCols);
      cmoveRed1(mat, nRows, nCols);
    }
  }
  else
  {
    for(int i=0; i<((*T)/2); i++)
    {
      cmoveBlue1(mat, nRows, nCols);
      cmoveRed1(mat, nRows, nCols);
    }
    cmoveBlue1(mat, nRows, nCols);
  }   
}


