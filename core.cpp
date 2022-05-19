#include <Rcpp.h>
using namespace Rcpp;
#define statusNoFireRegime 1
#define statusTooFrequentlyBurnt 2
#define statusVulnerable 3
#define statusLongUnburnt 4
#define statusUnknown 9
#define statusWithinThreshold 5

#define is_WithinThreshold 1
#define is_Vulnerable 2
#define is_TooFreq 3

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
IntegerVector differ(IntegerVector inp) {
  int len = inp.length();
  int l1 = len-1;
  IntegerVector result(l1);
  for(int i=0; i<l1;i++){
    result[i] = inp[i+1]-inp[i];
  }
  return result;
}


// [[Rcpp::export]]
int CalcStatus(int MaxThresh,
               int MinThresh,
               int FireFrequency,
               int TSFF,
               IntegerVector int_list,
               IntegerVector IntervalList,
               int TSF) {
  
  //int statusNoFireRegime=1;
  
  
  int Status = 0;
  int IntervalStatus = 0;
  
  int overburnt = 0;
  
  /// Make int list
  
  if(MaxThresh == 0 & MinThresh == 0){
    Status = statusNoFireRegime;
    return Status;
  }else{
    if(Rcpp::IntegerVector::is_na(FireFrequency)){
      FireFrequency = 0;
    }
    
    if(MaxThresh == 9999 & MinThresh == 9999){
      
      
      if(FireFrequency > 0){
        Status = statusTooFrequentlyBurnt;
        return Status;
      }else{
        Status = statusVulnerable;
        return Status; 
      }
    }else{
      
      if(FireFrequency == 0){
        
        if(TSFF > MaxThresh){
          Status = statusLongUnburnt;
          return Status; 
        }else{
          Status = statusUnknown;
          return Status;
        }
      }
    }
  }
  
  
  if(Status == 0){
    
    // Length of list of years
    int il_len = int_list.length();
    
    // Count how many have fire
    int n_int = 0;
    
    for(int i=0; i<il_len; i++){
      n_int = n_int + IntervalList[i];
    }
    
    // if n_int is still zero then we just need to skip all this
    if(n_int >0){
      IntegerVector fint(n_int);
      
      int place=0;
      
      for(int i=0; i<il_len; i++){
        if(IntervalList[i]==1){
          fint[place]=int_list[i];
          place++;
        }
      }
      
      fint = differ(fint);
      IntervalStatus = is_WithinThreshold;
      
      overburnt = 0;
      int this_int = 0;
      
      for(int i=0; i<n_int; i++){
        this_int = fint[i];
        if(this_int < MinThresh){
          
          if(IntervalStatus == is_WithinThreshold){
            IntervalStatus = is_Vulnerable;
            continue;
          }else{
            IntervalStatus = is_TooFreq;
            overburnt = overburnt + 1;
            continue;
          }
        }else if(this_int > 2 * MinThresh){
          IntervalStatus = is_WithinThreshold;
          continue;
        }else if(IntervalStatus == is_WithinThreshold | IntervalStatus == is_Vulnerable){////////
          IntervalStatus = is_WithinThreshold;
        }
        
        
      }
    }
  } else{
    IntervalStatus = is_WithinThreshold;
  }
  
  
  if(Rcpp::IntegerVector::is_na(TSF)){TSF = TSFF;}
  if(IntervalStatus == is_TooFreq){
    if(TSF > 2 * MinThresh){
      Status = statusWithinThreshold;
      return Status;
    } else {
      Status = statusTooFrequentlyBurnt;
      return Status;
    }
  }else if(TSF < MinThresh){
    Status = statusVulnerable; 
    return Status;
  }else if(TSF > MaxThresh){
    Status = statusLongUnburnt;
    return Status;
  } else{
    Status = statusWithinThreshold;
    return Status;
  }
  if(Status==0){
    Status = statusUnknown;
  }
  return Status;
}
