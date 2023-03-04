#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector pixelC(NumericMatrix cluster_temp_data, NumericMatrix temp, int cluster_no) {       // Defining the cpp function named sumsC that takes a square matrix x and a random number s from 1 to total number of columns of x as input
  
  NumericVector index(temp.nrow());
  for(int j = 0; j < temp.nrow(); j++){
    NumericVector ind(cluster_no);                    // defining a numeric vector sum_cols of size s
    
    for(int i = 0; i < cluster_no; i++){
      float add = 0;
      for(int k = 0; k < 3; k++){
        add = add + pow(temp(j,k) - cluster_temp_data(i,k),2); // computing euclidean distance between
        // the pixel and different cluster centers
      }
      ind[i] = sqrt(add);
    }
    
    index[j] = which_min(ind);
    
  }
  
  
  // returning the index of the pixels belonging to different clusters;
  return index;
}




// [[Rcpp::export]]
NumericMatrix pixel_C(NumericMatrix updated_cluster1, NumericVector index_col1, NumericVector col_proportion1,NumericMatrix temp, int no_cluster) {       // Defining the cpp function named sumsC that takes a square matrix x and a random number s from 1 to total number of columns of x as input
  
  int checking = 1;
  while(checking != 0){
    
    checking = 0;
    NumericVector index_col2(temp.nrow());
   
    index_col2 = pixelC(updated_cluster1, temp, no_cluster);
       
    NumericVector col_proportion2(no_cluster);
    NumericMatrix updated_cluster2(no_cluster, 3);
    
    for(int i = 0; i < no_cluster; i++){

        float sum1 = 0;
        float sum2 = 0;
        float sum3 = 0;
        float cluster_count = 0;
        
        for(int j = 0; j < temp.nrow(); j++){
          if(index_col2[j] == i){
            sum1 = sum1 + temp(j,0);
            sum2 = sum2 + temp(j,1);
            sum3 = sum3 + temp(j,2);
            cluster_count = cluster_count + 1;
          }
        }
        col_proportion2[i] = cluster_count/temp.nrow();
        updated_cluster2(i,0) = sum1/cluster_count;
        updated_cluster2(i,1) = sum2/cluster_count;
        updated_cluster2(i,2) = sum3/cluster_count;
       
    }
    
    for(int i = 0; i < temp.nrow(); i++){
      if(index_col2[i] == index_col1[i]){
        checking = checking + 0;
      }
      else{
        checking = checking + 1;
      }
    }
    
    index_col1 = index_col2;
    updated_cluster1 = updated_cluster2;
    col_proportion1 = col_proportion2;
  }
  
  NumericMatrix return_mat(no_cluster,4);
  for(int i = 0; i < no_cluster; i++){
    for(int j = 0; j < 3; j++){
      return_mat(i,j) = updated_cluster1(i,j);
    }
    return_mat(i,3) = col_proportion1[i];
  }
  return return_mat;
}






// [[Rcpp::export]]
NumericMatrix red_mat(NumericMatrix individual_img_data, NumericVector iter,int no_cluster){
  NumericMatrix red_matrix(no_cluster*100, 1200);
  for(int x = 0; x < no_cluster; x++){
    for(int i = 0; i < 1200; i++){
      for(int j = (iter[x]-1); j < (iter[x+1]-1); j++){
        
        red_matrix(j,i) = individual_img_data(x,0);
        
      }
    }
  }
  return red_matrix;
}


// [[Rcpp::export]]
NumericMatrix green_mat(NumericMatrix individual_img_data, NumericVector iter,int no_cluster){
  NumericMatrix green_matrix(no_cluster*100, 1200);
  for(int x = 0; x < no_cluster; x++){
    for(int i = 0; i < 1200; i++){
      for(int j = (iter[x]-1); j < (iter[x+1]-1); j++){
        
        green_matrix(j,i) = individual_img_data(x,1);
        
      }
    }
  }
  return green_matrix;
}



// [[Rcpp::export]]
NumericMatrix blue_mat(NumericMatrix individual_img_data, NumericVector iter,int no_cluster){
  NumericMatrix blue_matrix(no_cluster*100, 1200);
  for(int x = 0; x < no_cluster; x++){
    for(int i = 0; i < 1200; i++){
      for(int j = (iter[x]-1); j < (iter[x+1]-1); j++){
        
        blue_matrix(j,i) = individual_img_data(x,2);
        
      }
    }
  }
  return blue_matrix;
}



