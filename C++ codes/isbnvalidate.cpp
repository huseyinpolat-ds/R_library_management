#include <Rcpp.h>
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
bool isbnvalidate(string isbn) {
  
  isbn.erase(std::remove_if(isbn.begin(), isbn.end(), [](char c) {
    return !std::isalnum(c);
  }), isbn.end());
  
  size_t xIdx = isbn.find('X');
  if (xIdx != std::string::npos) {
    isbn[xIdx] = '0'; // Replace 'X' with '0' to calculate the sum
  }
  
  if (std::any_of(isbn.begin(), isbn.end(), [](char c) {
    return !std::isdigit(c);
  })) {
    throw std::invalid_argument("Error: ISBN code can only contain digits and character 'X'.");
  }
  
  
  if (isbn.length() != 10 && isbn.length() != 13) {
    throw std::invalid_argument("Error: ISBN code must be a string and have 10 or 13 digits.");
  }
  
  
  int weightMultiple = 0;
  for (size_t i = 0; i < isbn.length(); i++) {
    int digit = isbn[i] - '0';
    weightMultiple += (i % 2 == 0) ? digit : digit * 3;
  }
  
  int modulo = (isbn.length() == 13) ? weightMultiple % 10 : weightMultiple % 11;
  
  return (modulo == 0);
  
}
