#include <Rcpp.h>
#include <iostream>
#include <string>
#include <regex>
using namespace Rcpp;

// [[Rcpp::export]]
std::string card_number_validate(const std::string& cardNumber) {
  std::regex pattern("[^0-9A-Za-z]*");
  std::string cleanedNumber = std::regex_replace(cardNumber, pattern, "");
  
  if (cleanedNumber.length() != 9) {
    throw std::runtime_error("Error: Card number must be a string consisting 9 digits.");
  }
  
  std::regex digitPattern("\\D");
  if (std::regex_search(cleanedNumber, digitPattern)) {
    throw std::runtime_error("Error: Card number can only contain digits.");
  }
  
  std::string separatedNumber = cleanedNumber.substr(0, 3) + "-" +
    cleanedNumber.substr(3, 3) + "-" +
    cleanedNumber.substr(6, 3);
  
  return separatedNumber;
}
