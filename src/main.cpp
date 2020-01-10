#include <iostream>
#include <fstream>

#include "tokens.hpp"

int main(int argc, char** argv) {
  if(argc != 2) return -1;
  std::cout << "\nOpening " << argv[1];
  std::ifstream in(argv[1]);

  Token tok;
  while(in.good() && !in.eof()) {
    in >> tok;
    std::cout << ToString(tok.tag) << std::endl;
  }

  return 0;
}
