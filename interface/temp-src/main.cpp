
#include <iostream>
#include <fstream>
#include "sexpr.hpp"

int main() {
    SExprParser parser(&std::cin);
    SExpr expr;

    while(parser) {
        parser >> expr;
        std::cout << expr << "\n--------" << std::endl;
    }

    return 0;
}

