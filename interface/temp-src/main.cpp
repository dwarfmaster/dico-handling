
#include <iostream>
#include <fstream>
#include <boost/asio.hpp>
#include "sexpr.hpp"
#include "server.hpp"

using namespace boost::asio;

int main() {
    Server server(4444);
    SExpr expr;

    while(true) {
        server >> expr;
        std::cout << expr << std::endl;
    }

    return 0;
}

