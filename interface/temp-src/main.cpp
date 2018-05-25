
#include <iostream>
#include <fstream>
#include <boost/asio.hpp>
#include "sexpr.hpp"
#include "server.hpp"
#include "framegraph.hpp"

using namespace boost::asio;

int main() {
    Server server(4444);
    SExpr expr;

    while(true) {
        server >> expr;
        FrameGraph<std::string,std::string> frameGraph(expr,
                [] (const std::string& name, const std::vector<std::string>& fes) {
                    std::map<std::string,std::string> mp;
                    for(auto fe : fes) { mp[fe] = fe; }
                    return FrameGraph<std::string,std::string>::Frame {
                        .name = name,
                        .node = name,
                        .fes  = mp
                    };
                });
        for(auto cc_it = frameGraph.ccbegin(); cc_it != frameGraph.ccend(); ++cc_it) {
            for(auto pid : *cc_it) {
                std::cout << pid.node << "->" << pid.place << std::endl;
            }
            std::cout << "*******************" << std::endl;
        }
        std::cout << "------------------------" << std::endl;
    }

    return 0;
}

