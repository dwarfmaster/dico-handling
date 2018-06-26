
#include <iostream>
#include "app.hpp"

using namespace std;

int
main(int argc, char* argv[])
{
    try{
        App app(argc, argv);
        return app.exec();
    } catch(const char* str) {
        cout << "Exception: " << str << endl;
    } catch(const std::string& str) {
        cout << "Exception: " << str << endl;
    }
}
