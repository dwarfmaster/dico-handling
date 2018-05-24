
#include "server.hpp"
#include <iostream>
using namespace boost::asio;

/* Takes over ownership of the socket */
class SocketReader : public Reader {
    public:
        SocketReader() = delete;

        SocketReader(ip::tcp::socket* socket)
            : m_socket(socket)
            { }

        ~SocketReader() {
            delete m_socket;
        }

        std::streamsize read(char* data, std::streamsize size) override {
            return m_socket->read_some(buffer(data, size));
        }

        operator bool() override {
            /* There is no way at the TCP level to check if the connection is
             * still open
             */
            return true;
        }

        bool operator!() override {
            return !static_cast<bool>(*this);
        }

    private:
        ip::tcp::socket* m_socket;
};

Server::Server(int port)
    : m_endpoint(ip::tcp::v4(), port), m_acceptor(m_ios, m_endpoint)
    , m_socket(nullptr), m_reader(nullptr), m_parser(nullptr)
    { }

Server::~Server() {
    if(m_parser) {
        delete m_parser;
        delete m_reader;
        delete m_socket;
    }
}

void Server::receive(SExpr& expr) {
    open_stream();
    (*m_parser) >> expr;
}

Server& Server::operator>>(SExpr& expr) {
    receive(expr);
    return *this;
}

void Server::send(const SExpr& expr) {
    std::ostringstream oss;
    oss << expr;

    boost::system::error_code ignored_error;
    write(*m_socket, buffer(oss.str()), ignored_error); 
}

Server& Server::operator<<(const SExpr& expr) {
    send(expr);
    return *this;
}

void Server::open_stream() {
    if(m_parser && (*m_parser)) return;
    if(m_parser) {
        delete m_parser;
        delete m_reader;
        delete m_socket;
        m_parser = nullptr;
    }

    m_socket = new ip::tcp::socket(m_ios);
    m_reader = new SocketReader(m_socket);
    m_acceptor.accept(*m_socket);
    m_parser = new SExprParser(m_reader);
}

