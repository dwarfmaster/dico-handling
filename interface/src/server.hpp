
#pragma once

#include <string>
#include <boost/asio.hpp>
#include <boost/optional.hpp>
#include "sexpr.hpp"

class Server {
    public:
        Server() = delete;
        Server(int port);
        ~Server();

        /* Non-blocking read */
        bool receive(SExpr& expr, std::chrono::milliseconds timeout);
        Server& operator>>(SExpr& expr);
        void send(const SExpr& expr);
        Server& operator<<(const SExpr& expr);

    private:
        bool open_stream(bool reset = false);
        boost::asio::io_service m_ios;
        boost::asio::ip::tcp::endpoint m_endpoint;
        boost::asio::ip::tcp::acceptor m_acceptor;
        boost::asio::ip::tcp::socket*  m_socket;
        Reader* m_reader;
        boost::optional<SExprParser> m_parser;
};

