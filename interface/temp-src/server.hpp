
#pragma once

#include <string>
#include <boost/asio.hpp>
#include "sexpr.hpp"

class Server {
    public:
        Server() = delete;
        Server(int port);
        ~Server();

        void receive(SExpr& expr);
        Server& operator>>(SExpr& expr);
        void send(const SExpr& expr);
        Server& operator<<(const SExpr& expr);

    private:
        void open_stream();
        boost::asio::io_service m_ios;
        boost::asio::ip::tcp::endpoint m_endpoint;
        boost::asio::ip::tcp::acceptor m_acceptor;
        boost::asio::ip::tcp::socket*  m_socket;
        Reader* m_reader;
        SExprParser* m_parser;
};

