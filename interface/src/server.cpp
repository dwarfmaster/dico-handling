
#include "server.hpp"
#include <iostream>
#include <boost/timer.hpp>
#include <boost/utility/typed_in_place_factory.hpp>
using namespace boost::asio;

/* Takes over ownership of the socket */
class SocketReader : public Reader {
    public:
        SocketReader() = delete;

        SocketReader(ip::tcp::socket* socket)
            : m_socket(socket)
            { }

        ~SocketReader() {
            /* Nothing to do */
        }

        /* code taken from there:
         * https://stackoverflow.com/a/25018876
         */
        std::streamsize read(char* data, std::streamsize size, timeout_t timeout) override {
            boost::optional<boost::system::error_code> timer_result;
            boost::asio::deadline_timer timer(m_socket->get_io_service());
            timer.expires_from_now(boost::posix_time::milliseconds(timeout.count()));
            timer.async_wait([&timer_result] (const boost::system::error_code& error)
                             { timer_result = error; });

            boost::optional<size_t> read_result;
            boost::asio::async_read(*m_socket, buffer(data,size),
                    [&read_result] (const boost::system::error_code&, size_t rd)
                    { read_result = rd; });

            m_socket->get_io_service().reset();
            while(m_socket->get_io_service().run_one()) {
                if(read_result)
                    timer.cancel();
                if(timer_result)
                    m_socket->cancel();
            }

            if(read_result) return *read_result;
            else            return 0;
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
    , m_socket(nullptr), m_reader(nullptr), m_parser(boost::none)
    { open_stream(true); }

Server::~Server() {
    if(m_parser) {
        delete m_reader;
        delete m_socket;
    }
}

bool Server::receive(SExpr& expr, std::chrono::milliseconds timeout) {
    if(!open_stream()) return false;
    return m_parser->read(expr, timeout);
}

Server& Server::operator>>(SExpr& expr) {
    if(open_stream()) (*m_parser) >> expr;
    return *this;
}

void Server::send(const SExpr& expr) {
    std::ostringstream oss;
    oss << expr << std::endl;

    boost::system::error_code ignored_error;
    write(*m_socket, buffer(oss.str()), ignored_error); 
}

Server& Server::operator<<(const SExpr& expr) {
    send(expr);
    return *this;
}

bool Server::open_stream(bool reset) {
    if(m_parser && (*m_parser)) return true;
    if(!m_parser && !reset) {
        m_ios.poll();
        return false;
    }
    if(m_reader) {
        delete m_reader;
        delete m_socket;
        m_parser = boost::none;
    }

    m_socket = new ip::tcp::socket(m_ios);
    m_reader = new SocketReader(m_socket);
    m_acceptor.async_accept(*m_socket,
            [this] (const boost::system::error_code& error)
            { this->m_parser = boost::in_place<SExprParser>(this->m_reader); });
    return false;
}

