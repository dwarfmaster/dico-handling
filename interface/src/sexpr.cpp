
#include "sexpr.hpp"
#include <cstdio>
#include <cstring>
#include <iostream>

#include <unistd.h>
#include <fcntl.h>
#include <poll.h>

using namespace std::chrono_literals;

//  _   _       _      _____ ____  ____                _           
// | | | |_ __ (_)_  _|  ___|  _ \|  _ \ ___  __ _  __| | ___ _ __ 
// | | | | '_ \| \ \/ / |_  | | | | |_) / _ \/ _` |/ _` |/ _ \ '__|
// | |_| | | | | |>  <|  _| | |_| |  _ <  __/ (_| | (_| |  __/ |   
//  \___/|_| |_|_/_/\_\_|   |____/|_| \_\___|\__,_|\__,_|\___|_|   
//                                                                 

UnixFDReader::UnixFDReader(int input)
    : m_input(input)
    {}

UnixFDReader::~UnixFDReader() {
    /* Nothing to do */
}

std::streamsize UnixFDReader::read(char* buffer, std::streamsize size, timeout_t timeout) {
    pollfd pfd;

    if(timeout.count() == 0) {
        return ::read(m_input, buffer, size);
    }

    pfd.fd = m_input;
    pfd.events = POLLIN;
    poll(&pfd, 1, timeout.count());
    if(pfd.revents & POLLIN) {
        return ::read(m_input, buffer, size);
    }
    return 0;
}

UnixFDReader::operator bool() {
    return static_cast<bool>(fcntl(m_input, F_GETFD) != -1 || errno != EBADF);
}

bool UnixFDReader::operator!() {
    return !static_cast<bool>(*this);
}


//  ____  _        _             ____                _           
// / ___|| |_ _ __(_)_ __   __ _|  _ \ ___  __ _  __| | ___ _ __ 
// \___ \| __| '__| | '_ \ / _` | |_) / _ \/ _` |/ _` |/ _ \ '__|
//  ___) | |_| |  | | | | | (_| |  _ <  __/ (_| | (_| |  __/ |   
// |____/ \__|_|  |_|_| |_|\__, |_| \_\___|\__,_|\__,_|\___|_|   
//                         |___/                                 

StringReader::StringReader(const std::string& str)
    : m_str(str), m_pos(0)
    { }

StringReader::~StringReader() {
    /* Nothing to do */
}

std::streamsize StringReader::read(char* buffer, std::streamsize size, timeout_t timeout) {
    if(m_pos >= m_str.size()) return 0;

    size_t cpsize = std::min(static_cast<size_t>(size), m_str.size() - m_pos);
    std::memcpy(buffer, m_str.c_str(), cpsize);
    m_pos += cpsize;
    return cpsize;
}

StringReader::operator bool() {
    return m_pos >= m_str.size(); 
}

bool StringReader::operator!() {
    return !static_cast<bool>(*this);
}


//  ____  _____                 ____                          
// / ___|| ____|_  ___ __  _ __|  _ \ __ _ _ __ ___  ___ _ __ 
// \___ \|  _| \ \/ / '_ \| '__| |_) / _` | '__/ __|/ _ \ '__|
//  ___) | |___ >  <| |_) | |  |  __/ (_| | |  \__ \  __/ |   
// |____/|_____/_/\_\ .__/|_|  |_|   \__,_|_|  |___/\___|_|   
//                  |_|                                       

SExprParser::SExprParser(const std::string& str) 
    : m_input(new StringReader(str)), m_free_input(true)
    {}
        
SExprParser::SExprParser(int input)
    : m_input(new UnixFDReader(input)), m_free_input(true)
    {}
        
SExprParser::SExprParser(Reader* rd)
    : m_input(rd), m_free_input(false)
    {}

SExprParser::~SExprParser() {
    for(SList* ptr : m_stack) {
        delete ptr;
    }
    if(m_free_input) delete m_input;
}

void SExprParser::pop_symbol(SList* lst) {
    if(!m_symbol.empty()) lst->m_childrens.push_back(m_symbol);
    m_symbol.clear();
}

bool SExprParser::read(SExpr& expr, std::chrono::milliseconds timeout) {
    const size_t bufsize = 256;
    char buf[bufsize];
    SList* lst;

    if(!m_parsed.empty()) {
        expr = m_parsed.front();
        m_parsed.pop();
        return true;
    }

    bool expr_filled = false;
    std::streamsize count = m_input->read(buf, bufsize, timeout);
    for(std::streamsize i = 0; i < count; ++i) {
        char c = buf[i];
        switch(c) {
            case '(':
                m_stack.push_back(new SList);
                break;
            case ')':
                lst = m_stack.back();
                m_stack.pop_back();
                pop_symbol(lst);
                if(m_stack.empty()) {
                    if(expr_filled) m_parsed.push(std::shared_ptr<SList>(lst));
                    else {
                        expr = std::shared_ptr<SList>(lst);
                        expr_filled = true;
                    }
                } else {
                    m_stack.back()->m_childrens.push_back(std::shared_ptr<SList>(lst));
                }
                break;
            case ' ': case '\t': case '\n':
                if(m_stack.empty()) {
                    if(m_symbol.empty()) continue;
                    if(expr_filled) m_parsed.push(m_symbol);
                    else {
                        expr = m_symbol;
                        expr_filled = true;
                    }
                    m_symbol.clear();
                } else pop_symbol(m_stack.back());
                break;
            default:
                m_symbol.push_back(c);
                break;
        }
    }
    return expr_filled;
}

SExprParser& SExprParser::operator>>(SExpr& expr) {
    while(!read(expr, 0ms));
    return *this;
}

SExprParser::operator bool() const {
    return !m_parsed.empty() || static_cast<bool>(*m_input);
}

bool SExprParser::operator!() const {
    return !static_cast<bool>(*this);
}


//  ____  _     _     _   
// / ___|| |   (_)___| |_ 
// \___ \| |   | / __| __|
//  ___) | |___| \__ \ |_ 
// |____/|_____|_|___/\__|
//                        

SList::SList() {
    /* Nothing to do */
}
        
SList::SList(const std::vector<SExpr>& expr)
    : m_childrens(expr)
    { }

SList::~SList() {
    /* Nothing to do */
}

size_t SList::childrens() const {
    return m_childrens.size();
}

SExpr SList::operator[](size_t i) {
    return m_childrens[i];
}

class expr_printer : public boost::static_visitor<> {
    std::ostream& m_os;

    public:
        expr_printer(std::ostream& os) : m_os(os) { }

        void operator()(const std::string& str) const {
            m_os << str;
        }

        void operator()(const std::shared_ptr<SList>& lst) const {
            m_os << "(";
            for(size_t i = 0; i < lst->childrens(); ++i) {
                SExpr e = (*lst)[i];
                boost::apply_visitor(*this, e);
                m_os << " ";
            }
            m_os << ")";
        }
};

std::ostream& operator<<(std::ostream& os, const SExpr& expr) {
    boost::apply_visitor(expr_printer(os), expr);
    return os;
}

std::string read_string_from_sexpr(const SExpr& expr) {
    return sexpr_map<std::string>( [](const std::string& str)
                                     { return str; }
                                 , [](std::shared_ptr<SList>)
                                     { throw "Expected string, got SList in SExpr"; 
                                       return "";
                                     }
                                 , expr
                                 );
}

std::shared_ptr<SList> read_slist_from_sexpr(const SExpr& expr) {
    return sexpr_map<std::shared_ptr<SList>>
        ( [](const std::string&)
            { throw "Expected SList, got string in SExpr";
              return std::shared_ptr<SList>(nullptr);
            }
        , [](std::shared_ptr<SList> lst)
            { return lst; }
        , expr
        );
}

