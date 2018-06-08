
#include "sexpr.hpp"
#include <sstream>
#include <cstdio>
#include <iostream>


//  ___ ____  _                            ____                _           
// |_ _/ ___|| |_ _ __ ___  __ _ _ __ ___ |  _ \ ___  __ _  __| | ___ _ __ 
//  | |\___ \| __| '__/ _ \/ _` | '_ ` _ \| |_) / _ \/ _` |/ _` |/ _ \ '__|
//  | | ___) | |_| | |  __/ (_| | | | | | |  _ <  __/ (_| | (_| |  __/ |   
// |___|____/ \__|_|  \___|\__,_|_| |_| |_|_| \_\___|\__,_|\__,_|\___|_|   
//                                                                         

IStreamReader::IStreamReader(std::istream* input)
    : m_input(input)
    {}

IStreamReader::~IStreamReader() {
    /* Nothing to do */
}

std::streamsize IStreamReader::read(char* buffer, std::streamsize size) {
    m_input->read(buffer, size);
    return m_input->gcount();
}

IStreamReader::operator bool() {
    return static_cast<bool>(*m_input);
}

bool IStreamReader::operator!() {
    return !static_cast<bool>(*this);
}


//  ____  _        _             ____                _           
// / ___|| |_ _ __(_)_ __   __ _|  _ \ ___  __ _  __| | ___ _ __ 
// \___ \| __| '__| | '_ \ / _` | |_) / _ \/ _` |/ _` |/ _ \ '__|
//  ___) | |_| |  | | | | | (_| |  _ <  __/ (_| | (_| |  __/ |   
// |____/ \__|_|  |_|_| |_|\__, |_| \_\___|\__,_|\__,_|\___|_|   
//                         |___/                                 

StringReader::StringReader(const std::string& str)
    : m_sstring(new std::istringstream(str)), m_rd(m_sstring)
    { }

StringReader::~StringReader() {
    delete m_sstring;
}

std::streamsize StringReader::read(char* buffer, std::streamsize size) {
    return m_rd.read(buffer, size);
}

StringReader::operator bool() {
    return m_rd; 
}

bool StringReader::operator!() {
    return !m_rd;
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
        
SExprParser::SExprParser(std::istream* input)
    : m_input(new IStreamReader(input)), m_free_input(true)
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

void SExprParser::read(SExpr& expr) {
    const size_t bufsize = 256;
    char buf[bufsize];
    SList* lst;

    if(!m_parsed.empty()) {
        expr = m_parsed.front();
        m_parsed.pop();
        return;
    }

    bool expr_filled = false;
    while(!expr_filled && m_input) {
        std::streamsize count = m_input->read(buf, bufsize);
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
    }
}

SExprParser& SExprParser::operator>>(SExpr& expr) {
    read(expr);
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

