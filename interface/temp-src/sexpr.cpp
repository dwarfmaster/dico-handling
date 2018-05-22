
#include "sexpr.hpp"
#include <sstream>
#include <cstdio>

SExprParser::SExprParser(const std::string& str) 
    : m_input(new std::istringstream(str)), m_free_input(true)
    {}
        
SExprParser::SExprParser(std::istream& input)
    : m_input(&input), m_free_input(false)
    {}

SExprParser::SExprParser(std::istream* input)
    : m_input(input), m_free_input(false)
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
        m_input->read(buf, bufsize);
        for(std::streamsize i = 0; i < m_input->gcount(); ++i) {
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
                boost::apply_visitor(*this, (*lst)[i]);
                m_os << " ";
            }
            m_os << ")";
        }
};

std::ostream& operator<<(std::ostream& os, const SExpr& expr) {
    boost::apply_visitor(expr_printer(os), expr);
    return os;
}

