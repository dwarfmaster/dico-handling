
#pragma once

#include <string>
#include <vector>
#include <memory>
#include <istream>
#include <ostream>
#include <queue>
#include <boost/variant.hpp>

class SList;
using SExpr = boost::variant<std::shared_ptr<SList>,std::string>;

class SExprParser {
    public:
        SExprParser() = delete;
        SExprParser(const std::string& str);
        SExprParser(std::istream& input);
        SExprParser(std::istream* input);
        ~SExprParser();

        void read(SExpr& expr);
        SExprParser& operator>>(SExpr& expr);
        operator bool() const;
        bool operator!() const;

    private:
        void pop_symbol(SList* lst);

        std::queue<SExpr> m_parsed;
        std::vector<SList*> m_stack;
        std::string m_symbol;
        std::istream* m_input;
        bool m_free_input;
};

class SList {
    public:
        SList();
        ~SList();
        
        size_t childrens() const;
        SExpr operator[](size_t i);

    private:
        friend SExprParser;
        friend std::ostream& operator<<(std::ostream& os, const SExpr& expr);
        std::vector<SExpr> m_childrens;
};

std::ostream& operator<<(std::ostream& os, const SExpr& expr);

