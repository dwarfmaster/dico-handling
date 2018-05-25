
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

class Reader {
    public:
        Reader() {}
        virtual ~Reader() {}

        virtual std::streamsize read(char* buffer, std::streamsize size) = 0;
        virtual operator bool() = 0;
        virtual bool operator!() = 0;
};

class IStreamReader : public Reader {
    public:
        IStreamReader() = delete;
        IStreamReader(std::istream* input);
        ~IStreamReader();

        std::streamsize read(char* buffer, std::streamsize size) override;
        operator bool() override;
        bool operator!() override;

    private:
        std::istream* m_input;
};

class StringReader : public Reader {
    public:
        StringReader() = delete;
        StringReader(const std::string& str);
        ~StringReader();

        std::streamsize read(char* buffer, std::streamsize size) override;
        operator bool() override;
        bool operator!() override;

    private:
        std::istringstream* m_sstring;
        IStreamReader m_rd;
};

class SExprParser {
    public:
        SExprParser() = delete;
        SExprParser(const std::string& str);
        SExprParser(std::istream* input);
        SExprParser(Reader* rd);
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
        Reader* m_input;
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

template <typename T, typename Fstring, typename Flist>
class map_visitor : public boost::static_visitor<T> {
    Fstring& m_fstring;
    Flist&   m_flist;
    public:
        map_visitor(Fstring& fstring, Flist& flist)
            : m_fstring(fstring), m_flist(flist)
            {}

        T operator()(const std::string& str) const {
            return m_fstring(str);
        }

        T operator()(std::shared_ptr<SList> lst) const {
            return m_flist(lst);
        }
};

template <typename T, typename Fstring, typename Flist>
T sexpr_map(Fstring fstring, Flist flist, const SExpr& expr) {
    return boost::apply_visitor(map_visitor<T,Fstring,Flist>(fstring, flist), expr);
}

