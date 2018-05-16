
#include "frames.hpp"


Frame::Frame(const std::string& name) : m_name (name) { }


std::string Frame::name() const {
    return m_name;
}


void Frame::addFe(const std::string& name) {
    m_fes.insert(name);
}


size_t Frame::nbFes() const {
    return m_fes.size();
}


Frame::iterator Frame::fe_begin() {
    return m_fes.begin();
}


Frame::iterator Frame::fe_end() {
    return m_fes.end();
}


Frame::const_iterator Frame::fe_begin() const {
    return m_fes.begin();
}


Frame::const_iterator Frame::fe_end() const {
    return m_fes.end();
}


