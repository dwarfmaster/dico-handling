
#include "frames.hpp"
#include <algorithm>

Frame::Frame(const std::string& name) : m_name (name) { }


std::string Frame::name() const {
    return m_name;
}


void Frame::addFe(const std::string& name) {
    if(std::find(m_fes.begin(), m_fes.end(), name) == m_fes.end()) m_fes.push_back(name);
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

const std::string& Frame::operator[](size_t i) const {
    return m_fes[i];
}

