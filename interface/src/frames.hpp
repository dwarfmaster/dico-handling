
#pragma once

#include <string>
#include <vector>

class Frame {
    public:
        using iterator = std::vector<std::string>::iterator;
        using const_iterator = std::vector<std::string>::const_iterator;
        Frame() = delete;
        Frame(const std::string& name);
        std::string name() const;
        void addFe(const std::string& name);
        size_t nbFes() const;
        iterator fe_begin();
        iterator fe_end();
        const_iterator fe_begin() const;
        const_iterator fe_end() const;
        const std::string& operator[](size_t i) const;

    private:
        std::string m_name;
        std::vector<std::string> m_fes;
};

