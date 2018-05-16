
#pragma once

#include <string>
#include <unordered_set>

class Frame {
    public:
        using iterator = std::unordered_set<std::string>::iterator;
        using const_iterator = std::unordered_set<std::string>::const_iterator;
        Frame() = delete;
        Frame(const std::string& name);
        std::string name() const;
        void addFe(const std::string& name);
        size_t nbFes() const;
        iterator fe_begin();
        iterator fe_end();
        const_iterator fe_begin() const;
        const_iterator fe_end() const;

    private:
        std::string m_name;
        std::unordered_set<std::string> m_fes;
};

