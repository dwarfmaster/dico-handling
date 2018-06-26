
#pragma once

#include "sexpr.hpp"
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <utility>

class TreeSequence {
    public:
        TreeSequence();
        ~TreeSequence();

        void rebuild(const std::vector<std::string>& words, const SExpr& data);

        std::vector<std::pair<size_t,size_t>> encapsulate(size_t word_id);
        std::string get_word(size_t word_id);
        size_t get_word_from_var(const std::string& var);

    private:
        std::vector<std::string> m_words;
        std::map<std::string,size_t> m_wordVars;
        struct treeNode {
            size_t first;
            size_t last;
            std::vector<std::shared_ptr<treeNode>> children;
            std::shared_ptr<treeNode> parent;
        };
        std::shared_ptr<treeNode> m_root;
};

