
#include "treeSequence.hpp"
#include <cassert>

TreeSequence::TreeSequence() {
    /* Nothing to do */
}


TreeSequence::~TreeSequence() {
    /* Nothing to do */
}


void TreeSequence::rebuild(const std::vector<std::string>& words, const SExpr& data) {
    m_words = words;

    std::shared_ptr<SList> top_level = read_slist_from_sexpr(data);
    assert(top_level->childrens() == 3);

    std::shared_ptr<SList> seq_sexpr = read_slist_from_sexpr((*top_level)[2]);
    for(size_t i = 0; i < seq_sexpr->childrens(); ++i) {
        m_wordVars[read_string_from_sexpr((*seq_sexpr)[i])] = i;
    }

    std::map<std::string,std::shared_ptr<treeNode>> nodeNames;
    std::shared_ptr<SList> tree_sexpr = read_slist_from_sexpr((*top_level)[1]);
    for(size_t i = 0; i < tree_sexpr->childrens(); ++i) {
        std::shared_ptr<SList> node_sexpr = read_slist_from_sexpr((*tree_sexpr)[i]);
        treeNode node;
        assert(node_sexpr->childrens() == 4);

        node.first = get_word_from_var(read_string_from_sexpr((*node_sexpr)[2]));
        node.last  = get_word_from_var(read_string_from_sexpr((*node_sexpr)[3]));
        std::string node_name   = read_string_from_sexpr((*node_sexpr)[0]);
        std::string node_parent = read_string_from_sexpr((*node_sexpr)[1]);

        if(nodeNames.find(node_name) == nodeNames.end()) {
            nodeNames[node_name] = std::make_shared<treeNode>(node);
        } else {
            nodeNames[node_name]->first = node.first;
            nodeNames[node_name]->last  = node.last;
        }

        if(nodeNames.find(node_parent) == nodeNames.end()) {
            nodeNames[node_parent] = std::make_shared<treeNode>();
            nodeNames[node_parent]->parent = nullptr;
        }
        nodeNames[node_parent]->children.push_back(nodeNames[node_name]);
        nodeNames[node_name]->parent = nodeNames[node_parent];
    }

    m_root = nodeNames.begin()->second;
    while(m_root->parent) m_root = m_root->parent;
}


std::vector<std::pair<size_t,size_t>> TreeSequence::encapsulate(size_t word_id) {
    std::vector<std::pair<size_t,size_t>> result;
    std::shared_ptr<treeNode> nd = m_root;
    size_t child = 0;

    while(child < nd->children.size()) {
        if(word_id <= nd->children[child]->last) {
            child = 0;
            nd = nd->children[child];
            result.push_back(std::make_pair(nd->first, nd->last));
        } else ++child;
    }

    return result;
}


std::string TreeSequence::get_word(size_t word_id) {
    return m_words[word_id];
}


size_t TreeSequence::get_word_from_var(const std::string& var) {
    return m_wordVars[var];
}



