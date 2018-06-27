
#pragma once

#include "sexpr.hpp"
#include "framegraph.hpp"
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <utility>
#include <cassert>
#include <boost/optional.hpp>


//  ____            _                 _   _             
// |  _ \  ___  ___| | __ _ _ __ __ _| |_(_) ___  _ __  
// | | | |/ _ \/ __| |/ _` | '__/ _` | __| |/ _ \| '_ \
// | |_| |  __/ (__| | (_| | | | (_| | |_| | (_) | | | |
// |____/ \___|\___|_|\__,_|_|  \__,_|\__|_|\___/|_| |_|
//                                                      

template <typename Node, typename Place>
class TreeSequence {
    public:
        TreeSequence();
        ~TreeSequence();

        using Graph = FrameGraph<Node,Place>;
        using Frame = typename FrameGraph<Node,Place>::Frame;
        using PId   = typename FrameGraph<Node,Place>::PlaceId;

        void rebuild(const std::vector<std::string>& words, Graph& fg, const SExpr& data);

        std::vector<std::pair<size_t,size_t>> encapsulate(size_t word_id);
        std::string get_word(size_t word_id);
        size_t get_word_from_var(const std::string& var);
        std::vector<Frame> get_word_frames(size_t word_id);

    private:
        std::vector<std::string> m_words;
        std::map<std::string,size_t> m_wordVars;
        boost::optional<Graph> m_frames;
        struct treeNode {
            size_t first;
            size_t last;
            std::vector<std::shared_ptr<treeNode>> children;
            std::shared_ptr<treeNode> parent;
        };
        std::shared_ptr<treeNode> m_root;
};


//  ____        __ _       _ _   _             
// |  _ \  ___ / _(_)_ __ (_) |_(_) ___  _ __  
// | | | |/ _ \ |_| | '_ \| | __| |/ _ \| '_ \
// | |_| |  __/  _| | | | | | |_| | (_) | | | |
// |____/ \___|_| |_|_| |_|_|\__|_|\___/|_| |_|
//                                             

template <typename Node, typename Place>
TreeSequence<Node,Place>::TreeSequence() {
    /* Nothing to do */
}


template <typename Node, typename Place>
TreeSequence<Node,Place>::~TreeSequence() {
    /* Nothing to do */
}


template <typename Node, typename Place>
void TreeSequence<Node,Place>::rebuild(const std::vector<std::string>& words,
        TreeSequence<Node,Place>::Graph& gr,
        const SExpr& data) {
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


template <typename Node, typename Place>
std::vector<std::pair<size_t,size_t>>
TreeSequence<Node,Place>::encapsulate(size_t word_id) {
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


template <typename Node, typename Place>
std::string TreeSequence<Node,Place>::get_word(size_t word_id) {
    return m_words[word_id];
}


template <typename Node, typename Place>
size_t TreeSequence<Node,Place>::get_word_from_var(const std::string& var) {
    return m_wordVars[var];
}


template <typename Node, typename Place>
std::vector<typename TreeSequence<Node,Place>::Frame>
TreeSequence<Node,Place>::get_word_frames(size_t word_id) {
    /* TODO */
}

