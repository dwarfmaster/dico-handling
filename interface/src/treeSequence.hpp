
#pragma once

#include "sexpr.hpp"
#include "framegraph.hpp"
#include "utils.hpp"
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <utility>
#include <cassert>
#include <boost/optional.hpp>
#include <boost/bimap.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/irange.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/property_map/property_map.hpp>


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

        std::vector<std::pair<size_t,size_t>> encapsulate(size_t word_id) const;
        std::string get_word(size_t word_id) const;
        size_t get_word_from_var(const std::string& var) const;
        std::vector<Frame> get_word_frames(size_t word_id) const;
        const Graph& graph() const;

    private:
        std::vector<std::string> m_words;
        using wdvars_t = boost::bimap<std::string,size_t>;
        wdvars_t m_wordVars;
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
    m_frames = gr;

    std::shared_ptr<SList> top_level = read_slist_from_sexpr(data);
    assert(top_level->childrens() == 3);

    std::shared_ptr<SList> seq_sexpr = read_slist_from_sexpr((*top_level)[2]);
    for(size_t i = 0; i < seq_sexpr->childrens(); ++i) {
        m_wordVars.insert( TreeSequence<Node,Place>::wdvars_t::value_type(
                    boost::to_lower_copy(read_string_from_sexpr((*seq_sexpr)[i])),
                    i));
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
TreeSequence<Node,Place>::encapsulate(size_t word_id) const {
    std::vector<std::pair<size_t,size_t>> result;
    std::shared_ptr<treeNode> nd = m_root;
    size_t child = 0;

    while(child < nd->children.size()) {
        if(nd->children.size() == 0 || nd->children[0]->first > word_id) {
            return result;
        }
        if(word_id <= nd->children[child]->last) {
            child = 0;
            nd = nd->children[child];
            result.push_back(std::make_pair(nd->first, nd->last));
        } else ++child;
    }

    return result;
}


template <typename Node, typename Place>
std::string TreeSequence<Node,Place>::get_word(size_t word_id) const {
    return m_words[word_id];
}


template <typename Node, typename Place>
size_t TreeSequence<Node,Place>::get_word_from_var(const std::string& var) const {
    return m_wordVars.left.find(var)->second;
}

/* A graph structure for boost
 * Implements the concepts Graph, VertexListGraph, IncidenceGraph and EdgeListGraph
 */
template <typename Node, typename Place>
struct FrGraph {
    using Fr = typename FrameGraph<Node,Place>::Frame;
    FrGraph() = delete;
    FrGraph(const FrameDico& d) : dico(d) { }
    std::vector<Fr> vertices;
    const FrameDico& dico;
};

namespace internal {
    struct Edge {
        size_t source, target;
        bool operator==(const Edge& edg) {
            return source == edg.source && target == edg.target;
        }
        bool operator!=(const Edge& edg) {
            return source != edg.source || target != edg.target;
        }
    };
    Edge make_edge(size_t src, size_t trg);

    /* Could be replaced by a boost::filter_iterator */
    template <typename Node,typename Place>
    struct FrGraphEdgeIterator {
        typename std::vector<typename FrGraph<Node,Place>::Fr>::const_iterator actual;
        size_t start;
        const FrGraph<Node,Place>* graph;

        FrGraphEdgeIterator() : actual(), start(0), graph(nullptr)
                            { }

        bool operator==(const FrGraphEdgeIterator& it) const {
            return (!graph && !it.graph)
                || (start == it.start && actual == it.actual);
        }

        bool operator!=(const FrGraphEdgeIterator& it) const {
            return !operator==(it);
        }

        inline void incr() {
            if(!graph) return;

            do {
                ++actual;
                /* Reached end */
                if(actual == graph->vertices.end()) {
                    graph = nullptr;
                    return;
                }
            } while(!graph->dico.related(graph->vertices[start].name, actual->name));
        }

        inline Edge get() const {
            return make_edge(start, actual - graph->vertices.begin());
        }

        Edge operator*() const {
            return get();
        }

        FrGraphEdgeIterator& operator++() {
            incr();
            return *this;
        }

        struct DerefPair {
            Edge pair;
            Edge operator*() const {
                return pair;
            }
        };

        DerefPair operator++(int) {
            DerefPair ret;
            ret.pair = get();
            incr();
            return ret;
        }
    };
}

namespace boost {

    class fr_graph_tag : public vertex_list_graph_tag,
                         public incidence_graph_tag,
                         public edge_list_graph_tag
    { };

    template <typename Node, typename Place>
    struct graph_traits<FrGraph<Node,Place>> {
        /* Graph concept */
        using vertex_descriptor      = size_t;
        using edge_descriptor        = ::internal::Edge;
        using directed_category      = directed_tag;
        using edge_parallel_category = disallow_parallel_edge_tag;
        using traversal_category     = fr_graph_tag;

        static vertex_descriptor null_vertex() {
            return (size_t)-1;
        }

        /* VertexListGraph concept */
        using vertex_iterator    =
            iterator_range<range_detail::integer_iterator<size_t>>::iterator;
        using vertices_size_type = size_t;

        /* IncidenceGraph concept */
        using out_edge_iterator = ::internal::FrGraphEdgeIterator<Node,Place>;
        using degree_size_type  = size_t;

        /* EdgeListGraph concept */
        using edge_iterator_inner =
            typename std::vector<typename FrameGraph<Node,Place>::Frame>::const_iterator;
        using edge_iterator_outer =
            boost::transform_iterator<
                std::function<ItPair<out_edge_iterator>
                              (const typename FrameGraph<Node,Place>::Frame&)>,
                edge_iterator_inner>;
        using edge_iterator   = flattening_iterator<
            edge_iterator_outer,
            typename ItPair<out_edge_iterator>::iterator>;
        using edges_size_type = size_t;
    };
}

template <typename Node, typename Place>
using GTG = boost::graph_traits<FrGraph<Node,Place>>;

template <typename Node, typename Place>
static std::pair<typename GTG<Node,Place>::vertex_iterator,
                 typename GTG<Node,Place>::vertex_iterator>
vertices(const FrGraph<Node,Place>& gr) {
    auto ir = boost::irange<size_t>(0, gr.vertices.size());
    return std::make_pair(ir.begin(), ir.end());
}

template <typename Node, typename Place>
static typename GTG<Node,Place>::vertices_size_type
num_vertices(const FrGraph<Node,Place>& gr) {
    return gr.vertices.size();
}

template <typename Node, typename Place>
static typename GTG<Node,Place>::vertex_descriptor
source(const typename GTG<Node,Place>::edge_descriptor& e,
       const FrGraph<Node,Place>&) {
    return e.source;
}

template <typename Node, typename Place>
static typename GTG<Node,Place>::vertex_descriptor
target(const typename GTG<Node,Place>::edge_descriptor& e,
       const FrGraph<Node,Place>&) {
    return e.target;
}

template <typename Node, typename Place>
static std::pair<typename GTG<Node,Place>::out_edge_iterator,
                 typename GTG<Node,Place>::out_edge_iterator>
out_edges(typename GTG<Node,Place>::vertex_descriptor u, 
          const FrGraph<Node,Place>& gr) {
    typename GTG<Node,Place>::out_edge_iterator beg, end;
    beg.graph  = &gr;
    beg.start  = u;
    beg.actual = gr.vertices.begin();
    return std::make_pair(beg, end);
}

template <typename Node, typename Place>
static typename GTG<Node,Place>::degree_size_type
out_degree(typename GTG<Node,Place>::vertex_descriptor u,
           const FrGraph<Node,Place>& gr) {
    size_t count = 0;
    for(auto& fr : gr.vertices) {
        if(gr.dico.related(gr.vertices[u].name, fr.name)) ++count;
    }
    return count;
}

template <typename Node, typename Place>
static std::pair<typename GTG<Node,Place>::edge_iterator,
                 typename GTG<Node,Place>::edge_iterator>
edges(const FrGraph<Node,Place>& gr) {
    using Iterator      = typename GTG<Node,Place>::edge_iterator;
    using EdgeIterator  = typename GTG<Node,Place>::out_edge_iterator;
    using InnerIterator = typename GTG<Node,Place>::edge_iterator_inner;
    using TransIterator = typename GTG<Node,Place>::edge_iterator_outer;

    Iterator end;
    TransIterator tit = TransIterator(gr.vertices.begin(),
            [gr] (const typename FrameGraph<Node,Place>::Frame& fr)
                 { return ItPair<EdgeIterator>(out_edges<Node,Place>(fr.index, gr)); });
    TransIterator tend;
    Iterator begin(tit, tend);
    return std::make_pair(begin, end);
}

template <typename Node, typename Place>
static typename GTG<Node,Place>::edges_size_type
num_edges(const FrGraph<Node,Place>& gr) {
    auto pr = edges(gr);
    return std::distance(pr.first, pr.second);
}

namespace std {
    template <typename Node, typename Place>
    struct iterator_traits<internal::FrGraphEdgeIterator<Node,Place>> {
        using value_type        = internal::Edge;
        using difference_type   = long int;
        using iterator_category = input_iterator_tag;
        using reference         = internal::Edge&;
        using pointer           = internal::Edge*;
    };
}

template <typename Node, typename Place>
std::vector<typename TreeSequence<Node,Place>::Frame>
TreeSequence<Node,Place>::get_word_frames(size_t word_id) const {
    using Fr = typename FrameGraph<Node,Place>::Frame;
    std::string var = m_wordVars.right.find(word_id)->second;
    std::vector<Fr> ret;
    for(size_t i = 0; i < m_frames->nbFrames(); ++i) {
        if(m_frames->getFrame(i).lexeme_var == var) ret.push_back(m_frames->getFrame(i));
    }

    /* We can't use std::sort because it doesn't support partial orders
     * but only strict weak orderings */
    FrGraph<Node,Place> graph(m_frames->dico());
    graph.vertices = ret;

    std::ofstream ofs("graph.dot");
    boost::write_graphviz_dp(ofs, graph,
            boost::dynamic_properties()
            .property<boost::typed_identity_property_map<size_t>>
            ("node_id", boost::typed_identity_property_map<size_t>()));
    ofs.close();

    std::vector<size_t> indexes;
    boost::topological_sort(graph, std::back_inserter(indexes),
            boost::vertex_index_map(boost::typed_identity_property_map<size_t>()));

    std::transform(indexes.begin(), indexes.end(), ret.begin(),
            [&graph] (size_t id)
                     { return graph.vertices[id]; });
    return ret;
}

template <typename Node, typename Place>
const typename TreeSequence<Node,Place>::Graph&
TreeSequence<Node,Place>::graph() const {
    return *m_frames;
}

