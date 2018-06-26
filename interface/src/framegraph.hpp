
#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <list>
#include <boost/algorithm/string.hpp>
#include "sexpr.hpp"
#include "framedico.hpp"



//  ____            _                 _   _             
// |  _ \  ___  ___| | __ _ _ __ __ _| |_(_) ___  _ ___ 
// | | | |/ _ \/ __| |/ _` | '__/ _` | __| |/ _ \| '_  |
// | |_| |  __/ (__| | (_| | | | (_| | |_| | (_) | | | |
// |____/ \___|\___|_|\__,_|_|  \__,_|\__|_|\___/|_| |_|
//                                                      

template <typename Node, typename Place>
class FrameGraph {
    public:
        struct Frame {
            std::string name;
            Node node;
            std::map<std::string,Place> fes;
        };
        struct PlaceId {
            Node node;
            Place place;

            bool operator<(const PlaceId& rhs) const {
                return node < rhs.node
                    || (!(rhs.node > node) && place < rhs.place);
            }
        };
        using cc_iterator = typename std::list<std::set<PlaceId>>::const_iterator;

        FrameGraph() = delete;
        template <typename Handler>
        FrameGraph(const SExpr& expr, Handler handler) {
            compute(expr, handler);
        }

        const Frame& getFrame(size_t i) const;
        size_t nbFrames() const;
        
        size_t nbCCs() const;
        cc_iterator ccbegin() const;
        cc_iterator ccend() const;
        const std::set<PlaceId>& getCCOf(PlaceId pid) const;
        void bind(PlaceId id1, PlaceId id2);

    private:
        std::vector<Frame> m_frames;
        std::list<std::set<PlaceId>> m_connections;

        template <typename Handler>
        void compute(const SExpr& expr, Handler handler);
};



//  ____        __ _       _ _   _             
// |  _ \  ___ / _(_)_ __ (_) |_(_) ___  _ ___ 
// | | | |/ _ \ |_| | '_ \| | __| |/ _ \| '_  |
// | |_| |  __/  _| | | | | | |_| | (_) | | | |
// |____/ \___|_| |_|_| |_|_|\__|_|\___/|_| |_|
//                                             

template <typename Node, typename Place>
size_t FrameGraph<Node,Place>::nbFrames() const {
    return m_frames.size();
}

template <typename Node, typename Place>
const typename FrameGraph<Node,Place>::Frame&
FrameGraph<Node,Place>::getFrame(size_t i) const {
    return m_frames[i];
}

template <typename Node, typename Place>
size_t FrameGraph<Node,Place>::nbCCs() const {
    return m_connections.size();
}

template <typename Node, typename Place>
typename FrameGraph<Node,Place>::cc_iterator FrameGraph<Node,Place>::ccbegin() const {
    return m_connections.begin();
}

template <typename Node, typename Place>
typename FrameGraph<Node,Place>::cc_iterator FrameGraph<Node,Place>::ccend() const {
    return m_connections.end();
}

template <typename Node, typename Place>
const std::set<typename FrameGraph<Node,Place>::PlaceId>&
FrameGraph<Node,Place>::getCCOf(typename FrameGraph<Node,Place>::PlaceId pid) const {
    for(auto cc : m_connections) {
        if(cc.find(pid) != cc.end()) return cc;
    }
}

template <typename Node, typename Place>
void FrameGraph<Node,Place>::bind(typename FrameGraph<Node,Place>::PlaceId id1,
        typename FrameGraph<Node,Place>::PlaceId id2) {
    typename std::list<std::set<PlaceId>>::iterator it1, it2;
    it1 = m_connections.end(); it2 = m_connections.end();

    for(auto it = m_connections.begin(); it != m_connections.end(); ++it) {
        if(it->find(id1) != it->end()) it1 = it;
        if(it->find(id2) != it->end()) it2 = it;
    }

    if(it1 == it2) return;

    it1->insert(it2->begin(), it2->end());
    m_connections->erase(it2);
}

namespace internal {
    using namespace std;
    // Map from frame variable name to frame name and list of fes
    using temp_frames_t = map<string, vector<pair<string, vector<string>>>>;
    // Map from fe variable to the according set
    template <typename Node, typename Place>
    using temp_connections_t = map<string, set<typename FrameGraph<Node,Place>::PlaceId>>;

    void parse_a_meaning(shared_ptr<SList> lst, temp_frames_t& temp_frames,
            FrameDico* dico);

    template <typename Node, typename Place>
    void connect(shared_ptr<SList> lst, temp_connections_t<Node,Place>& temp_conn,
            temp_frames_t& temp_frames, // should be const
            const std::vector<typename FrameGraph<Node,Place>::Frame>& frames) {
        std::string meaning_ident = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[0]));
        std::string frame_var = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[1]));
        std::string name = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[2]));
        std::string conn = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[3]));

        if(meaning_ident == "frame") return;

        typename FrameGraph<Node,Place>::PlaceId pid;
        for(auto fr : temp_frames[frame_var]) {
            string frame_name = std::get<0>(fr);
            if(frame_name.empty()) continue;
            if(find(fr.second.begin(), fr.second.end(), name) == fr.second.end()) continue;
            const typename FrameGraph<Node,Place>::Frame& frame
                = *find_if(frames.begin(), frames.end(),
                    [&frame_name](const typename FrameGraph<Node,Place>::Frame& fr)
                                 { return fr.name == frame_name; });
            pid.node = frame.node;
            pid.place = frame.fes.find(name)->second;
            if(temp_conn.find(conn) == temp_conn.end()) {
                temp_conn[conn] = set<typename FrameGraph<Node,Place>::PlaceId>();
            }
            temp_conn[conn].insert(pid);
        }
    }
}

template <typename Node, typename Place>
template <typename Handler>
void FrameGraph<Node,Place>::compute(const SExpr& expr, Handler handler) {
    internal::temp_frames_t temp_frames;
    std::shared_ptr<SList> top_level = read_slist_from_sexpr(expr);
    std::shared_ptr<SList> expr_as_list = read_slist_from_sexpr((*top_level)[0]);
    FrameDico dico("grammar.json");

    // Precompute frames
    for(size_t i = 0; i < expr_as_list->childrens(); ++i) {
        std::shared_ptr<SList> child = read_slist_from_sexpr((*expr_as_list)[i]);
        internal::parse_a_meaning(child, temp_frames, &dico);
    }

    // Apply handler to build definite frames
    for(auto frl : temp_frames) {
        for(auto pr : frl.second) {
            if(pr.first.empty()) continue;
            m_frames.push_back(handler(pr.first, pr.second));
        }
    }

    // Compute connections
    internal::temp_connections_t<Node,Place> temp_conn;
    for(size_t i = 0; i < expr_as_list->childrens(); ++i) {
        std::shared_ptr<SList> child = read_slist_from_sexpr((*expr_as_list)[i]);
        internal::connect<Node,Place>(child, temp_conn, temp_frames, m_frames);
    }

    // Set them
    for(auto pr : temp_conn) {
        m_connections.push_back(pr.second);
    }
}

