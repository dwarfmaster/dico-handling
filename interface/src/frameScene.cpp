
#include "frameScene.hpp"

#include "frameModel.hpp"
#include "frames.hpp"
#include <boost/utility/typed_in_place_factory.hpp>

using QtNodes::Node;

FrameScene::FrameScene() {
    setScene(&m_scene);
}

FrameScene::~FrameScene() {
    /* Nothing to do */
}

FrameGraph<Node*,size_t>::Frame
FrameScene::handler(const std::string& name, const std::vector<std::string>& fes) {
    Frame *fr = new Frame(name);
    for(auto& fe : fes) fr->addFe(fe);
    std::map<std::string,size_t> ids;
    for(size_t i = 0; i < fes.size(); ++i) ids[fes[i]] = i;

    return FrameGraph<Node*,size_t>::Frame {
        .name = name,
        .node = &m_scene.createNode(std::unique_ptr<NodeDataModel>(new FrameModel(fr))),
        .fes  = ids
    };
}

void FrameScene::reset(const SExpr& expr) {
    m_scene.clearScene();

    m_graph = boost::in_place<FrameGraph<Node*,size_t>>(expr,
            [this] (const std::string& name, const std::vector<std::string>& fes)
            { return this->handler(name, fes); } );

    for(auto cc_it = m_graph->ccbegin(); cc_it != m_graph->ccend(); ++cc_it) {
        std::vector<FrameGraph<Node*,size_t>::PlaceId> cc(cc_it->begin(), cc_it->end());
        for(size_t i = 0; i < cc.size() - 1; ++i) {
            m_scene.createConnection(*cc[i].node, cc[i].place
                                    ,*cc[i+1].node, cc[i+1].place);
        }
    }
}

