
#pragma once

#include <nodes/FlowView>
#include <nodes/FlowScene>
#include <nodes/DataModelRegistry>

#include <iostream>
#include <boost/optional.hpp>

#include "framegraph.hpp"
#include "sexpr.hpp"

class FrameScene : public QtNodes::FlowView {
    Q_OBJECT

    public:
        FrameScene();
        FrameScene(QWidget* wd);
        virtual ~FrameScene();

        void reset(const SExpr& expr);

    private:
        FrameGraph<QtNodes::Node*,size_t>::Frame
            handler(const std::string& name, const std::vector<std::string>& fes);

        boost::optional<FrameGraph<QtNodes::Node*,size_t>> m_graph;
        QtNodes::FlowScene m_scene;
};

