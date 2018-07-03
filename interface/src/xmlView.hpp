
#pragma once

#include <QtWidgets/QTextEdit>
#include <nodes/NodeData>
#include <nodes/FlowScene>
#include "treeSequence.hpp"

class XmlView : public QTextEdit {
    public:
        XmlView() = delete;
        XmlView(QWidget* parent = nullptr);
        virtual ~XmlView();

        void setText(const std::string& str);
        void set(const TreeSequence<QtNodes::Node*,size_t>& tree, size_t word,
                const std::vector<std::pair<size_t,size_t>>& wordBounds);
};

