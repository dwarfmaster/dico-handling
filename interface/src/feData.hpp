
#pragma once

#include <nodes/NodeDataModel>

using QtNodes::NodeData;
using QtNodes::NodeDataType;

class FeData : public NodeData
{
    public:
        FeData();
        NodeDataType type() const override;
};

