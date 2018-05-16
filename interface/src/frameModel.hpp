
#pragma once

#include <QtCore/QObject>
#include <QtWidgets/QLabel>

#include <nodes/DataModelRegistry>
#include <nodes/NodeDataModel>

#include "frames.hpp"

using QtNodes::PortType;
using QtNodes::PortIndex;
using QtNodes::NodeData;
using QtNodes::NodeDataType;
using QtNodes::NodeDataModel;
using QtNodes::NodeValidationState;

class FrameModel : public NodeDataModel
{
    Q_OBJECT
    Frame* m_fr;

    public:
        FrameModel() = delete;
        FrameModel(Frame* fr) : m_fr(fr) { }
        virtual ~FrameModel() {}

        QString caption() const override
        { return QString(m_fr->name().c_str()); }

        QString name() const override
        { return QString("FrameModel"); }

        virtual QString modelName() const
        { return QString("Frame Displayer"); }

        unsigned int nPorts(PortType portType) const override;

        NodeDataType dataType(PortType portType, PortIndex portIndex) const override;

        std::shared_ptr<NodeData> outData(PortIndex port) override;

        void setInData(std::shared_ptr<NodeData> nodeData, PortIndex port) override;

        QWidget* embeddedWidget() override;

        bool resizable() const override
        { return false; }

        bool eventFilter(QObject* object, QEvent* event) override;
};

