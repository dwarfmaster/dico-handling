
#include "frameModel.hpp"
#include "feData.hpp"

unsigned int FrameModel::nPorts(PortType portType) const  {
    return m_fr->nbFes();
}


NodeDataType FrameModel::dataType(PortType portType, PortIndex portIndex) const  {
    return FeData().type();
}


std::shared_ptr<NodeData> FrameModel::outData(PortIndex port)  {
    return std::shared_ptr<NodeData>(new FeData);
}


void FrameModel::setInData(std::shared_ptr<NodeData> nodeData, PortIndex port)  {
}


QWidget* FrameModel::embeddedWidget()  {
    return nullptr;
}


bool FrameModel::eventFilter(QObject* object, QEvent* event)  {
    return false;
}



