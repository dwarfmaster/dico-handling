
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

QString FrameModel::portCaption(PortType portType, PortIndex portIndex) const {
    if(portType == PortType::In && portIndex < m_fr->nbFes()) {
        return QString((*m_fr)[portIndex].c_str());
    }
    return QString();
}

void FrameModel::setInData(std::shared_ptr<NodeData> nodeData, PortIndex port)  {
    /* Nothing to do */
}


QWidget* FrameModel::embeddedWidget()  {
    return nullptr;
}


bool FrameModel::eventFilter(QObject* object, QEvent* event)  {
    return false;
}



