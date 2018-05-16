#include <QtWidgets/QApplication>

#include <nodes/NodeData>
#include <nodes/FlowScene>
#include <nodes/FlowView>
#include <nodes/DataModelRegistry>
#include <nodes/ConnectionStyle>

#include "frameModel.hpp"

using QtNodes::DataModelRegistry;
using QtNodes::FlowScene;
using QtNodes::FlowView;
using QtNodes::ConnectionStyle;

static
void
setStyle()
{
  ConnectionStyle::setConnectionStyle(
    R"(
  {
    "ConnectionStyle": {
      "UseDataDefinedColors": true
    }
  }
  )");
}


//------------------------------------------------------------------------------

int
main(int argc, char* argv[])
{
  QApplication app(argc, argv);
  Frame fr("Entity");
  fr.addFe("Entity");
  fr.addFe("Constituent_parts");
  fr.addFe("Formational_cause");
  fr.addFe("Name");
  fr.addFe("Type");

  setStyle();

  FlowScene scene;
  auto& node1 = scene.createNode(std::unique_ptr<NodeDataModel>(new FrameModel(&fr)));
  auto& node2 = scene.createNode(std::unique_ptr<NodeDataModel>(new FrameModel(&fr)));
  scene.createConnection(node1, 0, node2, 3);

  FlowView view(&scene);

  view.setWindowTitle("Node-based flow editor");
  view.resize(800, 600);
  view.show();

  return app.exec();
}
