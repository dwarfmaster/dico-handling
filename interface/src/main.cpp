#include <QtWidgets/QApplication>

#include <nodes/NodeData>
#include <nodes/FlowScene>
#include <nodes/FlowView>
#include <nodes/DataModelRegistry>
#include <nodes/NodeStyle>
#include <nodes/FlowViewStyle>
#include <nodes/ConnectionStyle>

#include "frameModel.hpp"

using QtNodes::DataModelRegistry;
using QtNodes::FlowScene;
using QtNodes::FlowView;
using QtNodes::ConnectionStyle;
using QtNodes::FlowViewStyle;
using QtNodes::NodeStyle;

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

  FlowViewStyle::setStyle(
   R"(
   {
     "FlowViewStyle": {
       "BackgroundColor": [255,255,240],
       "FineGridColor":   [245,245,230],
       "CoarseGridColor": [235,235,220]
     }
   }
   )");

  NodeStyle::setNodeStyle(
   R"(
   {
     "NodeStyle": {
       "NormalBoundaryColor": "darkgray",
       "SelectedBoundaryColor": "deepskyblue",
       "GradientColor0": "mintcream",
       "GradientColor1": "mintcream",
       "GradientColor2": "mintcream",
       "GradientColor3": "mintcream",
       "ShadowColor": [200, 200, 200],
       "FontColor": [10, 10, 10],
       "FontColorFaded": [100, 100, 100],
       "ConnectionPointColor": "white",
       "PenWidth": 2.0,
       "HoveredPenWidth": 2.5,
       "ConnectionPointDiameter": 10.0,
       "Opacity": 1.0
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
