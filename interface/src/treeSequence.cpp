
#include "treeSequence.hpp"

namespace internal {
    Edge make_edge(size_t src, size_t trg) {
        Edge edg;
        edg.source = src;
        edg.target = trg;
        return edg;
    }
}

