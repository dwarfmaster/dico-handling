
#include "framedico.hpp"
#include <algorithm>
#include <sstream>
#include <cassert>
#include <iostream>
#include <unordered_set>
#include <queue>
#include <boost/algorithm/string.hpp>

#include <QtCore/QFile>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonValue>
#include <QtCore/QJsonArray>

using boost::algorithm::to_lower_copy;

template <typename T>
T atoi(const std::string& str) {
    std::istringstream iss(str);
    T ret;
    iss >> ret;
    return ret;
}

FrameDico::FrameDico(const std::string& path) {
    QFile file(QString::fromStdString(path));
    assert(file.open(QIODevice::ReadOnly));
    QByteArray data = file.readAll();
    QJsonDocument json(QJsonDocument::fromJson(data));

    std::map<uint64_t,std::string> frameNames;

    /* Compute fes and frameNames */
    QJsonArray root = json.array();
    for(auto it = root.begin(); it != root.end(); ++it) {
        assert(it->isObject());
        QJsonObject object = it->toObject();
        std::string name(object["name"].toString().toUtf8().constData());
        name = to_lower_copy(name);

        std::string id(object["id"].toString().toUtf8().constData());
        frameNames[atoi<uint64_t>(id)] = name;
        for(auto fe : object["fes"].toArray()) {
            auto fe_array = fe.toArray();
            std::string fe_name = to_lower_copy(std::string(
                            fe_array[0].toString().toUtf8().constData()));
            size_t fe_id = static_cast<size_t>(fe_array[1].toDouble());
            m_frames[name].fes.push_back(fe_name);
            m_frames[name].feIDs[fe_name] = fe_id;
        }
    }

    /* Compute direct_parents */
    for(auto it = root.begin(); it != root.end(); ++it) {
        QJsonObject object = it->toObject();
        std::string name(object["name"].toString().toUtf8().constData());
        for(auto fr : object["rels"].toArray()) {
            m_frames[name].direct_parents.push_back(frameNames[
                    atoi<uint64_t>(std::string(fr.toString().toUtf8().constData()))]);
        }
    }

    /* Compute all_parents */
    for(auto fr : m_frames) {
        std::unordered_set<std::string> seens;
        std::queue<std::string> tosee;
        tosee.push(fr.first);
        while(!tosee.empty()) {
            std::string next = tosee.front();
            tosee.pop();
            if(seens.find(next) != seens.end()) continue;
            seens.insert(next);

            m_frames[fr.first].all_parents.push_back(next);
            for(auto follow : m_frames[next].direct_parents) {
                if(seens.find(follow) != seens.end()) continue;
                tosee.push(follow);
            }
        }
    }
}

FrameDico::~FrameDico() {
    /* Nothing to do */
}

bool FrameDico::has_frame(const std::string& name) {
    return m_frames.find(to_lower_copy(name)) != m_frames.end();
}

bool FrameDico::frame_has_fe(const std::string& frame, const std::string& fe) {
    std::string lframe = to_lower_copy(frame);
    std::string lfe    = to_lower_copy(fe);
    return std::find(m_frames[lframe].fes.begin(), m_frames[lframe].fes.end(), lfe)
        != m_frames[lframe].fes.end();
}

std::vector<std::string>& FrameDico::fes(const std::string& frame) {
    return m_frames[to_lower_copy(frame)].fes;
}

bool FrameDico::related(const std::string& child, const std::string& parent) const {
    std::string cname = to_lower_copy(child);
    std::string pname = to_lower_copy(parent);
    if(cname == pname) return false; /* Special case : we want a strict partial ordering
                                        on frames, to use with std::sort */
    return std::find(m_frames.at(cname).all_parents.cbegin(),
            m_frames.at(cname).all_parents.cend(), pname)
        != m_frames.at(cname).all_parents.cend();
}

size_t FrameDico::feID(const std::string& frame, const std::string& fe) const {
    return m_frames.at(frame).feIDs.at(fe);
}

