
#pragma once

#include <map>
#include <string>
#include <vector>

class FrameDico {
    public:
        FrameDico() = delete;
        FrameDico(const std::string& path);
        ~FrameDico();

        bool has_frame(const std::string& name);
        bool frame_has_fe(const std::string& frame, const std::string& fe);
        std::vector<std::string>& fes(const std::string& frame);
        size_t feID(const std::string& frame, const std::string& fe) const;
        bool related(const std::string& child, const std::string& parent) const;
        
    private:
        struct Frame {
            std::vector<std::string> fes;
            std::map<std::string,size_t> feIDs;
            std::vector<std::string> direct_parents;
            std::vector<std::string> all_parents;
        };
        std::map<std::string,Frame> m_frames;
};

