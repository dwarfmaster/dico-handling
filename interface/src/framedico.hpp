
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
        
    private:
        std::map<std::string,std::vector<std::string>> m_frames;
};

