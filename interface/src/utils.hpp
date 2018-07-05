
#pragma once

#include <iterator>

//  _____ _       _   _             _             
// |  ___| | __ _| |_| |_ ___ _ __ (_)_ __   __ _ 
// | |_  | |/ _` | __| __/ _ \ '_ \| | '_ \ / _` |
// |  _| | | (_| | |_| ||  __/ | | | | | | | (_| |
// |_|   |_|\__,_|\__|\__\___|_| |_|_|_| |_|\__, |
//                                          |___/ 
//  ___ _                 _             
// |_ _| |_ ___ _ __ __ _| |_ ___  _ __ 
//  | || __/ _ \ '__/ _` | __/ _ \| '__|
//  | || ||  __/ | | (_| | || (_) | |   
// |___|\__\___|_|  \__,_|\__\___/|_|   
//                                      

template <typename OuterIterator, typename InnerIterator>
class flattening_iterator {
    public:
        using value_type        = typename std::iterator_traits<InnerIterator>::value_type;
        using reference         = typename std::iterator_traits<InnerIterator>::reference;
        using pointer           = typename std::iterator_traits<InnerIterator>::pointer;
        using difference_type   =
            typename std::iterator_traits<InnerIterator>::difference_type;
        using iterator_category = std::input_iterator_tag;

        flattening_iterator() : m_end(true) { }
        flattening_iterator(OuterIterator beg, OuterIterator end = OuterIterator())
                : m_end(false), m_out(beg), m_out_end(end) {
            setin();
            move();
        }

        bool operator==(const flattening_iterator<OuterIterator,InnerIterator>& it) const {
            return (m_end && it.m_end)
                || (it.m_out == m_out && it.m_out_end == m_out_end
                        && it.m_in == m_in && it.m_in_end == m_in_end);
        }
        
        inline bool
        operator!=(const flattening_iterator<OuterIterator,InnerIterator>& it) const {
            return !operator==(it);
        }

        value_type operator*() {
            return *m_in;
        }

        flattening_iterator<OuterIterator,InnerIterator>&
        operator++() {
            ++m_in;
            move();
        }

        flattening_iterator<OuterIterator,InnerIterator>
        operator++(int) {
            auto ret(*this);
            ++(*this);
            return ret;
        }

    private:
        bool m_end;
        OuterIterator m_out;
        OuterIterator m_out_end;
        InnerIterator m_in;
        InnerIterator m_in_end;

        void setin() {
            if(m_out == m_out_end) {
                m_end = true;
                return;
            }
            m_in     = std::begin(*m_out);
            m_in_end = std::end(*m_out);
        }

        void move() {
            if(m_in == m_in_end) {
                ++m_out;
                setin();
            }
        }
};

namespace std {
    template <typename OuterIterator, typename InnerIterator>
    struct iterator_traits<flattening_iterator<OuterIterator,InnerIterator>> {
        using it = flattening_iterator<OuterIterator,InnerIterator>;
        using value_type        = typename it::value_type;
        using reference         = value_type&;
        using pointer           = value_type*;
        using difference_type   = typename it::difference_type;
        using iterator_category = typename it::iterator_category;
    };
}


//  ___ _                 _               ____       _      
// |_ _| |_ ___ _ __ __ _| |_ ___  _ __  |  _ \ __ _(_)_ __ 
//  | || __/ _ \ '__/ _` | __/ _ \| '__| | |_) / _` | | '__|
//  | || ||  __/ | | (_| | || (_) | |    |  __/ (_| | | |   
// |___|\__\___|_|  \__,_|\__\___/|_|    |_|   \__,_|_|_|   
//                                                          

template <typename Iterator>
struct ItPair {
    using iterator = Iterator;

    ItPair() = default;
    ItPair(const std::pair<Iterator,Iterator>& pr)
        : m_beg(pr.first), m_end(pr.second)
        { }
    ItPair(Iterator beg, Iterator end)
        : m_beg(beg), m_end(end)
        { }
    ItPair& operator=(const ItPair& it) {
        m_beg = it.m_beg;
        m_end = it.m_end;
    }


    Iterator begin() const {
        return m_beg;
    }

    Iterator end() const {
        return m_end;
    }

    Iterator m_beg, m_end;
};

