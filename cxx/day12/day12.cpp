#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: ./day12 input.txt" << std::endl;
        return 1;
    }

    std::ifstream f(argv[1]);

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(f, line))
    {
        // std::cout << line << std::endl;
        lines.push_back(line);
    }

    struct Vertex
    {
        int x;
        int y;
    };
    typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, Vertex, boost::no_property> Graph;

    Graph g;
    Vertex v1{0, 0};
    Vertex v2{0, 1};
    auto d1 = boost::add_vertex(v1, g);
    auto d2 = boost::add_vertex(v2, g);
    boost::add_edge(d1, d2, g);

    boost::write_graphviz(std::cout, g);

    return 0;
}
