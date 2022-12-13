#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
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

    typedef boost::adjacency_list<
        boost::vecS, boost::vecS, boost::directedS,
        std::tuple<int, int>, boost::property<boost::edge_weight_t, int>>
        Graph;

    Graph g;

    // For each (x, y) in the input, add a vertex to the graph, and an edge to the
    // appropriate neighbours.
    int x_end = std::size(lines[0]);
    int y_end = std::size(lines);
    std::map<std::tuple<int, int>, Graph::vertex_descriptor> descriptors;

    for (int x = 0; x < x_end; ++x)
    {
        for (int y = 0; y < y_end; ++y)
        {
            auto vertex = std::make_tuple(x, y);
            auto v_desc = boost::add_vertex(vertex, g);
            descriptors[vertex] = v_desc;
        }
    }

    std::tuple<int, int> start;
    Graph::vertex_descriptor start_desc;
    std::tuple<int, int> end;
    Graph::vertex_descriptor end_desc;
    for (int x = 0; x < x_end; ++x)
    {
        for (int y = 0; y < y_end; ++y)
        {
            auto v_desc = descriptors[std::make_tuple(x, y)];
            char height = lines[y][x];
            if (height == 'S')
            {
                height = 'a';
                start = std::make_tuple(x, y);
                start_desc = v_desc;
            }
            if (height == 'E')
            {
                height = 'z';
                end = std::make_tuple(x, y);
                end_desc = v_desc;
            }

            // std::cout << "(" << x << ", " << y << ") is " << height << std::endl;

            // NESW
            if (y - 1 >= 0 && lines[y - 1][x] <= height + 1)
            {
                auto n_desc = descriptors[std::make_tuple(x, y - 1)];
                boost::add_edge(v_desc, n_desc, g);
            }
            if (x + 1 < x_end && lines[y][x + 1] <= height + 1)
            {
                auto n_desc = descriptors[std::make_tuple(x + 1, y)];
                boost::add_edge(v_desc, n_desc, g);
            }
            if (y + 1 < y_end && lines[y + 1][x] <= height + 1)
            {
                auto n_desc = descriptors[std::make_tuple(x, y + 1)];
                boost::add_edge(v_desc, n_desc, g);
            }
            if (x - 1 >= 0 && lines[y][x - 1] <= height + 1)
            {
                auto n_desc = descriptors[std::make_tuple(x - 1, y)];
                boost::add_edge(v_desc, n_desc, g);
            }
        }
    }

    // boost::write_graphviz(std::cout, g);

    // Find paths from end back to start.
    std::vector<Graph::vertex_descriptor> p(num_vertices(g));
    std::vector<int> d(num_vertices(g));

    boost::dijkstra_shortest_paths(
        g, end_desc,
        boost::predecessor_map(boost::make_iterator_property_map(p.begin(), get(boost::vertex_index, g)))
            .distance_map(boost::make_iterator_property_map(d.begin(), get(boost::vertex_index, g))));

    for (auto it = d.begin(); it != d.end(); ++it)
    {
        std::cout << *it << std::endl;
    }
    // std::cout << d[end_desc] << std::endl;

    return 0;
}
