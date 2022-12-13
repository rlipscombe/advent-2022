#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/graphviz.hpp>

using Lines = std::vector<std::string>;

using Point = std::tuple<int, int>;
using Rect = std::tuple<std::tuple<int, int>>;

using VertexProperty = boost::property<boost::vertex_name_t, std::tuple<int, int>>;
using EdgeProperty = boost::property<boost::edge_weight_t, int>;
using Graph = boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, VertexProperty, EdgeProperty>;

using VertexDescriptor = boost::graph_traits<Graph>::vertex_descriptor;

int height_of(Point pt, Lines lines)
{
    auto [x, y] = pt;
    char ch = lines[y][x];
    if (ch == 'S')
    {
        return 'a';
    }
    else if (ch == 'E')
    {
        return 'z';
    }
    else
    {
        return ch;
    }
}

Graph::vertex_descriptor vertex_descriptor_for(Point pt, int x_end, int y_end)
{
    // Gonna cheat slightly here. We know that because we're using
    // an adjacency list with vecS, that the vertex at (x, y) has
    // descriptor `(y * x_end) + x`.

    auto [x, y] = pt;
    return (y * x_end) + x;
}

bool is_in_bounds(Point pt, int x_end, int y_end)
{
    auto [x, y] = pt;
    return x >= 0 && x < x_end && y >= 0 && y < y_end;
}

void maybe_add_edge(Point from, Point to, Lines lines, int x_end, int y_end, Graph &g)
{
    if (is_in_bounds(to, x_end, y_end) && height_of(to, lines) <= height_of(from, lines) + 1)
    {
        add_edge(vertex_descriptor_for(from, x_end, y_end), vertex_descriptor_for(to, x_end, y_end), g);
    }
}

Graph create_graph(std::vector<std::string> lines)
{
    // For each (x, y) in the input, add a vertex to the graph, and an edge to the
    // appropriate neighbours.
    int x_end = std::size(lines[0]);
    int y_end = std::size(lines);

    Graph g;

    for (int y = 0; y < y_end; ++y)
    {
        for (int x = 0; x < x_end; ++x)
        {
            auto vertex = std::make_tuple(x, y);
            boost::add_vertex(vertex, g);
        }
    }

    // Adding edges to "new" vertices also puts those vertices in the adjacency list,
    // which screws up our cheat for working out the descriptors, so we add all of the vertices,
    // and then all of the edges.
    for (int y = 0; y < y_end; ++y)
    {
        for (int x = 0; x < x_end; ++x)
        {
            maybe_add_edge(std::make_tuple(x, y), std::make_tuple(x - 1, y), lines, x_end, y_end, g);
            maybe_add_edge(std::make_tuple(x, y), std::make_tuple(x + 1, y), lines, x_end, y_end, g);
            maybe_add_edge(std::make_tuple(x, y), std::make_tuple(x, y - 1), lines, x_end, y_end, g);
            maybe_add_edge(std::make_tuple(x, y), std::make_tuple(x, y + 1), lines, x_end, y_end, g);
        }
    }

    return g;
}

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

    Graph g = create_graph(lines);

    boost::write_graphviz(std::cout, g);

    // // Find paths from end back to start.
    // std::vector<Graph::vertex_descriptor> p(num_vertices(g));
    // std::vector<int> d(num_vertices(g));
    // auto w = boost::make_constant_property<boost::edge_weight_t>(1);

    // boost::dijkstra_shortest_paths(
    //     g, start_desc,
    //     boost::predecessor_map(boost::make_iterator_property_map(p.begin(), get(boost::vertex_index, g)))
    //         .distance_map(boost::make_iterator_property_map(d.begin(), get(boost::vertex_index, g)))
    //         .weight_map(get(boost::edge_weight, g)));

    // for (auto it = d.begin(); it != d.end(); ++it)
    // {
    //     std::cout << *it << std::endl;
    // }

    return 0;
}
