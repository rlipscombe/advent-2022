#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/visitors.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/graphviz.hpp>

using Lines = std::vector<std::string>;

using Point = std::tuple<int, int>;
using Rect = std::tuple<std::tuple<int, int>>;

using VertexProperty = boost::property<boost::vertex_name_t, std::tuple<int, int>>;
using EdgeProperty = boost::property<boost::edge_weight_t, int>;
using Graph = boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS,
                                    VertexProperty, EdgeProperty>;

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

std::vector<VertexDescriptor> get_path(std::vector<VertexDescriptor> predecessors, VertexDescriptor source, VertexDescriptor destination)
{
    std::vector<VertexDescriptor> path;
    VertexDescriptor current = destination;
    while (current != source)
    {
        path.push_back(current);
        current = predecessors[current];
    }
    path.push_back(source);
    return path;
}

VertexDescriptor find_cell(Lines lines, char ch)
{
    int x_end = std::size(lines[0]);
    int y_end = std::size(lines);
    for (int y = 0; y < y_end; ++y)
    {
        for (int x = 0; x < x_end; ++x)
        {
            if (lines[y][x] == ch)
            {
                return vertex_descriptor_for(std::make_tuple(x, y), x_end, y_end);
            }
        }
    }

    return -1;
}

VertexDescriptor find_start(Lines lines)
{
    return find_cell(lines, 'S');
}

VertexDescriptor find_end(Lines lines)
{
    return find_cell(lines, 'E');
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
        lines.push_back(line);
    }

    Graph g = create_graph(lines);

    // boost::write_graphviz(std::cout, g);

    // Part 1: Find the shortest path from 'S' to 'E'.
    VertexDescriptor source = find_start(lines);
    VertexDescriptor destination = find_end(lines);

    auto vertex_count = boost::num_vertices(g);
    std::vector<int> distances(vertex_count);

    auto visitor = boost::make_bfs_visitor(boost::record_distances(
        boost::make_iterator_property_map(distances.begin(), get(boost::vertex_index, g)), boost::on_tree_edge()));
    boost::breadth_first_search(g, source, boost::visitor(visitor));

    // 'distances' has all of the distances from 'S'. Look up 'E'.
    std::cout << distances[destination] << std::endl;

    return 0;
}
