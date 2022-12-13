#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/graphviz.hpp>

using VertexProperty = boost::property<boost::vertex_name_t, std::tuple<int, int>>;
using EdgeProperty = boost::property<boost::edge_weight_t, int>;
using Graph = boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, VertexProperty, EdgeProperty>;

using VertexDescriptor = boost::graph_traits<Graph>::vertex_descriptor;

Graph create_graph(std::vector<std::string> lines)
{
    // For each (x, y) in the input, add a vertex to the graph, and an edge to the
    // appropriate neighbours.
    int x_end = std::size(lines[0]);
    int y_end = std::size(lines);

    Graph g;

    for (int x = 0; x < x_end; ++x)
    {
        for (int y = 0; y < y_end; ++y)
        {
            auto vertex = std::make_tuple(x, y);
            auto v_desc = boost::add_vertex(vertex, g);
        }
    }

    // VertexDescriptor a = add_vertex(VertexPropertyType("a", white_color), g);
    // VertexDescriptor b = add_vertex(VertexPropertyType("b", white_color), g);
    // VertexDescriptor c = add_vertex(VertexPropertyType("c", white_color), g);
    // VertexDescriptor d = add_vertex(VertexPropertyType("d", white_color), g);
    // VertexDescriptor e = add_vertex(VertexPropertyType("e", white_color), g);

    // add_edge(a, c, EdgePropertyType(1, black_color), g);
    // add_edge(b, d, EdgePropertyType(1, black_color), g);
    // add_edge(b, e, EdgePropertyType(2, black_color), g);
    // add_edge(c, b, EdgePropertyType(5, black_color), g);
    // add_edge(c, d, EdgePropertyType(10, black_color), g);
    // add_edge(d, e, EdgePropertyType(4, black_color), g);
    // add_edge(e, a, EdgePropertyType(3, black_color), g);
    // add_edge(e, b, EdgePropertyType(7, black_color), g);

    return g;
}

// Graph g;

// std::map<std::tuple<int, int>, Graph::vertex_descriptor> descriptors;

// std::tuple<int, int> start;
// Graph::vertex_descriptor start_desc;
// std::tuple<int, int> end;
// Graph::vertex_descriptor end_desc;
// for (int x = 0; x < x_end; ++x)
// {
//     for (int y = 0; y < y_end; ++y)
//     {
//         auto v_desc = descriptors[std::make_tuple(x, y)];
//         char height = lines[y][x];
//         if (height == 'S')
//         {
//             height = 'a';
//             start = std::make_tuple(x, y);
//             start_desc = v_desc;
//             std::cerr << "S" << start_desc << std::endl;
//         }
//         if (height == 'E')
//         {
//             height = 'z';
//             end = std::make_tuple(x, y);
//             end_desc = v_desc;
//             std::cerr << "E" << end_desc << std::endl;
//         }

//         // std::cout << "(" << x << ", " << y << ") is " << height << std::endl;

//         // NESW
//         if (y - 1 >= 0 && lines[y - 1][x] <= height + 1)
//         {
//             auto n_desc = descriptors[std::make_tuple(x, y - 1)];
//             boost::add_edge(v_desc, n_desc, g);
//         }
//         if (x + 1 < x_end && lines[y][x + 1] <= height + 1)
//         {
//             auto n_desc = descriptors[std::make_tuple(x + 1, y)];
//             boost::add_edge(v_desc, n_desc, g);
//         }
//         if (y + 1 < y_end && lines[y + 1][x] <= height + 1)
//         {
//             auto n_desc = descriptors[std::make_tuple(x, y + 1)];
//             boost::add_edge(v_desc, n_desc, g);
//         }
//         if (x - 1 >= 0 && lines[y][x - 1] <= height + 1)
//         {
//             auto n_desc = descriptors[std::make_tuple(x - 1, y)];
//             boost::add_edge(v_desc, n_desc, g);
//         }
//     }
// }


int    main(int argc, char *argv[])
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
