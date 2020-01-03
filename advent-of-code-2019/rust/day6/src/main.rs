// TODO: Factor out an iterator over edges from the `read_graph` and
// `read_undirected_graph` functions.
use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io::BufRead;
use std::io::BufReader;

type DirectedGraph = HashMap<String, Vec<String>>;

type Graph = HashMap<String, Vec<String>>;

fn read_graph() -> Graph {
    let re = Regex::new(r"^([A-Z0-9]{3})\)([A-Z0-9]{3})$").unwrap();

    let mut neighbors: DirectedGraph = DirectedGraph::new();
    let file = fs::File::open("input.txt").expect("Couldn't read file");
    let reader = BufReader::new(file);
    for s in reader.lines() {
        for cap in re.captures_iter(&s.unwrap()) {
            if neighbors.contains_key(&cap[1]) {
                neighbors.get_mut(&cap[1]).unwrap().push(cap[2].to_string());
            } else {
                neighbors.insert(cap[1].to_string(), vec![cap[2].to_string()]);
            }

            if neighbors.contains_key(&cap[2]) {
                neighbors.get_mut(&cap[2]).unwrap().push(cap[1].to_string());
            } else {
                neighbors.insert(cap[2].to_string(), vec![cap[1].to_string()]);
            }
        }
    }
    return neighbors;
}

fn read_directed_graph() -> DirectedGraph {
    let re = Regex::new(r"^([A-Z0-9]{3})\)([A-Z0-9]{3})$").unwrap();

    let mut neighbors: DirectedGraph = DirectedGraph::new();
    let file = fs::File::open("input.txt").expect("Couldn't read file");
    let reader = BufReader::new(file);
    for s in reader.lines() {
        for cap in re.captures_iter(&s.unwrap()) {
            if neighbors.contains_key(&cap[1]) {
                neighbors.get_mut(&cap[1]).unwrap().push(cap[2].to_string());
            } else {
                neighbors.insert(cap[1].to_string(), vec![cap[2].to_string()]);
            }
        }
    }
    return neighbors;
}

fn count_orbits(graph: &DirectedGraph, root: &str) -> Option<usize> {
    if !graph.contains_key(root) {
        return None;
    }

    let mut count = 0;
    let mut open = vec![(root, 0)];

    while !open.is_empty() {
        let (next, r) = open.pop().unwrap();
        let neighbors = graph.get(next);
        match neighbors {
            None => continue,
            Some(ns) => {
                for n in ns.iter() {
                    open.push((n, r + 1));
                    count += r + 1;
                }
            }
        }
    }

    return Some(count);
}

fn compute_distance(graph: &Graph, start: &str, end: &str) -> Option<usize> {
    if !graph.contains_key(start) || !graph.contains_key(end) {
        return None;
    }

    let mut open = vec![(start, 0)];
    let mut visited = HashSet::new();
    // TODO: What happens here. My guess is that we create a new copy of
    // whatever value `start` references. That is strictly unecessary.
    visited.insert(start.to_string());

    while !open.is_empty() {
        let (next, d) = open.pop().unwrap();
        if next == end {
            // There is only a single path from start to end, since we are in a
            // tree
            return Some(d);
        }
        let neighbors = graph.get(next);
        match neighbors {
            None => continue,
            Some(ns) => {
                for n in ns.iter() {
                    if !visited.contains(n) {
                        open.push((n, d + 1));
                        visited.insert(n.to_string());
                    }
                }
            }
        }
    }
    return None;
}

#[allow(dead_code)]
fn count_edges(graph: &DirectedGraph) -> usize {
    let mut count = 0;
    for (_, neighbors) in graph {
        count += &neighbors.len();
    }
    return count;
}

fn compute_6a() -> usize {
    let graph = read_directed_graph();
    return count_orbits(&graph, "COM").unwrap();
}

fn compute_6b() -> usize {
    let graph = read_graph();
    return compute_distance(&graph, "YOU", "SAN").unwrap() - 2;
}

fn main() {
    println!("Answer 6a: {}", compute_6a());
    println!("Answer 6a: {}", compute_6b());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_6a() {
        assert_eq!(158090, compute_6a());
    }

    #[test]
    fn test_compute_6b() {
        assert_eq!(241, compute_6b());
    }
}
