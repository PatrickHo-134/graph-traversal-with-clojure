(ns graph-traversal.core
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as pm]))

;;;; DEMONSTRATION CODE TO UNDERSTAND PROBLEM

(def G {:1 [:2 :3]
        :2 [:4]
        :3 [:4]
        :4 []})

(defn traverse-graph-dfs [g s]
  (loop [vertices []
         explored #{s}
         frontier [s]]
    (if (empty? frontier)
      vertices
      (let [v (peek frontier)
            neighbors (g v)]
        (recur
         (conj vertices v)
         (into explored neighbors)
         (into (pop frontier) (remove explored neighbors)))))))

(defn seq-graph-dfs [g s]
  ((fn rec-dfs [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (peek frontier)
              neighbors (g v)]
          (cons v (rec-dfs
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{s} [s]))

(defn seq-graph-bfs [g s]
  ((fn rec-bfs [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (peek frontier)
              neighbors (g v)]
          (cons v (rec-bfs
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj (clojure.lang.PersistentQueue/EMPTY) s)))

(defn seq-graph [d g s]
  ((fn rec-seq [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (peek frontier)
              neighbors (g v)]
          (cons v (rec-seq
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj d s)))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

(seq-graph-dfs G :1) ; => (:1 :3 :4 :2)
(seq-graph-bfs G :1) ; => (:1 :2 :3 :4)


;;;; QUESTION 1:

;; Assumptions:
;; - weight is an integer and 0 <= weight < 1000
;; - 2 neighbor nodes can have the same weight
;; - graph MUST be connected

(def G2 {:1 [[:2 1] [:3 2]]
         :2 [[:4 4] [:3 4]]
         :3 [[:4 2]]
         :4 []})


;;;; QUESTION 2:

(defn simple-directed-graph?
  "Checks if the given graph is a simple directed graph.

   A simple directed graph must:
   1. Have directed edges.
   2. Not contain duplicate edges.
   3. Not contain self-loops.

   Parameters:
   - graph: A map representing the graph where each key is a vertex (keyword) and each value is a list of
            tuples representing the edges and their weights. The edges are directed and have weights.

   Returns:
   - true if the graph is a simple directed graph, false otherwise."
  [graph]
  (let [edges (reduce (fn [acc [src dests]]
                        (concat acc (map (fn [[dest _]] [src dest]) dests)))
                      []
                      graph)
        unique-edges (set edges)]
    (and (= (count edges) (count unique-edges)) ;; No duplicate edges
         (not-any? (fn [[src dest]] (= src dest)) edges))))

(defn random-weight []
  (rand-int 1000))

(defn create-initial-edges
  "Creates a spanning tree with n nodes ensuring the graph is connected."
  [nodes]
  (let [edges (for [i (range 1 (count nodes))]
                [(nodes (dec i)) [(nodes i) (random-weight)]])]
    (vec edges)))

(defn add-extra-edges
  "Adds extra random edges to the graph while avoiding duplicates and self-loops."
  [edges nodes num-edges]
  (loop [edges           edges
         remaining-edges num-edges]
    (if (zero? remaining-edges)
      edges
      (let [src  (rand-nth nodes)
            dest (rand-nth (remove #(= src %) nodes))]
        (if (some #(= [src dest] %) (map (fn [e] [(first e) (first (second e))]) edges))
          (recur edges remaining-edges)
          (recur (conj edges [src [dest (random-weight)]])
                 (dec remaining-edges)))))))

(defn create-graph-edges [nodes num-edges]
  (let [initial-edges (create-initial-edges nodes)]
    (add-extra-edges initial-edges nodes (- num-edges (count initial-edges)))))

(defn create-simple-directed-graph
  "Generates a simple directed graph with a specified number of vertices and edges.

   Parameters:
   - n: Integer, the number of vertices in the graph.
   - s: Integer, the number of directed edges. Must be at least (n - 1) to ensure connectivity
        and at most n*(n - 1) for a simple directed graph.

   Returns a map representing the graph where each key is a vertex (keyword) and each value is a list of
   vectors representing the edges and their weights. The edges are directed and have random weights
   between 1 and 1000

   Example:
   (create-simple-directed-graph 5 7)
   => {:1 [[:2 3] [:4 48]]
       :2 [[:3 84] [:5 34]]
       :3 [[:4 59]]
       :4 [[:5 67]]
       :5 [[:1 14]]}
  "
  [n s]
  (assert (and (<= (dec n) s)
               (<= s (* n (dec n))))
          "Invalid number of edges")
  (let [nodes (->> n
                   inc
                   (range 1)
                   (map (comp keyword str))
                   (vec))
        edges (create-graph-edges nodes s)]
    (reduce (fn [graph [src [dest weight]]]
              (update graph src (fnil conj []) [dest weight]))
            {}
            edges)))

;; Example usage:
#_(def new-graph (create-simple-directed-graph 5 20))
#_(simple-directed-graph? new-graph)

;;;; QUESTION 3

(defn reconstruct-path
  "Reconstructs the shortest path from start to end using the previous nodes map."
  [previous-nodes end start]
  (loop [current end
         path    []]
    (if (nil? current)
      (reverse path)
      (recur (get previous-nodes current) (conj path current)))))

(defn shortest-path
  "Finds the shortest path between start and end vertices in a graph using Dijkstra's algorithm.

   Parameters:
   - graph: A map representing the graph where each key is a vertex and each value is a list of vectors
            representing edges and their weights.
   - start: The starting vertex.
   - end: The target vertex.

   Returns a vector representing the shortest path from start to end. If no path exists, returns an empty vector.
   "
  [graph start end]
  (assert (and (some #(= start %) (keys graph))
               (some #(= end %) (keys graph)))
          "start and end must exist in the given graph")
  (let [distances        (atom (zipmap (keys graph) (repeat Double/POSITIVE_INFINITY)))
        previous-nodes   (atom (zipmap (keys graph) (repeat nil)))
        nodes-to-explore (pm/priority-map start 0)]
    (swap! distances assoc start 0)
    (loop [nodes nodes-to-explore]
      (if (empty? nodes)
        (let [path (reconstruct-path @previous-nodes end start)]
          (if (= (first path) start)
            path
            []))
        (let [[current-node distance-to-node] (peek nodes)
              remaining-nodes (pop nodes)]
          (if (= distance-to-node (get @distances current-node))
            (let [neighbors        (graph current-node)
                  update-distances (fn [nodes [neighbor weight]]
                                     (let [alternative          (+ distance-to-node weight)
                                           distance-to-neighbor (get @distances neighbor)]
                                       (if (and distance-to-neighbor
                                                (< alternative distance-to-neighbor))
                                         (do
                                           (swap! distances assoc neighbor alternative)
                                           (swap! previous-nodes assoc neighbor current-node)
                                           (assoc nodes neighbor alternative))
                                         nodes)))
                  updated-nodes   (reduce update-distances
                                          remaining-nodes
                                          neighbors)]
              (recur updated-nodes))
            (recur nodes)))))))

;; Example usage:
#_(def random-graph (create-simple-directed-graph 10 20))
#_(shortest-path random-graph (first (keys random-graph)) (last (keys random-graph)))


;;;; QUESTION 4

(defn get-shortest-paths-to-vertices [graph vertice]
  (mapv #(shortest-path graph vertice %) (keys (dissoc graph vertice))))

(defn- get-edge-length [graph v1 v2]
  (let [edges (into {} (graph v1))]
    (if-let [weight (get edges v2)]
      weight
      (throw (Exception. (str "No edge between " v1 " and " v2))))))

(defn compute-path-distance
  "Computes the total distance of a given path in the graph.

   Parameters:
   - graph: A map representing the graph where each key is a vertex and each value is a list of vectors
            representing edges and their weights.
   - path: A vector of vertices representing the path.

   Returns the total distance of the path. If the path is invalid (i.e., there is no edge between two consecutive vertices), returns nil."
  [graph path]
  (if (empty? path)
    0
    (let [distances (map (fn [[v1 v2]] (get-edge-length graph v1 v2)) (partition 2 1 path))]
      (reduce + distances))))

(defn calculate-eccentricity
  "Calculates the eccentricity of a given vertex in the graph g.
   (The eccentricity of a vertex v is defined as the greatest distance between v and any other vertex.)"
  [g v]
  (->> (get-shortest-paths-to-vertices g v)
       (map #(compute-path-distance g %))
       (apply max)))

(defn calculate-radius
  "Calculates the radius of the graph, which is the minimum eccentricity among all vertices."
  [graph]
  (->> (keys graph)
       (map #(calculate-eccentricity graph %))
       (apply min)))

(defn calculate-diameter
  "Calculates the diameter of the graph, which is the maximum eccentricity among all vertices."
  [graph]
  (->> (keys graph)
       (map #(calculate-eccentricity graph %))
       (apply max)))


;; Example usage:

#_(def random-graph (create-simple-directed-graph 10 20))
#_(calculate-eccentricity random-graph (first (keys random-graph)))
#_(calculate-radius random-graph)
#_(calculate-diameter random-graph)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))