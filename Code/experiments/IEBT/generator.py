import random


def generate_bipartite_graph(n):
    set1 = range(1, n+1)
    set2 = range(n+1, 2*n+1)
    edges = [(random.choice(set1), random.choice(set2)) for _ in range(n)]
    return edges


def write_to_file(n, edges):
    with open(r'C:\Users\polfo\Documents\UNI\TFG\Pruebas Haskell\experiments\IEBT\{}.txt'.format(n), 'w') as f:
        for edge in edges:
            f.write(f'{edge[0]} {edge[1]}\n')


def write_query_file(m, edges):
    with open(r'C:\Users\polfo\Documents\UNI\TFG\Pruebas Haskell\experiments\IEBT\Q{}.txt'.format(m), 'w') as f:
        f.write('by-edges ' +
                ' '.join(f'({edge[0]},{edge[1]})' for edge in edges))


n = 3000  # Cambia esto al número de aristas que desees
edges = generate_bipartite_graph(n)
write_to_file(n, edges)
for m in range(1000, n+1, 500):  # Genera ficheros de query desde 1000 a 5000 aristas
    random_edges = random.sample(edges, m)
    write_query_file(m, random_edges)
