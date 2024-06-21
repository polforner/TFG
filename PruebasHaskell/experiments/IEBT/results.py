import pandas as pd
import matplotlib.pyplot as plt

# Leer los archivos
df1 = pd.read_csv(
    r'C:\Users\polfo\Documents\UNI\TFG\Pruebas Haskell\experiments\IEBT\bt-graph-dpOld\results.txt', sep=" ", header=None)
df2 = pd.read_csv(
    r'C:\Users\polfo\Documents\UNI\TFG\Pruebas Haskell\experiments\IEBT\bt-graph-dpNew\results.txt', sep=" ", header=None)

# Calcular la media y la desviación estándar de los tiempos de ejecución
df1['mean'] = df1.iloc[:, 1:].mean(axis=1)
df1['std'] = df1.iloc[:, 1:].std(axis=1)
df2['mean'] = df2.iloc[:, 1:].mean(axis=1)
df2['std'] = df2.iloc[:, 1:].std(axis=1)

# Crear el gráfico
plt.figure(figsize=(10, 6))
plt.plot(df1[0], df1['mean'], label='Set')
plt.plot(df2[0], df2['mean'], label='HashSet')
# plt.xscale('log')
# plt.yscale('log')
plt.xlabel('Number of edges')
plt.ylabel('Execution Time (s)')
# plt.title('Execution Time vs Input Size')
labels = range(1000, 3001, 500)
plt.xticks(ticks=labels, labels=labels)
plt.legend()
plt.show()

# Generar la tabla en formato LaTeX
print(df1.to_latex(index=False))
print(df2.to_latex(index=False))
